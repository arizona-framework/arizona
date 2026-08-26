-module(arizona_parse_transform).
-moduledoc """
Compile-time parse transform that converts Erlang element tuples into
optimized template maps with static/dynamic separation.

Intercepts calls to `arizona_template:html/1` (or `az:html/1`) and
`arizona_template:each/2` (or `az:each/2`), compiling element tuples
into `#{s => Statics, d => Dynamics, f => Fingerprint}` maps at compile
time. For `arizona_stateful` modules, it additionally validates and
transforms the `render/1` callback (single root element, `?get(id)` on
root, auto-injection of `az-view`).

## Compilation Pipeline

1. Detect `arizona_stateful` behaviour to enable live-render mode
2. Walk all function bodies, transforming `?html(...)` and `?each(...)` calls
3. For stateful `render/1`, validate root element constraints and inject `az-view`
4. Compile element tuples into statics (binaries) and dynamics (closures)
5. Assign `az` indices to elements with dynamic content for diff targeting
6. Generate a base-36 `phash2` fingerprint from statics for change detection
7. Scope `az` values with the fingerprint prefix to avoid collisions

## Element Forms

| Form | Example | Description |
|------|---------|-------------|
| `{Tag, Attrs, Children}` | `{'div', [], [?get(x)]}` | Standard element |
| `{Tag, Attrs, Expr}` | `{'span', [], ?get(x)}` | Single expression as children |
| `{Tag, Attrs}` | `{'br', []}` | Void element shorthand |

## Attribute Forms

| Form | Example | Output |
|------|---------|--------|
| `{name, ~"value"}` | `{class, ~"box"}` | `class="box"` |
| `{name, Expr}` | `{class, Theme}` | Dynamic attribute |
| `name` (atom) | `disabled` | Boolean attribute |
| `~"name"` (binary) | `~"hidden"` | Boolean attribute |
| `{name, true}` | `{hidden, true}` | Emitted |
| `{name, false}` | `{hidden, false}` | Stripped |
| `'az-nodiff'` | Directive | Stripped; emits `diff => false` |

## Dynamic Tuple Forms

```erlang
{Az, fun(() -> term()), {Module, Line}}               %% text dynamic
{Az, {attr, Name, fun(() -> term())}, {Module, Line}} %% attribute dynamic
{undefined, fun(() -> term()), {Module, Line}}        %% nodiff dynamic
```

## Example

```erlang
%% Input (in a module including arizona_stateless.hrl):
render(Bindings) ->
    ?html({'div', [{class, ~"box"}], [?get(name)]}).

%% Output (after parse transform):
render(Bindings) ->
    #{s => [~"<div class=\"box\" az=\"a1-0\"><!--az:a1-0-->", ~"<!--/az--></div>"],
      d => [{~"a1-0", fun() -> arizona_template:get(name, Bindings) end, {?MODULE, 3}}],
      f => ~"a1"}.
```
""".

-compile({nowarn_redefined_builtin_type, [{dynamic, 0}]}).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([parse_transform/2]).
-export([format_error/1]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

-ignore_xref([parse_transform/2, format_error/1]).

%% --------------------------------------------------------------------
%% Ignore elvis warnings
%% --------------------------------------------------------------------

%% AST construction is inherently repetitive: each make_*_dynamic_ast and
%% build_*_ast helper builds nested {tuple, ...} / {map_field_assoc, ...}
%% literals that look structurally similar but represent different shapes.
-elvis([{elvis_style, dont_repeat_yourself, disable}]).

%% fresh_helper_var/1 and fresh_each_var/2 mint compile-time variable names via
%% list_to_atom -- fresh names are the point (no source expression may contain
%% them), so list_to_existing_atom cannot apply; growth is bounded by helper call
%% sites and ?each call sites.
-elvis([{elvis_style, no_common_caveats_call, disable}]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([static/0]).
-export_type([dynamic/0]).
-export_type([az/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-nominal static() :: binary().
-nominal dynamic() :: erl_parse:abstract_form().
-nominal az() :: non_neg_integer().

%% One piece of a static under construction. Plain bytes are literal output;
%% the two tagged forms are the framework's own `az` markers, held apart from
%% the literal bytes so the fingerprint scoping can rebuild exactly those and
%% never touch user-authored content that happens to look like a marker (a
%% static text child is spliced verbatim -- it is the documented raw-HTML seam
%% -- so ` az="` / `<!--az:` in a page *about* markup is ordinary content).
-type segment() :: binary() | {az_attr, binary()} | {az_slot, binary()}.

%% Compile state threaded through element/attribute/child compilation.
-record(state, {
    %% Segments of the static under construction, newest first.
    buf = [] :: [segment()],
    statics = [] :: [[segment()]],
    dynamics = [] :: [dynamic()],
    az = 0 :: az(),
    nodiff = false :: boolean(),
    %% Raw-text context of the enclosing element while compiling its children
    %% (Backend:raw_text_kind/2). `raw`/`escapable` mean a dynamic content slot
    %% must be emitted markerless and render-once -- HTML comment markers would
    %% become literal content there (script/style/textarea/title).
    raw_text_kind = none :: none | raw | escapable,
    %% The tag that produced `raw_text_kind`, handed to the backend's `raw_text/2`:
    %% the neutralization a value needs belongs to *that element's* tokenizer state
    %% (`<script>` is script data, `<style>` is RAWTEXT), not to raw text at large.
    raw_text_tag = undefined :: atom(),
    %% Content context of the enclosing element while compiling its children
    %% (Backend:content_context/2). `foreign` is inside `<svg>`, where an element
    %% is ordinary parsed content, so a tag HTML treats as escapable raw text
    %% (`title`) keeps its markers and stays diffable.
    content_ctx = html :: arizona_renderer:content_context(),
    module = undefined :: module() | undefined,
    live_render = false :: boolean(),
    root = false :: boolean(),
    backend = target_backend(html) :: module()
}).

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc """
Entry point for the Erlang compiler parse transform.

Walks all forms in the module, transforming `arizona_template:html/1`
and `arizona_template:each/2` calls into compiled template maps. For
modules with `-behaviour(arizona_stateful)`, the `render/1` callback
receives additional validation and `az-view` injection.
""".
-spec parse_transform(Forms, Options) -> Forms | {error, Errors, []} when
    Forms :: [erl_parse:abstract_form()],
    Options :: [compile:option()],
    Errors :: [{file:filename(), [{erl_anno:line(), module(), term()}]}].
parse_transform(Forms, _Options) ->
    File = extract_file(Forms),
    Module = extract_module(Forms),
    IsLive = has_behaviour(Forms, arizona_stateful),
    FunDefs = collect_fun_defs(Forms),
    try
        Transformed = [
            transform_form(mark_targets(Form, none), Module, IsLive, FunDefs)
         || Form <- Forms
        ],
        WithSuppressions = inject_each_callback_suppressions(Transformed, Forms, FunDefs, Module),
        erl_syntax:revert_forms(WithSuppressions)
    catch
        throw:{arizona_parse_error, Line, Reason} ->
            {error, [{File, [{Line, ?MODULE, Reason}]}], []}
    end.

%% Map every `{Name, Arity}` to its clause list, so a `?each(fun Name/Arity, _)`
%% callback can be resolved to its body and inlined like an anonymous fun.
collect_fun_defs(Forms) ->
    #{{Name, Arity} => Clauses || {function, _, Name, Arity, Clauses} <- Forms}.

%% A local `fun Name/Arity` ?each callback -- or a local element helper called
%% in a template (inline_helper_calls/4) -- is inlined at its use site, so the
%% function loses its only reference. Suppress the resulting unused-function
%% warning (compiler, under warnings_as_errors) and xref finding (locals_not_used /
%% exports_not_used) by injecting -compile(nowarn_unused_function) and -ignore_xref
%% attributes for the consumed pairs. A no-op when the function is also used elsewhere.
inject_each_callback_suppressions(Forms, OrigForms, FunDefs, Module) ->
    EachPairs = collect_each_callback_pairs(OrigForms, FunDefs, Module),
    HelperPairs = collect_helper_pairs(OrigForms, FunDefs, Module),
    case lists:usort(EachPairs ++ HelperPairs) of
        [] ->
            Forms;
        Pairs ->
            insert_suppression_attrs(Forms, Pairs)
    end.

%% Scan the original forms for local (and same-module explicit) calls whose
%% callee helper_plan_shape/4 would inline under some render target -- those
%% calls disappear when they sit in a template, orphaning the definition. The
%% scan is position-blind, which is sound for suppression: a helper also (or
%% only) called outside a template keeps a real reference, and
%% nowarn_unused_function on a referenced function is a no-op.
collect_helper_pairs(Forms, FunDefs, Module) ->
    Pairs = helper_call_pairs(Forms, #{}, FunDefs, Module),
    [Pair || Pair := _ <- Pairs].

helper_call_pairs({call, _, {atom, _, Name}, Args} = Node, Acc, FunDefs, Module) ->
    Acc1 = maybe_helper_pair(Name, length(Args), FunDefs, Acc),
    helper_call_pairs(tuple_to_list(Node), Acc1, FunDefs, Module);
helper_call_pairs(
    {call, _, {remote, _, {atom, _, Module}, {atom, _, Name}}, Args} = Node,
    Acc,
    FunDefs,
    Module
) ->
    Acc1 = maybe_helper_pair(Name, length(Args), FunDefs, Acc),
    helper_call_pairs(tuple_to_list(Node), Acc1, FunDefs, Module);
helper_call_pairs(Node, Acc, FunDefs, Module) when is_tuple(Node) ->
    helper_call_pairs(tuple_to_list(Node), Acc, FunDefs, Module);
helper_call_pairs([H | T], Acc, FunDefs, Module) ->
    helper_call_pairs(T, helper_call_pairs(H, Acc, FunDefs, Module), FunDefs, Module);
helper_call_pairs(_Node, Acc, _FunDefs, _Module) ->
    Acc.

maybe_helper_pair(Name, Arity, FunDefs, Acc) ->
    Inlineable = lists:any(
        fun(Target) ->
            case helper_plan_shape(Name, Arity, FunDefs, Target) of
                {inline, _Params, _Body} -> true;
                _ -> false
            end
        end,
        [html, native, terminal]
    ),
    case Inlineable of
        true -> Acc#{{Name, Arity} => true};
        false -> Acc
    end.

%% Scan the original forms (pre-mark_targets, so every ?each is still spelled `each`
%% -- mark_targets renames nested each to native_each/terminal_each and unwraps an
%% inline `?html`/`?native`/`?terminal` callback body, but never rewrites a
%% `fun Name/Arity` reference) for local `fun Name/Arity` (and same-module
%% `fun ?MODULE:Name/Arity`) callbacks. Returns the deduped pairs defined in this module.
collect_each_callback_pairs(Forms, FunDefs, Module) ->
    Pairs = each_callback_pairs(Forms, #{}, Module),
    [Pair || Pair := _ <- Pairs, is_map_key(Pair, FunDefs)].

each_callback_pairs(
    {call, _, {remote, _, {atom, _, Mod}, {atom, _, each}}, [Callback | _]} = Node, Acc, Module
) when
    Mod =:= arizona_template; Mod =:= az
->
    Acc1 =
        case each_callback_pair(Callback, Module) of
            {ok, Pair} -> Acc#{Pair => true};
            none -> Acc
        end,
    each_callback_pairs(tuple_to_list(Node), Acc1, Module);
each_callback_pairs(Node, Acc, Module) when is_tuple(Node) ->
    each_callback_pairs(tuple_to_list(Node), Acc, Module);
each_callback_pairs(Nodes, Acc, Module) when is_list(Nodes) ->
    lists:foldl(fun(N, A) -> each_callback_pairs(N, A, Module) end, Acc, Nodes);
each_callback_pairs(_Node, Acc, _Module) ->
    Acc.

%% A local `fun Name/Arity` or same-module `fun ?MODULE:Name/Arity` callback -- the pair to
%% suppress. An inline fun or a genuinely remote ref contributes nothing.
each_callback_pair({'fun', _, {function, Name, Arity}}, _Module) ->
    {ok, {Name, Arity}};
each_callback_pair(
    {'fun', _, {function, {atom, _, Module}, {atom, _, Name}, {integer, _, Arity}}}, Module
) ->
    {ok, {Name, Arity}};
each_callback_pair(_Callback, _Module) ->
    none.

insert_suppression_attrs(Forms, Pairs) ->
    {Before, [ModAttr | After]} = lists:splitwith(
        fun(Form) -> not is_module_attr(Form) end, Forms
    ),
    Anno = element(2, ModAttr),
    NowarnAttr = {attribute, Anno, compile, {nowarn_unused_function, Pairs}},
    IgnoreXrefAttr = {attribute, Anno, ignore_xref, Pairs},
    Before ++ [ModAttr, NowarnAttr, IgnoreXrefAttr | After].

is_module_attr({attribute, _, module, _}) -> true;
is_module_attr(_) -> false.

%% Top-down pre-pass threading the enclosing render target `Ctx` (`none` outside
%% any template, else `html` | `native` | `terminal`). Three jobs:
%%
%%   1. A single `?each` serves every target. Inside a `?native(...)` each nested
%%      `?each` is rewritten to the internal `native_each`, and inside a
%%      `?terminal(...)` to `terminal_each`, so the bottom-up transform compiles it
%%      with the matching backend. `?each` under `?html` (or standalone) keeps the
%%      `each` name (default `html` target).
%%   2. Reject inline cross-target nesting: any target macro literally inside a
%%      different one (e.g. `?html(...)` in `?native(...)`) would mix incompatible
%%      statics in one tree. Caught here as a compile error instead of corrupting
%%      the output at runtime.
%%   3. Unwrap a whole-body backend wrapper in an `?each` INLINE callback
%%      (`fun(I) -> ?html({li,...}) end` -> `fun(I) -> {li,...} end`) before the
%%      bottom-up transform pre-compiles the inner wrapper to a map. This routes
%%      the body through `compile_each_clause`'s `{element, Inner}` path (like a
%%      bare/named-ref body), so a single-root item keeps its `single_root` flag.
%%      See `unwrap_each_body/2`.
%%
%% Each target call resets `Ctx` for its own argument, so sibling targets (e.g. a
%% dual-serve render with `?html` and `?native` in different clauses) are fine --
%% only one literally nested in the other errors. Cross-target nesting via a
%% `?stateful`/`?stateless` child *module* is invisible at this AST level, so it
%% is caught at render instead (`arizona_render:assert_same_target/2`); it used to
%% pass silently and emit a payload the client could not parse.
mark_targets({call, L, {remote, _, {atom, _, Mod}, {atom, _, Target}}, _}, Ctx, _CCtx) when
    (Mod =:= arizona_template orelse Mod =:= az) andalso
        (Target =:= html orelse Target =:= native orelse Target =:= terminal) andalso
        (Ctx =:= html orelse Ctx =:= native orelse Ctx =:= terminal) andalso
        Ctx =/= Target
->
    parse_error(cross_target_nesting, L);
mark_targets({call, L, {remote, RL, {atom, ML, Mod}, {atom, FL, Target}}, [Arg]}, _Ctx, _CCtx) when
    (Mod =:= arizona_template orelse Mod =:= az) andalso
        (Target =:= html orelse Target =:= native orelse Target =:= terminal)
->
    %% Entering a target resets the content context to that backend's default.
    Default = (target_backend(Target)):content_context(undefined, html),
    {call, L, {remote, RL, {atom, ML, Mod}, {atom, FL, Target}}, [
        mark_targets(Arg, Target, Default)
    ]};
%% `?each` under `?html` (or standalone -- `none`) keeps the name `each` (the
%% bottom-up transform compiles it with the default `html` target), but
%% we still unwrap an inline `?html(...)` callback body here. The guard lets any
%% other (future) `Ctx` fall through to the generic tuple recursion below.
mark_targets({call, L, {remote, RL, {atom, ML, Mod}, {atom, FL, each}}, Args}, Ctx, CCtx) when
    Mod =:= arizona_template orelse Mod =:= az
->
    %% The backend names its own each marker for this content context, so the
    %% transform hardcodes neither a target nor a context vocabulary. A backend
    %% with one context answers the same marker every time, which makes this
    %% inert for it without a target check here.
    Backend = ctx_backend(Ctx),
    Marker = Backend:each_marker(CCtx),
    {call, L, {remote, RL, {atom, ML, Mod}, {atom, FL, Marker}}, [
        mark_targets(A, Ctx, CCtx)
     || A <- unwrap_each_body(Args, Backend:target())
    ]};
%% An element tuple: its children may sit in a different content context (the
%% backend decides -- `<svg>` opens foreign content for `?html`). Matched on the
%% element shape only, so a plain data tuple a user writes cannot shift it.
mark_targets({tuple, L, [{atom, _, Tag}, _Attrs] = Parts}, Ctx, CCtx) ->
    mark_element_children(L, Tag, Parts, Ctx, CCtx);
mark_targets({tuple, L, [{atom, _, Tag}, _Attrs, _Children] = Parts}, Ctx, CCtx) ->
    mark_element_children(L, Tag, Parts, Ctx, CCtx);
mark_targets(Node, Ctx, CCtx) when is_tuple(Node) ->
    list_to_tuple([mark_targets(E, Ctx, CCtx) || E <- tuple_to_list(Node)]);
mark_targets(Nodes, Ctx, CCtx) when is_list(Nodes) ->
    [mark_targets(E, Ctx, CCtx) || E <- Nodes];
mark_targets(Node, _Ctx, _CCtx) ->
    Node.

mark_element_children(L, Tag, Parts, Ctx, CCtx) ->
    ChildCCtx = (ctx_backend(Ctx)):content_context(Tag, CCtx),
    {tuple, L, [mark_targets(P, Ctx, ChildCCtx) || P <- Parts]}.

%% The backend for a marking context. `none` (no target entered yet) uses the
%% default target's backend so the two callbacks above always have an answer.
ctx_backend(none) -> target_backend(html);
ctx_backend(Ctx) -> target_backend(Ctx).

mark_targets(Node, Ctx) ->
    mark_targets(Node, Ctx, (ctx_backend(Ctx)):content_context(undefined, html)).

%% Unwrap a whole-body backend wrapper (`?html`/`?native`/`?terminal`, spelled
%% `Wrapper`) in an each's INLINE single-clause callback: rewrite the body's last
%% expr `Wrapper(Inner)` to the bare `Inner` element. This MUST run in the
%% top-down pre-pass, before the bottom-up transform compiles that inner wrapper
%% to a template-map literal. An unwrapped bare element then reaches
%% `compile_each_clause` via the `{element, Inner}` path -- exactly as a bare-body
%% or named-fun-ref body does -- so a single-root item is classified and gets
%% `single_root => true` (positional diffing). Without this, an inline
%% `fun(U) -> ?html({li,...}) end` is pre-compiled to `{compiled, Map}`, which
%% skips classification and silently drops the flag (wholesale re-render instead).
%% Only the first arg, and only an inline fun, is touched; a `fun Name/Arity` ref
%% (resolved from its untransformed clause) or a non-fun arg is left unchanged, so
%% the named-ref and bare paths are unaffected. The unwrap mirrors
%% `each_body_unwrap/2`'s `{element, Inner}` clause for the not-yet-compiled call.
unwrap_each_body([{'fun', FL, {clauses, [{clause, CL, Vars, Guards, Body}]}} | Rest], Wrapper) ->
    NewBody = unwrap_last_wrapper(Body, Wrapper),
    [{'fun', FL, {clauses, [{clause, CL, Vars, Guards, NewBody}]}} | Rest];
unwrap_each_body(Args, _Wrapper) ->
    Args.

unwrap_last_wrapper(Body, Wrapper) ->
    {Prefix, Last} = split_fun_body(Body),
    case Last of
        {call, _, {remote, _, {atom, _, Mod}, {atom, _, Wrapper}}, [Inner]} when
            Mod =:= arizona_template orelse Mod =:= az
        ->
            Prefix ++ [Inner];
        _ ->
            Body
    end.

-doc """
Formats parse transform error reasons into human-readable messages.

Called by the compiler when `parse_transform/2` returns an error tuple.
""".
-spec format_error(Reason) -> string() when
    Reason :: term().
format_error({render_reject, Message}) ->
    unicode:characters_to_list(Message);
format_error(invalid_element) ->
    "invalid element form, expected {Tag, Attrs, Children}, "
    "{Tag, Attrs, Expr}, or {Tag, Attrs} where Tag is an atom";
format_error({void_with_children, Tag}) ->
    lists:flatten(
        io_lib:format(
            "void element '~s' cannot have children", [Tag]
        )
    );
format_error(invalid_attribute) ->
    "invalid attribute form, expected {Name, Value}, Name (atom), "
    "<<\"Name\">> (binary), or {Name, true|false}";
format_error(invalid_each_fun) ->
    "each/2 expects a fun with a single clause and one or two parameters";
format_error(each_body_not_element) ->
    "an ?each callback over a list must return an element ({Tag, Attrs, Children}) or a "
    "list of elements -- ?each builds a per-item template for fine-grained diffing, so a "
    "single-value body defeats its purpose (and a template or descriptor value crashes on "
    "the first diff, keyed by to_bin of the first dynamic). For a list of plain values use a "
    "list comprehension or lists:map/2. For a conditional, put it inside an element as a "
    "text/value child: {li, [], [case ... of ... end]}";
format_error(each_stream_body_not_element) ->
    "an ?each callback over a stream or map (a 2-arg fun) must return an element ({Tag, "
    "Attrs, Children}) or a list of elements -- a stream/map keys each item for per-item "
    "diffing, which a single-value body throws away. Unlike a list there is no comprehension "
    "fallback (a comprehension has no stream/keyed semantics): wrap the value in an element, "
    "e.g. fun(Item, Key) -> {li, [], [Item]} end";
format_error(each_named_fun_multi_clause) ->
    "an ?each callback given as a local fun reference (fun name/1 or fun name/2) must have a "
    "single clause -- ?each inlines the function's body into one shared per-item template, so "
    "multiple clauses (which would select different per-item structures) can't be compiled to "
    "a single template. Collapse them into one clause with a case inside the returned element: "
    "name(I) -> {li, [], [case I of ... end]}";
format_error(each_named_fun_undefined) ->
    "the ?each callback references a local fun (fun name/1 or fun name/2) that is not defined "
    "in this module. Define it as a single-clause function returning an element, or inline the "
    "callback (fun(I) -> {li, [], [...]} end)";
format_error(each_remote_fun_ref) ->
    "an ?each callback cannot be a remote fun reference (fun mod:name/arity) -- ?each inlines "
    "the callback body to build a per-item template, which is impossible across a module "
    "boundary. Inline it (fun(I) -> {li, [], [...]} end), or move the body into a single-clause "
    "local function and pass fun name/1";
format_error(live_render_not_single_element) ->
    "arizona_stateful render/1 must return a single root element, not a list";
format_error(live_render_missing_id) ->
    "arizona_stateful render/1 root element must have an id attribute";
format_error(live_render_id_must_be_get_id) ->
    "arizona_stateful render/1 root element id must use "
    "?get(id), arizona_template:get(id, Bindings), or az:get(id, Bindings)";
format_error(az_view_not_allowed) ->
    "az_view attribute is auto-injected by the parse transform in "
    "arizona_stateful render/1 and must not be set manually";
format_error({reserved_attr, Name}) ->
    lists:flatten(
        io_lib:format(
            "the ~ts attribute is reserved -- the parse transform emits it itself "
            "(az addresses an element for the diff, az-local describes its ?local "
            "slots), so a template-authored one renders as a duplicate whose value "
            "can collide with a real slot address and misroute a patch. Remove it: "
            "az-key, the az-* event attributes and any az-* name of your own are "
            "unaffected",
            [Name]
        )
    );
format_error({invalid_child, ValueStr}) ->
    lists:flatten(
        io_lib:format(
            "invalid child: static tuple is not a valid template child "
            "-- use a binary, element, or dynamic expression. "
            "Got: ~s",
            [ValueStr]
        )
    );
format_error(local_key_not_literal) ->
    "?local/2 key must be a literal binary or atom";
format_error(local_in_nodiff) ->
    "?local cannot be used in an az-nodiff template -- the element has no "
    "diff target for the client to address";
format_error(local_unsupported) ->
    "?local is not supported by this render target";
format_error(dynamic_in_raw_text) ->
    "a dynamic content slot inside <script>/<style> is spliced verbatim -- the browser "
    "decodes no character references in raw text, so HTML-escaping cannot apply there and "
    "the value can close the JavaScript string it sits in, or the element itself (XSS). "
    "Mark it with a literal ?raw(...) at the slot to state the value is already safe for "
    "the script/CSS context, and only then -- serialize data first, e.g. "
    "{script, [], [?raw(json:encode(Data))]}. The ?raw has to sit at the slot itself: a "
    "variable holding one reaches the slot only when its expression reads a binding, so "
    "compute the value into a variable and wrap it here (Json = json:encode(Data), then "
    "{script, [], [?raw(Json)]}), not the other way round. Literal script/CSS text is "
    "static, not a slot, and needs no wrapper";
format_error(local_in_raw_text) ->
    "a content ?local cannot be used inside a raw-text element "
    "(script/style/textarea/title) -- raw-text content carries no slot markers "
    "for the client to address, so the value would never update";
format_error(local_key_reused) ->
    "a ?local key cannot bind both content and an attribute on the same element";
format_error(local_orphaned) ->
    "?local must sit inside an element -- as a content child or an attribute "
    "value. At a fragment top level, as a whole template body, or as a bare "
    "conditional-branch value there is no enclosing element to carry the "
    "az-local descriptor, so the client could never bind the key and "
    "set/set_all would be silent no-ops";
%% The helper_* rejection reasons are the only 3-tuple reasons; dispatch to
%% format_helper_error/3 to keep format_error/1's clause complexity in check.
format_error({HelperReason, Name, Arity}) ->
    format_helper_error(HelperReason, Name, Arity);
format_error(local_attr_multiple) ->
    "an attribute value can interpolate at most one ?local -- multiple ?local in "
    "one attribute can't be recomposed client-side";
format_error(local_attr_mixed) ->
    "a ?local in an attribute value can only be combined with static text, not "
    "another dynamic expression";
format_error(nested_nodiff) ->
    "az-nodiff is only honored on a template's top-level (root) element -- it is a "
    "whole-template directive, not a per-element one, so on a nested element it cannot "
    "be scoped and would leak the reserved attribute into the DOM. Move it to the root "
    "element, or split the subtree into its own ?html/?stateless template whose root "
    "carries az-nodiff";
format_error(cross_target_nesting) ->
    "cannot nest ?html, ?native and ?terminal in one template -- they produce "
    "incompatible statics. A ?stateful/?stateless child does NOT bridge them: the "
    "child's target is invisible here, so the mismatch survives compilation and "
    "is caught at render instead (cross_target_child), where the payload would "
    "otherwise be unparseable. One target per template tree -- render the other "
    "target from its own tree";
format_error(tracked_get_on_non_bindings_map) ->
    "arizona_template:get/get_lazy/with (and the az: aliases) track every read against "
    "the view bindings, so their map argument must be the bindings -- a parameter or a "
    "direct alias (`B = Bindings`). This local is not provably the bindings. If it is a "
    "nested map, read it with maps:get/2 (`User = arizona_template:get(user, Bindings), "
    "Name = maps:get(name, User)`); if it is a bindings value reached through a "
    "case/merge the transform cannot see into, alias it directly with `B = Bindings` first".

format_helper_error(helper_multi_clause, Name, Arity) ->
    lists:flatten(
        io_lib:format(
            "local helper ~ts/~p returns an element from a multi-clause "
            "definition -- an element helper is inlined at the call site, so it "
            "must be a single clause. Collapse the clauses into a case inside "
            "the returned element",
            [Name, Arity]
        )
    );
format_helper_error(helper_guarded, Name, Arity) ->
    lists:flatten(
        io_lib:format(
            "local element helper ~ts/~p has a guard -- a guard cannot be "
            "preserved when the body is inlined at the call site. Move the "
            "condition into a case inside the returned element",
            [Name, Arity]
        )
    );
format_helper_error(helper_params_not_vars, Name, Arity) ->
    lists:flatten(
        io_lib:format(
            "local element helper ~ts/~p has a non-variable, repeated, or _ "
            "parameter -- arguments are inlined by substitution, which needs "
            "each parameter to be a distinct plain variable. Destructure inside "
            "the returned element instead",
            [Name, Arity]
        )
    );
format_helper_error(helper_body_not_single_expr, Name, Arity) ->
    lists:flatten(
        io_lib:format(
            "local element helper ~ts/~p has statements before its element -- "
            "an inlined body must be a single element expression (a begin-block "
            "would leak its bindings into the caller). Compute values inside "
            "the element, or take them as parameters",
            [Name, Arity]
        )
    );
format_helper_error(helper_recursive, Name, Arity) ->
    lists:flatten(
        io_lib:format(
            "local element helper ~ts/~p is recursive -- its body cannot be "
            "inlined into itself. Render repetition with ?each or a "
            "?stateless/?stateful child",
            [Name, Arity]
        )
    ).

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

parse_error(Reason, Line) ->
    throw({arizona_parse_error, Line, Reason}).

%% `arizona_template:get`/`get_lazy`/`with` (and the `az:` aliases) call `track/1`
%% regardless of which map they read, so reading a sub-map records the inner key
%% as a top-level slot dependency. Reject a tracked read whose map argument is a
%% variable that is not in scope as a bindings-like value: a clause/fun parameter
%% (so `?each` item/key vars pass) or an alias of one (including a var bound by a
%% `with/2` projection). A non-variable map argument (a literal map, a `maps:get`
%% call, any expression) is never flagged.
check_tracked_get_targets(Patterns, Body) ->
    Params = collect_fun_param_vars(Body, collect_pattern_vars(Patterns, #{})),
    walk_tracked_gets(Body, alias_closure(Body, Params)).

%% Every fun/named_fun parameter anywhere in Body. Flat across nesting depth:
%% over-inclusion only drops a flag (false negative), never adds one.
collect_fun_param_vars({'fun', _, {clauses, Cs}}, Acc) ->
    collect_clause_param_vars(Cs, Acc);
collect_fun_param_vars({named_fun, _, _Name, Cs}, Acc) ->
    collect_clause_param_vars(Cs, Acc);
collect_fun_param_vars(T, Acc) when is_tuple(T) ->
    collect_fun_param_vars(tuple_to_list(T), Acc);
collect_fun_param_vars([H | T], Acc) ->
    collect_fun_param_vars(T, collect_fun_param_vars(H, Acc));
collect_fun_param_vars(_, Acc) ->
    Acc.

collect_clause_param_vars(Clauses, Acc) ->
    lists:foldl(
        fun({clause, _, Params, _Guards, ClauseBody}, A) ->
            collect_fun_param_vars(ClauseBody, collect_pattern_vars(Params, A))
        end,
        Acc,
        Clauses
    ).

%% Grow the scope with every `V = W` (var = var) where W is already in scope, plus
%% every `V = arizona_template:with(_, W)` / `az:with(_, W)` (a tracked projection of W
%% is bindings-like), to a fixpoint (handles `B = Bindings, C = B`).
alias_closure(Body, Scope) ->
    Matches = collect_matches(Body, []),
    VarAliases = [{V, W} || {match, _, {var, _, V}, {var, _, W}} <- Matches],
    WithAliases = [
        {V, W}
     || {match, _, {var, _, V},
            {call, _, {remote, _, {atom, _, Mod}, {atom, _, with}}, [_Keys, {var, _, W} | _]}} <-
            Matches,
        Mod =:= arizona_template orelse Mod =:= az
    ],
    alias_fixpoint(VarAliases ++ WithAliases, Scope).

alias_fixpoint(Aliases, Scope) ->
    Scope1 = lists:foldl(
        fun
            ({V, W}, A) when is_map_key(W, A) -> A#{V => true};
            (_Pair, A) -> A
        end,
        Scope,
        Aliases
    ),
    case map_size(Scope1) =:= map_size(Scope) of
        true -> Scope1;
        false -> alias_fixpoint(Aliases, Scope1)
    end.

%% Flat across nesting depth and scope-unaware: a `with`/var alias bound inside an inner
%% fun/case registers its name as bindings-like globally. Like the param collection above,
%% the only consequence is a dropped flag (a benign over-track if an outer same-named
%% sub-map read is masked), never a wrong rejection.
collect_matches(T, Acc) when is_tuple(T) ->
    Acc1 =
        case T of
            {match, _, _, _} -> [T | Acc];
            _ -> Acc
        end,
    collect_matches(tuple_to_list(T), Acc1);
collect_matches([H | T], Acc) ->
    collect_matches(T, collect_matches(H, Acc));
collect_matches(_, Acc) ->
    Acc.

walk_tracked_gets(AST, Scope) when is_tuple(AST) ->
    flag_tracked_get(AST, Scope),
    walk_tracked_gets(tuple_to_list(AST), Scope);
walk_tracked_gets([H | T], Scope) ->
    walk_tracked_gets(H, Scope),
    walk_tracked_gets(T, Scope);
walk_tracked_gets(_, _Scope) ->
    ok.

flag_tracked_get(
    {call, L, {remote, _, {atom, _, Mod}, {atom, _, F}}, [_Key, {var, _, V} | _]}, Scope
) when
    (Mod =:= arizona_template orelse Mod =:= az) andalso
        (F =:= get orelse F =:= get_lazy orelse F =:= with) andalso
        not is_map_key(V, Scope)
->
    parse_error(tracked_get_on_non_bindings_map, L);
flag_tracked_get(_Node, _Scope) ->
    ok.

line(Node) when is_tuple(Node), tuple_size(Node) >= 2 ->
    erl_anno:line(element(2, Node));
line(_) ->
    0.

extract_file([{attribute, _, file, {File, _}} | _]) -> File;
extract_file([_ | Rest]) -> extract_file(Rest);
extract_file([]) -> "nofile".

extract_module([{attribute, _, module, Mod} | _]) -> Mod;
extract_module([_ | Rest]) -> extract_module(Rest);
extract_module([]) -> undefined.

has_behaviour([{attribute, _, behaviour, B} | _], B) -> true;
has_behaviour([{attribute, _, behavior, B} | _], B) -> true;
has_behaviour([_ | Rest], B) -> has_behaviour(Rest, B);
has_behaviour([], _) -> false.

transform_form({function, L, render, 1, Clauses}, Module, true, FunDefs) ->
    {function, L, render, 1, [transform_live_render_clause(C, Module, FunDefs) || C <- Clauses]};
transform_form({function, L, Name, Arity, Clauses}, Module, _IsLive, FunDefs) ->
    {function, L, Name, Arity, [transform_clause(C, Module, FunDefs) || C <- Clauses]};
transform_form(Form, _Module, _IsLive, _FunDefs) ->
    Form.

transform_clause({clause, L, Patterns, Guards, Body0}, Module, FunDefs) ->
    check_tracked_get_targets(Patterns, Body0),
    Body = normalize_tail_binds(Body0),
    Inline = prepare_inline(collect_inline(Body), Body),
    Body1 = [transform_expr(Expr, Module, Inline, FunDefs) || Expr <- Body],
    {clause, L, Patterns, Guards, suppress_unused_inline_matches(Body1, Inline)}.

transform_expr(Expr, Module, Inline, FunDefs) ->
    erl_syntax_lib:map(fun(Node) -> transform_node(Node, Module, Inline, FunDefs) end, Expr).

transform_node(Node, Module, Inline, FunDefs) ->
    N = erl_syntax:revert(Node),
    case N of
        %% `?html`/`?native`/`?terminal` all compile non-live here (LiveRender =
        %% false). The live root is handled in transform_live_render_leaf/4; and
        %% `?terminal` is *always* non-live -- no client root id / az_view is
        %% injected (the transport repaints whole frames rather than addressing the
        %% root node), so the live-render last-expr path also falls through to here.
        {call, L, {remote, _, {atom, _, Mod}, {atom, _, Target}}, [Arg]} when
            (Mod =:= arizona_template orelse Mod =:= az) andalso
                (Target =:= html orelse Target =:= native orelse Target =:= terminal)
        ->
            Arg1 = inline_vars(Arg, Inline),
            {Arg2, HelperCh} = inline_helper_calls(Arg1, Module, FunDefs, Target),
            Arg3 = retransform_spliced(
                Arg2, map_size(Inline) > 0 orelse HelperCh, Module, FunDefs
            ),
            compile_template(Arg3, L, Module, false, target_backend(Target));
        {call, L, {remote, _, {atom, _, Mod}, {atom, _, EachFn}}, [FunArg, SourceArg]} when
            (Mod =:= arizona_template orelse Mod =:= az) andalso
                (EachFn =:= each orelse EachFn =:= foreign_each orelse
                    EachFn =:= native_each orelse EachFn =:= terminal_each)
        ->
            case is_compiled_each_pairing(EachFn, SourceArg) of
                true ->
                    %% Already-compiled pairing (`arizona_template:each(Source,
                    %% #{s,d,f,...})` from a previous pass) revisited by a
                    %% retransform_spliced/4 re-run: pass it through untouched.
                    N;
                false ->
                    Spliced = map_size(Inline) > 0,
                    FunArg1 = unwrap_spliced_each_callback(
                        inline_vars(FunArg, Inline), EachFn, Inline
                    ),
                    {FunArg2, FunCh} = inline_helper_calls(
                        FunArg1, Module, FunDefs, each_target(EachFn)
                    ),
                    compile_each(
                        retransform_spliced(FunArg2, Spliced orelse FunCh, Module, FunDefs),
                        retransform_spliced(
                            inline_vars(SourceArg, Inline), Spliced, Module, FunDefs
                        ),
                        L,
                        Module,
                        target_backend(each_target(EachFn)),
                        each_content_ctx(EachFn),
                        FunDefs
                    )
            end;
        %% Sugar: `arizona_template:stateless(atom, Props)` with a literal atom
        %% callback is rewritten to `arizona_template:stateless(fun atom/1, Props)`.
        %% Fun references and other shapes pass through unchanged.
        {call, L, {remote, _, {atom, _, Mod}, {atom, _, stateless}} = Callee, [
            {atom, AL, Name}, PropsArg
        ]} when
            Mod =:= arizona_template orelse Mod =:= az
        ->
            FunRef = {'fun', AL, {function, Name, 1}},
            {call, L, Callee, [FunRef, PropsArg]};
        _ ->
            N
    end.

%% Inlining a hoisted variable (inline_vars/2) or a local element helper
%% (inline_helper_calls/4) may splice a raw, untransformed template-constructor
%% call -- `arizona_template:html/1`, `each/2`, the `stateless(atom, Props)`
%% sugar -- into an expression the bottom-up transform has already finished
%% with. Left raw, the call hits the runtime stub (parse_transform_not_applied)
%% or the untransformed sugar (function_clause) at first render. Re-run the
%% bottom-up transform over the spliced expression so those constructors
%% compile exactly as written-inline code would. The re-run is idempotent on
%% already-compiled output: template maps are not calls, the stateless sugar
%% only matches an atom callback, and a compiled each pairing is recognized and
%% skipped (is_compiled_each_pairing/2). Skipped entirely when nothing was
%% spliced.
retransform_spliced(Expr, false, _Module, _FunDefs) ->
    Expr;
retransform_spliced(Expr, true, Module, FunDefs) ->
    transform_expr(Expr, Module, #{}, FunDefs).

%% True for the OUTPUT form of a compiled ?each -- `arizona_template:each(Source,
%% #{t,s,d,f,...})` (build_each_ast/build_each_from_compiled) -- revisited when
%% retransform_spliced/4 re-runs the transform. The SOURCE form pairs
%% `(Fun, Source)`, and a compiled pairing always carries the `t` assoc on top
%% of `s`/`d`/`f`, so a literal map source can never be mistaken for one short
%% of spelling out all four keys.
is_compiled_each_pairing(each, {map, _, Fields}) ->
    is_compiled_template_map(Fields) andalso
        lists:member(t, [K || {map_field_assoc, _, {atom, _, K}, _} <:- Fields]);
is_compiled_each_pairing(_EachFn, _SourceArg) ->
    false.

%% Post-splice mirror of the mark-time unwrap_each_body/2 jobs, for material the
%% inline splice just introduced into an ?each callback: flatten a spliced
%% track-wrap block in the callback's last expr, then unwrap a spliced
%% whole-body wrapper (`fun(_I) -> Row end` where Row hoisted `?html({li,...})`)
%% -- BEFORE retransform_spliced/4 would compile that wrapper to a map, which
%% skips body classification and silently drops `single_root` (positional list
%% patching). A no-op when the clause hoists nothing.
unwrap_spliced_each_callback(FunArg, _EachFn, Inline) when map_size(Inline) =:= 0 ->
    FunArg;
unwrap_spliced_each_callback(FunArg, EachFn, _Inline) ->
    [FunArg1 | _] = unwrap_each_body([flatten_spliced_tracks(FunArg)], each_target(EachFn)),
    FunArg1.

%% Flatten a spliced track-wrap block (`begin ok = track(K), ..., Expr end`, the
%% shape prepare_inline_rhs/2 synthesizes) sitting as the last expression of an
%% inline ?each callback: hoist the touches into the body statements so the last
%% expr is the wrapped value again. Without this the block would classify as a
%% non-element body (each_body_not_element). The touches then run per item
%% inside the each's saved-deps bracket, where they are dependency no-ops --
%% exactly the pre-wrap behavior.
flatten_spliced_tracks({'fun', FL, {clauses, [{clause, CL, Vars, Guards, Body}]}} = Fun) ->
    {Prefix, Last} = split_fun_body(Body),
    case Last of
        {block, _, [_, _ | _] = BlockBody} ->
            {Touches, _Wrapped} = split_fun_body(BlockBody),
            case lists:all(fun is_track_touch/1, Touches) of
                true ->
                    {'fun', FL, {clauses, [{clause, CL, Vars, Guards, Prefix ++ BlockBody}]}};
                false ->
                    Fun
            end;
        _ ->
            Fun
    end;
flatten_spliced_tracks(FunArg) ->
    FunArg.

%% The exact statement shape track_call_ast/1 synthesizes.
is_track_touch(
    {match, _, {atom, _, ok},
        {call, _, {remote, _, {atom, _, arizona_template}, {atom, _, track}}, [_Key]}}
) ->
    true;
is_track_touch(_Stmt) ->
    false.

%% --------------------------------------------------------------------
%% Local element-helper inlining
%% --------------------------------------------------------------------
%%
%% A LOCAL call in a template whose callee is a single-clause function
%% returning an element -- `brand()`, `alert(Kind, Msg)` -- is inlined at the
%% call site: the body (unwrapped from a whole-body `?html`/`?native`/
%% `?terminal` wrapper, exactly as ?each callbacks unwrap) replaces the call,
%% with each parameter substituted by its argument expression via the ordinary
%% inline_vars/2 machinery. The spliced element then compiles like a literal
%% one -- it flattens (or leaf-expands) into the template, and `?get` reads in
%% the body or the args land in the enclosing slot's dependency bracket, so
%% the slot stays reactive. Without this, a bare-element body compiled clean
%% and crashed at first render (bad_template_value in to_bin/1).
%%
%% Scope: bare local calls and same-module explicit calls (`?MODULE:helper()`)
%% resolvable through FunDefs -- mirroring the ?each named-fun-ref rules. A
%% callee whose body is not element-shaped (a scalar helper) is untouched: the
%% call runs inside the slot closure, so its reads already fire in-bracket.
%% Rejections fire ONLY for shapes that would otherwise crash at render (a
%% BARE-element body that cannot be cleanly inlined): multi-clause, guarded,
%% non-variable/repeated params, statements before the element, and recursion.
%% A `?html`-wrapped body that cannot be cleanly inlined is NOT rejected -- it
%% already renders as a runtime nested template (the az:with handoff pattern)
%% and keeps doing so. Genuinely remote calls, imported functions (invisible
%% bodies, indistinguishable from auto-imported BIFs), and variable-bound fun
%% calls are undetectable and stay as-is.
%%
%% Returns {Expr1, Changed} so the caller re-runs the bottom-up transform
%% (retransform_spliced/4) only when a body was actually spliced.
inline_helper_calls(Expr, Module, FunDefs, Target) ->
    ih(Expr, {Module, FunDefs, Target, #{}}, false).

ih({call, L, {atom, _, Name}, Args} = Call, Ctx, Ch) ->
    ih_call(Call, Name, Args, L, Ctx, Ch);
ih(
    {call, L, {remote, _, {atom, _, Module}, {atom, _, Name}}, Args} = Call,
    {Module, _, _, _} = Ctx,
    Ch
) ->
    ih_call(Call, Name, Args, L, Ctx, Ch);
ih({map, _, _} = Map, _Ctx, Ch) ->
    %% Map literals hold runtime data (user maps, ?stateful props) or
    %% already-compiled templates from a previous pass -- never descend.
    {Map, Ch};
ih({map, _, _, _} = Map, _Ctx, Ch) ->
    {Map, Ch};
ih(T, Ctx, Ch) when is_tuple(T) ->
    {L1, Ch1} = ih_list(tuple_to_list(T), Ctx, Ch),
    {list_to_tuple(L1), Ch1};
ih(L, Ctx, Ch) when is_list(L) ->
    ih_list(L, Ctx, Ch);
ih(Other, _Ctx, Ch) ->
    {Other, Ch}.

ih_list([], _Ctx, Ch) ->
    {[], Ch};
ih_list([H | T], Ctx, Ch) ->
    {H1, Ch1} = ih(H, Ctx, Ch),
    {T1, Ch2} = ih_list(T, Ctx, Ch1),
    {[H1 | T1], Ch2}.

ih_call(Call, Name, Args, L, {Module, FunDefs, Target, Stack} = Ctx, Ch) ->
    Arity = length(Args),
    case helper_plan(Name, Arity, FunDefs, Target, L) of
        skip ->
            %% Not an inlineable element helper: recurse into the args (they
            %% may contain helper calls of their own).
            {Parts, Ch1} = ih_list(tuple_to_list(Call), Ctx, Ch),
            {list_to_tuple(Parts), Ch1};
        {inline, Params, BodyExpr} ->
            case Stack of
                #{{Name, Arity} := _} -> parse_error({helper_recursive, Name, Arity}, L);
                #{} -> ok
            end,
            BodyExpr1 = rename_inlined_body(Name, Arity, Params, BodyExpr),
            Spliced = mark_targets(subst_helper_args(BodyExpr1, Params, Args), Target),
            Ctx1 = {Module, FunDefs, Target, Stack#{{Name, Arity} => true}},
            {Spliced1, true} = ih(Spliced, Ctx1, true),
            {Spliced1, true}
    end.

%% Substitute the helper's params with the caller's argument expressions via
%% two inline_vars/2 passes through fresh intermediate names. A direct
%% one-pass substitution loops: iv/2 re-enters the substituted expression, so
%% an argument referencing a caller variable named like a parameter
%% (`badge(A)` passing the caller's `A` into a param `A`) recurses forever.
%% Pass one renames each param occurrence to a fresh variable no caller
%% expression can contain; pass two binds the fresh variable to the argument
%% -- both terminate, and iv's scope handling (shadowing funs, patterns,
%% guards untouched) applies unchanged.
subst_helper_args(BodyExpr, [], []) ->
    BodyExpr;
subst_helper_args(BodyExpr, Params, Args) ->
    Fresh = [fresh_helper_var(P) || P <- Params],
    Rename = maps:from_list([{P, {var, 0, F}} || {P, F} <:- lists:zip(Params, Fresh)]),
    Bind = maps:from_list(lists:zip(Fresh, Args)),
    inline_vars(inline_vars(BodyExpr, Rename), Bind).

fresh_helper_var(P) ->
    list_to_atom(
        "AzHelperArg" ++ integer_to_list(erlang:unique_integer([positive])) ++ atom_to_list(P)
    ).

%% Decide what to do with a local call: `skip` (leave the call -- a scalar
%% helper, an unresolvable callee, or a wrapped body that renders fine as-is)
%% or `{inline, ParamNames, BodyExpr}`. Raises the helper_* errors only for
%% bare-element bodies, which pre-fix compiled clean and crashed at render.
helper_plan(Name, Arity, FunDefs, Target, CallLine) ->
    case helper_plan_shape(Name, Arity, FunDefs, Target) of
        {reject, Reason} -> parse_error(Reason, CallLine);
        Plan -> Plan
    end.

%% The non-throwing decision core -- also reused by the position-blind
%% suppression scan (collect_helper_pairs/3), which must never raise for a
%% helper called outside a template.
helper_plan_shape(Name, Arity, FunDefs, Target) ->
    case FunDefs of
        #{{Name, Arity} := Clauses} ->
            helper_clause_plan(Clauses, Name, Arity, Target);
        #{} ->
            skip
    end.

helper_clause_plan([{clause, _, Params, Guards, Body}], Name, Arity, Target) ->
    case helper_body_shape(lists:last(Body), Target) of
        none ->
            skip;
        {Wrap, BodyExpr} ->
            ParamNames = helper_param_names(Params),
            Clean =
                Guards =:= [] andalso is_single_expr_body(Body) andalso
                    ParamNames =/= invalid,
            case {Clean, Wrap} of
                {true, _} ->
                    {inline, ParamNames, BodyExpr};
                {false, wrapped} ->
                    %% Renders as a runtime nested template today; keep it.
                    skip;
                {false, bare} ->
                    {reject, helper_reject_reason(Guards, Body, Name, Arity)}
            end
    end;
helper_clause_plan(Clauses, Name, Arity, Target) ->
    BareClause = fun({clause, _, _P, _G, B}) ->
        case helper_body_shape(lists:last(B), Target) of
            {bare, _} -> true;
            _ -> false
        end
    end,
    case lists:any(BareClause, Clauses) of
        true -> {reject, {helper_multi_clause, Name, Arity}};
        false -> skip
    end.

is_single_expr_body([_]) -> true;
is_single_expr_body(_Body) -> false.

helper_reject_reason([_ | _], _Body, Name, Arity) ->
    {helper_guarded, Name, Arity};
helper_reject_reason([], [_, _ | _], Name, Arity) ->
    {helper_body_not_single_expr, Name, Arity};
helper_reject_reason([], [_], Name, Arity) ->
    {helper_params_not_vars, Name, Arity}.

%% `{bare, Expr}` for a bare element body, `{wrapped, Expr}` for a whole-body
%% wrapper of THIS template's target whose inner is element-shaped, `none`
%% otherwise (scalar bodies, case/if bodies, cross-target wrappers).
helper_body_shape(
    {call, _, {remote, _, {atom, _, Mod}, {atom, _, Target}}, [Inner]}, Target
) when
    Mod =:= arizona_template; Mod =:= az
->
    case is_nested_template_leaf(Inner) of
        true -> {wrapped, Inner};
        false -> none
    end;
helper_body_shape(Expr, _Target) ->
    case is_nested_template_leaf(Expr) of
        true -> {bare, Expr};
        false -> none
    end.

%% All params must be distinct plain variables (not `_`) so substitution is
%% faithful; anything else returns `invalid`.
helper_param_names(Params) ->
    Names = [V || {var, _, V} <- Params, V =/= '_'],
    case length(Names) =:= length(Params) andalso length(lists:usort(Names)) =:= length(Names) of
        true -> Names;
        false -> invalid
    end.

transform_live_render_clause({clause, L, Patterns, Guards, Body}, Module, FunDefs) ->
    check_tracked_get_targets(Patterns, Body),
    {Init0, [Last]} = lists:split(length(Body) - 1, Body),
    %% Only the init statements are normalized: the last expr carries the live root
    %% template and is handled by transform_live_render_last/4.
    Init = normalize_tail_binds(Init0),
    Inline = prepare_inline(collect_inline(Init ++ [Last]), Init ++ [Last]),
    TransformedInit = [transform_expr(Expr, Module, Inline, FunDefs) || Expr <- Init],
    TransformedLast = transform_live_render_last(Last, Module, Inline, FunDefs),
    Body1 = TransformedInit ++ [TransformedLast],
    {clause, L, Patterns, Guards, suppress_unused_inline_matches(Body1, Inline)}.

%% The render clause's last expression carries the live-root template -- a direct
%% ?html/?native call, or a control-flow expression whose every tail carries a
%% root. Walk each tail position (shared with content-slot expansion via
%% map_tail_exprs/3), compiling the root at each leaf; non-tail sub-expressions
%% are transformed normally.
transform_live_render_last(Expr, Module, Inline, FunDefs) ->
    map_tail_exprs(
        Expr,
        fun(Leaf) -> transform_live_render_leaf(Leaf, Module, Inline, FunDefs) end,
        fun(NonTail) -> transform_expr(NonTail, Module, Inline, FunDefs) end
    ).

transform_live_render_leaf(Expr, Module, Inline, FunDefs) ->
    case erl_syntax:revert(Expr) of
        %% A live root is `?html` or `?native` (LiveRender = true, injecting the
        %% client root id / az_view). `?terminal` is always non-live, so it is not
        %% matched here and falls through to the non-live path (transform_node/4).
        {call, L, {remote, _, {atom, _, Mod}, {atom, _, Target}}, [Arg]} when
            (Mod =:= arizona_template orelse Mod =:= az) andalso
                (Target =:= html orelse Target =:= native)
        ->
            validate_live_root(Arg, L, Inline),
            Arg1 = transform_expr(Arg, Module, Inline, FunDefs),
            Arg2 = inline_vars(Arg1, Inline),
            {Arg3, HelperCh} = inline_helper_calls(Arg2, Module, FunDefs, Target),
            Arg4 = retransform_spliced(
                Arg3, map_size(Inline) > 0 orelse HelperCh, Module, FunDefs
            ),
            compile_template(Arg4, L, Module, true, target_backend(Target));
        _ ->
            transform_expr(Expr, Module, Inline, FunDefs)
    end.

%% Walk the tail (value-producing) positions of a control-flow expression,
%% applying `TailFun` at each tail leaf (a tail that is not itself control-flow)
%% and `NonTailFun` at every non-tail sub-expression: a `case` scrutinee, the
%% init statements before a body's last expression, a `receive` timeout, a `try`
%% `after` body. Tails that are themselves control-flow recurse; a
%% non-control-flow `Expr` is itself a tail leaf. This is the single definition
%% of "tail position" shared by the live-render-root transform
%% (transform_live_render_last/3) and the content-slot element expansion
%% (expand_block_element_tails/3). `try` body last is treated as a tail even with
%% `of` clauses (its value is then matched by `of`, but this mirrors the original
%% live-render behaviour and never matters for real templates).
map_tail_exprs(Expr, TailFun, NonTailFun) ->
    case erl_syntax:revert(Expr) of
        {'case', L, Scrutinee, Clauses} ->
            {'case', L, NonTailFun(Scrutinee), [
                map_tail_clause(C, TailFun, NonTailFun)
             || C <- Clauses
            ]};
        {'if', L, Clauses} ->
            {'if', L, [map_tail_clause(C, TailFun, NonTailFun) || C <- Clauses]};
        {block, L, Body} ->
            {block, L, map_tail_body(Body, TailFun, NonTailFun)};
        {'receive', L, Clauses} ->
            {'receive', L, [map_tail_clause(C, TailFun, NonTailFun) || C <- Clauses]};
        {'receive', L, Clauses, AfterExpr, AfterBody} ->
            {'receive', L, [map_tail_clause(C, TailFun, NonTailFun) || C <- Clauses],
                NonTailFun(AfterExpr), map_tail_body(AfterBody, TailFun, NonTailFun)};
        {'try', L, Body, OfClauses, CatchClauses, AfterBody} ->
            {'try', L, map_tail_body(Body, TailFun, NonTailFun),
                [map_tail_clause(C, TailFun, NonTailFun) || C <- OfClauses],
                [map_tail_clause(C, TailFun, NonTailFun) || C <- CatchClauses], [
                    NonTailFun(E)
                 || E <- AfterBody
                ]};
        {'maybe', L, Body} ->
            {'maybe', L, map_tail_body(Body, TailFun, NonTailFun)};
        {'maybe', L, Body, {'else', L2, ElseClauses}} ->
            {'maybe', L, map_tail_body(Body, TailFun, NonTailFun),
                {'else', L2, [map_tail_clause(C, TailFun, NonTailFun) || C <- ElseClauses]}};
        _ ->
            TailFun(Expr)
    end.

map_tail_clause({clause, L, Patterns, Guards, Body}, TailFun, NonTailFun) ->
    {clause, L, Patterns, Guards, map_tail_body(Body, TailFun, NonTailFun)}.

%% Only a body's last expression is a tail (recursing through map_tail_exprs/3 so
%% a control-flow last is walked too); the init statements are non-tail.
map_tail_body(Body, TailFun, NonTailFun) ->
    {Init, [Last]} = lists:split(length(Body) - 1, Body),
    [NonTailFun(E) || E <- Init] ++ [map_tail_exprs(Last, TailFun, NonTailFun)].

validate_live_root({tuple, _, [_Tag, Attrs | _]}, L, Inline) ->
    validate_id_expr(Attrs, L, Inline);
validate_live_root(_, L, _Inline) ->
    parse_error(live_render_not_single_element, L).

validate_id_expr({cons, _, {tuple, _, [{atom, _, id}, ValueAST]}, _}, L, Inline) ->
    case is_get_id_call(ValueAST, Inline) of
        true -> ok;
        false -> parse_error(live_render_id_must_be_get_id, L)
    end;
validate_id_expr({cons, _, _, Rest}, L, Inline) ->
    validate_id_expr(Rest, L, Inline);
validate_id_expr(_, L, _Inline) ->
    parse_error(live_render_missing_id, L).

%% A bare `?get(id)` at the root, or a variable hoisted into the body whose
%% definition resolves (through the inline map) to `get(id, _)`.
is_get_id_call(
    {call, _, {remote, _, {atom, _, Mod}, {atom, _, get}}, [{atom, _, id}, _]}, _Inline
) when
    Mod =:= arizona_template; Mod =:= az
->
    true;
is_get_id_call({var, _, V}, Inline) ->
    case Inline of
        #{V := RHS} -> is_get_id_call(RHS, Inline);
        #{} -> false
    end;
is_get_id_call(_, _Inline) ->
    false.

maybe_inject_or_raise_az_view(Attrs, Line, #state{live_render = true, root = true}) ->
    case lists:any(fun is_az_view_attr/1, Attrs) of
        true -> Attrs;
        false -> [{atom, Line, az_view} | Attrs]
    end;
maybe_inject_or_raise_az_view(Attrs, Line, _State) ->
    case lists:any(fun is_az_view_attr/1, Attrs) of
        true -> parse_error(az_view_not_allowed, Line);
        false -> Attrs
    end.

is_az_view_attr(Attr) ->
    case attr_name(Attr) of
        undefined -> false;
        Name -> framework_attr_name(Name) =:= ~"az-view"
    end.

%% --------------------------------------------------------------------
%% Binding-read inlining
%% --------------------------------------------------------------------
%%
%% A read hoisted into the function body --
%%
%%     Name = arizona_template:get(name, Bindings),
%%     ?html({p, [], [Name]}).
%%
%% -- would otherwise compile the slot to `fun() -> Name end`: a closure that
%% captures a plain value and runs no `get`, so it records no dependency and the
%% slot freezes after the first render. We rewrite each interpolated variable back
%% into its defining expression, so the `get` re-executes inside the per-slot
%% dependency bracket -- exactly as if it had been written inline in the template.

%% Build the inline map for a clause body: top-level `Var = RHS` matches whose RHS
%% transitively reaches an `arizona_template`/`az` `get`/`get_lazy`/`track`/`with` call.
%% Variables bound more than once are dropped (ambiguous to inline); a binding-derived
%% expression with no read (e.g. `Id = make_uuid()`) is left captured so a pure
%% side effect is never re-run per slot.
collect_inline(Body) ->
    {Raw, Poisoned} = scan_top_matches(Body, #{}, #{}),
    keep_reaching(maps:without(maps:keys(Poisoned), Raw)).

%% Post-process the collected inline map: an RHS containing a template
%% constructor (`?html`/`?native`/`?terminal`) is pre-inlined (in statement
%% order, so a reference to an earlier hoisted var resolves to its prepared
%% form) and wrapped with a track touch per literal binding key read inside the
%% constructor subtrees. The constructor compiles (retransform_spliced/4) to a
%% nested template whose inner reads are isolated from the enclosing slot's
%% dependency bracket (eval_template/2's with_saved_deps), so without the
%% touches the spliced template would render once and freeze -- the touches
%% record those reads as slot deps, mirroring the conditional branch-read
%% auto-tracking (branch_track_touches/1). Every other RHS is stored raw,
%% exactly as before. A fun-literal RHS is never wrapped: its reads fire
%% wherever the fun is called, and a wrap would break splicing it as an ?each
%% callback.
prepare_inline(Raw, _Body) when map_size(Raw) =:= 0 ->
    Raw;
prepare_inline(Raw, Body) ->
    lists:foldl(fun(Stmt, Acc) -> prepare_inline_stmt(Stmt, Raw, Acc) end, #{}, Body).

prepare_inline_stmt({match, _, {var, _, V}, _RHS}, Raw, Acc) ->
    case Raw of
        #{V := RHS} -> Acc#{V => prepare_inline_rhs(RHS, Acc)};
        #{} -> Acc
    end;
prepare_inline_stmt(_Stmt, _Raw, Acc) ->
    Acc.

prepare_inline_rhs({'fun', _, _} = RHS, _Prepared) ->
    RHS;
prepare_inline_rhs({named_fun, _, _, _} = RHS, _Prepared) ->
    RHS;
prepare_inline_rhs(RHS0, Prepared) ->
    case rhs_has_template_ctor(RHS0) of
        false ->
            RHS0;
        true ->
            RHS = inline_vars(RHS0, Prepared),
            track_wrap(RHS, dedup_keys(ctor_read_keys(RHS, [])))
    end.

%% Does the AST contain a template-constructor call that compiles to a
%% template-map literal? (`?each`/`?stateless` are deliberately not listed:
%% their spliced calls evaluate inside the slot bracket, so their reads track
%% without a wrap.)
rhs_has_template_ctor({call, _, {remote, _, {atom, _, Mod}, {atom, _, F}}, _Args}) when
    (Mod =:= arizona_template orelse Mod =:= az) andalso
        (F =:= html orelse F =:= native orelse F =:= terminal)
->
    true;
rhs_has_template_ctor(T) when is_tuple(T) ->
    rhs_has_template_ctor_any(tuple_to_list(T));
rhs_has_template_ctor(L) when is_list(L) ->
    rhs_has_template_ctor_any(L);
rhs_has_template_ctor(_Other) ->
    false.

rhs_has_template_ctor_any([]) ->
    false;
rhs_has_template_ctor_any([H | T]) ->
    rhs_has_template_ctor(H) orelse rhs_has_template_ctor_any(T).

%% Literal binding keys read anywhere inside the template-constructor subtrees
%% of an RHS -- the reads a spliced compiled template isolates from the slot.
ctor_read_keys({call, _, {remote, _, {atom, _, Mod}, {atom, _, F}}, _Args} = Call, Acc) when
    (Mod =:= arizona_template orelse Mod =:= az) andalso
        (F =:= html orelse F =:= native orelse F =:= terminal)
->
    collect_read_keys(Call, Acc);
ctor_read_keys(T, Acc) when is_tuple(T) ->
    ctor_read_keys(tuple_to_list(T), Acc);
ctor_read_keys([H | T], Acc) ->
    ctor_read_keys(T, ctor_read_keys(H, Acc));
ctor_read_keys(_Other, Acc) ->
    Acc.

track_wrap(Expr, []) ->
    Expr;
track_wrap(Expr, Keys) ->
    {block, 0, [track_call_ast(K) || K <- Keys] ++ [Expr]}.

scan_top_matches([], Raw, Poisoned) ->
    {Raw, Poisoned};
scan_top_matches([{match, _, {var, _, V}, RHS} | Rest], Raw, Poisoned) ->
    case Raw of
        #{V := _} -> scan_top_matches(Rest, Raw, Poisoned#{V => true});
        #{} -> scan_top_matches(Rest, Raw#{V => RHS}, Poisoned)
    end;
scan_top_matches([_ | Rest], Raw, Poisoned) ->
    scan_top_matches(Rest, Raw, Poisoned).

keep_reaching(Candidates) ->
    maps:with(maps:keys(reaching_fixpoint(Candidates, #{})), Candidates).

reaching_fixpoint(Candidates, Acc) ->
    Acc1 = maps:fold(
        fun
            (V, _RHS, A) when is_map_key(V, A) -> A;
            (V, RHS, A) ->
                case rhs_reaches(RHS, A) of
                    true -> A#{V => true};
                    false -> A
                end
        end,
        Acc,
        Candidates
    ),
    case map_size(Acc1) =:= map_size(Acc) of
        true -> Acc1;
        false -> reaching_fixpoint(Candidates, Acc1)
    end.

%% True when an AST subtree contains a get/get_lazy/track/with call, or references a
%% variable already known to reach one. `with` counts: like `get`, it calls `track/1`,
%% so a hoisted `Sub = with(Keys, Bindings)` must be inlined back into the slot bracket
%% or its tracking runs outside any bracket (a no-op) and the slot freezes.
rhs_reaches({call, _, {remote, _, {atom, _, Mod}, {atom, _, F}}, _Args}, _Reaching) when
    (Mod =:= arizona_template orelse Mod =:= az) andalso
        (F =:= get orelse F =:= get_lazy orelse F =:= track orelse F =:= with)
->
    true;
rhs_reaches({var, _, V}, Reaching) ->
    is_map_key(V, Reaching);
rhs_reaches(T, Reaching) when is_tuple(T) ->
    rhs_reaches_any(tuple_to_list(T), Reaching);
rhs_reaches(L, Reaching) when is_list(L) ->
    rhs_reaches_any(L, Reaching);
rhs_reaches(_, _Reaching) ->
    false.

rhs_reaches_any([], _Reaching) ->
    false;
rhs_reaches_any([H | T], Reaching) ->
    rhs_reaches(H, Reaching) orelse rhs_reaches_any(T, Reaching).

%% Lift a statement-form `case` that binds one variable as the whole body of every
%% branch --
%%
%%     case ?get(mode) of dark -> X = ?get(a); _ -> X = ?get(b) end,
%%
%% -- into value form --
%%
%%     X = case ?get(mode) of dark -> ?get(a); _ -> ?get(b) end,
%%
%% so the existing top-level-match machinery can inline it. Restricted to clauses
%% whose body is exactly a single `Var = E` match (no other bindings to strip out of
%% scope), and only when the lifted expression actually reaches a read.
%%
%% `if`/`receive` are deliberately excluded: their conditions are guards, which
%% cannot hold a read, so a binding-derived condition would stay captured and the
%% slot would track only the branch bodies -- partial tracking that looks correct.
%% A `case` scrutinee is an expression and inlines fully, so it is always sound.
normalize_tail_binds(Body) ->
    [normalize_tail_bind(Stmt) || Stmt <- Body].

normalize_tail_bind({'case', L, Scrutinee, Clauses} = Stmt) ->
    lift_tail_bind(Stmt, L, Clauses, fun(Stripped) -> {'case', L, Scrutinee, Stripped} end);
normalize_tail_bind(Stmt) ->
    Stmt.

%% `strip_tail_binds/1` is only safe once `tail_bind_var/1` has confirmed every
%% clause body is a single `Var = E` match, so it is computed lazily here.
lift_tail_bind(Stmt, L, Clauses, Rebuild) ->
    case tail_bind_var(Clauses) of
        {ok, V} ->
            Lifted = Rebuild(strip_tail_binds(Clauses)),
            case rhs_reaches(Lifted, #{}) of
                true -> {match, L, {var, L, V}, Lifted};
                false -> Stmt
            end;
        error ->
            Stmt
    end.

%% `{ok, V}` when every clause's body is exactly `[{match, {var, V}, _}]` for the
%% same `V`; `error` otherwise.
tail_bind_var([First | _] = Clauses) ->
    case clause_bind_var(First) of
        {ok, V} ->
            case lists:all(fun(C) -> clause_bind_var(C) =:= {ok, V} end, Clauses) of
                true -> {ok, V};
                false -> error
            end;
        error ->
            error
    end;
tail_bind_var([]) ->
    error.

%% A clause qualifies only when lifting cannot move a binding out of scope. The
%% lifted value-form case is ALSO substituted into the slot closure, so any variable
%% a clause introduces would be bound twice. Refuse if the clause pattern binds a
%% variable (e.g. `{admin, Name} ->` -- would become an unsafe_var) or the branch
%% RHS contains a nested match (e.g. `X = (Z = E)` -- would export `Z` from both
%% copies). Such cases are left in statement form (captured, not fine-grained).
clause_bind_var({clause, _, Patterns, _Guards, [{match, _, {var, _, V}, E}]}) ->
    case pattern_vars(Patterns) =:= [] andalso not contains_match(E) of
        true -> {ok, V};
        false -> error
    end;
clause_bind_var(_) ->
    error.

contains_match({match, _, _, _}) ->
    true;
contains_match(T) when is_tuple(T) ->
    contains_match(tuple_to_list(T));
contains_match([H | T]) ->
    contains_match(H) orelse contains_match(T);
contains_match(_) ->
    false.

strip_tail_binds(Clauses) ->
    [strip_tail_bind(C) || C <- Clauses].

strip_tail_bind({clause, L, Patterns, Guards, [{match, _, {var, _, _V}, E}]}) ->
    {clause, L, Patterns, Guards, [E]}.

%% Recursively substitute interpolated variables with their inlined defining
%% expression. Scope-aware: variables shadowed by fun parameters or comprehension
%% generators are not substituted, and patterns/guards are left untouched so a
%% substitution can never produce an illegal pattern.
inline_vars(Expr, Inline) when map_size(Inline) =:= 0 ->
    Expr;
%% A bare top-level fun is an ?each callback (compile_each is the only caller that passes a
%% fun straight to inline_vars); inline its clauses but DON'T wrap it -- compile_each needs a
%% fun literal, not a block. A fun NESTED in a content slot is wrapped by iv/2 below.
inline_vars({'fun', L, {clauses, Cs}}, Inline) ->
    {'fun', L, {clauses, [iv_fun_clause(C, Inline) || C <- Cs]}};
inline_vars({named_fun, L, Name, Cs}, Inline) ->
    Inline1 = maps:remove(Name, Inline),
    {named_fun, L, Name, [iv_fun_clause(C, Inline1) || C <- Cs]};
inline_vars(Expr, Inline) ->
    iv(Expr, Inline).

iv({var, _, V} = Var, Inline) ->
    case Inline of
        #{V := RHS} -> iv(RHS, Inline);
        #{} -> Var
    end;
iv({'fun', L, {clauses, Cs}}, Inline) ->
    iv_fun(L, Cs, Inline);
iv({named_fun, L, Name, Cs}, Inline) ->
    iv_named_fun(L, Name, Cs, Inline);
iv({'case', L, E, Cs}, Inline) ->
    iv_case(L, E, Cs, Inline);
iv({'if', L, Cs}, Inline) ->
    iv_if(L, Cs, Inline);
iv({'receive', L, Cs}, Inline) ->
    iv_receive(L, Cs, Inline);
iv({'receive', L, Cs, AE, AB}, Inline) ->
    iv_receive(L, Cs, AE, AB, Inline);
iv({'try', L, B, OfCs, CatchCs, Aft}, Inline) ->
    iv_try(L, B, OfCs, CatchCs, Aft, Inline);
iv({'catch', L, E}, Inline) ->
    {'catch', L, iv(E, Inline)};
iv({Comp, L, T, Qs}, Inline) when Comp =:= lc; Comp =:= bc; Comp =:= mc ->
    {Qs1, Inline1} = iv_quals(Qs, Inline),
    {Comp, L, iv(T, Inline1), Qs1};
iv({block, L, B}, Inline) ->
    {block, L, iv_body(B, Inline)};
iv({match, L, P, E}, Inline) ->
    {match, L, P, iv(E, Inline)};
iv({'maybe', L, B}, Inline) ->
    {'maybe', L, iv_body(B, Inline)};
iv({'maybe', L, B, {'else', L2, Cs}}, Inline) ->
    iv_maybe_else(L, B, L2, Cs, Inline);
iv({maybe_match, L, P, E}, Inline) ->
    {maybe_match, L, P, iv(E, Inline)};
iv(T, Inline) when is_tuple(T) ->
    list_to_tuple([iv(E, Inline) || E <- tuple_to_list(T)]);
iv(L, Inline) when is_list(L) ->
    [iv(E, Inline) || E <- L];
iv(Other, _Inline) ->
    Other.

iv_clauses(Cs, Inline) ->
    [iv_clause(C, Inline) || C <- Cs].

iv_fun_clauses(Cs, Inline) ->
    [iv_fun_clause(C, Inline) || C <- Cs].

%% Guard-bearing forms build their node and wrap it (wrap_guard_touches/4) so a tracked
%% binding read in a clause guard is recorded as a slot dependency. Kept as separate
%% helpers so iv/2 stays a thin dispatcher.
iv_fun(L, Cs, Inline) ->
    wrap_guard_touches({'fun', L, {clauses, iv_fun_clauses(Cs, Inline)}}, L, Cs, Inline).

iv_named_fun(L, Name, Cs, Inline) ->
    Inline1 = maps:remove(Name, Inline),
    wrap_guard_touches({named_fun, L, Name, iv_fun_clauses(Cs, Inline1)}, L, Cs, Inline1).

iv_case(L, E, Cs, Inline) ->
    wrap_guard_touches({'case', L, iv(E, Inline), iv_clauses(Cs, Inline)}, L, Cs, Inline).

iv_if(L, Cs, Inline) ->
    wrap_guard_touches({'if', L, iv_clauses(Cs, Inline)}, L, Cs, Inline).

iv_receive(L, Cs, Inline) ->
    wrap_guard_touches({'receive', L, iv_clauses(Cs, Inline)}, L, Cs, Inline).

iv_receive(L, Cs, AE, AB, Inline) ->
    Node = {'receive', L, iv_clauses(Cs, Inline), iv(AE, Inline), iv_body(AB, Inline)},
    wrap_guard_touches(Node, L, Cs, Inline).

iv_try(L, B, OfCs, CatchCs, Aft, Inline) ->
    Node =
        {'try', L, iv_body(B, Inline), iv_clauses(OfCs, Inline), iv_clauses(CatchCs, Inline),
            iv_body(Aft, Inline)},
    wrap_guard_touches(Node, L, OfCs ++ CatchCs, Inline).

iv_maybe_else(L, B, L2, Cs, Inline) ->
    Node = {'maybe', L, iv_body(B, Inline), {'else', L2, iv_clauses(Cs, Inline)}},
    wrap_guard_touches(Node, L, Cs, Inline).

%% Fun clause: parameters bind and shadow; drop them from the map for the body. Guards
%% stay untouched (a binding read can't live in a guard); a tracked binding read in a
%% nested fun's guard is auto-tracked by the enclosing fun node (wrap_guard_touches/4 in
%% iv/2). A top-level ?each callback fun is inlined through here too but not wrapped (see
%% inline_vars/2: compile_each needs a single-clause fun, whose guards are over the item
%% param, not outer tracked vars).
iv_fun_clause({clause, L, Params, Guards, Body}, Inline) ->
    Inline1 = maps:without(pattern_vars(Params), Inline),
    {clause, L, Params, Guards, iv_body(Body, Inline1)}.

%% case/receive/try-of/catch clause: patterns match the (already inlined) scrutinee,
%% so patterns and guards are left untouched. Any name a pattern binds is dropped from
%% the map for the body as a conservative shadow guard. A tracked binding read in a guard
%% is handled by the enclosing node via wrap_guard_touches/4, not here.
iv_clause({clause, L, Patterns, Guards, Body}, Inline) ->
    Inline1 = maps:without(pattern_vars(Patterns), Inline),
    {clause, L, Patterns, Guards, iv_body(Body, Inline1)}.

%% A binding read (`?get`/`get_lazy`/`with`-derived, hence in the inline map) cannot live
%% in a guard -- Erlang forbids a function call there -- so guards are left as bound
%% variables. The read then never re-runs inside the slot's dependency bracket, and the
%% slot would silently freeze on that binding. To keep the slot reactive, wrap the
%% guard-bearing expression in a block that first reads (for the `track/1` side effect)
%% each tracked binding its guards reference, recording them as slot dependencies. The
%% guard keeps using the captured value, which each diff cycle rebuilds from the current
%% bindings, so a change to a guard binding re-renders the slot. No tracked guard var ->
%% node returned unchanged.
wrap_guard_touches(Node, L, Clauses, Inline) ->
    case guard_tracked_vars(Clauses, Inline) of
        [] -> Node;
        Vars -> {block, L, [guard_touch(V, L, Inline) || V <- Vars] ++ [Node]}
    end.

%% Read V's inlined definition (its get/get_lazy/with call) for the track side effect,
%% discarding the value. Reusing the inline expansion handles get/get_lazy/with and
%% transitively-derived vars uniformly without extracting the binding key.
guard_touch(V, L, Inline) ->
    {match, L, {var, L, '_'}, iv({var, L, V}, Inline)}.

%% Union (first-seen order) of inline-map variables referenced in any clause guard,
%% respecting each clause's own pattern/parameter shadowing.
guard_tracked_vars(Clauses, Inline) ->
    lists:reverse(
        lists:foldl(
            fun({clause, _, Patterns, Guards, _}, Acc) ->
                ClauseInline = maps:without(pattern_vars(Patterns), Inline),
                collect_guard_vars(Guards, ClauseInline, Acc)
            end,
            [],
            Clauses
        )
    ).

%% Depth-first collect of inline-map variable names in a guard AST (they may be nested in
%% `andalso`/`is_binary(...)`/comparisons). The `{var, _, _}` clause is matched ahead of
%% the generic tuple walk so a var node is never decomposed.
collect_guard_vars({var, _, V}, Inline, Acc) ->
    case is_map_key(V, Inline) andalso not lists:member(V, Acc) of
        true -> [V | Acc];
        false -> Acc
    end;
collect_guard_vars(T, Inline, Acc) when is_tuple(T) ->
    collect_guard_vars(tuple_to_list(T), Inline, Acc);
collect_guard_vars([H | T], Inline, Acc) ->
    collect_guard_vars(T, Inline, collect_guard_vars(H, Inline, Acc));
collect_guard_vars(_Other, _Inline, Acc) ->
    Acc.

%% A body is a sequence; a `Var = RHS` match binds Var (shadowing) for later exprs.
iv_body(Exprs, Inline) ->
    {Rev, _} = lists:foldl(
        fun(E, {Acc, Inl}) ->
            Inl1 =
                case E of
                    {match, _, P, _} -> maps:without(pattern_vars(P), Inl);
                    _ -> Inl
                end,
            {[iv(E, Inl) | Acc], Inl1}
        end,
        {[], Inline},
        Exprs
    ),
    lists:reverse(Rev).

%% Comprehension qualifiers, left to right: generator patterns bind for subsequent
%% qualifiers and the template; filters are plain expressions.
iv_quals(Qs, Inline) ->
    lists:mapfoldl(fun iv_qual/2, Inline, Qs).

iv_qual({generate, L, P, E}, Inline) ->
    {{generate, L, P, iv(E, Inline)}, maps:without(pattern_vars(P), Inline)};
iv_qual({generate_strict, L, P, E}, Inline) ->
    {{generate_strict, L, P, iv(E, Inline)}, maps:without(pattern_vars(P), Inline)};
iv_qual({b_generate, L, P, E}, Inline) ->
    {{b_generate, L, P, iv(E, Inline)}, maps:without(pattern_vars(P), Inline)};
iv_qual({b_generate_strict, L, P, E}, Inline) ->
    {{b_generate_strict, L, P, iv(E, Inline)}, maps:without(pattern_vars(P), Inline)};
iv_qual({m_generate, L, P, E}, Inline) ->
    {{m_generate, L, P, iv(E, Inline)}, maps:without(pattern_vars(P), Inline)};
iv_qual({m_generate_strict, L, P, E}, Inline) ->
    {{m_generate_strict, L, P, iv(E, Inline)}, maps:without(pattern_vars(P), Inline)};
iv_qual(Filter, Inline) ->
    {iv(Filter, Inline), Inline}.

pattern_vars(Pattern) ->
    maps:keys(collect_pattern_vars(Pattern, #{})).

collect_pattern_vars({var, _, '_'}, Acc) ->
    Acc;
collect_pattern_vars({var, _, V}, Acc) ->
    Acc#{V => true};
collect_pattern_vars(T, Acc) when is_tuple(T) ->
    collect_pattern_vars(tuple_to_list(T), Acc);
collect_pattern_vars([H | T], Acc) ->
    collect_pattern_vars(T, collect_pattern_vars(H, Acc));
collect_pattern_vars(_, Acc) ->
    Acc.

%% After inlining, a variable used only inside the template no longer appears outside
%% its own binding. Underscore-prefix such matches so `warnings_as_errors` (unused
%% variable) doesn't reject the module; matches still referenced elsewhere are kept.
suppress_unused_inline_matches(Body, Inline) when map_size(Inline) =:= 0 ->
    Body;
suppress_unused_inline_matches(Body, Inline) ->
    [maybe_underscore_match(E, Inline, Body) || E <- Body].

%% Rename the now-unused match LHS to the anonymous `_`: it never collides with a
%% pre-existing `_Foo` binding (which would otherwise trip erl_lint's
%% match_underscore_var) and never warns.
maybe_underscore_match({match, L, {var, VL, V}, RHS} = M, Inline, Body) ->
    case is_map_key(V, Inline) andalso count_var(V, Body) =:= 1 of
        true -> {match, L, {var, VL, '_'}, RHS};
        false -> M
    end;
maybe_underscore_match(E, _Inline, _Body) ->
    E.

count_var(V, AST) ->
    count_var(V, AST, 0).

count_var(V, {var, _, V}, N) ->
    N + 1;
count_var(_V, {var, _, _}, N) ->
    N;
count_var(V, T, N) when is_tuple(T) ->
    count_var(V, tuple_to_list(T), N);
count_var(V, [H | T], N) ->
    count_var(V, T, count_var(V, H, N));
count_var(_V, _Other, N) ->
    N.

compile_template(Arg, Line, Module, LiveRender, Backend) ->
    {Statics, DynASTs, Fingerprint, Opts0} = compile_body_parts(Arg, Module, LiveRender, Backend),
    Opts = Opts0#{backend => Backend},
    {S1, D1} = scope_az(Backend, Fingerprint, Statics, DynASTs),
    build_template_ast(Line, S1, D1, Fingerprint, Opts).

compile_each(FunAST, SourceAST, Line, Module, Backend, CCtx, FunDefs) ->
    case FunAST of
        {'fun', _, {clauses, [{clause, _, [ItemVar, KeyVar], Guards, Body}]}} ->
            compile_each_clause(
                stream, [ItemVar, KeyVar], Guards, Body, SourceAST, Line, Module, Backend, CCtx
            );
        {'fun', _, {clauses, [{clause, _, [ItemVar], Guards, Body}]}} ->
            compile_each_clause(
                list, [ItemVar], Guards, Body, SourceAST, Line, Module, Backend, CCtx
            );
        %% Local `fun Name/1` or `fun Name/2` ref: resolve its single clause and compile
        %% it exactly like an inline fun, so the same element-body validation runs. The
        %% looked-up clause is the original untransformed body, which is what the inline
        %% path expects. (Its now-orphaned definition is covered by the injected
        %% nowarn_unused_function / ignore_xref attributes.)
        {'fun', L, {function, Name, Arity}} when Arity =:= 1; Arity =:= 2 ->
            compile_named_each(Name, Arity, SourceAST, L, Module, Backend, CCtx, FunDefs);
        %% A local ref of any other arity isn't a valid callback (1 = list, 2 = stream/map).
        {'fun', L, {function, _Name, _Arity}} ->
            parse_error(invalid_each_fun, L);
        %% Same-module explicit ref `fun ?MODULE:Name/Arity` (literal module = this module):
        %% the body is visible here, so rewrite to the bare local form and re-dispatch -- it
        %% then behaves exactly like `fun Name/Arity` (resolve + inline, or the same
        %% arity/multi-clause/undefined errors).
        {'fun', L, {function, {atom, _, Module}, {atom, _, Name}, {integer, _, Arity}}} ->
            compile_each(
                {'fun', L, {function, Name, Arity}}, SourceAST, Line, Module, Backend, CCtx, FunDefs
            );
        %% A remote `fun Mod:Name/Arity` ref to another module: its body isn't visible at
        %% compile time, so it can't be inlined into the per-item template.
        {'fun', L, {function, _Mod, _Name, _Arity}} ->
            parse_error(each_remote_fun_ref, L);
        _ ->
            parse_error(invalid_each_fun, Line)
    end.

%% Resolve a local `Name/Arity` callback (from a bare `fun Name/Arity` or a same-module
%% `fun ?MODULE:Name/Arity`) to its single clause and compile it via the inline-fun path.
%% `L` is the fun-ref location, used for the error and the synthesized clause.
compile_named_each(Name, Arity, SourceAST, L, Module, Backend, CCtx, FunDefs) ->
    case FunDefs of
        #{{Name, Arity} := [{clause, CL, Vars, Guards, Body}]} ->
            %% FunDefs holds the ORIGINAL, untransformed clause. The inline-fun path
            %% reaches compile_each already processed -- the top-down mark_targets
            %% pre-pass has run and the bottom-up transform has reduced its nested
            %% ?html/?each macros to template maps. A resolved clause has seen neither,
            %% so a nested ?html/?each left in it would compile to a raw runtime stub
            %% call and crash at render (function_clause in arizona_template:each/2).
            %% Mirror the inline pipeline on the resolved clause: unwrap a whole-body
            %% wrapper (so a single-root item keeps positional diffing, exactly as
            %% unwrap_each_body does for an inline fun), run mark_targets with this
            %% each's render-target context, then the bottom-up transform.
            UnwrappedBody = unwrap_last_wrapper(Body, Backend:target()),
            Clause0 = {clause, CL, Vars, Guards, UnwrappedBody},
            Clause1 = rename_inlined_clause(Name, Arity, Clause0),
            FunAST0 = mark_targets({'fun', L, {clauses, [Clause1]}}, Backend:target()),
            {FunAST1, _HelperCh} = inline_helper_calls(
                FunAST0, Module, FunDefs, Backend:target()
            ),
            FunAST = transform_expr(FunAST1, Module, #{}, FunDefs),
            %% Stamp the each with the CALLEE's clause line, not the `?each` call
            %% site: the whole per-item template comes from that clause, so a crash
            %% in it reports a stack line inside the callee while `arizona_loc` named
            %% a different function entirely. The error page reconciles the two from
            %% the stack, but raw surfaces cannot -- the dev MCP's `render_component`
            %% catches with no stacktrace at all, so this line is the only one an
            %% agent ever sees. The source expression is unaffected: it evaluates in
            %% the enclosing slot's closure, which carries the call site's line.
            compile_each(FunAST, SourceAST, CL, Module, Backend, CCtx, FunDefs);
        #{{Name, Arity} := [_ | _]} ->
            parse_error(each_named_fun_multi_clause, L);
        #{} ->
            parse_error(each_named_fun_undefined, L)
    end.

%% Compile one inline `?each` callback clause (`Kind` = list | stream, from the arity) into
%% the iteration AST. The body's last expr may be a bare element/fragment (the common case),
%% or a whole-body backend wrapper (`?html`/`?native`/`?terminal`): raw (a named-fun ref
%% resolves to its untransformed clause) or already compiled to a template map (an inline fun,
%% compiled bottom-up before the enclosing each). Both wrapper forms reduce to the same
%% per-item template the bare element would build -- `?html` and `?each` share
%% `compile_body_parts`/`scope_az` with the same fingerprint. Anything else falls through to
%% `validate_each_body` (element path or reject).
compile_each_clause(Kind, Vars, Guards, Body, SourceAST, Line, Module, Backend, CCtx) ->
    {Prefix, LastExpr} = split_fun_body(Body),
    case each_body_unwrap(LastExpr, Backend) of
        {compiled, Map} ->
            build_each_from_compiled(Line, SourceAST, Vars, Guards, Prefix, Map);
        {element, ElemAST} ->
            Classification = classify_body(ElemAST),
            ok = validate_each_body(Kind, Classification, ElemAST),
            %% The per-item template compiles in the content context its `?each`
            %% sits in, so an element the backend classifies differently there
            %% (an SVG `<title>`) is treated the same as if it were written
            %% literally at that position.
            {Statics, DynASTs, Fingerprint, Opts0} = compile_body_parts(
                ElemAST, Module, false, Backend, CCtx
            ),
            Opts1 = Opts0#{backend => Backend},
            Opts = maybe_single_root_opt(Backend, Kind, Classification, Opts1),
            {S1, D1} = scope_az(Backend, Fingerprint, Statics, DynASTs),
            build_each_ast(Line, SourceAST, Vars, Guards, Prefix, S1, D1, Fingerprint, Opts)
    end.

%% A single-root list item (one top-level element per item, the `element_tuple`
%% classification) lets the diff address items by DOM-order position between the
%% slot's `<!--az:X-->...<!--/az-->` markers -- so a content change patches items
%% in place (?OP_LIST_PATCH) instead of re-rendering the whole list, which churns
%% childList and reverts an in-progress scroll on WebKit. Whether that op is usable
%% is the backend's call, asked at compile time via the `supports_list_patch/0`
%% renderer callback (the web client implements it; native/terminal don't, and keep
%% the wholesale re-render). Multi-root/fragment items have no unambiguous
%% per-position DOM node, so they are never flagged.
%%
%% The flag describes the item BODY, not the source kind, so a 2-arg callback gets
%% it too: that one template serves both a stream and a map, and a MAP source needs
%% it to patch positionally. A stream never reads it -- it keys items by `az-key`
%% and diffs through `diff_stream/4`, which has no positional walk to gate.
maybe_single_root_opt(Backend, _Kind, element_tuple, Opts) ->
    case Backend:supports_list_patch() of
        true -> Opts#{single_root => true};
        false -> Opts
    end;
maybe_single_root_opt(_Backend, _Kind, _Classification, Opts) ->
    Opts.

%% Classify an ?each callback's last expr. A whole-body backend wrapper call
%% (`?html`/`?native`/`?terminal` matching this each's `Backend`) unwraps to the element it
%% wraps, so the normal element path builds the per-item template. An already-compiled
%% template map literal (an inline wrapper, compiled bottom-up) is taken as the per-item
%% template directly. Anything else (a non-matching wrapper, a user map, a bare value) is
%% handed back as-is for the normal validation, which compiles a bare element or rejects.
each_body_unwrap(
    {call, _, {remote, _, {atom, _, Mod}, {atom, _, Fn}}, [Inner]} = Call, Backend
) when
    Mod =:= arizona_template; Mod =:= az
->
    Wrapper = Backend:target(),
    case Fn of
        Wrapper -> {element, Inner};
        _ -> {element, Call}
    end;
each_body_unwrap({map, _, Fields} = Map, _Backend) ->
    case is_compiled_template_map(Fields) of
        true -> {compiled, Map};
        false -> {element, Map}
    end;
each_body_unwrap(LastExpr, _Backend) ->
    {element, LastExpr}.

%% The single registry mapping a render-target name (`html`/`native`/`terminal`,
%% the `?html`/`?native`/`?terminal` macro atoms) to its renderer backend module.
%% This is the ONLY place the parse transform names a concrete backend module;
%% each backend's behaviour (`Backend:target/0`, `name/1`, `supports_local/0`, ...)
%% covers everything else.
target_backend(html) -> arizona_html;
target_backend(native) -> arizona_native;
target_backend(terminal) -> arizona_terminal.

%% The render target a `?each`/`?native_each`/`?terminal_each` macro compiles for.
each_target(each) -> html;
each_target(foreign_each) -> html;
each_target(native_each) -> native;
each_target(terminal_each) -> terminal.

%% The content context the marker was chosen for, inverting `each_marker/1` so
%% the per-item template compiles in the context its `?each` sits in.
each_content_ctx(foreign_each) -> foreign;
each_content_ctx(_Marker) -> html.

%% A compiled template map literal carries all three of the `s`/`d`/`f` assoc keys (from
%% build_template_ast). A user map or a ?stateful/?stateless descriptor (a runtime call, not
%% a map literal) does not, so they fall through to the normal reject.
is_compiled_template_map(Fields) ->
    Keys = [K || {map_field_assoc, _, {atom, _, K}, _} <:- Fields],
    lists:all(fun(Key) -> lists:member(Key, Keys) end, [s, d, f]).

%% Build the ?each iteration AST from an already-compiled template map (an inline
%% `?html`/`?native`/`?terminal` body). Mirror build_each_ast but reuse the map's prebuilt
%% assocs: keep `s`/`f`/opts verbatim, wrap the existing `d`-list (nullary closures capturing
%% the item vars) in the per-item fun, and add `t => 0`. Scoping/fingerprint were already
%% computed by compile_template from the same body the element path would use, so the result
%% matches the bare-element form.
build_each_from_compiled(Line, SourceAST, Vars, Guards, Prefix, {map, MapLine, Fields}) ->
    DListAST = template_map_field(d, Fields),
    Fingerprint = extract_binary_value(template_map_field(f, Fields)),
    {Vars1, Guards1, Prefix1, DListAST1} = rename_each_params(
        Fingerprint, Vars, Guards, Prefix, DListAST
    ),
    DFunAST =
        {'fun', Line, {clauses, [{clause, Line, Vars1, Guards1, Prefix1 ++ [DListAST1]}]}},
    TField = {map_field_assoc, MapLine, {atom, MapLine, t}, {integer, MapLine, 0}},
    NewFields = [TField | [set_template_map_d_field(Field, DFunAST) || Field <- Fields]],
    {call, Line, {remote, Line, {atom, Line, arizona_template}, {atom, Line, each}}, [
        SourceAST, {map, MapLine, NewFields}
    ]}.

template_map_field(Key, Fields) ->
    [Val] = [V || {map_field_assoc, _, {atom, _, K}, V} <:- Fields, K =:= Key],
    Val.

set_template_map_d_field({map_field_assoc, FL, {atom, AL, d}, _}, DFunAST) ->
    {map_field_assoc, FL, {atom, AL, d}, DFunAST};
set_template_map_d_field(Field, _DFunAST) ->
    Field.

%% An ?each callback must build a per-item template: its body must be an element, a list
%% of elements, a static/mixed fragment, or a whole-body `?html`/`?native`/`?terminal`
%% wrapper (unwrapped to its element before this check). A `text_dynamic` body (a bare value,
%% a runtime binary, a ?stateful/?stateless descriptor, or a case/if) compiles to one opaque
%% value slot that renders at SSR but loses per-item diffing (and a descriptor value crashes
%% on diff). Reject it at compile time. `Kind` (list | stream) is the source shape, inferred
%% from the callback arity (1-arg = list, 2-arg = stream/map): the error it raises tailors the
%% fix advice, since a list has a comprehension fallback and a stream does not.
validate_each_body(Kind, text_dynamic, LastExpr) ->
    parse_error(each_body_error(Kind), line(LastExpr));
validate_each_body(Kind, list_ast, LastExpr) ->
    %% A mixed-list fragment is fine UNLESS an item is a nested template (a transformed
    %% ?html/?native/?terminal map literal) or a ?stateful/?stateless descriptor: those land
    %% in a per-item value slot and crash on diff exactly like a bare body. (A component as
    %% an ?each item child is a known limitation -- it crashes on the per-item diff for now.)
    walk_each_list_items(Kind, LastExpr);
validate_each_body(_Kind, _Classification, _LastExpr) ->
    ok.

each_body_error(list) -> each_body_not_element;
each_body_error(stream) -> each_stream_body_not_element.

walk_each_list_items(Kind, {cons, _, Item, Tail}) ->
    case is_fragile_each_item(Item) of
        true -> parse_error(each_body_error(Kind), line(Item));
        false -> walk_each_list_items(Kind, Tail)
    end;
walk_each_list_items(_Kind, _Nil) ->
    ok.

%% A list item that compiles to a fragile per-item value slot (renders at SSR, crashes on
%% diff): a nested template (a transformed ?html/?native/?terminal map literal) or a
%% ?stateful/?stateless descriptor call.
is_fragile_each_item({map, _, _}) ->
    true;
is_fragile_each_item({call, _, {remote, _, {atom, _, Mod}, {atom, _, F}}, _Args}) ->
    (Mod =:= arizona_template orelse Mod =:= az) andalso (F =:= stateful orelse F =:= stateless);
is_fragile_each_item(_Item) ->
    false.

compile_body_parts(ExprAST, Module, LiveRender, Backend) ->
    compile_body_parts(ExprAST, Module, LiveRender, Backend, html).

compile_body_parts(ExprAST, Module, LiveRender, Backend, CCtx) ->
    compile_classified_body(classify_body(ExprAST), ExprAST, Module, LiveRender, Backend, CCtx).

classify_body(AST) ->
    case is_static_binary(AST) of
        true -> static_binary;
        false -> classify_complex_body(AST)
    end.

classify_complex_body(AST) ->
    case is_element_tuple(AST) of
        true -> element_tuple;
        false -> classify_list_body(AST)
    end.

classify_list_body(AST) ->
    case is_element_list(AST) of
        true -> element_list;
        false -> classify_other_body(AST)
    end.

classify_other_body(AST) ->
    case is_list_ast(AST) of
        true -> list_ast;
        false -> text_dynamic
    end.

compile_classified_body(static_binary, ExprAST, _Module, _LiveRender, _Backend, _CCtx) ->
    Statics = [[extract_binary_value(ExprAST)]],
    {Statics, [], generate_fingerprint(Statics), #{}};
compile_classified_body(element_tuple, ExprAST, Module, LiveRender, Backend, CCtx) ->
    compile_fragment_parts([ExprAST], Module, LiveRender, Backend, CCtx);
compile_classified_body(element_list, ExprAST, Module, LiveRender, Backend, CCtx) ->
    compile_fragment_parts(ast_list_to_list(ExprAST), Module, LiveRender, Backend, CCtx);
compile_classified_body(list_ast, ExprAST, Module, _LiveRender, Backend, CCtx) ->
    compile_mixed_items(ast_list_to_list(ExprAST), Module, Backend, CCtx);
compile_classified_body(text_dynamic, ExprAST, Module, _LiveRender, Backend, _CCtx) ->
    %% The slot's own markers anchor it (see below), but a marker is not an
    %% ELEMENT, so it still cannot carry an `az-local` attribute -- an orphaned
    %% ?local here stays a compile error.
    ok = reject_orphaned_local(ExprAST),
    %% A whole-template bare dynamic (`?html(case ...)`, `?html(?get(x))`, a
    %% root `?stateless`/`?stateful` descriptor) sits inside no element, so no
    %% content slot would otherwise anchor it: wrap the slot in its own
    %% text-slot markers, exactly like a content slot inside an element
    %% (emit_child_dynamic/4) and like a bare dynamic in a root fragment
    %% (compile_mixed_dynamic/3). Without the anchor, SSR renders no marker
    %% for the slot, so its diff ops (both branch toggles of a
    %% conditional-only child) target an az that exists nowhere in the DOM
    %% and the client drops them.
    Statics = [[{az_slot, ~"0"}], [Backend:text_slot_close()]],
    DynASTs = [make_esc_text_dynamic_ast(~"0", ExprAST, Module, line(ExprAST), Backend)],
    {Statics, DynASTs, generate_fingerprint(Statics), #{}}.

%% A ?local outside an element -- a whole template body, a fragment top-level
%% item, or a conditional-branch tail -- gets a slot but no enclosing element
%% to carry the `az-local` descriptor (only compile_element/5 injects it via
%% maybe_inject_local_descriptor/6). The client binds keys by scanning that
%% attribute, so the key would be unreachable and set/set_all silent no-ops.
%% Reject at compile time.
reject_orphaned_local(ExprAST) ->
    case is_local_marker(ExprAST) of
        true -> parse_error(local_orphaned, line(ExprAST));
        false -> ok
    end.

compile_fragment_parts(ElementASTs, Module, LiveRender, Backend, CCtx) ->
    Opts = prescan_directives(ElementASTs),
    State0 = #state{
        module = Module,
        nodiff = maps:is_key(diff, Opts),
        live_render = LiveRender,
        root = LiveRender,
        backend = Backend,
        content_ctx = CCtx
    },
    State1 = lists:foldl(
        fun(Elem, State) ->
            {Tag, Attrs0, Children, ElemLine} = extract_element(Elem),
            {Attrs, _ElemOpts} = extract_directives(Attrs0),
            compile_element(Tag, Attrs, Children, ElemLine, State)
        end,
        State0,
        ElementASTs
    ),
    {Statics, DynASTs} = finalize(State1),
    Fingerprint = generate_fingerprint(Statics),
    {Statics, DynASTs, Fingerprint, Opts}.

compile_mixed_items(Items, Module, Backend, CCtx) ->
    Opts = prescan_directives(Items),
    State0 = #state{
        module = Module,
        nodiff = maps:is_key(diff, Opts),
        backend = Backend,
        content_ctx = CCtx
    },
    State1 = lists:foldl(
        fun(Item, State) -> compile_mixed_item(Item, Module, State) end, State0, Items
    ),
    {Statics, DynASTs} = finalize(State1),
    Fingerprint = generate_fingerprint(Statics),
    {Statics, DynASTs, Fingerprint, Opts}.

compile_mixed_item(Item, Module, State) ->
    case is_static_binary(Item) of
        true -> buf_append(State, (State#state.backend):text_child(extract_binary_value(Item)));
        false -> compile_mixed_non_static(Item, Module, State)
    end.

compile_mixed_non_static(Item, Module, State) ->
    case is_element_tuple(Item) of
        true ->
            {Tag, Attrs0, Children, ElemLine} = extract_element(Item),
            {Attrs, _ElemOpts} = extract_directives(Attrs0),
            compile_element(Tag, Attrs, Children, ElemLine, State);
        false ->
            ok = reject_orphaned_local(Item),
            compile_mixed_dynamic(Item, Module, State)
    end.

compile_mixed_dynamic(Item, Module, #state{nodiff = true, backend = Backend} = State) ->
    flush(State, make_nodiff_dynamic_ast(Item, Module, line(Item), Backend));
compile_mixed_dynamic(Item, Module, #state{backend = Backend} = State0) ->
    %% A bare (non-element) dynamic at the fragment top level. Allocate it a
    %% unique az from the shared element counter and wrap it in text-slot markers,
    %% exactly like a content slot (emit_child_dynamic/4). A hardcoded, markerless
    %% `"0"` collided with the first element's az -- and its first content slot's
    %% marker az is also `"0"` (text_az(0, 0)) -- so an OP_TEXT for this value
    %% resolved to that element's slot and overwrote it, while this value, having
    %% no marker of its own, could not be patched and stayed stale.
    Az = integer_to_binary(State0#state.az),
    State1 = State0#state{az = State0#state.az + 1},
    State2 = buf_az_slot(State1, Az),
    DynAST = make_esc_text_dynamic_ast(Az, Item, Module, line(Item), Backend),
    State3 = flush(State2, DynAST),
    State3#state{buf = [Backend:text_slot_close()]}.

extract_element({tuple, _, [{atom, _, Tag}, AttrsAST, ChildrenAST]} = Node) ->
    case is_list_ast(AttrsAST) of
        true ->
            {Tag, ast_list_to_list(AttrsAST), normalize_children(ChildrenAST), line(Node)};
        false ->
            parse_error(invalid_element, line(Node))
    end;
extract_element({tuple, _, [{atom, _, Tag}, AttrsAST]} = Node) ->
    case is_list_ast(AttrsAST) of
        true ->
            {Tag, ast_list_to_list(AttrsAST), [], line(Node)};
        false ->
            parse_error(invalid_element, line(Node))
    end;
extract_element(Node) ->
    parse_error(invalid_element, line(Node)).

compile_element(Tag, Attrs0, Children, Line, State0) ->
    ok = reject_framework_attrs(Attrs0, Line),
    Backend = State0#state.backend,
    RawKind = Backend:raw_text_kind(Tag, State0#state.content_ctx),
    ChildCtx = Backend:content_context(Tag, State0#state.content_ctx),
    Attrs1 = maybe_inject_or_raise_az_view(Attrs0, Line, State0),
    Attrs = maybe_inject_local_descriptor(Backend, Attrs1, Children, RawKind, Line, State0),
    State1 = State0#state{root = false},
    %% A dynamic content slot inside a raw-text element is markerless/render-once
    %% (see emit_child_dynamic/4), so it never needs an element-level `az` target.
    %% Only dynamic *attributes* (still diffable) force one there.
    HasDyn =
        has_dynamic_attr(Attrs) orelse (RawKind =:= none andalso has_dynamic_child(Children)),
    {ElemAz, State2} =
        case HasDyn andalso (not State1#state.nodiff) of
            true -> {State1#state.az, State1#state{az = State1#state.az + 1}};
            false -> {none, State1}
        end,
    TagBin = Backend:name(Tag),
    State3 = buf_append(State2, emit_backend(fun() -> Backend:element_open(TagBin) end, Line)),
    State4 =
        case ElemAz of
            none ->
                State3;
            N ->
                buf_az_attr(State3, integer_to_binary(N))
        end,
    State5 = compile_attrs(Attrs, ElemAz, State4, Line),
    case Backend:is_void(Tag) andalso Children =/= [] of
        true -> parse_error({void_with_children, Tag}, Line);
        false -> ok
    end,
    case Backend:is_void(Tag) of
        true ->
            buf_append(State5, Backend:element_void_close());
        false ->
            State6 = buf_append(State5, Backend:element_open_end()),
            %% Scope the raw-text context to this element's children, then restore
            %% the parent's so a following sibling is not treated as raw text.
            State7 = compile_children(
                Children,
                ElemAz,
                State6#state{
                    raw_text_kind = RawKind, raw_text_tag = Tag, content_ctx = ChildCtx
                }
            ),
            State8 = buf_append(State7, Backend:element_close(TagBin)),
            State8#state{
                raw_text_kind = State0#state.raw_text_kind,
                raw_text_tag = State0#state.raw_text_tag,
                content_ctx = State0#state.content_ctx
            }
    end.

compile_attrs([], _ElemAz, State, _ElemLine) ->
    State;
compile_attrs([Attr | Rest], ElemAz, State0, ElemLine) ->
    State1 = compile_attr(Attr, ElemAz, State0, ElemLine),
    compile_attrs(Rest, ElemAz, State1, ElemLine).

compile_attr({bin, _, _} = Bin, _ElemAz, State0, ElemLine) ->
    Backend = State0#state.backend,
    NameBin = extract_binary_value(Bin),
    buf_append(State0, emit_backend(fun() -> Backend:attr_boolean(NameBin) end, ElemLine));
compile_attr({tuple, _, [NameAST, {atom, _, false}]}, _ElemAz, State0, _ElemLine) when
    element(1, NameAST) =:= atom; element(1, NameAST) =:= bin
->
    State0;
compile_attr({tuple, _, [NameAST, {atom, _, true}]}, _ElemAz, State0, ElemLine) when
    element(1, NameAST) =:= atom; element(1, NameAST) =:= bin
->
    Backend = State0#state.backend,
    NameBin = extract_attr_name(Backend, NameAST),
    buf_append(State0, emit_backend(fun() -> Backend:attr_boolean(NameBin) end, ElemLine));
compile_attr({tuple, _, [NameAST, ValueAST]}, ElemAz, State0, ElemLine) when
    element(1, NameAST) =:= atom; element(1, NameAST) =:= bin
->
    Backend = State0#state.backend,
    NameBin = extract_attr_name(Backend, NameAST),
    case is_static_binary(ValueAST) of
        true ->
            ValBin = extract_binary_value(ValueAST),
            buf_append(State0, emit_backend(fun() -> Backend:attr(NameBin, ValBin) end, ElemLine));
        false ->
            compile_dynamic_attr(Backend, NameBin, ValueAST, ElemAz, State0)
    end;
compile_attr({atom, _, Name}, _ElemAz, State0, ElemLine) ->
    Backend = State0#state.backend,
    NameBin = Backend:name(Name),
    buf_append(State0, emit_backend(fun() -> Backend:attr_boolean(NameBin) end, ElemLine));
compile_attr(Attr, _ElemAz, _State0, ElemLine) ->
    AttrLine =
        try
            line(Attr)
        catch
            _:_ -> ElemLine
        end,
    parse_error(invalid_attribute, AttrLine).

%% Run a backend byte-emitting callback, turning a backend's rejection --
%% `error({arizona_render_reject, Message})` -- into a line-accurate parse error
%% carrying the backend's message. Lets a render backend cleanly refuse what it
%% cannot express (the terminal target rejecting an unknown style atom, or an
%% element outside its vocabulary) instead of silently dropping or mis-rendering
%% it. Wraps element emission as well as attributes, so a backend can police its
%% tag vocabulary the same way it polices attribute names.
emit_backend(Fun, Line) ->
    try
        Fun()
    catch
        error:{arizona_render_reject, Message} ->
            parse_error({render_reject, Message}, Line)
    end.

%% Emit a dynamic attribute value: a folded arizona_js command becomes a static,
%% otherwise the backend bakes the name and the value flushes as a dynamic.
compile_dynamic_attr(Backend, NameBin, ValueAST, ElemAz, State0) ->
    case attr_local_spec(ValueAST, line(ValueAST)) of
        {whole, _LocalCall} ->
            compile_local_attr(Backend, NameBin, ValueAST, ElemAz, State0);
        {interp, LocalCall, Prefix, Suffix} ->
            compile_interp_local_attr(
                Backend, NameBin, LocalCall, Prefix, Suffix, ElemAz, State0
            );
        none ->
            compile_dynamic_attr_value(Backend, NameBin, ValueAST, ElemAz, State0)
    end.

compile_dynamic_attr_value(Backend, NameBin, ValueAST, ElemAz, State0) ->
    ValLine = line(ValueAST),
    case try_fold_arizona_js(ValueAST) of
        {ok, Cmd} ->
            buf_append(
                State0, emit_backend(fun() -> Backend:attr_command(NameBin, Cmd) end, ValLine)
            );
        error when State0#state.nodiff ->
            Module = State0#state.module,
            State1 = buf_append(
                State0, emit_backend(fun() -> Backend:attr_dyn_name(NameBin) end, ValLine)
            ),
            DynAST = make_nodiff_attr_dynamic_ast(
                NameBin, ValueAST, Module, line(ValueAST)
            ),
            flush(State1, DynAST);
        error ->
            Module = State0#state.module,
            State1 = buf_append(
                State0, emit_backend(fun() -> Backend:attr_dyn_name(NameBin) end, ValLine)
            ),
            AzBin = integer_to_binary(ElemAz),
            DynAST = make_attr_dynamic_ast(
                AzBin, NameBin, ValueAST, Module, line(ValueAST)
            ),
            flush(State1, DynAST)
    end.

%% Emit a client-owned attribute slot (`?local` in attribute-value position): a
%% dynamic whose evaluated value is the local-map (with target = {attr, Name}).
%% It is never the normal `{attr, Name, Fun}` shape -- that would store
%% `{attr, Name, Map}` and the per-dynamic `#{diff := false}` skip in arizona_diff
%% would not match it.
compile_local_attr(Backend, NameBin, ValueAST, ElemAz, State0) ->
    Module = State0#state.module,
    State1 = buf_append(State0, Backend:attr_dyn_name(NameBin)),
    AzBin = integer_to_binary(ElemAz),
    Expr = local_attr_expr_ast(NameBin, ValueAST),
    DynAST = make_text_dynamic_ast(AzBin, Expr, Module, line(ValueAST)),
    flush(State1, DynAST).

%% AST for `(arizona_template:local(Key, Init))#{target => {attr, Name}}` -- stamps
%% the attribute name/target onto the local-map the macro call produces.
local_attr_expr_ast(NameBin, LocalCallAST) ->
    TargetAST = {tuple, 0, [{atom, 0, attr}, ast_binary(NameBin)]},
    {map, 0, LocalCallAST, [{map_field_assoc, 0, {atom, 0, target}, TargetAST}]}.

%% Emit a client-owned attribute slot interpolated with static text, e.g.
%% `{class, [~"foo ", ?local(K, Init)]}`. Same bind-map shape as compile_local_attr,
%% but `v` is the composed value so SSR renders the full attribute; the client
%% stores the affixes (descriptor `ap`) to recompose on set / strip on read.
compile_interp_local_attr(Backend, NameBin, LocalCall, Prefix, Suffix, ElemAz, State0) ->
    Module = State0#state.module,
    State1 = buf_append(State0, Backend:attr_dyn_name(NameBin)),
    AzBin = integer_to_binary(ElemAz),
    Expr = local_attr_interp_expr_ast(NameBin, LocalCall, Prefix, Suffix),
    DynAST = make_text_dynamic_ast(AzBin, Expr, Module, line(LocalCall)),
    flush(State1, DynAST).

%% AST for `(arizona_template:local(Key, Init))#{target => {attr, Name},
%% v => [Prefix, arizona_template:to_bin(Init), Suffix]}`. to_bin wraps Init so a
%% non-binary init (number/atom) composes as its text -- a bare integer in an
%% iolist would otherwise be emitted as a byte, not its decimal digits.
local_attr_interp_expr_ast(NameBin, LocalCall, Prefix, Suffix) ->
    TargetAST = {tuple, 0, [{atom, 0, attr}, ast_binary(NameBin)]},
    InitBinAST =
        {call, 0, {remote, 0, {atom, 0, arizona_template}, {atom, 0, to_bin}}, [
            local_init(LocalCall)
        ]},
    VAST = ast_list([ast_binary(Prefix), InitBinAST, ast_binary(Suffix)]),
    {map, 0, LocalCall, [
        {map_field_assoc, 0, {atom, 0, target}, TargetAST},
        {map_field_assoc, 0, {atom, 0, v}, VAST}
    ]}.

local_init({call, _, _, [_KeyAST, InitAST]}) -> InitAST.

%% `?local` expands to `arizona_template:local/2`; `az:local/2` is the facade.
%% Recognize both so the macro and a direct facade call both work in templates.
is_local_marker(
    {call, _, {remote, _, {atom, _, Mod}, {atom, _, local}}, [_Key, _Init]}
) when Mod =:= arizona_template; Mod =:= az ->
    true;
is_local_marker(_) ->
    false.

%% Classify an attribute value for `?local`: a bare local call is whole-value;
%% a list mixing static binaries and exactly one local call is interpolated (the
%% statics before/after the local become the prefix/suffix). A list with multiple
%% locals, or a local mixed with a non-static (server-owned dynamic or nested
%% element), is a compile error.
attr_local_spec(ValueAST, Line) ->
    case is_local_marker(ValueAST) of
        true ->
            {whole, ValueAST};
        false ->
            case is_list_ast(ValueAST) of
                false ->
                    none;
                true ->
                    Elems = ast_list_to_list(ValueAST),
                    case [E || E <- Elems, is_local_marker(E)] of
                        [] -> none;
                        [_] -> attr_interp_spec(Elems, Line);
                        _ -> parse_error(local_attr_multiple, Line)
                    end
            end
    end.

attr_interp_spec(Elems, Line) ->
    {Before, [LocalCall | After]} = lists:splitwith(
        fun(E) -> not is_local_marker(E) end, Elems
    ),
    assert_all_static(Before ++ After, Line),
    Prefix = iolist_to_binary([extract_binary_value(E) || E <- Before]),
    Suffix = iolist_to_binary([extract_binary_value(E) || E <- After]),
    {interp, LocalCall, Prefix, Suffix}.

assert_all_static(Elems, Line) ->
    case lists:all(fun is_static_binary/1, Elems) of
        true -> ok;
        false -> parse_error(local_attr_mixed, Line)
    end.

local_key({call, _, _, [{atom, _, Atom}, _Init]}, _Line) ->
    atom_to_binary(Atom);
local_key({call, _, _, [KeyAST, _Init]}, Line) ->
    case is_static_binary(KeyAST) of
        true -> extract_binary_value(KeyAST);
        false -> parse_error(local_key_not_literal, Line)
    end.

%% Scan an element's attrs + direct children for ?local markers and, if any,
%% inject a static `az-local` descriptor attribute the client scans on the DOM.
%% The descriptor JSON (`#{a => #{AttrName => Key}, c => ContentKey}`) is emitted
%% as an ordinary static attribute value, so the backend's attr/2 HTML-escapes it
%% for the attribute context (the client reads it via getAttribute, which decodes
%% the entities back). No separate escape here -- that would double-escape.
maybe_inject_local_descriptor(Backend, Attrs, Children, RawKind, Line, State) ->
    AttrLocals = collect_attr_locals(Backend, Attrs, Line),
    AttrAffixes = collect_attr_affixes(Backend, Attrs, Line),
    ContentLocals = collect_content_locals(Children, Line),
    case map_size(AttrLocals) > 0 orelse map_size(ContentLocals) > 0 of
        false ->
            Attrs;
        true ->
            assert_content_local_not_raw_text(ContentLocals, RawKind, Line),
            assert_local_supported(Backend, State, Line),
            assert_no_key_reuse(AttrLocals, ContentLocals, Line),
            Desc = build_local_descriptor(AttrLocals, AttrAffixes, ContentLocals),
            Json = iolist_to_binary(json:encode(Desc)),
            [{tuple, 0, [ast_binary(~"az-local"), ast_binary(Json)]} | Attrs]
    end.

%% A content ?local under a raw-text element (`<script>`/`<style>`/`<textarea>`/
%% `<title>`) renders markerless -- raw-text content carries no `<!--az:...-->`
%% slot markers -- so the client's az-local scan can never resolve the slot and
%% the value silently never updates. Reject it at compile time. An *attribute*
%% ?local on the same element is unaffected (the attribute is not raw-text content).
assert_content_local_not_raw_text(ContentLocals, RawKind, Line) ->
    case map_size(ContentLocals) > 0 andalso RawKind =/= none of
        true -> parse_error(local_in_raw_text, Line);
        false -> ok
    end.

assert_local_supported(Backend, State, Line) ->
    case Backend:supports_local() of
        true -> ok;
        false -> parse_error(local_unsupported, Line)
    end,
    case State#state.nodiff of
        false -> ok;
        true -> parse_error(local_in_nodiff, Line)
    end.

%% A key must not bind both content and an attribute on one element: a single
%% `set` would write the value into the content AND the attribute, almost always
%% a mistake.
assert_no_key_reuse(AttrLocals, ContentLocals, Line) ->
    AttrKeys = maps:values(AttrLocals),
    case [K || K <- maps:values(ContentLocals), lists:member(K, AttrKeys)] of
        [] -> ok;
        _ -> parse_error(local_key_reused, Line)
    end.

build_local_descriptor(AttrLocals, AttrAffixes, ContentLocals) ->
    Desc0 =
        case map_size(AttrLocals) of
            0 -> #{};
            _ -> #{~"a" => AttrLocals}
        end,
    Desc1 =
        case map_size(AttrAffixes) of
            0 -> Desc0;
            _ -> Desc0#{~"ap" => AttrAffixes}
        end,
    case map_size(ContentLocals) of
        0 -> Desc1;
        _ -> Desc1#{~"c" => ContentLocals}
    end.

%% Both whole-value and interpolated attribute locals contribute `attrName => Key`
%% (homogeneous, so the key-reuse check stays simple); interpolated ones also add
%% their affixes via collect_attr_affixes/3.
collect_attr_locals(Backend, Attrs, Line) ->
    maps:from_list([
        {extract_attr_name(Backend, NameAST), local_key(local_call(Spec), Line)}
     || {tuple, _, [NameAST, ValueAST]} <- Attrs,
        Spec <- [attr_local_spec(ValueAST, Line)],
        Spec =/= none
    ]).

%% The static prefix/suffix around an interpolated attribute local, keyed by
%% attribute name -- the client recomposes `prefix ++ value ++ suffix` on set.
collect_attr_affixes(Backend, Attrs, Line) ->
    maps:from_list([
        {extract_attr_name(Backend, NameAST), [Prefix, Suffix]}
     || {tuple, _, [NameAST, ValueAST]} <- Attrs,
        {interp, _LC, Prefix, Suffix} <- [attr_local_spec(ValueAST, Line)]
    ]).

local_call({whole, LocalCall}) -> LocalCall;
local_call({interp, LocalCall, _Prefix, _Suffix}) -> LocalCall.

%% Collect each content `?local` keyed by its dynamic-text slot index -- the
%% suffix the client needs to reconstruct the slot's comment-marker az (see
%% the backend's `text_az/2`). The slot counter advances on every dynamic text
%% child (the same classification compile_child/4 uses), so static text and
%% nested elements don't consume a slot. No sole-child restriction: multiple
%% content locals, and locals mixed with other children, each get their own
%% marked slot.
collect_content_locals(Children, Line) ->
    collect_content_locals(Children, Line, 0, #{}).

collect_content_locals([], _Line, _Slot, Acc) ->
    Acc;
collect_content_locals([Child | Rest], Line, Slot, Acc) ->
    case is_dynamic_text_child(Child) of
        false ->
            collect_content_locals(Rest, Line, Slot, Acc);
        true ->
            Acc1 =
                case is_local_marker(Child) of
                    true -> Acc#{integer_to_binary(Slot) => local_key(Child, Line)};
                    false -> Acc
                end,
            collect_content_locals(Rest, Line, Slot + 1, Acc1)
    end.

%% A child gets its own text marker (and bumps the slot counter) iff it is
%% neither static text nor a nested element -- mirrors compile_child/4.
is_dynamic_text_child(Child) ->
    not is_static_binary(Child) andalso not is_element_tuple(Child).

compile_children(Children, ElemAz, State) ->
    compile_children(Children, ElemAz, State, 0, 0).

compile_children([], _ElemAz, State, _Slot, _Index) ->
    State;
compile_children([Child | Rest], ElemAz, State0, Slot, Index) ->
    State1 = maybe_children_sep(State0, Index),
    {State2, NextSlot} = compile_child(Child, ElemAz, State1, Slot),
    compile_children(Rest, ElemAz, State2, NextSlot, Index + 1).

%% Emit a separator before every child after the first. HTML uses an empty
%% separator (no-op); native emits a comma between JSON array elements.
maybe_children_sep(State, 0) ->
    State;
maybe_children_sep(State, _Index) ->
    buf_append(State, (State#state.backend):children_sep()).

compile_child(Child, ElemAz, State0, Slot) ->
    case is_static_binary(Child) of
        true ->
            Bin = (State0#state.backend):text_child(extract_binary_value(Child)),
            {buf_append(State0, Bin), Slot};
        false ->
            compile_non_static_child(Child, ElemAz, State0, Slot)
    end.

compile_non_static_child(Child, ElemAz, State0, Slot) ->
    case is_element_tuple(Child) of
        true ->
            {Tag, Attrs, Children, ElemLine} = extract_element(Child),
            {compile_element(Tag, Attrs, Children, ElemLine, State0), Slot};
        false ->
            compile_dynamic_child(Child, ElemAz, State0, Slot)
    end.

compile_dynamic_child(Child, ElemAz, State0, Slot) ->
    case is_invalid_static_child(Child) of
        true ->
            ValueStr = erl_pp:expr(Child),
            parse_error({invalid_child, ValueStr}, line(Child));
        false ->
            emit_child_dynamic(Child, ElemAz, State0, Slot)
    end.

%% The `raw` raw-text clause comes FIRST, ahead of nodiff: both make the slot
%% markerless and render-once (`undefined` az), so they agree on the diff
%% question, but only the raw-text path applies the escaping policy the *element*
%% demands. Matching nodiff first let an `az-nodiff` region -- a layout, which is
%% exactly where an inline `<script>` lives -- skip the opt-out guard and
%% HTML-escape inside raw text, so a marked value was spliced with no
%% neutralization and an unmarked one drew no compile error. The `escapable`
%% clause stays below nodiff because the two agree there: both HTML-escape the
%% scalar through make_nodiff_dynamic_ast/4.
emit_child_dynamic(
    Child,
    _ElemAz,
    #state{raw_text_kind = raw, raw_text_tag = Tag, module = Module, backend = Backend} = State0,
    Slot
) ->
    %% script/style: raw text, the browser decodes neither character references
    %% nor HTML comments here, so the value is emitted verbatim, markerless and
    %% render-once. Comment markers would become literal bytes and corrupt the
    %% script/CSS (and a module script's HTML-comment tokens are a SyntaxError).
    %% Diffing is impossible by construction (no marker to patch), so the slot
    %% renders once -- the diff engine skips its `undefined` az.
    ok = assert_raw_text_opt_out(Child),
    DynAST = make_raw_text_dynamic_ast(Child, Tag, Module, line(Child), Backend),
    {flush(State0, DynAST), Slot};
emit_child_dynamic(
    Child, _ElemAz, #state{nodiff = true, module = Module, backend = Backend} = State0, Slot
) ->
    DynAST = make_nodiff_dynamic_ast(Child, Module, line(Child), Backend),
    {flush(State0, DynAST), Slot};
emit_child_dynamic(
    Child,
    _ElemAz,
    #state{raw_text_kind = escapable, module = Module, backend = Backend} = State0,
    Slot
) ->
    %% textarea/title: escapable raw text, the browser DOES decode character
    %% references, so a scalar is HTML-escaped (make_nodiff_dynamic_ast's
    %% esc_spec), but comments are still literal -- so the slot is markerless and
    %% render-once, exactly like the layout/nodiff value path.
    DynAST = make_nodiff_dynamic_ast(Child, Module, line(Child), Backend),
    {flush(State0, DynAST), Slot};
emit_child_dynamic(Child, ElemAz, #state{module = Module, backend = Backend} = State0, Slot) ->
    ElemAzBin = integer_to_binary(ElemAz),
    MarkerAz = Backend:text_az(ElemAzBin, Slot),
    State1 = buf_az_slot(State0, MarkerAz),
    DynAST = make_esc_text_dynamic_ast(MarkerAz, Child, Module, line(Child), Backend),
    State2 = flush(State1, DynAST),
    {State2#state{buf = [Backend:text_slot_close()]}, Slot + 1}.

%% Verbatim is the whole point of a `raw` raw-text slot (script/style) and also its
%% danger: the browser decodes no character references there, so the escaping every
%% other slot kind applies is *impossible* -- the value reaches the output byte for
%% byte and can close the JS string it sits in, or the element. This is the only
%% content position where an unmarked value would be spliced unescaped, so require
%% the author to state the value is safe for the script/CSS context with a literal
%% `?raw(...)` at the slot -- the same "literal at the template site" rule the
%% escape opt-out already follows everywhere else. Static text children never reach
%% here (compile_child/4 splices them from the template's own bytes).
assert_raw_text_opt_out(Child) ->
    case is_raw_call(Child) of
        true -> ok;
        false -> parse_error(dynamic_in_raw_text, line(Child))
    end.

is_raw_call({call, _, {remote, _, {atom, _, Mod}, {atom, _, raw}}, [_Value]}) ->
    Mod =:= arizona_template orelse Mod =:= az;
is_raw_call(_Expr) ->
    false.

make_text_dynamic_ast(AzBin, ExprAST, Module, ExprLine) ->
    LocAST = loc_ast(Module, ExprLine),
    {tuple, 0, [
        ast_binary(AzBin),
        {'fun', 0, {clauses, [{clause, 0, [], [], [ExprAST]}]}},
        LocAST
    ]}.

%% Like make_text_dynamic_ast/4 but tags a *value* element-content interpolation
%% as `{esc, Fun}` so the renderer HTML-escapes it. A *block* -- a nested
%% template, ?each, ?inner_content, ?stateful/?stateless, a map, or raw/1 -- is
%% left untagged and rendered structurally (escaping is decided at compile time
%% so the runtime can never confuse user scalars with spliced framework HTML).
make_esc_text_dynamic_ast(AzBin, ExprAST0, Module, ExprLine, Backend) ->
    ExprAST = expand_block_element_tails(ExprAST0, Module, Backend),
    LocAST = loc_ast(Module, ExprLine),
    Body = branch_track_touches(ExprAST0) ++ [ExprAST],
    FunAST = {'fun', 0, {clauses, [{clause, 0, [], [], Body}]}},
    {tuple, 0, [ast_binary(AzBin), esc_spec(ExprAST, FunAST), LocAST]}.

%% A content-slot control-flow expression (`case`/`if`/`maybe`/...) compiles each
%% branch *element* into a nested template (expand_block_element_tails/3) whose `?get`
%% reads are isolated from the conditional dynamic's own dependency bracket -- see
%% eval_template/2's with_saved_deps. Left alone, the conditional slot records only
%% its scrutinee's reads, so a change to a binding read solely in such a branch is
%% skipped and the branch freezes. (A *value* branch does not freeze: its read fires
%% eagerly in the slot closure when that branch is taken, and a non-taken value
%% branch's read is genuinely not a dependency.) Mirror the guard auto-tracking
%% (wrap_guard_touches/4): prepend a `track/1` for each binding key read in a branch
%% tail that becomes a nested template, so those keys become deps of the conditional
%% dynamic itself. `track/1` records the key without reading the binding, so a key
%% present only in a non-taken (and possibly absent) branch never raises
%% missing_binding. The residual over-tracking (two element branches both
%% contribute their reads) is op-free: a change to a non-taken-branch read
%% re-evaluates the slot but the structurally equal snapshot emits no op
%% (diff_changed_dynamic/8's `case New of Old`). Computed (non-literal) keys are
%% skipped: the key expression may reference a clause-bound variable not in scope.
branch_track_touches(ExprAST0) ->
    Expr = erl_syntax:revert(ExprAST0),
    case is_control_flow_ast(Expr) of
        false ->
            %% A BARE nested-template leaf -- a literal element list (or mixed
            %% fragment) child, or a local helper's inlined element-list body --
            %% is compiled into a nested template by expand_element_leaf/3 with
            %% its reads equally isolated, so it needs the same touches or the
            %% slot freezes (a scalar or already-compiled expr contributes
            %% nothing and stays untouched).
            case is_nested_template_leaf(Expr) of
                true ->
                    Keys = dedup_keys(collect_read_keys(Expr, [])),
                    [track_call_ast(K) || K <- Keys];
                false ->
                    []
            end;
        true ->
            Keys = dedup_keys(collect_branch_keys(Expr, [])),
            [track_call_ast(K) || K <- Keys]
    end.

is_control_flow_ast({'case', _, _, _}) -> true;
is_control_flow_ast({'if', _, _}) -> true;
is_control_flow_ast({block, _, _}) -> true;
is_control_flow_ast({'receive', _, _}) -> true;
is_control_flow_ast({'receive', _, _, _, _}) -> true;
is_control_flow_ast({'try', _, _, _, _, _}) -> true;
is_control_flow_ast({'maybe', _, _}) -> true;
is_control_flow_ast({'maybe', _, _, _}) -> true;
is_control_flow_ast(_) -> false.

%% Walk only the value-producing tail leaves of a control-flow expression (the same
%% positions map_tail_exprs/3 compiles), collecting binding-read keys from each leaf
%% that expand_element_leaf/3 turns into a nested template. Those are the isolated
%% reads; a scalar leaf or the scrutinee already tracks when it runs, so they are
%% skipped (tracking them would spuriously widen a value-form conditional's deps).
collect_branch_keys(Expr, Acc) ->
    case erl_syntax:revert(Expr) of
        {'case', _, _Scrutinee, Clauses} ->
            collect_clauses_keys(Clauses, Acc);
        {'if', _, Clauses} ->
            collect_clauses_keys(Clauses, Acc);
        {block, _, Body} ->
            collect_tail_keys(Body, Acc);
        {'receive', _, Clauses} ->
            collect_clauses_keys(Clauses, Acc);
        {'receive', _, Clauses, _AfterExpr, AfterBody} ->
            collect_tail_keys(AfterBody, collect_clauses_keys(Clauses, Acc));
        {'try', _, Body, OfClauses, CatchClauses, _AfterBody} ->
            collect_clauses_keys(
                CatchClauses, collect_clauses_keys(OfClauses, collect_tail_keys(Body, Acc))
            );
        {'maybe', _, Body} ->
            collect_tail_keys(Body, Acc);
        {'maybe', _, Body, {'else', _, ElseClauses}} ->
            collect_clauses_keys(ElseClauses, collect_tail_keys(Body, Acc));
        Leaf ->
            collect_leaf_keys(Leaf, Acc)
    end.

collect_clauses_keys(Clauses, Acc) ->
    lists:foldl(
        fun({clause, _, _Patterns, _Guards, Body}, A) -> collect_tail_keys(Body, A) end,
        Acc,
        Clauses
    ).

%% Only a body's last expression is a value-producing tail.
collect_tail_keys(Body, Acc) ->
    collect_branch_keys(lists:last(Body), Acc).

%% A tail leaf contributes its reads only when expand_element_leaf/3 would compile it
%% into a nested template; otherwise its reads track eagerly when the branch runs.
collect_leaf_keys(Leaf, Acc) ->
    case is_nested_template_leaf(Leaf) of
        true -> collect_read_keys(Leaf, Acc);
        false -> Acc
    end.

is_nested_template_leaf(Leaf) ->
    case classify_body(Leaf) of
        element_tuple -> true;
        element_list -> true;
        list_ast -> list_has_element_tuple(Leaf);
        _ -> false
    end.

%% Collect (prepended) the literal key ASTs read via `arizona_template`/`az`
%% get/get_lazy/with/track anywhere in Expr. A matched read call still recurses into
%% its args (a default that itself reads, `get(a, B, get(b, B))`).
collect_read_keys({call, _, {remote, _, {atom, _, Mod}, {atom, _, Fun}}, Args}, Acc) when
    Mod =:= arizona_template orelse Mod =:= az
->
    collect_read_keys(Args, collect_call_keys(Fun, Args, Acc));
collect_read_keys(T, Acc) when is_tuple(T) ->
    collect_read_keys(tuple_to_list(T), Acc);
collect_read_keys([H | T], Acc) ->
    collect_read_keys(T, collect_read_keys(H, Acc));
collect_read_keys(_, Acc) ->
    Acc.

collect_call_keys(get, [Key | _], Acc) -> add_literal_key(Key, Acc);
collect_call_keys(get_lazy, [Key | _], Acc) -> add_literal_key(Key, Acc);
collect_call_keys(track, [Key | _], Acc) -> add_literal_key(Key, Acc);
collect_call_keys(with, [Keys | _], Acc) -> add_literal_keys(Keys, Acc);
collect_call_keys(_, _, Acc) -> Acc.

add_literal_key({atom, _, _} = Key, Acc) -> [Key | Acc];
add_literal_key({bin, _, _} = Key, Acc) -> [Key | Acc];
add_literal_key(_NonLiteral, Acc) -> Acc.

add_literal_keys({cons, _, Head, Tail}, Acc) ->
    add_literal_keys(Tail, add_literal_key(Head, Acc));
add_literal_keys(_NilOrNonLiteral, Acc) ->
    Acc.

%% Reverse the prepend order back to source order, dropping duplicates by key value.
dedup_keys(KeysRev) ->
    {Keys, _Seen} = lists:foldl(
        fun(Key, {Acc, Seen}) ->
            Value = key_value(Key),
            case is_map_key(Value, Seen) of
                true -> {Acc, Seen};
                false -> {[Key | Acc], Seen#{Value => true}}
            end
        end,
        {[], #{}},
        lists:reverse(KeysRev)
    ),
    lists:reverse(Keys).

key_value({atom, _, Atom}) -> {atom, Atom};
key_value({bin, _, [{bin_element, _, {string, _, Str}, default, default}]}) -> {bin, Str};
key_value({bin, _, _} = Bin) -> {raw, Bin}.

track_call_ast(KeyAST) ->
    {match, 0, {atom, 0, ok},
        {call, 0, {remote, 0, {atom, 0, arizona_template}, {atom, 0, track}}, [KeyAST]}}.

%% A content slot's value can be the result of a control-flow expression
%% (`case`/`if`/`begin`/`receive`/`try`/`maybe`). Those tail positions are
%% themselves content positions: an element tuple (or element list, or mixed
%% fragment) sitting in a tail is compiled into a nested template -- exactly as a
%% literal ?html(...) there would be -- so branches don't need an explicit ?html
%% wrap. The current Backend is threaded through, so a bare element under
%% ?native/?terminal inherits that target (mirroring how ?each does). Non-tail
%% sub-expressions and non-element tails are left untouched (a non-element tail
%% renders as an escaped scalar, as before). The set of walked forms is shared
%% with the live-render-root transform via map_tail_exprs/3.
expand_block_element_tails(Expr, Module, Backend) ->
    case is_local_marker(Expr) of
        true ->
            %% A direct ?local content child: a legit slot (the enclosing
            %% element injects the az-local descriptor) with no tails to walk.
            %% Any ?local reaching expand_element_leaf/3 below is therefore a
            %% conditional-branch one -- orphaned by construction.
            Expr;
        false ->
            map_tail_exprs(
                Expr,
                fun(Leaf) -> expand_element_leaf(Leaf, Module, Backend) end,
                fun(NonTail) -> NonTail end
            )
    end.

%% At a tail leaf, compile a bare element tuple / element list (or a mixed list
%% that contains an element tuple) into a nested template, as a literal ?html
%% there would; leave plain values (and pure value lists) untouched. A ?local
%% leaf is rejected: a conditional-branch ?local emits no az-local descriptor
%% (see reject_orphaned_local/1).
expand_element_leaf(Expr, Module, Backend) ->
    ok = reject_orphaned_local(Expr),
    case classify_body(Expr) of
        Class when Class =:= element_tuple; Class =:= element_list ->
            compile_template(Expr, line(Expr), Module, false, Backend);
        list_ast ->
            case list_has_element_tuple(Expr) of
                true -> compile_template(Expr, line(Expr), Module, false, Backend);
                false -> Expr
            end;
        _ ->
            Expr
    end.

list_has_element_tuple({cons, _, Head, Tail}) ->
    is_element_tuple(Head) orelse list_has_element_tuple(Tail);
list_has_element_tuple(_) ->
    false.

%% Tag a value interpolation `{esc, Fun}` so it is escaped at the render boundary,
%% or leave a block bare. Every backend marks scalar values uniformly -- the
%% backend's `arizona_renderer:escape/1` is the sole escaping authority (HTML
%% entity-escapes, the terminal sanitizes control bytes, the native JSON wire is
%% the identity), so the parse transform never special-cases a backend here.
%% Blocks (nested templates, descriptors, `raw/1`, effects) are always left bare
%% and spliced structurally.
esc_spec(ExprAST, FunAST) ->
    case is_block_content_expr(ExprAST) of
        true -> FunAST;
        false -> {tuple, 0, [{atom, 0, esc}, FunAST]}
    end.

is_block_content_expr({map, _, _}) ->
    true;
is_block_content_expr({map, _, _, _}) ->
    true;
is_block_content_expr({call, _, {remote, _, {atom, _, M}, {atom, _, F}}, _Args}) ->
    (M =:= arizona_template orelse M =:= az) andalso
        lists:member(F, [
            html,
            native,
            terminal,
            each,
            native_each,
            terminal_each,
            stateful,
            stateless,
            inner_content,
            local,
            raw
        ]);
is_block_content_expr(_) ->
    false.

make_attr_dynamic_ast(AzBin, AttrNameBin, ExprAST, Module, ExprLine) ->
    LocAST = loc_ast(Module, ExprLine),
    {tuple, 0, [
        ast_binary(AzBin),
        {tuple, 0, [
            {atom, 0, attr},
            ast_binary(AttrNameBin),
            {'fun', 0, {clauses, [{clause, 0, [], [], [ExprAST]}]}}
        ]},
        LocAST
    ]}.

%% Same value/block classification as make_esc_text_dynamic_ast/5, but for the
%% nodiff (layout) path: a value interpolation (e.g. a layout `title`) is tagged
%% `{esc, Fun}` so SSR HTML-escapes it; a block (?inner_content, nested template)
%% is left untagged and spliced raw.
make_nodiff_dynamic_ast(ExprAST0, Module, ExprLine, Backend) ->
    ExprAST = expand_block_element_tails(ExprAST0, Module, Backend),
    LocAST = loc_ast(Module, ExprLine),
    FunAST = {'fun', 0, {clauses, [{clause, 0, [], [], [ExprAST]}]}},
    {tuple, 0, [{atom, 0, undefined}, esc_spec(ExprAST, FunAST), LocAST]}.

%% Markerless render-once for a `raw` raw-text element (script/style). Unlike
%% make_nodiff_dynamic_ast/4 the value is left bare (never `{esc, Fun}`): the
%% browser does not decode character references inside these, so HTML-escaping a
%% scalar would corrupt it (`&` -> `&amp;`). `undefined` az makes it non-diffable
%% -- there is no comment marker to patch.
make_raw_text_dynamic_ast(ExprAST0, Tag, Module, ExprLine, Backend) ->
    ExprAST = expand_block_element_tails(ExprAST0, Module, Backend),
    LocAST = loc_ast(Module, ExprLine),
    %% Neutralize a tokenizer breakout (`</script>`, `<!--`) in the value: the
    %% content is emitted verbatim, so the backend that owns raw-text elements
    %% sanitizes it. The tag goes along because the sequences that break out are
    %% the enclosing element's, not raw text's in general.
    GuardedAST =
        {call, 0, {remote, 0, {atom, 0, Backend}, {atom, 0, raw_text}}, [
            {atom, 0, Tag}, ExprAST
        ]},
    FunAST = {'fun', 0, {clauses, [{clause, 0, [], [], [GuardedAST]}]}},
    {tuple, 0, [{atom, 0, undefined}, FunAST, LocAST]}.

make_nodiff_attr_dynamic_ast(AttrNameBin, ExprAST, Module, ExprLine) ->
    LocAST = loc_ast(Module, ExprLine),
    {tuple, 0, [
        {atom, 0, undefined},
        {tuple, 0, [
            {atom, 0, attr},
            ast_binary(AttrNameBin),
            {'fun', 0, {clauses, [{clause, 0, [], [], [ExprAST]}]}}
        ]},
        LocAST
    ]}.

loc_ast(Module, Line) ->
    {tuple, 0, [{atom, 0, Module}, {integer, 0, Line}]}.

buf_append(State, Bin) ->
    State#state{buf = [Bin | State#state.buf]}.

%% Record the framework's own `az` markers as tagged segments rather than bytes,
%% so scope_az/4 can rebuild them from the id alone.
buf_az_attr(State, Az) ->
    State#state{buf = [{az_attr, Az} | State#state.buf]}.

buf_az_slot(State, Az) ->
    State#state{buf = [{az_slot, Az} | State#state.buf]}.

flush(State, DynAST) ->
    State#state{
        statics = State#state.statics ++ [lists:reverse(State#state.buf)],
        dynamics = State#state.dynamics ++ [DynAST],
        buf = []
    }.

finalize(State) ->
    Statics = State#state.statics ++ [lists:reverse(State#state.buf)],
    Dynamics = State#state.dynamics,
    {Statics, Dynamics}.

%% Render a static's segments to bytes, each `az` marker built from `Prefix`
%% plus its id. `Prefix` is empty for the fingerprint pass (the fingerprint is
%% taken over the unscoped bytes) and `<Fp>-` for the emitted statics.
-spec render_static(module(), binary(), [segment()]) -> static().
render_static(Backend, Prefix, Segments) ->
    iolist_to_binary([render_segment(Backend, Prefix, Seg) || Seg <- Segments]).

render_segment(_Backend, _Prefix, Bin) when is_binary(Bin) ->
    Bin;
render_segment(Backend, Prefix, {az_attr, Az}) ->
    Backend:az_attr(<<Prefix/binary, Az/binary>>);
render_segment(Backend, Prefix, {az_slot, Az}) ->
    Backend:text_slot_open(<<Prefix/binary, Az/binary>>).

%% Prefix az values with the template fingerprint to prevent collisions
%% when stateless children are inlined in a parent template. The markers are
%% re-emitted from the tagged segments, so only ids the framework itself
%% allocated are prefixed -- literal bytes (a verbatim static text child, a
%% user-written `az` attribute) are copied through untouched.
scope_az(Backend, Fp, Statics, DynASTs) ->
    Prefix = <<Fp/binary, "-">>,
    {[render_static(Backend, Prefix, S) || S <- Statics], [
        scope_dynamic_ast(Fp, D)
     || D <- DynASTs
    ]}.

scope_dynamic_ast(_Fp, {tuple, _, [{atom, _, undefined} | _]} = D) ->
    D;
scope_dynamic_ast(Fp, {tuple, L, [AzAST | Rest]}) ->
    AzBin = extract_binary_value(AzAST),
    {tuple, L, [ast_binary(<<Fp/binary, "-", AzBin/binary>>) | Rest]}.

%% Hashes the template's shape over its `Statics` *segments* (`binary()` literals
%% interleaved with `{az_attr, _}` / `{az_slot, _}` markers), not their rendered
%% bytes. Segments distinguish a framework marker from literal bytes that merely
%% look like one, so two templates differing only in that respect -- exactly the
%% pair the scoping bug used to conflate -- get different fingerprints; hashing
%% the rendered bytes would give a repaired template the same `f` as its
%% corrupted predecessor, and since `f` keys the client's persistent (IndexedDB)
%% statics cache, the repair would never reach a client holding the broken entry.
%%
%% That cache's collision domain is every template version a browser has ever
%% seen, and a collision silently renders another template's markup. `phash2/1`
%% spans only 2^27, so a few thousand distinct templates already carry a real
%% birthday-collision risk; the full 2^32 range (`phash2/2`) is the same fast
%% native hash with ~32x fewer collisions, and `f` stays an opaque base-36 string
%% so the wire format is unchanged.
generate_fingerprint(Statics) ->
    Hash = erlang:phash2(Statics, 1 bsl 32),
    integer_to_binary(Hash, 36).

split_fun_body([Last]) ->
    {[], Last};
split_fun_body([H | T]) ->
    {Rest, Last} = split_fun_body(T),
    {[H | Rest], Last}.

compile_parts_ast(Statics, DynASTs, Fingerprint) ->
    {ast_list([ast_binary(S) || S <- Statics]), ast_list(DynASTs), ast_binary(Fingerprint)}.

build_template_ast(Line, Statics, DynASTs, Fingerprint, Opts) ->
    {StaticsAST, DynamicsAST, FpAST} = compile_parts_ast(Statics, DynASTs, Fingerprint),
    BaseFields = [
        {map_field_assoc, Line, {atom, Line, s}, StaticsAST},
        {map_field_assoc, Line, {atom, Line, d}, DynamicsAST},
        {map_field_assoc, Line, {atom, Line, f}, FpAST}
    ],
    {map, Line, BaseFields ++ opts_to_map_fields(Opts, Line)}.

%% The per-item fun is inlined into the CALLER, so it binds the callback's own
%% parameter names in the caller's scope. A callback that reads the item with
%% `?get` must name that parameter `Bindings` -- `?get` expands to the literal
%% identifier and a tracked read in a scope without it is rejected outright
%% (tracked_get_on_non_bindings_map) -- so it shadows the caller's `Bindings` and
%% the compiler warns. Worse, the inlined clause carries the CALLEE's annotation,
%% so the warning names the callback's head rather than the caller where the fun
%% is actually built, sending anyone who investigates to an innocent function.
%%
%% Alpha-rename the bound parameters to names no source can contain. The item's
%% bindings still win inside the item template (that shadowing was always
%% correct); only the name changes, so nothing about the render differs. The
%% rename is a blind substitution over the clause, which is alpha-equivalent even
%% where a nested fun rebinds the same name: renaming binder and references
%% together preserves the shadowing that was there.
%%
%% Keyed by the template fingerprint rather than a counter so the output stays
%% byte-identical across compiles -- these names reach the abstract-code chunk.
rename_each_params(Fingerprint, Vars, Guards, Prefix, Body) ->
    Renames = [
        {Name, fresh_each_var(Fingerprint, Name)}
     || {var, _, Name} <- Vars, Name =/= '_'
    ],
    Rename = fun(Term) -> lists:foldl(fun rename_var/2, Term, Renames) end,
    {Rename(Vars), Rename(Guards), Rename(Prefix), Rename(Body)}.

fresh_each_var(Fingerprint, Name) ->
    Chars = atom_to_list(Name),
    Base = "AzItem@" ++ binary_to_list(Fingerprint) ++ "@" ++ Chars,
    case Chars of
        %% A `_`-prefixed parameter is the author declaring the item unused. Keep
        %% the prefix, or the rename turns that into an unused_var warning -- the
        %% same class of noise this rename exists to remove.
        [$_ | _] -> list_to_atom([$_ | Base]);
        _ -> list_to_atom(Base)
    end.

%% `{var, _, Name}` is the only variable form in the abstract format, so a blind
%% structural walk renames every occurrence -- binder and reference alike.
rename_var({Old, New}, {var, L, Old}) ->
    {var, L, New};
rename_var({_Old, _New} = R, Term) when is_tuple(Term) ->
    list_to_tuple([rename_var(R, E) || E <- tuple_to_list(Term)]);
rename_var({_Old, _New} = R, Terms) when is_list(Terms) ->
    [rename_var(R, E) || E <- Terms];
rename_var({_Old, _New}, Term) ->
    Term.

%% A named `fun Name/Arity` ?each callback and a local element helper are both
%% top-level functions, so the only free variables in their bodies are their
%% parameters -- every other name is bound inside them. Inlining splices those
%% binders into the caller's scope, where a caller that already bound the same
%% name turns the callee's binding into an equality test against it: the item
%% renders for as long as the two values agree, then fails the first time they
%% differ, naming a value and no variable.
%% Blind-renaming every variable is alpha-equivalent for a body with no free
%% variables, so the callee's names cannot collide with the caller's. This does
%% not apply to an inline `fun(Item) -> ... end`, whose body legitimately closes
%% over the caller's variables.
%%
%% Keyed by the callee's name/arity rather than a counter so the output stays
%% byte-identical across compiles -- these names reach the abstract-code chunk.
%%
%% Runtime attribution: inlining flattens the whole chain into the OUTERMOST
%% template function's generated fun, so a crash inside an inlined callback
%% reports the callee's line under a fun named for that outermost function --
%% neither the callee nor necessarily the function holding the `?each`. With
%% `render/1` -> `?each(fun section/1)` -> `?each(fun row/1)`, a failure in
%% `row/1` surfaces as `-render/1-fun-0-` at `row/1`'s line, and neither callee
%% is a compiled function any more. Element-helper hops flatten identically. The
%% compiler derives a fun's name from the function it is written in, and a
%% `named_fun` does not change it, so the transform cannot relabel these frames.
rename_inlined_clause(Name, Arity, Clause) ->
    rename_inlined_body(Name, Arity, [], Clause).

%% `Keep` are names the caller substitutes itself (an element helper's
%% parameters, replaced with the caller's argument expressions by
%% subst_helper_args/3). Renaming those here would rewrite the caller's own
%% variables once the arguments are spliced in, so leave them to that pass and
%% run this before it.
rename_inlined_body(Name, Arity, Keep, Term) ->
    Renames = [
        {Var, fresh_inline_var(Name, Arity, Var)}
     || Var <- lists:usort(collect_var_names(Term)), not lists:member(Var, Keep)
    ],
    lists:foldl(fun rename_var/2, Term, Renames).

%% `_` binds nothing, so renaming it would put a variable where the author wrote
%% a discard.
collect_var_names({var, _, '_'}) ->
    [];
collect_var_names({var, _, Name}) ->
    [Name];
collect_var_names(Term) when is_tuple(Term) ->
    collect_var_names(tuple_to_list(Term));
collect_var_names(Terms) when is_list(Terms) ->
    lists:append([collect_var_names(Term) || Term <- Terms]);
collect_var_names(_Term) ->
    [].

fresh_inline_var(Name, Arity, Var) ->
    Chars = atom_to_list(Var),
    Base = "AzFun@" ++ atom_to_list(Name) ++ "@" ++ integer_to_list(Arity) ++ "@" ++ Chars,
    case Chars of
        %% Keep a `_` prefix, or the rename turns a deliberately-unused variable
        %% into an unused_var warning.
        [$_ | _] -> list_to_atom([$_ | Base]);
        _ -> list_to_atom(Base)
    end.

build_each_ast(Line, SourceAST, Vars, Guards, Prefix, Statics, DynASTs, Fingerprint, Opts) ->
    {StaticsAST, DynamicsAST, FpAST} = compile_parts_ast(Statics, DynASTs, Fingerprint),
    {Vars1, Guards1, Prefix1, DynamicsAST1} = rename_each_params(
        Fingerprint, Vars, Guards, Prefix, DynamicsAST
    ),
    DFunAST =
        {'fun', Line,
            {clauses, [
                {clause, Line, Vars1, Guards1, Prefix1 ++ [DynamicsAST1]}
            ]}},
    BaseFields = [
        {map_field_assoc, Line, {atom, Line, t}, {integer, Line, 0}},
        {map_field_assoc, Line, {atom, Line, s}, StaticsAST},
        {map_field_assoc, Line, {atom, Line, d}, DFunAST},
        {map_field_assoc, Line, {atom, Line, f}, FpAST}
    ],
    TmplAST = {map, Line, BaseFields ++ opts_to_map_fields(Opts, Line)},
    {call, Line, {remote, Line, {atom, Line, arizona_template}, {atom, Line, each}}, [
        SourceAST, TmplAST
    ]}.

ast_binary(Bin) ->
    {bin, 0, [{bin_element, 0, {string, 0, binary_to_list(Bin)}, default, default}]}.

ast_list([]) ->
    {nil, 0};
ast_list([H | T]) ->
    {cons, 0, H, ast_list(T)}.

ast_list_to_list({nil, _}) -> [];
ast_list_to_list({cons, _, Head, Tail}) -> [Head | ast_list_to_list(Tail)].

normalize_children(AST) ->
    case is_list_ast(AST) of
        true -> ast_list_to_list(AST);
        false -> [AST]
    end.

extract_attr_name(Backend, {atom, _, Name}) -> Backend:name(Name);
extract_attr_name(_Backend, BinAST) -> extract_binary_value(BinAST).

extract_binary_value({bin, _, Elements}) ->
    iolist_to_binary([extract_bin_element(E) || E <- Elements]).

%% Templates treat binary-literal content as UTF-8 text: a code point is encoded
%% to its UTF-8 bytes (`<<"caf", 233/utf8>>` and a bare `<<233>>` alike render é
%% as `<<195,169>>`), so folded content matches SSR and the diff path. Only the
%% elements is_static_binary/1 admits -- default-unit string/integer, `default` or
%% `[utf8]` type -- reach here.
extract_bin_element({bin_element, _, {string, _, Chars}, default, _Type}) ->
    unicode:characters_to_binary(Chars);
extract_bin_element({bin_element, _, {integer, _, N}, default, _Type}) ->
    <<N/utf8>>.

is_static_binary({bin, _, Elements}) ->
    lists:all(fun is_foldable_bin_element/1, Elements);
is_static_binary(_) ->
    false.

%% A binary literal folds into a compile-time static only when its content is the
%% UTF-8 text the template convention assumes. An explicit **size** (`<<X:16>>`)
%% or a non-`utf8` **type** spec (`<<X/little>>`, `<<X:16/big>>`) instead pins an
%% exact byte layout -- `<<1024:16>>` is `<<4,0>>`, not U+0400's UTF-8 -- so it
%% must not be UTF-8 re-encoded here; it falls through to the runtime dynamic path
%% (rendered by `to_bin/1`), which preserves its bytes. Default-unit
%% `default`/`[utf8]` string and integer elements fold.
is_foldable_bin_element({bin_element, _, {string, _, _Chars}, default, default}) ->
    true;
is_foldable_bin_element({bin_element, _, {string, _, _Chars}, default, [utf8]}) ->
    true;
is_foldable_bin_element({bin_element, _, {integer, _, _N}, default, default}) ->
    true;
is_foldable_bin_element({bin_element, _, {integer, _, _N}, default, [utf8]}) ->
    true;
is_foldable_bin_element(_) ->
    false.

%% Try to fold an attribute value AST that is a literal `arizona_js`
%% command (or literal list of commands) into the final
%% HTML-attribute-safe encoded binary at compile time. Returns
%% `{ok, Bin}` on success; `error` if the value isn't foldable (any
%% non-literal sub-expression, unknown shape, or `arizona_js` not
%% loaded). Falling through to the runtime dynamic path is always safe.
try_fold_arizona_js(ExprAST) ->
    try
        {ok, eval_arizona_js_expr(ExprAST)}
    catch
        _:_ -> error
    end.

%% Evaluate a command-builder call AST (`arizona_js:Fn(...)` /
%% `arizona_android:Fn(...)`) or a literal list of such calls into the runtime
%% term it would yield. Throws on any non-literal sub-expression --
%% `try_fold_arizona_js/1` catches it. Add new platform builder modules here.
eval_arizona_js_expr(
    {call, _, {remote, _, {atom, _, Mod}, {atom, _, Fn}}, ArgsAST}
) when Mod =:= arizona_js orelse Mod =:= arizona_android ->
    Args = [erl_syntax:concrete(A) || A <- ArgsAST],
    apply(Mod, Fn, Args);
eval_arizona_js_expr({nil, _}) ->
    [];
eval_arizona_js_expr({cons, _, H, T}) ->
    [eval_arizona_js_expr(H) | eval_arizona_js_expr(T)].

is_element_tuple({tuple, _, [_, Second]}) ->
    is_list_ast(Second);
is_element_tuple({tuple, _, [_, Second, _Third]}) ->
    is_list_ast(Second);
is_element_tuple({tuple, _, [{atom, _, _} | _]}) ->
    true;
is_element_tuple(_) ->
    false.

is_element_list({cons, _, Head, Tail}) ->
    is_element_tuple(Head) andalso is_element_list_tail(Tail);
is_element_list(_) ->
    false.

is_element_list_tail({nil, _}) ->
    true;
is_element_list_tail({cons, _, Head, Tail}) ->
    is_element_tuple(Head) andalso is_element_list_tail(Tail);
is_element_list_tail(_) ->
    false.

is_list_ast({nil, _}) -> true;
is_list_ast({cons, _, _, _}) -> true;
is_list_ast(_) -> false.

has_dynamic_attr([]) ->
    false;
has_dynamic_attr([{bin, _, _} | Rest]) ->
    has_dynamic_attr(Rest);
has_dynamic_attr([{tuple, _, [_, {atom, _, Val}]} | Rest]) when is_boolean(Val) ->
    has_dynamic_attr(Rest);
has_dynamic_attr([{tuple, _, [_NameAST, ValueAST]} | Rest]) ->
    case is_static_binary(ValueAST) of
        true -> has_dynamic_attr(Rest);
        false -> true
    end;
has_dynamic_attr([{atom, _, _} | Rest]) ->
    has_dynamic_attr(Rest);
has_dynamic_attr(_) ->
    false.

has_dynamic_child([]) ->
    false;
has_dynamic_child([Child | Rest]) ->
    case is_static_binary(Child) orelse is_element_tuple(Child) of
        true -> has_dynamic_child(Rest);
        false -> true
    end.

is_invalid_static_child({tuple, _, _}) -> true;
is_invalid_static_child(_) -> false.

%% Pre-scan items (elements or mixed) for directives before compilation.
%% This ensures nodiff is known upfront so all items compile consistently.
%%
%% Templates that contain a `?inner_content` slot (i.e. layouts) are
%% automatically marked nodiff: a layout is rendered once at SSR and never
%% re-rendered, so its dynamics can never update -- no point emitting `az`
%% targets that are never patched.
prescan_directives(Items) ->
    Opts = lists:foldl(
        fun(Item, Acc) ->
            case is_element_tuple(Item) of
                true ->
                    {_Tag, Attrs0, _Children, _Line} = extract_element(Item),
                    {_Attrs, ElemOpts} = extract_directives(Attrs0),
                    maps:merge(Acc, ElemOpts);
                false ->
                    Acc
            end
        end,
        #{},
        Items
    ),
    case lists:any(fun contains_inner_content/1, Items) of
        true -> Opts#{diff => false};
        false -> Opts
    end.

%% Walks an AST node looking for a `az:inner_content(_)` remote call.
%% `?inner_content` expands to `az:inner_content(Bindings)` before the
%% parse transform runs, so any layout template surfaces this call.
contains_inner_content({call, _, {remote, _, {atom, _, az}, {atom, _, inner_content}}, _}) ->
    true;
contains_inner_content(Tuple) when is_tuple(Tuple) ->
    lists:any(fun contains_inner_content/1, tuple_to_list(Tuple));
contains_inner_content(List) when is_list(List) ->
    lists:any(fun contains_inner_content/1, List);
contains_inner_content(_) ->
    false.

directive_opts(Name) ->
    directive_opts_1(framework_attr_name(Name)).

directive_opts_1(~"az-nodiff") -> {ok, #{diff => false}};
directive_opts_1(_Other) -> false.

%% Attribute names the template author may not write, checked on the element's
%% ORIGINAL attrs -- before az-view / az-local injection, so an injected name is
%% never mistaken for an authored one.
reject_framework_attrs(Attrs, Line) ->
    ok = reject_nested_directives(Attrs, Line),
    reject_reserved_attrs(Attrs, Line).

%% az-nodiff is a whole-compile-unit directive: it is stripped from a template's
%% top-level element attrs (compile_fragment_parts / compile_mixed_items) before
%% compile_element runs. A NESTED element reaches compile_element with its attrs
%% unstripped, so a directive attribute here is az-nodiff on a non-top-level element.
%% There is no per-element nodiff scope, and left as an ordinary boolean attribute it
%% would silently leak the reserved `az-nodiff` into the DOM while keeping the element
%% diffable. Reject it with a clear error rather than mis-compiling it.
reject_nested_directives(Attrs, Line) ->
    case lists:any(fun is_directive_attr/1, Attrs) of
        true -> parse_error(nested_nodiff, Line);
        false -> ok
    end.

is_directive_attr(Attr) ->
    case bare_attr_name(Attr) of
        {ok, Name} ->
            case directive_opts(Name) of
                {ok, _} -> true;
                false -> false
            end;
        error ->
            false
    end.

bare_attr_name({atom, _, Name}) ->
    {ok, directive_attr_name(Name)};
bare_attr_name({bin, _, _} = Bin) ->
    case is_static_binary(Bin) of
        true -> {ok, extract_binary_value(Bin)};
        false -> error
    end;
bare_attr_name(_) ->
    error.

%% The two attribute names the transform emits into the rendered output itself:
%% `az` (the element's diff address, emitted by `Backend:az_attr/1` on any element
%% carrying dynamics) and `az-local` (the `?local` descriptor injected by
%% maybe_inject_local_descriptor/6). A template-authored copy is emitted *beside*
%% the injected one: HTML keeps the first of a duplicate pair and the native wire
%% keeps the last, so one of the two silently disappears -- and when the element
%% has no dynamics the user's `az` is the only one, is fingerprint-scoped like a
%% real address (`scope_static/2` rewrites every ` az="`), and can collide with a
%% genuine slot id. The client resolves a slot with `querySelector('[az=...]')`,
%% which takes the first match in document order, so the op then patches the wrong
%% element. Reject both names at compile time.
%%
%% Deliberately narrow: `az-view` has its own check (it is legal, and injected, on a
%% live root), and every other `az-*` name is user territory -- `az-key` keys stream
%% items, `az-click`/`az-submit`/... carry effects, and an app is free to invent its
%% own `az-*` attributes.
reserved_attr_name(undefined) ->
    %% attr_name/1's "not a compile-time literal" answer (a dynamic attribute
    %% name): there is no name to compare, so it is not a reserved one.
    false;
reserved_attr_name(Name) ->
    is_reserved_attr_name(framework_attr_name(Name)).

is_reserved_attr_name(~"az") -> true;
is_reserved_attr_name(~"az-local") -> true;
is_reserved_attr_name(_Other) -> false.

reject_reserved_attrs(Attrs, Line) ->
    case [Name || Attr <- Attrs, Name <- [attr_name(Attr)], reserved_attr_name(Name)] of
        [] -> ok;
        [Name | _] -> parse_error({reserved_attr, Name}, Line)
    end.

%% The literal name of an attribute in any of its forms (`name`, `<<"name">>`,
%% `{name, Value}`), normalized like a directive so `az_local` and `'az-local'`
%% are one name. `undefined` when the name is not a compile-time literal.
attr_name({tuple, _, [NameAST | _]}) ->
    attr_name(NameAST);
attr_name(Attr) ->
    case bare_attr_name(Attr) of
        {ok, Name} -> Name;
        error -> undefined
    end.

%% Normalize a directive attribute atom to its canonical dashed binary
%% (`az_nodiff` -> `<<"az-nodiff">>`) for framework directive matching. Deliberately
%% NOT a render backend's `name/1`: `az-nodiff` and friends are framework directives
%% that must match identically for every render target -- and directive pre-scan
%% (`prescan_directives/1`) runs before any backend is resolved.
directive_attr_name(Name) ->
    binary:replace(atom_to_binary(Name), <<"_">>, <<"-">>, [global]).

%% The form every framework attribute NAME is compared in: lowercased, on top of
%% the dash normalization above. HTML attribute names are ASCII case-insensitive,
%% so `AZ="x"` is an `az` attribute to the browser and `AZ-VIEW` an `az-view` --
%% the client reads them back with `querySelector('[az=...]')` / `getAttribute`,
%% both of which fold case. Comparing only the lowercase spelling therefore let a
%% capitalized copy slip past the very checks that exist to stop it: a template
%% `AZ` could shadow a real slot address and misroute a patch, and an `AZ-NODIFF`
%% was not recognized as the directive at all, so the element stayed diffable AND
%% leaked the attribute into the DOM -- the opposite of what was asked for.
%%
%% Rejects strictly more than before and nothing a valid template writes: the
%% names are reserved in every casing. The name is normalized only for the
%% comparison; `attr_name/1` still returns it as authored, so an error message
%% quotes what the author actually typed.
framework_attr_name(Name) ->
    string:lowercase(Name).

extract_directives(Attrs) ->
    extract_directives(Attrs, #{}).

extract_directives([], Opts) ->
    {[], Opts};
extract_directives([Attr | Rest], Opts) ->
    case bare_attr_name(Attr) of
        {ok, Name} ->
            case directive_opts(Name) of
                {ok, NewOpts} ->
                    extract_directives(Rest, maps:merge(Opts, NewOpts));
                false ->
                    {Tail, Opts1} = extract_directives(Rest, Opts),
                    {[Attr | Tail], Opts1}
            end;
        error ->
            {Tail, Opts1} = extract_directives(Rest, Opts),
            {[Attr | Tail], Opts1}
    end.

opts_to_map_fields(Opts, Line) ->
    [{map_field_assoc, Line, {atom, Line, K}, {atom, Line, V}} || K := V <- Opts].
