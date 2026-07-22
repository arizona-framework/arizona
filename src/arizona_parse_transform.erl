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

%% Compile state threaded through element/attribute/child compilation.
-record(state, {
    buf = <<>> :: binary(),
    statics = [] :: [static()],
    dynamics = [] :: [dynamic()],
    az = 0 :: az(),
    nodiff = false :: boolean(),
    %% Raw-text context of the enclosing element while compiling its children
    %% (Backend:raw_text_kind/1). `raw`/`escapable` mean a dynamic content slot
    %% must be emitted markerless and render-once -- HTML comment markers would
    %% become literal content there (script/style/textarea/title).
    raw_text_kind = none :: none | raw | escapable,
    module = undefined :: module() | undefined,
    live_render = false :: boolean(),
    root = false :: boolean(),
    backend = arizona_html :: module()
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

%% A local `fun Name/Arity` ?each callback is inlined into the per-item template, so
%% the function loses its only reference. Suppress the resulting unused-function
%% warning (compiler, under warnings_as_errors) and xref finding (locals_not_used /
%% exports_not_used) by injecting -compile(nowarn_unused_function) and -ignore_xref
%% attributes for the consumed pairs. A no-op when the function is also used elsewhere.
inject_each_callback_suppressions(Forms, OrigForms, FunDefs, Module) ->
    case collect_each_callback_pairs(OrigForms, FunDefs, Module) of
        [] ->
            Forms;
        Pairs ->
            insert_suppression_attrs(Forms, Pairs)
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
%%      `each` name (default `arizona_html` backend).
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
%% `?stateful`/`?stateless` child *module* is invisible at this AST level and
%% stays a documented "one target per tree" rule.
mark_targets({call, L, {remote, _, {atom, _, Mod}, {atom, _, html}}, _}, Ctx) when
    (Mod =:= arizona_template orelse Mod =:= az) andalso
        (Ctx =:= native orelse Ctx =:= terminal)
->
    parse_error(cross_target_nesting, L);
mark_targets({call, L, {remote, _, {atom, _, Mod}, {atom, _, native}}, _}, Ctx) when
    (Mod =:= arizona_template orelse Mod =:= az) andalso
        (Ctx =:= html orelse Ctx =:= terminal)
->
    parse_error(cross_target_nesting, L);
mark_targets({call, L, {remote, _, {atom, _, Mod}, {atom, _, terminal}}, _}, Ctx) when
    (Mod =:= arizona_template orelse Mod =:= az) andalso
        (Ctx =:= html orelse Ctx =:= native)
->
    parse_error(cross_target_nesting, L);
mark_targets({call, L, {remote, RL, {atom, ML, Mod}, {atom, FL, html}}, [Arg]}, _Ctx) when
    Mod =:= arizona_template orelse Mod =:= az
->
    {call, L, {remote, RL, {atom, ML, Mod}, {atom, FL, html}}, [mark_targets(Arg, html)]};
mark_targets({call, L, {remote, RL, {atom, ML, Mod}, {atom, FL, native}}, [Arg]}, _Ctx) when
    Mod =:= arizona_template orelse Mod =:= az
->
    {call, L, {remote, RL, {atom, ML, Mod}, {atom, FL, native}}, [mark_targets(Arg, native)]};
mark_targets({call, L, {remote, RL, {atom, ML, Mod}, {atom, FL, terminal}}, [Arg]}, _Ctx) when
    Mod =:= arizona_template orelse Mod =:= az
->
    {call, L, {remote, RL, {atom, ML, Mod}, {atom, FL, terminal}}, [mark_targets(Arg, terminal)]};
mark_targets({call, L, {remote, RL, {atom, ML, Mod}, {atom, FL, each}}, Args}, native) when
    Mod =:= arizona_template orelse Mod =:= az
->
    {call, L, {remote, RL, {atom, ML, Mod}, {atom, FL, native_each}}, [
        mark_targets(A, native)
     || A <- unwrap_each_body(Args, native)
    ]};
mark_targets({call, L, {remote, RL, {atom, ML, Mod}, {atom, FL, each}}, Args}, terminal) when
    Mod =:= arizona_template orelse Mod =:= az
->
    {call, L, {remote, RL, {atom, ML, Mod}, {atom, FL, terminal_each}}, [
        mark_targets(A, terminal)
     || A <- unwrap_each_body(Args, terminal)
    ]};
%% `?each` under `?html` (or standalone -- `none`) keeps the name `each` (the
%% bottom-up transform compiles it with the default `arizona_html` backend), but
%% we still unwrap an inline `?html(...)` callback body here. The guard lets any
%% other (future) `Ctx` fall through to the generic tuple recursion below.
mark_targets({call, L, {remote, RL, {atom, ML, Mod}, {atom, FL, each}}, Args}, Ctx) when
    (Mod =:= arizona_template orelse Mod =:= az) andalso
        (Ctx =:= html orelse Ctx =:= none)
->
    {call, L, {remote, RL, {atom, ML, Mod}, {atom, FL, each}}, [
        mark_targets(A, Ctx)
     || A <- unwrap_each_body(Args, html)
    ]};
mark_targets(Node, Ctx) when is_tuple(Node) ->
    list_to_tuple([mark_targets(E, Ctx) || E <- tuple_to_list(Node)]);
mark_targets(Nodes, Ctx) when is_list(Nodes) ->
    [mark_targets(E, Ctx) || E <- Nodes];
mark_targets(Node, _Ctx) ->
    Node.

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
format_error(local_html_only) ->
    "?local is only supported in ?html templates";
format_error(local_key_reused) ->
    "a ?local key cannot bind both content and an attribute on the same element";
format_error(local_attr_multiple) ->
    "an attribute value can interpolate at most one ?local -- multiple ?local in "
    "one attribute can't be recomposed client-side";
format_error(local_attr_mixed) ->
    "a ?local in an attribute value can only be combined with static text, not "
    "another dynamic expression";
format_error(cross_target_nesting) ->
    "cannot nest ?html, ?native and ?terminal in one template -- they produce "
    "incompatible statics. Render cross-target content via a separate "
    "stateful/stateless child of the matching target";
format_error(tracked_get_on_non_bindings_map) ->
    "arizona_template:get/get_lazy/with (and the az: aliases) track every read against "
    "the view bindings, so their map argument must be the bindings -- a parameter or a "
    "direct alias (`B = Bindings`). This local is not provably the bindings. If it is a "
    "nested map, read it with maps:get/2 (`User = arizona_template:get(user, Bindings), "
    "Name = maps:get(name, User)`); if it is a bindings value reached through a "
    "case/merge the transform cannot see into, alias it directly with `B = Bindings` first".

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
    Inline = collect_inline(Body),
    Body1 = [transform_expr(Expr, Module, Inline, FunDefs) || Expr <- Body],
    {clause, L, Patterns, Guards, suppress_unused_inline_matches(Body1, Inline)}.

transform_expr(Expr, Module, Inline, FunDefs) ->
    erl_syntax_lib:map(fun(Node) -> transform_node(Node, Module, Inline, FunDefs) end, Expr).

transform_node(Node, Module, Inline, FunDefs) ->
    N = erl_syntax:revert(Node),
    case N of
        {call, L, {remote, _, {atom, _, Mod}, {atom, _, html}}, [Arg]} when
            Mod =:= arizona_template; Mod =:= az
        ->
            compile_template(inline_vars(Arg, Inline), L, Module);
        {call, L, {remote, _, {atom, _, Mod}, {atom, _, native}}, [Arg]} when
            Mod =:= arizona_template; Mod =:= az
        ->
            compile_template(inline_vars(Arg, Inline), L, Module, false, arizona_native);
        {call, L, {remote, _, {atom, _, Mod}, {atom, _, terminal}}, [Arg]} when
            Mod =:= arizona_template; Mod =:= az
        ->
            %% Terminal templates compile as non-live (no client root id / az_view
            %% injection): the live process derives the view id from bindings, and
            %% the terminal transport repaints whole frames rather than addressing
            %% the root node. So the live-render last-expr path falls through to
            %% here for `?terminal(...)`.
            compile_template(inline_vars(Arg, Inline), L, Module, false, arizona_terminal);
        {call, L, {remote, _, {atom, _, Mod}, {atom, _, each}}, [FunArg, SourceArg]} when
            Mod =:= arizona_template; Mod =:= az
        ->
            compile_each(
                inline_vars(FunArg, Inline),
                inline_vars(SourceArg, Inline),
                L,
                Module,
                arizona_html,
                FunDefs
            );
        {call, L, {remote, _, {atom, _, Mod}, {atom, _, native_each}}, [FunArg, SourceArg]} when
            Mod =:= arizona_template; Mod =:= az
        ->
            compile_each(
                inline_vars(FunArg, Inline),
                inline_vars(SourceArg, Inline),
                L,
                Module,
                arizona_native,
                FunDefs
            );
        {call, L, {remote, _, {atom, _, Mod}, {atom, _, terminal_each}}, [FunArg, SourceArg]} when
            Mod =:= arizona_template; Mod =:= az
        ->
            compile_each(
                inline_vars(FunArg, Inline),
                inline_vars(SourceArg, Inline),
                L,
                Module,
                arizona_terminal,
                FunDefs
            );
        %% Sugar: `arizona_template:stateless(atom, Props)` with a literal atom
        %% callback is rewritten to `arizona_template:stateless(fun atom/1, Props)`.
        %% Fun references and other shapes pass through unchanged.
        {call, L, {remote, _, {atom, _, Mod}, {atom, _, stateless}} = Callee, [
            {atom, AL, Name}, PropsArg
        ]} when
            Mod =:= arizona_template; Mod =:= az
        ->
            FunRef = {'fun', AL, {function, Name, 1}},
            {call, L, Callee, [FunRef, PropsArg]};
        _ ->
            N
    end.

transform_live_render_clause({clause, L, Patterns, Guards, Body}, Module, FunDefs) ->
    check_tracked_get_targets(Patterns, Body),
    {Init0, [Last]} = lists:split(length(Body) - 1, Body),
    %% Only the init statements are normalized: the last expr carries the live root
    %% template and is handled by transform_live_render_last/4.
    Init = normalize_tail_binds(Init0),
    Inline = collect_inline(Init ++ [Last]),
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
        {call, L, {remote, _, {atom, _, Mod}, {atom, _, html}}, [Arg]} when
            Mod =:= arizona_template; Mod =:= az
        ->
            validate_live_root(Arg, L, Inline),
            Arg1 = transform_expr(Arg, Module, Inline, FunDefs),
            compile_template(inline_vars(Arg1, Inline), L, Module, true);
        {call, L, {remote, _, {atom, _, Mod}, {atom, _, native}}, [Arg]} when
            Mod =:= arizona_template; Mod =:= az
        ->
            validate_live_root(Arg, L, Inline),
            Arg1 = transform_expr(Arg, Module, Inline, FunDefs),
            compile_template(inline_vars(Arg1, Inline), L, Module, true, arizona_native);
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

is_az_view_attr({atom, _, Name}) ->
    Name =:= az_view orelse Name =:= 'az-view';
is_az_view_attr({tuple, _, [{atom, _, Name} | _]}) ->
    Name =:= az_view orelse Name =:= 'az-view';
is_az_view_attr({bin, _, [{bin_element, _, {string, _, "az-view"}, _, _}]}) ->
    true;
is_az_view_attr(_) ->
    false.

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
iv_qual({b_generate, L, P, E}, Inline) ->
    {{b_generate, L, P, iv(E, Inline)}, maps:without(pattern_vars(P), Inline)};
iv_qual({m_generate, L, P, E}, Inline) ->
    {{m_generate, L, P, iv(E, Inline)}, maps:without(pattern_vars(P), Inline)};
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

compile_template(Arg, Line, Module) ->
    compile_template(Arg, Line, Module, false).

compile_template(Arg, Line, Module, LiveRender) ->
    compile_template(Arg, Line, Module, LiveRender, arizona_html).

compile_template(Arg, Line, Module, LiveRender, Backend) ->
    {Statics, DynASTs, Fingerprint, Opts0} = compile_body_parts(Arg, Module, LiveRender, Backend),
    Opts = maybe_target_opt(Backend, Opts0),
    {S1, D1} = scope_az(Backend, Fingerprint, Statics, DynASTs),
    build_template_ast(Line, S1, D1, Fingerprint, Opts).

%% Native templates carry `target => native` so the runtime (render_fp_val)
%% knows to inline dynamic attribute values as JSON. Terminal templates carry
%% `target => terminal` for honesty in the snapshot (the wire path treats it like
%% the default html, which terminal never uses). HTML templates carry no target
%% key (the default).
maybe_target_opt(arizona_native, Opts) ->
    Opts#{target => native};
maybe_target_opt(arizona_terminal, Opts) ->
    Opts#{target => terminal};
maybe_target_opt(_Backend, Opts) ->
    Opts.

compile_each(FunAST, SourceAST, Line, Module, Backend, FunDefs) ->
    case FunAST of
        {'fun', _, {clauses, [{clause, _, [ItemVar, KeyVar], Guards, Body}]}} ->
            compile_each_clause(
                stream, [ItemVar, KeyVar], Guards, Body, SourceAST, Line, Module, Backend
            );
        {'fun', _, {clauses, [{clause, _, [ItemVar], Guards, Body}]}} ->
            compile_each_clause(
                list, [ItemVar], Guards, Body, SourceAST, Line, Module, Backend
            );
        %% Local `fun Name/1` or `fun Name/2` ref: resolve its single clause and compile
        %% it exactly like an inline fun, so the same element-body validation runs. The
        %% looked-up clause is the original untransformed body, which is what the inline
        %% path expects. (Its now-orphaned definition is covered by the injected
        %% nowarn_unused_function / ignore_xref attributes.)
        {'fun', L, {function, Name, Arity}} when Arity =:= 1; Arity =:= 2 ->
            compile_named_each(Name, Arity, SourceAST, Line, L, Module, Backend, FunDefs);
        %% A local ref of any other arity isn't a valid callback (1 = list, 2 = stream/map).
        {'fun', L, {function, _Name, _Arity}} ->
            parse_error(invalid_each_fun, L);
        %% Same-module explicit ref `fun ?MODULE:Name/Arity` (literal module = this module):
        %% the body is visible here, so rewrite to the bare local form and re-dispatch -- it
        %% then behaves exactly like `fun Name/Arity` (resolve + inline, or the same
        %% arity/multi-clause/undefined errors).
        {'fun', L, {function, {atom, _, Module}, {atom, _, Name}, {integer, _, Arity}}} ->
            compile_each(
                {'fun', L, {function, Name, Arity}}, SourceAST, Line, Module, Backend, FunDefs
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
%% `L` is the fun-ref location (for the error/synthesized clause); `Line` the each call site.
compile_named_each(Name, Arity, SourceAST, Line, L, Module, Backend, FunDefs) ->
    case FunDefs of
        #{{Name, Arity} := [Clause]} ->
            compile_each(
                {'fun', L, {clauses, [Clause]}}, SourceAST, Line, Module, Backend, FunDefs
            );
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
compile_each_clause(Kind, Vars, Guards, Body, SourceAST, Line, Module, Backend) ->
    {Prefix, LastExpr} = split_fun_body(Body),
    case each_body_unwrap(LastExpr, Backend) of
        {compiled, Map} ->
            build_each_from_compiled(Line, SourceAST, Vars, Guards, Prefix, Map);
        {element, ElemAST} ->
            Classification = classify_body(ElemAST),
            ok = validate_each_body(Kind, Classification, ElemAST),
            {Statics, DynASTs, Fingerprint, Opts0} = compile_body_parts(
                ElemAST, Module, false, Backend
            ),
            Opts1 = maybe_target_opt(Backend, Opts0),
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
%% per-position DOM node, and stream items are keyed by `az-key`, so neither is
%% flagged regardless of backend.
maybe_single_root_opt(Backend, list, element_tuple, Opts) ->
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
    Wrapper = backend_template_fn(Backend),
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

backend_template_fn(arizona_html) -> html;
backend_template_fn(arizona_native) -> native;
backend_template_fn(arizona_terminal) -> terminal.

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
    DFunAST =
        {'fun', Line, {clauses, [{clause, Line, Vars, Guards, Prefix ++ [DListAST]}]}},
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
    compile_classified_body(classify_body(ExprAST), ExprAST, Module, LiveRender, Backend).

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

compile_classified_body(static_binary, ExprAST, _Module, _LiveRender, _Backend) ->
    Bin = extract_binary_value(ExprAST),
    Statics = [Bin],
    {Statics, [], generate_fingerprint(Statics), #{}};
compile_classified_body(element_tuple, ExprAST, Module, LiveRender, Backend) ->
    compile_fragment_parts([ExprAST], Module, LiveRender, Backend);
compile_classified_body(element_list, ExprAST, Module, LiveRender, Backend) ->
    compile_fragment_parts(ast_list_to_list(ExprAST), Module, LiveRender, Backend);
compile_classified_body(list_ast, ExprAST, Module, _LiveRender, Backend) ->
    compile_mixed_items(ast_list_to_list(ExprAST), Module, Backend);
compile_classified_body(text_dynamic, ExprAST, Module, _LiveRender, Backend) ->
    Statics = [<<>>, <<>>],
    DynASTs = [make_esc_text_dynamic_ast(<<"0">>, ExprAST, Module, line(ExprAST), Backend)],
    {Statics, DynASTs, generate_fingerprint(Statics), #{}}.

compile_fragment_parts(ElementASTs, Module, LiveRender, Backend) ->
    Opts = prescan_directives(ElementASTs),
    State0 = #state{
        module = Module,
        nodiff = maps:is_key(diff, Opts),
        live_render = LiveRender,
        root = LiveRender,
        backend = Backend
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

compile_mixed_items(Items, Module, Backend) ->
    Opts = prescan_directives(Items),
    State0 = #state{module = Module, nodiff = maps:is_key(diff, Opts), backend = Backend},
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
    State2 = buf_append(State1, Backend:text_slot_open(Az)),
    DynAST = make_esc_text_dynamic_ast(Az, Item, Module, line(Item), Backend),
    State3 = flush(State2, DynAST),
    State3#state{buf = Backend:text_slot_close()}.

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
    Backend = State0#state.backend,
    Attrs1 = maybe_inject_or_raise_az_view(Attrs0, Line, State0),
    Attrs = maybe_inject_local_descriptor(Backend, Attrs1, Children, Line, State0),
    RawKind = Backend:raw_text_kind(Tag),
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
    State3 = buf_append(State2, Backend:element_open(TagBin)),
    State4 =
        case ElemAz of
            none ->
                State3;
            N ->
                AzBin = integer_to_binary(N),
                buf_append(State3, Backend:az_attr(AzBin))
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
            State7 = compile_children(Children, ElemAz, State6#state{raw_text_kind = RawKind}),
            State8 = buf_append(State7, Backend:element_close(TagBin)),
            State8#state{raw_text_kind = State0#state.raw_text_kind}
    end.

compile_attrs([], _ElemAz, State, _ElemLine) ->
    State;
compile_attrs([Attr | Rest], ElemAz, State0, ElemLine) ->
    State1 = compile_attr(Attr, ElemAz, State0, ElemLine),
    compile_attrs(Rest, ElemAz, State1, ElemLine).

compile_attr({bin, _, _} = Bin, _ElemAz, State0, ElemLine) ->
    Backend = State0#state.backend,
    NameBin = extract_binary_value(Bin),
    buf_append(State0, emit_attr(fun() -> Backend:attr_boolean(NameBin) end, ElemLine));
compile_attr({tuple, _, [NameAST, {atom, _, false}]}, _ElemAz, State0, _ElemLine) when
    element(1, NameAST) =:= atom; element(1, NameAST) =:= bin
->
    State0;
compile_attr({tuple, _, [NameAST, {atom, _, true}]}, _ElemAz, State0, ElemLine) when
    element(1, NameAST) =:= atom; element(1, NameAST) =:= bin
->
    Backend = State0#state.backend,
    NameBin = extract_attr_name(Backend, NameAST),
    buf_append(State0, emit_attr(fun() -> Backend:attr_boolean(NameBin) end, ElemLine));
compile_attr({tuple, _, [NameAST, ValueAST]}, ElemAz, State0, ElemLine) when
    element(1, NameAST) =:= atom; element(1, NameAST) =:= bin
->
    Backend = State0#state.backend,
    NameBin = extract_attr_name(Backend, NameAST),
    case is_static_binary(ValueAST) of
        true ->
            ValBin = extract_binary_value(ValueAST),
            buf_append(State0, emit_attr(fun() -> Backend:attr(NameBin, ValBin) end, ElemLine));
        false ->
            compile_dynamic_attr(Backend, NameBin, ValueAST, ElemAz, State0)
    end;
compile_attr({atom, _, Name}, _ElemAz, State0, ElemLine) ->
    Backend = State0#state.backend,
    NameBin = Backend:name(Name),
    buf_append(State0, emit_attr(fun() -> Backend:attr_boolean(NameBin) end, ElemLine));
compile_attr(Attr, _ElemAz, _State0, ElemLine) ->
    AttrLine =
        try
            line(Attr)
        catch
            _:_ -> ElemLine
        end,
    parse_error(invalid_attribute, AttrLine).

%% Run a backend attribute-emitting callback, turning a backend's attribute
%% rejection -- `error({arizona_render_reject, Message})` -- into a line-accurate
%% parse error carrying the backend's message. Lets a render backend cleanly
%% refuse attributes it cannot express (e.g. the terminal target rejecting an
%% unknown style atom) instead of silently dropping them.
emit_attr(Fun, Line) ->
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
            buf_append(State0, emit_attr(fun() -> Backend:attr_command(NameBin, Cmd) end, ValLine));
        error when State0#state.nodiff ->
            Module = State0#state.module,
            State1 = buf_append(
                State0, emit_attr(fun() -> Backend:attr_dyn_name(NameBin) end, ValLine)
            ),
            DynAST = make_nodiff_attr_dynamic_ast(
                NameBin, ValueAST, Module, line(ValueAST)
            ),
            flush(State1, DynAST);
        error ->
            Module = State0#state.module,
            State1 = buf_append(
                State0, emit_attr(fun() -> Backend:attr_dyn_name(NameBin) end, ValLine)
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
%% The descriptor JSON (`#{a => #{AttrName => Key}, c => ContentKey}`) is built
%% and HTML-attribute-escaped at compile time (reusing arizona_effect:escape_attr/1).
maybe_inject_local_descriptor(Backend, Attrs, Children, Line, State) ->
    AttrLocals = collect_attr_locals(Backend, Attrs, Line),
    AttrAffixes = collect_attr_affixes(Backend, Attrs, Line),
    ContentLocals = collect_content_locals(Children, Line),
    case map_size(AttrLocals) > 0 orelse map_size(ContentLocals) > 0 of
        false ->
            Attrs;
        true ->
            assert_local_supported(Backend, State, Line),
            assert_no_key_reuse(AttrLocals, ContentLocals, Line),
            Desc = build_local_descriptor(AttrLocals, AttrAffixes, ContentLocals),
            Escaped = arizona_effect:escape_attr(iolist_to_binary(json:encode(Desc))),
            [{tuple, 0, [ast_binary(~"az-local"), ast_binary(Escaped)]} | Attrs]
    end.

assert_local_supported(Backend, State, Line) ->
    case Backend of
        arizona_html -> ok;
        _ -> parse_error(local_html_only, Line)
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
%% arizona_html:text_az/2). The slot counter advances on every dynamic text
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

emit_child_dynamic(
    Child, _ElemAz, #state{nodiff = true, module = Module, backend = Backend} = State0, Slot
) ->
    DynAST = make_nodiff_dynamic_ast(Child, Module, line(Child), Backend),
    {flush(State0, DynAST), Slot};
emit_child_dynamic(
    Child, _ElemAz, #state{raw_text_kind = raw, module = Module, backend = Backend} = State0, Slot
) ->
    %% script/style: raw text, the browser decodes neither character references
    %% nor HTML comments here, so the value is emitted verbatim, markerless and
    %% render-once. Comment markers would become literal bytes and corrupt the
    %% script/CSS (and a module script's HTML-comment tokens are a SyntaxError).
    %% Diffing is impossible by construction (no marker to patch), so the slot
    %% renders once -- the diff engine skips its `undefined` az.
    DynAST = make_raw_text_dynamic_ast(Child, Module, line(Child), Backend),
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
    State1 = buf_append(State0, Backend:text_slot_open(MarkerAz)),
    DynAST = make_esc_text_dynamic_ast(MarkerAz, Child, Module, line(Child), Backend),
    State2 = flush(State1, DynAST),
    {State2#state{buf = Backend:text_slot_close()}, Slot + 1}.

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
    {tuple, 0, [ast_binary(AzBin), esc_spec(Backend, ExprAST, FunAST), LocAST]}.

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
            [];
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
    map_tail_exprs(
        Expr,
        fun(Leaf) -> expand_element_leaf(Leaf, Module, Backend) end,
        fun(NonTail) -> NonTail end
    ).

%% At a tail leaf, compile a bare element tuple / element list (or a mixed list
%% that contains an element tuple) into a nested template, as a literal ?html
%% there would; leave plain values (and pure value lists) untouched.
expand_element_leaf(Expr, Module, Backend) ->
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

%% Tag a value interpolation `{esc, Fun}` (escaped at the render boundary per the
%% target's backend) or leave it bare. HTML entity-escapes and the terminal
%% sanitizes control bytes, so both mark scalar values; native (JSON wire) never
%% escapes, so it stays bare. Blocks are always left bare and spliced raw.
esc_spec(arizona_native, _ExprAST, FunAST) ->
    FunAST;
esc_spec(_Backend, ExprAST, FunAST) ->
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
            slot,
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
    {tuple, 0, [{atom, 0, undefined}, esc_spec(Backend, ExprAST, FunAST), LocAST]}.

%% Markerless render-once for a `raw` raw-text element (script/style). Unlike
%% make_nodiff_dynamic_ast/4 the value is left bare (never `{esc, Fun}`): the
%% browser does not decode character references inside these, so HTML-escaping a
%% scalar would corrupt it (`&` -> `&amp;`). `undefined` az makes it non-diffable
%% -- there is no comment marker to patch.
make_raw_text_dynamic_ast(ExprAST0, Module, ExprLine, Backend) ->
    ExprAST = expand_block_element_tails(ExprAST0, Module, Backend),
    LocAST = loc_ast(Module, ExprLine),
    FunAST = {'fun', 0, {clauses, [{clause, 0, [], [], [ExprAST]}]}},
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
    State#state{buf = <<(State#state.buf)/binary, Bin/binary>>}.

flush(State, DynAST) ->
    State#state{
        statics = State#state.statics ++ [State#state.buf],
        dynamics = State#state.dynamics ++ [DynAST],
        buf = <<>>
    }.

finalize(State) ->
    Statics = State#state.statics ++ [State#state.buf],
    Dynamics = State#state.dynamics,
    {Statics, Dynamics}.

%% Prefix az values with the template fingerprint to prevent collisions
%% when stateless children are inlined in a parent template.
scope_az(_Backend, _Fp, Statics, []) ->
    {Statics, []};
scope_az(Backend, Fp, Statics, DynASTs) ->
    {[Backend:scope_static(Fp, S) || S <- Statics], [
        scope_dynamic_ast(Fp, D)
     || D <- DynASTs
    ]}.

scope_dynamic_ast(_Fp, {tuple, _, [{atom, _, undefined} | _]} = D) ->
    D;
scope_dynamic_ast(Fp, {tuple, L, [AzAST | Rest]}) ->
    AzBin = extract_binary_value(AzAST),
    {tuple, L, [ast_binary(<<Fp/binary, "-", AzBin/binary>>) | Rest]}.

generate_fingerprint(Statics) ->
    Hash = erlang:phash2(Statics),
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

build_each_ast(Line, SourceAST, Vars, Guards, Prefix, Statics, DynASTs, Fingerprint, Opts) ->
    {StaticsAST, DynamicsAST, FpAST} = compile_parts_ast(Statics, DynASTs, Fingerprint),
    DFunAST =
        {'fun', Line,
            {clauses, [
                {clause, Line, Vars, Guards, Prefix ++ [DynamicsAST]}
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

extract_bin_element({bin_element, _, {string, _, Chars}, _, _}) ->
    unicode:characters_to_binary(Chars);
extract_bin_element({bin_element, _, {integer, _, N}, _, _}) ->
    <<N/utf8>>.

is_static_binary({bin, _, Elements}) ->
    lists:all(
        fun
            ({bin_element, _, {string, _, _}, _, _}) -> true;
            ({bin_element, _, {integer, _, _}, _, _}) -> true;
            (_) -> false
        end,
        Elements
    );
is_static_binary(_) ->
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

directive_opts(<<"az-nodiff">>) -> {ok, #{diff => false}};
directive_opts(_) -> false.

bare_attr_name({atom, _, Name}) ->
    {ok, arizona_html:name(Name)};
bare_attr_name({bin, _, _} = Bin) ->
    case is_static_binary(Bin) of
        true -> {ok, extract_binary_value(Bin)};
        false -> error
    end;
bare_attr_name(_) ->
    error.

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
