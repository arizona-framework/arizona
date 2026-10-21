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
| `'az-nodiff'` | Directive | Emits `diff => false`; kept as a compiler-owned attribute |

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
%% `{dyn_attr, Name, Bytes}` is the static part of a dynamic attribute: it renders
%% as the backend's `attr_dyn_name/1` bytes, but carries `Name` into the
%% fingerprint. HTML renders the name inside the dynamic and contributes no bytes,
%% so without it two templates differing only in a dynamic attribute's name share
%% a fingerprint, and the diff patches one into the other, leaving the old
%% attribute on the element.
-type segment() ::
    binary() | {az_attr, binary()} | {az_slot, binary()} | {dyn_attr, binary(), binary()}.

%% Compile state threaded through element/attribute/child compilation.
%% Process-dictionary key for the `az-*` attribute names collected while
%% compiling a module. See `inject_az_attrs/1`.
-define(AZ_ATTRS_KEY, arizona_az_attrs).

%% How many bytes of a raw-text slot's static neighbours reach `Backend:raw_text/4`
%% -- the bound `arizona_renderer` sets on that callback, so the call embeds this
%% many bytes per side rather than the whole static.
-define(RAW_TEXT_NEIGHBOUR_BYTES, 8).

%% Process-dictionary key for the binary literals folded into template statics
%% while compiling a module (see extract_binary_value/1 and latin1_warnings/2).
-define(FOLDED_KEY, arizona_folded_literals).

%% Process-dictionary counter naming the variables an inlined helper binds its
%% pattern- and guard-read parameters to (see bind_param_tests/5). Per module, so the
%% names stay byte-identical across compiles, and unique within it, so two inlined
%% calls in one scope never bind the same name (a second binding would silently
%% become an equality test against the first).
-define(PARAM_SEQ_KEY, arizona_helper_param_seq).

-define(IS_GENERATOR(Gen),
    (Gen =:= generate orelse Gen =:= generate_strict orelse Gen =:= b_generate orelse
        Gen =:= b_generate_strict orelse Gen =:= m_generate orelse Gen =:= m_generate_strict)
).

%% Process-dictionary counter numbering each substituted copy of a hoisted
%% definition (see hoist_site_copy/1). Reset per module, so the minted names --
%% which reach the abstract-code chunk -- are identical across compiles.
-define(HOIST_SITE_KEY, arizona_hoist_site).

%% Stands in for a helper argument when counting how many times substitution puts
%% it in (see check_helper_args_pure/5).
-define(PARAM_USE_MARKER, {atom, 0, '$arizona_param_use'}).

%% Module a template-constructor call is re-spelled under when it sits inside an
%% `az-nodiff` template (see inherit_nodiff/1). The constructors compile
%% bottom-up, so a nested `?each`/`?html` is a finished map before the enclosing
%% template is reached; the module slot is how the enclosing directive reaches it
%% first. Every such call is compiled away -- one left over is `undef` at render.
-define(NODIFF_MOD, '$arizona_nodiff').

%% Stands in for an az-nodiff element's fingerprint in its `az-nodiff` token call until the
%% statics, and so the fingerprint, are known (fill_nodiff_fingerprint/2).
-define(NODIFF_FP, '$arizona_nodiff_fp').

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
    %% become literal content there (script/style/textarea/title, and the other
    %% RAWTEXT elements: xmp/iframe/noembed/noframes).
    raw_text_kind = none :: arizona_renderer:raw_text_kind(),
    %% The tag that produced `raw_text_kind`, handed to the backend's `raw_text/4`:
    %% the neutralization a value needs belongs to *that element's* tokenizer state
    %% (`<script>` is script data, `<style>` is RAWTEXT), not to raw text at large.
    raw_text_tag = undefined :: atom(),
    %% The static text following the child being compiled, up to the next dynamic,
    %% while inside a `raw` raw-text element (static_run_after/2). With the static
    %% before it (static_run_before/1) it goes to `raw_text/4`: a breakout can
    %% straddle a slot and its static neighbour.
    raw_text_after = <<>> :: binary(),
    %% Content context of the enclosing element while compiling its children
    %% (Backend:content_context/2). `foreign` is inside `<svg>`, where an element
    %% is ordinary parsed content, so a tag HTML treats as escapable raw text
    %% (`title`) keeps its markers and stays diffable.
    content_ctx = html :: arizona_renderer:content_context(),
    module = undefined :: module() | undefined,
    live_render = false :: boolean(),
    root = false :: boolean(),
    %% What the element being compiled emits for its own `az-nodiff` directive
    %% (see nodiff_mark/3): its token, the bare attribute, or nothing.
    nodiff_mark = none :: none | bare | token,
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
    Errors :: [{file:filename(), [{erl_anno:location(), module(), term()}]}].
parse_transform(Forms, _Options) ->
    File = extract_file(Forms),
    Module = extract_module(Forms),
    IsLive = has_behaviour(Forms, arizona_stateful),
    FunDefs = collect_fun_defs(Forms),
    %% `az-*` names are collected in the process dictionary rather than threaded
    %% through `#state{}`: the state is per-template, and this set spans every
    %% template in the module. Erased on entry so a previous module in the same
    %% compiler process cannot leak into this one.
    _ = erase(?AZ_ATTRS_KEY),
    _ = erase(?FOLDED_KEY),
    undefined = put(?FOLDED_KEY, #{}),
    _ = put(?PARAM_SEQ_KEY, 0),
    _ = put(?HOIST_SITE_KEY, 0),
    try
        Transformed = map_forms_in_file(
            fun(Form) -> transform_form(mark_targets(Form, none), Module, IsLive, FunDefs) end,
            File,
            Forms
        ),
        WithSuppressions = inject_each_callback_suppressions(Transformed, Forms, FunDefs, Module),
        WithEvents = inject_az_attrs(WithSuppressions, collect_dep_mods(Module, Forms)),
        Reverted = map_forms_in_file(
            fun(Form) ->
                ok = assert_no_nodiff_call(Form),
                Form
            end,
            File,
            erl_syntax:revert_forms(WithEvents)
        ),
        with_warnings(Reverted, latin1_warnings(Forms, erase(?FOLDED_KEY)))
    catch
        throw:{arizona_parse_error, ErrorFile, Anno, Reason} ->
            _ = erase(?FOLDED_KEY),
            {error, [{ErrorFile, [{erl_anno:location(Anno), ?MODULE, Reason}]}], []}
    after
        _ = erase(?PARAM_SEQ_KEY),
        _ = erase(?HOIST_SITE_KEY)
    end.

%% Apply Fun to each form, attributing a parse error it raises to the file the form
%% came from: epp marks every include boundary with a `-file` attribute, so a template
%% in an included header reports the header (and its own line numbers), not the module.
%% An error in an inlined body names the file stamped on it by collect_fun_defs/1.
map_forms_in_file(Fun, File0, Forms) ->
    {Mapped, _File} = lists:mapfoldl(
        fun
            ({attribute, _, file, {File, _}} = Form, _Current) ->
                {Form, File};
            (Form, File) ->
                try
                    {Fun(Form), File}
                catch
                    throw:{arizona_parse_error, Anno, Reason} ->
                        throw({arizona_parse_error, anno_file(Anno, File), Anno, Reason})
                end
        end,
        File0,
        Forms
    ),
    Mapped.

anno_file(Anno, Default) ->
    case erl_anno:file(Anno) of
        undefined -> Default;
        File -> File
    end.

with_warnings(Forms, []) ->
    Forms;
with_warnings(Forms, Warnings) ->
    {warning, Forms, Warnings}.

%% A bare `<<"...">>` holding a character above 127 compiles to Latin-1 bytes,
%% one per character, while every render target writes UTF-8. A template static
%% is safe: the transform folds it as UTF-8 text (extract_bin_element/1), so
%% `{p, [], [<<"café">>]}` renders `café`. Anywhere else the bytes reach the
%% output as they are -- mojibake over HTTP, a JSON encoder failure over the
%% WebSocket -- or are compared against UTF-8 input that can never equal them.
%% Re-encoding those as well is not safe: the literal may be data (a pattern, a
%% protocol constant, a value compared against other Latin-1 bytes), and the
%% transform cannot tell which. So it warns, and leaves the bytes alone.
%%
%% The folded literals are recorded by extract_binary_value/1 and excluded here,
%% keyed by location and annotation-free shape: a hoisted variable or a local
%% helper spliced into a static position folds the very node the source holds.
latin1_warnings(Forms, Folded) ->
    {_File, Acc} = lists:foldl(
        fun
            ({attribute, _, file, {File, _}}, {_Current, Acc0}) ->
                {File, Acc0};
            (Form, {Current, Acc0}) ->
                {Current, latin1_literals(Form, Current, Folded, Acc0)}
        end,
        {"nofile", []},
        Forms
    ),
    group_by_file(lists:reverse(Acc)).

latin1_literals({bin, Anno, Elements} = Bin, File, Folded, Acc) when is_list(Elements) ->
    case is_latin1_literal(Elements) andalso not is_map_key(folded_key(Bin), Folded) of
        true -> [{File, {erl_anno:location(Anno), ?MODULE, latin1_binary_literal}} | Acc];
        false -> Acc
    end;
latin1_literals(Node, File, Folded, Acc) when is_tuple(Node) ->
    latin1_literals(tuple_to_list(Node), File, Folded, Acc);
latin1_literals([H | T], File, Folded, Acc) ->
    latin1_literals(T, File, Folded, latin1_literals(H, File, Folded, Acc));
latin1_literals(_Node, _File, _Folded, Acc) ->
    Acc.

%% A string element with no type spec is stored one byte per character; only a
%% character above 127 makes that differ from its UTF-8 encoding.
is_latin1_literal(Elements) ->
    lists:any(
        fun
            ({bin_element, _, {string, _, Chars}, default, default}) ->
                lists:any(fun(C) -> C > 127 end, Chars);
            (_Element) ->
                false
        end,
        Elements
    ).

folded_key({bin, Anno, _} = Bin) ->
    {erl_anno:location(Anno), erl_parse:map_anno(fun(_) -> 0 end, Bin)}.

group_by_file(Warnings) ->
    Files = lists:usort([File || {File, _W} <- Warnings]),
    [{File, [W || {F, W} <- Warnings, F =:= File]} || File <- Files].

%% An inherited constructor call (inherit_nodiff/1) that no compile path reached
%% would otherwise reach the runtime as an `undef` on a module that does not exist.
assert_no_nodiff_call({call, L, {remote, _, {atom, _, ?NODIFF_MOD}, _}, _}) ->
    parse_error(nodiff_call_not_compiled, L);
assert_no_nodiff_call(Node) when is_tuple(Node) ->
    assert_no_nodiff_call(tuple_to_list(Node));
assert_no_nodiff_call(Nodes) when is_list(Nodes) ->
    lists:foreach(fun assert_no_nodiff_call/1, Nodes);
assert_no_nodiff_call(_Node) ->
    ok.

%% Map every `{Name, Arity}` to its clause list, so a `?each(fun Name/Arity, _)`
%% callback can be resolved to its body and inlined like an anonymous fun. Each
%% clause's annotations carry the file the function is written in: an inlined body is
%% compiled inside the CALLER's form, so an error in it would otherwise name the
%% caller's file at the definition's line -- wrong whenever one of them sits in an
%% included header.
collect_fun_defs(Forms) ->
    {Defs, _File} = lists:foldl(
        fun
            ({attribute, _, file, {File, _}}, {Acc, _Current}) ->
                {Acc, File};
            ({function, _, Name, Arity, Clauses}, {Acc, File}) ->
                Stamp = fun(Anno) -> erl_anno:set_file(File, Anno) end,
                {Acc#{{Name, Arity} => [erl_parse:map_anno(Stamp, C) || C <- Clauses]}, File};
            (_Form, Acc) ->
                Acc
        end,
        {#{}, extract_file(Forms)},
        Forms
    ),
    Defs.

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
                {inline, _Params, _Body, _Wrap} -> true;
                _ -> false
            end
        end,
        [html, native, terminal]
    ),
    case Inlineable of
        true -> Acc#{{Name, Arity} => true};
        false -> Acc
    end.

%% Scan the original forms (pre-mark_targets -- which unwraps an inline
%% `?html`/`?native`/`?terminal` callback body, but never rewrites a
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

%% Emit the `az-*` attribute names this module compiled, so the client can
%% delegate exactly those DOM event types. The names are compile-time literals
%% here (`compile_attr` resolves every one through `Backend:name/1` or a binary),
%% which is why this belongs at compile time: recovering them later means a regex
%% over serialized HTML, which cannot tell an attribute NAME from `az-` text
%% inside an attribute VALUE, and is wrong in both directions.
inject_az_attrs(Forms, Deps) ->
    Names = lists:usort(
        case erase(?AZ_ATTRS_KEY) of
            undefined -> [];
            L -> L
        end
    ),
    case {Names, Deps} of
        {[], []} ->
            Forms;
        _ ->
            {Before, [ModAttr | After]} = lists:splitwith(
                fun(Form) -> not is_module_attr(Form) end, Forms
            ),
            Anno = element(2, ModAttr),
            Attrs =
                [{attribute, Anno, arizona_az_attrs, Names} || Names =/= []] ++
                    [{attribute, Anno, arizona_az_deps, Deps} || Deps =/= []],
            Before ++ [ModAttr | Attrs] ++ After
    end.

%% The modules this module can take markup from, as literal atoms: the component
%% modules its templates name, and every module it calls, imports, or takes a
%% `fun Mod:F/A` of by a literal name.
%%
%% `?stateful(Mod, _)` and `?stateless(Mod, Fun, _)` pass the module as DATA to
%% `arizona_template`, never as a call, so it does not survive into the compiled
%% artifact -- `beam_lib` imports for a module rendering two components report only
%% `[arizona_template, erlang]`. Compile time is the only moment the graph is
%% visible, which is why it is recorded here rather than recovered later.
%%
%% A remote helper's template (`other_mod:button()`) carries that module's FOLDED
%% names in its statics, which no render ever re-proves, so only the walk can
%% deliver them. Which calls return markup is not decidable here (a mount may store
%% the template in bindings for a later `?get`), so every literal callee is
%% recorded; the walk dead-ends on a module carrying no attributes, so a
%% non-template callee costs it one `module_info/1` read.
%%
%% A module reached only through a variable (`?stateful(Mod, _)` or `Mod:f()` with
%% `Mod` bound at runtime) is not resolvable and is not recorded.
collect_dep_mods(Module, Forms) ->
    lists:usort([
        Mod
     || Mod <- lists:flatten([dep_mods(Form) || Form <- Forms]),
        Mod =/= Module,
        Mod =/= arizona_template,
        Mod =/= az
    ]).

dep_mods({call, _, {remote, _, {atom, _, Mod}, {atom, _, Fun}}, Args}) when
    Mod =:= arizona_template orelse Mod =:= az
->
    component_mod_args(Fun, Args) ++ [dep_mods(A) || A <- Args];
dep_mods({call, _, {remote, _, {atom, _, Mod}, _Fun}, Args}) ->
    [Mod | [dep_mods(A) || A <- Args]];
dep_mods({'fun', _, {function, {atom, _, Mod}, _Fun, _Arity}}) ->
    [Mod];
dep_mods({attribute, _, import, {Mod, _Funs}}) ->
    [Mod];
dep_mods(Tuple) when is_tuple(Tuple) ->
    [dep_mods(E) || E <- tuple_to_list(Tuple)];
dep_mods(List) when is_list(List) ->
    [dep_mods(E) || E <- List];
dep_mods(_Other) ->
    [].

%% `stateful(Mod, Props)` and `stateless(Mod, Fun, Props)` name the module first;
%% `stateless(fun Mod:Fun/1, Props)` names it inside the fun reference, which the
%% recursion into the arguments records. Every other shape resolves within this
%% module, or not at all.
component_mod_args(stateful, [{atom, _, Mod} | _]) -> [Mod];
component_mod_args(stateless, [{atom, _, Mod}, {atom, _, _Fun} | _]) -> [Mod];
component_mod_args(_Fun, _Args) -> [].

%% Does the expression contain a command-builder call? The builders are the only
%% functions that produce `{arizona_effect, _}` tuples, so a call to one anywhere
%% in an attribute value is proof the value is a command and not app data. The
%% converse is settled at render time (see the dynamic-attr clause of
%% `compile_attr`), never guessed from shape.
has_builder_call({call, _, {remote, _, {atom, _, Mod}, _}, _}) when
    Mod =:= arizona_js; Mod =:= arizona_android; Mod =:= arizona_os
->
    true;
has_builder_call(Tuple) when is_tuple(Tuple) ->
    has_builder_call(tuple_to_list(Tuple));
has_builder_call([Head | Tail]) ->
    has_builder_call(Head) orelse has_builder_call(Tail);
has_builder_call(_Other) ->
    false.

%% The two recording levels for the declared `az-*` set, called from
%% `compile_attr`, the single point every attribute name resolves through.
%%
%% `record_az_attr` records a name proven to carry commands, so the client
%% delegates its DOM event type. `record_directive_attr` records nothing but
%% `az-prevent-default`: a command needs a value, so the bare, boolean, and
%% static-value forms are data or directives, and the only directive whose
%% presence the client reads out of the declared set rather than off the element
%% is `az-prevent-default` -- passive is fixed at listener registration, before
%% any element exists to look at, and a name left out of the set silently
%% disarms the `preventDefault` it asked for.
record_directive_attr(~"az-prevent-default" = NameBin) ->
    record_az_attr(NameBin);
record_directive_attr(NameBin) ->
    NameBin.

record_az_attr(<<"az-", _/binary>> = NameBin) ->
    put(?AZ_ATTRS_KEY, [
        NameBin
        | case get(?AZ_ATTRS_KEY) of
            undefined -> [];
            L -> L
        end
    ]),
    NameBin;
record_az_attr(NameBin) ->
    NameBin.

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
%%   1. A single `?each` serves every target. Each one is marked with the backend
%%      of the target it sits in (the default `html` one standalone) and the
%%      content context there, so the bottom-up transform compiles it with both.
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
    Default = root_content_context(Target),
    {call, L, {remote, RL, {atom, ML, Mod}, {atom, FL, Target}}, [
        maybe_inherit_nodiff(mark_targets(Arg, Target, Default))
    ]};
%% An `?each` gets the backend and content context of where it sits appended as
%% two bare atom args, `each(Fun, Source, Backend, CCtx)` -- bare, because a tuple
%% there would be walked as an element. It is marked once, where it appears in the
%% template that renders it: a hoisted one where its variable is used (splice_vars/4),
%% and a helper's body never re-marks the arguments it is handed (ih_call/7). Those
%% arguments may carry the 2-arg compiled output, which is final.
mark_targets(
    {call, L, {remote, _, {atom, _, Mod}, {atom, _, each}} = Callee, [FunArg, SourceArg] = Args},
    Ctx,
    CCtx
) when
    Mod =:= arizona_template orelse Mod =:= az
->
    case is_compiled_each_pairing(SourceArg) of
        true ->
            {call, L, Callee, mark_targets(Args, Ctx, CCtx)};
        false ->
            Backend = ctx_backend(Ctx),
            Unwrapped = unwrap_each_body([FunArg, SourceArg], Backend:target()),
            Marked = maybe_inherit_each_nodiff([mark_targets(A, Ctx, CCtx) || A <- Unwrapped]),
            {call, L, Callee, Marked ++ [{atom, L, Backend}, {atom, L, CCtx}]}
    end;
%% An element tuple: its children may sit in a different content context (the
%% backend decides -- `<svg>` opens foreign content for `?html`). Matched on the
%% element shape only, so a plain data tuple a user writes cannot shift it.
mark_targets({tuple, L, [{atom, _, Tag}, _Attrs] = Parts}, Ctx, CCtx) ->
    mark_element_children(L, Tag, Parts, Ctx, CCtx);
mark_targets({tuple, L, [{atom, _, Tag}, _Attrs, _Children] = Parts}, Ctx, CCtx) ->
    mark_element_children(L, Tag, Parts, Ctx, CCtx);
%% A bare element in a control-flow tail compiles into its own template, exactly
%% as a literal `?html` there would (expand_element_leaf/4), so it takes the same
%% nodiff inheritance the `?html` clause above gives a wrapped one. Only inside a
%% template: outside one (`none`) a tail is plain data -- `{K, [V]}` is a pair,
%% not an element -- and nothing compiles it.
mark_targets(Node, Ctx, CCtx) when is_tuple(Node) ->
    Marked = list_to_tuple([mark_targets(E, Ctx, CCtx) || E <- tuple_to_list(Node)]),
    case Ctx =/= none andalso is_control_flow_ast(Node) of
        true -> map_tail_exprs(Marked, fun maybe_inherit_nodiff/1, fun(E) -> E end);
        false -> Marked
    end;
mark_targets(Nodes, Ctx, CCtx) when is_list(Nodes) ->
    [mark_targets(E, Ctx, CCtx) || E <- Nodes];
mark_targets(Node, _Ctx, _CCtx) ->
    Node.

mark_element_children(L, Tag, Parts, Ctx, CCtx) ->
    ChildCCtx = (ctx_backend(Ctx)):content_context(Tag, CCtx),
    {tuple, L, [mark_targets(P, Ctx, ChildCCtx) || P <- unwrap_child_tail_wrappers(Parts, Ctx)]}.

%% A whole-tail `?html`/`?native`/`?terminal` (matching the enclosing target) in a
%% conditional child is the bare element in a wrapper: unwrap it here, before the
%% bottom-up pass compiles it at the root content context, so the branch compiles in
%% the context of the slot holding it exactly as the bare form does (an SVG `<title>`
%% stays diffable). Only markup is unwrapped: a wrapped value list renders its binaries
%% as raw statics, which a bare value tail would escape. A wrapper anywhere else (bound,
%% passed, or below the tail) keeps starting its own template.
unwrap_child_tail_wrappers([Tag, Attrs, Children], Ctx) when
    Ctx =:= html; Ctx =:= native; Ctx =:= terminal
->
    Unwrap = fun(Child) ->
        case is_control_flow_ast(Child) of
            true ->
                map_tail_exprs(Child, fun(Leaf) -> unwrap_tail_wrapper(Leaf, Ctx) end, fun(E) ->
                    E
                end);
            false ->
                Child
        end
    end,
    Children1 =
        case is_list_ast(Children) of
            true -> map_cons_items(Unwrap, Children);
            false -> Unwrap(Children)
        end,
    [Tag, Attrs, Children1];
unwrap_child_tail_wrappers(Parts, _Ctx) ->
    Parts.

unwrap_tail_wrapper(
    {call, _, {remote, _, {atom, _, Mod}, {atom, _, Ctx}}, [Inner]} = Leaf, Ctx
) when
    Mod =:= arizona_template; Mod =:= az
->
    case classify_body(Inner) of
        Class when Class =:= element_tuple; Class =:= element_list -> Inner;
        list_ast ->
            case list_has_markup_item(Inner) of
                true -> Inner;
                false -> Leaf
            end;
        _ ->
            Leaf
    end;
unwrap_tail_wrapper(Leaf, _Ctx) ->
    Leaf.

map_cons_items(F, {cons, L, Head, Tail}) ->
    {cons, L, F(Head), map_cons_items(F, Tail)};
map_cons_items(_F, Tail) ->
    Tail.

%% The backend for a marking context. `none` (no target entered yet) uses the
%% default target's backend so the two callbacks above always have an answer.
ctx_backend(none) -> target_backend(html);
ctx_backend(Ctx) -> target_backend(Ctx).

%% A hoisted value is bound outside every template, so an `?each` in it was marked
%% with the default target and context, which say nothing about where it renders.
%% prepare_inline/2 strips that marking and splice_vars/4 marks it again for the
%% position the value is spliced into.
unmark_eaches(
    {call, L, {remote, _, {atom, _, Mod}, {atom, _, each}} = Callee, [FunArg, SourceArg, _, _]}
) when
    Mod =:= arizona_template orelse Mod =:= az
->
    {call, L, Callee, [unmark_eaches(FunArg), unmark_eaches(SourceArg)]};
unmark_eaches(Node) when is_tuple(Node) ->
    list_to_tuple(unmark_eaches(tuple_to_list(Node)));
unmark_eaches(Nodes) when is_list(Nodes) ->
    [unmark_eaches(N) || N <- Nodes];
unmark_eaches(Node) ->
    Node.

%% Splice the hoisted values into `Expr`, then mark the `?each`es they carry for
%% the target `Ctx` and content context `CCtx` of where they land.
splice_vars(Expr, Inline, _Ctx, _CCtx) when map_size(Inline) =:= 0 ->
    Expr;
splice_vars(Expr, Inline, Ctx, CCtx) ->
    mark_targets(inline_vars(Expr, Inline), Ctx, CCtx).

%% The content context at the root of a `Target` template.
root_content_context(Target) ->
    (target_backend(Target)):content_context(undefined, html).

mark_targets(Node, Ctx) ->
    mark_targets(Node, Ctx, (ctx_backend(Ctx)):content_context(undefined, html)).

%% A template renders its nested templates (a conditional branch, an `?each` item,
%% a template passed as a `?stateless` prop) inside its own output, so under
%% `az-nodiff` they render once too and their markers are bytes no op ever targets.
%% Re-spell every constructor call in a nodiff template's argument so the bottom-up
%% transform compiles it nodiff as well. Two places are left as written, because
%% what they build can end up in a live view's diffed slot, where a `diff => false`
%% value keeps its first render: a `?stateful` child's props, and any fun other than an
%% `?each` callback (a fun prop is called later, by whichever view receives it).
maybe_inherit_nodiff(Arg) ->
    case template_is_nodiff(Arg) of
        true -> inherit_nodiff(Arg);
        false -> Arg
    end.

inherit_nodiff({call, _, {remote, _, {atom, _, Mod}, {atom, _, stateful}}, _} = Call) when
    Mod =:= arizona_template orelse Mod =:= az
->
    Call;
inherit_nodiff({call, L, {remote, RL, {atom, ML, Mod}, {atom, FL, F}}, Args} = Call) when
    Mod =:= arizona_template orelse Mod =:= az
->
    case is_template_ctor(F, Args) of
        true ->
            Remote = {remote, RL, {atom, ML, ?NODIFF_MOD}, {atom, FL, F}},
            {call, L, Remote, inherit_ctor_args(Args)};
        false ->
            list_to_tuple(inherit_nodiff(tuple_to_list(Call)))
    end;
inherit_nodiff({'fun', _, _} = Fun) ->
    Fun;
inherit_nodiff({named_fun, _, _, _} = Fun) ->
    Fun;
inherit_nodiff(Node) when is_tuple(Node) ->
    list_to_tuple(inherit_nodiff(tuple_to_list(Node)));
inherit_nodiff(Nodes) when is_list(Nodes) ->
    [inherit_nodiff(N) || N <- Nodes];
inherit_nodiff(Node) ->
    Node.

inherit_ctor_args([FunArg, SourceArg | Marking]) ->
    [inherit_each_fun(FunArg), inherit_nodiff(SourceArg) | Marking];
inherit_ctor_args(Args) ->
    inherit_nodiff(Args).

%% An `?each` callback is compiled into the per-item template on the spot, so
%% unlike any other fun its body renders inside the enclosing template.
inherit_each_fun({'fun', L, {clauses, Clauses}}) ->
    Inherited = [{clause, CL, Ps, Gs, inherit_nodiff(B)} || {clause, CL, Ps, Gs, B} <:- Clauses],
    {'fun', L, {clauses, Inherited}};
inherit_each_fun(FunRef) ->
    FunRef.

%% An `?each` item body is a template of its own, so `az-nodiff` on the item's
%% element reaches the templates nested in it the same way.
maybe_inherit_each_nodiff([{'fun', _, {clauses, [{clause, _, _, _, Body}]}} = Fun | Rest]) ->
    {_Prefix, Last} = split_fun_body(Body),
    case template_is_nodiff(Last) of
        true -> [inherit_each_fun(Fun) | Rest];
        false -> [Fun | Rest]
    end;
maybe_inherit_each_nodiff(Args) ->
    Args.

%% Material inline_vars/2 and inline_helper_calls/4 splice into a template after
%% the pre-pass ran -- a hoisted `?each`, a helper body -- reaches a nodiff
%% template unmarked, so it is re-spelled here, before retransform_spliced/4
%% compiles it.
inherit_spliced_nodiff(Inherited, Arg) ->
    case Inherited orelse template_is_nodiff(Arg) of
        true -> inherit_nodiff(Arg);
        false -> Arg
    end.

%% An inherited `?each` renders its source's templates as item content, so a
%% hoisted source is inherited like an inline one (inherit_ctor_args/1).
inherit_spliced_source(true, SourceArg) ->
    inherit_nodiff(SourceArg);
inherit_spliced_source(false, SourceArg) ->
    SourceArg.

inherit_spliced_each_nodiff(true, FunArg) ->
    inherit_each_fun(FunArg);
inherit_spliced_each_nodiff(false, FunArg) ->
    [FunArg1] = maybe_inherit_each_nodiff([FunArg]),
    FunArg1.

%% A not-yet-compiled template constructor. A compiled `?each` keeps the `each`
%% name with two args, and is already final; a marked one carries four.
is_template_ctor(F, [_Arg]) ->
    F =:= html orelse F =:= native orelse F =:= terminal;
is_template_ctor(each, [_FunArg, _SourceArg, _Backend, _CCtx]) ->
    true;
is_template_ctor(_F, _Args) ->
    false.

%% Whether a constructor's argument compiles as a nodiff template: the same
%% classification and directive prescan compile_body_parts/6 applies to it.
template_is_nodiff(Arg) ->
    case classify_body(Arg) of
        element_tuple ->
            maps:is_key(diff, prescan_directives([Arg]));
        Class when Class =:= element_list; Class =:= list_ast ->
            maps:is_key(diff, prescan_directives(ast_list_to_list(Arg)));
        _ ->
            false
    end.

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
format_error(computed_element_tag) ->
    "an element's tag must be a literal atom. The tag is baked into the template's "
    "statics at compile time, which is what lets a later change ship only the values, "
    "so a tag chosen at runtime has no single set of statics to bake. Put the CHOICE "
    "inside a stable element instead of choosing the element: {'div', [], [case Kind "
    "of note -> {'em', [], [X]}; _ -> {'strong', [], [X]} end]} -- a conditional in a "
    "content slot may return bare element tuples, so the branches can differ "
    "completely. Inside an ?each the stable element is the item itself and carries "
    "az_key, which is also what keeps its identity across a change: "
    "fun(Item, Key) -> {li, [{az_key, Key}], [case Item of ... end]} end";
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
%% The authored-attribute errors live in their own function, like the ?each family
%% below, to keep format_error/1 under the cyclomatic limit.
format_error({Kind, Name}) when Kind =:= reserved_attr; Kind =:= duplicate_attr ->
    format_attr_error(Kind, Name);
format_error({invalid_child, ValueStr}) ->
    lists:flatten(
        io_lib:format(
            "invalid child: static tuple is not a valid template child "
            "-- use a binary, element, or dynamic expression. "
            "Got: ~s",
            [ValueStr]
        )
    );
format_error({markup_var_in_content_list, Var}) ->
    lists:flatten(
        io_lib:format(
            "~s is bound to markup and used as an item of a content-slot list. The "
            "transform compiles a list's markup only where it is written in the list, "
            "so the variable is a plain value there and fails at render. Put the "
            "markup directly in the children list instead of binding it first",
            [Var]
        )
    );
format_error(local_key_not_literal) ->
    "?local/2 key must be a literal binary or atom";
format_error(nodiff_call_not_compiled) ->
    "internal: a template constructor inside an az-nodiff template was not compiled "
    "-- please report this as an Arizona bug";
format_error(local_unsupported) ->
    "?local is not supported by this render target";
format_error(dynamic_in_raw_text) ->
    "a dynamic content slot inside <script>/<style> (or another raw-text element: <xmp>, "
    "<iframe>, <noembed>, <noframes>) is spliced verbatim -- the browser "
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
    "(script/style/textarea/title/xmp/iframe/noembed/noframes) -- raw-text content "
    "carries no slot markers "
    "for the client to address, so the value would never update";
format_error(local_key_reused) ->
    "a ?local key cannot bind both content and an attribute on the same element";
format_error(local_orphaned) ->
    "?local must sit inside an element -- as a content child or an attribute "
    "value. At a fragment top level, as a whole template body, or as a bare "
    "conditional-branch value there is no enclosing element to carry the "
    "az-local descriptor, so the client could never bind the key and "
    "set/set_all would be silent no-ops";
%% The helper_* rejection reasons and template_impure_expr are the only 3-tuple
%% reasons; dispatch to format_helper_error/3 to keep format_error/1's clause
%% complexity in check.
format_error({HelperReason, Name, Arity}) ->
    format_helper_error(HelperReason, Name, Arity);
format_error(markup_in_raw_text) ->
    "a <title> or <textarea> holds text only (escapable raw text): the browser shows "
    "an ?each, a ?stateless/?stateful component or a nested template there as literal "
    "markup. Render the value as text instead";
format_error(local_attr_multiple) ->
    "an attribute value can interpolate at most one ?local -- multiple ?local in "
    "one attribute can't be recomposed client-side";
format_error(local_attr_mixed) ->
    "a ?local in an attribute value can only be combined with static text, not "
    "another dynamic expression";
format_error(nodiff_value) ->
    "az-nodiff is a bare directive and takes no value -- write 'az-nodiff' (or "
    "az_nodiff / <<\"az-nodiff\">>) alone in the attribute list. The value it carries "
    "in the rendered HTML belongs to the compiler: it names the az-nodiff element so the client "
    "can keep it across a re-render, and an authored one would be read as that name";
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
%% The `?each` callback errors live in their own function: they are a related family
%% and, together, they carried `format_error/1` past the cyclomatic limit.
format_error(Reason) when
    Reason =:= each_body_not_element;
    Reason =:= each_stream_body_not_element;
    Reason =:= each_stream_body_control_flow;
    Reason =:= each_named_fun_multi_clause;
    Reason =:= each_named_fun_undefined;
    Reason =:= each_remote_fun_ref
->
    format_each_error(Reason);
format_error({hoisted_var_captured, Kind, Binder, Hoisted}) ->
    lists:flatten(
        io_lib:format(
            "~s ~s shadows the ~s that hoisted ~s reads; rename it. ~s is re-read inside "
            "the template so it stays tracked, and there ~s would name the inner binding",
            [binder_kind(Kind), Binder, Binder, Hoisted, Hoisted, Binder]
        )
    );
format_error(Reason) when
    Reason =:= tracked_get_on_non_bindings_map;
    Reason =:= latin1_binary_literal;
    Reason =:= improper_template_list
->
    format_source_error(Reason).

%% Diagnostics about how a value is written in the source rather than the template's
%% structure. Kept apart for the same reason as format_each_error/1.
format_source_error(latin1_binary_literal) ->
    "non-ASCII binary literal is encoded as latin1; write ~\"...\" or <<\"...\"/utf8>>. "
    "For intended bytes, spell them so they do not warn: integer segments "
    "(<<16#89, \"PNG\">>) or a sized string (<<\"\\351\":8>>)";
format_source_error(improper_template_list) ->
    "a template's attribute, child and body lists must be proper list literals: "
    "their items are laid out one slot each at compile time, so a list ending in "
    "an expression ([A, B | Rest]) has no length to lay out. Write every item in "
    "the list; render a runtime list of children with ?each, as one item of it: "
    "{'ul', [], [{'li', [], [~\"first\"]}, ?each(fun(I) -> {'li', [], [I]} end, Rest)]}";
format_source_error(tracked_get_on_non_bindings_map) ->
    "arizona_template:get/get_lazy/with (and the az: aliases) track every read against "
    "the view bindings, so their map argument must be the bindings. This local holds a "
    "value read out of a map, so its keys would be recorded as spurious top-level "
    "dependencies; read it with maps:get/2 (`User = arizona_template:get(user, Bindings), "
    "Name = maps:get(name, User)`)".

format_attr_error(reserved_attr, Name) ->
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
format_attr_error(duplicate_attr, Name) ->
    lists:flatten(
        io_lib:format(
            "the ~ts attribute is written twice on one element (names compare the way "
            "the render target reads them). Both copies are emitted, and which one takes "
            "effect is up to the target and to which of them an update patches, so the "
            "output stops matching the template. Write it once; to combine values, use "
            "one list value, e.g. {class, [~~\"btn \", ?get(kind)]}",
            [Name]
        )
    ).

binder_kind(fun_param) -> "fun parameter";
binder_kind(named_fun) -> "named fun";
binder_kind(generator) -> "generator pattern variable".

format_each_error(each_body_not_element) ->
    "an ?each callback over a list must return an element ({Tag, Attrs, Children}) or a "
    "list of elements -- ?each builds a per-item template for fine-grained diffing, so a "
    "single-value body defeats its purpose (and a template or descriptor value crashes on "
    "the first diff, keyed by to_bin of the first dynamic). For a list of plain values use a "
    "list comprehension or lists:map/2. For a conditional, put it inside an element as a "
    "text/value child: {li, [], [case ... of ... end]}";
format_each_error(each_stream_body_not_element) ->
    "an ?each callback over a stream or map (a 2-arg fun) must return an element ({Tag, "
    "Attrs, Children}); a map source may also take a list of elements or a static/mixed "
    "fragment -- a stream/map keys each item for per-item diffing, which a single-value "
    "body throws away. Unlike a list there is no comprehension fallback (a comprehension "
    "has no stream/keyed semantics): wrap the value in an element, e.g. "
    "fun(Item, Key) -> {li, [{az_key, Key}], [Item]} end";
format_each_error(each_stream_body_control_flow) ->
    "an ?each callback over a stream or map (a 2-arg fun) must return an element, and a "
    "control-flow body (case, if, begin, try, receive) is not one: every item shares one "
    "compiled per-item template (statics once, dynamics per item), so an item's outer tag "
    "is fixed at compile time. The branches can still be entirely different elements -- "
    "put them inside one stable item element carrying az_key, e.g. fun(Item, Key) -> "
    "{li, [{az_key, Key}], [case Item of ... end]} end, where a conditional child may "
    "return bare element tuples (<em> vs <strong> vs a whole subtree all work), and a div "
    "with display: contents keeps the wrapper out of layout. NOT inside table/tr or "
    "select: the parser foster-parents a wrapper out of a table and Firefox drops one "
    "inside a select, so the keyed element lands where the client cannot address it. A "
    "varying sibling tag is legitimate in exactly those two (th/td, option/optgroup) and "
    "has no workaround today";
format_each_error(each_named_fun_multi_clause) ->
    "an ?each callback given as a local fun reference (fun name/1 or fun name/2) must have a "
    "single clause -- ?each inlines the function's body into one shared per-item template, so "
    "multiple clauses (which would select different per-item structures) can't be compiled to "
    "a single template. Collapse them into one clause with a case inside the returned element: "
    "name(I) -> {li, [], [case I of ... end]}";
format_each_error(each_named_fun_undefined) ->
    "the ?each callback references a local fun (fun name/1 or fun name/2) that is not defined "
    "in this module. Define it as a single-clause function returning an element, or inline the "
    "callback (fun(I) -> {li, [], [...]} end)";
format_each_error(each_remote_fun_ref) ->
    "an ?each callback cannot be a remote fun reference (fun mod:name/arity) -- ?each inlines "
    "the callback body to build a per-item template, which is impossible across a module "
    "boundary. Inline it (fun(I) -> {li, [], [...]} end), or move the body into a single-clause "
    "local function and pass fun name/1".

format_impure_error({hoisted, Var}, What) ->
    lists:flatten(
        io_lib:format(
            "~s contains ~s, but ~s is defined outside the template and reads a binding, so "
            "the transform re-inlines its definition into every slot that uses it to keep "
            "it tracked: it runs once in the function body and again in each of those "
            "slots, on first render and whenever that slot's dependencies change. Each run "
            "gets its own result, so two places reading ~s disagree. ~s",
            [Var, What, Var, Var, impure_remedy()]
        )
    );
format_impure_error({helper_arg, Name, Arity, Param, Uses}, What) ->
    lists:flatten(
        io_lib:format(
            "the argument for parameter ~s of local element helper ~ts/~p contains ~s. The "
            "helper is inlined by substituting the argument at each use of its parameter, "
            "~s. ~s",
            [Param, Name, Arity, What, helper_arg_runs(Uses), impure_remedy()]
        )
    ).

helper_arg_runs(0) ->
    "and the parameter is unused, so the argument never runs at all";
helper_arg_runs(Uses) ->
    io_lib:format(
        "and the parameter is used ~p times, so the argument runs once per use and the "
        "uses disagree",
        [Uses]
    ).

impure_remedy() ->
    "Template code must be a pure function of the bindings. Generate the value once and "
    "keep it in state -- create it in mount/1 or handle_event/3 and read it with ?get -- "
    "or derive it from bindings, e.g. <<\"field-\", (?get(name))/binary>>".

format_helper_error(template_impure_expr, Where, What) ->
    format_impure_error(Where, What);
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
    );
format_helper_error(helper_param_in_each_guard, Name, Arity) ->
    lists:flatten(
        io_lib:format(
            "local element helper ~ts/~p reads a parameter in the head or guard "
            "of an ?each callback -- the callback must stay a fun literal, so "
            "the inlined argument has nowhere to be bound. Test it inside the "
            "item element (a case on the item) instead",
            [Name, Arity]
        )
    ).

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

-spec parse_error(term(), term()) -> no_return().
parse_error(Reason, Anno) ->
    throw({arizona_parse_error, Anno, Reason}).

%% `arizona_template:get`/`get_lazy`/`with` (and the `az:` aliases) call `track/1`
%% regardless of which map they read, so reading a sub-map records the inner key
%% as a top-level slot dependency. Reject a tracked read whose map argument is a
%% variable PROVABLY holding a value read out of a map: bound by `V = Read` where Read
%% is a tracked accessor or `maps:get/2,3` / `map_get/2`, bound to a value by a map
%% pattern (`#{user := U}` in a match, a function or fun head, a case clause), or
%% aliased from such a variable. Anything else may be the bindings -- a parameter, a value threaded
%% through a helper (`Bindings = bump(Bindings0)`), a `case` or `maps:merge` -- and
%% the transform cannot tell, so it is left alone: a false rejection blocks correct
%% code, a missed one only over-tracks. A non-variable map argument is never flagged.
check_tracked_get_targets(Patterns, Body) ->
    Params = collect_fun_param_vars(Body, collect_pattern_vars(Patterns, #{})),
    PatternValues = map_pattern_values({clause, 0, Patterns, [], Body}, #{}),
    walk_tracked_gets(Body, map_value_vars(Body, Params, PatternValues)).

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

%% The variables bound to a value read out of a map (`V = Read`, or a map pattern's
%% value), grown through `V = W` aliases to a fixpoint (`Foo = ?get(foo), F = Foo`).
%% A name that is also a parameter somewhere in the clause -- other than one a map
%% pattern binds to a value -- is dropped: the scan is flat across nesting depth, and
%% a dropped name only loses a flag, never adds one.
map_value_vars(Body, Params, PatternValues) ->
    Matches = collect_matches(Body, []),
    Reads = #{V => true || {match, _, {var, _, V}, Expr} <- Matches, is_map_value_read(Expr)},
    VarAliases = [{V, W} || {match, _, {var, _, V}, {var, _, W}} <- Matches],
    Exempt = maps:without(maps:keys(PatternValues), Params),
    Values = alias_fixpoint(VarAliases, maps:merge(Reads, PatternValues)),
    maps:without(maps:keys(Exempt), Values).

%% Every variable a map pattern binds to a value, in any pattern position: clause
%% heads (function, fun, case, receive, try), match left-hand sides and generators.
%% Only patterns are walked -- `M#{k := V}` in an expression is an update, not a binder.
map_pattern_values({clause, _, Patterns, _Guards, Body}, Acc) ->
    map_pattern_values(Body, pattern_map_values(Patterns, Acc));
map_pattern_values({match, _, Pattern, Expr}, Acc) ->
    map_pattern_values(Expr, pattern_map_values(Pattern, Acc));
map_pattern_values({Gen, _, Pattern, Expr}, Acc) when
    Gen =:= generate;
    Gen =:= generate_strict;
    Gen =:= b_generate;
    Gen =:= b_generate_strict;
    Gen =:= m_generate;
    Gen =:= m_generate_strict
->
    map_pattern_values(Expr, pattern_map_values(Pattern, Acc));
map_pattern_values(T, Acc) when is_tuple(T) ->
    map_pattern_values(tuple_to_list(T), Acc);
map_pattern_values([H | T], Acc) ->
    map_pattern_values(T, map_pattern_values(H, Acc));
map_pattern_values(_Leaf, Acc) ->
    Acc.

pattern_map_values({map_field_exact, _, _Key, Value}, Acc) ->
    collect_pattern_vars(Value, Acc);
pattern_map_values(T, Acc) when is_tuple(T) ->
    pattern_map_values(tuple_to_list(T), Acc);
pattern_map_values([H | T], Acc) ->
    pattern_map_values(T, pattern_map_values(H, Acc));
pattern_map_values(_Leaf, Acc) ->
    Acc.

is_map_value_read({call, _, {remote, _, {atom, _, Mod}, {atom, _, F}}, [_Key, _Map | _]}) when
    Mod =:= arizona_template; Mod =:= az
->
    F =:= get orelse F =:= get_lazy;
is_map_value_read({call, _, {remote, _, {atom, _, maps}, {atom, _, get}}, [_Key, _Map | _]}) ->
    true;
is_map_value_read({call, _, {remote, _, {atom, _, erlang}, {atom, _, map_get}}, [_, _]}) ->
    true;
is_map_value_read({call, _, {atom, _, map_get}, [_, _]}) ->
    true;
is_map_value_read(_Expr) ->
    false.

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

walk_tracked_gets(AST, MapValues) when is_tuple(AST) ->
    flag_tracked_get(AST, MapValues),
    walk_tracked_gets(tuple_to_list(AST), MapValues);
walk_tracked_gets([H | T], MapValues) ->
    walk_tracked_gets(H, MapValues),
    walk_tracked_gets(T, MapValues);
walk_tracked_gets(_, _MapValues) ->
    ok.

flag_tracked_get(
    {call, L, {remote, _, {atom, _, Mod}, {atom, _, F}}, [_Key, {var, _, V} | _]}, MapValues
) when
    (Mod =:= arizona_template orelse Mod =:= az) andalso
        (F =:= get orelse F =:= get_lazy orelse F =:= with) andalso
        is_map_key(V, MapValues)
->
    parse_error(tracked_get_on_non_bindings_map, L);
flag_tracked_get(_Node, _MapValues) ->
    ok.

line(Node) when is_tuple(Node), tuple_size(Node) >= 2 ->
    erl_anno:line(element(2, Node));
line(_) ->
    0.

%% The annotation a compile error reports: its `{Line, Column}` location (or the
%% line, when scanned without columns) and, for an inlined body, its file.
anno(Node) when is_tuple(Node), tuple_size(Node) >= 2 ->
    element(2, Node);
anno(_) ->
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
    case has_template_ctor(Body0) of
        true ->
            transform_template_clause({clause, L, Patterns, Guards, Body0}, Module, FunDefs);
        false ->
            %% Nothing here is a template a hoisted read could be inlined into, so
            %% the body keeps its own shape (and its own lint warnings).
            {clause, L, Patterns, Guards, [
                transform_expr(Expr, Module, #{}, FunDefs)
             || Expr <- Body0
            ]}
    end.

%% Does the body construct a template a hoisted variable can be spliced into? The
%% splice happens only while compiling one (transform_node/4), and the pre-pass
%% has already spelled an inherited constructor with ?NODIFF_MOD.
has_template_ctor({call, _, {remote, _, {atom, _, Mod}, {atom, _, F}}, _Args}) when
    (Mod =:= arizona_template orelse Mod =:= az orelse Mod =:= ?NODIFF_MOD) andalso
        (F =:= html orelse F =:= native orelse F =:= terminal orelse F =:= each)
->
    true;
has_template_ctor(T) when is_tuple(T) ->
    has_template_ctor(tuple_to_list(T));
has_template_ctor([H | T]) ->
    has_template_ctor(H) orelse has_template_ctor(T);
has_template_ctor(_Other) ->
    false.

transform_template_clause({clause, L, Patterns, Guards, Body0}, Module, FunDefs) ->
    Body = normalize_tail_binds(Body0),
    Inline = prepare_inline(rename_hoisted_binders(Patterns, Body, collect_inline(Body)), Body),
    ok = check_markup_vars(Body, Inline),
    {Init, [Last]} = lists:split(length(Body) - 1, Body),
    Transform = fun(Expr, Inl) -> transform_expr(Expr, Module, Inl, FunDefs) end,
    Body1 =
        [Transform(Expr, Inline) || Expr <- Init] ++
            [
                transform_scoped_tail(
                    Last, Inline, clause_bound(Patterns, Init), {Transform, Transform}
                )
            ],
    {clause, L, Patterns, Guards,
        suppress_unused_inline_matches(
            Body1, without_prebound(without_source_unused(Inline, Body, []), Patterns, Body)
        )}.

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
            (Mod =:= arizona_template orelse Mod =:= az orelse Mod =:= ?NODIFF_MOD) andalso
                (Target =:= html orelse Target =:= native orelse Target =:= terminal)
        ->
            Inherited = Mod =:= ?NODIFF_MOD,
            Arg1 = splice_vars(Arg, Inline, Target, root_content_context(Target)),
            {Arg2, HelperCh} = inline_helper_calls(Arg1, Module, FunDefs, Target),
            Arg3 = retransform_spliced(
                inherit_spliced_nodiff(Inherited, Arg2),
                map_size(Inline) > 0 orelse HelperCh,
                Module,
                FunDefs
            ),
            compile_template(Arg3, L, Module, false, target_backend(Target), Inherited);
        %% A marked `?each` (mark_targets/3). The compiled 2-arg pairing a
        %% retransform_spliced/4 re-run revisits does not match, so it passes
        %% through untouched.
        {call, L, {remote, _, {atom, _, Mod}, {atom, _, each}}, [
            FunArg, SourceArg, {atom, _, Backend}, {atom, _, CCtx}
        ]} when
            Mod =:= arizona_template orelse Mod =:= az orelse Mod =:= ?NODIFF_MOD
        ->
            Inherited = Mod =:= ?NODIFF_MOD,
            Spliced = map_size(Inline) > 0,
            Target = Backend:target(),
            FunArg1 = unwrap_spliced_each_callback(
                splice_vars(FunArg, Inline, Target, CCtx), Target, Inline
            ),
            {FunArg2, FunCh} = inline_helper_calls(FunArg1, Module, FunDefs, Target),
            compile_each(
                retransform_spliced(
                    inherit_spliced_each_nodiff(Inherited, FunArg2),
                    Spliced orelse FunCh,
                    Module,
                    FunDefs
                ),
                retransform_spliced(
                    inherit_spliced_source(
                        Inherited, splice_vars(SourceArg, Inline, Target, CCtx)
                    ),
                    Spliced,
                    Module,
                    FunDefs
                ),
                L,
                Module,
                Backend,
                CCtx,
                FunDefs,
                Inherited
            );
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
%% only matches an atom callback, and a compiled each pairing has two args where
%% a marked `?each` has four. Skipped entirely when nothing was spliced.
retransform_spliced(Expr, false, _Module, _FunDefs) ->
    Expr;
retransform_spliced(Expr, true, Module, FunDefs) ->
    transform_expr(Expr, Module, #{}, FunDefs).

%% True when the second arg of a 2-arg `each` is the OUTPUT of a compiled ?each
%% -- `arizona_template:each(Source, #{t,s,d,f,...})` (build_each_ast/
%% build_each_from_compiled) -- which a helper argument carries back into
%% mark_targets/3. The SOURCE form pairs `(Fun, Source)`, and a compiled pairing
%% always carries the `t` assoc on top of `s`/`d`/`f`, so a literal map source can
%% never be mistaken for one short of spelling out all four keys.
is_compiled_each_pairing({map, _, Fields}) ->
    is_compiled_template_map(Fields) andalso
        lists:member(t, [K || {map_field_assoc, _, {atom, _, K}, _} <- Fields]);
is_compiled_each_pairing(_SourceArg) ->
    false.

%% Post-splice mirror of the mark-time unwrap_each_body/2 jobs, for material the
%% inline splice just introduced into an ?each callback: flatten a spliced
%% track-wrap block in the callback's last expr, then unwrap a spliced
%% whole-body wrapper (`fun(_I) -> Row end` where Row hoisted `?html({li,...})`)
%% -- BEFORE retransform_spliced/4 would compile that wrapper to a map, which
%% skips body classification and silently drops `single_root` (positional list
%% patching). A no-op when the clause hoists nothing.
unwrap_spliced_each_callback(FunArg, _Target, Inline) when map_size(Inline) =:= 0 ->
    FunArg;
unwrap_spliced_each_callback(FunArg, Target, _Inline) ->
    [FunArg1 | _] = unwrap_each_body([flatten_spliced_tracks(FunArg)], Target),
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
%% non-variable/repeated params, statements before the element, and recursion --
%% and only where the call's value is content (see `Tail` below).
%% A `?html`-wrapped body that cannot be cleanly inlined is NOT rejected -- it
%% already renders as a runtime nested template (the az:with handoff pattern)
%% and keeps doing so. Genuinely remote calls, imported functions (invisible
%% bodies, indistinguishable from auto-imported BIFs), and variable-bound fun
%% calls are undetectable and stay as-is.
%%
%% Returns {Expr1, Changed} so the caller re-runs the bottom-up transform
%% (retransform_spliced/4) only when a body was actually spliced.
inline_helper_calls(Expr, Module, FunDefs, Target) ->
    ih(Expr, {Module, FunDefs, Target, #{}}, false, true).

%% `Tail` is true where the expression's value IS template content: the template
%% body itself, an element's children (and each item of a children list or of a
%% content-slot list), and a control-flow tail of one. Elsewhere -- a function
%% argument, a case scrutinee, an attribute, a statement -- the value is consumed as
%% data. A bare-element body spliced there is the same value the call returned, so it
%% is inlined anywhere; a `?html`-wrapped body is not (unwrapping it would hand the
%% consumer the inner element or list instead of the template the call returned), so
%% off a tail the call is left to run. A comprehension in content builds the list the
%% slot renders, so its body is `lc_body`: content for rejecting a helper that would
%% crash at render, data for inlining (the comprehension is not compiled as markup).
ih({call, L, {atom, _, Name}, Args} = Call, Ctx, Ch, Tail) ->
    ih_call(Call, Name, Args, L, Ctx, Ch, Tail);
ih(
    {call, L, {remote, _, {atom, _, Module}, {atom, _, Name}}, Args} = Call,
    {Module, _, _, _} = Ctx,
    Ch,
    Tail
) ->
    ih_call(Call, Name, Args, L, Ctx, Ch, Tail);
ih({map, _, _} = Map, _Ctx, Ch, _Tail) ->
    %% Map literals hold runtime data (user maps, ?stateful props) or
    %% already-compiled templates from a previous pass -- never descend.
    {Map, Ch};
ih({map, _, _, _} = Map, _Ctx, Ch, _Tail) ->
    {Map, Ch};
ih({tuple, L, [Tag, Attrs, Children]} = T, Ctx, Ch, Tail) ->
    case is_element_tuple(T) of
        true ->
            {[Tag1, Attrs1], Ch1} = ih_list([Tag, Attrs], Ctx, Ch, false),
            {Children1, Ch2} = ih_children(Children, Ctx, Ch1),
            {{tuple, L, [Tag1, Attrs1, Children1]}, Ch2};
        false ->
            ih_generic(T, Ctx, Ch, Tail)
    end;
ih({lc, L, Body, Quals}, Ctx, Ch, true) ->
    ih_content_lc(L, Body, Quals, Ctx, Ch);
ih({lc, L, Body, Quals}, Ctx, Ch, lc_body) ->
    ih_content_lc(L, Body, Quals, Ctx, Ch);
ih({cons, L, H, T}, Ctx, Ch, true) ->
    %% A list nested inside the list is a single value, not content.
    {H1, Ch1} = ih(H, Ctx, Ch, not is_list_ast(H)),
    {T1, Ch2} = ih(T, Ctx, Ch1, true),
    {{cons, L, H1, T1}, Ch2};
ih(T, Ctx, Ch, Tail) when
    element(1, T) =:= 'case';
    element(1, T) =:= 'if';
    element(1, T) =:= block;
    element(1, T) =:= 'receive';
    element(1, T) =:= 'try';
    element(1, T) =:= 'maybe';
    element(1, T) =:= 'fun'
->
    ih_flow(T, Ctx, Ch, Tail);
ih(T, Ctx, Ch, _Tail) when is_tuple(T) ->
    ih_generic(T, Ctx, Ch, false);
ih(L, Ctx, Ch, _Tail) when is_list(L) ->
    ih_list(L, Ctx, Ch, false);
ih(Other, _Ctx, Ch, _Tail) ->
    {Other, Ch}.

ih_content_lc(L, Body, Quals, Ctx, Ch) ->
    {Body1, Ch1} = ih(Body, Ctx, Ch, lc_body),
    {Quals1, Ch2} = ih_list(Quals, Ctx, Ch1, false),
    {{lc, L, Body1, Quals1}, Ch2}.

%% Control flow: a clause body's last expression carries the value, so it
%% inherits `Tail`; scrutinees, timeouts and `after` bodies never do.
ih_flow({'case', L, Scrutinee, Clauses}, Ctx, Ch, Tail) ->
    {Scrutinee1, Ch1} = ih(Scrutinee, Ctx, Ch, false),
    {Clauses1, Ch2} = ih_clauses(Clauses, Ctx, Ch1, Tail),
    {{'case', L, Scrutinee1, Clauses1}, Ch2};
ih_flow({'if', L, Clauses}, Ctx, Ch, Tail) ->
    {Clauses1, Ch1} = ih_clauses(Clauses, Ctx, Ch, Tail),
    {{'if', L, Clauses1}, Ch1};
ih_flow({block, L, Body}, Ctx, Ch, Tail) ->
    {Body1, Ch1} = ih_body(Body, Ctx, Ch, Tail),
    {{block, L, Body1}, Ch1};
ih_flow({'receive', L, Clauses}, Ctx, Ch, Tail) ->
    {Clauses1, Ch1} = ih_clauses(Clauses, Ctx, Ch, Tail),
    {{'receive', L, Clauses1}, Ch1};
ih_flow({'receive', L, Clauses, After, AfterBody}, Ctx, Ch, Tail) ->
    {Clauses1, Ch1} = ih_clauses(Clauses, Ctx, Ch, Tail),
    {After1, Ch2} = ih(After, Ctx, Ch1, false),
    {AfterBody1, Ch3} = ih_body(AfterBody, Ctx, Ch2, Tail),
    {{'receive', L, Clauses1, After1, AfterBody1}, Ch3};
ih_flow({'try', L, Body, OfClauses, CatchClauses, AfterBody}, Ctx, Ch, Tail) ->
    {Body1, Ch1} = ih_body(Body, Ctx, Ch, Tail),
    {OfClauses1, Ch2} = ih_clauses(OfClauses, Ctx, Ch1, Tail),
    {CatchClauses1, Ch3} = ih_clauses(CatchClauses, Ctx, Ch2, Tail),
    {AfterBody1, Ch4} = ih_list(AfterBody, Ctx, Ch3, false),
    {{'try', L, Body1, OfClauses1, CatchClauses1, AfterBody1}, Ch4};
ih_flow({'maybe', L, Body}, Ctx, Ch, Tail) ->
    {Body1, Ch1} = ih_body(Body, Ctx, Ch, Tail),
    {{'maybe', L, Body1}, Ch1};
ih_flow({'maybe', L, Body, {'else', L2, ElseClauses}}, Ctx, Ch, Tail) ->
    {Body1, Ch1} = ih_body(Body, Ctx, Ch, Tail),
    {ElseClauses1, Ch2} = ih_clauses(ElseClauses, Ctx, Ch1, Tail),
    {{'maybe', L, Body1, {'else', L2, ElseClauses1}}, Ch2};
ih_flow({'fun', L, {clauses, Clauses}}, Ctx, Ch, Tail) ->
    {Clauses1, Ch1} = ih_clauses(Clauses, Ctx, Ch, Tail),
    {{'fun', L, {clauses, Clauses1}}, Ch1};
ih_flow(T, Ctx, Ch, _Tail) ->
    %% A fun reference (`fun f/1`) holds no body to walk.
    ih_generic(T, Ctx, Ch, false).

%% Each item of an element's children list is a content slot, and an item that
%% is itself a list is a content-slot list (a fragment), so its items are
%% content too -- one level only (see the cons clause of ih/4).
ih_children({cons, L, H, T}, Ctx, Ch) ->
    {H1, Ch1} = ih(H, Ctx, Ch, true),
    {T1, Ch2} = ih_children(T, Ctx, Ch1),
    {{cons, L, H1, T1}, Ch2};
ih_children(Children, Ctx, Ch) ->
    ih(Children, Ctx, Ch, true).

ih_generic(T, Ctx, Ch, Tail) ->
    {L1, Ch1} = ih_list(tuple_to_list(T), Ctx, Ch, Tail),
    {list_to_tuple(L1), Ch1}.

ih_list([], _Ctx, Ch, _Tail) ->
    {[], Ch};
ih_list([H | T], Ctx, Ch, Tail) ->
    {H1, Ch1} = ih(H, Ctx, Ch, Tail),
    {T1, Ch2} = ih_list(T, Ctx, Ch1, Tail),
    {[H1 | T1], Ch2}.

%% A clause's patterns and guards are never content; its body's last expression
%% carries the clause's value, so it inherits `Tail`.
ih_clauses(Clauses, Ctx, Ch, Tail) ->
    lists:mapfoldl(
        fun({clause, L, Patterns, Guards, Body}, Ch0) ->
            {Body1, Ch1} = ih_body(Body, Ctx, Ch0, Tail),
            {{clause, L, Patterns, Guards, Body1}, Ch1}
        end,
        Ch,
        Clauses
    ).

ih_body(Body, Ctx, Ch, Tail) ->
    {Init, [Last]} = lists:split(length(Body) - 1, Body),
    {Init1, Ch1} = ih_list(Init, Ctx, Ch, false),
    {Last1, Ch2} = ih(Last, Ctx, Ch1, Tail),
    {Init1 ++ [Last1], Ch2}.

ih_call(Call, Name, Args, L, {Module, FunDefs, Target, Stack} = Ctx, Ch, Tail) ->
    Arity = length(Args),
    case helper_plan(Name, Arity, FunDefs, Target, L, Tail) of
        {inline, Params, BodyExpr, Wrap} when Tail; Wrap =:= bare ->
            case Stack of
                #{{Name, Arity} := _} -> parse_error({helper_recursive, Name, Arity}, L);
                #{} -> ok
            end,
            BodyExpr1 = bind_param_tests(
                rename_inlined_body(Name, Arity, Params, BodyExpr), Params, Name, Arity, L
            ),
            ok = check_helper_args_pure(Name, Arity, Params, Args, BodyExpr1),
            Spliced = mark_targets(subst_helper_args(BodyExpr1, Params, Args), Target),
            Ctx1 = {Module, FunDefs, Target, Stack#{{Name, Arity} => true}},
            {Spliced1, true} = ih(Spliced, Ctx1, true, Tail),
            {Spliced1, true};
        _NotInlined ->
            %% Not an inlineable element helper here: recurse into the args (they
            %% may contain helper calls of their own).
            {Parts, Ch1} = ih_list(tuple_to_list(Call), Ctx, Ch, false),
            {list_to_tuple(Parts), Ch1}
    end.

%% Substitute the helper's params with the caller's argument expressions via
%% two inline_vars/2 passes through fresh intermediate names. A direct
%% one-pass substitution loops: iv/2 re-enters the substituted expression, so
%% an argument referencing a caller variable named like a parameter
%% (`badge(A)` passing the caller's `A` into a param `A`) recurses forever.
%% Pass one renames each param occurrence to a fresh variable no caller
%% expression can contain; pass two binds the fresh variable to the argument
%% -- both terminate, and iv's scope handling (shadowing funs, patterns,
%% guards untouched) applies unchanged. A param read in a pattern or guard was
%% already moved out of it by bind_param_tests/5.
subst_helper_args(BodyExpr, [], []) ->
    BodyExpr;
subst_helper_args(BodyExpr, Params, Args) ->
    Fresh = [fresh_helper_var(P) || P <- Params],
    Rename = maps:from_list([{P, {var, 0, F}} || {P, F} <:- lists:zip(Params, Fresh)]),
    %% The arguments were checked against their use count (check_helper_args_pure/5);
    %% the marker exempts them from the hoisted-definition check in iv/2.
    Bind = (maps:from_list(lists:zip(Fresh, Args)))#{'$helper_args' => true},
    inline_vars(inline_vars(BodyExpr, Rename), Bind).

%% An argument runs once per use of its parameter in the inlined body, so an impure
%% one is refused unless that count is exactly one. The uses are counted on the body
%% substitution sees (after bind_param_tests/5 turned pattern and guard reads into
%% one ordinary read each), by the same scope-aware walk that substitutes: a marker
%% stands in for the argument, and each marker in the result is one run.
check_helper_args_pure(Name, Arity, Params, Args, BodyExpr) ->
    ok = lists:foreach(
        fun({P, Arg}) ->
            Uses = count_param_uses(inline_vars(BodyExpr, #{P => ?PARAM_USE_MARKER})),
            check_helper_arg_pure(Name, Arity, P, Uses, Arg)
        end,
        lists:zip(Params, Args)
    ).

count_param_uses(?PARAM_USE_MARKER) ->
    1;
count_param_uses(T) when is_tuple(T) ->
    count_param_uses(tuple_to_list(T));
count_param_uses(L) when is_list(L) ->
    lists:sum([count_param_uses(E) || E <- L]);
count_param_uses(_Leaf) ->
    0.

check_helper_arg_pure(_Name, _Arity, _Param, 1, _Arg) ->
    ok;
check_helper_arg_pure(Name, Arity, Param, Uses, Arg) ->
    case impure_expr(Arg) of
        none ->
            ok;
        {Line, What} ->
            Where = {helper_arg, Name, Arity, Param, Uses},
            parse_error({template_impure_expr, Where, What}, Line)
    end.

fresh_helper_var(P) ->
    list_to_atom(
        "AzHelperArg" ++ integer_to_list(erlang:unique_integer([positive])) ++ atom_to_list(P)
    ).

%% A parameter the helper body reads in a pattern or a guard -- an equality test
%% (`case Id of Sel -> ...`), a map key, a binary size, a `?=`, a guard (`if N > Max`)
%% -- cannot take the argument expression subst_helper_args/3 puts in its place: a call
%% is not a pattern, nor legal in a guard. Left as written, the name is no longer the
%% parameter once spliced: a pattern turns it into a fresh binder that matches anything
%% or an equality test against a caller variable of the same name, and a guard reads
%% that caller variable or fails to compile.
%%
%% So each expression whose own patterns or guards read a parameter is wrapped in a
%% block that first binds a fresh variable to the parameter, and the reads in those
%% patterns and guards are renamed to it: `begin AzArg = Sel, case Id of AzArg -> ...
%% end end`. The `AzArg = Sel` match is an ordinary read, so substitution then puts the
%% argument there. The block lands inside the slot closure the expression compiles
%% into, so the argument's binding reads run in that slot's dependency bracket and the
%% slot tracks them. A fun head or a generator binds its own variables, shadowing a
%% parameter of the same name below it; only its map keys and binary sizes read one.
bind_param_tests(Body, [], _Name, _Arity, _Line) ->
    Body;
bind_param_tests(Body, Params, Name, Arity, Line) ->
    pt(Body, maps:from_keys(Params, true), {Name, Arity, Line}).

pt(Expr, Live, _Ctx) when map_size(Live) =:= 0 ->
    Expr;
pt({maybe_match, L, P, E}, Live, Ctx) ->
    %% Its pattern is renamed by the enclosing `maybe` (a `?=` cannot be wrapped).
    {maybe_match, L, P, pt(E, Live, Ctx)};
pt(
    {call, L, {remote, _, {atom, _, Mod}, {atom, _, each}} = F, [{'fun', FL, {clauses, Cs}}, Src]},
    Live,
    Ctx
) when
    Mod =:= arizona_template; Mod =:= az
->
    %% An ?each callback must stay a fun literal: nothing can be bound ahead of it.
    case pt_tests(fun_heads(Cs), Live) of
        [] -> ok;
        [_ | _] -> pt_each_error(Ctx)
    end,
    {call, L, F, [{'fun', FL, {clauses, pt_fun_clauses(Cs, Live, Ctx)}}, pt(Src, Live, Ctx)]};
pt(T, Live, Ctx) when is_tuple(T) ->
    case is_test_node(T) of
        true -> pt_wrap(pt_node(T, Live, Ctx), element(2, T), node_live(T, Live));
        false -> list_to_tuple(pt(tuple_to_list(T), Live, Ctx))
    end;
pt(List, Live, Ctx) when is_list(List) ->
    [pt(E, Live, Ctx) || E <- List];
pt(Leaf, _Live, _Ctx) ->
    Leaf.

%% The expressions with patterns or guards of their own.
is_test_node({'fun', _, {clauses, _}}) ->
    true;
is_test_node({Comp, _, _, _}) when Comp =:= lc; Comp =:= bc; Comp =:= mc -> true;
is_test_node({match, _, _, _}) ->
    true;
is_test_node(T) ->
    lists:member(element(1, T), [named_fun, 'case', 'if', 'receive', 'try', 'maybe']).

%% A named fun's name shadows a parameter of the same name in its own clauses.
node_live({named_fun, _, FName, _}, Live) -> maps:remove(FName, Live);
node_live(_Node, Live) -> Live.

%% Walk a node's subexpressions; its own heads are left for pt_wrap/3.
pt_node({'case', L, E, Cs}, Live, Ctx) ->
    {'case', L, pt(E, Live, Ctx), pt_clauses(Cs, Live, Ctx)};
pt_node({'if', L, Cs}, Live, Ctx) ->
    {'if', L, pt_clauses(Cs, Live, Ctx)};
pt_node({'receive', L, Cs}, Live, Ctx) ->
    {'receive', L, pt_clauses(Cs, Live, Ctx)};
pt_node({'receive', L, Cs, AE, AB}, Live, Ctx) ->
    {'receive', L, pt_clauses(Cs, Live, Ctx), pt(AE, Live, Ctx), pt(AB, Live, Ctx)};
pt_node({'try', L, B, OfCs, CatchCs, Aft}, Live, Ctx) ->
    {'try', L, pt(B, Live, Ctx), pt_clauses(OfCs, Live, Ctx), pt_clauses(CatchCs, Live, Ctx),
        pt(Aft, Live, Ctx)};
pt_node({'maybe', L, B}, Live, Ctx) ->
    {'maybe', L, pt(B, Live, Ctx)};
pt_node({'maybe', L, B, {'else', L2, Cs}}, Live, Ctx) ->
    {'maybe', L, pt(B, Live, Ctx), {'else', L2, pt_clauses(Cs, Live, Ctx)}};
pt_node(Node, Live, Ctx) ->
    pt_binding_node(Node, Live, Ctx).

pt_binding_node({match, L, P, E}, Live, Ctx) ->
    {match, L, P, pt(E, Live, Ctx)};
pt_binding_node({'fun', L, {clauses, Cs}}, Live, Ctx) ->
    {'fun', L, {clauses, pt_fun_clauses(Cs, Live, Ctx)}};
pt_binding_node({named_fun, L, FName, Cs}, Live, Ctx) ->
    {named_fun, L, FName, pt_fun_clauses(Cs, maps:remove(FName, Live), Ctx)};
pt_binding_node({Comp, L, T, Qs}, Live, Ctx) ->
    {Qs1, Live1} = lists:mapfoldl(fun(Q, Lv) -> pt_qual(Q, Lv, Ctx) end, Live, Qs),
    {Comp, L, pt(T, Live1, Ctx), Qs1}.

-spec pt_each_error({atom(), arity(), term()}) -> no_return().
pt_each_error({Name, Arity, Line}) ->
    parse_error({helper_param_in_each_guard, Name, Arity}, Line).

%% Clause bodies: a case-like pattern tests a parameter rather than binding it, so the
%% parameters stay live in the body.
pt_clauses(Cs, Live, Ctx) ->
    [{clause, L, Ps, Gs, pt(B, Live, Ctx)} || {clause, L, Ps, Gs, B} <:- Cs].

pt_fun_clauses(Cs, Live, Ctx) ->
    [
        {clause, L, Ps, Gs, pt(B, maps:without(pattern_vars(Ps), Live), Ctx)}
     || {clause, L, Ps, Gs, B} <:- Cs
    ].

%% A generator's source sees the scope before it; its pattern binds for what follows.
pt_qual({zip, L, Gens}, Live, Ctx) ->
    Gens1 = [{G, GL, P, pt(E, Live, Ctx)} || {G, GL, P, E} <:- Gens],
    {{zip, L, Gens1}, maps:without(pattern_vars([P || {_, _, P, _} <:- Gens]), Live)};
pt_qual({Gen, L, P, E}, Live, Ctx) when
    Gen =:= generate;
    Gen =:= generate_strict;
    Gen =:= b_generate;
    Gen =:= b_generate_strict;
    Gen =:= m_generate;
    Gen =:= m_generate_strict
->
    {{Gen, L, P, pt(E, Live, Ctx)}, maps:without(pattern_vars(P), Live)};
pt_qual(Filter, Live, Ctx) ->
    {pt(Filter, Live, Ctx), Live}.

%% Bind a fresh variable to each live parameter the node's own patterns and guards
%% read, and rename those reads to it. `Live` is the scope the node sits in.
pt_wrap(Node, L, Live) ->
    case node_tests(Node, Live) of
        [] ->
            Node;
        Params ->
            Ren = #{P => fresh_param_var(P) || P <- Params},
            Binds = [{match, L, {var, L, V}, {var, L, P}} || P := V <- Ren],
            {block, L, Binds ++ [rename_heads(Node, Ren)]}
    end.

fresh_param_var(P) ->
    N = get(?PARAM_SEQ_KEY),
    _ = put(?PARAM_SEQ_KEY, N + 1),
    list_to_atom("AzArg@" ++ integer_to_list(N) ++ "@" ++ atom_to_list(P)).

node_tests({Comp, _, _, Qs}, Live) when Comp =:= lc; Comp =:= bc; Comp =:= mc ->
    generator_tests(Qs, Live);
node_tests(Node, Live) ->
    pt_tests(node_heads(Node), Live).

%% The parameters (in first-seen order) a node's heads read. A head is
%% `{test, Form}` -- every variable in it is a read -- or `{bind, Pattern, Guards}`,
%% whose pattern binds (shadowing for its guards) and reads only map keys and
%% binary sizes.
pt_tests(Heads, Live) ->
    lists:reverse(lists:foldl(fun(H, Acc) -> head_tests(H, Live, Acc) end, [], Heads)).

head_tests({test, Form}, Live, Acc) ->
    collect_live_vars(Form, Live, Acc);
head_tests({bind, Pattern, Guards}, Live, Acc) ->
    Acc1 = collect_live_vars(pattern_reads(Pattern), Live, Acc),
    collect_live_vars(Guards, maps:without(pattern_vars(Pattern), Live), Acc1).

collect_live_vars({var, _, V}, Live, Acc) ->
    case is_map_key(V, Live) andalso not lists:member(V, Acc) of
        true -> [V | Acc];
        false -> Acc
    end;
collect_live_vars(T, Live, Acc) when is_tuple(T) ->
    collect_live_vars(tuple_to_list(T), Live, Acc);
collect_live_vars([H | T], Live, Acc) ->
    collect_live_vars(T, Live, collect_live_vars(H, Live, Acc));
collect_live_vars(_Leaf, _Live, Acc) ->
    Acc.

node_heads({'case', _, _, Cs}) -> clause_heads(Cs);
node_heads({'if', _, Cs}) -> clause_heads(Cs);
node_heads({'receive', _, Cs}) -> clause_heads(Cs);
node_heads({'receive', _, Cs, _, _}) -> clause_heads(Cs);
node_heads({'try', _, _, OfCs, CatchCs, _}) -> clause_heads(OfCs ++ CatchCs);
node_heads({'maybe', _, B}) -> maybe_heads(B);
node_heads({'maybe', _, B, {'else', _, Cs}}) -> maybe_heads(B) ++ clause_heads(Cs);
node_heads({match, _, P, _}) -> [{test, P}];
node_heads({'fun', _, {clauses, Cs}}) -> fun_heads(Cs);
node_heads({named_fun, _, _, Cs}) -> fun_heads(Cs).

clause_heads(Cs) ->
    [{test, [Ps, Gs]} || {clause, _, Ps, Gs, _} <:- Cs].

maybe_heads(B) ->
    [{test, P} || {maybe_match, _, P, _} <- B].

fun_heads(Cs) ->
    [{bind, Ps, Gs} || {clause, _, Ps, Gs, _} <:- Cs].

%% Each generator binds for the qualifiers after it, so a later generator's map key or
%% binary size reads a parameter only when no earlier one rebound its name. A map
%% generator's `K := V` binds both sides.
generator_tests(Qs, Live) ->
    {Acc, _} = lists:foldl(
        fun(Q, {Acc0, Lv}) ->
            Ps = [map_gen_as_tuple(P) || P <- qual_patterns(Q)],
            {collect_live_vars(pattern_reads(Ps), Lv, Acc0), maps:without(pattern_vars(Ps), Lv)}
        end,
        {[], Live},
        Qs
    ),
    lists:reverse(Acc).

%% A map generator's `K := V` is two patterns, not a map pattern: its key binds like its
%% value, where a map pattern's key is a read. Seen as a tuple, binder_vars/2 and
%% pattern_reads/1 treat both sides as the patterns they are.
map_gen_as_tuple({map_field_exact, L, K, V}) -> {tuple, L, [K, V]};
map_gen_as_tuple(P) -> P.

%% Rename the parameter reads in a node's own heads (never its bodies, which read the
%% parameter as an expression and are substituted with the argument).
rename_heads({'case', L, E, Cs}, Ren) ->
    {'case', L, E, rename_clause_heads(Cs, Ren)};
rename_heads({'if', L, Cs}, Ren) ->
    {'if', L, rename_clause_heads(Cs, Ren)};
rename_heads({'receive', L, Cs}, Ren) ->
    {'receive', L, rename_clause_heads(Cs, Ren)};
rename_heads({'receive', L, Cs, AE, AB}, Ren) ->
    {'receive', L, rename_clause_heads(Cs, Ren), AE, AB};
rename_heads({'try', L, B, OfCs, CatchCs, Aft}, Ren) ->
    {'try', L, B, rename_clause_heads(OfCs, Ren), rename_clause_heads(CatchCs, Ren), Aft};
rename_heads({'maybe', L, B}, Ren) ->
    {'maybe', L, rename_maybe_body(B, Ren)};
rename_heads({'maybe', L, B, {'else', L2, Cs}}, Ren) ->
    {'maybe', L, rename_maybe_body(B, Ren), {'else', L2, rename_clause_heads(Cs, Ren)}};
rename_heads({match, L, P, E}, Ren) ->
    {match, L, rename_vars(P, Ren), E};
rename_heads({'fun', L, {clauses, Cs}}, Ren) ->
    {'fun', L, {clauses, rename_fun_heads(Cs, Ren)}};
rename_heads({named_fun, L, FName, Cs}, Ren) ->
    {named_fun, L, FName, rename_fun_heads(Cs, Ren)};
rename_heads({Comp, L, T, Qs}, Ren) ->
    {Comp, L, T, rename_generators(Qs, Ren)}.

rename_clause_heads(Cs, Ren) ->
    [{clause, L, rename_vars(Ps, Ren), rename_vars(Gs, Ren), B} || {clause, L, Ps, Gs, B} <:- Cs].

rename_maybe_body(B, Ren) ->
    [
        case E of
            {maybe_match, L, P, X} -> {maybe_match, L, rename_vars(P, Ren), X};
            _ -> E
        end
     || E <- B
    ].

rename_fun_heads(Cs, Ren) ->
    [
        {clause, L, rename_reads(Ps, Ren), rename_vars(Gs, maps:without(pattern_vars(Ps), Ren)), B}
     || {clause, L, Ps, Gs, B} <:- Cs
    ].

%% Generators are renamed in order: once one binds a parameter's name, the later ones
%% read their own binding, not the parameter.
rename_generators(Qs, Ren) ->
    {Qs1, _} = lists:mapfoldl(fun rename_generator/2, Ren, Qs),
    Qs1.

rename_generator({zip, L, Gens}, Ren) ->
    Gens1 = [{G, GL, rename_gen_pattern(P, Ren), E} || {G, GL, P, E} <:- Gens],
    {{zip, L, Gens1}, maps:without(pattern_vars([P || {_, _, P, _} <:- Gens]), Ren)};
rename_generator(Q, Ren) ->
    case qual_patterns(Q) of
        [P] ->
            {G, L, P, E} = Q,
            {{G, L, rename_gen_pattern(P, Ren), E}, maps:without(pattern_vars(P), Ren)};
        [] ->
            {Q, Ren}
    end.

rename_gen_pattern({map_field_exact, L, K, V}, Ren) ->
    {map_field_exact, L, rename_reads(K, Ren), rename_reads(V, Ren)};
rename_gen_pattern(P, Ren) ->
    rename_reads(P, Ren).

rename_vars({var, L, V} = Var, Ren) ->
    case Ren of
        #{V := New} -> {var, L, New};
        #{} -> Var
    end;
rename_vars(T, Ren) when is_tuple(T) ->
    list_to_tuple(rename_vars(tuple_to_list(T), Ren));
rename_vars(List, Ren) when is_list(List) ->
    [rename_vars(E, Ren) || E <- List];
rename_vars(Leaf, _Ren) ->
    Leaf.

%% In a binding pattern only the map keys and binary sizes are reads.
rename_reads({map_field_exact, L, K, V}, Ren) ->
    {map_field_exact, L, rename_vars(K, Ren), rename_reads(V, Ren)};
rename_reads({bin_element, L, V, Size, Types}, Ren) ->
    {bin_element, L, rename_reads(V, Ren), rename_vars(Size, Ren), Types};
rename_reads({var, _, _} = Var, _Ren) ->
    Var;
rename_reads(T, Ren) when is_tuple(T) ->
    list_to_tuple(rename_reads(tuple_to_list(T), Ren));
rename_reads(List, Ren) when is_list(List) ->
    [rename_reads(E, Ren) || E <- List];
rename_reads(Leaf, _Ren) ->
    Leaf.

%% Decide what to do with a local call: `skip` (leave the call -- a scalar
%% helper, an unresolvable callee, or a wrapped body that renders fine as-is)
%% or `{inline, ParamNames, BodyExpr, bare | wrapped}`. Raises the helper_* errors only for
%% bare-element bodies called where the value is content: there it crashed at render.
%% Anywhere else (a function argument, a scrutinee) the value is consumed as data, and
%% an atom-tagged tuple is indistinguishable from an ordinary data function's result.
helper_plan(Name, Arity, FunDefs, Target, CallLine, Tail) ->
    case {helper_plan_shape(Name, Arity, FunDefs, Target), Tail} of
        {{reject, _Reason}, false} -> skip;
        {{reject, Reason}, _Content} -> parse_error(Reason, CallLine);
        {Plan, _Tail} -> Plan
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
                    {inline, ParamNames, BodyExpr, Wrap};
                {false, wrapped} ->
                    %% Renders as a runtime nested template today; keep it.
                    skip;
                {false, bare} ->
                    reject_element_helper(BodyExpr, Guards, Body, Name, Arity)
            end
    end;
helper_clause_plan(Clauses, Name, Arity, Target) ->
    BareClause = fun({clause, _, _P, _G, B}) ->
        case helper_body_shape(lists:last(B), Target) of
            {bare, Expr} -> is_element_body(Expr);
            _ -> false
        end
    end,
    case lists:any(BareClause, Clauses) of
        true -> {reject, {helper_multi_clause, Name, Arity}};
        false -> skip
    end.

%% An unclean helper (guarded, multi-statement, non-variable params) is rejected only
%% when its body is element-shaped: that one always crashed at render. A body that is
%% markup only through constructors (`[~"a", ?stateless(...)]`) renders fine wherever
%% the call is not a content slot, so it is left as a runtime call.
reject_element_helper(BodyExpr, Guards, Body, Name, Arity) ->
    case is_element_body(BodyExpr) of
        true -> {reject, helper_reject_reason(Guards, Body, Name, Arity)};
        false -> skip
    end.

is_element_body(Expr) ->
    case classify_body(Expr) of
        element_tuple -> true;
        element_list -> true;
        list_ast -> list_has_element_tuple(Expr);
        _ -> false
    end.

list_has_element_tuple({cons, _, Head, Tail}) ->
    is_element_tuple(Head) orelse list_has_element_tuple(Tail);
list_has_element_tuple(_) ->
    false.

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
    %% template and is handled by transform_scoped_tail/4.
    Init = normalize_tail_binds(Init0),
    Inline = prepare_inline(
        rename_hoisted_binders(Patterns, Init ++ [Last], collect_inline(Init ++ [Last])),
        Init ++ [Last]
    ),
    ok = check_markup_vars(Init ++ [Last], Inline),
    Transform = fun(Expr, Inl) -> transform_expr(Expr, Module, Inl, FunDefs) end,
    TransformedInit = [Transform(Expr, Inline) || Expr <- Init],
    %% The last expression carries the live-root template -- a direct ?html/?native
    %% call, or a control-flow expression whose every tail carries a root, compiled
    %% at each leaf.
    TransformedLast = transform_scoped_tail(
        Last,
        Inline,
        clause_bound(Patterns, Init),
        {fun(Leaf, Inl) -> transform_live_render_leaf(Leaf, Module, Inl, FunDefs) end, Transform}
    ),
    Body1 = TransformedInit ++ [TransformedLast],
    {clause, L, Patterns, Guards,
        suppress_unused_inline_matches(
            Body1,
            without_prebound(
                without_source_unused(Inline, Init ++ [Last], []), Patterns, Init ++ [Last]
            )
        )}.

%% The value of a render clause is the value of its last expression, so a template
%% there may sit at any tail of a control-flow expression, after statements that
%% belong to its branch alone:
%%
%%     case ?get(mode) of
%%         a -> N = ?get(n), ?html({p, [], [N]});
%%         b -> ...
%%     end.
%%
%% A hoisted read in a branch needs inlining exactly as a top-level one does, but
%% only that branch's templates may see it. Walk the tail positions (the forms and
%% positions map_tail_exprs/3 defines) and give every body on the way its own
%% inline map: the enclosing map plus the body's own hoisted reads. `Leaf` compiles
%% each tail leaf and `NonTail` every other sub-expression, both given the map in
%% scope there. `Bound` holds every name bound outside the body being entered: a
%% match on one of those is an equality test, so it is inlined (the slot stays
%% tracked) but never underscored, which would drop the test and leave the outer
%% binding unused (without_prebound/4).
transform_scoped_tail(Expr, Inline, Bound, {Leaf, NonTail} = Fns) ->
    case erl_syntax:revert(Expr) of
        {'case', L, Scrutinee, Cs} ->
            Cs1 = scoped_tail_clauses(Cs, Inline, stmt_bound(Scrutinee, Bound), Fns),
            {'case', L, NonTail(Scrutinee, Inline), Cs1};
        {'if', L, Cs} ->
            {'if', L, scoped_tail_clauses(Cs, Inline, Bound, Fns)};
        {block, L, Body} ->
            {block, L, scoped_tail_body(Body, Inline, Bound, Fns)};
        {'receive', L, Cs} ->
            {'receive', L, scoped_tail_clauses(Cs, Inline, Bound, Fns)};
        {'receive', L, Cs, AfterExpr, AfterBody} ->
            {'receive', L, scoped_tail_clauses(Cs, Inline, Bound, Fns), NonTail(AfterExpr, Inline),
                scoped_tail_body(AfterBody, Inline, Bound, Fns)};
        {'try', L, Body0, OfCs0, CatchCs0, After0} ->
            %% The try body's definitions stay in scope in its `of` clauses, so they
            %% inline there too, and whether one is left unused is counted over the
            %% whole `try`.
            {Body, BodyInline, BodyBound, Local} = scoped_body(
                Body0, Inline, Bound, Fns, [OfCs0, CatchCs0, After0]
            ),
            OfCs = scoped_tail_clauses(OfCs0, BodyInline, BodyBound, Fns),
            CatchCs = scoped_tail_clauses(CatchCs0, Inline, BodyBound, Fns),
            After = [NonTail(E, Inline) || E <- After0],
            Uses = Body ++ [OfCs, CatchCs, After],
            {'try', L, [maybe_underscore_match(S, Local, Uses) || S <- Body], OfCs, CatchCs, After};
        {'maybe', L, Body} ->
            {'maybe', L, scoped_tail_body(Body, Inline, Bound, Fns)};
        {'maybe', L, Body, {'else', L2, Cs}} ->
            {'maybe', L, scoped_tail_body(Body, Inline, Bound, Fns),
                {'else', L2, scoped_tail_clauses(Cs, Inline, Bound, Fns)}};
        _ ->
            Leaf(Expr, Inline)
    end.

scoped_tail_clauses(Clauses, Inline, Bound, Fns) ->
    [
        {clause, L, Ps, Gs,
            scoped_tail_body(Body, Inline, bind_names(pattern_vars(Ps), Bound), Fns)}
     || {clause, L, Ps, Gs, Body} <:- Clauses
    ].

scoped_tail_body(Body0, Inline, Bound, Fns) ->
    {Body, _Inline, _Bound, Local} = scoped_body(Body0, Inline, Bound, Fns, []),
    suppress_unused_inline_matches(Body, Local).

%% Transform one body: its init statements with its inline map, its last
%% expression as a tail. Returns the map in scope at its end, the names bound by
%% then, and the reads it hoisted itself that the source used (the matches it may
%% leave unused); `Others` is the rest of the source the body's names reach.
scoped_body(Body0, Outer, Bound0, {_Leaf, NonTail} = Fns, Others) ->
    {Init0, [Last]} = lists:split(length(Body0) - 1, Body0),
    Init = normalize_tail_binds(Init0),
    Body = Init ++ [Last],
    {Local, Inline} = body_inline(Body, Outer, Bound0),
    ok = check_markup_vars(Body, Inline),
    Last1 = transform_scoped_tail(Last, Inline, body_bound(Init, Bound0), Fns),
    {
        [NonTail(E, Inline) || E <- Init] ++ [Last1],
        Inline,
        body_bound(Body, Bound0),
        without_prebound(without_source_unused(Local, Body, Others), Body, Bound0, [])
    }.

%% A body's own hoisted reads (collect_inline/1 for a nested body: a read may
%% reach through a name the enclosing map already inlines), and the enclosing map
%% extended with them.
body_inline(Body, Outer, Bound) ->
    {Raw, Poisoned} = scan_scoped_matches(Body, Bound, #{}, #{}),
    Candidates = maps:without(maps:keys(Poisoned), Raw),
    Reaching = reaching_fixpoint(Candidates, #{V => true || V := _ <- Outer}),
    Local = maps:with(maps:keys(Reaching), Candidates),
    {Local, prepare_inline(Local, Body, Outer)}.

%% scan_top_matches/3, also refusing a name bound before the match -- outside the
%% body, or by an earlier statement that is not a plain match.
scan_scoped_matches([], _Bound, Raw, Poisoned) ->
    {Raw, Poisoned};
scan_scoped_matches([Stmt | Rest], Bound, Raw0, Poisoned0) ->
    {Raw, Poisoned} =
        case Stmt of
            {match, _, {var, _, '_'}, _RHS} ->
                {Raw0, Poisoned0};
            {match, _, {var, _, V}, RHS} ->
                {Raw0#{V => RHS}, Poisoned0};
            _ ->
                {Raw0, Poisoned0}
        end,
    scan_scoped_matches(Rest, stmt_bound(Stmt, Bound), Raw, Poisoned).

%% The names bound once a clause's head and its init statements have run.
clause_bound(Patterns, Init) ->
    body_bound(Init, bind_names(pattern_vars(Patterns), #{})).

body_bound(Body, Bound) ->
    lists:foldl(fun stmt_bound/2, Bound, Body).

stmt_bound(Stmt, Bound) ->
    Env = ordsets:from_list(maps:keys(Bound)),
    Ann = erl_syntax:get_ann(erl_syntax_lib:annotate_bindings(Stmt, Env)),
    {bound, Names} = lists:keyfind(bound, 1, Ann),
    bind_names(Names, Bound).

bind_names(Names, Bound) ->
    maps:merge(Bound, #{N => true || N <- Names}).

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
            Arg2 = splice_vars(Arg1, Inline, Target, root_content_context(Target)),
            {Arg3, HelperCh} = inline_helper_calls(Arg2, Module, FunDefs, Target),
            Arg4 = retransform_spliced(
                inherit_spliced_nodiff(false, Arg3),
                map_size(Inline) > 0 orelse HelperCh,
                Module,
                FunDefs
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
%% non-control-flow `Expr` is itself a tail leaf. This is the definition of "tail
%% position" for the content-slot element expansion (expand_block_element_tails/3);
%% transform_scoped_tail/4 walks the same forms and positions for a render clause's
%% last expression, adding a scope per body, so the two change together. `try`
%% body last is treated as a tail even with `of` clauses (its value is then
%% matched by `of`, but this mirrors the original live-render behaviour and never
%% matters for real templates).
map_tail_exprs(Expr, TailFun, NonTailFun) ->
    {Mapped, none} = mapfold_tail_exprs(
        Expr, fun(Leaf, none) -> {TailFun(Leaf), none} end, NonTailFun, none
    ),
    Mapped.

%% map_tail_exprs/3 threading an accumulator through the tail leaves, in source order.
mapfold_tail_exprs(Expr, TailFun, NonTailFun, Acc0) ->
    Clauses = fun(Cs, Acc) ->
        lists:mapfoldl(fun(C, A) -> mapfold_tail_clause(C, TailFun, NonTailFun, A) end, Acc, Cs)
    end,
    case erl_syntax:revert(Expr) of
        {'case', L, Scrutinee, Cs0} ->
            {Cs, Acc} = Clauses(Cs0, Acc0),
            {{'case', L, NonTailFun(Scrutinee), Cs}, Acc};
        {'if', L, Cs0} ->
            {Cs, Acc} = Clauses(Cs0, Acc0),
            {{'if', L, Cs}, Acc};
        {block, L, Body0} ->
            {Body, Acc} = mapfold_tail_body(Body0, TailFun, NonTailFun, Acc0),
            {{block, L, Body}, Acc};
        {'receive', L, Cs0} ->
            {Cs, Acc} = Clauses(Cs0, Acc0),
            {{'receive', L, Cs}, Acc};
        {'receive', L, Cs0, AfterExpr, AfterBody0} ->
            {Cs, Acc1} = Clauses(Cs0, Acc0),
            {AfterBody, Acc} = mapfold_tail_body(AfterBody0, TailFun, NonTailFun, Acc1),
            {{'receive', L, Cs, NonTailFun(AfterExpr), AfterBody}, Acc};
        {'try', L, Body0, OfClauses0, CatchClauses0, AfterBody} ->
            {Body, Acc1} = mapfold_tail_body(Body0, TailFun, NonTailFun, Acc0),
            {OfClauses, Acc2} = Clauses(OfClauses0, Acc1),
            {CatchClauses, Acc} = Clauses(CatchClauses0, Acc2),
            {{'try', L, Body, OfClauses, CatchClauses, [NonTailFun(E) || E <- AfterBody]}, Acc};
        {'maybe', L, Body0} ->
            {Body, Acc} = mapfold_tail_body(Body0, TailFun, NonTailFun, Acc0),
            {{'maybe', L, Body}, Acc};
        {'maybe', L, Body0, {'else', L2, ElseClauses0}} ->
            {Body, Acc1} = mapfold_tail_body(Body0, TailFun, NonTailFun, Acc0),
            {ElseClauses, Acc} = Clauses(ElseClauses0, Acc1),
            {{'maybe', L, Body, {'else', L2, ElseClauses}}, Acc};
        _ ->
            TailFun(Expr, Acc0)
    end.

mapfold_tail_clause({clause, L, Patterns, Guards, Body0}, TailFun, NonTailFun, Acc0) ->
    {Body, Acc} = mapfold_tail_body(Body0, TailFun, NonTailFun, Acc0),
    {{clause, L, Patterns, Guards, Body}, Acc}.

%% Only a body's last expression is a tail (recursing through mapfold_tail_exprs/4 so
%% a control-flow last is walked too); the init statements are non-tail.
mapfold_tail_body(Body, TailFun, NonTailFun, Acc0) ->
    {Init, [Last0]} = lists:split(length(Body) - 1, Body),
    {Last, Acc} = mapfold_tail_exprs(Last0, TailFun, NonTailFun, Acc0),
    {[NonTailFun(E) || E <- Init] ++ [Last], Acc}.

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
%% Variables bound more than once are dropped (ambiguous to inline); an expression
%% with no read (e.g. `Id = make_uuid()`) is left captured, so it runs once and every
%% slot sees that value. One that reads runs in the body and again in each slot using
%% it, so an always-impure call there is refused (check_hoisted_pure/3).
%% A content-slot list compiles its markup items only where they are written in it;
%% a variable item is a value. A hoisted read is substituted back first (it is in
%% `Inline`), but a variable bound to markup that reads nothing stays a variable and
%% fails at render, so it is refused here, naming the variable.
check_markup_vars(Body, Inline) ->
    {Raw, Poisoned} = scan_top_matches(Body, #{}, #{}),
    MarkupVars =
        #{
            V => true
         || V := RHS <- maps:without(maps:keys(Poisoned), Raw),
            not is_map_key(V, Inline),
            is_markup_rhs(RHS)
        },
    case map_size(MarkupVars) of
        0 -> ok;
        _ -> check_markup_vars(Body, MarkupVars, false)
    end.

is_markup_rhs(RHS) ->
    case is_list_ast(RHS) of
        true -> list_has_markup_item(RHS);
        false -> is_markup_item(RHS) andalso not is_value_constructor(RHS)
    end.

check_markup_vars(
    {call, _, {remote, _, {atom, _, Mod}, {atom, _, _}}, Args}, Vars, _InTemplate
) when
    Mod =:= arizona_template; Mod =:= az; Mod =:= ?NODIFF_MOD
->
    check_markup_vars(Args, Vars, true);
check_markup_vars({'fun', _, {clauses, Clauses}}, Vars, InTemplate) ->
    %% A fun parameter shadows a hoisted name in its body.
    _ = [
        check_markup_vars([G, B], maps:without(pattern_vars(Ps), Vars), InTemplate)
     || {clause, _, Ps, G, B} <:- Clauses
    ],
    ok;
check_markup_vars({Comp, _, T, Qualifiers}, Vars, InTemplate) when
    Comp =:= lc; Comp =:= bc; Comp =:= mc
->
    %% So does a generator pattern, in the qualifiers after it and the template.
    Bound = lists:append([qualifier_binders(Q) || Q <- Qualifiers]),
    check_markup_vars([T, Qualifiers], maps:without(Bound, Vars), InTemplate);
check_markup_vars({tuple, _, [_, _, Children]} = T, Vars, true) ->
    case is_element_tuple(T) of
        true -> ok = check_children_lists(Children, Vars);
        false -> ok
    end,
    check_markup_vars(tuple_to_list(T), Vars, true);
check_markup_vars(T, Vars, InTemplate) when is_tuple(T) ->
    check_markup_vars(tuple_to_list(T), Vars, InTemplate);
check_markup_vars([H | T], Vars, InTemplate) ->
    ok = check_markup_vars(H, Vars, InTemplate),
    check_markup_vars(T, Vars, InTemplate);
check_markup_vars(_Leaf, _Vars, _InTemplate) ->
    ok.

qualifier_binders({zip, _, Generators}) ->
    lists:append([qualifier_binders(G) || G <- Generators]);
qualifier_binders({Gen, _, Pattern, _Source}) when
    Gen =:= generate;
    Gen =:= generate_strict;
    Gen =:= b_generate;
    Gen =:= b_generate_strict;
    Gen =:= m_generate;
    Gen =:= m_generate_strict
->
    pattern_vars(Pattern);
qualifier_binders(_Filter) ->
    [].

%% Each children-list item that is a list -- written there or as a conditional's
%% tail -- is a content-slot list.
check_children_lists({cons, _, Item, Rest}, Vars) ->
    _ = map_tail_exprs(
        Item,
        fun(Leaf) ->
            ok = check_content_list(Leaf, Vars),
            Leaf
        end,
        fun(NonTail) -> NonTail end
    ),
    check_children_lists(Rest, Vars);
check_children_lists(_Children, _Vars) ->
    ok.

check_content_list({cons, _, {var, L, V}, Rest}, Vars) ->
    case Vars of
        #{V := _} -> parse_error({markup_var_in_content_list, V}, L);
        #{} -> check_content_list(Rest, Vars)
    end;
check_content_list({cons, _, _, Rest}, Vars) ->
    check_content_list(Rest, Vars);
check_content_list(_Leaf, _Vars) ->
    ok.

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
%% prepare_inline/3 extends an enclosing map (a branch body's, see
%% transform_scoped_tail/4), so a branch read can build on an outer one.
prepare_inline(Raw, Body) ->
    prepare_inline(Raw, Body, #{}).

prepare_inline(Raw, _Body, Outer) when map_size(Raw) =:= 0 ->
    Outer;
prepare_inline(Raw, Body, Outer) ->
    lists:foldl(fun(Stmt, Acc) -> prepare_inline_stmt(Stmt, Raw, Acc) end, Outer, Body).

prepare_inline_stmt({match, _, {var, _, V}, _RHS}, Raw, Acc) ->
    case Raw of
        #{V := RHS} -> Acc#{V => unmark_eaches(prepare_inline_rhs(RHS, Acc))};
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

%% A hoisted definition can bind names of its own -- `Owner = case ?get(user) of
%% #{name := N} -> N; _ -> ~"anon" end` binds `N`. Its copy is substituted into a slot
%% closure, where that binder would meet whatever the use site has in scope: a fun
%% parameter `N` turns the pattern into an equality test against it, and the original
%% statement left in the clause makes `N` unsafe there. So a name the definition binds
%% is renamed in the stored copy (hoist_site_copy/1 then makes it unique per use).
%% A name is the definition's own when it appears nowhere before it -- not in the
%% clause head, not in an earlier statement. That over-approximates the enclosing
%% scope, so the only error is to keep a name that could have been renamed: a pattern
%% variable already bound there tests that binding, and keeps reading it.
rename_hoisted_binders(_Patterns, _Body, Raw) when map_size(Raw) =:= 0 ->
    Raw;
rename_hoisted_binders(Patterns, Body, Raw) ->
    {Raw1, _Seen} = lists:foldl(
        fun(Stmt, {Acc, Seen}) ->
            Acc1 =
                case Stmt of
                    {match, _, {var, _, V}, RHS} -> rename_hoisted_rhs(V, RHS, Seen, Acc);
                    _ -> Acc
                end,
            {Acc1, var_names(Stmt, Seen)}
        end,
        {Raw, var_names(Patterns, #{})},
        Body
    ),
    Raw1.

%% Only the statement whose RHS was hoisted (the first match of `V`) is renamed.
rename_hoisted_rhs(V, RHS, Seen, Raw) ->
    case Raw of
        #{V := RHS} -> Raw#{V => rename_own_binders(V, RHS, Seen)};
        #{} -> Raw
    end.

rename_own_binders(Hoisted, RHS, Seen) ->
    Own = [V || V := _ <- var_names(RHS, #{}), not is_map_key(V, Seen)],
    Renames = [{V, hoisted_binder_var(Hoisted, V)} || V <- lists:sort(Own)],
    lists:foldl(fun rename_var/2, RHS, Renames).

var_names(Term, Acc) ->
    maps:merge(Acc, #{V => true || V <- collect_var_names(Term)}).

hoisted_binder_var(Hoisted, Var) ->
    with_unused_prefix(Var, "AzHoist@" ++ atom_to_list(Hoisted) ++ "@" ++ atom_to_list(Var)).

%% Every copy gets its own names: two copies in one closure (`<<Owner/binary,
%% Owner/binary>>`) would otherwise bind the same name twice, the second binding a
%% match against the first. Renaming binder and references together keeps the copy
%% alpha-equivalent.
hoist_site_copy(RHS) ->
    case [V || V <- lists:usort(collect_var_names(RHS)), is_hoisted_binder(V)] of
        [] ->
            RHS;
        Own ->
            Prev = get(?HOIST_SITE_KEY),
            Site = Prev + 1,
            Prev = put(?HOIST_SITE_KEY, Site),
            Suffix = "@" ++ integer_to_list(Site),
            Renames = [{V, list_to_atom(atom_to_list(V) ++ Suffix)} || V <- Own],
            lists:foldl(fun rename_var/2, RHS, Renames)
    end.

is_hoisted_binder(V) ->
    case atom_to_list(V) of
        "AzHoist@" ++ _ -> true;
        "_AzHoist@" ++ _ -> true;
        _ -> false
    end.

%% Keep a `_` prefix, or the rename turns a deliberately-unused variable into an
%% unused_var warning.
with_unused_prefix(Var, Base) ->
    case atom_to_list(Var) of
        [$_ | _] -> list_to_atom([$_ | Base]);
        _ -> list_to_atom(Base)
    end.

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
%% or its tracking runs outside any bracket (a no-op) and the slot freezes. Only a FREE
%% reference counts: a comprehension generator or a fun head binding the same name
%% reads its own variable, not the hoisted one.
rhs_reaches(RHS, Reaching) ->
    has_tracked_read(RHS) orelse
        lists:any(fun(V) -> is_map_key(V, Reaching) end, maps:keys(free_vars(RHS, #{}, #{}))).

has_tracked_read({call, _, {remote, _, {atom, _, Mod}, {atom, _, F}}, _Args}) when
    (Mod =:= arizona_template orelse Mod =:= az) andalso
        (F =:= get orelse F =:= get_lazy orelse F =:= track orelse F =:= with)
->
    true;
has_tracked_read(T) when is_tuple(T) ->
    has_tracked_read(tuple_to_list(T));
has_tracked_read([H | T]) ->
    has_tracked_read(H) orelse has_tracked_read(T);
has_tracked_read(_) ->
    false.

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
%% substitution can never produce an illegal pattern. A pattern or guard that reads an
%% inlined variable instead gets a touch of its definition ahead of it (wrap_touches/4).
inline_vars(Expr, Inline) when map_size(Inline) =:= 0 ->
    Expr;
%% A bare top-level fun is an ?each callback (compile_each is the only caller that passes a
%% fun straight to inline_vars); inline its clauses but DON'T wrap it -- compile_each needs a
%% fun literal, not a block. A fun NESTED in a content slot is wrapped by iv/2 below.
inline_vars({'fun', L, {clauses, Cs}}, Inline) ->
    {'fun', L, {clauses, [iv_fun_clause(C, Inline) || C <- Cs]}};
inline_vars({named_fun, L, Name, Cs}, Inline) ->
    Inline1 = bind([Name], named_fun, L, maps:remove(Name, Inline)),
    {named_fun, L, Name, [iv_fun_clause(C, Inline1) || C <- Cs]};
inline_vars(Expr, Inline) ->
    iv(Expr, Inline).

iv({var, _, V} = Var, Inline) ->
    case Inline of
        #{V := RHS} ->
            ok = check_capture(V, RHS, Inline),
            ok = check_hoisted_pure(V, RHS, Inline),
            iv(hoist_site_copy(RHS), Inline);
        #{} ->
            Var
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
    wrap_touches({Comp, L, iv(T, Inline1), Qs1}, L, generator_tracked_vars(Qs, Inline), Inline);
iv({block, L, B}, Inline) ->
    {block, L, iv_body(B, Inline)};
iv({match, L, P, E}, Inline) ->
    wrap_touches({match, L, P, iv(E, Inline)}, L, collect_tracked_vars(P, Inline), Inline);
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

%% Clause-bearing forms build their node and wrap it (wrap_touches/4) so a tracked
%% binding read in a clause pattern or guard is recorded as a slot dependency. Kept as
%% separate helpers so iv/2 stays a thin dispatcher.
iv_fun(L, Cs, Inline) ->
    Node = {'fun', L, {clauses, iv_fun_clauses(Cs, Inline)}},
    wrap_touches(Node, L, fun_tracked_vars(Cs, Inline), Inline).

iv_named_fun(L, Name, Cs, Inline) ->
    Inline1 = bind([Name], named_fun, L, maps:remove(Name, Inline)),
    Node = {named_fun, L, Name, iv_fun_clauses(Cs, Inline1)},
    wrap_touches(Node, L, fun_tracked_vars(Cs, Inline1), Inline1).

iv_case(L, E, Cs, Inline) ->
    Node = {'case', L, iv(E, Inline), iv_clauses(Cs, Inline)},
    wrap_touches(Node, L, clause_tracked_vars(Cs, Inline), Inline).

iv_if(L, Cs, Inline) ->
    wrap_touches({'if', L, iv_clauses(Cs, Inline)}, L, clause_tracked_vars(Cs, Inline), Inline).

iv_receive(L, Cs, Inline) ->
    Node = {'receive', L, iv_clauses(Cs, Inline)},
    wrap_touches(Node, L, clause_tracked_vars(Cs, Inline), Inline).

iv_receive(L, Cs, AE, AB, Inline) ->
    Node = {'receive', L, iv_clauses(Cs, Inline), iv(AE, Inline), iv_body(AB, Inline)},
    wrap_touches(Node, L, clause_tracked_vars(Cs, Inline), Inline).

iv_try(L, B, OfCs, CatchCs, Aft, Inline) ->
    Node =
        {'try', L, iv_body(B, Inline), iv_clauses(OfCs, Inline), iv_clauses(CatchCs, Inline),
            iv_body(Aft, Inline)},
    wrap_touches(Node, L, clause_tracked_vars(OfCs ++ CatchCs, Inline), Inline).

iv_maybe_else(L, B, L2, Cs, Inline) ->
    Node = {'maybe', L, iv_body(B, Inline), {'else', L2, iv_clauses(Cs, Inline)}},
    wrap_touches(Node, L, clause_tracked_vars(Cs, Inline), Inline).

%% Fun clause: parameters bind and shadow; drop them from the map for the body. Guards
%% stay untouched (a binding read can't live in a guard); a tracked binding read in a
%% nested fun's guard is auto-tracked by the enclosing fun node (wrap_touches/4 in
%% iv/2). A top-level ?each callback fun is inlined through here too but not wrapped (see
%% inline_vars/2: compile_each needs a single-clause fun, whose guards are over the item
%% param, not outer tracked vars).
iv_fun_clause({clause, L, Params, Guards, Body}, Inline) ->
    Inline1 = bind(
        pattern_binders(Params), fun_param, L, maps:without(pattern_vars(Params), Inline)
    ),
    {clause, L, Params, Guards, iv_body(Body, Inline1)}.

%% case/receive/try-of/catch clause: patterns match the (already inlined) scrutinee,
%% so patterns and guards are left untouched. An inline-map variable is bound in the
%% enclosing scope, so in such a pattern it is an equality test, not a new binding: the
%% clause body's use of it is the same value and stays substituted. A tracked binding
%% read in a pattern or guard is handled by the enclosing node via wrap_touches/4.
iv_clause({clause, L, Patterns, Guards, Body}, Inline) ->
    {clause, L, Patterns, Guards, iv_body(Body, Inline)}.

%% A binding read (`?get`/`get_lazy`/`with`-derived, hence in the inline map) cannot live
%% in a pattern or a guard -- Erlang forbids a function call there -- so both are left as
%% bound variables. The read then never re-runs inside the slot's dependency bracket, and
%% the slot would silently freeze on that binding. To keep the slot reactive, wrap the
%% expression in a block that first reads (for the `track/1` side effect) each tracked
%% binding its patterns and guards reference, recording them as slot dependencies. The
%% pattern or guard keeps using the captured value, which each diff cycle rebuilds from
%% the current bindings, so a change to that binding re-renders the slot. No tracked
%% var -> node returned unchanged.
wrap_touches(Node, _L, [], _Inline) ->
    Node;
wrap_touches(Node, L, Vars, Inline) ->
    {block, L, touches(Vars, L, Inline) ++ [Node]}.

%% Read each var's inlined definition (its get/get_lazy/with call) for the track side
%% effect, discarding the value. Reusing the inline expansion handles get/get_lazy/with
%% and transitively-derived vars uniformly without extracting the binding key.
touches(Vars, L, Inline) ->
    [{match, L, {var, L, '_'}, iv({var, L, V}, Inline)} || V <- Vars].

%% Inline-map variables read by case-like clauses (first-seen order). Every one in a
%% pattern is a read: it is already bound, so the pattern tests it.
clause_tracked_vars(Clauses, Inline) ->
    collect_tracked_vars(
        [[Patterns, Guards] || {clause, _, Patterns, Guards, _} <:- Clauses], Inline
    ).

%% Fun clauses bind their parameters, which shadow the enclosing scope for the guard;
%% only a parameter pattern's map keys and binary sizes read it.
fun_tracked_vars(Clauses, Inline) ->
    lists:reverse(
        lists:foldl(
            fun({clause, _, Params, Guards, _}, Acc) ->
                Acc1 = collect_guard_vars(pattern_reads(Params), Inline, Acc),
                collect_guard_vars(Guards, maps:without(pattern_vars(Params), Inline), Acc1)
            end,
            [],
            Clauses
        )
    ).

%% Generator patterns bind (shadowing for later qualifiers), but their map keys and
%% binary sizes read the scope the generator sits in.
generator_tracked_vars(Qs, Inline) ->
    {Reads, _} = lists:foldl(
        fun(Q, {Acc, Inl}) ->
            Acc1 = collect_guard_vars(generator_reads(Q), Inl, Acc),
            {Acc1, maps:without(pattern_vars(qual_patterns(Q)), Inl)}
        end,
        {[], Inline},
        Qs
    ),
    lists:reverse(Reads).

qual_patterns({zip, _, Gens}) ->
    [P || {_, _, P, _} <:- Gens];
qual_patterns({Gen, _, P, _}) when ?IS_GENERATOR(Gen) ->
    [P];
qual_patterns(_Filter) ->
    [].

%% A map generator's `K := V` is two patterns (both bind), not a map pattern whose key
%% is read.
generator_reads({zip, _, Gens}) ->
    lists:append([generator_reads(G) || G <- Gens]);
generator_reads({Gen, _, {map_field_exact, _, K, V}, _}) when
    Gen =:= m_generate; Gen =:= m_generate_strict
->
    pattern_reads([K, V]);
generator_reads({Gen, _, P, _}) when ?IS_GENERATOR(Gen) ->
    pattern_reads(P);
generator_reads(_Filter) ->
    [].

collect_tracked_vars(Form, Inline) ->
    lists:reverse(collect_guard_vars(Form, Inline, [])).

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

%% A hoisted definition substituted into a slot runs there as well as in the body, so
%% an impure one would produce a different value in each place. Helper arguments are
%% exempt here: they are substituted once per use and checked against that count.
check_hoisted_pure(_V, _RHS, #{'$helper_args' := true}) ->
    ok;
check_hoisted_pure(V, RHS, _Inline) ->
    case impure_expr(RHS) of
        none -> ok;
        {Line, What} -> parse_error({template_impure_expr, {hoisted, V}, What}, Line)
    end.

%% The first always-impure construct in an expression, as `{Line, Description}`, or
%% `none`. A fun that is only constructed is not entered: its body runs when called,
%% not where it is written. Calls into user code are assumed pure.
impure_expr({op, L, '!', _, _}) ->
    {L, "a send (!)"};
impure_expr({'receive', L, _}) ->
    {L, "a receive"};
impure_expr({'receive', L, _, _, _}) ->
    {L, "a receive"};
impure_expr({'fun', _, _}) ->
    none;
impure_expr({named_fun, _, _, _}) ->
    none;
impure_expr({call, L, Callee, Args}) ->
    case impure_call(Callee, length(Args)) of
        false -> impure_expr_list([Callee | Args]);
        What -> {L, What}
    end;
impure_expr(T) when is_tuple(T) ->
    impure_expr_list(tuple_to_list(T));
impure_expr(L) when is_list(L) ->
    impure_expr_list(L);
impure_expr(_Leaf) ->
    none.

impure_expr_list([]) ->
    none;
impure_expr_list([H | T]) ->
    case impure_expr(H) of
        none -> impure_expr_list(T);
        Found -> Found
    end.

impure_call({remote, _, {atom, _, M}, {atom, _, F}}, A) ->
    case is_impure_mfa(M, F, A) of
        true -> lists:flatten(io_lib:format("a call to ~s:~s/~p", [M, F, A]));
        false -> false
    end;
impure_call({atom, _, F}, A) ->
    case is_impure_mfa(erlang, F, A) andalso erl_internal:bif(F, A) of
        true -> lists:flatten(io_lib:format("a call to ~s/~p", [F, A]));
        false -> false
    end;
impure_call(_Callee, _A) ->
    false.

is_impure_mfa(M, _F, _A) when
    M =:= rand; M =:= os; M =:= ets; M =:= io; M =:= logger
->
    true;
is_impure_mfa(calendar, F, 0) ->
    F =:= local_time orelse F =:= universal_time;
is_impure_mfa(erlang, F, A) ->
    lists:member({F, A}, [
        {make_ref, 0},
        {unique_integer, 0},
        {unique_integer, 1},
        {now, 0},
        {timestamp, 0},
        {system_time, 0},
        {system_time, 1},
        {monotonic_time, 0},
        {monotonic_time, 1},
        {put, 2},
        {erase, 0},
        {erase, 1}
    ]) orelse lists:prefix("spawn", atom_to_list(F));
is_impure_mfa(_M, _F, _A) ->
    false.

%% A body is a sequence; a `Var = RHS` match binds Var (shadowing) for later exprs.

%% A body is a sequence; a `Var = RHS` match binds Var (shadowing) for later exprs. A
%% `?=` must stay a direct expression of its `maybe` body, so the touches for the
%% variables its pattern tests go ahead of it there rather than in a wrapping block.
iv_body(Exprs, Inline) ->
    {Rev, _} = lists:foldl(
        fun(E, {Acc, Inl}) ->
            Inl1 =
                case E of
                    {match, _, P, _} -> maps:without(pattern_vars(P), Inl);
                    {maybe_match, _, P, _} -> maps:without(pattern_vars(P), Inl);
                    _ -> Inl
                end,
            {[iv(E, Inl) | lists:reverse(maybe_match_touches(E, Inl)) ++ Acc], Inl1}
        end,
        {[], Inline},
        Exprs
    ),
    lists:reverse(Rev).

maybe_match_touches({maybe_match, L, P, _}, Inline) ->
    touches(collect_tracked_vars(P, Inline), L, Inline);
maybe_match_touches(_E, _Inline) ->
    [].

%% Comprehension qualifiers, left to right: generator patterns bind for subsequent
%% qualifiers and the template; filters are plain expressions. A zip generator
%% evaluates all its sources in the enclosing scope, then binds all its patterns.
iv_quals(Qs, Inline) ->
    lists:mapfoldl(fun iv_qual/2, Inline, Qs).

iv_qual({zip, L, Gens}, Inline) ->
    Gens1 = [{Gen, GL, P, iv(E, Inline)} || {Gen, GL, P, E} <:- Gens],
    {{zip, L, Gens1}, bind_generators([map_gen_as_tuple(P) || {_, _, P, _} <:- Gens], L, Inline)};
iv_qual({Gen, L, P, E}, Inline) when
    Gen =:= generate;
    Gen =:= generate_strict;
    Gen =:= b_generate;
    Gen =:= b_generate_strict;
    Gen =:= m_generate;
    Gen =:= m_generate_strict
->
    {{Gen, L, P, iv(E, Inline)}, bind_generators([map_gen_as_tuple(P)], L, Inline)};
iv_qual(Filter, Inline) ->
    {iv(Filter, Inline), Inline}.

bind_generators(Patterns, L, Inline) ->
    bind(pattern_binders(Patterns), generator, L, maps:without(pattern_vars(Patterns), Inline)).

%% A fun head, a named fun and a generator bind names in a new scope. A hoisted
%% definition substituted inside that scope was written against the enclosing one, so
%% one that reads a name bound there would silently read the inner binding instead.
%% The scope's binders are recorded under a key no variable can have, and
%% check_capture/3 refuses such a substitution at compile time.
bind([], _Kind, _Line, Inline) ->
    Inline;
bind(Names, Kind, Line, Inline) ->
    Binders = maps:get('$binders', Inline, #{}),
    Inline#{'$binders' => maps:merge(Binders, #{N => {Kind, Line} || N <- Names})}.

check_capture(Hoisted, RHS, #{'$binders' := Binders}) ->
    case [B || B := _ <- free_vars(RHS, #{}, #{}), is_map_key(B, Binders)] of
        [] ->
            ok;
        Captured ->
            Binder = lists:min(Captured),
            #{Binder := {Kind, Line}} = Binders,
            parse_error({hoisted_var_captured, Kind, Binder, Hoisted}, Line)
    end;
check_capture(_Hoisted, _RHS, _Inline) ->
    ok.

%% The names a pattern binds. A map key and a binary segment size are expressions the
%% pattern reads, not binders.
pattern_binders(Pattern) ->
    maps:keys(binder_vars(Pattern, #{})).

binder_vars({map_field_exact, _, _Key, Value}, Acc) ->
    binder_vars(Value, Acc);
binder_vars({bin_element, _, Value, _Size, _Types}, Acc) ->
    binder_vars(Value, Acc);
binder_vars({var, _, '_'}, Acc) ->
    Acc;
binder_vars({var, _, V}, Acc) ->
    Acc#{V => true};
binder_vars(T, Acc) when is_tuple(T) ->
    binder_vars(tuple_to_list(T), Acc);
binder_vars([H | T], Acc) ->
    binder_vars(T, binder_vars(H, Acc));
binder_vars(_Leaf, Acc) ->
    Acc.

%% The expressions a pattern reads: its map keys and binary segment sizes.
pattern_reads({map_field_exact, _, Key, Value}) ->
    [Key | pattern_reads(Value)];
pattern_reads({bin_element, _, Value, Size, _Types}) ->
    [Size | pattern_reads(Value)];
pattern_reads(T) when is_tuple(T) ->
    pattern_reads(tuple_to_list(T));
pattern_reads(L) when is_list(L) ->
    lists:append([pattern_reads(E) || E <- L]);
pattern_reads(_Leaf) ->
    [].

%% The variables an expression reads from its enclosing scope: a name a fun head, a
%% named fun or a generator binds inside it is not free there, while a pattern's map
%% keys and binary sizes are reads. A match or case pattern variable is counted free:
%% it may test a variable of the enclosing scope.
free_vars({var, _, '_'}, _Bound, Acc) ->
    Acc;
free_vars({var, _, V}, Bound, Acc) ->
    case Bound of
        #{V := _} -> Acc;
        #{} -> Acc#{V => true}
    end;
free_vars({'fun', _, {clauses, Cs}}, Bound, Acc) ->
    free_vars_fun_clauses(Cs, Bound, Acc);
free_vars({named_fun, _, Name, Cs}, Bound, Acc) ->
    free_vars_fun_clauses(Cs, Bound#{Name => true}, Acc);
free_vars({Comp, _, T, Qs}, Bound, Acc) when Comp =:= lc; Comp =:= bc; Comp =:= mc ->
    free_vars_quals(Qs, T, Bound, Acc);
free_vars(T, Bound, Acc) when is_tuple(T) ->
    free_vars(tuple_to_list(T), Bound, Acc);
free_vars([H | T], Bound, Acc) ->
    free_vars(T, Bound, free_vars(H, Bound, Acc));
free_vars(_Leaf, _Bound, Acc) ->
    Acc.

free_vars_fun_clauses(Cs, Bound, Acc) ->
    lists:foldl(
        fun({clause, _, Params, Guards, Body}, A) ->
            Bound1 = binder_vars(Params, Bound),
            free_vars([pattern_reads(Params), Guards, Body], Bound1, A)
        end,
        Acc,
        Cs
    ).

free_vars_quals([], T, Bound, Acc) ->
    free_vars(T, Bound, Acc);
free_vars_quals([{zip, _, Gens} | Rest], T, Bound, Acc) ->
    free_vars_generators(Gens, Rest, T, Bound, Acc);
free_vars_quals([{Gen, _, _, _} = G | Rest], T, Bound, Acc) when
    Gen =:= generate;
    Gen =:= generate_strict;
    Gen =:= b_generate;
    Gen =:= b_generate_strict;
    Gen =:= m_generate;
    Gen =:= m_generate_strict
->
    free_vars_generators([G], Rest, T, Bound, Acc);
free_vars_quals([Filter | Rest], T, Bound, Acc) ->
    free_vars_quals(Rest, T, Bound, free_vars(Filter, Bound, Acc)).

free_vars_generators(Gens, Rest, T, Bound, Acc) ->
    Patterns = [map_gen_as_tuple(P) || {_, _, P, _} <:- Gens],
    Acc1 = free_vars([[E || {_, _, _, E} <:- Gens] | pattern_reads(Patterns)], Bound, Acc),
    free_vars_quals(Rest, T, binder_vars(Patterns, Bound), Acc1).

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

%% A match on a name the head or an earlier statement binds is an equality test:
%% it is inlined (the slot stays tracked) but never underscored.
without_prebound(Inline, Patterns, Body) ->
    without_prebound(Inline, Body, bind_names(pattern_vars(Patterns), #{}), []).

without_prebound(Inline, [], _Bound, Drop) ->
    maps:without(Drop, Inline);
without_prebound(Inline, [{match, _, {var, _, V}, _} = Stmt | Rest], Bound, Drop) when
    is_map_key(V, Bound)
->
    without_prebound(Inline, Rest, stmt_bound(Stmt, Bound), [V | Drop]);
without_prebound(Inline, [Stmt | Rest], Bound, Drop) ->
    without_prebound(Inline, Rest, stmt_bound(Stmt, Bound), Drop).

%% After inlining, a variable used only inside the template no longer appears outside
%% its own binding. Underscore-prefix such matches so `warnings_as_errors` (unused
%% variable) doesn't reject the module; matches still referenced elsewhere are kept.
suppress_unused_inline_matches(Body, Inline) when map_size(Inline) =:= 0 ->
    Body;
suppress_unused_inline_matches(Body, Inline) ->
    [maybe_underscore_match(E, Inline, Body) || E <- Body].

%% A hoisted name the source never used is the author's unused variable, not one the
%% template splice left behind, so it is dropped from the map and keeps erl_lint's
%% warning. `Others` is the rest of the source the body's names reach.
without_source_unused(Inline, Body, Others) ->
    maps:without(
        [
            V
         || {match, _, {var, _, V}, _} = M <- Body,
            not is_map_key(V, free_vars([Body -- [M] | Others], #{}, #{}))
        ],
        Inline
    ).

%% Rename the now-unused match LHS to the anonymous `_`: it never collides with a
%% pre-existing `_Foo` binding (which would otherwise trip erl_lint's
%% match_underscore_var) and never warns.
%% A use is a FREE reference anywhere else in the body: a generator or fun head that
%% rebinds the name is its own variable.
maybe_underscore_match({match, L, {var, VL, V}, RHS} = M, Inline, Body) ->
    case is_map_key(V, Inline) andalso not is_map_key(V, free_vars(Body -- [M], #{}, #{})) of
        true -> {match, L, {var, VL, '_'}, RHS};
        false -> M
    end;
maybe_underscore_match(E, _Inline, _Body) ->
    E.

compile_template(Arg, Line, Module, LiveRender, Backend) ->
    compile_template(Arg, Line, Module, LiveRender, Backend, false).

%% `Inherited` is true for a template nested in an `az-nodiff` one (see
%% inherit_nodiff/1): it compiles nodiff whatever its own attributes say.
compile_template(Arg, Line, Module, LiveRender, Backend, Inherited) ->
    compile_template(Arg, Line, Module, LiveRender, Backend, Inherited, html).

%% `CCtx` is the content context the template renders in: a conditional tail's
%% element compiles in the context of the slot holding it (an SVG `<script>` or
%% `<title>` in a branch under `<svg>` is foreign content, as it is written directly).
compile_template(Arg, Line, Module, LiveRender, Backend, Inherited, CCtx) ->
    {Statics, DynASTs, Fingerprint, Opts0} = compile_body_parts(
        Arg, Module, LiveRender, Backend, CCtx, Inherited, template
    ),
    Opts = maybe_multi_root_opt(Backend, Arg, Statics, DynASTs, Opts0#{backend => Backend}),
    {S1, D1} = scope_az(Backend, Fingerprint, Statics, DynASTs),
    TmplAST = build_template_ast(Line, S1, D1, Fingerprint, Opts),
    case {Opts, Inherited} of
        {#{diff := false}, false} -> put_nodiff_key(Arg, TmplAST);
        _ -> TmplAST
    end.

%% A template nested as a child may hold several nodes, but one that is a whole
%% document may not on a backend whose document is one node (native: a JSON
%% value). Only render knows which templates are documents (a layout is any
%% function a route names), so stamp the shape here, asking the backend at compile
%% time, and arizona_render refuses a layout whose document root ends up stamped.
%%
%% Several roots: a body of two or more top-level items, or a body that compiled
%% to nothing but one bare dynamic holding a stamped template -- the shape a
%% literal list one level deeper (`[[A, B]]`) compiles to, so the stamp carries up
%% through any depth of such lists. A body whose roots are only known at runtime
%% is not judged here; arizona_render passes the wrapped root's state through it.
maybe_multi_root_opt(Backend, Arg, Statics, DynASTs, Opts) ->
    case
        Backend:single_root_document() andalso
            (is_multi_node_list(Arg) orelse holds_multi_root(Statics, DynASTs))
    of
        true -> Opts#{multi_root => true};
        false -> Opts
    end.

is_multi_node_list({cons, _, _, {cons, _, _, _}}) -> true;
is_multi_node_list(_Arg) -> false.

holds_multi_root(Statics, [{tuple, _, [_Az, FunAST, _Loc]}]) ->
    lists:all(fun(Static) -> lists:all(fun(Seg) -> Seg =:= <<>> end, Static) end, Statics) andalso
        is_multi_root_fun(FunAST);
holds_multi_root(_Statics, _DynASTs) ->
    false.

is_multi_root_fun({'fun', _, {clauses, [{clause, _, [], [], Body}]}}) ->
    case lists:last(Body) of
        {map, _, Fields} ->
            lists:any(
                fun
                    ({map_field_assoc, _, {atom, _, multi_root}, {atom, _, true}}) -> true;
                    (_Field) -> false
                end,
                Fields
            );
        _Other ->
            false
    end;
is_multi_root_fun(_FunAST) ->
    false.

%% An `az_key` on an az-nodiff root names the az-nodiff element beside its fingerprint and
%% branch, so a new key value replaces the az-nodiff element. The key is built with the
%% template, inside the slot that holds it, so its reads are that slot's deps.
put_nodiff_key({tuple, _, [_Tag, Attrs | _]}, {map, L, Fields} = TmplAST) ->
    case [V || {tuple, _, [NameAST, V]} <- attr_list(Attrs), is_az_key(NameAST)] of
        [KeyAST] -> {map, L, Fields ++ [{map_field_assoc, L, {atom, L, az_key}, KeyAST}]};
        [] -> TmplAST
    end;
put_nodiff_key(_Arg, TmplAST) ->
    TmplAST.

attr_list({cons, _, Head, Tail}) -> [Head | attr_list(Tail)];
attr_list(_Attrs) -> [].

is_az_key(NameAST) ->
    case bare_attr_name(NameAST) of
        {ok, Name} -> framework_attr_name(Name) =:= ~"az-key";
        error -> false
    end.

compile_each(FunAST, SourceAST, Line, Module, Backend, CCtx, FunDefs, Inherited) ->
    case FunAST of
        {'fun', _, {clauses, [{clause, _, [ItemVar, KeyVar], Guards, Body}]}} ->
            compile_each_clause(
                stream,
                [ItemVar, KeyVar],
                Guards,
                Body,
                SourceAST,
                Line,
                Module,
                Backend,
                CCtx,
                Inherited
            );
        {'fun', _, {clauses, [{clause, _, [ItemVar], Guards, Body}]}} ->
            compile_each_clause(
                list, [ItemVar], Guards, Body, SourceAST, Line, Module, Backend, CCtx, Inherited
            );
        %% Local `fun Name/1` or `fun Name/2` ref: resolve its single clause and compile
        %% it exactly like an inline fun, so the same element-body validation runs. The
        %% looked-up clause is the original untransformed body, which is what the inline
        %% path expects. (Its now-orphaned definition is covered by the injected
        %% nowarn_unused_function / ignore_xref attributes.)
        {'fun', L, {function, Name, Arity}} when Arity =:= 1; Arity =:= 2 ->
            compile_named_each(
                Name, Arity, SourceAST, L, Module, Backend, CCtx, FunDefs, Inherited
            );
        %% A local ref of any other arity isn't a valid callback (1 = list, 2 = stream/map).
        {'fun', L, {function, _Name, _Arity}} ->
            parse_error(invalid_each_fun, L);
        %% Same-module explicit ref `fun ?MODULE:Name/Arity` (literal module = this module):
        %% the body is visible here, so rewrite to the bare local form and re-dispatch -- it
        %% then behaves exactly like `fun Name/Arity` (resolve + inline, or the same
        %% arity/multi-clause/undefined errors).
        {'fun', L, {function, {atom, _, Module}, {atom, _, Name}, {integer, _, Arity}}} ->
            compile_each(
                {'fun', L, {function, Name, Arity}},
                SourceAST,
                Line,
                Module,
                Backend,
                CCtx,
                FunDefs,
                Inherited
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
compile_named_each(Name, Arity, SourceAST, L, Module, Backend, CCtx, FunDefs, Inherited) ->
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
            %% Helpers first, so the bodies they splice are inherited too.
            {FunAST1, _HelperCh} = inline_helper_calls(
                FunAST0, Module, FunDefs, Backend:target()
            ),
            FunAST = transform_expr(
                inherit_spliced_each_nodiff(Inherited, FunAST1), Module, #{}, FunDefs
            ),
            %% Stamp the each with the CALLEE's clause line, not the `?each` call
            %% site: the whole per-item template comes from that clause, so a crash
            %% in it reports a stack line inside the callee while `arizona_loc` named
            %% a different function entirely. The error page reconciles the two from
            %% the stack, but raw surfaces cannot -- the dev MCP's `render_component`
            %% catches with no stacktrace at all, so this line is the only one an
            %% agent ever sees. The source expression is unaffected: it evaluates in
            %% the enclosing slot's closure, which carries the call site's line.
            compile_each(FunAST, SourceAST, CL, Module, Backend, CCtx, FunDefs, Inherited);
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
compile_each_clause(Kind, Vars, Guards, Body, SourceAST, Line, Module, Backend, CCtx, Inherited) ->
    {Prefix, LastExpr} = split_fun_body(Body),
    case each_body_unwrap(LastExpr, Backend) of
        {compiled, Map} ->
            build_each_from_compiled(Line, SourceAST, Vars, Guards, Prefix, Map);
        {element, ElemAST0} ->
            ElemAST = unwrap_one_root_list(Kind, ElemAST0),
            Classification = classify_body(ElemAST),
            ok = validate_each_body(Kind, Classification, ElemAST),
            %% The per-item template compiles in the content context its `?each`
            %% sits in, so an element the backend classifies differently there
            %% (an SVG `<title>`) is treated the same as if it were written
            %% literally at that position.
            {Statics, DynASTs, Fingerprint, Opts0} = compile_body_parts(
                ElemAST, Module, false, Backend, CCtx, Inherited, each_item
            ),
            Opts1 = Opts0#{backend => Backend},
            Opts2 = maybe_single_root_opt(Backend, Kind, Classification, Opts1),
            Opts = maybe_multi_root_opt(Kind, Classification, Opts2),
            {S1, D1} = scope_az(Backend, Fingerprint, Statics, DynASTs),
            build_each_ast(Line, SourceAST, Vars, Guards, Prefix, S1, D1, Fingerprint, Opts)
    end.

%% A keyed body written as a one-element list (`[{li, [{az_key, K}], [I]}]`) is a single
%% root in a list wrapper: unwrapped like a whole-body `?html`, so it compiles to the
%% same per-item template as the bare element instead of being stamped multi-root.
unwrap_one_root_list(stream, {cons, _, Elem, {nil, _}} = Body) ->
    case is_element_tuple(Elem) of
        true -> Elem;
        false -> Body
    end;
unwrap_one_root_list(_Kind, Body) ->
    Body.

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

%% A 2-arg callback serves a map AND a stream, and only the source decides whether a
%% body that is not exactly one element is legal: a map diffs it positionally or
%% wholesale like a list, while a stream's keyed ops address an item by the single
%% element carrying its `az-key`, so any other root would be orphaned on a remove and
%% out of order on a move. The source is a runtime value, so the body is stamped here
%% and `arizona_template:each/2` refuses the stream pairing when it meets one.
maybe_multi_root_opt(stream, Classification, Opts) when
    Classification =:= element_list;
    Classification =:= list_ast;
    Classification =:= static_binary
->
    Opts#{multi_root => true};
maybe_multi_root_opt(_Kind, _Classification, Opts) ->
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

%% A compiled template map literal carries the `s`/`d`/`f` assoc keys plus the
%% `backend` build_template_ast/5 always adds, so a user data map spelling out
%% `s`/`d`/`f` is not taken for one. A field with a non-atom key (`~"a" => 1`, a
%% variable) is simply not a template key -- lazy, so it cannot crash the transform.
is_compiled_template_map(Fields) ->
    Keys = [K || {map_field_assoc, _, {atom, _, K}, _} <- Fields],
    lists:all(fun(Key) -> lists:member(Key, Keys) end, [s, d, f, backend]).

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
    NewFields =
        [TField | [set_template_map_d_field(Field, DFunAST) || Field <- Fields]] ++
            reuse_field(MapLine, Vars1, Prefix1, DListAST1),
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
%% fix advice, since a list has a comprehension fallback and a stream does not. Whether a
%% 2-arg body must be ONE element depends on the runtime source (see maybe_multi_root_opt/3).
validate_each_body(Kind, text_dynamic, LastExpr) ->
    parse_error(each_body_error(Kind, LastExpr), anno(LastExpr));
validate_each_body(Kind, list_ast, LastExpr) ->
    %% A mixed-list fragment is fine UNLESS an item is a nested template (a transformed
    %% ?html/?native/?terminal map literal) or a ?stateful/?stateless descriptor: those land
    %% in a per-item value slot and crash on diff exactly like a bare body. (A component as
    %% an ?each item child is a known limitation -- it crashes on the per-item diff for now.)
    walk_each_list_items(Kind, LastExpr);
validate_each_body(_Kind, _Classification, _LastExpr) ->
    ok.

%% A control-flow body is a different mistake from a bare value, and only the keyed
%% path needs telling apart: "wrap the value in an element" reads as advice to pick a
%% tag, which for branches that already return different elements changes the markup
%% instead of fixing it. The list message names the conditional case already.
each_body_error(list, _LastExpr) ->
    each_body_not_element;
each_body_error(stream, LastExpr) ->
    case is_control_flow_ast(LastExpr) of
        true -> each_stream_body_control_flow;
        false -> each_stream_body_not_element
    end.

walk_each_list_items(Kind, {cons, _, Item, Tail}) ->
    case is_fragile_each_item(Item) of
        true -> parse_error(each_body_error(Kind, Item), anno(Item));
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

%% `Unit` says what is being compiled: a `template`, or an `each_item` -- the
%% per-item template of an `?each`, whose root is an az-nodiff element only when keyed (see
%% is_keepable_nodiff/4).
compile_body_parts(ExprAST, Module, LiveRender, Backend, CCtx, Inherited, Unit) ->
    case classify_body(ExprAST) of
        element_tuple ->
            compile_fragment_parts([ExprAST], Module, LiveRender, Backend, CCtx, {Inherited, Unit});
        Class ->
            compile_classified_body(Class, ExprAST, Module, LiveRender, Backend, CCtx, Inherited)
    end.

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

compile_classified_body(
    static_binary, ExprAST, _Module, _LiveRender, _Backend, _CCtx, _Inherited
) ->
    Statics = [[extract_binary_value(ExprAST)]],
    {Statics, [], generate_fingerprint(Statics), #{}};
compile_classified_body(element_list, ExprAST, Module, LiveRender, Backend, CCtx, Inherited) ->
    compile_fragment_parts(
        ast_list_to_list(ExprAST), Module, LiveRender, Backend, CCtx, {Inherited, template}
    );
compile_classified_body(list_ast, ExprAST, Module, _LiveRender, Backend, CCtx, Inherited) ->
    compile_mixed_items(ast_list_to_list(ExprAST), Module, Backend, CCtx, Inherited);
compile_classified_body(text_dynamic, ExprAST, Module, _LiveRender, Backend, CCtx, true) ->
    %% Nodiff by inheritance: the enclosing template never diffs, so the slot needs
    %% no anchor and renders once, like any nodiff value.
    ok = reject_orphaned_local(ExprAST),
    DynASTs = [make_nodiff_dynamic_ast(ExprAST, Module, line(ExprAST), Backend, CCtx)],
    Statics = [[], []],
    {Statics, DynASTs, generate_fingerprint(Statics), #{diff => false}};
compile_classified_body(text_dynamic, ExprAST, Module, _LiveRender, Backend, CCtx, false) ->
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
    DynASTs = [make_esc_text_dynamic_ast(~"0", ExprAST, Module, line(ExprAST), Backend, CCtx)],
    {Statics, DynASTs, generate_fingerprint(Statics), #{}}.

%% A ?local outside an element -- a whole template body, a fragment top-level
%% item, or a conditional-branch tail -- gets a slot but no enclosing element
%% to carry the `az-local` descriptor (only compile_element/5 injects it via
%% maybe_inject_local_descriptor/5). The client binds keys by scanning that
%% attribute, so the key would be unreachable and set/set_all silent no-ops.
%% Reject at compile time.
reject_orphaned_local(ExprAST) ->
    case is_local_marker(ExprAST) of
        true -> parse_error(local_orphaned, anno(ExprAST));
        false -> ok
    end.

compile_fragment_parts(ElementASTs, Module, LiveRender, Backend, CCtx, {Inherited, Unit}) ->
    Opts = prescan_directives(ElementASTs, Inherited),
    Keepable = is_keepable_nodiff(ElementASTs, Inherited, Unit, Backend),
    Marked =
        not Inherited andalso not lists:any(fun contains_inner_content/1, ElementASTs) andalso
            Backend:supports_nodiff_keep(),
    State0 = #state{
        module = Module,
        nodiff = maps:is_key(diff, Opts),
        live_render = LiveRender,
        root = LiveRender,
        backend = Backend,
        content_ctx = CCtx
    },
    State1 = fold_fragment_items(
        fun(Elem, State) ->
            {Tag, Attrs0, Children, ElemLine} = extract_element(Elem),
            {Attrs, ElemOpts} = extract_directives(Attrs0),
            Mark = nodiff_mark(ElemOpts, Marked, Keepable),
            compile_element(Tag, Attrs, Children, ElemLine, State#state{nodiff_mark = Mark})
        end,
        State0,
        ElementASTs
    ),
    {Statics, DynASTs0} = finalize(State1),
    Fingerprint = generate_fingerprint(Statics),
    DynASTs =
        case Keepable of
            true -> fill_nodiff_fingerprint(Fingerprint, DynASTs0);
            false -> DynASTs0
        end,
    {Statics, DynASTs, Fingerprint, Opts}.

%% The `az-nodiff` directive stays in the HTML on the element that carries it,
%% compiler-owned, when the backend's client keeps az-nodiff elements across a wholesale
%% re-render (`supports_nodiff_keep/0`) -- not in a template that only inherits
%% nodiff (it is part of the enclosing az-nodiff element) nor in a layout (nodiff for
%% `?inner_content`, rendered once outside any view). Its value is the az-nodiff element's
%% token when the element is keepable (is_keepable_nodiff/4); otherwise it is
%% bare -- client-owned markup the client must never carry over: an unkeyed `?each`
%% item root, or a root of a multi-element fragment.
nodiff_mark(#{diff := false}, true, true) -> token;
nodiff_mark(#{diff := false}, true, false) -> bare;
nodiff_mark(_ElemOpts, _Marked, _Keepable) -> none.

%% A keepable az-nodiff element is the single root element of a template that carries the directive
%% itself -- not one that inherits it from an enclosing az-nodiff element, which is part of that
%% az-nodiff element, and not a layout, which is nodiff for `?inner_content`. Its root carries its
%% token as the `az-nodiff` value.
%%
%% An `?each` item is named only when it carries an `az_key`. Every item shares one
%% template, so an unkeyed item's name says nothing about WHICH item it is, and a
%% list re-renders wholesale exactly when the server cannot match items up -- a
%% client matching them by position would hand one item's widget to another.
is_keepable_nodiff([Elem], false, Unit, Backend) ->
    case is_element_tuple(Elem) of
        true ->
            {_Tag, Attrs, _Children, _Line} = extract_element(Elem),
            {_Rest, ElemOpts} = extract_directives(Attrs),
            maps:is_key(diff, ElemOpts) andalso not contains_inner_content(Elem) andalso
                (Unit =:= template orelse has_az_key(Attrs)) andalso
                Backend:supports_nodiff_keep();
        false ->
            false
    end;
is_keepable_nodiff(_ElementASTs, _Inherited, _Unit, _Backend) ->
    false.

has_az_key(Attrs) ->
    lists:any(
        fun
            ({tuple, _, [NameAST, _V]}) -> is_az_key(NameAST);
            (_Attr) -> false
        end,
        Attrs
    ).

%% The `az-nodiff` token: `arizona_template:nodiff_token/3` over the template's
%% fingerprint, the branch that produced it and its `az_key` -- the identity the
%% diff keeps an az-nodiff element by (`arizona_template:same_nodiff/3`). The fingerprint is
%% known only once the statics are, so the call is built with a placeholder and
%% filled in here; the branch is stamped later, by stamp_leaf/2 or
%% `arizona_template:branch/2`.
nodiff_token_attr(Attrs, Line) ->
    KeyAST =
        case [V || {tuple, _, [NameAST, V]} <- Attrs, is_az_key(NameAST)] of
            [K] -> K;
            [] -> {atom, Line, undefined}
        end,
    Call = nodiff_token_call(Line, {atom, Line, ?NODIFF_FP}, {atom, Line, undefined}, KeyAST),
    {tuple, Line, [{atom, Line, 'az-nodiff'}, Call]}.

maybe_inject_nodiff(Attrs, Line, #state{nodiff_mark = token}) ->
    [nodiff_token_attr(Attrs, Line) | Attrs];
maybe_inject_nodiff(Attrs, Line, #state{nodiff_mark = bare}) ->
    [{atom, Line, 'az-nodiff'} | Attrs];
maybe_inject_nodiff(Attrs, _Line, #state{nodiff_mark = none}) ->
    Attrs.

nodiff_token_call(L, FpAST, BranchAST, KeyAST) ->
    {call, L, {remote, L, {atom, L, arizona_template}, {atom, L, nodiff_token}}, [
        FpAST, BranchAST, KeyAST
    ]}.

fill_nodiff_fingerprint(Fingerprint, DynASTs) ->
    replace_ast(
        fun
            ({atom, _, ?NODIFF_FP}) -> {ok, ast_binary(Fingerprint)};
            (_Node) -> error
        end,
        DynASTs
    ).

%% Rewrites the token's branch argument in a literal az-nodiff template's
%% `d` field (stamp_leaf/2): a template whose branch is known at compile time.
stamp_nodiff_branch(N, Fields) ->
    [stamp_nodiff_branch_field(N, F) || F <- Fields].

stamp_nodiff_branch_field(N, {map_field_assoc, L, {atom, _, d} = K, DAST}) ->
    {map_field_assoc, L, K,
        replace_ast(
            fun
                (
                    {call, CL, {remote, _, {atom, _, arizona_template}, {atom, _, nodiff_token}}, [
                        FpAST, {atom, _, undefined}, KeyAST
                    ]}
                ) ->
                    {ok, nodiff_token_call(CL, FpAST, {integer, CL, N}, KeyAST)};
                (_Node) ->
                    error
            end,
            DAST
        )};
stamp_nodiff_branch_field(_N, Field) ->
    Field.

%% AST substitution: `Fun` answers `{ok, New}` for a node it
%% replaces (not descended into) and `error` for one to walk.
replace_ast(Fun, Node) when is_tuple(Node) ->
    case Fun(Node) of
        {ok, New} -> New;
        error -> list_to_tuple([replace_ast(Fun, E) || E <- tuple_to_list(Node)])
    end;
replace_ast(Fun, Nodes) when is_list(Nodes) ->
    [replace_ast(Fun, E) || E <- Nodes];
replace_ast(_Fun, Leaf) ->
    Leaf.

compile_mixed_items(Items, Module, Backend, CCtx, Inherited) ->
    Opts = prescan_directives(Items, Inherited),
    State0 = #state{
        module = Module,
        nodiff = maps:is_key(diff, Opts),
        backend = Backend,
        content_ctx = CCtx
    },
    State1 = fold_fragment_items(
        fun(Item, State) -> compile_mixed_item(Item, Module, State) end, State0, Items
    ),
    {Statics, DynASTs} = finalize(State1),
    Fingerprint = generate_fingerprint(Statics),
    {Statics, DynASTs, Fingerprint, Opts}.

%% Compile a fragment's top-level items in order, with the backend's children
%% separator between them as an element's children get one (native joins JSON
%% array members with a comma). Unlike maybe_children_sep/2 an empty separator
%% (HTML, terminal) adds no segment at all: a fragment's fingerprint hashes its
%% static segments, so an extra empty one would re-fingerprint every existing
%% fragment -- every layout among them.
fold_fragment_items(Fun, State0, Items) ->
    {State, _Index} = lists:foldl(
        fun(Item, {State1, Index}) -> {Fun(Item, fragment_sep(State1, Index)), Index + 1} end,
        {State0, 0},
        Items
    ),
    State.

fragment_sep(State, 0) ->
    State;
fragment_sep(#state{backend = Backend} = State, _Index) ->
    case Backend:children_sep() of
        <<>> -> State;
        Sep -> buf_append(State, Sep)
    end.

compile_mixed_item(Item, Module, State) ->
    case static_child_bytes(Item, State) of
        {ok, Bytes} -> buf_append(State, Bytes);
        error -> compile_mixed_non_static(Item, Module, State)
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
    flush(
        State, make_nodiff_dynamic_ast(Item, Module, line(Item), Backend, State#state.content_ctx)
    );
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
    DynAST = make_esc_text_dynamic_ast(
        Az, Item, Module, line(Item), Backend, State0#state.content_ctx
    ),
    State3 = flush(State2, DynAST),
    State3#state{buf = [Backend:text_slot_close()]}.

%% A BINARY tag is taken verbatim, exactly as a binary attribute name is
%% (`extract_attr_name/2`). The atom form is translated `_` -> `-` by the backend's
%% `name/1` so `my_widget` needs no quoting, which leaves a tag whose name really
%% does contain an underscore unwritable -- `{'my-widget_v2', ...}` silently emitted
%% `<my-widget-v2>`. The two forms are the same pair, and mean the same thing, for a
%% tag as for an attribute name.
extract_element({tuple, _, [{bin, _, _} = TagAST, AttrsAST, ChildrenAST]} = Node) ->
    extract_element_parts(extract_binary_value(TagAST), AttrsAST, ChildrenAST, Node);
extract_element({tuple, _, [{bin, _, _} = TagAST, AttrsAST]} = Node) ->
    extract_element_parts(extract_binary_value(TagAST), AttrsAST, {nil, 0}, Node);
extract_element({tuple, _, [{atom, _, Tag}, AttrsAST, ChildrenAST]} = Node) ->
    extract_element_parts(Tag, AttrsAST, ChildrenAST, Node);
extract_element({tuple, _, [{atom, _, Tag}, AttrsAST]} = Node) ->
    extract_element_parts(Tag, AttrsAST, {nil, 0}, Node);
%% Shaped like an element but with a tag that is neither a literal atom nor a
%% literal binary.
extract_element({tuple, _, [TagAST, _Attrs]} = Node) ->
    parse_error(element_tag_reason(TagAST), anno(Node));
extract_element({tuple, _, [TagAST, _Attrs, _Children]} = Node) ->
    parse_error(element_tag_reason(TagAST), anno(Node));
extract_element(Node) ->
    parse_error(invalid_element, anno(Node)).

%% A tag written as a literal of a type that is not a name (a string, a number) is a
%% malformed element form, and the form error says exactly what the accepted shapes
%% are. A tag that is EVALUATED is a different mistake with a different answer -- the
%% author is choosing the element at runtime -- so it gets advice instead.
%% Shared by both tag forms: the attrs must still be a literal list, and a void
%% element's `{Tag, Attrs}` shorthand is the same as an empty child list.
extract_element_parts(Tag, AttrsAST, ChildrenAST, Node) ->
    case is_list_ast(AttrsAST) of
        true ->
            {Tag, ast_list_to_list(AttrsAST), normalize_children(ChildrenAST), anno(Node)};
        false ->
            parse_error(invalid_element, anno(Node))
    end.

element_tag_reason(TagAST) ->
    case is_literal_tag(TagAST) of
        true -> invalid_element;
        false -> computed_element_tag
    end.

is_literal_tag({atom, _, _}) -> true;
is_literal_tag({string, _, _}) -> true;
is_literal_tag({integer, _, _}) -> true;
is_literal_tag({float, _, _}) -> true;
is_literal_tag({char, _, _}) -> true;
is_literal_tag(_Evaluated) -> false.

compile_element(Tag, Attrs0, Children, Line, State0) ->
    Backend = State0#state.backend,
    ok = reject_framework_attrs(Backend, Attrs0, Line),
    RawKind = Backend:raw_text_kind(Tag, State0#state.content_ctx),
    ChildCtx = Backend:content_context(Tag, State0#state.content_ctx),
    Attrs1 = maybe_inject_nodiff(maybe_inject_or_raise_az_view(Attrs0, Line, State0), Line, State0),
    Attrs = maybe_inject_local_descriptor(Backend, Attrs1, Children, RawKind, Line),
    State1 = State0#state{root = false, nodiff_mark = none},
    %% A raw-text element's dynamic children fold into ONE nested template carried at
    %% the ELEMENT's own az (see compile_element_children/7), so it needs one too --
    %% not only dynamic attributes force one there.
    HasDyn =
        has_dynamic_attr(Attrs) orelse has_dynamic_child(Children, RawKind),
    {ElemAz, State2} =
        case HasDyn andalso needs_elem_az(Children, Line, State1) of
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
    State5 = compile_element_attrs(Tag, Attrs, ElemAz, State4, Line),
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
            State7 = compile_element_children(
                Children, ElemAz, RawKind, Tag, ChildCtx, Line, State6
            ),
            State8 = buf_append(State7, Backend:element_close(TagBin)),
            State8#state{
                raw_text_kind = State0#state.raw_text_kind,
                raw_text_tag = State0#state.raw_text_tag,
                content_ctx = State0#state.content_ctx
            }
    end.

%% A nodiff element is never an op target, so it carries no `az` -- unless it holds
%% a content `?local`, whose slot the client finds by the element's `az` plus the
%% slot's comment marker (an attribute `?local` needs neither).
needs_elem_az(_Children, _Line, #state{nodiff = false}) ->
    true;
needs_elem_az(Children, Line, #state{nodiff = true}) ->
    map_size(collect_content_locals(Children, Line)) > 0.

%% A raw-text element (script/style/textarea/title) cannot carry comment markers
%% inside it -- a comment there is literal text, so a `<head>` title would render
%% `<!--az:0-->Home<!--/az-->` in the browser tab. Its dynamic children are therefore
%% markerless, and before this fold the diff had no target for them at all: the value
%% silently never updated, in the same template where a sibling `<p>` patched fine.
%%
%% The address does not have to live INSIDE the element. Fold the whole content --
%% statics and dynamics both -- into one nested template held at the element's own
%% `az`, which raw text permits. Any inner change then re-renders that element's
%% content, shipped as a single `?OP_TEXT` against that az by `make_ops/5`'s
%% markerless escalation. That escalation's granularity is what makes this safe: one
%% raw-text element, not the enclosing template, so a `<textarea>` is untouched when
%% a sibling `<title>` changes.
%%
%% Escaping is preserved structurally rather than reimplemented: the children compile
%% through the ordinary `compile_children/3` walk, so each dynamic keeps its own
%% policy -- `raw` (script/style) goes verbatim through `Backend:raw_text/4` breakout
%% neutralization, `escapable` (textarea/title) is HTML-escaped via `esc_spec`.
%% Nothing here rewrites that path.
compile_element_children(Children, ElemAz, RawKind, Tag, ChildCtx, Line, State0) ->
    ChildState = State0#state{
        raw_text_kind = RawKind, raw_text_tag = Tag, content_ctx = ChildCtx
    },
    Fold =
        RawKind =/= none andalso ElemAz =/= none andalso
            has_dynamic_child(Children, RawKind) andalso (not State0#state.nodiff),
    case Fold of
        false ->
            compile_children(Children, ElemAz, ChildState);
        true ->
            Backend = State0#state.backend,
            Inner0 = #state{
                module = State0#state.module,
                backend = Backend,
                raw_text_kind = RawKind,
                raw_text_tag = Tag,
                content_ctx = ChildCtx
            },
            Inner1 = compile_children(Children, none, Inner0),
            {Statics, DynASTs} = finalize(Inner1),
            Fp = generate_fingerprint(Statics),
            {S1, D1} = scope_az(Backend, Fp, Statics, DynASTs),
            TmplAST = build_template_ast(Line, S1, D1, Fp, #{backend => Backend}),
            %% The folded content is a nested template, so its inner `?get` reads
            %% are isolated from THIS slot's dependency bracket -- without touches
            %% the dep-aware diff skips the slot and the element freezes exactly as
            %% it did before the fold. Same remedy as branch_track_touches/1.
            Keys = dedup_keys(collect_read_keys(Children, [])),
            Body = [track_call_ast(K) || K <- Keys] ++ [TmplAST],
            FunAST = {'fun', 0, {clauses, [{clause, 0, [], [], Body}]}},
            DynAST =
                {tuple, 0, [
                    ast_binary(integer_to_binary(ElemAz)),
                    FunAST,
                    loc_ast(State0#state.module, erl_anno:line(Line))
                ]},
            flush(State0, DynAST)
    end.

%% A static `false` renders whatever the backend's attr_bool/2 says: nothing for a
%% presence backend, a `false` prop for a value backend.
compile_static_false(NameAST, State0, ElemLine) ->
    Backend = State0#state.backend,
    NameBin = extract_attr_name(Backend, NameAST),
    case emit_backend(fun() -> Backend:attr_bool(NameBin, false) end, ElemLine) of
        <<>> ->
            State0;
        Bytes ->
            buf_append(State0, Bytes)
    end.

%% The author's attributes, then the static ones the backend implies from them
%% (`arizona_renderer:implied_attrs/2`).
compile_element_attrs(Tag, Attrs, ElemAz, State0, Line) ->
    Backend = State0#state.backend,
    State1 = compile_attrs(Attrs, ElemAz, State0, Line),
    Names = [N || A <- Attrs, N <- [implied_attr_name(Backend, A)], is_binary(N)],
    case Backend:implied_attrs(Tag, Names) of
        [] ->
            State1;
        Implied ->
            buf_append(State1, <<
                <<(emit_backend(fun() -> Backend:attr(N, V) end, Line))/binary>>
             || {N, V} <:- Implied
            >>)
    end.

%% A static `false` renders nothing, so it is not an attribute the element carries.
implied_attr_name(_Backend, {tuple, _, [_NameAST, {atom, _, false}]}) ->
    undefined;
implied_attr_name(Backend, {tuple, _, [{atom, _, _} = NameAST, _Value]}) ->
    extract_attr_name(Backend, NameAST);
implied_attr_name(Backend, {tuple, _, [{bin, _, _} = NameAST, _Value]}) ->
    extract_attr_name(Backend, NameAST);
implied_attr_name(_Backend, {bin, _, _} = Bin) ->
    extract_binary_value(Bin);
implied_attr_name(Backend, {atom, _, Name}) ->
    Backend:name(Name);
implied_attr_name(_Backend, _Other) ->
    undefined.

compile_attrs([], _ElemAz, State, _ElemLine) ->
    State;
compile_attrs([Attr | Rest], ElemAz, State0, ElemLine) ->
    State1 = compile_attr(Attr, ElemAz, State0, ElemLine),
    compile_attrs(Rest, ElemAz, State1, ElemLine).

compile_attr({bin, _, _} = Bin, _ElemAz, State0, ElemLine) ->
    Backend = State0#state.backend,
    NameBin = record_directive_attr(extract_binary_value(Bin)),
    buf_append(State0, emit_backend(fun() -> Backend:attr_bool(NameBin, true) end, ElemLine));
compile_attr({tuple, _, [NameAST, {atom, _, false}]}, _ElemAz, State0, ElemLine) when
    element(1, NameAST) =:= atom; element(1, NameAST) =:= bin
->
    compile_static_false(NameAST, State0, ElemLine);
compile_attr({tuple, _, [NameAST, {atom, _, true}]}, _ElemAz, State0, ElemLine) when
    element(1, NameAST) =:= atom; element(1, NameAST) =:= bin
->
    Backend = State0#state.backend,
    NameBin = record_directive_attr(extract_attr_name(Backend, NameAST)),
    buf_append(State0, emit_backend(fun() -> Backend:attr_bool(NameBin, true) end, ElemLine));
compile_attr({tuple, _, [NameAST, ValueAST]}, ElemAz, State0, ElemLine) when
    element(1, NameAST) =:= atom; element(1, NameAST) =:= bin
->
    Backend = State0#state.backend,
    NameBin = extract_attr_name(Backend, NameAST),
    case fold_literal_text(ValueAST) of
        {ok, ValBin} ->
            %% A literal value (a binary, or a list of literals, folded by
            %% literal_text/1) is DATA by construction: an effect is always a call,
            %% never a literal. Deliberately not recorded -- recording it would
            %% delegate the event type and then hand the app's own data to the command
            %% interpreter (`{az_select, ~"[1,2,3]"}` parses as opcode 1 with selector
            %% 2 and throws out of a document listener on every dispatch).
            %%
            %% `az-prevent-default` is the exception, because it is a DIRECTIVE whose
            %% value the client ignores (it tests `hasAttribute`). Written with a value
            %% it is still a declaration, and leaving it out of the set makes the
            %% client register wheel/touch listeners passive, silently disarming the
            %% `preventDefault` the attribute asked for.
            _ = record_directive_attr(NameBin),
            buf_append(State0, emit_backend(fun() -> Backend:attr(NameBin, ValBin) end, ElemLine));
        error ->
            %% A dynamic value is recorded only on EVIDENCE that it carries
            %% commands: a builder call somewhere in the expression. An opaque
            %% dynamic (`{az_select, ?get(ids)}`) is indistinguishable from app
            %% data here, and recording it would delegate the event type and hand
            %% that data to the command interpreter -- so it is settled at render
            %% time instead, where the evaluated value's type is proof
            %% (arizona_event_attrs observes the `{arizona_effect, _}`
            %% classification and the socket ships the name on that frame).
            _ =
                case has_builder_call(ValueAST) of
                    true -> record_az_attr(NameBin);
                    false -> record_directive_attr(NameBin)
                end,
            compile_dynamic_attr(Backend, NameBin, ValueAST, ElemAz, State0)
    end;
compile_attr({atom, _, Name}, _ElemAz, State0, ElemLine) ->
    Backend = State0#state.backend,
    NameBin = record_directive_attr(Backend:name(Name)),
    buf_append(State0, emit_backend(fun() -> Backend:attr_bool(NameBin, true) end, ElemLine));
compile_attr(Attr, _ElemAz, _State0, ElemLine) ->
    AttrAnno =
        try
            anno(Attr)
        catch
            _:_ -> ElemLine
        end,
    parse_error(invalid_attribute, AttrAnno).

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
            parse_error({render_reject, Message}, Line);
        %% A folded command the encoder refuses (arizona_effect): reported at the
        %% attribute's line with the encoder's message, rather than as a crash of
        %% the transform itself.
        error:{flash_outside_live_effect, _Op} = Reason ->
            #{general := Message} = arizona_effect:format_error(Reason, []),
            parse_error({render_reject, Message}, Line)
    end.

%% Emit a dynamic attribute value: a folded arizona_js command becomes a static,
%% otherwise the backend bakes the name and the value flushes as a dynamic.
compile_dynamic_attr(Backend, NameBin, ValueAST, ElemAz, State0) ->
    case attr_local_spec(ValueAST, anno(ValueAST)) of
        {whole, _LocalCall} ->
            compile_local_attr(Backend, NameBin, ValueAST, ElemAz, State0);
        {interp, LocalCall, Prefix, Suffix} ->
            compile_interp_local_attr(
                Backend, NameBin, LocalCall, Prefix, Suffix, ElemAz, State0
            );
        none ->
            compile_dynamic_attr_value(Backend, NameBin, ValueAST, ElemAz, State0)
    end.

compile_dynamic_attr_value(Backend, NameBin, ValueAST0, ElemAz, State0) ->
    ValueAST = raw_interp_attr_ast(Backend, ValueAST0),
    ValAnno = anno(ValueAST),
    case try_fold_arizona_js(ValueAST) of
        {ok, Cmd} ->
            buf_append(
                State0, emit_backend(fun() -> Backend:attr_command(NameBin, Cmd) end, ValAnno)
            );
        error when State0#state.nodiff ->
            Module = State0#state.module,
            State1 = buf_dyn_attr(State0, Backend, NameBin, ValAnno),
            DynAST = make_nodiff_attr_dynamic_ast(
                NameBin, typed_attr_ast(Backend, ValueAST), Module, line(ValueAST)
            ),
            flush(State1, DynAST);
        error ->
            Module = State0#state.module,
            State1 = buf_dyn_attr(State0, Backend, NameBin, ValAnno),
            AzBin = integer_to_binary(ElemAz),
            DynAST = make_attr_dynamic_ast(
                AzBin, NameBin, typed_attr_ast(Backend, ValueAST), Module, line(ValueAST)
            ),
            flush(State1, DynAST)
    end.

%% An interpolated value holding a literal `?raw` part (`[~"/a?", ?raw(Q)]`) is
%% attribute markup as a whole, so it becomes one raw value with every other part
%% escaped: `arizona_template:raw_attr/2` assembles it. Left a plain list, the raw
%% tuple inside it is not text and the value cannot render. Only a literal `?raw`
%% counts, the rule the opt-out follows everywhere else.
raw_interp_attr_ast(Backend, ValueAST) ->
    Parts =
        case is_list_ast(ValueAST) of
            true -> attr_value_parts(ValueAST);
            false -> []
        end,
    case lists:any(fun is_raw_call/1, Parts) of
        true ->
            L = line(ValueAST),
            PartsAST = lists:foldr(fun(P, Acc) -> {cons, L, P, Acc} end, {nil, L}, Parts),
            {call, L, {remote, L, {atom, L, arizona_template}, {atom, L, raw_attr}}, [
                {atom, L, Backend}, PartsAST
            ]};
        false ->
            ValueAST
    end.

%% A backend whose booleans are typed prop values (`boolean_attrs/0`) gets the value
%% wrapped, so a dynamic `false` stays a value instead of removing the attribute.
typed_attr_ast(Backend, ValueAST) ->
    case Backend:boolean_attrs() of
        value ->
            L = line(ValueAST),
            {call, L, {remote, L, {atom, L, arizona_template}, {atom, L, bool_attr}}, [ValueAST]};
        presence ->
            ValueAST
    end.

%% Emit a client-owned attribute slot (`?local` in attribute-value position): a
%% dynamic whose evaluated value is the local-map (with target = {attr, Name}).
%% It is never the normal `{attr, Name, Fun}` shape -- that would store
%% `{attr, Name, Map}` and the per-dynamic `az_local` skip in arizona_diff would
%% not match it.
compile_local_attr(Backend, NameBin, ValueAST, ElemAz, State0) ->
    Module = State0#state.module,
    State1 = buf_dyn_attr(State0, Backend, NameBin, anno(ValueAST)),
    Expr = local_attr_expr_ast(NameBin, ValueAST),
    DynAST = make_text_dynamic_ast(local_attr_az(ElemAz), Expr, Module, line(ValueAST)),
    flush(State1, DynAST).

%% A nodiff element with only attribute locals has no `az` (needs_elem_az/3): the
%% client writes the attribute by name, so the slot is a render-once dynamic.
local_attr_az(none) -> undefined;
local_attr_az(ElemAz) -> integer_to_binary(ElemAz).

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
    State1 = buf_dyn_attr(State0, Backend, NameBin, anno(LocalCall)),
    Expr = local_attr_interp_expr_ast(NameBin, LocalCall, Prefix, Suffix),
    DynAST = make_text_dynamic_ast(local_attr_az(ElemAz), Expr, Module, line(LocalCall)),
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
                    Elems = attr_value_parts(ValueAST),
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
maybe_inject_local_descriptor(Backend, Attrs, Children, RawKind, Line) ->
    AttrLocals = collect_attr_locals(Backend, Attrs, Line),
    AttrAffixes = collect_attr_affixes(Backend, Attrs, Line),
    ContentLocals = collect_content_locals(Children, Line),
    case map_size(AttrLocals) > 0 orelse map_size(ContentLocals) > 0 of
        false ->
            Attrs;
        true ->
            assert_content_local_not_raw_text(ContentLocals, RawKind, Line),
            assert_local_supported(Backend, Line),
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

assert_local_supported(Backend, Line) ->
    case Backend:supports_local() of
        true -> ok;
        false -> parse_error(local_unsupported, Line)
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
%% neither static text nor a nested element -- mirrors compile_child/4. A folded
%% literal list is static outside raw text, the only place a content local exists
%% (assert_content_local_not_raw_text/3).
is_dynamic_text_child(Child) ->
    not is_static_binary(Child) andalso not is_element_tuple(Child) andalso
        literal_list_text(Child) =:= error.

compile_children(Children, ElemAz, State) ->
    compile_children(Children, ElemAz, State, 0, 0).

compile_children([], _ElemAz, State, _Slot, _Index) ->
    State;
compile_children([Child | Rest], ElemAz, State0, Slot, Index) ->
    State1 = maybe_children_sep(State0, Index),
    State2 = State1#state{raw_text_after = static_run_after(Rest, State1)},
    {State3, NextSlot} = compile_child(Child, ElemAz, State2, Slot),
    compile_children(Rest, ElemAz, State3, NextSlot, Index + 1).

%% The bytes the static children after a raw-text slot emit, up to the next child
%% that is not static text. Only a `raw` element's slot reads them.
static_run_after(Rest, #state{raw_text_kind = raw, backend = Backend}) ->
    After = iolist_to_binary(static_run(Rest, Backend)),
    binary_part(After, 0, min(?RAW_TEXT_NEIGHBOUR_BYTES, byte_size(After)));
static_run_after(_Rest, _State) ->
    <<>>.

static_run([Child | Rest], Backend) ->
    case is_static_binary(Child) of
        true ->
            [
                Backend:children_sep(),
                Backend:text_child(extract_binary_value(Child))
                | static_run(Rest, Backend)
            ];
        false ->
            []
    end;
static_run([], _Backend) ->
    [].

%% The static bytes already buffered right before a slot (newest segment first),
%% back to the previous dynamic or a framework marker.
static_run_before(Buf) ->
    Before = iolist_to_binary(lists:reverse(static_segments(Buf))),
    binary_part(Before, byte_size(Before), -min(?RAW_TEXT_NEIGHBOUR_BYTES, byte_size(Before))).

static_segments([Bin | Rest]) when is_binary(Bin) ->
    [Bin | static_segments(Rest)];
static_segments(_Rest) ->
    [].

%% Emit a separator before every child after the first. HTML uses an empty
%% separator (no-op); native emits a comma between JSON array elements.
maybe_children_sep(State, 0) ->
    State;
maybe_children_sep(State, _Index) ->
    buf_append(State, (State#state.backend):children_sep()).

compile_child(Child, ElemAz, State0, Slot) ->
    case static_child_bytes(Child, State0) of
        {ok, Bytes} ->
            {buf_append(State0, Bytes), Slot};
        error ->
            compile_non_static_child(Child, ElemAz, State0, Slot)
    end.

%% A child that compiles to static bytes. A binary literal is static text, spliced
%% verbatim (text_child/1, the raw-HTML seam). A list of literals is a VALUE -- at
%% runtime it is a text slot rendered through `Backend:escape/1` -- so its folded
%% text goes through that same callback, and the output only loses the slot markers.
%% Not under raw text: there a dynamic slot keeps its own policy (a `<script>` value
%% must be a literal `?raw`, a `<title>` one folds into the element's template).
static_child_bytes(Child, #state{backend = Backend, raw_text_kind = RawKind}) ->
    case is_static_binary(Child) of
        true ->
            {ok, Backend:text_child(extract_binary_value(Child))};
        false when RawKind =:= none ->
            case fold_literal_list_text(Child) of
                {ok, Bin} -> {ok, Backend:escape(Bin)};
                error -> error
            end;
        false ->
            error
    end.

compile_non_static_child(Child, ElemAz, State0, Slot) ->
    case is_element_tuple(Child) of
        true ->
            {Tag, Attrs, Children, ElemLine} = extract_element(Child),
            %% A literal element in title/textarea content shows as literal text.
            case State0#state.raw_text_kind of
                escapable -> parse_error(markup_in_raw_text, ElemLine);
                _ -> ok
            end,
            {compile_element(Tag, Attrs, Children, ElemLine, State0), Slot};
        false ->
            compile_dynamic_child(Child, ElemAz, State0, Slot)
    end.

compile_dynamic_child(Child, ElemAz, State0, Slot) ->
    case is_invalid_static_child(Child) of
        true ->
            ValueStr = erl_pp:expr(Child),
            parse_error({invalid_child, ValueStr}, anno(Child));
        false ->
            ok = reject_markup_in_raw_text(Child, State0#state.raw_text_kind),
            emit_child_dynamic(Child, ElemAz, State0, Slot)
    end.

%% Escapable raw text (title/textarea) decodes character references but parses no
%% tags, so markup there -- in the value itself or in any tail it can take -- would
%% show as literal text (comment markers included). A script/style slot is
%% already refused unless it is a literal ?raw (assert_raw_text_opt_out/1).
reject_markup_in_raw_text(Child, escapable) ->
    _Walked = map_tail_exprs(
        Child,
        fun(Leaf) ->
            case is_markup_leaf(Leaf) of
                true -> parse_error(markup_in_raw_text, anno(Leaf));
                false -> Leaf
            end
        end,
        fun(NonTail) -> NonTail end
    ),
    ok;
reject_markup_in_raw_text(_Child, _RawKind) ->
    ok.

%% A tail value that renders as markup: an element, an element list or a list holding
%% one, or a constructor other than ?raw/?local (both are values here).
is_markup_leaf({map, _, Fields} = Leaf) ->
    %% A compiled template with no dynamics and no tag in its statics renders as
    %% plain text (`?html(~"x")`), so it is a value here.
    case is_compiled_template_map(Fields) andalso is_text_only_template(Fields) of
        true -> false;
        false -> is_markup_item(Leaf)
    end;
is_markup_leaf(Leaf) ->
    case classify_body(Leaf) of
        element_tuple -> true;
        element_list -> true;
        list_ast -> list_has_markup_value(Leaf);
        _ -> is_markup_item(Leaf) andalso not is_value_constructor(Leaf)
    end.

%% In title/textarea a ?raw list item is a value, as a bare ?raw is.
list_has_markup_value({cons, _, Head, Tail}) ->
    (is_markup_item(Head) andalso not is_value_constructor(Head)) orelse
        list_has_markup_value(Tail);
list_has_markup_value(_) ->
    false.

is_text_only_template(Fields) ->
    case {template_map_field(d, Fields), template_map_field(s, Fields)} of
        {{nil, _}, SAST} ->
            Statics = [extract_binary_value(B) || B <- ast_list_to_list(SAST)],
            not lists:any(fun(B) -> binary:match(B, ~"<") =/= nomatch end, Statics);
        _ ->
            false
    end.

is_value_constructor({call, _, {remote, _, {atom, _, Mod}, {atom, _, F}}, _}) ->
    (Mod =:= arizona_template orelse Mod =:= az) andalso (F =:= raw orelse F =:= local);
is_value_constructor(_Expr) ->
    false.

%% The `raw` raw-text clause comes FIRST, ahead of nodiff: both make the slot
%% markerless and render-once (`undefined` az), so they agree on the diff
%% question, but only the raw-text path applies the escaping policy the *element*
%% demands. Matching nodiff first let an `az-nodiff` element -- a layout, which is
%% exactly where an inline `<script>` lives -- skip the opt-out guard and
%% HTML-escape inside raw text, so a marked value was spliced with no
%% neutralization and an unmarked one drew no compile error. The `escapable`
%% clause stays below nodiff because the two agree there: both HTML-escape the
%% scalar through make_nodiff_dynamic_ast/5.
emit_child_dynamic(
    Child,
    _ElemAz,
    #state{
        raw_text_kind = raw,
        raw_text_tag = Tag,
        raw_text_after = After,
        buf = Buf,
        module = Module,
        backend = Backend
    } = State0,
    Slot
) ->
    %% script/style: raw text, the browser decodes neither character references
    %% nor HTML comments here, so the value is emitted verbatim, markerless and
    %% render-once. Comment markers would become literal bytes and corrupt the
    %% script/CSS (and a module script's HTML-comment tokens are a SyntaxError).
    %% Diffing is impossible by construction (no marker to patch), so the slot
    %% renders once -- the diff engine skips its `undefined` az.
    ok = assert_raw_text_opt_out(Child),
    Neighbours = {static_run_before(Buf), After},
    DynAST = make_raw_text_dynamic_ast(
        Child, Tag, Neighbours, Module, line(Child), Backend, State0#state.content_ctx
    ),
    {flush(State0, DynAST), Slot};
emit_child_dynamic(
    Child, ElemAz, #state{nodiff = true, module = Module, backend = Backend} = State0, Slot
) ->
    %% Only a content `?local` keeps its marker. Every dynamic child still counts a
    %% slot, the numbering collect_content_locals/2 gives the client.
    case is_local_marker(Child) of
        true ->
            emit_marked_child(Child, ElemAz, State0, Slot);
        false ->
            DynAST = make_nodiff_dynamic_ast(
                Child, Module, line(Child), Backend, State0#state.content_ctx
            ),
            {flush(State0, DynAST), Slot + 1}
    end;
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
    DynAST = make_nodiff_dynamic_ast(
        Child, Module, line(Child), Backend, State0#state.content_ctx
    ),
    {flush(State0, DynAST), Slot};
emit_child_dynamic(Child, ElemAz, State0, Slot) ->
    emit_marked_child(Child, ElemAz, State0, Slot).

emit_marked_child(Child, ElemAz, #state{module = Module, backend = Backend} = State0, Slot) ->
    ElemAzBin = integer_to_binary(ElemAz),
    MarkerAz = Backend:text_az(ElemAzBin, Slot),
    State1 = buf_az_slot(State0, MarkerAz),
    DynAST = make_esc_text_dynamic_ast(
        MarkerAz, Child, Module, line(Child), Backend, State0#state.content_ctx
    ),
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
        false -> parse_error(dynamic_in_raw_text, anno(Child))
    end.

is_raw_call({call, _, {remote, _, {atom, _, Mod}, {atom, _, raw}}, [_Value]}) ->
    Mod =:= arizona_template orelse Mod =:= az;
is_raw_call(_Expr) ->
    false.

make_text_dynamic_ast(Az, ExprAST, Module, ExprLine) ->
    LocAST = loc_ast(Module, ExprLine),
    AzAST =
        case Az of
            undefined -> {atom, 0, undefined};
            AzBin -> ast_binary(AzBin)
        end,
    {tuple, 0, [
        AzAST,
        {'fun', 0, {clauses, [{clause, 0, [], [], [ExprAST]}]}},
        LocAST
    ]}.

%% Like make_text_dynamic_ast/4 but tags a *value* element-content interpolation
%% as `{esc, Fun}` so the renderer HTML-escapes it. A *block* -- a nested
%% template, ?each, ?inner_content, ?stateful/?stateless, a map, or raw/1 -- is
%% left untagged and rendered structurally (escaping is decided at compile time
%% so the runtime can never confuse user scalars with spliced framework HTML).
make_esc_text_dynamic_ast(AzBin, ExprAST0, Module, ExprLine, Backend, CCtx) ->
    ExprAST = stamp_branches(expand_block_element_tails(ExprAST0, Module, Backend, false, CCtx)),
    LocAST = loc_ast(Module, ExprLine),
    Body = branch_track_touches(ExprAST0) ++ [ExprAST],
    FunAST = {'fun', 0, {clauses, [{clause, 0, [], [], Body}]}},
    {tuple, 0, [ast_binary(AzBin), esc_spec(ExprAST, FunAST), LocAST]}.

%% Number the tails of a content-slot control-flow expression and stamp each with
%% its number, so an az-nodiff element names the branch that produced it (see
%% arizona_template:branch/2): two branches can produce the same template, and
%% once an az-nodiff element is no longer evaluated the branch is all that tells them apart.
%% A literal template takes the stamp as a field; a value only known at runtime
%% goes through branch/2; a value that can never be an az-nodiff element is left alone.
stamp_branches(Expr) ->
    case is_control_flow_ast(Expr) of
        true ->
            {Stamped, _Count} = mapfold_tail_exprs(Expr, fun stamp_branch/2, fun(E) -> E end, 1),
            Stamped;
        false ->
            Expr
    end.

stamp_branch(Leaf, N) ->
    {stamp_leaf(N, Leaf), N + 1}.

stamp_leaf(N, {map, L, Fields} = Map) ->
    case lists:any(fun is_nodiff_field/1, Fields) of
        true ->
            {map, L,
                stamp_nodiff_branch(N, Fields) ++
                    [{map_field_assoc, L, {atom, L, branch}, {integer, L, N}}]};
        false ->
            Map
    end;
stamp_leaf(_N, {Literal, _, _} = Leaf) when
    Literal =:= bin orelse Literal =:= string orelse Literal =:= integer orelse
        Literal =:= float orelse Literal =:= atom orelse Literal =:= char
->
    Leaf;
stamp_leaf(_N, {nil, _} = Leaf) ->
    Leaf;
stamp_leaf(N, {call, L, {remote, _, {atom, _, Mod}, {atom, _, F}}, _} = Leaf) when
    Mod =:= arizona_template orelse Mod =:= az
->
    case lists:member(F, [each, stateful, local, raw, inner_content]) of
        true -> Leaf;
        false -> branch_call(N, L, Leaf)
    end;
%% A BIF never returns a template, and the stamp would cost a call per evaluation.
stamp_leaf(N, {call, _, {atom, _, F}, Args} = Leaf) ->
    case erl_internal:bif(F, length(Args)) of
        true -> Leaf;
        false -> branch_call(N, line(Leaf), Leaf)
    end;
stamp_leaf(N, Leaf) ->
    branch_call(N, line(Leaf), Leaf).

is_nodiff_field({map_field_assoc, _, {atom, _, diff}, {atom, _, false}}) -> true;
is_nodiff_field(_Field) -> false.

branch_call(N, L, Leaf) ->
    {call, L, {remote, L, {atom, L, arizona_template}, {atom, L, branch}}, [{integer, L, N}, Leaf]}.

%% A content-slot control-flow expression (`case`/`if`/`maybe`/...) compiles each
%% branch *element* into a nested template (expand_block_element_tails/3) whose `?get`
%% reads are isolated from the conditional dynamic's own dependency bracket -- see
%% eval_template/2's with_saved_deps. Left alone, the conditional slot records only
%% its scrutinee's reads, so a change to a binding read solely in such a branch is
%% skipped and the branch freezes. (A *value* branch does not freeze: its read fires
%% eagerly in the slot closure when that branch is taken, and a non-taken value
%% branch's read is genuinely not a dependency.) Mirror the guard auto-tracking
%% (wrap_touches/4): prepend a `track/1` for each binding key read in a branch
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
            case is_tracked_template_leaf(Expr) of
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
    case is_tracked_template_leaf(Leaf) of
        true -> collect_read_keys(Leaf, Acc);
        false -> Acc
    end.

%% An az-nodiff leaf renders once, so a read inside it can never patch anything:
%% tracking it would only re-run the slot for a change it has to ignore.
is_tracked_template_leaf(Leaf) ->
    is_nested_template_leaf(Leaf) andalso not template_is_nodiff(Leaf).

is_nested_template_leaf(Leaf) ->
    case classify_body(Leaf) of
        element_tuple -> true;
        element_list -> true;
        list_ast -> list_has_markup_item(Leaf);
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
expand_block_element_tails(Expr, Module, Backend, Nodiff, CCtx) ->
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
                fun(Leaf) -> expand_element_leaf(Leaf, Module, Backend, Nodiff, CCtx) end,
                fun(NonTail) -> NonTail end
            )
    end.

%% At a tail leaf, compile a bare element tuple / element list (or a mixed list
%% that contains markup, see is_markup_item/1) into a nested template, as a literal ?html
%% there would; leave plain values (and pure value lists) untouched. A ?local
%% leaf is rejected: a conditional-branch ?local emits no az-local descriptor
%% (see reject_orphaned_local/1).
expand_element_leaf(Expr, Module, Backend, Nodiff, CCtx) ->
    ok = reject_orphaned_local(Expr),
    case classify_body(Expr) of
        Class when Class =:= element_tuple; Class =:= element_list ->
            compile_template(Expr, line(Expr), Module, false, Backend, Nodiff, CCtx);
        list_ast ->
            case list_has_markup_item(Expr) of
                true -> compile_template(Expr, line(Expr), Module, false, Backend, Nodiff, CCtx);
                false -> Expr
            end;
        _ ->
            Expr
    end.

list_has_markup_item({cons, _, Head, Tail}) ->
    is_markup_item(Head) orelse list_has_markup_item(Tail);
list_has_markup_item(_) ->
    false.

has_markup_tail(Flow) ->
    try
        _ = map_tail_exprs(
            Flow,
            fun(Leaf) ->
                case is_markup_item(Leaf) orelse list_has_markup_item(Leaf) of
                    true -> throw(markup_tail);
                    false -> Leaf
                end
            end,
            fun(NonTail) -> NonTail end
        ),
        false
    catch
        throw:markup_tail -> true
    end.

%% A list item that is a block rather than a value: an element, a compiled template
%% map, or any constructor call is_block_content_expr/1 recognises (raw or already
%% compiled -- a helper body or a hoisted variable splices the raw form). A hoisted
%% template arrives wrapped in its track block (prepare_inline_rhs/2), so the block's
%% last expression decides. Left in a plain value list, a block reaches to_bin/1 and
%% crashes; in a fragment each gets its own slot (and a ?local the usual
%% local_orphaned error).
is_markup_item({map, _, Fields}) ->
    is_compiled_template_map(Fields);
is_markup_item({block, _, [_ | _] = Exprs}) ->
    is_markup_item(lists:last(Exprs));
is_markup_item(Flow) when
    element(1, Flow) =:= 'case';
    element(1, Flow) =:= 'if';
    element(1, Flow) =:= 'receive';
    element(1, Flow) =:= 'try';
    element(1, Flow) =:= 'maybe'
->
    %% A conditional item whose tail can be markup is a block: its slot compiles
    %% each element tail into a nested template, as a conditional child does.
    has_markup_tail(Flow);
is_markup_item({call, _, {remote, _, {atom, _, _}, {atom, _, _}}, _} = Call) ->
    is_block_content_expr(Call);
is_markup_item(Item) ->
    is_element_tuple(Item).

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

%% Same value/block classification as make_esc_text_dynamic_ast/6, but for the
%% nodiff (layout) path: a value interpolation (e.g. a layout `title`) is tagged
%% `{esc, Fun}` so SSR HTML-escapes it; a block (?inner_content, nested template)
%% is left untagged and spliced raw.
make_nodiff_dynamic_ast(ExprAST0, Module, ExprLine, Backend, CCtx) ->
    ExprAST = expand_block_element_tails(ExprAST0, Module, Backend, true, CCtx),
    LocAST = loc_ast(Module, ExprLine),
    FunAST = {'fun', 0, {clauses, [{clause, 0, [], [], [ExprAST]}]}},
    {tuple, 0, [{atom, 0, undefined}, esc_spec(ExprAST, FunAST), LocAST]}.

%% Markerless render-once for a `raw` raw-text element (script/style). Unlike
%% make_nodiff_dynamic_ast/5 the value is left bare (never `{esc, Fun}`): the
%% browser does not decode character references inside these, so HTML-escaping a
%% scalar would corrupt it (`&` -> `&amp;`). `undefined` az makes it non-diffable
%% -- there is no comment marker to patch.
make_raw_text_dynamic_ast(ExprAST0, Tag, {Before, After}, Module, ExprLine, Backend, CCtx) ->
    ExprAST = expand_block_element_tails(ExprAST0, Module, Backend, false, CCtx),
    LocAST = loc_ast(Module, ExprLine),
    %% Neutralize a tokenizer breakout (`</script>`, `<!--`) in the value: the
    %% content is emitted verbatim, so the backend that owns raw-text elements
    %% sanitizes it. The tag goes along because the sequences that break out are
    %% the enclosing element's, not raw text's in general; the neighbouring static
    %% text because a sequence can begin in it and end in the value, or the reverse.
    GuardedAST =
        {call, 0, {remote, 0, {atom, 0, Backend}, {atom, 0, raw_text}}, [
            {atom, 0, Tag}, ast_binary(Before), ast_binary(After), ExprAST
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

buf_dyn_attr(State, Backend, NameBin, Anno) ->
    Bin = emit_backend(fun() -> Backend:attr_dyn_name(NameBin) end, Anno),
    State#state{buf = [{dyn_attr, NameBin, Bin} | State#state.buf]}.

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
%% plus its id. `Prefix` is `<Fp>-`, scoping the emitted statics to their
%% template.
-spec render_static(module(), binary(), [segment()]) -> static().
render_static(Backend, Prefix, Segments) ->
    iolist_to_binary([render_segment(Backend, Prefix, Seg) || Seg <- Segments]).

render_segment(_Backend, _Prefix, Bin) when is_binary(Bin) ->
    Bin;
render_segment(Backend, Prefix, {az_attr, Az}) ->
    Backend:az_attr(<<Prefix/binary, Az/binary>>);
render_segment(Backend, Prefix, {az_slot, Az}) ->
    Backend:text_slot_open(<<Prefix/binary, Az/binary>>);
render_segment(_Backend, _Prefix, {dyn_attr, _Name, Bin}) ->
    Bin.

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
%% interleaved with `{az_attr, _}` / `{az_slot, _}` markers and `{dyn_attr, _, _}`
%% dynamic attribute names), not their rendered bytes. Segments distinguish a
%% framework marker from literal bytes that merely look like one, so two
%% templates differing only in that respect -- exactly the pair the scoping bug
%% used to conflate -- get different fingerprints; hashing
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
%% identifier and a tracked read in a scope without it does not compile (an
%% unbound variable) -- so it shadows the caller's `Bindings` and
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
    TmplAST =
        {map, Line,
            BaseFields ++ opts_to_map_fields(Opts, Line) ++
                reuse_field(Line, Vars1, Prefix1, DynamicsAST1)},
    {call, Line, {remote, Line, {atom, Line, arizona_template}, {atom, Line, each}}, [
        SourceAST, TmplAST
    ]}.

%% When a stream item changes, the diff may keep a per-item dynamic's old render
%% only if its deps prove the changed fields were not read. Deps record a read
%% only when it is a tracked `get` running in the slot's own closure: a plain
%% `maps:get(n, Item)`, a head-destructured field, a value bound in the callback
%% body, or a read inside a nested template's closure never reaches them, and
%% reusing on such deps ships no op for a change that happened. The template
%% carries `reuse => [boolean()]`, one flag per dynamic: `true` only when the
%% dynamic references no variable but the item key, outside the map argument of a
%% tracked read in its own closure. Omitted when no dynamic qualifies, and for a
%% 1-arg callback, whose list source re-renders every item anyway.
%%
%% The map argument is masked only when it is the item itself (the bare parameter,
%% or the variable side of a `Pat = Var` head) or bound outside the callback (the
%% view bindings). A sub-map bound in the head or the body records ITS keys, while
%% the item changes in the field holding it, so that read vouches for nothing.
reuse_field(Line, [ItemPat, KeyPat], Prefix, DynamicsAST) ->
    KeyVars = ordsets:from_list(collect_var_names(KeyPat)),
    HeadVars = collect_var_names(ItemPat) ++ collect_var_names(KeyPat),
    Local = (HeadVars ++ prefix_bound(Prefix, HeadVars)) -- item_vars(ItemPat),
    Flags = [reusable_dynamic(D, KeyVars, Local) || D <- ast_list_to_list(DynamicsAST)],
    case lists:member(true, Flags) of
        true ->
            [
                {map_field_assoc, Line, {atom, Line, reuse},
                    ast_list([{atom, Line, F} || F <- Flags])}
            ];
        false ->
            []
    end;
reuse_field(_Line, _Vars, _Prefix, _DynamicsAST) ->
    [].

%% The variables a head pattern binds to the whole item.
item_vars({var, _, Name}) -> [Name];
item_vars({match, _, Left, Right}) -> item_vars(Left) ++ item_vars(Right);
item_vars(_Pattern) -> [].

prefix_bound([], _HeadVars) ->
    [];
prefix_bound(Prefix, HeadVars) ->
    Block = erl_syntax:block_expr(Prefix),
    Tree = erl_syntax_lib:annotate_bindings(Block, ordsets:from_list(HeadVars)),
    proplists:get_value(bound, erl_syntax:get_ann(Tree)).

reusable_dynamic({tuple, L, [Az, Spec | Rest]}, KeyVars, Local) ->
    Masked = {tuple, L, [Az, mask_slot_spec(Spec, Local) | Rest]},
    Tree = erl_syntax_lib:annotate_bindings(Masked, ordsets:new()),
    Free = proplists:get_value(free, erl_syntax:get_ann(Tree)),
    ordsets:is_subset(Free, KeyVars);
reusable_dynamic(_Dynamic, _KeyVars, _Local) ->
    false.

%% The slot closure is the bare fun or the one an `{esc, F}` / `{attr, Name, F}`
%% spec wraps; only there does a tracked read land in the dynamic's own deps.
mask_slot_spec({tuple, L, [{atom, _, esc} = Tag, Fun]}, Local) ->
    {tuple, L, [Tag, mask_slot_fun(Fun, Local)]};
mask_slot_spec({tuple, L, [{atom, _, attr} = Tag, Name, Fun]}, Local) ->
    {tuple, L, [Tag, Name, mask_slot_fun(Fun, Local)]};
mask_slot_spec(Spec, Local) ->
    mask_slot_fun(Spec, Local).

mask_slot_fun({'fun', L, {clauses, [{clause, CL, [], [], Body}]}}, Local) ->
    {'fun', L, {clauses, [{clause, CL, [], [], mask_tracked_reads(Body, Local)}]}};
mask_slot_fun(Spec, _Local) ->
    Spec.

%% Replace the map argument of each tracked read of the item or the view bindings
%% (a variable not in `Local`) with an atom, so it no longer counts as a reference.
%% A nested fun is left alone: its body runs later, in a dependency scope of its
%% own (a nested template's closure), or never.
mask_tracked_reads({'fun', _, _} = Fun, _Local) ->
    Fun;
mask_tracked_reads({named_fun, _, _, _} = Fun, _Local) ->
    Fun;
mask_tracked_reads(
    {call, L, {remote, _, {atom, _, Mod}, {atom, _, F}} = Callee, [Key, {var, VL, V} | Rest]} =
        Call,
    Local
) when Mod =:= arizona_template; Mod =:= az ->
    case is_tracked_read(F, Rest) andalso not lists:member(V, Local) of
        true ->
            Args = [
                mask_tracked_reads(Key, Local),
                {atom, VL, tracked}
                | mask_tracked_reads(Rest, Local)
            ],
            {call, L, Callee, Args};
        false ->
            list_to_tuple(mask_tracked_reads(tuple_to_list(Call), Local))
    end;
mask_tracked_reads(Node, Local) when is_tuple(Node) ->
    list_to_tuple(mask_tracked_reads(tuple_to_list(Node), Local));
mask_tracked_reads(Nodes, Local) when is_list(Nodes) ->
    [mask_tracked_reads(N, Local) || N <- Nodes];
mask_tracked_reads(Leaf, _Local) ->
    Leaf.

%% `get/2,3`, `get_lazy/3` and `with/2` track the key they read (`with/2` every
%% key it projects to), given the arguments after the key and the map.
is_tracked_read(get, []) -> true;
is_tracked_read(get, [_Default]) -> true;
is_tracked_read(get_lazy, [_DefaultFun]) -> true;
is_tracked_read(with, []) -> true;
is_tracked_read(_F, _Rest) -> false.

%% A binary literal AST holding exactly `Bin`'s bytes. Generated literals are read
%% back as template text (an injected `az-local` value folds through compile_attr/4,
%% which takes a string element's characters as code points), so non-ASCII UTF-8 is
%% spelled as its characters with the `utf8` type: spelled byte by byte, each byte
%% would be read back as a code point and encoded a second time. Bytes that are not
%% UTF-8 keep the byte spelling, which no text reading applies to.
ast_binary(Bin) ->
    case unicode:characters_to_list(Bin) of
        Chars when is_list(Chars) ->
            Type =
                case lists:all(fun(C) -> C < 128 end, Chars) of
                    true -> default;
                    false -> [utf8]
                end,
            {bin, 0, [{bin_element, 0, {string, 0, Chars}, default, Type}]};
        _ ->
            {bin, 0, [{bin_element, 0, {string, 0, binary_to_list(Bin)}, default, default}]}
    end.

ast_list([]) ->
    {nil, 0};
ast_list([H | T]) ->
    {cons, 0, H, ast_list(T)}.

%% A template list (children, attrs, a body) is laid out at compile time, one slot
%% per item, so its length must be known: a variable or other non-list tail has no
%% items to lay out.
ast_list_to_list({nil, _}) -> [];
ast_list_to_list({cons, _, Head, Tail}) -> [Head | ast_list_to_list(Tail)];
ast_list_to_list(Tail) -> parse_error(improper_template_list, line(Tail)).

%% An attribute value list is text, where an improper tail is one more part of the
%% chardata (`[~"/u/" | Id]` means `[~"/u/", Id]`), so it is kept as a part instead.
attr_value_parts({nil, _}) -> [];
attr_value_parts({cons, _, Head, Tail}) -> [Head | attr_value_parts(Tail)];
attr_value_parts(Tail) -> [Tail].

normalize_children(AST) ->
    case is_list_ast(AST) of
        true -> ast_list_to_list(AST);
        false -> [AST]
    end.

extract_attr_name(Backend, {atom, _, Name}) -> Backend:name(Name);
extract_attr_name(_Backend, BinAST) -> extract_binary_value(BinAST).

extract_binary_value({bin, _, _} = Bin) ->
    ok = note_folded(Bin),
    binary_text(Bin).

%% The text a static binary literal folds to, without recording it as folded: a
%% probe that may still give up (literal_list_text/1) must not silence the
%% latin1_binary_literal warning for a literal that ends up a runtime value.
binary_text({bin, _, Elements}) ->
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

%% Records a binary literal folded as UTF-8 text, so latin1_warnings/2 does not
%% warn about it. Only a non-ASCII one can be warned about, so only those pay.
note_folded({bin, _, Elements} = Bin) ->
    case get(?FOLDED_KEY) of
        Folded when is_map(Folded) ->
            case is_latin1_literal(Elements) of
                true ->
                    Folded = put(?FOLDED_KEY, Folded#{folded_key(Bin) => true}),
                    ok;
                false ->
                    ok
            end;
        undefined ->
            ok
    end.

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

%% The text a literal attribute value folds to: a binary literal, or a (possibly
%% nested) list of literals, which renders at runtime as the chardata it is. A pure
%% probe; fold_literal_text/1 is the variant for a value actually folded.
literal_text(AST) ->
    case is_static_binary(AST) of
        true -> {ok, binary_text(AST)};
        false -> literal_list_text(AST)
    end.

fold_literal_text(AST) ->
    note_literal_bins(AST, literal_text(AST)).

fold_literal_list_text(AST) ->
    note_literal_bins(AST, literal_list_text(AST)).

%% Records the binary literals of a value that folded as a whole, and only then:
%% a list holding a dynamic, or one probed but left a runtime value (raw text),
%% keeps its literals warnable.
note_literal_bins({bin, _, _} = Bin, {ok, _} = Folded) ->
    ok = note_folded(Bin),
    Folded;
note_literal_bins({cons, _, Head, Tail}, {ok, _} = Folded) ->
    Folded = note_literal_bins(Head, Folded),
    note_literal_bins(Tail, Folded);
note_literal_bins(_AST, Result) ->
    Result.

%% A string literal or a list -- nested to any depth -- of binary literals (folded
%% as is_static_binary/1 admits them), string literals and character codes, whose
%% runtime value would be rendered by `arizona_template:to_bin/1` as chardata. The
%% same conversion runs here, so a folded value is the bytes the runtime emits; a
%% list it would reject (an atom, an invalid code point) is left to the runtime,
%% which raises `bad_template_value` exactly as before. Any other element -- a
%% variable, a call, `?raw`, `?local`, an effect command -- leaves the whole value
%% dynamic.
literal_list_text(AST) ->
    try unicode:characters_to_binary(literal_chardata(AST)) of
        Bin when is_binary(Bin) -> {ok, Bin};
        _Invalid -> error
    catch
        throw:not_literal -> error;
        error:badarg -> error
    end.

literal_chardata({nil, _}) -> [];
literal_chardata({string, _, Chars}) -> Chars;
literal_chardata({cons, _, Head, Tail}) -> [literal_chardata_item(Head) | literal_chardata(Tail)];
literal_chardata(_) -> throw(not_literal).

literal_chardata_item({integer, _, N}) ->
    N;
literal_chardata_item({char, _, C}) ->
    C;
literal_chardata_item({bin, _, _} = Bin) ->
    case is_static_binary(Bin) of
        true -> binary_text(Bin);
        false -> throw(not_literal)
    end;
literal_chardata_item(Item) ->
    literal_chardata(Item).

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
) when Mod =:= arizona_js orelse Mod =:= arizona_android orelse Mod =:= arizona_os ->
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
    case is_static_attr_value(ValueAST) of
        true -> has_dynamic_attr(Rest);
        false -> true
    end;
has_dynamic_attr([{atom, _, _} | Rest]) ->
    has_dynamic_attr(Rest);
has_dynamic_attr(_) ->
    false.

%% A value compile_attr/4 emits as static bytes: a literal (literal_text/1) or a
%% literal command, which compile_dynamic_attr_value/5 folds through the same
%% try_fold_arizona_js/1. The element's `az` is only an op target -- event
%% delegation resolves the view through the closest `az-view` (or `az-target`),
%% and a native tap through the node's view id -- so a folded command needs none.
is_static_attr_value(ValueAST) ->
    case literal_text(ValueAST) of
        {ok, _} -> true;
        error -> try_fold_arizona_js(ValueAST) =/= error
    end.

has_dynamic_child([], _RawKind) ->
    false;
has_dynamic_child([Child | Rest], RawKind) ->
    case
        is_static_binary(Child) orelse is_element_tuple(Child) orelse
            (RawKind =:= none andalso literal_list_text(Child) =/= error)
    of
        true -> has_dynamic_child(Rest, RawKind);
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
prescan_directives(Items, true) ->
    (prescan_directives(Items))#{diff => false};
prescan_directives(Items, false) ->
    prescan_directives(Items).

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

%% Attribute names the template author may not write (or write twice), checked on the element's
%% ORIGINAL attrs -- before az-view / az-local injection, so an injected name is
%% never mistaken for an authored one.
reject_framework_attrs(Backend, Attrs, Line) ->
    ok = reject_nested_directives(Attrs, Line),
    ok = reject_nodiff_value(Attrs, Line),
    ok = reject_reserved_attrs(Attrs, Line),
    reject_duplicate_attrs(Backend, Attrs, Line).

%% `az-nodiff` is a bare directive. Its value in the HTML is the compiler's (the
%% token the client keeps the element by), so an authored one -- any value,
%% `true` included -- is rejected rather than emitted or ignored.
reject_nodiff_value(Attrs, Line) ->
    case lists:any(fun is_nodiff_value_attr/1, Attrs) of
        true -> parse_error(nodiff_value, Line);
        false -> ok
    end.

is_nodiff_value_attr({tuple, _, [NameAST, _Value]}) ->
    is_directive_attr(NameAST);
is_nodiff_value_attr(_Attr) ->
    false.

%% az-nodiff is a whole-compile-unit directive: it is stripped from a template's
%% top-level element attrs (compile_fragment_parts / compile_mixed_items) before
%% compile_element runs, which re-emits it as the compiler-owned marker the
%% element earns (nodiff_mark/3). A NESTED element reaches compile_element with its attrs
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

bare_attr_name(Attr) ->
    bare_attr_name(fun directive_attr_name/1, Attr).

%% `AtomName` spells an atom name: the directive normalization, or a backend's `name/1`.
bare_attr_name(AtomName, {atom, _, Name}) ->
    {ok, AtomName(Name)};
bare_attr_name(_AtomName, {bin, _, _} = Bin) ->
    case is_static_binary(Bin) of
        true -> {ok, extract_binary_value(Bin)};
        false -> error
    end;
bare_attr_name(_AtomName, _) ->
    error.

%% The two attribute names the transform emits into the rendered output itself:
%% `az` (the element's diff address, emitted by `Backend:az_attr/1` on any element
%% carrying dynamics) and `az-local` (the `?local` descriptor injected by
%% maybe_inject_local_descriptor/5). A template-authored copy is emitted *beside*
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

%% Two attributes of one name are both emitted, and which one takes effect depends on
%% the target (the HTML parser keeps the first, a JSON object the last) and on which
%% one an update patches, so the output stops matching the template. Names compare as
%% the backend emits them (`name/1`, so `az_click` and `<<"az-click">>` meet), under its
%% `attr_names/0` policy; the error quotes the repeated one as written.
reject_duplicate_attrs(Backend, Attrs, Line) ->
    case Backend:attr_names() of
        repeatable ->
            ok;
        Policy ->
            Names = [
                Name
             || Attr <- Attrs, Name <- [attr_name(fun Backend:name/1, Attr)], Name =/= undefined
            ],
            reject_repeated_attr(Policy, Names, #{}, Line)
    end.

reject_repeated_attr(_Policy, [], _Seen, _Line) ->
    ok;
reject_repeated_attr(Policy, [Name | Rest], Seen, Line) ->
    Key = attr_compare_key(Policy, Name),
    case Seen of
        #{Key := true} -> parse_error({duplicate_attr, Name}, Line);
        #{} -> reject_repeated_attr(Policy, Rest, Seen#{Key => true}, Line)
    end.

attr_compare_key(case_sensitive, Name) ->
    Name;
attr_compare_key(case_insensitive, Name) ->
    ascii_lowercase(Name).

%% ASCII only, as the browser folds names: `string:lowercase/1` also maps a
%% non-ASCII letter such as the Kelvin sign (U+212A) to `k`.
ascii_lowercase(Name) ->
    <<<<(ascii_lower(C))>> || <<C>> <= Name>>.

ascii_lower(C) when C >= $A, C =< $Z -> C + ($a - $A);
ascii_lower(C) -> C.

%% The literal name of an attribute in any of its forms (`name`, `<<"name">>`,
%% `{name, Value}`), normalized like a directive so `az_local` and `'az-local'`
%% are one name. `undefined` when the name is not a compile-time literal.
attr_name(Attr) ->
    attr_name(fun directive_attr_name/1, Attr).

attr_name(AtomName, {tuple, _, [NameAST | _]}) ->
    attr_name(AtomName, NameAST);
attr_name(AtomName, Attr) ->
    case bare_attr_name(AtomName, Attr) of
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
    ascii_lowercase(Name).

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
