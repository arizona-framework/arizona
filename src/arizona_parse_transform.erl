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
    IsLive =
        has_behaviour(Forms, arizona_stateful) orelse
            has_behaviour(Forms, arizona_view),
    try
        Transformed = [transform_form(mark_targets(Form, none), Module, IsLive) || Form <- Forms],
        erl_syntax:revert_forms(Transformed)
    catch
        throw:{arizona_parse_error, Line, Reason} ->
            {error, [{File, [{Line, ?MODULE, Reason}]}], []}
    end.

%% Top-down pre-pass threading the enclosing render target `Ctx` (`none` outside
%% any template, else `html` | `native` | `terminal`). Two jobs:
%%
%%   1. A single `?each` serves html and native. Inside a `?native(...)` every
%%      nested `?each` is rewritten to the internal `native_each` so the
%%      bottom-up transform compiles it with the native backend. `?each` under
%%      `?html` (or standalone) is left untouched; `?each` under `?terminal` is
%%      rejected (the terminal backend has no `?each` wiring yet).
%%   2. Reject inline cross-target nesting: any target macro literally inside a
%%      different one (e.g. `?html(...)` in `?native(...)`) would mix incompatible
%%      statics in one tree. Caught here as a compile error instead of corrupting
%%      the output at runtime.
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
     || A <- Args
    ]};
mark_targets({call, L, {remote, _, {atom, _, Mod}, {atom, _, each}}, _}, terminal) when
    Mod =:= arizona_template orelse Mod =:= az
->
    parse_error(each_in_terminal, L);
mark_targets(Node, Ctx) when is_tuple(Node) ->
    list_to_tuple([mark_targets(E, Ctx) || E <- tuple_to_list(Node)]);
mark_targets(Nodes, Ctx) when is_list(Nodes) ->
    [mark_targets(E, Ctx) || E <- Nodes];
mark_targets(Node, _Ctx) ->
    Node.

-doc """
Formats parse transform error reasons into human-readable messages.

Called by the compiler when `parse_transform/2` returns an error tuple.
""".
-spec format_error(Reason) -> string() when
    Reason :: term().
format_error({render_reject, Message}) ->
    unicode:characters_to_list(Message);
format_error(each_in_terminal) ->
    "?each is not supported inside ?terminal -- pre-format list content into a "
    "binary in the handler and render it as a dynamic child";
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
    "stateful/stateless child of the matching target".

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

parse_error(Reason, Line) ->
    throw({arizona_parse_error, Line, Reason}).

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

transform_form({function, L, render, 1, Clauses}, Module, true) ->
    {function, L, render, 1, [transform_live_render_clause(C, Module) || C <- Clauses]};
transform_form({function, L, Name, Arity, Clauses}, Module, _IsLive) ->
    {function, L, Name, Arity, [transform_clause(C, Module) || C <- Clauses]};
transform_form(Form, _Module, _IsLive) ->
    Form.

transform_clause({clause, L, Patterns, Guards, Body}, Module) ->
    {clause, L, Patterns, Guards, [transform_expr(Expr, Module) || Expr <- Body]}.

transform_expr(Expr, Module) ->
    erl_syntax_lib:map(fun(Node) -> transform_node(Node, Module) end, Expr).

transform_node(Node, Module) ->
    N = erl_syntax:revert(Node),
    case N of
        {call, L, {remote, _, {atom, _, Mod}, {atom, _, html}}, [Arg]} when
            Mod =:= arizona_template; Mod =:= az
        ->
            compile_template(Arg, L, Module);
        {call, L, {remote, _, {atom, _, Mod}, {atom, _, native}}, [Arg]} when
            Mod =:= arizona_template; Mod =:= az
        ->
            compile_template(Arg, L, Module, false, arizona_native);
        {call, L, {remote, _, {atom, _, Mod}, {atom, _, terminal}}, [Arg]} when
            Mod =:= arizona_template; Mod =:= az
        ->
            %% Terminal templates compile as non-live (no client root id / az_view
            %% injection): the live process derives the view id from bindings, and
            %% the terminal transport repaints whole frames rather than addressing
            %% the root node. So the live-render last-expr path falls through to
            %% here for `?terminal(...)`.
            compile_template(Arg, L, Module, false, arizona_terminal);
        {call, L, {remote, _, {atom, _, Mod}, {atom, _, each}}, [FunArg, SourceArg]} when
            Mod =:= arizona_template; Mod =:= az
        ->
            compile_each(FunArg, SourceArg, L, Module);
        {call, L, {remote, _, {atom, _, Mod}, {atom, _, native_each}}, [FunArg, SourceArg]} when
            Mod =:= arizona_template; Mod =:= az
        ->
            compile_each(FunArg, SourceArg, L, Module, arizona_native);
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

transform_live_render_clause({clause, L, Patterns, Guards, Body}, Module) ->
    {Init, [Last]} = lists:split(length(Body) - 1, Body),
    TransformedInit = [transform_expr(Expr, Module) || Expr <- Init],
    TransformedLast = transform_live_render_last(Last, Module),
    {clause, L, Patterns, Guards, TransformedInit ++ [TransformedLast]}.

transform_live_render_last(Expr, Module) ->
    N = erl_syntax:revert(Expr),
    case N of
        {call, L, {remote, _, {atom, _, Mod}, {atom, _, html}}, [Arg]} when
            Mod =:= arizona_template; Mod =:= az
        ->
            validate_live_root(Arg, L),
            Arg1 = transform_expr(Arg, Module),
            compile_template(Arg1, L, Module, true);
        {call, L, {remote, _, {atom, _, Mod}, {atom, _, native}}, [Arg]} when
            Mod =:= arizona_template; Mod =:= az
        ->
            validate_live_root(Arg, L),
            Arg1 = transform_expr(Arg, Module),
            compile_template(Arg1, L, Module, true, arizona_native);
        {'case', L, CaseExpr, Clauses} ->
            {'case', L, transform_expr(CaseExpr, Module), [
                transform_live_render_branch(C, Module)
             || C <- Clauses
            ]};
        {'if', L, Clauses} ->
            {'if', L, [transform_live_render_branch(C, Module) || C <- Clauses]};
        {block, L, Body} ->
            transform_live_render_block(L, Body, Module);
        {'receive', L, Clauses} ->
            {'receive', L, [transform_live_render_branch(C, Module) || C <- Clauses]};
        {'receive', L, Clauses, AfterExpr, AfterBody} ->
            {'receive', L, [transform_live_render_branch(C, Module) || C <- Clauses],
                transform_expr(AfterExpr, Module),
                transform_live_render_block_body(AfterBody, Module)};
        {'try', L, Body, OfClauses, CatchClauses, AfterBody} ->
            {'try', L, transform_live_render_block_body(Body, Module),
                [transform_live_render_branch(C, Module) || C <- OfClauses],
                [transform_live_render_branch(C, Module) || C <- CatchClauses], [
                    transform_expr(E, Module)
                 || E <- AfterBody
                ]};
        {'maybe', L, Body} ->
            {'maybe', L, transform_live_render_block_body(Body, Module)};
        {'maybe', L, Body, {'else', L2, ElseClauses}} ->
            {'maybe', L, transform_live_render_block_body(Body, Module),
                {'else', L2, [transform_live_render_branch(C, Module) || C <- ElseClauses]}};
        _ ->
            transform_expr(Expr, Module)
    end.

transform_live_render_block(L, Body, Module) ->
    {block, L, transform_live_render_block_body(Body, Module)}.

transform_live_render_block_body(Body, Module) ->
    {Init, [Last]} = lists:split(length(Body) - 1, Body),
    [transform_expr(E, Module) || E <- Init] ++ [transform_live_render_last(Last, Module)].

transform_live_render_branch({clause, L, Patterns, Guards, Body}, Module) ->
    {Init, [Last]} = lists:split(length(Body) - 1, Body),
    TransInit = [transform_expr(E, Module) || E <- Init],
    {clause, L, Patterns, Guards, TransInit ++ [transform_live_render_last(Last, Module)]}.

validate_live_root({tuple, _, [_Tag, Attrs | _]}, L) ->
    validate_id_expr(Attrs, L);
validate_live_root(_, L) ->
    parse_error(live_render_not_single_element, L).

validate_id_expr({cons, _, {tuple, _, [{atom, _, id}, ValueAST]}, _}, L) ->
    case is_get_id_call(ValueAST) of
        true -> ok;
        false -> parse_error(live_render_id_must_be_get_id, L)
    end;
validate_id_expr({cons, _, _, Rest}, L) ->
    validate_id_expr(Rest, L);
validate_id_expr(_, L) ->
    parse_error(live_render_missing_id, L).

is_get_id_call({call, _, {remote, _, {atom, _, Mod}, {atom, _, get}}, [{atom, _, id}, _]}) when
    Mod =:= arizona_template; Mod =:= az
->
    true;
is_get_id_call(_) ->
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

compile_each(FunAST, SourceAST, Line, Module) ->
    compile_each(FunAST, SourceAST, Line, Module, arizona_html).

compile_each(FunAST, SourceAST, Line, Module, Backend) ->
    case FunAST of
        {'fun', _, {clauses, [{clause, _, [ItemVar, KeyVar], Guards, Body}]}} ->
            {Prefix, LastExpr} = split_fun_body(Body),
            {Statics, DynASTs, Fingerprint, Opts0} = compile_body_parts(
                LastExpr, Module, false, Backend
            ),
            Opts = maybe_target_opt(Backend, Opts0),
            {S1, D1} = scope_az(Backend, Fingerprint, Statics, DynASTs),
            build_each_ast(
                Line, SourceAST, [ItemVar, KeyVar], Guards, Prefix, S1, D1, Fingerprint, Opts
            );
        {'fun', _, {clauses, [{clause, _, [ItemVar], Guards, Body}]}} ->
            {Prefix, LastExpr} = split_fun_body(Body),
            {Statics, DynASTs, Fingerprint, Opts0} = compile_body_parts(
                LastExpr, Module, false, Backend
            ),
            Opts = maybe_target_opt(Backend, Opts0),
            {S1, D1} = scope_az(Backend, Fingerprint, Statics, DynASTs),
            build_each_ast(
                Line, SourceAST, [ItemVar], Guards, Prefix, S1, D1, Fingerprint, Opts
            );
        %% `fun name/arity` / `fun Mod:name/arity` -- synthesize an anonymous
        %% wrapper clause that calls the referenced function with the item
        %% (and optional key) var, then recurse into the clause path above.
        {'fun', L, {function, Name, Arity}} when
            is_atom(Name) andalso (Arity =:= 1 orelse Arity =:= 2)
        ->
            compile_each(
                wrap_fun_ref(L, Arity, {atom, L, Name}, none), SourceAST, Line, Module, Backend
            );
        {'fun', L, {function, Mod, Name, {integer, _, Arity}}} when
            Arity =:= 1 orelse Arity =:= 2
        ->
            compile_each(wrap_fun_ref(L, Arity, Name, Mod), SourceAST, Line, Module, Backend);
        _ ->
            parse_error(invalid_each_fun, Line)
    end.

%% Build `fun(I) -> Callee(I) end` or `fun(I, K) -> Callee(I, K) end`,
%% where Callee is either `Name(...)` or `Mod:Name(...)`.
wrap_fun_ref(L, Arity, NameAST, ModAST) ->
    VarNames = ['__I', '__K'],
    Vars = [{var, L, V} || V <- lists:sublist(VarNames, Arity)],
    Callee =
        case ModAST of
            none -> NameAST;
            _ -> {remote, L, ModAST, NameAST}
        end,
    Call = {call, L, Callee, Vars},
    {'fun', L, {clauses, [{clause, L, Vars, [], [Call]}]}}.

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
compile_classified_body(text_dynamic, ExprAST, Module, _LiveRender, _Backend) ->
    Statics = [<<>>, <<>>],
    DynASTs = [make_text_dynamic_ast(<<"0">>, ExprAST, Module, line(ExprAST))],
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

compile_mixed_dynamic(Item, Module, #state{nodiff = true} = State) ->
    flush(State, make_nodiff_dynamic_ast(Item, Module, line(Item)));
compile_mixed_dynamic(Item, Module, State) ->
    flush(State, make_text_dynamic_ast(<<"0">>, Item, Module, line(Item))).

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
    State1 = State0#state{root = false},
    HasDyn = has_dynamic_attr(Attrs) orelse has_dynamic_child(Children),
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
            State7 = compile_children(Children, ElemAz, State6),
            buf_append(State7, Backend:element_close(TagBin))
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

emit_child_dynamic(Child, _ElemAz, #state{nodiff = true, module = Module} = State0, Slot) ->
    DynAST = make_nodiff_dynamic_ast(Child, Module, line(Child)),
    {flush(State0, DynAST), Slot};
emit_child_dynamic(Child, ElemAz, #state{module = Module, backend = Backend} = State0, Slot) ->
    ElemAzBin = integer_to_binary(ElemAz),
    MarkerAz = Backend:text_az(ElemAzBin, Slot),
    State1 = buf_append(State0, Backend:text_slot_open(MarkerAz)),
    DynAST = make_text_dynamic_ast(MarkerAz, Child, Module, line(Child)),
    State2 = flush(State1, DynAST),
    {State2#state{buf = Backend:text_slot_close()}, Slot + 1}.

make_text_dynamic_ast(AzBin, ExprAST, Module, ExprLine) ->
    LocAST = loc_ast(Module, ExprLine),
    {tuple, 0, [
        ast_binary(AzBin),
        {'fun', 0, {clauses, [{clause, 0, [], [], [ExprAST]}]}},
        LocAST
    ]}.

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

make_nodiff_dynamic_ast(ExprAST, Module, ExprLine) ->
    LocAST = loc_ast(Module, ExprLine),
    {tuple, 0, [
        {atom, 0, undefined},
        {'fun', 0, {clauses, [{clause, 0, [], [], [ExprAST]}]}},
        LocAST
    ]}.

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
