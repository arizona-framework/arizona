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
    root = false :: boolean()
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
    try
        Transformed = [transform_form(Form, Module, IsLive) || Form <:- Forms],
        erl_syntax:revert_forms(Transformed)
    catch
        throw:{arizona_parse_error, Line, Reason} ->
            {error, [{File, [{Line, ?MODULE, Reason}]}], []}
    end.

-doc """
Formats parse transform error reasons into human-readable messages.

Called by the compiler when `parse_transform/2` returns an error tuple.
""".
-spec format_error(Reason) -> string() when
    Reason :: term().
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
    ).

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
    {function, L, render, 1, [transform_live_render_clause(C, Module) || C <:- Clauses]};
transform_form({function, L, Name, Arity, Clauses}, Module, _IsLive) ->
    {function, L, Name, Arity, [transform_clause(C, Module) || C <:- Clauses]};
transform_form(Form, _Module, _IsLive) ->
    Form.

transform_clause({clause, L, Patterns, Guards, Body}, Module) ->
    {clause, L, Patterns, Guards, [transform_expr(Expr, Module) || Expr <:- Body]}.

transform_expr(Expr, Module) ->
    erl_syntax_lib:map(fun(Node) -> transform_node(Node, Module) end, Expr).

transform_node(Node, Module) ->
    N = erl_syntax:revert(Node),
    case N of
        {call, L, {remote, _, {atom, _, Mod}, {atom, _, html}}, [Arg]} when
            Mod =:= arizona_template; Mod =:= az
        ->
            compile_template(Arg, L, Module);
        {call, L, {remote, _, {atom, _, Mod}, {atom, _, each}}, [FunArg, SourceArg]} when
            Mod =:= arizona_template; Mod =:= az
        ->
            compile_each(FunArg, SourceArg, L, Module);
        _ ->
            N
    end.

transform_live_render_clause({clause, L, Patterns, Guards, Body}, Module) ->
    {Init, [Last]} = lists:split(length(Body) - 1, Body),
    TransformedInit = [transform_expr(Expr, Module) || Expr <:- Init],
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
        {'case', L, CaseExpr, Clauses} ->
            {'case', L, transform_expr(CaseExpr, Module), [
                transform_live_render_branch(C, Module)
             || C <:- Clauses
            ]};
        {'if', L, Clauses} ->
            {'if', L, [transform_live_render_branch(C, Module) || C <:- Clauses]};
        {block, L, Body} ->
            transform_live_render_block(L, Body, Module);
        {'receive', L, Clauses} ->
            {'receive', L, [transform_live_render_branch(C, Module) || C <:- Clauses]};
        {'receive', L, Clauses, AfterExpr, AfterBody} ->
            {'receive', L, [transform_live_render_branch(C, Module) || C <:- Clauses],
                transform_expr(AfterExpr, Module),
                transform_live_render_block_body(AfterBody, Module)};
        {'try', L, Body, OfClauses, CatchClauses, AfterBody} ->
            {'try', L, transform_live_render_block_body(Body, Module),
                [transform_live_render_branch(C, Module) || C <:- OfClauses],
                [transform_live_render_branch(C, Module) || C <:- CatchClauses], [
                    transform_expr(E, Module)
                 || E <:- AfterBody
                ]};
        {'maybe', L, Body} ->
            {'maybe', L, transform_live_render_block_body(Body, Module)};
        {'maybe', L, Body, {'else', L2, ElseClauses}} ->
            {'maybe', L, transform_live_render_block_body(Body, Module),
                {'else', L2, [transform_live_render_branch(C, Module) || C <:- ElseClauses]}};
        _ ->
            transform_expr(Expr, Module)
    end.

transform_live_render_block(L, Body, Module) ->
    {block, L, transform_live_render_block_body(Body, Module)}.

transform_live_render_block_body(Body, Module) ->
    {Init, [Last]} = lists:split(length(Body) - 1, Body),
    [transform_expr(E, Module) || E <:- Init] ++ [transform_live_render_last(Last, Module)].

transform_live_render_branch({clause, L, Patterns, Guards, Body}, Module) ->
    {Init, [Last]} = lists:split(length(Body) - 1, Body),
    TransInit = [transform_expr(E, Module) || E <:- Init],
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
    {Statics, DynASTs, Fingerprint, Opts} = compile_body_parts(Arg, Module, LiveRender),
    {S1, D1} = scope_az(Fingerprint, Statics, DynASTs),
    build_template_ast(Line, S1, D1, Fingerprint, Opts).

compile_each(FunAST, SourceAST, Line, Module) ->
    case FunAST of
        {'fun', _, {clauses, [{clause, _, [ItemVar, KeyVar], Guards, Body}]}} ->
            {Prefix, LastExpr} = split_fun_body(Body),
            {Statics, DynASTs, Fingerprint, Opts} = compile_body_parts(LastExpr, Module),
            {S1, D1} = scope_az(Fingerprint, Statics, DynASTs),
            build_each_ast(
                Line, SourceAST, [ItemVar, KeyVar], Guards, Prefix, S1, D1, Fingerprint, Opts
            );
        {'fun', _, {clauses, [{clause, _, [ItemVar], Guards, Body}]}} ->
            {Prefix, LastExpr} = split_fun_body(Body),
            {Statics, DynASTs, Fingerprint, Opts} = compile_body_parts(LastExpr, Module),
            {S1, D1} = scope_az(Fingerprint, Statics, DynASTs),
            build_each_ast(
                Line, SourceAST, [ItemVar], Guards, Prefix, S1, D1, Fingerprint, Opts
            );
        %% `fun name/arity` / `fun Mod:name/arity` -- synthesize an anonymous
        %% wrapper clause that calls the referenced function with the item
        %% (and optional key) var, then recurse into the clause path above.
        {'fun', L, {function, Name, Arity}} when
            is_atom(Name) andalso (Arity =:= 1 orelse Arity =:= 2)
        ->
            compile_each(wrap_fun_ref(L, Arity, {atom, L, Name}, none), SourceAST, Line, Module);
        {'fun', L, {function, Mod, Name, {integer, _, Arity}}} when
            Arity =:= 1 orelse Arity =:= 2
        ->
            compile_each(wrap_fun_ref(L, Arity, Name, Mod), SourceAST, Line, Module);
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

compile_body_parts(ExprAST, Module) ->
    compile_body_parts(ExprAST, Module, false).

compile_body_parts(ExprAST, Module, LiveRender) ->
    compile_classified_body(classify_body(ExprAST), ExprAST, Module, LiveRender).

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

compile_classified_body(static_binary, ExprAST, _Module, _LiveRender) ->
    Bin = extract_binary_value(ExprAST),
    Statics = [Bin],
    {Statics, [], generate_fingerprint(Statics), #{}};
compile_classified_body(element_tuple, ExprAST, Module, LiveRender) ->
    compile_fragment_parts([ExprAST], Module, LiveRender);
compile_classified_body(element_list, ExprAST, Module, LiveRender) ->
    compile_fragment_parts(ast_list_to_list(ExprAST), Module, LiveRender);
compile_classified_body(list_ast, ExprAST, Module, _LiveRender) ->
    compile_mixed_items(ast_list_to_list(ExprAST), Module);
compile_classified_body(text_dynamic, ExprAST, Module, _LiveRender) ->
    Statics = [<<>>, <<>>],
    DynASTs = [make_text_dynamic_ast(<<"0">>, ExprAST, Module, line(ExprAST))],
    {Statics, DynASTs, generate_fingerprint(Statics), #{}}.

compile_fragment_parts(ElementASTs, Module, LiveRender) ->
    Opts = prescan_directives(ElementASTs),
    State0 = #state{
        module = Module,
        nodiff = maps:is_key(diff, Opts),
        live_render = LiveRender,
        root = LiveRender
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

compile_mixed_items(Items, Module) ->
    Opts = prescan_directives(Items),
    State0 = #state{module = Module, nodiff = maps:is_key(diff, Opts)},
    State1 = lists:foldl(
        fun(Item, State) -> compile_mixed_item(Item, Module, State) end, State0, Items
    ),
    {Statics, DynASTs} = finalize(State1),
    Fingerprint = generate_fingerprint(Statics),
    {Statics, DynASTs, Fingerprint, Opts}.

compile_mixed_item(Item, Module, State) ->
    case is_static_binary(Item) of
        true -> buf_append(State, extract_binary_value(Item));
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
    Attrs = maybe_inject_or_raise_az_view(Attrs0, Line, State0),
    State1 = State0#state{root = false},
    HasDyn = has_dynamic_attr(Attrs) orelse has_dynamic_child(Children),
    {ElemAz, State2} =
        case HasDyn andalso (not State1#state.nodiff) of
            true -> {State1#state.az, State1#state{az = State1#state.az + 1}};
            false -> {none, State1}
        end,
    TagBin = atom_to_html_binary(Tag),
    State3 = buf_append(State2, <<"<", TagBin/binary>>),
    State4 =
        case ElemAz of
            none ->
                State3;
            N ->
                AzBin = integer_to_binary(N),
                buf_append(State3, <<" az=\"", AzBin/binary, "\"">>)
        end,
    State5 = compile_attrs(Attrs, ElemAz, State4, Line),
    case is_void(Tag) andalso Children =/= [] of
        true -> parse_error({void_with_children, Tag}, Line);
        false -> ok
    end,
    case is_void(Tag) of
        true ->
            buf_append(State5, <<" />">>);
        false ->
            State6 = buf_append(State5, <<">">>),
            State7 = compile_children(Children, ElemAz, State6),
            buf_append(State7, <<"</", TagBin/binary, ">">>)
    end.

compile_attrs([], _ElemAz, State, _ElemLine) ->
    State;
compile_attrs([Attr | Rest], ElemAz, State0, ElemLine) ->
    State1 = compile_attr(Attr, ElemAz, State0, ElemLine),
    compile_attrs(Rest, ElemAz, State1, ElemLine).

compile_attr({bin, _, _} = Bin, _ElemAz, State0, _ElemLine) ->
    NameBin = extract_binary_value(Bin),
    buf_append(State0, <<" ", NameBin/binary>>);
compile_attr({tuple, _, [NameAST, {atom, _, false}]}, _ElemAz, State0, _ElemLine) when
    element(1, NameAST) =:= atom; element(1, NameAST) =:= bin
->
    State0;
compile_attr({tuple, _, [NameAST, {atom, _, true}]}, _ElemAz, State0, _ElemLine) when
    element(1, NameAST) =:= atom; element(1, NameAST) =:= bin
->
    NameBin = extract_attr_name(NameAST),
    buf_append(State0, <<" ", NameBin/binary>>);
compile_attr({tuple, _, [NameAST, ValueAST]}, ElemAz, State0, _ElemLine) when
    element(1, NameAST) =:= atom; element(1, NameAST) =:= bin
->
    NameBin = extract_attr_name(NameAST),
    case is_static_binary(ValueAST) of
        true ->
            ValBin = extract_binary_value(ValueAST),
            buf_append(State0, <<" ", NameBin/binary, "=\"", ValBin/binary, "\"">>);
        false when State0#state.nodiff ->
            Module = State0#state.module,
            DynAST = make_nodiff_attr_dynamic_ast(NameBin, ValueAST, Module, line(ValueAST)),
            flush(State0, DynAST);
        false ->
            Module = State0#state.module,
            AzBin = integer_to_binary(ElemAz),
            DynAST = make_attr_dynamic_ast(AzBin, NameBin, ValueAST, Module, line(ValueAST)),
            flush(State0, DynAST)
    end;
compile_attr({atom, _, Name}, _ElemAz, State0, _ElemLine) ->
    NameBin = atom_to_html_binary(Name),
    buf_append(State0, <<" ", NameBin/binary>>);
compile_attr(Attr, _ElemAz, _State0, ElemLine) ->
    AttrLine =
        try
            line(Attr)
        catch
            _:_ -> ElemLine
        end,
    parse_error(invalid_attribute, AttrLine).

compile_children(Children, ElemAz, State) ->
    compile_children(Children, ElemAz, State, 0).

compile_children([], _ElemAz, State, _Slot) ->
    State;
compile_children([Child | Rest], ElemAz, State0, Slot) ->
    {State1, NextSlot} = compile_child(Child, ElemAz, State0, Slot),
    compile_children(Rest, ElemAz, State1, NextSlot).

compile_child(Child, ElemAz, State0, Slot) ->
    case is_static_binary(Child) of
        true ->
            {buf_append(State0, extract_binary_value(Child)), Slot};
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
emit_child_dynamic(Child, ElemAz, #state{module = Module} = State0, Slot) ->
    ElemAzBin = integer_to_binary(ElemAz),
    MarkerAz = marker_az(ElemAzBin, Slot),
    State1 = buf_append(State0, <<"<!--az:", MarkerAz/binary, "-->">>),
    DynAST = make_text_dynamic_ast(MarkerAz, Child, Module, line(Child)),
    State2 = flush(State1, DynAST),
    {State2#state{buf = <<"<!--/az-->">>}, Slot + 1}.

marker_az(ElemAzBin, 0) ->
    ElemAzBin;
marker_az(ElemAzBin, Slot) ->
    <<ElemAzBin/binary, ":", (integer_to_binary(Slot))/binary>>.

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
scope_az(_Fp, Statics, []) ->
    {Statics, []};
scope_az(Fp, Statics, DynASTs) ->
    {[scope_static(Fp, S) || S <:- Statics], [scope_dynamic_ast(Fp, D) || D <:- DynASTs]}.

scope_static(Fp, S0) ->
    S1 = binary:replace(S0, <<" az=\"">>, <<" az=\"", Fp/binary, "-">>, [global]),
    binary:replace(S1, <<"<!--az:">>, <<"<!--az:", Fp/binary, "-">>, [global]).

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
    {ast_list([ast_binary(S) || S <:- Statics]), ast_list(DynASTs), ast_binary(Fingerprint)}.

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

extract_attr_name({atom, _, Name}) -> atom_to_html_binary(Name);
extract_attr_name(BinAST) -> extract_binary_value(BinAST).

extract_binary_value({bin, _, Elements}) ->
    iolist_to_binary([extract_bin_element(E) || E <:- Elements]).

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
prescan_directives(Items) ->
    lists:foldl(
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
    ).

directive_opts(<<"az-nodiff">>) -> {ok, #{diff => false}};
directive_opts(_) -> false.

bare_attr_name({atom, _, Name}) ->
    {ok, atom_to_html_binary(Name)};
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
    maps:fold(
        fun(K, V, Acc) ->
            [{map_field_assoc, Line, {atom, Line, K}, {atom, Line, V}} | Acc]
        end,
        [],
        Opts
    ).

atom_to_html_binary(Atom) ->
    binary:replace(atom_to_binary(Atom), <<"_">>, <<"-">>, [global]).

is_void(area) -> true;
is_void(base) -> true;
is_void(br) -> true;
is_void(col) -> true;
is_void(embed) -> true;
is_void(hr) -> true;
is_void(img) -> true;
is_void(input) -> true;
is_void(link) -> true;
is_void(meta) -> true;
is_void(param) -> true;
is_void(source) -> true;
is_void(track) -> true;
is_void(wbr) -> true;
is_void(_) -> false.
