-module(arizona_parse_transform).
-export([parse_transform/2, format_error/1]).
-ignore_xref([parse_transform/2, format_error/1]).

-record(cs, {
    buf = <<>> :: binary(),
    statics = [] :: [binary()],
    dynamics = [] :: [term()],
    az = 0 :: non_neg_integer(),
    nodiff = false :: boolean(),
    module = undefined :: atom() | undefined,
    live_render = false :: boolean(),
    root = false :: boolean()
}).

%% --- Parse Transform Entry Point --------------------------------------------

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

%% --- Error Reporting --------------------------------------------------------

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

parse_error(Reason, Line) ->
    throw({arizona_parse_error, Line, Reason}).

line(Node) when is_tuple(Node), tuple_size(Node) >= 2 ->
    erl_anno:line(element(2, Node));
line(_) ->
    0.

%% --- Form / Expression Transformation ---------------------------------------

transform_form({function, L, render, 1, Clauses}, Module, true) ->
    {function, L, render, 1, [transform_live_render_clause(C, Module) || C <:- Clauses]};
transform_form({function, L, Name, Arity, Clauses}, Module, _IsLive) ->
    {function, L, Name, Arity, [transform_clause(C, Module) || C <:- Clauses]};
transform_form(Form, _Module, _IsLive) ->
    Form.

transform_live_render_clause({clause, L, Patterns, Guards, Body}, Module) ->
    {Init, [Last]} = lists:split(length(Body) - 1, Body),
    TransformedInit = [transform_expr(Expr, Module) || Expr <:- Init],
    TransformedLast = transform_live_render_last(Last, Module),
    {clause, L, Patterns, Guards, TransformedInit ++ [TransformedLast]}.

%% Walk the last expression of render/1 looking for ?html(...) calls.
%% Recurses into branching expressions so that conditional renders
%% get az-view auto-injected on each branch's root element.
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

%% --- Template Compilation ----------------------------------------------------

compile_body_parts(ExprAST, Module) ->
    compile_body_parts(ExprAST, Module, false).

compile_body_parts(ExprAST, Module, LiveRender) ->
    case is_static_binary(ExprAST) of
        true ->
            Bin = extract_binary_value(ExprAST),
            Statics = [Bin],
            {Statics, [], generate_fingerprint(Statics), #{}};
        false ->
            case is_element_tuple(ExprAST) of
                true ->
                    compile_fragment_parts([ExprAST], Module, LiveRender);
                false ->
                    case is_element_list(ExprAST) of
                        true ->
                            compile_fragment_parts(ast_list_to_list(ExprAST), Module, LiveRender);
                        false ->
                            case is_list_ast(ExprAST) of
                                true ->
                                    compile_mixed_items(ast_list_to_list(ExprAST), Module);
                                false ->
                                    Statics = [<<>>, <<>>],
                                    DynASTs = [
                                        make_text_dynamic_ast(
                                            <<"0">>, ExprAST, Module, line(ExprAST)
                                        )
                                    ],
                                    {Statics, DynASTs, generate_fingerprint(Statics), #{}}
                            end
                    end
            end
    end.

compile_fragment_parts(ElementASTs, Module, LiveRender) ->
    Opts = prescan_directives(ElementASTs),
    CS0 = #cs{
        module = Module,
        nodiff = maps:is_key(diff, Opts),
        live_render = LiveRender,
        root = LiveRender
    },
    CS1 = lists:foldl(
        fun(Elem, CS) ->
            {Tag, Attrs0, Children, ElemLine} = extract_element(Elem),
            {Attrs, _ElemOpts} = extract_directives(Attrs0),
            compile_element(Tag, Attrs, Children, ElemLine, CS)
        end,
        CS0,
        ElementASTs
    ),
    {Statics, DynASTs} = finalize(CS1),
    Fingerprint = generate_fingerprint(Statics),
    {Statics, DynASTs, Fingerprint, Opts}.

compile_mixed_items(Items, Module) ->
    Opts = prescan_directives(Items),
    CS0 = #cs{module = Module, nodiff = maps:is_key(diff, Opts)},
    CS1 = lists:foldl(
        fun(Item, CS) ->
            case is_static_binary(Item) of
                true ->
                    buf_append(CS, extract_binary_value(Item));
                false ->
                    case is_element_tuple(Item) of
                        true ->
                            {Tag, Attrs0, Children, ElemLine} = extract_element(Item),
                            {Attrs, _ElemOpts} = extract_directives(Attrs0),
                            compile_element(Tag, Attrs, Children, ElemLine, CS);
                        false ->
                            DynAST =
                                case CS#cs.nodiff of
                                    true ->
                                        make_nodiff_dynamic_ast(Item, Module, line(Item));
                                    false ->
                                        make_text_dynamic_ast(<<"0">>, Item, Module, line(Item))
                                end,
                            flush(CS, DynAST)
                    end
            end
        end,
        CS0,
        Items
    ),
    {Statics, DynASTs} = finalize(CS1),
    Fingerprint = generate_fingerprint(Statics),
    {Statics, DynASTs, Fingerprint, Opts}.

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
        _ ->
            parse_error(invalid_each_fun, Line)
    end.

%% --- Element Compilation -----------------------------------------------------

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

maybe_inject_or_raise_az_view(Attrs, Line, #cs{live_render = true, root = true}) ->
    case lists:any(fun is_az_view_attr/1, Attrs) of
        true -> Attrs;
        false -> [{atom, Line, az_view} | Attrs]
    end;
maybe_inject_or_raise_az_view(Attrs, Line, _CS) ->
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

compile_element(Tag, Attrs0, Children, Line, CS0) ->
    Attrs = maybe_inject_or_raise_az_view(Attrs0, Line, CS0),
    CS1 = CS0#cs{root = false},
    HasDyn = has_dynamic_attr(Attrs) orelse has_dynamic_child(Children),
    {ElemAz, CS2} =
        case HasDyn andalso (not CS1#cs.nodiff) of
            true -> {CS1#cs.az, CS1#cs{az = CS1#cs.az + 1}};
            false -> {none, CS1}
        end,
    TagBin = atom_to_html_binary(Tag),
    CS3 = buf_append(CS2, <<"<", TagBin/binary>>),
    CS4 =
        case ElemAz of
            none ->
                CS3;
            N ->
                AzBin = integer_to_binary(N),
                buf_append(CS3, <<" az=\"", AzBin/binary, "\"">>)
        end,
    CS5 = compile_attrs(Attrs, ElemAz, CS4, Line),
    case is_void(Tag) andalso Children =/= [] of
        true -> parse_error({void_with_children, Tag}, Line);
        false -> ok
    end,
    case is_void(Tag) of
        true ->
            buf_append(CS5, <<" />">>);
        false ->
            CS6 = buf_append(CS5, <<">">>),
            CS7 = compile_children(Children, ElemAz, CS6),
            buf_append(CS7, <<"</", TagBin/binary, ">">>)
    end.

%% --- Attribute Compilation ---------------------------------------------------

compile_attrs([], _ElemAz, CS, _ElemLine) ->
    CS;
compile_attrs([Attr | Rest], ElemAz, CS0, ElemLine) ->
    CS1 = compile_attr(Attr, ElemAz, CS0, ElemLine),
    compile_attrs(Rest, ElemAz, CS1, ElemLine).

compile_attr({bin, _, _} = Bin, _ElemAz, CS0, _ElemLine) ->
    NameBin = extract_binary_value(Bin),
    buf_append(CS0, <<" ", NameBin/binary>>);
compile_attr({tuple, _, [NameAST, {atom, _, false}]}, _ElemAz, CS0, _ElemLine) when
    element(1, NameAST) =:= atom; element(1, NameAST) =:= bin
->
    CS0;
compile_attr({tuple, _, [NameAST, {atom, _, true}]}, _ElemAz, CS0, _ElemLine) when
    element(1, NameAST) =:= atom; element(1, NameAST) =:= bin
->
    NameBin = extract_attr_name(NameAST),
    buf_append(CS0, <<" ", NameBin/binary>>);
compile_attr({tuple, _, [NameAST, ValueAST]}, ElemAz, CS0, _ElemLine) when
    element(1, NameAST) =:= atom; element(1, NameAST) =:= bin
->
    NameBin = extract_attr_name(NameAST),
    case is_static_binary(ValueAST) of
        true ->
            ValBin = extract_binary_value(ValueAST),
            buf_append(CS0, <<" ", NameBin/binary, "=\"", ValBin/binary, "\"">>);
        false when CS0#cs.nodiff ->
            Module = CS0#cs.module,
            DynAST = make_nodiff_attr_dynamic_ast(NameBin, ValueAST, Module, line(ValueAST)),
            flush(CS0, DynAST);
        false ->
            Module = CS0#cs.module,
            AzBin = integer_to_binary(ElemAz),
            DynAST = make_attr_dynamic_ast(AzBin, NameBin, ValueAST, Module, line(ValueAST)),
            flush(CS0, DynAST)
    end;
compile_attr({atom, _, Name}, _ElemAz, CS0, _ElemLine) ->
    NameBin = atom_to_html_binary(Name),
    buf_append(CS0, <<" ", NameBin/binary>>);
compile_attr(Attr, _ElemAz, _CS0, ElemLine) ->
    AttrLine =
        try
            line(Attr)
        catch
            _:_ -> ElemLine
        end,
    parse_error(invalid_attribute, AttrLine).

%% --- Children Compilation ----------------------------------------------------

compile_children(Children, ElemAz, CS) ->
    compile_children(Children, ElemAz, CS, 0).

compile_children([], _ElemAz, CS, _Slot) ->
    CS;
compile_children([Child | Rest], ElemAz, CS0, Slot) ->
    {CS1, NextSlot} = compile_child(Child, ElemAz, CS0, Slot),
    compile_children(Rest, ElemAz, CS1, NextSlot).

compile_child(Child, ElemAz, CS0, Slot) ->
    case is_static_binary(Child) of
        true ->
            {buf_append(CS0, extract_binary_value(Child)), Slot};
        false ->
            case is_element_tuple(Child) of
                true ->
                    {Tag, Attrs, Children, ElemLine} = extract_element(Child),
                    {compile_element(Tag, Attrs, Children, ElemLine, CS0), Slot};
                false ->
                    case is_invalid_static_child(Child) of
                        true ->
                            ValueStr = erl_pp:expr(Child),
                            parse_error({invalid_child, ValueStr}, line(Child));
                        false when CS0#cs.nodiff ->
                            Module = CS0#cs.module,
                            DynAST = make_nodiff_dynamic_ast(Child, Module, line(Child)),
                            {flush(CS0, DynAST), Slot};
                        false ->
                            Module = CS0#cs.module,
                            ElemAzBin = integer_to_binary(ElemAz),
                            MarkerAz =
                                case Slot of
                                    0 -> ElemAzBin;
                                    N -> <<ElemAzBin/binary, ":", (integer_to_binary(N))/binary>>
                                end,
                            CS1 = buf_append(CS0, <<"<!--az:", MarkerAz/binary, "-->">>),
                            DynAST = make_text_dynamic_ast(MarkerAz, Child, Module, line(Child)),
                            CS2 = flush(CS1, DynAST),
                            {CS2#cs{buf = <<"<!--/az-->">>}, Slot + 1}
                    end
            end
    end.

%% --- Dynamic AST Construction ------------------------------------------------

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

%% --- Compile State Operations ------------------------------------------------

buf_append(CS, Bin) ->
    CS#cs{buf = <<(CS#cs.buf)/binary, Bin/binary>>}.

flush(CS, DynAST) ->
    CS#cs{
        statics = CS#cs.statics ++ [CS#cs.buf],
        dynamics = CS#cs.dynamics ++ [DynAST],
        buf = <<>>
    }.

finalize(CS) ->
    Statics = CS#cs.statics ++ [CS#cs.buf],
    Dynamics = CS#cs.dynamics,
    {Statics, Dynamics}.

%% --- Az Scoping --------------------------------------------------------------
%% Prefix az values with the template fingerprint to prevent collisions when
%% stateless children are inlined in a parent template.

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

%% --- Fingerprint -------------------------------------------------------------

%% Base-36 encoding of phash2 for compact wire representation (max 6 chars).
generate_fingerprint(Statics) ->
    Hash = erlang:phash2(Statics),
    integer_to_binary(Hash, 36).

%% --- Fun Body Helpers --------------------------------------------------------

split_fun_body([Last]) ->
    {[], Last};
split_fun_body([H | T]) ->
    {Rest, Last} = split_fun_body(T),
    {[H | Rest], Last}.

%% --- Output AST Construction ------------------------------------------------

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

%% --- AST Helpers -------------------------------------------------------------

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

%% --- Value Extraction --------------------------------------------------------

extract_attr_name({atom, _, Name}) -> atom_to_html_binary(Name);
extract_attr_name(BinAST) -> extract_binary_value(BinAST).

extract_binary_value({bin, _, Elements}) ->
    iolist_to_binary([extract_bin_element(E) || E <:- Elements]).

extract_bin_element({bin_element, _, {string, _, Chars}, _, _}) ->
    unicode:characters_to_binary(Chars);
extract_bin_element({bin_element, _, {integer, _, N}, _, _}) ->
    <<N/utf8>>.

%% --- Static Detection --------------------------------------------------------

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

%% --- Dynamic Detection -------------------------------------------------------

has_dynamic_attr([]) ->
    false;
has_dynamic_attr([{bin, _, _} | Rest]) ->
    has_dynamic_attr(Rest);
has_dynamic_attr([{tuple, _, [_, {atom, _, Val}]} | Rest]) when
    Val =:= true; Val =:= false
->
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

%% --- Static Child Validation -------------------------------------------------

is_invalid_static_child({tuple, _, _}) -> true;
is_invalid_static_child(_) -> false.

%% --- Directive Extraction ----------------------------------------------------

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

%% --- Template Options --------------------------------------------------------

opts_to_map_fields(Opts, Line) ->
    maps:fold(
        fun(K, V, Acc) ->
            [{map_field_assoc, Line, {atom, Line, K}, {atom, Line, V}} | Acc]
        end,
        [],
        Opts
    ).

%% --- Atom to HTML Binary -----------------------------------------------------

atom_to_html_binary(Atom) ->
    binary:replace(atom_to_binary(Atom), <<"_">>, <<"-">>, [global]).

%% --- Void Elements -----------------------------------------------------------

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
