-module(arizona_template_compiler).
-moduledoc false.

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([compile/3]).

%

-ignore_xref([compile/3]).

%% --------------------------------------------------------------------
%% Types (and their exports)
%% --------------------------------------------------------------------

-record(state, {
    module :: module(),
    function :: atom(),
    macros :: macros(),
    attributes :: [arizona_template_parser:attribute()],
    index :: non_neg_integer(),
    path :: [non_neg_integer()]
}).

-type macros() :: #{atom() := arizona_html:safe_type()}.
-export_type([macros/0]).

-opaque changeable() :: {expr, expr()} | {block, block()}.
-export_type([changeable/0]).

-opaque expr() :: #{
    id := [non_neg_integer()],
    function := fun((Assigns :: map()) -> arizona_html:safe_type()),
    vars := [atom()]
}.
-export_type([expr/0]).

-opaque block() :: #{
    id => [non_neg_integer()],
    module => module(),
    function => atom(),
    static => [binary()],
    changeable => #{non_neg_integer() := changeable()},
    changeable_vars => #{atom() := [[non_neg_integer()]]},
    changeable_indexes => [non_neg_integer()],
    norm_assigns => #{atom() := {text, binary()} | {expr, expr()}},
    norm_assigns_vars => [{atom(), [non_neg_integer()]}]
}.
-export_type([block/0]).

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-spec compile(Mod, Fun, Macros) -> Result
    when Mod :: module(),
         Fun :: atom(),
         Macros :: map(),
         Result :: {ok, Block}
                 | {error, {ErrReason, {ErrMod, ErrFun, ErrLoc}}},
         Block :: block(),
         ErrReason :: unexpected_stateful_tag
                    | undef_block_fun
                    | invalid_block_name
                    | invalid_macro_value,
         ErrMod :: module(),
         ErrFun :: atom(),
         ErrLoc :: arizona_template_scanner:location().
compile(Mod, Fun, Macros) ->
    try
        State = #state{
            module = Mod,
            function = Fun,
            macros = Macros,
            attributes = [],
            index = 0,
            path = []
        },
        Tree = expand_block(Mod, Fun, _Attrs = [], State),
        {ok, block(Tree, _NormAssigns = #{}, State)}
    catch
        % That's not the location in the module but the location
        % in the template starting from {1, 1}.
        throw:{ErrReason, ErrLoc, ErrState} ->
            ErrMod = ErrState#state.module,
            ErrFun = ErrState#state.function,
            {error, {ErrReason, {ErrMod, ErrFun, ErrLoc}}}
    end.

%% --------------------------------------------------------------------
%% Private
%% --------------------------------------------------------------------

compile_elems([{text, _Loc, Txt} | T], State) ->
    compile_text(Txt, T, State);
compile_elems([{expr, Loc, Expr} | T], State) ->
    compile_expr(Expr, Loc, T, State);
compile_elems([{block, Loc, Block} | T], State) ->
    compile_block(Block, Loc, T, State);
compile_elems([{tag, Loc, #{is_stateful := true}} | _T], State) ->
    throw({unexpected_stateful_tag, Loc, State});
compile_elems([{tag, Loc, Tag} | T], State) ->
    compile_tag(Tag, Loc, T, State);
compile_elems([], _State) ->
    [].

compile_text(Txt, T, State) ->
    [{text, Txt} | compile_elems(T, State)].

compile_expr(Expr, Loc, T, State) ->
    Elem = expand_expr(Expr, Loc, true, State),
    NIndex = case Elem of {text, _} -> 0; {expr, _} -> 1 end,
    [Elem | compile_elems(T, incr_index(NIndex, State))].

expand_expr(Expr, Loc, Eval, State) ->
    ExprTree = merl:quote(Expr),
    AttrsEnv = lists:map(fun
        ({K, {expr, _KLoc, KExpr}}) ->
            {binary_to_existing_atom(K, utf8), merl:quote(KExpr)};
        ({K, {text, _KLoc, KTxt}}) ->
            {binary_to_existing_atom(K, utf8),
              merl:quote(iolist_to_binary(["<<\"", KTxt, "\"/utf8>>"]))}
    end, State#state.attributes),
    AttrsTree = merl:tsubst(ExprTree, AttrsEnv),
    Macros = State#state.macros,
    MacrosEnv = [{K, merl:term(V)} || K := V <- Macros],
    MacrosTree = merl:tsubst(AttrsTree, MacrosEnv),
    AllVars = merl:template_vars(merl:template(MacrosTree)),
    case AllVars -- maps:keys(Macros) of
        [] when Eval ->
            try
                Form = erl_syntax:revert(merl:tree(MacrosTree)),
                {value, Value, _} = erl_eval:exprs([Form], []),
                {text, arizona_html:to_safe(Value)}
            catch
                _:_ ->
                    throw({invalid_macro_value, Loc, State})
            end;
        Vars ->
            VarsSubst = [{Var, var_form(Var)} || Var <- Vars],
            Env = [{subst, merl:subst(MacrosTree, VarsSubst)}],
            [Form] = erl_syntax:revert_forms([merl:qquote(~"_@subst", Env)]),
            NormExpr = iolist_to_binary(erl_pp:expr(Form)),
            Forms = [merl:quote(<<"fun(Assigns) -> ", NormExpr/binary, " end">>)],
            {value, Fun, _} = erl_eval:exprs(Forms, []),
            {expr, expr(Fun, Vars, State)}
    end.

var_form(Var) ->
    VarName = atom_to_binary(Var, utf8),
    merl:quote(<<"maps:get(", VarName/binary, ", Assigns)">>).

expr(Fun, Vars, State) ->
    #{
        id => changeable_id(State),
        function => Fun,
        vars => Vars
     }.

compile_block(#{name := Name} = Block, Loc, T, State) ->
    case block_mod_fun(Name, State) of
        {ok, {Mod, Fun}} ->
            Attrs = maps:get(attributes, Block),
            Elems = expand_block(Mod, Fun, Attrs, State),
            NIndex = changeable_count(Elems, 0),
            Elems ++ compile_elems(T, incr_index(NIndex, State));
        {error, Reason} ->
            throw({Reason, Loc, State})
    end.

block_mod_fun(Name, State) ->
    try
        {Mod, Fun} = case binary:split(Name, <<":">>) of
            [M, F] ->
                {binary_to_existing_atom(M, utf8), binary_to_existing_atom(F, utf8)};
            [F] ->
                {State#state.module, binary_to_existing_atom(F, utf8)}
        end,
        case erlang:function_exported(Mod, Fun, 1) of
            true ->
                {ok, {Mod, Fun}};
            false ->
                {error, undef_block_fun}
        end
    catch
        _:_ ->
            {error, invalid_block_name}
    end.

expand_block(Mod, Fun, Attrs, State) ->
    case erlang:apply(Mod, Fun, [State#state.macros]) of
        {ok, {[{tag, Loc, #{is_stateful := true} = Tag}], Macros}} ->
            NormAssigns = norm_block_assigns(Attrs, Macros, State),
            StatefulState = stateful_state(Mod, Fun, Macros, State),
            Tree = compile_tag(Tag, Loc, [], StatefulState),
            [{block, block(Tree, NormAssigns, State)}];
        {ok, {Elems, Macros}} ->
            concat_texts(compile_elems(Elems, stateless_state(Macros, Attrs, State)));
        {error, {Reason, Loc}} ->
            throw({Reason, Loc, State})
    end.

norm_block_assigns(Attrs, Macros, State) ->
    maps:from_list(lists:map(fun
        ({K, {text, _, Txt}}) ->
            {binary_to_existing_atom(K), {text, Txt}};
        ({K, {expr, Loc, Expr}}) ->
            Eval = should_eval_expr(Expr, Macros),
            {binary_to_existing_atom(K), expand_expr(Expr, Loc, Eval, State)}
    end, Attrs)).

should_eval_expr(Expr, Macros) ->
    ExprTree = merl:quote(Expr),
    ExprVars = merl:template_vars(merl:template(ExprTree)),
    ExprVars =/= [] andalso map_size(Macros) > 0 andalso
        lists:all(fun(Var) -> is_map_key(Var, Macros) end, ExprVars).

block(Tree0, NormAssigns, State) ->
    Tree = concat_texts(Tree0),
    Changeable = normalize_changeable(filter_changeable(Tree)),
    ChangeableVars = changeable_vars(Changeable),
    #{
        id => changeable_id(State),
        module => State#state.module,
        function => State#state.function,
        static => filter_static(Tree),
        changeable => Changeable,
        changeable_vars => group_vars(ChangeableVars),
        changeable_indexes => changeable_indexes(Changeable),
        norm_assigns => NormAssigns,
        norm_assigns_vars => norm_assigns_vars(NormAssigns, ChangeableVars)
    }.

group_vars(Vars) ->
    maps:groups_from_list(
        fun({Var, _}) -> Var end,
        fun({_, Path}) -> Path end,
        Vars).

compile_tag(#{name := Name} = Tag, Loc, T, State) ->
    Attrs = maps:get(attributes, Tag),
    [{text, <<$<, Name/binary>>} | compile_tag_attrs(Attrs, Tag, Loc, T, State)].

compile_tag_attrs([{Name, {text, _, Value}} | Attrs], Tag, Loc, T, State) ->
    [{text, <<$\s, Name/binary, $=, $", Value/binary, $">>}
     | compile_tag_attrs(Attrs, Tag, Loc, T, State)];
compile_tag_attrs([{Name, {expr, ExprLoc, Expr}} | Attrs], Tag, Loc, T, State) ->
    Elem = expand_expr(Expr, ExprLoc, true, State),
    NIndex = case Elem of {text, _} -> 0; {expr, _} -> 1 end,
    [{text, <<$\s, Name/binary, $=, $">>}, Elem, {text, <<$">>}
     | compile_tag_attrs(Attrs, Tag, Loc, T, incr_index(NIndex, State))];
compile_tag_attrs([], #{is_void := true}, _Loc, T, State) ->
    [{text, <<$\s, $/, $>>>} | compile_elems(T, State)];
compile_tag_attrs([], #{is_void := false} = Tag, _Loc, T, State) ->
    InnerContent = maps:get(inner_content, Tag),
    [{text, <<$>>>} | compile_tag_inner_content(InnerContent, Tag, T, State)].

compile_tag_inner_content(InnerContent, Tag, T, State) ->
    Elems = compile_elems(InnerContent, State),
    NIndex = changeable_count(Elems, 0),
    Elems ++ compile_tag_closing(Tag, T, incr_index(NIndex, State)).

compile_tag_closing(#{name :=  Name}, T, State) ->
    [{text, <<$<, $/, Name/binary, $>>>} | compile_elems(T, State)].

concat_texts([{text, TxtA}, {text, TxtB} | T]) ->
    concat_texts([{text, <<TxtA/binary, TxtB/binary>>} | T]);
concat_texts([H | T])  ->
    [H | concat_texts(T)];
concat_texts([]) ->
    [].

filter_static([{text, Txt} | T]) ->
    [Txt | filter_static(T)];
filter_static([_, {text, Txt} | T]) ->
    [Txt | filter_static(T)];
filter_static([_, _ | T]) ->
    [<<>> | filter_static(T)];
filter_static([]) ->
    [].

filter_changeable([{text, _} | T]) ->
    filter_changeable(T);
filter_changeable([Changeable | T]) ->
    [Changeable | filter_changeable(T)];
filter_changeable([]) ->
    [].

normalize_changeable(Changeable) ->
    maps:from_list(lists:map(fun
        ({Kind, #{id := Id} = Elem}) ->
            {lists:last(Id), {Kind, Elem}}
    end, Changeable)).

changeable_id(State) ->
    changeable_id(State#state.index, State#state.path).

changeable_id(Index, Path) ->
    lists:reverse([Index | Path]).

changeable_indexes(Changeable) ->
    lists:usort(maps:keys(Changeable)).

changeable_count([{text, _} | T], Count) ->
    changeable_count(T, Count);
changeable_count([_ | T], Count) ->
    changeable_count(T, Count + 1);
changeable_count([], Count) ->
    Count.

changeable_vars(Changeable) ->
    changeable_vars_1(maps:to_list(Changeable)).

changeable_vars_1([{_Index, {expr, #{id := Id, vars := Vars}}} | T]) ->
    changeable_vars_2(Vars, Id, T);
changeable_vars_1([{_Index, {block, #{norm_assigns_vars := Vars}}} | T]) ->
    Vars ++ changeable_vars_1(T);
changeable_vars_1([_ | T]) ->
    changeable_vars_1(T);
changeable_vars_1([]) ->
    [].

changeable_vars_2([Var | Vars], Id, T) ->
    [{Var, Id} | changeable_vars_2(Vars, Id, T)];
changeable_vars_2([], _, T) ->
    changeable_vars_1(T).

norm_assigns_vars(NormAssigns, ChangeableVars) ->
    norm_assigns_vars_1(maps:to_list(NormAssigns), ChangeableVars).

norm_assigns_vars_1([{ChangeableVar, {expr, #{vars := Vars}}} | T], ChangeableVars) ->
    Indexes = proplists:get_all_values(ChangeableVar, ChangeableVars),
    norm_assigns_vars_2(Vars, ChangeableVar, Indexes, T, ChangeableVars);
norm_assigns_vars_1([_ | T], ChangeableVars) ->
    norm_assigns_vars_1(T, ChangeableVars);
norm_assigns_vars_1([], _) ->
    [].

norm_assigns_vars_2([Var | Vars], ChangeableVar, Indexes, T, ChangeableVars) ->
    norm_assigns_vars_3(Indexes, Var, Vars, ChangeableVar, Indexes, T, ChangeableVars);
norm_assigns_vars_2([], _, _, T, ChangeableVars) ->
    norm_assigns_vars_1(T, ChangeableVars).

norm_assigns_vars_3([Index | Indexes], Var, Vars, ChangeableVar, AllIndexes, T, ChangeableVars) ->
    [{Var, Index}
     | norm_assigns_vars_3(Indexes, Var, Vars, ChangeableVar, AllIndexes, T, ChangeableVars)];
norm_assigns_vars_3([], _, Vars, ChangeableVar, Indexes, T, ChangeableVars) ->
    norm_assigns_vars_2(Vars, ChangeableVar, Indexes, T, ChangeableVars).

stateful_state(Mod, Fun, Macros, State) ->
    State#state{
        module = Mod,
        function = Fun,
        macros = Macros,
        index = 0,
        path = [State#state.index | State#state.path]
    }.

stateless_state(Macros, Attrs, State) ->
    State#state{
        macros = maps:merge(State#state.macros, Macros),
        attributes = Attrs
    }.

incr_index(N, #state{index = Index} = State) ->
    State#state{index = Index + N}.

%% --------------------------------------------------------------------
%% EUnit
%% --------------------------------------------------------------------

-ifdef(TEST).
-compile([export_all, nowarn_export_all]).
-include_lib("eunit/include/eunit.hrl").

compile_test() ->
    ?assertMatch({ok,
         #{function := render, id := [0], module := arizona_template_compiler,
           changeable :=
            #{0 :=
               {block,
                #{function := render, id := [0], module := arizona_template_compiler,
                  changeable :=
                   #{0 :=
                      {expr, #{function := _, id := [0, 0], vars := [count]}},
                     1 :=
                      {expr, #{function := _, id := [0, 1], vars := [btn_text]}}},
                  static :=
                   [<<"<div><span>Count:">>,
                    <<"</span><button type=\"button\" "
                      "onclick=\"arizona.send.bind(this)('incr')\">">>,
                    <<"</button></div>">>],
                  changeable_vars := #{count := [[0, 0]], btn_text := [[0, 1]]},
                  norm_assigns_vars := [{count, [0, 0]}],
                  changeable_indexes := [0, 1],
                  norm_assigns :=
                   #{count := {expr, #{function := _, id := [0], vars := [count]}},
                     btn_text := {text, <<"Increment">>}}}},
              1 :=
               {block,
                #{function := render, id := [1], module := arizona_template_compiler,
                  changeable :=
                   #{0 :=
                      {expr, #{function := _, id := [1, 0], vars := [count]}},
                     1 :=
                      {expr, #{function := _, id := [1, 1], vars := [btn_text]}}},
                  static :=
                   [<<"<div><span>Count:">>,
                    <<"</span><button type=\"button\" "
                      "onclick=\"arizona.send.bind(this)('incr')\">">>,
                    <<"</button></div>">>],
                  changeable_vars :=
                   #{count := [[1, 0]], btn_text := [[1, 1]]},
                  norm_assigns_vars := [],
                  changeable_indexes := [0, 1],
                  norm_assigns :=
                   #{count := {expr, #{function := _, id := [1], vars := []}},
                     btn_text := {text, <<"Increment #2">>}}}}},
           static :=
            [<<"<!DOCTYPE html=\"html\" />"
               "<html lang=\"en\">"
               "<head><meta charset=\"UTF-8\" />"
               "<title>Arizona Framework</title>"
               "<script src=\"assets/js/main.js\"></script>"
               "</head><body><h1>Arizona Counter</h1>">>,
             <<>>, <<"</body></html>">>],
           changeable_vars := #{count := [[0, 0]]},
           norm_assigns_vars := [],
           changeable_indexes := [0, 1],
           norm_assigns := #{}}}
    , compile(?MODULE, render, #{
        title => ~"Arizona Framework",
        inc_btn_text => ~"Increment #2"})).

%% --------------------------------------------------------------------
%% Test support
%% --------------------------------------------------------------------

render(Macros) ->
    maybe_parse(~"""
    <!DOCTYPE html>
    <html lang="en">
    <head>
        <meta charset="UTF-8">
        <title>{_@title}</title>
        <script src="assets/js/main.js"></script>
    </head>
    <body>
        <h1>Arizona Counter</h1>
        <.counter
            count={_@count}
            btn_text="Increment"
        />
        <.counter
            count={88}
            btn_text={_@inc_btn_text}
        />
    </body>
    </html>
    """, Macros).

counter(Macros) ->
    maybe_parse(~"""
    <div :stateful>
        <span>Count: {_@count}</span>
        <.button event="incr" text={_@btn_text} />
    </div>
    """, Macros).

button(Macros) ->
    maybe_parse(~"""
    <button type="button" :onclick={arizona_js:send(_@event)}>
        {_@text}
    </button>
    """, Macros).

maybe_parse(Template, Macros) ->
    maybe
        {ok, Tokens} ?= arizona_template_scanner:scan(Template),
        {ok, Elems} ?= arizona_template_parser:parse(Tokens),
        {ok, {Elems, Macros}}
    else
        {error, Reason} ->
            {error, Reason}
    end.

-endif.
