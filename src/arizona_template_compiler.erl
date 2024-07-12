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
    macros :: assigns(),
    attributes :: [arizona_template_parser:attribute()],
    index :: non_neg_integer(),
    path :: [non_neg_integer()]
}).

-type assigns() :: #{atom() := term()}.
-export_type([assigns/0]).

-opaque changeable() :: {expr, expr()} | {block, block()}.
-export_type([changeable/0]).

-opaque expr() :: #{
    id := [non_neg_integer()],
    function := fun((assigns()) -> arizona_html:safe_type()),
    vars := [atom()]
}.
-export_type([expr/0]).

-opaque block() :: #{
    id := [non_neg_integer()],
    module := module(),
    function := atom(),
    static := [binary()],
    changeable := #{non_neg_integer() := changeable()},
    changeable_vars := #{atom() := [[non_neg_integer()]]},
    changeable_indexes := [non_neg_integer()],
    norm_assigns := #{atom() := #{
        function := fun((assigns()) -> term()),
        vars := [atom()]
    }},
    norm_assigns_vars := [{atom(), [non_neg_integer()]}]
}.
-export_type([block/0]).

%

-elvis([{elvis_style, max_module_length, disable}]).

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

compile_expr(ExprStr, Loc, T, State) ->
    case expand_expr(ExprStr, Loc, true, State) of
        {text, Txt} ->
            [{text, Txt} | compile_elems(T, State)];
        {expr, Expr} ->
            [{expr, Expr} | compile_elems(T, incr_index(1, State))]
    end.

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
    {Mod, Fun} = block_mod_fun(Name, Loc, State),
    case erlang:function_exported(Mod, Fun, 1) of
        true ->
            Attrs = maps:get(attributes, Block),
            Elems = expand_block(Mod, Fun, Attrs, State),
            Elems ++ compile_elems(T, incr_index(changeable_count(Elems, 0), State));
        false ->
            throw({undef_block_fun, Loc, State})
    end.

block_mod_fun(Name, Loc, State) ->
    try
        case binary:split(Name, <<":">>) of
            [M, F] ->
                {binary_to_existing_atom(M, utf8), binary_to_existing_atom(F, utf8)};
            [F] ->
                {State#state.module, binary_to_existing_atom(F, utf8)}
        end
    catch
        _:_ ->
            throw({invalid_block_name, Loc, State})
    end.

expand_block(Mod, Fun, Attrs, State) ->
    case erlang:apply(Mod, Fun, [State#state.macros]) of
        {ok, {[{tag, Loc, #{is_stateful := true} = Tag}], AttrsMacros}} ->
            BlockAssigns = block_assigns(Attrs, AttrsMacros, State),
            Macros = block_assigns_macros(BlockAssigns),
            NormAssigns = norm_block_assigns(BlockAssigns),
            StatefulState = stateful_state(Mod, Fun, Macros, State),
            Tree = compile_tag(Tag, Loc, [], StatefulState),
            [{block, block(Tree, NormAssigns, State)}];
        {ok, {Elems, Macros}} ->
            concat_texts(compile_elems(Elems, stateless_state(Macros, Attrs, State)));
        {error, {Reason, Loc}} ->
            throw({Reason, Loc, State})
    end.

block_assigns(Attrs, Macros, State) ->
    maps:from_list(lists:map(fun
        ({K, {text, _, Txt}}) ->
            {binary_to_existing_atom(K), {text, Txt}};
        ({K, {expr, Loc, Expr}}) ->
            Eval = should_eval_expr(Expr, Macros),
            {binary_to_existing_atom(K), expand_expr(Expr, Loc, Eval, State)}
    end, Attrs)).

block_assigns_macros(Assigns) ->
    #{K => Txt || K := {text, Txt} <- Assigns}.

norm_block_assigns(Assigns) ->
    #{K => maps:with([function, vars], Expr) || K := {expr, Expr} <- Assigns}.

should_eval_expr(Expr, Macros) when map_size(Macros) > 0 ->
    ExprTree = merl:quote(Expr),
    ExprVars = merl:template_vars(merl:template(ExprTree)),
    ExprVars =/= [] andalso
        lists:all(fun(Var) -> is_map_key(Var, Macros) end, ExprVars);
should_eval_expr(_, _) ->
    false.

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
    Attrs = get_tag_attrs(Tag, State#state.path),
    [{text, <<$<, Name/binary>>} | compile_tag_attrs(Attrs, Tag, Loc, T, State)].

get_tag_attrs(Tag, BlockId) ->
    Attrs0 = maps:get(attributes, Tag),
    Attrs = maybe_push_target_attr(Attrs0, BlockId),
    maybe_push_tag_id_attr(Tag, Attrs, BlockId).

maybe_push_target_attr(Attrs, BlockId) ->
    case attrs_contains_action(Attrs) andalso not attrs_contains_target(Attrs) of
        true ->
            EncodedBlockId = iolist_to_binary(json:encode(BlockId)),
            push_text_attr(~"arizona-target", EncodedBlockId, Attrs);
        false ->
            Attrs
    end.

attrs_contains_action(Attrs) ->
    lists:any(fun({<<"on", _/binary>>, _}) -> true; (_) -> false end, Attrs).

attrs_contains_target(Attrs) ->
    lists:any(fun({Name, _}) -> Name =:= ~"arizona-target" end, Attrs).

maybe_push_tag_id_attr(#{is_stateful := true}, Attrs, BlockId) ->
    EncodedBlockId = iolist_to_binary(json:encode(BlockId)),
    push_text_attr(~"arizona-id", EncodedBlockId, Attrs);
maybe_push_tag_id_attr(#{is_stateful := false}, Attrs, _) ->
    Attrs.

push_text_attr(Name, Txt, Attrs) ->
    [arizona_template_parser:dummy_text_attribute(Name, Txt) | Attrs].

compile_tag_attrs([{Name, {text, _, Value}} | Attrs], Tag, Loc, T, State) ->
    [{text, <<$\s, Name/binary, $=, $", Value/binary, $">>}
     | compile_tag_attrs(Attrs, Tag, Loc, T, State)];
compile_tag_attrs([{Name, {expr, ExprLoc, Expr}} | Attrs], Tag, Loc, T, State) ->
    Elem = expand_expr(Expr, ExprLoc, true, State),
    ChangeableCount = case Elem of {text, _} -> 0; {expr, _} -> 1 end,
    [{text, <<$\s, Name/binary, $=, $">>}, Elem, {text, <<$">>}
     | compile_tag_attrs(Attrs, Tag, Loc, T, incr_index(ChangeableCount, State))];
compile_tag_attrs([], #{is_void := true}, _Loc, T, State) ->
    [{text, <<$\s, $/, $>>>} | compile_elems(T, State)];
compile_tag_attrs([], #{is_void := false} = Tag, _Loc, T, State) ->
    InnerContent = maps:get(inner_content, Tag),
    [{text, <<$>>>} | compile_tag_inner_content(InnerContent, Tag, T, State)].

compile_tag_inner_content(InnerContent, #{name := Name} = Tag, T, State) ->
    Elems = compile_elems(maybe_push_js_scripts(Name, InnerContent), State),
    Elems ++ compile_tag_closing(Tag, T, incr_index(changeable_count(Elems, 0), State)).

maybe_push_js_scripts(<<"head">>, InnerContent) ->
    push_js_scripts(InnerContent);
maybe_push_js_scripts(_, InnerContent) ->
    InnerContent.

push_js_scripts([{tag, _, #{name := ~"script"}} | _] = T) ->
    js_scripts() ++ T;
push_js_scripts([H | T]) ->
    [H | push_js_scripts(T)];
push_js_scripts([]) ->
    js_scripts().

js_scripts() ->
    Scripts = case arizona_cfg:endpoint() of
        #{live_reload := true} ->
            [live_reload_script() | required_js_script()];
        #{} ->
            required_js_script()
    end,
    [arizona_template_parser:dummy_script_tag(Script) || Script <- Scripts].

required_js_script() ->
    [~"assets/js/morphdom.min.js", ~"assets/js/arizona.js"].

live_reload_script() ->
    ~"assets/js/arizona-live-reload.js".

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
filter_static([_ | T]) ->
    filter_static(T);
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

norm_assigns_vars_1([{ChangeableVar, #{vars := Vars}} | T], ChangeableVars) ->
    Indexes = proplists:get_all_values(ChangeableVar, ChangeableVars),
    norm_assigns_vars_2(Vars, ChangeableVar, Indexes, T, ChangeableVars);
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
                      {expr, #{function := _, id := [0, 0], vars := [count]}}},
                  static :=
                   [<<"<div arizona-id=\"[0]\"><span>Count:">>,
                    <<"</span><button arizona-target=\"[0]\" type=\"button\" "
                      "onclick=\"arizona.send.bind(this)('incr')\">",
                     "Increment</button></div>">>],
                  changeable_vars := #{count := [[0, 0]]},
                  norm_assigns_vars := [{count, [0, 0]}],
                  changeable_indexes := [0],
                  norm_assigns :=
                   #{count := #{function := _, vars := [count]}}}},
              1 :=
               {block,
                #{function := render, id := [1], module := arizona_template_compiler,
                  changeable :=
                   #{0 :=
                      {expr, #{function := _, id := [1, 0], vars := [count]}}},
                  static :=
                   [<<"<div arizona-id=\"[1]\"><span>Count:">>,
                    <<"</span><button arizona-target=\"[1]\" type=\"button\" "
                      "onclick=\"arizona.send.bind(this)('incr')\">"
                      "Increment #2</button></div>">>],
                  changeable_vars := #{count := [[1, 0]]},
                  norm_assigns_vars := [],
                  changeable_indexes := [0],
                  norm_assigns :=
                   #{count := #{function := _, vars := []}}}}},
           static :=
            [<<"<!DOCTYPE html=\"html\" />"
               "<html lang=\"en\">"
               "<head><meta charset=\"UTF-8\" />"
               "<title>Arizona Framework</title>"
               "<script src=\"assets/js/morphdom.min.js\"></script>"
               "<script src=\"assets/js/arizona.js\"></script>"
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

unexpected_stateful_tag_test() ->
    [
        ?assertEqual({error, {unexpected_stateful_tag,
                              {?MODULE, render_unexpected_stateful_tag_1, {2, 1}}}},
                     compile(?MODULE, render_unexpected_stateful_tag_1, #{})),
        ?assertEqual({error, {unexpected_stateful_tag,
                              {?MODULE, render_unexpected_stateful_tag_2, {2, 5}}}},
                     compile(?MODULE, render_unexpected_stateful_tag_2, #{}))
    ].

undef_block_fun_test() ->
    [
        ?assertEqual({error, {undef_block_fun, {?MODULE, render_undef_block_fun_1, {1, 1}}}},
                     compile(?MODULE, render_undef_block_fun_1, #{})),
        ?assertEqual({error, {undef_block_fun, {?MODULE, render_undef_block_fun_2, {1, 1}}}},
                     compile(?MODULE, render_undef_block_fun_2, #{}))
    ].

invalid_block_name_test() ->
    ?assertEqual({error, {invalid_block_name, {?MODULE, render_invalid_block_name, {1, 1}}}},
                 compile(?MODULE, render_invalid_block_name, #{})).

render_invalid_macro_value_test() ->
    ?assertEqual({error, {invalid_macro_value, {?MODULE, render_invalid_macro_value, {1, 1}}}},
                 compile(?MODULE, render_invalid_macro_value, #{
                    foo => [see_arizona_html_for_valid_types]})).

norm_assigns_test() ->
    {ok, #{changeable := Changeable}} = compile(?MODULE, render_norm_assigns, #{}),
    #{0 := {block, Block}} = Changeable,
    #{foo := #{function := Fun}} = maps:get(norm_assigns, Block),
    ?assertEqual(foo, Fun(#{bar => foo})).

macros_test() ->
    ?assertMatch(
        {ok,
         #{function := render_macros,
           id := [0],
           module := arizona_template_compiler,
           static := [<<"foo">>],
           changeable :=
            #{0 :=
               {block,
                #{function := render_macros,
                  id := [0],
                  module := arizona_template_compiler,
                  static := [<<"<div arizona-id=\"[0]\">foobaz">>,
                             <<"</div>">>],
                  changeable :=
                   #{0 :=
                      {expr, #{function := _, id := [0, 0], vars := [qux]}}},
                  changeable_vars := #{qux := [[0, 0]]},
                  changeable_indexes := [0],
                  norm_assigns :=
                   #{qux :=
                      #{function := _, vars := [qux]}},
                  norm_assigns_vars := [{qux, [0, 0]}]}}},
           changeable_vars := #{qux := [[0, 0]]},
           changeable_indexes := [0],
           norm_assigns := #{}, norm_assigns_vars := []}}
       , compile(?MODULE, render_macros, #{foo => foo})).

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

render_unexpected_stateful_tag_1(Macros) ->
    maybe_parse(~"""
    foo
    <div :stateful></div>
    """, Macros).

render_unexpected_stateful_tag_2(Macros) ->
    maybe_parse(~"""
    <div :stateful>
        <div :stateful></div>
    </div>
    """, Macros).

render_undef_block_fun_1(Macros) ->
    % Just to make binary_to_existing_atom happy :)
    _ = [unexistent_mod, unexistent_fun],
    maybe_parse(~"""
    <.unexistent_mod:unexistent_fun />
    """, Macros).

render_undef_block_fun_2(Macros) ->
    maybe_parse(~"""
    <.unexistent_fun />
    """, Macros).

render_invalid_block_name(Macros) ->
    maybe_parse(~"""
    <.this_is_an_unexistent_atom />
    """, Macros).

render_invalid_macro_value(Macros) ->
    maybe_parse(~"""
    {_@foo}
    """, Macros).

render_norm_assigns(Macros) ->
    maybe_parse(~"""
    {% _@bar is _@foo in the block, so it must be normalized. }
    <.render_norm_assigns_block foo={_@bar} />
    """, Macros).

render_norm_assigns_block(Macros) ->
    maybe_parse(~"""
    <div :stateful>{_@foo}</div>
    """, Macros).

render_macros(Macros) ->
    % Just to make binary_to_existing_atom happy :)
    _ = baz,
    maybe_parse(~"""
    {_@foo}
    <.render_macros_block bar={_@foo} baz="baz" qux={_@qux} />
    """, Macros).

render_macros_block(Macros) ->
    maybe_parse(~"""
    <div :stateful>
        {_@bar}{_@baz}{_@qux}
    </div>
    """, Macros).

% The below is what the arizona_live_view:parse_str should return.
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
