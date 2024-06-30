%%
%% %CopyrightBegin%
%%
%% Copyright 2024 William Fank Thomé
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%
-module(arizona_tpl_compile).
-moduledoc """
Template compiler.
""".
-moduledoc #{author => "William Fank Thomé <willilamthome@hotmail.com>"}.

%% API functions.
-export([compile/1]).

-record(state, {view, parent = root, macros = #{}, assign_fun_args = [~"Assigns"]}).

%% --------------------------------------------------------------------
%% API funtions.
%% --------------------------------------------------------------------

% NOTE: Multiple tags is not allowed for a root, e.g.:
%       Good: <html><head></head><body></body></html>
%        Bad: <head></head><body></body>
compile({Mod, Fun, Args}) when is_atom(Mod), is_atom(Fun) ->
    [{0, Block}] = compile([{block, #{
        module => Mod,
        function => Fun,
        args => Args,
        directives => #{stateful => true},
        attrs => []}}], [], 0, #state{view = Mod, macros = Args}),
    {ok, Block};
compile(Tree) ->
    [{0, Block}] = compile(Tree, [], 0, #state{}),
    {ok, Block}.

%% --------------------------------------------------------------------
%% Internal funtions.
%% --------------------------------------------------------------------

compile([{text, Txt} | T], P, I, State) ->
    [{I, text_struct(Txt, P, I)} | compile(T, P, I + 1, State)];
compile([{expr, Expr} | T], P, I, State) ->
    [{I, expr_struct(Expr, State#state.macros, State#state.assign_fun_args, P, I)}
     | compile(T, P, I + 1, State)];
compile([{tag, Tag} | T], P, I, State) ->
    compile_tag(Tag, <<>>, T, P, I, State);
compile([{block, Block} | T], P, I, State) ->
    [{I, block_struct(Block, P, I, State)} | compile(T, P, I + 1, State)];
compile([], _P, _I, _State) ->
    [].

% TODO: Create functions in favor of DRY.
compile_tag(#{directives := Directives} = Tag, Txt, T, P, I, State) ->
    case Directives of
        #{'for' := {expr, {ForExpr, _ForMacros}}, 'in' := {expr, {InExpr, InMacros}}} ->
            Tokens = compile_tag_1(Tag, Txt, T, P, I, State),
            case Directives of
                #{'if' := {expr, {IfExpr, IfMacros}}} ->
                    % TODO: Expand ForExpr.
                    FunArgs = [ForExpr | State#state.assign_fun_args],
                    DynamicTokens = compile(
                        filter_dynamic_tokens(Tokens), [I | P], 0,
                        tag_tokens_state(Tag, P, I, State#state{assign_fun_args = FunArgs})),
                    DynamicTokensMap = maps:from_list(DynamicTokens),
                    Indexes = lists:usort(maps:keys(DynamicTokensMap)),
                    [{I, {'for', expand(InExpr, InMacros, State#state.assign_fun_args),
                        {'if', expand(IfExpr, IfMacros, FunArgs)},
                        filter_static_tokens(Tokens),
                        {Indexes, DynamicTokensMap}
                    }} | compile(T, P, I + 1, State)];
                #{'case' := {expr, {CaseExpr, CaseMacros}},
                  'of' := {expr, {OfExpr, _OfMacros}}} ->
                    % TODO: Expand OfExpr first.
                    VarNameExpr = case erl_scan:string(binary_to_list(OfExpr)) of
                        {ok, [{atom, _, VarName} | _], _} ->
                            atom_to_binary(VarName);
                        {ok, [{var, _, VarName} | _], _} ->
                            atom_to_binary(VarName)
                    end,
                    Expr = <<"case ", CaseExpr/binary, " of ",
                        OfExpr/binary, " -> {true, ", VarNameExpr/binary, "}; "
                        "_ -> false "
                    "end">>,
                    % TODO: Expand ForExpr.
                    FunArgs = [ForExpr | State#state.assign_fun_args],
                    DynamicTokens = compile(
                        filter_dynamic_tokens(Tokens), [I | P], 0,
                        tag_tokens_state(Tag, P, I, State#state{assign_fun_args = [VarNameExpr | FunArgs]})),
                    DynamicTokensMap = maps:from_list(DynamicTokens),
                    Indexes = lists:usort(maps:keys(DynamicTokensMap)),
                    [{I, {'for', expand(InExpr, InMacros, State#state.assign_fun_args),
                        {'case', expand(Expr, CaseMacros, FunArgs)},
                        filter_static_tokens(Tokens),
                        {Indexes, DynamicTokensMap}
                    }} | compile(T, P, I + 1, State)];
                #{} ->
                    % TODO: Expand ForExpr.
                    FunArgs = [ForExpr | State#state.assign_fun_args],
                    DynamicTokens = compile(
                        filter_dynamic_tokens(Tokens), [I | P], 0,
                        tag_tokens_state(Tag, P, I, State#state{assign_fun_args = FunArgs})),
                    DynamicTokensMap = maps:from_list(DynamicTokens),
                    Indexes = lists:usort(maps:keys(DynamicTokensMap)),
                    [{I, {'for', expand(InExpr, InMacros, State#state.assign_fun_args),
                        nocond,
                        filter_static_tokens(Tokens),
                        {Indexes, DynamicTokensMap}
                    }} | compile(T, P, I + 1, State)]
            end;
        #{'case' := {expr, {CaseExpr, CaseMacros}}, 'of' := {expr, {OfExpr, _OfMacros}}} ->
            % TODO: Expand OfExpr first.
            VarNameExpr = case erl_scan:string(binary_to_list(OfExpr)) of
                {ok, [{atom, _, VarName} | _], _} ->
                    atom_to_binary(VarName);
                {ok, [{var, _, VarName} | _], _} ->
                    atom_to_binary(VarName)
            end,
            Expr = <<"case ", CaseExpr/binary, " of ",
                OfExpr/binary, " -> {true, ", VarNameExpr/binary, "}; "
                "_ -> false "
            "end">>,
            FunArgs = [VarNameExpr | State#state.assign_fun_args],
            Tokens = compile(compile_tag_1(Tag, Txt, T, P, I, State),
                            [I | P], 0, tag_tokens_state(Tag, P, I,
                                State#state{assign_fun_args = FunArgs})),
            TokensMap = maps:from_list(Tokens),
            [{I, {'case', expand(Expr, CaseMacros, State#state.assign_fun_args), #{
                tag => TokensMap,
                indexes => lists:usort(maps:keys(TokensMap))
            }}} | compile(T, P, I + 1, State)];
        #{'if' := {expr, {Expr, Macros}}} ->
            [{I, {'if', expand(Expr, Macros, State#state.assign_fun_args),
                tag_struct(Tag, Txt, T, P, I, State)
            }} | compile(T, P, I + 1, State)];
        #{} ->
            [{I, tag_struct(Tag, Txt, T, P, I, State)}
                | compile(T, P, I + 1, State)]
    end.

expand(ExprStr, {Macros, _Eval}, Args) ->
    Eval = false,
    Bindings = #{},
    ExprTree = merl:quote(ExprStr),
    MacrosEnv = [{K, merl:term(V)} || K := V <- Macros],
    MacrosTree = merl:tsubst(ExprTree, MacrosEnv),
    AllVars = merl:template_vars(merl:template(MacrosTree)),
    case AllVars -- maps:keys(Macros) of
        [] ->
            case Eval andalso
                 lists:member(erl_syntax:type(MacrosTree),
                              arizona_html:safe_types()) of
                true ->
                    {value, V, []} =
                        erl_eval:exprs(
                            [erl_syntax:revert(MacrosTree)], []),
                    {text, arizona_html:safe(V)};
                false ->
                    MacrosStr = iolist_to_binary(
                        erl_pp:expr(erl_syntax:revert(MacrosTree))),
                    eval(MacrosStr, [], Bindings, Args)
            end;
        Vars ->
            FunStr = <<"_@subst">>,
            VarsSubst = [{Var, subst_var(Var)} || Var <- Vars],
            Env = [{subst, merl:subst(MacrosTree, VarsSubst)}],
            [Tree] = erl_syntax:revert_forms([merl:qquote(FunStr, Env)]),
            TreeStr = iolist_to_binary(erl_pp:expr(Tree)),
            eval(TreeStr, Vars, Bindings, Args)
    end.

subst_var(Var) ->
    VarStr = atom_to_binary(Var, utf8),
    merl:quote(<<"maps:get(", VarStr/binary, ", Assigns)">>).

eval(ExprStr, Vars, Bindings, Args) ->
    FunStr = iolist_to_binary([~"fun([", lists:join(~", ", Args), ~"]) -> ", ExprStr, ~" end"]),
    Tree = [merl:quote(FunStr)],
    {value, Fun, _NewBindings} = erl_eval:exprs(Tree, Bindings),
    {expr, {Fun, Vars}}.

filter_static_tokens([{text, Txt} | T]) ->
    [Txt | filter_static_tokens(T)];
filter_static_tokens([_, {text, Txt} | T]) ->
    [Txt | filter_static_tokens(T)];
filter_static_tokens([_, _ | T]) ->
    [<<>> | filter_static_tokens(T)];
filter_static_tokens([_ | T]) ->
    filter_static_tokens(T);
filter_static_tokens([]) ->
    [].

filter_dynamic_tokens([{text, _} | T]) ->
    filter_dynamic_tokens(T);
filter_dynamic_tokens([Dynamic | T]) ->
    [Dynamic | filter_dynamic_tokens(T)];
filter_dynamic_tokens([]) ->
    [].

compile_tag_1(#{name := Name} = Tag, Txt, T, P, I, State) ->
    Attrs = norm_tag_attrs(Tag, P, State#state.parent),
    do_compile_tag(Attrs, Tag#{attrs => Attrs}, T, P, I, State, [Name, $<, Txt]).

norm_tag_attrs(#{attrs := Attrs0, directives := Dirs}, Id, Target) ->
    maps:fold(fun
        (stateful, true, Attrs) ->
            [{<<"arz-id">>, {text, arz_id(Id)}} | Attrs];
        (K, V, Attrs) ->
            case atom_to_binary(K, utf8) of
                <<"on", _/binary>> = Action ->
                    ActionAttr = case V of
                        {text, Event} ->
                            {Action, {text, arizona_js:send(Event)}};
                        Expr ->
                            {Action, Expr}
                    end,
                    case is_map_key(target, Dirs) of
                        true ->
                            [ActionAttr | Attrs];
                        false ->
                            [{<<"arz-target">>, {text, arz_id(Target)}},
                                ActionAttr | Attrs]
                    end;
                _ ->
                    Attrs
            end
    end, Attrs0, Dirs).

arz_id([]) ->
    <<"root">>;
arz_id([_]) ->
    <<"root">>;
arz_id(root) ->
    <<"root">>;
arz_id(P) ->
    [_ | Id] = lists:reverse(P),
    iolist_to_binary(json:encode(Id)).

do_compile_tag([{K, {expr, {Expr, Vars}}} | T], Tag, TT, P, I, State, Acc) ->
    [{text, iolist_to_binary(lists:reverse([$", $=, K, $\s | Acc]))},
      {expr, {Expr, Vars}}
        | do_compile_tag(T, Tag, TT, P, I + 2, State, [$"])];
do_compile_tag([{K, {text, Txt}} | T], Tag, TT, P, I, State, Acc) ->
    do_compile_tag(T, Tag, TT, P, I, State, [$", Txt, $", $=, K, $\s | Acc]);
do_compile_tag([K | T], Tag, TT, P, I, State, Acc) ->
    do_compile_tag(T, Tag, TT, P, I, State, [K, $\s | Acc]);
% IMPORTANT: Text concat must be reviewed when :if and :for be implemented.
%            Cannot concat when this kind of directive is defined.
do_compile_tag([], Tag, _TT, _P, _I, _State, Acc) ->
    case maps:get(void, Tag, false) of
        true ->
            [{text, tag_open(Tag, Acc)}];
        false ->
            [{text, tag_open(Tag, Acc)} | maps:get(tokens, Tag)] ++
                [{text, tag_closing(Tag)}]
    end.

is_stateful(#{directives := #{stateful := true}}) ->
    true;
is_stateful(#{}) ->
    false.

tag_tokens_state(Tag, P, I, State) ->
    case is_stateful(Tag) of
        true ->
            State#state{parent = id(P, I)};
        false ->
            State
    end.

tag_open(#{void := true} = Tag, Acc) ->
    case is_dtd(maps:get(name, Tag)) of
        true ->
            iolist_to_binary(lists:reverse([$> | Acc]));
        false ->
            iolist_to_binary(lists:reverse([$>, $/ | Acc]))
    end;
tag_open(#{void := false}, Acc) ->
    iolist_to_binary(lists:reverse([$> | Acc])).

tag_closing(#{name := Name}) ->
    <<$<, $/, Name/binary, $>>>.

id([], I) ->
    [I];
id(P, I) ->
    [0 | Id] = lists:reverse([I | P]),
    Id.

text_struct(Txt, P, I) ->
    #{
        id => id(P, I),
        text => Txt
     }.

% TODO: Marge all expr macros and user macros.
expr_struct({Expr, ExprMacros}, _Macros, Args, P, I) ->
    case expand(Expr, ExprMacros, Args) of
        {expr, {Fun, Vars}} ->
            #{
                id => id(P, I),
                expr => Fun,
                vars => Vars
            };
        {text, Txt} ->
            text_struct(Txt, P, I)
    end.

tag_struct(Tag, Txt, T, P, I, State) ->
    Tokens = compile(compile_tag_1(Tag, Txt, T, P, I, State),
                [I | P], 0, tag_tokens_state(Tag, P, I, State)),
    TokensMap = maps:from_list(Tokens),
    #{
        tag => TokensMap,
        indexes => lists:usort(maps:keys(TokensMap))
    }.

% Document Type Definition
is_dtd(<<"!DOCTYPE">>) -> true;
is_dtd(<<"!doctype">>) -> true;
is_dtd(<<"?xml">>) -> true;
is_dtd(_) -> false.

block_struct(Block, P, I, State) ->
    {Mod, Fun} = block_mod_fun(Block, State),
    Tree = apply(Mod, Fun, [maps:get(args, Block, #{})]),
    Attrs = block_attrs(Block),
    Directives = maps:get(directives, Block),
    AllDirectives = case find_first_tag(Tree) of
        {ok, Tag} ->
            maps:merge(maps:get(directives, Tag), Directives);
        none ->
            Directives
    end,
    NonAttrs = [stateful, 'if', 'for', 'in', 'case', 'of'],
    AllAttrs = maps:merge(Attrs, maps:without(NonAttrs, Directives)),
    Id = id(P, I),
    Stateful = maps:get(stateful, Directives, false),
    Tokens = case Stateful of
        true ->
            compile(Tree, [I | P], 0,
                State#state{view = Mod, parent = Id});
        false ->
            compile(Tree, [I | P], 0, State)
    end,
    TokensMap = maps:from_list(Tokens),
    ExprVars = expr_vars(Tokens),
    #{
        id => Id,
        view => case Stateful of
            true ->
                Mod;
            false ->
                State#state.view
        end,
        block => TokensMap,
        indexes => lists:usort(maps:keys(TokensMap)),
        directives => AllDirectives,
        attrs => AllAttrs,
        %attrs_vars => vars_group(vars(ExprVars, Id)),
        %vars => vars_group(ExprVars)
        vars => vars_group(vars(ExprVars, Id))
    }.

block_mod_fun(#{name := Name}, State) ->
    case binary:split(Name, <<":">>) of
        [M, F] ->
            {binary_to_atom(M, utf8), binary_to_atom(F, utf8)};
        [F] ->
            {State#state.view, binary_to_atom(F, utf8)}
    end;
block_mod_fun(#{module := M, function := F}, _State) ->
    {M, F};
block_mod_fun(#{function := F}, State) ->
    {State#state.view, F}.

find_first_tag([{tag, #{name := Name} = Tag} | T]) ->
    case is_tag(Name) of
        true ->
            {ok, Tag};
        false ->
            find_first_tag(T)
    end;
find_first_tag(_) ->
    none.

is_tag(Name) ->
    not lists:member(Name, [<<"!DOCTYPE">>, <<"!doctype">>, <<"?xml">>]).

block_attrs(#{attrs := Attrs}) ->
    #{binary_to_atom(K, utf8) => V || {K, V} <- Attrs}.

vars_group(Vars) ->
    maps:groups_from_list(
        fun({Var, _Id}) -> Var end,
        fun({_Var, Id}) -> Id end,
        lists:usort(Vars)).

attrs_vars(Attrs, ExprVars, Id) ->
    maps:fold(fun
        (Attr, {expr, {_Fun, AttrVars}}, Acc0) ->
            lists:foldl(fun(Var, Acc1) ->
                lists:foldl(fun(VId, Acc) ->
                    [{Var, Id ++ VId} | Acc]
                end, Acc1, maps:get(Attr, ExprVars))
            end, Acc0, AttrVars);
        (_, _, Acc0) ->
            Acc0
    end, [], Attrs).

vars(ExprVars, [0]) ->
    ExprVars;
vars(ExprVars, PId) ->
    lists:map(fun({Var, Id}) -> {Var, Id -- PId} end, ExprVars).

expr_vars([{_I, #{expr := _, id := Id, vars := Vars}} | T]) ->
    expr_vars_1(Vars, Id, T);
expr_vars([{_I, #{block := _, id := Id, vars := Vars, attrs := Attrs}} | T]) ->
    attrs_vars(Attrs, Vars, Id) ++ expr_vars(T);
expr_vars([_ | T]) ->
    expr_vars(T);
expr_vars([]) ->
    [].

expr_vars_1([Var | Vars], Id, T) ->
    [{Var, Id} | expr_vars_1(Vars, Id, T)];
expr_vars_1([], _Id, T) ->
    expr_vars(T).

%% --------------------------------------------------------------------
%% EUnit tests.
%% --------------------------------------------------------------------

-ifdef(TEST).
-compile([export_all, nowarn_export_all]).
-include_lib("eunit/include/eunit.hrl").

compile_tree_test() ->
    ?assertMatch({ok,
                  #{block :=
                     #{0 :=
                        #{id := [0], text := <<"<main arz-id=\"root\"><h1>">>},
                       1 :=
                        #{id := [1],
                          expr := _,
                          vars := [title]},
                       2 := #{id := [2], text := <<"</h1>">>},
                       3 :=
                        #{block :=
                           #{0 :=
                              #{id := [3, 0],
                                text := <<"<div arz-id=\"[3]\" id=\"">>},
                             1 :=
                              #{id := [3, 1],
                                expr := _,
                                vars := [id]},
                             2 := #{id := [3, 2], text := <<"\"><span>">>},
                             3 :=
                              #{id := [3, 3],
                                expr := _,
                                vars := [label]},
                             4 := #{id := [3, 4], text := <<"<b>">>},
                             5 :=
                              #{id := [3, 5],
                                expr := _,
                                vars := [counter_count]},
                             6 :=
                              #{id := [3, 6], text := <<"</b></span><br/>">>},
                             7 :=
                              #{block :=
                                 #{0 :=
                                    #{id := [3, 7, 0],
                                      expr := _,
                                      vars := [text]},
                                   1 :=
                                    #{id := [3, 7, 1],
                                      text :=
                                       <<"<button arz-target=\"[3]\" onclick=\"">>},
                                   2 :=
                                    #{id := [3, 7, 2],
                                      expr := _,
                                      vars := [event]},
                                   3 :=
                                    #{id := [3, 7, 3],
                                      text := <<"\" type=\"button\">">>},
                                   4 :=
                                    #{id := [3, 7, 4],
                                      expr := _,
                                      vars := [text]},
                                   5 :=
                                    #{id := [3, 7, 5], text := <<"</button>">>},
                                   6 :=
                                    #{id := [3, 7, 6],
                                      expr := _,
                                      vars := [text]}},
                                id := [3, 7],
                                attrs :=
                                 #{text :=
                                    {expr,
                                     {_, [btn_text]}},
                                   event :=
                                    {expr,
                                     {_,
                                      [btn_event]}}},
                                directives := #{},
                                vars :=
                                 #{text := [[0], [4], [6]], event := [[2]]},
                                indexes := [0, 1, 2, 3, 4, 5, 6],
                                view := arizona_tpl_compile},
                             8 := #{id := [3, 8], text := <<"</div>">>}},
                          id := [3],
                          attrs :=
                           #{id := {text, <<"1">>},
                             counter_count :=
                              {expr, {_, [view_count]}},
                             btn_text := {text, <<"Increment">>},
                             btn_event := {text, <<"incr">>}},
                          directives := #{stateful := true},
                          vars :=
                           #{id := [[1]],
                             label := [[3]],
                             counter_count := [[5]],
                             btn_text := [[7, 0], [7, 4], [7, 6]],
                             btn_event := [[7, 2]]},
                          indexes := [0, 1, 2, 3, 4, 5, 6, 7, 8],
                          view := arizona_tpl_compile},
                       4 :=
                        #{block :=
                           #{0 :=
                              #{id := [4, 0],
                                text := <<"<div arz-id=\"[4]\" id=\"">>},
                             1 :=
                              #{id := [4, 1],
                                expr := _,
                                vars := [id]},
                             2 := #{id := [4, 2], text := <<"\"><span>">>},
                             3 :=
                              #{id := [4, 3],
                                expr := _,
                                vars := [label]},
                             4 := #{id := [4, 4], text := <<"<b>">>},
                             5 :=
                              #{id := [4, 5],
                                expr := _,
                                vars := [counter_count]},
                             6 :=
                              #{id := [4, 6], text := <<"</b></span><br/>">>},
                             7 :=
                              #{block :=
                                 #{0 :=
                                    #{id := [4, 7, 0],
                                      expr := _,
                                      vars := [text]},
                                   1 :=
                                    #{id := [4, 7, 1],
                                      text :=
                                       <<"<button arz-target=\"[4]\" onclick=\"">>},
                                   2 :=
                                    #{id := [4, 7, 2],
                                      expr := _,
                                      vars := [event]},
                                   3 :=
                                    #{id := [4, 7, 3],
                                      text := <<"\" type=\"button\">">>},
                                   4 :=
                                    #{id := [4, 7, 4],
                                      expr := _,
                                      vars := [text]},
                                   5 :=
                                    #{id := [4, 7, 5], text := <<"</button>">>},
                                   6 :=
                                    #{id := [4, 7, 6],
                                      expr := _,
                                      vars := [text]}},
                                id := [4, 7],
                                attrs :=
                                 #{text :=
                                    {expr,
                                     {_, [btn_text]}},
                                   event :=
                                    {expr,
                                     {_,
                                      [btn_event]}}},
                                directives := #{},
                                vars :=
                                 #{text := [[0], [4], [6]], event := [[2]]},
                                indexes := [0, 1, 2, 3, 4, 5, 6],
                                view := arizona_tpl_compile},
                             8 := #{id := [4, 8], text := <<"</div>">>}},
                          id := [4],
                          attrs :=
                           #{id := {text, <<"2">>},
                             label := {text, <<"Rev. Counter:">>},
                             counter_count :=
                              {expr, {_, [view_count]}},
                             btn_text :=
                              {expr,
                               {_, [decr_btn_text]}},
                             btn_event := {text, <<"decr">>}},
                          directives := #{stateful := true},
                          vars :=
                           #{id := [[1]],
                             label := [[3]],
                             counter_count := [[5]],
                             btn_text := [[7, 0], [7, 4], [7, 6]],
                             btn_event := [[7, 2]]},
                          indexes := [0, 1, 2, 3, 4, 5, 6, 7, 8],
                          view := arizona_tpl_compile},
                       5 := #{id := [5], text := <<"</main>">>}},
                    id := [0],
                    attrs := #{},
                    directives := #{stateful := true},
                    vars :=
                     #{title := [[1]],
                       view_count := [[3, 5], [4, 5]],
                       decr_btn_text := [[4, 7, 0], [4, 7, 4], [4, 7, 6]]},
                    indexes := [0, 1, 2, 3, 4, 5],
                    view := arizona_tpl_compile}}, compile({?MODULE, view, #{}})).

%% Start compile support.

mount(Socket) ->
    {ok, Socket}.

view(Macros) ->
    {ok, Tokens, _EndLoc} = arizona_tpl_scan:string("""
    <main :stateful>
        <h1>{_@title}</h1>
        <.arizona_tpl_compile:counter
            id="1"
            counter_count={_@view_count}
            btn_event="incr"
            btn_text="Increment"
        />
        <.arizona_tpl_compile:counter
            id="2"
            label="Rev. Counter:"
            counter_count={_@view_count}
            btn_event="decr"
            btn_text={_@decr_btn_text}
        />
    </main>
    """),
    {ok, Tree} = arizona_tpl_parse:parse_exprs(Tokens, Macros),
    Tree.

counter(Macros) ->
    {ok, Tokens, _EndLoc} = arizona_tpl_scan:string("""
    {% NOTE: :stateful directive defines the arz-id attribue,   }
    {%       and adds this to the blocks param in the state.    }
    <div id={_@id} :stateful>
        <span>
            {try _@label catch _:_ -> <<"Count:">> end}

            {% NOTE: _@counter_count not wrapper by try/catch,  }
            {%       so it's required.                          }
            <b>{_@counter_count}</b>
        </span>

        <br/>

        <.arizona_tpl_compile:button
            event={_@btn_event}
            text={_@btn_text}
        />

        {% NOTE: The 'content' var is a private one.            }
        {%       It's filled with tokens when is not a void tag }
        {%       and the content between `>...</`, for example  }
        {%       > <div>mycontent</div>                         }
        {% {try _@content catch _:_ -> <<>> end} }
    </div>
    """),
    {ok, Tree} = arizona_tpl_parse:parse_exprs(Tokens, Macros),
    Tree.

button(Macros) ->
    {ok, Tokens, _EndLoc} = arizona_tpl_scan:string("""
    {_@text}
    <button type="button" :onclick={_@event}>
        {_@text}
    </button>
    {_@text}
    """),
    {ok, Tree} = arizona_tpl_parse:parse_exprs(Tokens, Macros),
    Tree.

%% End compile support.

expr_vars_test() ->
    Tpl = compile(button(#{}), [], 0, #state{}),
    Vars = expr_vars(Tpl),
    [?assertEqual([{text, [0]}, {event, [2]}, {text, [4]}, {text, [6]}], Vars),
     ?assertMatch(#{text := [[0], [4], [6]], event := [[2]]}, vars_group(Vars))].

vars_test() ->
    {ok, #{vars := Vars}} = compile({?MODULE, counter, #{}}),
    ?assertMatch(#{id := [[1]],
                   label := [[3]],
                   counter_count := [[5]],
                   btn_text := [[7, 0], [7, 4], [7, 6]],
                   btn_event := [[7, 2]]}, Vars).

tpl2(Macros) ->
    {ok, Tokens, _} = arizona_tpl_scan:string(~"""
    <div :if={_@bool}>
      I'll be visible when _@bool equals true
    </div>

    <ul :if={_@list =/= []}>
      <li
        :for={Item = #{name := ItemName}} :in={_@list}
        :case={maps:get(available, Item)} :of={true}
      >
        <b>{ItemName}</b>
      </li>
    </ul>
    """),
    {ok, Tree} = arizona_tpl_parse:parse_exprs(Tokens, Macros),
    Tree.


tpl(Macros) ->
    {ok, Tokens, _} = arizona_tpl_scan:string(~"""
    foo
    <div :if={_@bool}></div>
    <ul>
        {% NOTE: The :let directive could be removed by defining the  }
        {%       :for directive as we wrote the begining of a list    }
        {%       comprehension, like:                                 }
        {%       > `<li :for={Item <- _@list} ...`                    }
        {% NOTE: Should be possible to pattern match Item, e.g.:      }
        {%       > :let={#{available := Available} = Item}            }
        {%       > :for={#{available := Available} := Item <- _@list} }
        <li :for={Item} :in={_@list} :if={maps:get(available, Item)}>
            <b>{_@prefix}.{maps:get(name, Item)}</b>
            <p :for={Subitem} :in={_@other_list}>
                {maps:get(id, Item)}-{_@subprefix}{Subitem}
            </p>
        </li>
    </ul>

    <div :if={_@bool}>
        <p>{_@foo}</p>
    </div>

    {% NOTE: A shorcut could be             }
    {%       > <div :case={_@foo of bar}... }
    <div :case={_@foo} :of={foo}>foo</div>

    <p :case={_@foo} :of={bar}>bar</p>

    <b :case={_@foo} :of={Other when is_atom(Other)}>
        <div :case={Other =:= baz} :of={true}>baz</div>

        <em :case={Other =:= baz} :of={false}>{Other}</em>
    </b>
    """),
    {ok, Tree} = arizona_tpl_parse:parse_exprs(Tokens, Macros),
    Tree.

new_implementation_test() ->
    ?assertEqual(false, arizona_tpl_compile:compile({arizona_tpl_compile, tpl2, #{}})).

-endif.

