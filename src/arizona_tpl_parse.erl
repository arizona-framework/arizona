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
-module(arizona_tpl_parse).
-moduledoc """
Template parser.
""".
-moduledoc #{author => "William Fank Thomé <willilamthome@hotmail.com>"}.

%% API functions.
-export([parse_exprs/1, parse_exprs/2]).

%% --------------------------------------------------------------------
%% API funtions.
%% --------------------------------------------------------------------

parse_exprs(Tokens) ->
    parse_exprs(Tokens, #{}).

parse_exprs(Tokens, Macros) ->
    {ok, do_parse_exprs(Tokens, Macros)}.

%% --------------------------------------------------------------------
%% Internal funtions.
%% --------------------------------------------------------------------

do_parse_exprs([{text, Txt} | T], Macros) ->
    [{text, Txt} | do_parse_exprs(T, Macros)];
do_parse_exprs([{expr, ExprStr} | T], Macros) ->
    case parse_expr_str(ExprStr, Macros, true) of
        {comment, _} ->
            do_parse_exprs(T, Macros);
        Token ->
            [Token | do_parse_exprs(T, Macros)]
    end;
do_parse_exprs([tag_open | T], Macros) ->
    parse_tag(T, Macros);
do_parse_exprs([closing_tag | _] = T, _Macros) ->
    T;
do_parse_exprs([], _Macros) ->
    [].

parse_tag([{tag_name, <<$., Name/binary>>} | T], Macros) ->
    do_parse_block(T, [{name, Name}], Macros);
parse_tag([{tag_name, Name} | T], Macros) ->
    do_parse_tag(T, [{name, Name}], Macros).

do_parse_block([{attr_key, K}, {attr_value, V} | T], Props, Macros) ->
    do_parse_block(T, [parse_attr(K, {text, V}, Macros) | Props], Macros);
do_parse_block([{attr_key, K}, {attr_expr, Expr} | T], Props, Macros) ->
    do_parse_block(T, [parse_attr(K, {expr, Expr}, Macros) | Props], Macros);
do_parse_block([{attr_key, K} | T], Props, Macros) ->
    do_parse_block(T, [parse_attr(K, {text, K}, Macros) | Props], Macros);
do_parse_block([tag_close | T0], Props0, Macros) ->
    {T1, Props} = collect_tokens(T0, Props0, Macros),
    [{tag_name, <<$., Name/binary>>}, tag_close | T] = T1,
    case Name =:= get(name, Props) of
        true ->
            [{block, block_struct(Props)} | do_parse_exprs(T, Macros)];
        false ->
            error({unexpected_block_end, {Props0, Props}})
    end;
do_parse_block([void_close | T], Props, Macros) ->
    [{block, block_struct(Props)} | do_parse_exprs(T, Macros)].

do_parse_tag([{attr_key, K}, {attr_value, V} | T], Props, Macros) ->
    do_parse_tag(T, [parse_attr(K, {text, V}, Macros) | Props], Macros);
do_parse_tag([{attr_key, K}, {attr_expr, Expr} | T], Props, Macros) ->
    do_parse_tag(T, [parse_attr(K, {expr, Expr}, Macros) | Props], Macros);
do_parse_tag([{attr_key, K} | T], Props, Macros) ->
    do_parse_tag(T, [parse_attr(K, {text, K}, Macros) | Props], Macros);
do_parse_tag([tag_close | T0], Props0, Macros) ->
    OpenName = get(name, Props0),
    case is_void(OpenName) of
        true ->
            [{tag, tag_struct([void | Props0])} | do_parse_exprs(T0, Macros)];
        false ->
            {T1, Props} = collect_tokens(T0, Props0, Macros),
            [{tag_name, CloseName}, tag_close | T] = T1,
            case CloseName =:= OpenName of
                true ->
                    [{tag, tag_struct(Props)} | do_parse_exprs(T, Macros)];
                false ->
                    error({unexpected_tag_end, {Props0, Props}})
            end
    end;
do_parse_tag([void_close | T], Props, Macros) ->
    [{tag, tag_struct([void | Props])} | do_parse_exprs(T, Macros)].

is_void(Name) ->
    lists:member(Name, [<<"!DOCTYPE">>, <<"!doctype">>, <<"?xml">>,
        <<"area">>, <<"base">>, <<"br">>, <<"col">>, <<"command">>,
        <<"embed">>, <<"hr">>, <<"img">>, <<"input">>, <<"keygen">>,
        <<"link">>, <<"meta">>, <<"param">>, <<"source">>, <<"track">>,
        <<"wbr">>]).

collect_tokens([closing_tag | T], Props, _Macros) ->
    {T, Props};
collect_tokens([tag_open | T], Props, Macros) ->
    collect_tokens(parse_tag(T, Macros), Props, Macros);
collect_tokens([{expr, ExprStr} | T], Props, Macros) when is_binary(ExprStr) ->
    case parse_expr_str(ExprStr, Macros, true) of
        {comment, _} ->
            collect_tokens(T, Props, Macros);
        Token ->
            collect_tokens(T, [{token, Token} | Props], Macros)
    end;
collect_tokens([H|T], Props, Macros) ->
    collect_tokens(T, [{token, H} | Props], Macros).

parse_attr(K, {expr, ExprStr}, Macros) ->
    % NOTE: Expressions in attributes are not evaluated.
    %       The real value must be returned and not a binary.
    case parse_expr_str(ExprStr, Macros, false) of
        {comment, _} ->
            error(unexpected_comment);
        Expr ->
            do_parse_attr(K, Expr)
    end;
parse_attr(K, {text, Text}, _Macros) ->
    do_parse_attr(K, {text, Text}).

% TODO: Directive keys to atom using binary_to_existing_atom.
do_parse_attr(<<$:, K/binary>>, {text, <<$:, K/binary>>}) ->
    {directive, {binary_to_atom(K, utf8), true}};
do_parse_attr(<<$:, K/binary>>, V) ->
    {directive, {binary_to_atom(K, utf8), V}};
% TODO: Comments support.
do_parse_attr(<<$%, _K/binary>>, _V) ->
    error(not_supported_yet);
do_parse_attr(K, V) ->
    {attr, {K, V}}.

parse_expr_str(ExprStr, Macros, Eval) ->
    parse_expr_str(ExprStr, Macros, #{}, Eval).

parse_expr_str(ExprStr, Macros, Bindings, Eval) ->
    ExprTree = merl:quote(ExprStr),
    case erl_syntax:type(ExprTree) =:= comment of
        true ->
            {comment, erl_syntax:comment_text(ExprTree)};
        false ->
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
                            FunStr = <<"fun(_Assigns) -> ", MacrosStr/binary, " end">>,
                            Tree = [merl:quote(FunStr)],
                            expr_struct(Tree, [], Bindings)
                    end;
                Vars ->
                    FunStr = <<"fun(Assigns) -> _@subst end">>,
                    VarsSubst = [{Var, subst_var(Var)} || Var <- Vars],
                    Env = [{subst, merl:subst(MacrosTree, VarsSubst)}],
                    Tree = erl_syntax:revert_forms([merl:qquote(FunStr, Env)]),
                    expr_struct(Tree, Vars, Bindings)
            end
    end.

subst_var(Var) ->
  VarStr = atom_to_binary(Var, utf8),
  merl:quote(<<"maps:get(", VarStr/binary, ", Assigns)">>).

block_struct(Props) ->
    #{
        name => get(name, Props),
        directives => maps:from_list(get_all(directive, Props)),
        attrs => get_all(attr, Props),
        tokens => get_all(token, Props)
    }.

tag_struct(Props) ->
    #{
        name => get(name, Props),
        void => get(void, Props, false),
        directives => maps:from_list(get_all(directive, Props)),
        attrs => get_all(attr, Props),
        tokens => get_all(token, Props)
     }.

expr_struct(Tree, Vars, Bindings) ->
    {value, Fun, _NewBindings} = erl_eval:exprs(Tree, Bindings),
    {expr, {Fun, Vars}}.

get(K, L, D) ->
    proplists:get_value(K, L, D).

get_all(K, L) ->
    lists:reverse(proplists:get_all_values(K, L)).

get(K, L) ->
    case proplists:lookup(K, L) of
        {K, V} ->
            V;
        none ->
            error({badkey, {K, L}})
    end.

%% --------------------------------------------------------------------
%% EUnit tests.
%% --------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

parse_exprs_test() ->
    {ok, Tokens, _EndLoc} = arizona_tpl_scan:string(~"""
    Start
    {% This is a comment. }
    <main id="foo" class={_@class} style='display: none;' hidden>
        foo{% Comments are allowed in expressions.
            _@bar}baz
        <br/>
        <div>
            <span id="nested">
                <.foo:nested>ok</.foo:nested>
            </span>
        </div>
        <.foo:block :if={_@true}/>
        <.foo:counter id="counter" count={0}></.foo:counter>
    </main>
    End
    """),
    ?assertMatch({ok,
                  [{text,<<"Start">>},
                   {tag,
                    #{name := <<"main">>,void := false,
                      tokens :=
                       [{text,<<"foo">>},
                        {expr,{ExprFun1,[bar]}},
                        {text,<<"baz">>},
                        {tag,
                         #{name := <<"br">>,void := true,tokens := [],
                           directives := #{},attrs := []}},
                        {tag,
                         #{name := <<"div">>,void := false,
                           tokens :=
                            [{tag,
                              #{name := <<"span">>,void := false,
                                tokens :=
                                 [{block,
                                   #{name := <<"foo:nested">>,
                                     tokens := [{text,<<"ok">>}],
                                     attrs := [],directives := #{}}}],
                                attrs := [{<<"id">>,{text,<<"nested">>}}],
                                directives := #{}}}],
                           attrs := [],directives := #{}}},
                        {block,
                         #{name := <<"foo:block">>,tokens := [],attrs := [],
                           directives :=
                            #{'if' :=
                               {expr,{ExprFun2,[true]}}}}},
                        {block,
                         #{name := <<"foo:counter">>,tokens := [],
                           attrs :=
                            [{<<"id">>,{text,<<"counter">>}},
                             {<<"count">>,
                              {expr,{ExprFun3,[]}}}],
                           directives := #{}}}],
                      attrs :=
                       [{<<"id">>,{text,<<"foo">>}},
                        {<<"class">>,
                         {expr,{ExprFun4,[class]}}},
                        {<<"style">>,{text,<<"display: none;">>}},
                        {<<"hidden">>,{text,<<"hidden">>}}],
                      directives := #{}}},
                   {text,<<"End">>}]}
        when is_function(ExprFun1, 1) andalso
             is_function(ExprFun2, 1) andalso
             is_function(ExprFun3, 1) andalso
             is_function(ExprFun4, 1), parse_exprs(Tokens)).

macros_test() ->
    {ok, Tokens, _} = arizona_tpl_scan:string(<<"foo{_@bar}baz">>),
    ?assertEqual({ok, [
        {text, <<"foo">>},
        {text, <<"bar">>},
        {text, <<"baz">>}
    ]}, parse_exprs(Tokens, #{bar => <<"bar">>})).

-endif.

