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
-export([parse_exprs/1]).

%% --------------------------------------------------------------------
%% API funtions.
%% --------------------------------------------------------------------

parse_exprs(Tokens) ->
    {ok, do_parse_exprs(Tokens)}.

%% --------------------------------------------------------------------
%% Internal funtions.
%% --------------------------------------------------------------------

do_parse_exprs([{text, Txt} | T]) ->
    [{text, Txt} | do_parse_exprs(T)];
do_parse_exprs([{expr, ExprStr} | T]) ->
    case parse_expr_str(ExprStr) of
        {comment, _} ->
            do_parse_exprs(T);
        Expr ->
            [Expr | do_parse_exprs(T)]
    end;
do_parse_exprs([tag_open | T]) ->
    parse_tag(T);
do_parse_exprs([closing_tag | _] = T) ->
    T;
do_parse_exprs([]) ->
    [].

parse_tag([{tag_name, <<$., Name/binary>>} | T]) ->
    {M, F} = parse_block_name(Name),
    do_parse_block(T, [{module, M}, {function, F}]);
parse_tag([{tag_name, Name} | T]) ->
    do_parse_tag(T, [{name, Name}]).

parse_block_name(Name) ->
    case binary:split(Name, <<":">>) of
        [M, F] ->
            % TODO: Change to binary_to_existing_atom
            {binary_to_atom(M, utf8), binary_to_atom(F, utf8)};
        % TODO: Allow define just a function without a module, e.g.:
        %       <.function_name/>
        [_F] ->
            error(not_implemented_yet)
    end.

do_parse_block([{attr_key, K}, {attr_value, V} | T], Props) ->
    do_parse_block(T, [parse_attr(K, {text, V}) | Props]);
do_parse_block([{attr_key, K}, {attr_expr, Expr} | T], Props) ->
    do_parse_block(T, [parse_attr(K, {expr, Expr}) | Props]);
do_parse_block([{attr_key, K} | T], Props) ->
    do_parse_block(T, [parse_attr(K, {text, K}) | Props]);
do_parse_block([tag_close | T0], Props0) ->
    {T1, Props} = collect_tokens(T0, Props0),
    [{tag_name, <<$., Name/binary>>}, tag_close | T] = T1,
    {M, F} = parse_block_name(Name),
    case M =:= get(module, Props) andalso F =:= get(function, Props) of
        true ->
            [{block, block_struct(Props)} | do_parse_exprs(T)];
        false ->
            error({unexpected_block_end, {{M, F}, Props}})
    end;
do_parse_block([void_close | T], Props) ->
    [{block, block_struct(Props)} | do_parse_exprs(T)].

do_parse_tag([{attr_key, K}, {attr_value, V} | T], Props) ->
    do_parse_tag(T, [parse_attr(K, {text, V}) | Props]);
do_parse_tag([{attr_key, K}, {attr_expr, Expr} | T], Props) ->
    do_parse_tag(T, [parse_attr(K, {expr, Expr}) | Props]);
do_parse_tag([{attr_key, K} | T], Props) ->
    do_parse_tag(T, [parse_attr(K, {text, K}) | Props]);
do_parse_tag([tag_close | T0], Props0) ->
    {T1, Props} = collect_tokens(T0, Props0),
    [{tag_name, Name}, tag_close | T] = T1,
    case Name =:= get(name, Props) of
        true ->
            [{tag, tag_struct(Props)} | do_parse_exprs(T)];
        false ->
            error({unexpected_tag_end, {Name, Props}})
    end;
do_parse_tag([void_close | T], Props) ->
    [{tag, tag_struct([void | Props])} | do_parse_exprs(T)].

collect_tokens([closing_tag | T], Props) ->
    {T, Props};
collect_tokens([tag_open | T], Props) ->
    collect_tokens(parse_tag(T), Props);
collect_tokens([{expr, ExprStr} | T], Props) when is_binary(ExprStr) ->
    case parse_expr_str(ExprStr) of
        {comment, _} ->
            collect_tokens(T, Props);
        Expr ->
            collect_tokens(T, [{token, Expr} | Props])
    end;
collect_tokens([H|T], Props) ->
    collect_tokens(T, [{token, H} | Props]).

parse_attr(K, {expr, ExprStr}) ->
    case parse_expr_str(ExprStr) of
        {comment, _} ->
            error(unexpected_comment);
        Expr ->
            do_parse_attr(K, Expr)
    end;
parse_attr(K, {text, Text}) ->
    do_parse_attr(K, {text, Text}).

% TODO: Directive keys to atom using binary_to_existing_atom.
do_parse_attr(<<$:, K/binary>>, {text, <<$:, K/binary>>}) ->
    {directive, {binary_to_atom(K, utf8), true}};
do_parse_attr(<<$:, K/binary>>, V) ->
    {directive, {binary_to_atom(K, utf8), V}};
do_parse_attr(K, V) ->
    {attr, {K, V}}.

parse_expr_str(ExprStr) ->
    parse_expr_str(ExprStr, #{}).

%% TODO: Use 'Bindings' to solve the _@inner_content issue, e.g.:
%%       <div>{_@inner_content}</div>
%%       'inner_content' must be compiled into tokens.
%% NOTE: Maybe the expression could be {:inner_content}.
parse_expr_str(ExprStr, Bindings) ->
    ExprTree = merl:quote(ExprStr),
    case erl_syntax:type(ExprTree) =:= comment of
        true ->
            {comment, erl_syntax:comment_text(ExprTree)};
        false ->
            case merl:template_vars(merl:template(ExprTree)) of
                [] ->
                    FunStr = <<"fun(_Assigns) -> ", ExprStr/binary, " end">>,
                    Tree = [merl:quote(FunStr)],
                    expr_struct(Tree, [], Bindings);
                Vars ->
                    FunStr = <<"fun(Assigns) -> _@subst end">>,
                    Env = [{Var, subst_var(Var)} || Var <- Vars],
                    Tree = erl_syntax:revert_forms([
                        merl:qquote(FunStr, [
                            {subst, merl:subst(ExprTree, Env)}])]),
                    expr_struct(Tree, Vars, Bindings)
            end
    end.

subst_var(Var) ->
  VarStr = atom_to_binary(Var, utf8),
  merl:quote(<<"maps:get(", VarStr/binary, ", Assigns)">>).

block_struct(Props) ->
    #{
        module => get(module, Props),
        function => get(function, Props),
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
    element(2, proplists:lookup(K, L)).

%% --------------------------------------------------------------------
%% EUnit tests.
%% --------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

parse_exprs_test() ->
    ?assertMatch({ok, [
        {text,<<"Start">>},
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
                        #{function := nested,module := foo,
                          tokens := [{text,<<"ok">>}],
                          directives := #{},attrs := []}}],
                     directives := #{},
                     attrs := [{<<"id">>,{text,<<"nested">>}}]}}],
                directives := #{},attrs := []}},
             {block,
              #{function := block,module := foo,tokens := [],
                directives :=
                 #{'if' :=
                    {expr,{ExprFun2,[true]}}},
                attrs := []}},
             {block,
              #{function := counter,module := foo,tokens := [],
                directives := #{},
                attrs :=
                 [{<<"id">>,{text,<<"counter">>}},
                  {<<"count">>,
                   {expr,{ExprFun3,[]}}}]}}],
           directives := #{},
           attrs :=
            [{<<"id">>,{text,<<"foo">>}},
             {<<"class">>,
              {expr,{ExprFun4,[class]}}},
             {<<"style">>,{text,<<"display: none;">>}},
             {<<"hidden">>,{text,<<"hidden">>}}]}},
        {text,<<"End">>}
    ]} when is_function(ExprFun1, 1) andalso
            is_function(ExprFun2, 1) andalso
            is_function(ExprFun3, 1) andalso
            is_function(ExprFun4, 1),
    parse_exprs([
        {text,<<"Start">>},
        {expr,<<"% This is a comment. ">>},
        tag_open,
        {tag_name,<<"main">>},
        {attr_key,<<"id">>},
        {attr_value,<<"foo">>},
        {attr_key,<<"class">>},
        {attr_expr,<<"_@class">>},
        {attr_key,<<"style">>},
        {attr_value,<<"display: none;">>},
        {attr_key,<<"hidden">>},
        tag_close,
        {text,<<"foo">>},
        {expr,<<"% Comments are allowed in expressions.\n        _@bar">>},
        {text,<<"baz">>},
        tag_open,
        {tag_name,<<"br">>},
        void_close,tag_open,
        {tag_name,<<"div">>},
        tag_close,tag_open,
        {tag_name,<<"span">>},
        {attr_key,<<"id">>},
        {attr_value,<<"nested">>},
        tag_close,tag_open,
        {tag_name,<<".foo:nested">>},
        tag_close,
        {text,<<"ok">>},
        closing_tag,
        {tag_name,<<".foo:nested">>},
        tag_close,closing_tag,
        {tag_name,<<"span">>},
        tag_close,closing_tag,
        {tag_name,<<"div">>},
        tag_close,tag_open,
        {tag_name,<<".foo:block">>},
        {attr_key,<<":if">>},
        {attr_expr,<<"_@true">>},
        void_close,tag_open,
        {tag_name,<<".foo:counter">>},
        {attr_key,<<"id">>},
        {attr_value,<<"counter">>},
        {attr_key,<<"count">>},
        {attr_expr,<<"0">>},
        tag_close,closing_tag,
        {tag_name,<<".foo:counter">>},
        tag_close,closing_tag,
        {tag_name,<<"main">>},
        tag_close,
        {text,<<"End">>}
    ])).

-endif.

