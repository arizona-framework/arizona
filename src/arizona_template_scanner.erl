%%
%% %CopyrightBegin%
%%
%% Copyright 2024 Arizona Framework <contact@arizonaframe.work>
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
-module(arizona_template_scanner).
-moduledoc false.

%% API functions.
-export([scan/2, new_anno/1]).

%% Types
-export_type([anno/0, anno_options/0]).

% Module and function could be undefined in favor of files scanning.
-opaque anno() :: #{
    module => module() | undefined,
    function => atom() | undefined,
    file => binary(),
    line => non_neg_integer(),
    column => non_neg_integer(),
    first_column => non_neg_integer(),
    position => non_neg_integer()
}.

-type anno_options() :: #{
    module := module(),
    function := atom(),
    file := string() | binary(),
    line := non_neg_integer(),
    column := non_neg_integer(),
    first_column := non_neg_integer(),
    position := non_neg_integer()
}.

%% --------------------------------------------------------------------
%% API funtions.
%% --------------------------------------------------------------------

scan(Bin, Anno) when is_binary(Bin), is_map(Anno) ->
    scan(Bin, Bin, _Len = 0, Anno).

new_anno(Opts) when is_map(Opts) ->
    Anno = maps:merge(default_anno(), Opts),
    maps:map(fun normalize_anno_value/2, Anno).

%% --------------------------------------------------------------------
%% Internal funtions.
%% --------------------------------------------------------------------

scan(<<${, Rest/binary>>, Bin, Len, Anno) ->
    maybe_prepend_text_token(Bin, Len, Anno,
        scan_expr(Rest, Bin, incr_anno_pos(Len + 1, Anno)));
scan(<<_, Rest/binary>>, Bin, Len, Anno) ->
    scan(Rest, Bin, Len + 1, Anno);
scan(<<>>, Bin, Len, Anno) ->
    maybe_prepend_text_token(Bin, Len, Anno, []).

maybe_prepend_text_token(Bin, Len, _Anno = #{position := Pos}, Tokens) ->
    case string:trim(binary_part(Bin, Pos, Len)) of
        <<>> ->
            Tokens;
        Text ->
            [{text, Text} | Tokens]
    end.

scan_expr(Rest0, Bin, Anno = #{position := Pos}) ->
    case find_expr_end(Rest0, 0, 0) of
        {ok, {Len, MarkerLen, Rest}} ->
            Expr = binary_part(Bin, Pos, Len),
            Category = expr_category(Expr),
            [{Category, Expr}
             | scan(Rest, Bin, 0, incr_anno_pos(Len + MarkerLen, Anno))];
        {error, Reason} ->
            erlang:error(Reason, [Rest0, Bin, Anno], [{error_info, Anno}])
    end.

expr_category(Expr) ->
    case erl_syntax:type(merl:quote(Expr)) =:= comment of
        true ->
            comment;
        false ->
            expr
    end.

find_expr_end(<<$}, Rest/binary>>, 0, Len) ->
    {ok, {Len, 1, Rest}};
find_expr_end(<<$}, Rest/binary>>, Depth, Len) ->
    find_expr_end(Rest, Depth - 1, Len + 1);
find_expr_end(<<${, Rest/binary>>, Depth, Len) ->
    find_expr_end(Rest, Depth + 1, Len + 1);
find_expr_end(<<$", Rest0/binary>>, Depth, Len0) ->
    case find_string_end(Rest0, Len0 + 1) of
        {ok, {Len, Rest}} ->
            find_expr_end(Rest, Depth, Len);
        {error, Reason} ->
            {error, Reason}
    end;
find_expr_end(<<_, Rest/binary>>, Depth, Len) ->
    find_expr_end(Rest, Depth, Len + 1);
find_expr_end(<<>>, _Depth, _Len) ->
    {error, unexpected_expr_end}.

find_string_end(<<$\\, $", Rest/binary>>, Len) ->
    find_string_end(Rest, Len + 2);
find_string_end(<<$", Rest/binary>>, Len) ->
    {ok, {Len + 1, Rest}};
find_string_end(<<_, Rest/binary>>, Len) ->
    find_string_end(Rest, Len + 1);
find_string_end(<<>>, _) ->
    {error, unexpected_string_end}.

default_anno() ->
    #{
        module => undefined,
        function => undefined,
        file => undefined,
        line => 1,
        column => 1,
        first_column => 1,
        position => 0
    }.

normalize_anno_value(module, Mod) when is_atom(Mod) ->
    Mod;
normalize_anno_value(function, Fun) when is_atom(Fun) ->
    Fun;
normalize_anno_value(file, File) when is_binary(File) ->
    File;
normalize_anno_value(file, File) when is_list(File) ->
    iolist_to_binary(File);
normalize_anno_value(line, Ln) when is_integer(Ln), Ln >= 0 ->
    Ln;
normalize_anno_value(column, Col) when is_integer(Col), Col >= 0 ->
    Col;
normalize_anno_value(first_column, Col) when is_integer(Col), Col >= 0 ->
    Col;
normalize_anno_value(position, Pos) when is_integer(Pos), Pos >= 0 ->
    Pos.

incr_anno_pos(N, #{position := Pos} = Anno) ->
    Anno#{position => Pos + N}.

%% --------------------------------------------------------------------
%% EUnit tests.
%% --------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

scan_test() ->
    ?assertEqual(
        [
            {text, <<"begin">>},
            {comment, <<"% comment ">>},
            {expr, <<"foo">>},
            {text, <<"end">>}
        ],
        scan(~"""
        begin
        {% comment }
        {foo}
        end
        """, new_anno(#{file => ?FILE}))).

find_expr_end_test() ->
    [
        ?assertEqual({ok, {10, 1, <<"baz">>}},
                     find_expr_end(<<"{foo\"bar\"}}baz">>, 0, 0)),
        ?assertEqual({ok, {8, 1, <<"baz">>}},
                     find_expr_end(<<"foo\"bar\"}baz">>, 0, 0)),
        ?assertEqual({error, unexpected_expr_end},
                     find_expr_end(<<"foo\"bar\"baz">>, 0, 0))
    ].

find_string_end_test() ->
    [
        ?assertEqual({ok, {9, <<"baz">>}},
                     find_string_end(<<"foo\\\"bar\"baz">>, 0)),
        ?assertEqual({error, unexpected_string_end},
                     find_string_end(<<"foo">>, 0))
    ].

new_anno_test() ->
    [
        ?assertError(function_clause, new_anno(#{})),
        ?assertError(function_clause, new_anno(#{module => -1})),
        ?assertError(function_clause, new_anno(#{function => -1})),
        ?assertError(function_clause, new_anno(#{file => -1})),
        ?assertError(function_clause, new_anno(#{line => -1})),
        ?assertError(function_clause, new_anno(#{column => -1})),
        ?assertError(function_clause, new_anno(#{first_column => -1})),
        ?assertError(function_clause, new_anno(#{position => -10})),
        ?assertEqual(#{
            module => undefined,
            function => undefined,
            file => iolist_to_binary(?FILE),
            line => 1,
            column => 1,
            first_column => 1,
            position => 0
        }, new_anno(#{file => ?FILE}))
    ].

-endif.

