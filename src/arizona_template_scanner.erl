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
-export_type([anno/0, token_anno/0, anno_options/0]).

% Module and function could be undefined in favor of files scanning.
% This anno struct helps error_info of erlang:error/3 to display
% a more accurated message when an exception occurs.
-opaque anno() :: #{
    module => module() | undefined,
    function => atom() | undefined,
    file => binary(),
    line => line(),
    column => column(),
    first_column => non_neg_integer(),
    position => non_neg_integer()
}.

-type anno_options() :: #{
    module := module(),
    function := atom(),
    file := string() | binary(),
    line := line(),
    column := column(),
    first_column := non_neg_integer(),
    position := non_neg_integer()
}.

-type token_anno() :: {source(), location()}.
-type source() :: {file, binary()} | {module(), atom()}.
-type location() :: {line(), column()}.
-type line() :: non_neg_integer().
-type column() :: non_neg_integer().

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
scan(<<$\r, $\n, Rest/binary>>, Bin, Len, Anno) ->
    scan(Rest, Bin, Len + 2, new_line(Anno));
scan(<<$\r, Rest/binary>>, Bin, Len, Anno) ->
    scan(Rest, Bin, Len + 1, new_line(Anno));
scan(<<$\n, Rest/binary>>, Bin, Len, Anno) ->
    scan(Rest, Bin, Len + 1, new_line(Anno));
scan(<<_, Rest/binary>>, Bin, Len, Anno) ->
    scan(Rest, Bin, Len + 1, incr_anno_col(1, Anno));
scan(<<>>, Bin, Len, Anno) ->
    % FIXME: The location (line, column) is incorrect in the last text token.
    maybe_prepend_text_token(Bin, Len, Anno, []).

maybe_prepend_text_token(Bin, Len, Anno = #{position := Pos}, Tokens) ->
    case string:trim(binary_part(Bin, Pos, Len)) of
        <<>> ->
            Tokens;
        Text ->
            [{text, token_anno(Anno), Text} | Tokens]
    end.

token_anno(Anno) ->
    {token_source(Anno), token_location(Anno)}.

token_source(#{module := Mod, function := Fun}) when Mod =/= undefined,
                                                     Fun =/= undefined ->
    {Mod, Fun};
token_source(#{file := File}) ->
    {file, File}.

token_location(#{line := Ln, column := Col}) ->
    {Ln, Col}.

scan_expr(Rest0, Bin, ExprAnno) ->
    case find_expr_end(Rest0, _Depth = 0, _Len = 0, ExprAnno) of
        {ok, {Len, Anno, Rest}} ->
            Pos = maps:get(position, ExprAnno),
            Expr = binary_part(Bin, Pos, Len),
            Category = expr_category(Expr),
            [{Category, token_anno(ExprAnno), Expr} | scan(Rest, Bin, 0, Anno)];
        {error, {Reason, Anno}} ->
            erlang:error(Reason, [Rest0, Bin, ExprAnno], [{error_info, Anno}])
    end.

expr_category(Expr) ->
    case erl_syntax:type(merl:quote(Expr)) =:= comment of
        true ->
            comment;
        false ->
            expr
    end.

find_expr_end(<<$}, Rest/binary>>, 0, Len, Anno) ->
    {ok, {Len, incr_anno_pos(Len + 1, Anno), Rest}};
find_expr_end(<<$}, Rest/binary>>, Depth, Len, Anno) ->
    find_expr_end(Rest, Depth - 1, Len + 1, incr_anno_col(1, Anno));
find_expr_end(<<${, Rest/binary>>, Depth, Len, Anno) ->
    find_expr_end(Rest, Depth + 1, Len + 1, incr_anno_col(1, Anno));
find_expr_end(<<$", Rest0/binary>>, Depth, Len0, Anno0) ->
    case find_string_end(Rest0, Len0 + 1, incr_anno_col(1, Anno0)) of
        {ok, {Len, Anno, Rest}} ->
            find_expr_end(Rest, Depth, Len, Anno);
        {error, ErrInfo} ->
            {error, ErrInfo}
    end;
find_expr_end(<<$\r, $\n, Rest/binary>>, Depth, Len, Anno) ->
    find_expr_end(Rest, Depth, Len + 2, new_line(Anno));
find_expr_end(<<$\r, Rest/binary>>, Depth, Len, Anno) ->
    find_expr_end(Rest, Depth, Len + 1, new_line(Anno));
find_expr_end(<<$\n, Rest/binary>>, Depth, Len, Anno) ->
    find_expr_end(Rest, Depth, Len + 1, new_line(Anno));
find_expr_end(<<_, Rest/binary>>, Depth, Len, Anno) ->
    find_expr_end(Rest, Depth, Len + 1, incr_anno_col(1, Anno));
find_expr_end(<<>>, _Depth, Len, Anno) ->
    {error, {unexpected_expr_end, incr_anno_pos(Len, Anno)}}.

find_string_end(<<$\\, $", Rest/binary>>, Len, Anno) ->
    find_string_end(Rest, Len + 2, incr_anno_col(2, Anno));
find_string_end(<<$", Rest/binary>>, Len, Anno) ->
    {ok, {Len + 1, incr_anno_col(1, Anno), Rest}};
find_string_end(<<_, Rest/binary>>, Len, Anno) ->
    find_string_end(Rest, Len + 1, incr_anno_col(1, Anno));
find_string_end(<<>>, Len, Anno) ->
    {error, {unexpected_string_end, incr_anno_pos(Len, Anno)}}.

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

new_line(Anno = #{line := Ln, first_column := Col}) ->
    Anno#{line => Ln + 1, column => Col}.

incr_anno_col(N, Anno = #{column := Col}) ->
    Anno#{column => Col + N}.

incr_anno_pos(N, Anno = #{position := Pos}) ->
    Anno#{position => Pos + N}.

%% --------------------------------------------------------------------
%% EUnit tests.
%% --------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-define(ANNO, new_anno(#{file => ?FILE})).

scan_test() ->
    ?assertMatch(
        [
            {text, _, <<"begin">>},
            {comment, _, <<"% comment ">>},
            {expr, _, <<"foo">>},
            {text, _, <<"end">>}
        ],
        scan(~"""
        begin
        {% comment }
        {foo}
        end
        """, ?ANNO)).

find_expr_end_test() ->
    [
        ?assertMatch({ok, {10, _, <<"baz">>}},
                     find_expr_end(<<"{foo\"bar\"}}baz">>, 0, 0, ?ANNO)),
        ?assertMatch({ok, {8, _, <<"baz">>}},
                     find_expr_end(<<"foo\"bar\"}baz">>, 0, 0, ?ANNO)),
        ?assertMatch({error, {unexpected_expr_end, _}},
                     find_expr_end(<<"foo\"bar\"baz">>, 0, 0, ?ANNO))
    ].

find_string_end_test() ->
    [
        ?assertMatch({ok, {9, _, <<"baz">>}},
                     find_string_end(<<"foo\\\"bar\"baz">>, 0, ?ANNO)),
        ?assertMatch({error, {unexpected_string_end, _}},
                     find_string_end(<<"foo">>, 0, ?ANNO))
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
        }, ?ANNO)
    ].

incr_anno_pos_test() ->
    #{position := Pos} = incr_anno_pos(1, new_anno(#{file => ?FILE})),
    ?assertEqual(1, Pos).

-endif.

