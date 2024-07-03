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
-export([scan/2]).
-export([new_anno/1]).

%% Types
-export_type([anno/0]).
-export_type([anno_options/0]).
-export_type([token_anno/0]).
-export_type([source/0]).
-export_type([location/0]).
-export_type([line/0]).
-export_type([column/0]).

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
    scan(Bin, Bin, 0, Anno).

new_anno(Opts) when is_map(Opts) ->
    Anno = maps:merge(default_anno(), Opts),
    maps:map(fun normalize_anno_value/2, Anno).

%% --------------------------------------------------------------------
%% Internal funtions.
%% --------------------------------------------------------------------

scan(<<${, Rest/binary>>, Bin, Len, Anno) ->
    maybe_prepend_text_token(Bin, Len, Anno,
        scan_expr(Rest, Bin, incr_anno_pos(Len + 1, Anno)));
scan(<<$<, $/, Rest/binary>>, Bin, Len, Anno) ->
    maybe_prepend_text_token(Bin, Len, Anno,
        scan_closing_tag(Rest, Bin, incr_anno_pos(Len + 2, Anno)));
scan(<<$<, Rest/binary>>, Bin, Len, Anno) ->
    maybe_prepend_text_token(Bin, Len, Anno,
        scan_tag(Rest, Bin, incr_anno_pos(Len + 1, Anno)));
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

scan_expr(Rest0, Bin, ExprAnno) ->
    case scan_expr_end(Rest0, 0, 0, ExprAnno) of
        {ok, {Len, MarkerLen, Anno, Rest}} ->
            Pos = maps:get(position, ExprAnno),
            Expr = binary_part(Bin, Pos, Len),
            case expr_category(Expr) of
                {ok, Category} ->
                    [{Category, token_anno(ExprAnno), Expr}
                     | scan(Rest, Bin, 0, incr_anno_pos(Len + MarkerLen, Anno))];
                error ->
                    raise({badexpr, Anno})
            end;
        {error, Reason} ->
            raise(Reason)
    end.

scan_expr_end(<<$}, Rest/binary>>, 0, Len, Anno) ->
    {ok, {Len, _MarkerLen = 1, incr_anno_col(1, Anno), Rest}};
scan_expr_end(<<$}, Rest/binary>>, Depth, Len, Anno) ->
    scan_expr_end(Rest, Depth - 1, Len + 1, incr_anno_col(1, Anno));
scan_expr_end(<<${, Rest/binary>>, Depth, Len, Anno) ->
    scan_expr_end(Rest, Depth + 1, Len + 1, incr_anno_col(1, Anno));
scan_expr_end(<<$\r, $\n, Rest/binary>>, Depth, Len, Anno) ->
    scan_expr_end(Rest, Depth, Len + 2, new_line(Anno));
scan_expr_end(<<$\r, Rest/binary>>, Depth, Len, Anno) ->
    scan_expr_end(Rest, Depth, Len + 1, new_line(Anno));
scan_expr_end(<<$\n, Rest/binary>>, Depth, Len, Anno) ->
    scan_expr_end(Rest, Depth, Len + 1, new_line(Anno));
scan_expr_end(<<_, Rest/binary>>, Depth, Len, Anno) ->
    scan_expr_end(Rest, Depth, Len + 1, incr_anno_col(1, Anno));
scan_expr_end(<<>>, _Depth, Len, Anno) ->
    {error, {unexpected_expr_end, incr_anno_pos(Len, Anno)}}.

expr_category(Expr) ->
    try
        case erl_syntax:type(merl:quote(Expr)) =:= comment of
            true ->
                {ok, comment};
            false ->
                {ok, expr}
        end
    catch
        _:_ ->
            error
    end.

scan_tag(Rest0, Bin, TagAnno) ->
    case scan_tag_name(Rest0, 0, TagAnno) of
        {ok, {Len, Anno, Rest}} when Len > 0 ->
            Pos = maps:get(position, TagAnno),
            TagName = binary_part(Bin, Pos, Len),
            [{open_tag, token_anno(TagAnno), TagName}
             | scan_tag_attrs(Rest, Bin, incr_anno_pos(Len, Anno))];
        {error, Reason} ->
            raise(Reason)
    end.

scan_tag_name(<<$/, $>, _/binary>> = Rest, Len, Anno) ->
    {ok, {Len, Anno, Rest}};
scan_tag_name(<<$>, _/binary>> = Rest, Len, Anno) ->
    {ok, {Len, Anno, Rest}};
scan_tag_name(<<$\s, _/binary>> = Rest, Len, Anno) ->
    {ok, {Len, Anno, Rest}};
scan_tag_name(<<$\r, _/binary>> = Rest, Len, Anno) ->
    {ok, {Len, Anno, Rest}};
scan_tag_name(<<$\n, _/binary>> = Rest, Len, Anno) ->
    {ok, {Len, Anno, Rest}};
scan_tag_name(<<_, Rest/binary>>, Len, Anno) ->
    scan_tag_name(Rest, Len + 1, incr_anno_col(1, Anno));
scan_tag_name(<<>>, Len, Anno) ->
    {error, {unexpected_tag_end, incr_anno_pos(Len, Anno)}}.

scan_tag_attrs(<<$/, $>, Rest/binary>>, Bin, Anno) ->
    [{close_tag, token_anno(Anno), void}
     | scan(Rest, Bin, 0, incr_anno_col(2, incr_anno_pos(2, Anno)))];
scan_tag_attrs(<<$>, Rest/binary>>, Bin, Anno) ->
    [{close_tag, token_anno(Anno), nonvoid}
     | scan(Rest, Bin, 0, incr_anno_col(1, incr_anno_pos(1, Anno)))];
scan_tag_attrs(<<$\s, Rest/binary>>, Bin, Anno) ->
    scan_tag_attrs(Rest, Bin, incr_anno_col(1, incr_anno_pos(1, Anno)));
scan_tag_attrs(<<$\r, $\n, Rest/binary>>, Bin, Anno) ->
    scan_tag_attrs(Rest, Bin, new_line(incr_anno_pos(2, Anno)));
scan_tag_attrs(<<$\r, Rest/binary>>, Bin, Anno) ->
    scan_tag_attrs(Rest, Bin, new_line(incr_anno_pos(1, Anno)));
scan_tag_attrs(<<$\n, Rest/binary>>, Bin, Anno) ->
    scan_tag_attrs(Rest, Bin, new_line(incr_anno_pos(1, Anno)));
scan_tag_attrs(Rest0, Bin, AttrsAnno) ->
    case scan_tag_attr_key(Rest0, 0, AttrsAnno) of
        {ok, {KeyLen, KeyAnno, Rest1}} when KeyLen > 0 ->
            KeyPos = maps:get(position, AttrsAnno),
            Key = binary_part(Bin, KeyPos, KeyLen),
            ValAnno = incr_anno_pos(KeyLen, KeyAnno),
            case scan_tag_attr_value(Rest1, Bin, ValAnno) of
                {ok, {Value, Anno, Rest}} ->
                    [{attr_key, token_anno(KeyAnno), Key},
                     {attr_value, token_anno(ValAnno), Value}
                     | scan_tag_attrs(Rest, Bin, Anno)];
                {none, {Anno, Rest}} ->
                    [{bool_attr, token_anno(ValAnno), Key}
                     | scan_tag_attrs(Rest, Bin, Anno)];
                {error, Reason} ->
                    raise(Reason)
            end;
        {error, Reason} ->
            raise(Reason)
    end.

scan_tag_attr_key(<<$=, _/binary>> = Rest, Len, Anno) ->
    {ok, {Len, Anno, Rest}};
scan_tag_attr_key(<<$/, $>, _/binary>> = Rest, Len, Anno) ->
    {ok, {Len, Anno, Rest}};
scan_tag_attr_key(<<$>, _/binary>> = Rest, Len, Anno) ->
    {ok, {Len, Anno, Rest}};
scan_tag_attr_key(<<$\s, _/binary>> = Rest, Len, Anno) ->
    {ok, {Len, Anno, Rest}};
scan_tag_attr_key(<<$\r, _/binary>> = Rest, Len, Anno) ->
    {ok, {Len, Anno, Rest}};
scan_tag_attr_key(<<$\n, _/binary>> = Rest, Len, Anno) ->
    {ok, {Len, Anno, Rest}};
scan_tag_attr_key(<<_, Rest/binary>>, Len, Anno) ->
    scan_tag_attr_key(Rest, Len + 1, incr_anno_col(1, Anno));
scan_tag_attr_key(<<>>, Len, Anno) ->
    {error, {unexpected_tag_end, incr_anno_pos(Len, Anno)}}.

scan_tag_attr_value(<<$=, $', Rest0/binary>>, Bin, KeyAnno) ->
    ValAnno = incr_anno_col(2, incr_anno_pos(2, KeyAnno)),
    case scan_single_quoted_string(Rest0, 0, ValAnno) of
        {ok, {Len, MarkerLen, TxtAnno, Rest}} ->
            Pos = maps:get(position, ValAnno),
            Txt = binary_part(Bin, Pos, Len),
            Anno = incr_anno_pos(Len + MarkerLen, TxtAnno),
            {ok, {{text, Txt}, Anno, Rest}};
        {error, {Reason, Anno}} ->
            {error, {Reason, Anno}}
    end;
scan_tag_attr_value(<<$=, $", Rest0/binary>>, Bin, KeyAnno) ->
    ValAnno = incr_anno_col(2, incr_anno_pos(2, KeyAnno)),
    case scan_double_quoted_string(Rest0, 0, ValAnno) of
        {ok, {Len, MarkerLen, TxtAnno, Rest}} ->
            Pos = maps:get(position, ValAnno),
            Txt = binary_part(Bin, Pos, Len),
            Anno = incr_anno_pos(Len + MarkerLen, TxtAnno),
            {ok, {{text, Txt}, Anno, Rest}};
        {error, {Reason, Anno}} ->
            {error, {Reason, Anno}}
    end;
scan_tag_attr_value(<<$=, ${, Rest0/binary>>, Bin, KeyAnno) ->
    ValAnno = incr_anno_col(2, incr_anno_pos(2, KeyAnno)),
    case scan_expr_end(Rest0, 0, 0, ValAnno) of
        {ok, {Len, MarkerLen, ExprAnno, Rest}} ->
            Pos = maps:get(position, ValAnno),
            Expr = binary_part(Bin, Pos, Len),
            case expr_category(Expr) of
                {ok, expr} ->
                    Anno = incr_anno_pos(Len + MarkerLen, ExprAnno),
                    {ok, {{expr, Expr}, Anno, Rest}};
                {ok, comment} ->
                    % Comments are not allowed in attributes, e.g.:
                    % > id={% comment }
                    {error, {unexpected_comment, ExprAnno}};
                error ->
                    {error, {badexpr, ExprAnno}}
            end;
        {error, {Reason, Anno}} ->
            {error, {Reason, Anno}}
    end;
scan_tag_attr_value(Rest, _Bin, Anno) ->
    {none, {Anno, Rest}}.

scan_closing_tag(Rest0, Bin, TagAnno) ->
    case scan_closing_tag_name(Rest0, 0, TagAnno) of
        {ok, {TagNameLen, TagNameAnno, Rest1}} when TagNameLen > 0 ->
            TagEndAnno = incr_anno_pos(TagNameLen, TagNameAnno),
            case scan_closing_tag_end(Rest1, 0, TagEndAnno) of
                {ok, {Anno, Rest}} ->
                    Pos = maps:get(position, TagAnno),
                    TagName = binary_part(Bin, Pos, TagNameLen),
                    [{closing_tag, token_anno(TagAnno), TagName}
                     | scan(Rest, Bin, 0, Anno)];
                {error, Reason} ->
                    raise(Reason)
            end;
        {error, Reason} ->
            raise(Reason)
    end.

scan_closing_tag_name(<<$>, _/binary>> = Rest, Len, Anno) ->
    {ok, {Len, Anno, Rest}};
scan_closing_tag_name(<<$\s, _/binary>> = Rest, Len, Anno) ->
    {ok, {Len, Anno, Rest}};
scan_closing_tag_name(<<$\r, _/binary>> = Rest, Len, Anno) ->
    {ok, {Len, Anno, Rest}};
scan_closing_tag_name(<<$\n, _/binary>> = Rest, Len, Anno) ->
    {ok, {Len, Anno, Rest}};
scan_closing_tag_name(<<_, Rest/binary>>, Len, Anno) ->
    scan_closing_tag_name(Rest, Len + 1, incr_anno_col(1, Anno));
scan_closing_tag_name(<<>>, Len, Anno) ->
    {error, {unexpected_tag_end, incr_anno_pos(Len, Anno)}}.

scan_closing_tag_end(<<$>, Rest/binary>>, Len, Anno) ->
    {ok, {incr_anno_col(1, incr_anno_pos(Len + 1, Anno)), Rest}};
scan_closing_tag_end(<<$\r, $\n, Rest/binary>>, Len, Anno) ->
    scan_closing_tag_end(Rest, Len + 2, new_line(Anno));
scan_closing_tag_end(<<$\r, Rest/binary>>, Len, Anno) ->
    scan_closing_tag_end(Rest, Len + 1, new_line(Anno));
scan_closing_tag_end(<<$\n, Rest/binary>>, Len, Anno) ->
    scan_closing_tag_end(Rest, Len + 1, new_line(Anno));
scan_closing_tag_end(<<_, Rest/binary>>, Len, Anno) ->
    scan_closing_tag_end(Rest, Len + 1, incr_anno_col(1, Anno));
scan_closing_tag_end(<<>>, Len, Anno) ->
    {error, {unexpected_tag_end, incr_anno_pos(Len, Anno)}}.

scan_single_quoted_string(<<$\\, $', Rest/binary>>, Len, Anno) ->
    scan_single_quoted_string(Rest, Len + 2, incr_anno_col(2, Anno));
scan_single_quoted_string(<<$', Rest/binary>>, Len, Anno) ->
    {ok, {Len, _MarkerLen = 1, Anno, Rest}};
scan_single_quoted_string(<<$\r, $\n, Rest/binary>>, Len, Anno) ->
    scan_single_quoted_string(Rest, Len + 2, new_line(Anno));
scan_single_quoted_string(<<$\r, Rest/binary>>, Len, Anno) ->
    scan_single_quoted_string(Rest, Len + 1, new_line(Anno));
scan_single_quoted_string(<<$\n, Rest/binary>>, Len, Anno) ->
    scan_single_quoted_string(Rest, Len + 1, new_line(Anno));
scan_single_quoted_string(<<_, Rest/binary>>, Len, Anno) ->
    scan_single_quoted_string(Rest, Len + 1, incr_anno_col(1, Anno));
scan_single_quoted_string(<<>>, Len, Anno) ->
    {error, {unexpected_string_end, incr_anno_pos(Len, Anno)}}.

scan_double_quoted_string(<<$\\, $", Rest/binary>>, Len, Anno) ->
    scan_double_quoted_string(Rest, Len + 2, incr_anno_col(2, Anno));
scan_double_quoted_string(<<$", Rest/binary>>, Len, Anno) ->
    {ok, {Len, _MarkerLen = 1, Anno, Rest}};
scan_double_quoted_string(<<$\r, $\n, Rest/binary>>, Len, Anno) ->
    scan_double_quoted_string(Rest, Len + 2, new_line(Anno));
scan_double_quoted_string(<<$\r, Rest/binary>>, Len, Anno) ->
    scan_double_quoted_string(Rest, Len + 1, new_line(Anno));
scan_double_quoted_string(<<$\n, Rest/binary>>, Len, Anno) ->
    scan_double_quoted_string(Rest, Len + 1, new_line(Anno));
scan_double_quoted_string(<<_, Rest/binary>>, Len, Anno) ->
    scan_double_quoted_string(Rest, Len + 1, incr_anno_col(1, Anno));
scan_double_quoted_string(<<>>, Len, Anno) ->
    {error, {unexpected_string_end, incr_anno_pos(Len, Anno)}}.

maybe_prepend_text_token(Bin, Len, #{position := Pos} = Anno, Tokens) ->
    case string:trim(binary_part(Bin, Pos, Len)) of
        <<>> ->
            Tokens;
        Text ->
            [{text, token_anno(Anno), Text} | Tokens]
    end.

token_anno(Anno) ->
    {token_source(Anno), token_location(Anno)}.

token_source(#{module := Mod, function := Fun})
    when Mod =/= undefined,
         Fun =/= undefined ->
    {Mod, Fun};
token_source(#{file := File}) ->
    {file, File}.

token_location(#{line := Ln, column := Col}) ->
    {Ln, Col}.

raise({Reason, Anno}) ->
    error(Reason, none, [{error_info, error_info(Anno)}]).

error_info(#{module := Mod, function := Fun} = Anno)
    when Mod =/= undefined,
         Fun =/= undefined ->
    maps:with([module, function, line, column], Anno);
error_info(Anno) ->
    maps:with([file, line, column], Anno).

%% Anno.

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

new_line(#{line := Ln, first_column := Col} = Anno) ->
    Anno#{line => Ln + 1, column => Col}.

incr_anno_col(N, #{column := Col} = Anno) ->
    Anno#{column => Col + N}.

incr_anno_pos(N, #{position := Pos} = Anno) ->
    Anno#{position => Pos + N}.

%% --------------------------------------------------------------------
%% EUnit tests.
%% --------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-define(ANNO, new_anno(#{file => ?FILE})).

scan_test() ->
    [
        ?assertMatch([
            {text, _, <<"begin">>},
            {comment, _, <<"% comment ">>},
            {expr, _, <<"foo">>},
            {text, _, <<"end">>}
        ], scan(~"""
            begin
            {% comment }
            {foo}
            <div>bar</div>
            <input id="input" class='input' hidden/>
            end
            """, ?ANNO)),
        ?assertError(badexpr, scan(<<"foo{@}bar">>, ?ANNO))
    ].

expr_category_test() ->
    [
        ?assertEqual({ok, expr}, expr_category("foo")),
        ?assertEqual({ok, comment}, expr_category("%foo")),
        ?assertEqual(error, expr_category("@"))
    ].

scan_expr_end_test() ->
    [
        ?assertMatch({ok, {10, 1, _, <<"baz">>}},
                     scan_expr_end(<<"{foo\"bar\"}}baz">>, 0, 0, ?ANNO)),
        ?assertMatch({ok, {8, 1, _, <<"baz">>}},
                     scan_expr_end(<<"foo\"bar\"}baz">>, 0, 0, ?ANNO)),
        ?assertMatch({error, {unexpected_expr_end, _}},
                     scan_expr_end(<<"foo\"bar\"baz">>, 0, 0, ?ANNO))
    ].

scan_single_quoted_string_test() ->
    [
        ?assertMatch({ok, {8, 1, _, <<"baz">>}},
                     scan_single_quoted_string(<<"foo\\'bar'baz">>, 0, ?ANNO)),
        ?assertMatch({error, {unexpected_string_end, _}},
                     scan_single_quoted_string(<<"foo">>, 0, ?ANNO))
    ].

scan_double_quoted_string_test() ->
    [
        ?assertMatch({ok, {8, 1, _, <<"baz">>}},
                     scan_double_quoted_string(<<"foo\\\"bar\"baz">>, 0, ?ANNO)),
        ?assertMatch({error, {unexpected_string_end, _}},
                     scan_double_quoted_string(<<"foo">>, 0, ?ANNO))
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

new_line_test() ->
    #{line := Ln, column := Col} = new_line(new_anno(#{file => ?FILE, column => 2})),
    ?assertEqual({2, 1}, {Ln, Col}).

incr_anno_col_test() ->
    #{column := Col} = incr_anno_col(1, new_anno(#{file => ?FILE})),
    ?assertEqual(2, Col).

incr_anno_pos_test() ->
    #{position := Pos} = incr_anno_pos(1, new_anno(#{file => ?FILE})),
    ?assertEqual(1, Pos).

-endif.

