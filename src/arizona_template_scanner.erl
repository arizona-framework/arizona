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
-export_type([location/0]).
-export_type([line/0]).
-export_type([column/0]).

-opaque anno() :: #{
    source => {function, {module(), atom()}} | {file, binary()} | none,
    line => line(),
    column => column(),
    first_column => non_neg_integer(),
    position => non_neg_integer()
}.
-type anno_options() :: #{
    source := {module(), atom()} | file:filename_all() | none,
    line := line(),
    column := column(),
    first_column := non_neg_integer(),
    position := non_neg_integer()
}.
-type token_anno() :: location().
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

scan(Rest0, Bin, Len, Anno0) ->
    case skip_trailing_spaces(Rest0, Anno0) of
        {true, {Rest, Anno}} ->
            scan(Rest, Bin, Len, Anno, Anno);
        false ->
            scan(Rest0, Bin, Len, Anno0, Anno0)
    end.

skip_trailing_spaces(<<$\s, Rest/binary>>, Anno) ->
    skip_trailing_spaces(Rest, incr_anno_pos(1, Anno));
skip_trailing_spaces(<<$\r, $\n, Rest/binary>>, Anno) ->
    {true, {Rest, new_line(incr_anno_pos(2, Anno))}};
skip_trailing_spaces(<<$\r, Rest/binary>>, Anno) ->
    {true, {Rest, new_line(incr_anno_pos(1, Anno))}};
skip_trailing_spaces(<<$\n, Rest/binary>>, Anno) ->
    {true, {Rest, new_line(incr_anno_pos(1, Anno))}};
skip_trailing_spaces(_Rest, _Anno) ->
    false.

scan(<<${, Rest/binary>>, Bin, Len, TxtAnno, Anno) ->
    maybe_prepend_text_token(Bin, Len, TxtAnno,
        scan_expr(Rest, Bin, 1, incr_anno_pos(Len + 1, Anno)));
scan(<<$<, $/, Rest/binary>>, Bin, Len, TxtAnno, Anno) ->
    maybe_prepend_text_token(Bin, Len, TxtAnno,
        scan_closing_tag(Rest, Bin, 2, incr_anno_pos(Len + 2, Anno)));
scan(<<$<, Rest/binary>>, Bin, Len, TxtAnno, Anno) ->
    maybe_prepend_text_token(Bin, Len, TxtAnno,
        scan_tag(Rest, Bin, 1, incr_anno_pos(Len + 1, Anno)));
scan(<<$\r, $\n, Rest/binary>>, Bin, Len, TxtAnno, Anno) ->
    scan(Rest, Bin, Len + 2, TxtAnno, new_line(Anno));
scan(<<$\r, Rest/binary>>, Bin, Len, TxtAnno, Anno) ->
    scan(Rest, Bin, Len + 1, TxtAnno, new_line(Anno));
scan(<<$\n, Rest/binary>>, Bin, Len, TxtAnno, Anno) ->
    scan(Rest, Bin, Len + 1, TxtAnno, new_line(Anno));
scan(<<_, Rest/binary>>, Bin, Len, TxtAnno, Anno) ->
    scan(Rest, Bin, Len + 1, TxtAnno, incr_anno_col(1, Anno));
scan(<<>>, Bin, Len, TxtAnno, _Anno) ->
    maybe_prepend_text_token(Bin, Len, TxtAnno, []).

scan_expr(Rest0, Bin, StartMarkerLen, ExprAnno) ->
    case scan_expr_end(Rest0, 0, 0, ExprAnno) of
        {ok, {Len, EndMarkerLen, Anno0, Rest}} ->
            Pos = maps:get(position, ExprAnno),
            Expr = binary_part(Bin, Pos, Len),
            case expr_category(Expr) of
                {ok, Category} ->
                    Anno = incr_anno_col(StartMarkerLen, incr_anno_pos(Len + EndMarkerLen, Anno0)),
                    [{Category, token_anno(ExprAnno), Expr} | scan(Rest, Bin, 0, Anno)];
                error ->
                    raise({badexpr, Anno0})
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

scan_tag(Rest0, Bin, StartMarkerLen, TagAnno) ->
    case scan_tag_name(Rest0, 0, TagAnno) of
        {ok, {Len, Anno0, Rest}} when Len > 0 ->
            Pos = maps:get(position, TagAnno),
            TagName = binary_part(Bin, Pos, Len),
            Anno = incr_anno_col(StartMarkerLen, incr_anno_pos(Len, Anno0)),
            [{open_tag, token_anno(TagAnno), TagName} | scan_tag_attrs(Rest, Bin, Anno)];
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
scan_tag_attrs(Rest0, Bin, KeyAnno) ->
    case scan_tag_attr_key(Rest0, 0, KeyAnno) of
        {ok, {KeyLen, NextAnno, Rest1}} when KeyLen > 0 ->
            KeyPos = maps:get(position, KeyAnno),
            Key = binary_part(Bin, KeyPos, KeyLen),
            ValAnno = incr_anno_pos(KeyLen, NextAnno),
            case scan_tag_attr_value(Rest1, Bin, ValAnno) of
                {ok, {Value, MarkerLen, Anno, Rest}} ->
                    [{attr_key, token_anno(KeyAnno), Key},
                     {attr_value, token_anno(incr_anno_col(MarkerLen, ValAnno)), Value}
                     | scan_tag_attrs(Rest, Bin, Anno)];
                {none, {Anno, Rest}} ->
                    [{bool_attr, token_anno(KeyAnno), Key}
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
            Anno = incr_anno_col(MarkerLen, incr_anno_pos(Len + MarkerLen, TxtAnno)),
            {ok, {{text, Txt}, 1, Anno, Rest}};
        {error, {Reason, Anno}} ->
            {error, {Reason, Anno}}
    end;
scan_tag_attr_value(<<$=, $", Rest0/binary>>, Bin, KeyAnno) ->
    ValAnno = incr_anno_col(2, incr_anno_pos(2, KeyAnno)),
    case scan_double_quoted_string(Rest0, 0, ValAnno) of
        {ok, {Len, MarkerLen, TxtAnno, Rest}} ->
            Pos = maps:get(position, ValAnno),
            Txt = binary_part(Bin, Pos, Len),
            Anno = incr_anno_col(MarkerLen, incr_anno_pos(Len + MarkerLen, TxtAnno)),
            {ok, {{text, Txt}, 1, Anno, Rest}};
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
                    Anno = incr_anno_col(MarkerLen, incr_anno_pos(Len + MarkerLen, ExprAnno)),
                    {ok, {{expr, Expr}, 1, Anno, Rest}};
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

scan_closing_tag(Rest0, Bin, StartMarkerLen, TagAnno) ->
    case scan_closing_tag_name(Rest0, 0, TagAnno) of
        {ok, {TagNameLen, TagNameAnno, Rest1}} when TagNameLen > 0 ->
            TagEndAnno = incr_anno_pos(TagNameLen, TagNameAnno),
            case scan_closing_tag_end(Rest1, 0, TagEndAnno) of
                {ok, {Anno0, Rest}} ->
                    Pos = maps:get(position, TagAnno),
                    TagName = binary_part(Bin, Pos, TagNameLen),
                    Anno = incr_anno_col(StartMarkerLen, Anno0),
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
        Txt ->
            [{text, token_anno(Anno), Txt} | Tokens]
    end.

token_anno(Anno) ->
    token_location(Anno).

token_location(#{line := Ln, column := Col}) ->
    {Ln, Col}.

raise({Reason, Anno}) ->
    error(Reason, none, [{error_info, error_info(Anno)}]).

error_info(#{source := Source} = Anno) ->
    maps:merge(error_source_info(Source),
               maps:with([line, column], Anno)).

error_source_info({function, {Mod, Fun}}) ->
    #{module => Mod, funtion => Fun};
error_source_info({file, File}) ->
    #{file => File};
error_source_info(none) ->
    #{}.

%% Anno.

default_anno() ->
    #{
        source => none,
        line => 1,
        column => 1,
        first_column => 1,
        position => 0
    }.

normalize_anno_value(source, {Mod, Fun}) when is_atom(Mod), is_atom(Fun) ->
    {function, {Mod, Fun}};
normalize_anno_value(source, File) when is_list(File) ->
    {file, iolist_to_binary(File)};
normalize_anno_value(source, File) when is_binary(File) ->
    {file, File};
normalize_anno_value(source, none) ->
    none;
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
-define(ANNO, new_anno(#{})).

scan_test() ->
    [
        ?assertMatch([
            {open_tag, {1, 1}, <<"!DOCTYPE">>},
            {bool_attr, {1, 11}, <<"html">>},
            {close_tag, {1, 15}, nonvoid},
            {open_tag, {2, 1}, <<"html">>},
            {attr_key, {2, 7}, <<"lang">>},
            {attr_value, {2, 12}, {text, <<"en">>}},
            {close_tag, {2, 16}, nonvoid},
            {open_tag, {3, 1}, <<"head">>},
            {close_tag, {3, 6}, nonvoid},
            {open_tag, {4, 5}, <<"meta">>},
            {attr_key, {4, 11}, <<"charset">>},
            {attr_value, {4, 19}, {text, <<"UTF-8">>}},
            {close_tag, {4, 26}, nonvoid},
            {open_tag, {5, 5}, <<"meta">>},
            {attr_key, {5, 11}, <<"http-equiv">>},
            {attr_value, {5, 22}, {text, <<"X-UA-Compatible">>}},
            {attr_key, {5, 40}, <<"content">>},
            {attr_value, {5, 48}, {text, <<"IE=edge">>}},
            {close_tag, {5, 57}, nonvoid},
            {open_tag, {6, 5}, <<"meta">>},
            {attr_key, {6, 11}, <<"name">>},
            {attr_value, {6, 16}, {text, <<"viewport">>}},
            {attr_key, {6, 27}, <<"content">>},
            {attr_value, {6, 35},
                        {text, <<"width=device-width, initial-scale=1.0">>}},
            {close_tag, {6, 74}, nonvoid},
            {open_tag, {7, 5}, <<"title">>},
            {close_tag, {7, 11}, nonvoid},
            {expr, {7, 12}, <<"_@title">>},
            {closing_tag, {7, 21}, <<"title">>},
            {open_tag, {8, 5}, <<"script">>},
            {attr_key, {8, 13}, <<"src">>},
            {attr_value, {8, 17}, {text, <<"assets/js/main.js">>}},
            {close_tag, {8, 36}, nonvoid},
            {closing_tag, {8, 37}, <<"script">>},
            {closing_tag, {9, 1}, <<"head">>},
            {open_tag, {10, 1}, <<"body">>},
            {close_tag, {10, 6}, nonvoid},
            {open_tag, {11, 5}, <<".counter">>},
            {attr_key, {12, 9}, <<"count">>},
            {attr_value, {12, 15}, {expr, <<"_@count">>}},
            {attr_key, {13, 9}, <<"btn_text">>},
            {attr_value, {13, 18}, {text, <<"Increment">>}},
            {attr_key, {14, 9}, <<"event">>},
            {attr_value, {14, 15}, {text, <<"incr">>}},
            {close_tag, {15, 5}, void},
            {open_tag, {16, 5}, <<".counter">>},
            {attr_key, {17, 9}, <<"count">>},
            {attr_value, {17, 15}, {expr, <<"99">>}},
            {attr_key, {18, 9}, <<"btn_text">>},
            {attr_value, {18, 18}, {text, <<"Decrement">>}},
            {attr_key, {19, 9}, <<"event">>},
            {attr_value, {19, 15}, {text, <<"decr">>}},
            {close_tag, {20, 5}, void},
            {closing_tag, {21, 1}, <<"body">>},
            {closing_tag, {22, 1}, <<"html">>}
        ], scan(~"""
            <!DOCTYPE html>
            <html lang="en">
            <head>
                <meta charset="UTF-8">
                <meta http-equiv="X-UA-Compatible" content="IE=edge">
                <meta name="viewport" content="width=device-width, initial-scale=1.0">
                <title>{_@title}</title>
                <script src="assets/js/main.js"></script>
            </head>
            <body>
                <.counter
                    count={_@count}
                    btn_text="Increment"
                    event="incr"
                />
                <.counter
                    count={99}
                    btn_text="Decrement"
                    event="decr"
                />
            </body>
            </html>
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
        ?assertError(function_clause, new_anno(#{source => -1})),
        ?assertError(function_clause, new_anno(#{line => -1})),
        ?assertError(function_clause, new_anno(#{column => -1})),
        ?assertError(function_clause, new_anno(#{first_column => -1})),
        ?assertError(function_clause, new_anno(#{position => -10})),
        ?assertEqual(#{
            source => none,
            line => 1,
            column => 1,
            first_column => 1,
            position => 0
        }, ?ANNO)
    ].

new_line_test() ->
    #{line := Ln, column := Col} = new_line(new_anno(#{column => 2})),
    ?assertEqual({2, 1}, {Ln, Col}).

incr_anno_col_test() ->
    #{column := Col} = incr_anno_col(1, new_anno(#{})),
    ?assertEqual(2, Col).

incr_anno_pos_test() ->
    #{position := Pos} = incr_anno_pos(1, new_anno(#{})),
    ?assertEqual(1, Pos).

-endif.

