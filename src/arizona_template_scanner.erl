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
-export([scan/2]). -ignore_xref([scan/2]).

%% Types
-export_type([options/0]).
-export_type([token/0]).
-export_type([anno/0]).
-export_type([result/0]).

-type state() :: #{
    source => {function, {module(), atom()}} | {file, binary()} | none,
    line => line(),
    column => column(),
    first_column => non_neg_integer(),
    position => non_neg_integer()
}.
-type token() :: {open_tag, anno(), binary()}
               | {attr_key, anno(), binary()}
               | {attr_value, anno(), {text, binary()} | {expr, binary()}}
               | {bool_attr, anno(), binary()}
               | {close_tag, anno(), void | nonvoid}
               | {closing_tag, anno(), binary()}
               | {text, anno(), binary()}
               | {expr, anno(), binary()}.
-type anno() :: location().
-type location() :: {line(), column()}.
-type line() :: non_neg_integer().
-type column() :: non_neg_integer().
-type options() :: #{
    source := {module(), atom()} | file:filename_all() | none,
    line := line(),
    column := column(),
    first_column := non_neg_integer(),
    position := non_neg_integer()
}.
-type result() :: [token()].

%% --------------------------------------------------------------------
%% API funtions.
%% --------------------------------------------------------------------

-spec scan(Bin, Opts) -> Result
    when Bin :: binary(),
         Opts :: options(),
         Result :: result().
scan(Bin, Opts) when is_binary(Bin), is_map(Opts) ->
    scan(Bin, Bin, 0, new_state(Opts)).

%% --------------------------------------------------------------------
%% Internal funtions.
%% --------------------------------------------------------------------

-spec scan(Rest, Bin, Len, State) -> Result
    when Rest :: binary(),
         Bin :: binary(),
         Len :: non_neg_integer(),
         State :: state(),
         Result :: result().
scan(Rest0, Bin, Len, State0) ->
    case skip_trailing_spaces(Rest0, State0) of
        {true, {Rest, State}} ->
            scan(Rest, Bin, Len, State, State);
        false ->
            scan(Rest0, Bin, Len, State0, State0)
    end.

skip_trailing_spaces(<<$\s, Rest/binary>>, State) ->
    skip_trailing_spaces(Rest, incr_pos(1, State));
skip_trailing_spaces(<<$\r, $\n, Rest/binary>>, State) ->
    {true, {Rest, new_line(incr_pos(2, State))}};
skip_trailing_spaces(<<$\r, Rest/binary>>, State) ->
    {true, {Rest, new_line(incr_pos(1, State))}};
skip_trailing_spaces(<<$\n, Rest/binary>>, State) ->
    {true, {Rest, new_line(incr_pos(1, State))}};
skip_trailing_spaces(_Rest, _State) ->
    false.

scan(<<${, Rest/binary>>, Bin, Len, TxtState, State) ->
    maybe_prepend_text_token(Bin, Len, TxtState,
        scan_expr(Rest, Bin, 1, incr_pos(Len + 1, State)));
scan(<<$<, $/, Rest/binary>>, Bin, Len, TxtState, State) ->
    maybe_prepend_text_token(Bin, Len, TxtState,
        scan_closing_tag(Rest, Bin, 2, incr_pos(Len + 2, State)));
scan(<<$<, Rest/binary>>, Bin, Len, TxtState, State) ->
    maybe_prepend_text_token(Bin, Len, TxtState,
        scan_tag(Rest, Bin, 1, incr_pos(Len + 1, State)));
scan(<<$\r, $\n, Rest/binary>>, Bin, Len, TxtState, State) ->
    scan(Rest, Bin, Len + 2, TxtState, new_line(State));
scan(<<$\r, Rest/binary>>, Bin, Len, TxtState, State) ->
    scan(Rest, Bin, Len + 1, TxtState, new_line(State));
scan(<<$\n, Rest/binary>>, Bin, Len, TxtState, State) ->
    scan(Rest, Bin, Len + 1, TxtState, new_line(State));
scan(<<_, Rest/binary>>, Bin, Len, TxtState, State) ->
    scan(Rest, Bin, Len + 1, TxtState, incr_col(1, State));
scan(<<>>, Bin, Len, TxtState, _State) ->
    maybe_prepend_text_token(Bin, Len, TxtState, []).

scan_expr(Rest0, Bin, StartMarkerLen, ExprState) ->
    case scan_expr_end(Rest0, 0, 0, ExprState) of
        {ok, {Len, EndMarkerLen, State0, Rest}} ->
            Pos = maps:get(position, ExprState),
            Expr = binary_part(Bin, Pos, Len),
            case expr_category(Expr) of
                {ok, Category} ->
                    State = incr_col(StartMarkerLen, incr_pos(Len + EndMarkerLen, State0)),
                    [{Category, anno(ExprState), Expr} | scan(Rest, Bin, 0, State)];
                error ->
                    raise({badexpr, State0})
            end;
        {error, Reason} ->
            raise(Reason)
    end.

scan_expr_end(<<$}, Rest/binary>>, 0, Len, State) ->
    {ok, {Len, _MarkerLen = 1, incr_col(1, State), Rest}};
scan_expr_end(<<$}, Rest/binary>>, Depth, Len, State) ->
    scan_expr_end(Rest, Depth - 1, Len + 1, incr_col(1, State));
scan_expr_end(<<${, Rest/binary>>, Depth, Len, State) ->
    scan_expr_end(Rest, Depth + 1, Len + 1, incr_col(1, State));
scan_expr_end(<<$\r, $\n, Rest/binary>>, Depth, Len, State) ->
    scan_expr_end(Rest, Depth, Len + 2, new_line(State));
scan_expr_end(<<$\r, Rest/binary>>, Depth, Len, State) ->
    scan_expr_end(Rest, Depth, Len + 1, new_line(State));
scan_expr_end(<<$\n, Rest/binary>>, Depth, Len, State) ->
    scan_expr_end(Rest, Depth, Len + 1, new_line(State));
scan_expr_end(<<_, Rest/binary>>, Depth, Len, State) ->
    scan_expr_end(Rest, Depth, Len + 1, incr_col(1, State));
scan_expr_end(<<>>, _Depth, Len, State) ->
    {error, {unexpected_expr_end, incr_pos(Len, State)}}.

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

scan_tag(Rest0, Bin, StartMarkerLen, TagState) ->
    case scan_tag_name(Rest0, 0, TagState) of
        {ok, {Len, State0, Rest}} when Len > 0 ->
            Pos = maps:get(position, TagState),
            TagName = binary_part(Bin, Pos, Len),
            State = incr_col(StartMarkerLen, incr_pos(Len, State0)),
            [{open_tag, anno(TagState), TagName} | scan_tag_attrs(Rest, Bin, State)];
        {error, Reason} ->
            raise(Reason)
    end.

scan_tag_name(<<$/, $>, _/binary>> = Rest, Len, State) ->
    {ok, {Len, State, Rest}};
scan_tag_name(<<$>, _/binary>> = Rest, Len, State) ->
    {ok, {Len, State, Rest}};
scan_tag_name(<<$\s, _/binary>> = Rest, Len, State) ->
    {ok, {Len, State, Rest}};
scan_tag_name(<<$\r, _/binary>> = Rest, Len, State) ->
    {ok, {Len, State, Rest}};
scan_tag_name(<<$\n, _/binary>> = Rest, Len, State) ->
    {ok, {Len, State, Rest}};
scan_tag_name(<<_, Rest/binary>>, Len, State) ->
    scan_tag_name(Rest, Len + 1, incr_col(1, State));
scan_tag_name(<<>>, Len, State) ->
    {error, {unexpected_tag_end, incr_pos(Len, State)}}.

scan_tag_attrs(<<$/, $>, Rest/binary>>, Bin, State) ->
    [{close_tag, anno(State), void}
     | scan(Rest, Bin, 0, incr_col(2, incr_pos(2, State)))];
scan_tag_attrs(<<$>, Rest/binary>>, Bin, State) ->
    [{close_tag, anno(State), nonvoid}
     | scan(Rest, Bin, 0, incr_col(1, incr_pos(1, State)))];
scan_tag_attrs(<<$\s, Rest/binary>>, Bin, State) ->
    scan_tag_attrs(Rest, Bin, incr_col(1, incr_pos(1, State)));
scan_tag_attrs(<<$\r, $\n, Rest/binary>>, Bin, State) ->
    scan_tag_attrs(Rest, Bin, new_line(incr_pos(2, State)));
scan_tag_attrs(<<$\r, Rest/binary>>, Bin, State) ->
    scan_tag_attrs(Rest, Bin, new_line(incr_pos(1, State)));
scan_tag_attrs(<<$\n, Rest/binary>>, Bin, State) ->
    scan_tag_attrs(Rest, Bin, new_line(incr_pos(1, State)));
scan_tag_attrs(Rest0, Bin, KeyState) ->
    case scan_tag_attr_key(Rest0, 0, KeyState) of
        {ok, {KeyLen, NextState, Rest1}} when KeyLen > 0 ->
            KeyPos = maps:get(position, KeyState),
            Key = binary_part(Bin, KeyPos, KeyLen),
            ValState = incr_pos(KeyLen, NextState),
            case scan_tag_attr_value(Rest1, Bin, ValState) of
                {ok, {Value, MarkerLen, State, Rest}} ->
                    [{attr_key, anno(KeyState), Key},
                     {attr_value, anno(incr_col(MarkerLen, ValState)), Value}
                     | scan_tag_attrs(Rest, Bin, State)];
                {none, {State, Rest}} ->
                    [{bool_attr, anno(KeyState), Key}
                     | scan_tag_attrs(Rest, Bin, State)];
                {error, Reason} ->
                    raise(Reason)
            end;
        {error, Reason} ->
            raise(Reason)
    end.

scan_tag_attr_key(<<$=, _/binary>> = Rest, Len, State) ->
    {ok, {Len, State, Rest}};
scan_tag_attr_key(<<$/, $>, _/binary>> = Rest, Len, State) ->
    {ok, {Len, State, Rest}};
scan_tag_attr_key(<<$>, _/binary>> = Rest, Len, State) ->
    {ok, {Len, State, Rest}};
scan_tag_attr_key(<<$\s, _/binary>> = Rest, Len, State) ->
    {ok, {Len, State, Rest}};
scan_tag_attr_key(<<$\r, _/binary>> = Rest, Len, State) ->
    {ok, {Len, State, Rest}};
scan_tag_attr_key(<<$\n, _/binary>> = Rest, Len, State) ->
    {ok, {Len, State, Rest}};
scan_tag_attr_key(<<_, Rest/binary>>, Len, State) ->
    scan_tag_attr_key(Rest, Len + 1, incr_col(1, State));
scan_tag_attr_key(<<>>, Len, State) ->
    {error, {unexpected_tag_end, incr_pos(Len, State)}}.

scan_tag_attr_value(<<$=, $', Rest0/binary>>, Bin, KeyState) ->
    ValState = incr_col(2, incr_pos(2, KeyState)),
    case scan_single_quoted_string(Rest0, 0, ValState) of
        {ok, {Len, MarkerLen, TxtState, Rest}} ->
            Pos = maps:get(position, ValState),
            Txt = binary_part(Bin, Pos, Len),
            State = incr_col(MarkerLen, incr_pos(Len + MarkerLen, TxtState)),
            {ok, {{text, Txt}, 1, State, Rest}};
        {error, {Reason, State}} ->
            {error, {Reason, State}}
    end;
scan_tag_attr_value(<<$=, $", Rest0/binary>>, Bin, KeyState) ->
    ValState = incr_col(2, incr_pos(2, KeyState)),
    case scan_double_quoted_string(Rest0, 0, ValState) of
        {ok, {Len, MarkerLen, TxtState, Rest}} ->
            Pos = maps:get(position, ValState),
            Txt = binary_part(Bin, Pos, Len),
            State = incr_col(MarkerLen, incr_pos(Len + MarkerLen, TxtState)),
            {ok, {{text, Txt}, 1, State, Rest}};
        {error, {Reason, State}} ->
            {error, {Reason, State}}
    end;
scan_tag_attr_value(<<$=, ${, Rest0/binary>>, Bin, KeyState) ->
    ValState = incr_col(2, incr_pos(2, KeyState)),
    case scan_expr_end(Rest0, 0, 0, ValState) of
        {ok, {Len, MarkerLen, ExprState, Rest}} ->
            Pos = maps:get(position, ValState),
            Expr = binary_part(Bin, Pos, Len),
            case expr_category(Expr) of
                {ok, expr} ->
                    State = incr_col(MarkerLen, incr_pos(Len + MarkerLen, ExprState)),
                    {ok, {{expr, Expr}, 1, State, Rest}};
                {ok, comment} ->
                    % Comments are not allowed in attributes, e.g.:
                    % > id={% comment }
                    {error, {unexpected_comment, ExprState}};
                error ->
                    {error, {badexpr, ExprState}}
            end;
        {error, {Reason, State}} ->
            {error, {Reason, State}}
    end;
scan_tag_attr_value(Rest, _Bin, State) ->
    {none, {State, Rest}}.

scan_closing_tag(Rest0, Bin, StartMarkerLen, TagState) ->
    case scan_closing_tag_name(Rest0, 0, TagState) of
        {ok, {TagNameLen, TagNameState, Rest1}} when TagNameLen > 0 ->
            TagEndState = incr_pos(TagNameLen, TagNameState),
            case scan_closing_tag_end(Rest1, 0, TagEndState) of
                {ok, {State0, Rest}} ->
                    Pos = maps:get(position, TagState),
                    TagName = binary_part(Bin, Pos, TagNameLen),
                    State = incr_col(StartMarkerLen, State0),
                    [{closing_tag, anno(TagState), TagName}
                     | scan(Rest, Bin, 0, State)];
                {error, Reason} ->
                    raise(Reason)
            end;
        {error, Reason} ->
            raise(Reason)
    end.

scan_closing_tag_name(<<$>, _/binary>> = Rest, Len, State) ->
    {ok, {Len, State, Rest}};
scan_closing_tag_name(<<$\s, _/binary>> = Rest, Len, State) ->
    {ok, {Len, State, Rest}};
scan_closing_tag_name(<<$\r, _/binary>> = Rest, Len, State) ->
    {ok, {Len, State, Rest}};
scan_closing_tag_name(<<$\n, _/binary>> = Rest, Len, State) ->
    {ok, {Len, State, Rest}};
scan_closing_tag_name(<<_, Rest/binary>>, Len, State) ->
    scan_closing_tag_name(Rest, Len + 1, incr_col(1, State));
scan_closing_tag_name(<<>>, Len, State) ->
    {error, {unexpected_tag_end, incr_pos(Len, State)}}.

scan_closing_tag_end(<<$>, Rest/binary>>, Len, State) ->
    {ok, {incr_col(1, incr_pos(Len + 1, State)), Rest}};
scan_closing_tag_end(<<$\r, $\n, Rest/binary>>, Len, State) ->
    scan_closing_tag_end(Rest, Len + 2, new_line(State));
scan_closing_tag_end(<<$\r, Rest/binary>>, Len, State) ->
    scan_closing_tag_end(Rest, Len + 1, new_line(State));
scan_closing_tag_end(<<$\n, Rest/binary>>, Len, State) ->
    scan_closing_tag_end(Rest, Len + 1, new_line(State));
scan_closing_tag_end(<<_, Rest/binary>>, Len, State) ->
    scan_closing_tag_end(Rest, Len + 1, incr_col(1, State));
scan_closing_tag_end(<<>>, Len, State) ->
    {error, {unexpected_tag_end, incr_pos(Len, State)}}.

scan_single_quoted_string(<<$\\, $', Rest/binary>>, Len, State) ->
    scan_single_quoted_string(Rest, Len + 2, incr_col(2, State));
scan_single_quoted_string(<<$', Rest/binary>>, Len, State) ->
    {ok, {Len, _MarkerLen = 1, State, Rest}};
scan_single_quoted_string(<<$\r, $\n, Rest/binary>>, Len, State) ->
    scan_single_quoted_string(Rest, Len + 2, new_line(State));
scan_single_quoted_string(<<$\r, Rest/binary>>, Len, State) ->
    scan_single_quoted_string(Rest, Len + 1, new_line(State));
scan_single_quoted_string(<<$\n, Rest/binary>>, Len, State) ->
    scan_single_quoted_string(Rest, Len + 1, new_line(State));
scan_single_quoted_string(<<_, Rest/binary>>, Len, State) ->
    scan_single_quoted_string(Rest, Len + 1, incr_col(1, State));
scan_single_quoted_string(<<>>, Len, State) ->
    {error, {unexpected_string_end, incr_pos(Len, State)}}.

scan_double_quoted_string(<<$\\, $", Rest/binary>>, Len, State) ->
    scan_double_quoted_string(Rest, Len + 2, incr_col(2, State));
scan_double_quoted_string(<<$", Rest/binary>>, Len, State) ->
    {ok, {Len, _MarkerLen = 1, State, Rest}};
scan_double_quoted_string(<<$\r, $\n, Rest/binary>>, Len, State) ->
    scan_double_quoted_string(Rest, Len + 2, new_line(State));
scan_double_quoted_string(<<$\r, Rest/binary>>, Len, State) ->
    scan_double_quoted_string(Rest, Len + 1, new_line(State));
scan_double_quoted_string(<<$\n, Rest/binary>>, Len, State) ->
    scan_double_quoted_string(Rest, Len + 1, new_line(State));
scan_double_quoted_string(<<_, Rest/binary>>, Len, State) ->
    scan_double_quoted_string(Rest, Len + 1, incr_col(1, State));
scan_double_quoted_string(<<>>, Len, State) ->
    {error, {unexpected_string_end, incr_pos(Len, State)}}.

maybe_prepend_text_token(Bin, Len, #{position := Pos} = State, Tokens) ->
    case string:trim(binary_part(Bin, Pos, Len)) of
        <<>> ->
            Tokens;
        Txt ->
            [{text, anno(State), Txt} | Tokens]
    end.

anno(#{line := Ln, column := Col}) ->
    {Ln, Col}.

raise({Reason, State}) ->
    error(Reason, none, [{error_info, error_info(State)}]).

error_info(#{source := Source} = State) ->
    maps:merge(error_source_info(Source),
               maps:with([line, column], State)).

error_source_info({function, {Mod, Fun}}) ->
    #{module => Mod, funtion => Fun};
error_source_info({file, File}) ->
    #{file => File};
error_source_info(none) ->
    #{}.

%% State.

new_state(Opts) ->
    State = maps:merge(default_state(), Opts),
    maps:map(fun normalize_state_value/2, State).

default_state() ->
    #{
        source => none,
        line => 1,
        column => 1,
        first_column => 1,
        position => 0
    }.

normalize_state_value(source, {Mod, Fun}) when is_atom(Mod), is_atom(Fun) ->
    {function, {Mod, Fun}};
normalize_state_value(source, File) when is_list(File) ->
    {file, iolist_to_binary(File)};
normalize_state_value(source, File) when is_binary(File) ->
    {file, File};
normalize_state_value(source, none) ->
    none;
normalize_state_value(line, Ln) when is_integer(Ln), Ln >= 0 ->
    Ln;
normalize_state_value(column, Col) when is_integer(Col), Col >= 0 ->
    Col;
normalize_state_value(first_column, Col) when is_integer(Col), Col >= 0 ->
    Col;
normalize_state_value(position, Pos) when is_integer(Pos), Pos >= 0 ->
    Pos.

new_line(#{line := Ln, first_column := Col} = State) ->
    State#{line => Ln + 1, column => Col}.

incr_col(N, #{column := Col} = State) ->
    State#{column => Col + N}.

incr_pos(N, #{position := Pos} = State) ->
    State#{position => Pos + N}.

%% --------------------------------------------------------------------
%% EUnit tests.
%% --------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-define(STATE, new_state(#{})).

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
            {open_tag, {11, 5}, <<"h1">>},
            {close_tag, {11, 8}, nonvoid},
            {text, {11, 9}, <<"Arizona Counter">>},
            {closing_tag, {11, 24}, <<"h1">>},
            {open_tag, {12, 5}, <<".counter">>},
            {attr_key, {13, 9}, <<"count">>},
            {attr_value, {13, 15}, {expr, <<"_@count">>}},
            {attr_key, {14, 9}, <<"btn_text">>},
            {attr_value, {14, 18}, {text, <<"Increment">>}},
            {attr_key, {15, 9}, <<"event">>},
            {attr_value, {15, 15}, {text, <<"incr">>}},
            {close_tag, {16, 5}, void},
            {open_tag, {17, 5}, <<".counter">>},
            {attr_key, {18, 9}, <<"count">>},
            {attr_value, {18, 15}, {expr, <<"99">>}},
            {attr_key, {19, 9}, <<"btn_text">>},
            {attr_value, {19, 18}, {text, <<"Decrement">>}},
            {attr_key, {20, 9}, <<"event">>},
            {attr_value, {20, 15}, {text, <<"decr">>}},
            {close_tag, {21, 5}, void},
            {closing_tag, {22, 1}, <<"body">>},
            {closing_tag, {23, 1}, <<"html">>}
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
                <h1>Arizona Counter</h1>
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
            """, ?STATE)),
        ?assertError(badexpr, scan(<<"foo{@}bar">>, ?STATE))
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
                     scan_expr_end(<<"{foo\"bar\"}}baz">>, 0, 0, ?STATE)),
        ?assertMatch({ok, {8, 1, _, <<"baz">>}},
                     scan_expr_end(<<"foo\"bar\"}baz">>, 0, 0, ?STATE)),
        ?assertMatch({error, {unexpected_expr_end, _}},
                     scan_expr_end(<<"foo\"bar\"baz">>, 0, 0, ?STATE))
    ].

scan_single_quoted_string_test() ->
    [
        ?assertMatch({ok, {8, 1, _, <<"baz">>}},
                     scan_single_quoted_string(<<"foo\\'bar'baz">>, 0, ?STATE)),
        ?assertMatch({error, {unexpected_string_end, _}},
                     scan_single_quoted_string(<<"foo">>, 0, ?STATE))
    ].

scan_double_quoted_string_test() ->
    [
        ?assertMatch({ok, {8, 1, _, <<"baz">>}},
                     scan_double_quoted_string(<<"foo\\\"bar\"baz">>, 0, ?STATE)),
        ?assertMatch({error, {unexpected_string_end, _}},
                     scan_double_quoted_string(<<"foo">>, 0, ?STATE))
    ].

new_state_test() ->
    [
        ?assertError(function_clause, new_state(#{source => -1})),
        ?assertError(function_clause, new_state(#{line => -1})),
        ?assertError(function_clause, new_state(#{column => -1})),
        ?assertError(function_clause, new_state(#{first_column => -1})),
        ?assertError(function_clause, new_state(#{position => -10})),
        ?assertEqual(#{
            source => none,
            line => 1,
            column => 1,
            first_column => 1,
            position => 0
        }, ?STATE)
    ].

new_line_test() ->
    #{line := Ln, column := Col} = new_line(new_state(#{column => 2})),
    ?assertEqual({2, 1}, {Ln, Col}).

incr_col_test() ->
    #{column := Col} = incr_col(1, new_state(#{})),
    ?assertEqual(2, Col).

incr_pos_test() ->
    #{position := Pos} = incr_pos(1, new_state(#{})),
    ?assertEqual(1, Pos).

-endif.

