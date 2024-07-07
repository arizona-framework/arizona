-module(arizona_template_scanner).
-moduledoc false.

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([scan/1]).

%

-ignore_xref([scan/1]).

%% --------------------------------------------------------------------
%% Types (and their exports)
%% --------------------------------------------------------------------

-record(state, {
    line :: line(),
    column :: column(),
    position :: non_neg_integer()
}).
-opaque token() :: {open_tag, location(), binary()}
                 | {attr_name, location(), binary()}
                 | {attr_value, location(), {text, binary()} | {expr, binary()}}
                 | {bool_attr, location(), binary()}
                 | {close_tag, location(), {void, boolean()}}
                 | {closing_tag, location(), binary()}
                 | {text, location(), binary()}
                 | {expr, location(), binary()}.
-export_type([token/0]).

-type location() :: {line(), column()}.
-export_type([location/0]).

-type line() :: non_neg_integer().
-export_type([line/0]).

-type column() :: non_neg_integer().
-export_type([column/0]).

-type error_reason() :: badexpr
                      | unexpected_comment
                      | unexpected_tag_end
                      | unexpected_expr_end
                      | unexpected_string_end.
-export_type([error_reason/0]).

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-spec scan(Bin) -> Result
    when Bin :: binary(),
         Result :: {ok, [token()]} | {error, {error_reason(), location()}}.
scan(Bin) when is_binary(Bin) ->
    try
        {ok, scan(Bin, Bin, 0, new_state())}
    catch
        throw:{Reason, State} ->
            {error, {Reason, location(State)}}
    end.

%% --------------------------------------------------------------------
%% Private
%% --------------------------------------------------------------------

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
            Pos = ExprState#state.position,
            Expr = binary_part(Bin, Pos, Len),
            case expr_category(Expr) of
                {ok, Category} ->
                    State = incr_col(StartMarkerLen, incr_pos(Len + EndMarkerLen, State0)),
                    [{Category, location(ExprState), Expr} | scan(Rest, Bin, 0, State)];
                error ->
                    throw({badexpr, ExprState})
            end;
        {error, Reason} ->
            throw(Reason)
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
            Pos = TagState#state.position,
            TagName = binary_part(Bin, Pos, Len),
            State = incr_col(StartMarkerLen, incr_pos(Len, State0)),
            [{open_tag, location(TagState), TagName}
             | scan_tag_attrs(Rest, Bin, is_tag_name_void(TagName), State)];
        {error, Reason} ->
            throw(Reason)
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

is_tag_name_void(Name) ->
    lists:member(Name, [
        <<"!DOCTYPE">>, <<"!doctype">>, <<"?xml">>, <<"area">>,
        <<"base">>, <<"br">>, <<"col">>, <<"command">>, <<"embed">>,
        <<"hr">>, <<"img">>, <<"input">>, <<"keygen">>, <<"link">>,
        <<"meta">>, <<"param">>, <<"source">>, <<"track">>, <<"wbr">>]).

scan_tag_attrs(<<$/, $>, Rest/binary>>, Bin, _IsVoid, State) ->
    [{close_tag, location(State), {void, true}}
     | scan(Rest, Bin, 0, incr_col(2, incr_pos(2, State)))];
scan_tag_attrs(<<$>, Rest/binary>>, Bin, IsVoid, State) ->
    [{close_tag, location(State), {void, IsVoid}}
     | scan(Rest, Bin, 0, incr_col(1, incr_pos(1, State)))];
scan_tag_attrs(<<$\s, Rest/binary>>, Bin, IsVoid, State) ->
    scan_tag_attrs(Rest, Bin, IsVoid, incr_col(1, incr_pos(1, State)));
scan_tag_attrs(<<$\r, $\n, Rest/binary>>, Bin, IsVoid, State) ->
    scan_tag_attrs(Rest, Bin, IsVoid, new_line(incr_pos(2, State)));
scan_tag_attrs(<<$\r, Rest/binary>>, Bin, IsVoid, State) ->
    scan_tag_attrs(Rest, Bin, IsVoid, new_line(incr_pos(1, State)));
scan_tag_attrs(<<$\n, Rest/binary>>, Bin, IsVoid, State) ->
    scan_tag_attrs(Rest, Bin, IsVoid, new_line(incr_pos(1, State)));
scan_tag_attrs(Rest0, Bin, IsVoid, NameState) ->
    case scan_tag_attr_name(Rest0, 0, NameState) of
        {ok, {NameLen, NextState, Rest1}} when NameLen > 0 ->
            NamePos = NameState#state.position,
            Name = binary_part(Bin, NamePos, NameLen),
            ValState = incr_pos(NameLen, NextState),
            case scan_tag_attr_value(Rest1, Bin, ValState) of
                {ok, {Value, MarkerLen, State, Rest}} ->
                    [{attr_name, location(NameState), Name},
                     {attr_value, location(incr_col(MarkerLen, ValState)), Value}
                     | scan_tag_attrs(Rest, Bin, IsVoid, State)];
                {none, {State, Rest}} ->
                    [{bool_attr, location(NameState), Name}
                     | scan_tag_attrs(Rest, Bin, IsVoid, State)];
                {error, Reason} ->
                    throw(Reason)
            end;
        {error, Reason} ->
            throw(Reason)
    end.

scan_tag_attr_name(<<$=, _/binary>> = Rest, Len, State) ->
    {ok, {Len, State, Rest}};
scan_tag_attr_name(<<$/, $>, _/binary>> = Rest, Len, State) ->
    {ok, {Len, State, Rest}};
scan_tag_attr_name(<<$>, _/binary>> = Rest, Len, State) ->
    {ok, {Len, State, Rest}};
scan_tag_attr_name(<<$\s, _/binary>> = Rest, Len, State) ->
    {ok, {Len, State, Rest}};
scan_tag_attr_name(<<$\r, _/binary>> = Rest, Len, State) ->
    {ok, {Len, State, Rest}};
scan_tag_attr_name(<<$\n, _/binary>> = Rest, Len, State) ->
    {ok, {Len, State, Rest}};
scan_tag_attr_name(<<_, Rest/binary>>, Len, State) ->
    scan_tag_attr_name(Rest, Len + 1, incr_col(1, State));
scan_tag_attr_name(<<>>, Len, State) ->
    {error, {unexpected_tag_end, incr_pos(Len, State)}}.

scan_tag_attr_value(<<$=, $', Rest0/binary>>, Bin, NameState) ->
    ValState = incr_col(2, incr_pos(2, NameState)),
    case scan_single_quoted_string(Rest0, 0, ValState) of
        {ok, {Len, MarkerLen, TxtState, Rest}} ->
            Pos = ValState#state.position,
            Txt = binary_part(Bin, Pos, Len),
            State = incr_col(MarkerLen, incr_pos(Len + MarkerLen, TxtState)),
            {ok, {{text, Txt}, 1, State, Rest}};
        {error, {Reason, State}} ->
            {error, {Reason, State}}
    end;
scan_tag_attr_value(<<$=, $", Rest0/binary>>, Bin, NameState) ->
    ValState = incr_col(2, incr_pos(2, NameState)),
    case scan_double_quoted_string(Rest0, 0, ValState) of
        {ok, {Len, MarkerLen, TxtState, Rest}} ->
            Pos = ValState#state.position,
            Txt = binary_part(Bin, Pos, Len),
            State = incr_col(MarkerLen, incr_pos(Len + MarkerLen, TxtState)),
            {ok, {{text, Txt}, 1, State, Rest}};
        {error, {Reason, State}} ->
            {error, {Reason, State}}
    end;
scan_tag_attr_value(<<$=, ${, Rest0/binary>>, Bin, NameState) ->
    ValState = incr_col(2, incr_pos(2, NameState)),
    case scan_expr_end(Rest0, 0, 0, ValState) of
        {ok, {Len, MarkerLen, ExprState, Rest}} ->
            Pos = ValState#state.position,
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
                    Pos = TagState#state.position,
                    TagName = binary_part(Bin, Pos, TagNameLen),
                    State = incr_col(StartMarkerLen, State0),
                    [{closing_tag, location(TagState), TagName}
                     | scan(Rest, Bin, 0, State)];
                {error, Reason} ->
                    throw(Reason)
            end;
        {error, Reason} ->
            throw(Reason)
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

maybe_prepend_text_token(Bin, Len, State, Tokens) ->
    case string:trim(binary_part(Bin, State#state.position, Len)) of
        <<>> ->
            Tokens;
        Txt ->
            [{text, location(State), Txt} | Tokens]
    end.

location(#state{line = Ln, column = Col}) ->
    {Ln, Col}.

%% State.

new_state() ->
    #state{
        line = 1,
        column = 1,
        position = 0
    }.

new_line(#state{line = Ln} = State) ->
    State#state{line = Ln + 1, column = 1}.

incr_col(N, #state{column = Col} = State) ->
    State#state{column = Col + N}.

incr_pos(N, #state{position = Pos} = State) ->
    State#state{position = Pos + N}.

%% --------------------------------------------------------------------
%% EUnit
%% --------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

scan_test() ->
    [
        ?assertEqual({ok, [
            {open_tag, {1, 1}, <<"!DOCTYPE">>},
            {bool_attr, {1, 11}, <<"html">>},
            {close_tag, {1, 15}, {void, true}},
            {open_tag, {2, 1}, <<"html">>},
            {attr_name, {2, 7}, <<"lang">>},
            {attr_value, {2, 12}, {text, <<"en">>}},
            {close_tag, {2, 16}, {void, false}},
            {open_tag, {3, 1}, <<"head">>},
            {close_tag, {3, 6}, {void, false}},
            {open_tag, {4, 5}, <<"meta">>},
            {attr_name, {4, 11}, <<"charset">>},
            {attr_value, {4, 19}, {text, <<"UTF-8">>}},
            {close_tag, {4, 26}, {void, true}},
            {open_tag, {5, 5}, <<"title">>},
            {close_tag, {5, 11}, {void, false}},
            {expr, {5, 12}, <<"_@title">>},
            {closing_tag, {5, 21}, <<"title">>},
            {open_tag, {6, 5}, <<"script">>},
            {attr_name, {6, 13}, <<"src">>},
            {attr_value, {6, 17}, {text, <<"assets/js/main.js">>}},
            {close_tag, {6, 36}, {void, false}},
            {closing_tag, {6, 37}, <<"script">>},
            {closing_tag, {7, 1}, <<"head">>},
            {open_tag, {8, 1}, <<"body">>},
            {close_tag, {8, 6}, {void, false}},
            {open_tag, {9, 5}, <<"h1">>},
            {close_tag, {9, 8}, {void, false}},
            {text, {9, 9}, <<"Arizona Counter">>},
            {closing_tag, {9, 24}, <<"h1">>},
            {open_tag, {10, 5}, <<".counter">>},
            {attr_name, {11, 9}, <<"count">>},
            {attr_value, {11, 15}, {expr, <<"_@count">>}},
            {attr_name, {12, 9}, <<"btn_text">>},
            {attr_value, {12, 18}, {text, <<"Increment">>}},
            {attr_name, {13, 9}, <<"event">>},
            {attr_value, {13, 15}, {text, <<"incr">>}},
            {close_tag, {14, 5}, {void, true}},
            {closing_tag, {15, 1}, <<"body">>},
            {closing_tag, {16, 1}, <<"html">>}
        ]}, scan(~"""
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
                    event="incr"
                />
            </body>
            </html>
            """)),
        ?assertEqual({error, {badexpr, {1, 4}}}, scan(<<"foo{@}bar">>))
    ].

expr_category_test() ->
    [
        ?assertEqual({ok, expr}, expr_category("foo")),
        ?assertEqual({ok, comment}, expr_category("%foo")),
        ?assertEqual(error, expr_category("@"))
    ].

scan_expr_end_test() ->
    [
        ?assertMatch({ok, {10, 1, #state{column = 12}, <<"baz">>}},
                     scan_expr_end(<<"{foo\"bar\"}}baz">>, 0, 0, new_state())),
        ?assertMatch({ok, {8, 1, #state{column = 10}, <<"baz">>}},
                     scan_expr_end(<<"foo\"bar\"}baz">>, 0, 0, new_state())),
        ?assertMatch({error, {unexpected_expr_end, #state{column = 12, position = 11}}},
                     scan_expr_end(<<"foo\"bar\"baz">>, 0, 0, new_state()))
    ].

scan_single_quoted_string_test() ->
    [
        ?assertMatch({ok, {8, 1, #state{column = 9}, <<"baz">>}},
                     scan_single_quoted_string(<<"foo\\'bar'baz">>, 0, new_state())),
        ?assertMatch({error, {unexpected_string_end, #state{column = 4, position = 3}}},
                     scan_single_quoted_string(<<"foo">>, 0, new_state()))
    ].

scan_double_quoted_string_test() ->
    [
        ?assertMatch({ok, {8, 1, #state{column = 9}, <<"baz">>}},
                     scan_double_quoted_string(<<"foo\\\"bar\"baz">>, 0, new_state())),
        ?assertMatch({error, {unexpected_string_end, #state{column = 4, position = 3}}},
                     scan_double_quoted_string(<<"foo">>, 0, new_state()))
    ].

new_line_test() ->
    #state{line = Ln, column = Col} = new_line(new_state()),
    ?assertEqual({2, 1}, {Ln, Col}).

incr_col_test() ->
    #state{column = Col} = incr_col(1, new_state()),
    ?assertEqual(2, Col).

incr_pos_test() ->
    #state{position = Pos} = incr_pos(1, new_state()),
    ?assertEqual(1, Pos).

-endif.
