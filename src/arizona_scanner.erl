-module(arizona_scanner).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([scan_string/2, format_error/2]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

-ignore_xref([format_error/2]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

%% Internal state record for scanner operations
-record(state, {
    line :: arizona_token:line(),
    position :: non_neg_integer()
}).

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-spec scan_string(Line, String) -> [Token] when
    Line :: arizona_token:line(),
    String :: string() | binary(),
    Token :: arizona_token:token().
scan_string(Line, String) when
    is_integer(Line), Line >= 0, (is_binary(String) orelse is_list(String))
->
    Bin = iolist_to_binary(String),
    scan(Bin, Bin, #state{
        line = Line,
        position = 0
    }).

-spec format_error(Reason, StackTrace) -> ErrorMap when
    Reason :: invalid_utf8 | unexpected_expr_end | badexpr | term(),
    StackTrace :: erlang:stacktrace(),
    ErrorMap :: #{general => string(), reason => io_lib:chars()}.
format_error(invalid_utf8, [{_M, _F, _As, Info} | _]) ->
    {error_info, ErrorInfo} = proplists:lookup(error_info, Info),
    {Line, Position, InvalidByte} = maps:get(cause, ErrorInfo),
    #{
        general => "Arizona template scanner UTF-8 validation failed",
        reason => io_lib:format(
            "Invalid UTF-8 byte 0x~2.16.0B at line ~p, position ~p. "
            "Arizona templates must use well-formed UTF-8 encoding.",
            [InvalidByte, Line, Position]
        )
    };
format_error(unexpected_expr_end, [{_M, _F, _As, Info} | _]) ->
    {error_info, ErrorInfo} = proplists:lookup(error_info, Info),
    {Line, Expr} = maps:get(cause, ErrorInfo),
    #{
        general => "Arizona template scanner expression parsing failed",
        reason => io_lib:format(
            "Unexpected end of expression '~s' at line ~p. "
            "Check for missing closing braces or malformed expressions.",
            [Expr, Line]
        )
    };
format_error(badexpr, [{_M, _F, _As, Info} | _]) ->
    {error_info, ErrorInfo} = proplists:lookup(error_info, Info),
    {Line, Expr, Reason} = maps:get(cause, ErrorInfo),
    #{
        general => "Arizona template scanner expression validation failed",
        reason => io_lib:format(
            "Invalid expression '~s' at line ~p: ~p",
            [Expr, Line, Reason]
        )
    }.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% Main scanning loop entry point
%% Tracks both current position and start of current text token
scan(Rest, Bin, State) ->
    scan(Rest, Bin, 0, State, State).

scan(Input, Bin, Len, TextState, State) ->
    case scan_next(Input, Bin, Len, State) of
        {escape, Rest, NewBin, NewLen} ->
            scan(Rest, NewBin, NewLen, reset_pos(TextState), reset_pos(State));
        {expression, Rest, ExprState} ->
            ExprTokens = scan_expr(Rest, Bin, ExprState),
            maybe_prepend_text_token(Bin, Len, TextState, ExprTokens);
        {continue, Rest, NewLen, NewState} ->
            scan(Rest, Bin, NewLen, TextState, NewState);
        end_of_input ->
            maybe_prepend_text_token(Bin, Len, TextState, [])
    end.

scan_next(<<$\\, ${, Rest/binary>>, Bin0, Len, State) ->
    % Handle escaped brace
    PrefixBin = binary_part(Bin0, State#state.position, Len),
    SuffixPos = State#state.position + Len + 2,
    SuffixBin = binary:part(Bin0, SuffixPos, byte_size(Bin0) - SuffixPos),
    NewBin = <<PrefixBin/binary, ${, SuffixBin/binary>>,
    {escape, Rest, NewBin, Len + 1};
scan_next(<<${, Rest/binary>>, _Bin, Len, State) ->
    {expression, Rest, incr_pos(Len + 1, State)};
scan_next(<<$\r, $\n, Rest/binary>>, _Bin, Len, State) ->
    {continue, Rest, Len + 2, new_line(State)};
scan_next(<<$\r, Rest/binary>>, _Bin, Len, State) ->
    {continue, Rest, Len + 1, new_line(State)};
scan_next(<<$\n, Rest/binary>>, _Bin, Len, State) ->
    {continue, Rest, Len + 1, new_line(State)};
scan_next(<<Char/utf8, Rest/binary>>, _Bin, Len, State) ->
    CharByteSize = utf8_char_byte_size(Char),
    {continue, Rest, Len + CharByteSize, State};
scan_next(<<InvalidByte, _Rest/binary>>, _Bin, _Len, State) ->
    error(
        invalid_utf8,
        none,
        error_info({State#state.line, State#state.position, InvalidByte})
    );
scan_next(<<>>, _Bin, _Len, _State) ->
    end_of_input.

maybe_prepend_text_token(Bin, Len, State, Tokens) ->
    case binary_part(Bin, State#state.position, Len) of
        <<>> ->
            Tokens;
        Text ->
            StaticToken = arizona_token:new(static, State#state.line, Text),
            [StaticToken | Tokens]
    end.

%% Scan an Erlang expression, handling nested braces
scan_expr(Rest0, Bin, State0) ->
    case find_expression_end(Rest0, State0) of
        {ok, ExprInfo} ->
            process_found_expression(ExprInfo, Bin, State0);
        {error, unexpected_expr_end, ErrState} ->
            Expr = binary_part(
                Bin, State0#state.position, ErrState#state.position - State0#state.position
            ),
            error(
                unexpected_expr_end,
                none,
                error_info({State0#state.line, Expr})
            )
    end.

%% Find the end of an expression and extract relevant information
find_expression_end(Rest0, State0) ->
    scan_expr_end(Rest0, 0, 0, State0).

%% Process a successfully found expression
process_found_expression({Len, EndMarkerLen, State1, Rest1}, Bin, State0) ->
    Expr0 = binary_part(Bin, State0#state.position, Len),
    {Expr, Category} = expr_category(Expr0, State0),
    Token = arizona_token:new(Category, State0#state.line, Expr),

    case maybe_skip_new_line(Rest1) of
        {true, NLMarker, Rest} ->
            continue_after_newline(Token, Rest, Bin, Len, EndMarkerLen, NLMarker, State1);
        false ->
            continue_without_newline(Token, Rest1, Bin, Len, EndMarkerLen, State1)
    end.

%% Continue scanning after skipping a newline
continue_after_newline(Token, Rest, Bin, Len, EndMarkerLen, NLMarker, State1) ->
    NLMarkerLen = byte_size(NLMarker),
    State = new_line(incr_pos(Len + EndMarkerLen + NLMarkerLen, State1)),
    % Continue scanning normally, then prepend newline to first static token
    NextTokens = scan(Rest, Bin, State),
    % Prepend the newline to the first static token found
    PrependedTokens = prepend_newline_to_first_static(NextTokens, NLMarker),
    [Token | PrependedTokens].

%% Continue scanning without skipping a newline
continue_without_newline(Token, Rest1, Bin, Len, EndMarkerLen, State1) ->
    State = incr_pos(Len + EndMarkerLen, State1),
    [Token | scan(Rest1, Bin, State)].

%% Find the end of an expression, tracking nested braces
%% Depth tracking ensures expressions like {case X of {ok, Y} -> Y end}
%% are properly handled.
scan_expr_end(<<$}, Rest/binary>>, 0, Len, State) ->
    {ok, {Len, _MarkerLen = 1, State, Rest}};
scan_expr_end(<<$}, Rest/binary>>, Depth, Len, State) ->
    scan_expr_end(Rest, Depth - 1, Len + 1, State);
scan_expr_end(<<${, Rest/binary>>, Depth, Len, State) ->
    scan_expr_end(Rest, Depth + 1, Len + 1, State);
scan_expr_end(<<$\r, $\n, Rest/binary>>, Depth, Len, State) ->
    scan_expr_end(Rest, Depth, Len + 2, new_line(State));
scan_expr_end(<<$\r, Rest/binary>>, Depth, Len, State) ->
    scan_expr_end(Rest, Depth, Len + 1, new_line(State));
scan_expr_end(<<$\n, Rest/binary>>, Depth, Len, State) ->
    scan_expr_end(Rest, Depth, Len + 1, new_line(State));
scan_expr_end(<<Char/utf8, Rest/binary>>, Depth, Len, State) ->
    CharByteSize = utf8_char_byte_size(Char),
    scan_expr_end(Rest, Depth, Len + CharByteSize, State);
scan_expr_end(<<InvalidByte, _Rest/binary>>, _Depth, _Len, State) ->
    error(
        invalid_utf8,
        none,
        error_info({State#state.line, State#state.position, InvalidByte})
    );
scan_expr_end(<<>>, _Depth, Len, State) ->
    {error, unexpected_expr_end, incr_pos(Len, State)}.

maybe_skip_new_line(<<$\r, $\n, Rest/binary>>) ->
    {true, <<$\r, $\n>>, Rest};
maybe_skip_new_line(<<$\r, Rest/binary>>) ->
    {true, <<$\r>>, Rest};
maybe_skip_new_line(<<$\n, Rest/binary>>) ->
    {true, <<$\n>>, Rest};
maybe_skip_new_line(_Rest) ->
    false.

%% Determine if an expression is Erlang code or a comment
expr_category(Expr, State) ->
    case parse_expression(Expr) of
        {ok, ParsedForms} ->
            categorize_parsed_forms(ParsedForms, Expr);
        {error, Reason} ->
            error(
                badexpr,
                none,
                error_info({State#state.line, Expr, Reason})
            )
    end.

%% Parse expression using merl, handling exceptions gracefully
parse_expression(Expr) ->
    try
        Forms =
            case merl:quote(Expr) of
                F when is_list(F) -> F;
                F -> [F]
            end,
        {ok, Forms}
    catch
        Class:Exception:StackTrace ->
            {error, {parse_failed, Class, Exception, StackTrace}}
    end.

%% Categorize parsed forms as either comments or dynamic expressions
categorize_parsed_forms(Forms, OriginalExpr) when is_list(Forms) ->
    case all_comments(Forms) of
        true ->
            {normalize_multiline_comment(OriginalExpr), comment};
        false ->
            {OriginalExpr, dynamic}
    end.

%% Check if all forms in a list are comments
all_comments(Forms) ->
    lists:all(fun is_comment/1, Forms).

%% Normalize multiline comments
normalize_multiline_comment(Expr) ->
    CommentLines = re:split(Expr, ~"\\n", [{return, binary}, {newline, lf}]),
    NormalizedLines = [norm_comment(Line) || Line <- CommentLines],
    iolist_to_binary(lists:join("\n", NormalizedLines)).

is_comment(Form) ->
    erl_syntax:type(Form) =:= comment.

norm_comment(<<$%, Rest/binary>>) ->
    norm_comment(Rest);
norm_comment(<<$\s, Rest/binary>>) ->
    norm_comment(Rest);
norm_comment(Comment) ->
    string:trim(Comment, trailing).

new_line(#state{line = Ln} = State) ->
    State#state{line = Ln + 1}.

%% Update byte position in the original binary
incr_pos(N, #state{position = Pos} = State) ->
    State#state{position = Pos + N}.

reset_pos(State) ->
    State#state{position = 0}.

%% Prepend newline to the first static token in the list
prepend_newline_to_first_static([], _NLMarker) ->
    [];
prepend_newline_to_first_static([Token | Rest], NLMarker) ->
    case arizona_token:get_category(Token) of
        static ->
            Text = arizona_token:get_content(Token),
            NLStaticToken = arizona_token:set_content(<<NLMarker/binary, Text/binary>>, Token),
            [NLStaticToken | Rest];
        _ ->
            [Token | prepend_newline_to_first_static(Rest, NLMarker)]
    end.

%% Calculate byte size of a UTF-8 character from its codepoint

% ASCII (0-127)
utf8_char_byte_size(Codepoint) when Codepoint =< 16#7F -> 1;
% 2-byte UTF-8
utf8_char_byte_size(Codepoint) when Codepoint =< 16#7FF -> 2;
% 3-byte UTF-8
utf8_char_byte_size(Codepoint) when Codepoint =< 16#FFFF -> 3;
% 4-byte UTF-8
utf8_char_byte_size(Codepoint) when Codepoint =< 16#10FFFF -> 4.

%% Create error_info for proper compiler diagnostics with enhanced details
-spec error_info(Cause) -> ErrorInfo when
    Cause :: term(),
    ErrorInfo :: [{error_info, map()}].
error_info(Cause) ->
    [
        {error_info, #{
            cause => Cause,
            module => ?MODULE
        }}
    ].
