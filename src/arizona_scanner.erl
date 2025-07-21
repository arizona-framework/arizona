-module(arizona_scanner).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([scan/2]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([scan_opts/0]).
-export_type([token/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-type scan_opts() :: #{
    line => pos_integer()
}.

-type token() :: {
    Category :: static | dynamic | comment,
    Line :: pos_integer(),
    Text :: binary()
}.

%% Internal state record for scanner operations
-record(state, {
    line :: pos_integer(),
    position :: non_neg_integer()
}).

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-spec scan(Opts, Html) -> [Token] when
    Opts :: scan_opts(),
    Html :: arizona_html:html(),
    Token :: token().
scan(Opts, Html) when is_map(Opts), (is_binary(Html) orelse is_list(Html)) ->
    BinaryTemplate = iolist_to_binary(Html),
    State = #state{
        line = maps:get(line, Opts, 1),
        position = 0
    },
    scan(BinaryTemplate, BinaryTemplate, State).

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
scan_next(<<_Char, Rest/binary>>, _Bin, Len, State) ->
    {continue, Rest, Len + 1, State};
scan_next(<<>>, _Bin, _Len, _State) ->
    end_of_input.

maybe_prepend_text_token(Bin, Len, State, Tokens) ->
    case binary_part(Bin, State#state.position, Len) of
        <<>> ->
            Tokens;
        Text ->
            [{static, State#state.line, Text} | Tokens]
    end.

%% Scan an Erlang expression, handling nested braces
scan_expr(Rest0, Bin, State0) ->
    case find_expression_end(Rest0, State0) of
        {ok, ExprInfo} ->
            process_found_expression(ExprInfo, Bin, State0);
        {error, unexpected_expr_end, ErrState} ->
            handle_scan_error({unexpected_expr_end, ErrState}, Bin, State0)
    end.

%% Find the end of an expression and extract relevant information
find_expression_end(Rest0, State0) ->
    scan_expr_end(Rest0, 0, 0, State0).

%% Process a successfully found expression
process_found_expression({Len, EndMarkerLen, State1, Rest1}, Bin, State0) ->
    Expr0 = binary_part(Bin, State0#state.position, Len),
    {Expr, Category} = expr_category(Expr0, State0),
    Token = {Category, State0#state.line, Expr},

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
    PrependedTokens = prepend_newline_to_first_static(NLMarker, NextTokens),
    [Token | PrependedTokens].

%% Continue scanning without skipping a newline
continue_without_newline(Token, Rest1, Bin, Len, EndMarkerLen, State1) ->
    State = incr_pos(Len + EndMarkerLen, State1),
    [Token | scan(Rest1, Bin, State)].

%% Handle all scanning errors
handle_scan_error({unexpected_expr_end, ErrState}, Bin, State0) ->
    Expr = binary_part(Bin, State0#state.position, ErrState#state.position - State0#state.position),
    error({unexpected_expr_end, State0#state.line, Expr});
handle_scan_error({badexpr, Line, Expr}, _Bin, _State) ->
    error({badexpr, Line, Expr}).

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
scan_expr_end(<<_Char, Rest/binary>>, Depth, Len, State) ->
    scan_expr_end(Rest, Depth, Len + 1, State);
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
        {error, _Reason} ->
            handle_scan_error({badexpr, State#state.line, Expr}, <<>>, State)
    end.

%% Parse expression using merl, handling exceptions gracefully
parse_expression(Expr) ->
    try
        {ok, merl:quote(Expr)}
    catch
        _Class:_Exception ->
            {error, parse_failed}
    end.

%% Categorize parsed forms as either comments or dynamic expressions
categorize_parsed_forms(Forms, OriginalExpr) when is_list(Forms) ->
    case all_comments(Forms) of
        true ->
            {normalize_multiline_comment(OriginalExpr), comment};
        false ->
            {OriginalExpr, dynamic}
    end;
categorize_parsed_forms(SingleForm, OriginalExpr) ->
    case is_comment(SingleForm) of
        true ->
            {norm_comment(OriginalExpr), comment};
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
prepend_newline_to_first_static(NLMarker, [{static, Line, Text} | Rest]) ->
    [{static, Line, <<NLMarker/binary, Text/binary>>} | Rest];
prepend_newline_to_first_static(NLMarker, [Token | Rest]) ->
    [Token | prepend_newline_to_first_static(NLMarker, Rest)];
prepend_newline_to_first_static(_NLMarker, []) ->
    [].
