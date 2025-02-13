-module(arizona_scanner).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([scan/2]).

%

-ignore_xref([scan/2]).

%% --------------------------------------------------------------------
%% Types (and their exports)
%% --------------------------------------------------------------------

-type scan_opts() :: #{
    line => pos_integer(),
    column => pos_integer(),
    indentation => non_neg_integer()
}.
-export_type([scan_opts/0]).

-type token() :: {
    Category :: html | erlang | comment,
    Location :: {Line :: pos_integer(), Column :: pos_integer()},
    Content :: binary()
}.
-export_type([token/0]).

%% --------------------------------------------------------------------
%% Private types
%% --------------------------------------------------------------------

-record(state, {
    line :: pos_integer(),
    column :: pos_integer(),
    indentation :: non_neg_integer(),
    position :: non_neg_integer()
}).

%% --------------------------------------------------------------------
%% Doctests
%% --------------------------------------------------------------------

-ifdef(TEST).
-include_lib("doctest/include/doctest.hrl").
-endif.

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-doc ~"""
Tokenizes a template.

## Examples

```
> arizona_scanner:scan(#{}, ~"foo{bar}{% baz }").
[{html,{1,1},<<"foo">>},
 {erlang,{1,4},<<"bar">>},
 {comment,{1,9},<<"baz">>}]
```

## Result

It returns `[Token]` where a Token is one of:

- `{html, Location, Content}`
- `{erlang, Location, Content}`
- `{comment, Location, Content}`
""".
-spec scan(Opts, Template) -> [Token] when
    Opts :: scan_opts(),
    Template :: binary(),
    Token :: token().
scan(Opts, Template) when is_map(Opts), is_binary(Template) ->
    State = #state{
        line = maps:get(line, Opts, 1),
        column = maps:get(column, Opts, 1 + maps:get(indentation, Opts, 0)),
        indentation = maps:get(indentation, Opts, 0),
        position = 0
    },
    scan(Template, Template, State).

%% --------------------------------------------------------------------
%% Private functions
%% --------------------------------------------------------------------

scan(Rest, Bin, State) ->
    scan(Rest, Bin, 0, State, State).

scan(<<${, Rest/binary>>, Bin, Len, TextState, State) ->
    ExprTokens = scan_expr(Rest, Bin, 1, incr_pos(Len + 1, State)),
    maybe_prepend_text_token(Bin, Len, TextState, ExprTokens);
scan(<<$\r, $\n, Rest/binary>>, Bin, Len, TextState, State) ->
    scan(Rest, Bin, Len + 2, TextState, new_line(State));
scan(<<$\r, Rest/binary>>, Bin, Len, TextState, State) ->
    scan(Rest, Bin, Len + 1, TextState, new_line(State));
scan(<<$\n, Rest/binary>>, Bin, Len, TextState, State) ->
    scan(Rest, Bin, Len + 1, TextState, new_line(State));
scan(<<_, Rest/binary>>, Bin, Len, TextState, State) ->
    scan(Rest, Bin, Len + 1, TextState, incr_col(1, State));
scan(<<>>, Bin, Len, TextState, _State) ->
    maybe_prepend_text_token(Bin, Len, TextState, []).

maybe_prepend_text_token(Bin, Len, State, Tokens) ->
    case string:trim(binary_part(Bin, State#state.position, Len)) of
        <<>> ->
            Tokens;
        Text ->
            [{html, location(State), Text} | Tokens]
    end.

scan_expr(Rest0, Bin, StartMarkerLen, State0) ->
    {Len, EndMarkerLen, State1, Rest1} = scan_expr_end(Rest0, 0, 0, State0),
    Expr0 = binary_part(Bin, State0#state.position, Len),
    {Expr, Category} = expr_category(Expr0, State0),
    case maybe_skip_new_line(Rest1) of
        {true, NLMarkerLen, Rest} ->
            State = new_line(incr_pos(Len + EndMarkerLen + NLMarkerLen, State1)),
            [{Category, location(State0), Expr} | scan(Rest, Bin, State)];
        false ->
            State = incr_col(StartMarkerLen, incr_pos(Len + EndMarkerLen, State1)),
            [{Category, location(State0), Expr} | scan(Rest1, Bin, State)]
    end.

scan_expr_end(<<$}, Rest/binary>>, 0, Len, State) ->
    {Len, _MarkerLen = 1, incr_col(1, State), Rest};
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
    error({unexpected_expr_end, location(incr_pos(Len, State))}).

maybe_skip_new_line(<<$\r, $\n, Rest/binary>>) ->
    {true, 2, Rest};
maybe_skip_new_line(<<$\r, Rest/binary>>) ->
    {true, 1, Rest};
maybe_skip_new_line(<<$\n, Rest/binary>>) ->
    {true, 1, Rest};
maybe_skip_new_line(_Rest) ->
    false.

expr_category(Expr, State) ->
    try
        case merl:quote(Expr) of
            Forms when is_list(Forms) ->
                case lists:all(fun is_comment/1, Forms) of
                    true ->
                        Comments0 = re:split(Expr, <<"\\n">>, [{return, binary}, {newline, lf}]),
                        Comments1 = [norm_comment(Comment) || Comment <- Comments0],
                        Comment = iolist_to_binary(lists:join("\n", Comments1)),
                        {Comment, comment};
                    false ->
                        {Expr, erlang}
                end;
            Form ->
                case is_comment(Form) of
                    true ->
                        {norm_comment(Expr), comment};
                    false ->
                        {Expr, erlang}
                end
        end
    catch
        _:_ ->
            error({badexpr, location(State), Expr})
    end.

is_comment(Form) ->
    erl_syntax:type(Form) =:= comment.

norm_comment(<<$%, Rest/binary>>) ->
    norm_comment(Rest);
norm_comment(<<$\s, Rest/binary>>) ->
    norm_comment(Rest);
norm_comment(Comment) ->
    string:trim(Comment, trailing).

new_line(#state{line = Ln} = State) ->
    State#state{line = Ln + 1, column = 1 + State#state.indentation}.

incr_col(N, #state{column = Col} = State) ->
    State#state{column = Col + N}.

incr_pos(N, #state{position = Pos} = State) ->
    State#state{position = Pos + N}.

location(#state{line = Ln, column = Col}) ->
    location(Ln, Col).

location(Ln, Col) ->
    {Ln, Col}.
