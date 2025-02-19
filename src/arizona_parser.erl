-module(arizona_parser).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([parse/2]).

%

-ignore_xref([parse/2]).

%% --------------------------------------------------------------------
%% Types (and their exports)
%% --------------------------------------------------------------------

-type options() :: #{
    render_context => from_socket | render | none,
    bindings => erl_eval:binding_struct()
}.
-export_type([options/0]).

%% --------------------------------------------------------------------
%% Doctests
%% --------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
doctest_test() -> doctest:module(?MODULE).
-endif.

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-doc ~"""
Parses scanned template tokens.

## Result

It returns a `{Static, Dynamic}` tuple where Static is an AST list of
binaries and the Dynamic is an AST list of Erlang terms.
""".
-spec parse(Tokens, Opts) -> {Static, Dynamic} when
    Tokens :: [Token],
    Opts :: options(),
    Token :: arizona_scanner:token(),
    Static :: [Ast],
    Dynamic :: [Ast],
    Ast :: tuple().
parse(Tokens0, Opts) when is_list(Tokens0), is_map(Opts) ->
    Tokens1 = drop_comments(Tokens0),
    Tokens = add_empty_text_tokens(Tokens1),
    {HtmlTokens, ErlTokens} = tokens_partition(Tokens),
    Static = [scan_and_parse_html_token_to_ast(HtmlToken) || HtmlToken <- HtmlTokens],
    ErlTokensEnum = lists:enumerate(0, ErlTokens),
    RenderContext = maps:get(render_context, Opts, from_socket),
    Bindings = maps:get(bindings, Opts, []),
    Dynamic = [
        scan_and_parse_erlang_token_to_ast(ErlToken, Index, RenderContext, Bindings)
     || {Index, ErlToken} <- ErlTokensEnum
    ],
    {Static, Dynamic}.

%% --------------------------------------------------------------------
%% Private functions
%% --------------------------------------------------------------------

% Comments are not rendered, so they're dropped.
drop_comments(Tokens) ->
    [Token || {Category, _Location, _Content} = Token <- Tokens, Category =/= comment].

% Dummy empty texts are required for the correct zip between Static and Dynamic
% when consecutive Erlang expressions are found.
add_empty_text_tokens([]) ->
    [];
add_empty_text_tokens([{erlang, _, _} = ExprA, {erlang, _, _} = ExprB | T]) ->
    [ExprA, {html, {0, 0}, ~""} | add_empty_text_tokens([ExprB | T])];
add_empty_text_tokens([H | T]) ->
    [H | add_empty_text_tokens(T)].

% Html tokens are static, so the partition result is {Static, Dynamic}.
tokens_partition(Tokens) ->
    lists:partition(fun(Token) -> element(1, Token) =:= html end, Tokens).

scan_and_parse_html_token_to_ast({html, _Loc, Text0}) ->
    Text = quote_text(Text0),
    scan_and_parse_to_ast(<<"<<", $", Text/binary, $", "/utf8>>">>).

scan_and_parse_erlang_token_to_ast({erlang, _Loc, Expr0}, Index0, RenderContext, Bindings) ->
    Index = integer_to_binary(Index0),
    Vars = vars_to_binary(expr_vars(Expr0)),
    Form0 = merl:quote(Expr0),
    Form = arizona_transform:transform(Form0, Bindings),
    Expr1 = erl_pp:expr(Form),
    Expr = norm_expr(RenderContext, Expr1, Index, Vars),
    scan_and_parse_to_ast(iolist_to_binary(Expr)).

norm_expr(from_socket, Expr, Index, Vars) ->
    [
        ["fun(ViewAcc, Socket, Opts) ->\n"],
        ["    case arizona_socket:render_context(Socket) of\n"],
        ["        render ->\n"],
        ["            arizona_render:render(", Expr, ", View, ViewAcc, Socket);\n"],
        ["        diff ->\n"],
        ["            Index = ", Index, ",\n"],
        ["            Vars = ", Vars, ",\n"],
        ["            TokenCallback = fun() -> ", Expr, " end,\n"],
        ["            arizona_diff:diff(Index, Vars, TokenCallback, ViewAcc, Socket, Opts)\n"],
        ["    end\n"],
        ["end"]
    ];
norm_expr(render, Expr, _Index, _Vars) ->
    [
        ["fun(ViewAcc, Socket, Opts) ->\n"],
        ["    arizona_render:render(", Expr, ", View, ViewAcc, Socket)\n"],
        ["end"]
    ];
norm_expr(none, Expr, _Index, _Vars) ->
    Expr.

% The text must be quoted to transform it in an Erlang AST form, for example:
%
% ~"""
% f"o\"o
% """.
%
% To produce a binary it must be <<"f\"o\\\"o">>.
quote_text(<<>>) ->
    <<>>;
quote_text(<<$\\, $", Rest/binary>>) ->
    <<$\\, $\\, $\\, $", (quote_text(Rest))/binary>>;
quote_text(<<$", Rest/binary>>) ->
    <<$\\, $", (quote_text(Rest))/binary>>;
quote_text(<<C, Rest/binary>>) ->
    <<C, (quote_text(Rest))/binary>>.

scan_and_parse_to_ast(Text) ->
    Str = binary_to_list(<<Text/binary, $.>>),
    {ok, Tokens, _EndLoc} = erl_scan:string(Str),
    {ok, [Ast]} = erl_parse:parse_exprs(Tokens),
    Ast.

expr_vars(Expr) ->
    case
        re:run(
            Expr,
            "arizona_view:get_assign\\(([a-z][a-zA-Z_@]*|'(.*?)')",
            [global, {capture, all_but_first, binary}]
        )
    of
        {match, Vars0} ->
            Vars = lists:flatten([pick_quoted_var(List) || List <- Vars0]),
            lists:usort(lists:map(fun binary_to_atom/1, Vars));
        nomatch ->
            []
    end.

pick_quoted_var([<<$', _/binary>> = Var | _T]) ->
    Var;
pick_quoted_var([Var]) ->
    iolist_to_binary([$', Var, $']);
pick_quoted_var([_Var | T]) ->
    pick_quoted_var(T).

vars_to_binary(Vars0) ->
    Vars = lists:map(fun atom_to_binary/1, Vars0),
    iolist_to_binary([$[, lists:join(", ", Vars), $]]).
