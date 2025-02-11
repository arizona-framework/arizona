-module(arizona_tpl_parser).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([parse/1]).

%

-ignore_xref([parse/1]).

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-doc ~"""
Parses scanned template tokens.

## Examples

```
> Tokens = arizona_tpl_scanner:scan(#{}, ~"foo{bar}").
[{html,{1,1},<<"foo">>},{erlang,{1,4},<<"bar">>}]
> arizona_tpl_parser:parse(Tokens).
{[{bin,1,[{bin_element,1,{string,1,"foo"},default,[utf8]}]}],[{atom,1,bar}]}
```

## Result

It returns a `{Static, Dynamic}` tuple where Static is an AST list of
binaries and the Dynamic is an AST list of Erlang terms.
""".
-spec parse(Tokens) -> {Static, Dynamic} when
    Tokens :: [Token],
    Token :: arizona_tpl_scanner:token(),
    Static :: [Ast],
    Dynamic :: [Ast],
    Ast :: tuple().
parse(Tokens0) when is_list(Tokens0) ->
    Tokens1 = drop_comments(Tokens0),
    Tokens = add_empty_text_tokens(Tokens1),
    {HtmlTokens, ErlTokens} = tokens_partition(Tokens),
    Static = [scan_and_parse_html_token_to_ast(HtmlToken) || HtmlToken <- HtmlTokens],
    Dynamic = [scan_and_parse_erlang_token_to_ast(ErlToken) || ErlToken <- ErlTokens],
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

scan_and_parse_erlang_token_to_ast({erlang, _Loc, Expr}) ->
    scan_and_parse_to_ast(Expr).

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
