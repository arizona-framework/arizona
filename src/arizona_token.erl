-module(arizona_token).
-moduledoc ~"""
Token data structure for Arizona template parsing.

Defines the token record and accessor functions used by the scanner
and parser to represent different types of content in templates.

## Token Categories

- `static` - Plain HTML text content
- `dynamic` - Erlang expressions enclosed in `{}`
- `comment` - Erlang comments (lines starting with `%`)

## Example

```erlang
1> Token = arizona_token:new(dynamic, 5, ~"Title").
#token{category = dynamic, line = 5, content = ~"Title"}
2> arizona_token:get_category(Token).
dynamic
3> arizona_token:get_content(Token).
~"Title"
```
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([new/3]).
-export([get_category/1]).
-export([get_line/1]).
-export([get_content/1]).
-export([set_content/2]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([token/0]).
-export_type([category/0]).
-export_type([line/0]).
-export_type([content/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-record(token, {
    category :: category(),
    line :: line(),
    content :: content()
}).

-opaque token() :: #token{}.
-nominal category() :: static | dynamic | comment.
-nominal line() :: erl_anno:line().
-nominal content() :: binary().

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-doc ~"""
Creates a new token with the specified category, line number, and content.

The category determines how the token content should be processed:
- `static` for plain HTML text
- `dynamic` for Erlang expressions
- `comment` for Erlang comments
""".
-spec new(Category, Line, Content) -> Token when
    Category :: category(),
    Line :: line(),
    Content :: content(),
    Token :: token().
new(Category, Line, Content) when
    (Category =:= static orelse Category =:= dynamic orelse Category =:= comment),
    is_integer(Line),
    Line >= 0,
    is_binary(Content)
->
    #token{
        category = Category,
        line = Line,
        content = Content
    }.

-doc ~"""
Returns the category of the token (static, dynamic, or comment).
""".
-spec get_category(Token) -> Category when
    Token :: token(),
    Category :: category().
get_category(#token{} = Token) ->
    Token#token.category.

-doc ~"""
Returns the line number where this token was found in the template.
""".
-spec get_line(Token) -> Line when
    Token :: token(),
    Line :: line().
get_line(#token{} = Token) ->
    Token#token.line.

-doc ~"""
Returns the binary content of the token.
""".
-spec get_content(Token) -> Content when
    Token :: token(),
    Content :: content().
get_content(#token{} = Token) ->
    Token#token.content.

-doc ~"""
Returns a new token with updated content, preserving category and line.
""".
-spec set_content(Content, Token) -> Token1 when
    Content :: content(),
    Token :: token(),
    Token1 :: token().
set_content(Content, #token{} = Token) when is_binary(Content) ->
    Token#token{content = Content}.
