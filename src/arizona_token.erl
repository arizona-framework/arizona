-module(arizona_token).

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

-spec get_category(Token) -> Category when
    Token :: token(),
    Category :: category().
get_category(#token{} = Token) ->
    Token#token.category.

-spec get_line(Token) -> Line when
    Token :: token(),
    Line :: line().
get_line(#token{} = Token) ->
    Token#token.line.

-spec get_content(Token) -> Content when
    Token :: token(),
    Content :: content().
get_content(#token{} = Token) ->
    Token#token.content.

-spec set_content(Content, Token) -> Token1 when
    Content :: content(),
    Token :: token(),
    Token1 :: token().
set_content(Content, #token{} = Token) when is_binary(Content) ->
    Token#token{content = Content}.
