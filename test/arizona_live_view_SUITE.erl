-module(arizona_live_view_SUITE).

-include_lib("stdlib/include/assert.hrl").

%% ct callbacks.
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).

%% arizona_live_view callbacks.
-export([mount/1]).
-export([render/1]).

%% Test cases.
-export([hello_world/1]).

%% --------------------------------------------------------------------
%% arizona_live_view callbacks.
%% --------------------------------------------------------------------

all() ->
    [hello_world].

init_per_suite(Config) ->
    application:set_env([{arizona, [
        {endpoint, #{
            routes => [
                {"/helloworld", arizona_live_handler, {?MODULE, render, #{}}}
            ]
        }}
    ]}]),
    {ok, _} = application:ensure_all_started(arizona),
    Config.

end_per_suite(Config) ->
    Config.

%% --------------------------------------------------------------------
%% arizona_live_view callbacks.
%% --------------------------------------------------------------------

mount(Socket) ->
    Socket.

render(Macros) ->
    arizona_live_view:parse_str(~s"""
    <html>
    <head>
    </head>
    <body>
        Hello, World!
    </body>
    </html>
    """, Macros).

%% --------------------------------------------------------------------
%% Test cases.
%% --------------------------------------------------------------------

hello_world(Config) when is_list(Config) ->
    Resp0 = httpc:request("http://localhost:8080/notfound"),
    ?assert(is_status(404, Resp0)),
    Resp1 = httpc:request("http://localhost:8080/helloworld"),
    ?assert(is_status(200, Resp1)),
    ?assert(is_body("Hello, World!", Resp1)).

is_body(Pattern, {ok, {{_HttpVersion, _StatusCode, _String}, _HttpHeaders, HttpBodyResult}}) ->
  nomatch =/= string:find(HttpBodyResult, Pattern);
is_body(_Pattern, _Result) ->
  false.

is_status(StatusCode, {ok, {{_HttpVersion, StatusCode, _String}, _HttpHeaders, _HttpBodyResult}}) ->
  true;
is_status(_StatusCode, _Result) ->
  false.
