-module(arizona_terminal_req).
-moduledoc """
Synthetic `arizona_req` adapter for terminal apps.

A terminal program has no HTTP request, but `arizona_live:start_link/5` and a
view's `mount/2` still take an `t:arizona_req:request/0`. `new/0` mints a blank
one (method `GET`, path `/`, empty bindings/params/cookies/headers/body) so the
live runtime can drive a view with no web server involved.
""".
-behaviour(arizona_req).

-export([new/0]).
-export([parse_bindings/1]).
-export([parse_params/1]).
-export([parse_cookies/1]).
-export([parse_headers/1]).
-export([read_body/1]).

-ignore_xref([new/0]).

-doc "Builds a blank request backed by this adapter.".
-spec new() -> arizona_req:request().
new() ->
    arizona_req:new(?MODULE, #{}, #{method => ~"GET", path => ~"/"}).

-spec parse_bindings(arizona_req:raw()) -> arizona_req:bindings().
parse_bindings(_Raw) ->
    #{}.

-spec parse_params(arizona_req:raw()) -> arizona_req:params().
parse_params(_Raw) ->
    [].

-spec parse_cookies(arizona_req:raw()) -> arizona_req:cookies().
parse_cookies(_Raw) ->
    [].

-spec parse_headers(arizona_req:raw()) -> arizona_req:headers().
parse_headers(_Raw) ->
    #{}.

-spec read_body(arizona_req:raw()) -> {arizona_req:body(), arizona_req:raw()}.
read_body(Raw) ->
    {<<>>, Raw}.
