-module(arizona_req_test_adapter).
-moduledoc """
Stub adapter used by `arizona_req_SUITE`.

Backing `raw` value is a map carrying pre-computed values; each
callback is a simple map lookup. `read_body/1` also marks the raw map
so tests can assert the updated raw is threaded through.
""".
-behaviour(arizona_req).

-export([new/1]).
-export([parse_bindings/1]).
-export([parse_params/1]).
-export([parse_cookies/1]).
-export([parse_headers/1]).
-export([read_body/1]).

new(Raw) ->
    arizona_req:new(?MODULE, Raw, #{method => ~"GET", path => ~"/"}).

parse_bindings(#{bindings := Bindings}) -> Bindings.

parse_params(#{params := Params}) -> Params.

parse_cookies(#{cookies := Cookies}) -> Cookies.

parse_headers(#{headers := Headers}) -> Headers.

read_body(#{body := Body} = Raw) -> {Body, Raw#{body_read => true}}.
