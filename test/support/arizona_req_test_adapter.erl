-module(arizona_req_test_adapter).
-moduledoc """
Stub adapter used by `arizona_req_SUITE` and as the canonical Req for
every `arizona_live:start_link` call site in the live/stream suites.

Backing `raw` value is a map carrying pre-computed values; each
`arizona_req` callback is a simple map lookup. `read_body/1` also marks
the raw map so tests can assert the updated raw is threaded through.

Implements `arizona_adapter` as a no-op: tests never trigger a real
`resolve_route/3` against this adapter (navigate paths exercised at the
socket layer use the cowboy adapter), but the behaviour is declared so
the contract surfaces in the module attributes and in dialyzer.
""".
-behaviour(arizona_req).
-behaviour(arizona_adapter).

-export([new/0]).
-export([new/1]).
-export([parse_bindings/1]).
-export([parse_params/1]).
-export([parse_cookies/1]).
-export([parse_headers/1]).
-export([read_body/1]).
-export([resolve_route/3]).

new() ->
    new(#{}).

new(Raw) ->
    arizona_req:new(?MODULE, Raw, #{method => ~"GET", path => ~"/"}).

parse_bindings(#{bindings := Bindings}) -> Bindings.

parse_params(#{params := Params}) -> Params.

parse_cookies(#{cookies := Cookies}) -> Cookies.

parse_headers(#{headers := Headers}) -> Headers.

read_body(#{body := Body} = Raw) -> {Body, Raw#{body_read => true}}.

resolve_route(_Path, _Qs, _Raw) -> error(test_adapter_resolve_route_not_implemented).
