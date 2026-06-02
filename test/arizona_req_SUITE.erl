-module(arizona_req_SUITE).
-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([groups/0]).

-export([new_stores_adapter_method_and_path/1]).
-export([new_accepts_prepopulated_fields/1]).
-export([method_and_path_are_eager/1]).
-export([raw_returns_underlying/1]).
-export([bindings_lazy_loads_and_caches/1]).
-export([params_preserves_repeats/1]).
-export([cookies_lazy_loads_and_caches/1]).
-export([headers_lazy_loads_and_caches/1]).
-export([user_agent_returns_header/1]).
-export([user_agent_absent_is_empty/1]).
-export([body_threads_updated_raw/1]).
-export([call_resolve_route_dispatches_to_adapter/1]).

all() ->
    [{group, accessors}, {group, resolve_route}].

groups() ->
    [
        {accessors, [parallel], [
            new_stores_adapter_method_and_path,
            new_accepts_prepopulated_fields,
            method_and_path_are_eager,
            raw_returns_underlying,
            bindings_lazy_loads_and_caches,
            params_preserves_repeats,
            cookies_lazy_loads_and_caches,
            headers_lazy_loads_and_caches,
            user_agent_returns_header,
            user_agent_absent_is_empty,
            body_threads_updated_raw
        ]},
        {resolve_route, [parallel], [
            call_resolve_route_dispatches_to_adapter
        ]}
    ].

%% --------------------------------------------------------------------
%% Accessor cases
%% --------------------------------------------------------------------

new_stores_adapter_method_and_path(Config) when is_list(Config) ->
    Req = arizona_req:new(arizona_req_test_adapter, #{}, #{
        method => ~"POST", path => ~"/items"
    }),
    ?assertEqual(~"POST", arizona_req:method(Req)),
    ?assertEqual(~"/items", arizona_req:path(Req)).

new_accepts_prepopulated_fields(Config) when is_list(Config) ->
    %% Pre-populating a field short-circuits the adapter callback on
    %% first access. Set distinct values in Raw vs Opts; the accessor
    %% must return the Opts value (pre-populated) not the Raw one
    %% (which would come back from the adapter).
    Raw = #{
        bindings => #{~"id" => ~"from_adapter"},
        params => [{~"x", ~"from_adapter"}]
    },
    Req = arizona_req:new(arizona_req_test_adapter, Raw, #{
        method => ~"GET",
        path => ~"/",
        bindings => #{~"id" => ~"from_opts"},
        params => [{~"x", ~"from_opts"}]
    }),
    {Bindings, Req1} = arizona_req:bindings(Req),
    ?assertEqual(#{~"id" => ~"from_opts"}, Bindings),
    {Params, _Req2} = arizona_req:params(Req1),
    ?assertEqual([{~"x", ~"from_opts"}], Params).

method_and_path_are_eager(Config) when is_list(Config) ->
    Req = arizona_req_test_adapter:new(#{}),
    %% Accessors return direct values, no threading.
    ?assertEqual(~"GET", arizona_req:method(Req)),
    ?assertEqual(~"/", arizona_req:path(Req)).

raw_returns_underlying(Config) when is_list(Config) ->
    Raw = #{marker => raw_value},
    Req = arizona_req_test_adapter:new(Raw),
    ?assertEqual(Raw, arizona_req:raw(Req)).

bindings_lazy_loads_and_caches(Config) when is_list(Config) ->
    Raw = #{bindings => #{~"user_id" => ~"42"}},
    Req0 = arizona_req_test_adapter:new(Raw),
    %% First call dispatches to the adapter and returns a cached request.
    {Bindings, Req1} = arizona_req:bindings(Req0),
    ?assertEqual(#{~"user_id" => ~"42"}, Bindings),
    ?assertNotEqual(Req0, Req1),
    %% Second call short-circuits via the cache.
    ?assertEqual({Bindings, Req1}, arizona_req:bindings(Req1)).

params_preserves_repeats(Config) when is_list(Config) ->
    Raw = #{params => [{~"tag", ~"a"}, {~"tag", ~"b"}, {~"sort", ~"asc"}]},
    Req = arizona_req_test_adapter:new(Raw),
    {Params, _Req1} = arizona_req:params(Req),
    ?assertEqual([{~"tag", ~"a"}, {~"tag", ~"b"}, {~"sort", ~"asc"}], Params).

cookies_lazy_loads_and_caches(Config) when is_list(Config) ->
    Raw = #{cookies => [{~"session", ~"abc"}]},
    Req0 = arizona_req_test_adapter:new(Raw),
    {Cookies, Req1} = arizona_req:cookies(Req0),
    ?assertEqual([{~"session", ~"abc"}], Cookies),
    ?assertEqual({Cookies, Req1}, arizona_req:cookies(Req1)).

headers_lazy_loads_and_caches(Config) when is_list(Config) ->
    Raw = #{headers => #{~"host" => ~"example.com"}},
    Req0 = arizona_req_test_adapter:new(Raw),
    {Headers, Req1} = arizona_req:headers(Req0),
    ?assertEqual(#{~"host" => ~"example.com"}, Headers),
    ?assertEqual({Headers, Req1}, arizona_req:headers(Req1)).

user_agent_returns_header(Config) when is_list(Config) ->
    UA = ~"Mozilla/5.0 (Windows NT 10.0; Win64) Chrome/120.0 Safari/537.36",
    Req0 = arizona_req_test_adapter:new(#{headers => #{~"user-agent" => UA}}),
    {Got, Req1} = arizona_req:user_agent(Req0),
    ?assertEqual(UA, Got),
    %% Built on headers/1, which caches; a second read short-circuits.
    ?assertEqual({UA, Req1}, arizona_req:user_agent(Req1)).

user_agent_absent_is_empty(Config) when is_list(Config) ->
    Req = arizona_req_test_adapter:new(#{headers => #{}}),
    {Got, _Req1} = arizona_req:user_agent(Req),
    ?assertEqual(<<>>, Got).

body_threads_updated_raw(Config) when is_list(Config) ->
    %% read_body in the test adapter returns {Body, Raw#{body_read => true}}.
    %% The accessor must update both the cached body and the raw.
    Raw = #{body => ~"hello"},
    Req0 = arizona_req_test_adapter:new(Raw),
    {Body, Req1} = arizona_req:body(Req0),
    ?assertEqual(~"hello", Body),
    ?assertEqual(#{body => ~"hello", body_read => true}, arizona_req:raw(Req1)).

%% --------------------------------------------------------------------
%% resolve_route -- exercises the optional callback wiring
%% --------------------------------------------------------------------

call_resolve_route_dispatches_to_adapter(Config) when is_list(Config) ->
    Routes = #{~"/items" => {items_handler, #{bindings => #{kind => ~"item"}}}},
    Raw = #{routes => Routes},
    {Handler, RouteOpts, NavReq} = arizona_req:call_resolve_route(
        arizona_req_test_adapter, ~"/items", ~"q=1", Raw
    ),
    ?assertEqual(items_handler, Handler),
    ?assertEqual(#{bindings => #{kind => ~"item"}}, RouteOpts),
    ?assertEqual(~"/items", arizona_req:path(NavReq)),
    ?assertEqual(arizona_req_test_adapter, arizona_req:adapter(NavReq)).
