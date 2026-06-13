-module(arizona_req_SUITE).
-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).

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
-export([put_resp_header_accumulates/1]).
-export([put_resp_cookie_accumulates/1]).
-export([put_resp_status_stashes/1]).
-export([resp_stash_defaults_empty/1]).
-export([put_flash_serializes_into_resp_cookies/1]).
-export([read_flash_round_trips_and_clears/1]).
-export([read_flash_absent_is_empty/1]).
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
            body_threads_updated_raw,
            put_resp_header_accumulates,
            put_resp_cookie_accumulates,
            put_resp_status_stashes,
            resp_stash_defaults_empty,
            put_flash_serializes_into_resp_cookies,
            read_flash_round_trips_and_clears,
            read_flash_absent_is_empty
        ]},
        {resolve_route, [parallel], [
            call_resolve_route_dispatches_to_adapter
        ]}
    ].

init_per_suite(Config) ->
    application:set_env(arizona, secret_key, ~"test-secret-key-0123456789abcdef"),
    Config.

end_per_suite(_Config) ->
    application:unset_env(arizona, secret_key),
    ok.

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
%% Response stash -- put_resp_header/3, put_resp_cookie/4, put_resp_status/2
%% --------------------------------------------------------------------

put_resp_header_accumulates(Config) when is_list(Config) ->
    Req0 = arizona_req_test_adapter:new(#{}),
    ?assertEqual([], arizona_req:resp_headers(Req0)),
    Req1 = arizona_req:put_resp_header(Req0, ~"x-a", ~"1"),
    Req2 = arizona_req:put_resp_header(Req1, ~"x-b", ~"2"),
    %% Newest first.
    ?assertEqual([{~"x-b", ~"2"}, {~"x-a", ~"1"}], arizona_req:resp_headers(Req2)).

put_resp_cookie_accumulates(Config) when is_list(Config) ->
    Req0 = arizona_req_test_adapter:new(#{}),
    ?assertEqual([], arizona_req:resp_cookies(Req0)),
    Opts = #{path => ~"/", http_only => true},
    Req1 = arizona_req:put_resp_cookie(Req0, ~"session", ~"abc", Opts),
    Req2 = arizona_req:put_resp_cookie(Req1, ~"theme", ~"dark", #{}),
    ?assertEqual(
        [{~"theme", ~"dark", #{}}, {~"session", ~"abc", Opts}],
        arizona_req:resp_cookies(Req2)
    ).

put_resp_status_stashes(Config) when is_list(Config) ->
    Req0 = arizona_req_test_adapter:new(#{}),
    ?assertEqual(undefined, arizona_req:resp_status(Req0)),
    Req1 = arizona_req:put_resp_status(Req0, 401),
    ?assertEqual(401, arizona_req:resp_status(Req1)),
    %% Last write wins.
    Req2 = arizona_req:put_resp_status(Req1, 404),
    ?assertEqual(404, arizona_req:resp_status(Req2)).

resp_stash_defaults_empty(Config) when is_list(Config) ->
    Req = arizona_req_test_adapter:new(#{}),
    ?assertEqual([], arizona_req:resp_headers(Req)),
    ?assertEqual([], arizona_req:resp_cookies(Req)),
    ?assertEqual(undefined, arizona_req:resp_status(Req)).

put_flash_serializes_into_resp_cookies(Config) when is_list(Config) ->
    Req0 = arizona_req_test_adapter:new(#{}),
    Req1 = arizona_req:put_flash(Req0, error, ~"Invalid email or password."),
    %% resp_cookies includes the signed flash cookie, which round-trips back.
    [{Name, Value, Opts}] = arizona_req:resp_cookies(Req1),
    ?assertEqual(~"az_flash", Name),
    ?assert(maps:get(max_age, Opts) > 0),
    ?assertEqual(#{~"error" => ~"Invalid email or password."}, arizona_flash:decode(Value)).

read_flash_round_trips_and_clears(Config) when is_list(Config) ->
    %% A request carrying a valid flash cookie reads it, and the response clears
    %% it (read-once).
    Encoded = arizona_flash:encode(#{~"error" => ~"nope"}),
    Req0 = arizona_req_test_adapter:new(#{cookies => [{~"az_flash", Encoded}]}),
    {Flash, Req1} = arizona_req:read_flash(Req0),
    ?assertEqual(#{~"error" => ~"nope"}, Flash),
    ?assertEqual(Flash, arizona_req:flash(Req1)),
    [{~"az_flash", <<>>, Opts}] = arizona_req:resp_cookies(Req1),
    ?assertEqual(0, maps:get(max_age, Opts)).

read_flash_absent_is_empty(Config) when is_list(Config) ->
    Req0 = arizona_req_test_adapter:new(#{cookies => []}),
    {Flash, Req1} = arizona_req:read_flash(Req0),
    ?assertEqual(#{}, Flash),
    %% Nothing consumed and nothing set -> no flash cookie on the response.
    ?assertEqual([], arizona_req:resp_cookies(Req1)).

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
