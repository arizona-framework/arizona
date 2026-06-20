-module(arizona_middleware_SUITE).
-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([groups/0]).

-export([apply_middlewares_empty_list/1]).
-export([apply_middlewares_cont_fun/1]).
-export([apply_middlewares_halt_fun/1]).
-export([apply_middlewares_cont_mf/1]).
-export([apply_middlewares_halt_stops_pipeline/1]).
-export([apply_middlewares_threads_bindings/1]).
-export([extract_path_bindings_merges_into_bindings/1]).
-export([extract_named_keys/1]).
-export([extract_is_usable_as_middleware/1]).
-export([put_request_exposes_request_binding/1]).
-export([check_origin_conts_same_origin/1]).
-export([check_origin_conts_missing_origin/1]).
-export([check_origin_halts_cross_origin_with_403/1]).
-export([fetch_session_reads_session_into_binding/1]).

%% Exported for use via {?MODULE, Fun} in apply_middlewares_cont_mf.
-export([sample_cont_middleware/2]).

all() ->
    [{group, middlewares}].

groups() ->
    [
        {middlewares, [parallel], [
            apply_middlewares_empty_list,
            apply_middlewares_cont_fun,
            apply_middlewares_halt_fun,
            apply_middlewares_cont_mf,
            apply_middlewares_halt_stops_pipeline,
            apply_middlewares_threads_bindings,
            extract_path_bindings_merges_into_bindings,
            extract_named_keys,
            extract_is_usable_as_middleware,
            put_request_exposes_request_binding,
            check_origin_conts_same_origin,
            check_origin_conts_missing_origin,
            check_origin_halts_cross_origin_with_403,
            fetch_session_reads_session_into_binding
        ]}
    ].

%% --------------------------------------------------------------------
%% apply_middlewares cases
%% --------------------------------------------------------------------

apply_middlewares_empty_list(Config) when is_list(Config) ->
    Req = arizona_req_test_adapter:new(#{}),
    ?assertEqual({cont, Req, #{}}, arizona_middleware:apply_middlewares([], Req, #{})).

apply_middlewares_cont_fun(Config) when is_list(Config) ->
    Req = arizona_req_test_adapter:new(#{}),
    Mw = [fun(R, B) -> {cont, R, B#{step => 1}} end],
    ?assertEqual({cont, Req, #{step => 1}}, arizona_middleware:apply_middlewares(Mw, Req, #{})).

apply_middlewares_halt_fun(Config) when is_list(Config) ->
    Req = arizona_req_test_adapter:new(#{}),
    Mw = [fun(R, _B) -> {halt, R} end],
    ?assertEqual({halt, Req}, arizona_middleware:apply_middlewares(Mw, Req, #{})).

apply_middlewares_cont_mf(Config) when is_list(Config) ->
    Req = arizona_req_test_adapter:new(#{}),
    Mw = [{?MODULE, sample_cont_middleware}],
    ?assertEqual(
        {cont, Req, #{from_mf => true}},
        arizona_middleware:apply_middlewares(Mw, Req, #{})
    ).

apply_middlewares_halt_stops_pipeline(Config) when is_list(Config) ->
    Req = arizona_req_test_adapter:new(#{}),
    Mw = [
        fun(R, _B) -> {halt, R} end,
        fun(_R, _B) -> error(should_not_reach) end
    ],
    ?assertEqual({halt, Req}, arizona_middleware:apply_middlewares(Mw, Req, #{})).

apply_middlewares_threads_bindings(Config) when is_list(Config) ->
    Req = arizona_req_test_adapter:new(#{}),
    Mw = [
        fun(R, B) -> {cont, R, B#{a => 1}} end,
        fun(R, B) -> {cont, R, B#{b => 2}} end
    ],
    ?assertEqual(
        {cont, Req, #{a => 1, b => 2}},
        arizona_middleware:apply_middlewares(Mw, Req, #{})
    ).

%% --------------------------------------------------------------------
%% extract/1 and put_request/2 -- request data into bindings
%% --------------------------------------------------------------------

extract_path_bindings_merges_into_bindings(Config) when is_list(Config) ->
    %% path_bindings spreads the route's path bindings into the bindings map
    %% (so a handler reads `?get(<<"item_id">>)`), leaving existing keys intact.
    Req = arizona_req_test_adapter:new(#{bindings => #{~"item_id" => ~"42"}}),
    Mw = arizona_middleware:extract([path_bindings]),
    {cont, _Req1, Bindings} = Mw(Req, #{kept => yes}),
    ?assertEqual(~"42", maps:get(~"item_id", Bindings)),
    ?assertEqual(yes, maps:get(kept, Bindings)).

extract_named_keys(Config) when is_list(Config) ->
    %% Non-path keys land under their own name; `method` is eager, the rest lazy.
    Req = arizona_req_test_adapter:new(#{
        params => [{~"locale", ~"pt"}],
        headers => #{~"host" => ~"example.com", ~"user-agent" => ~"Mozilla/5.0"},
        cookies => [{~"session", ~"abc"}],
        body => ~"payload"
    }),
    Mw = arizona_middleware:extract([params, headers, method, cookies, body, user_agent]),
    {cont, _Req1, Bindings} = Mw(Req, #{}),
    ?assertEqual([{~"locale", ~"pt"}], maps:get(params, Bindings)),
    ?assertEqual(
        #{~"host" => ~"example.com", ~"user-agent" => ~"Mozilla/5.0"},
        maps:get(headers, Bindings)
    ),
    ?assertEqual(~"GET", maps:get(method, Bindings)),
    ?assertEqual([{~"session", ~"abc"}], maps:get(cookies, Bindings)),
    ?assertEqual(~"payload", maps:get(body, Bindings)),
    ?assertEqual(~"Mozilla/5.0", maps:get(user_agent, Bindings)).

extract_is_usable_as_middleware(Config) when is_list(Config) ->
    %% The closure extract/1 returns drops straight into a middleware list.
    Req = arizona_req_test_adapter:new(#{params => [{~"q", ~"1"}]}),
    {cont, _Req1, Bindings} = arizona_middleware:apply_middlewares(
        [arizona_middleware:extract([params])], Req, #{}
    ),
    ?assertEqual([{~"q", ~"1"}], maps:get(params, Bindings)).

put_request_exposes_request_binding(Config) when is_list(Config) ->
    Req = arizona_req_test_adapter:new(#{}),
    ?assertEqual({cont, Req, #{request => Req}}, arizona_middleware:put_request(Req, #{})),
    %% Usable as a {Module, Function} middleware.
    ?assertEqual(
        {cont, Req, #{request => Req}},
        arizona_middleware:apply_middlewares([{arizona_middleware, put_request}], Req, #{})
    ).

%% --------------------------------------------------------------------
%% check_origin/2 -- CSRF Origin gate (default app env: enabled, no allowlist)
%% --------------------------------------------------------------------

check_origin_conts_same_origin(Config) when is_list(Config) ->
    Req = arizona_req_test_adapter:new(#{
        headers => #{~"origin" => ~"https://app.example", ~"host" => ~"app.example"}
    }),
    ?assertMatch({cont, _Req1, #{}}, arizona_middleware:check_origin(Req, #{})).

check_origin_conts_missing_origin(Config) when is_list(Config) ->
    Req = arizona_req_test_adapter:new(#{headers => #{~"host" => ~"app.example"}}),
    ?assertMatch({cont, _Req1, #{}}, arizona_middleware:check_origin(Req, #{})).

check_origin_halts_cross_origin_with_403(Config) when is_list(Config) ->
    Req = arizona_req_test_adapter:new(#{
        headers => #{~"origin" => ~"https://evil.example", ~"host" => ~"app.example"}
    }),
    {halt, Req1} = arizona_middleware:check_origin(Req, #{}),
    ?assertEqual(403, arizona_req:resp_status(Req1)).

fetch_session_reads_session_into_binding(Config) when is_list(Config) ->
    application:set_env(arizona, secret_key, ~"test-secret-key-0123456789abcdef"),
    try
        Encoded = arizona_session:encode(#{~"user_id" => ~"7"}),
        Req = arizona_req_test_adapter:new(#{cookies => [{~"az_session", Encoded}]}),
        {cont, _Req1, Bindings} = arizona_middleware:fetch_session(Req, #{}),
        ?assertEqual(#{~"user_id" => ~"7"}, maps:get(session, Bindings))
    after
        application:unset_env(arizona, secret_key)
    end.

%% --------------------------------------------------------------------
%% Module-function middleware helper
%% --------------------------------------------------------------------

sample_cont_middleware(Req, Bindings) ->
    {cont, Req, Bindings#{from_mf => true}}.
