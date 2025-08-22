-module(arizona_request_SUITE).
-behaviour(ct_suite).
-include_lib("stdlib/include/assert.hrl").
-compile([export_all, nowarn_export_all]).

%% --------------------------------------------------------------------
%% Behaviour (ct_suite) callbacks
%% --------------------------------------------------------------------

all() ->
    [
        {group, basic_operations},
        {group, getter_operations},
        {group, lazy_loading_operations},
        {group, adapter_behavior}
    ].

groups() ->
    [
        {basic_operations, [parallel], [
            new_with_minimal_options,
            new_with_full_options
        ]},
        {getter_operations, [parallel], [
            get_method,
            get_path,
            get_raw_request
        ]},
        {lazy_loading_operations, [parallel], [
            get_bindings_preloaded,
            get_params_preloaded,
            get_cookies_preloaded,
            get_headers_preloaded,
            get_body_preloaded
        ]},
        {adapter_behavior, [parallel], [
            get_bindings_lazy_load,
            get_params_lazy_load,
            get_cookies_lazy_load,
            get_headers_lazy_load,
            get_body_lazy_load
        ]}
    ].

init_per_suite(Config) ->
    MockModule = arizona_request_mock_adapter,
    MockAdapterCode = merl:qquote(~"""
    -module('@module').
    -behaviour(arizona_request).

    -export([parse_bindings/1]).
    -export([parse_params/1]).
    -export([parse_cookies/1]).
    -export([parse_headers/1]).
    -export([read_body/1]).

    parse_bindings(RawRequest) ->
        maps:get(bindings, RawRequest, #{user_id => ~"123"}).

    parse_params(RawRequest) ->
        maps:get(params, RawRequest, [{~"q", ~"test"}]).

    parse_cookies(RawRequest) ->
        maps:get(cookies, RawRequest, [{~"session", ~"abc123"}]).

    parse_headers(RawRequest) ->
        maps:get(headers, RawRequest, #{~"accept" => ~"application/json"}).

    read_body(RawRequest) ->
        Body = maps:get(body, RawRequest, ~"{\"test\": true}"),
        {Body, RawRequest}.
    """, [{module, merl:term(MockModule)}]),
    {ok, _Binary} = merl:compile_and_load(MockAdapterCode),

    [{mock_module, MockModule} | Config].

end_per_suite(Config) ->
    {mock_module, MockModule} = proplists:lookup(mock_module, Config),
    code:purge(MockModule),
    code:delete(MockModule),

    ok.

%% --------------------------------------------------------------------
%% Basic operations tests
%% --------------------------------------------------------------------

new_with_minimal_options(Config) when is_list(Config) ->
    ct:comment("new/3 should create request with minimal options"),
    {mock_module, MockModule} = proplists:lookup(mock_module, Config),
    Request = arizona_request:new(MockModule, #{}, #{}),
    ?assertEqual(~"GET", arizona_request:get_method(Request)),
    ?assertEqual(~"/", arizona_request:get_path(Request)),
    ?assertEqual(#{}, arizona_request:get_raw_request(Request)).

new_with_full_options(Config) when is_list(Config) ->
    ct:comment("new/3 should create request with provided options"),
    {mock_module, MockModule} = proplists:lookup(mock_module, Config),
    RawRequest = #{test => ~"data"},
    Opts = #{
        method => ~"POST",
        path => ~"/api/users",
        bindings => #{id => ~"456"},
        params => [{~"limit", ~"10"}],
        cookies => [{~"auth", ~"token123"}],
        headers => #{~"content-type" => ~"application/json"},
        body => ~"{\"name\":\"test\"}"
    },
    Request = arizona_request:new(MockModule, RawRequest, Opts),
    ?assertEqual(~"POST", arizona_request:get_method(Request)),
    ?assertEqual(~"/api/users", arizona_request:get_path(Request)),
    ?assertEqual(RawRequest, arizona_request:get_raw_request(Request)).

%% --------------------------------------------------------------------
%% Getter operations tests
%% --------------------------------------------------------------------

get_method(Config) when is_list(Config) ->
    ct:comment("get_method/1 should return request method"),
    {mock_module, MockModule} = proplists:lookup(mock_module, Config),
    Request = arizona_request:new(MockModule, #{}, #{method => ~"DELETE"}),
    ?assertEqual(~"DELETE", arizona_request:get_method(Request)).

get_path(Config) when is_list(Config) ->
    ct:comment("get_path/1 should return request path"),
    {mock_module, MockModule} = proplists:lookup(mock_module, Config),
    Request = arizona_request:new(MockModule, #{}, #{path => ~"/api/v1/test"}),
    ?assertEqual(~"/api/v1/test", arizona_request:get_path(Request)).

get_raw_request(Config) when is_list(Config) ->
    ct:comment("get_raw_request/1 should return raw request data"),
    {mock_module, MockModule} = proplists:lookup(mock_module, Config),
    RawData = #{server => cowboy, version => ~"2.9"},
    Request = arizona_request:new(MockModule, RawData, #{}),
    ?assertEqual(RawData, arizona_request:get_raw_request(Request)).

%% --------------------------------------------------------------------
%% Lazy loading operations tests
%% --------------------------------------------------------------------

get_bindings_preloaded(Config) when is_list(Config) ->
    ct:comment("get_bindings/1 should return preloaded bindings without adapter call"),
    {mock_module, MockModule} = proplists:lookup(mock_module, Config),
    Bindings = #{user_id => ~"999", category => ~"books"},
    Request = arizona_request:new(MockModule, #{}, #{bindings => Bindings}),
    {ResultBindings, UpdatedRequest} = arizona_request:get_bindings(Request),
    ?assertEqual(Bindings, ResultBindings),
    ?assertEqual(Request, UpdatedRequest).

get_params_preloaded(Config) when is_list(Config) ->
    ct:comment("get_params/1 should return preloaded params without adapter call"),
    {mock_module, MockModule} = proplists:lookup(mock_module, Config),
    Params = [{~"page", ~"1"}, {~"size", ~"20"}],
    Request = arizona_request:new(MockModule, #{}, #{params => Params}),
    {ResultParams, UpdatedRequest} = arizona_request:get_params(Request),
    ?assertEqual(Params, ResultParams),
    ?assertEqual(Request, UpdatedRequest).

get_cookies_preloaded(Config) when is_list(Config) ->
    ct:comment("get_cookies/1 should return preloaded cookies without adapter call"),
    {mock_module, MockModule} = proplists:lookup(mock_module, Config),
    Cookies = [{~"user", ~"john"}, {~"theme", ~"dark"}],
    Request = arizona_request:new(MockModule, #{}, #{cookies => Cookies}),
    {ResultCookies, UpdatedRequest} = arizona_request:get_cookies(Request),
    ?assertEqual(Cookies, ResultCookies),
    ?assertEqual(Request, UpdatedRequest).

get_headers_preloaded(Config) when is_list(Config) ->
    ct:comment("get_headers/1 should return preloaded headers without adapter call"),
    {mock_module, MockModule} = proplists:lookup(mock_module, Config),
    Headers = #{~"user-agent" => ~"test/1.0", ~"accept" => ~"text/html"},
    Request = arizona_request:new(MockModule, #{}, #{headers => Headers}),
    {ResultHeaders, UpdatedRequest} = arizona_request:get_headers(Request),
    ?assertEqual(Headers, ResultHeaders),
    ?assertEqual(Request, UpdatedRequest).

get_body_preloaded(Config) when is_list(Config) ->
    ct:comment("get_body/1 should return preloaded body without adapter call"),
    {mock_module, MockModule} = proplists:lookup(mock_module, Config),
    Body = ~"{\"data\":\"preloaded\"}",
    Request = arizona_request:new(MockModule, #{}, #{body => Body}),
    {ResultBody, UpdatedRequest} = arizona_request:get_body(Request),
    ?assertEqual(Body, ResultBody),
    ?assertEqual(Request, UpdatedRequest).

%% --------------------------------------------------------------------
%% Adapter behavior tests
%% --------------------------------------------------------------------

get_bindings_lazy_load(Config) when is_list(Config) ->
    ct:comment("get_bindings/1 should call adapter when bindings undefined"),
    {mock_module, MockModule} = proplists:lookup(mock_module, Config),
    RawRequest = #{bindings => #{custom => ~"value"}},
    Request = arizona_request:new(MockModule, RawRequest, #{bindings => undefined}),
    {ResultBindings, UpdatedRequest} = arizona_request:get_bindings(Request),
    ?assertEqual(#{custom => ~"value"}, ResultBindings),
    ?assertNotEqual(Request, UpdatedRequest),
    ct:comment("Second call should use cached value"),
    {ResultBindings2, UpdatedRequest2} = arizona_request:get_bindings(UpdatedRequest),
    ?assertEqual(#{custom => ~"value"}, ResultBindings2),
    ?assertEqual(UpdatedRequest, UpdatedRequest2).

get_params_lazy_load(Config) when is_list(Config) ->
    ct:comment("get_params/1 should call adapter when params undefined"),
    {mock_module, MockModule} = proplists:lookup(mock_module, Config),
    RawRequest = #{params => [{~"search", ~"query"}]},
    Request = arizona_request:new(MockModule, RawRequest, #{params => undefined}),
    {ResultParams, UpdatedRequest} = arizona_request:get_params(Request),
    ?assertEqual([{~"search", ~"query"}], ResultParams),
    ?assertNotEqual(Request, UpdatedRequest),
    ct:comment("Second call should use cached value"),
    {ResultParams2, UpdatedRequest2} = arizona_request:get_params(UpdatedRequest),
    ?assertEqual([{~"search", ~"query"}], ResultParams2),
    ?assertEqual(UpdatedRequest, UpdatedRequest2).

get_cookies_lazy_load(Config) when is_list(Config) ->
    ct:comment("get_cookies/1 should call adapter when cookies undefined"),
    {mock_module, MockModule} = proplists:lookup(mock_module, Config),
    RawRequest = #{cookies => [{~"lang", ~"en"}]},
    Request = arizona_request:new(MockModule, RawRequest, #{cookies => undefined}),
    {ResultCookies, UpdatedRequest} = arizona_request:get_cookies(Request),
    ?assertEqual([{~"lang", ~"en"}], ResultCookies),
    ?assertNotEqual(Request, UpdatedRequest),
    ct:comment("Second call should use cached value"),
    {ResultCookies2, UpdatedRequest2} = arizona_request:get_cookies(UpdatedRequest),
    ?assertEqual([{~"lang", ~"en"}], ResultCookies2),
    ?assertEqual(UpdatedRequest, UpdatedRequest2).

get_headers_lazy_load(Config) when is_list(Config) ->
    ct:comment("get_headers/1 should call adapter when headers undefined"),
    {mock_module, MockModule} = proplists:lookup(mock_module, Config),
    RawRequest = #{headers => #{~"authorization" => ~"Bearer token"}},
    Request = arizona_request:new(MockModule, RawRequest, #{headers => undefined}),
    {ResultHeaders, UpdatedRequest} = arizona_request:get_headers(Request),
    ?assertEqual(#{~"authorization" => ~"Bearer token"}, ResultHeaders),
    ?assertNotEqual(Request, UpdatedRequest),
    ct:comment("Second call should use cached value"),
    {ResultHeaders2, UpdatedRequest2} = arizona_request:get_headers(UpdatedRequest),
    ?assertEqual(#{~"authorization" => ~"Bearer token"}, ResultHeaders2),
    ?assertEqual(UpdatedRequest, UpdatedRequest2).

get_body_lazy_load(Config) when is_list(Config) ->
    ct:comment("get_body/1 should call adapter when body undefined"),
    {mock_module, MockModule} = proplists:lookup(mock_module, Config),
    RawRequest = #{body => ~"{\"lazy\":\"loaded\"}"},
    Request = arizona_request:new(MockModule, RawRequest, #{body => undefined}),
    {ResultBody, UpdatedRequest} = arizona_request:get_body(Request),
    ?assertEqual(~"{\"lazy\":\"loaded\"}", ResultBody),
    ?assertNotEqual(Request, UpdatedRequest),
    ct:comment("Second call should use cached value"),
    {ResultBody2, UpdatedRequest2} = arizona_request:get_body(UpdatedRequest),
    ?assertEqual(~"{\"lazy\":\"loaded\"}", ResultBody2),
    ?assertEqual(UpdatedRequest, UpdatedRequest2).
