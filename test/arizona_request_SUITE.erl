-module(arizona_request_SUITE).
-behaviour(ct_suite).
-include_lib("stdlib/include/assert.hrl").
-compile([export_all, nowarn_export_all]).

%% Suppress Dialyzer warnings for mock data test functions
%% These test the data structure wrapping behavior, not actual cowboy integration
-dialyzer(
    {nowarn_function, [
        test_raw_request_tag/1,
        test_lazy_loading_pattern/1
    ]}
).

%% --------------------------------------------------------------------
%% Behaviour (ct_suite) callbacks
%% --------------------------------------------------------------------

all() ->
    [
        {group, request_creation},
        {group, immediate_access},
        {group, lazy_loading_behavior}
    ].

groups() ->
    [
        {request_creation, [], [
            test_new_request_basic,
            test_new_request_with_options,
            test_raw_request_tag
        ]},
        {immediate_access, [], [
            test_immediate_method,
            test_immediate_path
        ]},
        {lazy_loading_behavior, [], [
            test_lazy_loading_pattern,
            test_caching_pattern
        ]}
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

%% --------------------------------------------------------------------
%% Request Creation Tests
%% --------------------------------------------------------------------

test_new_request_basic(Config) when is_list(Config) ->
    % Test creating basic request with minimal options
    Req = arizona_request:new(#{}),

    % Verify defaults
    ?assertEqual(~"GET", arizona_request:get_method(Req)),
    ?assertEqual(~"/", arizona_request:get_path(Req)),
    ?assertEqual(undefined, arizona_request:get_raw_request(Req)).

test_new_request_with_options(Config) when is_list(Config) ->
    % Test creating request with full options
    Req = arizona_request:new(#{
        method => ~"POST",
        path => ~"/users/123",
        bindings => #{user_id => ~"123"},
        params => [{~"tab", ~"profile"}],
        cookies => [{~"session_id", ~"abc123"}],
        headers => #{~"content-type" => ~"application/json"},
        body => ~"request body"
    }),

    % Verify all fields are set
    ?assertEqual(~"POST", arizona_request:get_method(Req)),
    ?assertEqual(~"/users/123", arizona_request:get_path(Req)),
    ?assertEqual(undefined, arizona_request:get_raw_request(Req)),

    % Test that pre-loaded values work without lazy loading
    {Bindings, Req1} = arizona_request:get_bindings(Req),
    ?assertEqual(#{user_id => ~"123"}, Bindings),
    % Should be same since no lazy loading needed
    ?assertEqual(Req, Req1).

test_raw_request_tag(Config) when is_list(Config) ->
    % Test that raw request is properly tagged
    MockData = mock_cowboy_req(),
    Req = arizona_request:new(#{raw => {cowboy_req, MockData}}),

    ?assertEqual({cowboy_req, MockData}, arizona_request:get_raw_request(Req)).

%% --------------------------------------------------------------------
%% Immediate Access Tests
%% --------------------------------------------------------------------

test_immediate_method(Config) when is_list(Config) ->
    Req = arizona_request:new(#{method => ~"POST"}),

    % Method should be immediately available
    ?assertEqual(~"POST", arizona_request:get_method(Req)).

test_immediate_path(Config) when is_list(Config) ->
    Req = arizona_request:new(#{path => ~"/test/path"}),

    % Path should be immediately available
    ?assertEqual(~"/test/path", arizona_request:get_path(Req)).

%% --------------------------------------------------------------------
%% Lazy Loading Behavior Tests
%% --------------------------------------------------------------------

test_lazy_loading_pattern(Config) when is_list(Config) ->
    % Test that undefined fields follow lazy loading pattern
    Req = arizona_request:new(#{
        method => ~"GET",
        path => ~"/test",
        % Leave bindings, params, etc as undefined to test lazy loading
        raw => {cowboy_req, mock_cowboy_req()}
    }),

    % Test that requests with undefined fields need raw data for lazy loading
    % Since we don't have real cowboy_req functions, we test the structure
    RawData = arizona_request:get_raw_request(Req),
    ?assertEqual({cowboy_req, mock_cowboy_req()}, RawData).

test_caching_pattern(Config) when is_list(Config) ->
    % Test caching behavior with pre-loaded data
    Req = arizona_request:new(#{
        bindings => #{user_id => ~"123"},
        params => [{~"tab", ~"profile"}],
        cookies => [{~"session", ~"abc"}]
    }),

    % First call should return cached data without modification
    {Bindings1, Req1} = arizona_request:get_bindings(Req),
    ?assertEqual(#{user_id => ~"123"}, Bindings1),
    % Should be same request since no lazy loading
    ?assertEqual(Req, Req1),

    % Second call should also return same data
    {Bindings2, Req2} = arizona_request:get_bindings(Req1),
    ?assertEqual(Bindings1, Bindings2),
    % Still same request
    ?assertEqual(Req1, Req2),

    % Test with params
    {Params1, Req3} = arizona_request:get_params(Req),
    ?assertEqual([{~"tab", ~"profile"}], Params1),
    % Should be same since pre-loaded
    ?assertEqual(Req, Req3),

    % Test with cookies
    {Cookies1, Req4} = arizona_request:get_cookies(Req),
    ?assertEqual([{~"session", ~"abc"}], Cookies1),
    % Should be same since pre-loaded
    ?assertEqual(Req, Req4).

%% --------------------------------------------------------------------
%% Helper Functions
%% --------------------------------------------------------------------

%% Helper function to create a mock cowboy_req that matches the expected structure
mock_cowboy_req() ->
    #{
        method => ~"GET",
        version => 'HTTP/1.1',
        scheme => ~"http",
        host => ~"localhost",
        port => 8080,
        path => ~"/",
        qs => ~"",
        headers => #{},
        peer => {{127, 0, 0, 1}, 12345},
        sock => {{127, 0, 0, 1}, 8080},
        cert => undefined,
        ref => test_ref,
        pid => self(),
        streamid => 1
    }.
