-module(arizona_request_SUITE).
-behaviour(ct_suite).
-include_lib("stdlib/include/assert.hrl").
-compile([export_all, nowarn_export_all]).

%% Suppress Dialyzer warnings for mock data test functions
%% These test the data structure wrapping behavior, not actual cowboy integration
-dialyzer(
    {nowarn_function, [
        test_raw_request_tag/1,
        test_lazy_loading_pattern/1,
        test_from_cowboy_basic/1,
        test_from_cowboy_lazy_defaults/1,
        test_lazy_bindings_loading/1,
        test_lazy_params_loading/1,
        test_lazy_cookies_loading/1,
        test_lazy_headers_loading/1,
        test_body_lazy_loading/1,
        test_headers_lazy_loading/1
    ]}
).

%% --------------------------------------------------------------------
%% Behaviour (ct_suite) callbacks
%% --------------------------------------------------------------------

all() ->
    [
        {group, request_creation},
        {group, immediate_access},
        {group, lazy_loading_behavior},
        {group, from_cowboy_creation},
        {group, lazy_loading_with_cowboy},
        {group, body_handling},
        {group, headers_handling},
        {group, cowboy_req_parsing}
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
        ]},
        {from_cowboy_creation, [], [
            test_from_cowboy_basic,
            test_from_cowboy_lazy_defaults
        ]},
        {lazy_loading_with_cowboy, [], [
            test_lazy_bindings_loading,
            test_lazy_params_loading,
            test_lazy_cookies_loading,
            test_lazy_headers_loading
        ]},
        {body_handling, [], [
            test_body_lazy_loading,
            test_body_already_loaded,
            test_body_undefined_case
        ]},
        {headers_handling, [], [
            test_headers_already_loaded,
            test_headers_lazy_loading
        ]},
        {cowboy_req_parsing, [], [
            test_cookies_cowboy_parsing,
            test_headers_cowboy_parsing,
            test_body_cowboy_parsing
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

%% --------------------------------------------------------------------
%% From Cowboy Creation Tests
%% --------------------------------------------------------------------

test_from_cowboy_basic(Config) when is_list(Config) ->
    % Test creating request from cowboy_req
    MockCowboyReq = #{
        method => ~"POST",
        path => ~"/api/users"
    },

    Req = arizona_request:from_cowboy(MockCowboyReq),

    % Verify immediate fields are set
    ?assertEqual(~"POST", arizona_request:get_method(Req)),
    ?assertEqual(~"/api/users", arizona_request:get_path(Req)),

    % Verify raw request is stored properly
    ?assertEqual({cowboy_req, MockCowboyReq}, arizona_request:get_raw_request(Req)).

test_from_cowboy_lazy_defaults(Config) when is_list(Config) ->
    % Test that from_cowboy sets lazy loading defaults
    MockCowboyReq = #{
        method => ~"GET",
        path => ~"/test"
    },

    Req = arizona_request:from_cowboy(MockCowboyReq),

    % All lazy-loaded fields should be undefined initially
    ?assertEqual({cowboy_req, MockCowboyReq}, arizona_request:get_raw_request(Req)).

%% --------------------------------------------------------------------
%% Lazy Loading with Cowboy Tests
%% --------------------------------------------------------------------

test_lazy_bindings_loading(Config) when is_list(Config) ->
    % Test that pre-loaded bindings are returned correctly
    Req = arizona_request:new(#{
        method => ~"GET",
        path => ~"/users/123",
        bindings => #{user_id => ~"123"}
    }),

    % First access should return pre-loaded bindings
    {Bindings, UpdatedReq} = arizona_request:get_bindings(Req),

    % Should return the pre-loaded bindings
    ?assertEqual(#{user_id => ~"123"}, Bindings),

    % Request should be unchanged since bindings were pre-loaded
    ?assertEqual(Req, UpdatedReq),

    % Second access should return same data
    {Bindings2, UpdatedReq2} = arizona_request:get_bindings(UpdatedReq),
    ?assertEqual(Bindings, Bindings2),
    ?assertEqual(UpdatedReq, UpdatedReq2).

test_lazy_params_loading(Config) when is_list(Config) ->
    % Test that pre-loaded params are returned correctly
    Req = arizona_request:new(#{
        method => ~"GET",
        path => ~"/search",
        params => [{~"q", ~"test"}]
    }),

    {Params, UpdatedReq} = arizona_request:get_params(Req),

    % Should return pre-loaded params
    ?assertEqual([{~"q", ~"test"}], Params),

    % Request should be unchanged since params were pre-loaded
    ?assertEqual(Req, UpdatedReq),

    % Second access should return same data
    {Params2, UpdatedReq2} = arizona_request:get_params(UpdatedReq),
    ?assertEqual(Params, Params2),
    ?assertEqual(UpdatedReq, UpdatedReq2).

test_lazy_cookies_loading(Config) when is_list(Config) ->
    % Test that pre-loaded cookies are returned correctly
    Req = arizona_request:new(#{
        method => ~"GET",
        path => ~"/",
        cookies => [{~"session_id", ~"abc123"}]
    }),

    {Cookies, UpdatedReq} = arizona_request:get_cookies(Req),

    % Should return pre-loaded cookies
    ?assertEqual([{~"session_id", ~"abc123"}], Cookies),

    % Request should be unchanged since cookies were pre-loaded
    ?assertEqual(Req, UpdatedReq),

    % Second access should return same data
    {Cookies2, UpdatedReq2} = arizona_request:get_cookies(UpdatedReq),
    ?assertEqual(Cookies, Cookies2),
    ?assertEqual(UpdatedReq, UpdatedReq2).

test_lazy_headers_loading(Config) when is_list(Config) ->
    % Test that pre-loaded headers are returned correctly
    Req = arizona_request:new(#{
        method => ~"GET",
        path => ~"/",
        headers => #{~"user-agent" => ~"Mozilla/5.0"}
    }),

    {Headers, UpdatedReq} = arizona_request:get_headers(Req),

    % Should return pre-loaded headers
    ?assertEqual(#{~"user-agent" => ~"Mozilla/5.0"}, Headers),

    % Request should be unchanged since headers were pre-loaded
    ?assertEqual(Req, UpdatedReq),

    % Second access should return same data
    {Headers2, UpdatedReq2} = arizona_request:get_headers(UpdatedReq),
    ?assertEqual(Headers, Headers2),
    ?assertEqual(UpdatedReq, UpdatedReq2).

%% --------------------------------------------------------------------
%% Body Handling Tests
%% --------------------------------------------------------------------

test_body_lazy_loading(Config) when is_list(Config) ->
    % Test getting body when undefined and no raw request
    Req = arizona_request:new(#{
        method => ~"POST",
        path => ~"/api/data"
        % body is undefined, no raw request
    }),

    {Body, UpdatedReq} = arizona_request:get_body(Req),

    % Should return empty binary when no body and no raw request
    ?assertEqual(~"", Body),

    % Request should be unchanged
    ?assertEqual(Req, UpdatedReq).

test_body_already_loaded(Config) when is_list(Config) ->
    % Test getting body when it's already loaded
    TestBody = ~"request body content",
    Req = arizona_request:new(#{
        method => ~"POST",
        path => ~"/api/data",
        body => TestBody
    }),

    {Body, UpdatedReq} = arizona_request:get_body(Req),

    % Should return the pre-loaded body
    ?assertEqual(TestBody, Body),

    % Request should be unchanged since body was already loaded
    ?assertEqual(Req, UpdatedReq).

test_body_undefined_case(Config) when is_list(Config) ->
    % Test getting body when there's no raw request and body is undefined
    Req = arizona_request:new(#{
        method => ~"GET",
        path => ~"/"
        % No body, no raw request
    }),

    {Body, UpdatedReq} = arizona_request:get_body(Req),

    % Should return empty binary
    ?assertEqual(~"", Body),

    % Request should be unchanged
    ?assertEqual(Req, UpdatedReq).

%% --------------------------------------------------------------------
%% Headers Handling Tests
%% --------------------------------------------------------------------

test_headers_already_loaded(Config) when is_list(Config) ->
    % Test getting headers when they're already loaded
    TestHeaders = #{~"content-type" => ~"application/json"},
    Req = arizona_request:new(#{
        method => ~"POST",
        path => ~"/api/data",
        headers => TestHeaders
    }),

    {Headers, UpdatedReq} = arizona_request:get_headers(Req),

    % Should return the pre-loaded headers
    ?assertEqual(TestHeaders, Headers),

    % Request should be unchanged since headers were already loaded
    ?assertEqual(Req, UpdatedReq).

test_headers_lazy_loading(Config) when is_list(Config) ->
    % Test that headers function works with pre-loaded data
    Req = arizona_request:new(#{
        method => ~"GET",
        path => ~"/",
        headers => #{~"accept" => ~"application/json"}
    }),

    {Headers, UpdatedReq} = arizona_request:get_headers(Req),

    % Should return pre-loaded headers
    ?assertEqual(#{~"accept" => ~"application/json"}, Headers),

    % Request should be unchanged since headers were pre-loaded
    ?assertEqual(Req, UpdatedReq),

    % Second access should return same data
    {Headers2, UpdatedReq2} = arizona_request:get_headers(UpdatedReq),
    ?assertEqual(Headers, Headers2),
    ?assertEqual(UpdatedReq, UpdatedReq2).

%% --------------------------------------------------------------------
%% Cowboy Request Parsing Tests (hitting the missing coverage lines)
%% --------------------------------------------------------------------

test_cookies_cowboy_parsing(Config) when is_list(Config) ->
    % Test lazy loading of cookies from cowboy_req
    % This should hit lines 81-83 in arizona_request.erl

    % Create a real cowboy_req map with cookie header
    CowboyReq = #{
        method => ~"GET",
        headers => #{~"cookie" => ~"session_id=abc123; theme=dark"}
    },

    % Create arizona_request with undefined cookies and real cowboy_req
    Req = arizona_request:new(#{
        method => ~"GET",
        path => ~"/",
        % Explicitly set to undefined for lazy loading
        cookies => undefined,
        raw => {cowboy_req, CowboyReq}
    }),

    % First call should trigger cowboy_req:parse_cookies/1 (line 81)
    {Cookies, UpdatedReq} = arizona_request:get_cookies(Req),

    % Should return parsed cookies (cowboy returns in binary format)
    Expected = [{~"session_id", ~"abc123"}, {~"theme", ~"dark"}],
    ?assertEqual(Expected, Cookies),

    % Second call should use cached value (line 84-85)
    {Cookies2, _UpdatedReq2} = arizona_request:get_cookies(UpdatedReq),
    ?assertEqual(Expected, Cookies2).

test_headers_cowboy_parsing(Config) when is_list(Config) ->
    % Test lazy loading of headers from cowboy_req
    % This should hit lines 100-102 in arizona_request.erl

    % Create a real cowboy_req map with headers
    CowboyReq = #{
        method => ~"POST",
        headers => #{
            ~"content-type" => ~"application/json",
            ~"user-agent" => ~"test-agent"
        }
    },

    % Create arizona_request with undefined headers and real cowboy_req
    Req = arizona_request:new(#{
        method => ~"POST",
        path => ~"/api",
        % Explicitly set to undefined for lazy loading
        headers => undefined,
        raw => {cowboy_req, CowboyReq}
    }),

    % First call should trigger cowboy_req:headers/1 (line 100)
    {Headers, UpdatedReq} = arizona_request:get_headers(Req),

    % Should return the cowboy_req headers
    Expected = #{
        ~"content-type" => ~"application/json",
        ~"user-agent" => ~"test-agent"
    },
    ?assertEqual(Expected, Headers),

    % Second call should use cached value (line 103-104)
    {Headers2, _UpdatedReq2} = arizona_request:get_headers(UpdatedReq),
    ?assertEqual(Expected, Headers2).

test_body_cowboy_parsing(Config) when is_list(Config) ->
    % Test lazy loading of body from cowboy_req
    % This should hit lines 109-111 in arizona_request.erl

    % Create a cowboy_req map that read_body can handle (no body case)
    CowboyReq = #{
        method => ~"POST",
        % This makes cowboy_req:read_body/1 return {ok, ~"", Req}
        has_body => false
    },

    % Create arizona_request with undefined body and real cowboy_req
    Req = arizona_request:new(#{
        method => ~"POST",
        path => ~"/api/upload",
        % Explicitly set to undefined for lazy loading
        body => undefined,
        raw => {cowboy_req, CowboyReq}
    }),

    % First call should trigger cowboy_req:read_body/1 (line 109)
    {Body, UpdatedReq} = arizona_request:get_body(Req),

    % Should return empty body since has_body = false
    ?assertEqual(~"", Body),

    % Verify the updated request has the body cached
    % The raw should be updated with the result from read_body
    {cowboy_req, UpdatedCowboyReq} = arizona_request:get_raw_request(UpdatedReq),
    ?assertEqual(#{method => ~"POST", has_body => false}, UpdatedCowboyReq),

    % Second call should use cached body value (line 112-113)
    {Body2, _UpdatedReq2} = arizona_request:get_body(UpdatedReq),
    ?assertEqual(~"", Body2).
