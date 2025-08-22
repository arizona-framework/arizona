-module(arizona_cowboy_request_SUITE).
-behaviour(ct_suite).
-include_lib("stdlib/include/assert.hrl").
-compile([export_all, nowarn_export_all]).

%% --------------------------------------------------------------------
%% Behaviour (ct_suite) callbacks
%% --------------------------------------------------------------------

all() ->
    [
        {group, new_function_tests},
        {group, behavior_callback_tests}
    ].

groups() ->
    [
        {new_function_tests, [parallel], [
            new_basic_request,
            new_request_extracts_method_path
        ]},
        {behavior_callback_tests, [parallel], [
            parse_bindings_test,
            parse_params_test,
            parse_cookies_test,
            parse_headers_test,
            read_body_test
        ]}
    ].

%% --------------------------------------------------------------------
%% New function tests
%% --------------------------------------------------------------------

new_basic_request(Config) when is_list(Config) ->
    ct:comment("new/1 should create arizona_request from cowboy request"),
    CowboyRequest = mock_cowboy_request(),
    ArizonaRequest = arizona_cowboy_request:new(CowboyRequest),
    ?assertEqual(~"GET", arizona_request:get_method(ArizonaRequest)),
    ?assertEqual(~"/", arizona_request:get_path(ArizonaRequest)),
    ?assertEqual(CowboyRequest, arizona_request:get_raw_request(ArizonaRequest)).

new_request_extracts_method_path(Config) when is_list(Config) ->
    ct:comment("new/1 should extract method and path from cowboy request"),
    CowboyRequest = mock_cowboy_request(#{
        method => ~"POST",
        path => ~"/api/users"
    }),
    ArizonaRequest = arizona_cowboy_request:new(CowboyRequest),
    ?assertEqual(~"POST", arizona_request:get_method(ArizonaRequest)),
    ?assertEqual(~"/api/users", arizona_request:get_path(ArizonaRequest)).

%% --------------------------------------------------------------------
%% Behavior callback tests
%% --------------------------------------------------------------------

parse_bindings_test(Config) when is_list(Config) ->
    ct:comment("parse_bindings/1 should extract bindings from cowboy request"),
    CowboyRequest = mock_cowboy_request(#{
        bindings => #{user_id => ~"123", category => ~"books"}
    }),
    Bindings = arizona_cowboy_request:parse_bindings(CowboyRequest),
    ?assertEqual(#{user_id => ~"123", category => ~"books"}, Bindings).

parse_params_test(Config) when is_list(Config) ->
    ct:comment("parse_params/1 should parse query string from cowboy request"),
    CowboyRequest = mock_cowboy_request(#{
        qs => ~"page=1&limit=10&active"
    }),
    Params = arizona_cowboy_request:parse_params(CowboyRequest),
    ?assertEqual([{~"page", ~"1"}, {~"limit", ~"10"}, {~"active", true}], Params).

parse_cookies_test(Config) when is_list(Config) ->
    ct:comment("parse_cookies/1 should parse cookie header from cowboy request"),
    CowboyRequest = mock_cowboy_request(#{
        headers => #{~"cookie" => ~"session=abc123; theme=dark"}
    }),
    Cookies = arizona_cowboy_request:parse_cookies(CowboyRequest),
    ?assertEqual([{~"session", ~"abc123"}, {~"theme", ~"dark"}], Cookies).

parse_headers_test(Config) when is_list(Config) ->
    ct:comment("parse_headers/1 should extract headers from cowboy request"),
    Headers = #{
        ~"accept" => ~"application/json",
        ~"user-agent" => ~"test/1.0"
    },
    CowboyRequest = mock_cowboy_request(#{headers => Headers}),
    ResultHeaders = arizona_cowboy_request:parse_headers(CowboyRequest),
    ?assertEqual(Headers, ResultHeaders).

read_body_test(Config) when is_list(Config) ->
    ct:comment("read_body/1 should handle cowboy_req fast paths"),

    ct:comment("Test case 1: has_body := false should return empty body"),
    CowboyRequestNoBody = mock_cowboy_request(#{has_body => false}),
    {ResultBody1, UpdatedRequest1} = arizona_cowboy_request:read_body(CowboyRequestNoBody),
    ?assertEqual(~"", ResultBody1),
    ?assertEqual(CowboyRequestNoBody, UpdatedRequest1),

    ct:comment("Test case 2: has_read_body := true should return empty body"),
    CowboyRequestAlreadyRead = mock_cowboy_request(#{
        has_body => true,
        has_read_body => true
    }),
    {ResultBody2, UpdatedRequest2} = arizona_cowboy_request:read_body(CowboyRequestAlreadyRead),
    ?assertEqual(~"", ResultBody2),
    ?assertEqual(CowboyRequestAlreadyRead, UpdatedRequest2).

%% --------------------------------------------------------------------
%% Helper functions
%% --------------------------------------------------------------------

mock_cowboy_request() ->
    mock_cowboy_request(#{}).

mock_cowboy_request(Overrides) ->
    BaseRequest = #{
        method => ~"GET",
        version => 'HTTP/1.1',
        scheme => ~"http",
        host => ~"localhost",
        port => 8080,
        path => ~"/",
        qs => ~"",
        headers => #{},
        bindings => #{},
        has_body => is_binary(maps:get(body, Overrides, undefined)),
        peer => {{127, 0, 0, 1}, 12345},
        sock => {{127, 0, 0, 1}, 8080},
        cert => undefined,
        ref => test_ref,
        pid => self(),
        streamid => 1
    },
    maps:merge(BaseRequest, Overrides).
