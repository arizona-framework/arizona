-module(arizona_handler_SUITE).
-behaviour(ct_suite).
-include_lib("stdlib/include/assert.hrl").
-compile([export_all, nowarn_export_all]).

%% --------------------------------------------------------------------
%% Behaviour (ct_suite) callbacks
%% --------------------------------------------------------------------

all() ->
    [
        {group, basic_handler_tests},
        {group, request_processing_tests},
        {group, error_handling_tests}
    ].

groups() ->
    [
        {basic_handler_tests, [sequence], [
            test_simple_component_rendering,
            test_component_with_layout
        ]},
        {request_processing_tests, [sequence], [
            test_get_request_processing,
            test_post_request_processing,
            test_request_with_query_params,
            test_request_with_path_bindings
        ]},
        {error_handling_tests, [sequence], [
            test_nonexistent_module_error,
            test_mount_callback_error
        ]}
    ].

init_per_suite(Config) ->
    % Ensure applications are started
    {ok, _} = application:ensure_all_started(cowboy),
    % For httpc
    {ok, _} = application:ensure_all_started(inets),
    Config.

end_per_suite(_Config) ->
    inets:stop(),
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    % Stop server after each test
    _ = arizona_server:stop(),
    ok.

%% --------------------------------------------------------------------
%% HTTP Helper Functions
%% --------------------------------------------------------------------

%% Simple GET request helper
http_get(Url) ->
    httpc:request(get, {Url, []}, [], [
        {body_format, binary},
        {full_result, false}
    ]).

%% POST request helper
http_post(Url, ContentType, Body) ->
    httpc:request(post, {Url, [], ContentType, Body}, [], [
        {body_format, binary},
        {full_result, false}
    ]).

%% --------------------------------------------------------------------
%% Basic Handler Tests
%% --------------------------------------------------------------------

test_simple_component_rendering(Config) when is_list(Config) ->
    % Start server with simple component
    {ok, _Pid} = arizona_server:start(#{
        port => 8083,
        routes => [{live, ~"/simple", simple_live_component}]
    }),

    % Test HTTP request
    {ok, {200, Body}} = http_get("http://localhost:8083/simple"),

    % Verify component content
    ?assert(binary:match(Body, ~"Simple Live Component") =/= nomatch),
    ?assert(binary:match(Body, ~"basic arizona_live component") =/= nomatch),

    ct:comment("Simple arizona_live component renders via arizona_handler").

test_component_with_layout(Config) when is_list(Config) ->
    % Start server with layout component (test_live_component already has layout)
    {ok, _Pid} = arizona_server:start(#{
        port => 8083,
        routes => [{live, ~"/layout", test_live_component}]
    }),

    % Test HTTP request
    {ok, {200, Body}} = http_get("http://localhost:8083/layout"),

    % Verify layout and component content are both present

    % from component
    ?assert(binary:match(Body, ~"Hello, World!") =/= nomatch),
    % from layout
    ?assert(binary:match(Body, ~"Test Layout") =/= nomatch),

    ct:comment("Component with layout renders correctly via arizona_handler").

%% --------------------------------------------------------------------
%% Request Processing Tests
%% --------------------------------------------------------------------

test_get_request_processing(Config) when is_list(Config) ->
    % Start server with request info component
    {ok, _Pid} = arizona_server:start(#{
        port => 8083,
        routes => [{live, ~"/request-info", request_info_live_component}]
    }),

    % Test GET request
    {ok, {200, Body}} = http_get("http://localhost:8083/request-info"),

    % Verify request method and path are captured
    ?assert(binary:match(Body, ~"Method: GET") =/= nomatch),
    ?assert(binary:match(Body, ~"Path: /request-info") =/= nomatch),
    ?assert(binary:match(Body, ~"Params: none") =/= nomatch),
    ?assert(binary:match(Body, ~"Bindings: none") =/= nomatch),

    ct:comment("GET request processing works correctly").

test_post_request_processing(Config) when is_list(Config) ->
    % Start server with request info component
    {ok, _Pid} = arizona_server:start(#{
        port => 8083,
        routes => [{live, ~"/request-info", request_info_live_component}]
    }),

    % Test POST request with body
    {ok, {200, Body}} = http_post(
        "http://localhost:8083/request-info",
        "application/x-www-form-urlencoded",
        "name=test&value=123"
    ),

    % Verify POST method is captured
    ?assert(binary:match(Body, ~"Method: POST") =/= nomatch),
    ?assert(binary:match(Body, ~"Path: /request-info") =/= nomatch),

    ct:comment("POST request processing works correctly").

test_request_with_query_params(Config) when is_list(Config) ->
    % Start server with request info component
    {ok, _Pid} = arizona_server:start(#{
        port => 8083,
        routes => [{live, ~"/request-info", request_info_live_component}]
    }),

    % Test request with query parameters
    {ok, {200, Body}} = http_get("http://localhost:8083/request-info?name=test&count=42"),

    % Verify query parameters are processed
    ?assert(binary:match(Body, ~"Params: name=test, count=42") =/= nomatch),

    ct:comment("Query parameter processing works correctly").

test_request_with_path_bindings(Config) when is_list(Config) ->
    % Start server with parameterized route
    {ok, _Pid} = arizona_server:start(#{
        port => 8083,
        routes => [{live, ~"/users/:id", request_info_live_component}]
    }),

    % Test request with path parameters
    {ok, {200, Body}} = http_get("http://localhost:8083/users/123"),

    % Verify path bindings are processed
    ?assert(binary:match(Body, ~"Bindings: id=123") =/= nomatch),

    ct:comment("Path binding processing works correctly").

%% --------------------------------------------------------------------
%% Error Handling Tests
%% --------------------------------------------------------------------

test_nonexistent_module_error(Config) when is_list(Config) ->
    % Start server with nonexistent module
    {ok, _Pid} = arizona_server:start(#{
        port => 8083,
        routes => [{live, ~"/nonexistent", nonexistent_module}]
    }),

    % Test request should return 500
    {ok, {500, Body}} = http_get("http://localhost:8083/nonexistent"),

    % Verify error response contains expected error information
    ?assert(binary:match(Body, ~"LiveView Error") =/= nomatch),
    ?assert(binary:match(Body, ~"undef") =/= nomatch),

    ct:comment("Nonexistent module returns 500 error as expected").

test_mount_callback_error(Config) when is_list(Config) ->
    % Create component that throws in mount
    {ok, _} = create_error_component(),

    % Start server with error component
    {ok, _Pid} = arizona_server:start(#{
        port => 8083,
        routes => [{live, ~"/mount-error", mount_error_component}]
    }),

    % Test request should return 500
    {ok, {500, Body}} = http_get("http://localhost:8083/mount-error"),

    % Verify error response contains mount error
    ?assert(binary:match(Body, ~"LiveView Error") =/= nomatch),
    ?assert(binary:match(Body, ~"mount_failed") =/= nomatch),

    ct:comment("Mount callback error is handled with 500 response").

%% --------------------------------------------------------------------
%% Helper Functions
%% --------------------------------------------------------------------

create_error_component() ->
    Code = merl:quote(~""""
    -module(mount_error_component).
    -behaviour(arizona_live).
    -export([mount/2, render/1]).

    mount(_Req, _Socket) ->
        throw(mount_failed).

    render(Socket) ->
        arizona_html:render_live(~"""
        <div>Should not render</div>
        """, Socket).
    """"),
    merl:compile_and_load(Code).
