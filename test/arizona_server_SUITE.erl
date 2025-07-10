-module(arizona_server_SUITE).
-behaviour(ct_suite).
-include_lib("stdlib/include/assert.hrl").
-compile([export_all, nowarn_export_all]).

%% --------------------------------------------------------------------
%% Behaviour (ct_suite) callbacks
%% --------------------------------------------------------------------

all() ->
    [
        {group, unit_tests},
        {group, integration_tests}
    ].

groups() ->
    [
        {unit_tests, [parallel], [
            test_route_to_cowboy_live,
            test_route_to_cowboy_live_websocket,
            test_route_to_cowboy_static_dir,
            test_route_to_cowboy_static_file,
            test_route_to_cowboy_priv_dir,
            test_route_to_cowboy_priv_file,
            test_route_metadata_functions
        ]},
        {integration_tests, [sequence], [
            test_server_lifecycle,
            test_live_route_integration,
            test_static_file_integration,
            test_static_directory_integration
        ]}
    ].

init_per_suite(Config) ->
    % Ensure applications are started
    {ok, _} = application:ensure_all_started(cowboy),
    {ok, _} = application:ensure_all_started(inets),
    Config.

end_per_suite(_Config) ->
    inets:stop(),
    ok.

init_per_testcase(_Testcase, Config) ->
    Config.

end_per_testcase(_Testcase, _Config) ->
    % Stop server if running
    _ = arizona_server:stop(),
    ok.

%% --------------------------------------------------------------------
%% Helper Functions
%% --------------------------------------------------------------------

%% Simple GET request helper
http_get(Url) ->
    httpc:request(get, {Url, []}, [], [
        {body_format, binary},
        {full_result, false}
    ]).

%% Helper to create test LiveView component
create_test_live_component() ->
    Code = merl:quote(~""""
    -module(arizona_live_component).
    -behaviour(arizona_live).
    -export([mount/2, render/1]).

    mount(_Req, Socket) ->
        Socket.

    render(Socket) ->
        arizona_html:render_stateless(~"""
        <div>Test LiveView Component</div>
        """, Socket).
    """"),

    merl:compile_and_load(Code).

%% --------------------------------------------------------------------
%% Unit Tests - Test individual functions
%% --------------------------------------------------------------------

test_route_to_cowboy_live(Config) when is_list(Config) ->
    % Test live route conversion
    Route = {live, ~"/test", test_module},
    CowboyRoute = arizona_server:route_to_cowboy(Route),
    Expected = {~"/test", arizona_handler, #{
        type => live,
        handler => test_module
    }},
    ?assertEqual(Expected, CowboyRoute),
    ct:comment("Live route converts to correct Cowboy format").

test_route_to_cowboy_live_websocket(Config) when is_list(Config) ->
    % Test live_websocket route conversion
    Route = {live_websocket, ~"/live/websocket"},
    CowboyRoute = arizona_server:route_to_cowboy(Route),
    Expected = {~"/live/websocket", arizona_websocket, #{
        type => live_websocket
    }},
    ?assertEqual(Expected, CowboyRoute),
    ct:comment("LiveWebSocket route converts to correct Cowboy format").

test_route_to_cowboy_static_dir(Config) when is_list(Config) ->
    % Test static directory route conversion
    Route = {static, ~"/static", {dir, ~"/var/www"}},
    CowboyRoute = arizona_server:route_to_cowboy(Route),
    Expected = {~"/static/[...]", cowboy_static, {dir, ~"/var/www"}},
    ?assertEqual(Expected, CowboyRoute),
    ct:comment("Static directory route converts correctly").

test_route_to_cowboy_static_file(Config) when is_list(Config) ->
    % Test static file route conversion
    Route = {static, ~"/favicon.ico", {file, ~"/var/www/favicon.ico"}},
    CowboyRoute = arizona_server:route_to_cowboy(Route),
    Expected = {~"/favicon.ico", cowboy_static, {file, ~"/var/www/favicon.ico"}},
    ?assertEqual(Expected, CowboyRoute),
    ct:comment("Static file route converts correctly").

test_route_to_cowboy_priv_dir(Config) when is_list(Config) ->
    % Test priv directory route conversion
    Route = {static, ~"/assets", {priv_dir, myapp, ~"static"}},
    CowboyRoute = arizona_server:route_to_cowboy(Route),
    Expected = {~"/assets/[...]", cowboy_static, {priv_dir, myapp, ~"static"}},
    ?assertEqual(Expected, CowboyRoute),
    ct:comment("Priv directory route converts correctly").

test_route_to_cowboy_priv_file(Config) when is_list(Config) ->
    % Test priv file route conversion
    Route = {static, ~"/logo.png", {priv_file, myapp, ~"logo.png"}},
    CowboyRoute = arizona_server:route_to_cowboy(Route),
    Expected = {~"/logo.png", cowboy_static, {priv_file, myapp, ~"logo.png"}},
    ?assertEqual(Expected, CowboyRoute),
    ct:comment("Priv file route converts correctly").

test_route_metadata_functions(Config) when is_list(Config) ->
    % Test route metadata creation and accessors
    LiveMetadata = arizona_server:new_route_metadata(live, test_live_module),
    ?assertEqual(live, arizona_server:get_route_type(LiveMetadata)),
    ?assertEqual(test_live_module, arizona_server:get_route_handler(LiveMetadata)),

    StaticMetadata = arizona_server:new_route_metadata(static, cowboy_static),
    ?assertEqual(static, arizona_server:get_route_type(StaticMetadata)),
    ?assertEqual(cowboy_static, arizona_server:get_route_handler(StaticMetadata)),

    ct:comment("Route metadata functions work correctly").

%% --------------------------------------------------------------------
%% Integration Tests - Test actual server functionality
%% --------------------------------------------------------------------

test_server_lifecycle(Config) when is_list(Config) ->
    % Test basic server start and stop lifecycle
    ServerConfig = #{
        port => 8080,
        routes => [
            {live, ~"/", arizona_live_component}
        ]
    },

    % Test server start
    {ok, Pid} = arizona_server:start(ServerConfig),
    ?assert(is_pid(Pid)),

    % Verify persistent_term was set during start
    Dispatch = persistent_term:get(arizona_dispatch, undefined),
    ?assertNotEqual(undefined, Dispatch),

    % Test server stop
    ok = arizona_server:stop(),

    ct:comment("Server lifecycle (start/stop) works correctly").

test_live_route_integration(Config) when is_list(Config) ->
    % Create test LiveView component and test actual HTTP requests
    {ok, _} = create_test_live_component(),

    ServerConfig = #{
        port => 8081,
        routes => [
            {live, ~"/test", arizona_live_component}
        ]
    },

    {ok, _Pid} = arizona_server:start(ServerConfig),

    % Test successful LiveView request
    {ok, {200, Body}} = http_get("http://localhost:8081/test"),
    ?assert(binary:match(Body, ~"Test LiveView Component") =/= nomatch),

    % Test 404 for non-existent route
    {ok, {404, _}} = http_get("http://localhost:8081/nonexistent"),

    ct:comment("Live route integration works correctly").

test_static_file_integration(Config) when is_list(Config) ->
    % Test static file serving with real files
    TempDir = proplists:get_value(priv_dir, Config),
    TestFile = filename:join(TempDir, "test.txt"),
    TestContent = ~"Hello Arizona Server!",
    ok = file:write_file(TestFile, TestContent),

    ServerConfig = #{
        port => 8082,
        routes => [
            {static, ~"/test.txt", {file, list_to_binary(TestFile)}}
        ]
    },

    {ok, _Pid} = arizona_server:start(ServerConfig),

    % Test successful file serving
    {ok, {200, Body}} = http_get("http://localhost:8082/test.txt"),
    ?assertEqual(TestContent, Body),

    % Test 404 for different path
    {ok, {404, _}} = http_get("http://localhost:8082/other.txt"),

    ct:comment("Static file integration works correctly").

test_static_directory_integration(Config) when is_list(Config) ->
    % Test static directory serving with real directory structure
    TempDir = proplists:get_value(priv_dir, Config),
    StaticDir = filename:join(TempDir, "static"),
    ok = file:make_dir(StaticDir),

    % Create multiple files
    IndexFile = filename:join(StaticDir, "index.html"),
    IndexContent = ~"<html><body>Welcome to Arizona!</body></html>",
    ok = file:write_file(IndexFile, IndexContent),

    CssFile = filename:join(StaticDir, "style.css"),
    CssContent = ~"body { color: blue; }",
    ok = file:write_file(CssFile, CssContent),

    ServerConfig = #{
        port => 8083,
        routes => [
            {static, ~"/static", {dir, list_to_binary(StaticDir)}}
        ]
    },

    {ok, _Pid} = arizona_server:start(ServerConfig),

    % Test index.html
    {ok, {200, IndexBody}} = http_get("http://localhost:8083/static/index.html"),
    ?assert(binary:match(IndexBody, ~"Welcome to Arizona") =/= nomatch),

    % Test style.css
    {ok, {200, CssBody}} = http_get("http://localhost:8083/static/style.css"),
    ?assert(binary:match(CssBody, ~"color: blue") =/= nomatch),

    % Test 404 for non-existent file
    {ok, {404, _}} = http_get("http://localhost:8083/static/nonexistent.js"),

    ct:comment("Static directory integration works correctly").
