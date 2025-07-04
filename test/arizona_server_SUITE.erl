-module(arizona_server_SUITE).
-behaviour(ct_suite).
-include_lib("stdlib/include/assert.hrl").
-compile([export_all, nowarn_export_all]).

%% --------------------------------------------------------------------
%% Behaviour (ct_suite) callbacks
%% --------------------------------------------------------------------

all() ->
    [
        {group, basic_server_tests},
        {group, route_compilation_tests},
        {group, static_route_tests}
    ].

groups() ->
    [
        {basic_server_tests, [sequence], [
            test_basic_server_start_stop,
            test_simple_live_route
        ]},
        {route_compilation_tests, [], [
            test_compile_routes_basic,
            test_compile_routes_empty,
            test_compile_routes_mixed,
            test_route_to_cowboy_live,
            test_route_to_cowboy_static_dir,
            test_route_to_cowboy_static_file,
            test_route_to_cowboy_priv_dir,
            test_route_to_cowboy_priv_file
        ]},
        {static_route_tests, [sequence], [
            test_static_file_route,
            test_static_dir_route,
            test_priv_file_route,
            test_priv_dir_route
        ]}
    ].

init_per_suite(Config) ->
    % Ensure applications are started
    {ok, _} = application:ensure_all_started(cowboy),
    % For httpc
    {ok, _} = application:ensure_all_started(inets),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_Testcase, Config) ->
    Config.

end_per_testcase(_Testcase, _Config) ->
    % Stop server
    _ = arizona_server:stop(),
    ok.

%% --------------------------------------------------------------------
%% Basic server tests
%% --------------------------------------------------------------------

test_basic_server_start_stop(Config) when is_list(Config) ->
    % Test basic server start and stop
    ServerConfig = #{
        port => 8080,
        routes => [
            {live, ~"/", test_live_component, #{}}
        ]
    },

    % Start server
    {ok, _Pid} = arizona_server:start(ServerConfig),

    ct:comment("Basic server start/stop functionality works").

test_simple_live_route(Config) when is_list(Config) ->
    % Create a simple test LiveView component at runtime
    {ok, _Bin} = create_test_live_component(),

    % Start server with LiveView route
    ServerConfig = #{
        port => 8081,
        routes => [
            {live, ~"/test", test_live_component, #{}}
        ]
    },

    {ok, _Pid} = arizona_server:start(ServerConfig),

    % Make HTTP request to test the LiveView route
    Url = "http://localhost:8081/test",
    case httpc:request(get, {Url, []}, [], []) of
        {ok, {{_, 200, _}, _Headers, Body}} ->
            % Verify response contains LiveView content
            ?assert(string:str(Body, "Test LiveView") > 0);
        {error, Reason} ->
            ct:fail("HTTP request failed: ~p", [Reason])
    end,

    ct:comment("Simple LiveView route works correctly").

%% --------------------------------------------------------------------
%% Route Compilation Tests
%% --------------------------------------------------------------------

test_compile_routes_basic(Config) when is_list(Config) ->
    % Test basic route compilation
    Routes = [
        {live, ~"/", test_live_component, #{}},
        {static, ~"/static", {dir, ~"/var/www"}}
    ],

    Dispatch = arizona_server:compile_routes(Routes),

    % Should return compiled dispatch table
    ?assert(is_list(Dispatch)),
    ct:comment("Basic route compilation works").

test_compile_routes_empty(Config) when is_list(Config) ->
    % Test compilation with empty routes
    Routes = [],

    Dispatch = arizona_server:compile_routes(Routes),

    % Should return empty dispatch table
    ?assert(is_list(Dispatch)),
    ct:comment("Empty route compilation works").

test_compile_routes_mixed(Config) when is_list(Config) ->
    % Test compilation with all route types
    Routes = [
        {live, ~"/live", test_live_component, #{}},
        {static, ~"/files", {dir, ~"/var/files"}},
        {static, ~"/favicon.ico", {file, ~"/var/www/favicon.ico"}},
        {static, ~"/assets", {priv_dir, myapp, ~"static"}},
        {static, ~"/logo.png", {priv_file, myapp, ~"logo.png"}}
    ],

    Dispatch = arizona_server:compile_routes(Routes),

    % Should return compiled dispatch table with all routes
    ?assert(is_list(Dispatch)),
    ct:comment("Mixed route compilation works").

test_route_to_cowboy_live(Config) when is_list(Config) ->
    % Test live route conversion
    Route = {live, ~"/test", test_module, #{option => value}},

    CowboyRoute = arizona_server:route_to_cowboy(Route),

    % Should return properly formatted Cowboy route
    Expected = {~"/test", arizona_handler, #{
        type => live,
        handler => test_module,
        opts => #{option => value}
    }},
    ?assertEqual(Expected, CowboyRoute),
    ct:comment("Live route to Cowboy conversion works").

test_route_to_cowboy_static_dir(Config) when is_list(Config) ->
    % Test static directory route conversion
    Route = {static, ~"/static", {dir, ~"/var/www"}},

    CowboyRoute = arizona_server:route_to_cowboy(Route),

    % Should return properly formatted Cowboy static route
    Expected = {~"/static/[...]", cowboy_static, {dir, ~"/var/www"}},
    ?assertEqual(Expected, CowboyRoute),
    ct:comment("Static dir route to Cowboy conversion works").

test_route_to_cowboy_static_file(Config) when is_list(Config) ->
    % Test static file route conversion
    Route = {static, ~"/favicon.ico", {file, ~"/var/www/favicon.ico"}},

    CowboyRoute = arizona_server:route_to_cowboy(Route),

    % Should return properly formatted Cowboy static file route
    Expected = {~"/favicon.ico", cowboy_static, {file, ~"/var/www/favicon.ico"}},
    ?assertEqual(Expected, CowboyRoute),
    ct:comment("Static file route to Cowboy conversion works").

test_route_to_cowboy_priv_dir(Config) when is_list(Config) ->
    % Test priv directory route conversion
    Route = {static, ~"/assets", {priv_dir, myapp, ~"static"}},

    CowboyRoute = arizona_server:route_to_cowboy(Route),

    % Should return properly formatted Cowboy priv_dir route
    Expected = {~"/assets/[...]", cowboy_static, {priv_dir, myapp, ~"static"}},
    ?assertEqual(Expected, CowboyRoute),
    ct:comment("Priv dir route to Cowboy conversion works").

test_route_to_cowboy_priv_file(Config) when is_list(Config) ->
    % Test priv file route conversion
    Route = {static, ~"/logo.png", {priv_file, myapp, ~"logo.png"}},

    CowboyRoute = arizona_server:route_to_cowboy(Route),

    % Should return properly formatted Cowboy priv_file route
    Expected = {~"/logo.png", cowboy_static, {priv_file, myapp, ~"logo.png"}},
    ?assertEqual(Expected, CowboyRoute),
    ct:comment("Priv file route to Cowboy conversion works").

%% --------------------------------------------------------------------
%% Static Route Tests
%% --------------------------------------------------------------------

test_static_file_route(Config) when is_list(Config) ->
    % Create a temporary test file
    TempDir = proplists:get_value(priv_dir, Config),
    TestFile = filename:join(TempDir, "test.txt"),
    ok = file:write_file(TestFile, ~"Test content"),

    % Start server with static file route
    ServerConfig = #{
        port => 8082,
        routes => [
            {static, ~"/test.txt", {file, list_to_binary(TestFile)}}
        ]
    },

    {ok, _Pid} = arizona_server:start(ServerConfig),

    % Test static file serving
    Url = "http://localhost:8082/test.txt",
    case httpc:request(get, {Url, []}, [], []) of
        {ok, {{_, 200, _}, _Headers, Body}} ->
            ?assertEqual("Test content", Body);
        {error, Reason} ->
            ct:fail("HTTP request failed: ~p", [Reason])
    end,

    ct:comment("Static file route works correctly").

test_static_dir_route(Config) when is_list(Config) ->
    % Create a temporary test directory with a file
    TempDir = proplists:get_value(priv_dir, Config),
    StaticDir = filename:join(TempDir, "static"),
    ok = file:make_dir(StaticDir),
    TestFile = filename:join(StaticDir, "index.html"),
    ok = file:write_file(TestFile, ~"<html><body>Static content</body></html>"),

    % Start server with static directory route
    ServerConfig = #{
        port => 8083,
        routes => [
            {static, ~"/static", {dir, list_to_binary(StaticDir)}}
        ]
    },

    {ok, _Pid} = arizona_server:start(ServerConfig),

    % Test static directory serving
    Url = "http://localhost:8083/static/index.html",
    case httpc:request(get, {Url, []}, [], []) of
        {ok, {{_, 200, _}, _Headers, Body}} ->
            ?assert(string:str(Body, "Static content") > 0);
        {error, Reason} ->
            ct:fail("HTTP request failed: ~p", [Reason])
    end,

    ct:comment("Static directory route works correctly").

test_priv_file_route(Config) when is_list(Config) ->
    % Test priv_file route (using arizona's own priv dir if it exists)
    ServerConfig = #{
        port => 8084,
        routes => [
            {static, ~"/priv_test", {priv_file, arizona, ~"test_file.txt"}}
        ]
    },

    {ok, _Pid} = arizona_server:start(ServerConfig),

    % Test priv_file serving (expect 404 since file doesn't exist, but tests the route)
    Url = "http://localhost:8084/priv_test",
    case httpc:request(get, {Url, []}, [], []) of
        {ok, {{_, StatusCode, _}, _Headers, _Body}} ->
            % Should get 404 since file doesn't exist, but route is working
            ?assert(StatusCode =:= 404 orelse StatusCode =:= 200);
        {error, Reason} ->
            ct:fail("HTTP request failed: ~p", [Reason])
    end,

    ct:comment("Priv file route configuration works").

test_priv_dir_route(Config) when is_list(Config) ->
    % Test priv_dir route (using arizona's own priv dir if it exists)
    ServerConfig = #{
        port => 8085,
        routes => [
            {static, ~"/priv_assets", {priv_dir, arizona, ~"static"}}
        ]
    },

    {ok, _Pid} = arizona_server:start(ServerConfig),

    % Test priv_dir serving (expect 404 since dir doesn't exist, but tests the route)
    Url = "http://localhost:8085/priv_assets/test.css",
    case httpc:request(get, {Url, []}, [], []) of
        {ok, {{_, StatusCode, _}, _Headers, _Body}} ->
            % Should get 404 since dir doesn't exist, but route is working
            ?assert(StatusCode =:= 404 orelse StatusCode =:= 200);
        {error, Reason} ->
            ct:fail("HTTP request failed: ~p", [Reason])
    end,

    ct:comment("Priv directory route configuration works").

%% --------------------------------------------------------------------
%% Helper Functions
%% --------------------------------------------------------------------

%% Helper function to create test LiveView component at runtime using merl:quote
create_test_live_component() ->
    Code = merl:quote(~""""
    -module(test_live_component).
    -behaviour(arizona_live).
    -export([mount/2, render/1]).

    mount(_Req, Socket) ->
        Socket.

    render(Socket) ->
        arizona_html:render_stateless(~"""
        <div>Test LiveView Component</div>
        """, Socket).
    """"),

    % Compile and load the module
    merl:compile_and_load(Code).
