-module(arizona_server_SUITE).
-behaviour(ct_suite).
-include_lib("stdlib/include/assert.hrl").
-compile([export_all, nowarn_export_all]).

%% --------------------------------------------------------------------
%% Behaviour (ct_suite) callbacks
%% --------------------------------------------------------------------

all() ->
    [
        {group, basic_server_tests}
    ].

groups() ->
    [
        {basic_server_tests, [sequence], [
            test_basic_server_start_stop,
            test_simple_live_route
        ]}
    ].

init_per_suite(Config) ->
    % Ensure applications are started
    application:ensure_all_started(cowboy),
    % For httpc
    application:ensure_all_started(inets),
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
            {live, "/", test_live_component, #{}}
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
            {live, "/test", test_live_component, #{}}
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

%% Helper function to create test LiveView component at runtime using merl:quote
create_test_live_component() ->
    Code = merl:quote(~""""
    -module(test_live_component).
    -behaviour(arizona_stateful).
    -export([mount/1, render/1]).

    mount(Socket) ->
        Socket.

    render(Socket) ->
        arizona_html:render_stateless(~"""
        <div>Test LiveView Component</div>
        """, Socket).
    """"),

    % Compile and load the module
    merl:compile_and_load(Code).
