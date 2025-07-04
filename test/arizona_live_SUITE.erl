-module(arizona_live_SUITE).
-behaviour(ct_suite).
-include_lib("stdlib/include/assert.hrl").
-compile([export_all, nowarn_export_all]).

%% --------------------------------------------------------------------
%% Behaviour (ct_suite) callbacks
%% --------------------------------------------------------------------

all() ->
    [
        {group, live_component_lifecycle},
        {group, live_component_rendering},
        {group, live_component_events},
        {group, layout_integration}
    ].

groups() ->
    [
        {live_component_lifecycle, [], [
            test_start_live_component,
            test_mount_callback
        ]},
        {live_component_rendering, [], [
            test_render_without_layout,
            test_render_with_layout,
            test_socket_bindings
        ]},
        {live_component_events, [], [
            test_handle_event_increment,
            test_handle_event_decrement,
            test_handle_event_reset
        ]},
        {layout_integration, [], [
            test_layout_injection,
            test_layout_slot_rendering
        ]}
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.

%% --------------------------------------------------------------------
%% Live Component Lifecycle Tests
%% --------------------------------------------------------------------

test_start_live_component(Config) when is_list(Config) ->
    {ok, Pid} = arizona_live_test_helpers:start_live(test_live_component),

    % Verify process was started
    ?assert(is_pid(Pid)),
    ?assert(is_process_alive(Pid)).

test_mount_callback(Config) when is_list(Config) ->
    {ok, Pid} = arizona_live_test_helpers:start_live(test_live_component),

    % Test mount with different request options
    {Socket, Opts} = arizona_live_test_helpers:mount_live(Pid, #{
        bindings => #{test => <<"value">>},
        params => [{<<"session">>, <<"data">>}]
    }),

    % Verify socket and opts are returned
    ?assert(arizona_socket:is_socket(Socket)),
    ?assert(is_map(Opts)).

%% --------------------------------------------------------------------
%% Live Component Rendering Tests
%% --------------------------------------------------------------------

test_render_without_layout(Config) when is_list(Config) ->
    % Create a socket without layout
    Socket = arizona_socket:new(#{mode => render}),

    % Render simple template
    RenderedSocket = arizona_html:render_live(~"""
    <div>Test content</div>
    """, Socket),

    % Check rendered HTML
    arizona_live_test_helpers:assert_html_contains(RenderedSocket, ~"Test content").

test_render_with_layout(Config) when is_list(Config) ->
    {ok, Pid} = arizona_live_test_helpers:start_live(test_live_component),

    % Render with layout
    Socket = arizona_live_test_helpers:render_live(Pid),

    % Check that layout is applied
    arizona_live_test_helpers:assert_html_contains(Socket, ~"<html>"),
    arizona_live_test_helpers:assert_html_contains(Socket, ~"Test Layout"),
    arizona_live_test_helpers:assert_html_contains(Socket, ~"Arizona Framework"),
    arizona_live_test_helpers:assert_html_contains(Socket, ~"Hello, World!").

test_socket_bindings(Config) when is_list(Config) ->
    {ok, Pid} = arizona_live_test_helpers:start_live(test_live_component),

    % Test initial bindings
    Socket = arizona_live_test_helpers:render_live(Pid),
    arizona_live_test_helpers:assert_binding(Socket, name, ~"World"),
    arizona_live_test_helpers:assert_binding(Socket, count, 0).

%% --------------------------------------------------------------------
%% Live Component Events Tests
%% --------------------------------------------------------------------

test_handle_event_increment(Config) when is_list(Config) ->
    {ok, Pid} = arizona_live_test_helpers:start_live(test_live_component),

    % Test increment event
    {noreply, Socket1} = arizona_live_test_helpers:send_event(Pid, ~"increment", #{}),
    arizona_live_test_helpers:assert_binding(Socket1, count, 1),

    % Test another increment
    {noreply, Socket2} = arizona_live_test_helpers:send_event(Pid, ~"increment", #{}),
    arizona_live_test_helpers:assert_binding(Socket2, count, 2).

test_handle_event_decrement(Config) when is_list(Config) ->
    {ok, Pid} = arizona_live_test_helpers:start_live(test_live_component),

    % Increment first to have something to decrement
    {noreply, _Socket1} = arizona_live_test_helpers:send_event(Pid, ~"increment", #{}),
    {noreply, _Socket2} = arizona_live_test_helpers:send_event(Pid, ~"increment", #{}),

    % Test decrement
    {noreply, Socket3} = arizona_live_test_helpers:send_event(Pid, ~"decrement", #{}),
    arizona_live_test_helpers:assert_binding(Socket3, count, 1).

test_handle_event_reset(Config) when is_list(Config) ->
    {ok, Pid} = arizona_live_test_helpers:start_live(test_live_component),

    % Increment first
    {noreply, _Socket1} = arizona_live_test_helpers:send_event(Pid, ~"increment", #{}),
    {noreply, _Socket2} = arizona_live_test_helpers:send_event(Pid, ~"increment", #{}),

    % Test reset
    {noreply, Socket3} = arizona_live_test_helpers:send_event(Pid, ~"reset", #{}),
    arizona_live_test_helpers:assert_binding(Socket3, count, 0).

%% --------------------------------------------------------------------
%% Layout Integration Tests
%% --------------------------------------------------------------------

test_layout_injection(Config) when is_list(Config) ->
    {ok, Pid} = arizona_live_test_helpers:start_live(test_live_component),

    % Render and check layout injection
    Socket = arizona_live_test_helpers:render_live(Pid),

    % Verify live content is inside layout
    arizona_live_test_helpers:assert_html_contains(Socket, ~"<main>"),
    arizona_live_test_helpers:assert_html_contains(Socket, ~"live-component"),
    arizona_live_test_helpers:assert_html_contains(Socket, ~"</main>").

test_layout_slot_rendering(Config) when is_list(Config) ->
    {ok, Pid} = arizona_live_test_helpers:start_live(test_live_component),

    % Render and check slot content
    Socket = arizona_live_test_helpers:render_live(Pid),

    % Verify slot content is rendered correctly
    arizona_live_test_helpers:assert_html_contains(Socket, ~"Hello, World!"),
    arizona_live_test_helpers:assert_html_contains(Socket, ~"You clicked 0 times").
