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
        {group, layout_integration},
        {group, gen_server_callbacks},
        {group, callback_wrappers},
        {group, error_conditions}
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
            test_handle_event_reset,
            test_handle_event_reply_branch
        ]},
        {layout_integration, [], [
            test_layout_injection,
            test_layout_slot_rendering
        ]},
        {gen_server_callbacks, [], [
            test_handle_info_callback,
            test_handle_cast_callback
        ]},
        {callback_wrappers, [], [
            test_call_handle_info_callback_with_function,
            test_call_handle_info_callback_without_function,
            test_call_handle_event_callback_with_function,
            test_call_handle_event_callback_without_function
        ]},
        {error_conditions, [], [
            test_unknown_handle_call,
            test_handle_event_unknown_event
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
    {ok, Pid} = arizona_live_test_helpers:start_live(arizona_live_component),

    % Verify process was started
    ?assert(is_pid(Pid)),
    ?assert(is_process_alive(Pid)).

test_mount_callback(Config) when is_list(Config) ->
    {ok, Pid} = arizona_live_test_helpers:start_live(arizona_live_component),

    % Test mount with different request options
    Socket = arizona_live_test_helpers:mount_live(Pid, #{
        bindings => #{test => <<"value">>},
        params => [{<<"session">>, <<"data">>}]
    }),

    % Verify socket is returned
    ?assert(arizona_socket:is_socket(Socket)).

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
    {ok, Pid} = arizona_live_test_helpers:start_live(arizona_live_component),

    % Render with layout
    Socket = arizona_live_test_helpers:render_live(Pid),

    % Check that layout is applied
    arizona_live_test_helpers:assert_html_contains(Socket, ~"<html>"),
    arizona_live_test_helpers:assert_html_contains(Socket, ~"Test Layout"),
    arizona_live_test_helpers:assert_html_contains(Socket, ~"Arizona Framework"),
    arizona_live_test_helpers:assert_html_contains(Socket, ~"Hello, World!").

test_socket_bindings(Config) when is_list(Config) ->
    {ok, Pid} = arizona_live_test_helpers:start_live(arizona_live_component),

    % Test initial bindings
    Socket = arizona_live_test_helpers:render_live(Pid),
    arizona_live_test_helpers:assert_binding(Socket, name, ~"World"),
    arizona_live_test_helpers:assert_binding(Socket, count, 0).

%% --------------------------------------------------------------------
%% Live Component Events Tests
%% --------------------------------------------------------------------

test_handle_event_increment(Config) when is_list(Config) ->
    {ok, Pid} = arizona_live_test_helpers:start_live(arizona_live_component),

    % Test increment event
    {noreply, Socket1} = arizona_live_test_helpers:send_event(Pid, ~"increment", #{}),
    arizona_live_test_helpers:assert_binding(Socket1, count, 1),

    % Test another increment
    {noreply, Socket2} = arizona_live_test_helpers:send_event(Pid, ~"increment", #{}),
    arizona_live_test_helpers:assert_binding(Socket2, count, 2).

test_handle_event_decrement(Config) when is_list(Config) ->
    {ok, Pid} = arizona_live_test_helpers:start_live(arizona_live_component),

    % Increment first to have something to decrement
    {noreply, _Socket1} = arizona_live_test_helpers:send_event(Pid, ~"increment", #{}),
    {noreply, _Socket2} = arizona_live_test_helpers:send_event(Pid, ~"increment", #{}),

    % Test decrement
    {noreply, Socket3} = arizona_live_test_helpers:send_event(Pid, ~"decrement", #{}),
    arizona_live_test_helpers:assert_binding(Socket3, count, 1).

test_handle_event_reset(Config) when is_list(Config) ->
    {ok, Pid} = arizona_live_test_helpers:start_live(arizona_live_component),

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
    {ok, Pid} = arizona_live_test_helpers:start_live(arizona_live_component),

    % Render and check layout injection
    Socket = arizona_live_test_helpers:render_live(Pid),

    % Verify live content is inside layout
    arizona_live_test_helpers:assert_html_contains(Socket, ~"<main>"),
    arizona_live_test_helpers:assert_html_contains(Socket, ~"live-component"),
    arizona_live_test_helpers:assert_html_contains(Socket, ~"</main>").

test_layout_slot_rendering(Config) when is_list(Config) ->
    {ok, Pid} = arizona_live_test_helpers:start_live(arizona_live_component),

    % Render and check slot content
    Socket = arizona_live_test_helpers:render_live(Pid),

    % Verify slot content is rendered correctly
    arizona_live_test_helpers:assert_html_contains(Socket, ~"Hello, World!"),
    arizona_live_test_helpers:assert_html_contains(Socket, ~"You clicked 0 times").

%% --------------------------------------------------------------------
%% Live Component Events Tests - Reply Branch
%% --------------------------------------------------------------------

test_handle_event_reply_branch(Config) when is_list(Config) ->
    {ok, Pid} = arizona_live_test_helpers:start_live(arizona_live_component_with_info),

    % Test handle_event that returns {reply, Reply, Socket}
    {reply, Reply, Socket} = arizona_live_test_helpers:send_event(Pid, ~"reply_test", #{}),

    % Verify reply and socket
    ?assertEqual(~"test_reply", Reply),
    ?assert(arizona_socket:is_socket(Socket)).

%% --------------------------------------------------------------------
%% Gen Server Callbacks Tests
%% --------------------------------------------------------------------

test_handle_info_callback(Config) when is_list(Config) ->
    {ok, Pid} = arizona_live_test_helpers:start_live(arizona_live_component_with_info),

    % Send info message to the gen_server process
    Pid ! {add_message, ~"Test message"},

    % Give some time for the message to be processed
    timer:sleep(10),

    % Render to get updated socket and verify message was handled
    Socket = arizona_live_test_helpers:render_live(Pid),
    arizona_live_test_helpers:assert_binding_contains(Socket, messages, ~"Test message").

test_handle_cast_callback(Config) when is_list(Config) ->
    {ok, Pid} = arizona_live_test_helpers:start_live(arizona_live_component),

    % Send a cast message (should be ignored but not crash)
    gen_server:cast(Pid, test_cast_message),

    % Verify process is still alive
    ?assert(is_process_alive(Pid)).

%% --------------------------------------------------------------------
%% Callback Wrappers Tests
%% --------------------------------------------------------------------

test_call_handle_info_callback_with_function(Config) when is_list(Config) ->
    Socket = arizona_socket:new(#{mode => render}),

    % Test with module that exports handle_info/2
    Result = arizona_live:call_handle_info_callback(
        arizona_live_component_with_info, test_info, Socket
    ),

    % Should return {noreply, Socket}
    ?assertMatch({noreply, Socket}, Result),
    ?assert(arizona_socket:is_socket(Socket)).

test_call_handle_info_callback_without_function(Config) when is_list(Config) ->
    Socket = arizona_socket:new(#{mode => render}),

    % Test with module that does NOT export handle_info/2
    Result = arizona_live:call_handle_info_callback(arizona_live_component, test_info, Socket),

    % Should return {noreply, Socket} as default
    ?assertMatch({noreply, Socket}, Result),
    ?assert(arizona_socket:is_socket(Socket)).

test_call_handle_event_callback_with_function(Config) when is_list(Config) ->
    Socket = arizona_socket:new(#{mode => render}),

    % Test with module that exports handle_event/3
    Result = arizona_live:call_handle_event_callback(
        arizona_live_component, ~"test_event", #{}, Socket
    ),

    % Should return {noreply, Socket}
    ?assertMatch({noreply, _}, Result).

test_call_handle_event_callback_without_function(Config) when is_list(Config) ->
    Socket = arizona_socket:new(#{mode => render}),

    % Test with a module that does NOT export handle_event/3
    % Create a simple test module without handle_event
    Result = arizona_live:call_handle_event_callback(arizona_socket, ~"test_event", #{}, Socket),

    % Should return {noreply, Socket} as default
    ?assertMatch({noreply, _}, Result).

%% --------------------------------------------------------------------
%% Error Conditions Tests
%% --------------------------------------------------------------------

test_unknown_handle_call(Config) when is_list(Config) ->
    {ok, Pid} = arizona_live_test_helpers:start_live(arizona_live_component),

    % This should test the handle_call pattern matching, but since all patterns
    % are covered in arizona_live.erl, we test the mount/render/handle_event functions
    % to ensure handle_call coverage

    % Test mount
    Socket1 = arizona_live:mount(Pid, arizona_request:new(#{})),
    ?assert(arizona_socket:is_socket(Socket1)),

    % Test render
    Socket2 = arizona_live:render(Pid),
    ?assert(arizona_socket:is_socket(Socket2)),

    % Test handle_event
    {noreply, Socket3} = arizona_live:handle_event(Pid, ~"increment", #{}),
    ?assert(arizona_socket:is_socket(Socket3)).

test_handle_event_unknown_event(Config) when is_list(Config) ->
    {ok, Pid} = arizona_live_test_helpers:start_live(arizona_live_component),

    % Test unknown event (should be handled by catch-all clause)
    {noreply, Socket} = arizona_live_test_helpers:send_event(Pid, ~"unknown_event", #{}),

    % Should return socket unchanged
    ?assert(arizona_socket:is_socket(Socket)).
