-module(arizona_live_SUITE).
-behaviour(ct_suite).
-include_lib("stdlib/include/assert.hrl").
-compile([export_all, nowarn_export_all]).

%% --------------------------------------------------------------------
%% Behaviour (ct_suite) callbacks
%% --------------------------------------------------------------------

all() ->
    [
        {group, live_process_tests}
    ].

groups() ->
    [
        {live_process_tests, [parallel], [
            start_link_success_test,
            get_view_test,
            initial_render_test,
            handle_view_event_with_actions_test,
            handle_view_event_no_actions_test,
            handle_view_event_dispatch_action_test,
            handle_stateful_event_with_actions_test,
            handle_stateful_event_no_actions_test,
            handle_info_test,
            is_connected_test,
            pubsub_message_test,
            concurrent_event_handling_test,
            terminate_callback_test
        ]}
    ].

init_per_suite(Config) ->
    % Start pg groups for testing (ignore if already started)
    PgLivePid =
        case pg:start(arizona_live) of
            {ok, Pid1} -> Pid1;
            {error, {already_started, Pid1}} -> Pid1
        end,
    PgPubSubPid =
        case pg:start(arizona_pubsub) of
            {ok, Pid2} -> Pid2;
            {error, {already_started, Pid2}} -> Pid2
        end,

    % Mock modules for testing
    MockViewModule = arizona_live_mock_view,
    MockViewWithStatefulModule = arizona_live_mock_view_with_stateful,
    MockStatefulComponentModule = arizona_live_mock_stateful_component,
    MockViewWithHandleInfoModule = arizona_live_mock_view_with_handle_info,

    % Create mock view module
    MockViewCode = merl:qquote(~""""
    -module('@module').
    -compile({parse_transform, arizona_parse_transform}).
    -behaviour(arizona_view).

    -export([mount/2]).
    -export([render/1]).
    -export([handle_event/3]).

    mount(_Arg, _Req) ->
        arizona_view:new('@module', #{
            id => ~"live_test_id",
            counter => 0
        }, none).

    render(Bindings) ->
        arizona_template:from_html(~"""
        <div>
            <p>Counter: {arizona_template:get_binding(counter, Bindings)}</p>
            <button>Increment</button>
        </div>
        """).

    handle_event(~"increment", _Params, View) ->
        ViewState = arizona_view:get_state(View),
        CurrentCounter = arizona_stateful:get_binding(counter, ViewState),
        NewCounter = CurrentCounter + 1,
        UpdatedViewState = arizona_stateful:put_binding(counter, NewCounter, ViewState),
        UpdatedView = arizona_view:update_state(UpdatedViewState, View),
        {[{dispatch, ~"counterUpdate", #{new_count => NewCounter}}], UpdatedView};
    handle_event(~"dispatch_test", _Params, View) ->
        {[{dispatch, ~"notification:hideAlert", #{
            alert_id => ~"test-alert",
            dismissed => true
        }}], View};
    handle_event(~"no_action", _Params, View) ->
        {[], View}.
    """", [{module, merl:term(MockViewModule)}]),

    % Create mock stateful component
    MockStatefulComponentCode = merl:qquote(~""""
    -module('@module').
    -compile({parse_transform, arizona_parse_transform}).
    -behaviour(arizona_stateful).

    -export([mount/1]).
    -export([render/1]).
    -export([handle_event/3]).

    mount(Bindings) ->
        arizona_stateful:new('@module', #{
            id => maps:get(id, Bindings),
            value => maps:get(value, Bindings, 0)
        }).

    render(Bindings) ->
        arizona_template:from_html(~"""
        <span>Value: {arizona_template:get_binding(value, Bindings)}</span>
        """).

    handle_event(~"update", #{~"value" := NewValue}, State) ->
        UpdatedState = arizona_stateful:put_binding(value, NewValue, State),
        {[{dispatch, ~"status:componentUpdated", #{
            updated => true,
            value => NewValue
        }}], UpdatedState};
    handle_event(~"update_no_action", #{~"value" := NewValue}, State) ->
        UpdatedState = arizona_stateful:put_binding(value, NewValue, State),
        {[], UpdatedState}.
    """", [{module, merl:term(MockStatefulComponentModule)}]),

    % Create mock view with stateful components
    MockViewWithStatefulCode = merl:qquote(~""""
    -module('@module').
    -compile({parse_transform, arizona_parse_transform}).
    -behaviour(arizona_view).

    -export([mount/2]).
    -export([render/1]).

    mount(_Arg, _Req) ->
        arizona_view:new('@module', #{
            id => ~"view_with_stateful_id"
        }, none).

    render(_Bindings) ->
        StatefulModule = '@stateful_module',
        arizona_template:from_html(~"""
        <div>
            {arizona_template:render_stateful(StatefulModule, #{
                id => ~"stateful_1",
                value => 10
            })}
            {arizona_template:render_stateful(StatefulModule, #{
                id => ~"stateful_2",
                value => 20
            })}
        </div>
        """).
    """", [
        {module, merl:term(MockViewWithStatefulModule)},
        {stateful_module, merl:term(MockStatefulComponentModule)}
    ]),

    % Create mock view with handle_info
    MockViewWithHandleInfoCode = merl:qquote(~""""
    -module('@module').
    -compile({parse_transform, arizona_parse_transform}).
    -behaviour(arizona_view).

    -export([mount/2]).
    -export([render/1]).
    -export([handle_info/2]).

    mount(_Arg, _Req) ->
        arizona_view:new('@module', #{
            id => ~"handle_info_test_id",
            message_count => 0
        }, none).

    render(Bindings) ->
        arizona_template:from_html(~"""
        <div>Messages: {arizona_template:get_binding(message_count, Bindings)}</div>
        """).

    handle_info(test_message, View) ->
        ViewState = arizona_view:get_state(View),
        CurrentCount = arizona_stateful:get_binding(message_count, ViewState),
        NewCount = CurrentCount + 1,
        UpdatedViewState = arizona_stateful:put_binding(message_count, NewCount, ViewState),
        UpdatedView = arizona_view:update_state(UpdatedViewState, View),
        {[], UpdatedView}.
    """", [{module, merl:term(MockViewWithHandleInfoModule)}]),

    % Compile and load mock modules
    {ok, _ViewBinary} = merl:compile_and_load(MockViewCode),
    {ok, _StatefulComponentBinary} = merl:compile_and_load(MockStatefulComponentCode),
    {ok, _ViewWithStatefulBinary} = merl:compile_and_load(MockViewWithStatefulCode),
    {ok, _ViewWithHandleInfoBinary} = merl:compile_and_load(MockViewWithHandleInfoCode),

    [
        {pg_live_pid, PgLivePid},
        {pg_pubsub_pid, PgPubSubPid},
        {mock_view_module, MockViewModule},
        {mock_view_with_stateful_module, MockViewWithStatefulModule},
        {mock_stateful_component_module, MockStatefulComponentModule},
        {mock_view_with_handle_info_module, MockViewWithHandleInfoModule}
        | Config
    ].

end_per_suite(Config) ->
    {pg_live_pid, PgLivePid} = proplists:lookup(pg_live_pid, Config),
    {pg_pubsub_pid, PgPubSubPid} = proplists:lookup(pg_pubsub_pid, Config),
    is_process_alive(PgLivePid) andalso exit(PgLivePid, normal),
    is_process_alive(PgPubSubPid) andalso exit(PgPubSubPid, normal),

    % Clean up mock modules
    {mock_view_module, MockViewModule} = proplists:lookup(mock_view_module, Config),
    {mock_view_with_stateful_module, MockViewWithStatefulModule} = proplists:lookup(
        mock_view_with_stateful_module, Config
    ),
    {mock_stateful_component_module, MockStatefulComponentModule} = proplists:lookup(
        mock_stateful_component_module, Config
    ),
    {mock_view_with_handle_info_module, MockViewWithHandleInfoModule} = proplists:lookup(
        mock_view_with_handle_info_module, Config
    ),

    Modules = [
        MockViewModule,
        MockViewWithStatefulModule,
        MockStatefulComponentModule,
        MockViewWithHandleInfoModule
    ],

    lists:foreach(
        fun(Module) ->
            code:purge(Module),
            code:delete(Module)
        end,
        Modules
    ),

    ok.

init_per_testcase(initial_render_test, Config) ->
    % Start basic live process but don't call initial_render (test will do it)
    {mock_view_module, MockViewModule} = proplists:lookup(mock_view_module, Config),
    {ok, Pid} = arizona_live:start_link(MockViewModule, #{}, mock_request(), self()),
    [{live_pid, Pid} | Config];
init_per_testcase(TestcaseName, Config) when
    TestcaseName =:= handle_stateful_event_with_actions_test;
    TestcaseName =:= handle_stateful_event_no_actions_test
->
    % Start live process with stateful components and initialize
    {mock_view_with_stateful_module, MockViewWithStatefulModule} = proplists:lookup(
        mock_view_with_stateful_module, Config
    ),
    MockRequest = mock_request(),
    {ok, Pid} = arizona_live:start_link(MockViewWithStatefulModule, #{}, MockRequest, self()),
    % Initialize with render to create stateful components
    _HierarchicalStructure = arizona_live:initial_render(Pid),
    [{live_pid, Pid} | Config];
init_per_testcase(handle_info_test, Config) ->
    % Start live process with handle_info capability
    {mock_view_with_handle_info_module, MockViewWithHandleInfoModule} = proplists:lookup(
        mock_view_with_handle_info_module, Config
    ),
    MockRequest = mock_request(),
    {ok, Pid} = arizona_live:start_link(MockViewWithHandleInfoModule, #{}, MockRequest, self()),
    % Initialize render to set up tracker
    _HierarchicalStructure = arizona_live:initial_render(Pid),
    [{live_pid, Pid} | Config];
init_per_testcase(_TestcaseName, Config) ->
    % Default: start basic live process and initialize render
    {mock_view_module, MockViewModule} = proplists:lookup(mock_view_module, Config),
    MockRequest = mock_request(),
    {ok, Pid} = arizona_live:start_link(MockViewModule, #{}, MockRequest, self()),
    % Initialize render to set up tracker
    _HierarchicalStructure = arizona_live:initial_render(Pid),
    [{live_pid, Pid} | Config].

end_per_testcase(_TestcaseName, Config) ->
    case proplists:lookup(live_pid, Config) of
        {live_pid, Pid} when is_pid(Pid) ->
            is_process_alive(Pid) andalso gen_server:stop(Pid);
        _ ->
            ok
    end.

%% --------------------------------------------------------------------
%% Test cases
%% --------------------------------------------------------------------

start_link_success_test(Config) when is_list(Config) ->
    ct:comment("Test successful arizona_live process start"),
    {live_pid, Pid} = proplists:lookup(live_pid, Config),
    ?assert(is_pid(Pid)),
    ?assert(is_process_alive(Pid)).

get_view_test(Config) when is_list(Config) ->
    ct:comment("Test get_view API call"),
    {live_pid, Pid} = proplists:lookup(live_pid, Config),
    View = arizona_live:get_view(Pid),

    ViewState = arizona_view:get_state(View),
    ViewId = arizona_stateful:get_binding(id, ViewState),
    ?assertEqual(~"live_test_id", ViewId).

initial_render_test(Config) when is_list(Config) ->
    ct:comment("Test initial_render API call and hierarchical structure"),
    {mock_view_module, MockViewModule} = proplists:lookup(mock_view_module, Config),
    MockRequest = mock_request(),
    {ok, Pid} = arizona_live:start_link(MockViewModule, #{}, MockRequest, self()),
    {HierarchicalStructure, Diff} = arizona_live:initial_render(Pid),

    ?assertMatch(#{~"live_test_id" := #{static := _, dynamic := _}}, HierarchicalStructure),
    ?assertEqual([], Diff),
    #{~"live_test_id" := #{static := Static, dynamic := Dynamic}} = HierarchicalStructure,
    ?assert(is_list(Static)),
    ?assert(is_list(Dynamic)).

handle_view_event_with_actions_test(Config) when is_list(Config) ->
    ct:comment("Test handle_event with undefined StatefulId (view events) - with actions"),
    {live_pid, Pid} = proplists:lookup(live_pid, Config),

    Result = arizona_live:handle_event(Pid, undefined, ~"increment", #{}),
    ?assertEqual(ok, Result),

    % Verify transport message was sent
    receive
        {actions_response, ~"live_test_id", _Diff, _HierarchicalStructure, [
            {dispatch, ~"counterUpdate", #{new_count := 1}}
        ]} ->
            ok
    after 1000 ->
        ct:fail("Expected actions_response message not received")
    end.

handle_view_event_no_actions_test(Config) when is_list(Config) ->
    ct:comment("Test handle_event with undefined StatefulId (view events) - no actions"),
    {live_pid, Pid} = proplists:lookup(live_pid, Config),

    Result = arizona_live:handle_event(Pid, undefined, ~"no_action", #{}),
    ?assertEqual(ok, Result),

    % Verify transport message was sent
    receive
        {actions_response, ~"live_test_id", _Diff, _HierarchicalStructure, []} -> ok
    after 1000 ->
        ct:fail("Expected actions_response message not received")
    end.

handle_view_event_dispatch_action_test(Config) when is_list(Config) ->
    ct:comment("Test handle_event with undefined StatefulId (view events) - dispatch action"),
    {live_pid, Pid} = proplists:lookup(live_pid, Config),

    Result = arizona_live:handle_event(Pid, undefined, ~"dispatch_test", #{}),
    ?assertEqual(ok, Result),

    % Verify transport message was sent with dispatch action
    receive
        {actions_response, ~"live_test_id", _Diff, _HierarchicalStructure, [
            {dispatch, ~"notification:hideAlert", #{
                alert_id := ~"test-alert", dismissed := true
            }}
        ]} ->
            ok
    after 1000 ->
        ct:fail("Expected actions_response message with dispatch action not received")
    end.

handle_stateful_event_with_actions_test(Config) when is_list(Config) ->
    ct:comment("Test handle_event with specific StatefulId (stateful events) - with actions"),
    {live_pid, Pid} = proplists:lookup(live_pid, Config),

    Result = arizona_live:handle_event(Pid, ~"stateful_1", ~"update", #{~"value" => 100}),
    ?assertEqual(ok, Result),

    % Verify transport message was sent
    receive
        {actions_response, ~"stateful_1", _Diff, _HierarchicalStructure, [
            {dispatch, ~"status:componentUpdated", #{updated := true, value := 100}}
        ]} ->
            ok
    after 1000 ->
        ct:fail("Expected actions_response message not received")
    end.

handle_stateful_event_no_actions_test(Config) when is_list(Config) ->
    ct:comment("Test handle_event with specific StatefulId (stateful events) - no actions"),
    {live_pid, Pid} = proplists:lookup(live_pid, Config),

    Result = arizona_live:handle_event(Pid, ~"stateful_2", ~"update_no_action", #{~"value" => 200}),
    ?assertEqual(ok, Result),

    % Verify transport message was sent
    receive
        {actions_response, ~"stateful_2", _Diff, _HierarchicalStructure, []} -> ok
    after 1000 ->
        ct:fail("Expected actions_response message not received")
    end.

handle_info_test(Config) when is_list(Config) ->
    ct:comment("Test handle_info callback delegation"),
    {live_pid, Pid} = proplists:lookup(live_pid, Config),

    % Send info message that should be handled by view's handle_info
    Pid ! test_message,

    % Give some time for message processing
    timer:sleep(100),

    % Process should still be alive
    ?assert(is_process_alive(Pid)),

    % Verify that the message was processed by checking state change
    View = arizona_live:get_view(Pid),
    ViewState = arizona_view:get_state(View),
    MessageCount = arizona_stateful:get_binding(message_count, ViewState),
    ?assertEqual(1, MessageCount).

is_connected_test(Config) when is_list(Config) ->
    ct:comment("Test is_connected API"),
    {live_pid, Pid} = proplists:lookup(live_pid, Config),

    % Process should be connected
    ?assert(arizona_live:is_connected(Pid)),

    % Non-existent process should not be connected
    ?assertNot(arizona_live:is_connected(spawn(fun() -> ok end))).

pubsub_message_test(Config) when is_list(Config) ->
    ct:comment("Test pubsub message handling"),
    {live_pid, Pid} = proplists:lookup(live_pid, Config),

    % Send pubsub message to live process
    Pid ! {pubsub_message, ~"increment", #{amount => 5}},

    % Expect actions message since increment returns actions
    receive
        {actions_response, ~"live_test_id", _Diff, _HierarchicalStructure, [
            {dispatch, ~"counterUpdate", #{new_count := _}}
        ]} ->
            ok
    after 1000 ->
        ct:fail("Expected actions_response message not received")
    end.

concurrent_event_handling_test(Config) when is_list(Config) ->
    ct:comment("Test concurrent event handling"),
    {live_pid, Pid} = proplists:lookup(live_pid, Config),

    % Send multiple concurrent events
    ok = arizona_live:handle_event(Pid, undefined, ~"increment", #{}),
    ok = arizona_live:handle_event(Pid, undefined, ~"increment", #{}),

    % Expect two actions messages
    receive
        {actions_response, ~"live_test_id", _Diff1, _HierarchicalStructure1, [
            {dispatch, ~"counterUpdate", #{new_count := _}}
        ]} ->
            ok
    after 1000 ->
        ct:fail("Expected first actions_response message not received")
    end,
    receive
        {actions_response, ~"live_test_id", _Diff2, _HierarchicalStructure2, [
            {dispatch, ~"counterUpdate", #{new_count := _}}
        ]} ->
            ok
    after 1000 ->
        ct:fail("Expected second actions_response message not received")
    end.

terminate_callback_test(Config) when is_list(Config) ->
    ct:comment("Test that view terminate callback is invoked on process termination"),
    {live_pid, Pid} = proplists:lookup(live_pid, Config),

    %% Verify the live process is initially running
    ?assert(is_process_alive(Pid)),

    %% Unlink from the process to avoid EXIT messages crashing the test
    unlink(Pid),

    %% Stop the live process with a shutdown reason
    %% This simulates what happens when WebSocket connection closes
    TestReason = {shutdown, {remote, 1001, ~""}},
    ok = gen_server:stop(Pid, TestReason, 1000),

    %% Verify the process terminated cleanly
    %% The terminate callback should have been called without errors
    ?assert(not is_process_alive(Pid)).

%% --------------------------------------------------------------------
%% Helper functions
%% --------------------------------------------------------------------

%% Create a standard mock request for testing
mock_request() ->
    arizona_request:new(arizona_cowboy_request, #{}, #{
        method => ~"GET", path => ~"/test"
    }).
