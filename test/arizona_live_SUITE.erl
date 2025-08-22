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
            handle_view_event_reply_test,
            handle_view_event_noreply_test,
            handle_stateful_event_reply_test,
            handle_stateful_event_noreply_test,
            handle_cast_test,
            handle_info_test,
            concurrent_event_handling_test
        ]}
    ].

init_per_suite(Config) ->
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

    -export([mount/1]).
    -export([render/1]).
    -export([handle_event/3]).

    mount(_Req) ->
        arizona_view:new('@module', #{
            id => ~"live_test_id",
            counter => 0
        }, none).

    render(Bindings) ->
        arizona_template:from_string(~"""
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
        {reply, #{new_count => NewCounter}, UpdatedView};
    handle_event(~"no_reply", _Params, View) ->
        {noreply, View}.
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
        arizona_template:from_string(~"""
        <span>Value: {arizona_template:get_binding(value, Bindings)}</span>
        """).

    handle_event(~"update", #{value := NewValue}, State) ->
        UpdatedState = arizona_stateful:put_binding(value, NewValue, State),
        {reply, #{updated => true}, UpdatedState};
    handle_event(~"update_no_reply", #{value := NewValue}, State) ->
        UpdatedState = arizona_stateful:put_binding(value, NewValue, State),
        {noreply, UpdatedState}.
    """", [{module, merl:term(MockStatefulComponentModule)}]),

    % Create mock view with stateful components
    MockViewWithStatefulCode = merl:qquote(~""""
    -module('@module').
    -compile({parse_transform, arizona_parse_transform}).
    -behaviour(arizona_view).

    -export([mount/1]).
    -export([render/1]).

    mount(_Req) ->
        arizona_view:new('@module', #{
            id => ~"view_with_stateful_id"
        }, none).

    render(_Bindings) ->
        StatefulModule = '@stateful_module',
        arizona_template:from_string(~"""
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

    -export([mount/1]).
    -export([render/1]).
    -export([handle_info/2]).

    mount(_Req) ->
        arizona_view:new('@module', #{
            id => ~"handle_info_test_id",
            message_count => 0
        }, none).

    render(Bindings) ->
        arizona_template:from_string(~"""
        <div>Messages: {arizona_template:get_binding(message_count, Bindings)}</div>
        """).

    handle_info(test_message, View) ->
        ViewState = arizona_view:get_state(View),
        CurrentCount = arizona_stateful:get_binding(message_count, ViewState),
        NewCount = CurrentCount + 1,
        UpdatedViewState = arizona_stateful:put_binding(message_count, NewCount, ViewState),
        UpdatedView = arizona_view:update_state(UpdatedViewState, View),
        {noreply, UpdatedView}.
    """", [{module, merl:term(MockViewWithHandleInfoModule)}]),

    % Compile and load mock modules
    {ok, _ViewBinary} = merl:compile_and_load(MockViewCode),
    {ok, _StatefulComponentBinary} = merl:compile_and_load(MockStatefulComponentCode),
    {ok, _ViewWithStatefulBinary} = merl:compile_and_load(MockViewWithStatefulCode),
    {ok, _ViewWithHandleInfoBinary} = merl:compile_and_load(MockViewWithHandleInfoCode),

    [
        {mock_view_module, MockViewModule},
        {mock_view_with_stateful_module, MockViewWithStatefulModule},
        {mock_stateful_component_module, MockStatefulComponentModule},
        {mock_view_with_handle_info_module, MockViewWithHandleInfoModule}
        | Config
    ].

end_per_suite(Config) ->
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
    MockRequest = arizona_request:new(arizona_cowboy_request, #{}, #{
        method => ~"GET", path => ~"/test"
    }),
    {ok, Pid} = arizona_live:start_link(MockViewModule, MockRequest),
    [{live_pid, Pid} | Config];
init_per_testcase(TestcaseName, Config) when
    TestcaseName =:= handle_stateful_event_reply_test;
    TestcaseName =:= handle_stateful_event_noreply_test
->
    % Start live process with stateful components and initialize
    {mock_view_with_stateful_module, MockViewWithStatefulModule} = proplists:lookup(
        mock_view_with_stateful_module, Config
    ),
    MockRequest = arizona_request:new(arizona_cowboy_request, #{}, #{
        method => ~"GET", path => ~"/test"
    }),
    {ok, Pid} = arizona_live:start_link(MockViewWithStatefulModule, MockRequest),
    % Initialize with render to create stateful components
    _HierarchicalStructure = arizona_live:initial_render(Pid),
    [{live_pid, Pid} | Config];
init_per_testcase(handle_info_test, Config) ->
    % Start live process with handle_info capability
    {mock_view_with_handle_info_module, MockViewWithHandleInfoModule} = proplists:lookup(
        mock_view_with_handle_info_module, Config
    ),
    MockRequest = arizona_request:new(arizona_cowboy_request, #{}, #{
        method => ~"GET", path => ~"/test"
    }),
    {ok, Pid} = arizona_live:start_link(MockViewWithHandleInfoModule, MockRequest),
    [{live_pid, Pid} | Config];
init_per_testcase(_TestcaseName, Config) ->
    % Default: start basic live process and initialize render
    {mock_view_module, MockViewModule} = proplists:lookup(mock_view_module, Config),
    MockRequest = arizona_request:new(arizona_cowboy_request, #{}, #{
        method => ~"GET", path => ~"/test"
    }),
    {ok, Pid} = arizona_live:start_link(MockViewModule, MockRequest),
    % Initialize render to set up tracker
    _HierarchicalStructure = arizona_live:initial_render(Pid),
    [{live_pid, Pid} | Config].

end_per_testcase(_TestcaseName, Config) ->
    case proplists:lookup(live_pid, Config) of
        {live_pid, Pid} when is_pid(Pid) ->
            gen_server:stop(Pid);
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

    ?assertMatch({view, _, _, _, _}, View),
    ViewState = arizona_view:get_state(View),
    ViewId = arizona_stateful:get_binding(id, ViewState),
    ?assertEqual(~"live_test_id", ViewId).

initial_render_test(Config) when is_list(Config) ->
    ct:comment("Test initial_render API call and hierarchical structure"),
    {live_pid, Pid} = proplists:lookup(live_pid, Config),
    HierarchicalStructure = arizona_live:initial_render(Pid),

    ?assertMatch(#{~"live_test_id" := #{static := _, dynamic := _}}, HierarchicalStructure),
    #{~"live_test_id" := #{static := Static, dynamic := Dynamic}} = HierarchicalStructure,
    ?assert(is_list(Static)),
    ?assert(is_list(Dynamic)).

handle_view_event_reply_test(Config) when is_list(Config) ->
    ct:comment("Test handle_event with undefined StatefulId (view events) - reply"),
    {live_pid, Pid} = proplists:lookup(live_pid, Config),

    Result = arizona_live:handle_event(Pid, undefined, ~"increment", #{}),
    ?assertMatch({reply, ~"live_test_id", _Diff, #{new_count := 1}}, Result),
    {reply, ViewId, Diff, Reply} = Result,
    ?assertEqual(~"live_test_id", ViewId),
    ?assertMatch(#{new_count := 1}, Reply),
    ?assert(is_list(Diff)).

handle_view_event_noreply_test(Config) when is_list(Config) ->
    ct:comment("Test handle_event with undefined StatefulId (view events) - noreply"),
    {live_pid, Pid} = proplists:lookup(live_pid, Config),

    Result = arizona_live:handle_event(Pid, undefined, ~"no_reply", #{}),
    ?assertMatch({noreply, ~"live_test_id", _Diff}, Result),
    {noreply, ViewId, Diff} = Result,
    ?assertEqual(~"live_test_id", ViewId),
    ?assert(is_list(Diff)).

handle_stateful_event_reply_test(Config) when is_list(Config) ->
    ct:comment("Test handle_event with specific StatefulId (stateful events) - reply"),
    {live_pid, Pid} = proplists:lookup(live_pid, Config),

    Result = arizona_live:handle_event(Pid, ~"stateful_1", ~"update", #{value => 100}),
    ?assertMatch({reply, ~"stateful_1", _Diff, #{updated := true}}, Result),
    {reply, StatefulId, Diff, Reply} = Result,
    ?assertEqual(~"stateful_1", StatefulId),
    ?assertMatch(#{updated := true}, Reply),
    ?assert(is_list(Diff)).

handle_stateful_event_noreply_test(Config) when is_list(Config) ->
    ct:comment("Test handle_event with specific StatefulId (stateful events) - noreply"),
    {live_pid, Pid} = proplists:lookup(live_pid, Config),

    Result = arizona_live:handle_event(Pid, ~"stateful_2", ~"update_no_reply", #{value => 200}),
    ?assertMatch({noreply, ~"stateful_2", _Diff}, Result),
    {noreply, StatefulId, Diff} = Result,
    ?assertEqual(~"stateful_2", StatefulId),
    ?assert(is_list(Diff)).

handle_cast_test(Config) when is_list(Config) ->
    ct:comment("Test handle_cast message handling"),
    {live_pid, Pid} = proplists:lookup(live_pid, Config),

    % Send cast message - should be ignored but not crash
    ok = gen_server:cast(Pid, test_cast_message),

    % Process should still be alive
    ?assert(is_process_alive(Pid)).

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

concurrent_event_handling_test(Config) when is_list(Config) ->
    ct:comment("Test concurrent event handling"),
    {live_pid, Pid} = proplists:lookup(live_pid, Config),

    % Send multiple concurrent events
    Parent = self(),
    spawn(fun() ->
        Result1 = arizona_live:handle_event(Pid, undefined, ~"increment", #{}),
        Parent ! {result, 1, Result1}
    end),
    spawn(fun() ->
        Result2 = arizona_live:handle_event(Pid, undefined, ~"increment", #{}),
        Parent ! {result, 2, Result2}
    end),

    % Collect results
    Results = [
        receive
            {result, 1, R1} -> R1
        after 5000 -> timeout
        end,
        receive
            {result, 2, R2} -> R2
        after 5000 -> timeout
        end
    ],

    % Both should succeed
    lists:foreach(
        fun(Result) ->
            ?assertMatch({reply, ~"live_test_id", _Diff, #{new_count := _}}, Result)
        end,
        Results
    ).
