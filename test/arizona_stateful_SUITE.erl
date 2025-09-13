-module(arizona_stateful_SUITE).
-behaviour(ct_suite).
-include_lib("stdlib/include/assert.hrl").
-compile([export_all, nowarn_export_all]).

%% --------------------------------------------------------------------
%% Ignore dialyzer warnings
%% --------------------------------------------------------------------

-dialyzer(
    {nowarn_function, [
        call_handle_event_callback_with_actions/1
    ]}
).

%% --------------------------------------------------------------------
%% Behaviour (ct_suite) callbacks
%% --------------------------------------------------------------------

all() ->
    [
        {group, callback_tests},
        {group, state_management_tests},
        {group, binding_tests}
    ].

groups() ->
    [
        {callback_tests, [parallel], [
            call_mount_callback_test,
            call_render_callback_test,
            call_handle_event_callback_with_actions,
            call_handle_event_callback_no_actions
        ]},
        {state_management_tests, [parallel], [
            new_creates_state,
            get_module_test
        ]},
        {binding_tests, [parallel], [
            get_binding_test,
            get_binding_with_default_test,
            get_bindings_test,
            put_binding_test,
            put_binding_same_value_test,
            merge_bindings_test,
            get_changed_bindings_test,
            set_changed_bindings_test
        ]}
    ].

init_per_suite(Config) ->
    MockModule = arizona_stateful_mock,
    MockEventsModule = arizona_stateful_mock_events,

    MockModuleCode = merl:qquote(~""""
    -module('@module').
    -behaviour(arizona_stateful).

    -export([mount/1]).
    -export([render/1]).

    mount(Bindings) ->
        arizona_stateful:new('@module', Bindings).

    render(_Bindings) ->
        arizona_template:from_string(~"""
        <h1>Mock Template</h1>
        """).
    """", [{module, merl:term(MockModule)}]),

    MockEventsModuleCode = merl:qquote(~""""
    -module('@module').
    -behaviour(arizona_stateful).

    -export([mount/1]).
    -export([render/1]).
    -export([handle_event/3]).

    mount(Bindings) ->
        arizona_stateful:new('@module', Bindings).

    render(_Bindings) ->
        arizona_template:from_string(~"""
        <h1>Mock Template</h1>
        """).

    handle_event(~"with_actions", Params, State) ->
        {[{reply, Params}], State};
    handle_event(~"no_actions", _Params, State) ->
        {[], State}.
    """", [{module, merl:term(MockEventsModule)}]),

    {ok, _Binary} = merl:compile_and_load(MockModuleCode),
    {ok, _EventsBinary} = merl:compile_and_load(MockEventsModuleCode),

    [
        {mock_module, MockModule},
        {mock_events_module, MockEventsModule}
        | Config
    ].

end_per_suite(Config) ->
    {mock_module, MockModule} = proplists:lookup(mock_module, Config),
    {mock_events_module, MockEventsModule} = proplists:lookup(mock_events_module, Config),

    code:purge(MockModule),
    code:purge(MockEventsModule),

    code:delete(MockModule),
    code:delete(MockEventsModule),

    ok.

%% --------------------------------------------------------------------
%% Callback tests
%% --------------------------------------------------------------------

call_mount_callback_test(Config) when is_list(Config) ->
    ct:comment("call_mount_callback/2 should call module mount function"),
    {mock_module, MockModule} = proplists:lookup(mock_module, Config),
    Bindings = #{name => ~"Arizona", version => ~"1.0"},
    State = arizona_stateful:call_mount_callback(MockModule, Bindings),
    ?assertEqual(MockModule, arizona_stateful:get_module(State)),
    ?assertEqual(arizona_binder:new(Bindings), arizona_stateful:get_bindings(State)).

call_render_callback_test(Config) when is_list(Config) ->
    ct:comment("call_render_callback/2 should call module render function"),
    {mock_module, MockModule} = proplists:lookup(mock_module, Config),
    Bindings = #{title => ~"Test Page"},
    State = arizona_stateful:new(MockModule, Bindings),
    Template = arizona_stateful:call_render_callback(State),
    ?assertEqual([~"<h1>Mock Template</h1>"], arizona_template:get_static(Template)).

call_handle_event_callback_with_actions(Config) when is_list(Config) ->
    ct:comment(
        "call_handle_event_callback/4 should return actions tuple when event returns actions"
    ),
    {mock_events_module, MockModule} = proplists:lookup(mock_events_module, Config),
    State = arizona_stateful:new(MockModule, #{}),
    Params = #{data => ~"test"},
    {Actions, NewState} = arizona_stateful:call_handle_event_callback(
        ~"with_actions", Params, State
    ),
    ?assertEqual([{reply, Params}], Actions),
    ?assertEqual(State, NewState).

call_handle_event_callback_no_actions(Config) when is_list(Config) ->
    ct:comment(
        "call_handle_event_callback/4 should return empty actions list "
        "when event returns no actions"
    ),
    {mock_events_module, MockModule} = proplists:lookup(mock_events_module, Config),
    State = arizona_stateful:new(MockModule, #{}),
    {Actions, NewState} = arizona_stateful:call_handle_event_callback(~"no_actions", #{}, State),
    ?assertEqual([], Actions),
    ?assertEqual(State, NewState).

%% --------------------------------------------------------------------
%% State management tests
%% --------------------------------------------------------------------

new_creates_state(Config) when is_list(Config) ->
    ct:comment("new/2 should create state record with module and bindings"),
    Bindings = #{user => ~"john", role => ~"admin"},
    State = arizona_stateful:new(my_module, Bindings),
    ?assertEqual(my_module, arizona_stateful:get_module(State)),
    ?assertEqual(arizona_binder:new(Bindings), arizona_stateful:get_bindings(State)),
    ?assertEqual(arizona_binder:new(#{}), arizona_stateful:get_changed_bindings(State)).

get_module_test(Config) when is_list(Config) ->
    ct:comment("get_module/1 should return module from state"),
    State = arizona_stateful:new(user_live, #{}),
    Module = arizona_stateful:get_module(State),
    ?assertEqual(user_live, Module).

%% --------------------------------------------------------------------
%% Binding tests
%% --------------------------------------------------------------------

get_binding_test(Config) when is_list(Config) ->
    ct:comment("get_binding/2 should retrieve value from state bindings"),
    State = arizona_stateful:new(test_module, #{name => ~"Alice", age => 25}),
    Name = arizona_stateful:get_binding(name, State),
    Age = arizona_stateful:get_binding(age, State),
    ?assertEqual(~"Alice", Name),
    ?assertEqual(25, Age).

get_binding_with_default_test(Config) when is_list(Config) ->
    ct:comment("get_binding/3 should use default when key not found"),
    State = arizona_stateful:new(test_module, #{name => ~"Bob"}),
    Name = arizona_stateful:get_binding(name, State, ~"Default"),
    Role = arizona_stateful:get_binding(role, State, ~"guest"),
    ?assertEqual(~"Bob", Name),
    ?assertEqual(~"guest", Role).

get_bindings_test(Config) when is_list(Config) ->
    ct:comment("get_bindings/1 should return all bindings from state"),
    Bindings = #{title => ~"Page", count => 42},
    State = arizona_stateful:new(test_module, Bindings),
    ResultBindings = arizona_stateful:get_bindings(State),
    ?assertEqual(arizona_binder:new(Bindings), ResultBindings).

put_binding_test(Config) when is_list(Config) ->
    ct:comment("put_binding/3 should add binding and mark as changed"),
    State = arizona_stateful:new(test_module, #{existing => ~"value"}),
    NewState = arizona_stateful:put_binding(new_key, ~"new_value", State),

    ?assertEqual(~"new_value", arizona_stateful:get_binding(new_key, NewState)),
    ?assertEqual(~"value", arizona_stateful:get_binding(existing, NewState)),

    ChangedBindings = arizona_stateful:get_changed_bindings(NewState),
    ?assertEqual(~"new_value", maps:get(new_key, ChangedBindings)).

put_binding_same_value_test(Config) when is_list(Config) ->
    ct:comment("put_binding/3 should not change state when value is same"),
    State = arizona_stateful:new(test_module, #{key => ~"value"}),
    NewState = arizona_stateful:put_binding(key, ~"value", State),

    ?assertEqual(State, NewState),
    ?assertEqual(arizona_binder:new(#{}), arizona_stateful:get_changed_bindings(NewState)).

merge_bindings_test(Config) when is_list(Config) ->
    ct:comment("merge_bindings/2 should merge new bindings into state"),
    State = arizona_stateful:new(test_module, #{a => 1, b => 2}),
    NewBindings = #{b => 20, c => 3},
    MergedState = arizona_stateful:merge_bindings(NewBindings, State),

    ?assertEqual(1, arizona_stateful:get_binding(a, MergedState)),
    ?assertEqual(20, arizona_stateful:get_binding(b, MergedState)),
    ?assertEqual(3, arizona_stateful:get_binding(c, MergedState)),

    ChangedBindings = arizona_stateful:get_changed_bindings(MergedState),
    ?assertEqual(20, maps:get(b, ChangedBindings)),
    ?assertEqual(3, maps:get(c, ChangedBindings)).

get_changed_bindings_test(Config) when is_list(Config) ->
    ct:comment("get_changed_bindings/1 should return changed bindings"),
    State = arizona_stateful:new(test_module, #{}),
    State1 = arizona_stateful:put_binding(key1, ~"value1", State),
    State2 = arizona_stateful:put_binding(key2, ~"value2", State1),

    ChangedBindings = arizona_stateful:get_changed_bindings(State2),
    ?assertEqual(~"value1", maps:get(key1, ChangedBindings)),
    ?assertEqual(~"value2", maps:get(key2, ChangedBindings)).

set_changed_bindings_test(Config) when is_list(Config) ->
    ct:comment("set_changed_bindings/2 should update changed bindings"),
    State = arizona_stateful:new(test_module, #{original => ~"value"}),
    NewChangedBindings = arizona_binder:new(#{custom => ~"changed"}),
    UpdatedState = arizona_stateful:set_changed_bindings(NewChangedBindings, State),

    ResultChangedBindings = arizona_stateful:get_changed_bindings(UpdatedState),
    ?assertEqual(NewChangedBindings, ResultChangedBindings),
    ?assertEqual(~"value", arizona_stateful:get_binding(original, UpdatedState)).
