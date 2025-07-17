-module(arizona_dependency_tracker_SUITE).
-behaviour(ct_suite).
-include_lib("stdlib/include/assert.hrl").
-compile([export_all, nowarn_export_all]).

%% --------------------------------------------------------------------
%% Behaviour (ct_suite) callbacks
%% --------------------------------------------------------------------

all() ->
    [
        {group, basic_operations},
        {group, dependency_tracking},
        {group, component_management}
    ].

groups() ->
    [
        {basic_operations, [parallel], [
            new_tracker,
            set_current_stateful_id,
            set_current_element_index,
            get_current_context,
            is_tracking_active,
            reset_tracker
        ]},
        {dependency_tracking, [parallel], [
            record_variable_dependency_active,
            record_variable_dependency_inactive,
            record_multiple_variables,
            record_duplicate_dependencies,
            conditional_dependency_tracking
        ]},
        {component_management, [parallel], [
            get_component_vars_indexes,
            get_stateful_dependencies,
            clear_component_dependencies,
            multiple_component_tracking,
            tracked_components_count
        ]}
    ].

%% --------------------------------------------------------------------
%% Basic Operations Tests
%% --------------------------------------------------------------------

new_tracker(Config) when is_list(Config) ->
    Tracker = arizona_dependency_tracker:new(),
    ?assertEqual({undefined, undefined}, arizona_dependency_tracker:get_current_context(Tracker)),
    ?assertNot(arizona_dependency_tracker:is_tracking_active(Tracker)),
    ?assertEqual(#{}, arizona_dependency_tracker:get_stateful_dependencies(Tracker)),
    ?assertEqual(0, arizona_dependency_tracker:tracked_components_count(Tracker)).

set_current_stateful_id(Config) when is_list(Config) ->
    Tracker0 = arizona_dependency_tracker:new(),
    Tracker1 = arizona_dependency_tracker:set_current_stateful_id(user123, Tracker0),
    ?assertEqual({user123, undefined}, arizona_dependency_tracker:get_current_context(Tracker1)),
    ?assertNot(arizona_dependency_tracker:is_tracking_active(Tracker1)).

set_current_element_index(Config) when is_list(Config) ->
    Tracker0 = arizona_dependency_tracker:new(),
    Tracker1 = arizona_dependency_tracker:set_current_stateful_id(user123, Tracker0),
    Tracker2 = arizona_dependency_tracker:set_current_element_index(0, Tracker1),
    ?assertEqual({user123, 0}, arizona_dependency_tracker:get_current_context(Tracker2)),
    ?assert(arizona_dependency_tracker:is_tracking_active(Tracker2)).

get_current_context(Config) when is_list(Config) ->
    Tracker0 = arizona_dependency_tracker:new(),
    ?assertEqual({undefined, undefined}, arizona_dependency_tracker:get_current_context(Tracker0)),

    Tracker1 = arizona_dependency_tracker:set_current_stateful_id(component1, Tracker0),
    ?assertEqual({component1, undefined}, arizona_dependency_tracker:get_current_context(Tracker1)),

    Tracker2 = arizona_dependency_tracker:set_current_element_index(5, Tracker1),
    ?assertEqual({component1, 5}, arizona_dependency_tracker:get_current_context(Tracker2)).

is_tracking_active(Config) when is_list(Config) ->
    Tracker0 = arizona_dependency_tracker:new(),
    ?assertNot(arizona_dependency_tracker:is_tracking_active(Tracker0)),

    Tracker1 = arizona_dependency_tracker:set_current_stateful_id(comp1, Tracker0),
    ?assertNot(arizona_dependency_tracker:is_tracking_active(Tracker1)),

    Tracker2 = arizona_dependency_tracker:set_current_element_index(2, Tracker1),
    ?assert(arizona_dependency_tracker:is_tracking_active(Tracker2)).

reset_tracker(Config) when is_list(Config) ->
    Tracker0 = arizona_dependency_tracker:new(),
    Tracker1 = arizona_dependency_tracker:set_current_stateful_id(user123, Tracker0),
    Tracker2 = arizona_dependency_tracker:set_current_element_index(0, Tracker1),
    Tracker3 = arizona_dependency_tracker:record_variable_dependency(name, Tracker2),

    % Verify tracker has data
    ?assert(arizona_dependency_tracker:is_tracking_active(Tracker3)),
    ?assertEqual(1, arizona_dependency_tracker:tracked_components_count(Tracker3)),

    % Reset and verify clean state
    TrackerReset = arizona_dependency_tracker:reset(Tracker3),
    ?assertEqual(
        {undefined, undefined}, arizona_dependency_tracker:get_current_context(TrackerReset)
    ),
    ?assertNot(arizona_dependency_tracker:is_tracking_active(TrackerReset)),
    ?assertEqual(0, arizona_dependency_tracker:tracked_components_count(TrackerReset)).

%% --------------------------------------------------------------------
%% Dependency Tracking Tests
%% --------------------------------------------------------------------

record_variable_dependency_active(Config) when is_list(Config) ->
    Tracker0 = arizona_dependency_tracker:new(),
    Tracker1 = arizona_dependency_tracker:set_current_stateful_id(user123, Tracker0),
    Tracker2 = arizona_dependency_tracker:set_current_element_index(0, Tracker1),
    Tracker3 = arizona_dependency_tracker:record_variable_dependency(is_auth, Tracker2),

    VarsIndexes = arizona_dependency_tracker:get_component_vars_indexes(user123, Tracker3),
    ?assertEqual(#{is_auth => [0]}, VarsIndexes).

record_variable_dependency_inactive(Config) when is_list(Config) ->
    Tracker0 = arizona_dependency_tracker:new(),

    % No stateful ID set
    Tracker1 = arizona_dependency_tracker:record_variable_dependency(ignored1, Tracker0),
    ?assertEqual(#{}, arizona_dependency_tracker:get_stateful_dependencies(Tracker1)),

    % Only stateful ID set (no element index)
    Tracker2 = arizona_dependency_tracker:set_current_stateful_id(user123, Tracker1),
    Tracker3 = arizona_dependency_tracker:record_variable_dependency(ignored2, Tracker2),
    ?assertEqual(#{}, arizona_dependency_tracker:get_stateful_dependencies(Tracker3)).

record_multiple_variables(Config) when is_list(Config) ->
    Tracker0 = arizona_dependency_tracker:new(),
    Tracker1 = arizona_dependency_tracker:set_current_stateful_id(user123, Tracker0),
    Tracker2 = arizona_dependency_tracker:set_current_element_index(0, Tracker1),
    Tracker3 = arizona_dependency_tracker:record_variable_dependency(is_auth, Tracker2),
    Tracker4 = arizona_dependency_tracker:record_variable_dependency(user_name, Tracker3),

    VarsIndexes = arizona_dependency_tracker:get_component_vars_indexes(user123, Tracker4),
    ?assertEqual(#{is_auth => [0], user_name => [0]}, VarsIndexes).

record_duplicate_dependencies(Config) when is_list(Config) ->
    Tracker0 = arizona_dependency_tracker:new(),
    Tracker1 = arizona_dependency_tracker:set_current_stateful_id(user123, Tracker0),
    Tracker2 = arizona_dependency_tracker:set_current_element_index(0, Tracker1),
    Tracker3 = arizona_dependency_tracker:record_variable_dependency(is_auth, Tracker2),
    Tracker4 = arizona_dependency_tracker:record_variable_dependency(is_auth, Tracker3),

    % Should not duplicate the dependency
    VarsIndexes = arizona_dependency_tracker:get_component_vars_indexes(user123, Tracker4),
    ?assertEqual(#{is_auth => [0]}, VarsIndexes).

conditional_dependency_tracking(Config) when is_list(Config) ->
    % Simulate conditional access pattern from user's example
    Tracker0 = arizona_dependency_tracker:new(),
    Tracker1 = arizona_dependency_tracker:set_current_stateful_id(user123, Tracker0),
    Tracker2 = arizona_dependency_tracker:set_current_element_index(0, Tracker1),

    % is_auth is always accessed
    Tracker3 = arizona_dependency_tracker:record_variable_dependency(is_auth, Tracker2),

    % Conditional: if is_auth = true, access greetings
    Tracker4 = arizona_dependency_tracker:record_variable_dependency(greetings, Tracker3),

    VarsIndexes = arizona_dependency_tracker:get_component_vars_indexes(user123, Tracker4),
    ?assertEqual(#{is_auth => [0], greetings => [0]}, VarsIndexes),

    % Simulate different path: if is_auth = false, access unauth_message
    Tracker5 = arizona_dependency_tracker:set_current_stateful_id(user456, Tracker0),
    Tracker6 = arizona_dependency_tracker:set_current_element_index(0, Tracker5),
    Tracker7 = arizona_dependency_tracker:record_variable_dependency(is_auth, Tracker6),
    Tracker8 = arizona_dependency_tracker:record_variable_dependency(unauth_message, Tracker7),

    VarsIndexes2 = arizona_dependency_tracker:get_component_vars_indexes(user456, Tracker8),
    ?assertEqual(#{is_auth => [0], unauth_message => [0]}, VarsIndexes2).

%% --------------------------------------------------------------------
%% Component Management Tests
%% --------------------------------------------------------------------

get_component_vars_indexes(Config) when is_list(Config) ->
    Tracker0 = arizona_dependency_tracker:new(),

    % Test empty component
    ?assertEqual(#{}, arizona_dependency_tracker:get_component_vars_indexes(nonexistent, Tracker0)),

    % Test component with dependencies
    Tracker1 = arizona_dependency_tracker:set_current_stateful_id(user123, Tracker0),
    Tracker2 = arizona_dependency_tracker:set_current_element_index(0, Tracker1),
    Tracker3 = arizona_dependency_tracker:record_variable_dependency(name, Tracker2),

    ?assertEqual(
        #{name => [0]}, arizona_dependency_tracker:get_component_vars_indexes(user123, Tracker3)
    ),
    ?assertEqual(#{}, arizona_dependency_tracker:get_component_vars_indexes(other, Tracker3)).

get_stateful_dependencies(Config) when is_list(Config) ->
    Tracker0 = arizona_dependency_tracker:new(),

    % Add dependencies for first component
    Tracker1 = arizona_dependency_tracker:set_current_stateful_id(user123, Tracker0),
    Tracker2 = arizona_dependency_tracker:set_current_element_index(0, Tracker1),
    Tracker3 = arizona_dependency_tracker:record_variable_dependency(name, Tracker2),

    % Add dependencies for second component
    Tracker4 = arizona_dependency_tracker:set_current_stateful_id(profile456, Tracker3),
    Tracker5 = arizona_dependency_tracker:set_current_element_index(1, Tracker4),
    Tracker6 = arizona_dependency_tracker:record_variable_dependency(email, Tracker5),

    AllVarsIndexes = arizona_dependency_tracker:get_stateful_dependencies(Tracker6),
    Expected = #{
        user123 => #{name => [0]},
        profile456 => #{email => [1]}
    },
    ?assertEqual(Expected, AllVarsIndexes).

clear_component_dependencies(Config) when is_list(Config) ->
    Tracker0 = arizona_dependency_tracker:new(),
    Tracker1 = arizona_dependency_tracker:set_current_stateful_id(user123, Tracker0),
    Tracker2 = arizona_dependency_tracker:set_current_element_index(0, Tracker1),
    Tracker3 = arizona_dependency_tracker:record_variable_dependency(name, Tracker2),

    % Verify dependency exists
    ?assertEqual(
        #{name => [0]}, arizona_dependency_tracker:get_component_vars_indexes(user123, Tracker3)
    ),
    ?assertEqual(1, arizona_dependency_tracker:tracked_components_count(Tracker3)),

    % Clear dependencies
    Tracker4 = arizona_dependency_tracker:clear_component_dependencies(user123, Tracker3),
    ?assertEqual(#{}, arizona_dependency_tracker:get_component_vars_indexes(user123, Tracker4)),
    ?assertEqual(0, arizona_dependency_tracker:tracked_components_count(Tracker4)).

multiple_component_tracking(Config) when is_list(Config) ->
    Tracker0 = arizona_dependency_tracker:new(),

    % Component 1: user123, element 0, variables [name, email]
    Tracker1 = arizona_dependency_tracker:set_current_stateful_id(user123, Tracker0),
    Tracker2 = arizona_dependency_tracker:set_current_element_index(0, Tracker1),
    Tracker3 = arizona_dependency_tracker:record_variable_dependency(name, Tracker2),
    Tracker4 = arizona_dependency_tracker:record_variable_dependency(email, Tracker3),

    % Component 2: profile456, element 1, variables [bio]
    Tracker5 = arizona_dependency_tracker:set_current_stateful_id(profile456, Tracker4),
    Tracker6 = arizona_dependency_tracker:set_current_element_index(1, Tracker5),
    Tracker7 = arizona_dependency_tracker:record_variable_dependency(bio, Tracker6),

    % Component 1: element 2, variable [status]
    Tracker8 = arizona_dependency_tracker:set_current_stateful_id(user123, Tracker7),
    Tracker9 = arizona_dependency_tracker:set_current_element_index(2, Tracker8),
    Tracker10 = arizona_dependency_tracker:record_variable_dependency(status, Tracker9),

    % Verify all dependencies
    User123Vars = arizona_dependency_tracker:get_component_vars_indexes(user123, Tracker10),
    Profile456Vars = arizona_dependency_tracker:get_component_vars_indexes(profile456, Tracker10),

    ?assertEqual(#{name => [0], email => [0], status => [2]}, User123Vars),
    ?assertEqual(#{bio => [1]}, Profile456Vars),
    ?assertEqual(2, arizona_dependency_tracker:tracked_components_count(Tracker10)).

tracked_components_count(Config) when is_list(Config) ->
    Tracker0 = arizona_dependency_tracker:new(),
    ?assertEqual(0, arizona_dependency_tracker:tracked_components_count(Tracker0)),

    % Add first component
    Tracker1 = arizona_dependency_tracker:set_current_stateful_id(comp1, Tracker0),
    Tracker2 = arizona_dependency_tracker:set_current_element_index(0, Tracker1),
    Tracker3 = arizona_dependency_tracker:record_variable_dependency(var1, Tracker2),
    ?assertEqual(1, arizona_dependency_tracker:tracked_components_count(Tracker3)),

    % Add second component
    Tracker4 = arizona_dependency_tracker:set_current_stateful_id(comp2, Tracker3),
    Tracker5 = arizona_dependency_tracker:set_current_element_index(0, Tracker4),
    Tracker6 = arizona_dependency_tracker:record_variable_dependency(var2, Tracker5),
    ?assertEqual(2, arizona_dependency_tracker:tracked_components_count(Tracker6)),

    % Clear one component
    Tracker7 = arizona_dependency_tracker:clear_component_dependencies(comp1, Tracker6),
    ?assertEqual(1, arizona_dependency_tracker:tracked_components_count(Tracker7)).
