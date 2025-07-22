-module(arizona_tracker_SUITE).
-behaviour(ct_suite).
-include_lib("stdlib/include/assert.hrl").
-compile([export_all, nowarn_export_all]).

%% --------------------------------------------------------------------
%% Behaviour (ct_suite) callbacks
%% --------------------------------------------------------------------

all() ->
    [
        {group, basic_operations},
        {group, state_management},
        {group, dependency_tracking},
        {group, query_operations}
    ].

groups() ->
    [
        {basic_operations, [parallel], [
            new_empty_tracker
        ]},
        {state_management, [parallel], [
            set_current_stateful_id,
            set_current_element_index
        ]},
        {dependency_tracking, [parallel], [
            record_variable_dependency_success,
            record_variable_dependency_no_stateful_id,
            record_variable_dependency_no_element_index,
            record_variable_dependency_duplicate
        ]},
        {query_operations, [parallel], [
            get_stateful_dependencies_missing,
            get_stateful_dependencies_existing,
            clear_stateful_dependencies
        ]}
    ].

%% --------------------------------------------------------------------
%% Basic operations tests
%% --------------------------------------------------------------------

new_empty_tracker(Config) when is_list(Config) ->
    ct:comment("new/0 should create empty tracker"),
    Tracker = arizona_tracker:new(),
    ?assertEqual(#{}, arizona_tracker:get_stateful_dependencies(~"any_id", Tracker)).

%% --------------------------------------------------------------------
%% State management tests
%% --------------------------------------------------------------------

set_current_stateful_id(Config) when is_list(Config) ->
    ct:comment("set_current_stateful_id/2 should update stateful id"),
    Tracker = arizona_tracker:new(),
    StatefulId = ~"component_123",
    Tracker1 = arizona_tracker:set_current_stateful_id(StatefulId, Tracker),
    % We can't directly get the stateful id, but we can verify the tracker was updated
    ?assertNotEqual(Tracker, Tracker1).

set_current_element_index(Config) when is_list(Config) ->
    ct:comment("set_current_element_index/2 should update element index"),
    Tracker = arizona_tracker:new(),
    Tracker1 = arizona_tracker:set_current_element_index(10, Tracker),
    ?assertNotEqual(Tracker, Tracker1).

%% --------------------------------------------------------------------
%% Dependency tracking tests
%% --------------------------------------------------------------------

record_variable_dependency_success(Config) when is_list(Config) ->
    ct:comment("record_variable_dependency/2 should track variable when state is set"),
    Tracker = arizona_tracker:new(),
    StatefulId = ~"component_1",
    Tracker1 = arizona_tracker:set_current_stateful_id(StatefulId, Tracker),
    Tracker2 = arizona_tracker:set_current_element_index(5, Tracker1),
    Tracker3 = arizona_tracker:record_variable_dependency(name, Tracker2),
    StatefulDeps = arizona_tracker:get_stateful_dependencies(StatefulId, Tracker3),
    ?assertEqual([5], maps:get(name, StatefulDeps)).

record_variable_dependency_no_stateful_id(Config) when is_list(Config) ->
    ct:comment("record_variable_dependency/2 should do nothing without stateful id"),
    Tracker = arizona_tracker:new(),
    Tracker1 = arizona_tracker:set_current_element_index(5, Tracker),
    Tracker2 = arizona_tracker:record_variable_dependency(name, Tracker1),
    ?assertEqual(#{}, arizona_tracker:get_stateful_dependencies(~"any_id", Tracker2)).

record_variable_dependency_no_element_index(Config) when is_list(Config) ->
    ct:comment("record_variable_dependency/2 should do nothing without element index"),
    Tracker = arizona_tracker:new(),
    StatefulId = ~"component_1",
    Tracker1 = arizona_tracker:set_current_stateful_id(StatefulId, Tracker),
    Tracker2 = arizona_tracker:record_variable_dependency(name, Tracker1),
    ?assertEqual(#{}, arizona_tracker:get_stateful_dependencies(StatefulId, Tracker2)).

record_variable_dependency_duplicate(Config) when is_list(Config) ->
    ct:comment("record_variable_dependency/2 should not create duplicates"),
    Tracker = arizona_tracker:new(),
    StatefulId = ~"component_1",
    Tracker1 = arizona_tracker:set_current_stateful_id(StatefulId, Tracker),
    Tracker2 = arizona_tracker:set_current_element_index(3, Tracker1),
    Tracker3 = arizona_tracker:record_variable_dependency(name, Tracker2),
    Tracker4 = arizona_tracker:record_variable_dependency(name, Tracker3),
    StatefulDeps = arizona_tracker:get_stateful_dependencies(StatefulId, Tracker4),
    ?assertEqual([3], maps:get(name, StatefulDeps)).

%% --------------------------------------------------------------------
%% Query operations tests
%% --------------------------------------------------------------------

get_stateful_dependencies_missing(Config) when is_list(Config) ->
    ct:comment("get_stateful_dependencies/2 should return empty map for missing component"),
    Tracker = arizona_tracker:new(),
    ?assertEqual(#{}, arizona_tracker:get_stateful_dependencies(~"missing", Tracker)).

get_stateful_dependencies_existing(Config) when is_list(Config) ->
    ct:comment("get_stateful_dependencies/2 should return component dependencies"),
    Tracker = arizona_tracker:new(),
    StatefulId = ~"component_1",
    Tracker1 = arizona_tracker:set_current_stateful_id(StatefulId, Tracker),
    Tracker2 = arizona_tracker:set_current_element_index(2, Tracker1),
    Tracker3 = arizona_tracker:record_variable_dependency(age, Tracker2),
    StatefulDeps = arizona_tracker:get_stateful_dependencies(StatefulId, Tracker3),
    ?assertEqual([2], maps:get(age, StatefulDeps)).

clear_stateful_dependencies(Config) when is_list(Config) ->
    ct:comment("clear_stateful_dependencies/2 should remove component dependencies"),
    Tracker = arizona_tracker:new(),
    StatefulId = ~"component_1",
    Tracker1 = arizona_tracker:set_current_stateful_id(StatefulId, Tracker),
    Tracker2 = arizona_tracker:set_current_element_index(1, Tracker1),
    Tracker3 = arizona_tracker:record_variable_dependency(name, Tracker2),
    Tracker4 = arizona_tracker:clear_stateful_dependencies(StatefulId, Tracker3),
    ?assertEqual(#{}, arizona_tracker:get_stateful_dependencies(StatefulId, Tracker4)).
