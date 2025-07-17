-module(arizona_dependency_tracker).

-moduledoc ~"""
Runtime dependency tracking for Arizona templates.

## Overview

This module provides runtime tracking of variable dependencies in Arizona templates.
Unlike compile-time tracking, it captures actual variable access patterns including
conditional dependencies that vary based on runtime state.

## Key Features

- **Runtime Precision**: Tracks only variables actually accessed during rendering
- **Conditional Support**: Handles complex conditional logic in templates
- **Opaque State**: Type-safe state management with controlled access
- **Efficient Diffing**: Enables precise element-level diffs

## Usage

The tracker maintains context about the current stateful component and element
being rendered, recording variable dependencies as they occur:

```erlang
Tracker0 = arizona_dependency_tracker:new(),
Tracker1 = arizona_dependency_tracker:set_current_stateful_id(user123, Tracker0),
Tracker2 = arizona_dependency_tracker:set_current_element_index(0, Tracker1),
Tracker3 = arizona_dependency_tracker:record_variable_dependency(is_auth, Tracker2),
VarsIndexes = arizona_dependency_tracker:get_component_vars_indexes(user123, Tracker3).
```

## Integration

This module is designed to be integrated with the arizona_live gen_server for
centralized dependency tracking across the entire LiveView lifecycle.
""".

%% API exports
-export([
    new/0,
    set_current_stateful_id/2,
    set_current_element_index/2,
    record_variable_dependency/2,
    get_component_vars_indexes/2,
    get_stateful_dependencies/1,
    clear_component_dependencies/2,
    reset/1
]).

%% Test exports
-export([
    get_current_context/1,
    is_tracking_active/1,
    tracked_components_count/1
]).

%% Types
-export_type([
    tracker/0,
    stateful_id/0,
    element_index/0,
    var_name/0
]).

-type stateful_id() :: arizona_stateful:id().
-type element_index() :: non_neg_integer().
-type var_name() :: atom().

-record(tracker, {
    current_stateful_id :: stateful_id() | undefined,
    current_element_index :: element_index() | undefined,
    stateful_dependencies :: #{stateful_id() => #{var_name() => [element_index()]}}
}).

-opaque tracker() :: #tracker{}.

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc ~"""
Create a new dependency tracker.

Returns a fresh tracker with no current context and empty dependency tracking.
""".
-spec new() -> tracker().
new() ->
    #tracker{
        current_stateful_id = undefined,
        current_element_index = undefined,
        stateful_dependencies = #{}
    }.

-doc ~"""
Set the current stateful component being rendered.

This establishes the context for subsequent variable dependency tracking.
Resets the current element index to undefined.
""".
-spec set_current_stateful_id(stateful_id(), tracker()) -> tracker().
set_current_stateful_id(StatefulId, Tracker) ->
    Tracker#tracker{
        current_stateful_id = StatefulId,
        % Reset element index for new component
        current_element_index = undefined
    }.

-doc ~"""
Set the current element index being rendered.

This sets the element context for variable dependency tracking.
Requires a current stateful ID to be set.
""".
-spec set_current_element_index(element_index(), tracker()) -> tracker().
set_current_element_index(ElementIndex, Tracker) ->
    Tracker#tracker{current_element_index = ElementIndex}.

-doc ~"""
Record a variable dependency for the current context.

If both current stateful ID and element index are set, records that the
variable is used by that element. Otherwise, the dependency is ignored.

## Examples

```erlang
% With active context
Tracker = arizona_dependency_tracker:set_current_stateful_id(user123, Tracker0),
Tracker1 = arizona_dependency_tracker:set_current_element_index(0, Tracker),
Tracker2 = arizona_dependency_tracker:record_variable_dependency(is_auth, Tracker1),
% Result: #{user123 => #{is_auth => [0]}}

% Without context (no-op)
Tracker3 = arizona_dependency_tracker:record_variable_dependency(ignored, Tracker0),
% Result: #{} (no change)
```
""".
-spec record_variable_dependency(var_name(), tracker()) -> tracker().
record_variable_dependency(VarName, Tracker) ->
    #tracker{
        current_stateful_id = CurrentStatefulId,
        current_element_index = CurrentElementIndex,
        stateful_dependencies = StatefulDependencies
    } = Tracker,

    case {CurrentStatefulId, CurrentElementIndex} of
        {undefined, _} ->
            % No current stateful component
            Tracker;
        {_, undefined} ->
            % No current element
            Tracker;
        {StatefulId, ElementIndex} ->
            ComponentVars = maps:get(StatefulId, StatefulDependencies, #{}),
            ExistingIndexes = maps:get(VarName, ComponentVars, []),
            NewIndexes =
                case lists:member(ElementIndex, ExistingIndexes) of
                    % Already tracked
                    true -> ExistingIndexes;
                    % Add new dependency
                    false -> [ElementIndex | ExistingIndexes]
                end,
            UpdatedComponentVars = ComponentVars#{VarName => NewIndexes},
            UpdatedStatefulDependencies = StatefulDependencies#{StatefulId => UpdatedComponentVars},
            Tracker#tracker{stateful_dependencies = UpdatedStatefulDependencies}
    end.

-doc ~"""
Get variable dependencies for a specific stateful component.

Returns a map of variable names to the list of element indexes that depend on them.
""".
-spec get_component_vars_indexes(stateful_id(), tracker()) ->
    #{var_name() => [element_index()]}.
get_component_vars_indexes(StatefulId, #tracker{stateful_dependencies = StatefulDependencies}) ->
    maps:get(StatefulId, StatefulDependencies, #{}).

-doc ~"""
Get all variable dependencies for all components.

Returns the complete dependency mapping for all tracked stateful components.
""".
-spec get_stateful_dependencies(tracker()) ->
    #{stateful_id() => #{var_name() => [element_index()]}}.
get_stateful_dependencies(#tracker{stateful_dependencies = StatefulDependencies}) ->
    StatefulDependencies.

-doc ~"""
Clear all dependencies for a specific stateful component.

Removes all tracked variable dependencies for the specified component.
""".
-spec clear_component_dependencies(stateful_id(), tracker()) -> tracker().
clear_component_dependencies(StatefulId, Tracker) ->
    #tracker{stateful_dependencies = StatefulDependencies} = Tracker,
    UpdatedStatefulDependencies = maps:remove(StatefulId, StatefulDependencies),
    Tracker#tracker{stateful_dependencies = UpdatedStatefulDependencies}.

-doc ~"""
Reset all tracking state.

Returns a fresh tracker equivalent to calling new/0.
""".
-spec reset(tracker()) -> tracker().
reset(_Tracker) ->
    new().

%% --------------------------------------------------------------------
%% Test Helper Functions
%% --------------------------------------------------------------------

-doc ~"""
Get the current tracking context.

Returns the current stateful ID and element index for testing purposes.
""".
-spec get_current_context(tracker()) ->
    {stateful_id() | undefined, element_index() | undefined}.
get_current_context(#tracker{
    current_stateful_id = StatefulId,
    current_element_index = ElementIndex
}) ->
    {StatefulId, ElementIndex}.

-doc ~"""
Check if tracking is currently active.

Returns true if both stateful ID and element index are set.
""".
-spec is_tracking_active(tracker()) -> boolean().
is_tracking_active(#tracker{
    current_stateful_id = StatefulId,
    current_element_index = ElementIndex
}) ->
    StatefulId =/= undefined andalso ElementIndex =/= undefined.

-doc ~"""
Get the number of components being tracked.

Returns the count of stateful components with recorded dependencies.
""".
-spec tracked_components_count(tracker()) -> non_neg_integer().
tracked_components_count(#tracker{stateful_dependencies = StatefulDependencies}) ->
    maps:size(StatefulDependencies).
