-module(arizona_tracker).
-moduledoc ~"""
Variable dependency tracking for differential rendering updates.

Tracks which template variables are used by which dynamic elements in
stateful components, enabling efficient differential updates when only
specific variables change. Essential for WebSocket-based real-time updates.

## Tracking Process

1. Set current stateful component ID during rendering
2. Set current element index for each dynamic element
3. Record variable dependencies as templates access bindings
4. Store mappings of variables to element indices per component
5. Use dependencies to generate minimal diffs when variables change

## Data Structure

- `current_stateful_id` - ID of component currently being rendered
- `current_element_index` - Index of dynamic element being processed
- `dependencies` - Nested map of component → variable → element indices

## Example

```erlang
1> Tracker = arizona_tracker:new().
#tracker{...}
2> T1 = arizona_tracker:set_current_stateful_id(~"my_comp", Tracker).
3> T2 = arizona_tracker:set_current_element_index(2, T1).
4> T3 = arizona_tracker:record_variable_dependency(title, T2).
5> arizona_tracker:get_stateful_dependencies(~"my_comp", T3).
#{title => [2]}
```
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([new/0]).
-export([set_current_stateful_id/2]).
-export([set_current_element_index/2]).
-export([record_variable_dependency/2]).
-export([get_stateful_dependencies/2]).
-export([clear_stateful_dependencies/2]).
-export([clear_changed_variable_dependencies/3]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([tracker/0]).
-export_type([dependencies/0]).
-export_type([stateful_dependencies/0]).
-export_type([element_index/0]).
-export_type([var_name/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-record(tracker, {
    current_stateful_id :: arizona_stateful:id() | undefined,
    current_element_index :: element_index() | undefined,
    dependencies :: dependencies()
}).

-opaque tracker() :: #tracker{}.
-nominal dependencies() :: #{arizona_stateful:id() => stateful_dependencies()}.
-nominal stateful_dependencies() :: #{var_name() => [element_index()]}.
-nominal element_index() :: non_neg_integer().
-nominal var_name() :: dynamic().

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-doc ~"""
Creates a new empty tracker for dependency tracking.

Initializes tracker with no current context and empty dependency maps.
Used to start fresh dependency tracking for a rendering session.
""".
-spec new() -> Tracker when
    Tracker :: tracker().
new() ->
    #tracker{
        current_stateful_id = undefined,
        current_element_index = undefined,
        dependencies = #{}
    }.

-doc ~"""
Sets the current stateful component ID for dependency tracking.

Updates the tracker to record dependencies for the specified component.
Resets element index since we're starting a new component context.
""".
-spec set_current_stateful_id(StatefulId, Tracker) -> Tracker1 when
    StatefulId :: arizona_stateful:id(),
    Tracker :: tracker(),
    Tracker1 :: tracker().
set_current_stateful_id(StatefulId, #tracker{} = Tracker) ->
    Tracker#tracker{
        current_stateful_id = StatefulId,
        % Reset element index for new component
        current_element_index = undefined
    }.

-doc ~"""
Sets the current dynamic element index for dependency tracking.

Updates the tracker with the index of the dynamic element currently
being processed, used to associate variable dependencies with specific elements.
""".
-spec set_current_element_index(ElementIndex, Tracker) -> Tracker1 when
    ElementIndex :: element_index(),
    Tracker :: tracker(),
    Tracker1 :: tracker().
set_current_element_index(ElementIndex, #tracker{} = Tracker) ->
    Tracker#tracker{current_element_index = ElementIndex}.

-doc ~"""
Records a variable dependency for the current element.

Tracks that the current dynamic element depends on the specified variable.
Only records dependencies when both stateful ID and element index are set.
""".
-spec record_variable_dependency(VarName, Tracker) -> Tracker1 when
    VarName :: var_name(),
    Tracker :: tracker(),
    Tracker1 :: tracker().
record_variable_dependency(VarName, #tracker{} = Tracker) ->
    CurrentStatefulId = Tracker#tracker.current_stateful_id,
    CurrentElementIndex = Tracker#tracker.current_element_index,
    case {CurrentStatefulId, CurrentElementIndex} of
        {StatefulId, ElementIndex} when is_binary(StatefulId), is_integer(ElementIndex) ->
            Dependencies = Tracker#tracker.dependencies,
            StatefulDependencies = maps:get(StatefulId, Dependencies, #{}),
            ExistingIndexes = maps:get(VarName, StatefulDependencies, []),
            NewIndexes =
                case lists:member(ElementIndex, ExistingIndexes) of
                    % Already tracked
                    true -> ExistingIndexes;
                    % Add new dependency
                    false -> [ElementIndex | ExistingIndexes]
                end,
            UpdatedStatefulDependencies = StatefulDependencies#{VarName => NewIndexes},
            UpdatedDependencies = Dependencies#{StatefulId => UpdatedStatefulDependencies},
            Tracker#tracker{dependencies = UpdatedDependencies};
        {StatefulId, undefined} when is_binary(StatefulId) ->
            Tracker
    end.

-doc ~"""
Retrieves all variable dependencies for a stateful component.

Returns a map of variable names to lists of element indices that
depend on those variables. Empty map if component has no dependencies.
""".
-spec get_stateful_dependencies(StatefulId, Tracker) -> StatefulDependencies when
    StatefulId :: arizona_stateful:id(),
    Tracker :: tracker(),
    StatefulDependencies :: stateful_dependencies().
get_stateful_dependencies(StatefulId, #tracker{} = Tracker) ->
    maps:get(StatefulId, Tracker#tracker.dependencies, #{}).

-doc ~"""
Clears all variable dependencies for a stateful component.

Removes all dependency tracking for the specified component.
Used when re-rendering a component to start fresh tracking.
""".
-spec clear_stateful_dependencies(StatefulId, Tracker) -> Tracker1 when
    StatefulId :: arizona_stateful:id(),
    Tracker :: tracker(),
    Tracker1 :: tracker().
clear_stateful_dependencies(StatefulId, #tracker{} = Tracker) ->
    Dependencies = Tracker#tracker.dependencies,
    UpdatedDependencies = maps:remove(StatefulId, Dependencies),
    Tracker#tracker{dependencies = UpdatedDependencies}.

-doc ~"""
Clears dependencies for specific changed variables.

Removes dependency tracking for only the specified variables,
keeping other variable dependencies intact. Cleans up empty component entries.
""".
-spec clear_changed_variable_dependencies(StatefulId, VarNames, Tracker) -> Tracker1 when
    StatefulId :: arizona_stateful:id(),
    VarNames :: [var_name()],
    Tracker :: tracker(),
    Tracker1 :: tracker().
clear_changed_variable_dependencies(StatefulId, VarNames, #tracker{} = Tracker) ->
    Dependencies = Tracker#tracker.dependencies,
    StatefulDeps = maps:get(StatefulId, Dependencies, #{}),

    % Remove only changed variable dependencies
    UpdatedStatefulDeps = lists:foldl(
        fun(VarName, AccDeps) ->
            maps:remove(VarName, AccDeps)
        end,
        StatefulDeps,
        VarNames
    ),

    % Clean up empty component entries
    UpdatedDependencies =
        case maps:size(UpdatedStatefulDeps) of
            0 -> maps:remove(StatefulId, Dependencies);
            _ -> Dependencies#{StatefulId => UpdatedStatefulDeps}
        end,

    Tracker#tracker{dependencies = UpdatedDependencies}.
