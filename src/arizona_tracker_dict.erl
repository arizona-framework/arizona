-module(arizona_tracker_dict).
-moduledoc ~"""
Process dictionary wrapper for arizona_tracker providing global access.

Provides convenient global access to tracker during template rendering
without explicitly passing tracker through the rendering pipeline.
All functions are thin wrappers around `arizona_tracker` operations.

## Usage Pattern

1. Store tracker in process dictionary with `set_tracker/1`
2. Use wrapper functions during rendering for dependency tracking
3. Retrieve final tracker with `get_tracker/0` for analysis

## Example

```erlang
1> arizona_tracker_dict:set_tracker(arizona_tracker:new()).
undefined
2> arizona_tracker_dict:set_current_stateful_id(~"my_comp").
#tracker{...}
3> arizona_tracker_dict:record_variable_dependency(title).
#tracker{...}
4> Tracker = arizona_tracker_dict:get_tracker().
#tracker{...}
```
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([get_tracker/0]).
-export([set_tracker/1]).
-export([set_current_stateful_id/1]).
-export([set_current_element_index/1]).
-export([record_variable_dependency/1]).
-export([clear_stateful_dependencies/1]).
-export([clear_changed_variable_dependencies/2]).

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc ~"""
Retrieves the tracker stored in the process dictionary.

Returns the current tracker or `undefined` if none is stored.
""".
-spec get_tracker() -> Tracker | undefined when
    Tracker :: arizona_tracker:tracker().
get_tracker() ->
    get(?MODULE).

-doc ~"""
Stores a tracker in the process dictionary.

Replaces any existing tracker and returns the previous one.
""".
-spec set_tracker(Tracker) -> OldTracker | undefined when
    Tracker :: arizona_tracker:tracker(),
    OldTracker :: arizona_tracker:tracker().
set_tracker(Tracker) ->
    put(?MODULE, Tracker).

-doc ~"""
Sets current stateful component ID in stored tracker.

Wrapper around `arizona_tracker:set_current_stateful_id/2` that operates
on the tracker stored in process dictionary.
""".
-spec set_current_stateful_id(StatefulId) -> OldTracker | undefined when
    StatefulId :: arizona_stateful:id(),
    OldTracker :: arizona_tracker:tracker().
set_current_stateful_id(StatefulId) ->
    case get_tracker() of
        undefined ->
            undefined;
        Tracker ->
            UpdatedTracker = arizona_tracker:set_current_stateful_id(StatefulId, Tracker),
            set_tracker(UpdatedTracker)
    end.

-doc ~"""
Sets current element index in stored tracker.

Wrapper around `arizona_tracker:set_current_element_index/2` that operates
on the tracker stored in process dictionary.
""".
-spec set_current_element_index(ElementIndex) -> OldTracker | undefined when
    ElementIndex :: arizona_tracker:element_index(),
    OldTracker :: arizona_tracker:tracker().
set_current_element_index(ElementIndex) ->
    case get_tracker() of
        undefined ->
            undefined;
        Tracker ->
            UpdatedTracker = arizona_tracker:set_current_element_index(ElementIndex, Tracker),
            set_tracker(UpdatedTracker)
    end.

-doc ~"""
Records variable dependency in stored tracker.

Wrapper around `arizona_tracker:record_variable_dependency/2` that operates
on the tracker stored in process dictionary.
""".
-spec record_variable_dependency(VarName) -> OldTracker | undefined when
    VarName :: arizona_tracker:var_name(),
    OldTracker :: arizona_tracker:tracker().
record_variable_dependency(VarName) ->
    case get_tracker() of
        undefined ->
            undefined;
        Tracker ->
            UpdatedTracker = arizona_tracker:record_variable_dependency(VarName, Tracker),
            set_tracker(UpdatedTracker)
    end.

-doc ~"""
Clears all dependencies for a component in stored tracker.

Wrapper around `arizona_tracker:clear_stateful_dependencies/2` that operates
on the tracker stored in process dictionary.
""".
-spec clear_stateful_dependencies(StatefulId) -> OldTracker | undefined when
    StatefulId :: arizona_stateful:id(),
    OldTracker :: arizona_tracker:tracker().
clear_stateful_dependencies(StatefulId) ->
    case get_tracker() of
        undefined ->
            undefined;
        Tracker ->
            UpdatedTracker = arizona_tracker:clear_stateful_dependencies(StatefulId, Tracker),
            set_tracker(UpdatedTracker)
    end.

-doc ~"""
Clears specific variable dependencies in stored tracker.

Wrapper around `arizona_tracker:clear_changed_variable_dependencies/3`
that operates on the tracker stored in process dictionary.
""".
-spec clear_changed_variable_dependencies(StatefulId, VarNames) -> OldTracker | undefined when
    StatefulId :: arizona_stateful:id(),
    VarNames :: [arizona_tracker:var_name()],
    OldTracker :: arizona_tracker:tracker().
clear_changed_variable_dependencies(StatefulId, VarNames) ->
    case get_tracker() of
        undefined ->
            undefined;
        Tracker ->
            UpdatedTracker = arizona_tracker:clear_changed_variable_dependencies(
                StatefulId, VarNames, Tracker
            ),
            set_tracker(UpdatedTracker)
    end.
