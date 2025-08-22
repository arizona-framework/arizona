-module(arizona_tracker_dict).

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

-spec get_tracker() -> Tracker | undefined when
    Tracker :: arizona_tracker:tracker().
get_tracker() ->
    get(?MODULE).

-spec set_tracker(Tracker) -> OldTracker | undefined when
    Tracker :: arizona_tracker:tracker(),
    OldTracker :: arizona_tracker:tracker().
set_tracker(Tracker) ->
    put(?MODULE, Tracker).

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
