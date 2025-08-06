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

-spec set_current_stateful_id(StatefulId) -> UpdatedTracker | undefined when
    StatefulId :: arizona_stateful:id(),
    UpdatedTracker :: arizona_tracker:tracker().
set_current_stateful_id(StatefulId) ->
    case get_tracker() of
        undefined ->
            undefined;
        Tracker ->
            UpdatedTracker = arizona_tracker:set_current_stateful_id(StatefulId, Tracker),
            Tracker = set_tracker(UpdatedTracker),
            UpdatedTracker
    end.

-spec set_current_element_index(ElementIndex) -> UpdatedTracker | undefined when
    ElementIndex :: arizona_tracker:element_index(),
    UpdatedTracker :: arizona_tracker:tracker().
set_current_element_index(ElementIndex) ->
    case get_tracker() of
        undefined ->
            undefined;
        Tracker ->
            UpdatedTracker = arizona_tracker:set_current_element_index(ElementIndex, Tracker),
            Tracker = set_tracker(UpdatedTracker),
            UpdatedTracker
    end.

-spec record_variable_dependency(VarName) -> UpdatedTracker | undefined when
    VarName :: arizona_tracker:var_name(),
    UpdatedTracker :: arizona_tracker:tracker().
record_variable_dependency(VarName) ->
    case get_tracker() of
        undefined ->
            undefined;
        Tracker ->
            UpdatedTracker = arizona_tracker:record_variable_dependency(VarName, Tracker),
            Tracker = set_tracker(UpdatedTracker),
            UpdatedTracker
    end.

-spec clear_stateful_dependencies(StatefulId) -> UpdatedTracker | undefined when
    StatefulId :: arizona_stateful:id(),
    UpdatedTracker :: arizona_tracker:tracker().
clear_stateful_dependencies(StatefulId) ->
    case get_tracker() of
        undefined ->
            undefined;
        Tracker ->
            UpdatedTracker = arizona_tracker:clear_stateful_dependencies(StatefulId, Tracker),
            Tracker = set_tracker(UpdatedTracker),
            UpdatedTracker
    end.

-spec clear_changed_variable_dependencies(StatefulId, VarNames) -> UpdatedTracker | undefined when
    StatefulId :: arizona_stateful:id(),
    VarNames :: [arizona_tracker:var_name()],
    UpdatedTracker :: arizona_tracker:tracker().
clear_changed_variable_dependencies(StatefulId, VarNames) ->
    case get_tracker() of
        undefined ->
            undefined;
        Tracker ->
            UpdatedTracker = arizona_tracker:clear_changed_variable_dependencies(
                StatefulId, VarNames, Tracker
            ),
            Tracker = set_tracker(UpdatedTracker),
            UpdatedTracker
    end.
