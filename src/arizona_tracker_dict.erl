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

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-spec get_tracker() -> Tracker | undefined when
    Tracker :: arizona_tracker:tracker().
get_tracker() ->
    get(?MODULE).

-spec set_tracker(Tracker) -> OldValue when
    Tracker :: arizona_tracker:tracker(),
    OldValue :: term() | undefined.
set_tracker(Tracker) ->
    put(?MODULE, Tracker).

-spec set_current_stateful_id(StatefulId) -> ok when
    StatefulId :: arizona_stateful:id().
set_current_stateful_id(StatefulId) ->
    case get_tracker() of
        undefined ->
            ok;
        Tracker ->
            UpdatedTracker = arizona_tracker:set_current_stateful_id(StatefulId, Tracker),
            _OldValue = set_tracker(UpdatedTracker),
            ok
    end.

-spec set_current_element_index(ElementIndex) -> ok when
    ElementIndex :: arizona_tracker:element_index().
set_current_element_index(ElementIndex) ->
    case get_tracker() of
        undefined ->
            ok;
        Tracker ->
            UpdatedTracker = arizona_tracker:set_current_element_index(ElementIndex, Tracker),
            _OldValue = set_tracker(UpdatedTracker),
            ok
    end.

-spec record_variable_dependency(VarName) -> ok when
    VarName :: arizona_tracker:var_name().
record_variable_dependency(VarName) ->
    case get_tracker() of
        undefined ->
            ok;
        Tracker ->
            UpdatedTracker = arizona_tracker:record_variable_dependency(VarName, Tracker),
            _OldValue = set_tracker(UpdatedTracker),
            ok
    end.

-spec clear_stateful_dependencies(StatefulId) -> ok when
    StatefulId :: arizona_stateful:id().
clear_stateful_dependencies(StatefulId) ->
    case get_tracker() of
        undefined ->
            ok;
        Tracker ->
            UpdatedTracker = arizona_tracker:clear_stateful_dependencies(StatefulId, Tracker),
            _OldValue = set_tracker(UpdatedTracker),
            ok
    end.
