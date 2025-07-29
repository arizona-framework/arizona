-module(arizona_tracker_dict).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([new/0]).
-export([get_tracker/0]).
-export([set_current_stateful_id/1]).
-export([set_current_element_index/1]).
-export([record_variable_dependency/1]).
-export([clear_stateful_dependencies/1]).
-export([clear/0]).

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-spec new() -> ok.
new() ->
    _ = put(?MODULE, arizona_tracker:new()),
    ok.

-spec get_tracker() -> Tracker | undefined when
    Tracker :: arizona_tracker:tracker().
get_tracker() ->
    get(?MODULE).

-spec set_current_stateful_id(StatefulId) -> ok when
    StatefulId :: arizona_stateful:id().
set_current_stateful_id(StatefulId) ->
    case get(?MODULE) of
        undefined ->
            ok;
        Tracker ->
            UpdatedTracker = arizona_tracker:set_current_stateful_id(StatefulId, Tracker),
            _ = put(?MODULE, UpdatedTracker),
            ok
    end.

-spec set_current_element_index(ElementIndex) -> ok when
    ElementIndex :: arizona_tracker:element_index().
set_current_element_index(ElementIndex) ->
    case get(?MODULE) of
        undefined ->
            ok;
        Tracker ->
            UpdatedTracker = arizona_tracker:set_current_element_index(ElementIndex, Tracker),
            _ = put(?MODULE, UpdatedTracker),
            ok
    end.

-spec record_variable_dependency(VarName) -> ok when
    VarName :: arizona_tracker:var_name().
record_variable_dependency(VarName) ->
    case get(?MODULE) of
        undefined ->
            ok;
        Tracker ->
            UpdatedTracker = arizona_tracker:record_variable_dependency(VarName, Tracker),
            _ = put(?MODULE, UpdatedTracker),
            ok
    end.

-spec clear_stateful_dependencies(StatefulId) -> ok when
    StatefulId :: arizona_stateful:id().
clear_stateful_dependencies(StatefulId) ->
    case get(?MODULE) of
        undefined ->
            ok;
        Tracker ->
            UpdatedTracker = arizona_tracker:clear_stateful_dependencies(StatefulId, Tracker),
            _ = put(?MODULE, UpdatedTracker),
            ok
    end.

-spec clear() -> ok.
clear() ->
    _ = erase(?MODULE),
    ok.
