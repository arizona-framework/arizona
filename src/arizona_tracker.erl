-module(arizona_tracker).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([new/0]).
-export([set_current_stateful_id/2]).
-export([get_current_element_index/1]).
-export([set_current_element_index/2]).
-export([record_variable_dependency/2]).
-export([get_stateful_dependencies/2]).
-export([get_stateful_dependencies/1]).
-export([clear_component_dependencies/2]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([tracker/0]).
-export_type([stateful_id/0]).
-export_type([element_index/0]).
-export_type([var_name/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-record(tracker, {
    current_stateful_id :: stateful_id() | undefined,
    current_element_index :: element_index() | undefined,
    stateful_dependencies :: #{stateful_id() => #{var_name() => [element_index()]}}
}).

-opaque tracker() :: #tracker{}.
-type stateful_id() :: arizona_stateful:id().
-type element_index() :: non_neg_integer().
-type var_name() :: atom().

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-spec new() -> Tracker when
    Tracker :: tracker().
new() ->
    #tracker{
        current_stateful_id = undefined,
        current_element_index = undefined,
        stateful_dependencies = #{}
    }.

-spec set_current_stateful_id(StatefulId, Tracker) -> Tracker1 when
    StatefulId :: stateful_id(),
    Tracker :: tracker(),
    Tracker1 :: tracker().
set_current_stateful_id(StatefulId, #tracker{} = Tracker) ->
    Tracker#tracker{
        current_stateful_id = StatefulId,
        % Reset element index for new component
        current_element_index = undefined
    }.

-spec get_current_element_index(Tracker) -> ElementIndex when
    Tracker :: tracker(),
    ElementIndex :: element_index() | undefined.
get_current_element_index(#tracker{} = Tracker) ->
    Tracker#tracker.current_element_index.

-spec set_current_element_index(ElementIndex, Tracker) -> Tracker1 when
    ElementIndex :: element_index(),
    Tracker :: tracker(),
    Tracker1 :: tracker().
set_current_element_index(ElementIndex, #tracker{} = Tracker) ->
    Tracker#tracker{current_element_index = ElementIndex}.

-spec record_variable_dependency(VarName, Tracker) -> Tracker1 when
    VarName :: var_name(),
    Tracker :: tracker(),
    Tracker1 :: tracker().
record_variable_dependency(VarName, #tracker{} = Tracker) ->
    CurrentStatefulId = Tracker#tracker.current_stateful_id,
    CurrentElementIndex = Tracker#tracker.current_element_index,
    case {CurrentStatefulId, CurrentElementIndex} of
        {undefined, _} ->
            % No current stateful component
            Tracker;
        {_, undefined} ->
            % No current element
            Tracker;
        {StatefulId, ElementIndex} ->
            StatefulDependencies = Tracker#tracker.stateful_dependencies,
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

-spec get_stateful_dependencies(StatefulId, Tracker) -> ComponentDependencies when
    StatefulId :: stateful_id(),
    Tracker :: tracker(),
    ComponentDependencies :: #{var_name() => [element_index()]}.
get_stateful_dependencies(StatefulId, #tracker{} = Tracker) ->
    maps:get(StatefulId, Tracker#tracker.stateful_dependencies, #{}).

-spec get_stateful_dependencies(Tracker) -> Dependencies when
    Tracker :: tracker(),
    Dependencies :: #{stateful_id() => #{var_name() => [element_index()]}}.
get_stateful_dependencies(#tracker{} = Tracker) ->
    Tracker#tracker.stateful_dependencies.

-spec clear_component_dependencies(StatefulId, Tracker) -> Tracker1 when
    StatefulId :: stateful_id(),
    Tracker :: tracker(),
    Tracker1 :: tracker().
clear_component_dependencies(StatefulId, #tracker{} = Tracker) ->
    StatefulDependencies = Tracker#tracker.stateful_dependencies,
    UpdatedStatefulDependencies = maps:remove(StatefulId, StatefulDependencies),
    Tracker#tracker{stateful_dependencies = UpdatedStatefulDependencies}.
