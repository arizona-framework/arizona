-module(arizona_tracker).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([new/0]).
-export([get_current_stateful_id/1]).
-export([set_current_stateful_id/2]).
-export([get_current_element_index/1]).
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
-nominal var_name() :: arizona_binder:key().

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-spec new() -> Tracker when
    Tracker :: tracker().
new() ->
    #tracker{
        current_stateful_id = undefined,
        current_element_index = undefined,
        dependencies = #{}
    }.

-spec get_current_stateful_id(Tracker) -> StatefulId | undefined when
    Tracker :: tracker(),
    StatefulId :: arizona_stateful:id().
get_current_stateful_id(#tracker{} = Tracker) ->
    Tracker#tracker.current_stateful_id.

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

-spec get_current_element_index(Tracker) -> ElementIndex | undefined when
    Tracker :: tracker(),
    ElementIndex :: element_index().
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

-spec get_stateful_dependencies(StatefulId, Tracker) -> StatefulDependencies when
    StatefulId :: arizona_stateful:id(),
    Tracker :: tracker(),
    StatefulDependencies :: stateful_dependencies().
get_stateful_dependencies(StatefulId, #tracker{} = Tracker) ->
    maps:get(StatefulId, Tracker#tracker.dependencies, #{}).

-spec clear_stateful_dependencies(StatefulId, Tracker) -> Tracker1 when
    StatefulId :: arizona_stateful:id(),
    Tracker :: tracker(),
    Tracker1 :: tracker().
clear_stateful_dependencies(StatefulId, #tracker{} = Tracker) ->
    Dependencies = Tracker#tracker.dependencies,
    UpdatedDependencies = maps:remove(StatefulId, Dependencies),
    Tracker#tracker{dependencies = UpdatedDependencies}.

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
