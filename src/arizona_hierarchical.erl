-module(arizona_hierarchical).
-moduledoc ~"""
Arizona Hierarchical Renderer - JSON-compatible structures for WebSocket clients.

This module implements hierarchical rendering for Arizona templates to enable
efficient real-time updates via WebSocket connections with surgical DOM updates.

Key features:
- JSON-compatible structure format for WebSocket transmission
- Stateful components: Separate hierarchical entries by ID
- Stateless components: Nested diffable structures
- List components: Static template + dynamic data separation
- Granular diffing: Element-level changes within any component type
""".

-export([
    % Structure creation
    create_structure/0,
    add_stateful/3,
    set_element/4,

    % Diff operations
    diff_structures/2,
    apply_diff/2,

    % JSON conversion
    to_json/1,
    from_json/1
]).

%% Types - JSON compatible

% JSON encode/decode handles integer keys
-type element_index() :: non_neg_integer().

-type element_content() ::
    % Static HTML
    binary()
    | #{type := stateful, id := arizona_stateful:id()}
    | #{type := stateless, structure := #{element_index() => element_content()}}
    | #{type := list, static := [binary()], dynamic := [#{element_index() => element_content()}]}.

-type stateful_render() :: #{element_index() => element_content()}.
-type hierarchical_structure() :: #{arizona_stateful:id() => stateful_render()}.

%% Diff types - JSON compatible
-type diff_operation() :: #{
    type := add_stateful | remove_stateful | update_stateful,
    stateful_id := arizona_stateful:id(),
    data := term()
}.

-type element_diff() :: #{
    type := set_element | remove_element | update_stateless | update_list_dynamic,
    element_index := element_index(),
    data := term()
}.

-type hierarchical_diff() :: [diff_operation()].

-export_type([
    element_index/0,
    element_content/0,
    stateful_render/0,
    hierarchical_structure/0,
    diff_operation/0,
    element_diff/0,
    hierarchical_diff/0
]).

%% ============================================================================
%% Structure Creation API
%% ============================================================================

%% @doc Create empty hierarchical structure
-spec create_structure() -> hierarchical_structure().
create_structure() ->
    #{}.

%% @doc Add stateful component to hierarchical structure
-spec add_stateful(arizona_stateful:id(), stateful_render(), hierarchical_structure()) ->
    hierarchical_structure().
add_stateful(StatefulId, StatefulRender, Structure) when
    is_map(StatefulRender), is_map(Structure)
->
    Structure#{StatefulId => StatefulRender}.

%% @doc Set element in stateful component
-spec set_element(
    arizona_stateful:id(), element_index(), element_content(), hierarchical_structure()
) ->
    hierarchical_structure().
set_element(StatefulId, ElementIndex, Content, Structure) when
    is_integer(ElementIndex), is_map(Structure)
->
    StatefulRender = maps:get(StatefulId, Structure, #{}),
    UpdatedRender = StatefulRender#{ElementIndex => Content},
    Structure#{StatefulId => UpdatedRender}.

%% ============================================================================
%% Diff Operations
%% ============================================================================

%% @doc Generate diff between two hierarchical structures
-spec diff_structures(hierarchical_structure(), hierarchical_structure()) ->
    hierarchical_diff().
diff_structures(OldStructure, NewStructure) when is_map(OldStructure), is_map(NewStructure) ->
    OldComponents = maps:keys(OldStructure),
    NewComponents = maps:keys(NewStructure),

    % Find added, removed, and potentially changed components
    AddedComponents = NewComponents -- OldComponents,
    RemovedComponents = OldComponents -- NewComponents,
    CommonComponents = NewComponents -- AddedComponents,

    % Generate diff operations
    AddOps = [
        #{
            type => add_stateful,
            stateful_id => StatefulId,
            data => maps:get(StatefulId, NewStructure)
        }
     || StatefulId <- AddedComponents
    ],

    RemoveOps = [
        #{
            type => remove_stateful,
            stateful_id => StatefulId,
            data => undefined
        }
     || StatefulId <- RemovedComponents
    ],

    UpdateOps = lists:filtermap(
        fun(StatefulId) ->
            OldComponent = maps:get(StatefulId, OldStructure),
            NewComponent = maps:get(StatefulId, NewStructure),
            case diff_components(OldComponent, NewComponent) of
                % No changes
                [] ->
                    false;
                ElementDiffs ->
                    {true, #{
                        type => update_stateful,
                        stateful_id => StatefulId,
                        data => ElementDiffs
                    }}
            end
        end,
        CommonComponents
    ),

    AddOps ++ RemoveOps ++ UpdateOps.

%% @doc Diff two stateful components
-spec diff_components(stateful_render(), stateful_render()) -> [element_diff()].
diff_components(OldComponent, NewComponent) when is_map(OldComponent), is_map(NewComponent) ->
    OldElements = maps:keys(OldComponent),
    NewElements = maps:keys(NewComponent),

    % Find added, removed, and potentially changed elements
    AddedElements = NewElements -- OldElements,
    RemovedElements = OldElements -- NewElements,
    CommonElements = NewElements -- AddedElements,

    % Generate element diffs
    AddDiffs = [
        #{
            type => set_element,
            element_index => ElementIndex,
            data => maps:get(ElementIndex, NewComponent)
        }
     || ElementIndex <- AddedElements
    ],

    RemoveDiffs = [
        #{
            type => remove_element,
            element_index => ElementIndex,
            data => undefined
        }
     || ElementIndex <- RemovedElements
    ],

    UpdateDiffs = lists:filtermap(
        fun(ElementIndex) ->
            OldContent = maps:get(ElementIndex, OldComponent),
            NewContent = maps:get(ElementIndex, NewComponent),
            diff_element_content(ElementIndex, OldContent, NewContent)
        end,
        CommonElements
    ),

    AddDiffs ++ RemoveDiffs ++ UpdateDiffs.

%% @doc Diff element content (handles nested structures)
-spec diff_element_content(element_index(), element_content(), element_content()) ->
    false | {true, element_diff()}.
diff_element_content(ElementIndex, OldContent, NewContent) ->
    case {OldContent, NewContent} of
        {Same, Same} ->
            % No change
            false;
        {#{type := stateless, structure := OldStruct}, #{type := stateless, structure := NewStruct}} ->
            % Diff stateless structure
            case diff_components(OldStruct, NewStruct) of
                [] ->
                    false;
                ElementDiffs ->
                    {true, #{
                        type => update_stateless,
                        element_index => ElementIndex,
                        data => ElementDiffs
                    }}
            end;
        {#{type := list, static := Static, dynamic := OldDynamic}, #{
            type := list, static := Static, dynamic := NewDynamic
        }} ->
            % List with same static template but different dynamic data
            case OldDynamic =:= NewDynamic of
                true ->
                    false;
                false ->
                    {true, #{
                        type => update_list_dynamic,
                        element_index => ElementIndex,
                        data => NewDynamic
                    }}
            end;
        _ ->
            % Different content, replace entirely
            {true, #{
                type => set_element,
                element_index => ElementIndex,
                data => NewContent
            }}
    end.

%% @doc Apply diff to hierarchical structure
-spec apply_diff(hierarchical_diff(), hierarchical_structure()) ->
    hierarchical_structure().
apply_diff(Diff, Structure) when is_list(Diff), is_map(Structure) ->
    lists:foldl(fun apply_diff_operation/2, Structure, Diff).

%% @doc Apply single diff operation
-spec apply_diff_operation(diff_operation(), hierarchical_structure()) ->
    hierarchical_structure().
apply_diff_operation(
    #{type := add_stateful, stateful_id := StatefulId, data := StatefulRender}, Structure
) ->
    Structure#{StatefulId => StatefulRender};
apply_diff_operation(#{type := remove_stateful, stateful_id := StatefulId}, Structure) ->
    maps:remove(StatefulId, Structure);
apply_diff_operation(
    #{type := update_stateful, stateful_id := StatefulId, data := ElementDiffs}, Structure
) ->
    OldComponent = maps:get(StatefulId, Structure, #{}),
    NewComponent = apply_element_diffs(ElementDiffs, OldComponent),
    Structure#{StatefulId => NewComponent}.

%% @doc Apply element diffs to stateful component
-spec apply_element_diffs([element_diff()], stateful_render()) -> stateful_render().
apply_element_diffs(ElementDiffs, Component) when is_list(ElementDiffs), is_map(Component) ->
    lists:foldl(fun apply_element_diff/2, Component, ElementDiffs).

%% @doc Apply single element diff
-spec apply_element_diff(element_diff(), stateful_render()) -> stateful_render().
apply_element_diff(
    #{type := set_element, element_index := ElementIndex, data := Content}, Component
) ->
    Component#{ElementIndex => Content};
apply_element_diff(#{type := remove_element, element_index := ElementIndex}, Component) ->
    maps:remove(ElementIndex, Component);
apply_element_diff(
    #{type := update_stateless, element_index := ElementIndex, data := ElementDiffs}, Component
) ->
    #{type := stateless, structure := OldStruct} = maps:get(ElementIndex, Component),
    NewStruct = apply_element_diffs(ElementDiffs, OldStruct),
    Component#{ElementIndex => #{type => stateless, structure => NewStruct}};
apply_element_diff(
    #{type := update_list_dynamic, element_index := ElementIndex, data := NewDynamic}, Component
) ->
    #{type := list, static := Static} = maps:get(ElementIndex, Component),
    Component#{ElementIndex => #{type => list, static => Static, dynamic => NewDynamic}}.

%% ============================================================================
%% JSON Conversion
%% ============================================================================

%% @doc Convert hierarchical structure to JSON-compatible format
-spec to_json(hierarchical_structure()) -> map().
to_json(Structure) when is_map(Structure) ->
    % Already JSON-compatible
    Structure.

%% @doc Convert JSON format back to hierarchical structure
-spec from_json(map()) -> hierarchical_structure().
from_json(JsonData) when is_map(JsonData) ->
    % Already in correct format
    JsonData.
