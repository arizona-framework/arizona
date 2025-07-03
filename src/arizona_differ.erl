-module(arizona_differ).
-moduledoc ~"""
Arizona Differ - Optimized JSON diff generation for WebSocket transmission.

This module implements efficient diffing for Arizona templates:
- Stateful components: Use changed_bindings + vars_indexes for optimized diffing
- Stateless components: Full re-render when variables change (no diffing)

Generates JSON diffs in format: [{StatefulId, [{ElementIndex, Changes}]}]
""".

-export([diff_stateful/3]).
-export([get_affected_elements/2, to_json/1]).

%% Diff format types
-type element_index() :: non_neg_integer().
-type element_value() :: term().
-type nested_changes() :: [component_change()].
-type element_change() :: element_value() | nested_changes().
-type component_change() :: {arizona_stateful:id(), [element_change_entry()]}.
-type element_change_entry() :: {element_index(), element_change()}.
-type diff_changes() :: [component_change()].

-export_type([diff_changes/0, element_index/0, element_change/0, element_change_entry/0]).

%% Optimized stateful component diffing using changed_bindings + vars_indexes
-spec diff_stateful(TemplateData, Stateful, Socket) -> Socket1 when
    TemplateData :: arizona_renderer:stateful_template_data(),
    Stateful :: arizona_stateful:stateful(),
    Socket :: arizona_socket:socket(),
    Socket1 :: arizona_socket:socket().
diff_stateful(TemplateData, Stateful, Socket) ->
    % Get changed bindings (already filtered by put_binding/3)
    ChangedBindings = arizona_stateful:get_changed_bindings(Stateful),
    case maps:size(ChangedBindings) of
        0 ->
            % No changes, return socket unchanged
            Socket;
        _ ->
            StatefulId = arizona_stateful:get_id(Stateful),

            % Find affected elements using the optimization
            AffectedElements = get_affected_elements(
                ChangedBindings,
                maps:get(vars_indexes, TemplateData, #{})
            ),

            case sets:size(AffectedElements) of
                0 ->
                    % No affected elements
                    Socket;
                _ ->
                    % Create element changes for affected elements
                    {ElementChanges, UpdatedSocket} = create_element_changes(
                        AffectedElements, TemplateData, Socket
                    ),
                    ComponentChange = {StatefulId, ElementChanges},

                    % Append changes with proper path tracking
                    arizona_socket:append_changes([ComponentChange], UpdatedSocket)
            end
    end.

%% Get affected element indexes from changed bindings and vars_indexes
-spec get_affected_elements(ChangedBindings, VarsIndexes) -> AffectedIndexes when
    ChangedBindings :: map(),
    VarsIndexes :: #{atom() => [element_index()]},
    AffectedIndexes :: sets:set(element_index()).
get_affected_elements(ChangedBindings, VarsIndexes) ->
    ChangedVarNames = maps:keys(ChangedBindings),
    AffectedIndexLists = [
        maps:get(VarName, VarsIndexes, [])
     || VarName <- ChangedVarNames
    ],
    sets:from_list(lists:flatten(AffectedIndexLists)).

%% Convert diff changes to JSON-serializable format
-spec to_json(DiffChanges) -> JsonData when
    DiffChanges :: diff_changes(),
    JsonData :: term().
to_json(DiffChanges) ->
    % Already in the correct format: [{StatefulId, [{ElementIndex, Changes}]}]
    DiffChanges.

%% Internal implementation functions

%% Create element changes for affected elements
-spec create_element_changes(
    sets:set(element_index()),
    arizona_renderer:stateful_template_data(),
    arizona_socket:socket()
) -> {[element_change_entry()], arizona_socket:socket()}.
create_element_changes(AffectedElements, TemplateData, Socket) ->
    ElementsList = sets:to_list(AffectedElements),
    Elements = maps:get(elems, TemplateData),
    {ElementChanges, FinalSocket} = lists:foldl(
        fun(ElementIndex, {ChangesAcc, AccSocket}) ->
            Element = maps:get(ElementIndex, Elements),
            {RenderedValue, UpdatedSocket} = arizona_renderer:render_element(Element, AccSocket),
            ElementChange = {ElementIndex, RenderedValue},
            {[ElementChange | ChangesAcc], UpdatedSocket}
        end,
        {[], Socket},
        ElementsList
    ),
    {lists:reverse(ElementChanges), FinalSocket}.
