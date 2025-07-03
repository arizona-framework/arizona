-module(arizona_differ).
-moduledoc ~"""
Arizona Differ - Optimized JSON diff generation for WebSocket transmission.

This module implements efficient diffing for Arizona templates:
- Stateful components: Use changed_bindings + vars_indexes for optimized diffing
- Stateless components: Full re-render when variables change (no diffing)

Generates JSON diffs in format: [{StatefulId, [{ElementIndex, Changes}]}]
""".

-export([diff_stateful/2]).
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
-spec diff_stateful(Stateful, Socket) -> Socket1 when
    Stateful :: arizona_stateful:stateful(),
    Socket :: arizona_socket:socket(),
    Socket1 :: arizona_socket:socket().
diff_stateful(Stateful, Socket) ->
    case arizona_socket:get_mode(Socket) of
        render ->
            % In render mode, just return socket (no diffing)
            Socket;
        diff ->
            diff_stateful_impl(Stateful, Socket)
    end.

%% Get affected element indexes from changed bindings and vars_indexes
-spec get_affected_elements(ChangedBindings, VarsIndexes) -> AffectedIndexes when
    ChangedBindings :: map(),
    VarsIndexes :: #{binary() => [element_index()]},
    AffectedIndexes :: sets:set(element_index()).
get_affected_elements(ChangedBindings, VarsIndexes) ->
    ChangedVarNames = [atom_to_binary(K) || K <- maps:keys(ChangedBindings)],
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

-spec diff_stateful_impl(arizona_stateful:stateful(), arizona_socket:socket()) ->
    arizona_socket:socket().
diff_stateful_impl(Stateful, Socket) ->
    % Get changed bindings (already filtered by put_binding/3)
    ChangedBindings = arizona_stateful:get_changed_bindings(Stateful),
    case maps:size(ChangedBindings) of
        0 ->
            % No changes, return socket unchanged
            Socket;
        _ ->
            StatefulId = arizona_stateful:get_id(Stateful),

            % Get template data for this stateful component
            % TODO: Need to get the actual template data structure from the component
            % For now, create a minimal structure to test the path logic
            TemplateData = #{
                vars_indexes => #{
                    % Example: if 'counter' binding changed, it affects element 2
                    ~"counter" => [2],
                    ~"name" => [1, 3]
                }
            },

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
                    ElementChanges = create_element_changes(AffectedElements, ChangedBindings),
                    ComponentChange = {StatefulId, ElementChanges},

                    % Append changes with proper path tracking
                    arizona_socket:append_changes([ComponentChange], Socket)
            end
    end.

%% Create element changes for affected elements
-spec create_element_changes(sets:set(element_index()), map()) -> [element_change_entry()].
create_element_changes(AffectedElements, ChangedBindings) ->
    ElementsList = sets:to_list(AffectedElements),
    [create_element_change(ElementIndex, ChangedBindings) || ElementIndex <- ElementsList].

%% Create a single element change entry
-spec create_element_change(element_index(), map()) -> element_change_entry().
create_element_change(ElementIndex, ChangedBindings) ->
    % For now, create a simple change entry
    % TODO: This should evaluate the actual element content with new bindings
    % For testing, just use the first changed binding value
    [FirstValue | _] = maps:values(ChangedBindings),
    {ElementIndex, FirstValue}.
