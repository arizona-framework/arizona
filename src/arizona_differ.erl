-module(arizona_differ).
-moduledoc ~"""
Provides efficient differential rendering for Arizona LiveView applications.

## Overview

The differ module implements optimized diffing for Arizona templates to minimize
data transmission over WebSocket connections. It analyzes changes in stateful
components and generates JSON diffs that contain only the modified elements,
reducing bandwidth usage and improving performance.

## Features

- **Optimized Diffing**: Uses changed_bindings and vars_indexes for efficient change detection
- **Stateful Support**: Specialized diffing for stateful components with binding tracking
- **Element Tracking**: Tracks which template elements are affected by variable changes
- **JSON Generation**: Produces WebSocket-compatible JSON diff format
- **Performance Focus**: Minimizes computation and data transmission overhead
- **Change Detection**: Skips diffing when no changes are detected

## Key Functions

- `diff_stateful/3`: Generate diffs for stateful components based on changed bindings
- `get_affected_elements/2`: Determine which elements need updating from changed variables

## Diff Format

Generates JSON diffs in the format: `[{StatefulId, [{ElementIndex, Changes}]}]`

## Diffing Strategy

- **Stateful Components**: Uses changed_bindings with vars_indexes for targeted diffing
- **Stateless Components**: Full re-render when variables change (no diffing optimization)

The module focuses on minimizing WebSocket payload size while maintaining
correctness and performance for real-time LiveView updates.
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([diff_stateful/3]).
-export([diff_stateless/3]).
-export([get_affected_elements/2]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

-ignore_xref([get_affected_elements/2]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([diff_changes/0]).
-export_type([element_index/0]).
-export_type([element_change/0]).
-export_type([element_change_entry/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-doc ~"""
Element index type for referencing template elements.

Non-negative integer representing the position of an element within
a template's element map.
""".
-type element_index() :: non_neg_integer().

-doc ~"""
Element value type for rendered template elements.

Can be any term depending on the element type and rendering context.
""".
-type element_value() :: term().

-doc ~"""
Nested changes type for hierarchical component updates.

List of component changes for nested stateful components.
""".
-type nested_changes() :: [component_change()].

-doc ~"""
Element change type for individual template element updates.

Either a rendered element value or nested changes for hierarchical components.
""".
-type element_change() :: element_value() | nested_changes().

-doc ~"""
Component change type for stateful component updates.

Tuple containing the component ID and its associated element changes.
""".
-type component_change() :: {arizona_stateful:id(), [element_change_entry()]}.

-doc ~"""
Element change entry type for individual element updates.

Tuple containing the element index and its change data.
""".
-type element_change_entry() :: {element_index(), element_change()}.

-doc ~"""
Diff changes type for complete diff result.

List of component changes representing all modifications in a diff operation.
""".
-type diff_changes() :: [component_change()].

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-doc ~"""
Generate optimized diffs for stateful components.

Analyzes changed bindings in a stateful component to determine which template
elements need updating, then generates minimal diff data for WebSocket transmission.
Uses the vars_indexes optimization to avoid checking unchanged elements.

## Examples

```erlang
1> TemplateData = #{elems => #{0 => {dynamic, 1, fun...}}, vars_indexes => #{name => [0]}}.
#{...}
2> StatefulState = arizona_stateful:put_binding(name, ~"John", State).
#state{changed_bindings = #{name => ~"John"}, ...}
3> arizona_differ:diff_stateful(TemplateData, StatefulState, Socket).
#socket{changes = [{root, [{0, ~"John"}]}], ...}
```

## Performance

Only processes elements that are actually affected by changed bindings,
making diffing extremely efficient even for large templates.
""".
-spec diff_stateful(TemplateData, StatefulState, Socket) -> Socket1 when
    TemplateData :: arizona_renderer:template_data(),
    StatefulState :: arizona_stateful:state(),
    Socket :: arizona_socket:socket(),
    Socket1 :: arizona_socket:socket().
diff_stateful(TemplateData, StatefulState, Socket) ->
    ChangedBindings = arizona_stateful:get_changed_bindings(StatefulState),
    case has_changes(ChangedBindings) of
        false ->
            Socket;
        true ->
            StatefulId = arizona_stateful:get_id(StatefulState),
            AffectedElements = get_affected_elements_from_stateful_template(
                TemplateData, ChangedBindings
            ),

            case has_affected_elements(AffectedElements) of
                false ->
                    Socket;
                true ->
                    process_stateful_changes(StatefulId, AffectedElements, TemplateData, Socket)
            end
    end.

-doc ~"""
Diff stateless component template data for hierarchical updates.

Stateless components can now report individual element changes instead of
consolidated HTML when they have proper vars_indexes optimization.
This enables nested component diffing where only changed elements are reported.

## Examples

```erlang
1> TemplateData = #{elems_order => [0,1,2], elems => #{...}, vars_indexes => #{name => [1]}}.
2> StatefulState = arizona_stateful:new(id, module, #{name => "John"}).
3> ChangedState = arizona_stateful:put_binding(name, "Jane", StatefulState).
4> arizona_differ:diff_stateless(TemplateData, ChangedState, Socket).
#socket{changes = [{stateless_element_changes, [{1, "Jane"}]}], ...}
```
""".
-spec diff_stateless(TemplateData, StatefulState, Socket) -> Socket1 when
    TemplateData :: arizona_renderer:template_data(),
    StatefulState :: arizona_stateful:state(),
    Socket :: arizona_socket:socket(),
    Socket1 :: arizona_socket:socket().
diff_stateless(TemplateData, StatefulState, Socket) ->
    ChangedBindings = arizona_stateful:get_changed_bindings(StatefulState),
    case has_changes(ChangedBindings) of
        false ->
            Socket;
        true ->
            AffectedElements = get_affected_elements_from_stateless_template(
                TemplateData, ChangedBindings
            ),

            case has_affected_elements(AffectedElements) of
                false ->
                    Socket;
                true ->
                    process_stateless_changes(AffectedElements, TemplateData, Socket)
            end
    end.

-doc ~"""
Diff stateless component template data for hierarchical updates.

Stateless components can now report individual element changes instead of
consolidated HTML when they have proper vars_indexes optimization.
This enables nested component diffing where only changed elements are reported.

## Examples

```erlang
1> TemplateData = #{elems_order => [0,1,2], elems => #{...}, vars_indexes => #{name => [1]}}.
2> StatefulState = arizona_stateful:new(id, module, #{name => "John"}).
3> ChangedState = arizona_stateful:put_binding(name, "Jane", StatefulState).
4> arizona_differ:diff_stateless(TemplateData, ChangedState, Socket).
#socket{changes = [{stateless_element_changes, [{1, "Jane"}]}], ...}
```
""".
-spec diff_stateless(TemplateData, StatefulState, Socket) -> Socket1 when
    TemplateData :: arizona_renderer:stateful_template_data(),
    StatefulState :: arizona_stateful:state(),
    Socket :: arizona_socket:socket(),
    Socket1 :: arizona_socket:socket().
diff_stateless(TemplateData, StatefulState, Socket) ->
    % Get changed bindings (already filtered by put_binding/3)
    ChangedBindings = arizona_stateful:get_changed_bindings(StatefulState),
    case maps:size(ChangedBindings) of
        0 ->
            % No changes, return socket unchanged
            Socket;
        _ ->
            % For stateless components, check if any changed binding affects this component
            VarsIndexes = maps:get(vars_indexes, TemplateData),
            AffectedElements = get_affected_elements(ChangedBindings, VarsIndexes),

            case sets:size(AffectedElements) of
                0 ->
                    % No elements affected, return socket unchanged
                    Socket;
                _ ->
                    % Create element changes for hierarchical diffing
                    {ElementChanges, UpdatedSocket} = create_element_changes(
                        AffectedElements, TemplateData, Socket
                    ),

                    % Store element changes in special format for parent to detect
                    FakeComponentChange = {stateless_element_changes, ElementChanges},
                    arizona_socket:append_changes([FakeComponentChange], UpdatedSocket)
            end
    end.

-doc ~"""
Get affected element indexes from changed bindings and variable indexes.

Analyzes which template elements are affected by variable changes using the
vars_indexes optimization. Returns a set of element indexes that need to be
re-rendered based on the changed variable bindings.

## Examples

```erlang
1> ChangedBindings = #{name => ~"John", age => 30}.
#{name => ~"John", age => 30}
2> VarsIndexes = #{name => [0, 2], age => [1], city => [3]}.
#{name => [0, 2], age => [1], city => [3]}
3> arizona_differ:get_affected_elements(ChangedBindings, VarsIndexes).
#{0, 1, 2}  % Set containing affected element indexes
```

## Performance

Uses efficient set operations to avoid duplicate element processing when
multiple changed variables affect the same elements.
""".
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

%% --------------------------------------------------------------------
%% Private functions
%% --------------------------------------------------------------------

%% Check if there are any changes in the bindings
-spec has_changes(ChangedBindings) -> boolean() when
    ChangedBindings :: map().
has_changes(ChangedBindings) ->
    maps:size(ChangedBindings) > 0.

%% Get affected elements from template data and changed bindings for stateful components
-spec get_affected_elements_from_stateful_template(TemplateData, ChangedBindings) ->
    AffectedElements
when
    TemplateData :: arizona_renderer:template_data(),
    ChangedBindings :: map(),
    AffectedElements :: sets:set(element_index()).
get_affected_elements_from_stateful_template(TemplateData, ChangedBindings) ->
    VarsIndexes = maps:get(vars_indexes, TemplateData, #{}),
    get_affected_elements(ChangedBindings, VarsIndexes).

%% Get affected elements from template data and changed bindings for stateless components
%% Does not provide default for vars_indexes to maintain original error behavior
-spec get_affected_elements_from_stateless_template(TemplateData, ChangedBindings) ->
    AffectedElements
when
    TemplateData :: arizona_renderer:template_data(),
    ChangedBindings :: map(),
    AffectedElements :: sets:set(element_index()).
get_affected_elements_from_stateless_template(TemplateData, ChangedBindings) ->
    % No default - will error if missing
    VarsIndexes = maps:get(vars_indexes, TemplateData),
    get_affected_elements(ChangedBindings, VarsIndexes).

%% Check if there are any affected elements
-spec has_affected_elements(AffectedElements) -> boolean() when
    AffectedElements :: sets:set(element_index()).
has_affected_elements(AffectedElements) ->
    sets:size(AffectedElements) > 0.

%% Process changes for stateful components
-spec process_stateful_changes(
    StatefulId,
    AffectedElements,
    TemplateData,
    Socket
) -> Socket1 when
    StatefulId :: arizona_stateful:id(),
    AffectedElements :: sets:set(element_index()),
    TemplateData :: arizona_renderer:template_data(),
    Socket :: arizona_socket:socket(),
    Socket1 :: arizona_socket:socket().
process_stateful_changes(StatefulId, AffectedElements, TemplateData, Socket) ->
    {ElementChanges, UpdatedSocket} = create_element_changes(
        AffectedElements, TemplateData, Socket
    ),
    case ElementChanges of
        [] ->
            % No element changes after processing, skip component change
            UpdatedSocket;
        _ ->
            % Has element changes, include component change
            ComponentChange = {StatefulId, ElementChanges},
            arizona_socket:append_changes([ComponentChange], UpdatedSocket)
    end.

%% Process changes for stateless components
-spec process_stateless_changes(
    AffectedElements,
    TemplateData,
    Socket
) -> Socket1 when
    AffectedElements :: sets:set(element_index()),
    TemplateData :: arizona_renderer:template_data(),
    Socket :: arizona_socket:socket(),
    Socket1 :: arizona_socket:socket().
process_stateless_changes(AffectedElements, TemplateData, Socket) ->
    {ElementChanges, UpdatedSocket} = create_element_changes(
        AffectedElements, TemplateData, Socket
    ),
    % Store element changes directly - parent will extract them as nested changes
    arizona_socket:append_changes(ElementChanges, UpdatedSocket).

%% Create element changes for affected elements
-spec create_element_changes(
    sets:set(element_index()),
    arizona_renderer:template_data(),
    arizona_socket:socket()
) -> {[element_change_entry()], arizona_socket:socket()}.
create_element_changes(AffectedElements, TemplateData, Socket) ->
    ElementsList = sets:to_list(AffectedElements),
    Elements = maps:get(elems, TemplateData),
    {ElementChanges, FinalSocket} = lists:foldl(
        fun(ElementIndex, {ChangesAcc, AccSocket}) ->
            Element = maps:get(ElementIndex, Elements),

            % Clear any previous changes before processing element
            CleanSocket = arizona_socket:clear_changes(AccSocket),

            % Handle element based on type for hierarchical diffing
            ElementChange =
                case Element of
                    {static, _Line, Content} ->
                        % Static element - use content directly
                        {ElementIndex, Content};
                    {dynamic, Line, Fun} ->
                        % Dynamic element - call function and check for nested changes
                        try
                            Result = arizona_stateful:call_dynamic_function(Fun, CleanSocket),

                            % Check if result is a socket (from stateless component in diff mode)
                            case arizona_socket:is_socket(Result) of
                                true ->
                                    % Socket returned - extract nested changes
                                    NestedChanges = arizona_socket:get_changes(Result),
                                    case NestedChanges of
                                        [] ->
                                            % No changes, get HTML content
                                            Html = arizona_socket:get_html(Result),
                                            {ElementIndex, Html};
                                        [{stateless_element_changes, ElementChanges}] ->
                                            % Stateless component changes - extract element changes
                                            {ElementIndex, ElementChanges};
                                        _ ->
                                            % Other nested changes, use them as-is
                                            {ElementIndex, NestedChanges}
                                    end;
                                false ->
                                    % Regular value - convert to HTML
                                    {Html, _} = arizona_html:to_html(Result, CleanSocket),
                                    {ElementIndex, Html}
                            end
                        catch
                            throw:{binding_not_found, Key} ->
                                error(
                                    {binding_not_found, Key},
                                    none,
                                    binding_error_info(Line, Key, CleanSocket)
                                );
                            _:Error ->
                                error({template_render_error, Error, Line})
                        end
                end,
            {[ElementChange | ChangesAcc], AccSocket}
        end,
        {[], Socket},
        ElementsList
    ),
    {lists:reverse(ElementChanges), FinalSocket}.

%% Error info for binding errors following OTP pattern
binding_error_info(Line, Key, Socket) ->
    CurrentState = arizona_socket:get_current_stateful_state(Socket),
    TemplateModule = arizona_stateful:get_module(CurrentState),
    [
        {error_info, #{
            cause => #{binding => Key, line => Line, template_module => TemplateModule},
            module => arizona_differ
        }}
    ].
