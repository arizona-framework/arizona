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
-export([merge_element_changes/2]).

%% --------------------------------------------------------------------
%% Testing helper exports
%% --------------------------------------------------------------------

-export([get_affected_elements/2]).
-export([html_content/1]).
-export([component_changes/1]).
-export([is_html_content/1]).
-export([is_component_changes/1]).
-export([is_element_change/1]).
-export([get_element_change/1]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

-ignore_xref([get_affected_elements/2]).
-ignore_xref([html_content/1]).
-ignore_xref([component_changes/1]).
-ignore_xref([is_html_content/1]).
-ignore_xref([is_component_changes/1]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([element_change/0]).
-export_type([diff_changes/0]).
-export_type([element_index/0]).
-export_type([component_change/0]).
-export_type([element_change_entry/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-doc ~"""
Element change type for individual template element updates.

Opaque type that encapsulates either HTML content or component changes,
providing controlled access through helper functions and future-proof evolution.
""".
-opaque element_change() ::
    {html_content, arizona_html:html()}
    | {component_changes, [component_change()]}.

-doc ~"""
Element index type for referencing template elements.

Non-negative integer representing the position of an element within
a template's element map.
""".
-type element_index() :: non_neg_integer().

-doc ~"""
Component change type for stateful component updates.

Tuple containing the component ID and its associated element changes.
""".
-type component_change() :: {
    arizona_stateful:id() | element_index(), [element_change_entry()] | element_change()
}.

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

%% Helper function for merging component changes lists
merge_component_changes_list(NewChanges, ExistingChanges) ->
    lists:foldl(
        fun({ComponentId, NewElementChanges}, Acc) ->
            case lists:keyfind(ComponentId, 1, Acc) of
                false ->
                    [{ComponentId, NewElementChanges} | Acc];
                {ComponentId, ExistingElementChanges} ->
                    MergedElementChanges = merge_element_changes_list(
                        NewElementChanges, ExistingElementChanges
                    ),
                    lists:keyreplace(ComponentId, 1, Acc, {ComponentId, MergedElementChanges})
            end
        end,
        ExistingChanges,
        NewChanges
    ).

%% Helper function for merging element changes within a component
merge_element_changes_list(NewElements, ExistingElements) ->
    lists:foldl(
        fun({ElementIndex, NewChange}, Acc) ->
            case lists:keyfind(ElementIndex, 1, Acc) of
                false ->
                    [{ElementIndex, NewChange} | Acc];
                {ElementIndex, ExistingChange} ->
                    MergedChange = merge_element_changes(NewChange, ExistingChange),
                    lists:keyreplace(ElementIndex, 1, Acc, {ElementIndex, MergedChange})
            end
        end,
        ExistingElements,
        NewElements
    ).

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
    process_elements(ElementsList, Elements, Socket).

%% Process elements recursively with ability to skip elements
process_elements([], _Elements, Socket) ->
    {[], Socket};
process_elements([ElementIndex | Rest], Elements, Socket) ->
    Element = maps:get(ElementIndex, Elements),

    % Clear any previous changes before processing element
    CleanSocket = arizona_socket:clear_changes(Socket),

    % Process this element
    case process_single_element(ElementIndex, Element, CleanSocket) of
        {skip, _UpdatedSocket} ->
            % Skip this element, continue with next using original socket
            process_elements(Rest, Elements, Socket);
        {ElementChange, _UpdatedSocket} ->
            % Include this element change, but use original socket for next elements
            % to prevent sharing of changes between elements
            {RestChages, FinalSocket} = process_elements(Rest, Elements, Socket),
            {[ElementChange | RestChages], FinalSocket}
    end.

%% Process a single element and return either {skip, Socket} or {{Index, Change}, Socket}
process_single_element(ElementIndex, Element, Socket) ->
    case Element of
        {static, _Line, Content} ->
            % Static element - tag as HTML content
            {{ElementIndex, html_content(Content)}, Socket};
        {dynamic, Line, Fun} ->
            % Dynamic element - call function and handle result
            try
                Result = arizona_stateful:call_dynamic_function(Fun, Socket),
                handle_dynamic_result(ElementIndex, Result, Socket)
            catch
                throw:{binding_not_found, Key} ->
                    error(
                        {binding_not_found, Key},
                        none,
                        binding_error_info(Line, Key, Socket)
                    );
                _:Error ->
                    error({template_render_error, Error, Line})
            end
    end.

%% Handle the result from a dynamic function call
handle_dynamic_result(ElementIndex, Result, Socket) ->
    case arizona_socket:is_socket(Result) of
        true ->
            % Socket returned - extract nested changes or HTML
            NestedChanges = arizona_socket:get_changes(Result),
            handle_socket_result(ElementIndex, NestedChanges, Result);
        false ->
            % Regular value - convert to HTML and tag it
            {Html, ResultSocket} = arizona_html:to_html(Result, Socket),
            {{ElementIndex, html_content(Html)}, ResultSocket}
    end.

%% Handle socket results with nested changes
handle_socket_result(ElementIndex, NestedChanges, ResultSocket) ->
    case NestedChanges of
        [] ->
            % No changes, check HTML content
            Html = arizona_socket:get_html(ResultSocket),
            case Html of
                [] ->
                    % No changes and no content, skip this element
                    {skip, ResultSocket};
                _ ->
                    % Has HTML content, tag it
                    {{ElementIndex, html_content(Html)}, ResultSocket}
            end;
        _ ->
            % Has nested changes - tag them as component changes
            CleanSocket = arizona_socket:clear_changes(ResultSocket),
            {{ElementIndex, component_changes(NestedChanges)}, CleanSocket}
    end.

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

%% --------------------------------------------------------------------
%% Element change helper functions (opaque type access)
%% --------------------------------------------------------------------

-doc ~"""
Create an element change containing HTML content.
""".
-spec html_content(Html) -> ElementChange when
    Html :: arizona_html:html(),
    ElementChange :: element_change().
html_content(Html) ->
    {html_content, Html}.

-doc ~"""
Create an element change containing component changes.
""".
-spec component_changes(Changes) -> ElementChange when
    Changes :: [component_change()],
    ElementChange :: element_change().
component_changes(Changes) ->
    {component_changes, Changes}.

-doc ~"""
Check if an element change contains HTML content.
""".
-spec is_html_content(ElementChange) -> boolean() when
    ElementChange :: element_change().
is_html_content({html_content, _}) -> true;
is_html_content(_) -> false.

-doc ~"""
Check if an element change contains component changes.
""".
-spec is_component_changes(ElementChange) -> boolean() when
    ElementChange :: element_change().
is_component_changes({component_changes, _}) -> true;
is_component_changes(_) -> false.

-doc ~"""
Check if a term is an element change opaque type.
""".
-spec is_element_change(Term) -> boolean() when
    Term :: term().
is_element_change({html_content, _}) -> true;
is_element_change({component_changes, _}) -> true;
is_element_change(_) -> false.

-doc ~"""
Extract the actual content from an element change opaque type for JSON encoding.
""".
-spec get_element_change(ElementChange) -> Content when
    ElementChange :: element_change(),
    Content :: term().
get_element_change({html_content, Html}) -> Html;
get_element_change({component_changes, Changes}) -> Changes.

-doc ~"""
Merge two element changes using the optimal pattern matching approach.
Replaces the complex heuristic-based merge_nested_changes function.
""".
-spec merge_element_changes(New, Existing) -> Merged when
    New :: element_change(),
    Existing :: element_change(),
    Merged :: element_change().
merge_element_changes({component_changes, NewChanges}, {component_changes, ExistingChanges}) ->
    % Merge component changes using existing logic
    MergedChanges = merge_component_changes_list(NewChanges, ExistingChanges),
    {component_changes, MergedChanges};
merge_element_changes(New, _Existing) ->
    % For HTML content or mixed types: new takes precedence
    New.
