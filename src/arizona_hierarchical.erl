-module(arizona_hierarchical).
-moduledoc ~"""
Provides hierarchical rendering functionality for Arizona templates to enable
efficient real-time updates via WebSocket connections with surgical DOM updates.

## Overview

The hierarchical renderer provides a structured approach to template rendering
that maintains JSON-compatible data structures for efficient WebSocket transmission.
It supports three main component types:

- **Stateful components**: Separate hierarchical entries by ID for independent updates
- **Stateless components**: Nested diffable structures for lightweight components
- **List components**: Static template + dynamic data separation for efficient list rendering

The module enables granular diffing at the element level within any component type,
allowing for surgical DOM updates that minimize client-side re-rendering.

## Key Functions

- `create_structure/0`: Initialize empty hierarchical structure
- `stateful_structure/2`: Generate structure for stateful components
- `stateless_structure/2`: Generate structure for stateless components
- `list_structure/3`: Generate structure for list components with static/dynamic parts
- `diff_structures/2`: Generate diff operations between two structures
- `apply_diff/2`: Apply diff operations to transform structures
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([create_structure/0]).
-export([add_stateful/3]).
-export([set_element/4]).
-export([stateful_structure/2]).
-export([stateless_structure/2]).
-export([list_structure/3]).
-export([diff_structures/2]).
-export([apply_diff/2]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

-ignore_xref([create_structure/0]).
-ignore_xref([add_stateful/3]).
-ignore_xref([set_element/4]).
-ignore_xref([diff_structures/2]).
-ignore_xref([apply_diff/2]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([element_index/0]).
-export_type([element_content/0]).
-export_type([stateful_render/0]).
-export_type([hierarchical_structure/0]).
-export_type([diff_operation/0]).
-export_type([element_diff/0]).
-export_type([hierarchical_diff/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-doc ~"""
Index for elements within a component structure.
""".
-type element_index() :: non_neg_integer().

-doc ~"""
Content that can be stored in a hierarchical element.

Can be static HTML, a reference to a stateful component, a nested stateless structure,
or a list component with static template and dynamic data.
""".
-type element_content() ::
    arizona_html:html()
    | #{type := stateful, id := arizona_stateful:id()}
    | #{type := stateless, structure := #{element_index() => element_content()}}
    | #{type := list, static := [binary()], dynamic := [#{element_index() => element_content()}]}.

-doc ~"""
Structure representing a rendered stateful component mapping element indices to content.
""".
-type stateful_render() :: #{element_index() => element_content()}.

-doc ~"""
Complete hierarchical structure mapping component IDs to their rendered content.
""".
-type hierarchical_structure() :: #{arizona_stateful:id() => stateful_render()}.

-doc ~"""
Operation describing a change to a hierarchical structure.

Can be adding, removing, or updating a stateful component.
""".
-type diff_operation() :: #{
    type := add_stateful | remove_stateful | update_stateful,
    stateful_id := arizona_stateful:id(),
    data := term()
}.

-doc ~"""
Operation describing a change to an element within a component.

Can be setting, removing, or updating an element at a specific index.
""".
-type element_diff() :: #{
    type := set_element | remove_element | update_stateless | update_list_dynamic,
    element_index := element_index(),
    data := term()
}.

-doc ~"""
List of diff operations to transform one hierarchical structure to another.
""".
-type hierarchical_diff() :: [diff_operation()].

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-doc ~"""
Create empty hierarchical structure for building component hierarchies.

Returns a new empty hierarchical structure that can be used to build up
component hierarchies incrementally.

## Examples

```erlang
1> arizona_hierarchical:create_structure().
#{}
```
""".
-spec create_structure() -> HierarchicalStructure when
    HierarchicalStructure :: hierarchical_structure().
create_structure() ->
    #{}.

-doc ~"""
Add stateful component with the given ID and render data to the hierarchical structure.

Adds a stateful component to the hierarchical structure, allowing it to be
independently updated and diffed.

## Examples

```erlang
1> Structure = arizona_hierarchical:create_structure().
#{}
2> Render = #{0 => <<"Hello">>, 1 => <<"World">>}.
#{0 => <<"Hello">>,1 => <<"World">>}
3> arizona_hierarchical:add_stateful(root, Render, Structure).
#{root => #{0 => <<"Hello">>,1 => <<"World">>}}
```
""".
-spec add_stateful(StatefulId, StatefulRender, Structure) -> UpdatedStructure when
    StatefulId :: arizona_stateful:id(),
    StatefulRender :: stateful_render(),
    Structure :: hierarchical_structure(),
    UpdatedStructure :: hierarchical_structure().
add_stateful(StatefulId, StatefulRender, Structure) when
    is_map(StatefulRender), is_map(Structure)
->
    Structure#{StatefulId => StatefulRender}.

-doc ~"""
Set or update an element at the specified index within a stateful component.

Updates a specific element within a stateful component's render structure,
creating the component if it doesn't exist.

## Examples

```erlang
1> Structure = arizona_hierarchical:create_structure().
#{}
2> arizona_hierarchical:set_element(root, 0, <<"Content">>, Structure).
#{root => #{0 => <<"Content">>}}
```
""".
-spec set_element(StatefulId, ElementIndex, Content, Structure) -> UpdatedStructure when
    StatefulId :: arizona_stateful:id(),
    ElementIndex :: element_index(),
    Content :: element_content(),
    Structure :: hierarchical_structure(),
    UpdatedStructure :: hierarchical_structure().
set_element(StatefulId, ElementIndex, Content, Structure) when
    is_integer(ElementIndex), is_map(Structure)
->
    StatefulRender = maps:get(StatefulId, Structure, #{}),
    UpdatedRender = StatefulRender#{ElementIndex => Content},
    Structure#{StatefulId => UpdatedRender}.

-doc ~"""
Generate hierarchical structure for stateful component with efficient diffing support.

Processes a stateful template and generates a hierarchical structure that can be
efficiently diffed and transmitted via WebSocket for real-time updates.
""".
-spec stateful_structure(TemplateData, Socket) -> {ComponentStructure, Socket1} when
    TemplateData :: arizona_renderer:stateful_template_data(),
    Socket :: arizona_socket:socket(),
    ComponentStructure :: stateful_render(),
    Socket1 :: arizona_socket:socket().
stateful_structure(#{elems_order := Order, elems := Elements}, Socket) ->
    StatefulId = arizona_socket:get_current_stateful_id(Socket),
    CurrentHierarchical = arizona_socket:get_hierarchical_acc(Socket),

    % Generate elements structure
    {ComponentRender, UpdatedSocket} = elements_structure(Order, Elements, Socket, #{}),

    % Update hierarchical accumulator
    NewHierarchical = CurrentHierarchical#{StatefulId => ComponentRender},
    UpdatedSocket1 = arizona_socket:set_hierarchical_acc(NewHierarchical, UpdatedSocket),

    {ComponentRender, UpdatedSocket1}.

-doc ~"""
Generate hierarchical structure for stateless component that can be embedded within
stateful components.

Processes a stateless template and generates a nested structure that can be
embedded within stateful components for efficient rendering and updates.
""".
-spec stateless_structure(TemplateData, Socket) -> {StatelessElement, Socket1} when
    TemplateData :: arizona_renderer:stateless_template_data(),
    Socket :: arizona_socket:socket(),
    StatelessElement :: element_content(),
    Socket1 :: arizona_socket:socket().
stateless_structure(StructuredList, Socket) ->
    {StatelessStructure, UpdatedSocket} = stateless_list_to_structure(StructuredList, Socket),
    StatelessElement = #{type => stateless, structure => StatelessStructure},
    UpdatedSocket1 = arizona_socket:set_hierarchical_pending_element(
        StatelessElement, UpdatedSocket
    ),
    {StatelessElement, UpdatedSocket1}.

-doc ~"""
Generate hierarchical structure for list component with static template and dynamic data for
efficient rendering.

Processes a list template with static and dynamic parts, generating an optimized
structure that separates static template content from dynamic data for efficient
list rendering and updates.
""".
-spec list_structure(ListData, Items, Socket) -> {ListElement, Socket1} when
    ListData :: arizona_renderer:list_template_data(),
    Items :: [term()],
    Socket :: arizona_socket:socket(),
    ListElement :: element_content(),
    Socket1 :: arizona_socket:socket().
list_structure(#{static := Static, dynamic := DynamicTemplate}, Items, Socket) ->
    % Generate dynamic data for each item
    {DynamicData, UpdatedSocket} = list_items_to_dynamic_data(DynamicTemplate, Items, Socket, []),

    % Create list structure
    ListElement = #{
        type => list,
        static => Static,
        dynamic => DynamicData
    },

    UpdatedSocket1 = arizona_socket:set_hierarchical_pending_element(ListElement, UpdatedSocket),
    {ListElement, UpdatedSocket1}.

-doc ~"""
Generate diff operations between two hierarchical structures for efficient updates.

Compares two hierarchical structures and generates a list of diff operations
that describe the changes needed to transform the old structure into the new
structure, enabling efficient WebSocket updates.

## Examples

```erlang
1> Old = #{root => #{0 => <<"Old">>}}.
#{root => #{0 => <<"Old">>}}
2> New = #{root => #{0 => <<"New">>}}.
#{root => #{0 => <<"New">>}}
3> arizona_hierarchical:diff_structures(Old, New).
[#{type => update_stateful, stateful_id => root,
   data => [#{type => set_element, element_index => 0, data => <<"New">>}]}]
```
""".
-spec diff_structures(OldStructure, NewStructure) -> HierarchicalDiff when
    OldStructure :: hierarchical_structure(),
    NewStructure :: hierarchical_structure(),
    HierarchicalDiff :: hierarchical_diff().
diff_structures(OldStructure, NewStructure) when is_map(OldStructure), is_map(NewStructure) ->
    OldComponents = maps:keys(OldStructure),
    NewComponents = maps:keys(NewStructure),

    % Find added, removed, and potentially changed components
    AddedComponents = NewComponents -- OldComponents,
    RemovedComponents = OldComponents -- NewComponents,
    CommonComponents = NewComponents -- AddedComponents,

    % Generate diff operations
    AddOps = generate_add_ops(AddedComponents, NewStructure),
    RemoveOps = generate_remove_ops(RemovedComponents),
    UpdateOps = generate_update_ops(CommonComponents, OldStructure, NewStructure),

    AddOps ++ RemoveOps ++ UpdateOps.

-doc ~"""
Apply diff operations to hierarchical structure to transform it according to specified changes.

Applies a list of diff operations to a hierarchical structure, transforming it
according to the specified changes. This is the inverse operation of diff_structures/2.

## Examples

```erlang
1> Structure = #{root => #{0 => <<"Old">>}}.
#{root => #{0 => <<"Old">>}}
2> Diff = [#{type => update_stateful, stateful_id => root,
2>          data => [#{type => set_element, element_index => 0, data => <<"New">>}]}].
[#{type => update_stateful, stateful_id => root,
   data => [#{type => set_element, element_index => 0, data => <<"New">>}]}]
3> arizona_hierarchical:apply_diff(Diff, Structure).
#{root => #{0 => <<"New">>}}
```
""".
-spec apply_diff(Diff, Structure) -> UpdatedStructure when
    Diff :: hierarchical_diff(),
    Structure :: hierarchical_structure(),
    UpdatedStructure :: hierarchical_structure().
apply_diff(Diff, Structure) when is_list(Diff), is_map(Structure) ->
    lists:foldl(fun apply_diff_operation/2, Structure, Diff).

%% --------------------------------------------------------------------
%% Private functions
%% --------------------------------------------------------------------

%% Generate add operations for new components
-spec generate_add_ops(AddedComponents, NewStructure) -> DiffOperations when
    AddedComponents :: [arizona_stateful:id()],
    NewStructure :: hierarchical_structure(),
    DiffOperations :: [diff_operation()].
generate_add_ops(AddedComponents, NewStructure) ->
    [
        #{
            type => add_stateful,
            stateful_id => StatefulId,
            data => maps:get(StatefulId, NewStructure)
        }
     || StatefulId <- AddedComponents
    ].

%% Generate remove operations for removed components
-spec generate_remove_ops(RemovedComponents) -> DiffOperations when
    RemovedComponents :: [arizona_stateful:id()],
    DiffOperations :: [diff_operation()].
generate_remove_ops(RemovedComponents) ->
    [
        #{
            type => remove_stateful,
            stateful_id => StatefulId,
            data => undefined
        }
     || StatefulId <- RemovedComponents
    ].

%% Generate update operations for potentially changed components
-spec generate_update_ops(CommonComponents, OldStructure, NewStructure) -> DiffOperations when
    CommonComponents :: [arizona_stateful:id()],
    OldStructure :: hierarchical_structure(),
    NewStructure :: hierarchical_structure(),
    DiffOperations :: [diff_operation()].
generate_update_ops(CommonComponents, OldStructure, NewStructure) ->
    lists:filtermap(
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
    ).

%% Compare two stateful components and generate element diffs
-spec diff_components(OldComponent, NewComponent) -> ElementDiffs when
    OldComponent :: stateful_render(),
    NewComponent :: stateful_render(),
    ElementDiffs :: [element_diff()].
diff_components(OldComponent, NewComponent) when is_map(OldComponent), is_map(NewComponent) ->
    OldElements = maps:keys(OldComponent),
    NewElements = maps:keys(NewComponent),

    % Find added, removed, and potentially changed elements
    AddedElements = NewElements -- OldElements,
    RemovedElements = OldElements -- NewElements,
    CommonElements = NewElements -- AddedElements,

    % Generate element diffs
    AddDiffs = generate_element_add_diffs(AddedElements, NewComponent),
    RemoveDiffs = generate_element_remove_diffs(RemovedElements),
    UpdateDiffs = generate_element_update_diffs(CommonElements, OldComponent, NewComponent),

    AddDiffs ++ RemoveDiffs ++ UpdateDiffs.

%% Generate add diffs for new elements
-spec generate_element_add_diffs(AddedElements, NewComponent) -> ElementDiffs when
    AddedElements :: [element_index()],
    NewComponent :: stateful_render(),
    ElementDiffs :: [element_diff()].
generate_element_add_diffs(AddedElements, NewComponent) ->
    [
        #{
            type => set_element,
            element_index => ElementIndex,
            data => maps:get(ElementIndex, NewComponent)
        }
     || ElementIndex <- AddedElements
    ].

%% Generate remove diffs for removed elements
-spec generate_element_remove_diffs(RemovedElements) -> ElementDiffs when
    RemovedElements :: [element_index()],
    ElementDiffs :: [element_diff()].
generate_element_remove_diffs(RemovedElements) ->
    [
        #{
            type => remove_element,
            element_index => ElementIndex,
            data => undefined
        }
     || ElementIndex <- RemovedElements
    ].

%% Generate update diffs for potentially changed elements
-spec generate_element_update_diffs(CommonElements, OldComponent, NewComponent) -> ElementDiffs when
    CommonElements :: [element_index()],
    OldComponent :: stateful_render(),
    NewComponent :: stateful_render(),
    ElementDiffs :: [element_diff()].
generate_element_update_diffs(CommonElements, OldComponent, NewComponent) ->
    lists:filtermap(
        fun(ElementIndex) ->
            OldContent = maps:get(ElementIndex, OldComponent),
            NewContent = maps:get(ElementIndex, NewComponent),
            diff_element_content(ElementIndex, OldContent, NewContent)
        end,
        CommonElements
    ).

%% Compare element content and generate appropriate diff operations
%% Handles nested structures like stateless components and lists
-spec diff_element_content(ElementIndex, OldContent, NewContent) -> Result when
    ElementIndex :: element_index(),
    OldContent :: element_content(),
    NewContent :: element_content(),
    Result :: false | {true, element_diff()}.
diff_element_content(ElementIndex, OldContent, NewContent) ->
    case {OldContent, NewContent} of
        {Same, Same} ->
            % No change
            false;
        {
            #{type := stateless, structure := OldStruct},
            #{type := stateless, structure := NewStruct}
        } ->
            diff_stateless_content(ElementIndex, OldStruct, NewStruct);
        {#{type := list, static := Static, dynamic := OldDynamic}, #{
            type := list, static := Static, dynamic := NewDynamic
        }} ->
            diff_list_content(ElementIndex, OldDynamic, NewDynamic);
        _ ->
            % Different content, replace entirely
            {true, #{
                type => set_element,
                element_index => ElementIndex,
                data => NewContent
            }}
    end.

%% Compare stateless content structures
-spec diff_stateless_content(ElementIndex, OldStruct, NewStruct) -> Result when
    ElementIndex :: element_index(),
    OldStruct :: #{element_index() => element_content()},
    NewStruct :: #{element_index() => element_content()},
    Result :: false | {true, element_diff()}.
diff_stateless_content(ElementIndex, OldStruct, NewStruct) ->
    case diff_components(OldStruct, NewStruct) of
        [] ->
            false;
        ElementDiffs ->
            {true, #{
                type => update_stateless,
                element_index => ElementIndex,
                data => ElementDiffs
            }}
    end.

%% Compare list content with same static template
-spec diff_list_content(ElementIndex, OldDynamic, NewDynamic) -> Result when
    ElementIndex :: element_index(),
    OldDynamic :: [#{element_index() => element_content()}],
    NewDynamic :: [#{element_index() => element_content()}],
    Result :: false | {true, element_diff()}.
diff_list_content(ElementIndex, OldDynamic, NewDynamic) ->
    case OldDynamic =:= NewDynamic of
        true ->
            false;
        false ->
            {true, #{
                type => update_list_dynamic,
                element_index => ElementIndex,
                data => NewDynamic
            }}
    end.

%% Apply single diff operation to hierarchical structure
-spec apply_diff_operation(DiffOperation, Structure) -> UpdatedStructure when
    DiffOperation :: diff_operation(),
    Structure :: hierarchical_structure(),
    UpdatedStructure :: hierarchical_structure().
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

%% Apply element diffs to stateful component
-spec apply_element_diffs(ElementDiffs, Component) -> UpdatedComponent when
    ElementDiffs :: [element_diff()],
    Component :: stateful_render(),
    UpdatedComponent :: stateful_render().
apply_element_diffs(ElementDiffs, Component) when is_list(ElementDiffs), is_map(Component) ->
    lists:foldl(fun apply_element_diff/2, Component, ElementDiffs).

%% Apply single element diff to component
-spec apply_element_diff(ElementDiff, Component) -> UpdatedComponent when
    ElementDiff :: element_diff(),
    Component :: stateful_render(),
    UpdatedComponent :: stateful_render().
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

%% Generate structure for elements in order
-spec elements_structure(Order, Elements, Socket, ComponentRender) ->
    {ComponentRender1, Socket1}
when
    Order :: [non_neg_integer()],
    Elements :: #{non_neg_integer() => term()},
    Socket :: arizona_socket:socket(),
    ComponentRender :: stateful_render(),
    ComponentRender1 :: stateful_render(),
    Socket1 :: arizona_socket:socket().
elements_structure([], _Elements, Socket, ComponentRender) ->
    {ComponentRender, Socket};
elements_structure([Index | Rest], Elements, Socket, ComponentRender) ->
    Element = maps:get(Index, Elements),
    {Content, UpdatedSocket} = render_element(Element, Socket),
    NewComponentRender = ComponentRender#{Index => Content},
    elements_structure(Rest, Elements, UpdatedSocket, NewComponentRender).

%% Render individual elements in hierarchical mode
-spec render_element(Element, Socket) -> {Content, Socket1} when
    Element ::
        {static, pos_integer(), binary()}
        | {dynamic, pos_integer(), fun((arizona_socket:socket()) -> term())},
    Socket :: arizona_socket:socket(),
    Content :: element_content(),
    Socket1 :: arizona_socket:socket().
render_element({static, _Line, Content}, Socket) when is_binary(Content) ->
    {Content, Socket};
render_element({dynamic, Line, Fun}, Socket) when is_function(Fun, 1) ->
    try
        Result = arizona_stateful:call_dynamic_function(Fun, Socket),
        case arizona_socket:is_socket(Result) of
            true ->
                % Function returned a socket (from arizona_html calls like
                % render_list/render_stateless)
                % Check if there's a pending hierarchical element
                case arizona_socket:get_hierarchical_pending_element(Result) of
                    undefined ->
                        % No pending element, fall back to HTML content
                        HierarchicalContent = arizona_socket:get_html(Result),
                        {HierarchicalContent, Result};
                    PendingElement ->
                        % Use the pending hierarchical element and clear it
                        ClearedSocket = arizona_socket:clear_hierarchical_pending_element(Result),
                        {PendingElement, ClearedSocket}
                end;
            false ->
                % Not a socket, return the value as-is for hierarchical storage
                {Result, Socket}
        end
    catch
        throw:{binding_not_found, Key} ->
            error({binding_not_found, Key}, none, binding_error_info(Line, Key, Socket));
        _:Error ->
            error({template_render_error, Error, Line})
    end.

%% Helper function for error reporting
-spec binding_error_info(Line, Key, Socket) -> ErrorInfo when
    Line :: pos_integer(),
    Key :: term(),
    Socket :: arizona_socket:socket(),
    ErrorInfo :: [term()].
binding_error_info(Line, Key, Socket) ->
    CurrentState = arizona_socket:get_current_stateful_state(Socket),
    TemplateModule = arizona_stateful:get_module(CurrentState),
    [
        {error_info, #{
            cause => #{binding => Key, line => Line, template_module => TemplateModule},
            module => arizona_hierarchical
        }}
    ].

%% Convert stateless template list to hierarchical structure
-spec stateless_list_to_structure(List, Socket) -> {Structure, Socket1} when
    List :: list(),
    Socket :: arizona_socket:socket(),
    Structure :: #{element_index() => element_content()},
    Socket1 :: arizona_socket:socket().
stateless_list_to_structure(List, Socket) ->
    stateless_list_to_structure(List, Socket, 0, #{}).

%% Helper for stateless list conversion with index tracking
-spec stateless_list_to_structure(List, Socket, Index, Acc) -> {Structure, Socket1} when
    List :: list(),
    Socket :: arizona_socket:socket(),
    Index :: non_neg_integer(),
    Acc :: #{element_index() => element_content()},
    Structure :: #{element_index() => element_content()},
    Socket1 :: arizona_socket:socket().
stateless_list_to_structure([], Socket, _Index, Acc) ->
    {Acc, Socket};
stateless_list_to_structure([Element | Rest], Socket, Index, Acc) ->
    {Content, UpdatedSocket} = render_element(Element, Socket),
    NewAcc = Acc#{Index => Content},
    stateless_list_to_structure(Rest, UpdatedSocket, Index + 1, NewAcc).

%% Convert list items to dynamic data for hierarchical structure
-spec list_items_to_dynamic_data(DynamicTemplate, Items, Socket, Acc) -> {DynamicData, Socket1} when
    DynamicTemplate :: #{elems_order := [non_neg_integer()], elems := map(), vars_indexes := map()},
    Items :: [term()],
    Socket :: arizona_socket:socket(),
    Acc :: [#{element_index() => element_content()}],
    DynamicData :: [#{element_index() => element_content()}],
    Socket1 :: arizona_socket:socket().
list_items_to_dynamic_data(_DynamicTemplate, [], Socket, Acc) ->
    {lists:reverse(Acc), Socket};
list_items_to_dynamic_data(DynamicTemplate, [Item | Rest], Socket, Acc) ->
    % Process this item with the dynamic template
    {ItemStructure, UpdatedSocket} = process_list_item(DynamicTemplate, Item, Socket),
    list_items_to_dynamic_data(DynamicTemplate, Rest, UpdatedSocket, [ItemStructure | Acc]).

%% Process a single list item with the dynamic template
-spec process_list_item(DynamicTemplate, Item, Socket) -> {ItemStructure, Socket1} when
    DynamicTemplate :: #{elems_order := [non_neg_integer()], elems := map(), vars_indexes := map()},
    Item :: term(),
    Socket :: arizona_socket:socket(),
    ItemStructure :: #{element_index() => element_content()},
    Socket1 :: arizona_socket:socket().
process_list_item(#{elems_order := Order, elems := Elements}, Item, Socket) ->
    % Build structure directly without intermediate list
    {ItemStructure, UpdatedSocket} = build_item_structure_direct(
        Order, Elements, Item, Socket, #{}
    ),
    {ItemStructure, UpdatedSocket}.

%% Build item structure directly from order and elements, evaluating as we go
-spec build_item_structure_direct(Order, Elements, Item, Socket, Acc) ->
    {ItemStructure, Socket1}
when
    Order :: [non_neg_integer()],
    Elements :: map(),
    Item :: term(),
    Socket :: arizona_socket:socket(),
    Acc :: map(),
    ItemStructure :: map(),
    Socket1 :: arizona_socket:socket().
build_item_structure_direct([], _Elements, _Item, Socket, Acc) ->
    {Acc, Socket};
build_item_structure_direct([Index | RestIndexes], Elements, Item, Socket, Acc) ->
    % Evaluate this element using the extracted function
    {Value, UpdatedSocket} = arizona_renderer:evaluate_single_dynamic_element(
        Index, Elements, Item, Socket
    ),
    % Add to accumulator and continue
    build_item_structure_direct(RestIndexes, Elements, Item, UpdatedSocket, Acc#{Index => Value}).
