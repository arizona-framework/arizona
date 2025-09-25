-module(arizona_hierarchical).
-moduledoc ~"""
Hierarchical component structure generation for tracking and updates.

Generates structured representations of component hierarchies that enable
efficient tracking and differential updates. Used as fallback when templates
change and diff updates aren't possible.

## Structure Types

- `stateful_struct/0` - Stateful component with ID reference
- `stateless_struct/0` - Stateless component with static/dynamic parts
- `list_struct/0` - Collection rendering with item structures (lists and maps)

## Process

1. Generate hierarchical structures for all component types
2. Store template fingerprints for future diff comparisons
3. Track component relationships and element indices
4. Store stateful component data in hierarchical dictionary
5. Build nested component trees from dynamic callbacks

## Example

```erlang
1> {Struct, View1} = arizona_hierarchical:hierarchical_template(
..     Template, ParentId, Index, stateless, View
.. ).
{#{type => stateless, static => [...], dynamic => [...]}, UpdatedView}
```
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([hierarchical_view/1]).
-export([hierarchical_stateful/5]).
-export([hierarchical_stateless/6]).
-export([hierarchical_list/5]).
-export([hierarchical_map/5]).
-export([hierarchical_template/5]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([stateful_struct/0]).
-export_type([stateless_struct/0]).
-export_type([list_struct/0]).
-export_type([hierarchical_structure/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-nominal stateful_struct() :: #{
    type := stateful,
    id := arizona_stateful:id()
}.

-nominal stateless_struct() :: #{
    type := stateless,
    static := arizona_template:static(),
    dynamic := arizona_renderer:dynamic()
}.

-nominal list_struct() :: #{
    type := list,
    static := arizona_template:static(),
    dynamic := arizona_renderer:dynamic()
}.

-nominal hierarchical_structure() :: stateful_struct() | stateless_struct() | list_struct().

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc ~"""
Generates hierarchical structure for a view component.

Extracts the view's stateful ID and template, then creates a stateful
structure with component tracking and dependency management.
""".
-spec hierarchical_view(View) -> {Struct, View1} when
    View :: arizona_view:view(),
    Struct :: stateful_struct(),
    View1 :: arizona_view:view().
hierarchical_view(View) ->
    State = arizona_view:get_state(View),
    Id = arizona_stateful:get_binding(id, State),
    Template = arizona_view:call_render_callback(View),
    track_hierarchical_stateful(Id, Template, View).

-doc ~"""
Generates hierarchical structure for a stateful component.

Prepares the component for rendering, stores template fingerprint for
future diff comparisons, and creates hierarchical tracking structure.
""".
-spec hierarchical_stateful(Module, Bindings, ParentId, ElementIndex, View) -> {Struct, View1} when
    Module :: module(),
    Bindings :: arizona_binder:map(),
    ParentId :: arizona_stateful:id(),
    ElementIndex :: arizona_tracker:element_index(),
    View :: arizona_view:view(),
    Struct :: stateful_struct(),
    View1 :: arizona_view:view().
hierarchical_stateful(Module, Bindings, ParentId, ElementIndex, View) ->
    {Id, Template, PrepRenderView} = arizona_lifecycle:prepare_render(Module, Bindings, View),

    % Store the fingerprint for future comparisons
    Fingerprint = arizona_template:get_fingerprint(Template),
    FingerprintView = arizona_view:put_fingerprint(
        ParentId, ElementIndex, Fingerprint, {stateful, Id}, PrepRenderView
    ),

    Result = track_hierarchical_stateful(Id, Template, FingerprintView),
    _OldTracker = arizona_tracker_dict:set_current_stateful_id(ParentId),
    Result.

-doc ~"""
Generates hierarchical structure for a stateless component.

Calls the stateless component's render function to get a template,
then delegates to `hierarchical_template/5` for structure generation.
""".
-spec hierarchical_stateless(Module, Function, Bindings, ParentId, ElementIndex, View) ->
    {Struct, View1}
when
    Module :: module(),
    Function :: atom(),
    Bindings :: arizona_binder:map(),
    ParentId :: arizona_stateful:id(),
    ElementIndex :: arizona_tracker:element_index(),
    View :: arizona_view:view(),
    Struct :: stateless_struct(),
    View1 :: arizona_view:view().
hierarchical_stateless(Module, Fun, Bindings, ParentId, ElementIndex, View) ->
    Template = arizona_stateless:call_render_callback(Module, Fun, Bindings),
    hierarchical_template(Template, ParentId, ElementIndex, stateless, View).

-doc ~"""
Generates hierarchical structure for list rendering.

Stores template fingerprint and creates hierarchical structures for
each list item using the template's dynamic callback function.
""".
-spec hierarchical_list(Template, List, ParentId, ElementIndex, View) -> {Struct, View1} when
    Template :: arizona_template:template(),
    List :: [dynamic()],
    ParentId :: arizona_stateful:id(),
    ElementIndex :: arizona_tracker:element_index(),
    View :: arizona_view:view(),
    Struct :: list_struct(),
    View1 :: arizona_view:view().
hierarchical_list(Template, List, ParentId, ElementIndex, View) ->
    % Store the fingerprint for future comparisons
    Fingerprint = arizona_template:get_fingerprint(Template),
    FingerprintView = arizona_view:put_fingerprint(ParentId, ElementIndex, Fingerprint, list, View),

    DynamicSequence = arizona_template:get_dynamic_sequence(Template),
    DynamicCallback = arizona_template:get_dynamic(Template),
    DynamicRender = [
        render_callback_hierarchical(
            DynamicSequence, DynamicCallback, CallbackArg, ParentId, ElementIndex, View
        )
     || CallbackArg <- List
    ],
    Struct = #{
        type => list,
        static => arizona_template:get_static(Template),
        dynamic => DynamicRender
    },
    {Struct, FingerprintView}.

-doc ~"""
Generates hierarchical structure for map rendering.

Similar to list rendering but iterates over map entries as {Key, Value} tuples.
Returns the same structure type as lists since the rendering pattern is identical.
""".
-spec hierarchical_map(Template, Map, ParentId, ElementIndex, View) -> {Struct, View1} when
    Template :: arizona_template:template(),
    Map :: map(),
    ParentId :: arizona_stateful:id(),
    ElementIndex :: arizona_tracker:element_index(),
    View :: arizona_view:view(),
    Struct :: list_struct(),
    View1 :: arizona_view:view().
hierarchical_map(Template, Map, ParentId, ElementIndex, View) ->
    % Store the fingerprint for future comparisons
    Fingerprint = arizona_template:get_fingerprint(Template),
    FingerprintView = arizona_view:put_fingerprint(ParentId, ElementIndex, Fingerprint, list, View),

    DynamicSequence = arizona_template:get_dynamic_sequence(Template),
    DynamicCallback = arizona_template:get_dynamic(Template),
    DynamicRender = [
        render_callback_hierarchical(
            DynamicSequence, DynamicCallback, {Key, Value}, ParentId, ElementIndex, View
        )
     || Key := Value <- Map
    ],
    Struct = #{
        type => list,
        static => arizona_template:get_static(Template),
        dynamic => DynamicRender
    },
    {Struct, FingerprintView}.

-doc ~"""
Generates hierarchical structure for a template.

Stores template fingerprint and processes dynamic callbacks to create
a stateless structure with static parts and hierarchical dynamic elements.
""".
-spec hierarchical_template(Template, ParentId, ElementIndex, ComponentType, View) ->
    {Struct, View1}
when
    Template :: arizona_template:template(),
    ParentId :: arizona_stateful:id(),
    ElementIndex :: arizona_tracker:element_index(),
    ComponentType :: arizona_view:component_type(),
    View :: arizona_view:view(),
    Struct :: stateless_struct(),
    View1 :: arizona_view:view().
hierarchical_template(Template, ParentId, ElementIndex, ComponentType, View) ->
    % Store the fingerprint for future comparisons
    Fingerprint = arizona_template:get_fingerprint(Template),
    FingerprintView = arizona_view:put_fingerprint(
        ParentId, ElementIndex, Fingerprint, ComponentType, View
    ),

    DynamicSequence = arizona_template:get_dynamic_sequence(Template),
    Dynamic = arizona_template:get_dynamic(Template),
    {DynamicRender, DynamicView} = hierarchical_dynamic(
        DynamicSequence, Dynamic, ParentId, ElementIndex, FingerprintView
    ),
    Struct = #{
        type => stateless,
        static => arizona_template:get_static(Template),
        dynamic => DynamicRender
    },
    {Struct, DynamicView}.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

track_hierarchical_stateful(Id, Template, View) ->
    _OldTracker = arizona_tracker_dict:clear_stateful_dependencies(Id),
    _ClearTracker = arizona_tracker_dict:set_current_stateful_id(Id),
    DynamicSequence = arizona_template:get_dynamic_sequence(Template),
    Dynamic = arizona_template:get_dynamic(Template),
    {DynamicRender, DynamicView} = hierarchical_dynamic(
        DynamicSequence, Dynamic, Id, undefined, View
    ),
    HierarchicalData = #{
        static => arizona_template:get_static(Template),
        dynamic => DynamicRender
    },
    _HierarchicalStructure = arizona_hierarchical_dict:put_stateful_data(Id, HierarchicalData),
    Struct = #{
        type => stateful,
        id => Id
    },
    {Struct, DynamicView}.

-spec hierarchical_dynamic(Sequence, Dynamic, ParentId, ElementIndex, View) -> {Render, View1} when
    Sequence :: arizona_template:dynamic_sequence(),
    Dynamic :: arizona_template:dynamic(),
    ParentId :: arizona_stateful:id(),
    ElementIndex :: arizona_tracker:element_index() | undefined,
    View :: arizona_view:view(),
    Render :: dynamic(),
    View1 :: arizona_view:view().
hierarchical_dynamic([], _Dynamic, _ParentId, _ElementIndex, View) ->
    {[], View};
hierarchical_dynamic(
    [DynamicElementIndex | T], Dynamic, ParentId, ElementIndex, View
) ->
    DynamicCallback = element(DynamicElementIndex, Dynamic),
    ok = maybe_set_tracker_index(ElementIndex, DynamicElementIndex),
    CallbackResult = DynamicCallback(),
    process_hierarchical_callback(
        CallbackResult, DynamicElementIndex, T, Dynamic, ParentId, ElementIndex, View
    ).

%% Helper function to set tracker index when needed
maybe_set_tracker_index(undefined, DynamicElementIndex) ->
    _OldTracker = arizona_tracker_dict:set_current_element_index(DynamicElementIndex),
    ok;
maybe_set_tracker_index(_ElementIndex, _DynamicElementIndex) ->
    ok.

%% Helper function to render list hierarchical elements
render_callback_hierarchical(
    DynamicSequence, DynamicCallback, CallbackArg, ParentId, ElementIndex, View
) ->
    Dynamic = DynamicCallback(CallbackArg),
    {DynamicHierarchical, _UpdatedView} = hierarchical_dynamic(
        DynamicSequence, Dynamic, ParentId, ElementIndex, View
    ),
    DynamicHierarchical.

%% Helper function to process hierarchical callback results
process_hierarchical_callback(
    Callback, DynamicElementIndex, T, Dynamic, ParentId, ElementIndex, View
) when is_function(Callback, 4) ->
    {Struct, CallbackView} = Callback(hierarchical, ParentId, DynamicElementIndex, View),
    {RestHtml, FinalView} = hierarchical_dynamic(T, Dynamic, ParentId, ElementIndex, CallbackView),
    {[Struct | RestHtml], FinalView};
process_hierarchical_callback(
    Result, _DynamicElementIndex, T, Dynamic, ParentId, ElementIndex, View
) ->
    case arizona_template:is_template(Result) of
        true ->
            process_hierarchical_template(Result, T, Dynamic, ParentId, ElementIndex, View);
        false ->
            process_hierarchical_html(Result, T, Dynamic, ParentId, ElementIndex, View)
    end.

%% Helper function to process hierarchical template results
process_hierarchical_template(Result, T, Dynamic, ParentId, ElementIndex, View) ->
    {Struct, TemplateView} = hierarchical_template(Result, ParentId, ElementIndex, stateless, View),
    {RestHtml, FinalView} = hierarchical_dynamic(T, Dynamic, ParentId, ElementIndex, TemplateView),
    {[Struct | RestHtml], FinalView}.

%% Helper function to process hierarchical HTML results
process_hierarchical_html(Result, T, Dynamic, ParentId, ElementIndex, View) ->
    Html = arizona_html:to_html(Result),
    {RestHtml, FinalView} = hierarchical_dynamic(T, Dynamic, ParentId, ElementIndex, View),
    {[Html | RestHtml], FinalView}.
