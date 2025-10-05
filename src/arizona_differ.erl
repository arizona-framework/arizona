-module(arizona_differ).
-moduledoc ~"""
Differential update engine for real-time WebSocket updates.

Generates minimal diff patches by comparing current component state with
previous renders, enabling efficient DOM updates in the browser via WebSocket.
Only changed dynamic parts are included in the diff.

## Diff Process

1. Check template fingerprints to detect structural changes
2. Track variable dependencies to identify affected elements
3. Generate diffs only for changed dynamic parts
4. Fall back to full hierarchical rendering when templates change

## Diff Types

- `nodiff` - No changes detected, no update needed
- `diff/0` - List of element index and updated content pairs
- `arizona_hierarchical:hierarchical_structure/0` - Full re-render when template changed

## Example

```erlang
1> {Diff, View1} = arizona_differ:diff_view(View).
{[{2, ~"New Content"}], UpdatedView}  % Only element 2 changed
```
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([diff_view/1]).
-export([diff_stateful/5]).
-export([diff_root_stateful/3]).
-export([diff_stateless/6]).
-export([diff_list/5]).
-export([diff_map/5]).
-export([diff_template/5]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([diff/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-nominal diff() :: [
    {
        arizona_tracker:element_index(),
        diff() | arizona_hierarchical:hierarchical_structure() | arizona_html:html()
    }
].

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc ~"""
Generates differential updates for a view component.

Extracts the view's stateful state, calls the render callback to get
a template, and generates a diff based on changed variable bindings.
""".
-spec diff_view(View) -> {Diff, View1} when
    View :: arizona_view:view(),
    Diff :: diff(),
    View1 :: arizona_view:view().
diff_view(View) ->
    StatefulState = arizona_view:get_state(View),
    Template = arizona_view:call_render_callback(View),
    Id = arizona_stateful:get_binding(id, StatefulState),
    {Diff, DiffState, DiffView} = track_diff_stateful(Id, Template, StatefulState, View),
    {Diff, arizona_view:update_state(DiffState, DiffView)}.

-doc ~"""
Generates differential updates for a stateful component.

Checks template fingerprint for structural changes. If template matches,
generates a diff of changed elements. If fingerprint changed, falls back
to full hierarchical rendering.
""".
-spec diff_stateful(Module, Bindings, ParentId, ElementIndex, View) -> {Result, View1} when
    Module :: module(),
    Bindings :: map(),
    ParentId :: arizona_stateful:id(),
    ElementIndex :: arizona_tracker:element_index(),
    View :: arizona_view:view(),
    Result :: nodiff | diff() | arizona_hierarchical:stateful_struct(),
    View1 :: arizona_view:view().
diff_stateful(Module, Bindings, ParentId, ElementIndex, View) ->
    {Id, Template, PrepRenderView} = arizona_lifecycle:prepare_render(
        Module, Bindings, View, preserve_state
    ),
    Fingerprint = arizona_template:get_fingerprint(Template),
    MatchResult = arizona_view:fingerprint_matches(
        ParentId, ElementIndex, Fingerprint, PrepRenderView
    ),
    case MatchResult of
        true ->
            StatefulState = arizona_view:get_stateful_state(Id, PrepRenderView),
            {Diff, DiffState, DiffView} = track_diff_stateful(
                Id, Template, StatefulState, PrepRenderView
            ),
            _OldTracker = arizona_tracker_dict:set_current_stateful_id(ParentId),
            StatefulView = arizona_view:put_stateful_state(Id, DiffState, DiffView),
            case Diff of
                [] ->
                    {nodiff, StatefulView};
                _ ->
                    {Diff, StatefulView}
            end;
        _Other ->
            ok = unmount_if_stateful_component(MatchResult, PrepRenderView),
            arizona_hierarchical:hierarchical_stateful(
                Module, Bindings, ParentId, ElementIndex, PrepRenderView
            )
    end.

-doc ~"""
Generates differential updates for a root stateful component.

Similar to `diff_stateful/5` but used for root-level components without
a parent context. Always generates a diff without fingerprint checking.
""".
-spec diff_root_stateful(Module, Bindings, View) -> {Diff, View1} when
    Module :: module(),
    Bindings :: map(),
    View :: arizona_view:view(),
    Diff :: diff(),
    View1 :: arizona_view:view().
diff_root_stateful(Module, Bindings, View) ->
    {Id, Template, PrepRenderView} = arizona_lifecycle:prepare_render(Module, Bindings, View),
    StatefulState = arizona_view:get_stateful_state(Id, PrepRenderView),
    {Diff, DiffState, DiffView} = track_diff_stateful(Id, Template, StatefulState, PrepRenderView),
    StatefulView = arizona_view:put_stateful_state(Id, DiffState, DiffView),
    {Diff, StatefulView}.

-doc ~"""
Generates differential updates for a stateless component.

Calls the stateless component's render function to get a template,
then delegates to `diff_template/5` for the actual diff generation.
""".
-spec diff_stateless(Module, Function, Bindings, ParentId, ElementIndex, View) ->
    {Result, View1}
when
    Module :: module(),
    Function :: atom(),
    Bindings :: map(),
    ParentId :: arizona_stateful:id(),
    ElementIndex :: arizona_tracker:element_index(),
    View :: arizona_view:view(),
    Result :: nodiff | diff() | arizona_hierarchical:stateless_struct(),
    View1 :: arizona_view:view().
diff_stateless(Module, Function, Bindings, ParentId, ElementIndex, View) ->
    Template = arizona_stateless:call_render_callback(Module, Function, Bindings),
    diff_template(Template, ParentId, ElementIndex, stateless, View).

-doc ~"""
Generates differential updates for list rendering.

Checks template fingerprint and renders each list item with the template's
dynamic callback. Falls back to `t:arizona_hierarchical:list_struct/0` rendering
if template changed.
""".
-spec diff_list(Template, List, ParentId, ElementIndex, View) -> {Result, View1} when
    Template :: arizona_template:template(),
    List :: [dynamic()],
    ParentId :: arizona_stateful:id(),
    ElementIndex :: arizona_tracker:element_index(),
    View :: arizona_view:view(),
    Result :: diff() | arizona_hierarchical:list_struct(),
    View1 :: arizona_view:view().
diff_list(Template, List, ParentId, ElementIndex, View) ->
    Fingerprint = arizona_template:get_fingerprint(Template),
    MatchResult = arizona_view:fingerprint_matches(ParentId, ElementIndex, Fingerprint, View),
    case MatchResult of
        true ->
            DynamicSequence = arizona_template:get_dynamic_sequence(Template),
            DynamicCallback = arizona_template:get_dynamic(Template),
            Diff = [
                render_callback_dynamic(
                    DynamicSequence, DynamicCallback, CallbackArg, ParentId, View
                )
             || CallbackArg <- List
            ],
            {Diff, View};
        _Other ->
            ok = unmount_if_stateful_component(MatchResult, View),
            arizona_hierarchical:hierarchical_list(Template, List, ParentId, ElementIndex, View)
    end.

-doc ~"""
Generates differential updates for a map template.

Compares current map rendering with previous state to generate minimal
diff patches for changed map elements. Each map entry is rendered as
a {Key, Value} tuple to the callback function.

Returns `t:diff/0` if fingerprints match and elements can be diffed,
otherwise falls back to `t:arizona_hierarchical:list_struct/0`.
""".
-spec diff_map(Template, Map, ParentId, ElementIndex, View) -> {Result, View1} when
    Template :: arizona_template:template(),
    Map :: map(),
    ParentId :: arizona_stateful:id(),
    ElementIndex :: arizona_tracker:element_index(),
    View :: arizona_view:view(),
    Result :: diff() | arizona_hierarchical:list_struct(),
    View1 :: arizona_view:view().
diff_map(Template, Map, ParentId, ElementIndex, View) ->
    Fingerprint = arizona_template:get_fingerprint(Template),
    MatchResult = arizona_view:fingerprint_matches(ParentId, ElementIndex, Fingerprint, View),
    case MatchResult of
        true ->
            DynamicSequence = arizona_template:get_dynamic_sequence(Template),
            DynamicCallback = arizona_template:get_dynamic(Template),
            Diff = [
                render_callback_dynamic(
                    DynamicSequence, DynamicCallback, {Key, Value}, ParentId, View
                )
             || Key := Value <- Map
            ],
            {Diff, View};
        _Other ->
            ok = unmount_if_stateful_component(MatchResult, View),
            arizona_hierarchical:hierarchical_map(Template, Map, ParentId, ElementIndex, View)
    end.

-doc ~"""
Generates differential updates for a template.

Checks template fingerprint and processes affected dynamic elements.
Returns `nodiff` if no elements changed, `diff/0` if elements updated,
or `t:arizona_hierarchical:stateless_struct/0` if template changed.
""".
-spec diff_template(Template, ParentId, ElementIndex, ComponentType, View) -> {Result, View1} when
    Template :: arizona_template:template(),
    ParentId :: arizona_stateful:id(),
    ElementIndex :: arizona_tracker:element_index(),
    ComponentType :: arizona_view:component_type(),
    View :: arizona_view:view(),
    Result :: nodiff | diff() | arizona_hierarchical:stateless_struct(),
    View1 :: arizona_view:view().
diff_template(Template, ParentId, ElementIndex, ComponentType, View) ->
    Fingerprint = arizona_template:get_fingerprint(Template),
    MatchResult = arizona_view:fingerprint_matches(ParentId, ElementIndex, Fingerprint, View),
    case MatchResult of
        true ->
            DynamicSequence = arizona_template:get_dynamic_sequence(Template),
            Dynamic = arizona_template:get_dynamic(Template),
            case
                process_affected_elements(DynamicSequence, Dynamic, ParentId, ElementIndex, View)
            of
                {[], RenderView} ->
                    {nodiff, RenderView};
                {Diff, RenderView} ->
                    {Diff, RenderView}
            end;
        _Other ->
            ok = unmount_if_stateful_component(MatchResult, View),
            arizona_hierarchical:hierarchical_template(
                Template, ParentId, ElementIndex, ComponentType, View
            )
    end.

%% --------------------------------------------------------------------
%% Internal Functions
%% --------------------------------------------------------------------

track_diff_stateful(Id, Template, StatefulState, View) ->
    Tracker = arizona_tracker_dict:set_current_stateful_id(Id),
    ChangedBindings = arizona_stateful:get_changed_bindings(StatefulState),
    case map_size(ChangedBindings) of
        0 ->
            {[], StatefulState, View};
        _ChangedSize ->
            StatefulDependencies = arizona_tracker:get_stateful_dependencies(Id, Tracker),
            % Clear dependencies only for changed variables
            ChangedVarNames = maps:keys(ChangedBindings),
            _NewOldTracker = arizona_tracker_dict:clear_changed_variable_dependencies(
                Id, ChangedVarNames
            ),
            AffectedElements = get_affected_elements(ChangedBindings, StatefulDependencies),
            {Diff, DiffView} = generate_element_diff(
                AffectedElements, Template, Id, undefined, View
            ),
            NoChangesStatefulState = arizona_stateful:set_changed_bindings(
                #{}, StatefulState
            ),
            {Diff, NoChangesStatefulState, DiffView}
    end.

%% Get affected elements from changed bindings and variable dependencies
get_affected_elements(ChangedBindings, StatefulDependencies) ->
    ChangedVarNames = maps:keys(ChangedBindings),
    AffectedIndexLists = [
        maps:get(VarName, StatefulDependencies, [])
     || VarName <- ChangedVarNames
    ],
    lists:usort(lists:flatten(AffectedIndexLists)).

%% Generate diff for affected elements
generate_element_diff(DynamicSequence, Template, ParentId, ElementIndex, View) ->
    case DynamicSequence of
        [] ->
            {[], View};
        _ ->
            Dynamic = arizona_template:get_dynamic(Template),
            process_affected_elements(DynamicSequence, Dynamic, ParentId, ElementIndex, View)
    end.

%% Process affected elements to create diff changes
process_affected_elements([], _Dynamic, _ParentId, _ElementIndex, View) ->
    {[], View};
process_affected_elements(
    [DynamicElementIndex | T], Dynamic, ParentId, ElementIndex, View
) ->
    DynamicCallback = element(DynamicElementIndex, Dynamic),
    ok = maybe_set_tracker_index(ElementIndex, DynamicElementIndex),
    CallbackResult = DynamicCallback(),
    process_callback_result(
        CallbackResult, DynamicElementIndex, T, Dynamic, ParentId, ElementIndex, View
    ).

%% Helper function to set tracker index when needed
maybe_set_tracker_index(undefined, DynamicElementIndex) ->
    _OldTracker = arizona_tracker_dict:set_current_element_index(DynamicElementIndex),
    ok;
maybe_set_tracker_index(_ElementIndex, _DynamicElementIndex) ->
    ok.

%% Helper function to process callback results
process_callback_result(
    Callback, DynamicElementIndex, T, Dynamic, ParentId, ElementIndex, View
) when is_function(Callback, 4) ->
    {Diff, CallbackView} = Callback(diff, ParentId, DynamicElementIndex, View),
    {RestChanges, FinalView} = process_affected_elements(
        T, Dynamic, ParentId, ElementIndex, CallbackView
    ),
    process_diff_result(Diff, DynamicElementIndex, RestChanges, FinalView);
process_callback_result(Result, DynamicElementIndex, T, Dynamic, ParentId, ElementIndex, View) ->
    case arizona_template:is_template(Result) of
        true ->
            process_template_result(
                Result, DynamicElementIndex, T, Dynamic, ParentId, ElementIndex, View
            );
        false ->
            process_html_result(
                Result, DynamicElementIndex, T, Dynamic, ParentId, ElementIndex, View
            )
    end.

%% Helper function to process template results
process_template_result(Result, DynamicElementIndex, T, Dynamic, ParentId, ElementIndex, View) ->
    {Diff, TemplateView} = diff_template(Result, ParentId, ElementIndex, stateless, View),
    {RestChanges, FinalView} = process_affected_elements(
        T, Dynamic, ParentId, ElementIndex, TemplateView
    ),
    process_diff_result(Diff, DynamicElementIndex, RestChanges, FinalView).

%% Helper function to process HTML results
process_html_result(Result, DynamicElementIndex, T, Dynamic, ParentId, ElementIndex, View) ->
    Html = arizona_html:to_html(Result),
    ElementChange = {DynamicElementIndex, Html},
    CleanView = arizona_view:remove_fingerprint(ParentId, DynamicElementIndex, View),
    {RestChanges, FinalView} = process_affected_elements(
        T, Dynamic, ParentId, ElementIndex, CleanView
    ),
    {[ElementChange | RestChanges], FinalView}.

%% Helper function to render dynamic elements from callback functions
render_callback_dynamic(DynamicSequence, DynamicCallback, CallbackArg, ParentId, View) ->
    Dynamic = DynamicCallback(CallbackArg),
    {Html, _UpdatedView} = arizona_renderer:render_dynamic(
        DynamicSequence, Dynamic, ParentId, View
    ),
    Html.

%% Helper function to process diff results
process_diff_result(nodiff, _DynamicElementIndex, RestChanges, FinalView) ->
    {RestChanges, FinalView};
process_diff_result(Diff, DynamicElementIndex, RestChanges, FinalView) ->
    ElementChange = {DynamicElementIndex, Diff},
    {[ElementChange | RestChanges], FinalView}.

% Unmounts stateful components when they are being replaced or removed.
% When a stateful component's fingerprint doesn't match (component is being replaced
% or removed), this function ensures proper cleanup by calling the component's unmount
% callback if it exists. Non-stateful components require no cleanup.
unmount_if_stateful_component({false, {stateful, StatefulId}}, View) ->
    case arizona_view:find_stateful_state(StatefulId, View) of
        {ok, StatefulState} ->
            arizona_stateful:call_unmount_callback(StatefulState);
        error ->
            % Component may have already been cleaned up
            ok
    end;
unmount_if_stateful_component({false, _NonStatefulComponentType}, _View) ->
    % Non-stateful components don't need cleanup
    ok;
unmount_if_stateful_component(none, _View) ->
    % No previous component to clean up
    ok.
