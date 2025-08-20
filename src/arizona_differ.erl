-module(arizona_differ).
%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([diff_view/1]).
-export([diff_stateful/5]).
-export([diff_root_stateful/3]).
-export([diff_stateless/6]).
-export([diff_list/5]).
-export([diff_template/4]).

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

-spec diff_stateful(Module, Bindings, ParentId, ElementIndex, View) -> {Result, View1} when
    Module :: module(),
    Bindings :: arizona_binder:map(),
    ParentId :: arizona_stateful:id(),
    ElementIndex :: arizona_tracker:element_index(),
    View :: arizona_view:view(),
    Result :: nodiff | diff() | arizona_hierarchical:stateful_struct(),
    View1 :: arizona_view:view().
diff_stateful(Module, Bindings, ParentId, ElementIndex, View) ->
    {Id, Template, PrepRenderView} = arizona_lifecycle:prepare_render(Module, Bindings, View),
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
            StatefulView = arizona_view:put_stateful_state(Id, DiffState, DiffView),
            case Diff of
                [] ->
                    {nodiff, StatefulView};
                _ ->
                    {Diff, StatefulView}
            end;
        false ->
            arizona_hierarchical:hierarchical_stateful(
                Module, Bindings, ParentId, ElementIndex, PrepRenderView
            )
    end.

-spec diff_root_stateful(Module, Bindings, View) -> {Diff, View1} when
    Module :: module(),
    Bindings :: arizona_binder:map(),
    View :: arizona_view:view(),
    Diff :: diff(),
    View1 :: arizona_view:view().
diff_root_stateful(Module, Bindings, View) ->
    {Id, Template, PrepRenderView} = arizona_lifecycle:prepare_render(Module, Bindings, View),
    StatefulState = arizona_view:get_stateful_state(Id, PrepRenderView),
    {Diff, DiffState, DiffView} = track_diff_stateful(Id, Template, StatefulState, PrepRenderView),
    StatefulView = arizona_view:put_stateful_state(Id, DiffState, DiffView),
    {Diff, StatefulView}.

-spec diff_stateless(Module, Function, Bindings, ParentId, ElementIndex, View) ->
    {Result, View1}
when
    Module :: module(),
    Function :: atom(),
    Bindings :: arizona_binder:map(),
    ParentId :: arizona_stateful:id(),
    ElementIndex :: arizona_tracker:element_index(),
    View :: arizona_view:view(),
    Result :: nodiff | diff() | arizona_hierarchical:stateless_struct(),
    View1 :: arizona_view:view().
diff_stateless(Module, Function, Bindings, ParentId, ElementIndex, View) ->
    Template = arizona_stateless:call_render_callback(Module, Function, Bindings),
    diff_template(Template, ParentId, ElementIndex, View).

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
    case arizona_view:fingerprint_matches(ParentId, ElementIndex, Fingerprint, View) of
        true ->
            DynamicSequence = arizona_template:get_dynamic_sequence(Template),
            DynamicCallback = arizona_template:get_dynamic(Template),
            Diff = [
                render_list_dynamic(DynamicSequence, DynamicCallback, CallbackArg, ParentId, View)
             || CallbackArg <- List
            ],
            {Diff, View};
        false ->
            arizona_hierarchical:hierarchical_list(Template, List, ParentId, ElementIndex, View)
    end.

-spec diff_template(Template, ParentId, ElementIndex, View) -> {Result, View1} when
    Template :: arizona_template:template(),
    ParentId :: arizona_stateful:id(),
    ElementIndex :: arizona_tracker:element_index(),
    View :: arizona_view:view(),
    Result :: nodiff | diff() | arizona_hierarchical:stateless_struct(),
    View1 :: arizona_view:view().
diff_template(Template, ParentId, ElementIndex, View) ->
    Fingerprint = arizona_template:get_fingerprint(Template),
    case arizona_view:fingerprint_matches(ParentId, ElementIndex, Fingerprint, View) of
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
        false ->
            arizona_hierarchical:hierarchical_template(Template, ParentId, ElementIndex, View)
    end.

%% --------------------------------------------------------------------
%% Internal Functions
%% --------------------------------------------------------------------

track_diff_stateful(Id, Template, StatefulState, View) ->
    _OldTracker = arizona_tracker_dict:set_current_stateful_id(Id),
    ChangedBindings = arizona_stateful:get_changed_bindings(StatefulState),
    case arizona_binder:is_empty(ChangedBindings) of
        true ->
            {[], StatefulState, View};
        false ->
            Tracker = arizona_tracker_dict:get_tracker(),
            StatefulDependencies = arizona_tracker:get_stateful_dependencies(Id, Tracker),
            % Clear dependencies only for changed variables
            ChangedVarNames = arizona_binder:keys(ChangedBindings),
            _NewOldTracker = arizona_tracker_dict:clear_changed_variable_dependencies(
                Id, ChangedVarNames
            ),
            AffectedElements = get_affected_elements(ChangedBindings, StatefulDependencies),
            {Diff, DiffView} = generate_element_diff(
                AffectedElements, Template, Id, undefined, View
            ),
            NoChangesStatefulState = arizona_stateful:set_changed_bindings(
                arizona_binder:new(#{}), StatefulState
            ),
            {Diff, NoChangesStatefulState, DiffView}
    end.

%% Get affected elements from changed bindings and variable dependencies
get_affected_elements(ChangedBindings, StatefulDependencies) ->
    ChangedVarNames = arizona_binder:keys(ChangedBindings),
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
    {Diff, TemplateView} = diff_template(Result, ParentId, ElementIndex, View),
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

%% Helper function to render list dynamic elements
render_list_dynamic(DynamicSequence, DynamicCallback, CallbackArg, ParentId, View) ->
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
