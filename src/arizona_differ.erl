-module(arizona_differ).
%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([diff_view/1]).
-export([diff_stateful/4]).
-export([diff_stateless/6]).
-export([diff_list/5]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([diff/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-nominal diff() :: [{ElementIndex :: arizona_tracker:element_index(), Html :: arizona_html:html()}].

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
    {Diff, DiffState, DiffView} = track_diff_stateful(Id, Template, StatefulState, undefined, View),
    {Diff, arizona_view:update_state(DiffState, DiffView)}.

-spec diff_stateful(Module, Bindings, ElementIndex, View) -> {Result, View1} when
    Module :: module(),
    Bindings :: arizona_binder:bindings(),
    ElementIndex :: arizona_tracker:element_index(),
    View :: arizona_view:view(),
    Result :: diff() | arizona_hierarchical:stateful_struct(),
    View1 :: arizona_view:view().
diff_stateful(Module, Bindings, ElementIndex, View) ->
    {Id, Template, PrepRenderView} = arizona_lifecycle:prepare_render(Module, Bindings, View),
    Fingerprint = arizona_template:get_fingerprint(Template),
    case arizona_view:fingerprint_matches(Id, ElementIndex, Fingerprint, PrepRenderView) of
        true ->
            StatefulState = arizona_view:get_stateful_state(Id, PrepRenderView),
            {Diff, DiffState, DiffView} = track_diff_stateful(
                Id, Template, StatefulState, ElementIndex, PrepRenderView
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
                Module, Bindings, ElementIndex, PrepRenderView
            )
    end.

-spec diff_stateless(Module, Function, Bindings, ParentId, ElementIndex, View) ->
    {Result, View1}
when
    Module :: module(),
    Function :: atom(),
    Bindings :: arizona_binder:bindings(),
    ParentId :: arizona_stateful:id(),
    ElementIndex :: arizona_tracker:element_index(),
    View :: arizona_view:view(),
    Result :: diff() | arizona_hierarchical:stateless_struct(),
    View1 :: arizona_view:view().
diff_stateless(Module, Function, Bindings, ParentId, ElementIndex, View) ->
    Template = arizona_stateless:call_render_callback(Module, Function, Bindings),
    Fingerprint = arizona_template:get_fingerprint(Template),
    case arizona_view:fingerprint_matches(ParentId, ElementIndex, Fingerprint, View) of
        true ->
            DynamicSequence = arizona_template:get_dynamic_sequence(Template),
            Dynamic = arizona_template:get_dynamic(Template),
            case
                process_affected_elements(
                    DynamicSequence, Dynamic, ok, ParentId, ElementIndex, View
                )
            of
                {[], RenderView} ->
                    {nodiff, RenderView};
                {Diff, RenderView} ->
                    {Diff, RenderView}
            end;
        false ->
            arizona_hierarchical:hierarchical_stateless(
                Module, Function, Bindings, ParentId, ElementIndex, View
            )
    end.

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
            Dynamic = arizona_template:get_dynamic(Template),
            Diff =
                [
                    begin
                        {Html, _UpdatedView} = arizona_renderer:render_dynamic(
                            DynamicSequence, Dynamic, CallbackArg, ParentId, View
                        ),
                        Html
                    end
                 || CallbackArg <- List
                ],
            {Diff, View};
        false ->
            arizona_hierarchical:hierarchical_list(Template, List, ParentId, ElementIndex, View)
    end.

%% --------------------------------------------------------------------
%% Internal Functions
%% --------------------------------------------------------------------

track_diff_stateful(Id, Template, StatefulState, ElementIndex, View) ->
    _OldTracker = arizona_tracker_dict:set_current_stateful_id(Id),
    ChangedBindings = arizona_stateful:get_changed_bindings(StatefulState),
    case arizona_binder:is_empty(ChangedBindings) of
        true ->
            NoChangesStatefulState = arizona_stateful:set_changed_bindings(
                arizona_binder:new(#{}), StatefulState
            ),
            {[], NoChangesStatefulState, View};
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
                AffectedElements, Template, ok, Id, ElementIndex, View
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
generate_element_diff(DynamicSequence, Template, CallbackArg, ParentId, ElementIndex, View) ->
    case DynamicSequence of
        [] ->
            {[], View};
        _ ->
            Dynamic = arizona_template:get_dynamic(Template),
            process_affected_elements(
                DynamicSequence, Dynamic, CallbackArg, ParentId, ElementIndex, View
            )
    end.

%% Process affected elements to create diff changes
%% % TODO: Check if we need ElementIndex
process_affected_elements([], _Dynamic, _CallbackArg, _ParentId, _ElementIndex, View) ->
    {[], View};
process_affected_elements(
    [DynamicElementIndex | T], Dynamic, CallbackArg, ParentId, ElementIndex, View
) ->
    DynamicCallback = element(DynamicElementIndex, Dynamic),
    _OldTracker =
        case ElementIndex of
            undefined ->
                arizona_tracker_dict:set_current_element_index(DynamicElementIndex);
            _ ->
                ok
        end,
    case DynamicCallback(CallbackArg) of
        Callback when is_function(Callback, 4) ->
            {Diff, CallbackView} = Callback(diff, ParentId, DynamicElementIndex, View),
            {RestChanges, FinalView} = process_affected_elements(
                T, Dynamic, CallbackArg, ParentId, ElementIndex, CallbackView
            ),
            case Diff of
                nodiff ->
                    {RestChanges, FinalView};
                _ ->
                    ElementChange = {DynamicElementIndex, Diff},
                    {[ElementChange | RestChanges], FinalView}
            end;
        Result ->
            case arizona_template:is_template(Result) of
                true ->
                    {Diff, TemplateView} = render_template(
                        Result, CallbackArg, ParentId, ElementIndex, View
                    ),
                    {RestChanges, FinalView} = process_affected_elements(
                        T, Dynamic, CallbackArg, ParentId, ElementIndex, TemplateView
                    ),
                    case Diff of
                        nodiff ->
                            {RestChanges, FinalView};
                        _ ->
                            ElementChange = {DynamicElementIndex, Diff},
                            {[ElementChange | RestChanges], FinalView}
                    end;
                false ->
                    Html = arizona_html:to_html(Result),
                    ElementChange = {DynamicElementIndex, Html},
                    % Remove fingerprint since this is not a template (pure HTML/string)
                    CleanView = arizona_view:remove_fingerprint(
                        ParentId, DynamicElementIndex, View
                    ),
                    {RestChanges, FinalView} = process_affected_elements(
                        T, Dynamic, CallbackArg, ParentId, ElementIndex, CleanView
                    ),
                    {[ElementChange | RestChanges], FinalView}
            end
    end.

render_template(Template, CallbackArg, ParentId, ElementIndex, View) ->
    DynamicSequence = arizona_template:get_dynamic_sequence(Template),
    Dynamic = arizona_template:get_dynamic(Template),
    process_affected_elements(DynamicSequence, Dynamic, CallbackArg, ParentId, ElementIndex, View).
