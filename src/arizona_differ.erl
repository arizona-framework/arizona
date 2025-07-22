-module(arizona_differ).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([diff_stateful/3]).
-export([diff_stateless/4]).

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-spec diff_stateful(Module, Bindings, View) -> {Diff, View1} when
    Module :: module(),
    Bindings :: arizona_binder:bindings(),
    View :: arizona_view:view(),
    Diff :: [dynamic()],
    View1 :: arizona_view:view().
diff_stateful(Module, Bindings, View) ->
    LivePid = arizona_view:get_live_pid(View),
    {Id, Template, PrepRenderView} = arizona_stateful:prepare_render(Module, Bindings, View),
    StatefulState = arizona_view:get_stateful_state(Id, PrepRenderView),
    ChangedBindings = arizona_stateful:get_changed_bindings(StatefulState),
    case arizona_binder:is_empty(ChangedBindings) of
        true ->
            % Clear dependencies for this component before starting new render
            ok = arizona_view:live_clear_stateful_dependencies(Id, PrepRenderView),
            NoChangesStatefulState = arizona_stateful:set_changed_bindings(
                arizona_binder:new(), StatefulState
            ),
            StatefulView = arizona_view:put_stateful_state(
                Id, NoChangesStatefulState, PrepRenderView
            ),
            {[], StatefulView};
        false when is_pid(LivePid) ->
            Tracker = arizona_live:get_dependency_tracker(LivePid),
            StatefulDependencies = arizona_tracker:get_stateful_dependencies(Id, Tracker),
            % Clear dependencies for this component before starting new render
            ok = arizona_view:live_clear_stateful_dependencies(Id, PrepRenderView),
            AffectedElements = get_affected_elements(ChangedBindings, StatefulDependencies),
            {Diff, DiffView} = generate_element_diff(AffectedElements, Template, PrepRenderView),
            NoChangesStatefulState = arizona_stateful:set_changed_bindings(
                arizona_binder:new(), StatefulState
            ),
            StatefulView = arizona_view:put_stateful_state(Id, NoChangesStatefulState, DiffView),
            {Diff, StatefulView}
    end.

-spec diff_stateless(Module, Function, Bindings, View) -> {Html, View1} when
    Module :: module(),
    Function :: atom(),
    Bindings :: arizona_binder:bindings(),
    View :: arizona_view:view(),
    Html :: arizona_html:html(),
    View1 :: arizona_view:view().
diff_stateless(Module, Fun, Bindings, View) ->
    % Stateless components don't have state tracking like stateful components
    % For now, we can implement a simple approach that re-renders the entire
    % stateless component when any changes occur, since stateless components
    % are typically small and don't benefit from fine-grained diffing

    % Re-render the entire stateless component
    Template = arizona_stateless:call_render_callback(Module, Fun, Bindings),

    % Convert template to HTML for diff
    {Html, RenderView} = arizona_renderer:render_template(Template, View),

    % Return the complete HTML as a change
    % In a more sophisticated implementation, we could track element-level
    % changes within stateless components, but for now this provides
    % a working diff mechanism
    {Html, RenderView}.

%% --------------------------------------------------------------------
%% Internal Functions
%% --------------------------------------------------------------------

%% Get affected elements from changed bindings and variable dependencies
get_affected_elements(ChangedBindings, StatefulDependencies) ->
    ChangedVarNames = arizona_binder:keys(ChangedBindings),
    AffectedIndexLists = [
        maps:get(VarName, StatefulDependencies, [])
     || VarName <- ChangedVarNames
    ],
    sets:from_list(lists:flatten(AffectedIndexLists), [{version, 2}]).

%% Generate diff for affected elements
generate_element_diff(AffectedElements, Template, View) ->
    case sets:size(AffectedElements) of
        0 ->
            {[], View};
        _ ->
            DynamicSequence = sets:to_list(AffectedElements),
            Dynamic = arizona_template:get_dynamic(Template),
            process_affected_elements(DynamicSequence, Dynamic, View)
    end.

%% Process affected elements to create diff changes
process_affected_elements([], _Dynamic, View) ->
    {[], View};
process_affected_elements([ElementIndex | T], Dynamic, View) ->
    ok = arizona_view:live_set_current_element_index(ElementIndex, View),
    DynamicCallback = element(ElementIndex, Dynamic),
    case DynamicCallback() of
        Callback when is_function(Callback, 1) ->
            {Html, CallbackView} = Callback(View),
            ElementChange = {ElementIndex, Html},
            {RestChanges, FinalView} = process_affected_elements(T, Dynamic, CallbackView),
            {[ElementChange | RestChanges], FinalView};
        Result ->
            Html = arizona_html:to_html(Result),
            ElementChange = {ElementIndex, Html},
            {RestChanges, FinalView} = process_affected_elements(T, Dynamic, View),
            {[ElementChange | RestChanges], FinalView}
    end.
