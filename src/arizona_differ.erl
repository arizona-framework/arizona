-module(arizona_differ).
%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([diff_view/1]).
-export([diff_stateful/3]).
-export([diff_stateless/4]).

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
    {Diff, DiffState, DiffView} = track_diff_stateful(Id, Template, StatefulState, View),
    {Diff, arizona_view:update_state(DiffState, DiffView)}.

-spec diff_stateful(Module, Bindings, View) -> {Diff, View1} when
    Module :: module(),
    Bindings :: arizona_binder:bindings(),
    View :: arizona_view:view(),
    Diff :: diff(),
    View1 :: arizona_view:view().
diff_stateful(Module, Bindings, View) ->
    {Id, Template, PrepRenderView} = arizona_lifecycle:prepare_render(Module, Bindings, View),
    StatefulState = arizona_view:get_stateful_state(Id, PrepRenderView),
    {Diff, DiffState, DiffView} = track_diff_stateful(Id, Template, StatefulState, PrepRenderView),
    StatefulView = arizona_view:put_stateful_state(Id, DiffState, DiffView),
    {Diff, StatefulView}.

-spec diff_stateless(Module, Function, Bindings, View) -> {Diff, View1} when
    Module :: module(),
    Function :: atom(),
    Bindings :: arizona_binder:bindings(),
    View :: arizona_view:view(),
    Diff :: diff(),
    View1 :: arizona_view:view().
diff_stateless(Module, Fun, Bindings, View) ->
    Template = arizona_stateless:call_render_callback(Module, Fun, Bindings),
    DynamicSequence = arizona_template:get_dynamic_sequence(Template),
    Dynamic = arizona_template:get_dynamic(Template),
    {Diff, RenderView} = process_affected_elements(DynamicSequence, Dynamic, ok, View),
    {Diff, RenderView}.

%% --------------------------------------------------------------------
%% Internal Functions
%% --------------------------------------------------------------------

track_diff_stateful(Id, Template, StatefulState, View) ->
    Tracker = arizona_tracker_dict:set_current_stateful_id(Id),
    ChangedBindings = arizona_stateful:get_changed_bindings(StatefulState),
    case arizona_binder:is_empty(ChangedBindings) of
        true ->
            % Clear dependencies for this component before starting new render
            _ClearTracker = arizona_tracker_dict:clear_stateful_dependencies(Id),
            NoChangesStatefulState = arizona_stateful:set_changed_bindings(
                arizona_binder:new(#{}), StatefulState
            ),
            {[], NoChangesStatefulState, View};
        false ->
            StatefulDependencies = arizona_tracker:get_stateful_dependencies(Id, Tracker),
            % Clear dependencies for this component before starting new render
            _ClearTracker = arizona_tracker_dict:clear_stateful_dependencies(Id),
            AffectedElements = get_affected_elements(ChangedBindings, StatefulDependencies),
            {Diff, DiffView} = generate_element_diff(AffectedElements, Template, ok, View),
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
generate_element_diff(DynamicSequence, Template, CallbackArg, View) ->
    case DynamicSequence of
        [] ->
            {[], View};
        _ ->
            Dynamic = arizona_template:get_dynamic(Template),
            process_affected_elements(DynamicSequence, Dynamic, CallbackArg, View)
    end.

%% Process affected elements to create diff changes
process_affected_elements([], _Dynamic, _CallbackArg, View) ->
    {[], View};
process_affected_elements([ElementIndex | T], Dynamic, CallbackArg, View) ->
    _Tracker = arizona_tracker_dict:set_current_element_index(ElementIndex),
    DynamicCallback = element(ElementIndex, Dynamic),
    case DynamicCallback(CallbackArg) of
        Callback when is_function(Callback, 2) ->
            {Html, CallbackView} = Callback(diff, View),
            ElementChange = {ElementIndex, Html},
            {RestChanges, FinalView} = process_affected_elements(
                T, Dynamic, CallbackArg, CallbackView
            ),
            {[ElementChange | RestChanges], FinalView};
        Result ->
            Html = arizona_html:to_html(Result),
            ElementChange = {ElementIndex, Html},
            {RestChanges, FinalView} = process_affected_elements(T, Dynamic, CallbackArg, View),
            {[ElementChange | RestChanges], FinalView}
    end.
