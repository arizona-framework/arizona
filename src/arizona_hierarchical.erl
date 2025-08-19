-module(arizona_hierarchical).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([hierarchical_view/1]).
-export([hierarchical_stateful/5]).
-export([hierarchical_stateless/6]).
-export([hierarchical_list/5]).
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

-spec hierarchical_view(View) -> {Struct, View1} when
    View :: arizona_view:view(),
    Struct :: stateful_struct(),
    View1 :: arizona_view:view().
hierarchical_view(View) ->
    State = arizona_view:get_state(View),
    Id = arizona_stateful:get_binding(id, State),
    Template = arizona_view:call_render_callback(View),
    track_hierarchical_stateful(Id, Template, View).

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
        ParentId, ElementIndex, Fingerprint, PrepRenderView
    ),

    track_hierarchical_stateful(Id, Template, FingerprintView).

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
    hierarchical_template(Template, ok, ParentId, ElementIndex, View).

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
    FingerprintView = arizona_view:put_fingerprint(ParentId, ElementIndex, Fingerprint, View),

    DynamicSequence = arizona_template:get_dynamic_sequence(Template),
    Dynamic = arizona_template:get_dynamic(Template),
    DynamicRender = [
        render_list_hierarchical(
            DynamicSequence, Dynamic, CallbackArg, ParentId, ElementIndex, View
        )
     || CallbackArg <- List
    ],
    Struct = #{
        type => list,
        static => arizona_template:get_static(Template),
        dynamic => DynamicRender
    },
    {Struct, FingerprintView}.

-spec hierarchical_template(Template, CallbackArg, ParentId, ElementIndex, View) ->
    {Struct, View1}
when
    Template :: arizona_template:template(),
    CallbackArg :: dynamic(),
    ParentId :: arizona_stateful:id(),
    ElementIndex :: arizona_tracker:element_index(),
    View :: arizona_view:view(),
    Struct :: stateless_struct(),
    View1 :: arizona_view:view().
hierarchical_template(Template, CallbackArg, ParentId, ElementIndex, View) ->
    % Store the fingerprint for future comparisons
    Fingerprint = arizona_template:get_fingerprint(Template),
    FingerprintView = arizona_view:put_fingerprint(ParentId, ElementIndex, Fingerprint, View),

    DynamicSequence = arizona_template:get_dynamic_sequence(Template),
    Dynamic = arizona_template:get_dynamic(Template),
    {DynamicRender, DynamicView} = hierarchical_dynamic(
        DynamicSequence, Dynamic, CallbackArg, ParentId, ElementIndex, FingerprintView
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
        DynamicSequence, Dynamic, ok, Id, undefined, View
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

-spec hierarchical_dynamic(Sequence, Dynamic, CallbackArg, ParentId, ElementIndex, View) ->
    {Render, View1}
when
    Sequence :: arizona_template:dynamic_sequence(),
    Dynamic :: arizona_template:dynamic(),
    CallbackArg :: dynamic(),
    ParentId :: arizona_stateful:id(),
    ElementIndex :: arizona_tracker:element_index() | undefined,
    View :: arizona_view:view(),
    Render :: dynamic(),
    View1 :: arizona_view:view().
hierarchical_dynamic([], _Dynamic, _CallbackArg, _ParentId, _ElementIndex, View) ->
    {[], View};
hierarchical_dynamic(
    [DynamicElementIndex | T], Dynamic, CallbackArg, ParentId, ElementIndex, View
) ->
    DynamicCallback = element(DynamicElementIndex, Dynamic),
    ok = maybe_set_tracker_index(ElementIndex, DynamicElementIndex),
    CallbackResult = DynamicCallback(CallbackArg),
    process_hierarchical_callback(
        CallbackResult, DynamicElementIndex, T, Dynamic, CallbackArg, ParentId, ElementIndex, View
    ).

%% Helper function to set tracker index when needed
maybe_set_tracker_index(undefined, DynamicElementIndex) ->
    _OldTracker = arizona_tracker_dict:set_current_element_index(DynamicElementIndex),
    ok;
maybe_set_tracker_index(_ElementIndex, _DynamicElementIndex) ->
    ok.

%% Helper function to render list hierarchical elements
render_list_hierarchical(DynamicSequence, Dynamic, CallbackArg, ParentId, ElementIndex, View) ->
    {DynamicHierarchical, _UpdatedView} = hierarchical_dynamic(
        DynamicSequence, Dynamic, CallbackArg, ParentId, ElementIndex, View
    ),
    DynamicHierarchical.

%% Helper function to process hierarchical callback results
process_hierarchical_callback(
    Callback, DynamicElementIndex, T, Dynamic, CallbackArg, ParentId, ElementIndex, View
) when is_function(Callback, 4) ->
    {Struct, CallbackView} = Callback(hierarchical, ParentId, DynamicElementIndex, View),
    {RestHtml, FinalView} = hierarchical_dynamic(
        T, Dynamic, CallbackArg, ParentId, ElementIndex, CallbackView
    ),
    {[Struct | RestHtml], FinalView};
process_hierarchical_callback(
    Result, _DynamicElementIndex, T, Dynamic, CallbackArg, ParentId, ElementIndex, View
) ->
    case arizona_template:is_template(Result) of
        true ->
            process_hierarchical_template(
                Result, T, Dynamic, CallbackArg, ParentId, ElementIndex, View
            );
        false ->
            process_hierarchical_html(
                Result, T, Dynamic, CallbackArg, ParentId, ElementIndex, View
            )
    end.

%% Helper function to process hierarchical template results
process_hierarchical_template(Result, T, Dynamic, CallbackArg, ParentId, ElementIndex, View) ->
    {Struct, TemplateView} = hierarchical_template(Result, ok, ParentId, ElementIndex, View),
    {RestHtml, FinalView} = hierarchical_dynamic(
        T, Dynamic, CallbackArg, ParentId, ElementIndex, TemplateView
    ),
    {[Struct | RestHtml], FinalView}.

%% Helper function to process hierarchical HTML results
process_hierarchical_html(Result, T, Dynamic, CallbackArg, ParentId, ElementIndex, View) ->
    Html = arizona_html:to_html(Result),
    {RestHtml, FinalView} = hierarchical_dynamic(
        T, Dynamic, CallbackArg, ParentId, ElementIndex, View
    ),
    {[Html | RestHtml], FinalView}.
