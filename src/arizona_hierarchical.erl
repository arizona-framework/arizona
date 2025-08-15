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
    track_hierarchical_stateful(Id, Template, Id, View).

-spec hierarchical_stateful(Module, Bindings, ParentId, ElementIndex, View) -> {Struct, View1} when
    Module :: module(),
    Bindings :: arizona_binder:bindings(),
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

    track_hierarchical_stateful(Id, Template, ParentId, FingerprintView).

-spec hierarchical_stateless(Module, Function, Bindings, ParentId, ElementIndex, View) ->
    {Struct, View1}
when
    Module :: module(),
    Function :: atom(),
    Bindings :: arizona_binder:bindings(),
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
    DynamicRender =
        [
            begin
                {DynamicHierarchical, _UpdatedView} = hierarchical_dynamic(
                    DynamicSequence,
                    Dynamic,
                    CallbackArg,
                    ParentId,
                    ElementIndex,
                    View
                ),
                DynamicHierarchical
            end
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

track_hierarchical_stateful(Id, Template, ParentId, View) ->
    _OldTracker = arizona_tracker_dict:clear_stateful_dependencies(Id),
    _ClearTracker = arizona_tracker_dict:set_current_stateful_id(Id),
    DynamicSequence = arizona_template:get_dynamic_sequence(Template),
    Dynamic = arizona_template:get_dynamic(Template),
    {DynamicRender, DynamicView} = hierarchical_dynamic(
        DynamicSequence, Dynamic, ok, ParentId, undefined, View
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
    ElementIndex :: arizona_tracker:element_index(),
    View :: arizona_view:view(),
    Render :: dynamic(),
    View1 :: arizona_view:view().
hierarchical_dynamic([], _Dynamic, _CallbackArg, _ParentId, _ElementIndex, View) ->
    {[], View};
hierarchical_dynamic([DynamicElementIndex | T], Dynamic, CallbackArg, ParentId, ElementIndex, View) ->
    DynamicCallback = element(DynamicElementIndex, Dynamic),
    ok =
        case ElementIndex of
            undefined ->
                _OldTracker = arizona_tracker_dict:set_current_element_index(DynamicElementIndex),
                ok;
            _ ->
                ok
        end,
    case DynamicCallback(CallbackArg) of
        Callback when is_function(Callback, 4) ->
            {Struct, CallbackView} = Callback(hierarchical, ParentId, DynamicElementIndex, View),
            {RestHtml, FinalView} = hierarchical_dynamic(
                T, Dynamic, CallbackArg, ParentId, ElementIndex, CallbackView
            ),
            {[Struct | RestHtml], FinalView};
        Result ->
            case arizona_template:is_template(Result) of
                true ->
                    {Struct, TemplateView} = hierarchical_template(
                        Result, ok, ParentId, ElementIndex, View
                    ),
                    {RestHtml, FinalView} = hierarchical_dynamic(
                        T, Dynamic, CallbackArg, ParentId, ElementIndex, TemplateView
                    ),
                    {[Struct | RestHtml], FinalView};
                false ->
                    Html = arizona_html:to_html(Result),
                    {RestHtml, FinalView} = hierarchical_dynamic(
                        T, Dynamic, CallbackArg, ParentId, ElementIndex, View
                    ),
                    {[Html | RestHtml], FinalView}
            end
    end.
