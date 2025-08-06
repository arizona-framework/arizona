-module(arizona_hierarchical).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([hierarchical_view/1]).
-export([hierarchical_stateful/4]).
-export([hierarchical_stateless/6]).
-export([hierarchical_list/5]).

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
    track_hierarchical_stateful(Id, Template, undefined, View).

-spec hierarchical_stateful(Module, Bindings, ElementIndex, View) -> {Struct, View1} when
    Module :: module(),
    Bindings :: arizona_binder:bindings(),
    ElementIndex :: arizona_tracker:element_index(),
    View :: arizona_view:view(),
    Struct :: stateful_struct(),
    View1 :: arizona_view:view().
hierarchical_stateful(Module, Bindings, ElementIndex, View) ->
    {Id, Template, PrepRenderView} = arizona_lifecycle:prepare_render(Module, Bindings, View),

    % Store the fingerprint for future comparisons
    Fingerprint = arizona_template:get_fingerprint(Template),
    FingerprintView = arizona_view:put_fingerprint(Id, ElementIndex, Fingerprint, PrepRenderView),

    track_hierarchical_stateful(Id, Template, ElementIndex, FingerprintView).

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

    % Store the fingerprint for future comparisons
    Fingerprint = arizona_template:get_fingerprint(Template),
    FingerprintView = arizona_view:put_fingerprint(ParentId, ElementIndex, Fingerprint, View),

    DynamicSequence = arizona_template:get_dynamic_sequence(Template),
    Dynamic = arizona_template:get_dynamic(Template),
    {DynamicRender, DynamicView} = arizona_renderer:render_dynamic(
        DynamicSequence, Dynamic, ok, hierarchical, ParentId, ElementIndex, FingerprintView
    ),
    Struct = #{
        type => stateless,
        static => arizona_template:get_static(Template),
        dynamic => DynamicRender
    },
    {Struct, DynamicView}.

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
                {DynamicHierarchical, _UpdatedView} = arizona_renderer:render_dynamic(
                    DynamicSequence,
                    Dynamic,
                    CallbackArg,
                    hierarchical,
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

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

track_hierarchical_stateful(Id, Template, ElementIndex, View) ->
    _OldTracker = arizona_tracker_dict:clear_stateful_dependencies(Id),
    _ClearTracker = arizona_tracker_dict:set_current_stateful_id(Id),
    DynamicSequence = arizona_template:get_dynamic_sequence(Template),
    Dynamic = arizona_template:get_dynamic(Template),
    {DynamicRender, DynamicView} = arizona_renderer:render_dynamic(
        DynamicSequence, Dynamic, ok, hierarchical, Id, ElementIndex, View
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
