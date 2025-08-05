-module(arizona_hierarchical).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([hierarchical_view/1]).
-export([hierarchical_stateful/3]).
-export([hierarchical_stateless/4]).
-export([hierarchical_list/3]).

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
    track_hierarchical_stateful(Id, Template, View).

-spec hierarchical_stateful(Module, Bindings, View) -> {Struct, View1} when
    Module :: module(),
    Bindings :: arizona_binder:bindings(),
    View :: arizona_view:view(),
    Struct :: stateful_struct(),
    View1 :: arizona_view:view().
hierarchical_stateful(Module, Bindings, View) ->
    {Id, Template, PrepRenderView} = arizona_lifecycle:prepare_render(Module, Bindings, View),
    track_hierarchical_stateful(Id, Template, PrepRenderView).

-spec hierarchical_stateless(Module, Function, Bindings, View) -> {Struct, View1} when
    Module :: module(),
    Function :: atom(),
    Bindings :: arizona_binder:bindings(),
    View :: arizona_view:view(),
    Struct :: stateless_struct(),
    View1 :: arizona_view:view().
hierarchical_stateless(Module, Fun, Bindings, View) ->
    Template = arizona_stateless:call_render_callback(Module, Fun, Bindings),
    DynamicSequence = arizona_template:get_dynamic_sequence(Template),
    Dynamic = arizona_template:get_dynamic(Template),
    {DynamicRender, DynamicView} = arizona_renderer:render_dynamic(
        DynamicSequence, Dynamic, hierarchical, ok, View
    ),
    Struct = #{
        type => stateless,
        static => arizona_template:get_static(Template),
        dynamic => DynamicRender
    },
    {Struct, DynamicView}.

-spec hierarchical_list(Template, List, View) -> {Struct, View1} when
    Template :: arizona_template:template(),
    List :: [dynamic()],
    View :: arizona_view:view(),
    Struct :: list_struct(),
    View1 :: arizona_view:view().
hierarchical_list(Template, List, View) ->
    DynamicSequence = arizona_template:get_dynamic_sequence(Template),
    Dynamic = arizona_template:get_dynamic(Template),
    DynamicRender =
        [
            begin
                {DynamicHtml, _UpdatedView} = arizona_renderer:render_dynamic(
                    DynamicSequence, Dynamic, hierarchical, CallbackArg, View
                ),
                DynamicHtml
            end
         || CallbackArg <- List
        ],
    Struct = #{
        type => list,
        static => arizona_template:get_static(Template),
        dynamic => DynamicRender
    },
    {Struct, View}.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

track_hierarchical_stateful(Id, Template, View) ->
    _ClearTracker = arizona_tracker_dict:clear_stateful_dependencies(Id),
    _Tracker = arizona_tracker_dict:set_current_stateful_id(Id),
    DynamicSequence = arizona_template:get_dynamic_sequence(Template),
    Dynamic = arizona_template:get_dynamic(Template),
    {DynamicRender, DynamicView} = arizona_renderer:render_dynamic(
        DynamicSequence, Dynamic, hierarchical, ok, View
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
