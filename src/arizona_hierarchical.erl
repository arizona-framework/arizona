-module(arizona_hierarchical).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([hierarchical_view/1]).
-export([hierarchical_stateful/3]).
-export([hierarchical_stateless/4]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([stateful_struct/0]).
-export_type([stateless_struct/0]).

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
    {Dynamic, DynamicView} = arizona_renderer:render_dynamic(Template, hierarchical, View),
    Struct = #{
        type => stateless,
        static => arizona_template:get_static(Template),
        dynamic => Dynamic
    },
    {Struct, DynamicView}.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

track_hierarchical_stateful(Id, Template, View) ->
    _ClearTracker = arizona_tracker_dict:clear_stateful_dependencies(Id),
    _Tracker = arizona_tracker_dict:set_current_stateful_id(Id),
    {Dynamic, DynamicView} = arizona_renderer:render_dynamic(
        Template, hierarchical, View
    ),
    HierarchicalData = #{
        static => arizona_template:get_static(Template),
        dynamic => Dynamic
    },
    _HierarchicalStructure = arizona_hierarchical_dict:put_stateful_data(Id, HierarchicalData),
    Struct = #{
        type => stateful,
        id => Id
    },
    {Struct, DynamicView}.
