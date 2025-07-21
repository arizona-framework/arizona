-module(arizona_template_hierarchical).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([hierarchical_stateful/3]).
-export([hierarchical_stateless/4]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([static_content/0]).
-export_type([dynamic_content/0]).
-export_type([hierarchical_data/0]).
-export_type([stateful_struct/0]).
-export_type([stateless_struct/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-nominal static_content() :: [binary()].
-nominal dynamic_content() :: [arizona_html:html()].
-nominal hierarchical_data() :: #{
    static := static_content(),
    dynamic := dynamic_content()
}.
-nominal stateful_struct() :: #{
    type := stateful,
    id := arizona_stateful:id()
}.
-nominal stateless_struct() :: #{
    type := stateless,
    static := static_content(),
    dynamic := dynamic_content()
}.

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-spec hierarchical_stateful(Module, Bindings, View) -> {Struct, View1} when
    Module :: module(),
    Bindings :: arizona_binder:bindings(),
    View :: arizona_view:view(),
    Struct :: stateful_struct(),
    View1 :: arizona_view:view().
hierarchical_stateful(Module, Bindings, View) ->
    {Id, Template, View1} = arizona_stateful:prepare_render(Module, Bindings, View),
    ok = arizona_view:live_clear_component_dependencies(Id, View1),
    ok = arizona_view:live_set_current_stateful_id(Id, View1),
    {Dynamic, DynamicView} = arizona_template_renderer:render_dynamic_content(
        Template, View1
    ),
    HierarchicalData = #{
        static => arizona_template:static(Template),
        dynamic => Dynamic
    },
    ok = arizona_view:live_put_stateful_hierarchical(Id, HierarchicalData, DynamicView),
    Struct = #{
        type => stateful,
        id => Id
    },
    {Struct, DynamicView}.

-spec hierarchical_stateless(Module, Function, Bindings, View) -> {Struct, View1} when
    Module :: module(),
    Function :: atom(),
    Bindings :: arizona_binder:bindings(),
    View :: arizona_view:view(),
    Struct :: stateless_struct(),
    View1 :: arizona_view:view().
hierarchical_stateless(Module, Fun, Bindings, View) ->
    Template = arizona_stateless:call_render_callback(Module, Fun, Bindings),
    {Dynamic, DynamicView} = arizona_template_renderer:render_dynamic_content(Template, View),
    Struct = #{
        type => stateless,
        static => arizona_template:static(Template),
        dynamic => Dynamic
    },
    {Struct, DynamicView}.
