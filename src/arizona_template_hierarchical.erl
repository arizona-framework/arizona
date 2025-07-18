-module(arizona_template_hierarchical).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([hierarchical_stateful/3]).
-export([hierarchical_stateless/4]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([hierarchical_data/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-nominal hierarchical_data() :: #{
    static := [binary()],
    dynamic := [arizona_html:html()]
}.

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-spec hierarchical_stateful(Module, Bindings, Socket) -> {Struct, Socket1} when
    Module :: atom(),
    Bindings :: arizona_binder:bindings(),
    Socket :: arizona_socket:socket(),
    Struct :: #{type := stateful, id := term()},
    Socket1 :: arizona_socket:socket().
hierarchical_stateful(Mod, Bindings, Socket) ->
    {Id, Template, Socket1} = arizona_stateful:prepare_render(Mod, Bindings, Socket),
    % Clear dependencies for this component before starting new render
    ok = arizona_socket:clear_component_dependencies(Id, Socket1),
    % Notify live process of current stateful component
    ok = arizona_socket:notify_current_stateful_id(Id, Socket1),
    {Dynamic, DynamicSocket} = arizona_template_renderer:render_dynamic_content(
        Template, Socket1
    ),
    % Store hierarchical data in live process
    HierarchicalData = #{
        static => arizona_template:static(Template),
        dynamic => Dynamic
    },
    ok = arizona_socket:put_hierarchical(Id, HierarchicalData, DynamicSocket),
    Struct = #{
        type => stateful,
        id => Id
    },
    {Struct, DynamicSocket}.

-spec hierarchical_stateless(Module, Function, Bindings, Socket) -> {Struct, Socket1} when
    Module :: atom(),
    Function :: atom(),
    Bindings :: arizona_binder:bindings(),
    Socket :: arizona_socket:socket(),
    Struct :: #{type := stateless, static := [binary()], dynamic := [arizona_html:html()]},
    Socket1 :: arizona_socket:socket().
hierarchical_stateless(Mod, Fun, Bindings, Socket) ->
    {Template, TempSocket} = arizona_stateless:prepare_render(Mod, Fun, Bindings, Socket),
    {Dynamic, DynamicSocket} = arizona_template_renderer:render_dynamic_content(
        Template, TempSocket
    ),
    Struct = #{
        type => stateless,
        static => arizona_template:static(Template),
        dynamic => Dynamic
    },
    {Struct, DynamicSocket}.
