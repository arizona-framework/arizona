-module(arizona_lifecycle).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([prepare_render/3]).

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-spec prepare_render(Module, Bindings, View) -> Result when
    Module :: module(),
    Bindings :: arizona_binder:bindings(),
    View :: arizona_view:view(),
    Result :: {Id, Template, View1},
    Id :: arizona_stateful:id(),
    Template :: arizona_template:template(),
    View1 :: arizona_view:view().
prepare_render(Module, Bindings, View) ->
    Id = arizona_stateful:get_id(Bindings),

    % Check if component already exists or needs mounting
    case arizona_view:find_stateful_state(Id, View) of
        {ok, State} ->
            BindingsState = arizona_stateful:merge_bindings(Bindings, State),
            RenderBindings = arizona_stateful:get_bindings(BindingsState),
            Template = arizona_stateful:call_render_callback(Module, RenderBindings),
            StatefulView = arizona_view:put_stateful_state(Id, BindingsState, View),
            {Id, Template, StatefulView};
        error ->
            MountState = arizona_stateful:call_mount_callback(Module, Bindings),
            MountBindings = arizona_stateful:get_bindings(MountState),
            Template = arizona_stateful:call_render_callback(Module, MountBindings),
            MountView = arizona_view:put_stateful_state(Id, MountState, View),
            {Id, Template, MountView}
    end.
