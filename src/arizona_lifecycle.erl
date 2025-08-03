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
    Id = arizona_binder:get(id, Bindings),
    % Check if component already exists or needs mounting
    case arizona_view:find_stateful_state(Id, View) of
        {ok, State} ->
            render(Id, State, View);
        error ->
            mount_and_render(Module, Bindings, View, Id)
    end.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

render(Id, State, View) ->
    Template = arizona_stateful:call_render_callback(State),
    MountView = arizona_view:put_stateful_state(Id, State, View),
    {Id, Template, MountView}.

mount_and_render(Module, Bindings, View, Id) ->
    State = arizona_stateful:call_mount_callback(Module, Bindings),
    render(Id, State, View).
