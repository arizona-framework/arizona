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
    Bindings :: arizona_binder:map(),
    View :: arizona_view:view(),
    Result :: {Id, Template, View1},
    Id :: arizona_stateful:id(),
    Template :: arizona_template:template(),
    View1 :: arizona_view:view().
prepare_render(Module, Bindings, View) ->
    maybe
        % Check if component already exists or needs mounting
        #{id := Id} ?= Bindings,
        {ok, State} ?= arizona_view:find_stateful_state(Id, View),
        UpdatedState = arizona_stateful:merge_bindings(Bindings, State),
        render(Id, UpdatedState, View)
    else
        _Other ->
            mount_and_render(Module, Bindings, View)
    end.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

render(Id, State, View) ->
    Template = arizona_stateful:call_render_callback(State),
    UpdatedView = arizona_view:put_stateful_state(Id, State, View),
    {Id, Template, UpdatedView}.

mount_and_render(Module, Bindings, View) ->
    State = arizona_stateful:call_mount_callback(Module, Bindings),
    Id = arizona_stateful:get_binding(id, State),
    render(Id, State, View).
