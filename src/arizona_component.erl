-module(arizona_component).

-export([call_stateful/3]).
-export([call_stateless/4]).

call_stateful(Mod, Bindings, Socket) ->
    Id = arizona_socket:get_current_stateful_id(Socket),
    case arizona_socket:find_stateful_state(Id, Socket) of
        {ok, State} ->
            case arizona_stateful:should_remount(State) of
                true ->
                    Socket1 = arizona_stateful:call_unmount_callback(Mod, Socket),
                    render_stateful_component(Mod, State, Socket1);
                false ->
                    arizona_differ:diff_stateful(State, Socket)
            end;
        error ->
            State = arizona_stateful:new(Id, Mod, Bindings),
            render_stateful_component(Mod, State, Socket)
    end.

call_stateless(Mod, Fun, Bindings, Socket) ->
    %% Update socket with component bindings
    CurrentState = arizona_socket:get_current_stateful_state(Socket),
    UpdatedState = arizona_stateful:put_bindings(Bindings, CurrentState),
    UpdatedSocket = arizona_socket:put_stateful_state(
        arizona_socket:get_current_stateful_id(Socket), 
        UpdatedState, 
        Socket
    ),
    render_stateless_component(Mod, Fun, UpdatedSocket).

%% Helper functions for component rendering
render_stateful_component(Mod, State, Socket) ->
    %% Call the component's render callback to get template data
    TemplateData = arizona_stateful:call_render_callback(Mod, Socket),
    %% Use arizona_renderer to render the template data
    arizona_renderer:render_stateful(TemplateData, Socket).

render_stateless_component(Mod, Fun, Socket) ->
    %% Call the stateless component function to get result
    Result = arizona_stateless:call_render_callback(Mod, Fun, Socket),
    %% Use arizona_renderer to render the result
    arizona_renderer:render_stateless(Result, Socket).