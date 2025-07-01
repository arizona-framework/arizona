-module(arizona_component).

-export([call_stateful/3]).
-export([call_stateless/4]).

-spec call_stateful(Mod, Bindings, Socket) -> Socket1 when
    Mod :: module(),
    Bindings :: arizona_socket:bindings(),
    Socket :: arizona_socket:socket(),
    Socket1 :: arizona_socket:socket().
call_stateful(Mod, Bindings, Socket) ->
    Id = arizona_socket:get_current_stateful_id(Socket),
    case arizona_socket:find_stateful_state(Id, Socket) of
        {ok, State} ->
            case arizona_stateful:should_remount(State) of
                true ->
                    Socket1 = arizona_stateful:call_unmount_callback(Mod, Socket),
                    render_stateful_component(Mod, Socket1);
                false ->
                    arizona_differ:diff_stateful(State, Socket)
            end;
        error ->
            State = arizona_stateful:new(Id, Mod, Bindings),
            Socket1 = arizona_socket:put_stateful_state(Id, State, Socket),
            render_stateful_component(Mod, Socket1)
    end.

-spec call_stateless(Mod, Fun, Bindings, Socket) -> Socket1 when
    Mod :: module(),
    Fun :: atom(),
    Bindings :: arizona_socket:bindings(),
    Socket :: arizona_socket:socket(),
    Socket1 :: arizona_socket:socket().
call_stateless(Mod, Fun, Bindings, Socket) ->
    %% Create temporary socket with bindings for stateless component
    TempSocket = arizona_socket:with_temp_bindings(Bindings, Socket),
    render_stateless_component(Mod, Fun, TempSocket).

%% Helper functions for component rendering
render_stateful_component(Mod, Socket) ->
    %% Call the component's render callback to get template data
    TemplateData = arizona_stateful:call_render_callback(Mod, Socket),
    %% Use arizona_renderer to render the template data
    {_Html, UpdatedSocket} = arizona_renderer:render_stateful(TemplateData, Socket),
    UpdatedSocket.

render_stateless_component(Mod, Fun, Socket) ->
    %% Call the stateless component function to get result
    Result = arizona_stateless:call_render_callback(Mod, Fun, Socket),
    %% Use arizona_renderer to render the result
    {_Html, UpdatedSocket} = arizona_renderer:render_stateless(Result, Socket),
    UpdatedSocket.