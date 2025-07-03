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
            %% Apply new bindings to existing state before checking remount
            UpdatedState = arizona_stateful:put_bindings(Bindings, State),
            case arizona_stateful:should_remount(UpdatedState) of
                true ->
                    Socket1 = arizona_stateful:call_unmount_callback(Mod, Socket),
                    %% Call mount callback after unmount for remount
                    Socket2 = arizona_stateful:call_mount_callback(Mod, Socket1),
                    %% Call the component's render callback which handles
                    %% rendering and returns updated socket
                    arizona_stateful:call_render_callback(Mod, Socket2);
                false ->
                    %% Update socket with new state and call render callback (which handles diffing)
                    Socket1 = arizona_socket:put_stateful_state(UpdatedState, Socket),
                    arizona_stateful:call_render_callback(Mod, Socket1)
            end;
        error ->
            State = arizona_stateful:new(Id, Mod, Bindings),
            Socket1 = arizona_socket:put_stateful_state(State, Socket),
            %% Call mount callback for new components
            Socket2 = arizona_stateful:call_mount_callback(Mod, Socket1),
            %% Call the component's render callback which handles
            %% rendering and returns updated socket
            arizona_stateful:call_render_callback(Mod, Socket2)
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
    %% Call the stateless component function to get result
    arizona_stateless:call_render_callback(Mod, Fun, TempSocket).
