-module(arizona_resolver).
-export([resolve_stateful/2]).

-spec resolve_stateful(StatefulId, Socket) -> {Html, Socket1} when
    StatefulId :: term(),
    Socket :: term(),
    Html :: iodata(),
    Socket1 :: term().
resolve_stateful(StatefulId, Socket) ->
    State = arizona_socket:get_stateful_state(StatefulId, Socket),
    Module = arizona_stateful:get_module(State),
    Bindings = arizona_stateful:get_bindings(State),
    Callback = arizona_template:render_stateful(Module, Bindings),
    Callback(Socket).
