-module(arizona_cowboy_ws).
-moduledoc """
Cowboy WebSocket handler that bridges to `arizona_socket`.

Translates Cowboy WebSocket callbacks into the transport-agnostic
`arizona_socket` API:

- `init/2` -- delegates the upgrade-time handshake to
  `arizona_ws:prepare/3` (parse `_az_path`/`_az_reconnect`, resolve
  the route via `arizona_cowboy_req`, run middlewares) and
  returns either a plain HTTP halt reply or the `cowboy_websocket`
  upgrade tuple.
- `websocket_init/1` -- creates the socket and starts the live process
- `websocket_handle/2` -- forwards incoming text frames to
  `arizona_socket:handle_in/2`
- `websocket_info/2` -- forwards inbox messages (typically
  `{arizona_push, ...}` from the live process) to
  `arizona_socket:handle_info/2`

`to_cowboy/1` translates the `t:arizona_socket:result/0` shape back into
Cowboy's expected return values for each callback.
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

-ignore_xref([init/2, websocket_init/1, websocket_handle/2, websocket_info/2]).

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc """
Cowboy `init/2` callback. Resolves the route, applies middlewares,
and prepares the WebSocket upgrade state.
""".
-spec init(Req, Opts) ->
    {cowboy_websocket, Req, State, Opts} | {ok, Req, map()}
when
    Req :: cowboy_req:req(),
    Opts :: term(),
    State :: map().
init(Req, Opts) ->
    QS = cowboy_req:parse_qs(Req),
    case arizona_ws:prepare(QS, arizona_cowboy_req, Req) of
        {halt, HaltReq} ->
            handle_halt(HaltReq, Req);
        {cont, State} ->
            {cowboy_websocket, Req, State, Opts}
    end.

%% Translate a middleware halt into either a stashed redirect reply
%% (the adapter-agnostic `arizona_req:redirect/2` path) or pass the
%% raw cowboy req through unchanged (when the middleware wrote its
%% own response via `cowboy_req:reply/4`).
handle_halt(HaltReq, Req) ->
    case arizona_req:halted_redirect(HaltReq) of
        {Status, Location} ->
            Req2 = cowboy_req:reply(Status, #{~"location" => Location}, Req),
            {ok, Req2, #{}};
        undefined ->
            {ok, arizona_req:raw(HaltReq), #{}}
    end.

-doc """
Cowboy `websocket_init/1` callback. Creates the Arizona socket and
starts the live process.
""".
-spec websocket_init(State) -> CowboyResult when
    State :: map(),
    CowboyResult :: term().
websocket_init(#{handler := H, bindings := IB, on_mount := OM, req := ArzReq, reconnect := R}) ->
    Opts = #{reconnect => R, on_mount => OM},
    to_cowboy(arizona_socket:init(H, IB, ArzReq, Opts)).

-doc """
Cowboy `websocket_handle/2` callback. Forwards text frames to the
socket; ignores binary, ping, and pong frames.
""".
-spec websocket_handle(Frame, State) -> CowboyResult when
    Frame :: term(),
    State :: map(),
    CowboyResult :: term().
websocket_handle({text, Data}, #{socket := Sock}) ->
    to_cowboy(arizona_socket:handle_in(Data, Sock));
websocket_handle(_Frame, State) ->
    {ok, State}.

-doc """
Cowboy `websocket_info/2` callback. Forwards mailbox messages
(typically `{arizona_push, Ops, Effects}`) to the socket.
""".
-spec websocket_info(Msg, State) -> CowboyResult when
    Msg :: term(),
    State :: map(),
    CowboyResult :: term().
websocket_info(Msg, #{socket := Sock}) ->
    to_cowboy(arizona_socket:handle_info(Msg, Sock)).

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

to_cowboy({ok, Sock}) ->
    {ok, #{socket => Sock}};
to_cowboy({reply, Data, Sock}) ->
    {reply, {text, Data}, #{socket => Sock}};
to_cowboy({close, Code, Reason, _Sock}) ->
    {reply, {close, Code, Reason}, #{}}.
