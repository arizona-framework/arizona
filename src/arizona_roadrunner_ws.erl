-module(arizona_roadrunner_ws).
-moduledoc """
Roadrunner WebSocket handler that bridges to `arizona_socket`.

Translates roadrunner's split handler model (upgrade-time
`roadrunner_handler` + post-upgrade `roadrunner_ws_handler`) into the
transport-agnostic `arizona_socket` API:

- `handle/1` -- the `roadrunner_handler` callback. Delegates the
  upgrade-time handshake to `arizona_ws:prepare/3` (parse
  `_az_path`/`_az_reconnect`, resolve the route via
  `arizona_roadrunner_req`, run middlewares) and returns either a
  plain HTTP halt reply or a `{websocket, ?MODULE, State}` upgrade
  tuple.
- `init/1` -- runs once in the session process after the upgrade.
  Constructs the Arizona socket and starts the live process; the
  reconnect path can immediately reply with the rendered page.
- `handle_frame/2` -- forwards inbound text frames to
  `arizona_socket:handle_in/2`; ignores binary, ping, and pong frames.
- `handle_info/2` -- forwards mailbox messages (typically
  `{arizona_push, ...}` from the live process) to
  `arizona_socket:handle_info/2`.
""".

-behaviour(roadrunner_handler).
-behaviour(roadrunner_ws_handler).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([handle/1]).
-export([init/1]).
-export([handle_frame/2]).
-export([handle_info/2]).

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc """
Roadrunner `handle/1` callback. Resolves the route, applies
middlewares, and returns the websocket upgrade tuple.
""".
-spec handle(Req) -> {Response, Req} when
    Req :: roadrunner_http1:request(),
    Response :: roadrunner_handler:response().
handle(Req) ->
    QS = roadrunner_req:parse_qs(Req),
    case arizona_ws:prepare(QS, arizona_roadrunner_req, Req) of
        {halt, HaltReq} ->
            {halt_response(HaltReq), arizona_req:raw(HaltReq)};
        {cont, State} ->
            {{websocket, ?MODULE, State}, Req}
    end.

-doc """
`roadrunner_ws_handler` `init/1` callback. Builds the Arizona socket
and starts the live process. On reconnect, returns the page snapshot
as the first outbound text frame.
""".
-spec init(State) -> Result when
    State :: map(),
    Result :: roadrunner_ws_handler:result().
init(#{handler := H, bindings := IB, on_mount := OM, req := ArzReq, reconnect := R} = State) ->
    Opts = #{reconnect => R, on_mount => OM},
    to_roadrunner(arizona_socket:init(H, IB, ArzReq, Opts), State).

-doc """
Roadrunner `handle_frame/2` callback. Forwards text frames to the
socket; ignores binary, ping, and pong frames.
""".
-spec handle_frame(Frame, State) -> Result when
    Frame :: roadrunner_ws:frame(),
    State :: map(),
    Result :: roadrunner_ws_handler:result().
handle_frame(#{opcode := text, payload := Data}, #{socket := Sock} = State) ->
    to_roadrunner(arizona_socket:handle_in(Data, Sock), State);
handle_frame(_Frame, State) ->
    {ok, State}.

-doc """
Roadrunner `handle_info/2` callback. Forwards mailbox messages
(typically `{arizona_push, Ops, Effects}`) to the socket.
""".
-spec handle_info(Msg, State) -> Result when
    Msg :: term(),
    State :: map(),
    Result :: roadrunner_ws_handler:result().
handle_info(Msg, #{socket := Sock} = State) ->
    to_roadrunner(arizona_socket:handle_info(Msg, Sock), State).

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

to_roadrunner({ok, Sock}, State) ->
    {ok, State#{socket => Sock}};
to_roadrunner({reply, Data, Sock}, State) ->
    {reply, [{text, Data}], State#{socket => Sock}};
to_roadrunner({close, Code, Reason, Sock}, State) ->
    %% Roadrunner's `{close, NewState}` return sends an empty close
    %% frame (no code). The arizona JS client distinguishes server
    %% crashes (4500) from clean exits (1000), so we emit the close
    %% as a `{reply, [{close, <<Code:16, Reason>>}]}` instead — that
    %% packs the status code into the close-frame payload per
    %% RFC 6455 §5.5.1 and lets the peer's reciprocal close drive
    %% the session shutdown.
    Payload = <<Code:16, Reason/binary>>,
    {reply, [{close, Payload}], State#{socket => Sock}}.

%% Middleware halted before the upgrade — emit a stashed redirect or a
%% bare 400 if the middleware did not write its own response.
halt_response(HaltReq) ->
    case arizona_req:halted_redirect(HaltReq) of
        {Status, Location} -> roadrunner_resp:redirect(Status, Location);
        undefined -> roadrunner_resp:bad_request()
    end.
