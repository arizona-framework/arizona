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
- `handle_drain/2` -- fires when the listener broadcasts a drain
  signal (`roadrunner_listener:drain/2` or `notify_drain/2`). Emits
  the telemetry ack and notifies the live process via
  `{arizona_drain, Deadline}` so the view can wind down gracefully
  (close the WS, push a reconnect indicator, etc.).
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
-export([handle_drain/2]).

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc """
Roadrunner `handle/1` callback. Resolves the route, applies
middlewares, and returns the websocket upgrade tuple.
""".
-spec handle(Req) -> {Response, Req} when
    Req :: roadrunner_req:request(),
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
(typically `{arizona_push, Ops, Effects}` from the live process) to
`arizona_socket:handle_info/2`.
""".
-spec handle_info(Msg, State) -> Result when
    Msg :: term(),
    State :: map(),
    Result :: roadrunner_ws_handler:result().
handle_info(Msg, #{socket := Sock} = State) ->
    to_roadrunner(arizona_socket:handle_info(Msg, Sock), State).

-doc """
Roadrunner `handle_drain/2` callback. Emits the telemetry ack and
forwards an adapter-agnostic `{arizona_drain, Deadline}` to the live
process so the view's optional `handle_drain/2` callback can run and
choose to push a "reconnecting" indicator, close the WS, or stay
alive past drain.
""".
-spec handle_drain(Deadline, State) -> Result when
    Deadline :: integer(),
    State :: map(),
    Result :: roadrunner_ws_handler:result().
handle_drain(Deadline, #{socket := Sock, req := Req} = State) ->
    ok = roadrunner:acknowledge_drain(arizona_req:raw(Req), Deadline),
    ok =
        case arizona_socket:live_pid(Sock) of
            undefined ->
                ok;
            Pid ->
                Pid ! {arizona_drain, Deadline},
                ok
        end,
    {ok, State}.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

to_roadrunner({ok, Sock}, State) ->
    {ok, State#{socket => Sock}};
to_roadrunner({reply, Data, Sock}, State) ->
    {reply, [{text, Data}], State#{socket => Sock}};
to_roadrunner({close, Code, Reason, Sock}, State) ->
    {close, Code, Reason, State#{socket => Sock}}.

%% Middleware halted before the upgrade — emit a stashed redirect or a
%% bare 400 if the middleware did not write its own response, then flush any
%% stashed response headers/cookies (put_resp_header/put_resp_cookie).
halt_response(HaltReq) ->
    Resp =
        case arizona_req:halted_redirect(HaltReq) of
            {Status, Location} ->
                roadrunner_resp:redirect(Status, Location);
            undefined ->
                %% A middleware (e.g. check_origin) may stash a status; honor it.
                case arizona_req:resp_status(HaltReq) of
                    undefined -> roadrunner_resp:bad_request();
                    Status -> roadrunner_resp:status(Status)
                end
        end,
    flush_resp(HaltReq, Resp).

%% Fold stashed response headers and cookies onto Resp.
flush_resp(ArzReq, Resp0) ->
    Resp1 = lists:foldl(
        fun({Name, Value}, R) -> roadrunner_resp:add_header(R, Name, Value) end,
        Resp0,
        arizona_req:resp_headers(ArzReq)
    ),
    lists:foldl(
        fun({Name, Value, Opts}, R) -> roadrunner_resp:set_cookie(R, Name, Value, Opts) end,
        Resp1,
        arizona_req:resp_cookies(ArzReq)
    ).
