-module(arizona_cowboy_ws).
-moduledoc """
Cowboy WebSocket handler that bridges to `arizona_socket`.

Translates Cowboy WebSocket callbacks into the transport-agnostic
`arizona_socket` API:

- `init/2` -- parses query params (`path`, `reconnect`, `params`),
  resolves the target route via `arizona_cowboy_adapter`, runs any
  middlewares declared on the route, and prepares the upgrade state
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
    Path = proplists:get_value(~"path", QS, ~"/"),
    Reconnect = proplists:get_value(~"reconnect", QS, ~"0") =:= ~"1",
    Params =
        case proplists:get_value(~"params", QS) of
            undefined -> #{};
            JSON -> json:decode(JSON)
        end,
    LiveReq = Req#{path => Path},
    {_CowboyHandler, _ResolvedReq, RouteOpts} = arizona_cowboy_adapter:resolve_cowboy_route(
        LiveReq
    ),
    H = maps:get(handler, RouteOpts),
    IB = maps:merge(maps:get(bindings, RouteOpts, #{}), Params),
    OnMount = maps:get(on_mount, RouteOpts, []),
    Middlewares = maps:get(middlewares, RouteOpts, []),
    case arizona_cowboy_req:apply_middlewares(Middlewares, Req, IB) of
        {halt, Req1} ->
            {ok, Req1, #{}};
        {cont, _Req1, Bindings1} ->
            State = #{
                handler => H,
                bindings => Bindings1,
                on_mount => OnMount,
                req => Req,
                reconnect => Reconnect
            },
            {cowboy_websocket, Req, State, Opts}
    end.

-doc """
Cowboy `websocket_init/1` callback. Creates the Arizona socket and
starts the live process.
""".
-spec websocket_init(State) -> CowboyResult when
    State :: map(),
    CowboyResult :: term().
websocket_init(#{handler := H, bindings := IB, on_mount := OM, req := Req, reconnect := R}) ->
    Opts = #{
        reconnect => R, on_mount => OM, adapter => arizona_cowboy_adapter, adapter_state => Req
    },
    to_cowboy(arizona_socket:init(H, IB, Opts)).

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
