-module(arizona_ws).
-moduledoc """
Transport-agnostic bootstrap for WebSocket handlers.

Factors out the upgrade-time logic common to every WebSocket
transport adapter:

1. Read the framework keys `_az_path`, `_az_reconnect`, and
   `_az_fps_follow` from the parsed upgrade query string.
2. Strip framework keys to recover the user-page query string.
3. Resolve the target route via the `arizona_req` adapter's
   `resolve_route/3` callback.
4. Apply any route middlewares against the synthesized `az:request()`.
5. Return either a halt signal (middleware blocked) or the state the
   transport should feed into `arizona_socket:init/4`.

The helper is transport-agnostic: callers pre-parse the query string
into the standard `[{binary(), binary() | true}]` shape from their own
transport APIs. On halt, callers unwrap the native transport request
via `arizona_req:raw/1`.
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([prepare/3]).
-export([upgrade/3]).
-export([init/1]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

%% The step under `upgrade/3`, kept public for an adapter that answers the upgrade itself.
-ignore_xref([prepare/3]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([qs/0]).
-export_type([state/0]).
-export_type([result/0]).
-export_type([upgrade/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-nominal qs() :: [{binary(), binary() | true}].

-nominal state() :: #{
    handler := module(),
    bindings := map(),
    on_mount := arizona_live:on_mount(),
    layouts := [arizona_render:layout()],
    req := az:request(),
    reconnect := boolean(),
    fps_follow := boolean(),
    capabilities := map()
}.

-nominal result() :: {halt, az:request()} | {cont, state()} | not_found.

%% What an adapter answers a WebSocket upgrade request with: a plain HTTP reply
%% (a middleware halt, or `404` for a path that is no live route), or the
%% upgrade itself, whose `101` carries `Headers`. Header lines are in send order.
-nominal upgrade() ::
    {reply, arizona_req:resp_status(), [{binary(), binary()}], AdapterState :: term()}
    | {upgrade, [{binary(), binary()}], state()}.

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc """
Prepares a WebSocket upgrade.

Reads `_az_path` / `_az_reconnect` / `_az_fps_follow` from `QS`,
resolves the route against `Adapter`/`AdapterState`, and applies route
middlewares.

Returns `{halt, HaltReq}` when middleware short-circuits; the
transport extracts the native raw request via `arizona_req:raw/1`
and emits its own response. Returns `not_found` when the client-supplied
`_az_path` does not resolve to a live route (no match, or a
controller/asset/ws route); the transport rejects the upgrade with a 404
rather than crashing on an attacker-controllable path. Returns `{cont,
State}` otherwise; the transport passes `State` (which carries `handler`,
`bindings`, `on_mount`, `layouts`, `req`, `reconnect`, `fps_follow`) on
through to `arizona_socket:init/4`.
""".
-spec prepare(QS, Adapter, AdapterState) -> result() when
    QS :: qs(),
    Adapter :: module(),
    AdapterState :: term().
prepare(QS, Adapter, AdapterState) ->
    Path = proplists:get_value(~"_az_path", QS, ~"/"),
    Reconnect = proplists:get_value(~"_az_reconnect", QS, ~"0") =:= ~"1",
    %% The client's promise that a `cached_fps` frame follows this reconnect
    %% open as its first frame, so the resync render may wait for it and dedup
    %% statics against the announced fingerprints (arizona_socket). One
    %% constant-size flag -- the fingerprint LIST stays off the URL (it can
    %% grow past URL limits); a client that does not set the flag keeps the
    %% immediate, undeduped resync.
    FpsFollow = proplists:get_value(~"_az_fps_follow", QS, ~"0") =:= ~"1",
    Caps = parse_caps(proplists:get_value(~"_az_caps", QS, undefined)),
    UserQs = user_qs(QS),
    case arizona_req:call_resolve_route(Adapter, Path, UserQs, AdapterState) of
        error ->
            not_found;
        {ok, H, RouteOpts, ArzReq} ->
            IB = maps:get(bindings, RouteOpts, #{}),
            OnMount = maps:get(on_mount, RouteOpts, []),
            Middlewares = maps:get(middlewares, RouteOpts, []),
            %% The layouts that wrapped THIS page at SSR. The socket keeps them to
            %% compare against a navigate/patch target's, since only the page
            %% inside them can be replaced over the wire (see arizona_socket).
            Layouts = maps:get(layouts, RouteOpts, []),
            case arizona_middleware:apply_middlewares(Middlewares, ArzReq, IB) of
                {halt, HaltReq} ->
                    {halt, HaltReq};
                {cont, ArzReq1, Bindings1} ->
                    {cont, #{
                        handler => H,
                        bindings => Bindings1,
                        on_mount => OnMount,
                        layouts => Layouts,
                        req => ArzReq1,
                        reconnect => Reconnect,
                        fps_follow => FpsFollow,
                        capabilities => Caps
                    }}
            end
    end.

-doc """
`prepare/3` folded into the answer to the upgrade request. A halt becomes its
`arizona_req:halt_reply/1` and an unresolvable `_az_path` a `404`, both with
the adapter state the reply belongs to; otherwise the upgrade proceeds and
`arizona_req:flush/1`'s lines (cookies and headers the route's middlewares
stashed, and a committed session write) ride the `101`.
""".
-spec upgrade(QS, Adapter, AdapterState) -> upgrade() when
    QS :: qs(),
    Adapter :: module(),
    AdapterState :: term().
upgrade(QS, Adapter, AdapterState) ->
    case prepare(QS, Adapter, AdapterState) of
        {halt, HaltReq} ->
            {Status, Headers} = arizona_req:halt_reply(HaltReq),
            {reply, Status, Headers, arizona_req:raw(HaltReq)};
        not_found ->
            {reply, 404, [], AdapterState};
        {cont, #{req := ArzReq} = State} ->
            {upgrade, arizona_req:flush(ArzReq), State}
    end.

-doc """
Starts the Arizona socket for an upgraded connection, in the process that owns
it, from the state `upgrade/3` returned. The result is `arizona_socket`'s,
which the adapter maps onto its server's WebSocket replies.
""".
-spec init(State) -> arizona_socket:result() when
    State :: state().
init(#{handler := H, bindings := IB, on_mount := OM, req := ArzReq} = State) ->
    arizona_socket:init(H, IB, ArzReq, #{
        reconnect => maps:get(reconnect, State),
        fps_follow => maps:get(fps_follow, State),
        on_mount => OM,
        layouts => maps:get(layouts, State),
        capabilities => maps:get(capabilities, State)
    }).

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% Strip framework keys (`_az_*`) from the parsed qs and rebuild a raw
%% qs binary for the adapter / arizona_req. Leaves user params untouched.
user_qs(QS) ->
    iolist_to_binary(
        lists:join(~"&", [encode_pair(Pair) || {Key, _} = Pair <:- QS, not is_framework_key(Key)])
    ).

is_framework_key(~"_az_path") -> true;
is_framework_key(~"_az_reconnect") -> true;
is_framework_key(~"_az_fps_follow") -> true;
is_framework_key(~"_az_caps") -> true;
is_framework_key(_) -> false.

%% Decode the client-advertised native-shell capabilities (`_az_caps`, a JSON
%% object) into a map. The value is untrusted and trivially malformable, so a bad
%% decode (or a non-object) is treated as "no capabilities" rather than crashing
%% the WS upgrade -- mirrors the malformed-frame handling in
%% `arizona_socket:handle_in/2`. The resulting map is a UI/effect hint only and
%% must never gate a server-side authorization decision.
parse_caps(undefined) ->
    #{};
parse_caps(Bin) ->
    try json:decode(Bin) of
        Caps when is_map(Caps) -> Caps;
        _ -> #{}
    catch
        error:_ -> #{}
    end.

encode_pair({K, true}) -> uri_string:quote(K);
encode_pair({K, V}) -> [uri_string:quote(K), $=, uri_string:quote(V)].
