-module(arizona_ws).
-moduledoc """
Transport-agnostic bootstrap for WebSocket handlers.

Factors out the upgrade-time logic common to every WebSocket
transport adapter:

1. Read the framework keys `_az_path` and `_az_reconnect` from the
   parsed upgrade query string.
2. Strip framework keys to recover the user-page query string.
3. Resolve the target route via `arizona_adapter:call_resolve_route/4`.
4. Apply any route middlewares against the synthesized `az:request()`.
5. Return either a halt signal (middleware blocked) or the state the
   transport should feed into `arizona_socket:init/3`.

The helper is transport-agnostic: callers pre-parse the query string
into the standard `[{binary(), binary() | true}]` shape (cowboy's
`parse_qs/1` output, which non-cowboy transports are expected to
produce from their own APIs). On halt, callers unwrap the native
transport request via `arizona_req:raw/1`.
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([prepare/3]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([qs/0]).
-export_type([state/0]).
-export_type([result/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-nominal qs() :: [{binary(), binary() | true}].

-nominal state() :: #{
    handler := module(),
    bindings := map(),
    on_mount := arizona_live:on_mount(),
    req := az:request(),
    reconnect := boolean()
}.

-nominal result() :: {halt, az:request()} | {cont, state()}.

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc """
Prepares a WebSocket upgrade.

Reads `_az_path` / `_az_reconnect` from `QS`, resolves the route
against `Adapter`/`AdapterState`, and applies route middlewares.

Returns `{halt, HaltReq}` when middleware short-circuits; the
transport extracts the native raw request via `arizona_req:raw/1`
and emits its own response. Returns `{cont, State}` otherwise; the
transport passes `State` (which carries `handler`, `bindings`,
`on_mount`, `req`, `reconnect`) on through to
`arizona_socket:init/3`.
""".
-spec prepare(QS, Adapter, AdapterState) -> result() when
    QS :: qs(),
    Adapter :: module(),
    AdapterState :: term().
prepare(QS, Adapter, AdapterState) ->
    Path = proplists:get_value(~"_az_path", QS, ~"/"),
    Reconnect = proplists:get_value(~"_az_reconnect", QS, ~"0") =:= ~"1",
    UserQs = user_qs(QS),
    {H, RouteOpts, ArzReq} = arizona_adapter:call_resolve_route(
        Adapter, Path, UserQs, AdapterState
    ),
    IB = maps:get(bindings, RouteOpts, #{}),
    OnMount = maps:get(on_mount, RouteOpts, []),
    Middlewares = maps:get(middlewares, RouteOpts, []),
    case arizona_req:apply_middlewares(Middlewares, ArzReq, IB) of
        {halt, HaltReq} ->
            {halt, HaltReq};
        {cont, ArzReq1, Bindings1} ->
            {cont, #{
                handler => H,
                bindings => Bindings1,
                on_mount => OnMount,
                req => ArzReq1,
                reconnect => Reconnect
            }}
    end.

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
is_framework_key(_) -> false.

encode_pair({K, true}) -> uri_string:quote(K);
encode_pair({K, V}) -> [uri_string:quote(K), $=, uri_string:quote(V)].
