-module(arizona_roadrunner_server).
-moduledoc """
Boots a roadrunner listener wired up with Arizona routes: the built-in
`arizona_server` adapter.

`start/2` compiles the route list, stashes the originals so they
can be re-compiled later, configures this listener's own dispatch
persistent term (`{arizona_roadrunner_dispatch, Name}`), and launches
a roadrunner listener under `roadrunner_sup` via
`roadrunner:start_listener/2`.

`recompile_routes/0` rebuilds every Arizona-managed dispatch by
walking persistent terms tagged with the routes key. The dev hot
reloader calls this after a successful recompile so new or changed
routes take effect without restarting the listener.

## Options

- `routes` -- list of `t:arizona_roadrunner_router:route/0` (required)
- `error_page` -- `{Module, Function}` to render the dev error page
  (default `{arizona_error_page, render}`)
- `scheme` -- `http` (default) or `https`
- `tls` -- `[ssl:tls_server_option()]`. Supplying it turns the listener
  into an HTTPS one, so `scheme => https` is optional sugar; pairing it
  with an explicit `scheme => http` is a contradiction and raises
  `tls_with_http_scheme`
- `transport_opts` -- `[{port, Port}]` sugar for the listen port. Only
  `port` is accepted; any other entry errors (`unsupported_transport_opt`)
  rather than being silently dropped -- roadrunner takes other
  listener/socket options (`ip`, `socket_backlog`, ...) directly in
  `proto_opts`. Port precedence: `transport_opts` port, then
  `proto_opts.port`, then `4040`
- `proto_opts` -- map of roadrunner listener opts merged on top of
  the dispatch (e.g. `max_clients`, `max_content_length`,
  `request_timeout`, `tls`, `slot_reconciliation`; since roadrunner
  0.9.0 also the per-connection memory bounds -- a top-level
  `recv_buffer`, and under `ws` the per-session `recv_buffer` and
  `hibernate_after` to park idle WebSocket sessions -- plus
  `tls_handshake_timeout`)
- `compress` -- `true` (default) or `false`. When true,
  `roadrunner_compress` is attached to `live` and `asset` routes so
  HTML pages and static assets above the threshold are gzip/deflate
  encoded per `Accept-Encoding`. The dev SSE reload stream and the
  WebSocket upgrade response are intentionally **not** compressed.
  Set to `false` if you have an upstream proxy doing compression
  already -- and note the one thing that still compresses under `false`:
  a route explicitly listing `arizona_middleware:compress()` in its
  `middlewares` (route-level explicit beats the listener default). Behind
  a compressing proxy, leave `compress()` out of routes too.

Any option value (`port`, `scheme`, `tls` cert paths, `proto_opts` tunables)
may be an env-var reference resolved at startup -- `{env, "PORT", 8080}` or the
required `{env, "PORT"}`. See `m:arizona_config`.

## Enabling HTTP/2

Roadrunner ships with HTTP/2 support (RFC 9113, h2spec-compliant).
Opt in via `proto_opts.protocols`. Browsers require TLS + ALPN for
HTTP/2, so pair it with `scheme => https` and a `tls` config:

```erlang
arizona_roadrunner_server:start(arizona_http, #{
    routes => Routes,
    scheme => https,
    tls => [{certfile, "priv/cert.pem"}, {keyfile, "priv/key.pem"}],
    proto_opts => #{protocols => [http1, http2]}
}).
```

Roadrunner negotiates the protocol per connection from the ALPN
offer; HTTP/1.1 clients still work on the same listener. Default
when `protocols` is omitted is `[http1]` (HTTP/1.1 only).
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-behaviour(arizona_server).

-export([start/2]).
-export([stop/1]).
-export([drain/2]).
-export([recompile_routes/0]).
-export([format_error/2]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

-ignore_xref([start/2, stop/1]).
-ignore_xref([format_error/2]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------

-define(ROUTES_KEY, arizona_roadrunner_routes).

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc """
Starts a roadrunner listener named `Name` configured with Arizona
routes from `Opts`.
""".
-spec start(Name, Opts) -> {ok, pid()} | {error, term()} when
    Name :: atom(),
    Opts :: map().
start(Name, #{routes := _} = Opts0) ->
    %% A running listener of the same name owns the route stash its WS upgrades
    %% read: a clash is refused before anything is built or written, or they
    %% would resolve against the rejected routes until roadrunner refused it.
    case whereis(Name) of
        undefined ->
            start_new(Name, Opts0);
        Pid ->
            {error, {already_started, Pid}}
    end.

-doc """
Stops the listener named `Name` and clears its persistent terms.
Returns `{error, not_found}` if `Name` is not a running listener.
""".
-spec stop(Name) -> ok | {error, not_found} when
    Name :: atom().
stop(Name) ->
    persistent_term:erase({?ROUTES_KEY, Name}),
    ok = arizona_roadrunner_router:forget_routes(Name),
    ok = forget_dev_terms(),
    roadrunner:stop_listener(Name).

-doc """
Drains the listener named `Name`, then stops it like `stop/1`. The listen socket
closes, every connection and WebSocket session is told to wind down -- a live view
runs `handle_drain/2` and closes `1001`, which clients reconnect from -- and the
call waits up to `Timeout` milliseconds for them before the listener exits;
sessions still open then are killed. Returns `{error, not_found}` if `Name` is not
a running listener.
""".
-spec drain(Name, Timeout) -> ok | {error, not_found} when
    Name :: atom(),
    Timeout :: non_neg_integer().
drain(Name, Timeout) ->
    case whereis(Name) of
        undefined ->
            {error, not_found};
        _Pid ->
            case roadrunner_listener:drain(Name, Timeout) of
                {ok, drained} -> ok;
                {timeout, _Remaining} -> ok
            end,
            stop(Name)
    end.

-doc """
Recompiles every Arizona-managed dispatch by walking persistent
terms. Called by the dev hot reloader after a successful recompile.

Refreshes two tables in lockstep:

- `{arizona_roadrunner_dispatch, Name}` (read by
  `arizona_roadrunner_req:resolve_route/3` on WS navigate), and
- the listener's own compiled route table (read by roadrunner on
  every incoming HTTP request).

Both must move together — otherwise WS navigate sees new routes
while a fresh HTTP request still 404s against the listener's stale
table. A stashed listener that is no longer running is skipped, so it
cannot stop the others from reloading.
""".
-spec recompile_routes() -> ok.
recompile_routes() ->
    Terms = persistent_term:get(),
    lists:foreach(
        fun
            ({{?ROUTES_KEY, Name}, {Routes, BuildOpts}}) ->
                ok = arizona_roadrunner_router:compile_routes(Routes, BuildOpts, Name),
                ListenerRoutes = arizona_roadrunner_router:routes(Routes, BuildOpts),
                ok = reload_listener_routes(Name, ListenerRoutes);
            (_) ->
                ok
        end,
        Terms
    ).

-doc """
Formats runtime errors raised with an `error_info` annotation pointing at
this module. Picked up by `erl_error:format_exception/3`.
""".
-spec format_error(Reason, Stacktrace) -> ErrorInfo when
    Reason :: term(),
    Stacktrace :: [tuple()],
    ErrorInfo :: #{general := iolist()}.
format_error(https_requires_tls, [{_M, _F, _Args, _Info} | _]) ->
    #{general => "scheme => https requires tls => [...] in opts or proto_opts.tls => [...]"};
format_error(tls_with_http_scheme, [{_M, _F, _Args, _Info} | _]) ->
    #{
        general =>
            "tls => [...] implies https; drop scheme => http (or drop tls) "
            "instead of serving the configured certs' traffic in cleartext"
    };
format_error({unsupported_transport_opt, Opt}, _ST) ->
    #{
        general =>
            io_lib:format(
                "transport_opts accepts only {port, Port}; ~tp would be silently "
                "dropped. Roadrunner takes listener/socket options (ip, "
                "socket_backlog, ...) directly in proto_opts, which is merged "
                "into the listener opts wholesale.",
                [Opt]
            )
    }.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

start_new(Name, Opts0) ->
    %% Arizona does not list roadrunner as an application (a server-less consumer
    %% never boots it), so the adapter starts it on its first listener.
    {ok, _} = application:ensure_all_started(roadrunner),
    %% Resolve `{env, ...}` references (port, tls paths, proto_opts, ...) before
    %% reading any option. `resolve/1` leaves the route tuples untouched, so
    %% `Routes` is unchanged; it is idempotent, so a caller that already resolved
    %% loses nothing.
    #{routes := Routes} = Opts = arizona_config:resolve(Opts0),
    %% Thread the listener's `error_page` through BuildOpts so it is baked into
    %% each live route's state (read back by `arizona_http` on a render crash),
    %% rather than a single global term two listeners would fight over.
    BuildOpts = #{
        compress => maps:get(compress, Opts, true),
        error_page => maps:get(error_page, Opts, {arizona_error_page, render})
    },
    Port = port_from_opts(Opts),
    UserProtoOpts = maps:get(proto_opts, Opts, #{}),
    %% Build the route table and validate (and inject) TLS before any
    %% persistent_term write, so a misconfig crashes cleanly instead of leaving
    %% a half-written dispatch behind.
    ListenerOpts = maybe_inject_tls(
        UserProtoOpts#{
            port => Port,
            routes => arizona_roadrunner_router:routes(Routes, BuildOpts)
        },
        Opts
    ),
    start_listener(Name, ListenerOpts, {Routes, BuildOpts}).

%% The routes are written BEFORE the listener starts so its first WS upgrade
%% already resolves, which is also why a failed start must put back whatever
%% was there: a stash for a listener that never came up sends
%% `recompile_routes/0` to a dead process, and its `{reload, ...}` path would
%% have every page connect to a reload endpoint nothing serves.
start_listener(Name, ListenerOpts, Stash) ->
    Previous = persistent_term:get({?ROUTES_KEY, Name}, undefined),
    PreviousReloadUrl = persistent_term:get(arizona_reload_url, undefined),
    PreviousDevMcp = persistent_term:get(arizona_dev_mcp_route, undefined),
    ok = put_routes(Name, Stash),
    case roadrunner:start_listener(Name, ListenerOpts) of
        {ok, _Pid} = Ok ->
            ok = arizona_server_registry:register(?MODULE),
            Ok;
        {error, _Reason} = Error ->
            ok = restore_routes(Name, Previous),
            ok = restore_reload_url(PreviousReloadUrl),
            ok = restore_dev_mcp_route(PreviousDevMcp),
            Error
    end.

%% Store this listener's WS dispatch under its own `{dispatch, Name}` key so a
%% second listener does not overwrite it, and stash {Routes, BuildOpts}
%% together so `recompile_routes/0` can replay the user's original build-time
%% choices (e.g. `compress => false`) on hot reload.
put_routes(Name, {Routes, BuildOpts} = Stash) ->
    ok = arizona_roadrunner_router:compile_routes(Routes, BuildOpts, Name),
    persistent_term:put({?ROUTES_KEY, Name}, Stash).

restore_routes(Name, undefined) ->
    true = persistent_term:erase({?ROUTES_KEY, Name}),
    arizona_roadrunner_router:forget_routes(Name);
restore_routes(Name, Previous) ->
    put_routes(Name, Previous).

restore_reload_url(undefined) ->
    persistent_term:erase(arizona_reload_url),
    ok;
restore_reload_url(Url) ->
    persistent_term:put(arizona_reload_url, Url).

restore_dev_mcp_route(undefined) ->
    persistent_term:erase(arizona_dev_mcp_route),
    ok;
restore_dev_mcp_route(Mounted) ->
    persistent_term:put(arizona_dev_mcp_route, Mounted).

%% The dev terms are node-wide, written by whichever listener compiled a reload
%% or dev MCP route, so a stopped listener's survive it unless they are
%% recomputed from the listeners still stashed. Left behind, a production
%% listener started later in the same node serves the dev error page, stack
%% traces included, on every render crash.
forget_dev_terms() ->
    Remaining = lists:append([
        Routes
     || {{?ROUTES_KEY, _Name}, {Routes, _BuildOpts}} <- persistent_term:get()
    ]),
    ok = keep_reload_url([Path || {reload, Path, _Opts} <- Remaining]),
    keep_dev_mcp_route([Path || {mcp, Path, arizona_dev_mcp, _Opts} <- Remaining]).

keep_reload_url([]) ->
    restore_reload_url(undefined);
keep_reload_url(Paths) ->
    Current = persistent_term:get(arizona_reload_url, undefined),
    case lists:member(Current, Paths) of
        true -> ok;
        false -> restore_reload_url(lists:last(Paths))
    end.

keep_dev_mcp_route([]) -> restore_dev_mcp_route(undefined);
keep_dev_mcp_route([_ | _]) -> restore_dev_mcp_route(true).

%% A listener can be gone while its stash survives (stopped behind
%% `stop/1`'s back, or its supervisor gave up on it); one such listener must
%% not abort the reload for every listener after it.
reload_listener_routes(Name, ListenerRoutes) ->
    try
        roadrunner_listener:reload_routes(Name, ListenerRoutes)
    catch
        exit:{noproc, _} -> ok
    end.

%% The listen port, by deliberate precedence: `transport_opts`'s `{port, Port}`
%% (the explicit sugar) wins, then `proto_opts.port`, then the 4040 default -- a
%% present-but-portless `transport_opts` no longer silently clobbers
%% `proto_opts.port`. `transport_opts` accepts ONLY `{port, Port}`: roadrunner
%% takes every other listener/socket option (`ip`, `socket_backlog`, ...)
%% directly in its flat listener-opts map, which `proto_opts` already merges
%% into wholesale -- so any other entry here would be silently dropped, and
%% errors up front instead (before any persistent_term is written).
port_from_opts(Opts) ->
    TransportOpts = maps:get(transport_opts, Opts, []),
    ok = check_transport_opts(TransportOpts),
    case lists:keyfind(port, 1, TransportOpts) of
        {port, Port} ->
            Port;
        false ->
            ProtoOpts = maps:get(proto_opts, Opts, #{}),
            maps:get(port, ProtoOpts, 4040)
    end.

check_transport_opts([]) ->
    ok;
check_transport_opts([{port, _Port} | Rest]) ->
    check_transport_opts(Rest);
check_transport_opts([Opt | _Rest]) ->
    erlang:error(
        {unsupported_transport_opt, Opt}, [Opt], [{error_info, #{module => ?MODULE}}]
    ).

%% Translate the `scheme => https` shorthand into roadrunner's
%% `tls => [...]` listener opt. Top-level `tls` overrides
%% `proto_opts.tls` (which has already flowed into `ListenerOpts`).
%% Roadrunner turns TLS on from the `tls` listener opt alone, so `tls`
%% with no `scheme` means https; dropping it (the old behaviour) served
%% the configured certs' traffic in cleartext on the same port, the same
%% silent-downgrade footgun as `scheme => https` with no certs. Both
%% halves of the contradiction now fail loudly instead: no certs for
%% https, and an explicit `scheme => http` beside certs.
maybe_inject_tls(ListenerOpts, #{scheme := https, tls := Tls}) ->
    ListenerOpts#{tls => Tls};
maybe_inject_tls(#{tls := _} = ListenerOpts, #{scheme := https}) ->
    ListenerOpts;
maybe_inject_tls(_ListenerOpts, #{scheme := https} = Opts) ->
    erlang:error(https_requires_tls, [Opts], [{error_info, #{module => ?MODULE}}]);
maybe_inject_tls(_ListenerOpts, #{scheme := http, tls := _} = Opts) ->
    erlang:error(tls_with_http_scheme, [Opts], [{error_info, #{module => ?MODULE}}]);
maybe_inject_tls(ListenerOpts, #{tls := Tls}) ->
    ListenerOpts#{tls => Tls};
maybe_inject_tls(ListenerOpts, _Opts) ->
    ListenerOpts.
