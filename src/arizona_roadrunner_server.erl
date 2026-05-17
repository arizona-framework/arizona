-module(arizona_roadrunner_server).
-moduledoc """
Boots a roadrunner listener wired up with Arizona routes.

`start/2` compiles the route list, stashes the originals so they
can be re-compiled later, configures the dispatch persistent term,
and launches a roadrunner listener under `roadrunner_sup` via
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
- `transport_opts` -- listener opts, currently `[{port, Port}]`
  (default `[{port, 4040}]`)
- `proto_opts` -- map of roadrunner listener opts merged on top of
  the dispatch (e.g. `max_clients`, `max_content_length`,
  `request_timeout`, `tls`, `slot_reconciliation`)
- `compress` -- `true` (default) or `false`. When true,
  `roadrunner_compress` is attached to `live` and `asset` routes so
  HTML pages and static assets above the threshold are gzip/deflate
  encoded per `Accept-Encoding`. The dev SSE reload stream and the
  WebSocket upgrade response are intentionally **not** compressed.
  Set to `false` if you have an upstream proxy doing compression
  already.

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

-export([start/2]).
-export([stop/1]).
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
start(Name, #{routes := Routes} = Opts) ->
    BuildOpts = #{compress => maps:get(compress, Opts, true)},
    Port = port_from_opts(Opts),
    UserProtoOpts = maps:get(proto_opts, Opts, #{}),
    ListenerOpts = UserProtoOpts#{
        port => Port,
        routes => arizona_roadrunner_router:routes(Routes, BuildOpts)
    },
    %% Validate (and inject) TLS before any persistent_term writes so an
    %% https-without-tls misconfig crashes cleanly instead of leaving a
    %% half-written dispatch behind.
    ListenerOpts1 = maybe_inject_tls(ListenerOpts, Opts),
    ok = arizona_roadrunner_router:compile_routes(Routes, BuildOpts),
    %% Stash {Routes, BuildOpts} together so `recompile_routes/0` can
    %% replay the user's original build-time choices (e.g.
    %% `compress => false`) on hot reload.
    persistent_term:put({?ROUTES_KEY, Name}, {Routes, BuildOpts}),
    ErrorPage = maps:get(error_page, Opts, {arizona_error_page, render}),
    persistent_term:put(arizona_error_page, ErrorPage),
    roadrunner:start_listener(Name, ListenerOpts1).

-doc """
Stops the listener named `Name` and clears its persistent terms.
Returns `{error, not_found}` if `Name` is not a running listener.
""".
-spec stop(Name) -> ok | {error, not_found} when
    Name :: atom().
stop(Name) ->
    persistent_term:erase({?ROUTES_KEY, Name}),
    persistent_term:erase(arizona_error_page),
    roadrunner:stop_listener(Name).

-doc """
Recompiles every Arizona-managed dispatch by walking persistent
terms. Called by the dev hot reloader after a successful recompile.

Refreshes two tables in lockstep:

- `arizona_roadrunner_dispatch` (read by
  `arizona_roadrunner_req:resolve_route/3` on WS navigate), and
- the listener's own compiled route table (read by roadrunner on
  every incoming HTTP request).

Both must move together — otherwise WS navigate sees new routes
while a fresh HTTP request still 404s against the listener's stale
table.
""".
-spec recompile_routes() -> ok.
recompile_routes() ->
    Terms = persistent_term:get(),
    lists:foreach(
        fun
            ({{?ROUTES_KEY, Name}, {Routes, BuildOpts}}) ->
                ok = arizona_roadrunner_router:compile_routes(Routes, BuildOpts),
                ListenerRoutes = arizona_roadrunner_router:routes(Routes, BuildOpts),
                ok = roadrunner_listener:reload_routes(Name, ListenerRoutes);
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
    #{general => "scheme => https requires tls => [...] in opts or proto_opts.tls => [...]"}.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% Pull the port out of either the cowboy-style `transport_opts`
%% proplist (kept for config-shape parity) or directly from
%% `proto_opts.port`. Default 4040.
port_from_opts(Opts) ->
    case maps:get(transport_opts, Opts, undefined) of
        undefined ->
            ProtoOpts = maps:get(proto_opts, Opts, #{}),
            maps:get(port, ProtoOpts, 4040);
        TransportOpts when is_list(TransportOpts) ->
            proplists:get_value(port, TransportOpts, 4040)
    end.

%% Translate the cowboy-style `scheme => https` shorthand into
%% roadrunner's `tls => [...]` listener opt. Top-level `tls` overrides
%% `proto_opts.tls` (which has already flowed into `ListenerOpts`).
%% Asking for `scheme => https` with no TLS config anywhere fails
%% loudly — silently downgrading to plain HTTP on the same port is a
%% security footgun.
maybe_inject_tls(ListenerOpts, #{scheme := https, tls := Tls}) ->
    ListenerOpts#{tls => Tls};
maybe_inject_tls(#{tls := _} = ListenerOpts, #{scheme := https}) ->
    ListenerOpts;
maybe_inject_tls(_ListenerOpts, #{scheme := https} = Opts) ->
    erlang:error(https_requires_tls, [Opts], [{error_info, #{module => ?MODULE}}]);
maybe_inject_tls(ListenerOpts, _Opts) ->
    ListenerOpts.
