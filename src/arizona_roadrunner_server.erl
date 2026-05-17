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
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([start/2]).
-export([stop/1]).
-export([recompile_routes/0]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

-ignore_xref([start/2, stop/1]).

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
    ok = arizona_roadrunner_router:compile_routes(Routes, BuildOpts),
    %% Stash {Routes, BuildOpts} together so `recompile_routes/0` can
    %% replay the user's original build-time choices (e.g.
    %% `compress => false`) on hot reload.
    persistent_term:put({?ROUTES_KEY, Name}, {Routes, BuildOpts}),
    ErrorPage = maps:get(error_page, Opts, {arizona_error_page, render}),
    persistent_term:put(arizona_error_page, ErrorPage),
    Port = port_from_opts(Opts),
    UserProtoOpts = maps:get(proto_opts, Opts, #{}),
    ListenerOpts = UserProtoOpts#{
        port => Port,
        routes => arizona_roadrunner_router:routes(Routes, BuildOpts)
    },
    ListenerOpts1 = maybe_inject_tls(ListenerOpts, Opts),
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
%% roadrunner's `tls => [...]` listener opt. Users supplying TLS opts
%% directly via `proto_opts.tls` get them passed through unchanged.
maybe_inject_tls(ListenerOpts, #{scheme := https, tls := Tls}) ->
    ListenerOpts#{tls => Tls};
maybe_inject_tls(ListenerOpts, _Opts) ->
    ListenerOpts.
