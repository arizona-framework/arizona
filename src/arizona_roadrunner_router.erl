-module(arizona_roadrunner_router).
-moduledoc """
Compiles a list of Arizona route specs into a roadrunner route table.

The resulting compiled routes are stored under the persistent term
key `arizona_roadrunner_dispatch`, where roadrunner listeners and
`arizona_roadrunner_req:resolve_route/3` look them up. Calling
`compile_routes/1` again replaces the previous compiled set, which
is how `arizona_roadrunner_server:recompile_routes/0` picks up new
routes after a hot reload without restarting the listener.

## Route shapes

| Tag | Description | Roadrunner handler |
|-----|-------------|--------------------|
| `{live, Path, Handler, Opts}` | Arizona stateful page | `arizona_roadrunner_http` |
| `{ws, Path, Opts}` | WebSocket endpoint | `arizona_roadrunner_ws` |
| `{asset, Path, {dir, Dir}}` | Static files from directory | `roadrunner_static` |
| `{asset, Path, {priv_dir, App, Sub}}` | Static files from app priv | `roadrunner_static` |
| `{controller, Path, Handler, Opts}` | Middleware-gated handler | `arizona_roadrunner_controller` |
| `{mcp, Path, Handler, Opts}` | MCP (Model Context Protocol) server | `arizona_mcp_handler` |
| `{reload, Path, Opts}` | Dev SSE reload endpoint | `arizona_roadrunner_reload` |

`{reload, ...}` also stashes the path in the `arizona_reload_url`
persistent term so the dev error page can build the SSE connect URL.
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([compile_routes/1]).
-export([compile_routes/2]).
-export([routes/1]).
-export([routes/2]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

%% compile_routes/1 + routes/1 are public convenience wrappers; the
%% in-tree callers all use the /2 form with build opts. Keep both shapes
%% exported for downstream users.
-ignore_xref([compile_routes/1, routes/1]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([path/0]).
-export_type([route/0]).
-export_type([controller_opts/0]).
-export_type([arizona_mcp_route_opts/0]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------

-define(DISPATCH_KEY, arizona_roadrunner_dispatch).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-nominal path() :: binary().

-nominal route() ::
    {live, path(), module(), arizona_live:route_opts()}
    | {ws, path(), map()}
    | {asset, path(), {dir, file:filename_all()}}
    | {asset, path(), {priv_dir, atom(), file:filename_all()}}
    | {controller, path(), module(), controller_opts()}
    | {mcp, path(), module(), arizona_mcp_route_opts()}
    | {reload, path(), map()}.

-nominal controller_opts() :: #{
    %% State passed to the controller's handle/1 (read via roadrunner_req:state/1).
    state => term(),
    middlewares => [arizona_middleware:middleware()],
    %% CSRF Origin check is on by default; set false to opt this route out.
    check_origin => boolean(),
    _ => term()
}.

-nominal arizona_mcp_route_opts() :: #{
    origins => [binary()],
    auth => arizona_mcp_handler:auth_hook(),
    sessions => boolean(),
    session_ttl_ms => pos_integer(),
    session_buffer_max => pos_integer(),
    _ => term()
}.

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc """
Compiles `Routes` into a roadrunner route table and stores it under
the persistent term key `arizona_roadrunner_dispatch`. Replaces any
previous compiled set atomically.
""".
-spec compile_routes(Routes) -> ok when
    Routes :: [route()].
compile_routes(Routes) ->
    compile_routes(Routes, #{}).

-doc """
Like `compile_routes/1` but threads a build-time options map through
to per-route expansion. Recognized opts:

- `compress` — when `true` (default), `roadrunner_compress` is
  attached as a per-route middleware on `live` and `asset` routes.
  WS, dev SSE reload, and controller routes are not compressed.
""".
-spec compile_routes(Routes, BuildOpts) -> ok when
    Routes :: [route()],
    BuildOpts :: map().
compile_routes(Routes, BuildOpts) when is_map(BuildOpts) ->
    RoadrunnerRoutes = lists:flatmap(
        fun(R) -> route_to_roadrunner(R, BuildOpts) end, Routes
    ),
    persistent_term:put(
        ?DISPATCH_KEY,
        roadrunner_router:compile(RoadrunnerRoutes, [])
    ),
    ok.

-doc """
Translates a list of Arizona routes into roadrunner's route entries
(map shape) without compiling. Used by the listener boot path so the
live and recompile flows share the same expansion logic.
""".
-spec routes(Routes) -> [roadrunner_router:route()] when
    Routes :: [route()].
routes(Routes) ->
    routes(Routes, #{}).

-doc "Like `routes/1` with build-time opts (see `compile_routes/2`).".
-spec routes(Routes, BuildOpts) -> [roadrunner_router:route()] when
    Routes :: [route()],
    BuildOpts :: map().
routes(Routes, BuildOpts) when is_map(BuildOpts) ->
    lists:flatmap(fun(R) -> route_to_roadrunner(R, BuildOpts) end, Routes).

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% Arizona's per-route data lives under the `arizona` key inside the
%% route's `state` so roadrunner's pipeline does not interpret arizona's
%% middleware list as its own (incompatible signatures). Roadrunner-side
%% per-route middlewares (e.g. `roadrunner_compress`) sit at the
%% top-level `middlewares` key of the map-shape route entry, where
%% `roadrunner_router:compile/2` bakes them into the pipeline closure.
route_to_roadrunner({live, Path, Handler, Opts}, BuildOpts) ->
    [
        with_compress(
            #{
                path => Path,
                handler => arizona_roadrunner_http,
                state => #{arizona => build_live_meta(Handler, Opts)}
            },
            BuildOpts
        )
    ];
route_to_roadrunner({ws, Path, Opts}, _BuildOpts) ->
    [
        #{
            path => Path,
            handler => arizona_roadrunner_ws,
            state => #{arizona => Opts}
        }
    ];
route_to_roadrunner({asset, Path, {dir, Dir}}, BuildOpts) ->
    asset_route(Path, Dir, BuildOpts);
route_to_roadrunner({asset, Path, {priv_dir, App, SubDir}}, BuildOpts) ->
    asset_route(Path, filename:join(code:priv_dir(App), SubDir), BuildOpts);
route_to_roadrunner({controller, Path, Handler, Opts}, _BuildOpts) ->
    %% Controllers run the Arizona middleware pipeline (CSRF check_origin on by
    %% default) via arizona_roadrunner_controller, which restores the app `state`
    %% before calling Handler:handle/1.
    [
        #{
            path => Path,
            handler => arizona_roadrunner_controller,
            state => #{
                arizona => #{
                    handler => Handler,
                    state => maps:get(state, Opts, #{}),
                    middlewares => with_origin_check(Opts, maps:get(middlewares, Opts, []))
                }
            }
        }
    ];
route_to_roadrunner({mcp, Path, Handler, Opts}, _BuildOpts) ->
    %% The handler module is folded into the opts so `arizona_mcp_handler`
    %% reads it from the per-route `arizona` state at request time, the
    %% same wrapping convention `live`/`reload` use. No compression
    %% middleware -- MCP replies must stay unbuffered for the later SSE path.
    [
        #{
            path => Path,
            handler => arizona_mcp_handler,
            state => #{arizona => Opts#{handler => Handler}}
        }
    ];
route_to_roadrunner({reload, Path, Opts}, _BuildOpts) ->
    persistent_term:put(arizona_reload_url, Path),
    [
        #{
            path => Path,
            handler => arizona_roadrunner_reload,
            state => #{arizona => Opts}
        }
    ].

%% Assets are served by roadrunner's built-in `roadrunner_static`: zero-copy
%% sendfile plus ETag/`If-None-Match` (304), `Range`, gzip-sibling serving
%% (nginx `gzip_static` style -- a `<file>.gz` built by the asset pipeline is
%% sent verbatim when the client accepts gzip), and path-traversal/symlink
%% guards. Its state is a plain `#{dir => Dir}` (not arizona-namespaced) and it
%% reads the `*path` wildcard binding, which arizona's route already provides.
asset_route(Path, Dir, BuildOpts) ->
    [
        with_compress(
            #{
                path => <<Path/binary, "/*path">>,
                handler => roadrunner_static,
                state => #{dir => Dir}
            },
            BuildOpts
        )
    ].

build_live_meta(Handler, Opts) ->
    #{
        handler => Handler,
        layouts => maps:get(layouts, Opts, []),
        bindings => maps:get(bindings, Opts, #{}),
        on_mount => maps:get(on_mount, Opts, []),
        middlewares => with_origin_check(Opts, maps:get(middlewares, Opts, []))
    }.

%% CSRF defense is on by default: prepend the check_origin step (covers the page
%% render and -- since arizona_ws:prepare runs the resolved route's middlewares -- the
%% WebSocket upgrade). Opt a route out with `check_origin => false`.
with_origin_check(#{check_origin := false}, Middlewares) ->
    Middlewares;
with_origin_check(_Opts, Middlewares) ->
    [{arizona_middleware, check_origin} | Middlewares].

%% Attach roadrunner_compress as a per-route middleware on the
%% map-shape route entry when the build-time `compress` flag is on.
with_compress(Route, #{compress := false}) ->
    Route;
with_compress(Route, _BuildOpts) ->
    Route#{middlewares => [roadrunner_compress]}.
