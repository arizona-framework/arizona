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
| `{asset, Path, {dir, Dir}}` | Static files from directory | `arizona_roadrunner_static` |
| `{asset, Path, {priv_dir, App, Sub}}` | Static files from app priv | `arizona_roadrunner_static` |
| `{controller, Path, Handler, State}` | Plain roadrunner handler | `Handler` |
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
    | {controller, path(), module(), term()}
    | {reload, path(), map()}.

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
    Compiled = roadrunner_router:compile(RoadrunnerRoutes),
    persistent_term:put(?DISPATCH_KEY, Compiled),
    ok.

-doc """
Translates a list of Arizona routes into roadrunner's
`{Path, Handler, Opts}` 3-tuples without compiling. Used by the
listener boot path so the live and recompile flows share the same
expansion logic.
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

%% Arizona route_opts are wrapped under the `arizona` key so roadrunner's
%% own pipeline (which auto-runs anything keyed `middlewares` from
%% `route_opts`) does not try to invoke arizona's middleware list with
%% the wrong signature. Roadrunner-side per-route middlewares (e.g.
%% `roadrunner_compress`) live at the top level under the `middlewares`
%% key, where `roadrunner_conn:route_middlewares/1` reads them.
route_to_roadrunner({live, Path, Handler, Opts}, BuildOpts) ->
    [
        {Path, arizona_roadrunner_http,
            with_compress(
                #{
                    arizona => #{
                        handler => Handler,
                        layouts => maps:get(layouts, Opts, []),
                        bindings => maps:get(bindings, Opts, #{}),
                        on_mount => maps:get(on_mount, Opts, []),
                        middlewares => maps:get(middlewares, Opts, [])
                    }
                },
                BuildOpts
            )}
    ];
route_to_roadrunner({ws, Path, Opts}, _BuildOpts) ->
    [{Path, arizona_roadrunner_ws, #{arizona => Opts}}];
route_to_roadrunner({asset, Path, {dir, Dir}}, BuildOpts) ->
    asset_route(Path, Dir, BuildOpts);
route_to_roadrunner({asset, Path, {priv_dir, App, SubDir}}, BuildOpts) ->
    asset_route(Path, filename:join(code:priv_dir(App), SubDir), BuildOpts);
route_to_roadrunner({controller, Path, Handler, State}, _BuildOpts) ->
    [{Path, Handler, State}];
route_to_roadrunner({reload, Path, Opts}, _BuildOpts) ->
    persistent_term:put(arizona_reload_url, Path),
    [{Path, arizona_roadrunner_reload, #{arizona => Opts}}].

asset_route(Path, Dir, BuildOpts) ->
    [
        {
            <<Path/binary, "/*path">>,
            arizona_roadrunner_static,
            with_compress(#{arizona => #{dir => Dir}}, BuildOpts)
        }
    ].

%% Attach roadrunner_compress as a per-route middleware when the build-time
%% `compress` flag is on. Reads at request time via
%% `roadrunner_conn:route_middlewares/1`.
with_compress(RouteOpts, #{compress := false}) ->
    RouteOpts;
with_compress(RouteOpts, _BuildOpts) ->
    RouteOpts#{middlewares => [roadrunner_compress]}.
