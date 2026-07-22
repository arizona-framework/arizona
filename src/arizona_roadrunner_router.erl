-module(arizona_roadrunner_router).
-moduledoc """
Compiles a list of Arizona route specs into a roadrunner route table.

The resulting compiled routes are stored in `persistent_term`. The
name-scoped `compile_routes/3` writes them under `{arizona_roadrunner_dispatch,
Name}` -- one entry per listener, so `arizona_roadrunner_req:resolve_route/3`
resolves a WS upgrade/navigate against the routes of the listener that accepted
it, and two listeners no longer clobber each other. The nameless
`compile_routes/1,2` write the single global `arizona_roadrunner_dispatch` key
(the convenience form). Calling either again replaces the previous compiled set,
which is how `arizona_roadrunner_server:recompile_routes/0` picks up new routes
after a hot reload without restarting the listener.

## Route shapes

| Tag | Description | Roadrunner handler |
|-----|-------------|--------------------|
| `{live, Path, Handler, Opts}` | Arizona stateful page | `arizona_roadrunner_http` |
| `{ws, Path, Opts}` | WebSocket endpoint | `arizona_roadrunner_ws` |
| `{asset, Path, {dir, Dir}[, Opts]}` | Static files from directory | `roadrunner_static` |
| `{asset, Path, {priv_dir, App, Sub}[, Opts]}` | Static files from app priv | `roadrunner_static` |
| `{Verb, Path, Handler, Opts}` | Single-verb controller | `arizona_roadrunner_controller` |
| `{match, Spec, Path, Handler, Opts}` | Multi/custom methods | `arizona_roadrunner_controller` |
| `{mcp, Path, Handler, Opts}` | MCP (Model Context Protocol) server | `arizona_mcp_handler` |
| `{reload, Path, Opts}` | Dev SSE reload endpoint | `arizona_roadrunner_reload` |

`Verb` is `get`/`post`/`put`/`patch`/`delete`/`head`/`options`; `match`'s `Spec` is a
single verb, a list of verbs, a custom uppercase method binary, or `'*'` (any method).

An `{asset, ...}` route's optional `Opts` is `#{cache_control => binary()}` -- a
`Cache-Control` header set verbatim on cacheable responses (off by default); use it
only for content-hashed assets, never arizona's own stable bundle filenames.

`{reload, ...}` also stashes the path in the `arizona_reload_url`
persistent term so the dev error page can build the SSE connect URL.
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([compile_routes/1]).
-export([compile_routes/2]).
-export([compile_routes/3]).
-export([forget_routes/1]).
-export([routes/1]).
-export([routes/2]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

%% compile_routes/1,2 + routes/1 are public convenience wrappers; the in-tree
%% callers all use the name-scoped compile_routes/3 (WS dispatch keyed per
%% listener) and routes/2 (the listener's own route table). Keep the nameless
%% shapes exported for downstream users.
-ignore_xref([compile_routes/1, compile_routes/2, routes/1]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([path/0]).
-export_type([route/0]).
-export_type([asset_source/0]).
-export_type([asset_opts/0]).
-export_type([method/0]).
-export_type([method_spec/0]).
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
    | {asset, path(), asset_source()}
    | {asset, path(), asset_source(), asset_opts()}
    | {method(), path(), module(), controller_opts()}
    | {match, method_spec(), path(), module(), controller_opts()}
    | {mcp, path(), module(), arizona_mcp_route_opts()}
    | {reload, path(), map()}.

%% Where an `{asset, ...}` route's files live: a directory path, or a
%% sub-path under an application's `priv` directory.
-nominal asset_source() ::
    {dir, file:filename_all()}
    | {priv_dir, atom(), file:filename_all()}.

%% Options for an `{asset, ...}` route. `cache_control` sets a `Cache-Control`
%% header (verbatim) on every cacheable static response; off by default. Use it
%% only for content-hashed assets (e.g. `~"public, max-age=31536000, immutable"`)
%% -- arizona's own bundle filenames are stable, not hashed, so an `immutable`
%% directive on them would pin a stale build past a deploy.
-nominal asset_opts() :: #{
    cache_control => binary()
}.

%% The verb-tag atoms a controller route may use as its first element
%% (sugar for a single-method allowlist). Custom or multi-method routes go
%% through `{match, MethodSpec, ...}`.
-nominal method() :: get | post | put | patch | delete | head | options.

%% The method argument of a `{match, ...}` route: a single verb, a list of
%% verbs, or the atom `'*'` for any method. A verb is an atom (upper-cased at
%% compile time -- the common ones are `method()`, but any atom works, e.g.
%% `move` -> `~"MOVE"`) or a custom uppercase method binary (`~"PROPFIND"`).
-nominal method_spec() :: atom() | binary() | [atom() | binary()].

-nominal controller_opts() :: #{
    %% State passed to the controller action (read via roadrunner_req:state/1).
    state => term(),
    %% Controller action function: dispatched as Handler:Action/1 (default `handle`).
    action => atom(),
    middlewares => [arizona_middleware:middleware()],
    %% CSRF Origin check is on by default; set false to opt this route out.
    check_origin => boolean(),
    _ => term()
}.

-nominal arizona_mcp_route_opts() :: #{
    origins => [binary()],
    auth => arizona_mcp_handler:auth_hook(),
    sessions => boolean(),
    max_sessions => pos_integer() | infinity,
    session_ttl_ms => pos_integer(),
    session_buffer_max => pos_integer(),
    %% Localhost gate (default `false`, safe-by-default like `origins`): unless
    %% `true`, refuse a request whose peer is not a loopback address, regardless of
    %% the listener's bind interface. `arizona_dev_mcp` relies on this for its
    %% always-on `eval` (RCE). Void behind a same-host proxy/tunnel -- use `auth`
    %% for non-direct remote exposure.
    allow_remote_access => boolean(),
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
  WS, dev SSE reload, and controller (verb/`match`) routes are not compressed.
""".
-spec compile_routes(Routes, BuildOpts) -> ok when
    Routes :: [route()],
    BuildOpts :: map().
compile_routes(Routes, BuildOpts) when is_map(BuildOpts) ->
    persistent_term:put(
        ?DISPATCH_KEY,
        roadrunner_router:compile(routes(Routes, BuildOpts), [])
    ),
    ok.

-doc """
Like `compile_routes/2` but stores the compiled set under the
listener-scoped key `{arizona_roadrunner_dispatch, Name}` instead of the
single global key, so each listener's WS upgrade/navigate
(`arizona_roadrunner_req:resolve_route/3`) resolves against its own routes.
This is the form the server boot/recompile path uses.
""".
-spec compile_routes(Routes, BuildOpts, Name) -> ok when
    Routes :: [route()],
    BuildOpts :: map(),
    Name :: atom().
compile_routes(Routes, BuildOpts, Name) when is_map(BuildOpts), is_atom(Name) ->
    persistent_term:put(
        {?DISPATCH_KEY, Name},
        roadrunner_router:compile(routes(Routes, BuildOpts), [])
    ),
    ok.

-doc """
Erases the listener-scoped dispatch key written by `compile_routes/3`.
Called by `arizona_roadrunner_server:stop/1` so a stopped listener leaves
no stale compiled routes behind.
""".
-spec forget_routes(Name) -> ok when
    Name :: atom().
forget_routes(Name) when is_atom(Name) ->
    persistent_term:erase({?DISPATCH_KEY, Name}),
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
    %% A live route is GET-only (the page render) plus HEAD; the WebSocket
    %% upgrade rides its own `{ws, ...}` route. A non-GET to a live path gets 405.
    [
        with_compress(
            #{
                path => Path,
                handler => arizona_roadrunner_http,
                methods => [~"GET", ~"HEAD"],
                state => #{arizona => build_live_meta(Handler, Opts, BuildOpts)}
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
route_to_roadrunner({asset, Path, Source}, BuildOpts) ->
    route_to_roadrunner({asset, Path, Source, #{}}, BuildOpts);
route_to_roadrunner({asset, Path, {dir, Dir}, Opts}, BuildOpts) ->
    asset_route(Path, Dir, Opts, BuildOpts);
route_to_roadrunner({asset, Path, {priv_dir, App, SubDir}, Opts}, BuildOpts) ->
    asset_route(Path, filename:join(code:priv_dir(App), SubDir), Opts, BuildOpts);
route_to_roadrunner({match, Spec, Path, Handler, Opts}, _BuildOpts) ->
    %% General controller route: a multi-verb list, a custom method binary,
    %% or `'*'` (any method) -- all normalized to roadrunner's allowlist.
    controller_route(normalize_methods(Spec), Path, Handler, Opts);
route_to_roadrunner({Verb, Path, Handler, Opts}, _BuildOpts) when
    Verb =:= get;
    Verb =:= post;
    Verb =:= put;
    Verb =:= patch;
    Verb =:= delete;
    Verb =:= head;
    Verb =:= options
->
    %% Single-verb controller sugar (`{post, ...}` etc.). The kind-tag clauses
    %% above match first, so `Verb` here is always an HTTP method atom; the
    %% explicit guard also makes an unknown verb tag fail loudly (no clause).
    controller_route(normalize_methods(Verb), Path, Handler, Opts);
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
%% sendfile plus ETag/`If-None-Match` (304), `Range`, precompressed-sibling
%% serving (nginx `brotli_static` / `gzip_static` style -- a `<file>.br` or
%% `<file>.gz` built by the asset pipeline is sent verbatim, brotli preferred,
%% when the client accepts it), and path-traversal/symlink guards. Its state is
%% `#{dir => Dir}` (not arizona-namespaced), optionally carrying `cache_control`,
%% and it reads the `*path` wildcard binding, which arizona's route provides.
asset_route(Path, Dir, Opts, BuildOpts) ->
    [
        with_compress(
            #{
                path => <<Path/binary, "/*path">>,
                handler => roadrunner_static,
                state => asset_state(Dir, Opts)
            },
            BuildOpts
        )
    ].

%% roadrunner_static's state is a plain `#{dir => Dir}`; `cache_control` is
%% threaded through only when the route declares it, so an asset route without
%% the opt produces byte-for-byte the same responses as before.
asset_state(Dir, #{cache_control := Value}) ->
    #{dir => Dir, cache_control => Value};
asset_state(Dir, _Opts) ->
    #{dir => Dir}.

%% `error_page` is a per-listener choice (the server's `error_page` opt, threaded
%% in via `BuildOpts`), baked into each live route's state so `arizona_http`'s
%% crash path reads the owning listener's error page from `Opts` -- no shared
%% global term that a second listener could clobber or `stop/1` erase.
build_live_meta(Handler, Opts, BuildOpts) ->
    #{
        handler => Handler,
        layouts => maps:get(layouts, Opts, []),
        bindings => maps:get(bindings, Opts, #{}),
        on_mount => maps:get(on_mount, Opts, []),
        middlewares => with_origin_check(Opts, maps:get(middlewares, Opts, [])),
        error_page => maps:get(error_page, BuildOpts, {arizona_error_page, render})
    }.

%% Build the roadrunner route entry shared by every controller shape (verb tags
%% and `match`). `arizona_roadrunner_controller` runs the Arizona middleware
%% pipeline (CSRF check_origin on by default), restores the app `state`, then
%% dispatches Handler:Action/1 (default `handle`). `Methods` is roadrunner's
%% method allowlist (`undefined` = any method).
controller_route(Methods, Path, Handler, Opts) ->
    [
        #{
            path => Path,
            handler => arizona_roadrunner_controller,
            methods => Methods,
            state => #{
                arizona => #{
                    handler => Handler,
                    action => maps:get(action, Opts, handle),
                    state => maps:get(state, Opts, #{}),
                    middlewares => with_origin_check(Opts, maps:get(middlewares, Opts, []))
                }
            }
        }
    ].

%% Normalize a verb tag or `{match, ...}` spec into roadrunner's uppercase
%% method-binary allowlist, or `undefined` (any method) for `'*'`.
normalize_methods('*') ->
    undefined;
normalize_methods(Methods) when is_list(Methods) ->
    with_head([method_bin(M) || M <- Methods]);
normalize_methods(Method) ->
    with_head([method_bin(Method)]).

method_bin(M) when is_atom(M) -> upper(atom_to_binary(M, utf8));
method_bin(M) when is_binary(M) -> upper(M).

%% HTTP methods are case-sensitive, conventionally uppercase, and roadrunner
%% compares byte-exact -- so normalize every declared method to uppercase.
upper(Bin) -> iolist_to_binary(string:uppercase(Bin)).

%% HEAD is a bodyless GET, so a GET allowlist answers HEAD too.
with_head(Methods) ->
    case lists:member(~"GET", Methods) andalso not lists:member(~"HEAD", Methods) of
        true -> Methods ++ [~"HEAD"];
        false -> Methods
    end.

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
