-module(arizona_roadrunner_router).
-moduledoc """
Compiles a list of Arizona route specs into a roadrunner route table.

The resulting compiled routes are stored in `persistent_term` under the
listener-scoped key `{arizona_roadrunner_dispatch, Name}` -- one entry per
listener, so `arizona_roadrunner_req:resolve_route/3` resolves a WS
upgrade/navigate against the routes of the listener that accepted it, and two
listeners do not clobber each other. Calling `compile_routes/3` again replaces
the previous compiled set, which is how
`arizona_roadrunner_server:recompile_routes/0` picks up new routes after a hot
reload without restarting the listener.

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

An `{asset, Path, ...}` route compiles to `Path/*path`, so the catch-all is generated
rather than written and is invisible in the route list. Matching is first-match-wins
in list order, which makes an asset path listed ahead of a longer one it prefixes
swallow that one -- `~"/static"` before `~"/static/arizona"` serves, and 404s, every
request meant for the latter. Order the longer prefix first.

`{reload, ...}` also stashes the path in the `arizona_reload_url`
persistent term so the dev error page can build the SSE connect URL.
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([compile_routes/3]).
-export([forget_routes/1]).
-export([routes/1]).
-export([routes/2]).
-export([format_error/2]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

%% routes/1 is a public convenience wrapper; the in-tree callers all use
%% routes/2 (the listener's own route table). Kept exported for downstream users.
-ignore_xref([routes/1]).
%% Called by `erl_error` via the `error_info` annotation, not directly.
-ignore_xref([format_error/2]).

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
    cache_control => binary(),
    %% HTTP response transforms ONLY (`arizona_middleware:cors/1`, ...): an
    %% asset route runs no Arizona pipeline, so a request-to-bindings step
    %% here has nothing to run it and route compilation fails loudly.
    middlewares => [arizona_middleware:http_transform()]
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
    %% Arizona pipeline steps and HTTP response transforms, split at
    %% route-compile time (see split_middlewares/1).
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
Compiles `Routes` into a roadrunner route table and stores it under the
listener-scoped persistent term key `{arizona_roadrunner_dispatch, Name}`, so
each listener's WS upgrade/navigate (`arizona_roadrunner_req:resolve_route/3`)
resolves against its own routes. Replaces any previous compiled set atomically.
This is the form the server boot/recompile path uses.

`BuildOpts` threads build-time options through to per-route expansion:

- `compress` — when `true` (default), `roadrunner_compress` is
  attached as a per-route middleware on `live` and `asset` routes.
  WS, dev SSE reload, and controller (verb/`match`) routes are not compressed.
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

-doc """
Formats route-compilation errors into a human-readable message. Picked up by
`erl_error:format_exception/3` via the `error_info` annotation at the raise site.
""".
-spec format_error(Reason, Stacktrace) -> ErrorInfo when
    Reason :: term(),
    Stacktrace :: [tuple()],
    ErrorInfo :: #{general := iolist()}.
format_error({asset_middleware_not_transform, Steps}, [{_M, _F, _Args, _Info} | _]) ->
    #{
        general => io_lib:format(
            "an asset route runs no Arizona middleware pipeline, so these "
            "`middlewares` entries would never run: ~tp. Asset routes accept "
            "HTTP response transforms only (arizona_middleware:cors/1, "
            "security_headers/0,1, ...).",
            [Steps]
        )
    };
format_error(wildcard_in_method_list, [{_M, _F, _Args, _Info} | _]) ->
    #{
        general =>
            "'*' (any method) is only valid as the whole method spec: "
            "{match, '*', Path, Handler, Opts}. Inside a method list -- or "
            "spelled as the binary ~\"*\" -- it would match no real method; "
            "list explicit verbs, or use '*' alone."
    }.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% Arizona's per-route data lives under the `arizona` key inside the
%% route's `state` so roadrunner's pipeline does not interpret arizona's
%% middleware list as its own (incompatible signatures). A route's single
%% `middlewares` opt may hold both kinds -- request-to-bindings steps and
%% HTTP response transforms (`arizona_middleware:etag/0`, ...) -- and is
%% split here at compile time: steps stay in `state.arizona`, transforms
%% map to roadrunner middlewares at the top-level `middlewares` key, where
%% `roadrunner_router:compile/2` bakes them into the pipeline closure
%% (with the framework's `roadrunner_compress` prepended outermost --
%% with_compress/2).
route_to_roadrunner({live, Path, Handler, Opts}, BuildOpts) ->
    %% A live route is GET-only (the page render) plus HEAD; the WebSocket
    %% upgrade rides its own `{ws, ...}` route. A non-GET to a live path gets 405.
    {Transforms, Steps} = split_middlewares(Opts),
    [
        with_compress(
            with_transforms(
                #{
                    path => Path,
                    handler => arizona_roadrunner_http,
                    methods => [~"GET", ~"HEAD"],
                    state => #{arizona => build_live_meta(Handler, Opts, Steps, BuildOpts)}
                },
                Transforms
            ),
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
    %% No Arizona pipeline runs on an asset route, so a request-to-bindings
    %% step in its `middlewares` would silently never run -- fail loudly.
    case split_middlewares(Opts) of
        {Transforms, []} ->
            [
                with_compress(
                    with_transforms(
                        #{
                            path => <<Path/binary, "/*path">>,
                            handler => roadrunner_static,
                            state => asset_state(Dir, Opts)
                        },
                        Transforms
                    ),
                    BuildOpts
                )
            ];
        {_Transforms, Steps} ->
            error({asset_middleware_not_transform, Steps}, none, [
                {error_info, #{module => ?MODULE}}
            ])
    end.

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
build_live_meta(Handler, Opts, Steps, BuildOpts) ->
    #{
        handler => Handler,
        layouts => maps:get(layouts, Opts, []),
        bindings => maps:get(bindings, Opts, #{}),
        on_mount => maps:get(on_mount, Opts, []),
        middlewares => with_origin_check(Opts, Steps),
        error_page => maps:get(error_page, BuildOpts, {arizona_error_page, render})
    }.

%% Build the roadrunner route entry shared by every controller shape (verb tags
%% and `match`). `arizona_roadrunner_controller` runs the Arizona middleware
%% pipeline (CSRF check_origin on by default), restores the app `state`, then
%% dispatches Handler:Action/1 (default `handle`). `Methods` is roadrunner's
%% method allowlist (`undefined` = any method).
controller_route(Methods, Path, Handler, Opts) ->
    {Transforms, Steps} = split_middlewares(Opts),
    [
        with_transforms(
            #{
                path => Path,
                handler => arizona_roadrunner_controller,
                methods => Methods,
                state => #{
                    arizona => #{
                        handler => Handler,
                        action => maps:get(action, Opts, handle),
                        state => maps:get(state, Opts, #{}),
                        middlewares => with_origin_check(Opts, Steps)
                    }
                }
            },
            Transforms
        )
    ].

%% Normalize a verb tag or `{match, ...}` spec into roadrunner's uppercase
%% method-binary allowlist, or `undefined` (any method) for `'*'`.
normalize_methods('*') ->
    undefined;
normalize_methods(Methods) when is_list(Methods) ->
    with_head([method_bin(M) || M <- Methods]);
normalize_methods(Method) ->
    with_head([method_bin(Method)]).

%% `'*'` (any method) is only meaningful as the WHOLE `{match, ...}` spec, where
%% `normalize_methods/1` turns it into roadrunner's "no allowlist". Reaching here
%% -- inside a method list, or as the binary spelling `~"*"` -- it would silently
%% normalize to the literal method `~"*"`, which no real request carries (while
%% the 405 Allow header advertised `*`), so fail loudly at route compilation
%% instead, mirroring the unknown-verb-tag failure.
method_bin('*') ->
    erlang:error(wildcard_in_method_list, ['*'], [{error_info, #{module => ?MODULE}}]);
method_bin(~"*") ->
    erlang:error(wildcard_in_method_list, [~"*"], [{error_info, #{module => ?MODULE}}]);
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
    %% Prepend, never set: the route may already carry response transforms,
    %% and compress belongs OUTERMOST (a middleware list runs head-outermost)
    %% so a transform -- an etag, say -- sees the response body BEFORE
    %% compress re-encodes it. That is the composition the etag's weak
    %% validator is designed for: the tag stays representation-independent
    %% and a 304 still flows out through compress untouched. An explicit
    %% `arizona_middleware:compress()` in the list keeps its written position
    %% instead of being doubled.
    Mws = maps:get(middlewares, Route, []),
    case lists:member(roadrunner_compress, Mws) of
        true -> Route;
        false -> Route#{middlewares => [roadrunner_compress | Mws]}
    end.

%% Split a route's single `middlewares` opt into its HTTP response transforms
%% (mapped to roadrunner middlewares, order preserved) and its Arizona
%% pipeline steps. Order across the two kinds has phase semantics, not list
%% semantics: transforms wrap the whole exchange, steps run inside it.
split_middlewares(Opts) ->
    {Transforms, Steps} = lists:partition(
        fun
            ({http_transform, _}) -> true;
            (_) -> false
        end,
        maps:get(middlewares, Opts, [])
    ),
    {[transform_to_roadrunner(T) || T <- Transforms], Steps}.

transform_to_roadrunner({http_transform, etag}) ->
    roadrunner_etag;
transform_to_roadrunner({http_transform, compress}) ->
    roadrunner_compress;
transform_to_roadrunner({http_transform, security_headers}) ->
    roadrunner_security_headers;
transform_to_roadrunner({http_transform, {cors, Config}}) ->
    {roadrunner_cors, Config};
transform_to_roadrunner({http_transform, {security_headers, Config}}) ->
    {roadrunner_security_headers, Config};
transform_to_roadrunner({http_transform, {custom, Entry}}) ->
    Entry.

with_transforms(Route, []) -> Route;
with_transforms(Route, Transforms) -> Route#{middlewares => Transforms}.
