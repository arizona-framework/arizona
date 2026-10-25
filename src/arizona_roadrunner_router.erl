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
| `{mcp, Path, Handler, Opts}` | MCP server | `arizona_roadrunner_controller` |
| `{reload, Path, Opts}` | Dev SSE reload endpoint | `arizona_roadrunner_controller` |

`Verb` is `get`/`post`/`put`/`patch`/`delete`/`head`/`options`; `match`'s `Spec` is a
single verb, a list of verbs, a custom uppercase method binary, or `'*'` (any method).

An `{asset, ...}` route answers GET and HEAD only (plus `OPTIONS` when it carries a
CORS transform, for the preflight); any other method gets `405` + `Allow`.

An `{asset, ...}` route's optional `Opts` is `#{cache_control => binary()}` -- a
`Cache-Control` header set verbatim on cacheable responses (off by default); use it
only for content-hashed assets, never arizona's own stable bundle filenames.

An `{asset, Path, ...}` route compiles to `Path/*path`, so the catch-all is generated
rather than written and is invisible in the route list. Matching is first-match-wins
in list order, which makes an asset path listed ahead of a longer one it prefixes
swallow that one -- `~"/static"` before `~"/static/arizona"` serves, and 404s, every
request meant for the latter. Order the longer prefix first.

The dev-mode signals a `{reload, ...}` or `arizona_dev_mcp` route carries are
not written here: the server reports its running listeners to
`arizona_server_registry`, which keeps them in step. `routes/1,2` only expand,
so a listener start can build and validate its table before writing anything.
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([compile_routes/3]).
-export([forget_routes/1]).
-export([routes/1]).
-export([routes/2]).
-export([format_error/2]).
-export([options_reply/3]).

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

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------

-define(DISPATCH_KEY, arizona_roadrunner_dispatch).
%% A method token no route declares, so matching it reports a URL's Allow set.
-define(ALLOW_PROBE, ~"ARIZONA-ALLOW-PROBE").

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
    | {mcp, path(), module(), arizona_mcp_handler:route_opts()}
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
    %% State handed to the controller action, as its context's `state`.
    state => term(),
    %% Controller action function: dispatched as Handler:Action/2 (default `handle`).
    action => atom(),
    %% Arizona pipeline steps and HTTP response transforms, split at
    %% route-compile time (see split_middlewares/1).
    middlewares => [arizona_middleware:middleware()],
    %% CSRF Origin check is on by default; set false to opt this route out.
    check_origin => boolean(),
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
    %% Roadrunner serves every route kind, so classifying only refuses a term
    %% that is no route, naming the valid shapes.
    ok = lists:foreach(fun(R) -> true = is_atom(arizona_route:kind(R)) end, Routes),
    open_cors_options(lists:flatmap(fun(R) -> route_to_roadrunner(R, BuildOpts) end, Routes)).

-doc """
The innermost roadrunner middleware of a route whose `OPTIONS` was opened only
so its CORS transform can answer a preflight (see `open_cors_options/1`). The
transform short-circuits a preflight, so an `OPTIONS` reaching here is a plain
one, answered `204` with the `Allow` set a `405` on that URL would carry rather
than handed to a controller action or live page that never declared the method.
""".
-spec options_reply(Req, Next, State) -> roadrunner_handler:result() when
    Req :: roadrunner_req:request(),
    Next :: roadrunner_middleware:next(),
    State :: term().
options_reply(Req, Next, _State) ->
    case roadrunner_req:method(Req) of
        ~"OPTIONS" -> {{204, allow_header(Req), <<>>}, Req};
        _ -> Next(Req)
    end.

%% The URL's Allow set is whatever every route matching it declares, through
%% any pattern (`/u/:id` beside `/u/:name`, a literal beside a param, a
%% wildcard). Asking the listener's own table with a method no route declares
%% yields exactly the set its 405 would advertise. A route with no allowlist
%% matching too answers every method, which no Allow value can enumerate.
allow_header(Req) ->
    Compiled = persistent_term:get({?DISPATCH_KEY, roadrunner_req:listener_name(Req)}),
    case roadrunner_router:match(?ALLOW_PROBE, roadrunner_req:path(Req), Compiled) of
        {method_not_allowed, Allowed} -> [{~"allow", iolist_to_binary(lists:join(~", ", Allowed))}];
        {ok, _Handler, _Bindings, _Pipeline, _State} -> []
    end.

-doc """
Formats route-compilation errors into a human-readable message. Picked up by
`erl_error:format_exception/3` via the `error_info` annotation at the raise site.
""".
-spec format_error(Reason, Stacktrace) -> ErrorInfo when
    Reason :: term(),
    Stacktrace :: [tuple()],
    ErrorInfo :: #{general := iolist()}.
format_error(
    {cors_options_shadows_route, {CorsPos, CorsPath}, {ShadowedPos, ShadowedPath}},
    [{_M, _F, _Args, _Info} | _]
) ->
    #{
        general => io_lib:format(
            "route #~b (~ts) carries a CORS transform, so it must answer OPTIONS "
            "for the browser's preflight -- but route #~b (~ts) declares OPTIONS "
            "on URLs route #~b also matches, and would never be reached. List the "
            "OPTIONS route first, give it a path that does not overlap, or drop "
            "the cors transform from route #~b.",
            [CorsPos, CorsPath, ShadowedPos, ShadowedPath, CorsPos, CorsPos]
        )
    }.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% Arizona's per-route data lives under the `arizona` key inside the
%% route's `state` so roadrunner's pipeline does not interpret arizona's
%% middleware list as its own (incompatible signatures). A route's single
%% `middlewares` opt may hold both kinds -- request-to-bindings steps and
%% HTTP response transforms (`arizona_middleware:etag/0`, ...) -- which
%% `arizona_route` splits: steps (`arizona_route:steps/1`) stay in
%% `state.arizona`, transforms (`arizona_route:transforms/2`, compression
%% outermost) map to roadrunner middlewares at the top-level `middlewares`
%% key, where `roadrunner_router:compile/2` bakes them into the pipeline.
route_to_roadrunner({live, Path, _Handler, _Opts} = Route, BuildOpts) ->
    %% A live route is GET-only (the page render) plus HEAD; the WebSocket
    %% upgrade rides its own `{ws, ...}` route. A non-GET to a live path gets 405.
    [
        roadrunner_route(Route, BuildOpts, #{
            path => Path,
            handler => arizona_roadrunner_http,
            state => #{arizona => arizona_route:live(Route, BuildOpts)}
        })
    ];
route_to_roadrunner({ws, Path, Opts}, _BuildOpts) ->
    [
        #{
            path => Path,
            handler => arizona_roadrunner_ws,
            state => #{arizona => Opts}
        }
    ];
route_to_roadrunner({asset, _Path, _Source} = Route, BuildOpts) ->
    route_to_roadrunner(erlang:append_element(Route, #{}), BuildOpts);
route_to_roadrunner({asset, Path, _Source, Opts} = Route, BuildOpts) ->
    %% roadrunner_static serves the file whatever the method, so the allowlist
    %% is what makes a POST/DELETE to an asset path a 405.
    [
        roadrunner_route(Route, BuildOpts, #{
            path => <<Path/binary, "/*path">>,
            handler => roadrunner_static,
            state => asset_state(arizona_route:asset_dir(Route), Opts)
        })
    ];
route_to_roadrunner(Route, BuildOpts) ->
    %% A controller (verb tag or `{match, ...}`), or the MCP or dev reload
    %% endpoint, which are ones too; `arizona_roadrunner_controller` runs
    %% `arizona_controller:dispatch/3` on what `arizona_route:controller/1` names.
    true = lists:member(arizona_route:kind(Route), [controller, mcp, reload]),
    [
        roadrunner_route(Route, BuildOpts, #{
            path => arizona_route:path(Route),
            handler => arizona_roadrunner_controller,
            state => #{arizona => arizona_route:controller(Route)}
        })
    ].

%% The allowlist and the response transforms are `arizona_route`'s; mapped onto
%% roadrunner's `methods` (`undefined` = any) and per-route middlewares, with a
%% cors route's OPTIONS placeholder innermost (see cors_options/2).
roadrunner_route(Route, BuildOpts, Entry) ->
    Methods =
        case arizona_route:methods(Route) of
            any -> undefined;
            List -> List
        end,
    Transforms = [transform_to_roadrunner(T) || T <- arizona_route:transforms(Route, BuildOpts)],
    with_transforms(Entry#{methods => Methods}, cors_options(Methods, Transforms)).

%% Assets are served by roadrunner's built-in `roadrunner_static`: zero-copy
%% sendfile plus ETag/`If-None-Match` (304), `Range`, precompressed-sibling
%% serving (nginx `brotli_static` / `gzip_static` style -- a `<file>.br` or
%% `<file>.gz` built by the asset pipeline is sent verbatim, brotli preferred,
%% when the client accepts it), and path-traversal/symlink guards. Its state is
%% `#{dir => Dir}` (not arizona-namespaced), optionally carrying `cache_control`,
%% and it reads the `*path` wildcard binding, which arizona's route provides.
%% roadrunner_static's state is a plain `#{dir => Dir}`; `cache_control` is
%% threaded through only when the route declares it, so an asset route without
%% the opt produces byte-for-byte the same responses as before.
asset_state(Dir, #{cache_control := Value}) ->
    #{dir => Dir, cache_control => Value};
asset_state(Dir, _Opts) ->
    #{dir => Dir}.

transform_to_roadrunner({http_transform, etag}) ->
    roadrunner_etag;
transform_to_roadrunner({http_transform, compress}) ->
    roadrunner_compress;
transform_to_roadrunner({http_transform, security_headers}) ->
    roadrunner_security_headers;
transform_to_roadrunner({http_transform, {cors, Config}}) ->
    {roadrunner_cors, Config};
transform_to_roadrunner({http_transform, {security_headers, Config}}) ->
    {roadrunner_security_headers, Config}.

%% roadrunner enforces a route's method allowlist (405) before any per-route
%% middleware runs, so a CORS transform on a route that does not declare
%% `OPTIONS` would never see the preflight it exists to answer. Mark such a
%% route with a `cors_options` placeholder, innermost under the transform, for
%% `open_cors_options/1` to resolve once the whole table is known. A route that
%% declares `OPTIONS` itself, or has no allowlist (`'*'`), already reaches the
%% transform, and its handler owns the plain `OPTIONS`.
cors_options(Methods, Transforms) when is_list(Methods) ->
    case
        lists:keymember(roadrunner_cors, 1, Transforms) andalso
            not lists:member(~"OPTIONS", Methods)
    of
        true -> Transforms ++ [cors_options];
        false -> Transforms
    end;
cors_options(undefined, Transforms) ->
    Transforms.

%% Open `OPTIONS` on each marked route, with `options_reply/3` answering the
%% plain `OPTIONS` the transform passes through. When that would leave a later
%% route declaring `OPTIONS` itself unreachable (the cors route ahead of it
%% takes the method on every URL they share), fail with an error naming both:
%% keeping `OPTIONS` closed instead would 405 every preflight to the cors route
%% with nothing at boot to say why, and roadrunner's own unreachable-route
%% error would name a method the author never wrote on the cors route. Decided
%% by roadrunner's reachability check, against the table as the earlier routes
%% left it.
open_cors_options(Entries) ->
    lists:foldl(fun open_cors_options/2, Entries, lists:seq(1, length(Entries))).

open_cors_options(Pos, Entries) ->
    {Before, [Entry | After]} = lists:split(Pos - 1, Entries),
    case Entry of
        #{methods := Methods, middlewares := Mws} ->
            case lists:member(cors_options, Mws) of
                true ->
                    Opened =
                        Before ++
                            [
                                Entry#{
                                    methods => Methods ++ [~"OPTIONS"],
                                    middlewares => [open_placeholder(M) || M <- Mws]
                                }
                                | After
                            ],
                    Closed = Before ++ [Entry#{middlewares => Mws -- [cors_options]} | After],
                    case {roadrunner_router:validate(Opened), roadrunner_router:validate(Closed)} of
                        {{error, {unreachable_route, ShadowedPos, ShadowedPath, _By}}, ok} ->
                            #{path := CorsPath} = Entry,
                            error(
                                {cors_options_shadows_route, {Pos, CorsPath}, {
                                    ShadowedPos, ShadowedPath
                                }},
                                none,
                                [{error_info, #{module => ?MODULE}}]
                            );
                        {_, _} ->
                            Opened
                    end;
                false ->
                    Entries
            end;
        #{} ->
            Entries
    end.

open_placeholder(cors_options) -> fun ?MODULE:options_reply/3;
open_placeholder(Mw) -> Mw.

with_transforms(Route, []) -> Route;
with_transforms(Route, Transforms) -> Route#{middlewares => Transforms}.
