-module(arizona_route).
-moduledoc """
The kinds of route an Arizona route list holds, independent of any server.

An `arizona_server` adapter compiles each route into its server's own routing.
`kind/1` names what a route is, so an adapter dispatches on one atom instead of
re-matching every tuple shape, and refuses a route it cannot serve with
`unsupported/2` -- at route compilation, before a request could reach it, the
way `arizona_controller:dispatch/3` refuses a reply kind the adapter cannot send.

| Kind | Route |
|------|-------|
| `live` | `{live, Path, Handler, Opts}` |
| `ws` | `{ws, Path, Opts}` |
| `asset` | `{asset, Path, Source}`, `{asset, Path, Source, Opts}` |
| `controller` | `{Verb, Path, Handler, Opts}`, `{match, Spec, Path, Handler, Opts}` |
| `mcp` | `{mcp, Path, Handler, Opts}` |
| `reload` | `{reload, Path, Opts}` |

`live`, `ws`, `asset`, and `controller` are what an application is built from,
and every adapter serves them. `mcp` (the Model Context Protocol endpoint) and
`reload` (the dev hot-reload event stream) are capabilities.

## Route rules

What a route means does not depend on the server, so the rules live here and
every adapter's router compiles from them rather than restating them:

- `methods/1` -- the method allowlist (`GET` implies `HEAD`; `'*'` is any).
- `steps/1` -- the request pipeline, with the CSRF `check_origin` step first
  unless the route opts out (`check_origin => false`).
- `transforms/2` -- the HTTP response transforms, validated, with compression
  outermost and on by default for pages and assets.
- `live/2`, `controller/1`, `asset_dir/1` -- what each kind is served with.
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([kind/1]).
-export([path/1]).
-export([methods/1]).
-export([steps/1]).
-export([transforms/2]).
-export([live/2]).
-export([controller/1]).
-export([asset_dir/1]).
-export([unsupported/2]).
-export([format_error/2]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

%% For adapters whose server lacks a route kind; roadrunner serves them all.
-ignore_xref([unsupported/2]).
%% Public for an adapter that compiles the pipeline itself; `live/2` and
%% `controller/1` call it in-tree.
-ignore_xref([steps/1]).
%% Called by `erl_error` via the `error_info` annotation, not directly.
-ignore_xref([format_error/2]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([kind/0]).
-export_type([methods/0]).
-export_type([build_opts/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-nominal kind() :: live | ws | asset | controller | mcp | reload.

%% A route's method allowlist: uppercase method binaries, or `any`.
-nominal methods() :: [binary()] | any.

%% Listener-wide choices a route is compiled with: `compress` (default `true`)
%% turns the default compression of pages and assets on or off, and
%% `error_page` (unset by default) names the page a live route renders on a
%% dev crash.
-nominal build_opts() :: #{
    compress => boolean(),
    error_page => {module(), atom()},
    _ => term()
}.

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc """
The kind of `Route`. Raises `{unknown_route, Route}` for a term that is no
route: a route list is plain data a `sys.config` can spell by hand, so a typo
lands here rather than at a constructor.
""".
-spec kind(Route) -> kind() when
    Route :: term().
kind({live, Path, _Handler, _Opts}) when is_binary(Path) ->
    live;
kind({ws, Path, _Opts}) when is_binary(Path) ->
    ws;
kind({asset, Path, _Source}) when is_binary(Path) ->
    asset;
kind({asset, Path, _Source, _Opts}) when is_binary(Path) ->
    asset;
kind({match, _Spec, Path, _Handler, _Opts}) when is_binary(Path) ->
    controller;
kind({mcp, Path, _Handler, _Opts}) when is_binary(Path) ->
    mcp;
kind({reload, Path, _Opts}) when is_binary(Path) ->
    reload;
kind({Verb, Path, _Handler, _Opts} = Route) when is_atom(Verb), is_binary(Path) ->
    case lists:member(Verb, [get, post, put, patch, delete, head, options]) of
        true -> controller;
        false -> unknown_route(Route)
    end;
kind(Route) ->
    unknown_route(Route).

-doc "The path pattern of `Route`.".
-spec path(Route) -> binary() when
    Route :: term().
path(Route) ->
    case kind(Route) of
        controller when element(1, Route) =:= match -> element(3, Route);
        _Kind -> element(2, Route)
    end.

-doc """
The method allowlist of `Route`. A page or asset answers `GET` and `HEAD`; a
controller its declared verbs, uppercased, `HEAD` added beside `GET`; a
`{match, '*', ...}` controller and the `ws` and `mcp` kinds `any`; a `reload`
route is an event stream, `GET` and `HEAD`.
`'*'` inside a method list raises `wildcard_in_method_list`: there it would
match no real method.
""".
-spec methods(Route) -> methods() when
    Route :: term().
methods(Route) ->
    case kind(Route) of
        Kind when Kind =:= live; Kind =:= asset; Kind =:= reload -> [~"GET", ~"HEAD"];
        controller -> controller_methods(Route);
        _Other -> any
    end.

-doc """
The request pipeline of a `live` or `controller` route: its `middlewares`
steps, in order, behind the CSRF `check_origin` step unless the route sets
`check_origin => false`. HTTP response transforms are not steps
(`transforms/2`).
""".
-spec steps(Route) -> [arizona_middleware:middleware()] when
    Route :: term().
steps(Route) ->
    Opts = opts(Route),
    Steps = [M || M <- maps:get(middlewares, Opts, []), not is_transform(M)],
    case Opts of
        #{check_origin := false} -> Steps;
        #{} -> [{arizona_middleware, check_origin} | Steps]
    end.

-doc """
The HTTP response transforms of `Route`, in the order they wrap the exchange
(first outermost). Each is validated, raising `{unknown_http_transform, T}` for
a hand-spelled typo. Compression is outermost wherever it is written, so an
etag always hashes the uncompressed body; a `live` or `asset` route also gets
it by default unless `BuildOpts` sets `compress => false` (an explicit
`arizona_middleware:compress()` on the route still applies).

An `asset` route runs no pipeline, so a step among its `middlewares` would
never run: it raises `{asset_middleware_not_transform, Steps}`.
""".
-spec transforms(Route, BuildOpts) -> [arizona_middleware:http_transform()] when
    Route :: term(),
    BuildOpts :: build_opts().
transforms(Route, BuildOpts) ->
    Kind = kind(Route),
    Middlewares = maps:get(middlewares, opts(Route), []),
    ok = check_asset_steps(Kind, Middlewares),
    Transforms = [check_transform(M) || M <- Middlewares, is_transform(M)],
    Compress = {http_transform, compress},
    Others = [T || T <- Transforms, T =/= Compress],
    case
        lists:member(Compress, Transforms) orelse
            (default_compressed(Kind) andalso maps:get(compress, BuildOpts, true))
    of
        true -> [Compress | Others];
        false -> Others
    end.

-doc """
What a `live` route renders with: its handler, `bindings`, `on_mount`,
`layouts`, the pipeline `steps/1`, and the listener's `error_page` when it
sets one (none by default: a dev crash is then served as plain text). An
adapter's `c:arizona_req:resolve_route/3` returns these, minus `handler`, as
the route options.
""".
-spec live(Route, BuildOpts) -> Live when
    Route :: term(),
    BuildOpts :: build_opts(),
    Live :: #{
        handler := module(),
        bindings := map(),
        on_mount := arizona_live:on_mount(),
        layouts := [arizona_render:layout()],
        middlewares := [arizona_middleware:middleware()],
        error_page => {module(), atom()}
    }.
live({live, _Path, Handler, Opts} = Route, BuildOpts) ->
    Live = #{
        handler => Handler,
        layouts => maps:get(layouts, Opts, []),
        bindings => maps:get(bindings, Opts, #{}),
        on_mount => maps:get(on_mount, Opts, []),
        middlewares => steps(Route)
    },
    case BuildOpts of
        #{error_page := ErrorPage} -> Live#{error_page => ErrorPage};
        #{} -> Live
    end.

-doc """
What a `controller` route runs: the `arizona_controller:route()` that
`arizona_controller:dispatch/3` takes. The `mcp` and `reload` kinds are
controllers too, behind no pipeline: `arizona_mcp_handler` gates its own
requests (origin, peer, auth hook) and answers its own `405`, with the route's
handler module folded into its state; `arizona_reload_controller` is a dev
endpoint the page it serves opens itself.
""".
-spec controller(Route) -> arizona_controller:route() when
    Route :: term().
controller({mcp, _Path, Handler, Opts}) ->
    #{
        handler => arizona_mcp_handler,
        action => handle,
        state => Opts#{handler => Handler},
        middlewares => []
    };
controller({reload, _Path, _Opts}) ->
    #{handler => arizona_reload_controller, action => handle, state => #{}, middlewares => []};
controller(Route) ->
    controller = kind(Route),
    Opts = opts(Route),
    #{
        handler => controller_handler(Route),
        action => maps:get(action, Opts, handle),
        state => maps:get(state, Opts, #{}),
        middlewares => steps(Route)
    }.

-doc "The directory an `asset` route serves, a `priv_dir` source resolved.".
-spec asset_dir(Route) -> file:filename_all() when
    Route :: term().
asset_dir(Route) ->
    asset = kind(Route),
    case element(3, Route) of
        {dir, Dir} -> Dir;
        {priv_dir, App, SubDir} -> filename:join(code:priv_dir(App), SubDir)
    end.

-doc """
Raises `{unsupported_route, Kind, Adapter}`: what an adapter does, while
compiling its routes, with a route kind its server cannot serve -- so the
listener refuses to start instead of answering that path with something else.
""".
-spec unsupported(Kind, Adapter) -> no_return() when
    Kind :: kind(),
    Adapter :: module().
unsupported(Kind, Adapter) ->
    erlang:error({unsupported_route, Kind, Adapter}, [Kind, Adapter], [
        {error_info, #{module => ?MODULE}}
    ]).

-doc """
Renders this module's errors. Picked up by `erl_error` via the `error_info`
annotation.
""".
-spec format_error(Reason, Stacktrace) -> ErrorInfo when
    Reason :: term(),
    Stacktrace :: [tuple()],
    ErrorInfo :: #{general := iolist()}.
format_error({unknown_route, Route}, [{_M, _F, _Args, _Info} | _]) ->
    #{
        general => io_lib:format(
            "~0tp is not a route. A route is {live, Path, Handler, Opts}, "
            "{ws, Path, Opts}, {asset, Path, Source[, Opts]}, "
            "{Verb, Path, Handler, Opts} (get, post, put, patch, delete, head, "
            "options), {match, Spec, Path, Handler, Opts}, {mcp, Path, Handler, "
            "Opts}, or {reload, Path, Opts}, with Path a binary.",
            [Route]
        )
    };
format_error({unknown_http_transform, Unknown}, _ST) ->
    #{
        general => io_lib:format(
            "~tp is not an HTTP response transform. The valid terms are "
            "{http_transform, etag | compress | security_headers | "
            "{cors, Config} | {security_headers, Config}} -- built by the "
            "arizona_middleware constructors, or spelled literally in a "
            "sys.config route.",
            [Unknown]
        )
    };
format_error({asset_middleware_not_transform, Steps}, _ST) ->
    #{
        general => io_lib:format(
            "an asset route runs no Arizona middleware pipeline, so these "
            "`middlewares` entries would never run: ~tp. Asset routes accept "
            "HTTP response transforms only (arizona_middleware:cors/1, "
            "security_headers/0,1, ...).",
            [Steps]
        )
    };
format_error(wildcard_in_method_list, _ST) ->
    #{
        general =>
            "'*' (any method) is only valid as the whole method spec: "
            "{match, '*', Path, Handler, Opts}. Inside a method list -- or "
            "spelled as the binary ~\"*\" -- it would match no real method; "
            "list explicit verbs, or use '*' alone."
    };
format_error({unsupported_route, Kind, Adapter}, _ST) ->
    #{
        general => io_lib:format(
            "the ~s server adapter cannot serve a ~s route. Drop the route, or "
            "serve it through an adapter that has it.",
            [Adapter, Kind]
        )
    }.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

unknown_route(Route) ->
    erlang:error({unknown_route, Route}, [Route], [{error_info, #{module => ?MODULE}}]).

opts({live, _Path, _Handler, Opts}) -> Opts;
opts({asset, _Path, _Source}) -> #{};
opts({asset, _Path, _Source, Opts}) -> Opts;
opts({match, _Spec, _Path, _Handler, Opts}) -> Opts;
opts({_Verb, _Path, _Handler, Opts}) -> Opts;
opts({_Kind, _Path, Opts}) -> Opts.

controller_handler({match, _Spec, _Path, Handler, _Opts}) -> Handler;
controller_handler({_Verb, _Path, Handler, _Opts}) -> Handler.

controller_methods({match, '*', _Path, _Handler, _Opts}) ->
    any;
controller_methods({match, Methods, _Path, _Handler, _Opts}) when is_list(Methods) ->
    with_head([method_bin(M) || M <- Methods]);
controller_methods({match, Method, _Path, _Handler, _Opts}) ->
    with_head([method_bin(Method)]);
controller_methods({Verb, _Path, _Handler, _Opts}) ->
    with_head([method_bin(Verb)]).

%% `'*'` is any method only as the whole spec; anywhere else it would silently
%% become the literal method `*`, which no request carries.
method_bin('*') ->
    erlang:error(wildcard_in_method_list, ['*'], [{error_info, #{module => ?MODULE}}]);
method_bin(~"*") ->
    erlang:error(wildcard_in_method_list, [~"*"], [{error_info, #{module => ?MODULE}}]);
method_bin(M) when is_atom(M) -> upper(atom_to_binary(M, utf8));
method_bin(M) when is_binary(M) -> upper(M).

%% HTTP methods are case-sensitive and conventionally uppercase.
upper(Bin) -> iolist_to_binary(string:uppercase(Bin)).

%% HEAD is a bodyless GET, so a GET allowlist answers HEAD too.
with_head(Methods) ->
    case lists:member(~"GET", Methods) andalso not lists:member(~"HEAD", Methods) of
        true -> Methods ++ [~"HEAD"];
        false -> Methods
    end.

is_transform({http_transform, _}) -> true;
is_transform(_) -> false.

check_transform({http_transform, T} = Transform) when
    T =:= etag; T =:= compress; T =:= security_headers
->
    Transform;
check_transform({http_transform, {cors, _Config}} = Transform) ->
    Transform;
check_transform({http_transform, {security_headers, _Config}} = Transform) ->
    Transform;
check_transform({http_transform, Unknown}) ->
    erlang:error({unknown_http_transform, Unknown}, none, [{error_info, #{module => ?MODULE}}]).

check_asset_steps(asset, Middlewares) ->
    case [M || M <- Middlewares, not is_transform(M)] of
        [] ->
            ok;
        Steps ->
            erlang:error({asset_middleware_not_transform, Steps}, none, [
                {error_info, #{module => ?MODULE}}
            ])
    end;
check_asset_steps(_Kind, _Middlewares) ->
    ok.

%% Pages and assets are what compression pays off on; a controller, a
%% WebSocket upgrade, and the event streams are left alone.
default_compressed(live) -> true;
default_compressed(asset) -> true;
default_compressed(_Kind) -> false.
