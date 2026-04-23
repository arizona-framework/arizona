-module(arizona_cowboy_adapter).
-moduledoc """
`arizona_adapter` implementation backed by Cowboy's dispatch table.

Walks the persistent-term-stored Cowboy dispatch (set up by
`arizona_cowboy_router`) to map a request path to a handler module
plus its route options.

Used by `arizona_socket` during navigation: when the client asks to
navigate to a new path, `resolve_route/3` returns the handler to mount
along with the route's static bindings, any `on_mount`/`layout` options
declared on the route, and a navigate-scoped `arizona_req:request()`
carrying the new path/qs.
""".
-behaviour(arizona_adapter).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([resolve_route/3]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

-ignore_xref([resolve_route/3]).

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc """
Resolves a path to `{Handler, RouteOpts, Request}` by running the
Cowboy router.

Returns the handler's static route options (including its `bindings`
config) untouched, plus a navigate-scoped `arizona_req:request()`
synthesized from the stored upgrade cowboy req with the new path and
query applied. URL-derived data is not merged into `RouteOpts` --
handlers that need it reach through `arizona_req` accessors or
middleware.
""".
-spec resolve_route(Path, Qs, Req) -> {Handler, RouteOpts, ArzReq} when
    Path :: arizona_adapter:path(),
    Qs :: arizona_adapter:qs(),
    Req :: map(),
    Handler :: module(),
    RouteOpts :: arizona_adapter:route_opts(),
    ArzReq :: arizona_req:request().
resolve_route(Path, Qs, Req) ->
    NavReq = Req#{path => Path, qs => Qs},
    {ok, ResolvedReq, Env} = cowboy_router:execute(
        NavReq,
        #{dispatch => {persistent_term, arizona_dispatch}}
    ),
    Opts = maps:get(handler_opts, Env),
    ArzReq = arizona_cowboy_req:new(ResolvedReq),
    {maps:get(handler, Opts), maps:without([handler], Opts), ArzReq}.
