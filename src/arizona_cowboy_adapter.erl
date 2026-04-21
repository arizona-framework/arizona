-module(arizona_cowboy_adapter).
-moduledoc """
`arizona_adapter` implementation backed by Cowboy's dispatch table.

Walks the persistent-term-stored Cowboy dispatch (set up by
`arizona_cowboy_router`) to map a request path to a handler module
plus its route options.

Used by `arizona_socket` during navigation: when the client asks to
navigate to a new path, `resolve_route/2` returns the handler to mount
along with the route's static bindings and any `on_mount`/`layout`
options declared on the route.
""".
-behaviour(arizona_adapter).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([resolve_route/2]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

-ignore_xref([resolve_route/2]).

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc """
Resolves a path to `{Handler, RouteOpts}` by running the Cowboy router.

Returns the handler's static route options (including its `bindings`
config) untouched. URL-derived data is not merged here -- handlers
that need it reach through `arizona_req` accessors or middleware.
""".
-spec resolve_route(Path, Req) -> {Handler, RouteOpts} when
    Path :: arizona_adapter:path(),
    Req :: map(),
    Handler :: module(),
    RouteOpts :: arizona_adapter:route_opts().
resolve_route(Path, Req) ->
    {ok, _ResolvedReq, Env} = cowboy_router:execute(
        Req#{path => Path},
        #{dispatch => {persistent_term, arizona_dispatch}}
    ),
    Opts = maps:get(handler_opts, Env),
    {maps:get(handler, Opts), maps:without([handler], Opts)}.
