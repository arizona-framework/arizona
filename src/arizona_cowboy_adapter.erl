-module(arizona_cowboy_adapter).
-moduledoc """
`arizona_adapter` implementation backed by Cowboy's dispatch table.

Walks the persistent-term-stored Cowboy dispatch (set up by
`arizona_cowboy_router`) to map a request path to a handler module
plus its route options. Path bindings produced by Cowboy's matcher
are merged into the route's static bindings before they reach the
handler.

Used by `arizona_socket` during navigation: when the client asks to
navigate to a new path, `resolve_route/2` returns the handler to mount
along with the merged bindings and any `on_mount`/`layout` options
declared on the route.
""".
-behaviour(arizona_adapter).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([resolve_route/2]).
-export([resolve_cowboy_route/1]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

-ignore_xref([resolve_route/2]).

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc """
Resolves a path to `{Handler, RouteOpts}` by running the Cowboy router
and merging path bindings into the route's static bindings.
""".
-spec resolve_route(Path, Req) -> {Handler, RouteOpts} when
    Path :: arizona_adapter:path(),
    Req :: map(),
    Handler :: module(),
    RouteOpts :: arizona_adapter:route_opts().
resolve_route(Path, Req) ->
    {_CowboyHandler, ResolvedReq, Opts} = resolve_cowboy_route(Req#{path => Path}),
    PathBindings = cowboy_req:bindings(ResolvedReq),
    RouteOpts = maps:without([handler], Opts),
    RouteBindings = maps:merge(maps:get(bindings, RouteOpts, #{}), PathBindings),
    {maps:get(handler, Opts), RouteOpts#{bindings => RouteBindings}}.

-doc """
Lower-level helper that runs Cowboy's router against the persistent
dispatch table and returns the raw `{Handler, ResolvedReq, HandlerOpts}`
tuple.

Exposed so the HTTP layer can reuse the same lookup without going
through `resolve_route/2`'s binding merge step.
""".
-spec resolve_cowboy_route(Req) -> {Handler, ResolvedReq, HandlerOpts} when
    Req :: map(),
    Handler :: module(),
    ResolvedReq :: map(),
    HandlerOpts :: map().
resolve_cowboy_route(Req) ->
    {ok, ResolvedReq, Env} = cowboy_router:execute(
        Req,
        #{dispatch => {persistent_term, arizona_dispatch}}
    ),
    {maps:get(handler, Env), ResolvedReq, maps:get(handler_opts, Env)}.
