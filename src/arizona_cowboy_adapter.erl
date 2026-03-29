-module(arizona_cowboy_adapter).
-behaviour(arizona_adapter).
-export([resolve_route/2, resolve_cowboy_route/1]).
-ignore_xref([resolve_route/2]).

-spec resolve_route(arizona_adapter:path(), map()) -> {module(), arizona_adapter:route_opts()}.
resolve_route(Path, Req) ->
    {_CowboyHandler, ResolvedReq, Opts} = resolve_cowboy_route(Req#{path => Path}),
    PathBindings = cowboy_req:bindings(ResolvedReq),
    RouteOpts = maps:without([handler], Opts),
    RouteBindings = maps:merge(maps:get(bindings, RouteOpts, #{}), PathBindings),
    {maps:get(handler, Opts), RouteOpts#{bindings => RouteBindings}}.

-spec resolve_cowboy_route(map()) -> {module(), map(), map()}.
resolve_cowboy_route(Req) ->
    {ok, ResolvedReq, Env} = cowboy_router:execute(
        Req,
        #{dispatch => {persistent_term, arizona_dispatch}}
    ),
    {maps:get(handler, Env), ResolvedReq, maps:get(handler_opts, Env)}.
