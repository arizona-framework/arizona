-module(arizona_cowboy_adapter).
-behaviour(arizona_adapter).
-export([resolve_route/2, resolve_cowboy_route/1]).
-ignore_xref([resolve_route/2]).

-spec resolve_route(arizona_adapter:path(), map()) -> {module(), arizona_adapter:route_opts()}.
resolve_route(Path, Req) ->
    {_CowboyHandler, Opts} = resolve_cowboy_route(Req#{path => Path}),
    {maps:get(handler, Opts), maps:without([handler], Opts)}.

-spec resolve_cowboy_route(map()) -> {module(), map()}.
resolve_cowboy_route(Req) ->
    {ok, _Req, Env} = cowboy_router:execute(
        Req,
        #{dispatch => {persistent_term, arizona_dispatch}}
    ),
    {maps:get(handler, Env), maps:get(handler_opts, Env)}.
