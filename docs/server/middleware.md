# Middleware

- [Behaviour](#behaviour)
  - [execute/2](#execute2)
- [Chain Processing](#chain-processing)
- [Route Config](#route-config)
- [Cowboy Integration](#cowboy-integration)

## Behaviour

Middleware modules implement the `arizona_middleware` behaviour to intercept and transform requests
before they reach the route handler. This is useful for cross-cutting concerns such as
authentication, logging, rate limiting, and request validation.

### execute/2

The single required callback. It receives the Cowboy request object and an options map, and must
return either `{continue, Req}` or `{halt, Req}`.

```erlang
-module(my_auth_middleware).
-behaviour(arizona_middleware).
-export([execute/2]).

execute(Req, Opts) ->
    case check_auth(Req) of
        ok ->
            {continue, Req};
        unauthorized ->
            Req1 = cowboy_req:reply(401, #{}, ~"Unauthorized", Req),
            {halt, Req1}
    end.
```

Return `{continue, Req}` to pass the request to the next middleware in the chain (or to the route
handler if this is the last middleware). Return `{halt, Req}` to stop the chain and return the
response immediately -- the remaining middlewares and the route handler are skipped.

## Chain Processing

Middlewares are processed sequentially via `arizona_middleware:process_middlewares/2`. Each
middleware in the list is executed in order. If any middleware returns `{halt, Req}`, the remaining
middlewares and the route handler are skipped, and the halted response is sent to the client.

```erlang
%% Internal processing (for illustration):
%% arizona_middleware:process_middlewares(Middlewares, CowboyReq)
%%
%% Middlewares = [{my_auth_middleware, #{}}, {my_logging_middleware, #{}}]
%%
%% 1. my_auth_middleware:execute(Req, #{}) -> {continue, Req1}
%% 2. my_logging_middleware:execute(Req1, #{}) -> {continue, Req2}
%% 3. Route handler receives Req2
```

## Route Config

Middlewares are specified per-route as the last element of the route tuple. Each middleware is a
`{Module, Opts}` tuple, where `Module` is the middleware module and `Opts` is a map passed as the
second argument to `execute/2`.

```erlang
{view, ~"/admin", admin_view, #{}, [
    {my_auth_middleware, #{role => admin}},
    {my_logging_middleware, #{}}
]}
```

Different routes can have different middleware chains. Routes that need no middleware use an empty
list:

```erlang
{view, ~"/", home_view, #{}, []}
```

## Cowboy Integration

Arizona integrates with Cowboy's middleware pipeline through `arizona_middleware_cowboy`, which
implements the `cowboy_middleware` behaviour. The full Cowboy pipeline is:

```text
cowboy_router -> arizona_middleware_cowboy -> cowboy_handler
```

The `arizona_middleware_cowboy` module extracts the per-route middleware list from the handler state
and runs them via `arizona_middleware:process_middlewares/2` before the handler executes. This
design keeps Arizona's middleware system composable and per-route while fitting cleanly into
Cowboy's existing architecture.

See also: [Routing](routing.md), [Configuration](configuration.md)
