-module(arizona_middleware_cowboy).
-moduledoc ~"""
Generic Cowboy middleware for processing Arizona middleware chains.

Implements the `cowboy_middleware` behavior to integrate Arizona's middleware
system into Cowboy's request processing pipeline. This middleware runs after
`cowboy_router` (which sets handler_opts) and before `cowboy_handler`.

## Middleware Pipeline

```
cowboy_router -> arizona_middleware_cowboy -> cowboy_handler
```

1. `cowboy_router` sets `handler_opts` in the environment
2. `arizona_middleware_cowboy` processes Arizona middleware chain if present
3. `cowboy_handler` calls the final handler

## Handler State Format

Arizona handlers should provide state in this format:
- `{view, ViewModule, MountArg, Middlewares}`
- `{controller, Handler, HandlerState, Middlewares}`
- `{websocket, WebSocketOpts, Middlewares}`
- `{asset, AssetConfig, Middlewares}`

## Integration

```erlang
#{middlewares => [cowboy_router, arizona_middleware_cowboy, cowboy_handler]}
```
""".
-behaviour(cowboy_middleware).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([execute/2]).

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-doc ~"""
Processes Arizona middleware chain if present in handler options.

Extracts middlewares from the handler state, processes them sequentially,
and either continues to the next Cowboy middleware or halts processing
if a middleware sends a response.
""".
-spec execute(Req, Env) -> Result when
    Req :: cowboy_req:req(),
    Env :: cowboy_middleware:env(),
    Result :: {ok, Req1, Env} | {stop, Req1},
    Req1 :: cowboy_req:req().
execute(Req, Env) ->
    HandlerOpts = maps:get(handler_opts, Env, undefined),
    Middlewares = extract_middlewares(HandlerOpts),
    % Process Arizona middleware chain
    case arizona_middleware:process_middlewares(Middlewares, Req) of
        {continue, Req1} ->
            % Normalize handler_opts for the next middleware (cowboy_handler)
            NormalizedEnv = Env#{handler_opts => norm_handler_opts(HandlerOpts)},
            {ok, Req1, NormalizedEnv};
        {halt, Req1} ->
            % Middleware sent response, stop processing
            {stop, Req1}
    end.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% Extract middlewares from various Arizona handler state formats
-spec extract_middlewares(HandlerOpts) -> Result when
    HandlerOpts :: tuple(),
    Result :: [arizona_middleware:middleware()].
extract_middlewares({view, _ViewModule, _MountArg, Middlewares}) ->
    Middlewares;
extract_middlewares({controller, _Handler, _HandlerState, Middlewares}) ->
    Middlewares;
extract_middlewares({websocket, _WebSocketOpts, Middlewares}) ->
    Middlewares;
extract_middlewares({asset, _AssetConfig, Middlewares}) ->
    Middlewares.

%% Normalize handler options by removing middleware information
-spec norm_handler_opts(HandlerOpts) -> NormalizedOpts when
    HandlerOpts :: dynamic(),
    NormalizedOpts :: dynamic().
norm_handler_opts({view, ViewModule, MountArg, _Middlewares}) ->
    {ViewModule, MountArg};
norm_handler_opts({controller, _Handler, HandlerState, _Middlewares}) ->
    HandlerState;
norm_handler_opts({websocket, WebSocketOpts, _Middlewares}) ->
    WebSocketOpts;
norm_handler_opts({asset, AssetConfig, _Middlewares}) ->
    AssetConfig.
