-module(arizona_server).
-moduledoc ~"""
Provides HTTP server functionality for Arizona LiveView applications.

## Overview

The server module manages HTTP routing, static file serving, and connection
handling for Arizona LiveView applications. It provides a high-level interface
for starting and configuring the underlying Cowboy HTTP server.

## Features

- **LiveView Routing**: Routes HTTP requests to appropriate LiveView modules
- **WebSocket Support**: Handles WebSocket connections for real-time updates
- **Static File Serving**: Serves static assets from filesystem or priv directories
- **Route Compilation**: Compiles Arizona routes into Cowboy dispatch format
- **Metadata Resolution**: Resolves route metadata for request handling
- **Server Management**: Start and stop HTTP server with configuration

## Key Functions

- `start/1`: Start HTTP server with configuration
- `stop/0`: Stop the running HTTP server
- `compile_routes/1`: Compile Arizona routes to Cowboy format
- `get_route_metadata/1`: Get route metadata for request handling
- `route_to_cowboy/1`: Convert single route to Cowboy format

## Route Types

- **live**: HTTP routes that render LiveView modules
- **live_websocket**: WebSocket endpoints for real-time communication
- **static**: Static file serving from filesystem or priv directories
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([start/1]).
-export([stop/0]).
-export([compile_routes/1]).
-export([route_to_cowboy/1]).
-export([get_route_metadata/1]).
-export([get_route_handler/1]).
-export([new_route_metadata/2]).
-export([get_route_type/1]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

-ignore_xref([start/1]).
-ignore_xref([stop/0]).
-ignore_xref([compile_routes/1]).
-ignore_xref([route_to_cowboy/1]).
-ignore_xref([new_route_metadata/2]).
-ignore_xref([get_route_type/1]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([route/0]).
-export_type([server_config/0]).
-export_type([route_metadata/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-record(route_metadata, {
    type :: live | live_websocket | static,
    handler :: module()
}).

-doc ~"""
Route configuration specification for Arizona applications.

Defines different types of routes that can be handled by the server:
- LiveView routes for interactive pages
- WebSocket routes for real-time communication
- Static file routes for assets and resources
""".
-type route() ::
    {live, Path :: binary(), LiveModule :: module()}
    | {live_websocket, Path :: binary()}
    | {static, Path :: binary(), {dir, Directory :: binary()}}
    | {static, Path :: binary(), {file, FileName :: binary()}}
    | {static, Path :: binary(), {priv_dir, App :: atom(), Directory :: binary()}}
    | {static, Path :: binary(), {priv_file, App :: atom(), FileName :: binary()}}.

-doc ~"""
Opaque route metadata containing routing information.

Stores the route type and handler module for request processing.
Used internally by the routing system.
""".
-opaque route_metadata() :: #route_metadata{}.

-doc ~"""
Server configuration for starting Arizona HTTP server.

Specifies the port number and list of routes to be served.
""".
-type server_config() :: #{
    port := pos_integer(),
    routes := [route()]
}.

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-doc ~"""
Start the Arizona HTTP server with the given configuration.

Compiles the provided routes, stores them in persistent_term for efficient
access, and starts a Cowboy HTTP listener on the specified port.

## Examples

```erlang
1> Config = #{port => 8080, routes => [{live, "/", home_live}]}.
#{port => 8080, routes => [...]}
2> arizona_server:start(Config).
{ok, <0.123.0>}
```
""".
-spec start(Config) -> Result when
    Config :: server_config(),
    Result :: {ok, pid()} | {error, term()}.
start(#{port := Port, routes := Routes}) ->
    % Compile routes into Cowboy dispatch format
    Dispatch = compile_routes(Routes),
    ok = persistent_term:put(arizona_dispatch, Dispatch),

    % Start Cowboy HTTP listener
    cowboy:start_clear(
        arizona_http_listener,
        [{port, Port}],
        #{env => #{dispatch => {persistent_term, arizona_dispatch}}}
    ).

-doc ~"""
Stop the Arizona HTTP server.

Stops the running Cowboy HTTP listener gracefully.

## Examples

```erlang
1> arizona_server:stop().
ok
```
""".
-spec stop() -> ok.
stop() ->
    cowboy:stop_listener(arizona_http_listener).

-doc ~"""
Get route metadata for a request using the dispatch table.

Uses Cowboy's router to resolve the request path and extract route metadata
including the route type and handler module.

## Examples

```erlang
1> Req = #{path => "/users", ...}.
#{...}
2> arizona_server:get_route_metadata(Req).
#route_metadata{type = live, handler = user_live}
```
""".
-spec get_route_metadata(Req) -> RouteMetadata when
    Req :: cowboy_req:req(),
    RouteMetadata :: route_metadata().
get_route_metadata(Req) ->
    {ok, _Req, Env} = cowboy_router:execute(
        Req,
        #{dispatch => {persistent_term, arizona_dispatch}}
    ),
    HandlerOpts = maps:get(handler_opts, Env),
    #route_metadata{
        type = maps:get(type, HandlerOpts),
        handler = maps:get(handler, HandlerOpts)
    }.

-doc ~"""
Get handler module from route metadata.

Extracts the handler module from route metadata for request processing.

## Examples

```erlang
1> Metadata = arizona_server:get_route_metadata(Req).
#route_metadata{...}
2> arizona_server:get_route_handler(Metadata).
user_live
```
""".
-spec get_route_handler(RouteMetadata) -> Handler when
    RouteMetadata :: route_metadata(),
    Handler :: module().
get_route_handler(#route_metadata{handler = Handler}) ->
    Handler.

-doc ~"""
Create new route metadata for testing purposes.

Utility function to create route metadata with specified type and handler
for use in test suites.

## Examples

```erlang
1> arizona_server:new_route_metadata(live, user_live).
#route_metadata{type = live, handler = user_live}
```
""".
-spec new_route_metadata(Type, Handler) -> RouteMetadata when
    Type :: atom(),
    Handler :: module(),
    RouteMetadata :: route_metadata().
new_route_metadata(Type, Handler) ->
    #route_metadata{
        type = Type,
        handler = Handler
    }.

-doc ~"""
Get route type from route metadata.

Extracts the route type (live, live_websocket, or static) from route metadata.

## Examples

```erlang
1> Metadata = arizona_server:new_route_metadata(live, user_live).
#route_metadata{...}
2> arizona_server:get_route_type(Metadata).
live
```
""".
-spec get_route_type(RouteMetadata) -> Type when
    RouteMetadata :: route_metadata(),
    Type :: atom().
get_route_type(#route_metadata{type = Type}) ->
    Type.

-doc ~"""
Compile Arizona routes into Cowboy dispatch format.

Converts a list of Arizona route specifications into the dispatch format
expected by Cowboy's router for efficient request routing.

## Examples

```erlang
1> Routes = [{live, "/users", user_live}, {live_websocket, "/live"}].
[...]
2> arizona_server:compile_routes(Routes).
[{'_', [...]}]
```
""".
-spec compile_routes(Routes) -> DispatchRules when
    Routes :: [route()],
    DispatchRules :: term().
compile_routes(Routes) ->
    % Convert Arizona routes to Cowboy routes
    CowboyRoutes = [route_to_cowboy(Route) || Route <- Routes],

    % Compile the dispatch rules
    cowboy_router:compile([
        % Match any host
        {'_', CowboyRoutes}
    ]).

%% --------------------------------------------------------------------
%% Private functions
%% --------------------------------------------------------------------

%% Convert a single Arizona route to Cowboy route format
-spec route_to_cowboy(Route) -> CowboyRoute when
    Route :: route(),
    CowboyRoute :: {'_' | iodata(), module(), term()}.
route_to_cowboy({live, Path, LiveModule}) when is_binary(Path), is_atom(LiveModule) ->
    % LiveView routes use arizona_handler
    {Path, arizona_handler, #{
        type => live,
        handler => LiveModule
    }};
route_to_cowboy({live_websocket, Path}) when is_binary(Path) ->
    % WebSocket endpoint for all LiveView connections
    {Path, arizona_websocket, #{
        type => live_websocket
    }};
route_to_cowboy({static, Path, {dir, Directory}}) when is_binary(Path), is_binary(Directory) ->
    % Static file serving from filesystem directory
    DirPath = filename:join(Path, "[...]"),
    {DirPath, cowboy_static, {dir, Directory}};
route_to_cowboy({static, Path, {file, FileName}}) when is_binary(Path), is_binary(FileName) ->
    % Static file serving for single file from priv directory
    {Path, cowboy_static, {file, FileName}};
route_to_cowboy({static, Path, {priv_dir, App, Directory}}) when
    is_binary(Path), is_atom(App), is_binary(Directory)
->
    % Static file serving from priv directory
    DirPath = filename:join(Path, "[...]"),
    {DirPath, cowboy_static, {priv_dir, App, Directory}};
route_to_cowboy({static, Path, {priv_file, App, FileName}}) when
    is_binary(Path), is_atom(App), is_binary(FileName)
->
    % Static file serving for single file from priv directory
    {Path, cowboy_static, {priv_file, App, FileName}}.
