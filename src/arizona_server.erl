-module(arizona_server).
-moduledoc ~"""
HTTP/WebSocket server configuration and startup using Cowboy web server.

Provides high-level interface for configuring and managing the Arizona
web server built on Cowboy. Handles route compilation, server lifecycle,
and integration between Arizona components and Cowboy infrastructure.

## Route Types

- **View Routes**: `{view, Path, ViewModule, MountArg}` - Arizona view handling
- **WebSocket Routes**: `{websocket, Path}` - Live connection endpoints
- **Controller Routes**: `{controller, Path, Handler, State}` - Custom Cowboy handlers for
  API endpoints
- **Asset Routes**: Static file serving with multiple source options:
  - `{asset, Path, {dir, Directory}}` - Filesystem directory
  - `{asset, Path, {file, FileName}}` - Single file
  - `{asset, Path, {priv_dir, App, Directory}}` - Application priv directory
  - `{asset, Path, {priv_file, App, FileName}}` - Application priv file

## Server Configuration

```erlang
Config = #{
    scheme => http,  % or https
    transport_opts => #{socket_opts => [{port, 4000}]},
    proto_opts => #{env => #{custom_option => value}},
    routes => [
        {view, ~"/", home_view, #{}},
        {view, ~"/users/[:id]", users_view, #{}},
        {websocket, ~"/live"},
        {controller, ~"/api/presence", my_api_controller, #{}},
        {asset, ~"/static", {priv_dir, myapp, ~"static"}}
    ]
}.
```

## Lifecycle Management

1. **Start**: Route compilation → Transport/Protocol setup → Listener start
2. **Runtime**: Request routing through compiled dispatch table
3. **Stop**: Graceful listener shutdown

## Integration

Integrates Arizona components with Cowboy:
- `arizona_handler` for view rendering
- `arizona_websocket` for live connections
- `cowboy_static` for asset serving
- Route-based dispatch with persistent term storage
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([start/1]).
-export([stop/0]).
-export([get_handler_opts/1]).
-export([is_running/0]).
-export([get_address/0]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

-ignore_xref([start/1]).
-ignore_xref([stop/0]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([route/0]).
-export_type([path/0]).
-export_type([config/0]).
-export_type([scheme/0]).
-export_type([transport_opts/0]).
-export_type([proto_opts/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-nominal route() ::
    {view, Path :: path(), ViewModule :: module(), MountArg :: arizona_view:mount_arg()}
    | {websocket, Path :: path()}
    | {asset, Path :: path(), {dir, Directory :: binary()}}
    | {asset, Path :: path(), {file, FileName :: binary()}}
    | {asset, Path :: path(), {priv_dir, App :: atom(), Directory :: binary()}}
    | {asset, Path :: path(), {priv_file, App :: atom(), FileName :: binary()}}
    | {controller, Path :: path(), Handler :: module(), State :: dynamic()}.
-nominal path() :: binary().

-nominal config() :: #{
    enabled => boolean(),
    scheme => scheme(),
    transport_opts => transport_opts(),
    proto_opts => proto_opts(),
    routes := [route()]
}.
-nominal scheme() :: http | https.
-nominal transport_opts() :: ranch:opts().
-nominal proto_opts() :: cowboy:opts().

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-doc ~"""
Starts the HTTP/WebSocket server with the given configuration.

Compiles routes, configures transport and protocol options, and starts
the Cowboy listener. Stores dispatch table in persistent term for
efficient request routing.
""".
-spec start(Config) -> Result when
    Config :: config(),
    Result :: {ok, pid()} | {error, term()}.
start(Config) when is_map(Config) ->
    % Compile routes
    Dispatch = compile_routes(maps:get(routes, Config)),
    ok = persistent_term:put(get_dispatch_key(), Dispatch),

    % Get transport and protocol options
    DefaultPort = 1912,
    TransportOpts = get_transport_opts(Config, DefaultPort),
    ProtoOpts = get_proto_opts(Config),

    % Start listener based on scheme
    Scheme = maps:get(scheme, Config, http),
    Ref = get_listener_key(),
    start_listener(Scheme, Ref, TransportOpts, ProtoOpts).

-doc ~"""
Stops the running HTTP/WebSocket server.

Gracefully shuts down the Cowboy listener and cleans up resources.
""".
-spec stop() -> ok.
stop() ->
    ok = cowboy:stop_listener(get_listener_key()),
    ok.

-doc ~"""
Retrieves handler options for a Cowboy request.

Used internally by Arizona handlers to get view module and mount
arguments from the compiled route dispatch table.
""".
-spec get_handler_opts(Req) -> Opts when
    Req :: cowboy_req:req(),
    Opts :: dynamic().
get_handler_opts(Req) ->
    {ok, _Req, Env} = cowboy_router:execute(
        Req,
        #{dispatch => {persistent_term, get_dispatch_key()}}
    ),
    maps:get(handler_opts, Env).

-doc ~"""
Checks if the Arizona server is currently running.

Returns `true` if the Ranch listener is active, `false` otherwise.
""".
-spec is_running() -> boolean().
is_running() ->
    ranch:get_status(get_listener_key()) =:= running.

-doc ~"""
Gets the server's current IP address and port.

Returns the actual address the server is listening on, which may
differ from configuration if port 0 was specified (random port).
""".
-spec get_address() -> {ok, IpAddress, Port} | error when
    IpAddress :: inet:ip_address(),
    Port :: inet:port_number().
get_address() ->
    case ranch:get_addr(arizona_listener) of
        {IpAddress, Port} when is_tuple(IpAddress), is_integer(Port) ->
            {ok, IpAddress, Port};
        {undefined, undefined} ->
            error
    end.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

get_dispatch_key() -> arizona_dispatch.

get_listener_key() -> arizona_listener.

compile_routes(Routes) ->
    % Convert Arizona routes to Cowboy routes
    CowboyRoutes = [route_to_cowboy(Route) || Route <- Routes],

    % Compile the dispatch rules
    cowboy_router:compile([
        % Match any host
        {'_', CowboyRoutes}
    ]).

%% Convert a single Arizona route to Cowboy route format
-spec route_to_cowboy(Route) -> CowboyRoute when
    Route :: route(),
    CowboyRoute :: {'_' | iodata(), module(), term()}.
route_to_cowboy({view, Path, ViewModule, MountArg}) when is_binary(Path), is_atom(ViewModule) ->
    % View routes use arizona_handler
    {Path, arizona_handler, {ViewModule, MountArg}};
route_to_cowboy({websocket, Path}) when is_binary(Path) ->
    % WebSocket endpoint for all View connections
    {Path, arizona_websocket, undefined};
route_to_cowboy({asset, Path, {dir, Directory}}) when is_binary(Path), is_binary(Directory) ->
    % Static file serving from filesystem directory
    DirPath = filename:join(Path, "[...]"),
    {DirPath, cowboy_static, {dir, Directory}};
route_to_cowboy({asset, Path, {file, FileName}}) when is_binary(Path), is_binary(FileName) ->
    % Static file serving for single file from priv directory
    {Path, cowboy_static, {file, FileName}};
route_to_cowboy({asset, Path, {priv_dir, App, Directory}}) when
    is_binary(Path), is_atom(App), is_binary(Directory)
->
    % Static file serving from priv directory
    DirPath = filename:join(Path, "[...]"),
    {DirPath, cowboy_static, {priv_dir, App, Directory}};
route_to_cowboy({asset, Path, {priv_file, App, FileName}}) when
    is_binary(Path), is_atom(App), is_binary(FileName)
->
    % Static file serving for single file from priv directory
    {Path, cowboy_static, {priv_file, App, FileName}};
route_to_cowboy({controller, Path, Handler, State}) when is_binary(Path), is_atom(Handler) ->
    % View routes use arizona_handler
    {Path, Handler, State}.

%% Get transport options with defaults
get_transport_opts(Config, DefaultPort) ->
    UserTransportOpts = maps:get(transport_opts, Config, []),
    ensure_transport_port(UserTransportOpts, DefaultPort).

%% Ensure a proplist has a port, adding default if missing
ensure_port_in_proplist(PropList, DefaultPort) when is_list(PropList) ->
    case proplists:lookup(port, PropList) of
        {port, _Port} ->
            PropList;
        none ->
            [{port, DefaultPort} | PropList]
    end.

%% Ensure transport options have a port
ensure_transport_port(TransportOpts, DefaultPort) when is_list(TransportOpts) ->
    ensure_port_in_proplist(TransportOpts, DefaultPort);
ensure_transport_port(TransportOpts, DefaultPort) when is_map(TransportOpts) ->
    case TransportOpts of
        #{socket_opts := SocketOpts} ->
            NewSocketOpts = ensure_port_in_proplist(SocketOpts, DefaultPort),
            TransportOpts#{socket_opts => NewSocketOpts};
        #{} ->
            % socket_opts not present, add default socket_opts with port
            TransportOpts#{socket_opts => [{port, DefaultPort}]}
    end.

%% Get protocol options, ensuring Arizona controls dispatch
get_proto_opts(Config) ->
    UserProtoOpts = maps:get(proto_opts, Config, #{}),
    UserEnv = maps:get(env, UserProtoOpts, #{}),

    % Warn if user tries to set dispatch
    case maps:is_key(dispatch, UserEnv) of
        true ->
            logger:warning(
                "User-defined 'dispatch' in proto_opts env "
                "will be overridden by Arizona"
            );
        false ->
            ok
    end,

    % Arizona always controls dispatch
    ArizonaEnv = UserEnv#{dispatch => {persistent_term, get_dispatch_key()}},
    UserProtoOpts#{env => ArizonaEnv}.

%% Start listener based on scheme
start_listener(http, Ref, TransportOpts, ProtoOpts) ->
    cowboy:start_clear(Ref, TransportOpts, ProtoOpts);
start_listener(https, Ref, TransportOpts, ProtoOpts) ->
    cowboy:start_tls(Ref, TransportOpts, ProtoOpts).
