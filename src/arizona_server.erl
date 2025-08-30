-module(arizona_server).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([start/1]).
-export([stop/0]).
-export([get_route_metadata/1]).
-export([get_route_handler/1]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

-ignore_xref([start/1]).
-ignore_xref([stop/0]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([route_metadata/0]).
-export_type([route/0]).
-export_type([server_config/0]).
-export_type([scheme/0]).
-export_type([transport_opts/0]).
-export_type([proto_opts/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-record(route_metadata, {
    type :: live | live_websocket | static,
    handler :: module()
}).

-nominal route() ::
    {live, Path :: binary(), LiveModule :: module()}
    | {live_websocket, Path :: binary()}
    | {static, Path :: binary(), {dir, Directory :: binary()}}
    | {static, Path :: binary(), {file, FileName :: binary()}}
    | {static, Path :: binary(), {priv_dir, App :: atom(), Directory :: binary()}}
    | {static, Path :: binary(), {priv_file, App :: atom(), FileName :: binary()}}.

-opaque route_metadata() :: #route_metadata{}.

-nominal server_config() :: #{
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

-spec start(Config) -> Result when
    Config :: server_config(),
    Result :: {ok, pid()} | {error, term()}.
start(Config) when is_map(Config) ->
    % Compile routes
    Dispatch = compile_routes(maps:get(routes, Config)),
    ok = persistent_term:put(arizona_dispatch, Dispatch),

    % Get transport and protocol options
    DefaultPort = 1912,
    TransportOpts = get_transport_opts(Config, DefaultPort),
    ProtoOpts = get_proto_opts(Config),

    % Start listener based on scheme
    Scheme = maps:get(scheme, Config, http),
    Ref = arizona_listener,
    start_listener(Scheme, Ref, TransportOpts, ProtoOpts).

-spec stop() -> ok.
stop() ->
    ok = cowboy:stop_listener(arizona_listener),
    ok.

-spec get_route_metadata(Req) -> RouteMetadata when
    Req :: cowboy_req:req(),
    RouteMetadata :: route_metadata().
get_route_metadata(Req) ->
    {ok, _Req, Env} = cowboy_router:execute(
        Req,
        #{dispatch => {persistent_term, arizona_dispatch}}
    ),
    HandlerOpts = maps:get(handler_opts, Env),
    validate_and_create_route_metadata(HandlerOpts).

-spec get_route_handler(RouteMetadata) -> Handler when
    RouteMetadata :: route_metadata(),
    Handler :: module().
get_route_handler(#route_metadata{handler = Handler}) ->
    Handler.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

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
    ArizonaEnv = UserEnv#{dispatch => {persistent_term, arizona_dispatch}},
    UserProtoOpts#{env => ArizonaEnv}.

%% Start listener based on scheme
start_listener(http, Ref, TransportOpts, ProtoOpts) ->
    cowboy:start_clear(Ref, TransportOpts, ProtoOpts);
start_listener(https, Ref, TransportOpts, ProtoOpts) ->
    cowboy:start_tls(Ref, TransportOpts, ProtoOpts).

%% Validate handler options and create route metadata
validate_and_create_route_metadata(#{type := Type, handler := Handler}) when
    (Type =:= live orelse Type =:= live_websocket orelse Type =:= static),
    is_atom(Handler)
->
    #route_metadata{
        type = Type,
        handler = Handler
    }.
