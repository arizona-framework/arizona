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

-opaque route() ::
    {live, Path :: binary(), LiveModule :: module()}
    | {live_websocket, Path :: binary()}
    | {static, Path :: binary(), {dir, Directory :: binary()}}
    | {static, Path :: binary(), {file, FileName :: binary()}}
    | {static, Path :: binary(), {priv_dir, App :: atom(), Directory :: binary()}}
    | {static, Path :: binary(), {priv_file, App :: atom(), FileName :: binary()}}.

-opaque route_metadata() :: #route_metadata{}.

-nominal server_config() :: #{
    port := pos_integer(),
    routes := [route()]
}.

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

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

-spec stop() -> ok.
stop() ->
    ok = cowboy:stop_listener(arizona_http_listener),
    ok.

-spec get_route_metadata(Req) -> RouteMetadata when
    Req :: cowboy_req:req(),
    RouteMetadata :: route_metadata().
get_route_metadata(Req) ->
    {ok, _Req, Env} = cowboy_router:execute(
        Req,
        #{dispatch => {persistent_term, arizona_dispatch}}
    ),
    case maps:get(handler_opts, Env) of
        #{type := Type, handler := Handler} when
            (Type =:= live orelse Type =:= live_websocket orelse Type =:= static),
            is_atom(Handler)
        ->
            #route_metadata{
                type = Type,
                handler = Handler
            }
    end.

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
