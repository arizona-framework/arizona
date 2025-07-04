-module(arizona_server).

-export([start/1, stop/0]).

-type route() ::
    {live, Path :: binary(), LiveModule :: atom(), Opts :: map()}
    | {static, Path :: binary(), {dir, Directory :: string()}}
    | {static, Path :: binary(), {file, FileName :: string()}}
    | {static, Path :: binary(), {priv_dir, App :: atom(), Directory :: string()}}
    | {static, Path :: binary(), {priv_file, App :: atom(), FileName :: string()}}.

-type server_config() :: #{
    port := integer(),
    routes := [route()]
}.
-export_type([server_config/0]).

%% @doc Start the Arizona HTTP server with the given configuration
-spec start(server_config()) -> {ok, pid()} | {error, term()}.
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

%% @doc Stop the Arizona HTTP server
-spec stop() -> ok.
stop() ->
    cowboy:stop_listener(arizona_http_listener).

%% @doc Compile Arizona routes into Cowboy dispatch format
-spec compile_routes([route()]) -> cowboy_router:dispatch_rules().
compile_routes(Routes) ->
    % Convert Arizona routes to Cowboy routes
    CowboyRoutes = [route_to_cowboy(Route) || Route <- Routes],

    % Compile the dispatch rules
    cowboy_router:compile([
        % Match any host
        {'_', CowboyRoutes}
    ]).

%% @doc Convert a single Arizona route to Cowboy route format
-spec route_to_cowboy(route()) -> cowboy_router:route_rule().
route_to_cowboy({live, Path, LiveModule, Opts}) ->
    % LiveView routes use arizona_handler
    CowboyPath = path_to_cowboy(Path),
    {CowboyPath, arizona_handler, #{
        type => live,
        handler => LiveModule,
        opts => Opts
    }};
route_to_cowboy({static, Path, {dir, Directory}}) ->
    % Static file serving from filesystem directory
    to_cowboy_static_route(dir_path_to_cowboy(Path), {dir, Directory});
route_to_cowboy({static, Path, {file, FileName}}) ->
    % Static file serving for single file from priv directory
    to_cowboy_static_route(Path, {file, FileName});
route_to_cowboy({static, Path, {priv_dir, App, Directory}}) ->
    % Static file serving from priv directory
    to_cowboy_static_route(dir_path_to_cowboy(Path), {priv_dir, App, Directory});
route_to_cowboy({static, Path, {priv_file, App, FileName}}) ->
    % Static file serving for single file from priv directory
    to_cowboy_static_route(Path, {priv_file, App, FileName}).

to_cowboy_static_route(Path, Static) ->
    CowboyPath = path_to_cowboy(Path),
    {CowboyPath, cowboy_static, Static}.

dir_path_to_cowboy(Path) ->
    % Match any sub-path
    filename:join(Path, "[...]").

%% @doc Convert Arizona path format to Cowboy path format
-spec path_to_cowboy(string()) -> binary().
path_to_cowboy(Path) ->
    % For now, simple conversion - we'll add parameter support later
    list_to_binary(Path).
