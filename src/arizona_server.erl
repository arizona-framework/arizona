-module(arizona_server).

-export([start/1, stop/0]).
-export([compile_routes/1, route_to_cowboy/1]).

-type route() ::
    {live, Path :: binary(), LiveModule :: atom(), Opts :: map()}
    | {static, Path :: binary(), {dir, Directory :: binary()}}
    | {static, Path :: binary(), {file, FileName :: binary()}}
    | {static, Path :: binary(), {priv_dir, App :: atom(), Directory :: binary()}}
    | {static, Path :: binary(), {priv_file, App :: atom(), FileName :: binary()}}.

-type server_config() :: #{
    port := pos_integer(),
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
-spec compile_routes([route()]) -> term().
compile_routes(Routes) ->
    % Convert Arizona routes to Cowboy routes
    CowboyRoutes = [route_to_cowboy(Route) || Route <- Routes],

    % Compile the dispatch rules
    cowboy_router:compile([
        % Match any host
        {'_', CowboyRoutes}
    ]).

%% @doc Convert a single Arizona route to Cowboy route format
-spec route_to_cowboy(route()) -> {'_' | iodata(), module(), term()}.
route_to_cowboy({live, Path, LiveModule, Opts}) when is_binary(Path), is_atom(LiveModule) ->
    % LiveView routes use arizona_handler
    {Path, arizona_handler, #{
        type => live,
        handler => LiveModule,
        opts => Opts
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
