-module(arizona_app).
-moduledoc false.
-behaviour(application).

%% --------------------------------------------------------------------
%% Behaviour (application) exports
%% --------------------------------------------------------------------

-export([start/2]).
-export([stop/1]).
-export([get_server_config/0]).

%% --------------------------------------------------------------------
%% Behaviour (application) callbacks
%% --------------------------------------------------------------------

-spec start(StartType, StartArgs) -> StartRet when
    StartType :: application:start_type(),
    StartArgs :: term(),
    StartRet :: {ok, Pid} | {error, ErrReason},
    Pid :: pid(),
    ErrReason :: term().
start(_StartType, _StartArgs) ->
    maybe
        % Start supervisor with config
        SupConfig = create_sup_config(),
        {ok, SupPid} ?= arizona_sup:start_link(SupConfig),
        % Start reloader instances if enabled
        ReloaderConfig = get_reloader_config(),
        ok ?= maybe_start_reloader(ReloaderConfig),
        % Start server if enabled
        ServerConfig = get_server_config(),
        ok ?= maybe_start_server(ServerConfig),
        {ok, SupPid}
    else
        {error, Reason} ->
            {error, Reason}
    end.

-spec stop(State) -> ok when
    State :: term().
stop(_State) ->
    ok.

-spec get_server_config() -> Config when
    Config :: arizona_server:config().
get_server_config() ->
    application:get_env(arizona, server, #{enabled => false}).

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

create_sup_config() ->
    ReloaderConfig = application:get_env(arizona, reloader, #{}),
    ReloaderEnabled = maps:get(enabled, ReloaderConfig, false),
    #{watcher_enabled => ReloaderEnabled}.

get_reloader_config() ->
    application:get_env(arizona, reloader, #{enabled => false}).

maybe_start_reloader(ReloaderConfig) ->
    case arizona_reloader:start(ReloaderConfig) of
        ok -> ok;
        {error, Reason} -> {error, {reloader_failed, Reason}}
    end.

maybe_start_server(ServerConfig) ->
    case arizona_server:start(ServerConfig) of
        ok -> ok;
        {error, Reason} -> {error, {server_failed, Reason}}
    end.
