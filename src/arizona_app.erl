-module(arizona_app).
-moduledoc false.
-behaviour(application).

%% --------------------------------------------------------------------
%% Behaviour (application) exports
%% --------------------------------------------------------------------

-export([start/2]).
-export([stop/1]).

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
        Config = arizona:get_config(),
        % Extract configs
        ReloaderConfig = maps:get(reloader, Config),
        ServerConfig = maps:get(server, Config),
        % Start supervisor with processed config
        SupConfig = create_sup_config(ReloaderConfig),
        {ok, SupPid} ?= arizona_sup:start_link(SupConfig),
        % Start reloader instances if enabled
        ok ?= maybe_start_reloader(ReloaderConfig),
        % Start server if enabled
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

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

create_sup_config(ReloaderConfig) ->
    ReloaderEnabled = maps:get(enabled, ReloaderConfig),
    #{watcher_enabled => ReloaderEnabled}.

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
