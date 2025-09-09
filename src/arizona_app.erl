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
        % Start supervisor with config
        SupConfig = create_sup_config(),
        {ok, SupPid} ?= arizona_sup:start_link(SupConfig),
        % Start watcher instances if enabled
        WatcherConfig = create_watcher_config(),
        ok ?= maybe_start_watchers(WatcherConfig),
        % Start server if enabled
        ServerConfig = create_server_config(),
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

create_sup_config() ->
    WatcherConfig = application:get_env(arizona, watcher, #{}),
    WatcherEnabled = maps:get(enabled, WatcherConfig, false),
    #{watcher_enabled => WatcherEnabled}.

create_server_config() ->
    application:get_env(arizona, server, #{enabled => false}).

create_watcher_config() ->
    application:get_env(arizona, watcher, #{enabled => false}).

maybe_start_watchers(#{enabled := true, rules := Rules}) ->
    % arizona_watcher_sup should already be started by arizona_sup
    start_watcher_instances(Rules);
maybe_start_watchers(_WatcherConfig) ->
    ok.

start_watcher_instances([]) ->
    ok;
start_watcher_instances([Rule | Rules]) ->
    case arizona_watcher_sup:start_child(Rule) of
        {ok, _Pid} ->
            start_watcher_instances(Rules);
        {error, Reason} ->
            {error, {watcher_instance_failed, Reason}}
    end.

maybe_start_server(#{enabled := true} = ServerConfig) ->
    case arizona_server:start(ServerConfig) of
        {ok, _Pid} -> ok;
        {error, Reason} -> {error, {server_failed, Reason}}
    end;
maybe_start_server(_ServerConfig) ->
    ok.
