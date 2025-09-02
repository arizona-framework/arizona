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
        {ok, SupPid} ?= arizona_sup:start_link(),
        ok ?= arizona:start(get_env_config()),
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

get_env_config() ->
    ServerConfig = application:get_env(arizona, server, #{enabled => false}),
    ReloaderConfig = application:get_env(arizona, reloader, #{enabled => false}),
    #{
        server => ServerConfig,
        reloader => ReloaderConfig
    }.
