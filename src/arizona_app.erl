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
    StartRet :: {ok, pid()} | {error, term()}.
start(_StartType, _StartArgs) ->
    maybe
        {ok, _ServerPid} ?= arizona_server:start(arizona_config:endpoint()),
        {ok, _SupPid} ?= arizona_sup:start_link()
    else
        {error, Reason} ->
            {error, Reason}
    end.

-spec stop(State) -> ok when
    State :: term().
stop(_State) ->
    ok.
