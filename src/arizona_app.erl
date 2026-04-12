-module(arizona_app).
-moduledoc """
OTP application entry point. Starts `arizona_sup` on boot.
""".
-behaviour(application).

%% --------------------------------------------------------------------
%% application callback exports
%% --------------------------------------------------------------------

-export([start/2]).
-export([stop/1]).

%% --------------------------------------------------------------------
%% application Callbacks
%% --------------------------------------------------------------------

-spec start(StartType, StartArgs) -> StartRet when
    StartType :: application:start_type(),
    StartArgs :: term(),
    StartRet :: {ok, Pid} | {error, ErrReason},
    Pid :: pid(),
    ErrReason :: term().
start(_Type, _Args) ->
    arizona_sup:start_link().

-spec stop(State) -> ok when
    State :: term().
stop(_State) ->
    ok.
