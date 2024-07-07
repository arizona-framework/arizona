-module(arizona_app).
-moduledoc false.

-behaviour(application).

%% Application callbacks.
-export([start/2]).
-export([stop/1]).

%% --------------------------------------------------------------------
%% Application callbacks.
%% --------------------------------------------------------------------

-spec start(StartType, StartArgs) -> supervisor:startlink_ret()
    when StartType :: application:start_type(),
         StartArgs :: term().
start(_StartType, _StartArgs) ->
    arizona_server:start(),
    arizona_sup:start_link().

-spec stop(State) -> ok
    when State :: term().
stop(_State) ->
    ok.
