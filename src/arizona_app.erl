%%%-------------------------------------------------------------------
%% @doc arizona public API
%% @end
%%%-------------------------------------------------------------------

-module(arizona_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    arizona_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
