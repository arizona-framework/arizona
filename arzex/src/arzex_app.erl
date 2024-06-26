%%%-------------------------------------------------------------------
%% @doc arzex public API
%% @end
%%%-------------------------------------------------------------------

-module(arzex_app).

-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_StartType, _StartArgs) ->
    arzex_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
