%%%-------------------------------------------------------------------
%% @doc arzex public API
%% @end
%%%-------------------------------------------------------------------

-module(arzex_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, _} = arizona_server:start(#{
        routes => [
            {"/", arizona_live_handler, {arzex_live_counter, render, #{}}}
        ]
    }),
    arzex_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
