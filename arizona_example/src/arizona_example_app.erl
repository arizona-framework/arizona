%%%-------------------------------------------------------------------
%% @doc arizona_example public API
%% @end
%%%-------------------------------------------------------------------

-module(arizona_example_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    arizona_example_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
