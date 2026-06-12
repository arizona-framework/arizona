-module(arizona_mcp_crashing_server).
-moduledoc false.
%% A server whose init/1 raises, to exercise the initialize crash guard.
-behaviour(arizona_mcp).

-export([init/1]).
-export([tools/1]).
-export([handle_tool/3]).

init(_InitParams) ->
    error(intentional_init_crash).

tools(_State) ->
    [].

handle_tool(_Name, _Args, State) ->
    {reply, ~"", State}.
