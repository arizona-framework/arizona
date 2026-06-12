-module(arizona_mcp_minimal_server).
-moduledoc false.
%% A tools-only server: it advertises no resources/prompts capability, so
%% the transport must answer resources/* and prompts/* with method-not-found.
-behaviour(arizona_mcp).

-export([init/1]).
-export([tools/1]).
-export([handle_tool/3]).

init(_InitParams) ->
    {ok, #{name => ~"minimal", version => ~"1.0.0"}, #{tools => #{}}, #{}}.

tools(_State) ->
    [].

handle_tool(Name, _Args, State) ->
    {error, <<"no such tool: ", Name/binary>>, State}.
