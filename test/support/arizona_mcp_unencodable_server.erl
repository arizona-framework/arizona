-module(arizona_mcp_unencodable_server).
-moduledoc false.
%% A server whose tools return content `json:encode/1` rejects -- a lone 0xE1
%% byte, valid latin1 but invalid UTF-8. Exercises the transport's encode
%% guards: such a tool must be answered with -32603, never take the connection
%% (or the session) down at encode time.
-behaviour(arizona_mcp).

-export([init/1]).
-export([tools/1]).
-export([handle_tool/4]).

init(_InitParams) ->
    {ok, #{name => ~"arizona_unencodable", version => ~"0.1.0"}, #{tools => #{}}, #{}}.

tools(_State) ->
    [
        #{
            name => ~"latin1",
            description => ~"Returns a binary that is not valid UTF-8",
            input_schema => #{type => ~"object", properties => #{}}
        },
        #{
            name => ~"latin1_progress",
            description => ~"Emits a progress message that is not valid UTF-8",
            input_schema => #{type => ~"object", properties => #{}}
        }
    ].

handle_tool(~"latin1", _Args, _Ctx, State) ->
    {reply, <<225>>, State};
handle_tool(~"latin1_progress", _Args, Ctx, State) ->
    ok = arizona_mcp:progress(Ctx, 1, #{message => <<225>>}),
    {reply, ~"unreachable while streaming", State}.
