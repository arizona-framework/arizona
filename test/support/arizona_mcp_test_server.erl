-module(arizona_mcp_test_server).
-moduledoc false.
-behaviour(arizona_mcp).

-export([init/1]).
-export([tools/1]).
-export([handle_tool/3]).

init(_InitParams) ->
    {ok, #{name => ~"arizona_test", version => ~"0.1.0"}, #{tools => #{}}, #{}}.

tools(_State) ->
    [
        #{
            name => ~"add",
            description => ~"Add two integers",
            input_schema => #{
                type => ~"object",
                properties => #{
                    a => #{type => ~"integer"},
                    b => #{type => ~"integer"}
                },
                required => [~"a", ~"b"]
            }
        },
        #{
            name => ~"boom",
            description => ~"Always returns an in-band tool error",
            input_schema => #{type => ~"object", properties => #{}}
        },
        #{
            name => ~"crash",
            description => ~"Always raises (exercises the -32603 path)",
            input_schema => #{type => ~"object", properties => #{}}
        },
        #{
            name => ~"echo",
            title => ~"Echo",
            description => ~"Returns the map-form result with structured content",
            input_schema => #{type => ~"object", properties => #{}}
        }
    ].

handle_tool(~"add", #{~"a" := A, ~"b" := B}, State) ->
    {reply, integer_to_binary(A + B), State};
handle_tool(~"boom", _Args, State) ->
    {error, ~"kaboom", State};
handle_tool(~"crash", _Args, _State) ->
    error(intentional_crash);
handle_tool(~"echo", Args, State) ->
    {reply,
        #{
            content => [#{type => ~"text", text => ~"echo"}],
            structured_content => Args
        },
        State}.
