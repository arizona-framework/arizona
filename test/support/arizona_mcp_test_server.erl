-module(arizona_mcp_test_server).
-moduledoc false.
-behaviour(arizona_mcp).

-export([init/1]).
-export([tools/1]).
-export([handle_tool/3]).
-export([resources/1]).
-export([read_resource/2]).
-export([prompts/1]).
-export([get_prompt/3]).
-export([channels/1]).
-export([terminate/2]).

init(_InitParams) ->
    {ok, #{name => ~"arizona_test", version => ~"0.1.0"},
        #{tools => #{}, resources => #{}, prompts => #{}}, #{}}.

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

resources(_State) ->
    [
        #{
            uri => ~"mem://greeting",
            name => ~"greeting",
            description => ~"A greeting",
            mime_type => ~"text/plain"
        },
        #{uri => ~"mem://locked", name => ~"locked"},
        #{uri => ~"mem://structured", name => ~"structured"}
    ].

read_resource(~"mem://greeting", State) ->
    {reply, ~"hello", State};
read_resource(~"mem://locked", State) ->
    {error, ~"resource is locked", State};
read_resource(~"mem://structured", State) ->
    %% Map form: the app supplies its own content entries verbatim.
    Contents = [#{uri => ~"mem://structured", mime_type => ~"text/plain", text => ~"raw"}],
    {reply, #{contents => Contents}, State}.

prompts(_State) ->
    [
        #{
            name => ~"greet",
            description => ~"Greet someone by name",
            arguments => [#{name => ~"who", description => ~"who to greet", required => true}]
        },
        #{name => ~"deny", description => ~"Always fails"}
    ].

get_prompt(~"greet", Args, State) ->
    Who = maps:get(~"who", Args, ~"world"),
    {reply,
        #{
            description => ~"A greeting",
            messages => [
                #{role => ~"user", content => #{type => ~"text", text => <<"Hello, ", Who/binary>>}}
            ]
        },
        State};
get_prompt(~"deny", _Args, State) ->
    {error, ~"prompt denied", State}.

channels(_State) ->
    [mcp_test_channel].

%% Signals teardown to a watcher pid when one was threaded through the state.
terminate(Reason, #{terminate_pid := Pid}) ->
    Pid ! {mcp_terminated, Reason},
    ok;
terminate(_Reason, _State) ->
    ok.
