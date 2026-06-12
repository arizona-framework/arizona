-module(arizona_mcp_test_server).
-moduledoc false.
-behaviour(arizona_mcp).

-export([init/1]).
-export([tools/1]).
-export([handle_tool/4]).
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
        },
        #{
            name => ~"count",
            description => ~"Increments and returns a per-session counter",
            input_schema => #{type => ~"object", properties => #{}}
        },
        #{
            name => ~"progress",
            description => ~"Emits two progress notifications then returns",
            input_schema => #{type => ~"object", properties => #{}}
        }
    ].

handle_tool(~"add", #{~"a" := A, ~"b" := B}, _Ctx, State) ->
    {reply, integer_to_binary(A + B), State};
handle_tool(~"boom", _Args, _Ctx, State) ->
    {error, ~"kaboom", State};
handle_tool(~"crash", _Args, _Ctx, _State) ->
    error(intentional_crash);
handle_tool(~"echo", Args, _Ctx, State) ->
    {reply,
        #{
            content => [#{type => ~"text", text => ~"echo"}],
            structured_content => Args
        },
        State};
handle_tool(~"count", Args, _Ctx, State) ->
    %% Stateful: each call increments the session-held counter, proving the
    %% returned state threads back. `fail => true` returns the in-band error
    %% outcome (still advancing the counter), proving the error path threads
    %% state too.
    Count = maps:get(count, State, 0) + 1,
    State1 = State#{count => Count},
    case Args of
        #{~"fail" := true} -> {error, integer_to_binary(Count), State1};
        _ -> {reply, integer_to_binary(Count), State1}
    end;
handle_tool(~"progress", _Args, Ctx, State) ->
    %% Emits progress while it runs, proving the streaming-POST path. The Ctx is
    %% inert for a non-streaming (no progressToken) call, so these are no-ops.
    ok = arizona_mcp:progress(Ctx, 1, #{total => 2, message => ~"step 1"}),
    ok = arizona_mcp:progress(Ctx, 2, #{total => 2, message => ~"step 2"}),
    {reply, ~"done", State}.

resources(_State) ->
    [
        #{
            uri => ~"mem://greeting",
            name => ~"greeting",
            description => ~"A greeting",
            mime_type => ~"text/plain"
        },
        #{uri => ~"mem://locked", name => ~"locked"},
        #{uri => ~"mem://structured", name => ~"structured"},
        #{uri => ~"mem://counter", name => ~"counter"}
    ].

read_resource(~"mem://greeting", State) ->
    {reply, ~"hello", State};
read_resource(~"mem://locked", State) ->
    {error, ~"resource is locked", State};
read_resource(~"mem://structured", State) ->
    %% Map form: the app supplies its own content entries verbatim.
    Contents = [#{uri => ~"mem://structured", mime_type => ~"text/plain", text => ~"raw"}],
    {reply, #{contents => Contents}, State};
read_resource(~"mem://counter", State) ->
    %% Stateful read: increments and returns the session counter, proving a
    %% resource read threads its returned state back too.
    Count = maps:get(count, State, 0) + 1,
    {reply, integer_to_binary(Count), State#{count => Count}}.

prompts(_State) ->
    [
        #{
            name => ~"greet",
            description => ~"Greet someone by name",
            arguments => [#{name => ~"who", description => ~"who to greet", required => true}]
        },
        #{name => ~"deny", description => ~"Always fails"},
        #{name => ~"count", description => ~"Increments and returns a per-session counter"}
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
    {error, ~"prompt denied", State};
get_prompt(~"count", _Args, State) ->
    %% Stateful get: increments and returns the session counter, proving a
    %% prompt get threads its returned state back too.
    Count = maps:get(count, State, 0) + 1,
    Message = #{role => ~"user", content => #{type => ~"text", text => integer_to_binary(Count)}},
    {reply, #{messages => [Message]}, State#{count => Count}}.

channels(_State) ->
    [mcp_test_channel].

%% Signals teardown to a watcher pid when one was threaded through the state.
terminate(Reason, #{terminate_pid := Pid}) ->
    Pid ! {mcp_terminated, Reason},
    ok;
terminate(_Reason, _State) ->
    ok.
