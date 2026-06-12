-module(arizona_mcp_session_SUITE).
-include_lib("stdlib/include/assert.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([
    lookup_after_start/1,
    lookup_unknown/1,
    dispatch_runs_method/1,
    stop_removes_registration/1,
    stale_pid_is_swept/1,
    idle_ttl_terminates/1,
    ignores_unknown_messages/1
]).

all() ->
    [
        lookup_after_start,
        lookup_unknown,
        dispatch_runs_method,
        stop_removes_registration,
        stale_pid_is_swept,
        idle_ttl_terminates,
        ignores_unknown_messages
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(arizona),
    Config.

end_per_suite(_Config) ->
    _ = application:stop(arizona),
    ok.

%% --------------------------------------------------------------------
%% Tests
%% --------------------------------------------------------------------

lookup_after_start(_Config) ->
    {Id, Pid} = start(60000),
    ?assertEqual({ok, Pid}, arizona_mcp_session_registry:lookup(Id)).

lookup_unknown(_Config) ->
    ?assertEqual(error, arizona_mcp_session_registry:lookup(~"no-such-session")).

dispatch_runs_method(_Config) ->
    {_Id, Pid} = start(60000),
    %% The session serves a method against its held state; tools/list works
    %% and the session is reusable across calls.
    {reply, #{~"result" := #{~"tools" := Tools}}} =
        arizona_mcp_session:dispatch(Pid, ~"tools/list", #{}, 1),
    ?assert(lists:member(~"add", [maps:get(~"name", T) || T <- Tools])),
    ?assertMatch({reply, _}, arizona_mcp_session:dispatch(Pid, ~"ping", #{}, 2)).

stop_removes_registration(_Config) ->
    {Id, Pid} = start(60000),
    ok = arizona_mcp_session:stop(Pid),
    ?assertEqual(error, arizona_mcp_session_registry:lookup(Id)).

stale_pid_is_swept(_Config) ->
    {Id, Pid} = start(60000),
    %% A brutal kill skips terminate/2, leaving a stale registry row; the
    %% next lookup detects the dead pid and sweeps it.
    Ref = erlang:monitor(process, Pid),
    exit(Pid, kill),
    receive
        {'DOWN', Ref, process, Pid, _} -> ok
    after 5000 -> ct:fail(session_did_not_die)
    end,
    ?assertEqual(error, arizona_mcp_session_registry:lookup(Id)).

idle_ttl_terminates(_Config) ->
    {Id, Pid} = start(100),
    Ref = erlang:monitor(process, Pid),
    receive
        {'DOWN', Ref, process, Pid, Reason} -> ?assertEqual(normal, Reason)
    after 5000 -> ct:fail(ttl_did_not_fire)
    end,
    ?assertEqual(error, arizona_mcp_session_registry:lookup(Id)).

ignores_unknown_messages(_Config) ->
    {Id, Pid} = start(60000),
    ok = gen_server:cast(Pid, some_cast),
    Pid ! some_info,
    %% The session shrugs off stray cast/info and keeps serving.
    ?assertEqual({ok, Pid}, arizona_mcp_session_registry:lookup(Id)),
    ?assertMatch({reply, _}, arizona_mcp_session:dispatch(Pid, ~"ping", #{}, 1)).

%% --------------------------------------------------------------------
%% Helpers
%% --------------------------------------------------------------------

start(TtlMs) ->
    Id = integer_to_binary(erlang:unique_integer([positive])),
    {ok, Pid} = arizona_mcp_sup:start_session(Id, session(), TtlMs),
    {Id, Pid}.

session() ->
    #{
        mod => arizona_mcp_test_server,
        state => #{},
        caps => #{tools => #{}, resources => #{}, prompts => #{}}
    }.
