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
    ignores_unknown_messages/1,
    notify_pushes_to_channel/1,
    broadcast_reaches_subscribed_session/1,
    notify_without_channel_is_noop/1,
    notify_unknown_session_is_noop/1,
    detach_re_enables_attach/1,
    channel_cancels_idle_ttl/1,
    detach_re_arms_ttl/1,
    replay_after_last_event_id/1,
    replay_buffer_evicts_oldest/1,
    fresh_attach_no_replay/1
]).

all() ->
    [
        lookup_after_start,
        lookup_unknown,
        dispatch_runs_method,
        stop_removes_registration,
        stale_pid_is_swept,
        idle_ttl_terminates,
        ignores_unknown_messages,
        notify_pushes_to_channel,
        broadcast_reaches_subscribed_session,
        notify_without_channel_is_noop,
        notify_unknown_session_is_noop,
        detach_re_enables_attach,
        channel_cancels_idle_ttl,
        detach_re_arms_ttl,
        replay_after_last_event_id,
        replay_buffer_evicts_oldest,
        fresh_attach_no_replay
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

notify_pushes_to_channel(_Config) ->
    {_Id, Pid} = start(60000),
    %% This test process plays the SSE channel and receives framed events.
    ok = arizona_mcp_session:attach_channel(Pid, self(), undefined),
    ok = arizona_mcp_session:notify(Pid, ~"notifications/message", #{~"text" => ~"hi"}),
    receive
        {mcp_event, Frame} ->
            Bin = iolist_to_binary(Frame),
            ?assertNotEqual(nomatch, binary:match(Bin, ~"notifications/message")),
            ?assertNotEqual(nomatch, binary:match(Bin, ~"hi"))
    after 5000 -> ct:fail(no_event)
    end.

broadcast_reaches_subscribed_session(_Config) ->
    {_Id, Pid} = start(60000),
    ok = arizona_mcp_session:attach_channel(Pid, self(), undefined),
    %% The fixture's channels/1 subscribed the session to mcp_test_channel.
    ok = arizona_mcp:broadcast(mcp_test_channel, ~"notifications/tools/list_changed", #{}),
    receive
        {mcp_event, Frame} ->
            ?assertNotEqual(nomatch, binary:match(iolist_to_binary(Frame), ~"list_changed"))
    after 5000 -> ct:fail(no_broadcast)
    end.

notify_without_channel_is_noop(_Config) ->
    {_Id, Pid} = start(60000),
    ok = arizona_mcp_session:notify(Pid, ~"notifications/message", #{}),
    receive
        {mcp_event, _} -> ct:fail(unexpected_event)
    after 100 -> ok
    end,
    ?assertMatch({reply, _}, arizona_mcp_session:dispatch(Pid, ~"ping", #{}, 1)).

notify_unknown_session_is_noop(_Config) ->
    %% arizona_mcp:notify/3 to an unknown session id is a silent no-op.
    ?assertEqual(ok, arizona_mcp:notify(~"no-such-session", ~"notifications/message", #{})).

detach_re_enables_attach(_Config) ->
    {_Id, Pid} = start(60000),
    ok = arizona_mcp_session:attach_channel(Pid, self(), undefined),
    ?assertEqual(
        {error, already_attached}, arizona_mcp_session:attach_channel(Pid, self(), undefined)
    ),
    ok = arizona_mcp_session:detach_channel(Pid, self()),
    %% detach is a cast; a synchronous dispatch flushes it before we re-attach.
    ?assertMatch({reply, _}, arizona_mcp_session:dispatch(Pid, ~"ping", #{}, 1)),
    ?assertEqual(ok, arizona_mcp_session:attach_channel(Pid, self(), undefined)).

channel_cancels_idle_ttl(_Config) ->
    {_Id, Pid} = start(100),
    ok = arizona_mcp_session:attach_channel(Pid, self(), undefined),
    %% A live channel keeps the session up past what the idle TTL would allow.
    timer:sleep(250),
    ?assertMatch({reply, _}, arizona_mcp_session:dispatch(Pid, ~"ping", #{}, 1)).

detach_re_arms_ttl(_Config) ->
    {_Id, Pid} = start(100),
    ok = arizona_mcp_session:attach_channel(Pid, self(), undefined),
    ok = arizona_mcp_session:detach_channel(Pid, self()),
    Ref = erlang:monitor(process, Pid),
    receive
        {'DOWN', Ref, process, Pid, normal} -> ok
    after 5000 -> ct:fail(ttl_did_not_rearm)
    end.

replay_after_last_event_id(_Config) ->
    {_Id, Pid} = start(60000),
    %% Two events emitted with no channel attached: they buffer.
    ok = arizona_mcp_session:notify(Pid, ~"notifications/message", #{~"n" => 1}),
    ok = arizona_mcp_session:notify(Pid, ~"notifications/message", #{~"n" => 2}),
    %% Resume from event 1: only event 2 (id 2) replays, carrying its id.
    ok = arizona_mcp_session:attach_channel(Pid, self(), ~"1"),
    Frame = recv_event(),
    ?assertNotEqual(nomatch, binary:match(Frame, ~"\"n\":2")),
    ?assertNotEqual(nomatch, binary:match(Frame, ~"id: 2")),
    no_more_events().

replay_buffer_evicts_oldest(_Config) ->
    {_Id, Pid} = start(60000, 2),
    ok = arizona_mcp_session:notify(Pid, ~"notifications/message", #{~"n" => 1}),
    ok = arizona_mcp_session:notify(Pid, ~"notifications/message", #{~"n" => 2}),
    ok = arizona_mcp_session:notify(Pid, ~"notifications/message", #{~"n" => 3}),
    %% A 2-deep buffer keeps events 2 and 3; event 1 was evicted.
    ok = arizona_mcp_session:attach_channel(Pid, self(), ~"0"),
    Second = recv_event(),
    Third = recv_event(),
    ?assertNotEqual(nomatch, binary:match(Second, ~"\"n\":2")),
    ?assertNotEqual(nomatch, binary:match(Third, ~"\"n\":3")),
    no_more_events().

fresh_attach_no_replay(_Config) ->
    {_Id, Pid} = start(60000),
    ok = arizona_mcp_session:notify(Pid, ~"notifications/message", #{~"n" => 1}),
    %% A fresh attach (no Last-Event-ID) replays nothing.
    ok = arizona_mcp_session:attach_channel(Pid, self(), undefined),
    no_more_events().

%% --------------------------------------------------------------------
%% Helpers
%% --------------------------------------------------------------------

no_more_events() ->
    receive
        {mcp_event, _} -> ct:fail(unexpected_event)
    after 100 -> ok
    end.

start(TtlMs) ->
    start(TtlMs, 256).

start(TtlMs, BufferMax) ->
    Id = integer_to_binary(erlang:unique_integer([positive])),
    Opts = #{ttl_ms => TtlMs, buffer_max => BufferMax},
    {ok, Pid} = arizona_mcp_sup:start_session(Id, session(), Opts),
    {Id, Pid}.

recv_event() ->
    receive
        {mcp_event, Frame} -> iolist_to_binary(Frame)
    after 5000 -> ct:fail(no_event)
    end.

session() ->
    #{
        mod => arizona_mcp_test_server,
        state => #{},
        caps => #{tools => #{}, resources => #{}, prompts => #{}}
    }.
