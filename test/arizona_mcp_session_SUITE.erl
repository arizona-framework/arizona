-module(arizona_mcp_session_SUITE).
-include_lib("stdlib/include/assert.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([
    lookup_after_start/1,
    lookup_unknown/1,
    dispatch_runs_method/1,
    state_accumulates_across_calls/1,
    error_outcome_threads_state/1,
    crash_preserves_prior_state/1,
    resource_read_threads_state/1,
    prompt_get_threads_state/1,
    stop_removes_registration/1,
    stale_pid_is_swept/1,
    idle_ttl_terminates/1,
    ignores_unknown_messages/1,
    notify_pushes_to_channel/1,
    broadcast_reaches_subscribed_session/1,
    resource_subscribe_receives_update/1,
    resource_unsubscribe_stops_updates/1,
    resource_subscribe_idempotent/1,
    resource_unsubscribe_without_subscribe/1,
    log_filtered_by_default_level/1,
    log_respects_set_level/1,
    cancel_stops_streaming_tool/1,
    cancel_unknown_id_is_noop/1,
    streaming_worker_crash_errors_conn/1,
    dispatch_bounded_timeout/1,
    streaming_tool_holds_session/1,
    terminate_kills_streaming_worker/1,
    buffered_dispatch_runs_in_worker/1,
    buffered_cancel_frees_caller/1,
    buffered_worker_crash_frees_caller/1,
    buffered_block_does_not_wedge_session/1,
    buffered_requests_serialize/1,
    cancel_queued_buffered_request/1,
    notify_without_channel_is_noop/1,
    notify_unknown_session_is_noop/1,
    log_unknown_session_is_noop/1,
    detach_re_enables_attach/1,
    channel_cancels_idle_ttl/1,
    detach_re_arms_ttl/1,
    replay_after_last_event_id/1,
    replay_buffer_evicts_oldest/1,
    fresh_attach_no_replay/1,
    terminate_calls_app/1
]).

all() ->
    [
        lookup_after_start,
        lookup_unknown,
        dispatch_runs_method,
        state_accumulates_across_calls,
        error_outcome_threads_state,
        crash_preserves_prior_state,
        resource_read_threads_state,
        prompt_get_threads_state,
        stop_removes_registration,
        stale_pid_is_swept,
        idle_ttl_terminates,
        ignores_unknown_messages,
        notify_pushes_to_channel,
        broadcast_reaches_subscribed_session,
        resource_subscribe_receives_update,
        resource_unsubscribe_stops_updates,
        resource_subscribe_idempotent,
        resource_unsubscribe_without_subscribe,
        log_filtered_by_default_level,
        log_respects_set_level,
        cancel_stops_streaming_tool,
        cancel_unknown_id_is_noop,
        streaming_worker_crash_errors_conn,
        dispatch_bounded_timeout,
        streaming_tool_holds_session,
        terminate_kills_streaming_worker,
        buffered_dispatch_runs_in_worker,
        buffered_cancel_frees_caller,
        buffered_worker_crash_frees_caller,
        buffered_block_does_not_wedge_session,
        buffered_requests_serialize,
        cancel_queued_buffered_request,
        notify_without_channel_is_noop,
        notify_unknown_session_is_noop,
        log_unknown_session_is_noop,
        detach_re_enables_attach,
        channel_cancels_idle_ttl,
        detach_re_arms_ttl,
        replay_after_last_event_id,
        replay_buffer_evicts_oldest,
        fresh_attach_no_replay,
        terminate_calls_app
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

state_accumulates_across_calls(_Config) ->
    {_Id, Pid} = start(60000),
    %% The fixture's `count` tool increments a session-held counter and returns
    %% it; across calls on the same session the value accumulates, proving the
    %% callback's returned state threads back into the session.
    ?assertEqual({false, ~"1"}, count_call(Pid, 1, #{})),
    ?assertEqual({false, ~"2"}, count_call(Pid, 2, #{})),
    ?assertEqual({false, ~"3"}, count_call(Pid, 3, #{})).

error_outcome_threads_state(_Config) ->
    {_Id, Pid} = start(60000),
    %% A `count` call that fails in-band (isError true) still advances the
    %% session counter, so the next call continues from it -- the error path
    %% threads state, not just the success path.
    ?assertEqual({false, ~"1"}, count_call(Pid, 1, #{})),
    ?assertEqual({true, ~"2"}, count_call(Pid, 2, #{~"fail" => true})),
    ?assertEqual({false, ~"3"}, count_call(Pid, 3, #{})).

crash_preserves_prior_state(_Config) ->
    {_Id, Pid} = start(60000),
    %% A crashing call answers -32603 but must leave the session's prior state
    %% intact: the next call continues the counter rather than resetting it.
    ?assertEqual({false, ~"1"}, count_call(Pid, 1, #{})),
    ?assertMatch(
        {error, #{~"error" := #{~"code" := -32603}}},
        arizona_mcp_session:dispatch(Pid, ~"tools/call", #{~"name" => ~"crash"}, 2)
    ),
    ?assertEqual({false, ~"2"}, count_call(Pid, 3, #{})).

resource_read_threads_state(_Config) ->
    {_Id, Pid} = start(60000),
    %% A stateful resource read threads its returned state back too.
    ?assertEqual(~"1", read_counter(Pid, 1)),
    ?assertEqual(~"2", read_counter(Pid, 2)).

prompt_get_threads_state(_Config) ->
    {_Id, Pid} = start(60000),
    %% A stateful prompt get threads its returned state back too.
    ?assertEqual(~"1", get_counter(Pid, 1)),
    ?assertEqual(~"2", get_counter(Pid, 2)).

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

resource_subscribe_receives_update(_Config) ->
    {_Id, Pid} = start(60000),
    ok = arizona_mcp_session:attach_channel(Pid, self(), undefined),
    Uri = ~"mem://greeting",
    %% Dispatched in the session process, so the session joins the per-uri
    %% channel; a resource_updated/1 broadcast then reaches it. subscribe answers
    %% the MCP empty result.
    {reply, #{~"result" := #{}}} =
        arizona_mcp_session:dispatch(Pid, ~"resources/subscribe", #{~"uri" => Uri}, 1),
    ok = arizona_mcp:resource_updated(Uri),
    receive
        {mcp_event, Frame} ->
            Bin = iolist_to_binary(Frame),
            ?assertNotEqual(nomatch, binary:match(Bin, ~"notifications/resources/updated")),
            ?assertNotEqual(nomatch, binary:match(Bin, Uri))
    after 5000 -> ct:fail(no_update)
    end.

resource_unsubscribe_stops_updates(_Config) ->
    {_Id, Pid} = start(60000),
    ok = arizona_mcp_session:attach_channel(Pid, self(), undefined),
    Uri = ~"mem://greeting",
    {reply, _} = arizona_mcp_session:dispatch(Pid, ~"resources/subscribe", #{~"uri" => Uri}, 1),
    {reply, _} = arizona_mcp_session:dispatch(Pid, ~"resources/unsubscribe", #{~"uri" => Uri}, 2),
    %% The synchronous unsubscribe has left the channel before we publish.
    ok = arizona_mcp:resource_updated(Uri),
    receive
        {mcp_event, _} -> ct:fail(unexpected_update)
    after 200 -> ok
    end.

resource_subscribe_idempotent(_Config) ->
    {_Id, Pid} = start(60000),
    Uri = ~"mem://greeting",
    %% A second subscribe to the same uri is idempotent (already_joined -> ok).
    {reply, #{~"result" := #{}}} =
        arizona_mcp_session:dispatch(Pid, ~"resources/subscribe", #{~"uri" => Uri}, 1),
    ?assertMatch(
        {reply, #{~"result" := #{}}},
        arizona_mcp_session:dispatch(Pid, ~"resources/subscribe", #{~"uri" => Uri}, 2)
    ).

resource_unsubscribe_without_subscribe(_Config) ->
    {_Id, Pid} = start(60000),
    %% Unsubscribing a uri that was never subscribed is a no-op (not_joined -> ok).
    ?assertMatch(
        {reply, #{~"result" := #{}}},
        arizona_mcp_session:dispatch(Pid, ~"resources/unsubscribe", #{~"uri" => ~"mem://never"}, 1)
    ).

log_filtered_by_default_level(_Config) ->
    {Id, Pid} = start(60000),
    ok = arizona_mcp_session:attach_channel(Pid, self(), undefined),
    %% Default level is info: debug drops, info delivers. Casts are ordered, so
    %% the only event that arrives is the info one.
    ok = arizona_mcp:log(Id, debug, ~"should-drop"),
    ok = arizona_mcp:log(Id, info, ~"should-send"),
    receive
        {mcp_event, Frame} ->
            Bin = iolist_to_binary(Frame),
            ?assertNotEqual(nomatch, binary:match(Bin, ~"notifications/message")),
            ?assertNotEqual(nomatch, binary:match(Bin, ~"should-send")),
            ?assertEqual(nomatch, binary:match(Bin, ~"should-drop"))
    after 5000 -> ct:fail(no_log)
    end,
    no_more_events().

log_respects_set_level(_Config) ->
    {Id, Pid} = start(60000),
    ok = arizona_mcp_session:attach_channel(Pid, self(), undefined),
    %% Raise the threshold to warning: info now drops, error delivers.
    {reply, #{~"result" := #{}}} =
        arizona_mcp_session:dispatch(Pid, ~"logging/setLevel", #{~"level" => ~"warning"}, 1),
    ok = arizona_mcp:log(Id, info, ~"info-drop"),
    ok = arizona_mcp:log(Id, error, ~"error-send"),
    receive
        {mcp_event, Frame} ->
            Bin = iolist_to_binary(Frame),
            ?assertNotEqual(nomatch, binary:match(Bin, ~"error-send")),
            ?assertEqual(nomatch, binary:match(Bin, ~"info-drop"))
    after 5000 -> ct:fail(no_log)
    end.

cancel_stops_streaming_tool(_Config) ->
    {_Id, Pid} = start(60000),
    ConnPid = self(),
    %% Start a blocking streaming tool in a worker, then cancel it by id. The
    %% casts are ordered, so the worker is registered before the cancel.
    ok = arizona_mcp_session:start_streaming_tool(Pid, ~"block", #{}, 7, ~"tok", ConnPid),
    ok = arizona_mcp_session:cancel(Pid, 7),
    %% The POST loop (us) is told to stop and gets no result.
    receive
        mcp_cancelled -> ok
    after 5000 -> ct:fail(no_cancel)
    end,
    no_result(),
    %% The session survived the cancel and still serves.
    ?assertMatch({reply, _}, arizona_mcp_session:dispatch(Pid, ~"ping", #{}, 8)).

cancel_unknown_id_is_noop(_Config) ->
    {_Id, Pid} = start(60000),
    %% A cancel that races past a request's completion (no in-flight stream for
    %% that id) is a silent no-op; the session keeps serving.
    ok = arizona_mcp_session:cancel(Pid, 999),
    ?assertMatch({reply, _}, arizona_mcp_session:dispatch(Pid, ~"ping", #{}, 1)).

streaming_worker_crash_errors_conn(_Config) ->
    {_Id, Pid} = start(60000),
    ConnPid = self(),
    %% A streaming worker that dies without reporting done makes the session
    %% answer the POST loop with -32603, rather than leaving it hanging.
    ok = arizona_mcp_session:start_streaming_tool(Pid, ~"selfkill", #{}, 7, ~"tok", ConnPid),
    receive
        {mcp_result, Object} ->
            ?assertMatch(#{~"error" := #{~"code" := -32603}}, Object)
    after 5000 -> ct:fail(no_error_result)
    end.

dispatch_bounded_timeout(_Config) ->
    {_Id, Pid} = start(60000),
    %% A buffered tool slower than the timeout frees the client with -32603 (the
    %% session keeps running it; the sleep tool finishes, so nothing leaks).
    Result = arizona_mcp_session:dispatch(Pid, ~"tools/call", #{~"name" => ~"sleep"}, 9, 50),
    ?assertMatch({error, #{~"error" := #{~"code" := -32603}}}, Result).

streaming_tool_holds_session(_Config) ->
    {_Id, Pid} = start(100),
    ConnPid = self(),
    %% A blocking streaming tool holds the session up past what the 100ms idle
    %% TTL would otherwise allow -- an in-flight stream is not idle-reaped.
    ok = arizona_mcp_session:start_streaming_tool(Pid, ~"block", #{}, 7, ~"tok", ConnPid),
    timer:sleep(250),
    ?assertMatch({reply, _}, arizona_mcp_session:dispatch(Pid, ~"ping", #{}, 8)),
    %% Cancelling the last stream re-arms the idle timer, so the session is reaped.
    Ref = erlang:monitor(process, Pid),
    ok = arizona_mcp_session:cancel(Pid, 7),
    receive
        {'DOWN', Ref, process, Pid, normal} -> ok
    after 5000 -> ct:fail(ttl_did_not_rearm)
    end.

terminate_kills_streaming_worker(_Config) ->
    {_Id, Pid} = start(60000),
    %% Stopping a session with an in-flight streaming worker kills the worker, so
    %% a blocking tool does not orphan and run forever.
    ok = arizona_mcp_session:start_streaming_tool(
        Pid, ~"block", #{watch => self()}, 7, ~"tok", self()
    ),
    Worker =
        receive
            {block_started, W} -> W
        after 5000 -> ct:fail(worker_did_not_start)
        end,
    WRef = erlang:monitor(process, Worker),
    ok = arizona_mcp_session:stop(Pid),
    receive
        {'DOWN', WRef, process, Worker, killed} -> ok
    after 5000 -> ct:fail(worker_not_killed)
    end.

buffered_dispatch_runs_in_worker(_Config) ->
    {_Id, Pid} = start(60000),
    %% A buffered tool runs in a worker, not the session process: the tool's
    %% pid is reported and differs from the session pid.
    Params = #{~"name" => ~"block", ~"arguments" => #{watch => self()}},
    Caller = spawn(fun() -> arizona_mcp_session:dispatch(Pid, ~"tools/call", Params, 7) end),
    Worker =
        receive
            {block_started, W} -> W
        after 5000 -> ct:fail(worker_did_not_start)
        end,
    ?assertNotEqual(Pid, Worker),
    ?assertNotEqual(Caller, Worker),
    %% The session stays responsive while the buffered tool blocks: a cancel is
    %% served and frees the caller with a -32603.
    ok = arizona_mcp_session:cancel(Pid, 7),
    ?assertMatch({reply, _}, arizona_mcp_session:dispatch(Pid, ~"ping", #{}, 8)).

buffered_cancel_frees_caller(_Config) ->
    {_Id, Pid} = start(60000),
    Self = self(),
    %% A blocking buffered tool from another process; cancelling it by id frees
    %% that caller with a -32603 and leaves the session serving.
    Params = #{~"name" => ~"block", ~"arguments" => #{watch => Self}},
    _Caller = spawn(fun() ->
        Result = arizona_mcp_session:dispatch(Pid, ~"tools/call", Params, 7),
        Self ! {caller_result, Result}
    end),
    receive
        {block_started, _W} -> ok
    after 5000 -> ct:fail(block_did_not_start)
    end,
    ok = arizona_mcp_session:cancel(Pid, 7),
    receive
        {caller_result, R} -> ?assertMatch({error, #{~"error" := #{~"code" := -32603}}}, R)
    after 5000 -> ct:fail(caller_not_freed)
    end,
    ?assertMatch({reply, _}, arizona_mcp_session:dispatch(Pid, ~"ping", #{}, 8)).

buffered_worker_crash_frees_caller(_Config) ->
    {_Id, Pid} = start(60000),
    %% An untrappable worker death (the tool kills itself, bypassing the crash
    %% guard) frees the caller with a -32603 rather than hanging it.
    R = arizona_mcp_session:dispatch(Pid, ~"tools/call", #{~"name" => ~"selfkill"}, 7),
    ?assertMatch({error, #{~"error" := #{~"code" := -32603}}}, R),
    ?assertMatch({reply, _}, arizona_mcp_session:dispatch(Pid, ~"ping", #{}, 8)).

buffered_block_does_not_wedge_session(_Config) ->
    {_Id, Pid} = start(100),
    Self = self(),
    %% A never-returning buffered tool no longer wedges the session: with a 100ms
    %% idle TTL the session is still reaped, and teardown kills the stuck worker.
    Params = #{~"name" => ~"block", ~"arguments" => #{watch => Self}},
    _Caller = spawn(fun() ->
        Result = arizona_mcp_session:dispatch(Pid, ~"tools/call", Params, 7),
        Self ! {caller_result, Result}
    end),
    Worker =
        receive
            {block_started, W} -> W
        after 5000 -> ct:fail(block_did_not_start)
        end,
    SRef = erlang:monitor(process, Pid),
    WRef = erlang:monitor(process, Worker),
    receive
        {'DOWN', SRef, process, Pid, normal} -> ok
    after 5000 -> ct:fail(session_wedged)
    end,
    receive
        {'DOWN', WRef, process, Worker, killed} -> ok
    after 5000 -> ct:fail(worker_not_killed)
    end.

buffered_requests_serialize(_Config) ->
    {_Id, Pid} = start(60000),
    Self = self(),
    %% A first buffered tool blocks, holding the single worker slot.
    P1 = #{~"name" => ~"block", ~"arguments" => #{watch => Self}},
    _C1 = spawn(fun() ->
        R1 = arizona_mcp_session:dispatch(Pid, ~"tools/call", P1, 7),
        Self ! {r1, R1}
    end),
    receive
        {block_started, _} -> ok
    after 5000 -> ct:fail(block_did_not_start)
    end,
    %% A second request queues behind it -- one worker at a time, so it must not
    %% run while the first blocks.
    _C2 = spawn(fun() ->
        R2 = arizona_mcp_session:dispatch(Pid, ~"ping", #{}, 8),
        Self ! {r2, R2}
    end),
    receive
        {r2, _} -> ct:fail(ran_out_of_order)
    after 300 -> ok
    end,
    %% Cancelling the first frees the slot, so the queued request then runs.
    ok = arizona_mcp_session:cancel(Pid, 7),
    receive
        {r1, R1} -> ?assertMatch({error, #{~"error" := #{~"code" := -32603}}}, R1)
    after 5000 -> ct:fail(first_not_freed)
    end,
    receive
        {r2, R2} -> ?assertMatch({reply, _}, R2)
    after 5000 -> ct:fail(queued_not_run)
    end.

cancel_queued_buffered_request(_Config) ->
    {_Id, Pid} = start(60000),
    Self = self(),
    P1 = #{~"name" => ~"block", ~"arguments" => #{watch => Self}},
    _C1 = spawn(fun() ->
        R1 = arizona_mcp_session:dispatch(Pid, ~"tools/call", P1, 7),
        Self ! {r1, R1}
    end),
    receive
        {block_started, _} -> ok
    after 5000 -> ct:fail(block_did_not_start)
    end,
    _C2 = spawn(fun() ->
        R2 = arizona_mcp_session:dispatch(Pid, ~"ping", #{}, 8),
        Self ! {r2, R2}
    end),
    %% Let the second request enqueue, then cancel it while it is still queued
    %% (not the active one); its caller is freed with a -32603.
    timer:sleep(50),
    ok = arizona_mcp_session:cancel(Pid, 8),
    receive
        {r2, R2} -> ?assertMatch({error, #{~"error" := #{~"code" := -32603}}}, R2)
    after 5000 -> ct:fail(queued_not_cancelled)
    end,
    %% The active (blocking) request is untouched; cancel it to clean up.
    ok = arizona_mcp_session:cancel(Pid, 7),
    receive
        {r1, _} -> ok
    after 5000 -> ct:fail(active_not_freed)
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

log_unknown_session_is_noop(_Config) ->
    %% arizona_mcp:log/3 to an unknown session id is a silent no-op.
    ?assertEqual(ok, arizona_mcp:log(~"no-such-session", info, ~"x")).

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

terminate_calls_app(_Config) ->
    Id = integer_to_binary(erlang:unique_integer([positive])),
    %% Thread a watcher pid through the handler state; the fixture's
    %% terminate/2 signals it on teardown.
    Session = #{
        mod => arizona_mcp_test_server,
        state => #{terminate_pid => self()},
        caps => #{tools => #{}},
        page_size => 50,
        log_min_severity => 1
    },
    {ok, Pid} = arizona_mcp_sup:start_session(Id, Session, #{ttl_ms => 60000, buffer_max => 256}),
    ok = arizona_mcp_session:stop(Pid),
    receive
        {mcp_terminated, _Reason} -> ok
    after 5000 -> ct:fail(terminate_not_called)
    end.

%% --------------------------------------------------------------------
%% Helpers
%% --------------------------------------------------------------------

no_more_events() ->
    receive
        {mcp_event, _} -> ct:fail(unexpected_event)
    after 100 -> ok
    end.

%% Assert no streaming result frame follows (a cancelled request gets none).
no_result() ->
    receive
        {mcp_result, _} -> ct:fail(unexpected_result)
    after 100 -> ok
    end.

%% Dispatch the `count` tool with Args; return {IsError, CounterText} from the
%% reply so a test can assert both the in-band error flag and the value.
count_call(Pid, Id, Args) ->
    Params = #{~"name" => ~"count", ~"arguments" => Args},
    {reply, #{~"result" := #{~"content" := [#{~"text" := Text}], ~"isError" := IsError}}} =
        arizona_mcp_session:dispatch(Pid, ~"tools/call", Params, Id),
    {IsError, Text}.

%% Read the stateful `mem://counter` resource and return its counter text.
read_counter(Pid, Id) ->
    {reply, #{~"result" := #{~"contents" := [#{~"text" := Text}]}}} =
        arizona_mcp_session:dispatch(Pid, ~"resources/read", #{~"uri" => ~"mem://counter"}, Id),
    Text.

%% Get the stateful `count` prompt and return its message counter text. The
%% message keeps atom keys until the JSON encode at the transport boundary.
get_counter(Pid, Id) ->
    {reply, #{~"result" := #{~"messages" := [#{content := #{text := Text}}]}}} =
        arizona_mcp_session:dispatch(Pid, ~"prompts/get", #{~"name" => ~"count"}, Id),
    Text.

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
        caps => #{
            tools => #{}, resources => #{}, prompts => #{}, logging => #{}, completions => #{}
        },
        page_size => 50,
        log_min_severity => 1
    }.
