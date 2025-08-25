-module(arizona_pubsub_SUITE).
-behaviour(ct_suite).
-include_lib("stdlib/include/assert.hrl").
-compile([export_all, nowarn_export_all]).

%% --------------------------------------------------------------------
%% Behaviour (ct_suite) callbacks
%% --------------------------------------------------------------------

all() ->
    [
        {group, pubsub_tests}
    ].

groups() ->
    [
        {pubsub_tests, [parallel], [
            join_leave_test,
            broadcast_test,
            broadcast_from_test,
            multiple_subscribers_test,
            cross_topic_isolation_test,
            empty_topic_test,
            subscriber_cleanup_test,
            message_ordering_test
        ]}
    ].

init_per_suite(Config) ->
    % Start pg group for arizona_pubsub (ignore if already started)
    PgPid =
        case pg:start(arizona_pubsub) of
            {ok, Pid} -> Pid;
            {error, {already_started, Pid}} -> Pid
        end,
    [{pg_pid, PgPid} | Config].

end_per_suite(Config) ->
    {pg_pid, PgPid} = proplists:lookup(pg_pid, Config),
    exit(PgPid, normal),

    ok.

%% --------------------------------------------------------------------
%% PubSub tests
%% --------------------------------------------------------------------

join_leave_test(_Config) ->
    ct:comment("Test joining and leaving topics"),
    Topic = atom_to_binary(?FUNCTION_NAME),

    % Initially no members
    ?assertEqual([], arizona_pubsub:get_members(Topic)),

    % Join topic
    ok = arizona_pubsub:join(Topic, self()),
    ?assertEqual([self()], arizona_pubsub:get_members(Topic)),

    % Leave topic
    ok = arizona_pubsub:leave(Topic, self()),
    ?assertEqual([], arizona_pubsub:get_members(Topic)),

    % Leave again (should return not_joined since already left)
    not_joined = arizona_pubsub:leave(Topic, self()).

broadcast_test(_Config) ->
    ct:comment("Test broadcasting messages to all subscribers"),
    Topic = atom_to_binary(?FUNCTION_NAME),
    Message = #{type => ~"test", data => ~"hello"},

    ok = arizona_pubsub:join(Topic, self()),
    ok = arizona_pubsub:broadcast(Topic, Message),
    expect_message(Topic, Message),
    ok = arizona_pubsub:leave(Topic, self()).

broadcast_from_test(_Config) ->
    ct:comment("Test broadcasting messages excluding sender"),
    Topic = atom_to_binary(?FUNCTION_NAME),
    Message = #{type => ~"test", data => ~"exclude_sender"},

    ok = arizona_pubsub:join(Topic, self()),
    ok = arizona_pubsub:broadcast_from(self(), Topic, Message),
    expect_no_message(),
    ok = arizona_pubsub:leave(Topic, self()).

multiple_subscribers_test(_Config) ->
    ct:comment("Test broadcasting to multiple subscribers"),
    Topic = atom_to_binary(?FUNCTION_NAME),
    Message = #{type => ~"multi", data => ~"broadcast"},

    % Spawn helper processes
    TestPids = spawn_subscribers(3, Topic),

    % Wait for all spawned processes to join
    wait_for_subscribers_joined(TestPids),

    % Also join from test process
    ok = arizona_pubsub:join(Topic, self()),

    % Verify all subscribers are registered
    Members = arizona_pubsub:get_members(Topic),
    % 3 spawned + self
    ?assertEqual(4, length(Members)),

    % Broadcast message
    ok = arizona_pubsub:broadcast(Topic, Message),

    expect_message(Topic, Message),

    % Verify all spawned processes received message
    verify_subscribers_received(TestPids, Topic, Message),

    % Clean up
    cleanup_subscribers(TestPids, Topic),
    ok = arizona_pubsub:leave(Topic, self()).

cross_topic_isolation_test(_Config) ->
    ct:comment("Test that topics are isolated from each other"),
    Topic1 = <<(atom_to_binary(?FUNCTION_NAME))/binary, "_1">>,
    Topic2 = <<(atom_to_binary(?FUNCTION_NAME))/binary, "_2">>,
    Message1 = #{topic => Topic1},
    Message2 = #{topic => Topic2},

    % Join only topic1
    ok = arizona_pubsub:join(Topic1, self()),

    % Broadcast to both topics
    ok = arizona_pubsub:broadcast(Topic1, Message1),
    ok = arizona_pubsub:broadcast(Topic2, Message2),

    expect_message(Topic1, Message1),
    expect_no_message(),

    % Clean up
    ok = arizona_pubsub:leave(Topic1, self()).

empty_topic_test(_Config) ->
    ct:comment("Test broadcasting to empty topics"),
    Topic = atom_to_binary(?FUNCTION_NAME),
    Message = #{data => ~"nobody_listening"},

    % Verify topic is empty
    ?assertEqual([], arizona_pubsub:get_members(Topic)),

    % Broadcasting to empty topic should work without error
    ok = arizona_pubsub:broadcast(Topic, Message),
    ok = arizona_pubsub:broadcast_from(self(), Topic, Message).

subscriber_cleanup_test(_Config) ->
    ct:comment("Test that dead processes are cleaned up from topics"),
    Topic = atom_to_binary(?FUNCTION_NAME),

    % Spawn a process that will die
    DeadPid = spawn(fun() ->
        arizona_pubsub:join(Topic, self()),
        timer:sleep(10),
        exit(normal)
    end),

    % Wait for process to join and die
    timer:sleep(50),

    % Dead process should eventually be removed from members
    % Note: pg handles cleanup automatically when processes die
    Members = arizona_pubsub:get_members(Topic),
    ?assertNot(lists:member(DeadPid, Members)).

message_ordering_test(_Config) ->
    ct:comment("Test message ordering for single subscriber"),
    Topic = atom_to_binary(?FUNCTION_NAME),
    Messages = [#{seq => N} || N <- lists:seq(1, 5)],

    % Join topic
    ok = arizona_pubsub:join(Topic, self()),

    % Send messages in order
    lists:foreach(
        fun(Message) ->
            arizona_pubsub:broadcast(Topic, Message)
        end,
        Messages
    ),

    % Receive messages and verify order
    ReceivedMessages = collect_messages(Topic, length(Messages), []),
    ExpectedMessages = [{pubsub_message, Topic, Msg} || Msg <- Messages],
    ?assertEqual(ExpectedMessages, ReceivedMessages),

    % Clean up
    ok = arizona_pubsub:leave(Topic, self()).

%% --------------------------------------------------------------------
%% Helper functions
%% --------------------------------------------------------------------

% Expect to receive a specific pubsub message
expect_message(Topic, Message) ->
    expect_message(Topic, Message, 1000).

expect_message(Topic, Message, Timeout) ->
    receive
        {pubsub_message, ReceivedTopic, ReceivedMessage} ->
            ?assertEqual(Topic, ReceivedTopic),
            ?assertEqual(Message, ReceivedMessage)
    after Timeout ->
        ct:fail("Did not receive expected pubsub message")
    end.

% Expect NOT to receive any pubsub message
expect_no_message() ->
    expect_no_message(100).

expect_no_message(Timeout) ->
    receive
        {pubsub_message, _, _} ->
            ct:fail("Should not have received any pubsub message")
    after Timeout ->
        ok
    end.

spawn_subscribers(Count, Topic) ->
    TestPid = self(),
    [
        spawn_link(fun() ->
            ok = arizona_pubsub:join(Topic, self()),
            TestPid ! {subscriber_joined, self()},
            subscriber_loop(TestPid)
        end)
     || _ <- lists:seq(1, Count)
    ].

subscriber_loop(TestPid) ->
    receive
        {pubsub_message, Topic, Message} ->
            % Send confirmation back to test process
            TestPid ! {subscriber_received, self(), Topic, Message},
            subscriber_loop(TestPid);
        stop ->
            ok
    after 5000 ->
        % Timeout - this indicates a test problem
        ct:fail("Subscriber process timed out waiting for message or stop signal")
    end.

verify_subscribers_received(Pids, ExpectedTopic, ExpectedMessage) ->
    lists:foreach(
        fun(Pid) ->
            receive
                {subscriber_received, Pid, ReceivedTopic, ReceivedMessage} ->
                    ?assertEqual(ExpectedTopic, ReceivedTopic),
                    ?assertEqual(ExpectedMessage, ReceivedMessage)
            after 1000 ->
                ct:fail(io_lib:format("Subscriber ~p did not receive message", [Pid]))
            end
        end,
        Pids
    ).

cleanup_subscribers(Pids, Topic) ->
    lists:foreach(
        fun(Pid) ->
            arizona_pubsub:leave(Topic, Pid),
            Pid ! stop
        end,
        Pids
    ).

collect_messages(_Topic, 0, Acc) ->
    lists:reverse(Acc);
collect_messages(Topic, Count, Acc) ->
    receive
        {pubsub_message, Topic, _Message} = Msg ->
            collect_messages(Topic, Count - 1, [Msg | Acc])
    after 1000 ->
        ct:fail(io_lib:format("Expected ~p more messages", [Count]))
    end.

wait_for_subscribers_joined(Pids) ->
    lists:foreach(
        fun(Pid) ->
            receive
                {subscriber_joined, Pid} ->
                    ok
            after 1000 ->
                ct:fail(io_lib:format("Subscriber ~p did not join in time", [Pid]))
            end
        end,
        Pids
    ).
