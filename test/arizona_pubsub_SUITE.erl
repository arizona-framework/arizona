-module(arizona_pubsub_SUITE).
-include_lib("stdlib/include/assert.hrl").
-export([
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    end_per_testcase/2
]).
-export([
    join_leave/1,
    broadcast/1,
    broadcast_from/1,
    multiple_subscribers/1,
    cross_group_isolation/1,
    empty_group/1,
    subscriber_cleanup/1,
    message_ordering/1,
    duplicate_join/1
]).

all() ->
    [{group, tests}].

groups() ->
    [
        {tests, [sequence], [
            join_leave,
            broadcast,
            broadcast_from,
            multiple_subscribers,
            cross_group_isolation,
            empty_group,
            subscriber_cleanup,
            message_ordering,
            duplicate_join
        ]}
    ].

init_per_suite(Config) ->
    case erlang:whereis(arizona_pubsub) of
        undefined ->
            {ok, Pid} = pg:start_link(arizona_pubsub),
            unlink(Pid);
        _ ->
            ok
    end,
    Config.

end_per_suite(Config) when is_list(Config) ->
    ok.

end_per_testcase(_TC, _Config) ->
    %% Clean up: leave all groups this process may have joined.
    lists:foreach(
        fun(Group) ->
            case lists:member(self(), arizona_pubsub:subscribers(Group)) of
                true -> arizona_pubsub:unsubscribe(Group, self());
                false -> ok
            end
        end,
        [test_group, group_a, group_b, order_group]
    ),
    _ = flush(),
    ok.

join_leave(Config) when is_list(Config) ->
    ?assertEqual([], arizona_pubsub:subscribers(test_group)),
    ok = arizona_pubsub:subscribe(test_group, self()),
    ?assert(lists:member(self(), arizona_pubsub:subscribers(test_group))),
    ok = arizona_pubsub:unsubscribe(test_group, self()),
    ?assertNot(lists:member(self(), arizona_pubsub:subscribers(test_group))).

broadcast(Config) when is_list(Config) ->
    ok = arizona_pubsub:subscribe(test_group, self()),
    ok = arizona_pubsub:broadcast(test_group, hello),
    receive
        hello -> ok
    after 1000 -> ct:fail(timeout)
    end.

broadcast_from(Config) when is_list(Config) ->
    ok = arizona_pubsub:subscribe(test_group, self()),
    ok = arizona_pubsub:broadcast_from(self(), test_group, excluded),
    timer:sleep(50),
    ?assertEqual([], flush()).

multiple_subscribers(Config) when is_list(Config) ->
    Self = self(),
    Pids = [
        spawn_link(fun() ->
            ok = arizona_pubsub:subscribe(test_group, self()),
            Self ! {joined, self()},
            receive
                msg -> Self ! {got, self()}
            after 5000 -> exit(timeout)
            end
        end)
     || _ <:- [1, 2, 3]
    ],
    [
        receive
            {joined, P} -> ok
        after 1000 -> ct:fail({join_timeout, P})
        end
     || P <:- Pids
    ],
    ok = arizona_pubsub:broadcast(test_group, msg),
    Received = [
        receive
            {got, P} -> P
        after 1000 -> ct:fail({timeout, P})
        end
     || P <:- Pids
    ],
    ?assertEqual(lists:sort(Pids), lists:sort(Received)).

cross_group_isolation(Config) when is_list(Config) ->
    ok = arizona_pubsub:subscribe(group_a, self()),
    ok = arizona_pubsub:broadcast(group_b, leaked),
    timer:sleep(50),
    ?assertEqual([], flush()).

empty_group(Config) when is_list(Config) ->
    ?assertEqual(ok, arizona_pubsub:broadcast(test_group, nobody_here)).

subscriber_cleanup(Config) when is_list(Config) ->
    Pid = spawn(fun() ->
        receive
            stop -> ok
        after 5000 -> exit(timeout)
        end
    end),
    ok = arizona_pubsub:subscribe(test_group, Pid),
    ?assert(lists:member(Pid, arizona_pubsub:subscribers(test_group))),
    exit(Pid, kill),
    timer:sleep(50),
    ?assertNot(lists:member(Pid, arizona_pubsub:subscribers(test_group))).

message_ordering(Config) when is_list(Config) ->
    ok = arizona_pubsub:subscribe(order_group, self()),
    ok = arizona_pubsub:broadcast(order_group, 1),
    ok = arizona_pubsub:broadcast(order_group, 2),
    ok = arizona_pubsub:broadcast(order_group, 3),
    receive
        1 -> ok
    after 1000 -> ct:fail(timeout)
    end,
    receive
        2 -> ok
    after 1000 -> ct:fail(timeout)
    end,
    receive
        3 -> ok
    after 1000 -> ct:fail(timeout)
    end.

duplicate_join(Config) when is_list(Config) ->
    ok = arizona_pubsub:subscribe(test_group, self()),
    {error, already_joined} = arizona_pubsub:subscribe(test_group, self()),
    Count = length([
        P
     || P <- arizona_pubsub:subscribers(test_group),
        P =:= self()
    ]),
    ?assertEqual(1, Count),
    ok = arizona_pubsub:broadcast(test_group, dup_msg),
    receive
        dup_msg -> ok
    after 1000 -> ct:fail(timeout)
    end,
    timer:sleep(50),
    %% Should have received exactly one copy.
    ?assertEqual([], flush()).

%% Helpers

flush() ->
    receive
        Msg -> [Msg | flush()]
    after 0 -> []
    end.
