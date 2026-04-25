-module(arizona_watcher_SUITE).
-include_lib("stdlib/include/assert.hrl").
-export([
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

-export([
    watch_bad_dir/1,
    watch_starts/1,
    event_matching/1,
    event_nonmatching/1,
    event_outside_dir/1,
    event_irrelevant/1,
    event_created/1,
    event_deleted/1,
    debounce_accumulates/1,
    debounce_resets_timer/1,
    debounce_deduplicates/1,
    callback_undefined/1,
    callback_receives_filtered/1,
    similarly_named_dir/1,
    stop_during_debounce/1,
    broadcast_on_debounce/1
]).

%% ============================================================================
%% CT callbacks
%% ============================================================================

all() ->
    [{group, tests}].

groups() ->
    [
        {tests, [sequence], [
            watch_bad_dir,
            watch_starts,
            event_matching,
            event_nonmatching,
            event_outside_dir,
            event_irrelevant,
            event_created,
            event_deleted,
            debounce_accumulates,
            debounce_resets_timer,
            debounce_deduplicates,
            callback_undefined,
            callback_receives_filtered,
            similarly_named_dir,
            stop_during_debounce,
            broadcast_on_debounce
        ]}
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(fs),
    _ = ensure_pg_started(),
    Config.

end_per_suite(Config) when is_list(Config) ->
    ensure_pg_stopped(),
    ok.

%% -- init_per_testcase -------------------------------------------------------

init_per_testcase(watch_bad_dir, Config) ->
    Config;
init_per_testcase(broadcast_on_debounce, Config) ->
    ok = arizona_pubsub:subscribe(arizona_watcher, self()),
    init_watcher(Config, watcher_opts(broadcast_on_debounce));
init_per_testcase(callback_undefined, Config) ->
    ok = arizona_pubsub:subscribe(arizona_watcher, self()),
    init_watcher(Config, watcher_opts(callback_undefined));
init_per_testcase(TC, Config) ->
    init_watcher(Config, watcher_opts(TC)).

%% -- end_per_testcase --------------------------------------------------------

end_per_testcase(broadcast_on_debounce, Config) ->
    ok = arizona_pubsub:unsubscribe(arizona_watcher, self()),
    cleanup(Config);
end_per_testcase(callback_undefined, Config) ->
    ok = arizona_pubsub:unsubscribe(arizona_watcher, self()),
    cleanup(Config);
end_per_testcase(_TC, Config) ->
    cleanup(Config).

%% ============================================================================
%% Tests
%% ============================================================================

watch_bad_dir(Config) when is_list(Config) ->
    ?assertMatch(
        {error, {not_a_directory, _}},
        arizona_watcher:start_link(
            "/nonexistent_" ++ integer_to_list(erlang:unique_integer([positive])), #{}
        )
    ).

watch_starts(Config) when is_list(Config) ->
    W = proplists:get_value(watcher, Config),
    ?assert(is_process_alive(W)).

event_matching(Config) when is_list(Config) ->
    W = proplists:get_value(watcher, Config),
    Dir = proplists:get_value(tmp_dir, Config),
    File = Dir ++ "/my_mod.erl",
    send_fs_event(W, File, [modified]),
    receive
        {cb, Files} -> ?assert(lists:member(File, Files))
    after 500 -> ct:fail(no_callback)
    end.

event_nonmatching(Config) when is_list(Config) ->
    W = proplists:get_value(watcher, Config),
    Dir = proplists:get_value(tmp_dir, Config),
    send_fs_event(W, Dir ++ "/readme.txt", [modified]),
    timer:sleep(100),
    assert_no_messages().

event_outside_dir(Config) when is_list(Config) ->
    W = proplists:get_value(watcher, Config),
    send_fs_event(W, "/some/other/dir/file.erl", [modified]),
    timer:sleep(100),
    assert_no_messages().

event_irrelevant(Config) when is_list(Config) ->
    W = proplists:get_value(watcher, Config),
    Dir = proplists:get_value(tmp_dir, Config),
    send_fs_event(W, Dir ++ "/file.erl", [isdir, renamed]),
    timer:sleep(100),
    assert_no_messages().

event_created(Config) when is_list(Config) ->
    W = proplists:get_value(watcher, Config),
    Dir = proplists:get_value(tmp_dir, Config),
    send_fs_event(W, Dir ++ "/new.erl", [created]),
    receive
        {cb, _} -> ok
    after 500 -> ct:fail(no_callback)
    end.

event_deleted(Config) when is_list(Config) ->
    W = proplists:get_value(watcher, Config),
    Dir = proplists:get_value(tmp_dir, Config),
    send_fs_event(W, Dir ++ "/old.erl", [deleted]),
    receive
        {cb, _} -> ok
    after 500 -> ct:fail(no_callback)
    end.

debounce_accumulates(Config) when is_list(Config) ->
    W = proplists:get_value(watcher, Config),
    Dir = proplists:get_value(tmp_dir, Config),
    F1 = Dir ++ "/a.erl",
    F2 = Dir ++ "/b.erl",
    F3 = Dir ++ "/c.erl",
    send_fs_event(W, F1, [modified]),
    send_fs_event(W, F2, [created]),
    send_fs_event(W, F3, [modified]),
    receive
        {cb, Files} ->
            ?assertEqual(lists:sort([F1, F2, F3]), Files)
    after 500 -> ct:fail(no_callback)
    end,
    timer:sleep(100),
    ?assertEqual([], [M || {cb, _} = M <- flush()]).

debounce_resets_timer(Config) when is_list(Config) ->
    W = proplists:get_value(watcher, Config),
    Dir = proplists:get_value(tmp_dir, Config),
    T0 = erlang:monotonic_time(millisecond),
    send_fs_event(W, Dir ++ "/a.erl", [modified]),
    timer:sleep(60),
    send_fs_event(W, Dir ++ "/b.erl", [modified]),
    receive
        {cb, T1} ->
            ?assert((T1 - T0) >= 130)
    after 500 -> ct:fail(no_callback)
    end.

debounce_deduplicates(Config) when is_list(Config) ->
    W = proplists:get_value(watcher, Config),
    Dir = proplists:get_value(tmp_dir, Config),
    File = Dir ++ "/dup.erl",
    send_fs_event(W, File, [modified]),
    send_fs_event(W, File, [modified]),
    receive
        {cb, Files} -> ?assertEqual([File], Files)
    after 500 -> ct:fail(no_callback)
    end.

callback_undefined(Config) when is_list(Config) ->
    W = proplists:get_value(watcher, Config),
    Dir = proplists:get_value(tmp_dir, Config),
    File = Dir ++ "/file.erl",
    %% Synchronize on the broadcast rather than wallclock: with
    %% callback => undefined the watcher must still fire the debounce,
    %% broadcast on pubsub, and stay alive.
    send_fs_event(W, File, [modified]),
    receive
        {arizona_watcher, Files} -> ?assert(lists:member(File, Files))
    after 500 -> ct:fail(no_broadcast)
    end,
    ?assert(is_process_alive(W)).

callback_receives_filtered(Config) when is_list(Config) ->
    W = proplists:get_value(watcher, Config),
    Dir = proplists:get_value(tmp_dir, Config),
    send_fs_event(W, Dir ++ "/mod.erl", [modified]),
    send_fs_event(W, Dir ++ "/readme.txt", [modified]),
    receive
        {cb, Files} -> ?assertEqual([Dir ++ "/mod.erl"], Files)
    after 500 -> ct:fail(no_callback)
    end.

similarly_named_dir(Config) when is_list(Config) ->
    W = proplists:get_value(watcher, Config),
    Dir = proplists:get_value(tmp_dir, Config),
    send_fs_event(W, Dir ++ "_bar/file.erl", [modified]),
    timer:sleep(100),
    assert_no_messages().

stop_during_debounce(Config) when is_list(Config) ->
    W = proplists:get_value(watcher, Config),
    Dir = proplists:get_value(tmp_dir, Config),
    Ref = monitor(process, W),
    send_fs_event(W, Dir ++ "/file.erl", [modified]),
    stop_watcher(Config),
    receive
        {'DOWN', Ref, process, W, normal} -> ok
    after 1000 -> ct:fail(not_stopped)
    end.

broadcast_on_debounce(Config) when is_list(Config) ->
    W = proplists:get_value(watcher, Config),
    Dir = proplists:get_value(tmp_dir, Config),
    File = Dir ++ "/file.erl",
    send_fs_event(W, File, [modified]),
    receive
        {arizona_watcher, Files} ->
            ?assert(lists:member(File, Files))
    after 500 -> ct:fail(no_broadcast)
    end.

%% ============================================================================
%% Setup / Cleanup
%% ============================================================================

watcher_opts(callback_undefined) ->
    #{callback => undefined, debounce => 20};
watcher_opts(event_matching) ->
    #{patterns => ["\\.erl$"], debounce => 30};
watcher_opts(event_nonmatching) ->
    #{patterns => ["\\.erl$"], debounce => 20};
watcher_opts(callback_receives_filtered) ->
    #{patterns => ["\\.erl$"], debounce => 30};
watcher_opts(debounce_accumulates) ->
    Self = self(),
    #{callback => fun(Files) -> Self ! {cb, lists:sort(Files)} end, debounce => 50};
watcher_opts(debounce_resets_timer) ->
    Self = self(),
    #{callback => fun(_) -> Self ! {cb, erlang:monotonic_time(millisecond)} end, debounce => 100};
watcher_opts(stop_during_debounce) ->
    #{debounce => 500};
watcher_opts(broadcast_on_debounce) ->
    #{debounce => 30};
watcher_opts(_TC) ->
    #{debounce => 20}.

init_watcher(Config, Opts) ->
    Dir = filename:join(
        "/tmp",
        "arizona_watcher_ct_" ++
            integer_to_list(erlang:unique_integer([positive]))
    ),
    ok = file:make_dir(Dir),
    Self = self(),
    WatcherOpts =
        case maps:is_key(callback, Opts) of
            true -> Opts;
            false -> Opts#{callback => fun(Files) -> Self ! {cb, Files} end}
        end,
    {ok, W} = arizona_watcher:start_link(Dir, WatcherOpts),
    [{watcher, W}, {tmp_dir, Dir} | Config].

stop_watcher(Config) ->
    case proplists:get_value(watcher, Config) of
        undefined ->
            ok;
        W ->
            try
                gen_server:stop(W)
            catch
                _:_ -> ok
            end
    end.

cleanup(Config) ->
    stop_watcher(Config),
    case proplists:get_value(tmp_dir, Config) of
        undefined -> ok;
        Dir -> ok = file:del_dir_r(Dir)
    end,
    _ = flush(),
    ok.

%% ============================================================================
%% Helpers
%% ============================================================================

ensure_pg_started() ->
    case erlang:whereis(arizona_pubsub) of
        undefined ->
            {ok, Pid} = pg:start_link(arizona_pubsub),
            unlink(Pid),
            Pid;
        Pid ->
            Pid
    end.

ensure_pg_stopped() ->
    case erlang:whereis(arizona_pubsub) of
        undefined ->
            ok;
        Pid ->
            case is_process_alive(Pid) of
                true -> exit(Pid, kill);
                false -> ok
            end,
            ok
    end.

send_fs_event(Watcher, FilePath, Events) ->
    _ = Watcher ! {self(), {fs, file_event}, {FilePath, Events}},
    ok.

flush() ->
    receive
        Msg -> [Msg | flush()]
    after 0 -> []
    end.

assert_no_messages() ->
    case flush() of
        [] -> ok;
        Msgs -> ct:fail({unexpected_messages, Msgs})
    end.
