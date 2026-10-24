-module(arizona_reloader_rule_SUITE).
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
    watcher_init_args/1,
    watcher_bare_module_opts/1,
    watcher_init_error/1,
    watcher_unrelated_message/1,
    watcher_terminate_on_stop/1,
    watcher_terminate_on_shutdown/1,
    watcher_without_terminate/1,
    linked_crash_stops_rule/1,
    linked_normal_exit_ignored/1,
    watcher_stop_return/1,
    event_matching/1,
    event_nonmatching/1,
    event_outside_dir_passes/1,
    event_many_paths/1,
    debounce_accumulates/1,
    debounce_resets_timer/1,
    debounce_deduplicates/1,
    debounce_flushes_stale_fire/1,
    callback_undefined/1,
    callback_receives_filtered/1,
    stop_during_debounce/1,
    broadcast_on_debounce/1
]).

-export([
    child_specs_disabled/1,
    child_specs_no_rules/1,
    child_specs_one_per_rule/1,
    child_specs_watcher_with_opts/1,
    child_specs_no_watcher/1,
    child_specs_watcher_not_found/1,
    child_specs_watcher_not_implemented/1,
    child_specs_watcher_init_only/1,
    child_specs_watcher_bad_shape/1,
    child_specs_rule_without_directory/1,
    format_error_messages/1
]).

%% ============================================================================
%% CT callbacks
%% ============================================================================

all() ->
    [{group, rule}, {group, child_specs}].

groups() ->
    [
        {rule, [sequence], [
            watch_bad_dir,
            watch_starts,
            watcher_init_args,
            watcher_bare_module_opts,
            watcher_init_error,
            watcher_unrelated_message,
            watcher_terminate_on_stop,
            watcher_terminate_on_shutdown,
            watcher_without_terminate,
            linked_crash_stops_rule,
            linked_normal_exit_ignored,
            watcher_stop_return,
            event_matching,
            event_nonmatching,
            event_outside_dir_passes,
            event_many_paths,
            debounce_accumulates,
            debounce_resets_timer,
            debounce_deduplicates,
            debounce_flushes_stale_fire,
            callback_undefined,
            callback_receives_filtered,
            stop_during_debounce,
            broadcast_on_debounce
        ]},
        {child_specs, [parallel], [
            child_specs_disabled,
            child_specs_no_rules,
            child_specs_one_per_rule,
            child_specs_watcher_with_opts,
            child_specs_no_watcher,
            child_specs_watcher_not_found,
            child_specs_watcher_not_implemented,
            child_specs_watcher_init_only,
            child_specs_watcher_bad_shape,
            child_specs_rule_without_directory,
            format_error_messages
        ]}
    ].

init_per_suite(Config) ->
    _ = ensure_pg_started(),
    Config.

end_per_suite(Config) when is_list(Config) ->
    ensure_pg_stopped(),
    ok.

%% -- init_per_testcase -------------------------------------------------------

init_per_testcase(TC, Config) ->
    case proplists:get_value(name, proplists:get_value(tc_group_properties, Config)) of
        child_specs -> Config;
        rule -> init_rule_case(TC, Config)
    end.

init_rule_case(TC, Config) when
    TC =:= watch_bad_dir;
    TC =:= watcher_init_error;
    TC =:= watcher_bare_module_opts;
    TC =:= watcher_without_terminate
->
    init_dir(Config);
init_rule_case(TC, Config) when
    TC =:= broadcast_on_debounce; TC =:= callback_undefined
->
    ok = arizona_pubsub:subscribe(arizona_watcher, self()),
    init_rule(Config, rule_opts(TC));
init_rule_case(TC, Config) ->
    init_rule(Config, rule_opts(TC)).

%% -- end_per_testcase --------------------------------------------------------

end_per_testcase(TC, Config) when
    TC =:= broadcast_on_debounce; TC =:= callback_undefined
->
    ok = arizona_pubsub:unsubscribe(arizona_watcher, self()),
    cleanup(Config);
end_per_testcase(_TC, Config) ->
    cleanup(Config).

%% ============================================================================
%% rule group tests
%% ============================================================================

watch_bad_dir(Config) when is_list(Config) ->
    ?assertMatch(
        {error, {not_a_directory, _}},
        arizona_reloader_rule:start_link(
            "/nonexistent_" ++ integer_to_list(erlang:unique_integer([positive])),
            #{watcher => arizona_fake_watcher}
        )
    ).

watch_starts(Config) when is_list(Config) ->
    W = proplists:get_value(rule, Config),
    ?assert(is_process_alive(W)).

%% The watcher is started in the rule process, with the absolute directory and
%% the opts given as `{Module, Opts}`.
watcher_init_args(Config) when is_list(Config) ->
    Dir = proplists:get_value(tmp_dir, Config),
    Self = self(),
    receive
        {arizona_fake_watcher, init, InitDir, Opts} ->
            ?assertEqual(Dir, InitDir),
            ?assertEqual(#{notify => Self}, Opts)
    after 500 -> ct:fail(no_init)
    end.

%% A bare module gets `#{}` as its opts; a relative directory arrives absolute.
watcher_bare_module_opts(Config) when is_list(Config) ->
    Dir = proplists:get_value(tmp_dir, Config),
    {ok, Cwd} = file:get_cwd(),
    true = register(arizona_fake_watcher_bare_probe, self()),
    ok = file:set_cwd(filename:dirname(Dir)),
    try
        {ok, W} = arizona_reloader_rule:start_link(filename:basename(Dir), #{
            watcher => arizona_fake_watcher_bare
        }),
        receive
            {arizona_fake_watcher_bare, init, InitDir, Opts} ->
                ?assertEqual(Dir, InitDir),
                ?assertEqual(#{}, Opts)
        after 500 -> ct:fail(no_init)
        end,
        ok = gen_server:stop(W)
    after
        ok = file:set_cwd(Cwd),
        true = unregister(arizona_fake_watcher_bare_probe)
    end.

watcher_init_error(Config) when is_list(Config) ->
    Dir = proplists:get_value(tmp_dir, Config),
    process_flag(trap_exit, true),
    try
        ?assertEqual(
            {error, no_inotify},
            arizona_reloader_rule:start_link(Dir, #{
                watcher => {arizona_fake_watcher, #{fail => no_inotify}}
            })
        )
    after
        process_flag(trap_exit, false)
    end.

%% Every message the rule process does not own goes to the watcher; one it does
%% not recognise changes nothing and keeps the process up.
watcher_unrelated_message(Config) when is_list(Config) ->
    W = proplists:get_value(rule, Config),
    _ = W ! unrelated,
    timer:sleep(100),
    ?assert(is_process_alive(W)),
    assert_no_messages().

watcher_terminate_on_stop(Config) when is_list(Config) ->
    W = proplists:get_value(rule, Config),
    ok = gen_server:stop(W),
    receive
        {arizona_fake_watcher, terminate} -> ok
    after 500 -> ct:fail(no_terminate)
    end.

%% A supervisor shuts its children down with an exit signal from the parent,
%% which kills a process that does not trap exits before terminate/2 can run.
%% The rule traps them, so the watcher's terminate/1 runs on that path too.
watcher_terminate_on_shutdown(Config) when is_list(Config) ->
    W = proplists:get_value(rule, Config),
    process_flag(trap_exit, true),
    try
        exit(W, shutdown),
        receive
            {arizona_fake_watcher, terminate} -> ok
        after 500 -> ct:fail(no_terminate)
        end,
        receive
            {'EXIT', W, shutdown} -> ok
        after 500 -> ct:fail(not_stopped)
        end
    after
        process_flag(trap_exit, false)
    end.

%% Something the watcher linked to the rule dying abnormally stops the rule with
%% that reason (so its supervisor restarts the watch), instead of the trapped
%% exit leaving the rule up with a dead watch.
linked_crash_stops_rule(Config) when is_list(Config) ->
    W = proplists:get_value(rule, Config),
    Helper = proplists:get_value(helper, Config),
    Ref = monitor(process, W),
    unlink(W),
    _ = Helper ! {exit, inotify_gone},
    receive
        {'DOWN', Ref, process, W, inotify_gone} -> ok
    after 500 -> ct:fail(rule_not_stopped)
    end,
    receive
        {arizona_fake_watcher, terminate} -> ok
    after 500 -> ct:fail(no_terminate)
    end.

linked_normal_exit_ignored(Config) when is_list(Config) ->
    W = proplists:get_value(rule, Config),
    Helper = proplists:get_value(helper, Config),
    HelperRef = monitor(process, Helper),
    _ = Helper ! {exit, normal},
    receive
        {'DOWN', HelperRef, process, Helper, normal} -> ok
    after 500 -> ct:fail(helper_not_stopped)
    end,
    _ = sys:get_state(W),
    ?assert(is_process_alive(W)).

%% A watcher reporting its watch dead stops the rule with its reason, running
%% terminate/1, so the supervisor restarts the watch.
watcher_stop_return(Config) when is_list(Config) ->
    W = proplists:get_value(rule, Config),
    Ref = monitor(process, W),
    unlink(W),
    _ = W ! {arizona_fake_watcher, stop, backend_gone},
    receive
        {'DOWN', Ref, process, W, backend_gone} -> ok
    after 500 -> ct:fail(rule_not_stopped)
    end,
    receive
        {arizona_fake_watcher, terminate} -> ok
    after 500 -> ct:fail(no_terminate)
    end.

watcher_without_terminate(Config) when is_list(Config) ->
    Dir = proplists:get_value(tmp_dir, Config),
    {ok, W} = arizona_reloader_rule:start_link(Dir, #{watcher => arizona_fake_watcher_bare}),
    ?assertEqual(ok, gen_server:stop(W)).

event_matching(Config) when is_list(Config) ->
    W = proplists:get_value(rule, Config),
    Dir = proplists:get_value(tmp_dir, Config),
    File = Dir ++ "/my_mod.erl",
    change(W, [File]),
    receive
        {cb, Files} -> ?assert(lists:member(File, Files))
    after 500 -> ct:fail(no_callback)
    end.

event_nonmatching(Config) when is_list(Config) ->
    W = proplists:get_value(rule, Config),
    Dir = proplists:get_value(tmp_dir, Config),
    change(W, [Dir ++ "/readme.txt"]),
    timer:sleep(100),
    assert_no_messages().

%% The rule trusts the watcher's paths: one reported outside the directory's
%% literal prefix (an implementation resolving a symlinked directory reports
%% the resolved path) is kept when it matches the patterns.
event_outside_dir_passes(Config) when is_list(Config) ->
    W = proplists:get_value(rule, Config),
    File = "/private/elsewhere/file.erl",
    change(W, [File]),
    receive
        {cb, Files} -> ?assertEqual([File], Files)
    after 500 -> ct:fail(no_callback)
    end.

%% One watcher message may carry several paths; each is filtered on its own.
event_many_paths(Config) when is_list(Config) ->
    W = proplists:get_value(rule, Config),
    Dir = proplists:get_value(tmp_dir, Config),
    change(W, [Dir ++ "/a.erl", Dir ++ "/notes.txt", Dir ++ "/b.hrl", Dir ++ "/c.erl"]),
    receive
        {cb, Files} -> ?assertEqual([Dir ++ "/a.erl", Dir ++ "/c.erl"], lists:sort(Files))
    after 500 -> ct:fail(no_callback)
    end.

debounce_accumulates(Config) when is_list(Config) ->
    W = proplists:get_value(rule, Config),
    Dir = proplists:get_value(tmp_dir, Config),
    F1 = Dir ++ "/a.erl",
    F2 = Dir ++ "/b.erl",
    F3 = Dir ++ "/c.erl",
    change(W, [F1]),
    change(W, [F2]),
    change(W, [F3]),
    receive
        {cb, Files} ->
            ?assertEqual(lists:sort([F1, F2, F3]), Files)
    after 500 -> ct:fail(no_callback)
    end,
    timer:sleep(100),
    ?assertEqual([], [M || {cb, _} = M <- flush()]).

debounce_resets_timer(Config) when is_list(Config) ->
    W = proplists:get_value(rule, Config),
    Dir = proplists:get_value(tmp_dir, Config),
    T0 = erlang:monotonic_time(millisecond),
    change(W, [Dir ++ "/a.erl"]),
    timer:sleep(60),
    change(W, [Dir ++ "/b.erl"]),
    receive
        {cb, T1} ->
            ?assert((T1 - T0) >= 130)
    after 500 -> ct:fail(no_callback)
    end.

debounce_deduplicates(Config) when is_list(Config) ->
    W = proplists:get_value(rule, Config),
    Dir = proplists:get_value(tmp_dir, Config),
    File = Dir ++ "/dup.erl",
    change(W, [File]),
    change(W, [File, File]),
    receive
        {cb, Files} -> ?assertEqual([File], Files)
    after 500 -> ct:fail(no_callback)
    end.

%% A debounce timer that fires while the rule process is busy leaves a
%% `debounce_fire` queued *behind* a newly arriving change. When the change is
%% handled it arms a fresh timer, so the stale fire (if not flushed) makes the
%% debounce trigger early -- before the new window elapses -- defeating
%% coalescing. `sys:suspend/resume` forces exactly that mailbox ordering.
debounce_flushes_stale_fire(Config) when is_list(Config) ->
    W = proplists:get_value(rule, Config),
    Dir = proplists:get_value(tmp_dir, Config),
    F1 = Dir ++ "/first.erl",
    F2 = Dir ++ "/second.erl",
    Debounce = 100,
    %% Arm the debounce with a first change; `sys:suspend` returns only after the
    %% change (queued ahead of the suspend system message) is handled, so the
    %% timer is running by the time the process freezes.
    change(W, [F1]),
    ok = sys:suspend(W),
    %% Queue a second change, then wait past the window so the first timer's
    %% `debounce_fire` lands in the (frozen) mailbox behind it: [F2, fire].
    change(W, [F2]),
    timer:sleep(Debounce * 3),
    ok = sys:resume(W),
    %% Handling F2 re-arms the debounce; the stale fire must be flushed, so no
    %% callback may run before the fresh window elapses.
    receive
        {cb, _} -> ct:fail(fired_early)
    after Debounce div 2 -> ok
    end,
    %% It still fires exactly once, after the window, carrying both files.
    receive
        {cb, Files} -> ?assertEqual(lists:sort([F1, F2]), lists:sort(Files))
    after 500 -> ct:fail(no_callback)
    end.

callback_undefined(Config) when is_list(Config) ->
    W = proplists:get_value(rule, Config),
    Dir = proplists:get_value(tmp_dir, Config),
    File = Dir ++ "/file.erl",
    %% Synchronize on the broadcast rather than wallclock: with
    %% callback => undefined the rule must still fire the debounce,
    %% broadcast on pubsub, and stay alive.
    change(W, [File]),
    receive
        {arizona_watcher, Dir, Files} -> ?assert(lists:member(File, Files))
    after 500 -> ct:fail(no_broadcast)
    end,
    ?assert(is_process_alive(W)).

callback_receives_filtered(Config) when is_list(Config) ->
    W = proplists:get_value(rule, Config),
    Dir = proplists:get_value(tmp_dir, Config),
    change(W, [Dir ++ "/mod.erl"]),
    change(W, [Dir ++ "/readme.txt"]),
    receive
        {cb, Files} -> ?assertEqual([Dir ++ "/mod.erl"], Files)
    after 500 -> ct:fail(no_callback)
    end.

stop_during_debounce(Config) when is_list(Config) ->
    W = proplists:get_value(rule, Config),
    Dir = proplists:get_value(tmp_dir, Config),
    Ref = monitor(process, W),
    change(W, [Dir ++ "/file.erl"]),
    stop_rule(Config),
    receive
        {'DOWN', Ref, process, W, normal} -> ok
    after 1000 -> ct:fail(not_stopped)
    end.

broadcast_on_debounce(Config) when is_list(Config) ->
    W = proplists:get_value(rule, Config),
    Dir = proplists:get_value(tmp_dir, Config),
    File = Dir ++ "/file.erl",
    change(W, [File]),
    receive
        {arizona_watcher, Dir, Files} ->
            ?assert(lists:member(File, Files))
    after 500 -> ct:fail(no_broadcast)
    end.

%% ============================================================================
%% child_specs group tests
%% ============================================================================

child_specs_disabled(Config) when is_list(Config) ->
    ?assertEqual([], arizona_reloader_rule:child_specs(#{})),
    ?assertEqual(
        [],
        arizona_reloader_rule:child_specs(#{
            enabled => false, watcher => arizona_fake_watcher, rules => [#{directory => "src"}]
        })
    ).

%% Enabled with no rules reloads through the dev MCP only, so it needs no watcher.
child_specs_no_rules(Config) when is_list(Config) ->
    ?assertEqual([], arizona_reloader_rule:child_specs(#{enabled => true, rules => []})).

child_specs_one_per_rule(Config) when is_list(Config) ->
    Fun = fun(_Files) -> ok end,
    ?assertMatch(
        [
            #{
                id := {arizona_reloader_rule, 1},
                start :=
                    {arizona_reloader_rule, start_link, [
                        "src",
                        #{watcher := arizona_fake_watcher, patterns := ["\\.erl$"], callback := Fun}
                    ]}
            },
            #{
                id := {arizona_reloader_rule, 2},
                start :=
                    {arizona_reloader_rule, start_link, [
                        "priv", #{watcher := arizona_fake_watcher} = Opts2
                    ]}
            }
        ] when not is_map_key(directory, Opts2),
        arizona_reloader_rule:child_specs(#{
            enabled => true,
            watcher => arizona_fake_watcher,
            rules => [
                #{directory => "src", patterns => ["\\.erl$"], callback => Fun},
                #{directory => "priv"}
            ]
        })
    ).

child_specs_watcher_with_opts(Config) when is_list(Config) ->
    Watcher = {arizona_fake_watcher, #{interval => 500}},
    ?assertMatch(
        [#{start := {arizona_reloader_rule, start_link, ["src", #{watcher := Watcher}]}}],
        arizona_reloader_rule:child_specs(#{
            enabled => true, watcher => Watcher, rules => [#{directory => "src"}]
        })
    ).

child_specs_no_watcher(Config) when is_list(Config) ->
    ?assertError(
        no_watcher,
        arizona_reloader_rule:child_specs(#{enabled => true, rules => [#{directory => "src"}]})
    ).

child_specs_watcher_not_found(Config) when is_list(Config) ->
    ?assertError(
        {watcher_not_found, arizona_watcher_nonexistent},
        arizona_reloader_rule:child_specs(#{
            enabled => true,
            watcher => arizona_watcher_nonexistent,
            rules => [#{directory => "src"}]
        })
    ).

%% A loadable module that is not an implementation (no init/2, handle_info/2).
child_specs_watcher_not_implemented(Config) when is_list(Config) ->
    ?assertError(
        {watcher_invalid, lists},
        arizona_reloader_rule:child_specs(#{
            enabled => true, watcher => lists, rules => [#{directory => "src"}]
        })
    ).

child_specs_watcher_init_only(Config) when is_list(Config) ->
    ?assertError(
        {watcher_invalid, arizona_fake_watcher_init_only},
        arizona_reloader_rule:child_specs(#{
            enabled => true,
            watcher => arizona_fake_watcher_init_only,
            rules => [#{directory => "src"}]
        })
    ).

child_specs_watcher_bad_shape(Config) when is_list(Config) ->
    Watcher = {arizona_fake_watcher, [{interval, 500}]},
    ?assertError(
        {watcher_invalid, Watcher},
        arizona_reloader_rule:child_specs(#{
            enabled => true, watcher => Watcher, rules => [#{directory => "src"}]
        })
    ).

child_specs_rule_without_directory(Config) when is_list(Config) ->
    ?assertError(
        {rule_without_directory, #{patterns := [".*"]}},
        arizona_reloader_rule:child_specs(#{
            enabled => true, watcher => arizona_fake_watcher, rules => [#{patterns => [".*"]}]
        })
    ).

%% The raise site's `error_info` renders each reason into a message naming the fix.
format_error_messages(Config) when is_list(Config) ->
    Rendered = fun(Reloader) ->
        try arizona_reloader_rule:child_specs(Reloader) of
            Specs -> ct:fail({no_error, Specs})
        catch
            error:Reason:ST ->
                lists:flatten(io_lib:format("~ts", [erl_error:format_exception(error, Reason, ST)]))
        end
    end,
    Rules = [#{directory => "src"}],
    NoWatcher = Rendered(#{enabled => true, rules => Rules}),
    ?assertNotEqual(nomatch, string:find(NoWatcher, "{deps, [arizona_watcher_fs]}")),
    ?assertNotEqual(nomatch, string:find(NoWatcher, "watcher => arizona_watcher_fs")),
    NotFound = Rendered(#{enabled => true, watcher => arizona_watcher_nonexistent, rules => Rules}),
    ?assertNotEqual(nomatch, string:find(NotFound, "nonexistent is not on the code path")),
    Invalid = Rendered(#{enabled => true, watcher => lists, rules => Rules}),
    ?assertNotEqual(nomatch, string:find(Invalid, "lists is not an `arizona_watcher`")),
    NoDir = Rendered(#{enabled => true, watcher => arizona_fake_watcher, rules => [#{}]}),
    ?assertNotEqual(nomatch, string:find(NoDir, "has no `directory`")).

%% ============================================================================
%% Setup / Cleanup
%% ============================================================================

rule_opts(callback_undefined) ->
    #{callback => undefined, debounce => 20};
rule_opts(event_matching) ->
    #{patterns => ["\\.erl$"], debounce => 30};
rule_opts(event_nonmatching) ->
    #{patterns => ["\\.erl$"], debounce => 20};
rule_opts(event_many_paths) ->
    #{patterns => ["\\.erl$"], debounce => 30};
rule_opts(callback_receives_filtered) ->
    #{patterns => ["\\.erl$"], debounce => 30};
rule_opts(debounce_accumulates) ->
    Self = self(),
    #{callback => fun(Files) -> Self ! {cb, lists:sort(Files)} end, debounce => 50};
rule_opts(debounce_resets_timer) ->
    Self = self(),
    #{callback => fun(_) -> Self ! {cb, erlang:monotonic_time(millisecond)} end, debounce => 100};
rule_opts(debounce_flushes_stale_fire) ->
    #{debounce => 100};
rule_opts(stop_during_debounce) ->
    #{debounce => 500};
rule_opts(broadcast_on_debounce) ->
    #{debounce => 30};
rule_opts(TC) when
    TC =:= watcher_init_args;
    TC =:= watcher_terminate_on_stop;
    TC =:= watcher_terminate_on_shutdown;
    TC =:= watcher_stop_return
->
    #{watcher => {arizona_fake_watcher, #{notify => self()}}, debounce => 20};
rule_opts(TC) when TC =:= linked_crash_stops_rule; TC =:= linked_normal_exit_ignored ->
    #{watcher => {arizona_fake_watcher, #{notify => self(), link => true}}, debounce => 20};
rule_opts(_TC) ->
    #{debounce => 20}.

init_dir(Config) ->
    Dir = filename:join(
        "/tmp",
        "arizona_reloader_rule_ct_" ++
            integer_to_list(erlang:unique_integer([positive]))
    ),
    ok = file:make_dir(Dir),
    [{tmp_dir, Dir} | Config].

init_rule(Config0, Opts) ->
    Config = init_dir(Config0),
    Dir = proplists:get_value(tmp_dir, Config),
    Self = self(),
    RuleOpts0 = maps:merge(#{watcher => arizona_fake_watcher}, Opts),
    RuleOpts =
        case maps:is_key(callback, RuleOpts0) of
            true -> RuleOpts0;
            false -> RuleOpts0#{callback => fun(Files) -> Self ! {cb, Files} end}
        end,
    {ok, W} = arizona_reloader_rule:start_link(Dir, RuleOpts),
    receive
        {arizona_fake_watcher, linked, Helper} -> [{rule, W}, {helper, Helper} | Config]
    after 0 -> [{rule, W} | Config]
    end.

stop_rule(Config) ->
    case proplists:get_value(rule, Config) of
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
    stop_rule(Config),
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

%% Reports `Paths` as changed through the fake watcher.
change(Rule, Paths) ->
    _ = Rule ! {arizona_fake_watcher, Paths},
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
