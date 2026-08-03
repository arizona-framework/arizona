-module(arizona_reloader_SUITE).
-include_lib("stdlib/include/assert.hrl").
-include_lib("kernel/include/file.hrl").
-export([
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_group/2,
    end_per_group/2,
    init_per_testcase/2,
    end_per_testcase/2
]).

%% logger handler callback (used by the consistency integration tests)
-export([log/2]).

%% pubsub group tests
-export([
    join_without_pg_scope/1,
    join_with_pg_scope/1,
    join_idempotent/1,
    broadcast_no_members/1,
    broadcast_single/1,
    broadcast_multiple/1,
    broadcast_dead_member/1
]).

%% integration group tests
-export([
    watcher_triggers_reload/1,
    watch_delegates_to_watcher/1
]).

%% compile group tests
-export([
    get_error_default_undefined/1,
    clear_error_resets/1,
    compile_success_reloads_module/1,
    compile_module_name_differs_from_basename/1,
    compile_error_stores_error/1,
    compile_error_cleared_on_success/1,
    non_erl_files_skip_compile/1,
    mixed_files_only_compile_erl/1,
    deleted_file_skips_compile/1,
    deleted_file_alongside_good_compiles_good/1
]).

%% consistency group tests
-export([
    broken_edge_detected/1,
    consistent_reload_reports_nothing/1,
    unreloaded_target_not_flagged/1,
    stale_beam_detected/1,
    fresh_beam_not_stale/1,
    beam_facts_cached_by_mtime/1,
    beam_cache_owned_by_supervisor/1,
    beam_cache_survives_transient_caller/1,
    candidate_modules_excludes_otp/1,
    check_reports_broken_edge/1,
    check_clean_reports_nothing/1,
    check_best_effort_never_crashes/1
]).

%% ============================================================================
%% CT callbacks
%% ============================================================================

all() ->
    [{group, pubsub}, {group, integration}, {group, compile}, {group, consistency}].

groups() ->
    [
        {pubsub, [sequence], [
            join_without_pg_scope,
            join_with_pg_scope,
            join_idempotent,
            broadcast_no_members,
            broadcast_single,
            broadcast_multiple,
            broadcast_dead_member
        ]},
        {integration, [sequence], [
            watcher_triggers_reload,
            watch_delegates_to_watcher
        ]},
        {compile, [sequence], [
            get_error_default_undefined,
            clear_error_resets,
            compile_success_reloads_module,
            compile_module_name_differs_from_basename,
            compile_error_stores_error,
            compile_error_cleared_on_success,
            non_erl_files_skip_compile,
            mixed_files_only_compile_erl,
            deleted_file_skips_compile,
            deleted_file_alongside_good_compiles_good
        ]},
        {consistency, [sequence], [
            broken_edge_detected,
            consistent_reload_reports_nothing,
            unreloaded_target_not_flagged,
            stale_beam_detected,
            fresh_beam_not_stale,
            beam_facts_cached_by_mtime,
            beam_cache_owned_by_supervisor,
            beam_cache_survives_transient_caller,
            candidate_modules_excludes_otp,
            check_reports_broken_edge,
            check_clean_reports_nothing,
            check_best_effort_never_crashes
        ]}
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(fs),
    Config.

end_per_suite(Config) when is_list(Config) ->
    ok.

init_per_group(pubsub, Config) ->
    ensure_pg_stopped(),
    Config;
init_per_group(integration, Config) ->
    _ = ensure_pg_started(),
    Config;
init_per_group(compile, Config) ->
    Config;
init_per_group(consistency, Config) ->
    %% The beam-facts cache table is created and owned by `arizona_sup`, so the
    %% consistency checks run against a started app exactly as they do in dev.
    {ok, _} = application:ensure_all_started(arizona),
    Config.

end_per_group(compile, _Config) ->
    ok;
end_per_group(consistency, _Config) ->
    _ = application:stop(arizona),
    ok;
end_per_group(_Group, _Config) ->
    ensure_pg_stopped(),
    ok.

init_per_testcase(join_without_pg_scope, Config) ->
    ensure_pg_stopped(),
    Config;
init_per_testcase(TC, Config) ->
    case lists:member(TC, pubsub_tests()) of
        true ->
            _ = ensure_pg_started(),
            Config;
        false ->
            arizona_reloader:clear_error(),
            Dir = make_tmp_dir(TC),
            [{tmp_dir, Dir} | Config]
    end.

end_per_testcase(_TC, Config) ->
    _ =
        case proplists:get_value(tmp_dir, Config) of
            undefined -> ok;
            Dir -> rm_rf(Dir)
        end,
    _ = flush(),
    Config.

%% ============================================================================
%% pubsub group tests
%% ============================================================================

join_without_pg_scope(Config) when is_list(Config) ->
    ?assertEqual(undefined, erlang:whereis(arizona_pubsub)),
    ?assertEqual(ok, arizona_reloader:join(self())).

join_with_pg_scope(Config) when is_list(Config) ->
    ?assertEqual(ok, arizona_reloader:join(self())),
    Subscribers = arizona_pubsub:subscribers(arizona_reloader),
    ?assert(lists:member(self(), Subscribers)),
    arizona_pubsub:unsubscribe(arizona_reloader, self()).

join_idempotent(Config) when is_list(Config) ->
    arizona_reloader:join(self()),
    arizona_reloader:join(self()),
    Subscribers = arizona_pubsub:subscribers(arizona_reloader),
    Count = length([P || P <- Subscribers, P =:= self()]),
    ?assertEqual(1, Count),
    arizona_pubsub:unsubscribe(arizona_reloader, self()).

broadcast_no_members(Config) when is_list(Config) ->
    ?assertEqual(ok, arizona_reloader:broadcast()),
    assert_no_messages().

broadcast_single(Config) when is_list(Config) ->
    arizona_reloader:join(self()),
    arizona_reloader:broadcast(),
    receive
        {arizona_reloader, reload} -> ok
    after 1000 -> ct:fail(timeout)
    end,
    arizona_pubsub:unsubscribe(arizona_reloader, self()).

broadcast_multiple(Config) when is_list(Config) ->
    Self = self(),
    Pids = [
        spawn_link(fun() ->
            arizona_reloader:join(self()),
            Self ! {joined, self()},
            receive
                {arizona_reloader, reload} ->
                    Self ! {got_reload, self()}
            after 2000 ->
                Self ! {timeout, self()}
            end
        end)
     || _ <- [1, 2, 3]
    ],
    %% Wait for all to join.
    [
        receive
            {joined, P} -> ok
        after 1000 -> ct:fail({join_timeout, P})
        end
     || P <- Pids
    ],
    arizona_reloader:broadcast(),
    Received = [
        receive
            {got_reload, P} -> P;
            {timeout, P} -> ct:fail({reload_timeout, P})
        after 2000 ->
            ct:fail(collect_timeout)
        end
     || P <- Pids
    ],
    ?assertEqual(lists:sort(Pids), lists:sort(Received)).

broadcast_dead_member(Config) when is_list(Config) ->
    Pid = spawn(fun() ->
        arizona_reloader:join(self()),
        receive
            stop -> ok
        after 5000 -> exit(timeout)
        end
    end),
    timer:sleep(20),
    exit(Pid, kill),
    timer:sleep(50),
    ?assertEqual(ok, arizona_reloader:broadcast()).

%% ============================================================================
%% integration group tests
%% ============================================================================

watcher_triggers_reload(Config) ->
    Dir = proplists:get_value(tmp_dir, Config),
    arizona_reloader:join(self()),
    {ok, W} = arizona_watcher:start_link(Dir, #{
        debounce => 30,
        callback => fun(_Files) -> arizona_reloader:broadcast() end
    }),
    send_fs_event(W, Dir ++ "/file.erl", [modified]),
    receive
        {arizona_reloader, reload} -> ok
    after 500 -> ct:fail(no_reload)
    end,
    gen_server:stop(W),
    arizona_pubsub:unsubscribe(arizona_reloader, self()).

watch_delegates_to_watcher(Config) ->
    Dir = proplists:get_value(tmp_dir, Config),
    Self = self(),
    {ok, W} = arizona_watcher:start_link(Dir, #{
        callback => fun(Files) -> Self ! {cb, Files} end,
        debounce => 30
    }),
    File = Dir ++ "/test.erl",
    send_fs_event(W, File, [modified]),
    receive
        {cb, Files} -> ?assert(lists:member(File, Files))
    after 500 -> ct:fail(no_callback)
    end,
    gen_server:stop(W).

%% ============================================================================
%% compile group tests (arizona_reloader)
%% ============================================================================

get_error_default_undefined(Config) when is_list(Config) ->
    ?assertEqual(undefined, arizona_reloader:get_error()).

clear_error_resets(Config) when is_list(Config) ->
    %% Simulate a stored error, then clear it.
    persistent_term:put(arizona_compile_error, #{errors => []}),
    ?assertMatch(#{errors := []}, arizona_reloader:get_error()),
    arizona_reloader:clear_error(),
    ?assertEqual(undefined, arizona_reloader:get_error()).

compile_success_reloads_module(Config) ->
    Dir = proplists:get_value(tmp_dir, Config),
    File = filename:join(Dir, "arizona_dev_ct_good.erl"),
    ok = file:write_file(File, good_module_src(1)),
    ?assertEqual(ok, arizona_reloader:compile([File])),
    ?assertEqual(undefined, arizona_reloader:get_error()),
    ?assertEqual(1, erlang:apply(arizona_dev_ct_good, value, [])),
    %% Recompile with updated value to confirm hot reload.
    ok = file:write_file(File, good_module_src(2)),
    ?assertEqual(ok, arizona_reloader:compile([File])),
    ?assertEqual(2, erlang:apply(arizona_dev_ct_good, value, [])).

%% A file whose `-module` differs from its filename compiles to the *declared*
%% module name, not the basename. manual_compile/2 must bind and reload that
%% actual module rather than pattern-matching the basename-derived atom (which
%% `case_clause`es, crashing the reloader gen_server).
compile_module_name_differs_from_basename(Config) ->
    Dir = proplists:get_value(tmp_dir, Config),
    File = filename:join(Dir, "arizona_dev_ct_basename.erl"),
    ok = file:write_file(File, mismatch_module_src()),
    ?assertEqual(ok, arizona_reloader:compile([File])),
    ?assertEqual(undefined, arizona_reloader:get_error()),
    ?assertEqual(ok, erlang:apply(arizona_dev_ct_declared, check, [])).

compile_error_stores_error(Config) ->
    Dir = proplists:get_value(tmp_dir, Config),
    File = filename:join(Dir, "arizona_dev_ct_bad.erl"),
    ok = file:write_file(File, bad_module_src()),
    ?assertMatch({error, #{errors := [_ | _]}}, arizona_reloader:compile([File])),
    ?assertMatch(#{errors := [_ | _]}, arizona_reloader:get_error()).

compile_error_cleared_on_success(Config) ->
    Dir = proplists:get_value(tmp_dir, Config),
    BadFile = filename:join(Dir, "arizona_dev_ct_bad2.erl"),
    GoodFile = filename:join(Dir, "arizona_dev_ct_good2.erl"),
    ok = file:write_file(BadFile, bad_module_src2()),
    %% First: compile error.
    ?assertMatch({error, _}, arizona_reloader:compile([BadFile])),
    ?assertMatch(#{errors := _}, arizona_reloader:get_error()),
    %% Second: successful compile clears error.
    ok = file:write_file(GoodFile, good_module_src2()),
    ?assertEqual(ok, arizona_reloader:compile([GoodFile])),
    ?assertEqual(undefined, arizona_reloader:get_error()).

non_erl_files_skip_compile(Config) when is_list(Config) ->
    ?assertEqual(ok, arizona_reloader:compile(["/tmp/app.js", "/tmp/style.css"])),
    ?assertEqual(undefined, arizona_reloader:get_error()).

mixed_files_only_compile_erl(Config) ->
    Dir = proplists:get_value(tmp_dir, Config),
    ErlFile = filename:join(Dir, "arizona_dev_ct_mixed.erl"),
    JsFile = filename:join(Dir, "app.js"),
    ok = file:write_file(ErlFile, mixed_good_module_src()),
    ok = file:write_file(JsFile, <<"// js">>),
    ?assertEqual(ok, arizona_reloader:compile([JsFile, ErlFile])),
    ?assertEqual(undefined, arizona_reloader:get_error()),
    ?assertEqual(ok, erlang:apply(arizona_dev_ct_mixed, check, [])).

%% The watcher treats a delete as a relevant event, so the manual-compile
%% fallback can receive a path that no longer exists. Compiling it would stash
%% a spurious {epp, enoent} on the dev error page until an unrelated successful
%% wave clears it -- a vanished path is skipped instead.
deleted_file_skips_compile(Config) ->
    Dir = proplists:get_value(tmp_dir, Config),
    Missing = filename:join(Dir, "arizona_dev_ct_gone.erl"),
    ?assertEqual(ok, arizona_reloader:compile([Missing])),
    ?assertEqual(undefined, arizona_reloader:get_error()).

%% A delete arriving in the same wave as a real edit must not poison the wave:
%% the surviving file compiles and no error is stashed.
deleted_file_alongside_good_compiles_good(Config) ->
    Dir = proplists:get_value(tmp_dir, Config),
    Missing = filename:join(Dir, "arizona_dev_ct_gone2.erl"),
    Good = filename:join(Dir, "arizona_dev_ct_alive.erl"),
    ok = file:write_file(Good, alive_module_src()),
    ?assertEqual(ok, arizona_reloader:compile([Missing, Good])),
    ?assertEqual(undefined, arizona_reloader:get_error()),
    ?assertEqual(ok, erlang:apply(arizona_dev_ct_alive, check, [])).

%% ============================================================================
%% consistency group tests (arizona_reloader_consistency)
%% ============================================================================

%% A caller whose beam still calls a function the reloaded callee dropped is the
%% exact broken edge the check exists to surface.
broken_edge_detected(Config) ->
    Dir = proplists:get_value(tmp_dir, Config),
    Caller = az_cc_caller_a,
    Callee = az_cc_callee_a,
    {Caller, _} = compile_and_load(Caller, caller_src(Caller, Callee), Dir),
    %% Reload the callee so it exports foo/2 and no longer foo/1.
    {Callee, _} = compile_and_load(Callee, callee_foo2_src(Callee), Dir),
    Edges = arizona_reloader_consistency:broken_edges([Callee], [Caller]),
    ?assertEqual([{Caller, {Callee, foo, 1}}], Edges).

%% A callee that still exports what the caller calls is consistent: no edge.
consistent_reload_reports_nothing(Config) ->
    Dir = proplists:get_value(tmp_dir, Config),
    Caller = az_cc_caller_b,
    Callee = az_cc_callee_b,
    {Caller, _} = compile_and_load(Caller, caller_src(Caller, Callee), Dir),
    {Callee, _} = compile_and_load(Callee, callee_foo1_src(Callee), Dir),
    ?assertEqual([], arizona_reloader_consistency:broken_edges([Callee], [Caller])).

%% A structurally-broken edge whose target was NOT reloaded this wave is not
%% flagged: the check reports only what this wave could have broken.
unreloaded_target_not_flagged(Config) ->
    Dir = proplists:get_value(tmp_dir, Config),
    Caller = az_cc_caller_c,
    Callee = az_cc_callee_c,
    {Caller, _} = compile_and_load(Caller, caller_src(Caller, Callee), Dir),
    {Callee, _} = compile_and_load(Callee, callee_foo2_src(Callee), Dir),
    %% The edge exists (callee dropped foo/1) but callee is not in Reloaded.
    ?assertEqual([], arizona_reloader_consistency:broken_edges([], [Caller])).

%% A module running an older version than its beam on disk is stale.
stale_beam_detected(Config) ->
    Dir = proplists:get_value(tmp_dir, Config),
    Mod = az_cc_stale_a,
    {Mod, Beam} = compile_and_load(Mod, versioned_src(Mod, 1), Dir),
    %% Rewrite the beam on disk to a different version WITHOUT reloading it.
    Bin2 = compile_only(Mod, versioned_src(Mod, 2), Dir),
    ok = file:write_file(Beam, Bin2),
    Stale = arizona_reloader_consistency:stale_modules([Mod]),
    ?assertMatch([{Mod, LoadedVsn, DiskVsn}] when LoadedVsn =/= DiskVsn, Stale),
    %% Reloading to match disk clears the staleness (and tidies up for later tests).
    {module, Mod} = code:load_binary(Mod, Beam, Bin2),
    ?assertEqual([], arizona_reloader_consistency:stale_modules([Mod])).

%% A freshly loaded module whose beam matches memory is not stale.
fresh_beam_not_stale(Config) ->
    Dir = proplists:get_value(tmp_dir, Config),
    Mod = az_cc_fresh_a,
    {Mod, _} = compile_and_load(Mod, versioned_src(Mod, 1), Dir),
    ?assertEqual([], arizona_reloader_consistency:stale_modules([Mod])).

%% The per-module beam facts (imports + disk vsn) are cached across waves keyed
%% by the beam file's mtime, so an unchanged module costs one stat -- not two
%% full disk reads -- per save. Proven by rewriting the beam while restoring
%% its mtime (cache hit: the old facts still apply) and then bumping the mtime
%% (cache invalidated: the new beam is read and the mismatch surfaces).
beam_facts_cached_by_mtime(Config) ->
    Dir = proplists:get_value(tmp_dir, Config),
    Mod = az_cc_cache_a,
    {Mod, Beam} = compile_and_load(Mod, versioned_src(Mod, 1), Dir),
    {ok, #file_info{mtime = Mtime} = Info} = file:read_file_info(Beam, [{time, posix}]),
    %% Prime the cache: memory matches disk, nothing stale.
    ?assertEqual([], arizona_reloader_consistency:stale_modules([Mod])),
    %% Rewrite the beam to a different version but restore the original mtime:
    %% an unchanged mtime means no re-read, so the cached facts still apply.
    Bin2 = compile_only(Mod, versioned_src(Mod, 2), Dir),
    ok = file:write_file(Beam, Bin2),
    ok = file:write_file_info(Beam, Info#file_info{mtime = Mtime}, [{time, posix}]),
    ?assertEqual([], arizona_reloader_consistency:stale_modules([Mod])),
    %% Bumping the mtime invalidates the entry: the new beam is read and the
    %% version mismatch surfaces.
    ok = file:write_file_info(Beam, Info#file_info{mtime = Mtime + 5}, [{time, posix}]),
    ?assertMatch(
        [{Mod, LoadedVsn, DiskVsn}] when LoadedVsn =/= DiskVsn,
        arizona_reloader_consistency:stale_modules([Mod])
    ).

%% The cache table has a stable owner: `arizona_sup` creates it at boot, so it
%% spans the node's lifetime rather than being created by (and dying with)
%% whichever process happened to run the first check.
beam_cache_owned_by_supervisor(Config) when is_list(Config) ->
    Cache = ets:whereis(arizona_reloader_beam_cache),
    ?assertNotEqual(undefined, Cache),
    ?assertEqual(erlang:whereis(arizona_sup), ets:info(Cache, owner)).

%% The dev MCP tools call the checks from a per-dispatch `spawn_link`ed worker
%% that dies immediately after answering. The cache used to be created BY that
%% worker and die with it, so the documented "one stat per loaded module" was
%% false for every MCP caller (each call re-read every beam) and the next caller
%% raced a half-dead table. Rows written from a transient caller must outlive it.
beam_cache_survives_transient_caller(Config) ->
    Dir = proplists:get_value(tmp_dir, Config),
    Mod = az_cc_transient_a,
    {Mod, _Beam} = compile_and_load(Mod, versioned_src(Mod, 1), Dir),
    Cache = ets:whereis(arizona_reloader_beam_cache),
    true = ets:delete(Cache, Mod),
    ok = run_in_short_lived_process(fun() ->
        [] = arizona_reloader_consistency:stale_modules([Mod])
    end),
    ?assertEqual(Cache, ets:whereis(arizona_reloader_beam_cache)),
    ?assertMatch([{Mod, _Path, _Mtime, _Facts}], ets:lookup(Cache, Mod)).

%% Runs `Fun` in a process that exits as soon as it returns, and waits for the
%% exit -- the MCP dispatch worker's lifetime.
run_in_short_lived_process(Fun) ->
    {Pid, Ref} = erlang:spawn_monitor(Fun),
    receive
        {'DOWN', Ref, process, Pid, normal} -> ok
    after 5000 -> ct:fail(worker_did_not_finish)
    end.

%% The candidate set is application code only: it includes a loaded module with a
%% readable non-OTP beam and excludes OTP (a stdlib module).
candidate_modules_excludes_otp(Config) ->
    Dir = proplists:get_value(tmp_dir, Config),
    Mod = az_cc_candidate_a,
    {Mod, _} = compile_and_load(Mod, versioned_src(Mod, 1), Dir),
    Candidates = arizona_reloader_consistency:candidate_modules(),
    ?assert(lists:member(Mod, Candidates)),
    ?assertNot(lists:member(lists, Candidates)).

%% End-to-end: check/1 discovers the caller itself, detects the broken edge, and
%% logs a warning naming the caller and the missing callee.
check_reports_broken_edge(Config) ->
    Dir = proplists:get_value(tmp_dir, Config),
    Caller = az_cc_caller_d,
    Callee = az_cc_callee_d,
    {Caller, _} = compile_and_load(Caller, caller_src(Caller, Callee), Dir),
    {Callee, _} = compile_and_load(Callee, callee_foo2_src(Callee), Dir),
    CalleeErl = filename:join(Dir, atom_to_list(Callee) ++ ".erl"),
    Msgs = with_log_capture(fun() ->
        ?assertEqual(ok, arizona_reloader_consistency:check([CalleeErl]))
    end),
    ?assert(
        lists:any(
            fun(M) -> contains(M, "az_cc_caller_d") andalso contains(M, "az_cc_callee_d") end,
            Msgs
        )
    ).

%% End-to-end: a consistent reload produces no finding about the reloaded pair.
check_clean_reports_nothing(Config) ->
    Dir = proplists:get_value(tmp_dir, Config),
    Caller = az_cc_caller_e,
    Callee = az_cc_callee_e,
    {Caller, _} = compile_and_load(Caller, caller_src(Caller, Callee), Dir),
    {Callee, _} = compile_and_load(Callee, callee_foo1_src(Callee), Dir),
    CalleeErl = filename:join(Dir, atom_to_list(Callee) ++ ".erl"),
    Msgs = with_log_capture(fun() ->
        ?assertEqual(ok, arizona_reloader_consistency:check([CalleeErl]))
    end),
    ?assertEqual(
        [],
        [M || M <- Msgs, contains(M, "az_cc_caller_e") orelse contains(M, "az_cc_callee_e")]
    ).

%% The whole pass is best-effort: a garbage argument that would crash the scan
%% is swallowed and check/1 still returns ok. (The bad value is laundered through
%% proplists:get_value/3 so its static type stays term().)
check_best_effort_never_crashes(Config) when is_list(Config) ->
    Bad = proplists:get_value(no_such_key, Config, not_a_list),
    ?assertEqual(ok, arizona_reloader_consistency:check(Bad)).

%% ============================================================================
%% Helpers
%% ============================================================================

pubsub_tests() ->
    [
        join_without_pg_scope,
        join_with_pg_scope,
        join_idempotent,
        broadcast_no_members,
        broadcast_single,
        broadcast_multiple,
        broadcast_dead_member
    ].

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
            stop_process(Pid),
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

make_tmp_dir(TC) ->
    Name =
        "arizona_reloader_ct_" ++ atom_to_list(TC) ++ "_" ++
            integer_to_list(erlang:unique_integer([positive])),
    Dir = filename:join("/tmp", Name),
    ok = file:make_dir(Dir),
    Dir.

rm_rf(Dir) ->
    file:del_dir_r(Dir).

stop_process(Pid) when is_pid(Pid), node(Pid) =:= node() ->
    case is_process_alive(Pid) of
        true ->
            exit(Pid, kill),
            ok;
        false ->
            ok
    end;
stop_process(_) ->
    ok.

good_module_src(Value) ->
    io_lib:format(
        "-module(arizona_dev_ct_good).\n"
        "-export([value/0]).\n"
        "value() -> ~b.\n",
        [Value]
    ).

good_module_src2() ->
    "-module(arizona_dev_ct_good2).\n"
    "-export([value/0]).\n"
    "value() -> ok.\n".

bad_module_src() ->
    "-module(arizona_dev_ct_bad).\n"
    "-export([value/0]).\n"
    "value() -> {\n".

bad_module_src2() ->
    "-module(arizona_dev_ct_bad2).\n"
    "-export([value/0]).\n"
    "value() -> {\n".

mismatch_module_src() ->
    "-module(arizona_dev_ct_declared).\n"
    "-export([check/0]).\n"
    "check() -> ok.\n".

mixed_good_module_src() ->
    "-module(arizona_dev_ct_mixed).\n"
    "-export([check/0]).\n"
    "check() -> ok.\n".

alive_module_src() ->
    "-module(arizona_dev_ct_alive).\n"
    "-export([check/0]).\n"
    "check() -> ok.\n".

%% --- consistency helpers ---

%% Compile Src to a real .beam file under Dir and load it from that path, so
%% code:which/1 returns a readable non-OTP beam (what the check reads). Returns
%% {Module, BeamPath}.
compile_and_load(Name, Src, Dir) ->
    Erl = filename:join(Dir, atom_to_list(Name) ++ ".erl"),
    ok = file:write_file(Erl, Src),
    {ok, Name, Bin} = compile:file(Erl, [binary, return_errors]),
    Beam = filename:join(Dir, atom_to_list(Name) ++ ".beam"),
    ok = file:write_file(Beam, Bin),
    code:purge(Name),
    {module, Name} = code:load_binary(Name, Beam, Bin),
    {Name, Beam}.

%% Compile Src to a binary without loading it. Returns the beam binary.
compile_only(Name, Src, Dir) ->
    Erl = filename:join(Dir, atom_to_list(Name) ++ ".erl"),
    ok = file:write_file(Erl, Src),
    {ok, Name, Bin} = compile:file(Erl, [binary, return_errors]),
    Bin.

caller_src(Name, Callee) ->
    io_lib:format(
        "-module(~p).\n"
        "-export([run/1]).\n"
        "run(X) -> ~p:foo(X).\n",
        [Name, Callee]
    ).

callee_foo1_src(Name) ->
    io_lib:format(
        "-module(~p).\n"
        "-export([foo/1]).\n"
        "foo(_) -> ok.\n",
        [Name]
    ).

callee_foo2_src(Name) ->
    io_lib:format(
        "-module(~p).\n"
        "-export([foo/2]).\n"
        "foo(_, _) -> ok.\n",
        [Name]
    ).

versioned_src(Name, Value) ->
    io_lib:format(
        "-module(~p).\n"
        "-export([value/0]).\n"
        "value() -> ~b.\n",
        [Name, Value]
    ).

%% Capture the warnings logged while Fun runs, as flattened strings.
with_log_capture(Fun) ->
    HandlerId = az_cc_capture,
    true = register(az_cc_log_collector, self()),
    ok = logger:add_handler(HandlerId, ?MODULE, #{level => warning}),
    try
        Fun(),
        collect_logs()
    after
        ok = logger:remove_handler(HandlerId),
        true = unregister(az_cc_log_collector)
    end.

collect_logs() ->
    receive
        {az_cc_log, Msg} -> [Msg | collect_logs()]
    after 200 -> []
    end.

contains(Str, Sub) ->
    string:find(Str, Sub) =/= nomatch.

%% logger handler callback: forwards each formatted message to the collector.
log(#{msg := Msg}, _Config) ->
    case erlang:whereis(az_cc_log_collector) of
        undefined ->
            ok;
        Pid ->
            Pid ! {az_cc_log, format_msg(Msg)},
            ok
    end.

format_msg({Format, Args}) when is_list(Format) ->
    lists:flatten(io_lib:format(Format, Args));
format_msg(Other) ->
    lists:flatten(io_lib:format("~p", [Other])).
