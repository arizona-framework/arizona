-module(arizona_reloader_SUITE).
-include_lib("stdlib/include/assert.hrl").
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
    compile_error_stores_error/1,
    compile_error_cleared_on_success/1,
    non_erl_files_skip_compile/1,
    mixed_files_only_compile_erl/1
]).

%% ============================================================================
%% CT callbacks
%% ============================================================================

all() ->
    [{group, pubsub}, {group, integration}, {group, compile}].

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
            compile_error_stores_error,
            compile_error_cleared_on_success,
            non_erl_files_skip_compile,
            mixed_files_only_compile_erl
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
    Config.

end_per_group(compile, _Config) ->
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
     || _ <:- [1, 2, 3]
    ],
    %% Wait for all to join.
    [
        receive
            {joined, P} -> ok
        after 1000 -> ct:fail({join_timeout, P})
        end
     || P <:- Pids
    ],
    arizona_reloader:broadcast(),
    Received = [
        receive
            {got_reload, P} -> P;
            {timeout, P} -> ct:fail({reload_timeout, P})
        after 2000 ->
            ct:fail(collect_timeout)
        end
     || P <:- Pids
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
    {ok, W} = arizona_watcher:watch(Dir, #{
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
    {ok, W} = arizona_watcher:watch(Dir, #{
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

mixed_good_module_src() ->
    "-module(arizona_dev_ct_mixed).\n"
    "-export([check/0]).\n"
    "check() -> ok.\n".
