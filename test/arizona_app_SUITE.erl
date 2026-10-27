-module(arizona_app_SUITE).
-include_lib("stdlib/include/assert.hrl").

-export([
    all/0,
    init_per_testcase/2,
    end_per_testcase/2
]).

-export([
    boot_with_server_and_reloader/1,
    boot_without_config/1,
    boot_with_only_server/1,
    boot_with_only_reloader/1,
    transport_deps_are_not_forced/1,
    prep_stop_stops_listener_before_tree/1,
    boot_logs_reloader_enabled/1,
    boot_logs_reloader_disabled/1,
    boot_fails_on_rules_without_watcher/1,
    crash_looping_watcher_spares_app/1,
    server_adapter_runs_the_listener_lifecycle/1,
    reload_recompiles_every_registered_adapter/1,
    boot_fails_without_an_adapter/1,
    boot_fails_on_an_adapter_that_is_not_loadable/1
]).

-define(WATCH_DIR, "/tmp/arizona_app_suite_watch").

all() ->
    [
        boot_with_server_and_reloader,
        boot_without_config,
        boot_with_only_server,
        boot_with_only_reloader,
        transport_deps_are_not_forced,
        prep_stop_stops_listener_before_tree,
        boot_logs_reloader_enabled,
        boot_logs_reloader_disabled,
        boot_fails_on_rules_without_watcher,
        crash_looping_watcher_spares_app,
        server_adapter_runs_the_listener_lifecycle,
        reload_recompiles_every_registered_adapter,
        boot_fails_without_an_adapter,
        boot_fails_on_an_adapter_that_is_not_loadable
    ].

init_per_testcase(boot_with_server_and_reloader, Config) ->
    ok = ensure_dir(?WATCH_DIR),
    ok = application:set_env(arizona, server, server_opts()),
    ok = application:set_env(arizona, reloader, #{
        enabled => true,
        watcher => arizona_fake_watcher,
        rules => [
            #{directory => ?WATCH_DIR, patterns => [".*"], callback => fun(_) -> ok end}
        ]
    }),
    {ok, _} = application:ensure_all_started(arizona),
    Config;
init_per_testcase(TC, Config) when
    TC =:= boot_fails_without_an_adapter;
    TC =:= boot_fails_on_an_adapter_that_is_not_loadable
->
    Config;
init_per_testcase(boot_without_config, Config) ->
    {ok, _} = application:ensure_all_started(arizona),
    Config;
init_per_testcase(transport_deps_are_not_forced, Config) ->
    {ok, _} = application:ensure_all_started(arizona),
    Config;
init_per_testcase(boot_with_only_server, Config) ->
    ok = application:set_env(arizona, server, server_opts()),
    {ok, _} = application:ensure_all_started(arizona),
    Config;
init_per_testcase(prep_stop_stops_listener_before_tree, Config) ->
    ok = application:set_env(arizona, server, server_opts()),
    {ok, _} = application:ensure_all_started(arizona),
    Config;
%% The two boot-log tests start the app in the test body, AFTER installing the
%% log capture handler -- the line under test fires in arizona_sup:init/1.
init_per_testcase(boot_logs_reloader_enabled, Config) ->
    ok = ensure_dir(?WATCH_DIR),
    ok = application:set_env(arizona, reloader, #{
        enabled => true,
        watcher => arizona_fake_watcher,
        rules => [
            #{directory => ?WATCH_DIR, patterns => [".*\\.erl$"], callback => fun(_) -> ok end}
        ]
    }),
    Config;
init_per_testcase(boot_logs_reloader_disabled, Config) ->
    Config;
init_per_testcase(crash_looping_watcher_spares_app, Config) ->
    ok = ensure_dir(?WATCH_DIR),
    ok = application:set_env(arizona, reloader, #{
        enabled => true,
        watcher => {arizona_fake_watcher, #{notify => self(), link => true}},
        rules => [#{directory => ?WATCH_DIR, callback => fun(_) -> ok end}]
    }),
    {ok, _} = application:ensure_all_started(arizona),
    Config;
init_per_testcase(boot_fails_on_rules_without_watcher, Config) ->
    ok = ensure_dir(?WATCH_DIR),
    ok = application:set_env(arizona, reloader, #{
        enabled => true,
        rules => [#{directory => ?WATCH_DIR, callback => fun(_) -> ok end}]
    }),
    Config;
init_per_testcase(TC, Config) when
    TC =:= server_adapter_runs_the_listener_lifecycle;
    TC =:= reload_recompiles_every_registered_adapter
->
    Registered = arizona_server_registry:adapters(),
    ok = application:set_env(arizona, server, #{
        adapter => arizona_fake_server,
        notify => self(),
        drain_timeout => 1234,
        own_opt => kept
    }),
    {ok, _} = application:ensure_all_started(arizona),
    [{registered, Registered} | Config];
init_per_testcase(boot_with_only_reloader, Config) ->
    ok = ensure_dir(?WATCH_DIR),
    ok = application:set_env(arizona, reloader, #{
        enabled => true,
        watcher => arizona_fake_watcher,
        rules => [
            #{directory => ?WATCH_DIR, patterns => [".*"], callback => fun(_) -> ok end}
        ]
    }),
    {ok, _} = application:ensure_all_started(arizona),
    Config.

end_per_testcase(boot_fails_on_rules_without_watcher, Config) ->
    ok = unset_env(reloader),
    ok = remove_dir(?WATCH_DIR),
    Config;
end_per_testcase(TC, Config) when
    TC =:= server_adapter_runs_the_listener_lifecycle;
    TC =:= reload_recompiles_every_registered_adapter
->
    ok = application:stop(arizona),
    ok = unset_env(server),
    %% The registry outlives the app; put back what other suites registered.
    persistent_term:put(arizona_server_adapters, proplists:get_value(registered, Config)),
    true = persistent_term:erase({arizona_fake_server, notify}),
    Config;
end_per_testcase(TC, Config) when
    TC =:= boot_fails_without_an_adapter;
    TC =:= boot_fails_on_an_adapter_that_is_not_loadable
->
    %% The boot failed, so there is no application to stop.
    ok = unset_env(server),
    Config;
end_per_testcase(_TC, Config) ->
    ok = application:stop(arizona),
    ok = unset_env(server),
    ok = unset_env(reloader),
    ok = remove_dir(?WATCH_DIR),
    Config.

%% ============================================================================
%% Tests
%% ============================================================================

boot_with_server_and_reloader(Config) when is_list(Config) ->
    ?assert(is_pid(erlang:whereis(arizona_sup))),
    ?assert(is_listener_up(arizona_http)),
    ?assert(has_watcher_child()).

boot_without_config(Config) when is_list(Config) ->
    ?assert(is_pid(erlang:whereis(arizona_sup))),
    ?assertNot(is_listener_up(arizona_http)),
    ?assertNot(has_watcher_child()).

boot_with_only_server(Config) when is_list(Config) ->
    ?assert(is_listener_up(arizona_http)),
    ?assertNot(has_watcher_child()).

boot_with_only_reloader(Config) when is_list(Config) ->
    ?assertNot(is_listener_up(arizona_http)),
    ?assert(has_watcher_child()).

%% The `server` env's `adapter` boots, drains, and stops the listener, and gets
%% the rest of the map minus the keys the application itself reads.
server_adapter_runs_the_listener_lifecycle(Config) when is_list(Config) ->
    Self = self(),
    receive
        {arizona_fake_server, {start, arizona_http, Opts}} ->
            ?assertEqual(#{notify => Self, own_opt => kept}, Opts)
    after 1000 -> ct:fail(adapter_not_started)
    end,
    ?assertEqual([], arizona_app:prep_stop([])),
    receive
        {arizona_fake_server, Drain} -> ?assertEqual({drain, arizona_http, 1234}, Drain)
    after 1000 -> ct:fail(adapter_not_drained)
    end,
    %% Drained already, so the backstop stop finds nothing and must not fail.
    ?assertEqual(ok, arizona_app:stop([])),
    receive
        {arizona_fake_server, Stop} -> ?assertEqual({stop, arizona_http}, Stop)
    after 1000 -> ct:fail(adapter_not_stopped)
    end.

%% Arizona ships no server, so a `server` env without an `adapter` has nothing to
%% fall back on: the boot fails, naming what to add, rather than serving nothing.
boot_fails_without_an_adapter(Config) when is_list(Config) ->
    ok = application:set_env(arizona, server, maps:remove(adapter, server_opts())),
    Result = application:ensure_all_started(arizona),
    ?assertMatch({error, _}, Result),
    ?assert(mentions(missing_server_adapter, Result)).

%% An adapter whose package is not a dependency fails the boot the same way.
boot_fails_on_an_adapter_that_is_not_loadable(Config) when is_list(Config) ->
    ok = application:set_env(arizona, server, (server_opts())#{adapter => arizona_no_such_server}),
    Result = application:ensure_all_started(arizona),
    ?assertMatch({error, _}, Result),
    ?assert(mentions({unknown_server_adapter, arizona_no_such_server}, Result)).

%% Whether `Term` appears anywhere in `Result` (the application controller wraps
%% a start crash in its own reason).
mentions(Term, Term) ->
    true;
mentions(Term, Tuple) when is_tuple(Tuple) ->
    mentions(Term, tuple_to_list(Tuple));
mentions(Term, List) when is_list(List) ->
    lists:any(fun(E) -> mentions(Term, E) end, List);
mentions(_Term, _Other) ->
    false.

%% A hot reload refreshes the routes of every adapter that ran a listener, not
%% only roadrunner's.
reload_recompiles_every_registered_adapter(Config) when is_list(Config) ->
    ?assert(lists:member(arizona_fake_server, arizona_server_registry:adapters())),
    ok = arizona_reloader:reload_erl([]),
    receive
        {arizona_fake_server, recompile_routes} -> ok
    after 1000 -> ct:fail(routes_not_recompiled)
    end.

%% Arizona ships no HTTP server -- an adapter package brings one and starts it
%% with its listener -- so it forces none onto the boot path. Listing one in
%% `applications` -- or in `optional_applications`, which relx also bundles AND
%% force-starts -- would boot it for every consumer, including a
%% server-less/static-generation user. A consumer that ships them declares its
%% adapter and server in its OWN app's `applications`.
%% The HTTP listener must go down in prep_stop/1 -- BEFORE the supervision tree
%% -- or shutdown briefly serves requests against dead infrastructure (pubsub,
%% MCP registry, session store). prep_stop stops the listener while arizona_sup
%% is still up; the follow-up application:stop (end_per_testcase) then runs
%% prep_stop again plus stop/1, so both must be idempotent about the
%% already-stopped listener.
prep_stop_stops_listener_before_tree(Config) when is_list(Config) ->
    ?assert(is_listener_up(arizona_http)),
    ?assertEqual([], arizona_app:prep_stop([])),
    ?assertNot(is_listener_up(arizona_http)),
    ?assert(is_pid(erlang:whereis(arizona_sup))).

transport_deps_are_not_forced(Config) when is_list(Config) ->
    {ok, Apps} = application:get_key(arizona, applications),
    ?assert(lists:member(crypto, Apps)),
    ?assertNot(lists:member(roadrunner, Apps)),
    ?assertNot(lists:member(cowboy, Apps)),
    Optional =
        case application:get_key(arizona, optional_applications) of
            {ok, L} -> L;
            undefined -> []
        end,
    ?assertNot(lists:member(roadrunner, Optional)),
    ?assertNot(lists:member(cowboy, Optional)).

%% Boot logs one line stating the reloader state, so a dev node silently
%% missing hot reload is visible. Enabled logs at `info`, naming the watched
%% dirs/patterns.
boot_logs_reloader_enabled(Config) when is_list(Config) ->
    {Level, Text} = with_boot_log(info, fun() ->
        {ok, _} = application:ensure_all_started(arizona),
        await_reload_log()
    end),
    ?assertEqual(info, Level),
    ?assertMatch({_, _}, binary:match(Text, ~"hot reload enabled")),
    ?assertMatch({_, _}, binary:match(Text, ~"arizona_fake_watcher")),
    ?assertMatch({_, _}, binary:match(Text, list_to_binary(?WATCH_DIR))),
    ?assertMatch({_, _}, binary:match(Text, ~".erl")).

%% Disabled logs at `notice` -- visible under OTP's default primary level
%% (notice), without being a warning -- and says how to enable it.
boot_logs_reloader_disabled(Config) when is_list(Config) ->
    {Level, Text} = with_boot_log(notice, fun() ->
        {ok, _} = application:ensure_all_started(arizona),
        await_reload_log()
    end),
    ?assertEqual(notice, Level),
    ?assertMatch({_, _}, binary:match(Text, ~"hot reload off")),
    ?assertMatch({_, _}, binary:match(Text, ~"reloader")).

%% Rules with no watcher to run them refuse to boot -- silently starting no
%% watcher would leave hot reload off with nothing saying why -- and the reason
%% is the `child_specs/1` config error that names the missing dependency.
boot_fails_on_rules_without_watcher(Config) when is_list(Config) ->
    ?assertMatch(
        {error, {arizona, {{no_watcher, _Stacktrace}, {arizona_app, start, _}}}},
        application:ensure_all_started(arizona)
    ),
    ?assertEqual(undefined, erlang:whereis(arizona_sup)).

%% A watch that keeps dying exhausts the reloader's own supervisor, which
%% `arizona_sup` runs as a temporary child: hot reload stops and the rest of the
%% application keeps running, instead of the restarts counting against
%% `arizona_sup` and stopping everything.
crash_looping_watcher_spares_app(Config) when is_list(Config) ->
    ReloaderSup = erlang:whereis(arizona_reloader_sup),
    ?assert(is_pid(ReloaderSup)),
    Ref = monitor(process, ReloaderSup),
    ok = crash_watch_until_down(Ref, 20),
    %% Given up for good: nothing restarts the reloader (a restart would mount
    %% a fresh watch, whose linked helper reports in).
    receive
        {arizona_fake_watcher, linked, _} -> ct:fail(reloader_restarted)
    after 500 -> ok
    end,
    ?assertEqual(undefined, erlang:whereis(arizona_reloader_sup)),
    ?assert(is_process_alive(erlang:whereis(arizona_sup))),
    ?assert(is_process_alive(erlang:whereis(arizona_pubsub))).

%% ============================================================================
%% Helpers

crash_watch_until_down(_Ref, 0) ->
    ct:fail(reloader_sup_never_gave_up);
crash_watch_until_down(Ref, Tries) ->
    receive
        {'DOWN', Ref, process, _, _} ->
            ok;
        {arizona_fake_watcher, linked, Helper} ->
            _ = Helper ! {exit, watch_lost},
            crash_watch_until_down(Ref, Tries - 1)
    after 2000 -> ct:fail(no_restart)
    end.
%% ============================================================================

%% Capture logs at `Level` while Fun runs: install the forwarding handler and
%% lower the primary level (OTP defaults it to notice, which would filter an
%% info line before any handler sees it), restoring both afterwards.
with_boot_log(Level, Fun) ->
    HandlerId = az_app_boot_log,
    OldPrimary = maps:get(level, logger:get_primary_config()),
    ok = logger:set_primary_config(level, Level),
    ok = logger:add_handler(HandlerId, arizona_test_log_handler, #{
        level => Level, config => #{pid => self()}
    }),
    try
        Fun()
    after
        ok = logger:remove_handler(HandlerId),
        ok = logger:set_primary_config(level, OldPrimary)
    end.

%% The first captured log line about hot reload, as {Level, Text}; other boot
%% logs are skipped.
await_reload_log() ->
    receive
        {arizona_test_log_handler, #{level := Level, msg := Msg}} ->
            Text = format_log_msg(Msg),
            case binary:match(Text, ~"hot reload") of
                nomatch -> await_reload_log();
                {_, _} -> {Level, Text}
            end
    after 5000 -> ct:fail(no_reloader_boot_log)
    end.

format_log_msg({string, Chardata}) ->
    unicode:characters_to_binary(Chardata);
format_log_msg({Format, Args}) when is_list(Format) ->
    unicode:characters_to_binary(io_lib:format(Format, Args));
format_log_msg(Other) ->
    unicode:characters_to_binary(io_lib:format("~tp", [Other])).

server_opts() ->
    #{
        adapter => arizona_roadrunner_server,
        scheme => http,
        transport_opts => [{port, arizona_test_port:pick()}],
        routes => [{asset, <<"/priv">>, {priv_dir, arizona, "static/assets/js"}}]
    }.

unset_env(Key) ->
    application:unset_env(arizona, Key).

ensure_dir(Dir) ->
    case file:make_dir(Dir) of
        ok -> ok;
        {error, eexist} -> ok
    end.

remove_dir(Dir) ->
    case file:del_dir_r(Dir) of
        ok -> ok;
        {error, enoent} -> ok
    end.

is_listener_up(Name) ->
    case erlang:whereis(roadrunner_sup) of
        undefined -> false;
        _ -> lists:member(Name, roadrunner:listeners())
    end.

has_watcher_child() ->
    case erlang:whereis(arizona_reloader_sup) of
        undefined ->
            false;
        _ ->
            lists:any(
                fun
                    ({{arizona_reloader_rule, _}, Pid, _, _}) when is_pid(Pid) -> true;
                    (_) -> false
                end,
                supervisor:which_children(arizona_reloader_sup)
            )
    end.
