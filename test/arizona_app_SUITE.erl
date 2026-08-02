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
    boot_logs_reloader_disabled/1
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
        boot_logs_reloader_disabled
    ].

init_per_testcase(boot_with_server_and_reloader, Config) ->
    ok = ensure_dir(?WATCH_DIR),
    ok = application:set_env(arizona, server, server_opts()),
    ok = application:set_env(arizona, reloader, #{
        enabled => true,
        rules => [
            #{directory => ?WATCH_DIR, patterns => [".*"], callback => fun(_) -> ok end}
        ]
    }),
    {ok, _} = application:ensure_all_started(arizona),
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
        rules => [
            #{directory => ?WATCH_DIR, patterns => [".*\\.erl$"], callback => fun(_) -> ok end}
        ]
    }),
    Config;
init_per_testcase(boot_logs_reloader_disabled, Config) ->
    Config;
init_per_testcase(boot_with_only_reloader, Config) ->
    ok = ensure_dir(?WATCH_DIR),
    ok = application:set_env(arizona, reloader, #{
        enabled => true,
        rules => [
            #{directory => ?WATCH_DIR, patterns => [".*"], callback => fun(_) -> ok end}
        ]
    }),
    {ok, _} = application:ensure_all_started(arizona),
    Config.

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

%% Arizona lazy-starts its heavy deps on demand: roadrunner only when a `server`
%% env is set (arizona_app:maybe_start_server/0), ssh only when the SSH transport
%% starts (arizona_terminal_ssh:start/1), fs via standalone fs:start_link/2. So it
%% forces none of them onto the boot path. Listing any in `applications` -- or in
%% `optional_applications`, which relx also bundles AND force-starts -- would boot
%% them for every consumer, including a server-less/static-generation user, and
%% would start fs's default CWD watcher in production. A consumer that ships the
%% server declares roadrunner/ssh in its OWN app's `applications`.
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
    ?assertNot(lists:member(ssh, Apps)),
    ?assertNot(lists:member(fs, Apps)),
    Optional =
        case application:get_key(arizona, optional_applications) of
            {ok, L} -> L;
            undefined -> []
        end,
    ?assertNot(lists:member(roadrunner, Optional)),
    ?assertNot(lists:member(ssh, Optional)),
    ?assertNot(lists:member(fs, Optional)).

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

%% ============================================================================
%% Helpers
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
        scheme => http,
        transport_opts => [{port, pick_port()}],
        routes => [{asset, <<"/priv">>, {priv_dir, arizona, "static/assets/js"}}]
    }.

pick_port() ->
    arizona_test_port:pick().

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
    case erlang:whereis(arizona_sup) of
        undefined ->
            false;
        _ ->
            lists:any(
                fun
                    ({{arizona_watcher, _}, Pid, _, _}) when is_pid(Pid) -> true;
                    (_) -> false
                end,
                supervisor:which_children(arizona_sup)
            )
    end.
