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
    transport_deps_are_not_forced/1
]).

-define(WATCH_DIR, "/tmp/arizona_app_suite_watch").

all() ->
    [
        boot_with_server_and_reloader,
        boot_without_config,
        boot_with_only_server,
        boot_with_only_reloader,
        transport_deps_are_not_forced
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

%% ============================================================================
%% Helpers
%% ============================================================================

server_opts() ->
    #{
        scheme => http,
        transport_opts => [{port, pick_port()}],
        routes => [{asset, <<"/priv">>, {priv_dir, arizona, "static/assets/js"}}]
    }.

%% A free OS-assigned port: bind an ephemeral port (0), read it back, release
%% it. The old `4040 + counter` scheme was deterministic across VM restarts --
%% the first call of each fresh VM landed on 4041 -- so it intermittently hit
%% eaddrinuse against a prior CT run's listener still in TIME_WAIT.
pick_port() ->
    {ok, Sock} = gen_tcp:listen(0, []),
    {ok, Port} = inet:port(Sock),
    ok = gen_tcp:close(Sock),
    Port.

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
