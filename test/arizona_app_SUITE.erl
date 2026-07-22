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
    declares_runtime_deps_in_applications/1
]).

-define(WATCH_DIR, "/tmp/arizona_app_suite_watch").

all() ->
    [
        boot_with_server_and_reloader,
        boot_without_config,
        boot_with_only_server,
        boot_with_only_reloader,
        declares_runtime_deps_in_applications
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
init_per_testcase(declares_runtime_deps_in_applications, Config) ->
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

%% relx (and OTP's boot script) ignore an app listed only in
%% optional_applications, so the runtime deps must also appear in
%% `applications` or a release omits them and the server crashes at boot.
declares_runtime_deps_in_applications(Config) when is_list(Config) ->
    {ok, Apps} = application:get_key(arizona, applications),
    ?assert(lists:member(roadrunner, Apps)),
    ?assert(lists:member(fs, Apps)),
    ?assert(lists:member(ssh, Apps)).

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
