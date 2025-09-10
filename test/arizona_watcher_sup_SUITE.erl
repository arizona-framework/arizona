-module(arizona_watcher_sup_SUITE).
-behaviour(ct_suite).
-include_lib("stdlib/include/assert.hrl").
-compile([export_all, nowarn_export_all]).

%% --------------------------------------------------------------------
%% Behaviour (ct_suite) callbacks
%% --------------------------------------------------------------------

all() ->
    [
        {group, supervisor_operations}
    ].

groups() ->
    [
        {supervisor_operations, [sequence], [
            supervisor_lifecycle_test
        ]}
    ].

%% --------------------------------------------------------------------
%% Tests
%% --------------------------------------------------------------------

supervisor_lifecycle_test(Config) when is_list(Config) ->
    % Test supervisor start_link
    {ok, SupPid} = arizona_watcher_sup:start_link(),

    % Verify supervisor is properly registered and running
    ?assert(is_pid(SupPid)),
    ?assertEqual(SupPid, whereis(arizona_watcher_sup)),
    ?assert(is_process_alive(SupPid)),

    % Verify initial state has no children
    InitialChildren = supervisor:which_children(arizona_watcher_sup),
    ?assertEqual([], InitialChildren),

    % Test start_child functionality
    TmpDir = proplists:get_value(priv_dir, Config),
    WatcherConfig = #{
        directories => [TmpDir],
        patterns => [".*\\.erl$"],
        callback => fun(_Files) -> ok end,
        debounce_ms => 1
    },

    {ok, WatcherPid} = arizona_watcher_sup:start_child(WatcherConfig),
    ?assert(is_pid(WatcherPid)),
    ?assert(is_process_alive(WatcherPid)),

    % Test stop_child functionality
    StopResult = arizona_watcher_sup:stop_child(WatcherPid),
    ?assertEqual(ok, StopResult),
    ?assertNot(is_process_alive(WatcherPid)).
