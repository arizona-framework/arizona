-module(arizona_watcher_SUITE).
-behaviour(ct_suite).
-include_lib("stdlib/include/assert.hrl").
-compile([export_all, nowarn_export_all]).

%% --------------------------------------------------------------------
%% Behaviour (ct_suite) callbacks
%% --------------------------------------------------------------------

all() ->
    [
        {group, file_events},
        {group, pattern_matching}
    ].

groups() ->
    [
        {file_events, [sequence], [
            file_created_test,
            file_modified_test,
            file_deleted_test
        ]},
        {pattern_matching, [parallel], [
            file_pattern_no_match_test
        ]}
    ].

init_per_suite(Config) ->
    % Start fs application for file system watching
    {ok, _} = application:ensure_all_started(fs),

    % Create temporary test directories
    TmpDir = proplists:get_value(priv_dir, Config),

    [{tmp_dir, TmpDir} | Config].

end_per_suite(_Config) ->
    ok = application:stop(fs),
    ok.

init_per_testcase(TestCase, Config) ->
    {tmp_dir, TmpDir} = proplists:lookup(tmp_dir, Config),
    Filename = filename:join(TmpDir, atom_to_list(TestCase) ++ ".erl"),
    WatcherConfig = #{
        directories => [TmpDir],
        patterns => [".*\\.erl$"],
        debounce_ms => 1
    },
    [{filename, Filename}, {watcher_config, WatcherConfig} | Config].

end_per_testcase(_TestCase, Config) ->
    % Delete test file
    {filename, Filename} = proplists:lookup(filename, Config),
    case filelib:is_regular(Filename) of
        true -> ok = file:delete(Filename);
        _ -> ok
    end.

%% --------------------------------------------------------------------
%% Tests
%% --------------------------------------------------------------------

file_created_test(Config) when is_list(Config) ->
    {watcher_config, WatcherConfig} = proplists:lookup(watcher_config, Config),
    {filename, Filename} = proplists:lookup(filename, Config),

    TestPid = self(),
    CallbackFun = fun(Files) -> TestPid ! {self(), files_changed, Files} end,

    {ok, Pid} = arizona_watcher:start_link(WatcherConfig#{callback => CallbackFun}),

    ok = file:write_file(Filename, "-module(test)."),

    receive
        {Pid, files_changed, Files} ->
            ?assertEqual(1, length(Files)),
            ?assert(lists:member(Filename, Files))
    after 1000 ->
        ct:fail("File created callback not received")
    end.

file_modified_test(Config) ->
    {watcher_config, WatcherConfig} = proplists:lookup(watcher_config, Config),
    {filename, Filename} = proplists:lookup(filename, Config),

    TestPid = self(),
    CallbackFun = fun(Files) -> TestPid ! {self(), files_changed, Files} end,

    {ok, Pid} = arizona_watcher:start_link(WatcherConfig#{callback => CallbackFun}),

    % Create file first
    ok = file:write_file(Filename, "-module(test)."),

    % Wait for create event
    receive
        {Pid, files_changed, _} -> ok
    after 1000 ->
        ct:fail("File created callback not received")
    end,

    % Now modify the file
    ok = file:write_file(Filename, "-module(test).\ntest() -> ok."),

    receive
        {Pid, files_changed, Files} ->
            ?assertEqual(1, length(Files)),
            ?assert(lists:member(Filename, Files))
    after 1000 ->
        ct:fail("File modified callback not received")
    end.

file_deleted_test(Config) ->
    {watcher_config, WatcherConfig} = proplists:lookup(watcher_config, Config),
    {filename, Filename} = proplists:lookup(filename, Config),

    TestPid = self(),
    CallbackFun = fun(Files) -> TestPid ! {self(), files_changed, Files} end,

    {ok, Pid} = arizona_watcher:start_link(WatcherConfig#{callback => CallbackFun}),

    % Create file first
    ok = file:write_file(Filename, "-module(test)."),

    % Wait for create event
    receive
        {Pid, files_changed, _} -> ok
    after 1000 ->
        ct:fail("File created callback not received")
    end,

    % Now delete the file
    ok = file:delete(Filename),

    receive
        {Pid, files_changed, Files} ->
            ?assertEqual(1, length(Files)),
            ?assert(lists:member(Filename, Files))
    after 1000 ->
        ct:fail("File deleted callback not received")
    end.

file_pattern_no_match_test(Config) ->
    {watcher_config, WatcherConfig} = proplists:lookup(watcher_config, Config),
    {tmp_dir, TmpDir} = proplists:lookup(tmp_dir, Config),

    % Create a .txt file (should not match .*\.erl$ pattern)
    TxtFilename = filename:join(TmpDir, "test.txt"),

    TestPid = self(),
    CallbackFun = fun(Files) -> TestPid ! {self(), files_changed, Files} end,

    {ok, Pid} = arizona_watcher:start_link(WatcherConfig#{callback => CallbackFun}),

    % Create .txt file - should not trigger callback
    ok = file:write_file(TxtFilename, "some text content"),

    receive
        {Pid, files_changed, _Files} ->
            % Cleanup
            _ = file:delete(TxtFilename),
            ct:fail("Callback should not be triggered for non-matching file pattern")
    after 500 ->
        % Expected - no callback for .txt file with .erl pattern
        file:delete(TxtFilename)
    end.
