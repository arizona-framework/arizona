-module(arizona_reloader_SUITE).
-behaviour(ct_suite).
-include_lib("stdlib/include/assert.hrl").
-compile([export_all, nowarn_export_all]).

%% --------------------------------------------------------------------
%% Behaviour (ct_suite) callbacks
%% --------------------------------------------------------------------

all() ->
    [
        {group, basic_functionality},
        {group, configuration},
        {group, file_patterns},
        {group, reload_commands}
    ].

groups() ->
    [
        {basic_functionality, [sequence], [
            file_change_detection_test,
            multiple_file_changes_test,
            debounced_events_test
        ]},
        {configuration, [sequence], [
            default_config_test,
            custom_config_test
        ]},
        {file_patterns, [sequence], [
            erl_file_pattern_test,
            ignore_non_matching_files_test
        ]},
        {reload_commands, [sequence], [
            successful_command_test,
            undefined_command_test
        ]}
    ].

init_per_suite(Config) ->
    % Create temporary test directories
    TmpDir = proplists:get_value(priv_dir, Config),

    % Start required PubSub process
    _PgPid =
        case pg:start(arizona_pubsub) of
            {ok, Pid} -> Pid;
            {error, {already_started, Pid}} -> Pid
        end,

    [{tmp_dir, TmpDir} | Config].

end_per_suite(_Config) ->
    ok.

init_per_testcase(TestCase, Config) ->
    {tmp_dir, TmpDir} = proplists:lookup(tmp_dir, Config),
    Filename = filename:join(TmpDir, atom_to_list(TestCase) ++ ".erl"),
    [{filename, Filename} | Config].

end_per_testcase(_TestCase, Config) ->
    {filename, Filename} = proplists:lookup(filename, Config),
    case filelib:is_regular(Filename) of
        true ->
            ok = file:delete(Filename);
        _ ->
            ok
    end.

%% --------------------------------------------------------------------
%% Basic functionality tests
%% --------------------------------------------------------------------

file_change_detection_test(Config) when is_list(Config) ->
    {filename, Filename} = proplists:lookup(filename, Config),
    ok = pubsub_join(),
    {ok, _Pid} = start_reloader(Filename),
    ok = file:write_file(Filename, "-module(test)."),
    receive
        {pubsub_message, ~"live_reload", {file_changed, Filename}} -> ok
    after 500 -> ct:fail("No reload message received")
    end.

multiple_file_changes_test(Config) when is_list(Config) ->
    {filename, Filename} = proplists:lookup(filename, Config),
    ok = pubsub_join(),
    {ok, _Pid} = start_reloader(Filename),

    % First change
    ok = file:write_file(Filename, "-module(test1)."),
    receive
        {pubsub_message, ~"live_reload", {file_changed, Filename}} -> ok
    after 500 -> ct:fail("First reload message not received")
    end,

    % Second change
    ok = file:write_file(Filename, "-module(test2)."),
    receive
        {pubsub_message, ~"live_reload", {file_changed, Filename}} -> ok
    after 500 -> ct:fail("Second reload message not received")
    end.

debounced_events_test(Config) when is_list(Config) ->
    {filename, Filename} = proplists:lookup(filename, Config),
    ok = pubsub_join(),
    {ok, _Pid} = start_reloader_with_debounce(Filename, 200),

    % Create multiple rapid changes with small delays to ensure separate events
    ok = file:write_file(Filename, "-module(test1)."),
    timer:sleep(50),
    ok = file:write_file(Filename, "-module(test2)."),
    timer:sleep(50),
    ok = file:write_file(Filename, "-module(test3)."),

    % Should receive only one debounced message
    receive
        {pubsub_message, ~"live_reload", {file_changed, Filename}} -> ok
    after 500 -> ct:fail("Debounced reload message not received")
    end,

    % Should not receive additional messages
    receive
        {pubsub_message, ~"live_reload", {file_changed, Filename}} ->
            ct:fail("Unexpected additional reload message")
    after 300 -> ok
    end.

%% --------------------------------------------------------------------
%% Configuration tests
%% --------------------------------------------------------------------

default_config_test(Config) when is_list(Config) ->
    {filename, Filename} = proplists:lookup(filename, Config),
    {ok, Pid} = gen_server:start_link(
        arizona_reloader,
        #{
            enabled => true,
            watch_paths => [filename:dirname(Filename)]
        },
        []
    ),
    State = gen_server:call(Pid, get_state, infinity),

    % Verify defaults
    ?assertEqual(100, maps:get(debounce_ms, State)),
    ?assertEqual("rebar3 compile", maps:get(reload_command, State)).

custom_config_test(Config) when is_list(Config) ->
    {filename, Filename} = proplists:lookup(filename, Config),
    {ok, Pid} = gen_server:start_link(
        arizona_reloader,
        #{
            enabled => true,
            watch_paths => [filename:dirname(Filename)],
            debounce_ms => 500,
            reload_command => "echo test"
        },
        []
    ),
    State = gen_server:call(Pid, get_state, infinity),

    % Verify custom values
    ?assertEqual(500, maps:get(debounce_ms, State)),
    ?assertEqual("echo test", maps:get(reload_command, State)).

%% --------------------------------------------------------------------
%% File pattern tests
%% --------------------------------------------------------------------

erl_file_pattern_test(Config) when is_list(Config) ->
    {filename, Filename} = proplists:lookup(filename, Config),
    ok = pubsub_join(),
    {ok, _Pid} = start_reloader_with_patterns(Filename, [~".*\\.erl$"]),

    % .erl file should trigger reload
    ok = file:write_file(Filename, "-module(test)."),
    receive
        {pubsub_message, ~"live_reload", {file_changed, Filename}} -> ok
    after 500 -> ct:fail("Erlang file should trigger reload")
    end.

ignore_non_matching_files_test(Config) when is_list(Config) ->
    {tmp_dir, TmpDir} = proplists:lookup(tmp_dir, Config),
    TxtFile = filename:join(TmpDir, "test.txt"),
    ok = pubsub_join(),
    {ok, _Pid} = start_reloader_with_patterns(TxtFile, [~".*\\.erl$"]),

    % .txt file should not trigger reload
    ok = file:write_file(TxtFile, "some content"),
    receive
        {pubsub_message, ~"live_reload", {file_changed, TxtFile}} ->
            ct:fail("Text file should not trigger reload")
    after 300 -> ok
    end,
    file:delete(TxtFile).

%% --------------------------------------------------------------------
%% Reload command tests
%% --------------------------------------------------------------------

successful_command_test(Config) when is_list(Config) ->
    {filename, Filename} = proplists:lookup(filename, Config),
    ok = pubsub_join(),
    {ok, _Pid} = start_reloader_with_command(Filename, "true"),
    ok = file:write_file(Filename, "-module(test)."),
    receive
        {pubsub_message, ~"live_reload", {file_changed, Filename}} -> ok
    after 500 -> ct:fail("Reload with successful command failed")
    end.

undefined_command_test(Config) when is_list(Config) ->
    {filename, Filename} = proplists:lookup(filename, Config),
    ok = pubsub_join(),
    {ok, _Pid} = start_reloader_with_command(Filename, undefined),
    ok = file:write_file(Filename, "-module(test)."),
    receive
        {pubsub_message, ~"live_reload", {file_changed, Filename}} -> ok
    after 500 -> ct:fail("Reload without command failed")
    end.

%% --------------------------------------------------------------------
%% Helper functions
%% --------------------------------------------------------------------

start_reloader(Filename) ->
    gen_server:start_link(
        arizona_reloader,
        #{
            enabled => true,
            watch_paths => [filename:dirname(Filename)],
            debounce_ms => 0
        },
        []
    ).

start_reloader_with_debounce(Filename, DebounceMs) ->
    gen_server:start_link(
        arizona_reloader,
        #{
            enabled => true,
            watch_paths => [filename:dirname(Filename)],
            debounce_ms => DebounceMs
        },
        []
    ).

start_reloader_with_patterns(Filename, Patterns) ->
    gen_server:start_link(
        arizona_reloader,
        #{
            enabled => true,
            watch_paths => [filename:dirname(Filename)],
            file_patterns => Patterns,
            debounce_ms => 0
        },
        []
    ).

start_reloader_with_command(Filename, Command) ->
    gen_server:start_link(
        arizona_reloader,
        #{
            enabled => true,
            watch_paths => [filename:dirname(Filename)],
            reload_command => Command,
            debounce_ms => 0
        },
        []
    ).

pubsub_join() ->
    arizona_pubsub:join(~"live_reload", self()).
