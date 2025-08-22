-module(arizona_app_SUITE).
-behaviour(ct_suite).
-include_lib("stdlib/include/assert.hrl").
-compile([export_all, nowarn_export_all]).

%% --------------------------------------------------------------------
%% Behaviour (ct_suite) callbacks
%% --------------------------------------------------------------------

all() ->
    [
        {group, application_callbacks},
        {group, application_lifecycle}
    ].

groups() ->
    [
        {application_callbacks, [parallel], [
            test_start_function,
            test_stop_function
        ]},
        {application_lifecycle, [parallel], [
            test_application_start_stop
        ]}
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    % Clean up after tests
    _ = application:stop(arizona),
    Config.

%% --------------------------------------------------------------------
%% Application callbacks tests
%% --------------------------------------------------------------------

test_start_function(Config) when is_list(Config) ->
    ct:comment("Test the start/2 function directly"),
    {ok, Pid} = arizona_app:start(normal, []),

    % Verify a supervisor process was started
    ?assert(is_pid(Pid)),
    ?assert(is_process_alive(Pid)),

    % Clean up
    exit(Pid, normal).

test_stop_function(Config) when is_list(Config) ->
    ct:comment("Test the stop/1 function directly"),
    Result = arizona_app:stop(some_state),

    % stop/1 should always return ok
    ?assertEqual(ok, Result).

%% --------------------------------------------------------------------
%% Application lifecycle tests
%% --------------------------------------------------------------------

test_application_start_stop(Config) when is_list(Config) ->
    ct:comment("Test application start/stop cycle using application:ensure_all_started"),
    {ok, StartedApps} = application:ensure_all_started(arizona),

    % Expected apps that should be started (arizona and its dependencies)
    ExpectedApps = [arizona, cowboy, cowlib, ranch, syntax_tools],

    % Verify all expected apps were started
    lists:foreach(
        fun(App) ->
            ?assert(lists:member(App, StartedApps))
        end,
        ExpectedApps
    ),

    % Verify application is running
    ?assert(lists:keymember(arizona, 1, application:which_applications())),

    % Test stopping the application
    ?assertEqual(ok, application:stop(arizona)),

    % Verify application is stopped
    ?assertNot(lists:keymember(arizona, 1, application:which_applications())).
