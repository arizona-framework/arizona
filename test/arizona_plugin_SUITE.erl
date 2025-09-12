-module(arizona_plugin_SUITE).
-behaviour(ct_suite).
-include_lib("stdlib/include/assert.hrl").
-compile([export_all, nowarn_export_all]).

%% --------------------------------------------------------------------
%% Behaviour (ct_suite) callbacks
%% --------------------------------------------------------------------

all() ->
    [
        {group, plugin_tests}
    ].

groups() ->
    [
        {plugin_tests, [parallel], [
            apply_plugins_empty_list_test,
            apply_plugins_single_plugin_test,
            apply_plugins_multiple_plugins_test,
            apply_plugins_plugin_error_test
        ]}
    ].

%% --------------------------------------------------------------------
%% Plugin tests
%% --------------------------------------------------------------------

apply_plugins_empty_list_test(_Config) ->
    ct:comment("Test applying empty plugin list returns original config"),

    ArizonaConfig = mock_arizona_config([{view, ~"/", home_view, #{}, []}], []),

    Result = arizona_plugin:apply_plugins(ArizonaConfig),
    ?assertEqual(ArizonaConfig, Result).

apply_plugins_single_plugin_test(_Config) ->
    ct:comment("Test applying single plugin transforms config correctly"),

    ArizonaConfig = mock_arizona_config([{view, ~"/test", test_view, #{}, []}], [
        {arizona_example_plugin, #{test => true}}
    ]),

    Result = arizona_plugin:apply_plugins(ArizonaConfig),

    % Check that arizona_example_plugin added middleware to the view route
    #{server := #{routes := [{view, ~"/test", test_view, #{}, Middlewares}]}} = Result,
    ?assertMatch([{example_middleware, #{test := true}} | _], Middlewares).

apply_plugins_multiple_plugins_test(_Config) ->
    ct:comment("Test applying multiple plugins in order"),

    ArizonaConfig = mock_arizona_config([{view, ~"/test", test_view, #{}, []}], [
        {arizona_example_plugin, #{test => plugin1}},
        {arizona_example_plugin, #{test => plugin2}}
    ]),

    Result = arizona_plugin:apply_plugins(ArizonaConfig),

    % Check that both plugins added middleware (plugin2 should be first in list)
    #{server := #{routes := [{view, ~"/test", test_view, #{}, Middlewares}]}} = Result,
    ?assertMatch(
        [
            {example_middleware, #{test := plugin2}},
            {example_middleware, #{test := plugin1}}
        ],
        Middlewares
    ).

apply_plugins_plugin_error_test(_Config) ->
    ct:comment("Test plugin error handling"),

    ArizonaConfig = mock_arizona_config([], [{nonexistent_plugin, #{}}]),

    % Should crash with clear error message
    ?assertError(
        {plugin_failed, nonexistent_plugin, error, undef},
        arizona_plugin:apply_plugins(ArizonaConfig)
    ).

%% --------------------------------------------------------------------
%% Helper Functions
%% --------------------------------------------------------------------

mock_arizona_config(Routes, Plugins) ->
    #{
        server => #{enabled => true, routes => Routes},
        reloader => #{enabled => false, rules => []},
        plugins => Plugins
    }.
