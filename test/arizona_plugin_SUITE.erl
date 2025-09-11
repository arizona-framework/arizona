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

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

%% --------------------------------------------------------------------
%% Plugin tests
%% --------------------------------------------------------------------

apply_plugins_empty_list_test(_Config) ->
    ct:comment("Test applying empty plugin list returns original config"),

    % Set empty plugins list
    application:set_env(arizona, plugins, []),

    OriginalConfig = #{
        enabled => true,
        routes => [{view, ~"/", home_view, #{}, []}]
    },

    Result = arizona_plugin:apply_plugins(OriginalConfig),
    ?assertEqual(OriginalConfig, Result).

apply_plugins_single_plugin_test(_Config) ->
    ct:comment("Test applying single plugin transforms config correctly"),

    % Set single plugin
    application:set_env(arizona, plugins, [{arizona_example_plugin, #{test => true}}]),

    OriginalConfig = #{
        enabled => true,
        routes => [{view, ~"/test", test_view, #{}, []}]
    },

    Result = arizona_plugin:apply_plugins(OriginalConfig),

    % Check that arizona_example_plugin added middleware to the view route
    #{routes := [{view, ~"/test", test_view, #{}, Middlewares}]} = Result,
    ?assertMatch([{example_middleware, #{test := true}} | _], Middlewares).

apply_plugins_multiple_plugins_test(_Config) ->
    ct:comment("Test applying multiple plugins in order"),

    % Set multiple plugins
    application:set_env(arizona, plugins, [
        {arizona_example_plugin, #{test => plugin1}},
        {arizona_example_plugin, #{test => plugin2}}
    ]),

    OriginalConfig = #{
        enabled => true,
        routes => [{view, ~"/test", test_view, #{}, []}]
    },

    Result = arizona_plugin:apply_plugins(OriginalConfig),

    % Check that both plugins added middleware (plugin2 should be first in list)
    #{routes := [{view, ~"/test", test_view, #{}, Middlewares}]} = Result,
    ?assertMatch(
        [
            {example_middleware, #{test := plugin2}},
            {example_middleware, #{test := plugin1}}
        ],
        Middlewares
    ).

apply_plugins_plugin_error_test(_Config) ->
    ct:comment("Test plugin error handling"),

    % Set plugin that doesn't exist
    application:set_env(arizona, plugins, [{nonexistent_plugin, #{}}]),

    OriginalConfig = #{enabled => true, routes => []},

    % Should crash with clear error message
    ?assertError(
        {plugin_failed, nonexistent_plugin, error, undef},
        arizona_plugin:apply_plugins(OriginalConfig)
    ).
