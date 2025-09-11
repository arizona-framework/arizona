-module(arizona_example_plugin).
-moduledoc ~"""
Example plugin for testing the Arizona plugin system.

This plugin demonstrates how to implement the arizona_plugin behavior
by transforming the server configuration to add a simple middleware
to all view routes.
""".

-behaviour(arizona_plugin).

%% --------------------------------------------------------------------
%% Behavior callback exports
%% --------------------------------------------------------------------

-export([transform_config/2]).

%% --------------------------------------------------------------------
%% Behavior callback definitions
%% --------------------------------------------------------------------

-doc ~"""
Transform the Arizona server config by adding example middleware to view routes.

This example plugin adds a simple logging middleware to all view routes
to demonstrate config transformation.
""".
transform_config(Config, PluginConfig) ->
    Routes = maps:get(routes, Config, []),

    % Add example middleware to all view routes
    TransformedRoutes = lists:map(
        fun(Route) ->
            case Route of
                {view, Path, ViewModule, MountArg, Middlewares} ->
                    % Add example middleware to the beginning of middleware list
                    ExampleMiddleware = {example_middleware, PluginConfig},
                    {view, Path, ViewModule, MountArg, [ExampleMiddleware | Middlewares]};
                Other ->
                    Other
            end
        end,
        Routes
    ),

    Config#{routes => TransformedRoutes}.
