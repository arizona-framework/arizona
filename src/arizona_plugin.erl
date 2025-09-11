-module(arizona_plugin).
-moduledoc ~"""
Arizona plugin system for configuration transformation.

Provides both the plugin behavior definition and the plugin loading functionality.
Plugins are simple Erlang modules that implement a single callback to transform
the Arizona server configuration before startup.

## Plugin Interface

```erlang
-module(my_plugin).
-behaviour(arizona_plugin).

transform_config(Config, PluginConfig) ->
    % Transform the Arizona server config
    % Add middleware, modify routes, etc.
    Config.
```

## Usage

Plugins are configured as a proplist in sys.config:

```erlang
{plugins, [
    {my_auth_plugin, #{jwt_secret => "secret123"}},  % Map config
    {my_cors_plugin, ["*"]},                          % List config
    {my_simple_plugin, true},                         % Boolean config
    {my_other_plugin, {option, value}}                % Tuple config
]}
```

Plugins execute in list order during Arizona application startup.
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([apply_plugins/1]).

%% --------------------------------------------------------------------
%% Behavior callback exports
%% --------------------------------------------------------------------

-callback transform_config(Config, PluginConfig) -> Config when
    Config :: arizona_server:config(),
    PluginConfig :: dynamic().

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-doc ~"""
Apply all configured plugins to transform the Arizona server config.

Plugins are executed sequentially in the order they appear in the configuration.
If any plugin fails, the application startup will fail with a clear error
message indicating which plugin caused the failure.
""".
-spec apply_plugins(Config) -> Config when
    Config :: arizona_server:config().
apply_plugins(Config) ->
    % Get list of plugins from Arizona config
    Plugins = application:get_env(arizona, plugins, []),

    % Apply each plugin transformation in order
    lists:foldl(fun apply_plugin/2, Config, Plugins).

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% Apply a single plugin transformation with error handling
apply_plugin({PluginName, PluginConfig}, Config) ->
    try
        call_transform_config_callback(PluginName, Config, PluginConfig)
    catch
        Error:Reason:_Stack ->
            ok = logger:warning("Plugin ~p failed: ~p:~p~n", [PluginName, Error, Reason]),
            error({plugin_failed, PluginName, Error, Reason})
    end.

call_transform_config_callback(PluginName, Config, PluginConfig) ->
    apply(PluginName, transform_config, [Config, PluginConfig]).
