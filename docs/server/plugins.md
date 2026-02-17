# Plugins

- [Behaviour](#behaviour)
  - [transform\_config/2](#transform_config2)
- [Execution Order](#execution-order)
- [Configuration](#configuration)
  - [sys.config](#sysconfig)
  - [Hex.pm](#hexpm)

## Behaviour

Plugins implement the `arizona_plugin` behaviour to transform the application configuration before
the server starts. This allows third-party packages to extend Arizona's setup -- adding routes,
injecting middleware, or modifying server options without requiring manual configuration from the
user.

### transform\_config/2

The single required callback. It receives the current config map and the plugin-specific config, and
must return the (possibly modified) config map.

```erlang
-module(my_auth_plugin).
-behaviour(arizona_plugin).
-export([transform_config/2]).

transform_config(Config, PluginConfig) ->
    %% Add additional routes from PluginConfig
    Routes = maps:get(routes, maps:get(server, Config, #{}), []),
    ExtraRoutes = maps:get(extra_routes, PluginConfig, []),
    ServerConfig = maps:get(server, Config, #{}),
    Config#{server => ServerConfig#{routes => Routes ++ ExtraRoutes}}.
```

The first argument is the full Arizona config map. The second argument is whatever value was paired
with the plugin module in the `plugins` list. The return value must be a valid Arizona config map.

## Execution Order

Plugins are applied sequentially in the order they appear in the configuration list via
`arizona_plugin:apply_plugins/1`. Each plugin receives the config as modified by all previous
plugins, so ordering matters when plugins depend on each other's transformations.

If a plugin raises an error during execution, the error is logged and then re-raised as
`{plugin_failed, PluginName, Error, Reason}`. This aborts the plugin pipeline — remaining plugins
will not execute.

## Configuration

Plugins are declared in the Arizona config under the `plugins` key. Each entry is a `{PluginModule,
PluginConfig}` tuple.

### sys.config

Add plugins to your `sys.config`:

```erlang
[{arizona, #{
    server => #{routes => [...]},
    plugins => [
        {my_auth_plugin, #{extra_routes => [{view, ~"/extra", extra_view, #{}, []}]}}
    ]
}}].
```

Each plugin entry is a `{PluginModule, PluginConfig}` tuple. The `PluginConfig` value can be any
Erlang term -- its structure is defined by the plugin module.

### Hex.pm

Third-party plugins can be distributed as Hex packages. Add the package to your `rebar.config`
dependencies, then reference the plugin module in your `sys.config`:

```erlang
%% rebar.config
{deps, [
    {arizona, {git, "https://github.com/arizona-framework/arizona", {branch, "main"}}},
    {my_auth_plugin, "~> 0.1"}
]}.
```

```erlang
%% sys.config
[{arizona, #{
    plugins => [
        {my_auth_plugin, #{jwt_secret => ~"secret123"}}
    ]
}}].
```

The plugin module must be available on the code path at startup. As long as the package is listed in
your deps and compiled, its plugin modules can be referenced directly in the config.

See also: [Configuration](configuration.md), [Routing](routing.md)
