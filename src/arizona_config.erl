-module(arizona_config).
-compile({no_auto_import, [get/0]}).
-moduledoc """
Arizona Framework - Configuration management.

Provides configuration loading, caching, and access for the Arizona
framework. Configuration is loaded from application environment and
cached in persistent_term for efficient access.
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([get/0]).
-export([reload/0]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

-ignore_xref([reload/0]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([config/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-doc """
Complete Arizona application configuration.

Contains all Arizona configuration sections including server, reloader,
plugins, and any future configuration sections.
""".
-nominal config() :: #{
    server := arizona_server:config(),
    reloader := arizona_reloader:config(),
    plugins := [arizona_plugin:plugin()],
    % Allow custom config sections
    _ => dynamic()
}.

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc """
Gets the current Arizona configuration.

Returns cached configuration from persistent_term if available,
otherwise loads configuration from application environment,
applies plugins, caches, and returns the result.
""".
-spec get() -> config().
get() ->
    case persistent_term:get(?MODULE, undefined) of
        undefined ->
            BaseConfig = #{
                server => get_server_config(),
                reloader => get_reloader_config(),
                plugins => get_plugins()
            },
            Config = arizona_plugin:apply_plugins(BaseConfig),
            ok = persistent_term:put(?MODULE, Config),
            Config;
        Config ->
            Config
    end.

-doc """
Reloads the Arizona configuration.

Clears the cached configuration from persistent_term, forcing
the next call to get/0 to reload from application environment.
Returns the newly loaded configuration.
""".
-spec reload() -> config().
reload() ->
    _ = persistent_term:erase(?MODULE),
    get().

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

-spec get_server_config() -> Config when
    Config :: arizona_server:config().
get_server_config() ->
    application:get_env(arizona, server, #{enabled => false}).

-spec get_reloader_config() -> Config when
    Config :: arizona_reloader:config().
get_reloader_config() ->
    application:get_env(arizona, reloader, #{enabled => false}).

-spec get_plugins() -> Plugins when
    Plugins :: [arizona_plugin:plugin()].
get_plugins() ->
    application:get_env(arizona, plugins, []).
