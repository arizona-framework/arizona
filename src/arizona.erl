-module(arizona).
-moduledoc """
Arizona Framework - Core types and utilities.

Provides central type definitions and utility functions.
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([get_config/0]).

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

-spec get_config() -> config().
get_config() ->
    case persistent_term:get(arizona_config, undefined) of
        undefined ->
            BaseConfig = #{
                server => get_server_config(),
                reloader => get_reloader_config(),
                plugins => get_plugins()
            },
            Config = arizona_plugin:apply_plugins(BaseConfig),
            ok = persistent_term:put(arizona_config, Config),
            Config;
        Config ->
            Config
    end.

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
