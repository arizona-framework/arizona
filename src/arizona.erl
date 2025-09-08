-module(arizona).
-moduledoc ~"""
Main application entry point and configuration management for Arizona framework.

Provides the primary API for starting, stopping, and configuring the Arizona
web framework. Coordinates the startup of all subsystems including the HTTP/WebSocket
server and development-time file reloader.

## Framework Configuration

The framework accepts a configuration map with two main sections:
- `server` - HTTP/WebSocket server configuration
- `watcher` - Development-time file watcher configuration

Both subsystems can be independently enabled or disabled.

## Example Configuration

```erlang
Config = #{
    server => #{
        enabled => true,
        scheme => http,
        transport_opts => #{socket_opts => [{port, 4000}]},
        routes => [
            {view, ~"/", home_view, #{}},
            {view, ~"/users/[:id]", users_view, #{}},
            {websocket, ~"/live"},
            {controller, ~"/api/presence", my_api_controller, #{}},
            {asset, ~"/static", {priv_dir, myapp, ~"static"}}
        ]
    },
    watcher => #{
        enabled => true,
        rules => [
            #{
                directories => ["src"],
                patterns => [".*\\.erl$"],
                callback => fun(Files) ->
                    lists:foreach(fun compile:file/1, Files)
                end,
                debounce_ms => 100
            }
        ]
    }
}.
```

## Startup Sequence

1. Apply defaults to server configuration
2. Start file watchers if enabled
3. Start HTTP/WebSocket server if enabled
4. Store final configuration in persistent term
5. Return `ok` or `{error, Reason}`

## Typical Usage

```erlang
%% Start framework
ok = arizona:start(Config).

%% Get runtime configuration
ServerConfig = arizona:get_config(server).

%% Stop framework
ok = arizona:stop().
```
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([start/1]).
-export([stop/0]).
-export([get_config/0]).
-export([get_config/1]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

-ignore_xref([start/1]).
-ignore_xref([stop/0]).
-ignore_xref([get_config/0]).
-ignore_xref([get_config/1]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([config/0]).
-export_type([watcher_config/0]).
-export_type([watcher_rule/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-nominal config() :: #{
    server => arizona_server:config(),
    watcher => watcher_config()
}.
-nominal watcher_config() :: #{
    enabled := boolean(),
    rules := [watcher_rule()]
}.
-nominal watcher_rule() :: #{
    directories := [string()],
    patterns => [string()],
    callback := arizona_watcher:watcher_callback(),
    debounce_ms => pos_integer()
}.

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-doc ~"""
Starts the Arizona framework with the given configuration.

Coordinates startup of all framework subsystems, applies configuration
defaults, and stores the final configuration for runtime access.
Returns `ok` on success or `{error, Reason}` on failure.
""".
-spec start(Config) -> Result when
    Config :: config(),
    Result :: ok | {error, ErrReason},
    ErrReason :: term().
start(Opts) when is_map(Opts) ->
    maybe
        % Apply defaults to server config
        UserServerConfig = maps:get(server, Opts, #{}),
        ServerConfig = apply_server_defaults(UserServerConfig),
        WatcherConfig = maps:get(watcher, Opts, #{}),
        ok ?= maybe_start_watchers(WatcherConfig),
        ok ?= maybe_start_server(ServerConfig),
        ok = persistent_term:put(arizona_config, #{
            server => ServerConfig,
            watcher => WatcherConfig
        }),
        ok
    else
        {error, Reason} ->
            {error, Reason}
    end.

-doc ~"""
Stops the Arizona framework.

Gracefully shuts down the HTTP/WebSocket server. Note that the
reloader process is not explicitly stopped as it will terminate
when the application shuts down.
""".
-spec stop() -> ok.
stop() ->
    arizona_server:stop().

-doc ~"""
Returns the complete framework configuration.

Gets the configuration that was stored during framework startup,
including any applied defaults.
""".
-spec get_config() -> config().
get_config() ->
    persistent_term:get(arizona_config).

-doc ~"""
Returns configuration for a specific subsystem.

Gets either the server or reloader configuration from the stored
framework configuration.
""".
-spec get_config(Key) -> Config when
    Key :: server | watcher,
    Config :: arizona_server:config() | watcher_config().
get_config(Key) ->
    maps:get(Key, get_config()).

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% Start file watcher system if enabled
maybe_start_watchers(#{enabled := true, rules := Rules}) ->
    case start_watcher_supervisor() of
        {ok, _SupPid} ->
            start_watcher_instances(Rules);
        {error, Reason} ->
            {error, {watcher_supervisor_failed, Reason}}
    end;
maybe_start_watchers(_WatcherConfig) ->
    ok.

start_watcher_supervisor() ->
    case arizona_watcher_sup:start_link() of
        {ok, Pid} -> {ok, Pid};
        {error, {already_started, Pid}} -> {ok, Pid};
        Error -> Error
    end.

start_watcher_instances([]) ->
    ok;
start_watcher_instances([Rule | Rules]) ->
    case arizona_watcher_sup:start_child(Rule) of
        {ok, _Pid} ->
            start_watcher_instances(Rules);
        {error, Reason} ->
            {error, {watcher_instance_failed, Reason}}
    end.

maybe_start_server(#{enabled := true} = ServerConfig) ->
    case arizona_server:start(ServerConfig) of
        {ok, _Pid} -> ok;
        {error, Reason} -> {error, {server_failed, Reason}}
    end;
maybe_start_server(_ServerConfig) ->
    ok.

%% Apply defaults to server config
apply_server_defaults(UserServerConfig) ->
    % Set basic defaults
    Enabled = maps:get(enabled, UserServerConfig, true),
    Scheme = maps:get(scheme, UserServerConfig, http),
    Routes = maps:get(routes, UserServerConfig, []),
    TransportOpts = maps:get(transport_opts, UserServerConfig, []),
    ProtoOpts = maps:get(proto_opts, UserServerConfig, #{}),

    #{
        enabled => Enabled,
        scheme => Scheme,
        routes => Routes,
        transport_opts => TransportOpts,
        proto_opts => ProtoOpts
    }.
