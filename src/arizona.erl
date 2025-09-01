-module(arizona).

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

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-nominal config() :: #{
    server => arizona_server:config(),
    reloader => arizona_reloader:config()
}.

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-spec start(Config) -> Result when
    Config :: config(),
    Result :: ok | {error, ErrReason},
    ErrReason :: term().
start(Opts) when is_map(Opts) ->
    maybe
        % Apply defaults to server config
        UserServerConfig = maps:get(server, Opts, #{}),
        ServerConfig = apply_server_defaults(UserServerConfig),
        ReloaderConfig = maps:get(reloader, Opts, #{}),
        ok ?= maybe_start_reloader(ReloaderConfig),
        ok ?= maybe_start_server(ServerConfig),
        ok = persistent_term:put(arizona_config, #{
            server => ServerConfig,
            reloader => ReloaderConfig
        }),
        ok
    else
        {error, Reason} ->
            {error, Reason}
    end.

-spec stop() -> ok.
stop() ->
    arizona_server:stop().

-spec get_config() -> config().
get_config() ->
    persistent_term:get(arizona_config).

-spec get_config(Key) -> Config when
    Key :: server | reloader,
    Config :: arizona_server:config() | arizona_reloader:config().
get_config(Key) ->
    maps:get(Key, get_config()).

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% Start reloader system if enabled
maybe_start_reloader(#{enabled := true} = ReloaderConfig) ->
    case arizona_reloader:start_link(ReloaderConfig) of
        {ok, _Pid} -> ok;
        {error, Reason} -> {error, {reloader_failed, Reason}}
    end;
maybe_start_reloader(_ReloaderConfig) ->
    ok.

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
