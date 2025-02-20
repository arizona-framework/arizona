-module(arizona_server).

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([start/1]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------

-define(LISTENER, arizona_http_listener).
-define(PERSIST_KEY, arizona_dispatch).
-define(DEFAULT_PORT, 8080).
-elvis([
    {elvis_style, no_macros, #{
        allow => [
            'LISTENER', 'PERSIST_KEY', 'DEFAULT_PORT'
        ]
    }}
]).

%% --------------------------------------------------------------------
%% Types (and their exports)
%% --------------------------------------------------------------------

-type opts() :: #{
    scheme => http | https,
    transport => ranch_tcp:opts(),
    host => '_' | iodata(),
    % as per cowboy_router:route_match(),
    routes => list(),
    proto => cowboy:opts()
}.
-export_type([opts/0]).

%% --------------------------------------------------------------------
%% API function definitions
%% --------------------------------------------------------------------

-spec start(Opts) -> {ok, ServerPid} | {error, Error} when
    Opts :: opts(),
    ServerPid :: pid(),
    Error :: term().
start(Opts) when is_map(Opts) ->
    start_1(norm_opts(Opts)).

%% --------------------------------------------------------------------
%% Private
%% --------------------------------------------------------------------

start_1(#{scheme := http, transport := Transport, proto := Proto}) ->
    cowboy:start_clear(?LISTENER, Transport, Proto);
start_1(#{scheme := https, transport := Transport, proto := Proto}) ->
    cowboy:start_tls(?LISTENER, Transport, Proto).

norm_opts(Opts) ->
    #{
        scheme => maps:get(scheme, Opts, http),
        transport => norm_transport_opts(maps:get(transport, Opts, [])),
        proto => norm_proto_opts(
            maps:get(host, Opts, '_'),
            maps:get(routes, Opts, []),
            maps:get(proto, Opts, #{})
        )
    }.

norm_transport_opts(Opts) when is_list(Opts) ->
    case proplists:lookup(port, Opts) of
        {port, _Port} ->
            Opts;
        none ->
            [{port, ?DEFAULT_PORT} | Opts]
    end.

norm_proto_opts(Host, Routes, Opts) when
    (Host =:= '_' orelse is_list(Host)), is_list(Routes), is_map(Opts)
->
    Dispatch = cowboy_router:compile([{Host, Routes}]),
    persistent_term:put(?PERSIST_KEY, Dispatch),
    Env = maps:get(env, Opts, #{}),
    Opts#{env => Env#{dispatch => {persistent_term, ?PERSIST_KEY}}}.
