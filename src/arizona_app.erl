-module(arizona_app).
-moduledoc """
OTP application entry point.

Boots `arizona_sup`, then -- if the `server` application env is set --
starts a listener named `arizona_http` through the `arizona_server` adapter
its `adapter` key names (default `arizona_roadrunner_server`). On shutdown,
drains that listener in `prep_stop/1` -- **before** the supervision tree goes
down -- so it never
serves a request against already-dead infrastructure (pubsub, the MCP
registry, the session store); `stop/1` re-runs the same idempotent
cleanup as a backstop. Draining, not just stopping, is what lets clients
come back: each live view gets `handle_drain/2` and closes `1001`, which
web and native clients reconnect from, where a plain stop closes `1000`,
which they take as a deliberate end of session.

## Server config

```erlang
{arizona, [
    {server, #{
        adapter => arizona_roadrunner_server,           %% arizona_server module (default)
        scheme => http,                                 %% http | https
        transport_opts => [{port, 4040}],               %% port shorthand
        drain_timeout => 5000,                          %% ms a stop waits for sessions
        proto_opts => #{                                %% adapter-specific: roadrunner's
            max_clients => 200,
            max_content_length => 10485760
        },
        routes => [
            {live, ~"/", my_page, #{layouts => [{my_layout, render}]}},
            {ws, ~"/ws", #{}},
            {asset, ~"/priv", {priv_dir, my_app, "static"}}
        ]
    }}
]}
```

The `server` key is optional: if absent, only the supervisor starts
(useful for tests that launch listeners manually).
""".
-behaviour(application).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------

-define(LISTENER, arizona_http).
-define(DRAIN_TIMEOUT, 5000).
-define(DEFAULT_ADAPTER, arizona_roadrunner_server).

%% --------------------------------------------------------------------
%% application callback exports
%% --------------------------------------------------------------------

-export([start/2]).
-export([prep_stop/1]).
-export([stop/1]).

%% --------------------------------------------------------------------
%% application Callbacks
%% --------------------------------------------------------------------

-spec start(StartType, StartArgs) -> StartRet when
    StartType :: application:start_type(),
    StartArgs :: term(),
    StartRet :: {ok, Pid} | {error, ErrReason},
    Pid :: pid(),
    ErrReason :: term().
start(_Type, _Args) ->
    maybe
        {ok, SupPid} ?= arizona_sup:start_link(),
        ok ?= maybe_start_server(),
        {ok, SupPid}
    else
        {error, _} = Err -> Err
    end.

%% Runs before the supervision tree is taken down: stop accepting HTTP/WS
%% traffic first, so no request is served against dead infrastructure.
-spec prep_stop(State) -> State when
    State :: term().
prep_stop(State) ->
    ok = drain_server(),
    %% Before the tree goes down, so the dev log handler stops writing while the
    %% table it writes to is still alive.
    ok = arizona_dev_log:uninstall(),
    State.

%% Runs after the supervision tree is down. The listener is already drained by
%% prep_stop/1; this backstop keeps the cleanup idempotent (stop_server/0
%% tolerates an already-stopped listener).
-spec stop(State) -> ok when
    State :: term().
stop(_State) ->
    ok = arizona_dev_log:uninstall(),
    stop_server().

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

maybe_start_server() ->
    case application:get_env(arizona, server) of
        {ok, ServerOpts} ->
            {Adapter, AdapterOpts} = adapter(ServerOpts),
            case Adapter:start(?LISTENER, AdapterOpts) of
                {ok, _} -> ok;
                {error, _} = Err -> Err
            end;
        undefined ->
            ok
    end.

drain_server() ->
    case application:get_env(arizona, server) of
        {ok, ServerOpts} ->
            {Adapter, _AdapterOpts} = adapter(ServerOpts),
            Timeout = maps:get(drain_timeout, arizona_config:resolve(ServerOpts), ?DRAIN_TIMEOUT),
            case Adapter:drain(?LISTENER, Timeout) of
                ok -> ok;
                {error, not_found} -> ok
            end;
        undefined ->
            ok
    end.

stop_server() ->
    case application:get_env(arizona, server) of
        {ok, ServerOpts} ->
            {Adapter, _AdapterOpts} = adapter(ServerOpts),
            case Adapter:stop(?LISTENER) of
                ok -> ok;
                {error, not_found} -> ok
            end;
        undefined ->
            ok
    end.

%% The `server` map names its adapter; every other key but `drain_timeout`
%% (which this module reads) is the adapter's own.
adapter(ServerOpts) ->
    Adapter = maps:get(adapter, ServerOpts, ?DEFAULT_ADAPTER),
    {Adapter, maps:without([adapter, drain_timeout], ServerOpts)}.
