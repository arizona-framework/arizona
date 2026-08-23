-module(arizona_app).
-moduledoc """
OTP application entry point.

Boots `arizona_sup`, then -- if the `server` application env is set --
starts a roadrunner listener named `arizona_http` via
`arizona_roadrunner_server:start/2`. On shutdown, stops that listener in
`prep_stop/1` -- **before** the supervision tree goes down -- so it never
serves a request against already-dead infrastructure (pubsub, the MCP
registry, the session store); `stop/1` re-runs the same idempotent
cleanup as a backstop.

## Server config

```erlang
{arizona, [
    {server, #{
        scheme => http,                                 %% http | https
        transport_opts => [{port, 4040}],               %% port shorthand
        proto_opts => #{                                %% roadrunner protocol opts
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
    ok = stop_server(),
    %% Before the tree goes down, so the dev log handler stops writing while the
    %% table it writes to is still alive.
    ok = arizona_dev_log:uninstall(),
    State.

%% Runs after the supervision tree is down. The listener is already stopped by
%% prep_stop/1; this backstop keeps the cleanup idempotent (stop_server/0
%% tolerates an already-stopped listener).
-spec stop(State) -> ok when
    State :: term().
stop(_State) ->
    stop_server().

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

maybe_start_server() ->
    case application:get_env(arizona, server) of
        {ok, ServerOpts} ->
            {ok, _} = application:ensure_all_started(roadrunner),
            case arizona_roadrunner_server:start(?LISTENER, ServerOpts) of
                {ok, _} -> ok;
                {error, _} = Err -> Err
            end;
        undefined ->
            ok
    end.

stop_server() ->
    case application:get_env(arizona, server) of
        {ok, _ServerOpts} ->
            case arizona_roadrunner_server:stop(?LISTENER) of
                ok -> ok;
                {error, not_found} -> ok
            end;
        undefined ->
            ok
    end.
