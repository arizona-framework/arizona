-module(arizona_app).
-moduledoc """
OTP application entry point.

Boots `arizona_sup`, then -- if the `server` application env is set --
starts a roadrunner listener named `arizona_http` via
`arizona_roadrunner_server:start/2`. On shutdown, stops that
listener.

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

-spec stop(State) -> ok when
    State :: term().
stop(_State) ->
    case application:get_env(arizona, server) of
        {ok, _ServerOpts} ->
            case arizona_roadrunner_server:stop(?LISTENER) of
                ok -> ok;
                {error, not_found} -> ok
            end;
        undefined ->
            ok
    end.

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
