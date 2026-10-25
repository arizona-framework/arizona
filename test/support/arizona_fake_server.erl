-module(arizona_fake_server).
-moduledoc """
In-memory `arizona_server` for the app and reloader suites: it serves nothing,
and reports each callback it receives to the `notify` pid its `start/2` opts
carry, as `{arizona_fake_server, Callback, Args}`. Its listener is a plain
process, so `stop/1`, `drain/2` and `notify_drain/2` answer `{error, not_found}` once it is gone.
""".
-behaviour(arizona_server).

-export([start/2]).
-export([stop/1]).
-export([drain/2]).
-export([notify_drain/2]).
-export([recompile_routes/0]).
-export([capabilities/0]).

-define(NOTIFY, {?MODULE, notify}).

start(Name, #{notify := Pid} = Opts) ->
    persistent_term:put(?NOTIFY, Pid),
    Listener = spawn(fun() ->
        receive
            stop -> ok
        after 60000 -> ok
        end
    end),
    true = register(Name, Listener),
    ok = arizona_server_registry:listener_up(?MODULE, Name, []),
    ok = notify({start, Name, Opts}),
    {ok, Listener}.

stop(Name) ->
    ok = notify({stop, Name}),
    stop_listener(Name).

drain(Name, Timeout) ->
    ok = notify({drain, Name, Timeout}),
    stop_listener(Name).

notify_drain(Name, Timeout) ->
    ok = notify({notify_drain, Name, Timeout}),
    case whereis(Name) of
        undefined -> {error, not_found};
        _Pid -> ok
    end.

recompile_routes() ->
    notify(recompile_routes).

%% The baseline only, so a capability kind is refused.
capabilities() ->
    #{routes => [], replies => []}.

stop_listener(Name) ->
    ok = arizona_server_registry:listener_down(Name),
    case whereis(Name) of
        undefined ->
            {error, not_found};
        Pid ->
            true = unregister(Name),
            Pid ! stop,
            ok
    end.

notify(Msg) ->
    case persistent_term:get(?NOTIFY, undefined) of
        undefined ->
            ok;
        Pid ->
            Pid ! {?MODULE, Msg},
            ok
    end.
