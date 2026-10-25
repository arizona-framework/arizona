-module(arizona_test_pubsub).
-moduledoc """
**TEST SUPPORT.** The `arizona_pubsub` scope for suites that run live views
without the arizona application, which normally owns it.

`start/0` starts the scope unless one is already running and returns what it
started; pass that to `stop/1` when the suite ends. A scope left behind would
make the next suite that boots the application fail on the taken name.
""".

-export([start/0]).
-export([stop/1]).

-doc "Starts the scope, unlinked, when none runs. Returns its pid, or `undefined`.".
-spec start() -> pid() | undefined.
start() ->
    case erlang:whereis(arizona_pubsub) of
        undefined ->
            {ok, Pid} = pg:start_link(arizona_pubsub),
            true = unlink(Pid),
            Pid;
        _Running ->
            undefined
    end.

-doc "Stops the scope `start/0` started; `undefined` (it started none) is a no-op.".
-spec stop(pid() | undefined) -> ok.
stop(undefined) ->
    ok;
stop(Pid) ->
    gen_server:stop(Pid).
