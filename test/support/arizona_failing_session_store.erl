-module(arizona_failing_session_store).
-moduledoc """
**TEST FIXTURE.** An `arizona_session_store` whose read always fails, standing in for a
backend with a fallible transport (a database outage, a connection-pool timeout). `get/1`
answers `{error, Reason}` so tests can assert that a store failure is observable via
`arizona_req:session_error/1`, distinct from a genuinely absent session. `Reason` is the
`test_failing_store_reason` app env (default `store_unreachable`), so a test can also drive
the `{error, undefined}` case that the tagged return keeps distinct from the no-error
sentinel.
""".

-behaviour(arizona_session_store).

%% `get/1` is the store API, not the `erlang:get/1` BIF.
-compile({no_auto_import, [get/1]}).

-export([get/1]).
-export([put/3]).
-export([delete/1]).

-spec get(Id) -> {error, term()} when Id :: binary().
get(_Id) ->
    {error, application:get_env(arizona, test_failing_store_reason, store_unreachable)}.

-spec put(Id, Session, TtlSecs) -> ok when
    Id :: binary(),
    Session :: arizona_req:session(),
    TtlSecs :: pos_integer().
put(_Id, _Session, _TtlSecs) ->
    ok.

-spec delete(Id) -> ok when Id :: binary().
delete(_Id) ->
    ok.
