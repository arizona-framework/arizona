-module(arizona_reload_fun_source).
-moduledoc """
A fun factory the dev-reload tests recompile with a different `STEP` and load
twice, as the reloader does: the second load purges the code a fun made before
the first one points into, so calling that fun raises `{badfun, _}`.

`STEP` changes only `version/0`, so the module's code differs per load while the
fun computes the same thing in every version.
""".

-export([make/0]).
-export([version/0]).

-ifndef(STEP).
-define(STEP, 1).
-endif.

-spec make() -> fun((integer()) -> integer()).
make() ->
    fun(N) -> N + 1 end.

-spec version() -> pos_integer().
version() ->
    ?STEP.
