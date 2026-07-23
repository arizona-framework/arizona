-module(arizona_term_stop_driver).
-moduledoc """
A terminal driver that stops on its very first paint.

Exercises the initial paint's `continue`/`stop` verdict: `arizona_terminal_session`
must honor a `stop` from the first `paint/3` (emit the teardown, stop the live
view, and answer `quit`) exactly as it does for every later paint. Every other
callback falls back to `m:arizona_terminal_default_driver`.
""".
-behaviour(arizona_terminal_driver).

-export([init/1]).
-export([paint/3]).
-export([teardown/1]).

-spec init(term()) -> map().
init(_Arg) ->
    #{}.

-spec paint(binary(), [arizona_effect:cmd()], State) -> {iodata(), stop, State}.
paint(Frame, _Effects, State) ->
    {Frame, stop, State}.

-doc "A recognizable marker so a test can prove the teardown was emitted.".
-spec teardown(term()) -> iodata().
teardown(_State) ->
    ~"stop-driver-teardown".
