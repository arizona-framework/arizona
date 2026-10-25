-module(arizona_slow_controller).
-moduledoc """
**TEST FIXTURE.** Answers after the delay its route `state` names
(`#{delay_ms => Ms}`), so a request is still in flight when a test drains the
listener.
""".

-export([handle/2]).

-spec handle(Req, Ctx) -> {Reply, Req} when
    Req :: arizona_req:request(),
    Ctx :: arizona_controller:context(),
    Reply :: arizona_controller:reply().
handle(Req, #{state := #{delay_ms := Ms}}) ->
    receive
    after Ms -> ok
    end,
    {{200, [{~"content-type", ~"text/plain"}], ~"SLOW_OK"}, Req}.
