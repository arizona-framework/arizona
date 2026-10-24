-module(arizona_transform_probe_controller).
-moduledoc """
**TEST FIXTURE.** Deterministic body for the HTTP response-transform wire
tests: same bytes every call, so a weak ETag derived from it revalidates.
""".

-export([handle/1]).

-spec handle(Req) -> {Reply, Req} when
    Req :: arizona_req:request(),
    Reply :: arizona_controller:reply().
handle(Req) ->
    {{200, [{~"content-type", ~"text/plain"}], ~"stable-transform-probe-body"}, Req}.
