-module(arizona_transform_probe_controller).
-moduledoc """
**TEST FIXTURE.** Deterministic body for the HTTP response-transform wire
tests: same bytes every call, so a weak ETag derived from it revalidates.
""".

-export([handle/2]).

-spec handle(Req, Ctx) -> {Reply, Req} when
    Req :: arizona_req:request(),
    Ctx :: arizona_controller:context(),
    Reply :: arizona_controller:reply().
handle(Req, _Ctx) ->
    {{200, [{~"content-type", ~"text/plain"}], ~"stable-transform-probe-body"}, Req}.
