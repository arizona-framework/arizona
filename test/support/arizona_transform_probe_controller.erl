-module(arizona_transform_probe_controller).
-moduledoc """
**TEST FIXTURE.** Deterministic body for the HTTP response-transform wire
tests: same bytes every call, so a weak ETag derived from it revalidates.
""".

-behaviour(roadrunner_handler).

-export([handle/1]).

-spec handle(Req) -> {Response, Req} when
    Req :: roadrunner_req:request(),
    Response :: roadrunner_handler:response().
handle(Req) ->
    {{200, [{~"content-type", ~"text/plain"}], ~"stable-transform-probe-body"}, Req}.
