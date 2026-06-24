-module(arizona_fetch_error_controller).
-moduledoc """
**TEST FIXTURE.** The controller `arizona_fetch_error` posts to via `arizona_js:fetch/2`.
It replies `500` with an empty body -- no usable effects body -- so the client runs the
fetch's `on_error` commands and dispatches `arizona:fetch-error`. Drives
`e2e/parallel/arizona_fetch_error.spec.js`.
""".

-behaviour(roadrunner_handler).

-export([handle/1]).

-spec handle(Req) -> {Response, Req} when
    Req :: roadrunner_req:request(),
    Response :: roadrunner_handler:response().
handle(Req) ->
    {roadrunner_resp:internal_error(), Req}.
