-module(arizona_fetch_error_controller).
-moduledoc """
**TEST FIXTURE.** The controller `arizona_fetch_error` posts to via `arizona_js:fetch/2`.
It replies `500` with an empty body -- no usable effects body -- so the client runs the
fetch's `on_error` commands and dispatches `arizona:fetch-error`. Drives
`e2e/parallel/arizona_fetch_error.spec.js`.
""".

-export([handle/2]).

-spec handle(Req, Ctx) -> {Reply, Req} when
    Req :: arizona_req:request(),
    Ctx :: arizona_controller:context(),
    Reply :: arizona_controller:reply().
handle(Req, _Ctx) ->
    {{500, [], <<>>}, Req}.
