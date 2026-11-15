-module(arizona_fetch_reset_controller).
-moduledoc """
**TEST FIXTURE.** The controller `arizona_fetch_reset` posts to: a 2xx with no
effects, so the form's `az-form-reset` applies. The spec holds the request in
flight from the browser side. Drives `e2e/parallel/arizona_fetch_reset.spec.js`.
""".

-export([handle/2]).

-spec handle(Req, Ctx) -> {Reply, Req} when
    Req :: arizona_req:request(),
    Ctx :: arizona_controller:context(),
    Reply :: arizona_controller:reply().
handle(Req, _Ctx) ->
    {arizona_controller:reply_effects([]), Req}.
