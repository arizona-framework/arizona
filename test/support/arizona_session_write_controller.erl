-module(arizona_session_write_controller).
-moduledoc """
**TEST FIXTURE.** Writes the session and threads the mutated request back with
`arizona_controller:put_req/2`.

The cookie-mode half of the round trip: with no `session_store` configured the
write has nowhere to land but the response, so this pins that `put_req/2` carries
it out as the encrypted `Set-Cookie`. The store-mode fixture
(`arizona_logout_controller`) covers the other mode, where there is also a
server-side entry to check.
""".

-behaviour(roadrunner_handler).

-export([handle/1]).

-spec handle(Req) -> {Response, Req} when
    Req :: roadrunner_req:request(),
    Response :: roadrunner_handler:response().
handle(Req) ->
    ArzReq = arizona_req:put_session(arizona_controller:req(Req), user_id, ~"u42"),
    {arizona_controller:reply_effects([]), arizona_controller:put_req(ArzReq, Req)}.
