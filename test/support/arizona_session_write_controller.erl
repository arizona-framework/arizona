-module(arizona_session_write_controller).
-moduledoc """
**TEST FIXTURE.** Writes the session and returns the written request.

The cookie-mode half: with no `session_store` configured the write has nowhere to
land but the response, so this pins that the request an action returns carries it
out as the encrypted `Set-Cookie`. The store-mode fixture
(`arizona_logout_controller`) covers the other mode, where there is also a
server-side entry to check.
""".

-export([handle/2]).

-spec handle(Req, Ctx) -> {Reply, Req} when
    Req :: arizona_req:request(),
    Ctx :: arizona_controller:context(),
    Reply :: arizona_controller:reply().
handle(Req, _Ctx) ->
    {arizona_controller:reply_effects([]), arizona_req:put_session(Req, user_id, ~"u42")}.
