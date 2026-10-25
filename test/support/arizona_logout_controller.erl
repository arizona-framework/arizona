-module(arizona_logout_controller).
-moduledoc """
**TEST FIXTURE.** Store-mode logout, both sides of the returned-request boundary.

`handle/1` clears the session (`arizona_req:clear_session/1`) and returns that
request, so the dispatcher flushes the **action's** request (the clearing
`Set-Cookie` plus the store delete).

`handle_dropped/1` does the same mutation but returns the request it was given,
pinning the boundary: what gets flushed is the request the action returns, so the
discarded write is dropped.
""".

-export([handle/2]).
-export([handle_dropped/2]).

-spec handle(Req, Ctx) -> {Reply, Req} when
    Req :: arizona_req:request(),
    Ctx :: arizona_controller:context(),
    Reply :: arizona_controller:reply().
handle(Req, _Ctx) ->
    {arizona_controller:reply_effects([]), arizona_req:clear_session(Req)}.

-spec handle_dropped(Req, Ctx) -> {Reply, Req} when
    Req :: arizona_req:request(),
    Ctx :: arizona_controller:context(),
    Reply :: arizona_controller:reply().
handle_dropped(Req, _Ctx) ->
    %% Deliberately NOT returned: the cleared request is discarded, so the
    %% dispatcher flushes the one the action was given.
    _ClearedReq = arizona_req:clear_session(Req),
    {arizona_controller:reply_effects([]), Req}.
