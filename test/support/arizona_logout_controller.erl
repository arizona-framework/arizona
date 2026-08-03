-module(arizona_logout_controller).
-moduledoc """
**TEST FIXTURE.** Store-mode logout, both halves of the thread-back boundary.

`handle/1` is the documented round trip: read the post-middleware request with
`arizona_controller:req/1`, mutate it (`arizona_req:clear_session/1`), thread it
back with `arizona_controller:put_req/2`, and return that request -- so the
dispatcher flushes the **action's** request (the clearing `Set-Cookie` plus the
store delete).

`handle_dropped/1` does the same mutation but returns the request it was given
unchanged, pinning the boundary: without the thread-back the pre-action request
is what gets flushed, so the write is dropped.
""".

-behaviour(roadrunner_handler).

-export([handle/1]).
-export([handle_dropped/1]).

-spec handle(Req) -> {Response, Req} when
    Req :: roadrunner_req:request(),
    Response :: roadrunner_handler:response().
handle(Req) ->
    ArzReq = arizona_req:clear_session(arizona_controller:req(Req)),
    {arizona_controller:reply_effects([]), arizona_controller:put_req(Req, ArzReq)}.

-spec handle_dropped(Req) -> {Response, Req} when
    Req :: roadrunner_req:request(),
    Response :: roadrunner_handler:response().
handle_dropped(Req) ->
    %% Deliberately NOT threaded back: the mutated request is discarded here, so
    %% the dispatcher flushes the pre-action one.
    _ClearedReq = arizona_req:clear_session(arizona_controller:req(Req)),
    {arizona_controller:reply_effects([]), Req}.
