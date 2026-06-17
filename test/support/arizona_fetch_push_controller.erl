-module(arizona_fetch_push_controller).
-moduledoc """
**TEST FIXTURE.** The controller `arizona_fetch_push` posts to via `arizona_js:fetch/2`.
It sets a cookie and returns an `arizona_js:push_event` -- the client relays it over the
existing WebSocket so the **submitting** view re-renders via `handle_event/3`, with no
pubsub. Drives `e2e/parallel/arizona_fetch_push.spec.js`.
""".

-behaviour(roadrunner_handler).

-export([handle/1]).

-spec handle(Req) -> {Response, Req} when
    Req :: roadrunner_req:request(),
    Response :: roadrunner_handler:response().
handle(Req) ->
    %% Tell the submitting view to refresh -- the client relays this over its WS.
    Resp0 = arizona_controller:reply_effects([
        arizona_js:push_event(~"saved")
    ]),
    Resp1 = roadrunner_resp:set_cookie(Resp0, ~"pushed", ~"1", #{
        http_only => true, path => ~"/", same_site => strict
    }),
    {Resp1, Req}.
