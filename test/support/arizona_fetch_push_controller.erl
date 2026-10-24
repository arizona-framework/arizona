-module(arizona_fetch_push_controller).
-moduledoc """
**TEST FIXTURE.** The controller `arizona_fetch_push` posts to via `arizona_js:fetch/2`.
It sets a cookie and returns an `arizona_js:push_event` -- the client relays it over the
existing WebSocket so the **submitting** view re-renders via `handle_event/3`, with no
pubsub. Drives `e2e/parallel/arizona_fetch_push.spec.js`.
""".

-export([handle/1]).

-spec handle(Req) -> {Reply, Req} when
    Req :: arizona_req:request(),
    Reply :: arizona_controller:reply().
handle(Req) ->
    %% Tell the submitting view to refresh -- the client relays this over its WS.
    Reply = arizona_controller:reply_effects([arizona_js:push_event(~"saved")]),
    {Reply,
        arizona_req:put_resp_cookie(Req, ~"pushed", ~"1", #{
            http_only => true, path => ~"/", same_site => strict
        })}.
