-module(arizona_fetch_account_controller).
-moduledoc """
**TEST FIXTURE.** The `{controller, ...}` endpoint `arizona_fetch_account` posts to via
`arizona_js:fetch/2`. It sets an HttpOnly cookie (the thing the WebSocket can't do),
broadcasts over `arizona_pubsub` so the live view re-renders, and returns a request-local
effect -- all without a page reload. Drives `e2e/sequential/arizona_fetch_account.spec.js`.
""".

-behaviour(roadrunner_handler).

-export([handle/1]).

-spec handle(Req) -> {Response, Req} when
    Req :: roadrunner_req:request(),
    Response :: roadrunner_handler:response().
handle(Req) ->
    %% Re-render the live view via pubsub: server-computed content (the message)
    %% reaches the connected view and is rendered server-authoritatively over the WS.
    arizona_pubsub:broadcast(fetch_account, {saved, ~"Account updated"}),
    %% Request-local effect applied on the page by the fetch command.
    Resp0 = arizona_controller:reply_effects([
        arizona_js:set_attr(~"#status", ~"data-saved", ~"yes")
    ]),
    %% The real Set-Cookie -- the reason fetch exists.
    Resp1 = roadrunner_resp:set_cookie(Resp0, ~"sid", ~"rotated", #{
        http_only => true, path => ~"/", same_site => strict
    }),
    {Resp1, Req}.
