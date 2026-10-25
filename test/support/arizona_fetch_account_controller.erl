-module(arizona_fetch_account_controller).
-moduledoc """
**TEST FIXTURE.** The `{post, ...}` controller endpoint `arizona_fetch_account` posts to via
`arizona_js:fetch/2`. It sets an HttpOnly cookie (the thing the WebSocket can't do),
broadcasts over `arizona_pubsub` so the live view re-renders, and returns a request-local
effect -- all without a page reload. Drives `e2e/sequential/arizona_fetch_account.spec.js`.
""".

-export([handle/2]).

-spec handle(Req, Ctx) -> {Reply, Req} when
    Req :: arizona_req:request(),
    Ctx :: arizona_controller:context(),
    Reply :: arizona_controller:reply().
handle(Req, _Ctx) ->
    %% Re-render the live view via pubsub: server-computed content (the message)
    %% reaches the connected view and is rendered server-authoritatively over the WS.
    arizona_pubsub:broadcast(fetch_account, {saved, ~"Account updated"}),
    %% Request-local effect applied on the page by the fetch command.
    Reply = arizona_controller:reply_effects([
        arizona_js:set_attr(~"#status", ~"data-saved", ~"yes")
    ]),
    %% The real Set-Cookie -- the reason fetch exists.
    {Reply,
        arizona_req:put_resp_cookie(Req, ~"sid", ~"rotated", #{
            http_only => true, path => ~"/", same_site => strict
        })}.
