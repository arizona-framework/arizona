-module(arizona_session_controller).
-moduledoc """
**TEST FIXTURE.** The `{post, ...}` endpoint `arizona_session_view` posts to via
`arizona_js:fetch/2`. It rotates the encrypted `az_session` cookie (the thing the
WebSocket can't do) via `arizona_session:set_cookie/1`, and returns a request-local
effect plus a `push_event` carrying the new name so the submitting view re-renders live,
all without a reload. Drives `e2e/parallel/arizona_session.spec.js`.
""".

-behaviour(roadrunner_handler).

-export([handle/1]).

-spec handle(Req) -> {Response, Req} when
    Req :: roadrunner_req:request(),
    Response :: roadrunner_handler:response().
handle(Req) ->
    Body = iolist_to_binary(roadrunner_req:body(Req)),
    Name = proplists:get_value(~"name", uri_string:dissect_query(Body), ~""),
    %% The real Set-Cookie -- the reason fetch exists -- carrying the encrypted session.
    {CookieName, CookieValue, CookieOpts} = arizona_session:set_cookie(#{~"name" => Name}),
    %% Flip a request-local status attr, and tell the submitting view the new name so it
    %% repaints over its WS (no pubsub needed for the submitter).
    Resp0 = arizona_controller:reply_effects([
        arizona_js:set_attr(~"#status", ~"data-saved", ~"yes"),
        arizona_js:push_event(~"saved", #{~"name" => Name})
    ]),
    Resp1 = roadrunner_resp:set_cookie(Resp0, CookieName, CookieValue, CookieOpts),
    {Resp1, Req}.
