-module(arizona_session_controller).
-moduledoc """
**TEST FIXTURE.** The `{post, ...}` endpoint `arizona_session_view` posts to via
`arizona_js:fetch/2`. It rotates the encrypted `az_session` cookie (the thing the
WebSocket can't do) via `arizona_session:set_cookie/1`, and returns a request-local
effect plus a `push_event` carrying the new name so the submitting view re-renders live,
all without a reload. Drives `e2e/parallel/arizona_session.spec.js`.
""".

-export([handle/1]).

-spec handle(Req) -> {Reply, Req} when
    Req :: arizona_req:request(),
    Reply :: arizona_controller:reply().
handle(Req0) ->
    {ok, Body, Req1} = arizona_req:body(Req0),
    Name = proplists:get_value(~"name", uri_string:dissect_query(Body), ~""),
    %% The real Set-Cookie -- the reason fetch exists -- carrying the encrypted session.
    {CookieName, CookieValue, CookieOpts} = arizona_session:set_cookie(#{~"name" => Name}),
    %% Flip a request-local status attr, and tell the submitting view the new name so it
    %% repaints over its WS (no pubsub needed for the submitter).
    Reply = arizona_controller:reply_effects([
        arizona_js:set_attr(~"#status", ~"data-saved", ~"yes"),
        arizona_js:push_event(~"saved", #{~"name" => Name})
    ]),
    {Reply, arizona_req:put_resp_cookie(Req1, CookieName, CookieValue, CookieOpts)}.
