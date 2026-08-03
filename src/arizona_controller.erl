-module(arizona_controller).
-moduledoc """
Reply helpers for controller routes -- plain HTTP handlers that drive the
page without a full reload.

A controller route (a verb tag like `{post, ...}`, or `{match, ...}`) dispatches to an
action function (`Handler:Action/1`, the `action` option defaulting to `handle`,
returning `{Response, Req}`) that the browser reaches via `arizona_js:fetch/2`. Because
it is a real HTTP request, the response may carry a `Set-Cookie` (HttpOnly honored) the
WebSocket transport can't -- so it suits flows that rotate a session cookie while the
page stays put.

These helpers build the response body the `fetch` command expects: the same
`{"e": [...]}` effects wire payload the WebSocket sends. Layer cookies/headers/status
on the returned response with the `roadrunner_resp` builders (e.g.
`roadrunner_resp:set_cookie/4`).

To re-render the live view after the request, broadcast over `arizona_pubsub` -- the
connected view re-renders and patches through the WebSocket as usual. The effects
returned here are for request-local UI (inline error/success) or a `navigate`.

## Example

```erlang
-behaviour(roadrunner_handler).
-export([handle/1]).

handle(Req) ->
    Body = roadrunner_req:body(Req),
    %% ...parse Body, validate, rotate the session, derive NewSid + UserId...
    %% Show server-computed content (a validation message, success state) by
    %% broadcasting it to a topic the view subscribed to in mount/1 -- the view
    %% renders it from its own state. Scope the topic by user/session so it reaches
    %% the right view; the response effects are for request-local imperative UI only.
    arizona_pubsub:broadcast({account, UserId}, {account_updated, Body}),
    Resp0 = arizona_controller:reply_effects([
        arizona_js:set_attr(~"#error", ~"hidden", ~"")
    ]),
    Resp1 = roadrunner_resp:set_cookie(Resp0, ~"sid", NewSid, #{
        http_only => true, secure => true, same_site => strict, path => ~"/"
    }),
    {Resp1, Req}.
```

`roadrunner_req:body/1` is the buffered body (testable from a plain request map); use it
rather than the streaming `read_body/1` so a controller stays unit-testable.
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([reply_effects/1]).
-export([reply_effects/2]).
-export([reply_redirect/1]).
-export([req/1]).
-export([put_req/2]).
-export([bindings/1]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

-ignore_xref([reply_effects/1]).
-ignore_xref([reply_effects/2]).
-ignore_xref([reply_redirect/1]).
-ignore_xref([req/1]).
-ignore_xref([put_req/2]).
-ignore_xref([bindings/1]).

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc """
Builds a `200 application/json` response whose body is the `{"e": [...]}` effects wire
payload the `arizona_js:fetch/2` command applies on the page (the same effects
`handle_event/3` returns). 200 is the **success** leg -- a fetch form with `az-form-reset`
clears only on a 2xx. For an error leg that keeps the typed fields, use `reply_effects/2`
with a non-2xx status (the effects still apply).
""".
-spec reply_effects(Effects) -> roadrunner_resp:buffered_response() when
    Effects :: [arizona_effect:cmd()].
reply_effects(Effects) ->
    reply_effects(200, Effects).

-doc """
Like `reply_effects/1` but with an explicit HTTP status. The `fetch` command applies the
effects on **any** status, so a controller can return a real `422` for a validation error
-- keeping the form's typed fields (a fetch form only resets on a 2xx) while still
rendering the error, e.g.
`reply_effects(422, [arizona_js:push_event(~"invalid", #{~"field" => ~"password"})])`.
""".
-spec reply_effects(Status, Effects) -> roadrunner_resp:buffered_response() when
    Status :: roadrunner_http:status(),
    Effects :: [arizona_effect:cmd()].
reply_effects(Status, Effects) ->
    roadrunner_resp:json(Status, #{~"e" => [Cmd || {arizona_effect, Cmd} <:- Effects]}).

-doc """
Builds a response that sends the client to `Location` via a SPA navigation. Sugar for
`reply_effects([arizona_js:navigate(Location)])`: a fetch-followed HTTP 3xx can't drive
a SPA navigation (its `Location` is unreadable), so a redirect is delivered as a
`navigate` effect instead.

This is a SPA navigation over the existing WebSocket, so it suits same-identity
redirects. After an **identity change** (login/logout) use `arizona_js:reload/0`
instead, so the socket re-handshakes with the new session.
""".
-spec reply_redirect(Location) -> roadrunner_resp:buffered_response() when
    Location :: binary().
reply_redirect(Location) ->
    reply_effects([arizona_js:navigate(Location)]).

-doc """
The post-middleware `arizona_req:request()` for this controller request,
recovered from the roadrunner request the action received.

This is the request the route's middleware pipeline produced, so middleware
effects are visible to the action -- e.g. after an
`{arizona_middleware, fetch_session}` step the action reads the session with
`arizona_req:get_session/2,3` or `arizona_req:session/1`. Mirrors how the
route's `state` rides the request to `roadrunner_req:state/1`. Only set by the
controller dispatcher, so calling it on a request that did not come through a
controller route crashes.

Reading is the whole contract here. **Writes need the round trip**: read ->
mutate -> `put_req/2` -> return that roadrunner request from the action. A
mutated `arizona_req:request()` is an ordinary immutable value, so an action
that keeps it in a local and returns the roadrunner request unchanged has its
session/flash/cookie writes dropped -- the dispatcher flushes whatever the
returned request carries, which is then still the pre-action copy.

```erlang
handle(Req) ->
    ArzReq = arizona_req:clear_session(arizona_controller:req(Req)),
    {arizona_controller:reply_effects([]), arizona_controller:put_req(ArzReq, Req)}.
```

Reading the body is the exception -- use `roadrunner_req:body/1` on the
roadrunner request, not `arizona_req:body/1` on this one, so the advanced body
reader rides back out to the transport.
""".
-spec req(RoadrunnerReq) -> arizona_req:request() when
    RoadrunnerReq :: roadrunner_req:request().
req(RoadrunnerReq) ->
    #{{arizona, req} := ArzReq} = roadrunner_req:private(RoadrunnerReq),
    ArzReq.

-doc """
Threads a mutated `arizona_req:request()` back onto the roadrunner request, so
the dispatcher flushes **this** request rather than the pre-action one.

The writer half of `req/1`. Return the request it gives you from the action and
the write is serialized and committed at flush: `arizona_req:put_session/3` /
`clear_session/1` reach the browser as a `Set-Cookie` and (in store mode) the
server-side store, `put_flash/3` reaches the next request, and
`put_resp_header/3` / `put_resp_cookie/4` land on the response. A logout action
that clears the session and threads it back revokes for real.

Only meaningful on a request that came through a controller route (the same
stash `req/1` reads); an action that never calls this keeps the read-only path,
where the pipeline's own request is flushed.
""".
-spec put_req(ArzReq, RoadrunnerReq) -> RoadrunnerReq when
    ArzReq :: arizona_req:request(),
    RoadrunnerReq :: roadrunner_req:request().
put_req(ArzReq, RoadrunnerReq) ->
    roadrunner_req:put_private({arizona, req}, ArzReq, RoadrunnerReq).

-doc """
The bindings the route's middleware pipeline produced -- e.g. `session` from
`fetch_session`, `flash` from `fetch_flash`, or the keys an
`arizona_middleware:extract/1` step pulled from the request -- the same map a
live route hands its view as mount bindings. Like `req/1`, only set by the
controller dispatcher.
""".
-spec bindings(RoadrunnerReq) -> az:bindings() when
    RoadrunnerReq :: roadrunner_req:request().
bindings(RoadrunnerReq) ->
    #{{arizona, bindings} := Bindings} = roadrunner_req:private(RoadrunnerReq),
    Bindings.
