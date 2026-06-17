-module(arizona_controller).
-moduledoc """
Reply helpers for `{controller, ...}` routes -- plain HTTP handlers that drive the
page without a full reload.

A controller is a `roadrunner_handler` (`handle/1` returning `{Response, Req}`) that
the browser reaches via `arizona_js:fetch/2`. Because it is a real HTTP request, the
response may carry a `Set-Cookie` (HttpOnly honored) the WebSocket transport can't --
so it suits flows that rotate a session cookie while the page stays put.

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

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

-ignore_xref([reply_effects/1]).
-ignore_xref([reply_effects/2]).
-ignore_xref([reply_redirect/1]).

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
""".
-spec reply_redirect(Location) -> roadrunner_resp:buffered_response() when
    Location :: binary().
reply_redirect(Location) ->
    reply_effects([arizona_js:navigate(Location)]).
