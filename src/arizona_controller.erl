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
    {ok, _Body, Req1} = roadrunner_req:read_body(Req),
    %% ...validate, rotate the session, derive NewSid + UserId...
    arizona_pubsub:broadcast({account, UserId}, account_updated),
    Resp0 = arizona_controller:reply_effects([
        arizona_js:set_attr(~"#error", ~"hidden", ~"")
    ]),
    Resp1 = roadrunner_resp:set_cookie(Resp0, ~"sid", NewSid, #{
        http_only => true, secure => true, same_site => strict, path => ~"/"
    }),
    {Resp1, Req1}.
```
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([reply_effects/1]).
-export([reply_redirect/1]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

-ignore_xref([reply_effects/1]).
-ignore_xref([reply_redirect/1]).

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc """
Builds a `200 application/json` response whose body is the `{"e": [...]}` effects wire
payload the `arizona_js:fetch/2` command applies on the page. `Effects` is a list of
`t:arizona_effect:cmd/0` -- the same effects `handle_event/3` returns.

The status is always 200: the `fetch` command applies the effects only on a 2xx
response and reserves non-2xx for its `on_error` path. To show inline validation,
return the error-rendering effects with 200 rather than a 4xx.
""".
-spec reply_effects(Effects) -> roadrunner_resp:buffered_response() when
    Effects :: [arizona_effect:cmd()].
reply_effects(Effects) ->
    roadrunner_resp:json(200, #{~"e" => [Cmd || {arizona_effect, Cmd} <:- Effects]}).

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
