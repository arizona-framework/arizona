-module(arizona_user_agent).
-moduledoc """
Pure, best-effort `User-Agent` classification helpers.

Each function takes the raw `User-Agent` string (from `arizona_req:user_agent/1`)
and answers one question, so a caller composes only the checks it needs instead
of building a map of fields it will not use.

The checks are heuristic substring matches -- good enough to pick a render target
(browser vs native app) but not authoritative. When the coarse answer is not
enough, match the raw string yourself: you know your own native client's UA.

```erlang
{UA, _Req1} = arizona_req:user_agent(Req),
case arizona_user_agent:browser(UA) of
    true -> ?html(...);
    false -> ?native(...)
end.
```
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([browser/1]).
-export([os/1]).
-export([mobile/1]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

-ignore_xref([browser/1]).
-ignore_xref([os/1]).
-ignore_xref([mobile/1]).

%% --------------------------------------------------------------------
%% API functions
%% --------------------------------------------------------------------

-doc """
Whether the `User-Agent` looks like a web browser or webview.

True when the UA carries `Mozilla` -- every mainstream browser and webview does.
A native app's HTTP stack typically does not, which is the signal to render a
native tree instead of HTML.
""".
-spec browser(UserAgent) -> boolean() when UserAgent :: binary().
browser(UserAgent) ->
    contains(UserAgent, ~"Mozilla").

-doc """
Coarse operating-system guess from a `User-Agent`, or `other` when nothing
matches.

Order matters where UAs overlap, so the more specific substrings are checked
first: Apple TV / Apple Watch UAs also carry `Mac OS X`; webOS / Tizen / Android
UAs carry `Linux`; iOS UAs carry `Mac OS X`. The smart-TV ones (`tvos`/`webos`/
`tizen`) come from real TV-browser UAs and are reliable. `watchos` and native
apps rely on a descriptive UA -- most native HTTP stacks send a generic UA that
names no platform (→ `other`), so when you control the client prefer an explicit
signal (a query param/header) over this heuristic.
""".
-spec os(UserAgent) ->
    watchos | tvos | webos | tizen | ios | android | windows | macos | linux | other
when
    UserAgent :: binary().
os(UserAgent) ->
    Rules = [
        {~"Apple Watch", watchos},
        {~"watchOS", watchos},
        {~"Apple TV", tvos},
        {~"tvOS", tvos},
        {~"Web0S", webos},
        {~"webOS", webos},
        {~"Tizen", tizen},
        {~"Android", android},
        {~"iPhone", ios},
        {~"iPad", ios},
        {~"iPod", ios},
        {~"Windows", windows},
        {~"Mac OS X", macos},
        {~"Macintosh", macos},
        {~"Linux", linux}
    ],
    case lists:search(fun({Sub, _Os}) -> contains(UserAgent, Sub) end, Rules) of
        {value, {_Sub, Os}} -> Os;
        false -> other
    end.

-doc """
Whether the `User-Agent` looks like a mobile device.

Best effort: true when the UA carries `Mobi` (most mobile browsers) or `Android`.
Some tablets -- recent iPad Safari, for instance -- report a desktop UA and read
as `false`.
""".
-spec mobile(UserAgent) -> boolean() when UserAgent :: binary().
mobile(UserAgent) ->
    contains(UserAgent, ~"Mobi") orelse contains(UserAgent, ~"Android").

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

contains(Bin, Sub) ->
    binary:match(Bin, Sub) =/= nomatch.
