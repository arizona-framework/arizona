-module(arizona_flash).
-moduledoc """
Signed, one-time flash cookie codec.

A flash is a small map of short-lived display messages carried across a single
redirect (the Post/Redirect/Get pattern). `arizona_req` owns the request/response
stash (`put_flash/3` sets, `flash/1` reads); the `arizona_middleware:fetch_flash/2`
step reads it into the `flash` binding.

This cookie is the mechanism for a **full-page HTTP redirect**: the flash rides a
dedicated `az_flash` cookie whose JSON payload is signed by `arizona_crypto`
(HMAC-SHA256 with the `arizona` `secret_key`), so a client cannot forge one, and is
consumed (cleared) on the next request that reads it. This module owns that domain:
JSON-encode/decode the payload and build the `Set-Cookie` tuples.

A flash across a **WebSocket SPA navigate** does *not* use this cookie. A live
navigate (a `{halt, redirect}`, or an `arizona_js:navigate`/`patch` `flash` opt) has
no `Set-Cookie` leg, so `arizona_socket` carries the flash in-process on the socket
to the follow-up navigate frame -- delivered exactly once, no cookie. Each navigation
kind thus arms exactly one mechanism: cookie for the full-page redirect, in-process
carry for the live navigate.

The cookie value is the flash JSON signed by `arizona_crypto:sign/3` under the
`arizona.flash` purpose with a `MAX_AGE`-second expiry baked into the signature
(`b64(payload) "." b64(signature)`, URL-safe base64 without padding). `decode/1`
returns `#{}` when the signature does not verify or has expired -- so a replayed
cookie kept past its `Max-Age` is rejected, not just the browser's soft expiry.
The purpose label domain-separates the flash from every other consumer of the
same `secret_key`: a session cookie replayed as a flash fails the HMAC outright
rather than relying on the JSON parse to reject it.

The `flash_secure` app env sets the `Secure` cookie flag (default `false`). **Set it to
`true` in production** (HTTPS); it defaults `false` because a `Secure` cookie is silently
dropped over plain HTTP, breaking local dev. (Mirrors `session_secure`.)
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([cookie_name/0]).
-export([encode/1]).
-export([decode/1]).
-export([set_cookie/1]).
-export([clear_cookie/0]).
-export([resp_cookie/2]).
-export([key/1]).
-export([secret/0]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

%% Public codec, exercised by arizona_req; no other internal callers by design.
-ignore_xref([cookie_name/0]).
-ignore_xref([encode/1]).
-ignore_xref([decode/1]).
-ignore_xref([set_cookie/1]).
-ignore_xref([clear_cookie/0]).
-ignore_xref([resp_cookie/2]).
-ignore_xref([key/1]).
-ignore_xref([secret/0]).

%% Flash cookie name and lifetime. The flash is consumed on the very next
%% request, so the max-age is only a safety net if that request never arrives.
-define(COOKIE, ~"az_flash").
-define(MAX_AGE, 60).
%% Crypto domain-separation label: binds a signed value to this consumer, so a
%% token minted for another purpose under the same secret never verifies here.
-define(PURPOSE, ~"arizona.flash").

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc "The flash cookie name.".
-spec cookie_name() -> binary().
cookie_name() ->
    ?COOKIE.

-doc """
Encodes a flash map into a signed cookie value with a baked-in `MAX_AGE`-second
expiry (via `arizona_crypto:sign/3`), matching the cookie's own `Max-Age`. The
signed expiry is defense-in-depth: even a client that ignores `Max-Age` and replays
a kept cookie past its lifetime gets `#{}` from `decode/1`. Errors if `secret_key`
is unset.
""".
-spec encode(Flash) -> binary() when
    Flash :: arizona_req:flash().
encode(Flash) ->
    arizona_crypto:sign(?PURPOSE, iolist_to_binary(json:encode(Flash)), #{ttl => ?MAX_AGE}).

-doc """
Decodes and verifies a cookie value into a flash map, returning `#{}` when the
signature does not match, has expired, or the value is malformed.
""".
-spec decode(Value) -> arizona_req:flash() when
    Value :: binary().
decode(Value) ->
    case arizona_crypto:verify(?PURPOSE, Value) of
        {ok, Payload} ->
            %% `verify` owns the tamper and cross-purpose checks; the remaining
            %% failure is a verified-but-non-JSON payload (this consumer's own
            %% past encoding, or a hand-rolled value signed under the flash
            %% purpose), which `decode/1`'s total `#{}`-on-anything contract
            %% must still swallow rather than crash on.
            try json:decode(Payload) of
                Flash when is_map(Flash) -> Flash;
                _ -> #{}
            catch
                _:_ -> #{}
            end;
        error ->
            #{}
    end.

-doc "The `Set-Cookie` tuple that carries `Flash` to the next request.".
-spec set_cookie(Flash) -> {binary(), binary(), arizona_req:resp_cookie_opts()} when
    Flash :: arizona_req:flash().
set_cookie(Flash) ->
    {?COOKIE, encode(Flash), cookie_opts(?MAX_AGE)}.

-doc "The `Set-Cookie` tuple that clears a consumed flash.".
-spec clear_cookie() -> {binary(), binary(), arizona_req:resp_cookie_opts()}.
clear_cookie() ->
    {?COOKIE, <<>>, cookie_opts(0)}.

-doc """
The flash `Set-Cookie` a response should carry, given the outgoing flash and
whether an incoming one was consumed: a freshly set flash (signed), a clearing
cookie when one was consumed and none was set, or `none`.
""".
-spec resp_cookie(FlashOut, Consumed) ->
    {binary(), binary(), arizona_req:resp_cookie_opts()} | none
when
    FlashOut :: arizona_req:flash(),
    Consumed :: boolean().
resp_cookie(FlashOut, _Consumed) when map_size(FlashOut) > 0 ->
    set_cookie(FlashOut);
resp_cookie(_FlashOut, true) ->
    clear_cookie();
resp_cookie(_FlashOut, false) ->
    none.

-doc "Normalizes a flash key (an atom is converted to a binary).".
-spec key(atom() | binary()) -> binary().
key(Key) when is_atom(Key) -> atom_to_binary(Key, utf8);
key(Key) when is_binary(Key) -> Key.

-doc """
Returns the configured flash signing secret. Delegates to `arizona_crypto:secret/0`
(the flash cookie is signed by `arizona_crypto`); erroring if it is unset or empty.
""".
-spec secret() -> binary().
secret() ->
    arizona_crypto:secret().

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

cookie_opts(MaxAge) ->
    #{
        http_only => true,
        same_site => lax,
        path => ~"/",
        max_age => MaxAge,
        secure => arizona_config:get_env(flash_secure, false)
    }.
