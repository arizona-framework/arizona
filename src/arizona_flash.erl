-module(arizona_flash).
-moduledoc """
Signed, one-time flash cookie codec.

A flash is a small map of short-lived display messages carried across a single
redirect (the Post/Redirect/Get pattern). It rides a dedicated cookie whose JSON
payload is signed by `arizona_crypto` (HMAC-SHA256 with the `arizona` `secret_key`
application env), so a client cannot forge one, and is consumed on the next
request that reads it.

This module owns the flash domain: JSON-encode/decode the payload and build the
`Set-Cookie` tuples. `arizona_req` integrates it into the request/response stash
(`put_flash/3`, `flash/1`, `read_flash/1`), and the `arizona_middleware:fetch_flash/2`
step reads it into the `flash` binding.

The cookie value is the flash JSON signed by `arizona_crypto:sign/1`
(`b64(payload) "." b64(signature)`, URL-safe base64 without padding);
`decode/1` returns `#{}` when the signature does not verify.
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

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc "The flash cookie name.".
-spec cookie_name() -> binary().
cookie_name() ->
    ?COOKIE.

-doc """
Encodes a flash map into a signed cookie value. Errors if `secret_key` is unset.
""".
-spec encode(Flash) -> binary() when
    Flash :: arizona_req:flash().
encode(Flash) ->
    arizona_crypto:sign(iolist_to_binary(json:encode(Flash))).

-doc """
Decodes and verifies a cookie value into a flash map, returning `#{}` when the
signature does not match or the value is malformed.
""".
-spec decode(Value) -> arizona_req:flash() when
    Value :: binary().
decode(Value) ->
    case arizona_crypto:verify(Value) of
        {ok, Payload} ->
            %% `verify` owns the tamper check; the remaining failure is a
            %% verified-but-non-JSON payload (possible now that arizona_crypto
            %% is shared -- another consumer could sign non-JSON under the same
            %% secret), which `decode/1`'s total `#{}`-on-anything contract
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
        secure => application:get_env(arizona, flash_secure, false)
    }.
