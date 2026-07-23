-module(arizona_session).
-moduledoc """
Encrypted, durable session cookie codec.

A session is a small map of state carried across requests in a dedicated cookie.
The cookie is encrypted (AES-256-GCM, via `arizona_crypto`) with the `arizona`
`secret_key` application env and stamped with an absolute expiry, so a client can
neither read, forge, nor outlive it. Unlike `arizona_flash`, a session is
**durable**: a plain read does not consume or re-emit it. The cookie is rewritten
only when the app writes the session (`arizona_req:put_session/3`,
`delete_session/2`) and cleared only on `arizona_req:clear_session/1` (logout).

This module owns the wire format: encode/decode the payload and build the
`Set-Cookie` tuples. `arizona_req` integrates it into the request/response stash
(`put_session/3`, `session/1`, `read_session/1`, ...), and the
`arizona_middleware:fetch_session/2` step reads it into the `session` binding.

The cookie is encrypted, so its contents are hidden from the client -- but it still
lives on the client and a cookie store cannot be revoked before its expiry. Keep
sessions small (an id plus light state, well under the ~4KB cookie limit); a
server-side store is the place for large, long-lived, or instantly-revocable state.

## Configuration (`arizona` app env)

- `secret_key` -- required (see `arizona_crypto`); the encryption key.
- `session_max_age` -- cookie lifetime in seconds (default 7 days).
- `session_secure` -- `Secure` cookie flag (default `false`). **Set this to `true` in
  production** so the session cookie is only sent over HTTPS. It defaults to `false`
  because a `Secure` cookie is *silently dropped* over plain HTTP -- breaking local dev
  with no error -- and a deployment behind a TLS-terminating proxy can't be auto-detected.
  Treat enabling it as a deploy-checklist item (mirroring `flash_secure`).
- `session_max_bytes` -- max encoded-cookie size (default 4096); `encode/1` errors with
  `{session_too_large, Size, Limit}` past it rather than letting the browser drop it.
- `session_store` -- opt into a server-side store (an `arizona_session_store` module, e.g.
  `arizona_session_store_ets`); the cookie then carries only a signed opaque id (`encode_id/1`,
  `decode_id/1`, `set_cookie_id/1`) and the map lives in the store, enabling revocation and
  large state. Unset (the default) keeps the encrypted cookie store. The mode switch is in
  `arizona_req`.
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
-export([max_age/0]).
-export([new_id/0]).
-export([encode_id/1]).
-export([decode_id/1]).
-export([set_cookie_id/1]).
-export([format_error/2]).

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
-ignore_xref([max_age/0]).
-ignore_xref([new_id/0]).
-ignore_xref([encode_id/1]).
-ignore_xref([decode_id/1]).
-ignore_xref([set_cookie_id/1]).
-ignore_xref([format_error/2]).

%% Session cookie name and default lifetime (7 days, in seconds). The session is
%% durable across requests, so the lifetime is the real expiry, not a safety net
%% (cf. arizona_flash's 60s). The encrypted payload carries the same TTL, so the
%% server rejects an expired cookie even if the browser replays it.
-define(COOKIE, ~"az_session").
-define(DEFAULT_MAX_AGE, 604800).
%% Crypto domain-separation labels. The cookie-store payload and the store-mode id
%% are distinct token types over the same `secret_key`, so each names its own
%% purpose: neither verifies as the other, nor as another consumer's value
%% (`arizona_flash`).
-define(PURPOSE, ~"arizona.session").
-define(PURPOSE_ID, ~"arizona.session_id").
%% Browsers cap a single cookie at ~4KB (name + value + attributes); an oversized
%% `Set-Cookie` is silently dropped, losing the session with no error. Guard the
%% encoded value against that so the failure is loud at write time instead.
-define(DEFAULT_MAX_BYTES, 4096).

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc "The session cookie name.".
-spec cookie_name() -> binary().
cookie_name() ->
    ?COOKIE.

-doc """
Encodes a session map into an encrypted cookie value with a baked-in absolute
expiry (`max_age/0` seconds from now). Errors if `secret_key` is unset, or with
`{session_too_large, Size, Limit}` if the encoded value exceeds `session_max_bytes`
(default 4096) -- a loud failure at write time instead of a cookie the browser
silently drops.
""".
-spec encode(Session) -> binary() when
    Session :: arizona_req:session().
encode(Session) ->
    Payload = iolist_to_binary(json:encode(Session)),
    Cookie = arizona_crypto:encrypt(?PURPOSE, Payload, #{ttl => max_age()}),
    Limit = max_bytes(),
    case byte_size(Cookie) of
        Size when Size > Limit ->
            erlang:error({session_too_large, Size, Limit}, [Session], [
                {error_info, #{module => ?MODULE}}
            ]);
        _ ->
            Cookie
    end.

-doc """
Decodes and authenticates a cookie value into a session map, returning `#{}` when
the value is tampered, malformed, or its baked-in expiry has passed.
""".
-spec decode(Value) -> arizona_req:session() when
    Value :: binary().
decode(Value) ->
    case arizona_crypto:decrypt(?PURPOSE, Value) of
        {ok, Payload} ->
            %% `decrypt` owns the tamper and cross-purpose checks; the remaining
            %% failure is a decrypted-but-non-JSON payload (this consumer's own
            %% past encoding, or a hand-rolled value encrypted under the session
            %% purpose), which `decode/1`'s total `#{}`-on-anything contract
            %% must still swallow rather than crash on.
            try json:decode(Payload) of
                Session when is_map(Session) -> Session;
                _ -> #{}
            catch
                _:_ -> #{}
            end;
        error ->
            #{}
    end.

-doc "The `Set-Cookie` tuple that carries `Session` to the next request.".
-spec set_cookie(Session) -> {binary(), binary(), arizona_req:resp_cookie_opts()} when
    Session :: arizona_req:session().
set_cookie(Session) ->
    {?COOKIE, encode(Session), cookie_opts(max_age())}.

-doc "The `Set-Cookie` tuple that clears the session (logout).".
-spec clear_cookie() -> {binary(), binary(), arizona_req:resp_cookie_opts()}.
clear_cookie() ->
    {?COOKIE, <<>>, cookie_opts(0)}.

-doc """
The session `Set-Cookie` a response should carry, given the desired next-state
session and whether it was written this request (`Dirty`). Durable semantics: a
clean request (untouched) emits `none` so the client keeps its cookie; a dirty
non-empty session is freshly encrypted and set; a dirty empty session (logout)
clears the cookie.
""".
-spec resp_cookie(SessionOut, Dirty) ->
    {binary(), binary(), arizona_req:resp_cookie_opts()} | none
when
    SessionOut :: arizona_req:session(),
    Dirty :: boolean().
resp_cookie(_SessionOut, false) ->
    none;
resp_cookie(SessionOut, true) when map_size(SessionOut) > 0 ->
    set_cookie(SessionOut);
resp_cookie(_SessionOut, true) ->
    clear_cookie().

-doc "Normalizes a session key (an atom is converted to a binary).".
-spec key(atom() | binary()) -> binary().
key(Key) when is_atom(Key) -> atom_to_binary(Key, utf8);
key(Key) when is_binary(Key) -> Key.

-doc """
The configured session lifetime in seconds (`session_max_age`, default 7 days). The
cookie max-age, the encrypted-payload TTL, and a server-side store entry's TTL are all
this value, so browser and server expiry agree.
""".
-spec max_age() -> non_neg_integer().
max_age() ->
    arizona_config:get_env(session_max_age, ?DEFAULT_MAX_AGE).

%% --- Store mode: the cookie carries a signed opaque id, the data lives in a store ---

-doc "A fresh opaque session id (128 random bits, lowercase hex).".
-spec new_id() -> binary().
new_id() ->
    binary:encode_hex(crypto:strong_rand_bytes(16), lowercase).

-doc "Signs a session id into the cookie value (the id is not secret, just unforgeable).".
-spec encode_id(Id) -> binary() when Id :: binary().
encode_id(Id) ->
    arizona_crypto:sign(?PURPOSE_ID, Id).

-doc """
Recovers a session id from a cookie value, returning `error` if tampered,
malformed, or signed under another purpose.
""".
-spec decode_id(Value) -> {ok, binary()} | error when Value :: binary().
decode_id(Value) ->
    arizona_crypto:verify(?PURPOSE_ID, Value).

-doc "The `Set-Cookie` tuple carrying the signed session `Id` (store mode).".
-spec set_cookie_id(Id) -> {binary(), binary(), arizona_req:resp_cookie_opts()} when
    Id :: binary().
set_cookie_id(Id) ->
    {?COOKIE, encode_id(Id), cookie_opts(max_age())}.

%% --------------------------------------------------------------------
%% Format error
%% --------------------------------------------------------------------

-doc """
Formats `arizona_session` runtime errors into a human-readable message. Picked up
by `erl_error:format_exception/3` via the `error_info` annotation at the raise site.
""".
-spec format_error(Reason, Stacktrace) -> ErrorInfo when
    Reason :: term(),
    Stacktrace :: [tuple()],
    ErrorInfo :: #{general := iolist()}.
format_error({session_too_large, Size, Limit}, _ST) ->
    #{
        general =>
            io_lib:format(
                "session cookie is ~b bytes, over the ~b-byte limit; browsers "
                "silently drop oversized cookies. Store less in the session (an id, "
                "not a blob), or raise the `session_max_bytes` application env.",
                [Size, Limit]
            )
    }.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% Maximum encoded-cookie size in bytes (default 4096, the ~4KB browser cap).
max_bytes() ->
    arizona_config:get_env(session_max_bytes, ?DEFAULT_MAX_BYTES).

cookie_opts(MaxAge) ->
    #{
        http_only => true,
        same_site => lax,
        path => ~"/",
        max_age => MaxAge,
        secure => arizona_config:get_env(session_secure, false)
    }.
