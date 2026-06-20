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

%% Session cookie name and default lifetime (7 days, in seconds). The session is
%% durable across requests, so the lifetime is the real expiry, not a safety net
%% (cf. arizona_flash's 60s). The encrypted payload carries the same TTL, so the
%% server rejects an expired cookie even if the browser replays it.
-define(COOKIE, ~"az_session").
-define(DEFAULT_MAX_AGE, 604800).

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc "The session cookie name.".
-spec cookie_name() -> binary().
cookie_name() ->
    ?COOKIE.

-doc """
Encodes a session map into an encrypted cookie value with a baked-in absolute
expiry (`max_age/0` seconds from now). Errors if `secret_key` is unset.
""".
-spec encode(Session) -> binary() when
    Session :: arizona_req:session().
encode(Session) ->
    Payload = iolist_to_binary(json:encode(Session)),
    arizona_crypto:encrypt(Payload, #{ttl => max_age()}).

-doc """
Decodes and authenticates a cookie value into a session map, returning `#{}` when
the value is tampered, malformed, or its baked-in expiry has passed.
""".
-spec decode(Value) -> arizona_req:session() when
    Value :: binary().
decode(Value) ->
    case arizona_crypto:decrypt(Value) of
        {ok, Payload} ->
            case json:decode(Payload) of
                Session when is_map(Session) -> Session;
                _ -> #{}
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

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% Configured session lifetime in seconds (default 7 days). The cookie max-age and
%% the encrypted payload TTL are both this value, so browser expiry and server-side
%% expiry agree.
max_age() ->
    application:get_env(arizona, session_max_age, ?DEFAULT_MAX_AGE).

cookie_opts(MaxAge) ->
    #{
        http_only => true,
        same_site => lax,
        path => ~"/",
        max_age => MaxAge,
        secure => application:get_env(arizona, session_secure, false)
    }.
