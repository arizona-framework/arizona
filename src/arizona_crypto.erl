-module(arizona_crypto).
-moduledoc """
Signed- and encrypted-value primitives over arbitrary binaries.

Every function takes a `Purpose` -- a caller-owned context label that binds the
value to one consumer (see "Domain separation" below).

**Signing (HMAC-SHA256)** -- `sign/2` wraps a binary into a tamper-evident,
URL-safe wire value `b64(tagged) "." b64(hmac)`; `sign/3` additionally bakes a
signed expiry (`#{ttl => Seconds}`) so the value rejects after a deadline.
`verify/2` constant-time-checks the HMAC, enforces any expiry, and returns the
original payload as `{ok, Payload}` (or `error` on a tampered, malformed,
expired, or wrong-purpose value). Use when the value may be read by the client
but must not be forged (e.g. `arizona_flash`).

**Encryption (AES-256-GCM)** -- `encrypt/2,3` wrap a binary into an authenticated,
URL-safe ciphertext (confidential *and* tamper-evident, with the same optional
`#{ttl => Seconds}` expiry); `decrypt/2` authenticates and returns `{ok, Payload}`
or `error`. Use when the value must also be unreadable by the client (e.g.
`arizona_session`).

The key is the `arizona` `secret_key` application env (a non-empty binary);
`secret/0` reads it and errors loudly if unset. The AES key is derived from it,
domain-separated from the raw secret used for HMAC, so the two uses never share key
material. This module owns no domain shape: callers layer their own encoding (e.g.
both `arizona_flash` and `arizona_session` carry JSON).

**Domain separation.** All consumers share one `secret_key`, so without a context
label two logically distinct token types would be cryptographically interchangeable:
a value minted by one consumer would verify for another, and the only thing standing
between that and a privilege confusion is whatever the second consumer's payload
parser happens to reject. `Purpose` closes that by construction -- it is mixed into
the HMAC message (signing) and passed as the GCM additional authenticated data
(encryption), so a value minted for `~"arizona.flash"` fails `verify/2` under
`~"arizona.session_id"`. The label is never transmitted: it is context both sides
already know, so it costs no wire bytes and cannot be attacker-selected. Pick a
stable, namespaced literal per token type and never reuse one across shapes.

**Key rotation.** `sign`/`encrypt` always use the primary `secret_key`, but
`verify`/`decrypt` also accept any key listed in the optional `secret_key_previous`
application env (`[binary()]`, default `[]`). Rotate with zero downtime and no forced
re-issue: move the old `secret_key` into `secret_key_previous`, set a fresh
`secret_key`, then drop the old key once the grace window (your longest cookie TTL)
has passed. There is no key-version byte in the wire format -- verify/decrypt simply
try each candidate key in turn.
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([sign/2]).
-export([sign/3]).
-export([verify/2]).
-export([encrypt/2]).
-export([encrypt/3]).
-export([decrypt/2]).
-export([secret/0]).
-export([format_error/2]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

%% Public primitive, exercised by arizona_flash (and future signed-token
%% callers); no other internal callers by design.
-ignore_xref([sign/2]).
-ignore_xref([sign/3]).
-ignore_xref([verify/2]).
-ignore_xref([encrypt/2]).
-ignore_xref([encrypt/3]).
-ignore_xref([decrypt/2]).
-ignore_xref([secret/0]).
-ignore_xref([format_error/2]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([purpose/0]).
-export_type([sign_opts/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

%% A stable, caller-owned context label naming the token type (e.g.
%% `~"arizona.session_id"`). Authenticated but never transmitted.
-nominal purpose() :: binary().

-nominal sign_opts() :: #{ttl := non_neg_integer()}.

%% Envelope tag bytes -- signed/authenticated alongside the payload so a flipped tag
%% breaks the MAC (sign) or the GCM tag (encrypt).
-define(TAG_RAW, 0).
-define(TAG_TTL, 1).

%% AES-256-GCM nonce (96-bit, the standard GCM IV size) and authentication tag sizes.
-define(IV_SIZE, 12).
-define(TAG_SIZE, 16).

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc """
Signs `Payload` under `Purpose` into the URL-safe `b64(tagged) "." b64(hmac)` wire
value, never expiring. Only `verify/2` under the same `Purpose` accepts it. Errors
if `secret_key` is unset (see `secret/0`).
""".
-spec sign(Purpose, Payload) -> binary() when
    Purpose :: purpose(),
    Payload :: binary().
sign(Purpose, Payload) when is_binary(Purpose), is_binary(Payload) ->
    sign_envelope(Purpose, <<?TAG_RAW, Payload/binary>>).

-doc """
Like `sign/2` but bakes a signed expiry: `#{ttl => Seconds}` makes the value
verify only until `Seconds` from now (inclusive). Errors if `secret_key` is unset.
""".
-spec sign(Purpose, Payload, Opts) -> binary() when
    Purpose :: purpose(),
    Payload :: binary(),
    Opts :: sign_opts().
sign(Purpose, Payload, #{ttl := Secs}) when
    is_binary(Purpose), is_binary(Payload), is_integer(Secs), Secs >= 0
->
    ExpiresAt = erlang:system_time(second) + Secs,
    sign_envelope(Purpose, <<?TAG_TTL, ExpiresAt:64, Payload/binary>>).

-doc """
Verifies a value signed under `Purpose`, returning `{ok, Payload}` when the HMAC
matches and the value is unexpired, and `error` when it is malformed, tampered,
expired, or was signed under a different purpose. The HMAC compare is
constant-time. Errors if `secret_key` is unset.
""".
-spec verify(Purpose, Signed) -> {ok, binary()} | error when
    Purpose :: purpose(),
    Signed :: binary().
verify(Purpose, Signed) when is_binary(Purpose), is_binary(Signed) ->
    do_verify(Purpose, Signed, erlang:system_time(second)).

-doc """
Encrypts `Payload` under `Purpose` into a URL-safe, authenticated (AES-256-GCM)
wire value that never expires. The result is confidential (the client cannot read
it) and tamper-evident. Errors if `secret_key` is unset (see `secret/0`).
""".
-spec encrypt(Purpose, Payload) -> binary() when
    Purpose :: purpose(),
    Payload :: binary().
encrypt(Purpose, Payload) when is_binary(Purpose), is_binary(Payload) ->
    encrypt_envelope(Purpose, <<?TAG_RAW, Payload/binary>>).

-doc """
Like `encrypt/2` but bakes an expiry (`#{ttl => Seconds}`) into the authenticated
plaintext; `decrypt/2` rejects the value after the deadline. Errors if `secret_key`
is unset.
""".
-spec encrypt(Purpose, Payload, Opts) -> binary() when
    Purpose :: purpose(),
    Payload :: binary(),
    Opts :: sign_opts().
encrypt(Purpose, Payload, #{ttl := Secs}) when
    is_binary(Purpose), is_binary(Payload), is_integer(Secs), Secs >= 0
->
    ExpiresAt = erlang:system_time(second) + Secs,
    encrypt_envelope(Purpose, <<?TAG_TTL, ExpiresAt:64, Payload/binary>>).

-doc """
Decrypts and authenticates a value produced by `encrypt/2,3` under the same
`Purpose`, returning `{ok, Payload}` when it is intact and unexpired, and `error`
when it is malformed, tampered, expired, or was encrypted under a different
purpose. Errors if `secret_key` is unset.
""".
-spec decrypt(Purpose, Encrypted) -> {ok, binary()} | error when
    Purpose :: purpose(),
    Encrypted :: binary().
decrypt(Purpose, Encrypted) when is_binary(Purpose), is_binary(Encrypted) ->
    do_decrypt(Purpose, Encrypted, erlang:system_time(second)).

-doc """
Returns the configured signing secret from the `arizona` `secret_key` application
env, erroring if it is unset or empty.
""".
-spec secret() -> binary().
secret() ->
    case arizona_config:get_env(secret_key) of
        {ok, Secret} when is_binary(Secret), Secret =/= <<>> ->
            Secret;
        _ ->
            erlang:error(secret_key_not_configured, [], [
                {error_info, #{module => ?MODULE}}
            ])
    end.

%% --------------------------------------------------------------------
%% Format error
%% --------------------------------------------------------------------

-doc """
Formats `arizona_crypto` runtime errors into a human-readable message. Picked up
by `erl_error:format_exception/3` via the `error_info` annotation attached at the
raise site.
""".
-spec format_error(Reason, Stacktrace) -> ErrorInfo when
    Reason :: term(),
    Stacktrace :: [tuple()],
    ErrorInfo :: #{general := iolist()}.
format_error(secret_key_not_configured, _ST) ->
    #{
        general =>
            "signing requires a secret: set the arizona `secret_key` "
            "application env to a random non-empty binary"
    }.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

sign_envelope(Purpose, Tagged) ->
    Sig = crypto:mac(hmac, sha256, secret(), mac_message(Purpose, Tagged)),
    <<(b64(Tagged))/binary, ".", (b64(Sig))/binary>>.

encrypt_envelope(Purpose, Plaintext) ->
    IV = crypto:strong_rand_bytes(?IV_SIZE),
    {Cipher, Tag} = crypto:crypto_one_time_aead(
        aes_256_gcm, aead_key(secret()), IV, Plaintext, Purpose, true
    ),
    b64(<<IV/binary, Tag/binary, Cipher/binary>>).

%% The HMAC message: the purpose label length-prefixed ahead of the envelope, so
%% the two halves cannot be shifted into each other (a purpose ending in the
%% envelope's first bytes must not collide with a shorter one). The label is
%% authenticated but not transmitted -- both sides supply it from context.
mac_message(Purpose, Tagged) ->
    <<(byte_size(Purpose)):32, Purpose/binary, Tagged/binary>>.

%% The clock is injected so the expiry boundary is deterministically testable.
do_decrypt(Purpose, Encrypted, Now) ->
    try
        <<IV:?IV_SIZE/binary, Tag:?TAG_SIZE/binary, Cipher/binary>> = unb64(Encrypted),
        case decrypt_candidates(candidate_keys(), Purpose, IV, Cipher, Tag) of
            error -> error;
            Plaintext -> unwrap(Plaintext, Now)
        end
    catch
        _:_ -> error
    end.

%% Try each candidate key (primary first, then rotated-out previous keys) until one
%% authenticates the ciphertext under `Purpose` (the GCM additional authenticated
%% data); `error` when none do.
decrypt_candidates([], _Purpose, _IV, _Cipher, _Tag) ->
    error;
decrypt_candidates([Key | Rest], Purpose, IV, Cipher, Tag) ->
    case crypto:crypto_one_time_aead(aes_256_gcm, aead_key(Key), IV, Cipher, Purpose, Tag, false) of
        error -> decrypt_candidates(Rest, Purpose, IV, Cipher, Tag);
        Plaintext when is_binary(Plaintext) -> Plaintext
    end.

%% Derive a 32-byte AES-256 key from a signing key, domain-separated from its raw
%% form (used for HMAC) so the encrypt and sign uses never share key material.
aead_key(Key) ->
    crypto:hash(sha256, <<"arizona.aead.v1:", Key/binary>>).

%% The keys to try when verifying/decrypting: the primary (`secret_key`) first, then
%% any `secret_key_previous` rotated-out keys. Signing/encryption always use the
%% primary. Rotate by moving the old primary into `secret_key_previous` and setting a
%% new `secret_key`: existing values keep verifying under the old key for the grace
%% window -- no forced re-issue, no wire-format change.
candidate_keys() ->
    [secret() | arizona_config:get_env(secret_key_previous, [])].

%% The clock is injected so the expiry boundary is deterministically testable.
do_verify(Purpose, Signed, Now) ->
    try
        [B64Tagged, B64Sig] = binary:split(Signed, ~"."),
        Tagged = unb64(B64Tagged),
        Sig = unb64(B64Sig),
        case verify_candidates(candidate_keys(), mac_message(Purpose, Tagged), Sig) of
            true -> unwrap(Tagged, Now);
            false -> error
        end
    catch
        _:_ -> error
    end.

%% Constant-time HMAC compare against each candidate key (primary first, then
%% rotated-out previous keys); `true` when any matches.
verify_candidates([], _Message, _Sig) ->
    false;
verify_candidates([Key | Rest], Message, Sig) ->
    case crypto:hash_equals(Sig, crypto:mac(hmac, sha256, Key, Message)) of
        true -> true;
        false -> verify_candidates(Rest, Message, Sig)
    end.

unwrap(<<?TAG_RAW, Payload/binary>>, _Now) ->
    {ok, Payload};
unwrap(<<?TAG_TTL, ExpiresAt:64, Payload/binary>>, Now) when Now =< ExpiresAt ->
    {ok, Payload};
unwrap(<<?TAG_TTL, _ExpiresAt:64, _Payload/binary>>, _Now) ->
    error.

b64(Bin) ->
    base64:encode(Bin, #{mode => urlsafe, padding => false}).

unb64(Bin) ->
    base64:decode(Bin, #{mode => urlsafe, padding => false}).

%% --------------------------------------------------------------------
%% EUnit tests
%% --------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(EUNIT_SECRET, ~"eunit-secret-key-0123456789abcdef").
-define(EUNIT_PURPOSE, ~"arizona.eunit").

ttl_expiry_boundary_test() ->
    application:set_env(arizona, secret_key, ?EUNIT_SECRET),
    %% A signed envelope with a fixed absolute expiry, verified against an
    %% injected clock -- no wall-clock, no sleep.
    Signed = sign_envelope(?EUNIT_PURPOSE, <<?TAG_TTL, 1000:64, "payload">>),
    ?assertEqual({ok, ~"payload"}, do_verify(?EUNIT_PURPOSE, Signed, 999)),
    ?assertEqual({ok, ~"payload"}, do_verify(?EUNIT_PURPOSE, Signed, 1000)),
    ?assertEqual(error, do_verify(?EUNIT_PURPOSE, Signed, 1001)),
    application:unset_env(arizona, secret_key).

raw_ignores_clock_test() ->
    application:set_env(arizona, secret_key, ?EUNIT_SECRET),
    Signed = sign(?EUNIT_PURPOSE, ~"payload"),
    ?assertEqual({ok, ~"payload"}, do_verify(?EUNIT_PURPOSE, Signed, 1 bsl 40)),
    application:unset_env(arizona, secret_key).

encrypt_ttl_expiry_boundary_test() ->
    application:set_env(arizona, secret_key, ?EUNIT_SECRET),
    %% An encrypted envelope with a fixed absolute expiry, authenticated against an
    %% injected clock -- no wall-clock, no sleep.
    Blob = encrypt_envelope(?EUNIT_PURPOSE, <<?TAG_TTL, 1000:64, "payload">>),
    ?assertEqual({ok, ~"payload"}, do_decrypt(?EUNIT_PURPOSE, Blob, 999)),
    ?assertEqual({ok, ~"payload"}, do_decrypt(?EUNIT_PURPOSE, Blob, 1000)),
    ?assertEqual(error, do_decrypt(?EUNIT_PURPOSE, Blob, 1001)),
    application:unset_env(arizona, secret_key).

-endif.
