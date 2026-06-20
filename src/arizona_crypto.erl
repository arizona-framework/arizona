-module(arizona_crypto).
-moduledoc """
Signed- and encrypted-value primitives over arbitrary binaries.

**Signing (HMAC-SHA256)** -- `sign/1` wraps a binary into a tamper-evident,
URL-safe wire value `b64(tagged) "." b64(hmac)`; `sign/2` additionally bakes a
signed expiry (`#{ttl => Seconds}`) so the value rejects after a deadline.
`verify/1` constant-time-checks the HMAC, enforces any expiry, and returns the
original payload as `{ok, Payload}` (or `error` on a tampered, malformed, or
expired value). Use when the value may be read by the client but must not be
forged (e.g. `arizona_flash`).

**Encryption (AES-256-GCM)** -- `encrypt/1,2` wrap a binary into an authenticated,
URL-safe ciphertext (confidential *and* tamper-evident, with the same optional
`#{ttl => Seconds}` expiry); `decrypt/1` authenticates and returns `{ok, Payload}`
or `error`. Use when the value must also be unreadable by the client (e.g.
`arizona_session`).

The key is the `arizona` `secret_key` application env (a non-empty binary);
`secret/0` reads it and errors loudly if unset. The AES key is derived from it,
domain-separated from the raw secret used for HMAC, so the two uses never share key
material. This module owns no domain shape: callers layer their own encoding (e.g.
both `arizona_flash` and `arizona_session` carry JSON).
""".

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([sign/1]).
-export([sign/2]).
-export([verify/1]).
-export([encrypt/1]).
-export([encrypt/2]).
-export([decrypt/1]).
-export([secret/0]).
-export([format_error/2]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

%% Public primitive, exercised by arizona_flash (and future signed-token
%% callers); no other internal callers by design.
-ignore_xref([sign/1]).
-ignore_xref([sign/2]).
-ignore_xref([verify/1]).
-ignore_xref([encrypt/1]).
-ignore_xref([encrypt/2]).
-ignore_xref([decrypt/1]).
-ignore_xref([secret/0]).
-ignore_xref([format_error/2]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([sign_opts/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

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
Signs `Payload` into the URL-safe `b64(tagged) "." b64(hmac)` wire value, never
expiring. Errors if `secret_key` is unset (see `secret/0`).
""".
-spec sign(Payload) -> binary() when Payload :: binary().
sign(Payload) when is_binary(Payload) ->
    sign_envelope(<<?TAG_RAW, Payload/binary>>).

-doc """
Like `sign/1` but bakes a signed expiry: `#{ttl => Seconds}` makes the value
verify only until `Seconds` from now (inclusive). Errors if `secret_key` is unset.
""".
-spec sign(Payload, Opts) -> binary() when
    Payload :: binary(),
    Opts :: sign_opts().
sign(Payload, #{ttl := Secs}) when is_binary(Payload), is_integer(Secs), Secs >= 0 ->
    ExpiresAt = erlang:system_time(second) + Secs,
    sign_envelope(<<?TAG_TTL, ExpiresAt:64, Payload/binary>>).

-doc """
Verifies a signed value, returning `{ok, Payload}` when the HMAC matches and the
value is unexpired, and `error` when it is malformed, tampered, or expired. The
HMAC compare is constant-time. Errors if `secret_key` is unset.
""".
-spec verify(Signed) -> {ok, binary()} | error when
    Signed :: binary().
verify(Signed) when is_binary(Signed) ->
    do_verify(Signed, erlang:system_time(second)).

-doc """
Encrypts `Payload` into a URL-safe, authenticated (AES-256-GCM) wire value that
never expires. The result is confidential (the client cannot read it) and
tamper-evident. Errors if `secret_key` is unset (see `secret/0`).
""".
-spec encrypt(Payload) -> binary() when Payload :: binary().
encrypt(Payload) when is_binary(Payload) ->
    encrypt_envelope(<<?TAG_RAW, Payload/binary>>).

-doc """
Like `encrypt/1` but bakes an expiry (`#{ttl => Seconds}`) into the authenticated
plaintext; `decrypt/1` rejects the value after the deadline. Errors if `secret_key`
is unset.
""".
-spec encrypt(Payload, Opts) -> binary() when
    Payload :: binary(),
    Opts :: sign_opts().
encrypt(Payload, #{ttl := Secs}) when is_binary(Payload), is_integer(Secs), Secs >= 0 ->
    ExpiresAt = erlang:system_time(second) + Secs,
    encrypt_envelope(<<?TAG_TTL, ExpiresAt:64, Payload/binary>>).

-doc """
Decrypts and authenticates a value produced by `encrypt/1,2`, returning
`{ok, Payload}` when it is intact and unexpired, and `error` when it is malformed,
tampered, or expired. Errors if `secret_key` is unset.
""".
-spec decrypt(Encrypted) -> {ok, binary()} | error when
    Encrypted :: binary().
decrypt(Encrypted) when is_binary(Encrypted) ->
    do_decrypt(Encrypted, erlang:system_time(second)).

-doc """
Returns the configured signing secret from the `arizona` `secret_key` application
env, erroring if it is unset or empty.
""".
-spec secret() -> binary().
secret() ->
    case application:get_env(arizona, secret_key) of
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

sign_envelope(Tagged) ->
    Sig = crypto:mac(hmac, sha256, secret(), Tagged),
    <<(b64(Tagged))/binary, ".", (b64(Sig))/binary>>.

encrypt_envelope(Plaintext) ->
    IV = crypto:strong_rand_bytes(?IV_SIZE),
    {Cipher, Tag} = crypto:crypto_one_time_aead(
        aes_256_gcm, aead_key(), IV, Plaintext, <<>>, true
    ),
    b64(<<IV/binary, Tag/binary, Cipher/binary>>).

%% The clock is injected so the expiry boundary is deterministically testable.
do_decrypt(Encrypted, Now) ->
    try
        <<IV:?IV_SIZE/binary, Tag:?TAG_SIZE/binary, Cipher/binary>> = unb64(Encrypted),
        case crypto:crypto_one_time_aead(aes_256_gcm, aead_key(), IV, Cipher, <<>>, Tag, false) of
            error -> error;
            Plaintext when is_binary(Plaintext) -> unwrap(Plaintext, Now)
        end
    catch
        _:_ -> error
    end.

%% Derive a 32-byte AES-256 key from secret_key, domain-separated from the raw
%% secret used for HMAC signing so the encrypt and sign uses never share key material.
aead_key() ->
    crypto:hash(sha256, <<"arizona.aead.v1:", (secret())/binary>>).

%% The clock is injected so the expiry boundary is deterministically testable.
do_verify(Signed, Now) ->
    try
        [B64Tagged, B64Sig] = binary:split(Signed, ~"."),
        Tagged = unb64(B64Tagged),
        Expected = crypto:mac(hmac, sha256, secret(), Tagged),
        true = crypto:hash_equals(unb64(B64Sig), Expected),
        unwrap(Tagged, Now)
    catch
        _:_ -> error
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

ttl_expiry_boundary_test() ->
    application:set_env(arizona, secret_key, ~"eunit-secret-key"),
    %% A signed envelope with a fixed absolute expiry, verified against an
    %% injected clock -- no wall-clock, no sleep.
    Signed = sign_envelope(<<?TAG_TTL, 1000:64, "payload">>),
    ?assertEqual({ok, ~"payload"}, do_verify(Signed, 999)),
    ?assertEqual({ok, ~"payload"}, do_verify(Signed, 1000)),
    ?assertEqual(error, do_verify(Signed, 1001)),
    application:unset_env(arizona, secret_key).

raw_ignores_clock_test() ->
    application:set_env(arizona, secret_key, ~"eunit-secret-key"),
    Signed = sign(~"payload"),
    ?assertEqual({ok, ~"payload"}, do_verify(Signed, 1 bsl 40)),
    application:unset_env(arizona, secret_key).

encrypt_ttl_expiry_boundary_test() ->
    application:set_env(arizona, secret_key, ~"eunit-secret-key"),
    %% An encrypted envelope with a fixed absolute expiry, authenticated against an
    %% injected clock -- no wall-clock, no sleep.
    Blob = encrypt_envelope(<<?TAG_TTL, 1000:64, "payload">>),
    ?assertEqual({ok, ~"payload"}, do_decrypt(Blob, 999)),
    ?assertEqual({ok, ~"payload"}, do_decrypt(Blob, 1000)),
    ?assertEqual(error, do_decrypt(Blob, 1001)),
    application:unset_env(arizona, secret_key).

-endif.
