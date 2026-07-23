-module(arizona_crypto_SUITE).
-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).

-export([sign_verify_round_trips/1]).
-export([sign_verify_round_trips_empty/1]).
-export([sign_verify_round_trips_payload_with_dot/1]).
-export([sign_ttl_verifies_before_expiry/1]).
-export([verify_rejects_tampered_payload/1]).
-export([verify_rejects_tampered_signature/1]).
-export([verify_rejects_malformed_no_dot/1]).
-export([verify_rejects_extra_dots/1]).
-export([verify_rejects_wrong_secret/1]).
-export([verify_rejects_other_purpose/1]).
-export([decrypt_rejects_other_purpose/1]).
-export([encrypt_decrypt_round_trips/1]).
-export([encrypt_decrypt_round_trips_empty/1]).
-export([encrypt_is_confidential/1]).
-export([decrypt_rejects_tampered_ciphertext/1]).
-export([decrypt_rejects_truncated/1]).
-export([decrypt_rejects_wrong_secret/1]).
-export([encrypt_ttl_verifies_before_expiry/1]).
-export([verify_accepts_previous_key/1]).
-export([decrypt_accepts_previous_key/1]).
-export([verify_rejects_fully_rotated_key/1]).
-export([decrypt_rejects_fully_rotated_key/1]).
-export([secret_returns_configured/1]).
-export([secret_errors_when_unset/1]).
-export([sign_errors_when_secret_unset/1]).
-export([encrypt_errors_when_secret_unset/1]).
-export([format_error_renders_secret_message/1]).

-define(SECRET, ~"test-secret-key-0123456789abcdef").
-define(PURPOSE, ~"arizona.test").

%% Sequential (not parallel): the cases share the `secret_key` application env,
%% and the secret-toggling cases mutate it.
all() ->
    [
        sign_verify_round_trips,
        sign_verify_round_trips_empty,
        sign_verify_round_trips_payload_with_dot,
        sign_ttl_verifies_before_expiry,
        verify_rejects_tampered_payload,
        verify_rejects_tampered_signature,
        verify_rejects_malformed_no_dot,
        verify_rejects_extra_dots,
        verify_rejects_wrong_secret,
        verify_rejects_other_purpose,
        decrypt_rejects_other_purpose,
        encrypt_decrypt_round_trips,
        encrypt_decrypt_round_trips_empty,
        encrypt_is_confidential,
        decrypt_rejects_tampered_ciphertext,
        decrypt_rejects_truncated,
        decrypt_rejects_wrong_secret,
        encrypt_ttl_verifies_before_expiry,
        verify_accepts_previous_key,
        decrypt_accepts_previous_key,
        verify_rejects_fully_rotated_key,
        decrypt_rejects_fully_rotated_key,
        secret_returns_configured,
        secret_errors_when_unset,
        sign_errors_when_secret_unset,
        encrypt_errors_when_secret_unset,
        format_error_renders_secret_message
    ].

init_per_suite(Config) ->
    application:set_env(arizona, secret_key, ?SECRET),
    Config.

end_per_suite(_Config) ->
    application:unset_env(arizona, secret_key),
    ok.

sign_verify_round_trips(Config) when is_list(Config) ->
    Payload = ~"hello, world",
    ?assertMatch(
        {ok, Payload}, arizona_crypto:verify(?PURPOSE, arizona_crypto:sign(?PURPOSE, Payload))
    ).

sign_verify_round_trips_empty(Config) when is_list(Config) ->
    ?assertMatch({ok, <<>>}, arizona_crypto:verify(?PURPOSE, arizona_crypto:sign(?PURPOSE, <<>>))).

sign_verify_round_trips_payload_with_dot(Config) when is_list(Config) ->
    %% A payload full of `.` round-trips: the split delimiter is the single `.`
    %% the wire format inserts, and base64 halves never contain `.`.
    Payload = ~"a.b.c.d",
    ?assertMatch(
        {ok, Payload}, arizona_crypto:verify(?PURPOSE, arizona_crypto:sign(?PURPOSE, Payload))
    ).

sign_ttl_verifies_before_expiry(Config) when is_list(Config) ->
    %% A far-future TTL verifies now without sleeping; the expiry boundary itself
    %% is covered deterministically by the inline EUnit tests in arizona_crypto.
    Payload = ~"fresh",
    ?assertMatch(
        {ok, Payload},
        arizona_crypto:verify(?PURPOSE, arizona_crypto:sign(?PURPOSE, Payload, #{ttl => 3600}))
    ).

verify_rejects_tampered_payload(Config) when is_list(Config) ->
    Signed = arizona_crypto:sign(?PURPOSE, ~"boom"),
    [B64Payload, B64Sig] = binary:split(Signed, ~"."),
    %% Flip a byte in the payload half; the recomputed HMAC must no longer match.
    Tampered = <<(flip_first_byte(B64Payload))/binary, ".", B64Sig/binary>>,
    ?assertEqual(error, arizona_crypto:verify(?PURPOSE, Tampered)).

verify_rejects_tampered_signature(Config) when is_list(Config) ->
    Signed = arizona_crypto:sign(?PURPOSE, ~"boom"),
    %% Corrupt the trailing signature byte; the HMAC check must fail closed.
    ?assertEqual(error, arizona_crypto:verify(?PURPOSE, <<Signed/binary, "x">>)).

verify_rejects_malformed_no_dot(Config) when is_list(Config) ->
    ?assertEqual(error, arizona_crypto:verify(?PURPOSE, ~"nodothere")),
    ?assertEqual(error, arizona_crypto:verify(?PURPOSE, <<>>)).

verify_rejects_extra_dots(Config) when is_list(Config) ->
    %% Split takes the first `.`; the right half is not valid base64 -> error.
    ?assertEqual(error, arizona_crypto:verify(?PURPOSE, ~"a.b.c")).

verify_rejects_wrong_secret(Config) when is_list(Config) ->
    Signed = arizona_crypto:sign(?PURPOSE, ~"secret-data"),
    application:set_env(arizona, secret_key, ~"a-totally-different-secret-key-99"),
    try
        ?assertEqual(error, arizona_crypto:verify(?PURPOSE, Signed))
    after
        application:set_env(arizona, secret_key, ?SECRET)
    end.

verify_rejects_other_purpose(Config) when is_list(Config) ->
    %% Domain separation: every consumer shares one secret_key, so a value minted
    %% for one token type must not verify as another. The purpose is mixed into the
    %% HMAC message, so the same bytes under a different label fail the MAC -- the
    %% payload parser is never reached.
    Signed = arizona_crypto:sign(~"arizona.one", ~"payload"),
    ?assertEqual(error, arizona_crypto:verify(~"arizona.two", Signed)),
    ?assertMatch({ok, ~"payload"}, arizona_crypto:verify(~"arizona.one", Signed)).

decrypt_rejects_other_purpose(Config) when is_list(Config) ->
    %% Same separation on the AEAD side: the purpose is the GCM additional
    %% authenticated data, so a mismatched label fails the tag check.
    Blob = arizona_crypto:encrypt(~"arizona.one", ~"payload"),
    ?assertEqual(error, arizona_crypto:decrypt(~"arizona.two", Blob)),
    ?assertMatch({ok, ~"payload"}, arizona_crypto:decrypt(~"arizona.one", Blob)).

encrypt_decrypt_round_trips(Config) when is_list(Config) ->
    Payload = ~"hello, world",
    ?assertMatch(
        {ok, Payload}, arizona_crypto:decrypt(?PURPOSE, arizona_crypto:encrypt(?PURPOSE, Payload))
    ).

encrypt_decrypt_round_trips_empty(Config) when is_list(Config) ->
    ?assertMatch(
        {ok, <<>>}, arizona_crypto:decrypt(?PURPOSE, arizona_crypto:encrypt(?PURPOSE, <<>>))
    ).

encrypt_is_confidential(Config) when is_list(Config) ->
    Payload = ~"super-secret-value",
    Blob1 = arizona_crypto:encrypt(?PURPOSE, Payload),
    Blob2 = arizona_crypto:encrypt(?PURPOSE, Payload),
    %% The ciphertext does not leak the plaintext, and a random IV makes two
    %% encryptions of the same payload differ.
    ?assertEqual(nomatch, binary:match(Blob1, Payload)),
    ?assertNotEqual(Blob1, Blob2).

decrypt_rejects_tampered_ciphertext(Config) when is_list(Config) ->
    Blob = arizona_crypto:encrypt(?PURPOSE, ~"boom"),
    %% Corrupt the trailing byte; the GCM tag must fail closed.
    ?assertEqual(error, arizona_crypto:decrypt(?PURPOSE, <<Blob/binary, "x">>)).

decrypt_rejects_truncated(Config) when is_list(Config) ->
    %% Too short to hold an IV + tag -> error, not a crash.
    ?assertEqual(error, arizona_crypto:decrypt(?PURPOSE, ~"short")),
    ?assertEqual(error, arizona_crypto:decrypt(?PURPOSE, <<>>)).

decrypt_rejects_wrong_secret(Config) when is_list(Config) ->
    Blob = arizona_crypto:encrypt(?PURPOSE, ~"secret-data"),
    application:set_env(arizona, secret_key, ~"a-totally-different-secret-key-99"),
    try
        ?assertEqual(error, arizona_crypto:decrypt(?PURPOSE, Blob))
    after
        application:set_env(arizona, secret_key, ?SECRET)
    end.

encrypt_ttl_verifies_before_expiry(Config) when is_list(Config) ->
    %% A far-future TTL decrypts now without sleeping; the expiry boundary itself
    %% is covered deterministically by the inline EUnit tests in arizona_crypto.
    Payload = ~"fresh",
    ?assertMatch(
        {ok, Payload},
        arizona_crypto:decrypt(?PURPOSE, arizona_crypto:encrypt(?PURPOSE, Payload, #{ttl => 3600}))
    ).

verify_accepts_previous_key(Config) when is_list(Config) ->
    %% Sign under the current key, then rotate it into secret_key_previous behind a
    %% fresh primary -- the old value still verifies during the grace window.
    Signed = arizona_crypto:sign(?PURPOSE, ~"payload"),
    application:set_env(arizona, secret_key, ~"a-fresh-primary-key-after-rotation"),
    application:set_env(arizona, secret_key_previous, [?SECRET]),
    try
        ?assertMatch({ok, ~"payload"}, arizona_crypto:verify(?PURPOSE, Signed))
    after
        application:set_env(arizona, secret_key, ?SECRET),
        application:unset_env(arizona, secret_key_previous)
    end.

decrypt_accepts_previous_key(Config) when is_list(Config) ->
    Blob = arizona_crypto:encrypt(?PURPOSE, ~"payload"),
    application:set_env(arizona, secret_key, ~"a-fresh-primary-key-after-rotation"),
    application:set_env(arizona, secret_key_previous, [?SECRET]),
    try
        ?assertMatch({ok, ~"payload"}, arizona_crypto:decrypt(?PURPOSE, Blob))
    after
        application:set_env(arizona, secret_key, ?SECRET),
        application:unset_env(arizona, secret_key_previous)
    end.

verify_rejects_fully_rotated_key(Config) when is_list(Config) ->
    %% Once the old key is dropped from secret_key_previous, its values stop verifying.
    Signed = arizona_crypto:sign(?PURPOSE, ~"payload"),
    application:set_env(arizona, secret_key, ~"a-fresh-primary-key-after-rotation"),
    try
        ?assertEqual(error, arizona_crypto:verify(?PURPOSE, Signed))
    after
        application:set_env(arizona, secret_key, ?SECRET)
    end.

decrypt_rejects_fully_rotated_key(Config) when is_list(Config) ->
    Blob = arizona_crypto:encrypt(?PURPOSE, ~"payload"),
    application:set_env(arizona, secret_key, ~"a-fresh-primary-key-after-rotation"),
    try
        ?assertEqual(error, arizona_crypto:decrypt(?PURPOSE, Blob))
    after
        application:set_env(arizona, secret_key, ?SECRET)
    end.

secret_returns_configured(Config) when is_list(Config) ->
    ?assertEqual(?SECRET, arizona_crypto:secret()).

secret_errors_when_unset(Config) when is_list(Config) ->
    application:unset_env(arizona, secret_key),
    try
        ?assertError(secret_key_not_configured, arizona_crypto:secret())
    after
        application:set_env(arizona, secret_key, ?SECRET)
    end.

sign_errors_when_secret_unset(Config) when is_list(Config) ->
    application:unset_env(arizona, secret_key),
    try
        ?assertError(secret_key_not_configured, arizona_crypto:sign(?PURPOSE, ~"x"))
    after
        application:set_env(arizona, secret_key, ?SECRET)
    end.

encrypt_errors_when_secret_unset(Config) when is_list(Config) ->
    application:unset_env(arizona, secret_key),
    try
        ?assertError(secret_key_not_configured, arizona_crypto:encrypt(?PURPOSE, ~"x"))
    after
        application:set_env(arizona, secret_key, ?SECRET)
    end.

format_error_renders_secret_message(Config) when is_list(Config) ->
    #{general := Msg} = arizona_crypto:format_error(secret_key_not_configured, []),
    ?assertNotEqual(nomatch, binary:match(iolist_to_binary(Msg), ~"secret_key")).

%% Flip the first byte to a guaranteed-different value, keeping it in the
%% URL-safe base64 alphabet so only the HMAC (not the decode) rejects it.
flip_first_byte(<<First, Rest/binary>>) ->
    Flipped =
        case First of
            $A -> $B;
            _ -> $A
        end,
    <<Flipped, Rest/binary>>.
