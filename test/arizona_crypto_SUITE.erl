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
-export([secret_warns_once_when_short/1]).
-export([secret_silent_when_long_enough/1]).
-export([secret_errors_when_unset/1]).
-export([sign_errors_when_secret_unset/1]).
-export([encrypt_errors_when_secret_unset/1]).
-export([verify_errors_when_secret_unset/1]).
-export([decrypt_errors_when_secret_unset/1]).
-export([verify_errors_on_broken_previous_key_env/1]).
-export([decrypt_errors_on_broken_previous_key_env/1]).
-export([verify_refuses_empty_previous_key/1]).
-export([decrypt_refuses_empty_previous_key/1]).
-export([consumers_refuse_empty_previous_key/1]).
-export([previous_key_refuses_non_binary_entry/1]).
-export([previous_key_refuses_non_list_value/1]).
-export([previous_key_improper_list_fails_loudly/1]).
-export([previous_key_error_trace_carries_position/1]).
-export([sign_refuses_empty_previous_key/1]).
-export([encrypt_refuses_empty_previous_key/1]).
-export([previous_key_list_env_default_means_none/1]).
-export([previous_key_warns_once_when_short/1]).
-export([format_error_renders_secret_message/1]).
-export([format_error_renders_previous_key_message/1]).

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
        secret_warns_once_when_short,
        secret_silent_when_long_enough,
        secret_errors_when_unset,
        sign_errors_when_secret_unset,
        encrypt_errors_when_secret_unset,
        verify_errors_when_secret_unset,
        decrypt_errors_when_secret_unset,
        verify_errors_on_broken_previous_key_env,
        decrypt_errors_on_broken_previous_key_env,
        verify_refuses_empty_previous_key,
        decrypt_refuses_empty_previous_key,
        consumers_refuse_empty_previous_key,
        previous_key_refuses_non_binary_entry,
        previous_key_refuses_non_list_value,
        previous_key_improper_list_fails_loudly,
        previous_key_error_trace_carries_position,
        sign_refuses_empty_previous_key,
        encrypt_refuses_empty_previous_key,
        previous_key_list_env_default_means_none,
        previous_key_warns_once_when_short,
        format_error_renders_secret_message,
        format_error_renders_previous_key_message
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

secret_warns_once_when_short(Config) when is_list(Config) ->
    %% The secret is key material used directly (no slow KDF), so a short one is
    %% brute-forceable offline from a single captured cookie. That must be loud --
    %% but only once per distinct key, not on every sign.
    HandlerId = arizona_crypto_suite_log_capture,
    ok = logger:add_handler(HandlerId, arizona_test_log_handler, #{
        level => warning,
        config => #{pid => self()}
    }),
    %% A fresh key each run: the marker is keyed by the secret, so a key another
    %% case already warned about would (correctly) stay quiet.
    Weak = <<"short-", (binary:encode_hex(crypto:strong_rand_bytes(4)))/binary>>,
    application:set_env(arizona, secret_key, Weak),
    try
        ?assertEqual(Weak, arizona_crypto:secret()),
        ?assert(received_weak_secret_warning(secret_key, 1000)),
        %% The same key again is silent -- one warning, not one per signed value.
        ?assertEqual(Weak, arizona_crypto:secret()),
        ?assertNot(received_weak_secret_warning(secret_key, 200))
    after
        ok = logger:remove_handler(HandlerId),
        application:set_env(arizona, secret_key, ?SECRET)
    end.

secret_silent_when_long_enough(Config) when is_list(Config) ->
    %% A full 32-byte key is the documented minimum, so it must not nag.
    HandlerId = arizona_crypto_suite_log_capture,
    ok = logger:add_handler(HandlerId, arizona_test_log_handler, #{
        level => warning,
        config => #{pid => self()}
    }),
    try
        ?assertEqual(?SECRET, arizona_crypto:secret()),
        ?assertEqual(32, byte_size(?SECRET)),
        ?assertNot(received_weak_secret_warning(secret_key, 200))
    after
        ok = logger:remove_handler(HandlerId)
    end.

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

verify_errors_when_secret_unset(Config) when is_list(Config) ->
    %% The read path must fail as loudly as the write path: a missing secret is a
    %% node misconfiguration, not a tampered cookie. Swallowing it into the
    %% tampered-value `error` sentinel would silently sign every user out with
    %% nothing logged.
    Signed = arizona_crypto:sign(?PURPOSE, ~"payload"),
    application:unset_env(arizona, secret_key),
    try
        ?assertError(secret_key_not_configured, arizona_crypto:verify(?PURPOSE, Signed))
    after
        application:set_env(arizona, secret_key, ?SECRET)
    end.

decrypt_errors_when_secret_unset(Config) when is_list(Config) ->
    Blob = arizona_crypto:encrypt(?PURPOSE, ~"payload"),
    application:unset_env(arizona, secret_key),
    try
        ?assertError(secret_key_not_configured, arizona_crypto:decrypt(?PURPOSE, Blob))
    after
        application:set_env(arizona, secret_key, ?SECRET)
    end.

verify_errors_on_broken_previous_key_env(Config) when is_list(Config) ->
    %% A broken `{env, ...}` reference in secret_key_previous is the same class of
    %% config error: resolution failure (env_not_set) must propagate from the read
    %% path, not read as a signed-out request.
    Var = "ARIZONA_CRYPTO_SUITE_UNSET_PREV",
    true = os:unsetenv(Var),
    Signed = arizona_crypto:sign(?PURPOSE, ~"payload"),
    application:set_env(arizona, secret_key_previous, {env, Var}),
    try
        ?assertError({env_not_set, Var}, arizona_crypto:verify(?PURPOSE, Signed))
    after
        application:unset_env(arizona, secret_key_previous)
    end.

decrypt_errors_on_broken_previous_key_env(Config) when is_list(Config) ->
    Var = "ARIZONA_CRYPTO_SUITE_UNSET_PREV",
    true = os:unsetenv(Var),
    Blob = arizona_crypto:encrypt(?PURPOSE, ~"payload"),
    application:set_env(arizona, secret_key_previous, {env, Var}),
    try
        ?assertError({env_not_set, Var}, arizona_crypto:decrypt(?PURPOSE, Blob))
    after
        application:unset_env(arizona, secret_key_previous)
    end.

verify_refuses_empty_previous_key(Config) when is_list(Config) ->
    %% `[{env, Var, ~""}]` with the variable unset resolves to `[<<>>]`. An empty
    %% HMAC key is a public constant, so a value an attacker signs with it must not
    %% verify: the misconfiguration raises instead.
    ok = set_empty_previous_key(),
    Forged = forge_signed(?PURPOSE, <<0, "forged">>),
    try
        ?assertError({secret_key_previous_invalid, 1}, arizona_crypto:verify(?PURPOSE, Forged))
    after
        application:unset_env(arizona, secret_key_previous)
    end.

decrypt_refuses_empty_previous_key(Config) when is_list(Config) ->
    %% The AES key derived from an empty secret is `sha256("arizona.aead.v1:")`.
    ok = set_empty_previous_key(),
    Forged = forge_encrypted(?PURPOSE, <<0, "forged">>),
    try
        ?assertError({secret_key_previous_invalid, 1}, arizona_crypto:decrypt(?PURPOSE, Forged))
    after
        application:unset_env(arizona, secret_key_previous)
    end.

consumers_refuse_empty_previous_key(Config) when is_list(Config) ->
    %% The forgeries the empty key enabled, end to end: a flash message, a signed
    %% store-mode session id, and a cookie-mode session.
    ok = set_empty_previous_key(),
    Flash = forge_signed(~"arizona.flash", <<0, "{\"info\":\"forged\"}">>),
    SessionId = forge_signed(~"arizona.session_id", <<0, "victim-session-id">>),
    Session = forge_encrypted(~"arizona.session", <<0, "{\"user_id\":\"admin\"}">>),
    try
        ?assertError({secret_key_previous_invalid, 1}, arizona_flash:decode(Flash)),
        ?assertError({secret_key_previous_invalid, 1}, arizona_session:decode_id(SessionId)),
        ?assertError({secret_key_previous_invalid, 1}, arizona_session:decode(Session))
    after
        application:unset_env(arizona, secret_key_previous)
    end.

previous_key_refuses_non_binary_entry(Config) when is_list(Config) ->
    %% A valid key ahead of a bad one does not mask it, and the error names the
    %% entry's position, never its value (a charlist entry is still key material).
    Signed = arizona_crypto:sign(?PURPOSE, ~"payload"),
    Charlist = "a-charlist-previous-key-0123456789",
    try
        application:set_env(arizona, secret_key_previous, [?SECRET, old_key]),
        ?assertError({secret_key_previous_invalid, 2}, arizona_crypto:verify(?PURPOSE, Signed)),
        application:set_env(arizona, secret_key_previous, [Charlist]),
        ?assertError({secret_key_previous_invalid, 1}, arizona_crypto:verify(?PURPOSE, Signed))
    after
        application:unset_env(arizona, secret_key_previous)
    end.

previous_key_refuses_non_list_value(Config) when is_list(Config) ->
    %% A bare binary instead of a list of them made an improper candidate list.
    Blob = arizona_crypto:encrypt(?PURPOSE, ~"payload"),
    application:set_env(arizona, secret_key_previous, ?SECRET),
    try
        ?assertError({secret_key_previous_invalid, 0}, arizona_crypto:decrypt(?PURPOSE, Blob))
    after
        application:unset_env(arizona, secret_key_previous)
    end.

previous_key_improper_list_fails_loudly(Config) when is_list(Config) ->
    %% `[Key | bad]` never verifies: env-reference resolution (`arizona_config`)
    %% already refuses the improper list before the key check sees it.
    Signed = arizona_crypto:sign(?PURPOSE, ~"payload"),
    application:set_env(arizona, secret_key_previous, [?SECRET | bad]),
    try
        ?assertError({bad_generator, bad}, arizona_crypto:verify(?PURPOSE, Signed))
    after
        application:unset_env(arizona, secret_key_previous)
    end.

previous_key_error_trace_carries_position(Config) when is_list(Config) ->
    %% The raising frame shows its real argument (the position), so the trace names
    %% `invalid_previous_key/1` with the entry, never an empty argument list.
    Signed = arizona_crypto:sign(?PURPOSE, ~"payload"),
    application:set_env(arizona, secret_key_previous, [<<>>]),
    try arizona_crypto:verify(?PURPOSE, Signed) of
        Result -> ct:fail({expected_error, Result})
    catch
        error:{secret_key_previous_invalid, 1}:Stacktrace ->
            ?assertMatch([{arizona_crypto, invalid_previous_key, [1], _} | _], Stacktrace)
    after
        application:unset_env(arizona, secret_key_previous)
    end.

sign_refuses_empty_previous_key(Config) when is_list(Config) ->
    %% The write paths fail too, so a bad previous key breaks the first response
    %% instead of setting cookies that crash every later request carrying one.
    ok = set_empty_previous_key(),
    try
        ?assertError({secret_key_previous_invalid, 1}, arizona_crypto:sign(?PURPOSE, ~"x")),
        ?assertError(
            {secret_key_previous_invalid, 1}, arizona_crypto:sign(?PURPOSE, ~"x", #{ttl => 60})
        )
    after
        application:unset_env(arizona, secret_key_previous)
    end.

encrypt_refuses_empty_previous_key(Config) when is_list(Config) ->
    ok = set_empty_previous_key(),
    try
        ?assertError({secret_key_previous_invalid, 1}, arizona_crypto:encrypt(?PURPOSE, ~"x")),
        ?assertError(
            {secret_key_previous_invalid, 1},
            arizona_crypto:encrypt(?PURPOSE, ~"x", #{ttl => 60})
        )
    after
        application:unset_env(arizona, secret_key_previous)
    end.

previous_key_list_env_default_means_none(Config) when is_list(Config) ->
    %% The documented "no previous key" spelling: the list default resolves to `[]`
    %% whether the variable is unset or set empty, so nothing is refused.
    Var = "ARIZONA_CRYPTO_SUITE_OPTIONAL_PREV",
    Signed = arizona_crypto:sign(?PURPOSE, ~"payload"),
    application:set_env(arizona, secret_key_previous, {env, Var, []}),
    try
        true = os:unsetenv(Var),
        ?assertEqual({ok, ~"payload"}, arizona_crypto:verify(?PURPOSE, Signed)),
        true = os:putenv(Var, ""),
        ?assertEqual({ok, ~"payload"}, arizona_crypto:verify(?PURPOSE, Signed))
    after
        true = os:unsetenv(Var),
        application:unset_env(arizona, secret_key_previous)
    end.

previous_key_warns_once_when_short(Config) when is_list(Config) ->
    %% A previous key is key material too: a short one verifies, but warns, once.
    HandlerId = arizona_crypto_suite_log_capture,
    ok = logger:add_handler(HandlerId, arizona_test_log_handler, #{
        level => warning,
        config => #{pid => self()}
    }),
    Weak = <<"short-", (binary:encode_hex(crypto:strong_rand_bytes(4)))/binary>>,
    application:set_env(arizona, secret_key, Weak),
    Signed = arizona_crypto:sign(?PURPOSE, ~"payload"),
    application:set_env(arizona, secret_key, ?SECRET),
    application:set_env(arizona, secret_key_previous, [Weak]),
    try
        ?assertEqual({ok, ~"payload"}, arizona_crypto:verify(?PURPOSE, Signed)),
        ?assert(received_weak_secret_warning(secret_key_previous, 1000)),
        ?assertEqual({ok, ~"payload"}, arizona_crypto:verify(?PURPOSE, Signed)),
        ?assertNot(received_weak_secret_warning(secret_key_previous, 200))
    after
        ok = logger:remove_handler(HandlerId),
        application:unset_env(arizona, secret_key_previous)
    end.

format_error_renders_secret_message(Config) when is_list(Config) ->
    #{general := Msg} = arizona_crypto:format_error(secret_key_not_configured, []),
    ?assertNotEqual(nomatch, binary:match(iolist_to_binary(Msg), ~"secret_key")).

format_error_renders_previous_key_message(Config) when is_list(Config) ->
    #{general := Msg} = arizona_crypto:format_error({secret_key_previous_invalid, 3}, []),
    Bin = iolist_to_binary(Msg),
    ?assertNotEqual(nomatch, binary:match(Bin, ~"secret_key_previous")),
    ?assertNotEqual(nomatch, binary:match(Bin, ~"entry 3")).

%% The rotation config that resolves to an empty key: an optional env reference with
%% a binary default, its variable unset.
set_empty_previous_key() ->
    Var = "ARIZONA_CRYPTO_SUITE_EMPTY_PREV",
    true = os:unsetenv(Var),
    application:set_env(arizona, secret_key_previous, [{env, Var, ~""}]).

%% What an attacker who knows only the wire format mints with the empty key.
forge_signed(Purpose, Tagged) ->
    Message = <<(byte_size(Purpose)):32, Purpose/binary, Tagged/binary>>,
    Sig = crypto:mac(hmac, sha256, <<>>, Message),
    <<(b64(Tagged))/binary, ".", (b64(Sig))/binary>>.

forge_encrypted(Purpose, Plaintext) ->
    Key = crypto:hash(sha256, ~"arizona.aead.v1:"),
    IV = crypto:strong_rand_bytes(12),
    {Cipher, Tag} = crypto:crypto_one_time_aead(aes_256_gcm, Key, IV, Plaintext, Purpose, true),
    b64(<<IV/binary, Tag/binary, Cipher/binary>>).

b64(Bin) ->
    base64:encode(Bin, #{mode => urlsafe, padding => false}).

%% Drain forwarded log events until one is the weak-key warning for `EnvKey`, else
%% time out. Unrelated warnings from elsewhere in the node are skipped, not counted.
received_weak_secret_warning(EnvKey, Timeout) ->
    receive
        {arizona_test_log_handler, #{msg := {Format, [EnvKey | _]}}} when is_list(Format) ->
            case string:find(Format, "bytes, under the") of
                nomatch -> received_weak_secret_warning(EnvKey, Timeout);
                _Found -> true
            end;
        {arizona_test_log_handler, _Event} ->
            received_weak_secret_warning(EnvKey, Timeout)
    after Timeout ->
        false
    end.

%% Flip the first byte to a guaranteed-different value, keeping it in the
%% URL-safe base64 alphabet so only the HMAC (not the decode) rejects it.
flip_first_byte(<<First, Rest/binary>>) ->
    Flipped =
        case First of
            $A -> $B;
            _ -> $A
        end,
    <<Flipped, Rest/binary>>.
