-module(arizona_session_SUITE).
-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).

-export([cookie_name_is_az_session/1]).
-export([encode_decode_round_trips/1]).
-export([decode_rejects_tampered/1]).
-export([decode_rejects_malformed/1]).
-export([key_normalizes_atom/1]).
-export([resp_cookie_sets_when_dirty_and_present/1]).
-export([resp_cookie_clears_when_dirty_and_empty/1]).
-export([resp_cookie_none_when_clean/1]).
-export([cookie_secure_follows_env/1]).
-export([encode_errors_when_too_large/1]).
-export([format_error_renders_too_large_message/1]).

%% Sequential (not parallel): the cases share the `secret_key` application env.
all() ->
    [
        cookie_name_is_az_session,
        encode_decode_round_trips,
        decode_rejects_tampered,
        decode_rejects_malformed,
        key_normalizes_atom,
        resp_cookie_sets_when_dirty_and_present,
        resp_cookie_clears_when_dirty_and_empty,
        resp_cookie_none_when_clean,
        cookie_secure_follows_env,
        encode_errors_when_too_large,
        format_error_renders_too_large_message
    ].

init_per_suite(Config) ->
    application:set_env(arizona, secret_key, ~"test-secret-key-0123456789abcdef"),
    Config.

end_per_suite(_Config) ->
    application:unset_env(arizona, secret_key),
    ok.

cookie_name_is_az_session(Config) when is_list(Config) ->
    ?assertEqual(~"az_session", arizona_session:cookie_name()).

encode_decode_round_trips(Config) when is_list(Config) ->
    Session = #{~"user_id" => ~"42", ~"theme" => ~"dark"},
    ?assertEqual(Session, arizona_session:decode(arizona_session:encode(Session))).

decode_rejects_tampered(Config) when is_list(Config) ->
    Encoded = arizona_session:encode(#{~"user_id" => ~"42"}),
    %% Corrupt the trailing byte; the AES-GCM tag must fail closed.
    ?assertEqual(#{}, arizona_session:decode(<<Encoded/binary, "x">>)).

decode_rejects_malformed(Config) when is_list(Config) ->
    ?assertEqual(#{}, arizona_session:decode(~"not-a-session")),
    ?assertEqual(#{}, arizona_session:decode(~"")).

key_normalizes_atom(Config) when is_list(Config) ->
    ?assertEqual(~"user_id", arizona_session:key(user_id)),
    ?assertEqual(~"user_id", arizona_session:key(~"user_id")).

resp_cookie_sets_when_dirty_and_present(Config) when is_list(Config) ->
    {Name, Value, Opts} = arizona_session:resp_cookie(#{~"user_id" => ~"42"}, true),
    ?assertEqual(~"az_session", Name),
    %% The set cookie carries the encrypted session, decodable back to the map.
    ?assertEqual(#{~"user_id" => ~"42"}, arizona_session:decode(Value)),
    ?assert(maps:get(max_age, Opts) > 0),
    ?assertEqual(true, maps:get(http_only, Opts)).

resp_cookie_clears_when_dirty_and_empty(Config) when is_list(Config) ->
    {Name, Value, Opts} = arizona_session:resp_cookie(#{}, true),
    ?assertEqual(~"az_session", Name),
    ?assertEqual(<<>>, Value),
    ?assertEqual(0, maps:get(max_age, Opts)).

resp_cookie_none_when_clean(Config) when is_list(Config) ->
    %% The durable invariant: a clean (unwritten) request emits nothing, even when
    %% the effective session is non-empty -- a read never re-emits the cookie.
    ?assertEqual(none, arizona_session:resp_cookie(#{~"user_id" => ~"42"}, false)).

cookie_secure_follows_env(Config) when is_list(Config) ->
    %% Defaults to false (dev-safe; a Secure cookie is dropped over plain HTTP), and
    %% honors session_secure when set (apps enable it in production).
    {_, _, Default} = arizona_session:resp_cookie(#{~"k" => ~"v"}, true),
    ?assertEqual(false, maps:get(secure, Default)),
    application:set_env(arizona, session_secure, true),
    try
        {_, _, Secure} = arizona_session:resp_cookie(#{~"k" => ~"v"}, true),
        ?assertEqual(true, maps:get(secure, Secure))
    after
        application:unset_env(arizona, session_secure)
    end.

encode_errors_when_too_large(Config) when is_list(Config) ->
    %% A tiny limit forces the guard; the encoded (encrypted) value exceeds 8 bytes.
    application:set_env(arizona, session_max_bytes, 8),
    try
        ?assertError(
            {session_too_large, _, 8},
            arizona_session:encode(#{~"k" => ~"a value well over eight bytes"})
        )
    after
        application:unset_env(arizona, session_max_bytes)
    end.

format_error_renders_too_large_message(Config) when is_list(Config) ->
    #{general := Msg} = arizona_session:format_error({session_too_large, 5000, 4096}, []),
    ?assertNotEqual(nomatch, binary:match(iolist_to_binary(Msg), ~"session")).
