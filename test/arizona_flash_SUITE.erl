-module(arizona_flash_SUITE).
-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).

-export([cookie_name_is_az_flash/1]).
-export([encode_decode_round_trips/1]).
-export([decode_rejects_tampered_signature/1]).
-export([decode_rejects_malformed/1]).
-export([key_normalizes_atom/1]).
-export([resp_cookie_sets_when_flash_present/1]).
-export([resp_cookie_clears_when_consumed/1]).
-export([resp_cookie_none_when_idle/1]).
-export([secret_errors_when_unset/1]).

%% Sequential (not parallel): the cases share the `secret_key` application env,
%% and secret_errors_when_unset/1 toggles it.
all() ->
    [
        cookie_name_is_az_flash,
        encode_decode_round_trips,
        decode_rejects_tampered_signature,
        decode_rejects_malformed,
        key_normalizes_atom,
        resp_cookie_sets_when_flash_present,
        resp_cookie_clears_when_consumed,
        resp_cookie_none_when_idle,
        secret_errors_when_unset
    ].

init_per_suite(Config) ->
    application:set_env(arizona, secret_key, ~"test-secret-key-0123456789abcdef"),
    Config.

end_per_suite(_Config) ->
    application:unset_env(arizona, secret_key),
    ok.

cookie_name_is_az_flash(Config) when is_list(Config) ->
    ?assertEqual(~"az_flash", arizona_flash:cookie_name()).

encode_decode_round_trips(Config) when is_list(Config) ->
    Flash = #{~"error" => ~"Invalid email or password.", ~"info" => ~"hi"},
    ?assertEqual(Flash, arizona_flash:decode(arizona_flash:encode(Flash))).

decode_rejects_tampered_signature(Config) when is_list(Config) ->
    Encoded = arizona_flash:encode(#{~"error" => ~"boom"}),
    %% Corrupt the trailing signature byte; the HMAC check must fail closed.
    ?assertEqual(#{}, arizona_flash:decode(<<Encoded/binary, "x">>)).

decode_rejects_malformed(Config) when is_list(Config) ->
    ?assertEqual(#{}, arizona_flash:decode(~"not-a-flash")),
    ?assertEqual(#{}, arizona_flash:decode(~"")).

key_normalizes_atom(Config) when is_list(Config) ->
    ?assertEqual(~"error", arizona_flash:key(error)),
    ?assertEqual(~"error", arizona_flash:key(~"error")).

resp_cookie_sets_when_flash_present(Config) when is_list(Config) ->
    {Name, Value, Opts} = arizona_flash:resp_cookie(#{~"error" => ~"x"}, false),
    ?assertEqual(~"az_flash", Name),
    ?assertNotEqual(<<>>, Value),
    ?assert(maps:get(max_age, Opts) > 0),
    ?assertEqual(true, maps:get(http_only, Opts)).

resp_cookie_clears_when_consumed(Config) when is_list(Config) ->
    {Name, Value, Opts} = arizona_flash:resp_cookie(#{}, true),
    ?assertEqual(~"az_flash", Name),
    ?assertEqual(<<>>, Value),
    ?assertEqual(0, maps:get(max_age, Opts)).

resp_cookie_none_when_idle(Config) when is_list(Config) ->
    ?assertEqual(none, arizona_flash:resp_cookie(#{}, false)).

secret_errors_when_unset(Config) when is_list(Config) ->
    application:unset_env(arizona, secret_key),
    try
        ?assertError(secret_key_not_configured, arizona_flash:secret())
    after
        application:set_env(arizona, secret_key, ~"test-secret-key-0123456789abcdef")
    end.
