-module(arizona_config_SUITE).
-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([groups/0]).

-export([resolve_integer/1]).
-export([resolve_float/1]).
-export([resolve_boolean/1]).
-export([resolve_binary/1]).
-export([resolve_atom/1]).
-export([resolve_list_comma_split/1]).
-export([resolve_string_default_kept/1]).
-export([resolve_required_returns_binary/1]).
-export([resolve_required_missing_crashes/1]).
-export([resolve_default_when_unset/1]).
-export([resolve_empty_uses_default/1]).
-export([resolve_empty_required_crashes/1]).
-export([resolve_invalid_integer_crashes/1]).
-export([resolve_invalid_boolean_crashes/1]).
-export([resolve_invalid_atom_crashes/1]).
-export([invalid_value_error_is_formatted/1]).
-export([resolve_scalar_passthrough/1]).
-export([resolve_nested_server_map/1]).
-export([resolve_tls_paths/1]).
-export([resolve_leaves_routes_untouched/1]).
-export([get_env_2_resolves/1]).
-export([get_env_1_resolves/1]).
-export([get_env_1_undefined/1]).

all() ->
    [{group, config}].

groups() ->
    %% Sequential: os env vars are VM-global, so cases must not run concurrently.
    [
        {config, [], [
            resolve_integer,
            resolve_float,
            resolve_boolean,
            resolve_binary,
            resolve_atom,
            resolve_list_comma_split,
            resolve_string_default_kept,
            resolve_required_returns_binary,
            resolve_required_missing_crashes,
            resolve_default_when_unset,
            resolve_empty_uses_default,
            resolve_empty_required_crashes,
            resolve_invalid_integer_crashes,
            resolve_invalid_boolean_crashes,
            resolve_invalid_atom_crashes,
            invalid_value_error_is_formatted,
            resolve_scalar_passthrough,
            resolve_nested_server_map,
            resolve_tls_paths,
            resolve_leaves_routes_untouched,
            get_env_2_resolves,
            get_env_1_resolves,
            get_env_1_undefined
        ]}
    ].

%% --------------------------------------------------------------------
%% resolve/1 -- coercion by default type
%% --------------------------------------------------------------------

resolve_integer(Config) when is_list(Config) ->
    with_env("AZ_CFG_INT", "9001", fun() ->
        ?assertEqual(9001, arizona_config:resolve({env, "AZ_CFG_INT", 8080}))
    end).

resolve_float(Config) when is_list(Config) ->
    with_env("AZ_CFG_FLOAT", "2.5", fun() ->
        ?assertEqual(2.5, arizona_config:resolve({env, "AZ_CFG_FLOAT", 1.0}))
    end).

resolve_boolean(Config) when is_list(Config) ->
    %% Case-insensitive, and the default's own boolean value is irrelevant to parsing.
    with_env("AZ_CFG_BOOL", "TRUE", fun() ->
        ?assertEqual(true, arizona_config:resolve({env, "AZ_CFG_BOOL", false}))
    end),
    with_env("AZ_CFG_BOOL", "false", fun() ->
        ?assertEqual(false, arizona_config:resolve({env, "AZ_CFG_BOOL", true}))
    end).

resolve_binary(Config) when is_list(Config) ->
    with_env("AZ_CFG_BIN", "hello", fun() ->
        ?assertEqual(~"hello", arizona_config:resolve({env, "AZ_CFG_BIN", ~"default"}))
    end).

resolve_atom(Config) when is_list(Config) ->
    %% `http`/`https` are interned by referencing them here, so list_to_existing_atom works.
    _ = https,
    with_env("AZ_CFG_ATOM", "https", fun() ->
        ?assertEqual(https, arizona_config:resolve({env, "AZ_CFG_ATOM", http}))
    end).

resolve_list_comma_split(Config) when is_list(Config) ->
    %% An empty-list default coerces a comma string into trimmed binaries (csrf_origins shape).
    with_env("AZ_CFG_LIST", "https://a.example, https://b.example ,", fun() ->
        ?assertEqual(
            [~"https://a.example", ~"https://b.example"],
            arizona_config:resolve({env, "AZ_CFG_LIST", []})
        )
    end).

resolve_string_default_kept(Config) when is_list(Config) ->
    %% A char-list (string) default -- e.g. a tls cert path -- keeps the raw string
    %% rather than comma-splitting into binaries.
    with_env("AZ_CFG_PATH", "/etc/ssl/app.pem", fun() ->
        ?assertEqual(
            "/etc/ssl/app.pem", arizona_config:resolve({env, "AZ_CFG_PATH", "priv/cert.pem"})
        )
    end).

resolve_required_returns_binary(Config) when is_list(Config) ->
    with_env("AZ_CFG_REQ", "s3cr3t", fun() ->
        ?assertEqual(~"s3cr3t", arizona_config:resolve({env, "AZ_CFG_REQ"}))
    end).

resolve_required_missing_crashes(Config) when is_list(Config) ->
    without_env("AZ_CFG_MISSING", fun() ->
        ?assertError(
            {env_not_set, "AZ_CFG_MISSING"},
            arizona_config:resolve({env, "AZ_CFG_MISSING"})
        )
    end).

resolve_default_when_unset(Config) when is_list(Config) ->
    without_env("AZ_CFG_UNSET", fun() ->
        ?assertEqual(8080, arizona_config:resolve({env, "AZ_CFG_UNSET", 8080}))
    end).

%% --------------------------------------------------------------------
%% resolve/1 -- empty and invalid values
%% --------------------------------------------------------------------

resolve_empty_uses_default(Config) when is_list(Config) ->
    %% A set-but-empty var (`PORT=`) is treated as unset: the optional form falls
    %% back to its default rather than trying (and failing) to coerce "".
    with_env("AZ_CFG_EMPTY", "", fun() ->
        ?assertEqual(8080, arizona_config:resolve({env, "AZ_CFG_EMPTY", 8080})),
        ?assertEqual([], arizona_config:resolve({env, "AZ_CFG_EMPTY", []}))
    end).

resolve_empty_required_crashes(Config) when is_list(Config) ->
    %% Empty is unset, so the required form raises `env_not_set` rather than <<>>.
    with_env("AZ_CFG_EMPTY_REQ", "", fun() ->
        ?assertError(
            {env_not_set, "AZ_CFG_EMPTY_REQ"},
            arizona_config:resolve({env, "AZ_CFG_EMPTY_REQ"})
        )
    end).

resolve_invalid_integer_crashes(Config) when is_list(Config) ->
    with_env("AZ_CFG_BADINT", "abc", fun() ->
        ?assertError(
            {env_invalid_value, "AZ_CFG_BADINT", "abc", integer},
            arizona_config:resolve({env, "AZ_CFG_BADINT", 8080})
        )
    end).

resolve_invalid_boolean_crashes(Config) when is_list(Config) ->
    with_env("AZ_CFG_BADBOOL", "yes", fun() ->
        ?assertError(
            {env_invalid_value, "AZ_CFG_BADBOOL", "yes", boolean},
            arizona_config:resolve({env, "AZ_CFG_BADBOOL", false})
        )
    end).

resolve_invalid_atom_crashes(Config) when is_list(Config) ->
    %% An atom default coerces via list_to_existing_atom; a value that is not an
    %% already-known atom raises env_invalid_value rather than a raw badarg. The env
    %% string is never written as an atom literal here, so it stays non-existing.
    with_env("AZ_CFG_BADATOM", "az_cfg_definitely_not_an_atom_42", fun() ->
        ?assertError(
            {env_invalid_value, "AZ_CFG_BADATOM", "az_cfg_definitely_not_an_atom_42", atom},
            arizona_config:resolve({env, "AZ_CFG_BADATOM", http})
        )
    end).

invalid_value_error_is_formatted(Config) when is_list(Config) ->
    #{general := General} = arizona_config:format_error(
        {env_invalid_value, "PORT", "abc", integer}, []
    ),
    Msg = unicode:characters_to_binary(General),
    ?assertMatch({_, _}, binary:match(Msg, ~"PORT")),
    ?assertMatch({_, _}, binary:match(Msg, ~"not a valid integer")).

%% --------------------------------------------------------------------
%% resolve/1 -- structure recursion
%% --------------------------------------------------------------------

resolve_scalar_passthrough(Config) when is_list(Config) ->
    ?assertEqual(true, arizona_config:resolve(true)),
    ?assertEqual(42, arizona_config:resolve(42)),
    ?assertEqual(~"x", arizona_config:resolve(~"x")),
    ?assertEqual([1, 2, 3], arizona_config:resolve([1, 2, 3])),
    ?assertEqual(#{a => 1}, arizona_config:resolve(#{a => 1})).

resolve_nested_server_map(Config) when is_list(Config) ->
    with_env("AZ_CFG_PORT", "9090", fun() ->
        Server = #{
            scheme => http,
            transport_opts => [{port, {env, "AZ_CFG_PORT", 8080}}],
            proto_opts => #{max_clients => {env, "AZ_CFG_MAXC", 200}}
        },
        Resolved = arizona_config:resolve(Server),
        ?assertEqual([{port, 9090}], maps:get(transport_opts, Resolved)),
        %% Unset nested env falls back to its default inside a map.
        ?assertEqual(#{max_clients => 200}, maps:get(proto_opts, Resolved))
    end).

resolve_tls_paths(Config) when is_list(Config) ->
    with_env("AZ_CFG_CERT", "/etc/ssl/app.pem", fun() ->
        Tls = [{certfile, {env, "AZ_CFG_CERT", ~"priv/cert.pem"}}, {keyfile, ~"priv/key.pem"}],
        ?assertEqual(
            [{certfile, ~"/etc/ssl/app.pem"}, {keyfile, ~"priv/key.pem"}],
            arizona_config:resolve(Tls)
        )
    end).

resolve_leaves_routes_untouched(Config) when is_list(Config) ->
    %% A literal {env, _, _} sitting inside a route's opts (a >=3-tuple) must NOT be
    %% rewritten -- resolve only descends into maps, lists, and 2-tuples.
    Routes = [
        {live, ~"/", my_page, #{state => {env, "SHOULD_NOT_RESOLVE", 1}}},
        {ws, ~"/ws", #{}}
    ],
    Server = #{transport_opts => [{port, {env, "AZ_CFG_RT_PORT", 4040}}], routes => Routes},
    Resolved = arizona_config:resolve(Server),
    ?assertEqual(Routes, maps:get(routes, Resolved)),
    ?assertEqual([{port, 4040}], maps:get(transport_opts, Resolved)).

%% --------------------------------------------------------------------
%% get_env/1,2 -- application env + resolution
%% --------------------------------------------------------------------

get_env_2_resolves(Config) when is_list(Config) ->
    with_env("AZ_CFG_GE2", "7777", fun() ->
        with_app_env(some_test_key, {env, "AZ_CFG_GE2", 8080}, fun() ->
            ?assertEqual(7777, arizona_config:get_env(some_test_key, 0))
        end)
    end).

get_env_1_resolves(Config) when is_list(Config) ->
    with_env("AZ_CFG_GE1", "abc", fun() ->
        with_app_env(some_test_key, {env, "AZ_CFG_GE1"}, fun() ->
            ?assertEqual({ok, ~"abc"}, arizona_config:get_env(some_test_key))
        end)
    end).

get_env_1_undefined(Config) when is_list(Config) ->
    application:unset_env(arizona, nonexistent_test_key),
    ?assertEqual(undefined, arizona_config:get_env(nonexistent_test_key)).

%% --------------------------------------------------------------------
%% Helpers
%% --------------------------------------------------------------------

with_env(Var, Value, Fun) ->
    true = os:putenv(Var, Value),
    try
        Fun()
    after
        os:unsetenv(Var)
    end.

without_env(Var, Fun) ->
    os:unsetenv(Var),
    Fun().

with_app_env(Key, Value, Fun) ->
    ok = application:set_env(arizona, Key, Value),
    try
        Fun()
    after
        application:unset_env(arizona, Key)
    end.
