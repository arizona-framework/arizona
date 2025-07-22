-module(arizona_binder_SUITE).
-behaviour(ct_suite).
-include_lib("stdlib/include/assert.hrl").
-compile([export_all, nowarn_export_all]).

%% --------------------------------------------------------------------
%% Behaviour (ct_suite) callbacks
%% --------------------------------------------------------------------

all() ->
    [
        {group, basic_operations},
        {group, retrieval_operations},
        {group, query_operations},
        {group, error_cases}
    ].

groups() ->
    [
        {basic_operations, [parallel], [
            new_empty_bindings,
            put_single_value,
            put_multiple_values
        ]},
        {retrieval_operations, [parallel], [
            get_existing_key,
            get_with_default_existing,
            get_with_default_missing,
            find_existing_key,
            find_missing_key
        ]},
        {query_operations, [parallel], [
            keys_empty_bindings,
            keys_with_values,
            is_empty_true,
            is_empty_false
        ]},
        {error_cases, [parallel], [
            get_missing_key_error,
            get_invalid_key_error,
            put_invalid_key_error
        ]}
    ].

%% --------------------------------------------------------------------
%% Basic operations tests
%% --------------------------------------------------------------------

new_empty_bindings(Config) when is_list(Config) ->
    ct:comment("new/0 should return empty bindings"),
    Bindings = arizona_binder:new(),
    ?assertEqual(true, arizona_binder:is_empty(Bindings)).

put_single_value(Config) when is_list(Config) ->
    ct:comment("put/3 should add a single key-value pair"),
    Bindings = arizona_binder:new(),
    Bindings1 = arizona_binder:put(name, ~"Alice", Bindings),
    ?assertEqual({ok, ~"Alice"}, arizona_binder:find(name, Bindings1)).

put_multiple_values(Config) when is_list(Config) ->
    ct:comment("put/3 should handle multiple key-value pairs"),
    Bindings = arizona_binder:new(),
    Bindings1 = arizona_binder:put(name, ~"Alice", Bindings),
    Bindings2 = arizona_binder:put(age, 30, Bindings1),
    Bindings3 = arizona_binder:put(city, ~"Phoenix", Bindings2),
    ?assertEqual({ok, ~"Alice"}, arizona_binder:find(name, Bindings3)),
    ?assertEqual({ok, 30}, arizona_binder:find(age, Bindings3)),
    ?assertEqual({ok, ~"Phoenix"}, arizona_binder:find(city, Bindings3)).

%% --------------------------------------------------------------------
%% Retrieval operations tests
%% --------------------------------------------------------------------

get_existing_key(Config) when is_list(Config) ->
    ct:comment("get/2 should return value for existing key"),
    Bindings = arizona_binder:put(name, ~"Alice", arizona_binder:new()),
    ?assertEqual(~"Alice", arizona_binder:get(name, Bindings)).

get_with_default_existing(Config) when is_list(Config) ->
    ct:comment("get/3 should return existing value, not default"),
    Bindings = arizona_binder:put(name, ~"Alice", arizona_binder:new()),
    DefaultFun = fun() -> ~"Default" end,
    ?assertEqual(~"Alice", arizona_binder:get(name, Bindings, DefaultFun)).

get_with_default_missing(Config) when is_list(Config) ->
    ct:comment("get/3 should return default value for missing key"),
    Bindings = arizona_binder:new(),
    DefaultFun = fun() -> ~"Default" end,
    ?assertEqual(~"Default", arizona_binder:get(missing_key, Bindings, DefaultFun)).

find_existing_key(Config) when is_list(Config) ->
    ct:comment("find/2 should return {ok, Value} for existing key"),
    Bindings = arizona_binder:put(name, ~"Alice", arizona_binder:new()),
    ?assertEqual({ok, ~"Alice"}, arizona_binder:find(name, Bindings)).

find_missing_key(Config) when is_list(Config) ->
    ct:comment("find/2 should return error for missing key"),
    Bindings = arizona_binder:new(),
    ?assertEqual(error, arizona_binder:find(missing_key, Bindings)).

%% --------------------------------------------------------------------
%% Query operations tests
%% --------------------------------------------------------------------

keys_empty_bindings(Config) when is_list(Config) ->
    ct:comment("keys/1 should return empty list for empty bindings"),
    Bindings = arizona_binder:new(),
    ?assertEqual([], arizona_binder:keys(Bindings)).

keys_with_values(Config) when is_list(Config) ->
    ct:comment("keys/1 should return list of all keys"),
    Bindings = arizona_binder:new(),
    Bindings1 = arizona_binder:put(name, ~"Alice", Bindings),
    Bindings2 = arizona_binder:put(age, 30, Bindings1),
    Keys = arizona_binder:keys(Bindings2),
    ?assertEqual(2, length(Keys)),
    ?assertEqual(true, lists:member(name, Keys)),
    ?assertEqual(true, lists:member(age, Keys)).

is_empty_true(Config) when is_list(Config) ->
    ct:comment("is_empty/1 should return true for empty bindings"),
    Bindings = arizona_binder:new(),
    ?assertEqual(true, arizona_binder:is_empty(Bindings)).

is_empty_false(Config) when is_list(Config) ->
    ct:comment("is_empty/1 should return false for non-empty bindings"),
    Bindings = arizona_binder:put(name, ~"Alice", arizona_binder:new()),
    ?assertEqual(false, arizona_binder:is_empty(Bindings)).

%% --------------------------------------------------------------------
%% Error cases tests
%% --------------------------------------------------------------------

get_missing_key_error(Config) when is_list(Config) ->
    ct:comment("get/2 should throw badkey error for missing key"),
    Bindings = arizona_binder:new(),
    ?assertError({badkey, missing_key}, arizona_binder:get(missing_key, Bindings)).

get_invalid_key_error(Config) when is_list(Config) ->
    ct:comment("get/2 should cause function_clause error for non-atom key"),
    Bindings = arizona_binder:new(),
    ?assertError(function_clause, arizona_binder:get(~"invalid_key", Bindings)).

put_invalid_key_error(Config) when is_list(Config) ->
    ct:comment("put/3 should cause function_clause error for non-atom key"),
    Bindings = arizona_binder:new(),
    ?assertError(function_clause, arizona_binder:put(~"invalid_key", ~"value", Bindings)).
