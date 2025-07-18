-module(arizona_binder_SUITE).
-behaviour(ct_suite).
-include_lib("stdlib/include/assert.hrl").
-compile([export_all, nowarn_export_all]).

%% --------------------------------------------------------------------
%% Behaviour (ct_suite) callbacks
%% --------------------------------------------------------------------

all() ->
    [
        {group, get_functions},
        {group, find_functions},
        {group, put_functions},
        {group, merge_functions},
        {group, remove_functions},
        {group, keys_functions},
        {group, values_functions}
    ].

groups() ->
    [
        {get_functions, [parallel], [
            get_success,
            get_missing_key,
            get_type_guards,
            get_with_default_key_exists,
            get_with_default_key_missing,
            get_with_default_nested,
            get_with_default_type_guards
        ]},
        {find_functions, [parallel], [
            find_success,
            find_missing_key,
            find_type_guards
        ]},
        {put_functions, [parallel], [
            put_new_key,
            put_existing_key,
            put_type_guards
        ]},
        {merge_functions, [parallel], [
            merge_overlapping_keys,
            merge_empty_maps,
            merge_type_guards
        ]},
        {remove_functions, [parallel], [
            remove_existing_key,
            remove_nonexistent_key,
            remove_type_guards
        ]},
        {keys_functions, [parallel], [
            keys_empty_map,
            keys_single_map,
            keys_multiple_map,
            keys_type_guards
        ]},
        {values_functions, [parallel], [
            values_empty_map,
            values_single_map,
            values_multiple_map,
            values_type_guards
        ]}
    ].

%% --------------------------------------------------------------------
%% Helper functions
%% --------------------------------------------------------------------

create_sample_bindings() ->
    #{
        title => ~"Sample Title",
        count => 42,
        active => true
    }.

%% --------------------------------------------------------------------
%% get/2 and get/3 function tests
%% --------------------------------------------------------------------

get_success(Config) when is_list(Config) ->
    Bindings = create_sample_bindings(),

    % Test successful get operations
    ?assertEqual(~"Sample Title", arizona_binder:get(title, Bindings)),
    ?assertEqual(42, arizona_binder:get(count, Bindings)),
    ?assertEqual(true, arizona_binder:get(active, Bindings)).

get_missing_key(Config) when is_list(Config) ->
    Bindings = create_sample_bindings(),

    % Test missing key raises badkey error
    ?assertError({badkey, missing_key}, arizona_binder:get(missing_key, Bindings)).

get_type_guards(Config) when is_list(Config) ->
    Bindings = create_sample_bindings(),

    % Test type guards
    ?assertError(function_clause, arizona_binder:get(~"not_atom", Bindings)),
    ?assertError(function_clause, arizona_binder:get(title, ~"not_map")).

get_with_default_key_exists(Config) when is_list(Config) ->
    Bindings = create_sample_bindings(),

    % Test key exists - should return actual value, not default
    ?assertEqual(~"Sample Title", arizona_binder:get(title, Bindings, fun() -> ~"Default" end)),
    ?assertEqual(42, arizona_binder:get(count, Bindings, fun() -> ~"Default" end)).

get_with_default_key_missing(Config) when is_list(Config) ->
    Bindings = create_sample_bindings(),

    % Test key missing - should execute default function
    ?assertEqual(
        ~"Default Value", arizona_binder:get(missing_key, Bindings, fun() -> ~"Default Value" end)
    ).

get_with_default_nested(Config) when is_list(Config) ->
    Bindings = create_sample_bindings(),

    % Test nested default calls
    ?assertEqual(
        42,
        arizona_binder:get(missing_key, Bindings, fun() ->
            arizona_binder:get(count, Bindings)
        end)
    ).

get_with_default_type_guards(Config) when is_list(Config) ->
    Bindings = create_sample_bindings(),

    % Test type guards
    ?assertError(
        function_clause, arizona_binder:get(~"not_atom", Bindings, fun() -> ~"default" end)
    ),
    ?assertError(function_clause, arizona_binder:get(title, ~"not_map", fun() -> ~"default" end)),
    ?assertError(function_clause, arizona_binder:get(title, Bindings, ~"not_function")).

%% --------------------------------------------------------------------
%% find/2 function tests
%% --------------------------------------------------------------------

find_success(Config) when is_list(Config) ->
    Bindings = create_sample_bindings(),

    % Test successful find operations
    ?assertEqual({ok, ~"Sample Title"}, arizona_binder:find(title, Bindings)),
    ?assertEqual({ok, 42}, arizona_binder:find(count, Bindings)),
    ?assertEqual({ok, true}, arizona_binder:find(active, Bindings)).

find_missing_key(Config) when is_list(Config) ->
    Bindings = create_sample_bindings(),

    % Test missing key returns error
    ?assertEqual(error, arizona_binder:find(missing_key, Bindings)).

find_type_guards(Config) when is_list(Config) ->
    Bindings = create_sample_bindings(),

    % Test type guards
    ?assertError(function_clause, arizona_binder:find(~"not_atom", Bindings)),
    ?assertError(function_clause, arizona_binder:find(title, ~"not_map")).

%% --------------------------------------------------------------------
%% put/3 function tests
%% --------------------------------------------------------------------

put_new_key(Config) when is_list(Config) ->
    Bindings = create_sample_bindings(),

    % Test adding new key
    Result = arizona_binder:put(new_key, ~"new_value", Bindings),
    ?assertEqual(~"new_value", arizona_binder:get(new_key, Result)),
    % Original preserved
    ?assertEqual(~"Sample Title", arizona_binder:get(title, Result)).

put_existing_key(Config) when is_list(Config) ->
    Bindings = create_sample_bindings(),

    % Test updating existing key
    Result = arizona_binder:put(title, ~"Updated Title", Bindings),
    ?assertEqual(~"Updated Title", arizona_binder:get(title, Result)),
    % Other keys preserved
    ?assertEqual(42, arizona_binder:get(count, Result)).

put_type_guards(Config) when is_list(Config) ->
    Bindings = create_sample_bindings(),

    % Test type guards
    ?assertError(function_clause, arizona_binder:put(~"not_atom", ~"value", Bindings)),
    ?assertError(function_clause, arizona_binder:put(key, ~"value", ~"not_map")).

%% --------------------------------------------------------------------
%% merge/2 function tests
%% --------------------------------------------------------------------

merge_overlapping_keys(Config) when is_list(Config) ->
    Bindings1 = #{key1 => ~"value1", key2 => ~"value2"},
    Bindings2 = #{key2 => ~"updated_value2", key3 => ~"value3"},

    % Test merge with overlapping keys (second map takes precedence)
    Result = arizona_binder:merge(Bindings1, Bindings2),
    Expected = #{
        % From first map
        key1 => ~"value1",
        % From second map (precedence)
        key2 => ~"updated_value2",
        % From second map
        key3 => ~"value3"
    },
    ?assertEqual(Expected, Result).

merge_empty_maps(Config) when is_list(Config) ->
    Bindings1 = #{key1 => ~"value1"},
    Bindings2 = #{key2 => ~"value2"},

    % Test merge with empty maps
    ?assertEqual(Bindings1, arizona_binder:merge(#{}, Bindings1)),
    ?assertEqual(Bindings2, arizona_binder:merge(Bindings2, #{})).

merge_type_guards(Config) when is_list(Config) ->
    Bindings = create_sample_bindings(),

    % Test type guards
    ?assertError(function_clause, arizona_binder:merge(~"not_map", Bindings)),
    ?assertError(function_clause, arizona_binder:merge(Bindings, ~"not_map")).

%% --------------------------------------------------------------------
%% remove/2 function tests
%% --------------------------------------------------------------------

remove_existing_key(Config) when is_list(Config) ->
    Bindings = create_sample_bindings(),

    % Test removing existing key
    Result = arizona_binder:remove(title, Bindings),
    ?assertEqual(error, arizona_binder:find(title, Result)),
    % Other keys preserved
    ?assertEqual({ok, 42}, arizona_binder:find(count, Result)).

remove_nonexistent_key(Config) when is_list(Config) ->
    Bindings = create_sample_bindings(),

    % Test removing non-existent key (should not change map)
    Result = arizona_binder:remove(nonexistent_key, Bindings),
    ?assertEqual(Bindings, Result).

remove_type_guards(Config) when is_list(Config) ->
    Bindings = create_sample_bindings(),

    % Test type guards
    ?assertError(function_clause, arizona_binder:remove(~"not_atom", Bindings)),
    ?assertError(function_clause, arizona_binder:remove(title, ~"not_map")).

%% --------------------------------------------------------------------
%% keys/1 function tests
%% --------------------------------------------------------------------

keys_empty_map(Config) when is_list(Config) ->
    % Test keys extraction from empty map
    ?assertEqual([], arizona_binder:keys(#{})).

keys_single_map(Config) when is_list(Config) ->
    SingleBindings = #{single_key => ~"value"},

    % Test keys extraction from single-key map
    ?assertEqual([single_key], arizona_binder:keys(SingleBindings)).

keys_multiple_map(Config) when is_list(Config) ->
    MultiBindings = #{key1 => ~"value1", key2 => ~"value2", key3 => ~"value3"},

    % Test keys extraction from multiple-key map
    MultiKeys = arizona_binder:keys(MultiBindings),
    ?assertEqual(3, length(MultiKeys)),
    ?assert(lists:member(key1, MultiKeys)),
    ?assert(lists:member(key2, MultiKeys)),
    ?assert(lists:member(key3, MultiKeys)).

keys_type_guards(Config) when is_list(Config) ->
    % Test type guards
    ?assertError(function_clause, arizona_binder:keys(~"not_map")).

%% --------------------------------------------------------------------
%% values/1 function tests
%% --------------------------------------------------------------------

values_empty_map(Config) when is_list(Config) ->
    % Test values extraction from empty map
    ?assertEqual([], arizona_binder:values(#{})).

values_single_map(Config) when is_list(Config) ->
    SingleBindings = #{key => ~"single_value"},

    % Test values extraction from single-value map
    ?assertEqual([~"single_value"], arizona_binder:values(SingleBindings)).

values_multiple_map(Config) when is_list(Config) ->
    MultiBindings = #{key1 => ~"value1", key2 => ~"value2", key3 => ~"value3"},

    % Test values extraction from multiple-value map
    MultiValues = arizona_binder:values(MultiBindings),
    ?assertEqual(3, length(MultiValues)),
    ?assert(lists:member(~"value1", MultiValues)),
    ?assert(lists:member(~"value2", MultiValues)),
    ?assert(lists:member(~"value3", MultiValues)).

values_type_guards(Config) when is_list(Config) ->
    % Test type guards
    ?assertError(function_clause, arizona_binder:values(~"not_map")).
