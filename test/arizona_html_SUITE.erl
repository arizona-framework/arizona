-module(arizona_html_SUITE).
-behaviour(ct_suite).
-include_lib("stdlib/include/assert.hrl").
-compile([export_all, nowarn_export_all]).
-dialyzer({nowarn_function, [to_html_tuple_error/1, to_html_map_error/1, to_html_pid_error/1]}).

%% --------------------------------------------------------------------
%% Behaviour (ct_suite) callbacks
%% --------------------------------------------------------------------

all() ->
    [
        {group, to_html_supported_types},
        {group, to_html_error_cases}
    ].

groups() ->
    [
        {to_html_supported_types, [parallel], [
            to_html_binary,
            to_html_atom,
            to_html_integer,
            to_html_float,
            to_html_iolist
        ]},
        {to_html_error_cases, [parallel], [
            to_html_tuple_error,
            to_html_map_error,
            to_html_pid_error
        ]}
    ].

%% --------------------------------------------------------------------
%% Supported types tests
%% --------------------------------------------------------------------

to_html_binary(Config) when is_list(Config) ->
    ct:comment("Binary values should be returned as-is"),
    ?assertEqual(~"hello", arizona_html:to_html(~"hello")),
    ?assertEqual(~"", arizona_html:to_html(~"")),
    ?assertEqual(~"test with spaces", arizona_html:to_html(~"test with spaces")).

to_html_atom(Config) when is_list(Config) ->
    ct:comment("Atoms should be converted to binary"),
    ?assertEqual(~"hello", arizona_html:to_html(hello)),
    ?assertEqual(~"undefined", arizona_html:to_html(undefined)),
    ?assertEqual(~"test", arizona_html:to_html(test)).

to_html_integer(Config) when is_list(Config) ->
    ct:comment("Integers should be converted to binary"),
    ?assertEqual(~"123", arizona_html:to_html(123)),
    ?assertEqual(~"0", arizona_html:to_html(0)),
    ?assertEqual(~"0", arizona_html:to_html(+0)),
    ?assertEqual(~"-123", arizona_html:to_html(-123)).

to_html_float(Config) when is_list(Config) ->
    ct:comment("Floats should be converted to binary using ~p format"),
    ?assertEqual(~"1.23", arizona_html:to_html(1.23)),
    ?assertEqual(~"0.0", arizona_html:to_html(0.0)),
    ?assertEqual(~"0.0", arizona_html:to_html(+0.0)),
    ?assertEqual(~"-1.23", arizona_html:to_html(-1.23)).

to_html_iolist(Config) when is_list(Config) ->
    ct:comment("Lists should be assumed to be iolists and returned as-is"),
    ?assertEqual([], arizona_html:to_html([])),
    ?assertEqual([~"hello", ~"world"], arizona_html:to_html([~"hello", ~"world"])),
    ?assertEqual([~"hello", 32, ~"world"], arizona_html:to_html([~"hello", 32, ~"world"])).

%% --------------------------------------------------------------------
%% Error cases tests
%% --------------------------------------------------------------------

to_html_tuple_error(Config) when is_list(Config) ->
    ct:comment("Tuples should cause function_clause error"),
    ?assertError(function_clause, arizona_html:to_html({hello, world})).

to_html_map_error(Config) when is_list(Config) ->
    ct:comment("Maps should cause function_clause error"),
    ?assertError(function_clause, arizona_html:to_html(#{key => value})).

to_html_pid_error(Config) when is_list(Config) ->
    ct:comment("PIDs should cause function_clause error"),
    ?assertError(function_clause, arizona_html:to_html(self())).
