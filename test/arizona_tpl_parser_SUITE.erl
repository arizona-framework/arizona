-module(arizona_tpl_parser_SUITE).
-behaviour(ct_suite).
-include_lib("stdlib/include/assert.hrl").
-compile([export_all, nowarn_export_all]).

%% --------------------------------------------------------------------
%% Behaviour (ct_suite) callbacks
%% --------------------------------------------------------------------

all() ->
    [{group, parse}].

groups() ->
    [
        {parse, [parallel], [
            parse
        ]}
    ].

%% --------------------------------------------------------------------
%% Tests
%% --------------------------------------------------------------------

parse(Config) when is_list(Config) ->
    Expect = {
        _Static = [
            {bin, 1, [{bin_element, 1, {string, 1, "foo"}, default, [utf8]}]},
            {bin, 1, [{bin_element, 1, {string, 1, []}, default, [utf8]}]},
            {bin, 1, [{bin_element, 1, {string, 1, "bar"}, default, [utf8]}]},
            {bin, 1, [{bin_element, 1, {string, 1, "qu\"o\"t\\\"ed"}, default, [utf8]}]}
        ],
        _Dynamic = [
            {atom, 1, foo},
            {tuple, 1, [{atom, 1, bar}]},
            {cons, 1, {atom, 1, bar}, {nil, 1}}
        ]
    },
    Tokens = arizona_tpl_scanner:scan(#{}, ~"""
    foo{foo}{{bar}}{% drop this }bar{[bar]}qu"o"t\"ed
    """),
    Got = arizona_tpl_parser:parse(Tokens),
    ?assertEqual(Expect, Got).
