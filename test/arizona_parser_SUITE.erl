-module(arizona_parser_SUITE).
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
            {'fun', 1,
                {clauses, [
                    {clause, 1, [{var, 1, 'ViewAcc'}, {var, 1, 'Socket'}], [], [
                        {call, 2, {remote, 2, {atom, 2, arizona_render}, {atom, 2, render}}, [
                            {atom, 2, foo},
                            {var, 2, 'View'},
                            {var, 2, 'ViewAcc'},
                            {var, 2, 'Socket'}
                        ]}
                    ]}
                ]}},
            {'fun', 1,
                {clauses, [
                    {clause, 1, [{var, 1, 'ViewAcc'}, {var, 1, 'Socket'}], [], [
                        {call, 2, {remote, 2, {atom, 2, arizona_render}, {atom, 2, render}}, [
                            {tuple, 2, [{atom, 2, bar}]},
                            {var, 2, 'View'},
                            {var, 2, 'ViewAcc'},
                            {var, 2, 'Socket'}
                        ]}
                    ]}
                ]}},
            {'fun', 1,
                {clauses, [
                    {clause, 1, [{var, 1, 'ViewAcc'}, {var, 1, 'Socket'}], [], [
                        {call, 2, {remote, 2, {atom, 2, arizona_render}, {atom, 2, render}}, [
                            {cons, 2, {atom, 2, bar}, {nil, 2}},
                            {var, 2, 'View'},
                            {var, 2, 'ViewAcc'},
                            {var, 2, 'Socket'}
                        ]}
                    ]}
                ]}}
        ]
    },
    Tokens = arizona_scanner:scan(#{}, ~"""
    foo{foo}{{bar}}{% drop this }bar{[bar]}qu"o"t\"ed
    """),
    Got = arizona_parser:parse(Tokens),
    ?assertEqual(Expect, Got).
