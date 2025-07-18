-module(arizona_html_SUITE).
-behaviour(ct_suite).
-include_lib("stdlib/include/assert.hrl").
-compile([export_all, nowarn_export_all]).

%% --------------------------------------------------------------------
%% Behaviour (ct_suite) callbacks
%% --------------------------------------------------------------------

all() ->
    [
        {group, basic_types},
        {group, list_handling},
        {group, edge_cases}
    ].

groups() ->
    [
        {basic_types, [parallel], [
            to_html_binary,
            to_html_atom,
            to_html_integer,
            to_html_float,
            to_html_other_terms
        ]},
        {list_handling, [parallel], [
            to_html_empty_list,
            to_html_simple_list,
            to_html_mixed_list,
            to_html_nested_list,
            to_html_list_with_empty_elements
        ]},
        {edge_cases, [parallel], [
            to_html_unicode_atom,
            to_html_large_integer,
            to_html_negative_number,
            to_html_complex_term,
            to_html_deeply_nested
        ]}
    ].

%% --------------------------------------------------------------------
%% Basic Type Tests
%% --------------------------------------------------------------------

to_html_binary(Config) when is_list(Config) ->
    % Test binary passthrough
    ?assertEqual(~"hello", arizona_html:to_html(~"hello")),
    ?assertEqual(~"", arizona_html:to_html(~"")),
    ?assertEqual(~"<div>content</div>", arizona_html:to_html(~"<div>content</div>")).

to_html_atom(Config) when is_list(Config) ->
    % Test atom conversion
    ?assertEqual(~"hello", arizona_html:to_html(hello)),
    ?assertEqual(~"test_atom", arizona_html:to_html(test_atom)),
    ?assertEqual(~"true", arizona_html:to_html(true)),
    ?assertEqual(~"false", arizona_html:to_html(false)).

to_html_integer(Config) when is_list(Config) ->
    % Test integer conversion
    ?assertEqual(~"42", arizona_html:to_html(42)),
    ?assertEqual(~"0", arizona_html:to_html(0)),
    ?assertEqual(~"-123", arizona_html:to_html(-123)),
    ?assertEqual(~"999999", arizona_html:to_html(999999)).

to_html_float(Config) when is_list(Config) ->
    % Test float conversion
    ?assertEqual(~"3.14", arizona_html:to_html(3.14)),
    ?assertEqual(~"0.0", arizona_html:to_html(0.0)),
    ?assertEqual(~"-2.5", arizona_html:to_html(-2.5)).

to_html_other_terms(Config) when is_list(Config) ->
    % Test other term conversion using ~tp format
    Tuple = {hello, world},
    Result = arizona_html:to_html(Tuple),
    ?assert(is_binary(Result)),
    ?assertMatch(~"{hello,world}", Result).

%% --------------------------------------------------------------------
%% List Handling Tests
%% --------------------------------------------------------------------

to_html_empty_list(Config) when is_list(Config) ->
    % Test empty list
    ?assertEqual([], arizona_html:to_html([])).

to_html_simple_list(Config) when is_list(Config) ->
    % Test simple list conversion
    Result = arizona_html:to_html([hello, world]),
    ?assertEqual([~"hello", ~"world"], Result).

to_html_mixed_list(Config) when is_list(Config) ->
    % Test list with mixed types
    Result = arizona_html:to_html([hello, 42, ~"test"]),
    ?assertEqual([~"hello", ~"42", ~"test"], Result).

to_html_nested_list(Config) when is_list(Config) ->
    % Test nested list conversion
    Result = arizona_html:to_html([hello, [world, 123]]),
    Expected = [~"hello", [~"world", ~"123"]],
    ?assertEqual(Expected, Result).

to_html_list_with_empty_elements(Config) when is_list(Config) ->
    % Test list with empty elements that get filtered
    Result = arizona_html:to_html([hello, [], world]),
    Expected = [~"hello", ~"world"],
    ?assertEqual(Expected, Result).

%% --------------------------------------------------------------------
%% Edge Case Tests
%% --------------------------------------------------------------------

to_html_unicode_atom(Config) when is_list(Config) ->
    % Test unicode atom conversion
    UnicodeAtom = binary_to_atom(~"café", utf8),
    Result = arizona_html:to_html(UnicodeAtom),
    ?assertEqual(~"café", Result).

to_html_large_integer(Config) when is_list(Config) ->
    % Test large integer conversion
    LargeInt = 123456789012345,
    Result = arizona_html:to_html(LargeInt),
    ?assertEqual(~"123456789012345", Result).

to_html_negative_number(Config) when is_list(Config) ->
    % Test negative numbers
    ?assertEqual(~"-42", arizona_html:to_html(-42)),
    ?assertEqual(~"-3.14", arizona_html:to_html(-3.14)).

to_html_complex_term(Config) when is_list(Config) ->
    % Test complex term conversion
    ComplexTerm = #{key => value, num => 42},
    Result = arizona_html:to_html(ComplexTerm),
    ?assert(is_binary(Result)),
    % Result should contain the map representation
    ?assert(binary:match(Result, ~"key") =/= nomatch),
    ?assert(binary:match(Result, ~"value") =/= nomatch).

to_html_deeply_nested(Config) when is_list(Config) ->
    % Test deeply nested structure
    Nested = [~"start", [atom1, [42, [~"deep"]], atom2], ~"end"],
    Result = arizona_html:to_html(Nested),
    Expected = [~"start", [~"atom1", [~"42", [~"deep"]], ~"atom2"], ~"end"],
    ?assertEqual(Expected, Result).
