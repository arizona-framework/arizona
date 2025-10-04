-module(arizona_html_SUITE).
-behaviour(ct_suite).
-include_lib("stdlib/include/assert.hrl").
-compile([export_all, nowarn_export_all]).

%% --------------------------------------------------------------------
%% Ignore dialyzer warnings
%% --------------------------------------------------------------------

-dialyzer(
    {nowarn_function, [
        to_html_tuple_error/1,
        to_html_map_error/1,
        to_html_pid_error/1
    ]}
).

%% --------------------------------------------------------------------
%% Behaviour (ct_suite) callbacks
%% --------------------------------------------------------------------

all() ->
    [
        {group, to_html_supported_types},
        {group, to_html_error_cases},
        {group, from_erl}
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
        ]},
        {from_erl, [parallel], [
            from_erl_simple_element,
            from_erl_element_with_static_attrs,
            from_erl_element_with_boolean_attrs,
            from_erl_element_with_dynamic_attr,
            from_erl_nested_elements,
            from_erl_dynamic_child,
            from_erl_escape_braces
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

%% --------------------------------------------------------------------
%% from_erl tests
%% --------------------------------------------------------------------

from_erl_simple_element(_Config) ->
    % Test: {'div', [], [~"Hello"]} -> "<div>Hello</div>"
    AST = merl:quote("{'div', [], [~\"Hello\"]}"),
    HTML = iolist_to_binary(arizona_html:from_erl(AST)),
    ?assertEqual(~"<div>Hello</div>", HTML),
    ok.

from_erl_element_with_static_attrs(_Config) ->
    % Test: {'div', [{class, ~"app"}], []} -> "<div class=\"app\" id=\"main\">Content</div>"
    AST = merl:quote("{'div', [{class, ~\"app\"}, {id, ~\"main\"}], [~\"Content\"]}"),
    HTML = iolist_to_binary(arizona_html:from_erl(AST)),
    ?assertEqual(~"<div class=\"app\" id=\"main\">Content</div>", HTML),
    ok.

from_erl_element_with_boolean_attrs(_Config) ->
    % Test: {'input', [disabled, hidden], []} -> "<input disabled type=\"text\" hidden></input>"
    AST = merl:quote("{'input', [disabled, {type, ~\"text\"}, hidden], []}"),
    HTML = iolist_to_binary(arizona_html:from_erl(AST)),
    ?assertEqual(~"<input disabled type=\"text\" hidden></input>", HTML),
    ok.

from_erl_element_with_dynamic_attr(_Config) ->
    % Test: {'div', [{id, Id}], []} -> "<div id=\"{Id}\"></div>"
    AST = merl:quote("{'div', [{id, Id}], []}"),
    HTML = iolist_to_binary(arizona_html:from_erl(AST)),
    ?assertEqual(~"<div id=\"{Id}\"></div>", HTML),
    ok.

from_erl_nested_elements(_Config) ->
    % Test: {'div', [], [{'span', [], [~"inner"]}]} -> "<div><span>inner</span></div>"
    AST = merl:quote("{'div', [], [{'span', [], [~\"inner\"]}]}"),
    HTML = iolist_to_binary(arizona_html:from_erl(AST)),
    ?assertEqual(~"<div><span>inner</span></div>", HTML),
    ok.

from_erl_dynamic_child(_Config) ->
    % Test: {'div', [], [Title]} -> "<div>{Title}</div>"
    AST = merl:quote("{'div', [], [Title]}"),
    HTML = iolist_to_binary(arizona_html:from_erl(AST)),
    ?assertEqual(~"<div>{Title}</div>", HTML),
    ok.

from_erl_escape_braces(_Config) ->
    % Test: Static { is escaped to \{
    AST = merl:quote("{'div', [{onclick, ~\"arizona.pushEvent('click', {id: 1})\"}], []}"),
    HTML = iolist_to_binary(arizona_html:from_erl(AST)),
    ?assertEqual(~"<div onclick=\"arizona.pushEvent('click', \\{id: 1})\"></div>", HTML),

    % Test: Already escaped \{ is preserved
    AST2 = merl:quote("{'div', [{onclick, ~\"arizona.pushEvent('click', \\{id: 1})\"}], []}"),
    HTML2 = iolist_to_binary(arizona_html:from_erl(AST2)),
    ?assertEqual(~"<div onclick=\"arizona.pushEvent('click', \\{id: 1})\"></div>", HTML2),

    % Test: Double quotes are escaped to \"
    AST3 = merl:quote("{'div', [{data, ~\"{\\\"key\\\": \\\"value\\\"}\"}], []}"),
    HTML3 = iolist_to_binary(arizona_html:from_erl(AST3)),
    ?assertEqual(~"<div data=\"\\{\\\"key\\\": \\\"value\\\"}\"></div>", HTML3),
    ok.
