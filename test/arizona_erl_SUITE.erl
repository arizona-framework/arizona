-module(arizona_erl_SUITE).
-behaviour(ct_suite).
-include_lib("stdlib/include/assert.hrl").
-compile([export_all, nowarn_export_all]).

%% --------------------------------------------------------------------
%% Behaviour (ct_suite) callbacks
%% --------------------------------------------------------------------

all() ->
    [
        {group, to_html}
    ].

groups() ->
    [
        {to_html, [parallel], [
            to_html_simple_element,
            to_html_element_with_attrs,
            to_html_element_with_boolean_attrs,
            to_html_nested_elements,
            to_html_list_of_elements,
            to_html_escape_special_chars,
            to_html_with_various_types
        ]}
    ].

%% --------------------------------------------------------------------
%% Test cases
%% --------------------------------------------------------------------

to_html_simple_element(_Config) ->
    ct:comment("Simple element with text child"),
    HTML = arizona_erl:to_html({'div', [], [~"Hello"]}),
    ?assertEqual(~"<div>Hello</div>", HTML),
    ok.

to_html_element_with_attrs(_Config) ->
    ct:comment("Element with static attributes"),
    HTML = arizona_erl:to_html({'div', [{class, ~"app"}, {id, ~"main"}], [~"Content"]}),
    ?assertEqual(~"<div class=\"app\" id=\"main\">Content</div>", HTML),
    ok.

to_html_element_with_boolean_attrs(_Config) ->
    ct:comment("Element with boolean attributes"),
    HTML = arizona_erl:to_html({input, [disabled, {type, ~"text"}, hidden], []}),
    ?assertEqual(~"<input disabled type=\"text\" hidden></input>", HTML),
    ok.

to_html_nested_elements(_Config) ->
    ct:comment("Nested elements"),
    HTML = arizona_erl:to_html({'div', [], [{span, [], [~"inner"]}]}),
    ?assertEqual(~"<div><span>inner</span></div>", HTML),
    ok.

to_html_list_of_elements(_Config) ->
    ct:comment("List of multiple elements"),
    HTML = arizona_erl:to_html([
        {'div', [], [~"First"]},
        {span, [], [~"Second"]}
    ]),
    ?assertEqual(~"<div>First</div><span>Second</span>", HTML),
    ok.

to_html_escape_special_chars(_Config) ->
    ct:comment("No escaping - just convert to HTML"),

    % Test regular HTML conversion without escaping
    HTML1 = arizona_erl:to_html({'div', [{onclick, ~"arizona.pushEvent('click', {id: 1})"}], []}),
    ?assertEqual(~"<div onclick=\"arizona.pushEvent('click', {id: 1})\"></div>", HTML1),

    % Test children conversion
    HTML2 = arizona_erl:to_html({'div', [], [~"Use {variable} here"]}),
    ?assertEqual(~"<div>Use {variable} here</div>", HTML2),

    ok.

to_html_with_various_types(_Config) ->
    ct:comment("Handle various Erlang types in children and attributes"),

    % Integer child
    HTML1 = arizona_erl:to_html({'div', [], [123]}),
    ?assertEqual(~"<div>123</div>", HTML1),

    % Atom child
    HTML2 = arizona_erl:to_html({'div', [], [hello]}),
    ?assertEqual(~"<div>hello</div>", HTML2),

    % Float child
    HTML3 = arizona_erl:to_html({'div', [], [3.14]}),
    ?assert(binary:match(HTML3, ~"3.14") =/= nomatch),

    % Integer attribute
    HTML4 = arizona_erl:to_html({'div', [{width, 100}], []}),
    ?assertEqual(~"<div width=\"100\"></div>", HTML4),

    ok.
