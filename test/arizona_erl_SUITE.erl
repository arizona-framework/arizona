-module(arizona_erl_SUITE).
-behaviour(ct_suite).
-include_lib("stdlib/include/assert.hrl").
-compile([export_all, nowarn_export_all]).

%% --------------------------------------------------------------------
%% Behaviour (ct_suite) callbacks
%% --------------------------------------------------------------------

all() ->
    [
        {group, to_html},
        {group, ast_to_html}
    ].

groups() ->
    [
        {to_html, [parallel], [
            to_html_simple_element,
            to_html_element_with_attrs,
            to_html_element_with_boolean_attrs,
            to_html_void_elements,
            to_html_nested_elements,
            to_html_list_of_elements,
            to_html_escape_special_chars,
            to_html_with_various_types,
            to_html_single_binary_child,
            to_html_single_element_child,
            to_html_single_atom_child
        ]},
        {ast_to_html, [parallel], [
            ast_to_html_simple_element,
            ast_to_html_element_with_static_attrs,
            ast_to_html_element_with_boolean_attrs,
            ast_to_html_element_with_dynamic_attr,
            ast_to_html_nested_elements,
            ast_to_html_dynamic_child,
            ast_to_html_list_of_elements,
            ast_to_html_escape_braces,
            ast_to_html_single_binary_child,
            ast_to_html_single_element_child,
            ast_to_html_single_dynamic_child
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
    ct:comment("Void element with boolean attributes"),
    HTML = arizona_erl:to_html({input, [disabled, {type, ~"text"}, hidden], []}),
    ?assertEqual(~"<input disabled type=\"text\" hidden />", HTML),
    ok.

to_html_void_elements(_Config) ->
    ct:comment("Void elements are self-closing (XHTML compatible)"),

    % Test various void elements
    ?assertEqual(~"<br />", arizona_erl:to_html({br, [], []})),
    ?assertEqual(~"<hr />", arizona_erl:to_html({hr, [], []})),
    ?assertEqual(~"<img src=\"test.jpg\" />", arizona_erl:to_html({img, [{src, ~"test.jpg"}], []})),
    ?assertEqual(
        ~"<link rel=\"stylesheet\" />", arizona_erl:to_html({link, [{rel, ~"stylesheet"}], []})
    ),
    ?assertEqual(
        ~"<meta charset=\"utf-8\" />", arizona_erl:to_html({meta, [{charset, ~"utf-8"}], []})
    ),

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

%% --------------------------------------------------------------------
%% AST conversion tests
%% --------------------------------------------------------------------

ast_to_html_simple_element(_Config) ->
    ct:comment("Convert simple element AST to HTML"),
    AST = merl:quote("{'div', [], [~\"Hello\"]}"),
    HTML = iolist_to_binary(arizona_erl:ast_to_html(AST)),
    ?assertEqual(~"<div>Hello</div>", HTML),
    ok.

ast_to_html_element_with_static_attrs(_Config) ->
    ct:comment("Convert element with static attributes"),
    AST = merl:quote("{'div', [{class, ~\"app\"}, {id, ~\"main\"}], [~\"Content\"]}"),
    HTML = iolist_to_binary(arizona_erl:ast_to_html(AST)),
    ?assertEqual(~"<div class=\"app\" id=\"main\">Content</div>", HTML),
    ok.

ast_to_html_element_with_boolean_attrs(_Config) ->
    ct:comment("Convert void element with boolean attributes"),
    AST = merl:quote("{input, [disabled, {type, ~\"text\"}, hidden], []}"),
    HTML = iolist_to_binary(arizona_erl:ast_to_html(AST)),
    ?assertEqual(~"<input disabled type=\"text\" hidden />", HTML),
    ok.

ast_to_html_element_with_dynamic_attr(_Config) ->
    ct:comment("Convert element with dynamic attribute to {Expr}"),
    AST = merl:quote("{'div', [{id, Id}], []}"),
    HTML = iolist_to_binary(arizona_erl:ast_to_html(AST)),
    ?assertEqual(~"<div id=\"{Id}\"></div>", HTML),
    ok.

ast_to_html_nested_elements(_Config) ->
    ct:comment("Convert nested elements"),
    AST = merl:quote("{'div', [], [{span, [], [~\"inner\"]}]}"),
    HTML = iolist_to_binary(arizona_erl:ast_to_html(AST)),
    ?assertEqual(~"<div><span>inner</span></div>", HTML),
    ok.

ast_to_html_dynamic_child(_Config) ->
    ct:comment("Convert dynamic child to {Expr}"),
    AST = merl:quote("{'div', [], [Title]}"),
    HTML = iolist_to_binary(arizona_erl:ast_to_html(AST)),
    ?assertEqual(~"<div>{Title}</div>", HTML),
    ok.

ast_to_html_list_of_elements(_Config) ->
    ct:comment("Convert list of elements"),
    AST = merl:quote("[{'div', [], [~\"First\"]}, {span, [], [~\"Second\"]}]"),
    HTML = iolist_to_binary(arizona_erl:ast_to_html(AST)),
    ?assertEqual(~"<div>First</div><span>Second</span>", HTML),
    ok.

ast_to_html_escape_braces(_Config) ->
    ct:comment("Escape { and \" in static content"),

    % Test { escaping in attributes
    AST1 = merl:quote("{'div', [{onclick, ~\"arizona.pushEvent('click', {id: 1})\"}], []}"),
    HTML1 = iolist_to_binary(arizona_erl:ast_to_html(AST1)),
    ?assertEqual(~"<div onclick=\"arizona.pushEvent('click', \\{id: 1})\"></div>", HTML1),

    % Test already escaped \{ is preserved
    AST2 = merl:quote("{'div', [{onclick, ~\"arizona.pushEvent('click', \\{id: 1})\"}], []}"),
    HTML2 = iolist_to_binary(arizona_erl:ast_to_html(AST2)),
    ?assertEqual(~"<div onclick=\"arizona.pushEvent('click', \\{id: 1})\"></div>", HTML2),

    % Test " escaping
    AST3 = merl:quote("{'div', [{data, ~\"{\\\"key\\\": \\\"value\\\"}\"}], []}"),
    HTML3 = iolist_to_binary(arizona_erl:ast_to_html(AST3)),
    ?assertEqual(~"<div data=\"\\{\\\"key\\\": \\\"value\\\"}\"></div>", HTML3),

    ok.

%% --------------------------------------------------------------------
%% Single child tests (runtime)
%% --------------------------------------------------------------------

to_html_single_binary_child(_Config) ->
    ct:comment("Element with single binary child (not in a list)"),
    HTML = arizona_erl:to_html({'div', [], ~"Hello"}),
    ?assertEqual(~"<div>Hello</div>", HTML),
    ok.

to_html_single_element_child(_Config) ->
    ct:comment("Element with single element child (not in a list)"),
    HTML = arizona_erl:to_html({'div', [], {span, [], [~"inner"]}}),
    ?assertEqual(~"<div><span>inner</span></div>", HTML),
    ok.

to_html_single_atom_child(_Config) ->
    ct:comment("Element with single atom child (not in a list)"),
    HTML = arizona_erl:to_html({'div', [], hello}),
    ?assertEqual(~"<div>hello</div>", HTML),
    ok.

%% --------------------------------------------------------------------
%% Single child tests (AST)
%% --------------------------------------------------------------------

ast_to_html_single_binary_child(_Config) ->
    ct:comment("Convert element with single binary child AST"),
    AST = merl:quote("{'div', [], ~\"Hello\"}"),
    HTML = iolist_to_binary(arizona_erl:ast_to_html(AST)),
    ?assertEqual(~"<div>Hello</div>", HTML),
    ok.

ast_to_html_single_element_child(_Config) ->
    ct:comment("Convert element with single element child AST"),
    AST = merl:quote("{'div', [], {span, [], [~\"inner\"]}}"),
    HTML = iolist_to_binary(arizona_erl:ast_to_html(AST)),
    ?assertEqual(~"<div><span>inner</span></div>", HTML),
    ok.

ast_to_html_single_dynamic_child(_Config) ->
    ct:comment("Convert element with single dynamic child AST"),
    AST = merl:quote("{'div', [], Title}"),
    HTML = iolist_to_binary(arizona_erl:ast_to_html(AST)),
    ?assertEqual(~"<div>{Title}</div>", HTML),
    ok.
