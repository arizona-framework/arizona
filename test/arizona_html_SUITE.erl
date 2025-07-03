-module(arizona_html_SUITE).
-behaviour(ct_suite).
-include_lib("stdlib/include/assert.hrl").
-compile([export_all, nowarn_export_all]).

%% --------------------------------------------------------------------
%% Behaviour (ct_suite) callbacks
%% --------------------------------------------------------------------

all() ->
    [
        {group, render_stateful_tests},
        {group, render_stateless_tests},
        {group, render_list_tests},
        {group, to_html_tests}
    ].

groups() ->
    [
        {render_stateful_tests, [parallel], [
            test_render_stateful_with_template_data,
            test_render_stateful_with_binary_html,
            test_render_stateful_with_list_html,
            test_render_stateful_complex_template,
            test_render_stateful_nested_calls
        ]},
        {render_stateless_tests, [parallel], [
            test_render_stateless_with_structured_list,
            test_render_stateless_with_binary_html,
            test_render_stateless_with_list_html,
            test_render_stateless_complex_template
        ]},
        {render_list_tests, [parallel], [
            test_render_list_with_list_data,
            test_render_list_with_item_function,
            test_render_list_empty_items,
            test_render_list_multiple_items,
            test_render_list_with_nested_html_calls,
            test_render_list_with_variable_extraction
        ]},
        {to_html_tests, [parallel], [
            test_to_html_binary,
            test_to_html_list,
            test_to_html_atom,
            test_to_html_integer,
            test_to_html_float,
            test_to_html_complex_term,
            test_to_html_socket
        ]}
    ].

%% --------------------------------------------------------------------
%% Render stateful tests
%% --------------------------------------------------------------------

test_render_stateful_with_template_data(Config) when is_list(Config) ->
    TemplateData = #{
        elems_order => [0, 1],
        elems => #{
            0 => {static, 1, ~"<h1>"},
            1 => {static, 1, ~"Hello</h1>"}
        },
        vars_indexes => #{}
    },
    Socket = create_mock_socket(),

    UpdatedSocket = arizona_html:render_stateful(TemplateData, Socket),

    ?assert(arizona_socket:is_socket(UpdatedSocket)),
    Html = arizona_socket:get_html(UpdatedSocket),
    Expected = ~"<h1>Hello</h1>",
    ?assertEqual(Expected, iolist_to_binary(Html)).

test_render_stateful_with_binary_html(Config) when is_list(Config) ->
    Html = ~"<div>Static content</div>",
    Socket = create_mock_socket(),

    UpdatedSocket = arizona_html:render_stateful(Html, Socket),

    ?assert(arizona_socket:is_socket(UpdatedSocket)),
    ResultHtml = arizona_socket:get_html(UpdatedSocket),
    Expected = ~"<div>Static content</div>",
    ?assertEqual(Expected, iolist_to_binary(ResultHtml)).

test_render_stateful_with_list_html(Config) when is_list(Config) ->
    Html = [~"<p>", ~"List content", ~"</p>"],
    Socket = create_mock_socket(),

    UpdatedSocket = arizona_html:render_stateful(Html, Socket),

    ?assert(arizona_socket:is_socket(UpdatedSocket)),
    ResultHtml = arizona_socket:get_html(UpdatedSocket),
    Expected = ~"<p>List content</p>",
    ?assertEqual(Expected, iolist_to_binary(ResultHtml)).

test_render_stateful_complex_template(Config) when is_list(Config) ->
    % Test with a more complex template structure using only static elements
    % to avoid dialyzer issues with dynamic function types in test data
    TemplateData = #{
        elems_order => [0, 1, 2, 3, 4],
        elems => #{
            0 => {static, 1, ~"<div class=\"header\">"},
            1 => {static, 1, ~"<h1>"},
            2 => {static, 1, ~"Title"},
            3 => {static, 1, ~"</h1>"},
            4 => {static, 1, ~"</div>"}
        },
        vars_indexes => #{}
    },
    Socket = create_mock_socket(),

    UpdatedSocket = arizona_html:render_stateful(TemplateData, Socket),

    ?assert(arizona_socket:is_socket(UpdatedSocket)),
    ResultHtml = arizona_socket:get_html(UpdatedSocket),
    Expected = ~"<div class=\"header\"><h1>Title</h1></div>",
    ?assertEqual(Expected, iolist_to_binary(ResultHtml)).

test_render_stateful_nested_calls(Config) when is_list(Config) ->
    % Test nested arizona_html calls within dynamic expressions
    % This verifies the bug fix for socket returns from dynamic functions
    Socket = arizona_socket:new(#{}),

    % Template with deeply nested arizona_html calls
    Template = ~"""""
    <div>
        Level 0: {
            _JustForm = multiple_forms,
            arizona_html:render_stateless(~""""
            <span>Level 1:
            {arizona_html:render_stateless(~"""
            <p>Level 2: Deep nesting test</p>
            """, Socket)}
            </span>
            """", Socket)
        }
    </div>
    """"",

    % Render the nested template
    UpdatedSocket = arizona_html:render_stateful(Template, Socket),

    % Verify socket is returned and extract HTML
    ?assert(arizona_socket:is_socket(UpdatedSocket)),
    ResultHtml = arizona_socket:get_html(UpdatedSocket),

    % Expected nested iolist structure (not socket records!)
    ExpectedHtml = [
        ~"<div>\n    Level 0: ",
        [
            ~"<span>Level 1: ",
            [~"<p>Level 2: Deep nesting test</p>"],
            ~"</span>"
        ],
        ~"</div>"
    ],

    ?assertEqual(ExpectedHtml, ResultHtml).

%% --------------------------------------------------------------------
%% Render stateless tests
%% --------------------------------------------------------------------

test_render_stateless_with_structured_list(Config) when is_list(Config) ->
    StructuredList = [
        {static, 1, ~"<span>"},
        {dynamic, 1, fun(_Socket) -> ~"content" end},
        {static, 1, ~"</span>"}
    ],
    Socket = create_mock_socket(),

    UpdatedSocket = arizona_html:render_stateless(StructuredList, Socket),

    ?assert(arizona_socket:is_socket(UpdatedSocket)),
    Html = arizona_socket:get_html(UpdatedSocket),
    Expected = ~"<span>content</span>",
    ?assertEqual(Expected, iolist_to_binary(Html)).

test_render_stateless_with_binary_html(Config) when is_list(Config) ->
    Html = ~"<nav>Navigation</nav>",
    Socket = create_mock_socket(),

    UpdatedSocket = arizona_html:render_stateless(Html, Socket),

    ?assert(arizona_socket:is_socket(UpdatedSocket)),
    ResultHtml = arizona_socket:get_html(UpdatedSocket),
    Expected = ~"<nav>Navigation</nav>",
    ?assertEqual(Expected, iolist_to_binary(ResultHtml)).

test_render_stateless_with_list_html(Config) when is_list(Config) ->
    Html = ~"<header>Header content</header>",
    Socket = create_mock_socket(),

    UpdatedSocket = arizona_html:render_stateless(Html, Socket),

    ?assert(arizona_socket:is_socket(UpdatedSocket)),
    ResultHtml = arizona_socket:get_html(UpdatedSocket),
    Expected = ~"<header>Header content</header>",
    ?assertEqual(Expected, iolist_to_binary(ResultHtml)).

test_render_stateless_complex_template(Config) when is_list(Config) ->
    % Use pre-structured list instead of raw HTML with expressions
    StructuredList = [
        {static, 1, ~"<section>"},
        {dynamic, 1, fun(_Socket) -> ~"My Title" end},
        {static, 1, ~"</section>"}
    ],
    Socket = create_mock_socket(),

    UpdatedSocket = arizona_html:render_stateless(StructuredList, Socket),

    ?assert(arizona_socket:is_socket(UpdatedSocket)),
    ResultHtml = arizona_socket:get_html(UpdatedSocket),
    Expected = ~"<section>My Title</section>",
    ?assertEqual(Expected, iolist_to_binary(ResultHtml)).

%% --------------------------------------------------------------------
%% Render list tests
%% --------------------------------------------------------------------

test_render_list_with_list_data(Config) when is_list(Config) ->
    ListData = #{
        static => [~"<li>", ~"", ~"</li>"],
        dynamic => #{
            elems_order => [0],
            elems => #{0 => {dynamic, 1, fun(Item, _Socket) -> Item end}},
            vars_indexes => #{}
        }
    },
    Items = [~"Item1", ~"Item2"],
    Socket = create_mock_socket(),

    UpdatedSocket = arizona_html:render_list(ListData, Items, Socket),

    ?assert(arizona_socket:is_socket(UpdatedSocket)),
    Html = arizona_socket:get_html(UpdatedSocket),
    Expected = ~"<li>Item1</li><li>Item2</li>",
    ?assertEqual(Expected, iolist_to_binary(Html)).

test_render_list_with_item_function(Config) when is_list(Config) ->
    ItemFun = fun(Item) -> [~"<div>", Item, ~"</div>"] end,
    Items = [~"First", ~"Second"],
    Socket = create_mock_socket(),

    UpdatedSocket = arizona_html:render_list(ItemFun, Items, Socket),

    ?assert(arizona_socket:is_socket(UpdatedSocket)),
    Html = arizona_socket:get_html(UpdatedSocket),
    Expected = ~"<div>First</div><div>Second</div>",
    ?assertEqual(Expected, iolist_to_binary(Html)).

test_render_list_empty_items(Config) when is_list(Config) ->
    ItemFun = fun(Item) -> [~"<p>", Item, ~"</p>"] end,
    Items = [],
    Socket = create_mock_socket(),

    UpdatedSocket = arizona_html:render_list(ItemFun, Items, Socket),

    ?assert(arizona_socket:is_socket(UpdatedSocket)),
    Html = arizona_socket:get_html(UpdatedSocket),
    Expected = ~"",
    ?assertEqual(Expected, iolist_to_binary(Html)).

test_render_list_multiple_items(Config) when is_list(Config) ->
    ItemFun = fun(Item) -> [~"<option>", Item, ~"</option>"] end,
    Items = [~"A", ~"B", ~"C"],
    Socket = create_mock_socket(),

    UpdatedSocket = arizona_html:render_list(ItemFun, Items, Socket),

    ?assert(arizona_socket:is_socket(UpdatedSocket)),
    Html = arizona_socket:get_html(UpdatedSocket),
    Expected = ~"<option>A</option><option>B</option><option>C</option>",
    ?assertEqual(Expected, iolist_to_binary(Html)).

test_render_list_with_nested_html_calls(Config) when is_list(Config) ->
    % Test render_list with ListData where element functions call arizona_html
    % This ensures coverage for arizona_renderer.erl lines 169-170 (socket handling)
    ListData = #{
        static => [~"<li>", ~"</li>"],
        dynamic => #{
            elems_order => [0],
            elems => #{
                0 =>
                    {dynamic, 1, fun(Item, Socket) ->
                        % Element function that calls arizona_html and returns a socket
                        arizona_html:render_stateless(
                            [
                                {static, 1, ~"<span>"},
                                {dynamic, 1, fun(_Socket) -> Item end},
                                {static, 1, ~"</span>"}
                            ],
                            Socket
                        )
                    end}
            },
            vars_indexes => #{}
        }
    },
    Items = [~"Item1", ~"Item2"],
    Socket = create_mock_socket(),

    UpdatedSocket = arizona_html:render_list(ListData, Items, Socket),

    % Verify socket is returned and contains expected HTML structure
    ?assert(arizona_socket:is_socket(UpdatedSocket)),
    Html = arizona_socket:get_html(UpdatedSocket),

    % Expected structure: the element function returns a socket which should be handled properly
    % The actual HTML structure shows nested content from arizona_html calls
    ExpectedHtml = [
        [
            [],
            [~"<li>", [~"<span>", ~"Item1", ~"</span>"], ~"</li>"]
        ],
        [~"<li>", [~"<span>", ~"Item2", ~"</span>"], ~"</li>"]
    ],

    ?assertEqual(ExpectedHtml, Html).

test_render_list_with_variable_extraction(Config) when is_list(Config) ->
    % Test variable extraction and binding like the user's example
    % This verifies that extract_list_item_var_name works and variables are bound correctly

    % Create socket with stateful state containing prefix binding
    Stateful = arizona_stateful:new(root, undefined, #{prefix => ~"foo"}),
    Socket = arizona_socket:put_stateful_state(Stateful, arizona_socket:new(#{})),

    % Template with nested render_list call using variable I
    Template = ~""""
    <ul>
    {arizona_html:render_list(fun(I) -> ~"""
    <li>{arizona_socket:get_binding(prefix, Socket)}_{I}</li>
    """ end, [1,2,3], Socket)}
    </ul>
    """",

    % Render the template
    ResultSocket = arizona_html:render_stateless(Template, Socket),

    % Verify socket is returned and extract HTML
    ?assert(arizona_socket:is_socket(ResultSocket)),
    Html = arizona_socket:get_html(ResultSocket),

    % Convert to binary to verify actual string output
    FlatHtml = iolist_to_binary(Html),

    % Expected output: <ul> followed by list items with prefix_value pattern
    ExpectedHtml = ~"<ul> <li>foo_1</li><li>foo_2</li><li>foo_3</li></ul>",

    ?assertEqual(ExpectedHtml, FlatHtml).

%% --------------------------------------------------------------------
%% to_html tests
%% --------------------------------------------------------------------

test_to_html_binary(Config) when is_list(Config) ->
    {Result, _Socket} = arizona_html:to_html(~"Hello World", create_mock_socket()),
    ?assertEqual(~"Hello World", Result).

test_to_html_list(Config) when is_list(Config) ->
    {Result, _Socket} = arizona_html:to_html([~"<div>", content, ~"</div>"], create_mock_socket()),
    ?assertEqual([[[[], ~"<div>"], ~"content"], ~"</div>"], Result).

test_to_html_atom(Config) when is_list(Config) ->
    {Result, _Socket} = arizona_html:to_html(hello, create_mock_socket()),
    ?assertEqual(~"hello", Result).

test_to_html_integer(Config) when is_list(Config) ->
    {Result, _Socket} = arizona_html:to_html(42, create_mock_socket()),
    ?assertEqual(~"42", Result).

test_to_html_float(Config) when is_list(Config) ->
    {Result, _Socket} = arizona_html:to_html(3.14, create_mock_socket()),
    Expected = ~"3.14",
    ?assertEqual(Expected, Result).

test_to_html_complex_term(Config) when is_list(Config) ->
    {Result, _Socket} = arizona_html:to_html({error, not_found}, create_mock_socket()),
    Expected = ~"{error,not_found}",
    ?assertEqual(Expected, Result).

test_to_html_socket(Config) when is_list(Config) ->
    % Test that to_html/2 properly handles socket values
    % Create a socket with some HTML content
    Socket1 = create_mock_socket(),
    HtmlContent = [~"<div>", ~"Socket HTML content", ~"</div>"],
    SocketWithHtml = arizona_socket:set_html_acc(HtmlContent, Socket1),

    % Pass the socket to to_html/2
    {Result, ResultSocket} = arizona_html:to_html(SocketWithHtml, create_mock_socket()),

    % Verify that the HTML was extracted from the socket
    ?assertEqual(HtmlContent, Result),
    % Verify that the returned socket is the one that contained the HTML
    ?assertEqual(SocketWithHtml, ResultSocket).

%% --------------------------------------------------------------------
%% Helper functions
%% --------------------------------------------------------------------

create_mock_socket() ->
    Id = ~"test_id",
    Module = test_stateful_module_with_mount,
    Bindings = #{},
    Stateful = arizona_stateful:new(Id, Module, Bindings),
    Opts = #{
        current_stateful_id => Id
    },
    Socket = arizona_socket:new(Opts),
    arizona_socket:put_stateful_state(Stateful, Socket).
