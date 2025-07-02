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
            test_render_stateful_complex_template
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
            test_render_list_multiple_items
        ]},
        {to_html_tests, [parallel], [
            test_to_html_binary,
            test_to_html_list,
            test_to_html_atom,
            test_to_html_integer,
            test_to_html_float,
            test_to_html_complex_term
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

%% --------------------------------------------------------------------
%% Render stateless tests
%% --------------------------------------------------------------------

test_render_stateless_with_structured_list(Config) when is_list(Config) ->
    StructuredList = [
        {static, 1, ~"<span>"},
        {dynamic, 1, ~"content"},
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
        {dynamic, 1, ~"My Title"},
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
            elems => #{0 => {1, fun(Item, _Socket) -> Item end}},
            vars_indexes => #{}
        }
    },
    Items = [~"Item1", ~"Item2"],
    KeyFun = fun(X) -> X end,
    Socket = create_mock_socket(),

    UpdatedSocket = arizona_html:render_list(ListData, Items, KeyFun, Socket),

    ?assert(arizona_socket:is_socket(UpdatedSocket)),
    Html = arizona_socket:get_html(UpdatedSocket),
    Expected = ~"<li>Item1</li><li>Item2</li>",
    ?assertEqual(Expected, iolist_to_binary(Html)).

test_render_list_with_item_function(Config) when is_list(Config) ->
    ItemFun = fun(Item) -> [~"<div>", Item, ~"</div>"] end,
    Items = [~"First", ~"Second"],
    KeyFun = fun(X) -> X end,
    Socket = create_mock_socket(),

    UpdatedSocket = arizona_html:render_list(ItemFun, Items, KeyFun, Socket),

    ?assert(arizona_socket:is_socket(UpdatedSocket)),
    Html = arizona_socket:get_html(UpdatedSocket),
    Expected = ~"<div>First</div><div>Second</div>",
    ?assertEqual(Expected, iolist_to_binary(Html)).

test_render_list_empty_items(Config) when is_list(Config) ->
    ItemFun = fun(Item) -> [~"<p>", Item, ~"</p>"] end,
    Items = [],
    KeyFun = fun(X) -> X end,
    Socket = create_mock_socket(),

    UpdatedSocket = arizona_html:render_list(ItemFun, Items, KeyFun, Socket),

    ?assert(arizona_socket:is_socket(UpdatedSocket)),
    Html = arizona_socket:get_html(UpdatedSocket),
    Expected = ~"",
    ?assertEqual(Expected, iolist_to_binary(Html)).

test_render_list_multiple_items(Config) when is_list(Config) ->
    ItemFun = fun(Item) -> [~"<option>", Item, ~"</option>"] end,
    Items = [~"A", ~"B", ~"C"],
    KeyFun = fun(X) -> X end,
    Socket = create_mock_socket(),

    UpdatedSocket = arizona_html:render_list(ItemFun, Items, KeyFun, Socket),

    ?assert(arizona_socket:is_socket(UpdatedSocket)),
    Html = arizona_socket:get_html(UpdatedSocket),
    Expected = ~"<option>A</option><option>B</option><option>C</option>",
    ?assertEqual(Expected, iolist_to_binary(Html)).

%% --------------------------------------------------------------------
%% to_html tests
%% --------------------------------------------------------------------

test_to_html_binary(Config) when is_list(Config) ->
    Result = arizona_html:to_html(~"Hello World"),
    ?assertEqual(~"Hello World", Result).

test_to_html_list(Config) when is_list(Config) ->
    Result = arizona_html:to_html([~"<div>", ~"content", ~"</div>"]),
    ?assertEqual(~"<div>content</div>", Result).

test_to_html_atom(Config) when is_list(Config) ->
    Result = arizona_html:to_html(hello),
    ?assertEqual(~"hello", Result).

test_to_html_integer(Config) when is_list(Config) ->
    Result = arizona_html:to_html(42),
    ?assertEqual(~"42", Result).

test_to_html_float(Config) when is_list(Config) ->
    Result = arizona_html:to_html(3.14),
    Expected = ~"3.14",
    ?assertEqual(Expected, Result).

test_to_html_complex_term(Config) when is_list(Config) ->
    Result = arizona_html:to_html({error, not_found}),
    Expected = ~"{error,not_found}",
    ?assertEqual(Expected, Result).

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
    arizona_socket:put_stateful_state(Id, Stateful, Socket).

create_mock_socket_with_bindings(Bindings) ->
    Id = ~"test_id",
    Module = test_stateful_module_with_mount,
    Stateful = arizona_stateful:new(Id, Module, Bindings),
    Opts = #{
        current_stateful_id => Id
    },
    Socket = arizona_socket:new(Opts),
    arizona_socket:put_stateful_state(Id, Stateful, Socket).
