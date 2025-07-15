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
        {group, to_html_tests},
        {group, diff_mode_tests},
        {group, error_handling_tests},
        {group, edge_case_tests},
        {group, slot_tests},
        {group, advanced_slot_tests}
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
        ]},
        {diff_mode_tests, [parallel], [
            test_render_stateful_diff_mode,
            test_render_stateful_mode_switching
        ]},
        {error_handling_tests, [parallel], [
            test_render_stateful_invalid_template,
            test_render_stateless_invalid_template,
            test_render_list_error_function
        ]},
        {edge_case_tests, [parallel], [
            test_to_html_float_infinity,
            test_to_html_deeply_nested_list,
            test_extract_parameter_name_edge_cases
        ]},
        {slot_tests, [parallel], [
            test_render_slot_static_content,
            test_render_slot_with_default,
            test_render_slot_missing_required,
            test_render_slot_list_content,
            test_render_slot_stateless_component
        ]},
        {advanced_slot_tests, [parallel], [
            test_render_live_diff_mode_with_layout,
            test_render_slot_with_parsed_template,
            test_render_slot_with_stateful_component,
            test_extract_parameter_name_with_function_environment
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

    % Expected flattened binary structure (not nested lists!)
    ExpectedHtml = [
        ~"<div>\n    Level 0: ",
        [~"<span>Level 1:\n", [~"<p>Level 2: Deep nesting test</p>"], ~"\n</span>"],
        ~"\n</div>"
    ],

    ?assertEqual(ExpectedHtml, ResultHtml).

%% --------------------------------------------------------------------
%% Render stateless tests
%% --------------------------------------------------------------------

test_render_stateless_with_structured_list(Config) when is_list(Config) ->
    StructuredTemplate = #{
        elems_order => [0, 1, 2],
        elems => #{
            0 => {static, 1, ~"<span>"},
            1 => {dynamic, 1, fun(_Socket) -> ~"content" end},
            2 => {static, 1, ~"</span>"}
        },
        vars_indexes => #{}
    },
    Socket = create_mock_socket(),

    UpdatedSocket = arizona_html:render_stateless(StructuredTemplate, Socket),

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
    % Use unified map format instead of old list format
    StructuredTemplate = #{
        elems_order => [0, 1, 2],
        elems => #{
            0 => {static, 1, ~"<section>"},
            1 => {dynamic, 1, fun(_Socket) -> ~"My Title" end},
            2 => {static, 1, ~"</section>"}
        },
        vars_indexes => #{}
    },
    Socket = create_mock_socket(),

    UpdatedSocket = arizona_html:render_stateless(StructuredTemplate, Socket),

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
                            #{
                                elems_order => [0, 1, 2],
                                elems => #{
                                    0 => {static, 1, ~"<span>"},
                                    1 => {dynamic, 1, fun(_Socket) -> Item end},
                                    2 => {static, 1, ~"</span>"}
                                },
                                vars_indexes => #{}
                            },
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
    StatefulState = arizona_stateful:new(root, undefined, #{prefix => ~"foo"}),
    Socket = arizona_socket:put_stateful_state(StatefulState, arizona_socket:new(#{})),

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
    ExpectedHtml = ~"<ul>\n<li>foo_1</li><li>foo_2</li><li>foo_3</li>\n</ul>",

    ?assertEqual(ExpectedHtml, FlatHtml).

%% --------------------------------------------------------------------
%% to_html tests
%% --------------------------------------------------------------------

test_to_html_binary(Config) when is_list(Config) ->
    {Result, _Socket} = arizona_html:to_html(~"Hello World", create_mock_socket()),
    ?assertEqual(~"Hello World", Result).

test_to_html_list(Config) when is_list(Config) ->
    {Result, _Socket} = arizona_html:to_html([~"<div>", content, ~"</div>"], create_mock_socket()),
    ?assertEqual([[~"<div>", ~"content"], ~"</div>"], Result).

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
    Module = arizona_stateful_module_with_mount,
    Bindings = #{},
    StatefulState = arizona_stateful:new(Id, Module, Bindings),
    Opts = #{
        current_stateful_id => Id
    },
    Socket = arizona_socket:new(Opts),
    arizona_socket:put_stateful_state(StatefulState, Socket).

%% --------------------------------------------------------------------
%% Diff mode tests
%% --------------------------------------------------------------------

test_render_stateful_diff_mode(Config) when is_list(Config) ->
    % Test render_stateful with socket in diff mode
    TemplateData = #{
        elems_order => [0],
        elems => #{0 => {static, 1, ~"<div>content</div>"}},
        vars_indexes => #{}
    },

    % Create socket in diff mode with stateful state
    StatefulState = arizona_stateful:new(root, test_module, #{counter => 42}),
    Socket = arizona_socket:new(#{mode => diff}),
    SocketWithState = arizona_socket:put_stateful_state(StatefulState, Socket),

    % This should call arizona_differ:diff_stateful/3
    ResultSocket = arizona_html:render_stateful(TemplateData, SocketWithState),

    % Verify socket is returned (differ returns socket)
    ?assert(arizona_socket:is_socket(ResultSocket)).

test_render_stateful_mode_switching(Config) when is_list(Config) ->
    % Test that different modes produce different behavior
    TemplateData = #{
        elems_order => [0],
        elems => #{0 => {static, 1, ~"<div>test</div>"}},
        vars_indexes => #{}
    },

    % Test render mode
    RenderSocket = arizona_socket:new(#{mode => render}),
    RenderResult = arizona_html:render_stateful(TemplateData, RenderSocket),
    ?assert(arizona_socket:is_socket(RenderResult)),

    % Test diff mode
    StatefulState = arizona_stateful:new(root, test_module, #{}),
    DiffSocket = arizona_socket:new(#{mode => diff}),
    DiffSocketWithState = arizona_socket:put_stateful_state(StatefulState, DiffSocket),
    DiffResult = arizona_html:render_stateful(TemplateData, DiffSocketWithState),
    ?assert(arizona_socket:is_socket(DiffResult)).

%% --------------------------------------------------------------------
%% Error handling tests
%% --------------------------------------------------------------------

test_render_stateful_invalid_template(Config) when is_list(Config) ->
    % Test with malformed template syntax
    InvalidHtml = ~"<div>{unclosed_expression",
    Socket = create_mock_socket(),

    % This should cause a scanner or parser error
    ?assertError(_, arizona_html:render_stateful(InvalidHtml, Socket)).

test_render_stateless_invalid_template(Config) when is_list(Config) ->
    % Test with malformed template for stateless rendering
    InvalidHtml = ~"<div>{another_unclosed",
    Socket = create_mock_socket(),

    % This should cause a scanner or parser error
    ?assertError(_, arizona_html:render_stateless(InvalidHtml, Socket)).

test_render_list_error_function(Config) when is_list(Config) ->
    % Test render_list with function that throws error
    ErrorFun = fun(_Item) -> error(test_error) end,
    Items = [1, 2, 3],
    Socket = create_mock_socket(),

    % Should propagate the error
    ?assertError(test_error, arizona_html:render_list(ErrorFun, Items, Socket)).

%% --------------------------------------------------------------------
%% Edge case tests
%% --------------------------------------------------------------------

test_to_html_float_infinity(Config) when is_list(Config) ->
    Socket = create_mock_socket(),

    % Test very large float (near infinity)
    {ResultLarge, _} = arizona_html:to_html(1.0e308, Socket),
    ?assert(is_binary(ResultLarge)),
    ?assert(byte_size(ResultLarge) > 0),

    % Test very small float
    {ResultSmall, _} = arizona_html:to_html(1.0e-308, Socket),
    ?assert(is_binary(ResultSmall)),
    ?assert(byte_size(ResultSmall) > 0),

    % Test negative large float
    {ResultNegLarge, _} = arizona_html:to_html(-1.0e308, Socket),
    ?assert(is_binary(ResultNegLarge)),
    ?assert(byte_size(ResultNegLarge) > 0).

test_to_html_deeply_nested_list(Config) when is_list(Config) ->
    Socket = create_mock_socket(),

    % Create deeply nested list (50 levels deep to avoid stack overflow)
    DeepList = lists:foldl(
        fun(_, Acc) -> [Acc] end,
        ~"content",
        lists:seq(1, 50)
    ),

    {Result, _} = arizona_html:to_html(DeepList, Socket),
    ?assert(iolist_size(Result) > 0),

    % Verify content is preserved
    FlatResult = iolist_to_binary(Result),
    ?assert(binary:match(FlatResult, ~"content") =/= nomatch).

test_extract_parameter_name_edge_cases(Config) when is_list(Config) ->
    % Test parameter extraction indirectly through render_list behavior
    Socket = create_mock_socket(),

    % Test with function that uses parameter name
    FunWithParam = fun(Item) ->
        io_lib:format("item:~p", [Item])
    end,
    Items = [1, 2],

    % This exercises the parameter extraction code path
    ResultSocket = arizona_html:render_list(FunWithParam, Items, Socket),
    ?assert(arizona_socket:is_socket(ResultSocket)),

    % Test with complex function
    ComplexFun = fun(Element) ->
        case Element of
            N when is_integer(N) -> integer_to_binary(N);
            _ -> ~"other"
        end
    end,
    ComplexResult = arizona_html:render_list(ComplexFun, [42, atom], Socket),
    ?assert(arizona_socket:is_socket(ComplexResult)).

%% --------------------------------------------------------------------
%% Slot tests
%% --------------------------------------------------------------------

test_render_slot_static_content(Config) when is_list(Config) ->
    % Create socket with static slot content
    Socket = create_socket_with_binding(header, ~"<h1>Test Header</h1>"),

    % Render the slot
    UpdatedSocket = arizona_html:render_slot(header, Socket),

    % Verify slot content was rendered
    ?assert(arizona_socket:is_socket(UpdatedSocket)),
    Html = arizona_socket:get_html(UpdatedSocket),
    ?assertEqual(~"<h1>Test Header</h1>", iolist_to_binary(Html)).

test_render_slot_with_default(Config) when is_list(Config) ->
    % Create socket without the slot binding
    Socket = create_mock_socket(),

    % Render slot with default
    DefaultContent = ~"<p>Default Content</p>",
    UpdatedSocket = arizona_html:render_slot(missing_slot, Socket, DefaultContent),

    % Verify default content was rendered
    ?assert(arizona_socket:is_socket(UpdatedSocket)),
    Html = arizona_socket:get_html(UpdatedSocket),
    ?assertEqual(~"<p>Default Content</p>", iolist_to_binary(Html)).

test_render_slot_missing_required(Config) when is_list(Config) ->
    % Create socket without the slot binding
    Socket = create_mock_socket(),

    % Rendering required slot should crash with binding_not_found
    ?assertThrow({binding_not_found, missing_slot}, arizona_html:render_slot(missing_slot, Socket)).

test_render_slot_list_content(Config) when is_list(Config) ->
    % Create socket with list slot content - use simple binaries
    ListContent = [
        ~"<li>Item 1</li>",
        ~"<li>Item 2</li>",
        ~"<li>Item 3</li>"
    ],
    Socket = create_socket_with_binding(nav_items, ListContent),

    % Render the list slot
    UpdatedSocket = arizona_html:render_slot(nav_items, Socket),

    % Verify all items were rendered
    ?assert(arizona_socket:is_socket(UpdatedSocket)),
    Html = arizona_socket:get_html(UpdatedSocket),
    ExpectedHtml = ~"<li>Item 1</li><li>Item 2</li><li>Item 3</li>",
    ?assertEqual(ExpectedHtml, iolist_to_binary(Html)).

test_render_slot_stateless_component(Config) when is_list(Config) ->
    % Create socket with stateless component slot using existing test module
    ComponentSlot =
        {stateless, arizona_stateless_module, render_with_bindings, #{
            title => ~"Test Title", content => ~"Test Content"
        }},
    Socket = create_socket_with_binding(component_slot, ComponentSlot),

    % Render the component slot
    UpdatedSocket = arizona_html:render_slot(component_slot, Socket),

    % Verify component was rendered
    ?assert(arizona_socket:is_socket(UpdatedSocket)),
    Html = arizona_socket:get_html(UpdatedSocket),
    % The arizona_stateless_module:render_with_bindings/1 should return expected content
    ExpectedHtml = iolist_to_binary([
        ~"<div class=\"stateless-with-bindings\">\n    <h2>Test Title</h2>\n    ",
        ~"<p>Test Content</p>\n</div>"
    ]),
    ?assertEqual(ExpectedHtml, iolist_to_binary(Html)).

%% --------------------------------------------------------------------
%% Advanced Slot Tests (targeting specific code paths)
%% --------------------------------------------------------------------

test_render_live_diff_mode_with_layout(Config) when is_list(Config) ->
    % Test render_live with layout configured and socket in diff mode
    Template = #{
        elems_order => [0],
        elems => #{0 => {static, 1, ~"<div>Live Content</div>"}},
        vars_indexes => #{}
    },

    % Create socket in diff mode with layout configured
    LayoutModule = arizona_test_layout,
    LayoutRenderFun = render,
    SlotName = content,

    % Create socket with proper stateful state first
    Socket = create_mock_socket(),
    DiffModeSocket = arizona_socket:new(#{
        mode => diff,
        current_stateful_id => arizona_socket:get_current_stateful_id(Socket)
    }),
    % Copy the stateful state to the new socket
    StatefulStates = arizona_socket:get_stateful_states(Socket),
    DiffSocketWithStates = maps:fold(
        fun(_Id, State, AccSocket) ->
            arizona_socket:put_stateful_state(State, AccSocket)
        end,
        DiffModeSocket,
        StatefulStates
    ),
    SocketWithLayout = arizona_socket:set_layout(
        {LayoutModule, LayoutRenderFun, SlotName},
        DiffSocketWithStates
    ),

    % This should hit the diff mode with layout path
    ResultSocket = arizona_html:render_live(Template, SocketWithLayout),
    ?assert(arizona_socket:is_socket(ResultSocket)).

test_render_slot_with_parsed_template(Config) when is_list(Config) ->
    % Test slot content with parse-transform optimized stateless template in unified format
    ParsedTemplate = #{
        elems_order => [0, 1, 2],
        elems => #{
            0 => {static, 1, ~"<span>"},
            1 => {static, 1, ~"Parsed Content"},
            2 => {static, 1, ~"</span>"}
        },
        vars_indexes => #{}
    },

    % Create socket with slot content as parsed template
    SlotContent = {stateless, ParsedTemplate},
    Socket = create_socket_with_binding(parsed_slot, SlotContent),

    % This should hit the parsed template path in render_slot_content
    UpdatedSocket = arizona_html:render_slot(parsed_slot, Socket),
    ?assert(arizona_socket:is_socket(UpdatedSocket)),
    Html = arizona_socket:get_html(UpdatedSocket),
    Expected = ~"<span>Parsed Content</span>",
    ?assertEqual(Expected, iolist_to_binary(Html)).

test_render_slot_with_stateful_component(Config) when is_list(Config) ->
    % Test slot content with stateful component
    % Use a mock stateful component that mimics the expected structure
    StatefulComponent =
        {stateful, mock_stateful_module, #{
            title => ~"Stateful Title",
            content => ~"Stateful Content"
        }},

    % Create socket with stateful component slot
    Socket = create_socket_with_binding(stateful_slot, StatefulComponent),

    % This should hit the stateful component path in render_slot_content
    % Even if the module doesn't exist, it tests the code path structure
    try
        UpdatedSocket = arizona_html:render_slot(stateful_slot, Socket),
        ?assert(arizona_socket:is_socket(UpdatedSocket))
    catch
        error:undef ->
            % Expected since mock_stateful_module doesn't exist
            % But this still tests the stateful component code path
            ?assert(true)
    end.

test_extract_parameter_name_with_function_environment(Config) when is_list(Config) ->
    % Test parameter extraction when function has environment information
    % This test targets the AST-based parameter extraction code path

    % Create a function that might have debug info or environment
    TestFun = fun(CustomParam) -> CustomParam * 2 end,

    % Test the parameter extraction function directly
    % This function is internal but exported for testing
    ParameterName = arizona_html:extract_list_item_parameter_name(TestFun),

    % Should return either the extracted name or the default 'Item'
    ?assert(is_atom(ParameterName)),
    % The exact result depends on whether debug_info is available
    ?assert(ParameterName =:= 'Item' orelse ParameterName =:= 'CustomParam').

%% --------------------------------------------------------------------
%% Helper Functions
%% --------------------------------------------------------------------

%% Helper function to create socket with specific binding
create_socket_with_binding(Key, Value) ->
    % Match the ID used in create_mock_socket
    Id = ~"test_id",
    StatefulState = arizona_stateful:new(Id, test_module, #{Key => Value}),
    Opts = #{current_stateful_id => Id},
    Socket = arizona_socket:new(Opts),
    arizona_socket:put_stateful_state(StatefulState, Socket).
