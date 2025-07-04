-module(arizona_renderer_SUITE).
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
        {group, error_handling_tests}
    ].

groups() ->
    [
        {render_stateful_tests, [parallel], [
            test_render_stateful_basic,
            test_render_stateful_with_dynamic,
            test_render_stateful_empty_elements,
            test_render_stateful_single_element,
            test_render_stateful_multiple_elements
        ]},
        {render_stateless_tests, [parallel], [
            test_render_stateless_basic,
            test_render_stateless_empty_list,
            test_render_stateless_mixed_content
        ]},
        {render_list_tests, [parallel], [
            test_render_list_basic,
            test_render_list_empty_items,
            test_render_list_single_item,
            test_render_list_multiple_items,
            test_render_list_with_variables
        ]},
        {error_handling_tests, [parallel], [
            test_render_element_binding_error,
            test_render_element_template_error,
            test_render_list_item_error,
            test_render_list_item_general_error,
            test_zip_dynamic_longer_than_static,
            test_format_error_with_info,
            test_format_error_without_info
        ]}
    ].

%% --------------------------------------------------------------------
%% Render stateful tests
%% --------------------------------------------------------------------

test_render_stateful_basic(Config) when is_list(Config) ->
    TemplateData = #{
        elems_order => [0],
        elems => #{0 => {static, 1, ~"<div>Hello</div>"}},
        vars_indexes => #{}
    },
    Socket = create_mock_socket(),

    {Html, UpdatedSocket} = arizona_renderer:render_stateful(TemplateData, Socket),

    ?assertEqual([~"<div>Hello</div>"], Html),
    ?assert(arizona_socket:is_socket(UpdatedSocket)).

test_render_stateful_with_dynamic(Config) when is_list(Config) ->
    TemplateData = #{
        elems_order => [0, 1, 2],
        elems => #{
            0 => {static, 1, ~"<div>Hello "},
            1 => {dynamic, 1, fun(_Socket) -> ~"World" end},
            2 => {static, 1, ~"</div>"}
        },
        vars_indexes => #{}
    },
    Socket = create_mock_socket(),

    {Html, UpdatedSocket} = arizona_renderer:render_stateful(TemplateData, Socket),

    Expected = [~"<div>Hello ", ~"World", ~"</div>"],
    ?assertEqual(Expected, Html),
    ?assert(arizona_socket:is_socket(UpdatedSocket)).

test_render_stateful_empty_elements(Config) when is_list(Config) ->
    TemplateData = #{
        elems_order => [],
        elems => #{},
        vars_indexes => #{}
    },
    Socket = create_mock_socket(),

    {Html, UpdatedSocket} = arizona_renderer:render_stateful(TemplateData, Socket),

    ?assertEqual([], Html),
    ?assert(arizona_socket:is_socket(UpdatedSocket)).

test_render_stateful_single_element(Config) when is_list(Config) ->
    TemplateData = #{
        elems_order => [0],
        elems => #{0 => {static, 1, ~"Single"}},
        vars_indexes => #{}
    },
    Socket = create_mock_socket(),

    {Html, UpdatedSocket} = arizona_renderer:render_stateful(TemplateData, Socket),

    ?assertEqual([~"Single"], Html),
    ?assert(arizona_socket:is_socket(UpdatedSocket)).

test_render_stateful_multiple_elements(Config) when is_list(Config) ->
    TemplateData = #{
        elems_order => [0, 1, 2, 3],
        elems => #{
            0 => {static, 1, ~"<p>"},
            1 => {static, 1, ~"Text"},
            2 => {static, 1, ~" content"},
            3 => {static, 1, ~"</p>"}
        },
        vars_indexes => #{}
    },
    Socket = create_mock_socket(),

    {Html, UpdatedSocket} = arizona_renderer:render_stateful(TemplateData, Socket),

    Expected = [~"<p>", ~"Text", ~" content", ~"</p>"],
    ?assertEqual(Expected, Html),
    ?assert(arizona_socket:is_socket(UpdatedSocket)).

%% --------------------------------------------------------------------
%% Render stateless tests
%% --------------------------------------------------------------------

test_render_stateless_basic(Config) when is_list(Config) ->
    StructuredList = [
        {static, 1, ~"<div>"},
        {dynamic, 1, fun(_Socket) -> ~"content" end},
        {static, 1, ~"</div>"}
    ],
    Socket = create_mock_socket(),

    {Html, UpdatedSocket} = arizona_renderer:render_stateless(StructuredList, Socket),

    Expected = [~"<div>", ~"content", ~"</div>"],
    ?assertEqual(Expected, Html),
    ?assert(arizona_socket:is_socket(UpdatedSocket)).

test_render_stateless_empty_list(Config) when is_list(Config) ->
    StructuredList = [],
    Socket = create_mock_socket(),

    {Html, UpdatedSocket} = arizona_renderer:render_stateless(StructuredList, Socket),

    ?assertEqual([], Html),
    ?assert(arizona_socket:is_socket(UpdatedSocket)).

test_render_stateless_mixed_content(Config) when is_list(Config) ->
    StructuredList = [
        {static, 1, ~"<h1>Title</h1>"},
        {dynamic, 2, fun(_Socket) -> ~"variable_content" end},
        {static, 3, ~"<p>End</p>"}
    ],
    Socket = create_mock_socket(),

    {Html, UpdatedSocket} = arizona_renderer:render_stateless(StructuredList, Socket),

    Expected = [~"<h1>Title</h1>", ~"variable_content", ~"<p>End</p>"],
    ?assertEqual(Expected, Html),
    ?assert(arizona_socket:is_socket(UpdatedSocket)).

%% --------------------------------------------------------------------
%% Render list tests
%% --------------------------------------------------------------------

test_render_list_basic(Config) when is_list(Config) ->
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

    {Html, UpdatedSocket} = arizona_renderer:render_list(ListData, Items, Socket),

    Expected = ~"<li>Item1</li><li>Item2</li>",
    ?assertEqual(Expected, iolist_to_binary(Html)),
    ?assert(arizona_socket:is_socket(UpdatedSocket)).

test_render_list_empty_items(Config) when is_list(Config) ->
    ListData = #{
        static => [~"<li>", ~"", ~"</li>"],
        dynamic => #{
            elems_order => [0],
            elems => #{0 => {dynamic, 1, fun(Item, _Socket) -> Item end}},
            vars_indexes => #{}
        }
    },
    Items = [],
    Socket = create_mock_socket(),

    {Html, UpdatedSocket} = arizona_renderer:render_list(ListData, Items, Socket),

    Expected = ~"",
    ?assertEqual(Expected, iolist_to_binary(Html)),
    ?assert(arizona_socket:is_socket(UpdatedSocket)).

test_render_list_single_item(Config) when is_list(Config) ->
    ListData = #{
        static => [~"<div>", ~"", ~"</div>"],
        dynamic => #{
            elems_order => [0],
            elems => #{0 => {dynamic, 1, fun(Item, _Socket) -> Item end}},
            vars_indexes => #{}
        }
    },
    Items = [~"SingleItem"],
    Socket = create_mock_socket(),

    {Html, UpdatedSocket} = arizona_renderer:render_list(ListData, Items, Socket),

    Expected = ~"<div>SingleItem</div>",
    ?assertEqual(Expected, iolist_to_binary(Html)),
    ?assert(arizona_socket:is_socket(UpdatedSocket)).

test_render_list_multiple_items(Config) when is_list(Config) ->
    ListData = #{
        static => [~"<span>", ~"", ~"</span>"],
        dynamic => #{
            elems_order => [0],
            elems => #{0 => {dynamic, 1, fun(Item, _Socket) -> Item end}},
            vars_indexes => #{}
        }
    },
    Items = [~"A", ~"B", ~"C"],
    Socket = create_mock_socket(),

    {Html, UpdatedSocket} = arizona_renderer:render_list(ListData, Items, Socket),

    Expected = ~"<span>A</span><span>B</span><span>C</span>",
    ?assertEqual(Expected, iolist_to_binary(Html)),
    ?assert(arizona_socket:is_socket(UpdatedSocket)).

test_render_list_with_variables(Config) when is_list(Config) ->
    ListData = #{
        static => [~"<p>", ~"", ~"</p>"],
        dynamic => #{
            elems_order => [0],
            elems => #{0 => {dynamic, 1, fun(Item, _Socket) -> Item end}},
            vars_indexes => #{~"test_var" => [0]}
        }
    },
    Items = [~"VarItem"],
    Socket = create_mock_socket(),

    {Html, UpdatedSocket} = arizona_renderer:render_list(ListData, Items, Socket),

    Expected = ~"<p>VarItem</p>",
    ?assertEqual(Expected, iolist_to_binary(Html)),
    ?assert(arizona_socket:is_socket(UpdatedSocket)).

%% --------------------------------------------------------------------
%% Error handling tests
%% --------------------------------------------------------------------

test_render_element_binding_error(Config) when is_list(Config) ->
    TemplateData = #{
        elems_order => [0],
        elems => #{0 => {dynamic, 42, fun(_Socket) -> throw({binding_not_found, test_key}) end}},
        vars_indexes => #{}
    },
    Socket = create_mock_socket(),

    ?assertError(
        {binding_not_found, test_key},
        arizona_renderer:render_stateful(TemplateData, Socket)
    ).

test_render_element_template_error(Config) when is_list(Config) ->
    TemplateData = #{
        elems_order => [0],
        elems => #{0 => {dynamic, 42, fun(_Socket) -> error(test_error) end}},
        vars_indexes => #{}
    },
    Socket = create_mock_socket(),

    ?assertError(
        {template_render_error, test_error, 42},
        arizona_renderer:render_stateful(TemplateData, Socket)
    ).

test_render_list_item_error(Config) when is_list(Config) ->
    ListData = #{
        static => [~"<div>", ~"", ~"</div>"],
        dynamic => #{
            elems_order => [0],
            elems => #{
                0 => {dynamic, 42, fun(_Item, _Socket) -> throw({binding_not_found, test_key}) end}
            },
            vars_indexes => #{}
        }
    },
    Items = [~"TestItem"],
    Socket = create_mock_socket(),

    ?assertError(
        {binding_not_found, test_key},
        arizona_renderer:render_list(ListData, Items, Socket)
    ).

test_format_error_with_info(Config) when is_list(Config) ->
    Reason = {binding_not_found, test_key},
    StackTrace = [
        {arizona_renderer, render_element, [element, socket], [
            {error_info, #{
                cause => #{binding => test_key, line => 42, template_module => test_module},
                module => arizona_renderer
            }}
        ]}
    ],

    ErrorMap = arizona_renderer:format_error(Reason, StackTrace),

    ?assertMatch(
        #{
            binding := test_key,
            line := 42,
            template_module := test_module,
            general := "Template rendering error",
            reason := _
        },
        ErrorMap
    ).

test_format_error_without_info(Config) when is_list(Config) ->
    Reason = some_error,
    StackTrace = [],

    ErrorMap = arizona_renderer:format_error(Reason, StackTrace),

    ?assertMatch(
        #{
            general := "Template rendering error",
            reason := _
        },
        ErrorMap
    ).

test_render_list_item_general_error(Config) when is_list(Config) ->
    ListData = #{
        static => [~"<div>", ~"", ~"</div>"],
        dynamic => #{
            elems_order => [0],
            elems => #{0 => {dynamic, 42, fun(_Item, _Socket) -> error(general_error) end}},
            vars_indexes => #{}
        }
    },
    Items = [~"TestItem"],
    Socket = create_mock_socket(),

    ?assertError(
        {list_item_render_error, general_error, ~"TestItem", 42},
        arizona_renderer:render_list(ListData, Items, Socket)
    ).

test_zip_dynamic_longer_than_static(Config) when is_list(Config) ->
    % Testing zip_static_dynamic internal function indirectly through render_list
    ListData = #{
        % Only one static part
        static => [~"<li>", ~"</li>"],
        dynamic => #{
            % Two dynamic parts - more than static parts
            elems_order => [0, 1],
            elems => #{
                0 => {dynamic, 1, fun(Item, _Socket) -> Item end},
                1 => {dynamic, 1, fun(_Item, _Socket) -> ~" - extra" end}
            },
            vars_indexes => #{}
        }
    },
    Items = [~"List Item"],
    Socket = create_mock_socket(),

    {Html, UpdatedSocket} = arizona_renderer:render_list(ListData, Items, Socket),

    Expected = ~"<li>List Item</li> - extra",
    ?assertEqual(Expected, iolist_to_binary(Html)),
    ?assert(arizona_socket:is_socket(UpdatedSocket)).

%% --------------------------------------------------------------------
%% Helper functions
%% --------------------------------------------------------------------

create_mock_socket() ->
    Id = ~"test_id",
    Module = test_stateful_module_with_mount,
    Bindings = #{},
    StatefulState = arizona_stateful:new(Id, Module, Bindings),
    Opts = #{
        current_stateful_id => Id
    },
    Socket = arizona_socket:new(Opts),
    arizona_socket:put_stateful_state(StatefulState, Socket).
