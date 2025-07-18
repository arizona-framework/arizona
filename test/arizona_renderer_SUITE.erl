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
            test_render_stateless_mixed_content,
            test_render_stateless_unified_format,
            test_render_stateless_unified_with_vars_indexes,
            test_render_stateless_unified_empty,
            test_render_stateless_unified_single_element,
            test_render_stateless_unified_cascade_simulation
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

    {Html, UpdatedSocket} = arizona_renderer:render_template(TemplateData, Socket),

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

    {Html, UpdatedSocket} = arizona_renderer:render_template(TemplateData, Socket),

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

    {Html, UpdatedSocket} = arizona_renderer:render_template(TemplateData, Socket),

    ?assertEqual([], Html),
    ?assert(arizona_socket:is_socket(UpdatedSocket)).

test_render_stateful_single_element(Config) when is_list(Config) ->
    TemplateData = #{
        elems_order => [0],
        elems => #{0 => {static, 1, ~"Single"}},
        vars_indexes => #{}
    },
    Socket = create_mock_socket(),

    {Html, UpdatedSocket} = arizona_renderer:render_template(TemplateData, Socket),

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

    {Html, UpdatedSocket} = arizona_renderer:render_template(TemplateData, Socket),

    Expected = [~"<p>", ~"Text", ~" content", ~"</p>"],
    ?assertEqual(Expected, Html),
    ?assert(arizona_socket:is_socket(UpdatedSocket)).

%% --------------------------------------------------------------------
%% Render stateless tests
%% --------------------------------------------------------------------

test_render_stateless_basic(Config) when is_list(Config) ->
    TemplateData = #{
        elems_order => [0, 1, 2],
        elems => #{
            0 => {static, 1, ~"<div>"},
            1 => {dynamic, 1, fun(_Socket) -> ~"content" end},
            2 => {static, 1, ~"</div>"}
        },
        vars_indexes => #{}
    },
    Socket = create_mock_socket(),

    {Html, UpdatedSocket} = arizona_renderer:render_template(TemplateData, Socket),

    Expected = [~"<div>", ~"content", ~"</div>"],
    ?assertEqual(Expected, Html),
    ?assert(arizona_socket:is_socket(UpdatedSocket)).

test_render_stateless_empty_list(Config) when is_list(Config) ->
    TemplateData = #{
        elems_order => [],
        elems => #{},
        vars_indexes => #{}
    },
    Socket = create_mock_socket(),

    {Html, UpdatedSocket} = arizona_renderer:render_template(TemplateData, Socket),

    ?assertEqual([], Html),
    ?assert(arizona_socket:is_socket(UpdatedSocket)).

test_render_stateless_mixed_content(Config) when is_list(Config) ->
    TemplateData = #{
        elems_order => [0, 1, 2],
        elems => #{
            0 => {static, 1, ~"<h1>Title</h1>"},
            1 => {dynamic, 2, fun(_Socket) -> ~"variable_content" end},
            2 => {static, 3, ~"<p>End</p>"}
        },
        vars_indexes => #{}
    },
    Socket = create_mock_socket(),

    {Html, UpdatedSocket} = arizona_renderer:render_template(TemplateData, Socket),

    Expected = [~"<h1>Title</h1>", ~"variable_content", ~"<p>End</p>"],
    ?assertEqual(Expected, Html),
    ?assert(arizona_socket:is_socket(UpdatedSocket)).

%% Tests for unified stateless format (same as stateful)
test_render_stateless_unified_format(Config) when is_list(Config) ->
    % Test that stateless components can use the same format as stateful
    TemplateData = #{
        elems_order => [0, 1, 2],
        elems => #{
            0 => {static, 1, ~"<div>"},
            1 => {dynamic, 1, fun(_Socket) -> ~"unified_content" end},
            2 => {static, 1, ~"</div>"}
        },
        vars_indexes => #{}
    },
    Socket = create_mock_socket(),

    {Html, UpdatedSocket} = arizona_renderer:render_template(TemplateData, Socket),

    % Should render in order according to elems_order
    ExpectedParts = [~"<div>", ~"unified_content", ~"</div>"],
    ?assertEqual(ExpectedParts, Html),
    ?assert(arizona_socket:is_socket(UpdatedSocket)).

test_render_stateless_unified_with_vars_indexes(Config) when is_list(Config) ->
    % Test that vars_indexes are properly handled (even though not used for stateless diffing)
    TemplateData = #{
        elems_order => [0, 1],
        elems => #{
            0 => {static, 1, ~"<span>User: "},
            1 =>
                {dynamic, 1, fun(Socket) ->
                    arizona_socket:get_binding(username, Socket, ~"default_user")
                end}
        },
        % This element depends on username
        vars_indexes => #{username => [1]}
    },
    Socket = arizona_socket:put_binding(username, ~"test_user", create_mock_socket()),

    {Html, UpdatedSocket} = arizona_renderer:render_template(TemplateData, Socket),

    Expected = [~"<span>User: ", ~"test_user"],
    ?assertEqual(Expected, Html),
    ?assert(arizona_socket:is_socket(UpdatedSocket)).

test_render_stateless_unified_empty(Config) when is_list(Config) ->
    % Test empty unified format
    TemplateData = #{
        elems_order => [],
        elems => #{},
        vars_indexes => #{}
    },
    Socket = create_mock_socket(),

    {Html, UpdatedSocket} = arizona_renderer:render_template(TemplateData, Socket),

    ?assertEqual([], Html),
    ?assert(arizona_socket:is_socket(UpdatedSocket)).

test_render_stateless_unified_single_element(Config) when is_list(Config) ->
    % Test single element in unified format
    TemplateData = #{
        elems_order => [0],
        elems => #{
            0 => {static, 1, ~"<p>Single element</p>"}
        },
        vars_indexes => #{}
    },
    Socket = create_mock_socket(),

    {Html, UpdatedSocket} = arizona_renderer:render_template(TemplateData, Socket),

    Expected = [~"<p>Single element</p>"],
    ?assertEqual(Expected, Html),
    ?assert(arizona_socket:is_socket(UpdatedSocket)).

test_render_stateless_unified_cascade_simulation(Config) when is_list(Config) ->
    % Simulate a cascade scenario where stateless component depends on parent binding
    % This represents: {arizona_component:call_stateless(module, fun,
    %                  #{foo => arizona_socket:get_binding(user, Socket)}, Socket)}
    TemplateData = #{
        elems_order => [0, 1, 2],
        elems => #{
            0 => {static, 1, ~"<div class=\"user-info\">"},
            1 =>
                {dynamic, 1, fun(Socket) ->
                    % Simulate the call_stateless dependency pattern
                    UserData = arizona_socket:get_binding(user, Socket, #{}),
                    Username = maps:get(username, UserData, ~"unknown"),
                    [~"Hello, ", Username, ~"!"]
                end},
            2 => {static, 1, ~"</div>"}
        },
        % Element 1 depends on user binding
        vars_indexes => #{user => [1]}
    },
    % Set up socket with user data
    UserData = #{username => ~"Alice", role => ~"admin"},
    Socket = arizona_socket:put_binding(user, UserData, create_mock_socket()),

    {Html, UpdatedSocket} = arizona_renderer:render_template(TemplateData, Socket),

    % The nested list structure from HTML accumulation may vary, so check the flattened content
    FlatHtml = iolist_to_binary(Html),
    ExpectedFlat = ~"<div class=\"user-info\">Hello, Alice!</div>",
    ?assertEqual(ExpectedFlat, FlatHtml),
    ?assert(arizona_socket:is_socket(UpdatedSocket)),

    % Verify the vars_indexes would allow proper diffing
    ?assertEqual(#{user => [1]}, maps:get(vars_indexes, TemplateData)).

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
        arizona_renderer:render_template(TemplateData, Socket)
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
        arizona_renderer:render_template(TemplateData, Socket)
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
    Module = arizona_stateful_module_with_mount,
    Bindings = #{},
    StatefulState = arizona_stateful:new(Id, Module, Bindings),
    Opts = #{
        current_stateful_id => Id
    },
    Socket = arizona_socket:new(Opts),
    arizona_socket:put_stateful_state(StatefulState, Socket).
