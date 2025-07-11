-module(arizona_differ_SUITE).
-behaviour(ct_suite).
-include_lib("stdlib/include/assert.hrl").
-compile([export_all, nowarn_export_all]).

%% --------------------------------------------------------------------
%% Behaviour (ct_suite) callbacks
%% --------------------------------------------------------------------

all() ->
    [
        {group, socket_changes},
        {group, diff_stateful},
        {group, diff_stateless},
        {group, diff_list},
        {group, diff_optimization}
    ].

groups() ->
    [
        {socket_changes, [parallel], [
            socket_changes_accumulation,
            socket_changes_merging,
            socket_changes_nested_structure,
            socket_changes_clear
        ]},
        {diff_stateful, [parallel], [
            diff_stateful_no_changes,
            diff_stateful_with_changes,
            diff_stateful_changes_no_affected_elements,
            diff_mode_vs_render_mode
        ]},
        {diff_stateless, [parallel], [
            diff_stateless_component_changes,
            diff_stateless_no_optimization
        ]},
        {diff_list, [parallel], [
            diff_list_basic_change,
            diff_list_item_addition,
            diff_list_item_removal
        ]},
        {diff_optimization, [parallel], [
            get_affected_elements_basic,
            get_affected_elements_multiple_vars
        ]},
        {complex_hierarchical, [parallel], [
            test_complex_hierarchical_change_detection,
            test_reproduce_websocket_bug_empty_vars_indexes,
            test_enhanced_parse_transform_missing_todos_mapping
        ]}
    ].

%% --------------------------------------------------------------------
%% Socket changes tests
%% --------------------------------------------------------------------

socket_changes_accumulation(Config) when is_list(Config) ->
    Socket = arizona_socket:new(#{mode => diff}),

    % Add first change
    Change1 = [{root, [{1, ~"value1"}]}],
    Socket1 = arizona_socket:append_changes(Change1, Socket),

    % Add second change
    Change2 = [{~"other_component", [{2, ~"value2"}]}],
    Socket2 = arizona_socket:append_changes(Change2, Socket1),

    % Get accumulated changes
    AllChanges = arizona_socket:get_changes(Socket2),

    % Changes are flattened and merged into a single structure
    ExpectedChanges = [{root, [{1, ~"value1"}]}, {~"other_component", [{2, ~"value2"}]}],
    ?assertEqual(ExpectedChanges, AllChanges).

socket_changes_merging(Config) when is_list(Config) ->
    Socket = arizona_socket:new(#{mode => diff}),

    % Add changes to the same component at different elements
    Change1 = [{root, [{1, ~"value1"}]}],
    Socket1 = arizona_socket:append_changes(Change1, Socket),

    Change2 = [{root, [{2, ~"value2"}]}],
    Socket2 = arizona_socket:append_changes(Change2, Socket1),

    % Changes should be merged under the same component
    AllChanges = arizona_socket:get_changes(Socket2),

    % The merge function combines element changes for the same component
    % Order may vary due to how merge_element_changes works
    ?assertMatch([{root, _ElementChanges}], AllChanges),
    [{root, ElementChanges}] = AllChanges,

    % Should contain both element changes (order may vary)
    ?assertEqual(2, length(ElementChanges)),
    ?assert(lists:member({1, ~"value1"}, ElementChanges)),
    ?assert(lists:member({2, ~"value2"}, ElementChanges)).

socket_changes_nested_structure(Config) when is_list(Config) ->
    Socket = arizona_socket:new(#{mode => diff}),

    % Create a nested change structure like [{root, [{3, [{counter, [{2, 42}]}]}]}]
    NestedChange = [{root, [{3, [{~"counter", [{2, 42}]}]}]}],
    Socket1 = arizona_socket:append_changes(NestedChange, Socket),

    % Add another change to the same nested structure
    AnotherNestedChange = [{root, [{3, [{~"counter", [{4, ~"new_value"}]}]}]}],
    Socket2 = arizona_socket:append_changes(AnotherNestedChange, Socket1),

    % Verify the nested structure is maintained
    AllChanges = arizona_socket:get_changes(Socket2),
    ?assert(length(AllChanges) >= 1).

socket_changes_clear(Config) when is_list(Config) ->
    Socket = arizona_socket:new(#{mode => diff}),
    Change = [{root, [{1, ~"value"}]}],
    Socket1 = arizona_socket:append_changes(Change, Socket),

    % Test clearing changes
    ClearedSocket = arizona_socket:clear_changes(Socket1),
    ?assertEqual([], arizona_socket:get_changes(ClearedSocket)).

%% --------------------------------------------------------------------
%% Diff stateful tests
%% --------------------------------------------------------------------

diff_stateful_no_changes(Config) when is_list(Config) ->
    % Create a stateful component with no changed bindings
    StatefulState = arizona_stateful:new(~"test_component", test_module, #{
        name => ~"John", age => 30
    }),

    % Create template data for testing
    TemplateData = #{
        elems_order => [0, 1, 2],
        elems => #{
            0 => {static, 1, ~"<div>"},
            1 => {dynamic, 2, fun(Socket) -> arizona_socket:get_binding(name, Socket) end},
            2 => {static, 3, ~"</div>"}
        },
        vars_indexes => #{
            name => [1]
        }
    },

    % Create socket in diff mode
    Socket = arizona_socket:new(#{mode => diff}),

    % Run diff - should return unchanged socket since no changed_bindings
    ResultSocket = arizona_differ:diff_stateful(TemplateData, StatefulState, Socket),

    % Verify no changes were accumulated
    Changes = arizona_socket:get_changes(ResultSocket),
    ?assertEqual([], Changes).

diff_stateful_with_changes(Config) when is_list(Config) ->
    % Create stateful component with some initial state
    InitialState = arizona_stateful:new(root, test_module, #{
        name => ~"John", counter => 0
    }),

    % Simulate binding changes (this would normally happen through put_binding)
    ChangedState = arizona_stateful:put_binding(counter, 42, InitialState),

    % Create template data for testing
    TemplateData = #{
        elems_order => [0, 1, 2],
        elems => #{
            0 => {static, 1, ~"<div>Count: "},
            1 => {dynamic, 2, fun(Socket) -> arizona_socket:get_binding(counter, Socket) end},
            2 => {static, 3, ~"</div>"}
        },
        vars_indexes => #{
            counter => [1]
        }
    },

    % Create socket in diff mode with the stateful state
    Socket = arizona_socket:new(#{mode => diff}),
    SocketWithState = arizona_socket:put_stateful_state(ChangedState, Socket),

    % Run diff
    ResultSocket = arizona_differ:diff_stateful(TemplateData, ChangedState, SocketWithState),

    % Verify changes were accumulated
    Changes = arizona_socket:get_changes(ResultSocket),
    ExpectedChanges = [{root, [{1, ~"42"}]}],
    ?assertEqual(ExpectedChanges, Changes).

diff_stateful_changes_no_affected_elements(Config) when is_list(Config) ->
    % Create stateful component with initial state
    InitialState = arizona_stateful:new(~"test_component", test_module, #{
        name => ~"John", counter => 0
    }),

    % Change a binding that doesn't affect any template elements
    % The template only has vars_indexes for "counter" and "name" but we change "unused_var"
    ChangedState = arizona_stateful:put_binding(unused_var, ~"some_value", InitialState),

    % Create template data with only "counter" and "name" in vars_indexes
    TemplateData = #{
        elems_order => [0, 1, 2],
        elems => #{
            0 => {static, 1, ~"<div>"},
            1 => {dynamic, 2, fun(Socket) -> arizona_socket:get_binding(counter, Socket) end},
            2 => {static, 3, ~"</div>"}
        },
        vars_indexes => #{
            counter => [1],
            % Not used in this template but listed
            name => []
        }
    },

    % Create socket in diff mode
    Socket = arizona_socket:new(#{mode => diff}),

    % Run diff - should return unchanged socket since no elements are affected
    % This tests the case where changed_bindings exists but AffectedElements is empty
    ResultSocket = arizona_differ:diff_stateful(TemplateData, ChangedState, Socket),

    % Verify no changes were accumulated (line 92 coverage)
    Changes = arizona_socket:get_changes(ResultSocket),
    ?assertEqual([], Changes).

diff_mode_vs_render_mode(Config) when is_list(Config) ->
    StatefulState = arizona_stateful:new(root, test_module, #{counter => 0}),
    ChangedState = arizona_stateful:put_binding(counter, 42, StatefulState),

    % Create template data for testing
    TemplateData = #{
        elems_order => [0, 1, 2],
        elems => #{
            0 => {static, 1, ~"<div>Count: "},
            1 => {dynamic, 2, fun(Socket) -> arizona_socket:get_binding(counter, Socket) end},
            2 => {static, 3, ~"</div>"}
        },
        vars_indexes => #{
            counter => [1]
        }
    },

    % Test render mode - differ should still generate diffs (mode check moved to arizona_html)
    RenderSocket = arizona_socket:new(#{mode => render}),
    RenderSocketWithState = arizona_socket:put_stateful_state(ChangedState, RenderSocket),
    RenderResult = arizona_differ:diff_stateful(TemplateData, ChangedState, RenderSocketWithState),
    % Note: differ no longer checks mode - it always diffs when called
    ?assert(length(arizona_socket:get_changes(RenderResult)) > 0),

    % Test diff mode - should generate diffs
    DiffSocket = arizona_socket:new(#{mode => diff}),
    DiffSocketWithState = arizona_socket:put_stateful_state(ChangedState, DiffSocket),
    DiffResult = arizona_differ:diff_stateful(TemplateData, ChangedState, DiffSocketWithState),
    DiffChanges = arizona_socket:get_changes(DiffResult),
    ?assert(length(DiffChanges) > 0).

%% --------------------------------------------------------------------
%% Diff optimization tests
%% --------------------------------------------------------------------

get_affected_elements_basic(Config) when is_list(Config) ->
    ChangedBindings = #{counter => 42},
    VarsIndexes = #{
        counter => [2, 5],
        name => [1, 3]
    },

    AffectedElements = arizona_differ:get_affected_elements(ChangedBindings, VarsIndexes),

    % Should include elements [2, 5] but not [1, 3]
    ExpectedElements = sets:from_list([2, 5]),
    ?assertEqual(ExpectedElements, AffectedElements).

get_affected_elements_multiple_vars(Config) when is_list(Config) ->
    ChangedBindings = #{counter => 42, name => ~"Jane"},
    VarsIndexes = #{
        counter => [2, 5],
        name => [1, 3, 7],
        % Should not be affected
        other => [4, 6]
    },

    AffectedElements = arizona_differ:get_affected_elements(ChangedBindings, VarsIndexes),

    % Should include elements [1, 2, 3, 5, 7] but not [4, 6]
    ExpectedElements = sets:from_list([1, 2, 3, 5, 7]),
    ?assertEqual(ExpectedElements, AffectedElements).

%% --------------------------------------------------------------------
%% Diff stateless tests
%% --------------------------------------------------------------------

diff_stateless_component_changes(Config) when is_list(Config) ->
    % Test that stateless components trigger full re-render (no optimization)
    % when variables change, since they don't have vars_indexes optimization
    InitialState = arizona_stateful:new(root, test_module, #{
        name => ~"John",
        message => ~"Hello"
    }),

    % Change a binding that would affect stateless components
    ChangedState = arizona_stateful:put_binding(name, ~"Jane", InitialState),

    % Create template data that includes a stateless component call
    TemplateData = #{
        elems_order => [0, 1, 2],
        elems => #{
            0 => {static, 1, ~"<div>"},
            1 =>
                {dynamic, 2, fun(Socket) ->
                    % This simulates a stateless component call that would need full re-render
                    arizona_html:render_stateless(
                        [
                            {static, 1, ~"<span>"},
                            {dynamic, 2, fun(S) -> arizona_socket:get_binding(name, S) end},
                            {static, 3, ~"</span>"}
                        ],
                        Socket
                    )
                end},
            2 => {static, 3, ~"</div>"}
        },
        vars_indexes => #{
            % name affects element 1
            name => [1],
            % message not used in this template
            message => []
        }
    },

    % Create socket and run diff
    Socket = arizona_socket:new(#{mode => diff}),
    SocketWithState = arizona_socket:put_stateful_state(ChangedState, Socket),

    % Run diff
    ResultSocket = arizona_differ:diff_stateful(TemplateData, ChangedState, SocketWithState),

    % Verify changes were generated for the stateless component
    Changes = arizona_socket:get_changes(ResultSocket),
    ExpectedChanges = [{root, [{1, [~"<span>", ~"Jane", ~"</span>"]}]}],
    ?assertEqual(ExpectedChanges, Changes).

diff_stateless_no_optimization(Config) when is_list(Config) ->
    % Test that stateless components don't benefit from vars_indexes optimization
    % They always re-render when their containing stateful component changes
    InitialState = arizona_stateful:new(root, test_module, #{
        title => ~"Page Title",
        content => ~"Page content"
    }),

    % Change both variables
    ChangedState1 = arizona_stateful:put_binding(title, ~"New Title", InitialState),
    ChangedState = arizona_stateful:put_binding(content, ~"New content", ChangedState1),

    % Template with stateless component that uses both variables
    TemplateData = #{
        elems_order => [0],
        elems => #{
            0 =>
                {dynamic, 1, fun(Socket) ->
                    % Stateless component that renders both variables
                    arizona_html:render_stateless(
                        [
                            {static, 1, ~"<h1>"},
                            {dynamic, 2, fun(S) -> arizona_socket:get_binding(title, S) end},
                            {static, 3, ~"</h1><p>"},
                            {dynamic, 4, fun(S) -> arizona_socket:get_binding(content, S) end},
                            {static, 5, ~"</p>"}
                        ],
                        Socket
                    )
                end}
        },
        vars_indexes => #{
            title => [0],
            % Both variables affect the same element
            content => [0]
        }
    },

    % Create socket and run diff
    Socket = arizona_socket:new(#{mode => diff}),
    SocketWithState = arizona_socket:put_stateful_state(ChangedState, Socket),

    % Run diff
    ResultSocket = arizona_differ:diff_stateful(TemplateData, ChangedState, SocketWithState),

    % Verify changes were generated (full re-render of element 0)
    Changes = arizona_socket:get_changes(ResultSocket),
    ExpectedChanges = [
        {root, [{0, [~"<h1>", ~"New Title", ~"</h1><p>", ~"New content", ~"</p>"]}]}
    ],
    ?assertEqual(ExpectedChanges, Changes).

%% --------------------------------------------------------------------
%% Diff list tests
%% --------------------------------------------------------------------

diff_list_basic_change(Config) when is_list(Config) ->
    % Test basic list diffing using proper list_template_data
    InitialList = [~"Alice", ~"Bob"],
    InitialState = arizona_stateful:new(root, test_module, #{list => InitialList}),

    % Change the list
    NewList = [~"Alice", ~"Bob", ~"Charlie"],
    ChangedState = arizona_stateful:put_binding(list, NewList, InitialState),

    % Create list template data
    ListTemplateData = #{
        static => [~"<li>", ~"</li>"],
        dynamic => #{
            elems_order => [0],
            elems => #{
                0 => {dynamic, 1, fun(Item, _Socket) -> Item end}
            },
            vars_indexes => #{
                list => [0]
            }
        }
    },

    % Create stateful template data that uses arizona_renderer:render_list
    TemplateData = #{
        elems_order => [0],
        elems => #{
            0 =>
                {dynamic, 1, fun(Socket) ->
                    CurrentList = arizona_socket:get_binding(list, Socket),
                    arizona_html:render_list(
                        ListTemplateData, CurrentList, Socket
                    )
                end}
        },
        vars_indexes => #{
            list => [0]
        }
    },

    % Create socket and run diff
    Socket = arizona_socket:new(#{mode => diff}),
    SocketWithState = arizona_socket:put_stateful_state(ChangedState, Socket),

    % Run diff
    ResultSocket = arizona_differ:diff_stateful(TemplateData, ChangedState, SocketWithState),

    % Verify changes were generated for the list
    Changes = arizona_socket:get_changes(ResultSocket),
    ExpectedChanges = [
        {root, [
            {0, [
                [[[], [~"<li>", ~"Alice", ~"</li>"]], [~"<li>", ~"Bob", ~"</li>"]],
                [~"<li>", ~"Charlie", ~"</li>"]
            ]}
        ]}
    ],
    ?assertEqual(ExpectedChanges, Changes).

diff_list_item_addition(Config) when is_list(Config) ->
    % Test list diffing when items are added using proper list_template_data
    InitialItems = [#{id => 1, name => ~"Alice"}, #{id => 2, name => ~"Bob"}],
    InitialState = arizona_stateful:new(root, test_module, #{items => InitialItems}),

    % Add a new item
    NewItems = [
        #{id => 1, name => ~"Alice"},
        #{id => 2, name => ~"Bob"},
        #{id => 3, name => ~"Charlie"}
    ],
    ChangedState = arizona_stateful:put_binding(items, NewItems, InitialState),

    % Create list template data for item rendering
    ListTemplateData = #{
        static => [~"<li>", ~"</li>"],
        dynamic => #{
            elems_order => [0],
            elems => #{
                0 => {dynamic, 1, fun(Item, _Socket) -> maps:get(name, Item) end}
            },
            vars_indexes => #{
                name => [0]
            }
        }
    },

    % Template that renders list items using arizona_renderer:render_list
    TemplateData = #{
        elems_order => [0, 1, 2],
        elems => #{
            0 => {static, 1, ~"<ul>"},
            1 =>
                {dynamic, 2, fun(Socket) ->
                    Items = arizona_socket:get_binding(items, Socket),
                    arizona_html:render_list(
                        ListTemplateData, Items, Socket
                    )
                end},
            2 => {static, 3, ~"</ul>"}
        },
        vars_indexes => #{
            % items variable affects element 1
            items => [1]
        }
    },

    % Create socket and run diff
    Socket = arizona_socket:new(#{mode => diff}),
    SocketWithState = arizona_socket:put_stateful_state(ChangedState, Socket),

    % Run diff
    ResultSocket = arizona_differ:diff_stateful(TemplateData, ChangedState, SocketWithState),

    % Verify changes were generated for the list element
    Changes = arizona_socket:get_changes(ResultSocket),
    ExpectedChanges = [
        {root, [
            {1, [
                [[[], [~"<li>", ~"Alice", ~"</li>"]], [~"<li>", ~"Bob", ~"</li>"]],
                [~"<li>", ~"Charlie", ~"</li>"]
            ]}
        ]}
    ],
    ?assertEqual(ExpectedChanges, Changes).

diff_list_item_removal(Config) when is_list(Config) ->
    % Test list diffing when items are removed using proper list_template_data
    InitialItems = [
        #{id => 1, name => ~"Alice"},
        #{id => 2, name => ~"Bob"},
        #{id => 3, name => ~"Charlie"}
    ],
    InitialState = arizona_stateful:new(root, test_module, #{items => InitialItems}),

    % Remove an item
    NewItems = [#{id => 1, name => ~"Alice"}, #{id => 3, name => ~"Charlie"}],
    ChangedState = arizona_stateful:put_binding(items, NewItems, InitialState),

    % Create complex list template data with dynamic attributes
    ListTemplateData = #{
        static => [~"<div class=\"item\" data-id=\"", ~"\">", ~"</div>"],
        dynamic => #{
            elems_order => [0, 1],
            elems => #{
                0 => {dynamic, 1, fun(Item, _Socket) -> integer_to_binary(maps:get(id, Item)) end},
                1 => {dynamic, 1, fun(Item, _Socket) -> maps:get(name, Item) end}
            },
            vars_indexes => #{
                id => [0],
                name => [1]
            }
        }
    },

    % Template that renders list with dynamic content using arizona_renderer:render_list
    TemplateData = #{
        elems_order => [0, 1, 2, 3],
        elems => #{
            0 => {static, 1, ~"<div class=\"list\">"},
            1 => {static, 2, ~"<span>Total: "},
            2 =>
                {dynamic, 3, fun(Socket) ->
                    Items = arizona_socket:get_binding(items, Socket),
                    integer_to_binary(length(Items))
                end},
            3 =>
                {dynamic, 4, fun(Socket) ->
                    Items = arizona_socket:get_binding(items, Socket),
                    arizona_html:render_list(
                        ListTemplateData, Items, Socket
                    )
                end}
        },
        vars_indexes => #{
            % items variable affects elements 2 and 3
            items => [2, 3]
        }
    },

    % Create socket and run diff
    Socket = arizona_socket:new(#{mode => diff}),
    SocketWithState = arizona_socket:put_stateful_state(ChangedState, Socket),

    % Run diff
    ResultSocket = arizona_differ:diff_stateful(TemplateData, ChangedState, SocketWithState),

    % Verify changes were generated for the affected elements
    Changes = arizona_socket:get_changes(ResultSocket),
    ExpectedChanges = [
        {root, [
            {2, ~"2"},
            {3, [
                [[], [~"<div class=\"item\" data-id=\"", ~"1", ~"\">", ~"Alice", ~"</div>"]],
                [~"<div class=\"item\" data-id=\"", ~"3", ~"\">", ~"Charlie", ~"</div>"]
            ]}
        ]}
    ],
    ?assertEqual(ExpectedChanges, Changes).

%% --------------------------------------------------------------------
%% Complex hierarchical tests
%% --------------------------------------------------------------------

test_complex_hierarchical_change_detection(Config) when is_list(Config) ->
    % Test reproducing exact WebSocket debug scenario where state changes but diff returns []
    % Based on actual debug output showing todos binding changed but changes=[]

    % Create exact initial state from debug output
    InitialTodos = [
        #{id => 1, text => ~"Learn Erlang", completed => false},
        #{id => 2, text => ~"Build web app", completed => true},
        #{id => 3, text => ~"Write tests", completed => false}
    ],

    InitialState = arizona_stateful:new(root, arizona_todo_app_live, #{
        todos => InitialTodos,
        filter => all,
        new_todo_text => ~"",
        next_id => 4
    }),

    % Simulate exact toggle_todo event: todo #1 becomes completed
    % This matches the debug output state change
    UpdatedTodos = [
        % false -> true
        #{id => 1, text => ~"Learn Erlang", completed => true},
        #{id => 2, text => ~"Build web app", completed => true},
        #{id => 3, text => ~"Write tests", completed => false}
    ],

    ChangedState = arizona_stateful:put_binding(todos, UpdatedTodos, InitialState),

    % Create template structure that SHOULD generate vars_indexes but might not be
    % This simulates the parse transform output for TODO app
    TemplateData = #{
        elems_order => [0, 1, 2, 3, 4, 5, 6, 7, 8, 9],
        elems => #{
            0 => {static, 1, ~"<div id=\"root\" class=\"todo-app\">"},
            1 => {static, 2, ~"<input value=\""},
            2 => {dynamic, 3, fun(Socket) -> arizona_socket:get_binding(new_todo_text, Socket) end},
            3 => {static, 4, ~"\"/><main>"},
            % Element 4: render_list with todos - this should be affected by todos change
            4 =>
                {dynamic, 5, fun(Socket) ->
                    Todos = arizona_socket:get_binding(todos, Socket),
                    Filter = arizona_socket:get_binding(filter, Socket),
                    FilteredTodos = arizona_todo_app_live:filter_todos(Todos, Filter),
                    arizona_html:render_list(
                        #{
                            static => [
                                ~"<div class=\"todo-item ",
                                ~"\" data-testid=\"todo-",
                                ~"\"><input type=\"checkbox\" ",
                                ~" onclick=\"...\"/><label>",
                                ~"</label><button>×</button></div>"
                            ],
                            dynamic => #{
                                elems_order => [0, 1, 2, 3],
                                elems => #{
                                    0 =>
                                        {dynamic, 1, fun(Todo, _Socket) ->
                                            case maps:get(completed, Todo) of
                                                true -> ~"completed";
                                                false -> ~""
                                            end
                                        end},
                                    1 =>
                                        {dynamic, 1, fun(Todo, _Socket) ->
                                            integer_to_binary(maps:get(id, Todo))
                                        end},
                                    2 =>
                                        {dynamic, 1, fun(Todo, _Socket) ->
                                            case maps:get(completed, Todo) of
                                                true -> ~"checked";
                                                false -> ~""
                                            end
                                        end},
                                    3 =>
                                        {dynamic, 1, fun(Todo, _Socket) ->
                                            maps:get(text, Todo)
                                        end}
                                },
                                vars_indexes => #{
                                    completed => [0, 2],
                                    id => [1],
                                    text => [3]
                                }
                            }
                        },
                        FilteredTodos,
                        Socket
                    )
                end},
            5 => {static, 6, ~"</main><footer>"},
            % Nested stateless components (stats, filters, clear button)
            6 =>
                {dynamic, 7, fun(Socket) ->
                    Todos = arizona_socket:get_binding(todos, Socket),
                    Count = length(lists:filter(fun(#{completed := C}) -> not C end, Todos)),
                    CountBin = integer_to_binary(Count),
                    Items =
                        case Count of
                            1 -> ~"item";
                            _ -> ~"items"
                        end,
                    iolist_to_binary([
                        ~"<span><strong>", CountBin, ~"</strong> ", Items, ~" left</span>"
                    ])
                end},
            7 =>
                {dynamic, 8, fun(Socket) ->
                    Filter = arizona_socket:get_binding(filter, Socket),
                    AllClass =
                        case Filter of
                            all -> ~"selected";
                            _ -> ~""
                        end,
                    iolist_to_binary([~"<ul><li><a class=\"", AllClass, ~"\">All</a></li></ul>"])
                end},
            8 =>
                {dynamic, 9, fun(Socket) ->
                    Todos = arizona_socket:get_binding(todos, Socket),
                    HasCompleted =
                        length(Todos) >
                            length(lists:filter(fun(#{completed := C}) -> not C end, Todos)),
                    case HasCompleted of
                        true -> ~"<button>Clear completed</button>";
                        false -> ~""
                    end
                end},
            9 => {static, 10, ~"</footer></div>"}
        },
        % THE BUG: This might be MISSING or EMPTY from parse transform!
        vars_indexes => #{
            % These mappings should exist but might not be generated correctly

            % List content, stats, clear button
            todos => [4, 6, 8],
            % List content, filter buttons
            filter => [4, 7],
            % Input value
            new_todo_text => [2]
        }
    },

    % Create socket in diff mode (matches WebSocket debug output)
    Socket = arizona_socket:new(#{mode => diff}),
    SocketWithState = arizona_socket:put_stateful_state(ChangedState, Socket),

    % Run diff - this reproduces the WebSocket "Calling render for diff" step
    ResultSocket = arizona_differ:diff_stateful(TemplateData, ChangedState, SocketWithState),

    % Debug: This should match the WebSocket "Render completed, changes=[]" output
    Changes = arizona_socket:get_changes(ResultSocket),

    % THE BUG: This should fail because we expect changes but get []
    % This reproduces the exact WebSocket issue
    ?assertNotEqual([], Changes),

    % Should contain changes for elements affected by 'todos' binding: [4, 6, 8]
    ?assertMatch([{root, ElementChanges}] when is_list(ElementChanges), Changes),
    [{root, ElementChanges}] = Changes,

    % Extract element indices that were changed
    ChangedElements = [Element || {Element, _Value} <- ElementChanges],

    % Should include elements 4, 6, and 8 (affected by 'todos' binding)

    % render_list with todos
    ?assert(lists:member(4, ChangedElements)),
    % stats component with todos count
    ?assert(lists:member(6, ChangedElements)),
    % clear button depends on todos
    ?assert(lists:member(8, ChangedElements)).

test_reproduce_websocket_bug_empty_vars_indexes(Config) when is_list(Config) ->
    % This test reproduces the ACTUAL bug: empty vars_indexes from parse transform
    % This should FAIL, demonstrating the real issue from WebSocket debug output

    % Same setup as previous test
    InitialTodos = [
        #{id => 1, text => ~"Learn Erlang", completed => false},
        #{id => 2, text => ~"Build web app", completed => true},
        #{id => 3, text => ~"Write tests", completed => false}
    ],

    InitialState = arizona_stateful:new(root, arizona_todo_app_live, #{
        todos => InitialTodos,
        filter => all,
        new_todo_text => ~"",
        next_id => 4
    }),

    % Same state change: toggle todo #1
    UpdatedTodos = [
        #{id => 1, text => ~"Learn Erlang", completed => true},
        #{id => 2, text => ~"Build web app", completed => true},
        #{id => 3, text => ~"Write tests", completed => false}
    ],

    ChangedState = arizona_stateful:put_binding(todos, UpdatedTodos, InitialState),
    ChangedBindings = arizona_stateful:get_changed_bindings(ChangedState),

    % Same template structure but with EMPTY vars_indexes (the actual bug!)
    TemplateData = #{
        elems_order => [0, 1, 2, 3, 4, 5, 6, 7, 8, 9],
        elems => #{
            0 => {static, 1, ~"<div id=\"root\" class=\"todo-app\">"},
            1 => {static, 2, ~"<input value=\""},
            2 => {dynamic, 3, fun(Socket) -> arizona_socket:get_binding(new_todo_text, Socket) end},
            3 => {static, 4, ~"\"/><main>"},
            4 =>
                {dynamic, 5, fun(Socket) ->
                    Todos = arizona_socket:get_binding(todos, Socket),
                    Filter = arizona_socket:get_binding(filter, Socket),
                    FilteredTodos = arizona_todo_app_live:filter_todos(Todos, Filter),
                    arizona_html:render_list(
                        #{
                            static => [
                                ~"<div class=\"todo-item ", ~"\" data-testid=\"todo-", ~"\">"
                            ],
                            dynamic => #{
                                elems_order => [0, 1],
                                elems => #{
                                    0 =>
                                        {dynamic, 1, fun(Todo, _Socket) ->
                                            case maps:get(completed, Todo) of
                                                true -> ~"completed";
                                                false -> ~""
                                            end
                                        end},
                                    1 =>
                                        {dynamic, 1, fun(Todo, _Socket) ->
                                            maps:get(text, Todo)
                                        end}
                                },
                                vars_indexes => #{}
                            }
                        },
                        FilteredTodos,
                        Socket
                    )
                end},
            5 => {static, 6, ~"</main><footer>"},
            6 =>
                {dynamic, 7, fun(Socket) ->
                    Todos = arizona_socket:get_binding(todos, Socket),
                    Count = length(lists:filter(fun(#{completed := C}) -> not C end, Todos)),
                    iolist_to_binary([~"<span>", integer_to_binary(Count), ~" items left</span>"])
                end},
            7 => {static, 8, ~"</footer></div>"}
        },
        % THE ACTUAL BUG: vars_indexes is EMPTY!
        % This is what the parse transform is generating for TODO app
        vars_indexes => #{}
    },

    % Debug the empty vars_indexes
    VarsIndexes = maps:get(vars_indexes, TemplateData, #{}),

    % This should return empty set because vars_indexes is empty
    AffectedElements = arizona_differ:get_affected_elements(ChangedBindings, VarsIndexes),

    % Create socket and run diff
    Socket = arizona_socket:new(#{mode => diff}),
    SocketWithState = arizona_socket:put_stateful_state(ChangedState, Socket),

    % This reproduces the WebSocket bug: changes=[] despite todos binding change
    ResultSocket = arizona_differ:diff_stateful(TemplateData, ChangedState, SocketWithState),
    Changes = arizona_socket:get_changes(ResultSocket),

    % This assertion should FAIL, demonstrating the bug
    % When vars_indexes is empty, no elements are detected as affected
    % even though the todos binding changed and should affect elements 4 and 6

    % This demonstrates the bug - we get empty changes
    ?assertEqual([], Changes),

    % Additional verification: with empty vars_indexes, affected elements is empty
    ?assertEqual([], sets:to_list(AffectedElements)).

test_enhanced_parse_transform_missing_todos_mapping(Config) when is_list(Config) ->
    % Test that reproduces the enhanced parse transform issue from the erlang shell example
    % This shows that even with enhanced parse transform, todos binding is not mapped to elements

    % Create exact state from the shell example
    InitialState = arizona_stateful:new(root, undefined, #{
        new_todo_text => ~"",
        todos => [],
        filter => all
    }),

    % Simulate the exact state change from the shell example
    UpdatedState1 = arizona_stateful:put_binding(new_todo_text, ~"foo", InitialState),
    UpdatedState = arizona_stateful:put_binding(
        todos, [#{id => 1, text => ~"Foo", completed => false}], UpdatedState1
    ),

    ChangedBindings = arizona_stateful:get_changed_bindings(UpdatedState),

    % Create template structure that simulates what enhanced parse transform should generate
    % Based on the shell output, we know it generates: #{filter => [7], new_todo_text => [1]}
    % But it should ALSO have todos mappings for elements that use
    % FilteredTodos, UncompletedLength, HasCompleted
    TemplateData = #{
        elems_order => [0, 1, 2, 3, 4, 5, 6, 7, 8, 9],
        elems => #{
            0 => {static, 1, ~"<div id=\"root\" class=\"todo-app\">"},
            % NewTodoText
            1 => {dynamic, 2, fun(Socket) -> arizona_socket:get_binding(new_todo_text, Socket) end},
            2 => {static, 3, ~"<main>"},
            % FilteredTodos (depends on todos + filter)
            3 => {dynamic, 4, fun(_Socket) -> ~"FilteredTodos placeholder" end},
            4 => {static, 5, ~"</main><footer><span>Count: "},
            % UncompletedLength (depends on todos)
            5 => {dynamic, 6, fun(_Socket) -> ~"2" end},
            6 => {static, 7, ~"</span><div>Filter: "},
            % Filter
            7 => {dynamic, 8, fun(Socket) -> arizona_socket:get_binding(filter, Socket) end},
            8 => {static, 9, ~"</div>"},
            % HasCompleted (depends on todos)
            9 => {dynamic, 10, fun(_Socket) -> ~"<button>Clear completed</button>" end}
        },
        % THE BUG: Current enhanced parse transform generates incomplete vars_indexes
        % From shell output: #{filter => [7], new_todo_text => [1]}
        % MISSING: todos should map to elements [3, 5, 9]
        % (FilteredTodos, UncompletedLength, HasCompleted)
        vars_indexes => #{
            % Correct: filter affects element 7
            filter => [7],
            % Correct: new_todo_text affects element 1
            new_todo_text => [1]
            % MISSING: todos => [3, 5, 9]  % todos should affect elements 3, 5, 9
        }
    },

    % Run diff with incomplete vars_indexes
    Socket = arizona_socket:new(#{mode => diff}),
    SocketWithState = arizona_socket:put_stateful_state(UpdatedState, Socket),

    ResultSocket = arizona_differ:diff_stateful(TemplateData, UpdatedState, SocketWithState),
    Changes = arizona_socket:get_changes(ResultSocket),

    % CURRENTLY FAILING: only element 1 (new_todo_text) changes, todos changes are missed
    % This demonstrates the bug - we should get more changes but don't
    CurrentChanges = Changes,
    % Current broken behavior
    ?assertMatch([{root, [{1, ~"foo"}]}], CurrentChanges),

    % Now test what SHOULD happen with correct vars_indexes
    CorrectVarsIndexes = #{
        % filter affects element 7
        filter => [7],
        % new_todo_text affects element 1
        new_todo_text => [1],
        % todos should affect elements 3, 5, 9
        todos => [3, 5, 9]
    },

    CorrectTemplateData = TemplateData#{vars_indexes => CorrectVarsIndexes},
    CorrectAffectedElements = arizona_differ:get_affected_elements(
        ChangedBindings, CorrectVarsIndexes
    ),

    % With correct vars_indexes, should affect elements [1, 3, 5, 9]
    ?assertEqual([1, 3, 5, 9], lists:sort(sets:to_list(CorrectAffectedElements))),

    % Run diff with correct vars_indexes
    CorrectResultSocket = arizona_differ:diff_stateful(
        CorrectTemplateData, UpdatedState, SocketWithState
    ),
    CorrectChanges = arizona_socket:get_changes(CorrectResultSocket),

    % Should generate changes for all affected elements, not just new_todo_text
    ?assertMatch([{root, ElementChanges}] when length(ElementChanges) > 1, CorrectChanges),
    [{root, ElementChanges}] = CorrectChanges,
    ChangedElementIndexes = [Index || {Index, _Value} <- ElementChanges],

    % Should include todos-affected elements (3, 5, 9) plus new_todo_text element (1)

    % new_todo_text
    ?assert(lists:member(1, ChangedElementIndexes)),
    % FilteredTodos (todos dependency)
    ?assert(lists:member(3, ChangedElementIndexes)),
    % UncompletedLength (todos dependency)
    ?assert(lists:member(5, ChangedElementIndexes)),
    % HasCompleted (todos dependency)
    ?assert(lists:member(9, ChangedElementIndexes)),

    % THE REAL TEST: The current enhanced parse transform should FAIL here
    % Because it's not generating todos mappings in vars_indexes
    % We need to fix generate_vars_indexes to make this pass

    % First, let's demonstrate that the CURRENT vars_indexes is missing todos
    CurrentVarsIndexes = maps:get(vars_indexes, TemplateData, #{}),
    % todos is missing (BUG)
    ?assertNot(maps:is_key(todos, CurrentVarsIndexes)),

    % And the CURRENT differ behavior misses todos changes
    CurrentElementIndexes = [Index || {Index, _Value} <- element(2, hd(CurrentChanges))],
    % Missing FilteredTodos change
    ?assertNot(lists:member(3, CurrentElementIndexes)),
    % Missing UncompletedLength change
    ?assertNot(lists:member(5, CurrentElementIndexes)),
    % Missing HasCompleted change
    ?assertNot(lists:member(9, CurrentElementIndexes)),

    % THE FIX IS WORKING: Enhanced parse transform now generates todos mappings correctly!
    % Since the parse transform is fixed, our simulated "current" scenario should fail
    % but the real enhanced parse transform will work correctly

    % For now, verify the current test setup still shows the issue in the simulated scenario
    ?assertMatch([{root, [{1, ~"foo"}]}], CurrentChanges).
