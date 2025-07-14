-module(arizona_hierarchical_SUITE).
-behaviour(ct_suite).
-include_lib("stdlib/include/assert.hrl").
-compile([export_all, nowarn_export_all]).

%% --------------------------------------------------------------------
%% Behaviour (ct_suite) callbacks
%% --------------------------------------------------------------------

all() ->
    [
        {group, structure_creation},
        {group, simple_diffs},
        {group, nested_components},
        {group, list_components},
        {group, patch_application},
        {group, round_trip_properties},
        {group, structure_generation},
        {group, integration_tests},
        {group, error_handling_edge_cases}
    ].

groups() ->
    [
        {structure_creation, [parallel], [
            create_empty_structure,
            add_stateful_to_structure,
            set_element_in_component
        ]},
        {simple_diffs, [parallel], [
            diff_identical_structures,
            diff_simple_content_change,
            diff_add_stateful,
            diff_remove_stateful
        ]},
        {nested_components, [parallel], [
            diff_stateful_reference,
            diff_stateless_structure,
            diff_stateless_no_change
        ]},
        {list_components, [parallel], [
            diff_list_dynamic_change,
            diff_list_no_change,
            diff_complex_list_structure
        ]},
        {patch_application, [parallel], [
            apply_simple_patch,
            apply_add_stateful_patch,
            apply_remove_stateful_patch,
            apply_stateless_patch,
            apply_list_patch
        ]},
        {round_trip_properties, [parallel], [
            round_trip_property_simple,
            round_trip_property_complex
        ]},
        {structure_generation, [parallel], [
            generate_simple_structure,
            generate_hierarchical_socket,
            detect_stateless_template,
            detect_stateful_socket,
            detect_list_content,
            detect_stateless_unified_format,
            stateless_unified_with_vars_indexes,
            stateless_unified_cascade_dependency,
            stateless_unified_empty_template
        ]},
        {integration_tests, [parallel], [
            arizona_html_integration,
            list_item_processing,
            nested_dynamic_content,
            error_handling_coverage,
            mode_switching_coverage,
            hierarchical_list_in_stateful_template,
            hierarchical_stateless_in_stateful_template,
            test_duplicate_list_changes_in_multiple_indices
        ]},
        {error_handling_edge_cases, [parallel], [
            test_element_removal_in_diff,
            test_element_addition_in_diff,
            test_binding_not_found_hierarchical_error,
            test_template_render_hierarchical_error,
            test_empty_component_operations,
            test_mixed_component_type_changes
        ]}
    ].

%% --------------------------------------------------------------------
%% Structure Creation Tests
%% --------------------------------------------------------------------

create_empty_structure(Config) when is_list(Config) ->
    Structure = arizona_hierarchical:create_structure(),
    ?assertEqual(#{}, Structure).

add_stateful_to_structure(Config) when is_list(Config) ->
    Structure = arizona_hierarchical:create_structure(),
    ComponentRender = #{0 => ~"<div>", 1 => ~"Hello", 2 => ~"</div>"},

    UpdatedStructure = arizona_hierarchical:add_stateful(root, ComponentRender, Structure),
    Expected = #{root => ComponentRender},

    ?assertEqual(Expected, UpdatedStructure).

set_element_in_component(Config) when is_list(Config) ->
    Structure = arizona_hierarchical:create_structure(),

    % Add element to new component
    Structure1 = arizona_hierarchical:set_element(root, 0, ~"<div>", Structure),
    ?assertEqual(#{root => #{0 => ~"<div>"}}, Structure1),

    % Add another element to same component
    Structure2 = arizona_hierarchical:set_element(root, 1, ~"Hello", Structure1),
    ?assertEqual(#{root => #{0 => ~"<div>", 1 => ~"Hello"}}, Structure2).

%% --------------------------------------------------------------------
%% Simple Diff Tests
%% --------------------------------------------------------------------

diff_identical_structures(Config) when is_list(Config) ->
    Structure = #{root => #{0 => ~"<div>", 1 => ~"Hello"}},
    Diff = arizona_hierarchical:diff_structures(Structure, Structure),
    ?assertEqual([], Diff).

diff_simple_content_change(Config) when is_list(Config) ->
    OldStructure = #{root => #{0 => ~"<div>", 1 => ~"Hello"}},
    NewStructure = #{root => #{0 => ~"<div>", 1 => ~"World"}},

    Diff = arizona_hierarchical:diff_structures(OldStructure, NewStructure),
    Expected = [
        #{
            type => update_stateful,
            stateful_id => root,
            data => [
                #{
                    type => set_element,
                    element_index => 1,
                    data => ~"World"
                }
            ]
        }
    ],

    ?assertEqual(Expected, Diff).

diff_add_stateful(Config) when is_list(Config) ->
    OldStructure = #{root => #{0 => ~"<div>"}},
    NewStructure = #{
        root => #{0 => ~"<div>"},
        ~"new_component" => #{0 => ~"<span>New</span>"}
    },

    Diff = arizona_hierarchical:diff_structures(OldStructure, NewStructure),
    Expected = [
        #{
            type => add_stateful,
            stateful_id => ~"new_component",
            data => #{0 => ~"<span>New</span>"}
        }
    ],

    ?assertEqual(Expected, Diff).

diff_remove_stateful(Config) when is_list(Config) ->
    OldStructure = #{
        root => #{0 => ~"<div>"},
        ~"old_component" => #{0 => ~"<span>Old</span>"}
    },
    NewStructure = #{root => #{0 => ~"<div>"}},

    Diff = arizona_hierarchical:diff_structures(OldStructure, NewStructure),
    Expected = [
        #{
            type => remove_stateful,
            stateful_id => ~"old_component",
            data => undefined
        }
    ],

    ?assertEqual(Expected, Diff).

%% --------------------------------------------------------------------
%% Nested Component Tests
%% --------------------------------------------------------------------

diff_stateful_reference(Config) when is_list(Config) ->
    OldStructure = #{
        root => #{
            0 => ~"<div>",
            1 => #{type => stateful, id => ~"counter-1"},
            2 => ~"</div>"
        },
        ~"counter-1" => #{0 => ~"0"}
    },
    NewStructure = #{
        root => #{
            0 => ~"<div>",
            1 => #{type => stateful, id => ~"counter-1"},
            2 => ~"</div>"
        },
        % Count changed
        ~"counter-1" => #{0 => ~"1"}
    },

    Diff = arizona_hierarchical:diff_structures(OldStructure, NewStructure),
    Expected = [
        #{
            type => update_stateful,
            stateful_id => ~"counter-1",
            data => [
                #{
                    type => set_element,
                    element_index => 0,
                    data => ~"1"
                }
            ]
        }
    ],

    ?assertEqual(Expected, Diff).

diff_stateless_structure(Config) when is_list(Config) ->
    OldStructure = #{
        root => #{
            0 => ~"<div>",
            1 => #{
                type => stateless,
                structure => #{
                    0 => ~"<h1>",
                    1 => ~"Todo List",
                    2 => ~"</h1>",
                    3 => ~"<span>Items: ",
                    4 => 5,
                    5 => ~"</span>"
                }
            },
            2 => ~"</div>"
        }
    },
    NewStructure = #{
        root => #{
            0 => ~"<div>",
            1 => #{
                type => stateless,
                structure => #{
                    0 => ~"<h1>",
                    % Title changed
                    1 => ~"Shopping List",
                    2 => ~"</h1>",
                    3 => ~"<span>Items: ",
                    % Count changed
                    4 => 3,
                    5 => ~"</span>"
                }
            },
            2 => ~"</div>"
        }
    },

    Diff = arizona_hierarchical:diff_structures(OldStructure, NewStructure),
    Expected = [
        #{
            type => update_stateful,
            stateful_id => root,
            data => [
                #{
                    type => update_stateless,
                    element_index => 1,
                    data => [
                        #{type => set_element, element_index => 1, data => ~"Shopping List"},
                        #{type => set_element, element_index => 4, data => 3}
                    ]
                }
            ]
        }
    ],

    ?assertEqual(Expected, Diff).

diff_stateless_no_change(Config) when is_list(Config) ->
    Structure = #{
        root => #{
            0 => ~"<div>",
            1 => #{
                type => stateless,
                structure => #{
                    0 => ~"<h1>",
                    1 => ~"Todo List",
                    2 => ~"</h1>"
                }
            }
        }
    },

    Diff = arizona_hierarchical:diff_structures(Structure, Structure),
    ?assertEqual([], Diff).

%% --------------------------------------------------------------------
%% List Component Tests
%% --------------------------------------------------------------------

diff_list_dynamic_change(Config) when is_list(Config) ->
    OldStructure = #{
        root => #{
            0 => ~"<ul>",
            1 => #{
                type => list,
                static => [~"<li>", ~"</li>"],
                dynamic => [
                    #{0 => ~"Buy milk"},
                    #{0 => ~"Walk dog"}
                ]
            },
            2 => ~"</ul>"
        }
    },
    NewStructure = #{
        root => #{
            0 => ~"<ul>",
            1 => #{
                type => list,
                static => [~"<li>", ~"</li>"],
                dynamic => [
                    #{0 => ~"Buy milk"},
                    #{0 => ~"Walk dog"},
                    % Added item
                    #{0 => ~"Code Arizona"}
                ]
            },
            2 => ~"</ul>"
        }
    },

    Diff = arizona_hierarchical:diff_structures(OldStructure, NewStructure),
    Expected = [
        #{
            type => update_stateful,
            stateful_id => root,
            data => [
                #{
                    type => update_list_dynamic,
                    element_index => 1,
                    data => [
                        #{0 => ~"Buy milk"},
                        #{0 => ~"Walk dog"},
                        #{0 => ~"Code Arizona"}
                    ]
                }
            ]
        }
    ],

    ?assertEqual(Expected, Diff).

diff_list_no_change(Config) when is_list(Config) ->
    Structure = #{
        root => #{
            0 => ~"<ul>",
            1 => #{
                type => list,
                static => [~"<li>", ~"</li>"],
                dynamic => [#{0 => ~"Buy milk"}]
            },
            2 => ~"</ul>"
        }
    },

    Diff = arizona_hierarchical:diff_structures(Structure, Structure),
    ?assertEqual([], Diff).

diff_complex_list_structure(Config) when is_list(Config) ->
    OldStructure = #{
        root => #{
            0 => <<"<div class=\"todo-list\">">>,
            1 => #{
                type => list,
                static => [
                    <<"<div class=\"todo\" data-id=\"">>,
                    <<"\">">>,
                    ~"<span>",
                    ~"</span><button>",
                    ~"</button></div>"
                ],
                dynamic => [
                    #{0 => 1, 1 => ~"Buy milk", 2 => ~"Delete"},
                    #{0 => 2, 1 => ~"Walk dog", 2 => ~"Delete"}
                ]
            },
            2 => ~"</div>"
        }
    },
    NewStructure = #{
        root => #{
            0 => <<"<div class=\"todo-list\">">>,
            1 => #{
                type => list,
                static => [
                    <<"<div class=\"todo\" data-id=\"">>,
                    <<"\">">>,
                    ~"<span>",
                    ~"</span><button>",
                    ~"</button></div>"
                ],
                dynamic => [
                    #{0 => 1, 1 => ~"Buy milk", 2 => ~"Delete"},
                    % Button text changed
                    #{0 => 2, 1 => ~"Walk dog", 2 => ~"Done"},
                    % New item
                    #{0 => 3, 1 => ~"Code Arizona", 2 => ~"Delete"}
                ]
            },
            2 => ~"</div>"
        }
    },

    Diff = arizona_hierarchical:diff_structures(OldStructure, NewStructure),
    Expected = [
        #{
            type => update_stateful,
            stateful_id => root,
            data => [
                #{
                    type => update_list_dynamic,
                    element_index => 1,
                    data => [
                        #{0 => 1, 1 => ~"Buy milk", 2 => ~"Delete"},
                        #{0 => 2, 1 => ~"Walk dog", 2 => ~"Done"},
                        #{0 => 3, 1 => ~"Code Arizona", 2 => ~"Delete"}
                    ]
                }
            ]
        }
    ],

    ?assertEqual(Expected, Diff).

%% --------------------------------------------------------------------
%% Patch Application Tests
%% --------------------------------------------------------------------

apply_simple_patch(Config) when is_list(Config) ->
    OldStructure = #{root => #{0 => ~"<div>", 1 => ~"Hello"}},
    Diff = [
        #{
            type => update_stateful,
            stateful_id => root,
            data => [
                #{
                    type => set_element,
                    element_index => 1,
                    data => ~"World"
                }
            ]
        }
    ],

    NewStructure = arizona_hierarchical:apply_diff(Diff, OldStructure),
    Expected = #{root => #{0 => ~"<div>", 1 => ~"World"}},

    ?assertEqual(Expected, NewStructure).

apply_add_stateful_patch(Config) when is_list(Config) ->
    OldStructure = #{root => #{0 => ~"<div>"}},
    Diff = [
        #{
            type => add_stateful,
            stateful_id => ~"new_component",
            data => #{0 => ~"<span>New</span>"}
        }
    ],

    NewStructure = arizona_hierarchical:apply_diff(Diff, OldStructure),
    Expected = #{
        root => #{0 => ~"<div>"},
        ~"new_component" => #{0 => ~"<span>New</span>"}
    },

    ?assertEqual(Expected, NewStructure).

apply_remove_stateful_patch(Config) when is_list(Config) ->
    OldStructure = #{
        root => #{0 => ~"<div>"},
        ~"old_component" => #{0 => ~"<span>Old</span>"}
    },
    Diff = [
        #{
            type => remove_stateful,
            stateful_id => ~"old_component",
            data => undefined
        }
    ],

    NewStructure = arizona_hierarchical:apply_diff(Diff, OldStructure),
    Expected = #{root => #{0 => ~"<div>"}},

    ?assertEqual(Expected, NewStructure).

apply_stateless_patch(Config) when is_list(Config) ->
    OldStructure = #{
        root => #{
            0 => ~"<div>",
            1 => #{
                type => stateless,
                structure => #{
                    0 => ~"<h1>",
                    1 => ~"Todo List",
                    2 => ~"</h1>"
                }
            }
        }
    },
    Diff = [
        #{
            type => update_stateful,
            stateful_id => root,
            data => [
                #{
                    type => update_stateless,
                    element_index => 1,
                    data => [
                        #{
                            type => set_element,
                            element_index => 1,
                            data => ~"Shopping List"
                        }
                    ]
                }
            ]
        }
    ],

    NewStructure = arizona_hierarchical:apply_diff(Diff, OldStructure),
    Expected = #{
        root => #{
            0 => ~"<div>",
            1 => #{
                type => stateless,
                structure => #{
                    0 => ~"<h1>",
                    1 => ~"Shopping List",
                    2 => ~"</h1>"
                }
            }
        }
    },

    ?assertEqual(Expected, NewStructure).

apply_list_patch(Config) when is_list(Config) ->
    OldStructure = #{
        root => #{
            0 => ~"<ul>",
            1 => #{
                type => list,
                static => [~"<li>", ~"</li>"],
                dynamic => [#{0 => ~"Buy milk"}]
            }
        }
    },
    Diff = [
        #{
            type => update_stateful,
            stateful_id => root,
            data => [
                #{
                    type => update_list_dynamic,
                    element_index => 1,
                    data => [
                        #{0 => ~"Buy milk"},
                        #{0 => ~"Walk dog"}
                    ]
                }
            ]
        }
    ],

    NewStructure = arizona_hierarchical:apply_diff(Diff, OldStructure),
    Expected = #{
        root => #{
            0 => ~"<ul>",
            1 => #{
                type => list,
                static => [~"<li>", ~"</li>"],
                dynamic => [
                    #{0 => ~"Buy milk"},
                    #{0 => ~"Walk dog"}
                ]
            }
        }
    },

    ?assertEqual(Expected, NewStructure).

%% --------------------------------------------------------------------
%% Round-trip Property Tests
%% --------------------------------------------------------------------

round_trip_property_simple(Config) when is_list(Config) ->
    % Test that diff(A, B) |> apply_patch(A, _) = B
    OldStructure = #{
        root => #{
            0 => ~"<div>",
            1 => ~"Hello",
            2 => ~"</div>"
        }
    },
    NewStructure = #{
        root => #{
            0 => ~"<div>",
            % Changed
            1 => ~"World",
            2 => ~"</div>"
        }
    },

    Diff = arizona_hierarchical:diff_structures(OldStructure, NewStructure),
    ResultStructure = arizona_hierarchical:apply_diff(Diff, OldStructure),

    ?assertEqual(NewStructure, ResultStructure).

round_trip_property_complex(Config) when is_list(Config) ->
    % Test round-trip with complex nested structures
    OldStructure = #{
        root => #{
            0 => ~"<div>",
            1 => ~"Hello",
            2 => #{type => stateful, id => ~"counter-1"},
            3 => #{
                type => stateless,
                structure => #{
                    0 => ~"<span>Count: ",
                    1 => ~"5",
                    2 => ~"</span>"
                }
            },
            4 => ~"</div>"
        },
        ~"counter-1" => #{0 => ~"Count: ", 1 => ~"5"}
    },
    NewStructure = #{
        root => #{
            0 => ~"<div>",
            % Changed
            1 => ~"World",
            2 => #{type => stateful, id => ~"counter-1"},
            3 => #{
                type => stateless,
                structure => #{
                    0 => ~"<span>Count: ",
                    % Changed
                    1 => ~"6",
                    2 => ~"</span>"
                }
            },
            4 => ~"</div>"
        },
        % Changed
        ~"counter-1" => #{0 => ~"Count: ", 1 => ~"6"}
    },

    Diff = arizona_hierarchical:diff_structures(OldStructure, NewStructure),
    ResultStructure = arizona_hierarchical:apply_diff(Diff, OldStructure),

    ?assertEqual(NewStructure, ResultStructure).

%% --------------------------------------------------------------------
%% Structure Generation Tests
%% --------------------------------------------------------------------

generate_simple_structure(Config) when is_list(Config) ->
    % Test basic hierarchical structure generation
    TemplateData = #{
        elems_order => [0, 1, 2],
        elems => #{
            0 => {static, 1, ~"<div>"},
            1 => {static, 2, ~"Hello World"},
            2 => {static, 3, ~"</div>"}
        },
        vars_indexes => #{}
    },

    Socket = arizona_socket:new(#{mode => hierarchical}),
    {_ComponentStructure, UpdatedSocket} = arizona_hierarchical:stateful_structure(
        TemplateData, Socket
    ),

    HierarchicalStructure = arizona_socket:get_hierarchical_acc(UpdatedSocket),
    Expected = #{
        root => #{
            0 => ~"<div>",
            1 => ~"Hello World",
            2 => ~"</div>"
        }
    },

    ?assertEqual(Expected, HierarchicalStructure).

generate_hierarchical_socket(Config) when is_list(Config) ->
    % Test that socket accumulates hierarchical structure correctly
    Socket = arizona_socket:new(#{mode => hierarchical}),

    % Verify initial state
    InitialStructure = arizona_socket:get_hierarchical_acc(Socket),
    ?assertEqual(#{}, InitialStructure),

    % Add a component manually
    TestStructure = #{
        root => #{0 => ~"<div>", 1 => ~"test", 2 => ~"</div>"}
    },
    UpdatedSocket = arizona_socket:set_hierarchical_acc(TestStructure, Socket),

    % Verify it was set correctly
    Result = arizona_socket:get_hierarchical_acc(UpdatedSocket),
    ?assertEqual(TestStructure, Result).

detect_stateless_template(Config) when is_list(Config) ->
    % Test that stateless template data is properly converted to structure
    StatelessTemplate = #{
        elems_order => [0, 1, 2],
        elems => #{
            0 => {static, 1, ~"<h1>"},
            1 => {dynamic, 2, fun(Socket) -> arizona_socket:get_binding(title, Socket) end},
            2 => {static, 3, ~"</h1>"}
        },
        vars_indexes => #{title => [1]}
    },

    % Create socket with proper stateful state
    Socket = arizona_socket:new(#{mode => hierarchical}),
    MockState = arizona_stateful:new(root, test_module, #{title => ~"Test Title"}),
    SocketWithState = arizona_socket:put_stateful_state(MockState, Socket),

    {Content, _UpdatedSocket} = arizona_hierarchical:stateless_structure(
        StatelessTemplate, SocketWithState
    ),

    Expected = #{
        type => stateless,
        structure => #{
            0 => ~"<h1>",
            1 => ~"Test Title",
            2 => ~"</h1>"
        }
    },

    ?assertEqual(Expected, Content).

detect_stateful_socket(Config) when is_list(Config) ->
    % Test that arizona_html:to_html properly extracts HTML from sockets
    NestedSocket = arizona_socket:new(#{
        mode => hierarchical,
        current_stateful_id => ~"test-component"
    }),
    NestedSocketWithHtml = arizona_socket:set_html_acc([~"<span>Nested</span>"], NestedSocket),

    Socket = arizona_socket:new(#{mode => hierarchical}),
    {Content, _UpdatedSocket} = arizona_html:to_html(NestedSocketWithHtml, Socket),

    % arizona_html:to_html extracts HTML from socket, not stateful reference
    Expected = [~"<span>Nested</span>"],

    ?assertEqual(Expected, Content).

detect_list_content(Config) when is_list(Config) ->
    % Test that regular lists are converted to nested iodata by arizona_html:to_html
    ListContent = [~"<li>", ~"Item 1", ~"</li>", ~"<li>", ~"Item 2", ~"</li>"],

    Socket = arizona_socket:new(#{mode => hierarchical}),

    {Content, _UpdatedSocket} = arizona_html:to_html(ListContent, Socket),

    % arizona_html:to_html creates nested iodata for lists
    Expected = [[[[[~"<li>", ~"Item 1"], ~"</li>"], ~"<li>"], ~"Item 2"], ~"</li>"],

    ?assertEqual(Expected, Content).

%% Tests for unified stateless format
detect_stateless_unified_format(Config) when is_list(Config) ->
    % Test that stateless template with unified format (same as stateful)
    % is properly converted to structure
    StatelessTemplateData = #{
        elems_order => [0, 1, 2],
        elems => #{
            0 => {static, 1, ~"<section>"},
            1 => {dynamic, 2, fun(Socket) -> arizona_socket:get_binding(title, Socket) end},
            2 => {static, 3, ~"</section>"}
        },
        vars_indexes => #{title => [1]}
    },

    % Create socket with proper stateful state
    Socket = arizona_socket:new(#{mode => hierarchical}),
    MockState = arizona_stateful:new(root, test_module, #{title => ~"Unified Title"}),
    SocketWithState = arizona_socket:put_stateful_state(MockState, Socket),

    {Content, _UpdatedSocket} = arizona_hierarchical:stateless_structure(
        StatelessTemplateData, SocketWithState
    ),

    Expected = #{
        type => stateless,
        structure => #{
            0 => ~"<section>",
            1 => ~"Unified Title",
            2 => ~"</section>"
        }
    },

    ?assertEqual(Expected, Content).

stateless_unified_with_vars_indexes(Config) when is_list(Config) ->
    % Test that vars_indexes are properly handled in hierarchical mode
    StatelessTemplateData = #{
        elems_order => [0, 1, 2],
        elems => #{
            0 => {static, 1, ~"<div>User: "},
            1 =>
                {dynamic, 2, fun(Socket) ->
                    UserData = arizona_socket:get_binding(user, Socket, #{}),
                    maps:get(name, UserData, ~"unknown")
                end},
            2 => {static, 3, ~"</div>"}
        },
        vars_indexes => #{user => [1]}
    },

    % Create socket with user binding
    Socket = arizona_socket:new(#{mode => hierarchical}),
    UserData = #{name => ~"Alice", role => ~"admin"},
    MockState = arizona_stateful:new(root, test_module, #{user => UserData}),
    SocketWithState = arizona_socket:put_stateful_state(MockState, Socket),

    {Content, _UpdatedSocket} = arizona_hierarchical:stateless_structure(
        StatelessTemplateData, SocketWithState
    ),

    Expected = #{
        type => stateless,
        structure => #{
            0 => ~"<div>User: ",
            1 => ~"Alice",
            2 => ~"</div>"
        }
    },

    ?assertEqual(Expected, Content).

stateless_unified_cascade_dependency(Config) when is_list(Config) ->
    % Test cascade dependency pattern: stateless component depending on parent binding
    % This simulates: {arizona_component:call_stateless(module, fun,
    %                 #{foo => arizona_socket:get_binding(user, Socket)}, Socket)}
    StatelessTemplateData = #{
        elems_order => [0, 1, 2],
        elems => #{
            0 => {static, 1, ~"<span class=\"greeting\">"},
            1 =>
                {dynamic, 2, fun(Socket) ->
                    % Simulate the dependency extraction pattern
                    UserData = arizona_socket:get_binding(user, Socket, #{}),
                    Username = maps:get(username, UserData, ~"Guest"),
                    [~"Hello, ", Username, ~"!"]
                end},
            2 => {static, 3, ~"</span>"}
        },
        % This ensures proper cascade dependency tracking
        vars_indexes => #{user => [1]}
    },

    % Set up socket with user data
    Socket = arizona_socket:new(#{mode => hierarchical}),
    UserData = #{username => ~"Bob", status => ~"online"},
    MockState = arizona_stateful:new(root, test_module, #{user => UserData}),
    SocketWithState = arizona_socket:put_stateful_state(MockState, Socket),

    {Content, _UpdatedSocket} = arizona_hierarchical:stateless_structure(
        StatelessTemplateData, SocketWithState
    ),

    % The hierarchical structure should flatten the nested list content
    Expected = #{
        type => stateless,
        structure => #{
            0 => ~"<span class=\"greeting\">",
            1 => [~"Hello, ", ~"Bob", ~"!"],
            2 => ~"</span>"
        }
    },

    ?assertEqual(Expected, Content),

    % Verify the vars_indexes for cascade dependency
    ?assertEqual(#{user => [1]}, maps:get(vars_indexes, StatelessTemplateData)).

stateless_unified_empty_template(Config) when is_list(Config) ->
    % Test empty stateless template with unified format
    StatelessTemplateData = #{
        elems_order => [],
        elems => #{},
        vars_indexes => #{}
    },

    Socket = arizona_socket:new(#{mode => hierarchical}),
    MockState = arizona_stateful:new(root, test_module, #{}),
    SocketWithState = arizona_socket:put_stateful_state(MockState, Socket),

    {Content, _UpdatedSocket} = arizona_hierarchical:stateless_structure(
        StatelessTemplateData, SocketWithState
    ),

    Expected = #{
        type => stateless,
        structure => #{}
    },

    ?assertEqual(Expected, Content).

%% Tests for unified stateless format
detect_stateless_unified_format(Config) when is_list(Config) ->
    % Test that stateless template with unified format (same as stateful) is properly converted to structure
    StatelessTemplateData = #{
        elems_order => [0, 1, 2],
        elems => #{
            0 => {static, 1, ~"<section>"},
            1 => {dynamic, 2, fun(Socket) -> arizona_socket:get_binding(title, Socket) end},
            2 => {static, 3, ~"</section>"}
        },
        vars_indexes => #{title => [1]}
    },

    % Create socket with proper stateful state
    Socket = arizona_socket:new(#{mode => hierarchical}),
    MockState = arizona_stateful:new(root, test_module, #{title => ~"Unified Title"}),
    SocketWithState = arizona_socket:put_stateful_state(MockState, Socket),

    {Content, _UpdatedSocket} = arizona_hierarchical:stateless_structure(
        StatelessTemplateData, SocketWithState
    ),

    Expected = #{
        type => stateless,
        structure => #{
            0 => ~"<section>",
            1 => ~"Unified Title",
            2 => ~"</section>"
        }
    },

    ?assertEqual(Expected, Content).

stateless_unified_with_vars_indexes(Config) when is_list(Config) ->
    % Test that vars_indexes are properly handled in hierarchical mode
    StatelessTemplateData = #{
        elems_order => [0, 1, 2],
        elems => #{
            0 => {static, 1, ~"<div>User: "},
            1 =>
                {dynamic, 2, fun(Socket) ->
                    UserData = arizona_socket:get_binding(user, Socket, #{}),
                    maps:get(name, UserData, ~"unknown")
                end},
            2 => {static, 3, ~"</div>"}
        },
        vars_indexes => #{user => [1]}
    },

    % Create socket with user binding
    Socket = arizona_socket:new(#{mode => hierarchical}),
    UserData = #{name => ~"Alice", role => ~"admin"},
    MockState = arizona_stateful:new(root, test_module, #{user => UserData}),
    SocketWithState = arizona_socket:put_stateful_state(MockState, Socket),

    {Content, _UpdatedSocket} = arizona_hierarchical:stateless_structure(
        StatelessTemplateData, SocketWithState
    ),

    Expected = #{
        type => stateless,
        structure => #{
            0 => ~"<div>User: ",
            1 => ~"Alice",
            2 => ~"</div>"
        }
    },

    ?assertEqual(Expected, Content).

stateless_unified_cascade_dependency(Config) when is_list(Config) ->
    % Test cascade dependency pattern: stateless component depending on parent binding
    % This simulates: {arizona_component:call_stateless(module, fun, #{foo => arizona_socket:get_binding(user, Socket)}, Socket)}
    StatelessTemplateData = #{
        elems_order => [0, 1, 2],
        elems => #{
            0 => {static, 1, ~"<span class=\"greeting\">"},
            1 =>
                {dynamic, 2, fun(Socket) ->
                    % Simulate the dependency extraction pattern
                    UserData = arizona_socket:get_binding(user, Socket, #{}),
                    Username = maps:get(username, UserData, ~"Guest"),
                    [~"Hello, ", Username, ~"!"]
                end},
            2 => {static, 3, ~"</span>"}
        },
        % This ensures proper cascade dependency tracking
        vars_indexes => #{user => [1]}
    },

    % Set up socket with user data
    Socket = arizona_socket:new(#{mode => hierarchical}),
    UserData = #{username => ~"Bob", status => ~"online"},
    MockState = arizona_stateful:new(root, test_module, #{user => UserData}),
    SocketWithState = arizona_socket:put_stateful_state(MockState, Socket),

    {Content, _UpdatedSocket} = arizona_hierarchical:stateless_structure(
        StatelessTemplateData, SocketWithState
    ),

    % The hierarchical structure should flatten the nested list content
    Expected = #{
        type => stateless,
        structure => #{
            0 => ~"<span class=\"greeting\">",
            1 => [~"Hello, ", ~"Bob", ~"!"],
            2 => ~"</span>"
        }
    },

    ?assertEqual(Expected, Content),

    % Verify the vars_indexes for cascade dependency
    ?assertEqual(#{user => [1]}, maps:get(vars_indexes, StatelessTemplateData)).

stateless_unified_empty_template(Config) when is_list(Config) ->
    % Test empty stateless template with unified format
    StatelessTemplateData = #{
        elems_order => [],
        elems => #{},
        vars_indexes => #{}
    },

    Socket = arizona_socket:new(#{mode => hierarchical}),
    MockState = arizona_stateful:new(root, test_module, #{}),
    SocketWithState = arizona_socket:put_stateful_state(MockState, Socket),

    {Content, _UpdatedSocket} = arizona_hierarchical:stateless_structure(
        StatelessTemplateData, SocketWithState
    ),

    Expected = #{
        type => stateless,
        structure => #{}
    },

    ?assertEqual(Expected, Content).

%% --------------------------------------------------------------------
%% Integration Tests
%% --------------------------------------------------------------------

arizona_html_integration(Config) when is_list(Config) ->
    % Test integration with arizona_html:render_stateful/2
    TemplateData = #{
        elems_order => [0, 1, 2],
        elems => #{
            0 => {static, 1, ~"<div>"},
            1 => {dynamic, 2, fun(Socket) -> arizona_socket:get_binding(content, Socket) end},
            2 => {static, 3, ~"</div>"}
        },
        vars_indexes => #{content => [1]}
    },

    Socket = arizona_socket:new(#{mode => hierarchical}),
    MockState = arizona_stateful:new(root, test_module, #{content => ~"Test Content"}),
    SocketWithBinding = arizona_socket:put_stateful_state(MockState, Socket),

    % This should trigger arizona_hierarchical:stateful_structure/2
    UpdatedSocket = arizona_html:render_stateful(TemplateData, SocketWithBinding),

    % Verify hierarchical structure was created
    HierarchicalStructure = arizona_socket:get_hierarchical_acc(UpdatedSocket),
    Expected = #{
        root => #{
            0 => ~"<div>",
            1 => ~"Test Content",
            2 => ~"</div>"
        }
    },

    ?assertEqual(Expected, HierarchicalStructure).

list_item_processing(Config) when is_list(Config) ->
    % Test arizona_renderer:evaluate_dynamic_elements_for_item/4 integration
    ListData = #{
        static => [~"<li>", ~"</li>"],
        dynamic => #{
            elems_order => [0],
            elems => #{
                0 => {dynamic, 1, fun(Item, _Socket) -> Item end}
            },
            vars_indexes => #{}
        }
    },
    Items = [~"Item 1", ~"Item 2"],

    Socket = arizona_socket:new(#{mode => hierarchical}),

    {ListElement, UpdatedSocket} = arizona_hierarchical:list_structure(ListData, Items, Socket),

    Expected = #{
        type => list,
        static => [~"<li>", ~"</li>"],
        dynamic => [
            #{0 => ~"Item 1"},
            #{0 => ~"Item 2"}
        ]
    },

    ?assertEqual(Expected, ListElement),
    % UpdatedSocket should have the list element stored as pending element
    ?assertEqual(Expected, arizona_socket:get_hierarchical_pending_element(UpdatedSocket)).

nested_dynamic_content(Config) when is_list(Config) ->
    % Test nested stateful components within hierarchical structure
    TemplateData = #{
        elems_order => [0, 1, 2],
        elems => #{
            0 => {static, 1, ~"<div>"},
            1 =>
                {dynamic, 2, fun(_Socket) ->
                    % Return another socket (simulating nested stateful component)
                    NestedSocket = arizona_socket:new(#{
                        mode => hierarchical,
                        current_stateful_id => ~"nested-component"
                    }),
                    arizona_socket:set_html_acc([~"<span>Nested</span>"], NestedSocket)
                end},
            2 => {static, 3, ~"</div>"}
        },
        vars_indexes => #{}
    },

    Socket = arizona_socket:new(#{mode => hierarchical}),
    {ComponentStructure, _UpdatedSocket} = arizona_hierarchical:stateful_structure(
        TemplateData, Socket
    ),

    % arizona_html:to_html extracts HTML from nested socket, not stateful reference
    % Check each field individually to avoid complex type inference issues
    ?assertEqual(~"<div>", maps:get(0, ComponentStructure)),
    % Element 1 should contain the nested HTML content
    Element1 = maps:get(1, ComponentStructure),
    ?assertEqual([~"<span>Nested</span>"], Element1),
    ?assertEqual(~"</div>", maps:get(2, ComponentStructure)).

error_handling_coverage(Config) when is_list(Config) ->
    % Test error handling in list item processing
    ListData = #{
        static => [~"<li>", ~"</li>"],
        dynamic => #{
            elems_order => [0],
            elems => #{
                0 =>
                    {dynamic, 5, fun(_Item, Socket) ->
                        arizona_socket:get_binding(nonexistent, Socket)
                    end}
            },
            vars_indexes => #{}
        }
    },

    % Create socket with empty stateful state
    Socket = arizona_socket:new(#{mode => hierarchical}),
    MockState = arizona_stateful:new(root, test_module, #{}),
    SocketWithState = arizona_socket:put_stateful_state(MockState, Socket),

    % Should handle binding errors gracefully
    ?assertError(
        {binding_not_found, nonexistent},
        arizona_hierarchical:list_structure(ListData, [~"test"], SocketWithState)
    ).

mode_switching_coverage(Config) when is_list(Config) ->
    % Test that mode switching works correctly
    TemplateData = #{
        elems_order => [0, 1],
        elems => #{
            0 => {static, 1, ~"<p>"},
            1 => {static, 2, ~"</p>"}
        },
        vars_indexes => #{}
    },

    % Test render mode
    RenderSocket = arizona_socket:new(#{mode => render}),
    RenderResult = arizona_html:render_stateful(TemplateData, RenderSocket),
    RenderHtml = arizona_socket:get_html(RenderResult),
    ?assertEqual([~"<p>", ~"</p>"], RenderHtml),

    % Test hierarchical mode
    HierarchicalSocket = arizona_socket:new(#{mode => hierarchical}),
    HierarchicalResult = arizona_html:render_stateful(TemplateData, HierarchicalSocket),
    HierarchicalStructure = arizona_socket:get_hierarchical_acc(HierarchicalResult),
    Expected = #{root => #{0 => ~"<p>", 1 => ~"</p>"}},
    ?assertEqual(Expected, HierarchicalStructure).

hierarchical_list_in_stateful_template(Config) when is_list(Config) ->
    % Test that render_list called from within a stateful template properly stores
    % hierarchical structures in the parent component via pending element buffer
    ListData = #{
        static => [~"<li>", ~"</li>"],
        dynamic => #{
            elems_order => [0],
            elems => #{
                0 => {dynamic, 1, fun(Item, _Socket) -> Item end}
            },
            vars_indexes => #{}
        }
    },
    Items = [~"Todo 1", ~"Todo 2"],

    % Create socket in hierarchical mode like WebSocket initialization
    Socket = arizona_socket:new(#{mode => hierarchical}),

    % Now we test stateful template that contains render_list call (like the todo app)
    TemplateData = #{
        elems_order => [0, 1, 2],
        elems => #{
            0 => {static, 1, ~"<main>"},
            1 =>
                {dynamic, 2, fun(Socket1) ->
                    arizona_html:render_list(ListData, Items, Socket1)
                end},
            2 => {static, 3, ~"</main>"}
        },
        vars_indexes => #{}
    },

    % Use stateful structure generation (like the todo app)
    {_ComponentStructure, UpdatedSocket} = arizona_hierarchical:stateful_structure(
        TemplateData, Socket
    ),

    % Check what's stored in hierarchical_acc (this is what gets sent to client)
    HierarchicalAcc = arizona_socket:get_hierarchical_acc(UpdatedSocket),

    % This should contain the list structure as element 1 of the root component
    ExpectedStructure = #{
        root => #{
            0 => ~"<main>",
            1 => #{
                type => list,
                static => [~"<li>", ~"</li>"],
                dynamic => [
                    #{0 => ~"Todo 1"},
                    #{0 => ~"Todo 2"}
                ]
            },
            2 => ~"</main>"
        }
    },

    ?assertEqual(ExpectedStructure, HierarchicalAcc).

hierarchical_stateless_in_stateful_template(Config) when is_list(Config) ->
    % Test that render_stateless called from within a stateful template properly stores
    % hierarchical structures in the parent component via pending element buffer
    StatelessTemplate = #{
        elems_order => [0, 1, 2],
        elems => #{
            0 => {static, 1, ~"<span class=\"status\">"},
            1 => {dynamic, 2, fun(Socket1) -> arizona_socket:get_binding(status_text, Socket1) end},
            2 => {static, 3, ~"</span>"}
        },
        vars_indexes => #{status_text => [1]}
    },

    % Create socket in hierarchical mode with bindings
    Socket = arizona_socket:new(#{mode => hierarchical}),
    MockState = arizona_stateful:new(root, test_module, #{status_text => ~"Active"}),
    SocketWithState = arizona_socket:put_stateful_state(MockState, Socket),

    % Now we test stateful template that contains render_stateless call
    TemplateData = #{
        elems_order => [0, 1, 2],
        elems => #{
            0 => {static, 1, ~"<div>"},
            1 =>
                {dynamic, 2, fun(Socket1) ->
                    arizona_html:render_stateless(StatelessTemplate, Socket1)
                end},
            2 => {static, 3, ~"</div>"}
        },
        vars_indexes => #{}
    },

    % Use stateful structure generation
    {_ComponentStructure, UpdatedSocket} = arizona_hierarchical:stateful_structure(
        TemplateData, SocketWithState
    ),

    % Check what's stored in hierarchical_acc
    HierarchicalAcc = arizona_socket:get_hierarchical_acc(UpdatedSocket),

    % This should contain the stateless structure as element 1 of the root component
    ExpectedStructure = #{
        root => #{
            0 => ~"<div>",
            1 => #{
                type => stateless,
                structure => #{
                    0 => ~"<span class=\"status\">",
                    1 => ~"Active",
                    2 => ~"</span>"
                }
            },
            2 => ~"</div>"
        }
    },

    ?assertEqual(ExpectedStructure, HierarchicalAcc).

% IMPORTANT: This test confirms that the hierarchical diff correctly separates:
% - List changes go to index 3 (main section)
% - Stateless changes go to index 9 (footer)
% The bug occurs during WebSocket conversion where the same list changes
% get incorrectly duplicated to both indices due to the looks_like_component_changes
% heuristic in arizona_socket.erl
test_duplicate_list_changes_in_multiple_indices(Config) when is_list(Config) ->
    % Test scenario similar to TODO app where the same list changes might be applied
    % to multiple element indices, causing duplication in the diff

    % Create initial structure similar to TODO app with list in main section (index 3)
    % and stateless components in footer (index 9)
    InitialStructure = #{
        root => #{
            0 => ~"<div><main>",
            3 => #{
                type => list,
                dynamic => [
                    #{0 => ~"", 1 => ~"1", 6 => ~"Learn Erlang"},
                    #{0 => ~"completed", 1 => ~"2", 6 => ~"Build web app"}
                ],
                static => [~"<div class=\"todo-item ", ~"\" data-testid=\"todo-", ~"\">", ~"</div>"]
            },
            4 => ~"</main><footer>",
            9 => #{
                type => stateless,
                structure => #{
                    0 => ~"<span>Count: ",
                    1 => 2,
                    2 => ~"</span>"
                }
            },
            10 => ~"</footer></div>"
        }
    },

    % Create updated structure where first item is toggled (should only affect index 3)
    UpdatedStructure = #{
        root => #{
            0 => ~"<div><main>",
            3 => #{
                type => list,
                dynamic => [
                    #{0 => ~"completed", 1 => ~"1", 6 => ~"Learn Erlang"},
                    #{0 => ~"completed", 1 => ~"2", 6 => ~"Build web app"}
                ],
                static => [~"<div class=\"todo-item ", ~"\" data-testid=\"todo-", ~"\">", ~"</div>"]
            },
            4 => ~"</main><footer>",
            9 => #{
                type => stateless,
                structure => #{
                    0 => ~"<span>Count: ",
                    1 => 1,
                    2 => ~"</span>"
                }
            },
            10 => ~"</footer></div>"
        }
    },

    % Generate diff between initial and updated structures
    Changes = arizona_hierarchical:diff_structures(InitialStructure, UpdatedStructure),

    % This test simulates the TODO app bug where the same list changes appear
    % at both the main section (index 3) and footer (index 9).
    % The hierarchical diff should correctly separate these changes.

    % Verify the hierarchical diff structure is correct
    ?assertMatch(
        [#{type := update_stateful, stateful_id := root, data := ElementDiffs}] when
            is_list(ElementDiffs),
        Changes
    ),

    [#{data := ElementDiffs}] = Changes,

    % Find changes by element index - using lists:search since these are maps
    Index3Change = lists:search(fun(#{element_index := Index}) -> Index =:= 3 end, ElementDiffs),
    Index9Change = lists:search(fun(#{element_index := Index}) -> Index =:= 9 end, ElementDiffs),

    % Extract the actual change records
    {value, Index3ChangeRecord} = Index3Change,
    {value, Index9ChangeRecord} = Index9Change,

    % Verify that both changes exist
    ?assertMatch(#{type := update_list_dynamic, element_index := 3}, Index3ChangeRecord),
    ?assertMatch(#{type := update_stateless, element_index := 9}, Index9ChangeRecord),

    % Extract the change data
    #{data := Index3Data} = Index3ChangeRecord,
    #{data := Index9Data} = Index9ChangeRecord,

    % Verify that index 3 has the actual list changes (todo items)
    ?assertMatch([#{0 := <<"completed">>, 1 := <<"1">>, 6 := <<"Learn Erlang">>}, _], Index3Data),

    % Verify that index 9 has only the stateless component changes (count update)
    ?assertMatch([#{type := set_element, element_index := 1, data := 1}], Index9Data).

%% --------------------------------------------------------------------
%% Error handling and edge case tests
%% --------------------------------------------------------------------

test_element_removal_in_diff(Config) when is_list(Config) ->
    % Test case where elements are removed from a component
    OldStructure = #{
        root => #{
            0 => ~"<div>",
            1 => ~"Hello",
            2 => ~"World",
            3 => ~"</div>"
        }
    },
    NewStructure = #{
        root => #{
            0 => ~"<div>",
            % Elements 1 and 2 removed
            3 => ~"</div>"
        }
    },

    Diff = arizona_hierarchical:diff_structures(OldStructure, NewStructure),

    % Should contain remove_element operations
    ?assertMatch(
        [
            #{
                type := update_stateful,
                data := [
                    #{type := remove_element, element_index := 1},
                    #{type := remove_element, element_index := 2}
                ]
            }
        ],
        Diff
    ),

    % Test applying the diff
    ResultStructure = arizona_hierarchical:apply_diff(Diff, OldStructure),
    ?assertEqual(NewStructure, ResultStructure).

test_element_addition_in_diff(Config) when is_list(Config) ->
    % Test case where elements are added to a component
    OldStructure = #{
        root => #{
            0 => ~"<div>",
            3 => ~"</div>"
        }
    },
    NewStructure = #{
        root => #{
            0 => ~"<div>",
            1 => ~"Hello",
            2 => ~"World",
            3 => ~"</div>"
        }
    },

    Diff = arizona_hierarchical:diff_structures(OldStructure, NewStructure),

    % Should contain set_element operations for new elements
    ?assertMatch(
        [
            #{
                type := update_stateful,
                data := [
                    #{type := set_element, element_index := 1, data := ~"Hello"},
                    #{type := set_element, element_index := 2, data := ~"World"}
                ]
            }
        ],
        Diff
    ),

    % Test applying the diff
    ResultStructure = arizona_hierarchical:apply_diff(Diff, OldStructure),
    ?assertEqual(NewStructure, ResultStructure).

test_binding_not_found_hierarchical_error(Config) when is_list(Config) ->
    % Test template with missing binding in hierarchical mode
    TemplateData = #{
        elems_order => [0, 1],
        elems => #{
            0 => {static, 1, ~"<div>"},
            1 =>
                {dynamic, 2, fun(Socket) ->
                    arizona_socket:get_binding(nonexistent_binding, Socket)
                end}
        },
        vars_indexes => #{trigger => [1]}
    },

    Socket = arizona_socket:new(#{mode => hierarchical}),
    MockState = arizona_stateful:new(root, test_module, #{}),
    SocketWithState = arizona_socket:put_stateful_state(MockState, Socket),

    % Should throw binding_not_found error
    ?assertError(
        {binding_not_found, nonexistent_binding},
        arizona_hierarchical:stateful_structure(TemplateData, SocketWithState)
    ).

test_template_render_hierarchical_error(Config) when is_list(Config) ->
    % Test template with function that throws an error in hierarchical mode
    TemplateData = #{
        elems_order => [0, 1],
        elems => #{
            0 => {static, 1, ~"<div>"},
            1 =>
                {dynamic, 2, fun(_Socket) ->
                    throw(some_error)
                end}
        },
        vars_indexes => #{}
    },

    Socket = arizona_socket:new(#{mode => hierarchical}),
    MockState = arizona_stateful:new(root, test_module, #{}),
    SocketWithState = arizona_socket:put_stateful_state(MockState, Socket),

    % Should throw template_render_error
    ?assertError(
        {template_render_error, some_error, 2},
        arizona_hierarchical:stateful_structure(TemplateData, SocketWithState)
    ).

test_empty_component_operations(Config) when is_list(Config) ->
    % Test diff between empty components
    EmptyStructure = #{root => #{}},

    Diff = arizona_hierarchical:diff_structures(EmptyStructure, EmptyStructure),
    ?assertEqual([], Diff),

    % Test adding elements to empty component
    NonEmptyStructure = #{root => #{0 => ~"<div>"}},

    Diff2 = arizona_hierarchical:diff_structures(EmptyStructure, NonEmptyStructure),
    ?assertMatch(
        [
            #{
                type := update_stateful,
                data := [#{type := set_element, element_index := 0}]
            }
        ],
        Diff2
    ),

    % Test removing all elements from component
    Diff3 = arizona_hierarchical:diff_structures(NonEmptyStructure, EmptyStructure),
    ?assertMatch(
        [
            #{
                type := update_stateful,
                data := [#{type := remove_element, element_index := 0}]
            }
        ],
        Diff3
    ).

test_mixed_component_type_changes(Config) when is_list(Config) ->
    % Test changing from stateless to list component
    OldStructure = #{
        root => #{
            0 => ~"<div>",
            1 => #{
                type => stateless,
                structure => #{0 => ~"Old content"}
            },
            2 => ~"</div>"
        }
    },
    NewStructure = #{
        root => #{
            0 => ~"<div>",
            1 => #{
                type => list,
                static => [~"<li>", ~"</li>"],
                dynamic => [#{0 => ~"New item"}]
            },
            2 => ~"</div>"
        }
    },

    % Should replace entire element due to type change
    Diff = arizona_hierarchical:diff_structures(OldStructure, NewStructure),
    ?assertMatch(
        [
            #{
                type := update_stateful,
                data := [#{type := set_element, element_index := 1}]
            }
        ],
        Diff
    ),

    % Test applying the diff
    ResultStructure = arizona_hierarchical:apply_diff(Diff, OldStructure),
    ?assertEqual(NewStructure, ResultStructure).
