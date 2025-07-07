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
        {group, round_trip_properties}
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
            round_trip_property_complex,
            json_conversion_identity
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
        ~"counter-1" => #{0 => 0}
    },
    NewStructure = #{
        root => #{
            0 => ~"<div>",
            1 => #{type => stateful, id => ~"counter-1"},
            2 => ~"</div>"
        },
        % Count changed
        ~"counter-1" => #{0 => 1}
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
                    data => 1
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
                    1 => 5,
                    2 => ~"</span>"
                }
            },
            4 => ~"</div>"
        },
        ~"counter-1" => #{0 => ~"Count: ", 1 => 5}
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
                    1 => 6,
                    2 => ~"</span>"
                }
            },
            4 => ~"</div>"
        },
        % Changed
        ~"counter-1" => #{0 => ~"Count: ", 1 => 6}
    },

    Diff = arizona_hierarchical:diff_structures(OldStructure, NewStructure),
    ResultStructure = arizona_hierarchical:apply_diff(Diff, OldStructure),

    ?assertEqual(NewStructure, ResultStructure).

json_conversion_identity(Config) when is_list(Config) ->
    Structure = #{
        root => #{
            0 => ~"<div>",
            1 => #{type => stateful, id => ~"counter-1"},
            2 => #{
                type => list,
                static => [~"<li>", ~"</li>"],
                dynamic => [#{0 => ~"Item 1"}]
            }
        }
    },

    % to_json and from_json should be identity for our structure
    JsonData = arizona_hierarchical:to_json(Structure),
    BackToErlang = arizona_hierarchical:from_json(JsonData),

    ?assertEqual(Structure, JsonData),
    ?assertEqual(Structure, BackToErlang).
