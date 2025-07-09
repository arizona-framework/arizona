-module(arizona_socket_SUITE).
-behaviour(ct_suite).
-include_lib("stdlib/include/assert.hrl").
-compile([export_all, nowarn_export_all]).

%% --------------------------------------------------------------------
%% Behaviour (ct_suite) callbacks
%% --------------------------------------------------------------------

all() ->
    [
        {group, socket_creation_tests},
        {group, socket_property_tests},
        {group, html_management_tests},
        {group, stateful_state_tests},
        {group, binding_tests},
        {group, changes_accumulator_tests},
        {group, binding_management_tests},
        {group, error_handling_tests}
    ].

groups() ->
    [
        {socket_creation_tests, [parallel], [
            test_socket_creation,
            test_socket_creation_with_options,
            test_socket_creation_defaults
        ]},
        {socket_property_tests, [parallel], [
            test_get_mode,
            test_get_current_stateful_id,
            test_set_current_stateful_id,
            test_set_current_stateful_id_validation,
            test_is_socket_validation
        ]},
        {html_management_tests, [parallel], [
            test_html_accumulation,
            test_get_html_empty
        ]},
        {stateful_state_tests, [parallel], [
            test_stateful_state_management,
            test_get_stateful_states,
            test_find_stateful_state,
            test_get_current_stateful_state
        ]},
        {binding_tests, [parallel], [
            test_temp_bindings,
            test_with_temp_bindings,
            test_get_temp_binding,
            test_get_temp_binding_not_found,
            test_get_binding_temp_priority,
            test_get_binding_stateful_fallback,
            test_get_binding_not_found
        ]},
        {changes_accumulator_tests, [parallel], [
            test_append_changes,
            test_get_changes,
            test_clear_changes,
            test_merge_changes_same_component,
            test_merge_changes_different_components,
            test_merge_nested_changes,
            test_merge_changes_edge_cases,
            test_merge_changes_html_data_vs_component_changes
        ]},
        {binding_management_tests, [parallel], [
            test_put_binding,
            test_put_bindings
        ]},
        {error_handling_tests, [parallel], [
            test_get_stateful_state_not_found,
            test_get_current_stateful_state_not_found
        ]}
    ].

%% --------------------------------------------------------------------
%% Socket Creation Tests
%% --------------------------------------------------------------------

test_socket_creation(Config) when is_list(Config) ->
    %% Test basic socket creation
    Opts = #{mode => render, current_stateful_id => root},
    Socket = arizona_socket:new(Opts),

    ?assert(arizona_socket:is_socket(Socket)),
    ?assertEqual(render, arizona_socket:get_mode(Socket)),
    ?assertEqual(root, arizona_socket:get_current_stateful_id(Socket)).

test_socket_creation_with_options(Config) when is_list(Config) ->
    %% Test socket creation with all options
    Opts = #{
        mode => diff,
        current_stateful_parent_id => ~"parent_1",
        current_stateful_id => ~"child_1"
    },
    Socket = arizona_socket:new(Opts),

    ?assertEqual(diff, arizona_socket:get_mode(Socket)),
    ?assertEqual(~"child_1", arizona_socket:get_current_stateful_id(Socket)).

test_socket_creation_defaults(Config) when is_list(Config) ->
    %% Test socket creation with minimal options (defaults)
    Opts = #{},
    Socket = arizona_socket:new(Opts),

    ?assertEqual(render, arizona_socket:get_mode(Socket)),
    ?assertEqual(root, arizona_socket:get_current_stateful_id(Socket)).

%% --------------------------------------------------------------------
%% Socket Property Tests
%% --------------------------------------------------------------------

test_get_mode(Config) when is_list(Config) ->
    RenderSocket = arizona_socket:new(#{mode => render}),
    DiffSocket = arizona_socket:new(#{mode => diff}),

    ?assertEqual(render, arizona_socket:get_mode(RenderSocket)),
    ?assertEqual(diff, arizona_socket:get_mode(DiffSocket)).

test_get_current_stateful_id(Config) when is_list(Config) ->
    Socket1 = arizona_socket:new(#{current_stateful_id => root}),
    Socket2 = arizona_socket:new(#{current_stateful_id => ~"test_id"}),

    ?assertEqual(root, arizona_socket:get_current_stateful_id(Socket1)),
    ?assertEqual(~"test_id", arizona_socket:get_current_stateful_id(Socket2)).

test_set_current_stateful_id(Config) when is_list(Config) ->
    Socket = arizona_socket:new(#{current_stateful_id => root}),

    %% Test setting root
    Socket2 = arizona_socket:set_current_stateful_id(root, Socket),
    ?assertEqual(root, arizona_socket:get_current_stateful_id(Socket2)),

    %% Test setting binary id
    Socket3 = arizona_socket:set_current_stateful_id(~"new_id", Socket),
    ?assertEqual(~"new_id", arizona_socket:get_current_stateful_id(Socket3)).

test_set_current_stateful_id_validation(Config) when is_list(Config) ->
    Socket = arizona_socket:new(#{}),

    %% Test invalid id types should fail
    ?assertError(function_clause, arizona_socket:set_current_stateful_id(invalid_atom, Socket)),
    ?assertError(function_clause, arizona_socket:set_current_stateful_id(123, Socket)).

%% --------------------------------------------------------------------
%% HTML Management Tests
%% --------------------------------------------------------------------

test_html_accumulation(Config) when is_list(Config) ->
    Socket = arizona_socket:new(#{}),

    %% Test setting HTML
    Html1 = [~"<div>", ~"content", ~"</div>"],
    Socket2 = arizona_socket:set_html_acc(Html1, Socket),
    ?assertEqual(Html1, arizona_socket:get_html(Socket2)),

    %% Test accumulating more HTML
    Html2 = [~"<span>", ~"more", ~"</span>"],
    Socket3 = arizona_socket:set_html_acc(Html2, Socket2),
    ?assertEqual(Html2, arizona_socket:get_html(Socket3)).

test_get_html_empty(Config) when is_list(Config) ->
    Socket = arizona_socket:new(#{}),

    %% New socket should have empty HTML
    Html = arizona_socket:get_html(Socket),
    ?assertEqual([], Html).

%% --------------------------------------------------------------------
%% Stateful State Tests
%% --------------------------------------------------------------------

test_stateful_state_management(Config) when is_list(Config) ->
    Socket = arizona_socket:new(#{}),

    %% Create mock stateful state
    MockState = create_mock_stateful_state(~"comp_1", test_module, #{name => ~"Test"}),

    %% Put stateful state
    Socket2 = arizona_socket:put_stateful_state(MockState, Socket),

    %% Get stateful state
    RetrievedState = arizona_socket:get_stateful_state(~"comp_1", Socket2),
    ?assertEqual(MockState, RetrievedState).

test_get_stateful_states(Config) when is_list(Config) ->
    Socket = arizona_socket:new(#{}),

    %% Initially empty
    ?assertEqual(#{}, arizona_socket:get_stateful_states(Socket)),

    %% Add some states
    State1 = create_mock_stateful_state(root, mod1, #{a => 1}),
    State2 = create_mock_stateful_state(~"comp_1", mod2, #{b => 2}),

    Socket2 = arizona_socket:put_stateful_state(State1, Socket),
    Socket3 = arizona_socket:put_stateful_state(State2, Socket2),

    States = arizona_socket:get_stateful_states(Socket3),
    ?assertEqual(#{root => State1, ~"comp_1" => State2}, States).

test_find_stateful_state(Config) when is_list(Config) ->
    Socket = arizona_socket:new(#{}),
    MockState = create_mock_stateful_state(~"test", test_mod, #{}),
    Socket2 = arizona_socket:put_stateful_state(MockState, Socket),

    %% Test found
    ?assertEqual({ok, MockState}, arizona_socket:find_stateful_state(~"test", Socket2)),

    %% Test not found
    ?assertEqual(error, arizona_socket:find_stateful_state(~"missing", Socket2)).

test_get_current_stateful_state(Config) when is_list(Config) ->
    Socket = arizona_socket:new(#{current_stateful_id => ~"current"}),
    MockState = create_mock_stateful_state(~"current", test_mod, #{key => value}),
    Socket2 = arizona_socket:put_stateful_state(MockState, Socket),

    CurrentState = arizona_socket:get_current_stateful_state(Socket2),
    ?assertEqual(MockState, CurrentState).

%% --------------------------------------------------------------------
%% Binding Tests
%% --------------------------------------------------------------------

test_temp_bindings(Config) when is_list(Config) ->
    Socket = arizona_socket:new(#{}),

    %% Test with_temp_bindings
    Bindings = #{name => ~"John", age => 30},
    Socket2 = arizona_socket:with_temp_bindings(Bindings, Socket),

    %% Temp bindings should be accessible
    ?assertEqual(~"John", arizona_socket:get_temp_binding(name, Socket2)),
    ?assertEqual(30, arizona_socket:get_temp_binding(age, Socket2)).

test_with_temp_bindings(Config) when is_list(Config) ->
    Socket = arizona_socket:new(#{}),
    Bindings = #{key1 => value1, key2 => value2},

    Socket2 = arizona_socket:with_temp_bindings(Bindings, Socket),

    %% Should be able to retrieve temp bindings
    ?assertEqual(value1, arizona_socket:get_temp_binding(key1, Socket2)),
    ?assertEqual(value2, arizona_socket:get_temp_binding(key2, Socket2)).

test_get_temp_binding(Config) when is_list(Config) ->
    Socket = arizona_socket:new(#{}),
    Bindings = #{test_key => test_value},
    Socket2 = arizona_socket:with_temp_bindings(Bindings, Socket),

    ?assertEqual(test_value, arizona_socket:get_temp_binding(test_key, Socket2)).

test_get_temp_binding_not_found(Config) when is_list(Config) ->
    Socket = arizona_socket:new(#{}),

    %% Should throw when binding not found
    ?assertThrow(
        {binding_not_found, missing_key},
        arizona_socket:get_temp_binding(missing_key, Socket)
    ).

test_get_binding_temp_priority(Config) when is_list(Config) ->
    %% Test that temp bindings have priority over stateful bindings
    Socket = arizona_socket:new(#{current_stateful_id => root}),

    %% Create stateful state with binding
    StatefulState = create_mock_stateful_state_with_binding(root, test_mod, #{
        name => ~"Stateful"
    }),
    Socket2 = arizona_socket:put_stateful_state(StatefulState, Socket),

    %% Add temp binding that overrides
    TempBindings = #{name => ~"Temp"},
    Socket3 = arizona_socket:with_temp_bindings(TempBindings, Socket2),

    %% Should get temp binding (priority)
    ?assertEqual(~"Temp", arizona_socket:get_binding(name, Socket3)).

test_get_binding_stateful_fallback(Config) when is_list(Config) ->
    %% Test fallback to stateful bindings when no temp binding
    Socket = arizona_socket:new(#{current_stateful_id => root}),

    %% Create stateful state with binding
    StatefulState = create_mock_stateful_state_with_binding(root, test_mod, #{
        name => ~"Stateful"
    }),
    Socket2 = arizona_socket:put_stateful_state(StatefulState, Socket),

    %% No temp bindings, should get stateful binding
    ?assertEqual(~"Stateful", arizona_socket:get_binding(name, Socket2)).

test_get_binding_not_found(Config) when is_list(Config) ->
    Socket = arizona_socket:new(#{current_stateful_id => root}),

    %% Create stateful state without the binding we're looking for
    StatefulState = create_mock_stateful_state_with_binding(
        root, test_mod, #{other => ~"value"}
    ),
    Socket2 = arizona_socket:put_stateful_state(StatefulState, Socket),

    %% Should throw when binding not found anywhere
    ?assertThrow({binding_not_found, missing}, arizona_socket:get_binding(missing, Socket2)).

test_is_socket_validation(Config) when is_list(Config) ->
    %% Create a valid socket
    Socket = arizona_socket:new(#{}),

    %% Test positive case (should be true for valid socket)
    ?assert(arizona_socket:is_socket(Socket)),

    %% Test negative cases (should be false for non-sockets)
    ?assertNot(arizona_socket:is_socket(undefined)),
    ?assertNot(arizona_socket:is_socket(~"not_a_socket")).

%% --------------------------------------------------------------------
%% Helper Functions
%% --------------------------------------------------------------------

%% Helper to create real stateful state
create_mock_stateful_state(Id, Module, Bindings) ->
    arizona_stateful:new(Id, Module, Bindings).

%% Helper to create real stateful state that supports get_binding
create_mock_stateful_state_with_binding(Id, Module, Bindings) ->
    arizona_stateful:new(Id, Module, Bindings).

%% --------------------------------------------------------------------
%% Changes Accumulator Tests
%% --------------------------------------------------------------------

test_append_changes(Config) when is_list(Config) ->
    Socket = arizona_socket:new(#{mode => diff}),

    % Test basic append
    Changes1 = [{~"comp1", [{0, ~"new_value"}]}],
    Socket2 = arizona_socket:append_changes(Changes1, Socket),
    ?assertEqual(Changes1, arizona_socket:get_changes(Socket2)),

    % Test appending to existing changes
    Changes2 = [{~"comp2", [{1, ~"another_value"}]}],
    Socket3 = arizona_socket:append_changes(Changes2, Socket2),
    Expected = Changes1 ++ Changes2,
    ?assertEqual(Expected, arizona_socket:get_changes(Socket3)).

test_get_changes(Config) when is_list(Config) ->
    Socket = arizona_socket:new(#{mode => diff}),

    % Test empty changes
    ?assertEqual([], arizona_socket:get_changes(Socket)),

    % Test with changes
    Changes = [{~"comp1", [{0, ~"value"}]}],
    Socket2 = arizona_socket:append_changes(Changes, Socket),
    ?assertEqual(Changes, arizona_socket:get_changes(Socket2)).

test_clear_changes(Config) when is_list(Config) ->
    Socket = arizona_socket:new(#{mode => diff}),
    Changes = [{~"comp1", [{0, ~"value"}]}],
    Socket2 = arizona_socket:append_changes(Changes, Socket),

    % Verify changes exist
    ?assertEqual(Changes, arizona_socket:get_changes(Socket2)),

    % Clear and verify empty
    Socket3 = arizona_socket:clear_changes(Socket2),
    ?assertEqual([], arizona_socket:get_changes(Socket3)).

test_merge_changes_same_component(Config) when is_list(Config) ->
    Socket = arizona_socket:new(#{mode => diff}),

    % Add changes to the same component at different elements
    Changes1 = [{~"comp1", [{0, ~"value1"}]}],
    Socket2 = arizona_socket:append_changes(Changes1, Socket),

    Changes2 = [{~"comp1", [{1, ~"value2"}]}],
    Socket3 = arizona_socket:append_changes(Changes2, Socket2),

    % Changes should be merged under the same component
    AllChanges = arizona_socket:get_changes(Socket3),
    ?assertMatch([{~"comp1", _ElementChanges}], AllChanges),
    [{~"comp1", ElementChanges}] = AllChanges,

    % Should contain both element changes
    ?assertEqual(2, length(ElementChanges)),
    ?assert(lists:member({0, ~"value1"}, ElementChanges)),
    ?assert(lists:member({1, ~"value2"}, ElementChanges)).

test_merge_changes_different_components(Config) when is_list(Config) ->
    Socket = arizona_socket:new(#{mode => diff}),

    % Add changes to different components
    Changes1 = [{~"comp1", [{0, ~"value1"}]}],
    Socket2 = arizona_socket:append_changes(Changes1, Socket),

    Changes2 = [{~"comp2", [{0, ~"value2"}]}],
    Socket3 = arizona_socket:append_changes(Changes2, Socket2),

    % Should have separate component entries
    AllChanges = arizona_socket:get_changes(Socket3),
    ?assertEqual(2, length(AllChanges)),
    ?assert(lists:member({~"comp1", [{0, ~"value1"}]}, AllChanges)),
    ?assert(lists:member({~"comp2", [{0, ~"value2"}]}, AllChanges)).

test_merge_nested_changes(Config) when is_list(Config) ->
    Socket = arizona_socket:new(#{mode => diff}),

    % Test nested component changes
    NestedChanges1 = [{~"comp1", [{0, [{~"nested", [{0, ~"old"}]}]}]}],
    Socket2 = arizona_socket:append_changes(NestedChanges1, Socket),

    NestedChanges2 = [{~"comp1", [{0, [{~"nested", [{1, ~"new"}]}]}]}],
    Socket3 = arizona_socket:append_changes(NestedChanges2, Socket2),

    % Should merge nested structures properly
    AllChanges = arizona_socket:get_changes(Socket3),
    ?assertMatch([{~"comp1", [{0, _}]}], AllChanges),
    [{~"comp1", [{0, NestedContent}]}] = AllChanges,
    ?assertMatch([{~"nested", _}], NestedContent),
    [{~"nested", NestedElements}] = NestedContent,
    ?assertEqual(2, length(NestedElements)).

test_merge_changes_edge_cases(Config) when is_list(Config) ->
    Socket = arizona_socket:new(#{mode => diff}),

    % Test with empty changes list
    Socket2 = arizona_socket:append_changes([], Socket),
    ?assertEqual([], arizona_socket:get_changes(Socket2)),

    % Test multiple changes to same element (last wins)
    Changes1 = [{~"comp1", [{0, ~"first"}]}],
    Changes2 = [{~"comp1", [{0, ~"second"}]}],
    Socket3 = arizona_socket:append_changes(Changes1, Socket),
    Socket4 = arizona_socket:append_changes(Changes2, Socket3),

    AllChanges = arizona_socket:get_changes(Socket4),
    ?assertMatch([{~"comp1", [{0, ~"second"}]}], AllChanges).

test_merge_changes_html_data_vs_component_changes(Config) when is_list(Config) ->
    Socket = arizona_socket:new(#{mode => diff}),

    % Test the bug fix: nested HTML data structures should not be treated as component changes
    % This simulates the todo app scenario where render_list produces complex nested HTML

    % Create complex nested HTML structure like what todo list rendering produces
    ComplexHtmlData = [
        [
            [
                [],
                [
                    ~"<div class=\"todo-item \">",
                    ~"",
                    ~"\" data-testid=\"todo-\">",
                    ~"1",
                    ~"\">",
                    ~"Learn Erlang",
                    ~"</div>"
                ]
            ],
            [
                ~"<div class=\"todo-item \">",
                ~"completed",
                ~"\" data-testid=\"todo-\">",
                ~"2",
                ~"\">",
                ~"Build web app",
                ~"</div>"
            ]
        ]
    ],

    % This should not crash when merging changes with complex HTML data
    Changes1 = [{root, [{3, ComplexHtmlData}]}],
    Socket2 = arizona_socket:append_changes(Changes1, Socket),

    % Add another change to the same element with different HTML data
    NewHtmlData = [
        [
            ~"<div class=\"todo-item\">", ~"Updated todo", ~"</div>"
        ]
    ],
    Changes2 = [{root, [{3, NewHtmlData}]}],
    Socket3 = arizona_socket:append_changes(Changes2, Socket2),

    % Should succeed without function_clause error
    AllChanges = arizona_socket:get_changes(Socket3),
    ?assertEqual(1, length(AllChanges)),

    % Verify the last change overwrote the previous one (new change precedence)
    [{root, ElementChanges}] = AllChanges,
    ?assertEqual(1, length(ElementChanges)),
    [{3, FinalHtmlData}] = ElementChanges,
    ?assertEqual(NewHtmlData, FinalHtmlData),

    % Test with mix of component changes and HTML data
    ComponentChanges = [{~"nested_comp", [{0, ~"simple value"}]}],
    MixedChanges = [{~"different_root", [{1, ~"simple binary"}, {2, ComponentChanges}]}],
    Socket4 = arizona_socket:append_changes(MixedChanges, Socket3),

    % Should handle mixed data types without crashing
    FinalChanges = arizona_socket:get_changes(Socket4),
    % root and different_root components
    ?assertEqual(2, length(FinalChanges)),

    % Verify the structure is correct
    ?assertMatch([{root, _}, {~"different_root", _}], FinalChanges).

%% --------------------------------------------------------------------
%% Binding Management Tests
%% --------------------------------------------------------------------

test_put_binding(Config) when is_list(Config) ->
    Socket = arizona_socket:new(#{current_stateful_id => root}),

    % Create initial state
    InitialState = create_mock_stateful_state(root, test_mod, #{}),
    Socket2 = arizona_socket:put_stateful_state(InitialState, Socket),

    % Put binding
    Socket3 = arizona_socket:put_binding(test_key, test_value, Socket2),

    % Verify binding was set
    ?assertEqual(test_value, arizona_socket:get_binding(test_key, Socket3)).

test_put_bindings(Config) when is_list(Config) ->
    Socket = arizona_socket:new(#{current_stateful_id => root}),

    % Create initial state
    InitialState = create_mock_stateful_state(root, test_mod, #{}),
    Socket2 = arizona_socket:put_stateful_state(InitialState, Socket),

    % Put multiple bindings
    Bindings = #{key1 => value1, key2 => value2},
    Socket3 = arizona_socket:put_bindings(Bindings, Socket2),

    % Verify bindings were set
    ?assertEqual(value1, arizona_socket:get_binding(key1, Socket3)),
    ?assertEqual(value2, arizona_socket:get_binding(key2, Socket3)).

%% --------------------------------------------------------------------
%% Error Handling Tests
%% --------------------------------------------------------------------

test_get_stateful_state_not_found(Config) when is_list(Config) ->
    Socket = arizona_socket:new(#{}),

    % Should throw when state not found
    ?assertError({badkey, ~"missing_id"}, arizona_socket:get_stateful_state(~"missing_id", Socket)).

test_get_current_stateful_state_not_found(Config) when is_list(Config) ->
    Socket = arizona_socket:new(#{current_stateful_id => ~"missing_id"}),

    % Should throw when current state not found
    ?assertError({badkey, ~"missing_id"}, arizona_socket:get_current_stateful_state(Socket)).
