-module(arizona_differ_SUITE).
-behaviour(ct_suite).
-include_lib("stdlib/include/assert.hrl").
-compile([export_all, nowarn_export_all]).

%% --------------------------------------------------------------------
%% Behaviour (ct_suite) callbacks
%% --------------------------------------------------------------------

all() ->
    [{group, socket_changes}, {group, diff_stateful}, {group, diff_optimization}].

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
        {diff_optimization, [parallel], [
            get_affected_elements_basic,
            get_affected_elements_multiple_vars,
            to_json_conversion
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
    ?assertMatch([{root, [_ | _]}], Changes),

    % Extract the component change
    [{ComponentId, ElementChanges}] = Changes,
    ?assertEqual(root, ComponentId),
    ?assert(length(ElementChanges) > 0).

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

to_json_conversion(Config) when is_list(Config) ->
    % Create sample diff changes
    DiffChanges = [{root, [{1, ~"value1"}, {2, [{~"counter", [{3, 42}]}]}]}],

    % Convert to JSON format
    JsonData = arizona_differ:to_json(DiffChanges),

    % Should be the same format (already JSON-compatible)
    ?assertEqual(DiffChanges, JsonData).
