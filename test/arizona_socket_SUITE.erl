-module(arizona_socket_SUITE).
-moduledoc ~"""
Test suite for Arizona Socket module.

This suite tests the core socket data structure for Arizona templates,
including state management, HTML accumulation, and binding handling.
""".

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%% --------------------------------------------------------------------
%% Test suite exports
%% --------------------------------------------------------------------

-export([all/0, groups/0]).

%% Test cases
-export([
    test_socket_creation/1,
    test_socket_creation_with_options/1,
    test_socket_creation_defaults/1,
    test_get_mode/1,
    test_get_current_stateful_id/1,
    test_set_current_stateful_id/1,
    test_set_current_stateful_id_validation/1,
    test_html_accumulation/1,
    test_get_html_empty/1,
    test_stateful_state_management/1,
    test_get_stateful_states/1,
    test_find_stateful_state/1,
    test_get_current_stateful_state/1,
    test_temp_bindings/1,
    test_with_temp_bindings/1,
    test_get_temp_binding/1,
    test_get_temp_binding_not_found/1,
    test_get_binding_temp_priority/1,
    test_get_binding_stateful_fallback/1,
    test_get_binding_not_found/1,
    test_is_socket_validation/1
]).

%% --------------------------------------------------------------------
%% Test suite configuration
%% --------------------------------------------------------------------

all() ->
    [
        {group, socket_creation_tests},
        {group, socket_property_tests},
        {group, html_management_tests},
        {group, stateful_state_tests},
        {group, binding_tests}
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
        ]}
    ].

%% --------------------------------------------------------------------
%% Socket Creation Tests
%% --------------------------------------------------------------------

test_socket_creation(_Config) ->
    %% Test basic socket creation
    Opts = #{mode => render, current_stateful_id => root},
    Socket = arizona_socket:new(Opts),

    ?assert(is_tuple(Socket)),
    ?assertEqual(render, arizona_socket:get_mode(Socket)),
    ?assertEqual(root, arizona_socket:get_current_stateful_id(Socket)).

test_socket_creation_with_options(_Config) ->
    %% Test socket creation with all options
    Opts = #{
        mode => diff,
        current_stateful_parent_id => <<"parent_1">>,
        current_stateful_id => <<"child_1">>
    },
    Socket = arizona_socket:new(Opts),

    ?assertEqual(diff, arizona_socket:get_mode(Socket)),
    ?assertEqual(<<"child_1">>, arizona_socket:get_current_stateful_id(Socket)).

test_socket_creation_defaults(_Config) ->
    %% Test socket creation with minimal options (defaults)
    Opts = #{},
    Socket = arizona_socket:new(Opts),

    ?assertEqual(render, arizona_socket:get_mode(Socket)),
    ?assertEqual(root, arizona_socket:get_current_stateful_id(Socket)).

%% --------------------------------------------------------------------
%% Socket Property Tests
%% --------------------------------------------------------------------

test_get_mode(_Config) ->
    RenderSocket = arizona_socket:new(#{mode => render}),
    DiffSocket = arizona_socket:new(#{mode => diff}),

    ?assertEqual(render, arizona_socket:get_mode(RenderSocket)),
    ?assertEqual(diff, arizona_socket:get_mode(DiffSocket)).

test_get_current_stateful_id(_Config) ->
    Socket1 = arizona_socket:new(#{current_stateful_id => root}),
    Socket2 = arizona_socket:new(#{current_stateful_id => <<"test_id">>}),

    ?assertEqual(root, arizona_socket:get_current_stateful_id(Socket1)),
    ?assertEqual(<<"test_id">>, arizona_socket:get_current_stateful_id(Socket2)).

test_set_current_stateful_id(_Config) ->
    Socket = arizona_socket:new(#{current_stateful_id => root}),

    %% Test setting root
    Socket2 = arizona_socket:set_current_stateful_id(root, Socket),
    ?assertEqual(root, arizona_socket:get_current_stateful_id(Socket2)),

    %% Test setting binary id
    Socket3 = arizona_socket:set_current_stateful_id(<<"new_id">>, Socket),
    ?assertEqual(<<"new_id">>, arizona_socket:get_current_stateful_id(Socket3)).

test_set_current_stateful_id_validation(_Config) ->
    Socket = arizona_socket:new(#{}),

    %% Test invalid id types should fail
    ?assertError(function_clause, arizona_socket:set_current_stateful_id(invalid_atom, Socket)),
    ?assertError(function_clause, arizona_socket:set_current_stateful_id(123, Socket)).

%% --------------------------------------------------------------------
%% HTML Management Tests
%% --------------------------------------------------------------------

test_html_accumulation(_Config) ->
    Socket = arizona_socket:new(#{}),

    %% Test setting HTML
    Html1 = [<<"<div>">>, <<"content">>, <<"</div>">>],
    Socket2 = arizona_socket:set_html_acc(Html1, Socket),
    ?assertEqual(Html1, arizona_socket:get_html(Socket2)),

    %% Test accumulating more HTML
    Html2 = [<<"<span>">>, <<"more">>, <<"</span>">>],
    Socket3 = arizona_socket:set_html_acc(Html2, Socket2),
    ?assertEqual(Html2, arizona_socket:get_html(Socket3)).

test_get_html_empty(_Config) ->
    Socket = arizona_socket:new(#{}),

    %% New socket should have empty HTML
    Html = arizona_socket:get_html(Socket),
    ?assertEqual([], Html).

%% --------------------------------------------------------------------
%% Stateful State Tests
%% --------------------------------------------------------------------

test_stateful_state_management(_Config) ->
    Socket = arizona_socket:new(#{}),

    %% Create mock stateful state
    MockState = create_mock_stateful_state(<<"comp_1">>, test_module, #{name => <<"Test">>}),

    %% Put stateful state
    Socket2 = arizona_socket:put_stateful_state(<<"comp_1">>, MockState, Socket),

    %% Get stateful state
    RetrievedState = arizona_socket:get_stateful_state(<<"comp_1">>, Socket2),
    ?assertEqual(MockState, RetrievedState).

test_get_stateful_states(_Config) ->
    Socket = arizona_socket:new(#{}),

    %% Initially empty
    ?assertEqual(#{}, arizona_socket:get_stateful_states(Socket)),

    %% Add some states
    State1 = create_mock_stateful_state(root, mod1, #{a => 1}),
    State2 = create_mock_stateful_state(<<"comp_1">>, mod2, #{b => 2}),

    Socket2 = arizona_socket:put_stateful_state(root, State1, Socket),
    Socket3 = arizona_socket:put_stateful_state(<<"comp_1">>, State2, Socket2),

    States = arizona_socket:get_stateful_states(Socket3),
    ?assertEqual(#{root => State1, <<"comp_1">> => State2}, States).

test_find_stateful_state(_Config) ->
    Socket = arizona_socket:new(#{}),
    MockState = create_mock_stateful_state(<<"test">>, test_mod, #{}),
    Socket2 = arizona_socket:put_stateful_state(<<"test">>, MockState, Socket),

    %% Test found
    ?assertEqual({ok, MockState}, arizona_socket:find_stateful_state(<<"test">>, Socket2)),

    %% Test not found
    ?assertEqual(error, arizona_socket:find_stateful_state(<<"missing">>, Socket2)).

test_get_current_stateful_state(_Config) ->
    Socket = arizona_socket:new(#{current_stateful_id => <<"current">>}),
    MockState = create_mock_stateful_state(<<"current">>, test_mod, #{key => value}),
    Socket2 = arizona_socket:put_stateful_state(<<"current">>, MockState, Socket),

    CurrentState = arizona_socket:get_current_stateful_state(Socket2),
    ?assertEqual(MockState, CurrentState).

%% --------------------------------------------------------------------
%% Binding Tests
%% --------------------------------------------------------------------

test_temp_bindings(_Config) ->
    Socket = arizona_socket:new(#{}),

    %% Test with_temp_bindings
    Bindings = #{name => <<"John">>, age => 30},
    Socket2 = arizona_socket:with_temp_bindings(Bindings, Socket),

    %% Temp bindings should be accessible
    ?assertEqual(<<"John">>, arizona_socket:get_temp_binding(name, Socket2)),
    ?assertEqual(30, arizona_socket:get_temp_binding(age, Socket2)).

test_with_temp_bindings(_Config) ->
    Socket = arizona_socket:new(#{}),
    Bindings = #{key1 => value1, key2 => value2},

    Socket2 = arizona_socket:with_temp_bindings(Bindings, Socket),

    %% Should be able to retrieve temp bindings
    ?assertEqual(value1, arizona_socket:get_temp_binding(key1, Socket2)),
    ?assertEqual(value2, arizona_socket:get_temp_binding(key2, Socket2)).

test_get_temp_binding(_Config) ->
    Socket = arizona_socket:new(#{}),
    Bindings = #{test_key => test_value},
    Socket2 = arizona_socket:with_temp_bindings(Bindings, Socket),

    ?assertEqual(test_value, arizona_socket:get_temp_binding(test_key, Socket2)).

test_get_temp_binding_not_found(_Config) ->
    Socket = arizona_socket:new(#{}),

    %% Should throw when binding not found
    ?assertThrow(
        {binding_not_found, missing_key},
        arizona_socket:get_temp_binding(missing_key, Socket)
    ).

test_get_binding_temp_priority(_Config) ->
    %% Test that temp bindings have priority over stateful bindings
    Socket = arizona_socket:new(#{current_stateful_id => root}),

    %% Create stateful state with binding
    StatefulState = create_mock_stateful_state_with_binding(root, test_mod, #{
        name => <<"Stateful">>
    }),
    Socket2 = arizona_socket:put_stateful_state(root, StatefulState, Socket),

    %% Add temp binding that overrides
    TempBindings = #{name => <<"Temp">>},
    Socket3 = arizona_socket:with_temp_bindings(TempBindings, Socket2),

    %% Should get temp binding (priority)
    ?assertEqual(<<"Temp">>, arizona_socket:get_binding(name, Socket3)).

test_get_binding_stateful_fallback(_Config) ->
    %% Test fallback to stateful bindings when no temp binding
    Socket = arizona_socket:new(#{current_stateful_id => root}),

    %% Create stateful state with binding
    StatefulState = create_mock_stateful_state_with_binding(root, test_mod, #{
        name => <<"Stateful">>
    }),
    Socket2 = arizona_socket:put_stateful_state(root, StatefulState, Socket),

    %% No temp bindings, should get stateful binding
    ?assertEqual(<<"Stateful">>, arizona_socket:get_binding(name, Socket2)).

test_get_binding_not_found(_Config) ->
    Socket = arizona_socket:new(#{current_stateful_id => root}),

    %% Create stateful state without the binding we're looking for
    StatefulState = create_mock_stateful_state_with_binding(
        root, test_mod, #{other => <<"value">>}
    ),
    Socket2 = arizona_socket:put_stateful_state(root, StatefulState, Socket),

    %% Should throw when binding not found anywhere
    ?assertThrow({binding_not_found, missing}, arizona_socket:get_binding(missing, Socket2)).

test_is_socket_validation(_Config) ->
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
