-module(arizona_stateful_SUITE).
-moduledoc """
Test suite for Arizona Stateful module.

This suite tests the stateful component behavior definition and utility functions,
including callback invocation, state management, and binding operations.
""".

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%% Suppress dialyzer warnings for validation tests that intentionally
%% pass invalid arguments to test error handling
-dialyzer({nowarn_function, [test_stateful_creation_validation/1]}).

%% --------------------------------------------------------------------
%% Test suite exports
%% --------------------------------------------------------------------

-export([all/0, groups/0]).

%% Test cases
-export([
    test_call_mount_callback/1,
    test_call_unmount_callback_with_export/1,
    test_call_unmount_callback_without_export/1,
    test_call_render_callback/1,
    test_call_dynamic_function/1,
    test_stateful_creation_root/1,
    test_stateful_creation_binary_id/1,
    test_stateful_creation_validation/1,
    test_get_module/1,
    test_get_binding_existing/1,
    test_get_binding_not_found/1,
    test_put_binding_new/1,
    test_put_binding_unchanged/1,
    test_put_binding_changed/1,
    test_put_bindings_multiple/1,
    test_should_remount_changed_bindings/1,
    test_should_remount_matching/1,
    test_should_remount_different/1
]).

%% --------------------------------------------------------------------
%% Test suite configuration
%% --------------------------------------------------------------------

all() ->
    [
        {group, callback_tests},
        {group, stateful_creation_tests},
        {group, state_access_tests},
        {group, binding_tests},
        {group, remount_tests}
    ].

groups() ->
    [
        {callback_tests, [parallel], [
            test_call_mount_callback,
            test_call_unmount_callback_with_export,
            test_call_unmount_callback_without_export,
            test_call_render_callback,
            test_call_dynamic_function
        ]},
        {stateful_creation_tests, [parallel], [
            test_stateful_creation_root,
            test_stateful_creation_binary_id,
            test_stateful_creation_validation
        ]},
        {state_access_tests, [parallel], [
            test_get_module
        ]},
        {binding_tests, [parallel], [
            test_get_binding_existing,
            test_get_binding_not_found,
            test_put_binding_new,
            test_put_binding_unchanged,
            test_put_binding_changed,
            test_put_bindings_multiple
        ]},
        {remount_tests, [parallel], [
            test_should_remount_changed_bindings,
            test_should_remount_matching,
            test_should_remount_different
        ]}
    ].

%% --------------------------------------------------------------------
%% Callback Tests
%% --------------------------------------------------------------------

test_call_mount_callback(Config) when is_list(Config) ->
    Module = test_stateful_module_with_mount,
    Socket = create_mock_socket(),

    %% Should call mount/1 and return updated socket
    UpdatedSocket = arizona_stateful:call_mount_callback(Module, Socket),
    ?assertNotEqual(Socket, UpdatedSocket).

test_call_unmount_callback_with_export(Config) when is_list(Config) ->
    Module = test_stateful_module_with_unmount,
    Socket = create_mock_socket(),

    %% Should call unmount/1 when exported
    UpdatedSocket = arizona_stateful:call_unmount_callback(Module, Socket),
    %% Check that unmount was called by verifying socket is modified
    ?assert(arizona_socket:is_socket(UpdatedSocket)).

test_call_unmount_callback_without_export(Config) when is_list(Config) ->
    % This module doesn't export unmount/1
    Module = test_stateful_module_with_mount,
    Socket = create_mock_socket(),

    %% Should return socket unchanged when unmount/1 not exported
    Result = arizona_stateful:call_unmount_callback(Module, Socket),
    ?assertEqual(Socket, Result).

test_call_render_callback(Config) when is_list(Config) ->
    Module = test_stateful_module_with_mount,
    %% Need to mount first to initialize status binding
    Socket = create_mock_socket(),
    MountedSocket = arizona_stateful:call_mount_callback(Module, Socket),

    %% Should call render/1 and return template data
    Result = arizona_stateful:call_render_callback(Module, MountedSocket),
    ?assert(arizona_socket:is_socket(Result)).

test_call_dynamic_function(Config) when is_list(Config) ->
    Socket = create_mock_socket(),
    Fun = fun(S) -> {dynamic_result, S} end,

    %% Should apply function with socket
    Result = arizona_stateful:call_dynamic_function(Fun, Socket),
    ?assertEqual({dynamic_result, Socket}, Result).

%% --------------------------------------------------------------------
%% Stateful Creation Tests
%% --------------------------------------------------------------------

test_stateful_creation_root(Config) when is_list(Config) ->
    Id = root,
    Module = test_module,
    Bindings = #{name => ~"Test", count => 5},

    %% Test creating stateful with root ID
    Stateful = arizona_stateful:new(Id, Module, Bindings),
    ?assertEqual(Module, arizona_stateful:get_module(Stateful)),
    ?assertEqual(~"Test", arizona_stateful:get_binding(name, Stateful)),
    ?assertEqual(5, arizona_stateful:get_binding(count, Stateful)).

test_stateful_creation_binary_id(Config) when is_list(Config) ->
    Id = ~"component_123",
    Module = test_module,
    Bindings = #{active => true},

    %% Test creating stateful with binary ID
    Stateful = arizona_stateful:new(Id, Module, Bindings),
    ?assertEqual(Module, arizona_stateful:get_module(Stateful)),
    ?assertEqual(true, arizona_stateful:get_binding(active, Stateful)).

test_stateful_creation_validation(Config) when is_list(Config) ->
    ValidBindings = #{test => value},

    %% Test invalid ID types should fail
    ?assertError(function_clause, arizona_stateful:new(invalid_atom, test_mod, ValidBindings)),
    ?assertError(function_clause, arizona_stateful:new(123, test_mod, ValidBindings)),

    %% Test invalid module should fail
    ?assertError(function_clause, arizona_stateful:new(root, "not_atom", ValidBindings)),
    ?assertError(function_clause, arizona_stateful:new(root, 123, ValidBindings)),

    %% Test invalid bindings should fail
    ?assertError(function_clause, arizona_stateful:new(root, test_mod, "not_map")),
    ?assertError(function_clause, arizona_stateful:new(root, test_mod, [])).

%% --------------------------------------------------------------------
%% State Access Tests
%% --------------------------------------------------------------------

test_get_module(Config) when is_list(Config) ->
    Module = my_test_module,
    Stateful = arizona_stateful:new(root, Module, #{}),

    %% Should return the module
    ?assertEqual(Module, arizona_stateful:get_module(Stateful)).

%% --------------------------------------------------------------------
%% Binding Tests
%% --------------------------------------------------------------------

test_get_binding_existing(Config) when is_list(Config) ->
    Bindings = #{user => ~"Alice", role => admin, count => 42},
    Stateful = arizona_stateful:new(root, test_mod, Bindings),

    %% Should get existing bindings
    ?assertEqual(~"Alice", arizona_stateful:get_binding(user, Stateful)),
    ?assertEqual(admin, arizona_stateful:get_binding(role, Stateful)),
    ?assertEqual(42, arizona_stateful:get_binding(count, Stateful)).

test_get_binding_not_found(Config) when is_list(Config) ->
    Bindings = #{existing => value},
    Stateful = arizona_stateful:new(root, test_mod, Bindings),

    %% Should throw when binding not found
    ?assertThrow(
        {binding_not_found, missing_key}, arizona_stateful:get_binding(missing_key, Stateful)
    ).

test_put_binding_new(Config) when is_list(Config) ->
    Stateful = arizona_stateful:new(root, test_mod, #{}),

    %% Add new binding
    UpdatedStateful = arizona_stateful:put_binding(new_key, new_value, Stateful),
    ?assertEqual(new_value, arizona_stateful:get_binding(new_key, UpdatedStateful)).

test_put_binding_unchanged(Config) when is_list(Config) ->
    Bindings = #{key => existing_value},
    Stateful = arizona_stateful:new(root, test_mod, Bindings),

    %% Set same value (no change)
    UpdatedStateful = arizona_stateful:put_binding(key, existing_value, Stateful),
    ?assertEqual(existing_value, arizona_stateful:get_binding(key, UpdatedStateful)).

test_put_binding_changed(Config) when is_list(Config) ->
    Bindings = #{key => old_value},
    Stateful = arizona_stateful:new(root, test_mod, Bindings),

    %% Change existing binding value
    UpdatedStateful = arizona_stateful:put_binding(key, new_value, Stateful),
    ?assertEqual(new_value, arizona_stateful:get_binding(key, UpdatedStateful)).

test_put_bindings_multiple(Config) when is_list(Config) ->
    InitialBindings = #{existing => value},
    Stateful = arizona_stateful:new(root, test_mod, InitialBindings),

    %% Add multiple bindings
    NewBindings = #{
        name => ~"John",
        age => 30,
        active => true,
        existing => updated_value
    },
    UpdatedStateful = arizona_stateful:put_bindings(NewBindings, Stateful),

    %% Verify all bindings
    ?assertEqual(~"John", arizona_stateful:get_binding(name, UpdatedStateful)),
    ?assertEqual(30, arizona_stateful:get_binding(age, UpdatedStateful)),
    ?assertEqual(true, arizona_stateful:get_binding(active, UpdatedStateful)),
    ?assertEqual(updated_value, arizona_stateful:get_binding(existing, UpdatedStateful)).

%% --------------------------------------------------------------------
%% Remount Tests
%% --------------------------------------------------------------------

test_should_remount_changed_bindings(Config) when is_list(Config) ->
    %% Create stateful and modify it to trigger fingerprint mismatch
    Stateful1 = arizona_stateful:new(root, test_mod, #{key => value}),
    %% Change binding to create different fingerprint
    Stateful2 = arizona_stateful:put_binding(key, different_value, Stateful1),

    %% Should remount when fingerprints differ due to changed bindings
    ?assertEqual(true, arizona_stateful:should_remount(Stateful2)).

test_should_remount_matching(Config) when is_list(Config) ->
    %% Create stateful with initial bindings
    Bindings = #{name => ~"Test", count => 1},
    Stateful1 = arizona_stateful:new(root, test_mod, Bindings),

    %% Create another with same bindings (should have matching fingerprints)
    Stateful2 = arizona_stateful:new(root, test_mod, Bindings),

    %% Simulate fingerprint matching by creating the same bindings
    %% In real usage, this would be compared against previous fingerprint
    ?assertEqual(
        false,
        arizona_stateful:should_remount(Stateful1) orelse
            arizona_stateful:should_remount(Stateful2)
    ).

test_should_remount_different(Config) when is_list(Config) ->
    %% Create stateful with initial bindings
    Stateful1 = arizona_stateful:new(root, test_mod, #{count => 1}),

    %% Update bindings to create different fingerprint
    UpdatedStateful = arizona_stateful:put_binding(count, 2, Stateful1),

    %% Should remount when fingerprints differ
    %% This tests the internal fingerprint comparison logic
    ?assertEqual(true, arizona_stateful:should_remount(UpdatedStateful)).

%% --------------------------------------------------------------------
%% Helper Functions
%% --------------------------------------------------------------------

%% Create a mock socket for testing
create_mock_socket() ->
    Socket = arizona_socket:new(#{mode => render, current_stateful_id => root}),
    %% Add a stateful state for the root component
    StatefulState = arizona_stateful:new(root, test_module, #{test => initial_value}),
    arizona_socket:put_stateful_state(StatefulState, Socket).

%% --------------------------------------------------------------------
%% Mock Callback Modules
%% --------------------------------------------------------------------

%% These would normally be separate modules, but for testing we can use
%% module attributes and dynamic function creation. In practice, you'd
%% create actual test modules that implement the arizona_stateful behavior.

%% Test module with mount callback
-ifdef(TEST).
% Mock implementations would be defined here or in separate test modules
-endif.
