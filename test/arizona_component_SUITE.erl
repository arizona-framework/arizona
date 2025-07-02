-module(arizona_component_SUITE).
-behaviour(ct_suite).
-include_lib("stdlib/include/assert.hrl").
-compile([export_all, nowarn_export_all]).

%% --------------------------------------------------------------------
%% Behaviour (ct_suite) callbacks
%% --------------------------------------------------------------------

all() ->
    [
        {group, call_stateful_tests},
        {group, call_stateless_tests}
    ].

groups() ->
    [
        {call_stateful_tests, [parallel], [
            test_call_stateful_new_component,
            test_call_stateful_existing_no_remount,
            test_call_stateful_existing_with_remount,
            test_call_stateful_component_not_found
        ]},
        {call_stateless_tests, [parallel], [
            test_call_stateless_basic,
            test_call_stateless_with_bindings,
            test_call_stateless_render_function
        ]}
    ].

%% --------------------------------------------------------------------
%% call_stateful tests
%% --------------------------------------------------------------------

test_call_stateful_new_component(_Config) ->
    %% Test creating a new stateful component when none exists
    Socket = create_mock_socket(),
    Bindings = #{initial_value => ~"test"},
    Module = test_stateful_module_with_mount,

    UpdatedSocket = arizona_component:call_stateful(Module, Bindings, Socket),

    %% Should be a valid socket
    ?assert(arizona_socket:is_socket(UpdatedSocket)),

    %% Should have created and stored stateful state
    Id = arizona_socket:get_current_stateful_id(UpdatedSocket),
    ?assertMatch({ok, _State}, arizona_socket:find_stateful_state(Id, UpdatedSocket)).

test_call_stateful_existing_no_remount(_Config) ->
    %% Test existing component that doesn't need remounting
    Socket = create_mock_socket(),
    Module = test_stateful_module_with_mount,
    Id = arizona_socket:get_current_stateful_id(Socket),

    %% Create existing state with same bindings that we'll pass
    %% This should NOT trigger remount since bindings are the same
    ExistingBindings = #{status => ~"mounted"},
    State = arizona_stateful:new(Id, Module, ExistingBindings),
    Socket1 = arizona_socket:put_stateful_state(Id, State, Socket),

    %% Pass the same bindings - should not remount, use differ instead
    Bindings = #{status => ~"mounted"},

    UpdatedSocket = arizona_component:call_stateful(Module, Bindings, Socket1),

    %% Should be a valid socket
    ?assert(arizona_socket:is_socket(UpdatedSocket)),

    %% Should still have the stateful state
    ?assertMatch({ok, _State}, arizona_socket:find_stateful_state(Id, UpdatedSocket)).

test_call_stateful_existing_with_remount(_Config) ->
    %% Test existing component that needs remounting
    Socket = create_mock_socket(),
    Module = test_stateful_module_with_unmount,
    Id = arizona_socket:get_current_stateful_id(Socket),

    %% Create existing state with initial bindings first
    InitialBindings = #{mounted => true, unmounted => false},
    State = arizona_stateful:new(Id, Module, InitialBindings),
    Socket1 = arizona_socket:put_stateful_state(Id, State, Socket),

    %% Pre-mount the component to set required bindings
    MountedSocket = arizona_stateful:call_mount_callback(Module, Socket1),

    Bindings = #{remount_value => ~"remounted"},

    UpdatedSocket = arizona_component:call_stateful(Module, Bindings, MountedSocket),

    %% Should be a valid socket
    ?assert(arizona_socket:is_socket(UpdatedSocket)),

    %% Verify the HTML contains expected content
    Html = arizona_socket:get_html(UpdatedSocket),
    HtmlBinary = iolist_to_binary(Html),
    ?assert(binary:match(HtmlBinary, ~"Component with Unmount") =/= nomatch).

test_call_stateful_component_not_found(_Config) ->
    %% Test when no existing component state is found (error case)
    Socket = create_mock_socket_without_id(),
    Bindings = #{value => ~"test"},
    Module = test_stateful_module_with_mount,

    %% arizona_component will handle the mount lifecycle automatically
    UpdatedSocket = arizona_component:call_stateful(Module, Bindings, Socket),

    %% Should still be a valid socket
    ?assert(arizona_socket:is_socket(UpdatedSocket)).

%% --------------------------------------------------------------------
%% call_stateless tests
%% --------------------------------------------------------------------

test_call_stateless_basic(_Config) ->
    %% Test basic stateless component call
    Socket = create_mock_socket(),
    Module = test_stateless_module,
    Fun = basic_render,
    Bindings = #{},

    UpdatedSocket = arizona_component:call_stateless(Module, Fun, Bindings, Socket),

    %% Should be a valid socket
    ?assert(arizona_socket:is_socket(UpdatedSocket)),

    %% Verify the HTML contains expected content
    Html = arizona_socket:get_html(UpdatedSocket),
    HtmlBinary = iolist_to_binary(Html),
    ?assert(binary:match(HtmlBinary, ~"Basic Stateless") =/= nomatch).

test_call_stateless_with_bindings(_Config) ->
    %% Test stateless component with bindings
    Socket = create_mock_socket(),
    Module = test_stateless_module,
    Fun = render_with_bindings,
    Bindings = #{title => ~"Test Title", content => ~"Test Content"},

    UpdatedSocket = arizona_component:call_stateless(Module, Fun, Bindings, Socket),

    %% Should be a valid socket
    ?assert(arizona_socket:is_socket(UpdatedSocket)),

    %% Verify the HTML contains the bound values
    Html = arizona_socket:get_html(UpdatedSocket),
    HtmlBinary = iolist_to_binary(Html),
    ?assert(binary:match(HtmlBinary, ~"Test Title") =/= nomatch),
    ?assert(binary:match(HtmlBinary, ~"Test Content") =/= nomatch).

test_call_stateless_render_function(_Config) ->
    %% Test that stateless component properly calls the render function
    Socket = create_mock_socket(),
    Module = test_stateless_module,
    Fun = render_simple,
    Bindings = #{message => ~"Hello World"},

    UpdatedSocket = arizona_component:call_stateless(Module, Fun, Bindings, Socket),

    %% Should be a valid socket
    ?assert(arizona_socket:is_socket(UpdatedSocket)),

    %% Verify the HTML contains expected content
    Html = arizona_socket:get_html(UpdatedSocket),
    HtmlBinary = iolist_to_binary(Html),
    ?assert(binary:match(HtmlBinary, ~"Hello World") =/= nomatch).

%% --------------------------------------------------------------------
%% Helper functions
%% --------------------------------------------------------------------

create_mock_socket() ->
    Id = ~"test_component_id",
    Opts = #{
        current_stateful_id => Id
    },
    arizona_socket:new(Opts).

create_mock_socket_without_id() ->
    Opts = #{},
    arizona_socket:new(Opts).
