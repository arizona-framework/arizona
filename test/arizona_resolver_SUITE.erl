-module(arizona_resolver_SUITE).
-behaviour(ct_suite).
-include_lib("stdlib/include/assert.hrl").
-compile([export_all, nowarn_export_all]).

%% --------------------------------------------------------------------
%% Behaviour (ct_suite) callbacks
%% --------------------------------------------------------------------

all() ->
    [
        {group, callback_handling}
    ].

groups() ->
    [
        {callback_handling, [parallel], [
            resolve_with_stateful_component,
            resolve_with_nested_stateful_calls
        ]}
    ].

%% --------------------------------------------------------------------
%% Helper functions
%% --------------------------------------------------------------------

create_test_socket() ->
    arizona_socket:new(#{mode => render}).

assert_html_contains(Html, Expected) ->
    HtmlBinary = iolist_to_binary(Html),
    ?assertMatch({_, _}, binary:match(HtmlBinary, Expected)).

assert_html_equals(Html, Expected) ->
    HtmlBinary = iolist_to_binary(Html),
    ?assertEqual(Expected, HtmlBinary).

create_user_component() ->
    Code = merl:quote(~""""
    -module(user_component).
    -behaviour(arizona_stateful).
    -export([mount/1, render/1]).

    mount(Socket) ->
        Socket.

    render(Bindings) ->
        arizona_template:from_string(user_component, 1, ~"""
        <div class="user">
            <h2>{arizona_template:get_binding(name, Bindings)}</h2>
            <p>Role: {arizona_template:get_binding(role, Bindings)}</p>
            {arizona_template:render_stateful(profile_component, #{
                id => arizona_template:get_binding(profile_id, Bindings),
                user_id => arizona_template:get_binding(id, Bindings)
            })}
        </div>
        """, Bindings).
    """"),
    merl:compile_and_load(Code).

create_profile_component() ->
    Code = merl:quote(~""""
    -module(profile_component).
    -behaviour(arizona_stateful).
    -export([mount/1, render/1]).

    mount(Socket) ->
        Socket.

    render(Bindings) ->
        arizona_template:from_string(profile_component, 1, ~"""
        <section class="profile">
            <p>Profile for user: {arizona_template:get_binding(user_id, Bindings)}</p>
        </section>
        """, Bindings).
    """"),
    merl:compile_and_load(Code).

create_button_component() ->
    Code = merl:quote(~""""
    -module(button_component).
    -export([render/1]).

    render(Bindings) ->
        arizona_template:from_string(button_component, 1, ~"""
        <button class="{arizona_template:get_binding(class, Bindings)}">
            {arizona_template:get_binding(text, Bindings)}
        </button>
        """, Bindings).
    """"),
    merl:compile_and_load(Code).

%% --------------------------------------------------------------------
%% Callback handling tests
%% --------------------------------------------------------------------

resolve_with_stateful_component(Config) when is_list(Config) ->
    % Test stateful component resolution with lifecycle management
    create_user_component(),
    create_profile_component(),

    % Setup user component state
    UserState = arizona_stateful:new(root, user_component, #{
        id => ~"user123",
        name => ~"John Doe",
        role => ~"admin",
        profile_id => ~"profile456"
    }),
    ProfileState = arizona_stateful:new(~"profile456", profile_component, #{
        id => ~"profile456",
        user_id => ~"user123"
    }),

    Socket = arizona_socket:put_stateful_state(UserState, create_test_socket()),
    Socket1 = arizona_socket:put_stateful_state(ProfileState, Socket),

    {Html, _UpdatedSocket} = arizona_resolver:resolve_stateful(root, Socket1),
    assert_html_contains(Html, ~"John Doe"),
    assert_html_contains(Html, ~"admin"),
    assert_html_contains(Html, ~"Profile for user: user123").

resolve_with_nested_stateful_calls(Config) when is_list(Config) ->
    % Test deeply nested stateful calls to exercise call_stateful branches
    create_user_component(),
    create_profile_component(),

    UserState = arizona_stateful:new(root, user_component, #{
        id => ~"nested_user",
        name => ~"Nested User",
        role => ~"tester",
        profile_id => ~"nested_profile"
    }),
    ProfileState = arizona_stateful:new(~"nested_profile", profile_component, #{
        id => ~"nested_profile", user_id => ~"nested_user"
    }),

    Socket = arizona_socket:put_stateful_state(UserState, create_test_socket()),
    Socket1 = arizona_socket:put_stateful_state(ProfileState, Socket),

    {Html, _UpdatedSocket} = arizona_resolver:resolve_stateful(root, Socket1),
    assert_html_contains(Html, ~"Nested User"),
    assert_html_contains(Html, ~"tester"),
    assert_html_contains(Html, ~"Profile for user: nested_user").
