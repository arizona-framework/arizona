-module(arizona_resolver_SUITE).
-behaviour(ct_suite).
-include_lib("stdlib/include/assert.hrl").
-compile([export_all, nowarn_export_all]).

%% --------------------------------------------------------------------
%% Behaviour (ct_suite) callbacks
%% --------------------------------------------------------------------

all() ->
    [
        {group, template_resolution},
        {group, callback_handling},
        {group, zip_utility}
    ].

groups() ->
    [
        {template_resolution, [parallel], [
            resolve_template_empty,
            resolve_template_static_only,
            resolve_template_dynamic_only,
            resolve_template_mixed_content,
            resolve_template_multiple_dynamics,
            resolve_template_complex_nesting
        ]},
        {callback_handling, [parallel], [
            resolve_with_different_data_types,
            resolve_with_socket_return,
            resolve_with_stateful_component,
            resolve_with_stateless_component,
            resolve_with_multiple_stateful_components,
            resolve_with_nested_stateful_calls
        ]},
        {zip_utility, [parallel], [
            zip_empty_lists,
            zip_static_longer_than_dynamic,
            zip_dynamic_longer_than_static,
            zip_equal_length_lists,
            zip_single_element_lists
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
%% Template resolution tests
%% --------------------------------------------------------------------

resolve_template_empty(Config) when is_list(Config) ->
    Template = arizona_template:from_string(~"", #{}),
    Socket = create_test_socket(),
    {Html, UpdatedSocket} = arizona_resolver:resolve_template(Template, Socket),
    ?assertEqual([], Html),
    ?assert(arizona_socket:is_socket(UpdatedSocket)).

resolve_template_static_only(Config) when is_list(Config) ->
    Template = arizona_template:from_string(~"<p>Static content</p>", #{}),
    Socket = create_test_socket(),
    {Html, _UpdatedSocket} = arizona_resolver:resolve_template(Template, Socket),
    assert_html_equals(Html, ~"<p>Static content</p>").

resolve_template_dynamic_only(Config) when is_list(Config) ->
    Template = arizona_template:from_string(
        ~"{arizona_template:get_binding(content, Bindings)}",
        #{content => ~"Dynamic only"}
    ),
    Socket = create_test_socket(),
    {Html, _UpdatedSocket} = arizona_resolver:resolve_template(Template, Socket),
    assert_html_equals(Html, ~"Dynamic only").

resolve_template_mixed_content(Config) when is_list(Config) ->
    Template = arizona_template:from_string(
        ~"<span>Before {arizona_template:get_binding(content, Bindings)} After</span>",
        #{content => ~"MIDDLE"}
    ),
    Socket = create_test_socket(),
    {Html, _UpdatedSocket} = arizona_resolver:resolve_template(Template, Socket),
    assert_html_equals(Html, ~"<span>Before MIDDLE After</span>").

resolve_template_multiple_dynamics(Config) when is_list(Config) ->
    Template = arizona_template:from_string(
        ~"{arizona_template:get_binding(a, Bindings)}{arizona_template:get_binding(b, Bindings)}{arizona_template:get_binding(c, Bindings)}",
        #{a => ~"A", b => ~"B", c => ~"C"}
    ),
    Socket = create_test_socket(),
    {Html, _UpdatedSocket} = arizona_resolver:resolve_template(Template, Socket),
    assert_html_equals(Html, ~"ABC").

resolve_template_complex_nesting(Config) when is_list(Config) ->
    Template = arizona_template:from_string(
        ~"<div>Start {arizona_template:get_binding(first, Bindings)} middle {arizona_template:get_binding(second, Bindings)} end</div>",
        #{first => ~"FIRST", second => ~"SECOND"}
    ),
    Socket = create_test_socket(),
    {Html, _UpdatedSocket} = arizona_resolver:resolve_template(Template, Socket),
    assert_html_equals(Html, ~"<div>Start FIRST middle SECOND end</div>").

%% --------------------------------------------------------------------
%% Callback handling tests
%% --------------------------------------------------------------------

resolve_with_different_data_types(Config) when is_list(Config) ->
    % Test resolver handles various Erlang data types correctly
    Template = arizona_template:from_string(
        ~"""
        <div>
            Binary: {arizona_template:get_binding(bin, Bindings)} |
            Integer: {arizona_template:get_binding(int, Bindings)} |
            Atom: {arizona_template:get_binding(atom, Bindings)} |
            Float: {arizona_template:get_binding(float, Bindings)} |
            List: {arizona_template:get_binding(list, Bindings)}
        </div>
        """,
        #{
            bin => ~"hello",
            int => 42,
            atom => success,
            float => 3.14,
            list => [~"x", ~"y"]
        }
    ),
    Socket = create_test_socket(),
    {Html, _UpdatedSocket} = arizona_resolver:resolve_template(Template, Socket),
    HtmlBin = iolist_to_binary(Html),
    ?assertMatch({_, _}, binary:match(HtmlBin, ~"hello")),
    ?assertMatch({_, _}, binary:match(HtmlBin, ~"42")),
    ?assertMatch({_, _}, binary:match(HtmlBin, ~"success")),
    ?assertMatch({_, _}, binary:match(HtmlBin, ~"3.14")),
    ?assertMatch({_, _}, binary:match(HtmlBin, ~"xy")).

resolve_with_socket_return(Config) when is_list(Config) ->
    % Test callback returning socket (for stateful operations)
    SocketWithHtml = arizona_socket:set_html_acc([~"Processed by socket"], create_test_socket()),
    Template = arizona_template:from_string(
        ~"<p>Result: {arizona_template:get_binding(socket_result, Bindings)}</p>",
        #{socket_result => SocketWithHtml}
    ),
    Socket = create_test_socket(),
    {Html, _UpdatedSocket} = arizona_resolver:resolve_template(Template, Socket),
    assert_html_contains(Html, ~"Processed by socket").

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

resolve_with_stateless_component(Config) when is_list(Config) ->
    % Test stateless component rendering
    create_button_component(),

    Template = arizona_template:from_string(
        ~"""
        <form>
            {arizona_template:render_stateless(button_component, render, 
                #{class => ~"btn-primary", text => ~"Submit"})} 
            {arizona_template:render_stateless(button_component, render, 
                #{class => ~"btn-secondary", text => ~"Cancel"})}
        </form>
        """,
        #{}
    ),

    Socket = create_test_socket(),
    {Html, _UpdatedSocket} = arizona_resolver:resolve_template(Template, Socket),
    assert_html_contains(Html, ~"btn-primary"),
    assert_html_contains(Html, ~"Submit"),
    assert_html_contains(Html, ~"btn-secondary"),
    assert_html_contains(Html, ~"Cancel").

resolve_with_multiple_stateful_components(Config) when is_list(Config) ->
    % Test multiple stateful components to hit more coverage paths
    create_user_component(),
    create_profile_component(),

    % Create template that calls stateful components directly (not through resolve_stateful)
    Template = arizona_template:from_string(
        ~"""
        <div class="users">
            {arizona_template:render_stateful(user_component, #{
                id => ~"user1", name => ~"Alice", role => ~"user", profile_id => ~"profile1"
            })}
            {arizona_template:render_stateful(user_component, #{
                id => ~"user2", name => ~"Bob", role => ~"admin", profile_id => ~"profile2"
            })}
        </div>
        """,
        #{}
    ),

    % Setup states for the components that will be called
    ProfileState1 = arizona_stateful:new(~"profile1", profile_component, #{
        id => ~"profile1", user_id => ~"user1"
    }),
    ProfileState2 = arizona_stateful:new(~"profile2", profile_component, #{
        id => ~"profile2", user_id => ~"user2"
    }),

    Socket = arizona_socket:put_stateful_state(ProfileState1, create_test_socket()),
    Socket1 = arizona_socket:put_stateful_state(ProfileState2, Socket),

    {Html, _UpdatedSocket} = arizona_resolver:resolve_template(Template, Socket1),
    assert_html_contains(Html, ~"Alice"),
    assert_html_contains(Html, ~"Bob"),
    assert_html_contains(Html, ~"Profile for user: user1"),
    assert_html_contains(Html, ~"Profile for user: user2").

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

%% --------------------------------------------------------------------
%% Utility function tests
%% --------------------------------------------------------------------

zip_empty_lists(Config) when is_list(Config) ->
    Result = arizona_resolver:zip_static_dynamic([], []),
    ?assertEqual([], Result).

zip_static_longer_than_dynamic(Config) when is_list(Config) ->
    % When static parts outnumber dynamic parts
    Static = [~"<p>", ~"</p><span>", ~"</span>"],
    Dynamic = [~"Hello", ~"World"],
    Result = arizona_resolver:zip_static_dynamic(Static, Dynamic),
    Expected = [~"<p>", ~"Hello", ~"</p><span>", ~"World", ~"</span>"],
    ?assertEqual(Expected, Result).

zip_dynamic_longer_than_static(Config) when is_list(Config) ->
    % When dynamic parts outnumber static parts
    Static = [~"Value: ", ~", Status: "],
    Dynamic = [~"42", ~"active", ~"ready"],
    Result = arizona_resolver:zip_static_dynamic(Static, Dynamic),
    Expected = [~"Value: ", ~"42", ~", Status: ", ~"active", ~"ready"],
    ?assertEqual(Expected, Result).

zip_equal_length_lists(Config) when is_list(Config) ->
    % When static and dynamic have equal length
    Static = [~"<li>", ~"</li><li>", ~"</li>"],
    Dynamic = [~"Item 1", ~"Item 2", ~"Item 3"],
    Result = arizona_resolver:zip_static_dynamic(Static, Dynamic),
    Expected = [~"<li>", ~"Item 1", ~"</li><li>", ~"Item 2", ~"</li>", ~"Item 3"],
    ?assertEqual(Expected, Result).

zip_single_element_lists(Config) when is_list(Config) ->
    % Simple case with one element each
    Static = [~"Hello "],
    Dynamic = [~"Arizona"],
    Result = arizona_resolver:zip_static_dynamic(Static, Dynamic),
    Expected = [~"Hello ", ~"Arizona"],
    ?assertEqual(Expected, Result).
