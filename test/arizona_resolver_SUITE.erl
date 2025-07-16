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
        {group, zip_static_dynamic}
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
            resolve_template_with_binaries,
            resolve_template_with_integers,
            resolve_template_with_atoms,
            resolve_template_with_floats,
            resolve_template_with_lists,
            resolve_template_with_socket_return,
            resolve_template_dynamic_sequence_coverage,
            resolve_template_with_stateful_callback,
            resolve_template_with_stateless_callback
        ]},
        {zip_static_dynamic, [parallel], [
            zip_empty_lists,
            zip_static_longer,
            zip_dynamic_longer,
            zip_equal_length,
            zip_single_elements
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

create_test_stateful_component() ->
    Code = merl:quote(~""""
    -module(test_stateful_component).
    -behaviour(arizona_stateful).
    -export([mount/1, render/1, render_nested/1]).

    mount(Socket) ->
        Socket.

    render(Bindings) ->
        arizona_template:from_string(test_stateful_component, 1, ~"""
        <div>
            <h1>Test Stateful Component</h1>
            <p>Value: {arizona_template:get_binding(value, Bindings)}</p>
            {arizona_template:render_stateful(test_stateful_component_1, #{
                id => arizona_template:get_binding(nested_id, Bindings),
                value => arizona_template:get_binding(value, Bindings)
            })}
            {arizona_template:render_stateful(test_stateful_component_1, #{
                id => arizona_template:get_binding(nested_id_1, Bindings),
                value => arizona_template:get_binding(value, Bindings)
            })}
            {arizona_template:render_stateless(test_stateful_component, render_nested, #{
                nested_value => arizona_template:get_binding(value, Bindings)
            })}
        </div>
        """, Bindings).

    render_nested(Bindings) ->
        arizona_template:from_string(test_stateful_component, 2, ~"""
        <span>Nested: {arizona_template:get_binding(nested_value, Bindings)}</span>
        """, Bindings).
    """"),
    merl:compile_and_load(Code).

create_test_stateful_component_1() ->
    Code = merl:quote(~""""
    -module(test_stateful_component_1).
    -behaviour(arizona_stateful).
    -export([mount/1, render/1, render_nested/1]).

    mount(Socket) ->
        Socket.

    render(Bindings) ->
        arizona_template:from_string(test_stateful_component_1, 1, ~"""
        <div>
            <h1>Test Stateful Component</h1>
            <p>Value: {arizona_template:get_binding(value, Bindings)}</p>
            {arizona_template:render_stateless(test_stateful_component_1, render_nested, #{
                nested_value => arizona_template:get_binding(value, Bindings)
            })}
        </div>
        """, Bindings).

    render_nested(Bindings) ->
        arizona_template:from_string(test_stateful_component_1, 2, ~"""
        <span>Nested: {arizona_template:get_binding(nested_value, Bindings)}</span>
        """, Bindings).
    """"),
    merl:compile_and_load(Code).

create_test_stateless_component() ->
    Code = merl:quote(~""""
    -module(test_stateless_component).
    -export([render/1]).

    render(Bindings) ->
        arizona_template:from_string(test_stateless_component_1, 1, ~"""
        <div>Stateless: {arizona_template:get_binding(content, Bindings)}</div>
        """, Bindings).
    """"),
    merl:compile_and_load(Code).

create_test_remount_stateful_component() ->
    Code = merl:quote(~""""
    -module(test_remount_stateful_component).
    -behaviour(arizona_stateful).
    -export([mount/1, render/1]).

    mount(Socket) ->
        Socket.

    render(Bindings) ->
        arizona_template:from_string(test_remount_stateful_component, 1, ~"""
        <div>
            <h1>Remount Component</h1>
            <p>Value: {arizona_template:get_binding(value, Bindings)}</p>
        </div>
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

resolve_template_with_binaries(Config) when is_list(Config) ->
    Template = arizona_template:from_string(
        ~"<p>{arizona_template:get_binding(value, Bindings)}</p>",
        #{value => ~"Binary value"}
    ),
    Socket = create_test_socket(),
    {Html, _UpdatedSocket} = arizona_resolver:resolve_template(Template, Socket),
    assert_html_contains(Html, ~"Binary value").

resolve_template_with_integers(Config) when is_list(Config) ->
    Template = arizona_template:from_string(
        ~"<p>{arizona_template:get_binding(count, Bindings)}</p>",
        #{count => 123}
    ),
    Socket = create_test_socket(),
    {Html, _UpdatedSocket} = arizona_resolver:resolve_template(Template, Socket),
    assert_html_contains(Html, ~"123").

resolve_template_with_atoms(Config) when is_list(Config) ->
    Template = arizona_template:from_string(
        ~"<p>{arizona_template:get_binding(status, Bindings)}</p>",
        #{status => active}
    ),
    Socket = create_test_socket(),
    {Html, _UpdatedSocket} = arizona_resolver:resolve_template(Template, Socket),
    assert_html_contains(Html, ~"active").

resolve_template_with_floats(Config) when is_list(Config) ->
    Template = arizona_template:from_string(
        ~"<p>{arizona_template:get_binding(price, Bindings)}</p>",
        #{price => 99.99}
    ),
    Socket = create_test_socket(),
    {Html, _UpdatedSocket} = arizona_resolver:resolve_template(Template, Socket),
    assert_html_contains(Html, ~"99.99").

resolve_template_with_lists(Config) when is_list(Config) ->
    Template = arizona_template:from_string(
        ~"<p>{arizona_template:get_binding(items, Bindings)}</p>",
        #{items => [~"a", ~"b", ~"c"]}
    ),
    Socket = create_test_socket(),
    {Html, _UpdatedSocket} = arizona_resolver:resolve_template(Template, Socket),
    assert_html_contains(Html, ~"abc").

resolve_template_with_socket_return(Config) when is_list(Config) ->
    % Create a socket with HTML content for testing socket return case
    SocketWithHtml = arizona_socket:set_html_acc([~"Socket HTML"], create_test_socket()),

    Template = arizona_template:from_string(
        ~"{arizona_template:get_binding(socket_result, Bindings)}",
        #{socket_result => SocketWithHtml}
    ),

    Socket = create_test_socket(),
    {Html, _UpdatedSocket} = arizona_resolver:resolve_template(Template, Socket),
    assert_html_contains(Html, ~"Socket HTML").

resolve_template_dynamic_sequence_coverage(Config) when is_list(Config) ->
    % Test to ensure resolve_template_dynamic and resolve_dynamic_callbacks coverage
    Template = arizona_template:from_string(
        erlang,
        1,
        ~"<div>{arizona_template:get_binding(first, Bindings)} and {arizona_template:get_binding(second, Bindings)}</div>",
        #{first => ~"ONE", second => ~"TWO"}
    ),
    Socket = create_test_socket(),
    {Html, _UpdatedSocket} = arizona_resolver:resolve_template(Template, Socket),
    assert_html_contains(Html, ~"ONE"),
    assert_html_contains(Html, ~"TWO").

resolve_template_with_stateful_callback(Config) when is_list(Config) ->
    % Create test stateful component
    create_test_stateful_component(),
    create_test_stateful_component_1(),

    % Create socket with stateful state
    StatefulState = arizona_stateful:new(root, test_stateful_component, #{
        nested_id => ~"nested", nested_id_1 => ~"nested_1", value => ~"TestValue"
    }),
    StatefulState1 = arizona_stateful:new(~"nested", test_stateful_component_1, #{}),
    Socket = arizona_socket:put_stateful_state(StatefulState, create_test_socket()),
    Socket1 = arizona_socket:put_stateful_state(StatefulState1, Socket),

    {Html, _UpdatedSocket} = arizona_resolver:resolve_stateful(root, Socket1),
    assert_html_contains(Html, ~"Test Stateful Component"),
    assert_html_contains(Html, ~"TestValue"),
    assert_html_contains(Html, ~"Nested: TestValue").

resolve_template_with_stateless_callback(Config) when is_list(Config) ->
    % Create test stateless component
    create_test_stateless_component(),

    % Create a template that calls a stateless component
    Template = arizona_template:from_string(
        ~"{arizona_template:render_stateless(test_stateless_component, render, #{content => ~\"StatelessContent\"})}",
        #{}
    ),

    Socket = create_test_socket(),
    {Html, _UpdatedSocket} = arizona_resolver:resolve_template(Template, Socket),
    assert_html_contains(Html, ~"Stateless: StatelessContent").

%% --------------------------------------------------------------------
%% zip_static_dynamic tests
%% --------------------------------------------------------------------

zip_empty_lists(Config) when is_list(Config) ->
    Result = arizona_resolver:zip_static_dynamic([], []),
    ?assertEqual([], Result).

zip_static_longer(Config) when is_list(Config) ->
    Static = [~"A", ~"B", ~"C"],
    Dynamic = [~"1", ~"2"],
    Result = arizona_resolver:zip_static_dynamic(Static, Dynamic),
    Expected = [~"A", ~"1", ~"B", ~"2", ~"C"],
    ?assertEqual(Expected, Result).

zip_dynamic_longer(Config) when is_list(Config) ->
    Static = [~"A", ~"B"],
    Dynamic = [~"1", ~"2", ~"3"],
    Result = arizona_resolver:zip_static_dynamic(Static, Dynamic),
    Expected = [~"A", ~"1", ~"B", ~"2", ~"3"],
    ?assertEqual(Expected, Result).

zip_equal_length(Config) when is_list(Config) ->
    Static = [~"A", ~"B", ~"C"],
    Dynamic = [~"1", ~"2", ~"3"],
    Result = arizona_resolver:zip_static_dynamic(Static, Dynamic),
    Expected = [~"A", ~"1", ~"B", ~"2", ~"C", ~"3"],
    ?assertEqual(Expected, Result).

zip_single_elements(Config) when is_list(Config) ->
    Static = [~"A"],
    Dynamic = [~"1"],
    Result = arizona_resolver:zip_static_dynamic(Static, Dynamic),
    Expected = [~"A", ~"1"],
    ?assertEqual(Expected, Result).
