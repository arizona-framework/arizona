-module(arizona_resolver_SUITE).
-behaviour(ct_suite).
-include_lib("stdlib/include/assert.hrl").
-compile([export_all, nowarn_export_all]).

%% --------------------------------------------------------------------
%% Behaviour (ct_suite) callbacks
%% --------------------------------------------------------------------

all() ->
    [
        {group, basic_resolution},
        {group, template_types},
        {group, edge_cases}
    ].

groups() ->
    [
        {basic_resolution, [parallel], [
            resolve_simple_template,
            resolve_multiple_bindings,
            resolve_mixed_content
        ]},
        {template_types, [parallel], [
            resolve_static_only,
            resolve_dynamic_only,
            resolve_empty_template
        ]},
        {edge_cases, [parallel], [
            resolve_with_binary_values,
            resolve_with_integer_values,
            resolve_with_atom_values
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

%% --------------------------------------------------------------------
%% Basic resolution tests
%% --------------------------------------------------------------------

resolve_simple_template(Config) when is_list(Config) ->
    Template = arizona_template:from_string(
        ~"<p>Hello {arizona_template:get_binding(name, Bindings)}!</p>",
        #{name => ~"World"}
    ),

    Socket = create_test_socket(),
    {Html, UpdatedSocket} = arizona_resolver:resolve_template(Template, Socket),

    assert_html_equals(Html, ~"<p>Hello World!</p>"),
    ?assert(arizona_socket:is_socket(UpdatedSocket)).

resolve_multiple_bindings(Config) when is_list(Config) ->
    Template = arizona_template:from_string(
        ~"""
        <div>
            <h1>{arizona_template:get_binding(title, Bindings)}</h1>
            <p>Count: {arizona_template:get_binding(count, Bindings)}</p>
        </div>
        """,
        #{
            title => ~"Dashboard",
            count => 42
        }
    ),

    Socket = create_test_socket(),
    {Html, _UpdatedSocket} = arizona_resolver:resolve_template(Template, Socket),

    assert_html_contains(Html, ~"Dashboard"),
    assert_html_contains(Html, ~"42").

resolve_mixed_content(Config) when is_list(Config) ->
    Template = arizona_template:from_string(
        ~"<span>Before {arizona_template:get_binding(content, Bindings)} After</span>",
        #{content => ~"MIDDLE"}
    ),

    Socket = create_test_socket(),
    {Html, _UpdatedSocket} = arizona_resolver:resolve_template(Template, Socket),

    assert_html_equals(Html, ~"<span>Before MIDDLE After</span>").

%% --------------------------------------------------------------------
%% Template types tests
%% --------------------------------------------------------------------

resolve_static_only(Config) when is_list(Config) ->
    Template = arizona_template:from_string(
        ~"<p>Static content only</p>",
        #{}
    ),

    Socket = create_test_socket(),
    {Html, _UpdatedSocket} = arizona_resolver:resolve_template(Template, Socket),

    assert_html_equals(Html, ~"<p>Static content only</p>").

resolve_dynamic_only(Config) when is_list(Config) ->
    Template = arizona_template:from_string(
        ~"{arizona_template:get_binding(content, Bindings)}",
        #{content => ~"Dynamic only"}
    ),

    Socket = create_test_socket(),
    {Html, _UpdatedSocket} = arizona_resolver:resolve_template(Template, Socket),

    assert_html_equals(Html, ~"Dynamic only").

resolve_empty_template(Config) when is_list(Config) ->
    Template = arizona_template:from_string(~"", #{}),

    Socket = create_test_socket(),
    {Html, _UpdatedSocket} = arizona_resolver:resolve_template(Template, Socket),

    ?assertEqual([], Html).

%% --------------------------------------------------------------------
%% Edge cases tests
%% --------------------------------------------------------------------

resolve_with_binary_values(Config) when is_list(Config) ->
    Template = arizona_template:from_string(
        ~"<p>{arizona_template:get_binding(value, Bindings)}</p>",
        #{value => ~"Binary value"}
    ),

    Socket = create_test_socket(),
    {Html, _UpdatedSocket} = arizona_resolver:resolve_template(Template, Socket),

    assert_html_contains(Html, ~"Binary value").

resolve_with_integer_values(Config) when is_list(Config) ->
    Template = arizona_template:from_string(
        ~"<p>Count: {arizona_template:get_binding(count, Bindings)}</p>",
        #{count => 123}
    ),

    Socket = create_test_socket(),
    {Html, _UpdatedSocket} = arizona_resolver:resolve_template(Template, Socket),

    assert_html_contains(Html, ~"123").

resolve_with_atom_values(Config) when is_list(Config) ->
    Template = arizona_template:from_string(
        ~"<p>Status: {arizona_template:get_binding(status, Bindings)}</p>",
        #{status => active}
    ),

    Socket = create_test_socket(),
    {Html, _UpdatedSocket} = arizona_resolver:resolve_template(Template, Socket),

    assert_html_contains(Html, ~"active").
