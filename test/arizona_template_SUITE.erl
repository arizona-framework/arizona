-module(arizona_template_SUITE).
-behaviour(ct_suite).
-include_lib("stdlib/include/assert.hrl").
-compile([export_all, nowarn_export_all]).

%% --------------------------------------------------------------------
%% Behaviour (ct_suite) callbacks
%% --------------------------------------------------------------------

all() ->
    [
        {group, function_callbacks},
        {group, diff_functionality},
        {group, nested_components}
    ].

groups() ->
    [
        {function_callbacks, [parallel], [
            render_stateful_mode_render,
            render_stateful_mode_hierarchical,
            render_stateless_mode_render,
            render_stateless_mode_hierarchical
        ]},
        {diff_functionality, [parallel], [
            diff_stateful_no_live_pid,
            diff_stateful_no_changes,
            diff_stateful_with_changes,
            diff_stateful_with_dependencies,
            diff_stateless_no_live_pid,
            diff_stateless_with_element_index,
            generate_element_diff_empty_set,
            generate_element_diff_with_elements,
            get_affected_elements_basic,
            get_affected_elements_multiple_vars,
            runtime_dependency_integration_test
        ]},
        {nested_components, [parallel], [
            nested_stateful_with_stateless_children,
            nested_stateless_with_stateful_children,
            deep_nesting_three_levels,
            nested_dependency_tracking_stateful_parent,
            nested_dependency_tracking_mixed_components,
            nested_diff_propagation_stateful_changes,
            nested_diff_propagation_child_changes,
            complex_nested_hierarchy_rendering
        ]}
    ].

%% --------------------------------------------------------------------
%% Helper functions
%% --------------------------------------------------------------------

create_test_socket(Mode) ->
    arizona_socket:new(#{mode => Mode}).

assert_html_contains(Html, Expected) ->
    HtmlBinary = iolist_to_binary(Html),
    ?assertMatch({_, _}, binary:match(HtmlBinary, Expected)).

create_test_component() ->
    Code = merl:quote(~""""
    -module(test_component).
    -behaviour(arizona_stateful).
    -export([mount/1, render/1]).

    mount(Socket) ->
        Socket.

    render(Bindings) ->
        arizona_template:from_string(test_component, 1, ~"""
        <div class="test">
            <h1>{arizona_template:get_binding(title, Bindings)}</h1>
        </div>
        """, Bindings).
    """"),
    merl:compile_and_load(Code).

create_simple_component() ->
    Code = merl:quote(~""""
    -module(simple_component).
    -export([render/1]).

    render(Bindings) ->
        arizona_template:from_string(simple_component, 1, ~"""
        <span>{arizona_template:get_binding(text, Bindings)}</span>
        """, Bindings).
    """"),
    merl:compile_and_load(Code).

%% --------------------------------------------------------------------
%% Function callback tests
%% --------------------------------------------------------------------

render_stateful_mode_render(Config) when is_list(Config) ->
    create_test_component(),

    Socket = create_test_socket(render),
    Callback = arizona_template:render_stateful(test_component, #{
        id => ~"test1",
        title => ~"Hello World"
    }),

    {Html, _UpdatedSocket} = Callback(Socket),
    assert_html_contains(Html, ~"Hello World"),
    assert_html_contains(Html, ~"<div class=\"test\">").

render_stateful_mode_hierarchical(Config) when is_list(Config) ->
    create_test_component(),

    Socket = create_test_socket(hierarchical),
    Callback = arizona_template:render_stateful(test_component, #{
        id => ~"test2",
        title => ~"Hierarchical Test"
    }),

    {Struct, UpdatedSocket} = Callback(Socket),
    ?assertEqual(stateful, maps:get(type, Struct)),
    ?assertEqual(~"test2", maps:get(id, Struct)),

    HierarchicalAcc = arizona_socket:get_hierarchical_acc(UpdatedSocket),
    ?assert(maps:is_key(~"test2", HierarchicalAcc)),
    ComponentData = maps:get(~"test2", HierarchicalAcc),
    ?assertEqual(stateful, maps:get(type, ComponentData)).

render_stateless_mode_render(Config) when is_list(Config) ->
    create_simple_component(),

    Socket = create_test_socket(render),
    Callback = arizona_template:render_stateless(simple_component, render, #{
        text => ~"Stateless Test"
    }),

    {Html, _UpdatedSocket} = Callback(Socket),
    assert_html_contains(Html, ~"Stateless Test"),
    assert_html_contains(Html, ~"<span>").

render_stateless_mode_hierarchical(Config) when is_list(Config) ->
    create_simple_component(),

    Socket = create_test_socket(hierarchical),
    Callback = arizona_template:render_stateless(simple_component, render, #{
        text => ~"Hierarchical Stateless"
    }),

    {Struct, _UpdatedSocket} = Callback(Socket),
    ?assertEqual(stateless, maps:get(type, Struct)),
    ?assert(maps:is_key(static, Struct)),
    ?assert(maps:is_key(dynamic, Struct)).

%% --------------------------------------------------------------------
%% Diff Functionality Tests
%% --------------------------------------------------------------------

diff_stateful_no_live_pid(Config) when is_list(Config) ->
    % Test diff_stateful when no live process is available
    create_test_component(),

    Socket = create_test_socket(diff),
    Bindings = #{title => ~"Test Title"},

    % Call diff_stateful without a live_pid in socket
    {Changes, UpdatedSocket} = arizona_template:diff_stateful(test_component, Bindings, Socket),

    % Should return empty changes when no live process
    ?assertEqual([], Changes),
    ?assertEqual(Socket, UpdatedSocket).

diff_stateful_no_changes(Config) when is_list(Config) ->
    % Test diff_stateful when there are no binding changes
    create_test_component(),

    {Module, Socket, _LivePid} = start_live_process(),

    % Prepare a stateful state with no changes
    StatefulState = arizona_stateful:new(~"test_component_1", Module, #{
        title => ~"Test Title"
    }),
    Socket1 = arizona_socket:put_stateful_state(StatefulState, Socket),

    % Call diff with bindings that include the component id
    Bindings = #{id => ~"test_component_1", title => ~"Test Title"},
    {Changes, _UpdatedSocket} = arizona_template:diff_stateful(test_component, Bindings, Socket1),

    % Should return empty changes when no binding changes
    ?assertEqual([], Changes).

diff_stateful_with_changes(Config) when is_list(Config) ->
    % Test diff_stateful when there are binding changes
    create_test_component(),

    {_Module, Socket, LivePid} = start_live_process(),

    % Set up runtime dependencies by simulating variable access
    % This simulates what happens during template rendering when get_binding is called
    arizona_live:set_current_stateful_id(LivePid, ~"test_component_1"),
    % Element 1 depends on title
    arizona_live:set_current_element_index(LivePid, 1),
    arizona_live:record_variable_dependency(LivePid, title),

    % Create stateful state with changes
    StatefulState0 = arizona_stateful:new(~"test_component_1", test_component, #{
        title => ~"Old Title"
    }),
    StatefulState = arizona_stateful:put_binding(title, ~"New Title", StatefulState0),
    Socket1 = arizona_socket:put_stateful_state(StatefulState, Socket),

    % Call diff with bindings that include the component id and new values
    Bindings = #{id => ~"test_component_1", title => ~"New Title"},
    {Changes, _UpdatedSocket} = arizona_template:diff_stateful(test_component, Bindings, Socket1),

    % Should return element changes for index 1 with the new title
    ExpectedChanges = [{1, ~"New Title"}],
    ?assertEqual(ExpectedChanges, Changes).

diff_stateful_with_dependencies(Config) when is_list(Config) ->
    % Test diff_stateful with runtime dependencies
    create_test_component(),

    {_Module, Socket, LivePid} = start_live_process(),

    % Set up runtime dependencies by simulating variable access
    arizona_live:set_current_stateful_id(LivePid, ~"test_component_1"),
    arizona_live:set_current_element_index(LivePid, 1),
    % Element 1 depends on title (the only dynamic element in test_component template)
    arizona_live:record_variable_dependency(LivePid, title),

    % Verify dependencies were recorded correctly
    Dependencies = arizona_live:get_component_dependencies(LivePid, ~"test_component_1"),
    ExpectedDeps = #{title => [1]},
    ?assertEqual(ExpectedDeps, Dependencies),

    % Create stateful state with changes
    StatefulState0 = arizona_stateful:new(~"test_component_1", test_component, #{
        title => ~"Old Title"
    }),
    StatefulState = arizona_stateful:put_binding(title, ~"New Title", StatefulState0),
    Socket1 = arizona_socket:put_stateful_state(StatefulState, Socket),

    % Call diff with bindings that include the component id and new values
    Bindings = #{id => ~"test_component_1", title => ~"New Title"},
    {Changes, _UpdatedSocket} = arizona_template:diff_stateful(test_component, Bindings, Socket1),

    % Should return element changes for index 1 with new value
    ExpectedChanges = [{1, ~"New Title"}],
    ?assertEqual(ExpectedChanges, Changes).

diff_stateless_no_live_pid(Config) when is_list(Config) ->
    % Test diff_stateless when no live process is available
    create_simple_component(),

    Socket = create_test_socket(diff),
    Bindings = #{text => ~"Test Text"},

    {Change, _UpdatedSocket} = arizona_template:diff_stateless(
        simple_component, render, Bindings, Socket
    ),

    % Should return rendered HTML regardless of live process status
    ExpectedHtml = [~"<span>", ~"Test Text", ~"</span>"],
    ?assertEqual(ExpectedHtml, Change).

diff_stateless_with_element_index(Config) when is_list(Config) ->
    % Test diff_stateless with element index from live process
    create_simple_component(),

    {_Module, Socket, LivePid} = start_live_process(),

    % Set up current element index in the live process
    arizona_live:set_current_element_index(LivePid, 2),

    Bindings = #{text => ~"Test Text"},
    {Change, _UpdatedSocket} = arizona_template:diff_stateless(
        simple_component, render, Bindings, Socket
    ),

    % Should return rendered HTML (element index is not relevant for stateless diff)
    ExpectedHtml = [~"<span>", ~"Test Text", ~"</span>"],
    ?assertEqual(ExpectedHtml, Change).

generate_element_diff_empty_set(Config) when is_list(Config) ->
    % Test generate_element_diff with empty affected elements set
    create_test_component(),

    Socket = create_test_socket(diff),
    Template = create_mock_template(),
    AffectedElements = sets:new(),

    {Changes, UpdatedSocket} = arizona_template:generate_element_diff(
        AffectedElements, Template, Socket
    ),

    ?assertEqual([], Changes),
    ?assertEqual(Socket, UpdatedSocket).

generate_element_diff_with_elements(Config) when is_list(Config) ->
    % Test generate_element_diff with affected elements
    create_test_component(),

    Socket = create_test_socket(diff),
    Template = create_mock_template_with_dynamics(),
    AffectedElements = sets:from_list([1, 2]),

    {Changes, _UpdatedSocket} = arizona_template:generate_element_diff(
        AffectedElements, Template, Socket
    ),

    % Should generate specific changes for elements 1 and 2 with expected HTML content
    % The mock template with dynamics should produce known HTML for elements 1 and 2
    ExpectedChanges = [
        {1, ~"Test"},
        {2, ~"Content"}
    ],
    ?assertEqual(ExpectedChanges, Changes).

get_affected_elements_basic(Config) when is_list(Config) ->
    % Test get_affected_elements with basic scenario
    ChangedBindings = #{name => ~"John"},
    Dependencies = #{name => [0, 2], age => [1]},

    AffectedElements = arizona_template:get_affected_elements(ChangedBindings, Dependencies),

    ExpectedSet = sets:from_list([0, 2]),
    ?assertEqual(ExpectedSet, AffectedElements).

get_affected_elements_multiple_vars(Config) when is_list(Config) ->
    % Test get_affected_elements with multiple changed variables
    ChangedBindings = #{name => ~"John", age => 25},
    Dependencies = #{name => [0, 2], age => [1, 3], email => [4]},

    AffectedElements = arizona_template:get_affected_elements(ChangedBindings, Dependencies),

    ExpectedSet = sets:from_list([0, 1, 2, 3]),
    ?assertEqual(ExpectedSet, AffectedElements).

runtime_dependency_integration_test(Config) when is_list(Config) ->
    % Integration test for runtime dependency tracking with diff
    create_test_component(),

    {Module, Socket, _LivePid} = start_live_process(),

    % Simulate the full flow: prepare stateful state -> change -> diff
    % 1. Create initial stateful state
    StatefulState0 = arizona_stateful:new(~"test_component_1", test_component, #{
        title => ~"Initial Title"
    }),
    Socket1 = arizona_socket:put_stateful_state(StatefulState0, Socket),

    % 2. Change bindings and diff
    StatefulState = arizona_stateful:put_binding(title, ~"Updated Title", StatefulState0),
    Socket2 = arizona_socket:put_stateful_state(StatefulState, Socket1),

    % Call diff with bindings that include the component id
    Bindings2 = #{id => ~"test_component_1", title => ~"Updated Title"},
    {Changes, _FinalSocket} = arizona_template:diff_stateful(Module, Bindings2, Socket2),

    % Should track dependencies and generate appropriate changes
    % Since no dependencies are tracked initially, should return empty list
    ?assertEqual([], Changes).

%% --------------------------------------------------------------------
%% Helper Functions for Diff Tests
%% --------------------------------------------------------------------

start_live_process() ->
    % Start a real arizona_live process for testing
    Module = test_component,
    Socket0 = create_test_socket(diff),
    {ok, LivePid} = arizona_live:start_link(Module, Socket0),
    Socket = arizona_socket:set_live_pid(LivePid, Socket0),
    {Module, Socket, LivePid}.

create_mock_template() ->
    % Create a minimal template for testing
    arizona_template:from_string(mock_template, 1, ~"<div>Static content</div>", #{}).

create_mock_template_with_dynamics() ->
    % Create a template with dynamic elements for testing
    arizona_template:from_string(mock_template, 1, ~"""
    <div>
        Static content
        {arizona_template:get_binding(title, Bindings)}
        {arizona_template:get_binding(content, Bindings)}
    </div>
    """, #{title => ~"Test", content => ~"Content"}).

%% --------------------------------------------------------------------
%% Complex Nested Component Helpers
%% --------------------------------------------------------------------

create_nested_stateful_parent() ->
    % Stateful parent component that renders stateless children
    Code = merl:quote(~""""
    -module(nested_stateful_parent).
    -behaviour(arizona_stateful).
    -export([mount/1, render/1]).

    mount(Socket) ->
        Socket.

    render(Bindings) ->
        arizona_template:from_string(nested_stateful_parent, 1, ~"""
        <div class="parent">
            <h1>{arizona_template:get_binding(parent_title, Bindings)}</h1>
            <div class="children">
                {arizona_template:render_stateless(nested_stateless_child, render, #{
                    child_id => ~"child1",
                    child_text => arizona_template:get_binding(child1_text, Bindings)
                })}
                {arizona_template:render_stateless(nested_stateless_child, render, #{
                    child_id => ~"child2", 
                    child_text => arizona_template:get_binding(child2_text, Bindings)
                })}
            </div>
        </div>
        """, Bindings).
    """"),
    merl:compile_and_load(Code).

create_nested_stateless_child() ->
    % Stateless child component
    Code = merl:quote(~""""
    -module(nested_stateless_child).
    -export([render/1]).

    render(Bindings) ->
        arizona_template:from_string(nested_stateless_child, 1, ~"""
        <span class="child" id="{arizona_template:get_binding(child_id, Bindings)}">
            {arizona_template:get_binding(child_text, Bindings)}
        </span>
        """, Bindings).
    """"),
    merl:compile_and_load(Code).

create_nested_stateless_parent() ->
    % Stateless parent component that renders stateful children
    Code = merl:quote(~""""
    -module(nested_stateless_parent).
    -export([render/1]).

    render(Bindings) ->
        arizona_template:from_string(nested_stateless_parent, 1, ~"""
        <section class="wrapper">
            <h2>{arizona_template:get_binding(wrapper_title, Bindings)}</h2>
            <div class="stateful-children">
                {arizona_template:render_stateful(nested_stateful_child, #{
                    id => ~"stateful_child_1",
                    counter => arizona_template:get_binding(child1_counter, Bindings),
                    label => ~"First Child"
                })}
                {arizona_template:render_stateful(nested_stateful_child, #{
                    id => ~"stateful_child_2", 
                    counter => arizona_template:get_binding(child2_counter, Bindings),
                    label => ~"Second Child"
                })}
            </div>
        </section>
        """, Bindings).
    """"),
    merl:compile_and_load(Code).

create_nested_stateful_child() ->
    % Stateful child component
    Code = merl:quote(~""""
    -module(nested_stateful_child).
    -behaviour(arizona_stateful).
    -export([mount/1, render/1]).

    mount(Socket) ->
        Socket.

    render(Bindings) ->
        arizona_template:from_string(nested_stateful_child, 1, ~"""
        <div class="stateful-child" data-id="{arizona_template:get_binding(id, Bindings)}">
            <h3>{arizona_template:get_binding(label, Bindings)}</h3>
            <p>Counter: {arizona_template:get_binding(counter, Bindings)}</p>
        </div>
        """, Bindings).
    """"),
    merl:compile_and_load(Code).

create_deep_nesting_components() ->
    % Level 1: Stateful root
    RootCode = merl:quote(~""""
    -module(deep_root_component).
    -behaviour(arizona_stateful).
    -export([mount/1, render/1]).

    mount(Socket) ->
        Socket.

    render(Bindings) ->
        arizona_template:from_string(deep_root_component, 1, ~"""
        <main class="deep-root">
            <h1>{arizona_template:get_binding(root_title, Bindings)}</h1>
            <nav class="navigation">
                {arizona_template:render_stateless(deep_nav_component, render, #{
                    nav_items => arizona_template:get_binding(nav_items, Bindings)
                })}
            </nav>
            <section class="content">
                {arizona_template:render_stateful(deep_content_component, #{
                    id => ~"main_content",
                    content_data => arizona_template:get_binding(content_data, Bindings),
                    widget1_value => arizona_template:get_binding(widget1_value, Bindings),
                    info_text => arizona_template:get_binding(info_text, Bindings)
                })}
            </section>
        </main>
        """, Bindings).
    """"),

    % Level 2: Stateless nav
    NavCode = merl:quote(~""""
    -module(deep_nav_component).
    -export([render/1]).

    render(Bindings) ->
        arizona_template:from_string(deep_nav_component, 1, ~"""
        <ul class="nav-list">
            {arizona_template:get_binding(nav_items, Bindings)}
        </ul>
        """, Bindings).
    """"),

    % Level 2: Stateful content
    ContentCode = merl:quote(~""""
    -module(deep_content_component).
    -behaviour(arizona_stateful).
    -export([mount/1, render/1]).

    mount(Socket) ->
        Socket.

    render(Bindings) ->
        arizona_template:from_string(deep_content_component, 1, ~"""
        <article class="main-content">
            <p>{arizona_template:get_binding(content_data, Bindings)}</p>
            <div class="widgets">
                {arizona_template:render_stateful(deep_widget_component, #{
                    id => ~"widget1",
                    widget_type => ~"counter",
                    widget_value => arizona_template:get_binding(widget1_value, Bindings)
                })}
                {arizona_template:render_stateless(deep_info_component, render, #{
                    info_text => arizona_template:get_binding(info_text, Bindings)
                })}
            </div>
        </article>
        """, Bindings).
    """"),

    % Level 3: Stateful widget
    WidgetCode = merl:quote(~""""
    -module(deep_widget_component).
    -behaviour(arizona_stateful).
    -export([mount/1, render/1]).

    mount(Socket) ->
        Socket.

    render(Bindings) ->
        arizona_template:from_string(deep_widget_component, 1, ~"""
        <div class="widget" data-type="{arizona_template:get_binding(widget_type, Bindings)}">
            <span class="widget-value">{arizona_template:get_binding(widget_value, Bindings)}</span>
        </div>
        """, Bindings).
    """"),

    % Level 3: Stateless info
    InfoCode = merl:quote(~""""
    -module(deep_info_component).
    -export([render/1]).

    render(Bindings) ->
        arizona_template:from_string(deep_info_component, 1, ~"""
        <aside class="info-panel">
            <p class="info-text">{arizona_template:get_binding(info_text, Bindings)}</p>
        </aside>
        """, Bindings).
    """"),

    merl:compile_and_load(RootCode),
    merl:compile_and_load(NavCode),
    merl:compile_and_load(ContentCode),
    merl:compile_and_load(WidgetCode),
    merl:compile_and_load(InfoCode).

%% --------------------------------------------------------------------
%% Nested Component Tests
%% --------------------------------------------------------------------

nested_stateful_with_stateless_children(Config) when is_list(Config) ->
    % Test stateful parent component with stateless children in render mode
    create_nested_stateful_parent(),
    create_nested_stateless_child(),

    Socket = create_test_socket(render),
    Callback = arizona_template:render_stateful(nested_stateful_parent, #{
        id => ~"parent1",
        parent_title => ~"Parent Component",
        child1_text => ~"First Child Text",
        child2_text => ~"Second Child Text"
    }),

    {Html, _UpdatedSocket} = Callback(Socket),
    HtmlBinary = iolist_to_binary(Html),

    % Verify parent structure
    assert_html_contains(Html, ~"Parent Component"),
    assert_html_contains(Html, ~"<div class=\"parent\">"),

    % Verify both children are rendered
    assert_html_contains(Html, ~"First Child Text"),
    assert_html_contains(Html, ~"Second Child Text"),
    assert_html_contains(Html, ~"id=\"child1\""),
    assert_html_contains(Html, ~"id=\"child2\""),

    % Verify complete nested structure is present
    ?assertMatch({_, _}, binary:match(HtmlBinary, ~"<div class=\"children\">")),
    ?assertMatch({_, _}, binary:match(HtmlBinary, ~"<span class=\"child\"")).

nested_stateless_with_stateful_children(Config) when is_list(Config) ->
    % Test stateless parent component with stateful children in render mode
    create_nested_stateless_parent(),
    create_nested_stateful_child(),

    Socket = create_test_socket(render),
    Callback = arizona_template:render_stateless(nested_stateless_parent, render, #{
        wrapper_title => ~"Wrapper Component",
        child1_counter => 42,
        child2_counter => 99
    }),

    {Html, _UpdatedSocket} = Callback(Socket),
    HtmlBinary = iolist_to_binary(Html),

    % Verify wrapper structure
    assert_html_contains(Html, ~"Wrapper Component"),
    assert_html_contains(Html, ~"<section class=\"wrapper\">"),

    % Verify both stateful children are rendered
    assert_html_contains(Html, ~"First Child"),
    assert_html_contains(Html, ~"Second Child"),
    assert_html_contains(Html, ~"Counter: 42"),
    assert_html_contains(Html, ~"Counter: 99"),
    assert_html_contains(Html, ~"data-id=\"stateful_child_1\""),
    assert_html_contains(Html, ~"data-id=\"stateful_child_2\""),

    % Verify complete nested structure is present
    ?assertMatch({_, _}, binary:match(HtmlBinary, ~"<div class=\"stateful-children\">")),
    ?assertMatch({_, _}, binary:match(HtmlBinary, ~"<div class=\"stateful-child\"")).

deep_nesting_three_levels(Config) when is_list(Config) ->
    % Test deep nesting with 3 levels of components
    create_deep_nesting_components(),

    Socket = create_test_socket(render),
    Callback = arizona_template:render_stateful(deep_root_component, #{
        id => ~"root1",
        root_title => ~"Deep Root Title",
        nav_items => ~"<li>Home</li><li>About</li>",
        content_data => ~"Main content paragraph",
        widget1_value => ~"100",
        info_text => ~"Information panel text"
    }),

    {Html, _UpdatedSocket} = Callback(Socket),
    HtmlBinary = iolist_to_binary(Html),

    % Verify level 1 (root)
    assert_html_contains(Html, ~"Deep Root Title"),
    assert_html_contains(Html, ~"<main class=\"deep-root\">"),

    % Verify level 2 (nav and content)
    assert_html_contains(Html, ~"<ul class=\"nav-list\">"),
    assert_html_contains(Html, ~"<li>Home</li><li>About</li>"),
    assert_html_contains(Html, ~"<article class=\"main-content\">"),
    assert_html_contains(Html, ~"Main content paragraph"),

    % Verify level 3 (widget and info)
    assert_html_contains(Html, ~"<div class=\"widget\""),
    assert_html_contains(Html, ~"data-type=\"counter\""),
    assert_html_contains(Html, ~"<span class=\"widget-value\">100</span>"),
    assert_html_contains(Html, ~"<aside class=\"info-panel\">"),
    assert_html_contains(Html, ~"Information panel text"),

    % Verify complete structure hierarchy
    ?assertMatch({_, _}, binary:match(HtmlBinary, ~"<nav class=\"navigation\">")),
    ?assertMatch({_, _}, binary:match(HtmlBinary, ~"<section class=\"content\">")),
    ?assertMatch({_, _}, binary:match(HtmlBinary, ~"<div class=\"widgets\">")).

nested_dependency_tracking_stateful_parent(Config) when is_list(Config) ->
    % Test dependency tracking in stateful parent with nested children
    create_nested_stateful_parent(),
    create_nested_stateless_child(),

    {_Module, Socket, LivePid} = start_live_process(),

    % Set up dependencies for parent component
    arizona_live:set_current_stateful_id(LivePid, ~"parent1"),
    arizona_live:set_current_element_index(LivePid, 1),
    arizona_live:record_variable_dependency(LivePid, parent_title),
    arizona_live:set_current_element_index(LivePid, 2),
    arizona_live:record_variable_dependency(LivePid, child1_text),
    arizona_live:set_current_element_index(LivePid, 3),
    arizona_live:record_variable_dependency(LivePid, child2_text),

    % Verify dependencies were recorded
    Dependencies = arizona_live:get_component_dependencies(LivePid, ~"parent1"),
    ExpectedDeps = #{
        parent_title => [1],
        child1_text => [2],
        child2_text => [3]
    },
    ?assertEqual(ExpectedDeps, Dependencies),

    % Create stateful state with changes
    StatefulState0 = arizona_stateful:new(~"parent1", nested_stateful_parent, #{
        parent_title => ~"Old Parent Title",
        child1_text => ~"Old Child 1",
        child2_text => ~"Old Child 2"
    }),
    StatefulState = arizona_stateful:put_binding(parent_title, ~"New Parent Title", StatefulState0),
    Socket1 = arizona_socket:put_stateful_state(StatefulState, Socket),

    % Test diff for parent title change
    Bindings = #{
        id => ~"parent1",
        parent_title => ~"New Parent Title",
        child1_text => ~"Old Child 1",
        child2_text => ~"Old Child 2"
    },
    {Changes, _UpdatedSocket} = arizona_template:diff_stateful(
        nested_stateful_parent, Bindings, Socket1
    ),

    % Should only track changes for parent_title (element 1)
    ExpectedChanges = [{1, ~"New Parent Title"}],
    ?assertEqual(ExpectedChanges, Changes).

nested_dependency_tracking_mixed_components(Config) when is_list(Config) ->
    % Test dependency tracking across mixed stateful and stateless components
    create_nested_stateless_parent(),
    create_nested_stateful_child(),

    Socket = create_test_socket(diff),

    % Test stateless parent with stateful children behavior
    Bindings = #{
        wrapper_title => ~"Test Wrapper",
        child1_counter => 10,
        child2_counter => 20
    },

    {Change, _UpdatedSocket} = arizona_template:diff_stateless(
        nested_stateless_parent, render, Bindings, Socket
    ),

    % Stateless component should return rendered HTML regardless of live process status
    % Since this is a stateless parent with stateful children, verify the HTML structure
    ExpectedHtml = [
        ~"<section class=\"wrapper\">\n    <h2>",
        ~"Test Wrapper",
        ~"</h2>\n    <div class=\"stateful-children\">\n        ",
        [],
        ~"\n        ",
        [],
        ~"\n    </div>\n</section>"
    ],
    ?assertEqual(ExpectedHtml, Change).

nested_diff_propagation_stateful_changes(Config) when is_list(Config) ->
    % Test diff propagation when stateful parent changes affect nested structure
    create_nested_stateful_parent(),
    create_nested_stateless_child(),

    {_Module, Socket, LivePid} = start_live_process(),

    % Set up dependencies for multiple elements
    arizona_live:set_current_stateful_id(LivePid, ~"parent1"),
    arizona_live:set_current_element_index(LivePid, 1),
    arizona_live:record_variable_dependency(LivePid, parent_title),
    arizona_live:set_current_element_index(LivePid, 2),
    arizona_live:record_variable_dependency(LivePid, child1_text),

    % Create initial state
    StatefulState0 = arizona_stateful:new(~"parent1", nested_stateful_parent, #{
        parent_title => ~"Original Title",
        child1_text => ~"Original Child 1",
        child2_text => ~"Original Child 2"
    }),

    % Change multiple bindings
    StatefulState1 = arizona_stateful:put_binding(parent_title, ~"Updated Title", StatefulState0),
    StatefulState = arizona_stateful:put_binding(child1_text, ~"Updated Child 1", StatefulState1),
    Socket1 = arizona_socket:put_stateful_state(StatefulState, Socket),

    % Test diff with multiple changes
    Bindings = #{
        id => ~"parent1",
        parent_title => ~"Updated Title",
        child1_text => ~"Updated Child 1",
        child2_text => ~"Original Child 2"
    },
    {Changes, _UpdatedSocket} = arizona_template:diff_stateful(
        nested_stateful_parent, Bindings, Socket1
    ),

    % Should track changes for both modified elements
    % Element 1 is just the title text, element 2 is the rendered stateless child HTML
    ExpectedChanges = [
        {1, ~"Updated Title"},
        {2, [
            ~"<span class=\"child\" id=\"",
            ~"child1",
            ~"\">\n    ",
            ~"Updated Child 1",
            ~"\n</span>"
        ]}
    ],
    ?assertEqual(ExpectedChanges, Changes).

nested_diff_propagation_child_changes(Config) when is_list(Config) ->
    % Test diff behavior when only child component data changes
    create_nested_stateful_parent(),
    create_nested_stateless_child(),

    {_Module, Socket, LivePid} = start_live_process(),

    % Set up dependency only for child text (child2_text is element 3)
    arizona_live:set_current_stateful_id(LivePid, ~"parent1"),
    arizona_live:set_current_element_index(LivePid, 3),
    arizona_live:record_variable_dependency(LivePid, child2_text),

    % Create initial state
    StatefulState0 = arizona_stateful:new(~"parent1", nested_stateful_parent, #{
        parent_title => ~"Static Title",
        child1_text => ~"Static Child 1",
        child2_text => ~"Original Child 2"
    }),

    % Change only child2_text
    StatefulState = arizona_stateful:put_binding(child2_text, ~"Modified Child 2", StatefulState0),
    Socket1 = arizona_socket:put_stateful_state(StatefulState, Socket),

    % Test diff with child-only change
    Bindings = #{
        id => ~"parent1",
        parent_title => ~"Static Title",
        child1_text => ~"Static Child 1",
        child2_text => ~"Modified Child 2"
    },
    {Changes, _UpdatedSocket} = arizona_template:diff_stateful(
        nested_stateful_parent, Bindings, Socket1
    ),

    % Should only track change for the specific child element (rendered stateless child HTML)
    ExpectedChanges = [
        {3, [
            ~"<span class=\"child\" id=\"",
            ~"child2",
            ~"\">\n    ",
            ~"Modified Child 2",
            ~"\n</span>"
        ]}
    ],
    ?assertEqual(ExpectedChanges, Changes).

complex_nested_hierarchy_rendering(Config) when is_list(Config) ->
    % Test complex hierarchy in hierarchical mode
    create_deep_nesting_components(),

    Socket = create_test_socket(hierarchical),
    Callback = arizona_template:render_stateful(deep_root_component, #{
        id => ~"complex_root",
        root_title => ~"Complex Hierarchy",
        nav_items => ~"<li>Nav Item</li>",
        content_data => ~"Complex content",
        widget1_value => ~"42",
        info_text => ~"Complex info"
    }),

    {Struct, UpdatedSocket} = Callback(Socket),

    % Verify the structure has the expected properties
    ?assertEqual(stateful, maps:get(type, Struct)),
    ?assertEqual(~"complex_root", maps:get(id, Struct)),

    % Verify hierarchical accumulator contains the component
    HierarchicalAcc = arizona_socket:get_hierarchical_acc(UpdatedSocket),
    ?assert(maps:is_key(~"complex_root", HierarchicalAcc)),

    ComponentData = maps:get(~"complex_root", HierarchicalAcc),
    ?assertEqual(stateful, maps:get(type, ComponentData)),
    ?assert(maps:is_key(static, ComponentData)),
    ?assert(maps:is_key(dynamic, ComponentData)),

    % Verify static content is properly structured
    StaticParts = maps:get(static, ComponentData),
    ?assert(is_list(StaticParts)).
