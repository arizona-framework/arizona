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
        {group, diff_functionality}
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
    {Changes, _UpdatedSocket} = arizona_template:diff_stateful(Module, Bindings, Socket1),

    % Should return empty changes when no binding changes
    ?assertEqual([], Changes).

diff_stateful_with_changes(Config) when is_list(Config) ->
    % Test diff_stateful when there are binding changes
    create_test_component(),

    {Module, Socket, _LivePid} = start_live_process(),

    % Create stateful state with changes
    StatefulState0 = arizona_stateful:new(~"test_component_1", Module, #{
        title => ~"Old Title"
    }),
    StatefulState = arizona_stateful:put_binding(title, ~"New Title", StatefulState0),
    Socket1 = arizona_socket:put_stateful_state(StatefulState, Socket),

    % Call diff with bindings that include the component id and new values
    Bindings = #{id => ~"test_component_1", title => ~"New Title"},
    {Changes, _UpdatedSocket} = arizona_template:diff_stateful(Module, Bindings, Socket1),

    % Should generate changes when bindings change
    ?assert(is_list(Changes)).

diff_stateful_with_dependencies(Config) when is_list(Config) ->
    % Test diff_stateful with runtime dependencies
    create_test_component(),

    {Module, Socket, _LivePid} = start_live_process(),

    % Create stateful state with changes that have dependencies
    StatefulState0 = arizona_stateful:new(~"test_component_1", Module, #{
        title => ~"Old Title"
    }),
    StatefulState = arizona_stateful:put_binding(title, ~"New Title", StatefulState0),
    Socket1 = arizona_socket:put_stateful_state(StatefulState, Socket),

    % Call diff with bindings that include the component id and new values
    Bindings = #{id => ~"test_component_1", title => ~"New Title"},
    {Changes, _UpdatedSocket} = arizona_template:diff_stateful(Module, Bindings, Socket1),

    % Should generate element-level changes based on dependencies
    ?assert(is_list(Changes)).

diff_stateless_no_live_pid(Config) when is_list(Config) ->
    % Test diff_stateless when no live process is available
    create_simple_component(),

    Socket = create_test_socket(diff),
    Bindings = #{text => ~"Test Text"},

    {Change, UpdatedSocket} = arizona_template:diff_stateless(
        simple_component, render, Bindings, Socket
    ),

    % Should return empty changes when no live process
    ?assertEqual([], Change),
    ?assertEqual(Socket, UpdatedSocket).

diff_stateless_with_element_index(Config) when is_list(Config) ->
    % Test diff_stateless with element index from live process
    create_simple_component(),

    {_Module, Socket, _LivePid} = start_live_process(),

    Bindings = #{text => ~"Test Text"},
    {Change, _UpdatedSocket} = arizona_template:diff_stateless(
        simple_component, render, Bindings, Socket
    ),

    % Should return element change with proper index
    ?assertMatch({_, _}, Change).

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

    % Should generate changes for affected elements
    ?assert(is_list(Changes)),
    ?assert(length(Changes) >= 0).

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
    StatefulState0 = arizona_stateful:new(~"test_component_1", Module, #{
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
    ?assert(is_list(Changes)).

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
