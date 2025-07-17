-module(arizona_template_SUITE).
-behaviour(ct_suite).
-include_lib("stdlib/include/assert.hrl").
-compile([export_all, nowarn_export_all]).

%% --------------------------------------------------------------------
%% Behaviour (ct_suite) callbacks
%% --------------------------------------------------------------------

all() ->
    [
        {group, function_callbacks}
    ].

groups() ->
    [
        {function_callbacks, [parallel], [
            render_stateful_mode_render,
            render_stateful_mode_hierarchical,
            render_stateless_mode_render,
            render_stateless_mode_hierarchical
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
