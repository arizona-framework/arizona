-module(arizona_render_SUITE).
-behaviour(ct_suite).
-include_lib("stdlib/include/assert.hrl").
-compile([export_all, nowarn_export_all]).

%% --------------------------------------------------------------------
%% Behaviour (ct_suite) callbacks
%% --------------------------------------------------------------------

all() ->
    [{group, render}].

groups() ->
    [
        {render, [parallel], [
            render_view_template,
            render_component_template,
            render_nested_template,
            render_view,
            render_component
        ]}
    ].

%% --------------------------------------------------------------------
%% Tests
%% --------------------------------------------------------------------

render_view_template(Config) when is_list(Config) ->
    View = arizona_view:new(#{id => ~"foo", foo => ~"foo", bar => ~"bar"}),
    Socket = arizona_socket:new(),
    Expect =
        {view_template, View, Socket, [~"<div id=\"", ~"\">", ~"", ~"</div>"], [
            ~"foo", ~"foo", ~"bar"
        ]},
    Got = arizona_render:view_template(View, Socket, ~"""
    <div id="{arizona_view:get_assign(id, View)}">
      {arizona_view:get_assign(foo, View)}
      {arizona_view:get_assign(bar, View)}
    </div>
    """),
    ?assertEqual(Expect, Got).

render_component_template(Config) when is_list(Config) ->
    View = arizona_view:new(#{foo => ~"foo", bar => ~"bar"}),
    Socket = arizona_socket:new(),
    Expect = {component_template, View, Socket, [~"<div>", ~"", ~"</div>"], [~"foo", ~"bar"]},
    Got = arizona_render:component_template(View, Socket, ~"""
    <div>
      {arizona_view:get_assign(foo, View)}
      {arizona_view:get_assign(bar, View)}
    </div>
    """),
    ?assertEqual(Expect, Got).

render_nested_template(Config) when is_list(Config) ->
    View = arizona_view:new(#{foo => ~"foo", bar => ~"bar"}),
    Socket = arizona_socket:new(),
    Expect = {nested_template, View, Socket, [~"<div>", ~"", ~"</div>"], [~"foo", ~"bar"]},
    {callback, Callback} = arizona_render:nested_template(~"""
    <div>
      {arizona_view:get_assign(foo, View)}
      {arizona_view:get_assign(bar, View)}
    </div>
    """),
    Got = erlang:apply(Callback, [View, Socket]),
    ?assertEqual(Expect, Got).

render_view(Config) when is_list(Config) ->
    Mod = foo,
    Assigns = #{id => ~"foo"},
    ParentView = arizona_view:new(#{}),
    Socket = arizona_socket:new(),
    Expect = {view, ParentView, Socket, Mod, Assigns},
    {callback, Callback} = arizona_render:view(Mod, Assigns),
    Got = erlang:apply(Callback, [ParentView, Socket]),
    ?assertEqual(Expect, Got).

render_component(Config) when is_list(Config) ->
    Mod = foo,
    Fun = bar,
    Assigns = #{},
    ParentView = arizona_view:new(#{}),
    Socket = arizona_socket:new(),
    Expect = {component, ParentView, Socket, Mod, Fun, Assigns},
    {callback, Callback} = arizona_render:component(Mod, Fun, Assigns),
    Got = erlang:apply(Callback, [ParentView, Socket]),
    ?assertEqual(Expect, Got).
