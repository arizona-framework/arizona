-module(arizona_renderer_SUITE).
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
    Got = arizona_renderer:render_view_template(View, ~"""
    <div id="{arizona:get_binding(id, View)}">
      {arizona:get_binding(foo, View)}
      {arizona:get_binding(bar, View)}
    </div>
    """),
    ?assertMatch(
        {view_template, [~"<div id=\"", ~"\"> ", ~"", ~"</div>"], [Callback, _, _]} when
            is_function(Callback, 3),
        Got
    ).

render_component_template(Config) when is_list(Config) ->
    View = arizona_view:new(#{foo => ~"foo", bar => ~"bar"}),
    Got = arizona_renderer:render_component_template(View, ~"""
    <div>
      {arizona:get_binding(foo, View)}
      {arizona:get_binding(bar, View)}
    </div>
    """),
    ?assertMatch(
        {component_template, [~"<div> ", ~"", ~"</div>"], [Callback, _]} when
            is_function(Callback, 3),
        Got
    ).

render_nested_template(Config) when is_list(Config) ->
    View = arizona_view:new(#{foo => ~"foo", bar => ~"bar"}),
    Got = arizona_renderer:render_nested_template(View, ~"""
    <div>
      {arizona:get_binding(foo, View)}
      {arizona:get_binding(bar, View)}
    </div>
    """),
    ?assertMatch(
        {nested_template, [~"<div> ", ~"", ~"</div>"], [Callback, _]} when is_function(Callback, 3),
        Got
    ).

render_view(Config) when is_list(Config) ->
    Mod = foo,
    Bindings = #{id => ~"foo"},
    Expect = {view, Mod, Bindings},
    Got = arizona_renderer:render_view(Mod, Bindings),
    ?assertEqual(Expect, Got).

render_component(Config) when is_list(Config) ->
    Mod = foo,
    Fun = bar,
    Bindings = #{},
    Expect = {component, Mod, Fun, Bindings},
    Got = arizona_renderer:render_component(Mod, Fun, Bindings),
    ?assertEqual(Expect, Got).
