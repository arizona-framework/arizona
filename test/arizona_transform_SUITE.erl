-module(arizona_transform_SUITE).
-behaviour(ct_suite).
-include_lib("stdlib/include/assert.hrl").
-compile([export_all, nowarn_export_all]).

%

-dialyzer({nowarn_function, render_view_template/1}).
-dialyzer({nowarn_function, render_component_template/1}).
-dialyzer({nowarn_function, render_nested_template/1}).

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
            render_nested_template
        ]}
    ].

%% --------------------------------------------------------------------
%% Tests
%% --------------------------------------------------------------------

render_view_template(Config) when is_list(Config) ->
    Forms = merl:quote(~""""
    -module(arizona_renderer_view_template).
    render(View) ->
        arizona:render_view_template(View, ~"""
        Hello, {arizona:get_binding(name, View)}!
        """).
    """"),
    Got = arizona_transform:parse_transform(Forms, []),
    ?assertMatch(
        [
            _,
            {function, 2, render, 1, [
                {clause, 2, [{var, 2, 'View'}], [], [
                    {tuple, 0, [{atom, 0, view_template}, _, _]}
                ]}
            ]}
        ],
        Got
    ).

render_component_template(Config) when is_list(Config) ->
    Forms = merl:quote(~""""
    -module(arizona_renderer_component_template).
    render(View) ->
        arizona:render_component_template(View, ~"""
        Hello, {arizona:get_binding(name, View)}!
        """).
    """"),
    Got = arizona_transform:parse_transform(Forms, []),
    ?assertMatch(
        [
            _,
            {function, 2, render, 1, [
                {clause, 2, [{var, 2, 'View'}], [], [
                    {tuple, 0, [{atom, 0, component_template}, _, _]}
                ]}
            ]}
        ],
        Got
    ).

render_nested_template(Config) when is_list(Config) ->
    Forms = merl:quote(~""""
    -module(arizona_renderer_nested_template).
    render(View) ->
        arizona:render_nested_template(View, ~"""
        Hello, {arizona:get_binding(name, View)}!
        """).
    """"),
    Got = arizona_transform:parse_transform(Forms, []),
    ?assertMatch(
        [
            _,
            {function, 2, render, 1, [
                {clause, 2, [{var, 2, 'View'}], [], [
                    {tuple, 0, [{atom, 0, nested_template}, _, _]}
                ]}
            ]}
        ],
        Got
    ).
