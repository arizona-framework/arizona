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
    -module(arizona_render_view_template).
    render(View) ->
        arizona_render:view_template(View, ~"""
        Hello, {arizona_view:get_assign(name, View)}!
        """).
    """"),
    Got = arizona_transform:parse_transform(Forms, []),
    ?assertMatch(
        [
            _,
            {function, 2, render, 1, [
                {clause, 2, [{var, 2, 'View'}], [], [
                    {call, 3, {remote, 3, {atom, 3, arizona_render}, {atom, 3, view_template}}, [
                        {tuple, 0, [_, _]}
                    ]}
                ]}
            ]}
        ],
        Got
    ).

render_component_template(Config) when is_list(Config) ->
    Forms = merl:quote(~""""
    -module(arizona_render_component_template).
    render(View) ->
        arizona_render:component_template(View, ~"""
        Hello, {arizona_view:get_assign(name, View)}!
        """).
    """"),
    Got = arizona_transform:parse_transform(Forms, []),
    ?assertMatch(
        [
            _,
            {function, 2, render, 1, [
                {clause, 2, [{var, 2, 'View'}], [], [
                    {call, 3, {remote, 3, {atom, 3, arizona_render}, {atom, 3, component_template}},
                        [
                            {tuple, 0, [_, _]}
                        ]}
                ]}
            ]}
        ],
        Got
    ).

render_nested_template(Config) when is_list(Config) ->
    Forms = merl:quote(~""""
    -module(arizona_render_nested_template).
    render(View) ->
        arizona_render:nested_template(View, ~"""
        Hello, {arizona_view:get_assign(name, View)}!
        """).
    """"),
    Got = arizona_transform:parse_transform(Forms, []),
    ?assertMatch(
        [
            _,
            {function, 2, render, 1, [
                {clause, 2, [{var, 2, 'View'}], [], [
                    {call, 3, {remote, 3, {atom, 3, arizona_render}, {atom, 3, nested_template}}, [
                        {tuple, 0, [_, _]}
                    ]}
                ]}
            ]}
        ],
        Got
    ).
