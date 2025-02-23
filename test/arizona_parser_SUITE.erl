-module(arizona_parser_SUITE).
-behaviour(ct_suite).
-include_lib("stdlib/include/assert.hrl").
-compile([export_all, nowarn_export_all]).

%% --------------------------------------------------------------------
%% Behaviour (ct_suite) callbacks
%% --------------------------------------------------------------------

all() ->
    [
        {group, parse},
        {group, parse_transform}
    ].

groups() ->
    [
        {parse, [parallel], [
            parse_from_socket,
            parse_render
        ]},
        {parse_transform, [parallel], [
            parse_transform_render_list
        ]}
    ].

%% --------------------------------------------------------------------
%% Tests
%% --------------------------------------------------------------------

parse_from_socket(Config) when is_list(Config) ->
    Expect = {
        _Static = [
            {bin, 1, [{bin_element, 1, {string, 1, "foo"}, default, [utf8]}]},
            {bin, 1, [{bin_element, 1, {string, 1, []}, default, [utf8]}]},
            {bin, 1, [{bin_element, 1, {string, 1, "bar"}, default, [utf8]}]},
            {bin, 1, [{bin_element, 1, {string, 1, "qu\"o\"t\\\"ed"}, default, [utf8]}]}
        ],
        _Dynamic = [
            {'fun', 1,
                {clauses, [
                    {clause, 1, [{var, 1, 'ViewAcc'}, {var, 1, 'Socket'}, {var, 1, 'Opts'}], [], [
                        {'case', 2,
                            {call, 2,
                                {remote, 2, {atom, 2, arizona_socket}, {atom, 2, render_context}}, [
                                    {var, 2, 'Socket'}
                                ]},
                            [
                                {clause, 3, [{atom, 3, render}], [], [
                                    {call, 4,
                                        {remote, 4, {atom, 4, arizona_renderer}, {atom, 4, render}},
                                        [
                                            {atom, 4, foo},
                                            {var, 4, 'View'},
                                            {var, 4, 'ViewAcc'},
                                            {var, 4, 'Socket'}
                                        ]}
                                ]},
                                {clause, 5, [{atom, 5, diff}], [], [
                                    {match, 6, {var, 6, 'Index'}, {integer, 6, 0}},
                                    {match, 7, {var, 7, 'Vars'}, {nil, 7}},
                                    {match, 8, {var, 8, 'TokenCallback'},
                                        {'fun', 8,
                                            {clauses, [{clause, 8, [], [], [{atom, 8, foo}]}]}}},
                                    {call, 9, {remote, 9, {atom, 9, arizona_diff}, {atom, 9, diff}},
                                        [
                                            {var, 9, 'Index'},
                                            {var, 9, 'Vars'},
                                            {var, 9, 'TokenCallback'},
                                            {var, 9, 'ViewAcc'},
                                            {var, 9, 'Socket'},
                                            {var, 9, 'Opts'}
                                        ]}
                                ]}
                            ]}
                    ]}
                ]}},
            {'fun', 1,
                {clauses, [
                    {clause, 1, [{var, 1, 'ViewAcc'}, {var, 1, 'Socket'}, {var, 1, 'Opts'}], [], [
                        {'case', 2,
                            {call, 2,
                                {remote, 2, {atom, 2, arizona_socket}, {atom, 2, render_context}}, [
                                    {var, 2, 'Socket'}
                                ]},
                            [
                                {clause, 3, [{atom, 3, render}], [], [
                                    {call, 4,
                                        {remote, 4, {atom, 4, arizona_renderer}, {atom, 4, render}},
                                        [
                                            {tuple, 4, [{atom, 4, bar}]},
                                            {var, 4, 'View'},
                                            {var, 4, 'ViewAcc'},
                                            {var, 4, 'Socket'}
                                        ]}
                                ]},
                                {clause, 5, [{atom, 5, diff}], [], [
                                    {match, 6, {var, 6, 'Index'}, {integer, 6, 1}},
                                    {match, 7, {var, 7, 'Vars'}, {nil, 7}},
                                    {match, 8, {var, 8, 'TokenCallback'},
                                        {'fun', 8,
                                            {clauses, [
                                                {clause, 8, [], [], [{tuple, 8, [{atom, 8, bar}]}]}
                                            ]}}},
                                    {call, 9, {remote, 9, {atom, 9, arizona_diff}, {atom, 9, diff}},
                                        [
                                            {var, 9, 'Index'},
                                            {var, 9, 'Vars'},
                                            {var, 9, 'TokenCallback'},
                                            {var, 9, 'ViewAcc'},
                                            {var, 9, 'Socket'},
                                            {var, 9, 'Opts'}
                                        ]}
                                ]}
                            ]}
                    ]}
                ]}},
            {'fun', 1,
                {clauses, [
                    {clause, 1, [{var, 1, 'ViewAcc'}, {var, 1, 'Socket'}, {var, 1, 'Opts'}], [], [
                        {'case', 2,
                            {call, 2,
                                {remote, 2, {atom, 2, arizona_socket}, {atom, 2, render_context}}, [
                                    {var, 2, 'Socket'}
                                ]},
                            [
                                {clause, 3, [{atom, 3, render}], [], [
                                    {call, 4,
                                        {remote, 4, {atom, 4, arizona_renderer}, {atom, 4, render}},
                                        [
                                            {cons, 4, {atom, 4, bar}, {nil, 4}},
                                            {var, 4, 'View'},
                                            {var, 4, 'ViewAcc'},
                                            {var, 4, 'Socket'}
                                        ]}
                                ]},
                                {clause, 5, [{atom, 5, diff}], [], [
                                    {match, 6, {var, 6, 'Index'}, {integer, 6, 2}},
                                    {match, 7, {var, 7, 'Vars'}, {nil, 7}},
                                    {match, 8, {var, 8, 'TokenCallback'},
                                        {'fun', 8,
                                            {clauses, [
                                                {clause, 8, [], [], [
                                                    {cons, 8, {atom, 8, bar}, {nil, 8}}
                                                ]}
                                            ]}}},
                                    {call, 9, {remote, 9, {atom, 9, arizona_diff}, {atom, 9, diff}},
                                        [
                                            {var, 9, 'Index'},
                                            {var, 9, 'Vars'},
                                            {var, 9, 'TokenCallback'},
                                            {var, 9, 'ViewAcc'},
                                            {var, 9, 'Socket'},
                                            {var, 9, 'Opts'}
                                        ]}
                                ]}
                            ]}
                    ]}
                ]}}
        ]
    },
    Tokens = arizona_scanner:scan(#{}, ~"""
    foo{foo}{{bar}}{% drop this }bar{[bar]}qu"o"t\"ed
    """),
    Got = arizona_parser:parse(Tokens, #{render_context => from_socket}),
    ?assertEqual(Expect, Got).

parse_render(Config) when is_list(Config) ->
    Expect = {
        _Static = [
            {bin, 1, [{bin_element, 1, {string, 1, "foo"}, default, [utf8]}]},
            {bin, 1, [{bin_element, 1, {string, 1, []}, default, [utf8]}]},
            {bin, 1, [{bin_element, 1, {string, 1, "bar"}, default, [utf8]}]},
            {bin, 1, [{bin_element, 1, {string, 1, "qu\"o\"t\\\"ed"}, default, [utf8]}]}
        ],
        _Dynamic = [
            {'fun', 1,
                {clauses, [
                    {clause, 1, [{var, 1, 'ViewAcc'}, {var, 1, 'Socket'}, {var, 1, 'Opts'}], [], [
                        {call, 2, {remote, 2, {atom, 2, arizona_renderer}, {atom, 2, render}}, [
                            {atom, 2, foo},
                            {var, 2, 'View'},
                            {var, 2, 'ViewAcc'},
                            {var, 2, 'Socket'}
                        ]}
                    ]}
                ]}},
            {'fun', 1,
                {clauses, [
                    {clause, 1, [{var, 1, 'ViewAcc'}, {var, 1, 'Socket'}, {var, 1, 'Opts'}], [], [
                        {call, 2, {remote, 2, {atom, 2, arizona_renderer}, {atom, 2, render}}, [
                            {tuple, 2, [{atom, 2, bar}]},
                            {var, 2, 'View'},
                            {var, 2, 'ViewAcc'},
                            {var, 2, 'Socket'}
                        ]}
                    ]}
                ]}},
            {'fun', 1,
                {clauses, [
                    {clause, 1, [{var, 1, 'ViewAcc'}, {var, 1, 'Socket'}, {var, 1, 'Opts'}], [], [
                        {call, 2, {remote, 2, {atom, 2, arizona_renderer}, {atom, 2, render}}, [
                            {cons, 2, {atom, 2, bar}, {nil, 2}},
                            {var, 2, 'View'},
                            {var, 2, 'ViewAcc'},
                            {var, 2, 'Socket'}
                        ]}
                    ]}
                ]}}
        ]
    },
    Tokens = arizona_scanner:scan(#{}, ~"""
    foo{foo}{{bar}}{% drop this }bar{[bar]}qu"o"t\"ed
    """),
    Got = arizona_parser:parse(Tokens, #{render_context => render}),
    ?assertEqual(Expect, Got).

parse_transform_render_list(Config) when is_list(Config) ->
    Expect = {
        _Static = [
            {bin, 1, [{bin_element, 1, {string, 1, "<ul> "}, default, [utf8]}]},
            {bin, 1, [{bin_element, 1, {string, 1, "</ul>"}, default, [utf8]}]}
        ],
        _Dynamic = [
            {'fun', 1,
                {clauses, [
                    {clause, 1, [{var, 1, 'ViewAcc'}, {var, 1, 'Socket'}, {var, 1, 'Opts'}], [], [
                        {call, 2, {remote, 2, {atom, 2, arizona_renderer}, {atom, 2, render}}, [
                            {tuple, 2, [
                                {atom, 2, list_template},
                                {cons, 3,
                                    {bin, 3, [
                                        {bin_element, 3, {string, 3, "<li> "}, default, [utf8]}
                                    ]},
                                    {cons, 3,
                                        {bin, 3, [
                                            {bin_element, 3, {string, 3, "<br/> "}, default, [utf8]}
                                        ]},
                                        {cons, 3,
                                            {bin, 3, [
                                                {bin_element, 3, {string, 3, "</li>"}, default, [
                                                    utf8
                                                ]}
                                            ]},
                                            {nil, 3}}}},
                                {'fun', 4,
                                    {clauses, [
                                        {clause, 4, [{var, 4, 'Item'}], [], [
                                            {cons, 5,
                                                {call, 5, {atom, 5, integer_to_binary}, [
                                                    {var, 5, 'Item'}
                                                ]},
                                                {cons, 5,
                                                    {call, 5, {atom, 5, integer_to_binary}, [
                                                        {var, 5, 'Item'}
                                                    ]},
                                                    {nil, 5}}}
                                        ]}
                                    ]}},
                                {call, 7, {remote, 7, {atom, 7, arizona}, {atom, 7, get_assign}}, [
                                    {atom, 7, list}, {var, 7, 'View'}
                                ]}
                            ]},
                            {var, 7, 'View'},
                            {var, 7, 'ViewAcc'},
                            {var, 7, 'Socket'}
                        ]}
                    ]}
                ]}}
        ]
    },

    Tokens = arizona_scanner:scan(#{}, ~""""
    <ul>
    {arizona:render_list(fun(Item) ->
        arizona:render_nested_template(#{'View' => View, 'Item' => Item}, ~"""
        <li>
            {integer_to_binary(Item)}
            <br/>
            {integer_to_binary(Item)}
        </li>
        """)
     end, arizona:get_assign(list, View))}
    </ul>
    """"),
    View = arizona_view:new(#{list => [1, 2, 3]}),
    Got = arizona_parser:parse(Tokens, #{
        render_context => render,
        bindings => #{'View' => View}
    }),
    ?assertEqual(Expect, Got).
