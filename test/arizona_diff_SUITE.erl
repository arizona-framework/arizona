-module(arizona_diff_SUITE).
-behaviour(ct_suite).
-include_lib("stdlib/include/assert.hrl").
-compile([export_all, nowarn_export_all]).

%% --------------------------------------------------------------------
%% Behaviour (ct_suite) callbacks
%% --------------------------------------------------------------------

all() ->
    [{group, diff}].

groups() ->
    [
        {diff, [parallel], [
            diff_view_template,
            diff_component_template,
            diff_nested_template,
            diff_list_template,
            diff_view,
            diff_view_new_id,
            diff_view_ignore,
            diff_component,
            diff_list
        ]}
    ].

%% --------------------------------------------------------------------
%% Tests
%% --------------------------------------------------------------------

diff_view_template(Config) when is_list(Config) ->
    Index = 0,
    Vars = [id, foo, bar],
    Mod = undefined,
    ViewId = ~"foo",
    Bindings = #{id => ViewId, foo => ~"foo", bar => ~"bar"},
    ChangedBindings = #{bar => ~"baz"},
    ExpectBindings = maps:merge(Bindings, ChangedBindings),
    Diff = [{2, ~"baz"}],
    Expect = {
        arizona_view:new(Mod, ExpectBindings, #{}, [], [], Diff),
        arizona_socket:new(diff, undefined, #{
            ViewId => arizona_view:new(Mod, ExpectBindings, #{}, [], [], [])
        })
    },
    View = arizona_view:new(Mod, Bindings, ChangedBindings, [], [], []),
    Token = arizona_renderer:render_view_template(View, ~"""
    <div id={arizona:get_binding(id, View)}>
    {arizona:get_binding(foo, View)}
    {arizona:get_binding(bar, View)}
    </div>
    """),
    TokenCallback = fun() -> Token end,
    Socket = arizona_socket:new(diff),
    Got = arizona_diff:diff(Index, Vars, TokenCallback, View, Socket, #{}),
    ?assertEqual(Expect, Got).

diff_component_template(Config) when is_list(Config) ->
    Index = 0,
    Vars = [foo, bar],
    Mod = undefined,
    Bindings = #{foo => ~"foo", bar => ~"bar"},
    ChangedBindings = #{bar => ~"baz"},
    Diff = [{1, ~"baz"}],
    Expect = {
        arizona_view:new(Mod, Bindings, ChangedBindings, [], [], Diff),
        arizona_socket:new(diff)
    },
    View = arizona_view:new(Mod, Bindings, ChangedBindings, [], [], []),
    Token = arizona_renderer:render_component_template(View, ~"""
    <div>
        {arizona:get_binding(foo, View)}
        {arizona:get_binding(bar, View)}
    </div>
    """),
    TokenCallback = fun() -> Token end,
    Socket = arizona_socket:new(diff),
    Got = arizona_diff:diff(Index, Vars, TokenCallback, View, Socket, #{}),
    ?assertEqual(Expect, Got).

diff_nested_template(Config) when is_list(Config) ->
    Index = 0,
    Vars = [foo, bar],
    Mod = undefined,
    Bindings = #{foo => ~"foo", bar => ~"bar"},
    ChangedBindings = #{bar => ~"baz"},
    Diff = [{0, [{1, ~"baz"}]}],
    Expect = {
        arizona_view:new(Mod, Bindings, ChangedBindings, [], [], Diff),
        arizona_socket:new(diff)
    },
    View = arizona_view:new(Mod, Bindings, ChangedBindings, [], [], []),
    Token = arizona_renderer:render_nested_template(View, ~"""
    <div>
        {arizona:get_binding(foo, View)}
        {arizona:get_binding(bar, View)}
    </div>
    """),
    TokenCallback = fun() -> Token end,
    Socket = arizona_socket:new(diff),
    Got = arizona_diff:diff(Index, Vars, TokenCallback, View, Socket, #{}),
    ?assertEqual(Expect, Got).

diff_list_template(Config) when is_list(Config) ->
    Index = 0,
    Vars = [foo, bar],
    Mod = undefined,
    Bindings = #{foo => ~"foo", bar => ~"bar"},
    ChangedBindings = #{bar => ~"baz"},
    Diff = [
        {0, [
            [{1, ~"baz"}, {0, ~"foo"}],
            [{1, ~"baz"}, {0, ~"foo"}]
        ]}
    ],
    Expect = {
        arizona_view:new(Mod, Bindings, ChangedBindings, [], [], Diff),
        arizona_socket:new(diff)
    },
    View = arizona_view:new(Mod, Bindings, ChangedBindings, [], [], []),
    Token = arizona_renderer:render_list(
        fun(Item) ->
            arizona_renderer:render_nested_template(#{'View' => View, 'Item' => Item}, ~"""
            <div>
                {arizona:get_binding(foo, View)}
                {arizona:get_binding(bar, View, Item)}
            </div>
            """)
        end,
        [~"1", ~"2"]
    ),
    TokenCallback = fun() -> Token end,
    Socket = arizona_socket:new(diff),
    Got = arizona_diff:diff(Index, Vars, TokenCallback, View, Socket, #{}),
    ?assertEqual(Expect, Got).

diff_view(Config) when is_list(Config) ->
    Index = 0,
    Vars = [id, count, btn_text],
    Mod = arizona_example_template,
    CounterMod = arizona_example_counter,
    ViewId = ~"app",
    CounterViewId = ~"counter",
    Bindings = #{id => ViewId, count => 0, btn_text => ~"Increment"},
    ChangedBindings = #{count => 1, btn_text => ~"+1"},
    ExpectBindings = maps:merge(Bindings, ChangedBindings),
    Rendered = [
        template,
        [
            ~"<html>\n    <head></head>\n    <body id=\"",
            ~"\"> ",
            ~"</body>\n</html>"
        ],
        [
            ~"app",
            [
                template,
                [
                    ~"<div id=\"",
                    ~"\"> ",
                    ~"",
                    ~"</div>"
                ],
                [
                    ~"counter",
                    ~"0",
                    [
                        template,
                        [~"<button> ", ~"</button>"],
                        [~"Increment"]
                    ]
                ]
            ]
        ]
    ],
    Diff = [
        {1, [
            template,
            [~"<div id=\"", ~"\"> ", ~"", ~"</div>"],
            [
                ~"counter",
                ~"1",
                [
                    template,
                    [~"<button> ", ~"</button>"],
                    [~"+1"]
                ]
            ]
        ]}
    ],
    Expect = {
        arizona_view:new(Mod, ExpectBindings, #{}, Rendered, [], Diff),
        arizona_socket:new(diff, undefined, #{
            ViewId => arizona_view:new(Mod, ExpectBindings, #{}, Rendered, [], []),
            CounterViewId => arizona_view:new(
                CounterMod,
                ExpectBindings#{id => CounterViewId},
                #{},
                [
                    template,
                    [
                        ~"<div id=\"",
                        ~"\"> ",
                        ~"",
                        ~"</div>"
                    ],
                    [
                        ~"counter",
                        ~"1",
                        [
                            template,
                            [~"<button> ", ~"</button>"],
                            [~"+1"]
                        ]
                    ]
                ],
                [],
                []
            )
        })
    },
    RenderSocket = arizona_socket:new(render),
    {ok, MountedView} = arizona_view:mount(Mod, Bindings, RenderSocket),
    RenderToken = arizona_view:render(MountedView),
    ParentView = arizona_view:new(#{}),
    {RenderedView, Socket0} = arizona_renderer:render(
        RenderToken, MountedView, ParentView, RenderSocket
    ),
    View0 = arizona_view:set_tmp_rendered([], RenderedView),
    View = arizona_view:put_bindings(ChangedBindings, View0),
    Token = arizona_view:render(View),
    TokenCallback = fun() -> Token end,
    Socket1 = arizona_socket:set_render_context(diff, Socket0),
    Socket = arizona_socket:remove_view(CounterViewId, Socket1),
    Got = arizona_diff:diff(Index, Vars, TokenCallback, View, Socket, #{}),
    ?assertEqual(Expect, Got).

diff_view_new_id(Config) when is_list(Config) ->
    Index = 0,
    Vars = [id, view_id, name, ignore],
    RootMod = arizona_example_template_new_id,
    Mod = arizona_example_new_id,
    RootViewId = ~"foo",
    ViewId = ~"bar",
    Bindings = #{id => RootViewId, view_id => ViewId, name => ~"World", ignore => false},
    ChangedBindings = #{view_id => ~"baz", name => ~"Arizona"},
    ExpectBindings = maps:merge(Bindings, ChangedBindings),
    Rendered = [
        template,
        [~"<div id=\"", ~"\"> ", ~"</div>"],
        [
            ~"foo",
            [
                template,
                [
                    ~"<div id=\"",
                    ~"\">\n    Hello, ",
                    ~"!\n</div>"
                ],
                [~"bar", ~"World"]
            ]
        ]
    ],
    Diff = [
        {1, [
            template,
            [~"<div id=\"", ~"\">\n    Hello, ", ~"!\n</div>"],
            [~"baz", ~"Arizona"]
        ]}
    ],
    Expect = {
        arizona_view:new(RootMod, ExpectBindings, #{}, Rendered, [], Diff),
        arizona_socket:new(diff, undefined, #{
            ~"baz" => arizona_view:new(
                Mod,
                #{id => ~"baz", ignore => false, name => ~"Arizona"},
                #{},
                [
                    template,
                    [
                        ~"<div id=\"",
                        ~"\">\n    Hello, ",
                        ~"!\n</div>"
                    ],
                    [~"baz", ~"Arizona"]
                ],
                [],
                []
            ),
            % FIXME: The 'ViewId' should be removed from the socket views.
            % The question is: How to know the previous id?
            ViewId => arizona_view:new(
                Mod,
                #{id => ~"bar", ignore => false, name => ~"World"},
                #{},
                [
                    template,
                    [
                        ~"<div id=\"",
                        ~"\">\n    Hello, ",
                        ~"!\n</div>"
                    ],
                    [~"bar", ~"World"]
                ],
                [],
                []
            ),
            RootViewId => arizona_view:new(RootMod, ExpectBindings, #{}, Rendered, [], [])
        })
    },
    RenderSocket = arizona_socket:new(render),
    {ok, MountedView} = arizona_view:mount(RootMod, Bindings, RenderSocket),
    RenderToken = arizona_view:render(MountedView),
    ParentView = arizona_view:new(#{}),
    {RenderedView, Socket0} = arizona_renderer:render(
        RenderToken, MountedView, ParentView, RenderSocket
    ),
    View0 = arizona_view:set_tmp_rendered([], RenderedView),
    View = arizona_view:put_bindings(ChangedBindings, View0),
    Token = arizona_view:render(View),
    TokenCallback = fun() -> Token end,
    Socket = arizona_socket:set_render_context(diff, Socket0),
    Got = arizona_diff:diff(Index, Vars, TokenCallback, View, Socket, #{}),
    ?assertEqual(Expect, Got).

diff_view_ignore(Config) when is_list(Config) ->
    Index = 0,
    Vars = [id, view_id, name, ignore],
    RootMod = arizona_example_template_new_id,
    Mod = arizona_example_new_id,
    RootViewId = ~"foo",
    ViewId = ~"bar",
    Bindings = #{id => RootViewId, view_id => ViewId, name => ~"World", ignore => false},
    ChangedBindings = #{view_id => ~"baz", name => ~"Arizona", ignore => true},
    ExpectBindings = maps:merge(Bindings, ChangedBindings),
    Rendered = [
        template,
        [~"<div id=\"", ~"\"> ", ~"</div>"],
        [
            ~"foo",
            [
                template,
                [
                    ~"<div id=\"",
                    ~"\">\n    Hello, ",
                    ~"!\n</div>"
                ],
                [~"bar", ~"World"]
            ]
        ]
    ],
    Expect = {
        arizona_view:new(RootMod, ExpectBindings, #{}, Rendered, [], []),
        arizona_socket:new(diff, undefined, #{
            % FIXME: The 'ViewId' should be removed from the socket views.
            % The question is: How to know the previous id?
            ViewId => arizona_view:new(
                Mod,
                #{id => ~"bar", ignore => false, name => ~"World"},
                #{},
                [
                    template,
                    [
                        ~"<div id=\"",
                        ~"\">\n    Hello, ",
                        ~"!\n</div>"
                    ],
                    [~"bar", ~"World"]
                ],
                [],
                []
            ),
            RootViewId => arizona_view:new(
                RootMod,
                ExpectBindings,
                #{},
                [
                    template,
                    [~"<div id=\"", ~"\"> ", ~"</div>"],
                    [
                        ~"foo",
                        [
                            template,
                            [
                                ~"<div id=\"",
                                ~"\">\n    Hello, ",
                                ~"!\n</div>"
                            ],
                            [~"bar", ~"World"]
                        ]
                    ]
                ],
                [],
                []
            )
        })
    },
    RenderSocket = arizona_socket:new(render),
    {ok, MountedView} = arizona_view:mount(RootMod, Bindings, RenderSocket),
    RenderToken = arizona_view:render(MountedView),
    ParentView = arizona_view:new(#{}),
    {RenderedView, Socket0} = arizona_renderer:render(
        RenderToken, MountedView, ParentView, RenderSocket
    ),
    View0 = arizona_view:set_tmp_rendered([], RenderedView),
    View = arizona_view:put_bindings(ChangedBindings, View0),
    Token = arizona_view:render(View),
    TokenCallback = fun() -> Token end,
    Socket = arizona_socket:set_render_context(diff, Socket0),
    Got = arizona_diff:diff(Index, Vars, TokenCallback, View, Socket, #{}),
    ?assertEqual(Expect, Got).

diff_component(Config) when is_list(Config) ->
    Index = 0,
    Vars = [text],
    Mod = arizona_example_components,
    Fun = button,
    Bindings = #{text => ~"Increment"},
    ChangedBindings = #{text => ~"+1"},
    Rendered = [template, [~"<button> ", ~"</button>"], [~"+1"]],
    Diff = [{0, ~"+1"}],
    Expect = {
        arizona_view:new(Mod, Bindings, ChangedBindings, Rendered, [], Diff),
        arizona_socket:new(diff)
    },
    RenderSocket = arizona_socket:new(render),
    View0 = arizona_view:new(Mod, Bindings, ChangedBindings, [], [], []),
    RenderToken = arizona_component:render(Mod, Fun, View0),
    ParentView = arizona_view:new(#{}),
    {RenderedView, Socket0} = arizona_renderer:render(
        RenderToken, View0, ParentView, RenderSocket
    ),
    View1 = arizona_view:set_tmp_rendered([], RenderedView),
    View = arizona_view:put_bindings(ChangedBindings, View1),
    Token = arizona_component:render(Mod, Fun, View),
    Socket = arizona_socket:set_render_context(diff, Socket0),
    TokenCallback = fun() -> Token end,
    Got = arizona_diff:diff(Index, Vars, TokenCallback, View, Socket, #{}),
    ?assertEqual(Expect, Got).

diff_list(Config) when is_list(Config) ->
    Index = 0,
    Vars = [foo, bar],
    Mod = undefined,
    Bindings = #{foo => ~"foo", bar => ~"bar"},
    ChangedBindings = #{bar => ~"baz"},
    Diff = [{0, [{0, [~"foo", ~"baz"]}]}],
    Expect = {
        arizona_view:new(Mod, Bindings, ChangedBindings, [], [], Diff),
        arizona_socket:new(diff)
    },
    View = arizona_view:new(Mod, Bindings, ChangedBindings, [], [], []),
    Token = arizona_renderer:render_nested_template(View, ~"""
    <div>
        {[
            arizona:get_binding(foo, View),
            arizona:get_binding(bar, View)
         ]}
    </div>
    """),
    TokenCallback = fun() -> Token end,
    Socket = arizona_socket:new(diff),
    Got = arizona_diff:diff(Index, Vars, TokenCallback, View, Socket, #{}),
    ?assertEqual(Expect, Got).
