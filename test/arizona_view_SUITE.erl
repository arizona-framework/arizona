-module(arizona_view_SUITE).
-behaviour(ct_suite).
-include_lib("stdlib/include/assert.hrl").
-compile([export_all, nowarn_export_all]).

%% --------------------------------------------------------------------
%% Behaviour (ct_suite) callbacks
%% --------------------------------------------------------------------

all() ->
    [
        {group, mount},
        {group, render},
        {group, render_to_iolist},
        {group, diff}
    ].

groups() ->
    [
        {mount, [parallel], [
            mount,
            mount_ignore
        ]},
        {render, [parallel], [
            render,
            render_table_component
        ]},
        {render_to_iolist, [parallel], [
            rendered_to_iolist,
            render_nested_template_to_iolist,
            render_table_component_to_iolist
        ]},
        {diff, [parallel], [
            diff,
            diff_to_iolist
        ]}
    ].

%% --------------------------------------------------------------------
%% Tests
%% --------------------------------------------------------------------

mount(Config) when is_list(Config) ->
    Mod = arizona_example_template,
    Bindings = #{id => ~"app", count => 0},
    Expect = {ok, arizona_view:new(Mod, Bindings)},
    Socket = arizona_socket:new(render),
    Got = arizona_view:mount(Mod, Bindings, Socket),
    ?assertEqual(Expect, Got).

mount_ignore(Config) when is_list(Config) ->
    Expect = ignore,
    Socket = arizona_socket:new(render),
    Got = arizona_view:mount(arizona_example_ignore, #{}, Socket),
    ?assertEqual(Expect, Got).

render(Config) when is_list(Config) ->
    Mod = arizona_example_template,
    Bindings = #{id => ~"app", count => 0},
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
                [~"<div id=\"", ~"\"> ", ~"", ~"</div>"],
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
    Expect = {
        arizona_view:new(Mod, Bindings, #{}, Rendered, Rendered, []),
        arizona_socket:new(render, undefined, #{
            ~"app" => arizona_view:new(Mod, Bindings, #{}, Rendered, [], []),
            ~"counter" => arizona_view:new(
                arizona_example_counter,
                #{id => ~"counter", count => 0, btn_text => ~"Increment"},
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
                        ~"0",
                        [
                            template,
                            [~"<button> ", ~"</button>"],
                            [~"Increment"]
                        ]
                    ]
                ],
                [],
                []
            )
        })
    },
    ParentView = arizona_view:new(#{}),
    Socket = arizona_socket:new(render),
    {ok, View} = arizona_view:mount(Mod, Bindings, Socket),
    Token = arizona_view:render(View),
    Got = arizona_renderer:render(Token, View, ParentView, Socket),
    ?assertEqual(Expect, Got).

rendered_to_iolist(Config) when is_list(Config) ->
    Expect = [
        ~"<html>\n    <head></head>\n    <body id=\"",
        ~"app",
        ~"\"> ",
        [
            ~"<div id=\"",
            ~"counter",
            ~"\"> ",
            ~"0",
            ~"",
            [~"<button> ", ~"Increment", ~"</button>"],
            ~"</div>"
        ],
        ~"</body>\n</html>"
    ],
    ParentView = arizona_view:new(#{}),
    Socket = arizona_socket:new(render),
    Mod = arizona_example_template,
    Bindings = #{id => ~"app", count => 0},
    {ok, View0} = arizona_view:mount(Mod, Bindings, Socket),
    Token = arizona_view:render(View0),
    {View, _Socket} = arizona_renderer:render(Token, View0, ParentView, Socket),
    Got = arizona_view:rendered_to_iolist(View),
    ?assertEqual(Expect, Got).

render_nested_template_to_iolist(Config) when is_list(Config) ->
    Expect = [
        [
            ~"<div> ",
            [~"<dialog open> ", ~"Hello, World!", ~"</dialog>"],
            ~"</div>"
        ]
    ],
    ParentView0 = arizona_view:new(#{show_dialog => true, message => ~"Hello, World!"}),
    Token = arizona_renderer:render_nested_template(ParentView0, ~""""
    <div>
        {arizona_renderer:render_if_true(arizona:get_binding(show_dialog, View), fun() ->
             arizona_renderer:render_nested_template(View, ~"""
             <dialog open>
                 {arizona_view:get_binding(message, View)}
             </dialog>
             """)
         end)}
    </div>
    """"),
    Socket = arizona_socket:new(render),
    {ParentView, _Socket} = arizona_renderer:render(Token, ParentView0, ParentView0, Socket),
    Got = arizona_view:rendered_to_iolist(ParentView),
    ?assertEqual(Expect, Got).

render_table_component(Config) when is_list(Config) ->
    Mod = arizona_example_components,
    Fun = table,
    Bindings = #{
        columns => [
            #{
                label => ~"Name",
                callback => fun(User) -> maps:get(name, User) end
            },
            #{
                label => ~"Age",
                callback => fun(User) -> maps:get(age, User) end
            }
        ],
        rows => [
            #{name => ~"Jane", age => ~"34"},
            #{name => ~"Bob", age => ~"51"}
        ]
    },
    Rendered = [
        template,
        [~"<table>\n    <tr> ", ~"</tr> ", ~"</table>"],
        [
            [
                list_template,
                [~"<th>", ~"</th>"],
                [[[~"Name"]], [[~"Age"]]]
            ],
            [
                list_template,
                [~"<tr> ", ~"</tr>"],
                [
                    [
                        [
                            [
                                list_template,
                                [~"<td> ", ~"</td>"],
                                [[[~"Jane"]], [[~"34"]]]
                            ]
                        ]
                    ],
                    [
                        [
                            [
                                list_template,
                                [~"<td> ", ~"</td>"],
                                [[[~"Bob"]], [[~"51"]]]
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ],
    Socket = arizona_socket:new(render),
    Expect = {
        arizona_view:new(undefined, Bindings, #{}, Rendered, Rendered, []),
        Socket
    },
    View = arizona_view:new(Bindings),
    Token = arizona_component:render(Mod, Fun, View),
    Got = arizona_renderer:render(Token, View, View, Socket),
    ?assertEqual(Expect, Got).

render_table_component_to_iolist(Config) when is_list(Config) ->
    Mod = arizona_example_components,
    Fun = table,
    Bindings = #{
        columns => [
            #{
                label => ~"Name",
                callback => fun(User) -> maps:get(name, User) end
            },
            #{
                label => ~"Age",
                callback => fun(User) -> maps:get(age, User) end
            }
        ],
        rows => [
            #{name => ~"Jane", age => ~"34"},
            #{name => ~"Bob", age => ~"51"}
        ]
    },
    View0 = arizona_view:new(Bindings),
    Socket = arizona_socket:new(render),
    Expect = [
        ~"<table>\n    <tr> ",
        [
            [~"<th>", [~"Name"], ~"</th>"],
            [~"<th>", [~"Age"], ~"</th>"]
        ],
        ~"</tr> ",
        [
            [
                ~"<tr> ",
                [
                    [
                        [~"<td> ", [~"Jane"], ~"</td>"],
                        [~"<td> ", [~"34"], ~"</td>"]
                    ]
                ],
                ~"</tr>"
            ],
            [
                ~"<tr> ",
                [
                    [
                        [~"<td> ", [~"Bob"], ~"</td>"],
                        [~"<td> ", [~"51"], ~"</td>"]
                    ]
                ],
                ~"</tr>"
            ]
        ],
        ~"</table>"
    ],
    Token = arizona_component:render(Mod, Fun, View0),
    {View, _Socket} = arizona_renderer:render(Token, View0, View0, Socket),
    Got = arizona_view:rendered_to_iolist(View),
    ?assertEqual(Expect, Got).

diff(Config) when is_list(Config) ->
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
                [~"<div id=\"", ~"\"> ", ~"", ~"</div>"],
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
    Diff = [{1, [{2, [{0, ~"+1"}]}, {1, ~"1"}]}],
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
                        ~"0",
                        [
                            template,
                            [~"<button> ", ~"</button>"],
                            [~"Increment"]
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
    Socket = arizona_socket:set_render_context(diff, Socket0),
    Got = arizona_diff:diff(Index, Vars, TokenCallback, View, Socket, #{}),
    ?assertEqual(Expect, Got).

diff_to_iolist(Config) when is_list(Config) ->
    Index = 0,
    Vars = [id, count, btn_text],
    Mod = arizona_example_template,
    ViewId = ~"app",
    Bindings = #{id => ViewId, count => 0, btn_text => ~"Increment"},
    ChangedBindings = #{count => 1, btn_text => ~"+1"},
    Expect = [
        ~"<html>\n    <head></head>\n    <body id=\"",
        ~"app",
        ~"\"> ",
        [
            ~"<div id=\"",
            ~"counter",
            ~"\"> ",
            ~"1",
            ~"",
            [~"<button> ", ~"+1", ~"</button>"],
            ~"</div>"
        ],
        ~"</body>\n</html>"
    ],
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
    Socket = arizona_socket:set_render_context(diff, Socket0),
    {DiffView, _Socket} = arizona_diff:diff(Index, Vars, TokenCallback, View, Socket, #{}),
    Got = arizona_view:diff_to_iolist(DiffView),
    ?assertEqual(Expect, Got).
