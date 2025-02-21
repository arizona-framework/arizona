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
            rendered_to_iolist,
            render_nested_template_to_iolist,
            render_table_component,
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
    Assigns = #{id => ~"app", count => 0},
    Expect = {ok, arizona_view:new(Mod, Assigns)},
    Socket = arizona_socket:new(render),
    Got = arizona_view:mount(Mod, Assigns, Socket),
    ?assertEqual(Expect, Got).

mount_ignore(Config) when is_list(Config) ->
    Expect = ignore,
    Socket = arizona_socket:new(render),
    Got = arizona_view:mount(arizona_example_ignore, #{}, Socket),
    ?assertEqual(Expect, Got).

render(Config) when is_list(Config) ->
    Mod = arizona_example_template,
    Assigns = #{id => ~"app", count => 0},
    RenderedView = arizona_view:new(
        Mod,
        Assigns,
        #{},
        [
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
        []
    ),
    Expect = {
        RenderedView,
        arizona_socket:new(render, #{
            ~"app" => RenderedView,
            ~"counter" => arizona_view:new(
                arizona_example_counter,
                #{id => ~"counter", count => 0, btn_text => ~"Increment"},
                #{},
                [],
                []
            )
        })
    },
    ParentView = arizona_view:new(#{}),
    Socket = arizona_socket:new(render),
    {ok, View} = arizona_view:mount(Mod, Assigns, Socket),
    Token = arizona_view:render(View),
    Got = arizona_render:render(Token, View, ParentView, Socket),
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
    Assigns = #{id => ~"app", count => 0},
    {ok, View0} = arizona_view:mount(Mod, Assigns, Socket),
    Token = arizona_view:render(View0),
    {View, _Socket} = arizona_render:render(Token, View0, ParentView, Socket),
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
    Token = arizona_render:nested_template(ParentView0, ~""""
    <div>
        {arizona_render:if_true(arizona_view:get_assign(show_dialog, View), fun() ->
             arizona_render:nested_template(View, ~"""
             <dialog open>
                 {arizona_view:get_assign(message, View)}
             </dialog>
             """)
         end)}
    </div>
    """"),
    Socket = arizona_socket:new(render),
    {ParentView, _Socket} = arizona_render:render(Token, ParentView0, ParentView0, Socket),
    Got = arizona_view:rendered_to_iolist(ParentView),
    ?assertEqual(Expect, Got).

render_table_component(Config) when is_list(Config) ->
    Mod = arizona_example_components,
    Fun = table,
    Assigns = #{
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
    View = arizona_view:new(Assigns),
    Socket = arizona_socket:new(render),
    Expect = {
        arizona_view:set_tmp_rendered(
            [
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
            View
        ),
        Socket
    },
    Token = arizona_component:render(Mod, Fun, View),
    Got = arizona_render:render(Token, View, View, Socket),
    ?assertEqual(Expect, Got).

render_table_component_to_iolist(Config) when is_list(Config) ->
    Mod = arizona_example_components,
    Fun = table,
    Assigns = #{
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
    View0 = arizona_view:new(Assigns),
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
    {View, _Socket} = arizona_render:render(Token, View0, View0, Socket),
    Got = arizona_view:rendered_to_iolist(View),
    ?assertEqual(Expect, Got).

diff(Config) when is_list(Config) ->
    Index = 0,
    Vars = [id, count, btn_text],
    Mod = arizona_example_template,
    CounterMod = arizona_example_counter,
    ViewId = ~"app",
    CounterViewId = ~"counter",
    Assigns = #{id => ViewId, count => 0, btn_text => ~"Increment"},
    ChangedAssigns = #{count => 1, btn_text => ~"+1"},
    ExpectAssigns = maps:merge(Assigns, ChangedAssigns),
    Diff = [{1, [{2, [{0, ~"+1"}]}, {1, ~"1"}]}],
    Expect = {
        arizona_view:new(Mod, ExpectAssigns, ChangedAssigns, [], Diff),
        arizona_socket:new(diff, #{
            ViewId => arizona_view:new(Mod, ExpectAssigns, ChangedAssigns, [], Diff),
            CounterViewId => arizona_view:new(
                CounterMod, ExpectAssigns#{id => CounterViewId}, ChangedAssigns, [], []
            )
        })
    },
    RenderSocket = arizona_socket:new(render),
    {ok, MountedView} = arizona_view:mount(Mod, Assigns, RenderSocket),
    RenderToken = arizona_view:render(MountedView),
    ParentView = arizona_view:new(#{}),
    {RenderedView, Socket0} = arizona_render:render(
        RenderToken, MountedView, ParentView, RenderSocket
    ),
    View0 = arizona_view:set_tmp_rendered([], RenderedView),
    View = arizona_view:put_assigns(ChangedAssigns, View0),
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
    Assigns = #{id => ViewId, count => 0, btn_text => ~"Increment"},
    ChangedAssigns = #{count => 1, btn_text => ~"+1"},
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
    {ok, MountedView} = arizona_view:mount(Mod, Assigns, RenderSocket),
    RenderToken = arizona_view:render(MountedView),
    ParentView = arizona_view:new(#{}),
    {RenderedView, Socket0} = arizona_render:render(
        RenderToken, MountedView, ParentView, RenderSocket
    ),
    Rendered = arizona_view:tmp_rendered(RenderedView),
    View0 = arizona_view:set_tmp_rendered([], RenderedView),
    View = arizona_view:put_assigns(ChangedAssigns, View0),
    Token = arizona_view:render(View),
    TokenCallback = fun() -> Token end,
    Socket = arizona_socket:set_render_context(diff, Socket0),
    {DiffView, _Socket} = arizona_diff:diff(Index, Vars, TokenCallback, View, Socket, #{}),
    Got = arizona_view:diff_to_iolist(Rendered, DiffView),
    ?assertEqual(Expect, Got).
