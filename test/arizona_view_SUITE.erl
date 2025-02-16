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
            render_nested_template_to_iolist
        ]},
        {diff, [
            diff
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
    RenderedView = arizona_view:new(Mod, Assigns, #{}, [
        template,
        [
            ~"<html>\n    <head></head>\n    <body id=\"",
            ~"\">",
            ~"</body>\n</html>"
        ],
        [
            ~"app",
            [
                template,
                [~"<div id=\"", ~"\">", ~"", ~"</div>"],
                [
                    ~"counter",
                    ~"0",
                    [
                        template,
                        [~"<button>", ~"</button>"],
                        [~"Increment"]
                    ]
                ]
            ]
        ]
    ]),
    Expect = {
        RenderedView,
        arizona_socket:new(render, #{
            ~"app" => RenderedView,
            ~"counter" => arizona_view:new(
                arizona_example_counter,
                #{id => ~"counter", count => 0, btn_text => ~"Increment"},
                #{},
                []
            )
        })
    },
    ParentView = arizona_view:new(#{}),
    Socket = arizona_socket:new(render),
    {ok, View} = arizona_view:mount(Mod, Assigns, Socket),
    Token = arizona_view:render(Mod, View),
    Got = arizona_render:render(Token, View, ParentView, Socket),
    ?assertEqual(Expect, Got).

rendered_to_iolist(Config) when is_list(Config) ->
    Expect = [
        ~"<html>\n    <head></head>\n    <body id=\"",
        ~"app",
        ~"\">",
        [
            ~"<div id=\"",
            ~"counter",
            ~"\">",
            ~"0",
            ~"",
            [~"<button>", ~"Increment", ~"</button>"],
            ~"</div>"
        ],
        ~"</body>\n</html>"
    ],
    ParentView = arizona_view:new(#{}),
    Socket = arizona_socket:new(render),
    Mod = arizona_example_template,
    Assigns = #{id => ~"app", count => 0},
    {ok, View0} = arizona_view:mount(Mod, Assigns, Socket),
    Token = arizona_view:render(Mod, View0),
    {View, _Socket} = arizona_render:render(Token, View0, ParentView, Socket),
    Got = arizona_view:rendered_to_iolist(View),
    ?assertEqual(Expect, Got).

render_nested_template_to_iolist(Config) when is_list(Config) ->
    Expect = [
        [
            ~"<div>",
            [~"<dialog open>", ~"Hello, World!", ~"</dialog>"],
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
    Diff = [{1, [{1, ~"0"}]}],
    Expect = {
        arizona_view:new(Mod, ExpectAssigns, ChangedAssigns, Diff),
        arizona_socket:new(diff, #{
            ViewId => arizona_view:new(Mod, ExpectAssigns, ChangedAssigns, Diff),
            CounterViewId => arizona_view:new(
                CounterMod, ExpectAssigns#{id => CounterViewId}, ChangedAssigns, []
            )
        })
    },
    RenderSocket = arizona_socket:new(render),
    {ok, MountedView} = arizona_view:mount(Mod, Assigns, RenderSocket),
    RenderToken = arizona_view:render(Mod, MountedView),
    ParentView = arizona_view:new(#{}),
    {RenderedView, Socket0} = arizona_render:render(
        RenderToken, MountedView, ParentView, RenderSocket
    ),
    View0 = arizona_view:set_rendered([], RenderedView),
    View = arizona_view:put_assigns(ChangedAssigns, View0),
    Token = arizona_view:render(Mod, View),
    TokenCallback = fun() -> Token end,
    Socket = arizona_socket:set_render_context(diff, Socket0),
    Got = arizona_diff:diff(Index, Vars, TokenCallback, View, Socket),
    ?assertEqual(Expect, Got).
