-module(arizona_view_SUITE).
-behaviour(ct_suite).
-include_lib("stdlib/include/assert.hrl").
-compile([export_all, nowarn_export_all]).

%% --------------------------------------------------------------------
%% Behaviour (ct_suite) callbacks
%% --------------------------------------------------------------------

all() ->
    [
        {group, callback},
        {group, to_iolist}
    ].

groups() ->
    [
        {callback, [parallel], [
            mount,
            mount_ignore,
            render
        ]},
        {to_iolist, [
            rendered_to_iolist,
            render_nested_template_to_iolist
        ]}
    ].

%% --------------------------------------------------------------------
%% Tests
%% --------------------------------------------------------------------

mount(Config) when is_list(Config) ->
    Mod = arizona_example_template,
    Assigns = #{id => ~"app", count => 0},
    Expect = {ok, arizona_view:new(Mod, Assigns)},
    Socket = arizona_socket:new(),
    Got = arizona_view:mount(Mod, Assigns, Socket),
    ?assertEqual(Expect, Got).

mount_ignore(Config) when is_list(Config) ->
    Expect = ignore,
    Socket = arizona_socket:new(),
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
        arizona_socket:new(#{
            ~"app" => RenderedView,
            ~"counter" => arizona_view:new(
                arizona_example_counter, #{id => ~"counter", count => 0}, #{}, []
            )
        })
    },
    ParentView = arizona_view:new(#{}),
    Socket = arizona_socket:new(),
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
    Socket = arizona_socket:new(),
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
            <<"<div>">>,
            [<<"<dialog open>">>, <<"Hello, World!">>, <<"</dialog>">>],
            <<"</div>">>
        ]
    ],
    ParentView0 = arizona_view:new(#{show_dialog => true, message => ~"Hello, World!"}),
    Token = arizona_render:nested_template(ParentView0, ~""""
    <div>
        {case arizona_view:get_assign(show_dialog, View) of
             true ->
                 arizona_render:nested_template(View, ~"""
                 <dialog open>
                     {arizona_view:get_assign(message, View)}
                 </dialog>
                 """);
             false ->
                 ~""
         end}
    </div>
    """"),
    Socket = arizona_socket:new(),
    {ParentView, _Socket} = arizona_render:render(Token, ParentView0, ParentView0, Socket),
    Got = arizona_view:rendered_to_iolist(ParentView),
    ?assertEqual(Expect, Got).
