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
    Expect = {ok, arizona_view:new(arizona_example_template, #{id => ~"app", count => 0}, #{}, [])},
    Socket = arizona_socket:new(#{}),
    Got = arizona_view:mount(arizona_example_template, #{id => ~"app", count => 0}, Socket),
    ?assertEqual(Expect, Got).

mount_ignore(Config) when is_list(Config) ->
    Expect = ignore,
    Socket = arizona_socket:new(#{}),
    Got = arizona_view:mount(arizona_example_ignore, #{}, Socket),
    ?assertEqual(Expect, Got).

render(Config) when is_list(Config) ->
    Expect = {
        arizona_view:new(
            arizona_example_template, #{count => 0, id => ~"app"}, #{}, [
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
            ]
        ),
        arizona_socket:new(#{
            ~"app" => arizona_view:new(
                arizona_example_template, #{count => 0, id => ~"app"}, #{}, [
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
                            [
                                ~"<div id=\"",
                                ~"\">",
                                ~"",
                                ~"</div>"
                            ],
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
                ]
            ),
            ~"counter" => arizona_view:new(
                arizona_example_counter, #{count => 0, id => ~"counter"}, #{}, []
            )
        })
    },
    Socket = arizona_socket:new(#{}),
    {ok, View} = arizona_view:mount(arizona_example_template, #{id => ~"app", count => 0}, Socket),
    Got = arizona_view:render(arizona_example_template, View, Socket),
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
    Socket = arizona_socket:new(#{}),
    {ok, View0} = arizona_view:mount(arizona_example_template, #{id => ~"app", count => 0}, Socket),
    {View, _Socket} = arizona_view:render(arizona_example_template, View0, Socket),
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
    Callback = arizona_render:nested_template(~""""
    <div>
        {case arizona_view:get_assign(show_dialog, View) of
             true ->
                 arizona_render:nested_template(~"""
                 <dialog open>
                     {arizona_view:get_assign(message, View)}
                 </dialog>
                 """);
             false ->
                 ~""
         end}
    </div>
    """"),
    ParentView0 = arizona_view:new(
        undefined, #{show_dialog => true, message => ~"Hello, World!"}, #{}, []
    ),
    Socket = arizona_socket:new(#{}),
    {ParentView, _Socket} = erlang:apply(Callback, [ParentView0, Socket]),
    Got = arizona_view:rendered_to_iolist(ParentView),
    ?assertEqual(Expect, Got).
