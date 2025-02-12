-module(arizona_view_SUITE).
-behaviour(ct_suite).
-include_lib("stdlib/include/assert.hrl").
-compile([export_all, nowarn_export_all]).

%% --------------------------------------------------------------------
%% Behaviour (ct_suite) callbacks
%% --------------------------------------------------------------------

all() ->
    [{group, callback}].

groups() ->
    [
        {callback, [parallel], [
            mount,
            mount_ignore,
            render
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
