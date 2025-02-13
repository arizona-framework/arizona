-module(arizona_render_SUITE).
-behaviour(ct_suite).
-include_lib("stdlib/include/assert.hrl").
-compile([export_all, nowarn_export_all]).

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
            render_nested_template,
            render_view,
            ignore_view,
            render_component
        ]}
    ].

%% --------------------------------------------------------------------
%% Tests
%% --------------------------------------------------------------------

render_view_template(Config) when is_list(Config) ->
    Expect = {
        arizona_view:new(?MODULE, #{id => ~"foo", bar => ~"bar"}, #{}, [
            template,
            [~"<div id=\"", ~"\">", ~"", ~"</div>"],
            [~"foo", ~"bar", ~"baz"]
        ]),
        arizona_socket:new(
            #{
                ~"foo" => arizona_view:new(?MODULE, #{id => ~"foo", bar => ~"bar"}, #{}, [
                    template,
                    [~"<div id=\"", ~"\">", ~"", ~"</div>"],
                    [~"foo", ~"bar", ~"baz"]
                ])
            }
        )
    },
    View = arizona_view:new(?MODULE, #{id => ~"foo", bar => ~"bar"}, #{}, []),
    Socket = arizona_socket:new(#{}),
    Got = arizona_render:view_template(View, Socket, ~"""
    <div id="{arizona_view:get_assign(id, View)}">
      {arizona_view:get_assign(bar, View)}{~"baz"}
    </div>
    """),
    ?assertEqual(Expect, Got).

render_component_template(Config) when is_list(Config) ->
    Expect = {
        arizona_view:new(?MODULE, #{}, #{}, [template, [~"Ok"], []]),
        arizona_socket:new(#{})
    },
    View = arizona_view:new(?MODULE, #{}, #{}, []),
    Socket = arizona_socket:new(#{}),
    Got = arizona_render:component_template(View, Socket, ~"""
    Ok
    """),
    ?assertEqual(Expect, Got).

render_nested_template(Config) when is_list(Config) ->
    Expect = {
        arizona_view:new(undefined, #{show_dialog => true, message => ~"Hello, World!"}, #{}, [
            [
                template,
                [~"<div>", ~"</div>"],
                [
                    [
                        template,
                        [~"<dialog open>", ~"</dialog>"],
                        [~"Hello, World!"]
                    ]
                ]
            ]
        ]),
        arizona_socket:new(#{})
    },
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
    ParentView = arizona_view:new(
        undefined, #{show_dialog => true, message => ~"Hello, World!"}, #{}, []
    ),
    Socket = arizona_socket:new(#{}),
    Got = erlang:apply(Callback, [ParentView, Socket]),
    ?assertEqual(Expect, Got).

render_view(Config) when is_list(Config) ->
    Expect = {
        arizona_view:new(?MODULE, #{}, #{}, [
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
        ]),
        arizona_socket:new(#{
            ~"counter" => arizona_view:new(
                arizona_example_counter, #{count => 0, id => ~"counter"}, #{}, []
            )
        })
    },
    Callback = arizona_render:view(arizona_example_counter, #{id => ~"counter", count => 0}),
    ParentView = arizona_view:new(?MODULE, #{}, #{}, []),
    Socket = arizona_socket:new(#{}),
    Got = erlang:apply(Callback, [ParentView, Socket]),
    ?assertEqual(Expect, Got).

ignore_view(Config) when is_list(Config) ->
    Expect = {
        arizona_view:new(?MODULE, #{}, #{}, []),
        arizona_socket:new(#{})
    },
    Callback = arizona_render:view(arizona_example_ignore, #{id => foo}),
    ParentView = arizona_view:new(?MODULE, #{}, #{}, []),
    Socket = arizona_socket:new(#{}),
    Got = erlang:apply(Callback, [ParentView, Socket]),
    ?assertEqual(Expect, Got).

render_component(Config) when is_list(Config) ->
    Expect = {
        arizona_view:new(arizona_render_SUITE, #{}, #{}, [
            [template, [~"<button>", ~"</button>"], [~"Ok"]]
        ]),
        arizona_socket:new(#{})
    },
    Callback = arizona_render:component(arizona_example_components, button, #{text => ~"Ok"}),
    ParentView = arizona_view:new(?MODULE, #{}, #{}, []),
    Socket = arizona_socket:new(#{}),
    Got = erlang:apply(Callback, [ParentView, Socket]),
    ?assertEqual(Expect, Got).
