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
            render_template
        ]}
    ].

%% --------------------------------------------------------------------
%% Tests
%% --------------------------------------------------------------------

render_template(Config) when is_list(Config) ->
    Expect = {
        arizona_view:new(?MODULE, #{id => ~"foo", bar => ~"bar"}, #{}, [
            template,
            [~"<div id=\"", ~"\">", ~"", ~"</div>"],
            [~"foo", ~"bar", ~"baz"]
        ]),
        arizona_socket:new(
            #{
                ~"foo" =>
                    {view, arizona_render_SUITE, #{id => ~"foo", bar => ~"bar"}, #{}, [
                        template,
                        [~"<div id=\"", ~"\">", ~"", ~"</div>"],
                        [~"foo", ~"bar", ~"baz"]
                    ]}
            },
            html
        )
    },
    View = arizona_view:new(?MODULE, #{id => ~"foo", bar => ~"bar"}, #{}, []),
    Socket = arizona_socket:new(#{}, html),
    Got = arizona_render:template(View, Socket, ~"""
    <div id="{arizona_view:get_assign(id, View)}">
      {arizona_view:get_assign(bar, View)}{~"baz"}
    </div>
    """),
    ?assertEqual(Expect, Got).
