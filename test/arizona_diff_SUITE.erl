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
            diff_view,
            diff_component
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
    Assigns = #{id => ViewId, foo => ~"foo", bar => ~"bar"},
    ChangedAssigns = #{bar => ~"baz"},
    ExpectAssigns = maps:merge(Assigns, ChangedAssigns),
    Diff = [{2, ~"baz"}],
    Expect = {
        arizona_view:new(Mod, ExpectAssigns, ChangedAssigns, Diff),
        arizona_socket:new(diff, #{
            ViewId => arizona_view:new(Mod, ExpectAssigns, ChangedAssigns, Diff)
        })
    },
    View = arizona_view:new(Mod, Assigns, ChangedAssigns, []),
    Token = arizona_render:view_template(View, ~"""
    <div id={arizona_view:get_assign(id, View)}>
    {arizona_view:get_assign(foo, View)}
    {arizona_view:get_assign(bar, View)}
    </div>
    """),
    TokenCallback = fun() -> Token end,
    Socket = arizona_socket:new(diff),
    Got = arizona_diff:diff(Index, Vars, TokenCallback, View, Socket),
    ?assertEqual(Expect, Got).

diff_component_template(Config) when is_list(Config) ->
    Index = 0,
    Vars = [foo, bar],
    Mod = undefined,
    Assigns = #{foo => ~"foo", bar => ~"bar"},
    ChangedAssigns = #{bar => ~"baz"},
    Diff = [{1, ~"baz"}],
    Expect = {
        arizona_view:new(Mod, Assigns, ChangedAssigns, Diff),
        arizona_socket:new(diff)
    },
    View = arizona_view:new(Mod, Assigns, ChangedAssigns, []),
    Token = arizona_render:component_template(View, ~"""
    <div>
    {arizona_view:get_assign(foo, View)}
    {arizona_view:get_assign(bar, View)}
    </div>
    """),
    TokenCallback = fun() -> Token end,
    Socket = arizona_socket:new(diff),
    Got = arizona_diff:diff(Index, Vars, TokenCallback, View, Socket),
    ?assertEqual(Expect, Got).

diff_nested_template(Config) when is_list(Config) ->
    Index = 0,
    Vars = [foo, bar],
    Mod = undefined,
    Assigns = #{foo => ~"foo", bar => ~"bar"},
    ChangedAssigns = #{bar => ~"baz"},
    Diff = [{0, [{1, ~"baz"}]}],
    Expect = {
        arizona_view:new(Mod, Assigns, ChangedAssigns, Diff),
        arizona_socket:new(diff)
    },
    View = arizona_view:new(Mod, Assigns, ChangedAssigns, []),
    Token = arizona_render:nested_template(View, ~"""
    <div>
    {arizona_view:get_assign(foo, View)}
    {arizona_view:get_assign(bar, View)}
    </div>
    """),
    TokenCallback = fun() -> Token end,
    Socket = arizona_socket:new(diff),
    Got = arizona_diff:diff(Index, Vars, TokenCallback, View, Socket),
    ?assertEqual(Expect, Got).

diff_view(Config) when is_list(Config) ->
    Index = 0,
    Vars = [id, count, btn_text],
    Mod = arizona_example_template,
    CounterMod = arizona_example_counter,
    ViewId = ~"app",
    CounterViewId = ~"counter",
    Assigns = #{id => ViewId, count => 0, btn_text => ~"Increment"},
    ChangedAssigns = #{count => 1, btn_text => ~"+1"},
    ExpectAssigns = maps:merge(Assigns, ChangedAssigns),
    Diff = [
        {1, [
            template,
            [~"<div id=\"", ~"\">", ~"", ~"</div>"],
            [
                ~"counter",
                ~"1",
                [
                    template,
                    [~"<button>", ~"</button>"],
                    [~"+1"]
                ]
            ]
        ]}
    ],
    Expect = {
        arizona_view:new(Mod, ExpectAssigns, ChangedAssigns, Diff),
        arizona_socket:new(diff, #{
            ViewId => arizona_view:new(Mod, ExpectAssigns, ChangedAssigns, Diff),
            CounterViewId => arizona_view:new(
                CounterMod, ExpectAssigns#{id => CounterViewId}, #{}, []
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
    Socket1 = arizona_socket:set_render_context(diff, Socket0),
    Socket = arizona_socket:remove_view(CounterViewId, Socket1),
    Got = arizona_diff:diff(Index, Vars, TokenCallback, View, Socket),
    ?assertEqual(Expect, Got).

diff_component(Config) when is_list(Config) ->
    Index = 0,
    Vars = [text],
    Mod = arizona_example_components,
    Fun = button,
    Assigns = #{text => ~"Increment"},
    ChangedAssigns = #{text => ~"+1"},
    View = arizona_view:new(Mod, Assigns, ChangedAssigns, []),
    Diff = [{0, [{0, ~"Increment"}]}],
    Expect = {
        arizona_view:new(Mod, Assigns, ChangedAssigns, Diff),
        arizona_socket:new(diff)
    },
    Token = arizona_render:component(Mod, Fun, Assigns),
    Socket = arizona_socket:new(diff),
    TokenCallback = fun() -> Token end,
    Got = arizona_diff:diff(Index, Vars, TokenCallback, View, Socket),
    ?assertEqual(Expect, Got).
