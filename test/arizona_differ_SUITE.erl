-module(arizona_differ_SUITE).
-behaviour(ct_suite).
-include_lib("stdlib/include/assert.hrl").
-compile([export_all, nowarn_export_all]).

%% --------------------------------------------------------------------
%% Behaviour (ct_suite) callbacks
%% --------------------------------------------------------------------

all() ->
    [
        {group, diff_tests}
    ].

groups() ->
    [
        {diff_tests, [parallel], [
            diff_stateful_no_changes_test,
            diff_stateful_with_changes_test,
            diff_stateful_with_stateless_changes_test,
            diff_stateless_test
        ]}
    ].

init_per_suite(Config) ->
    MockStatefulModule = arizona_differ_stateful_mock,
    MockStatelessModule = arizona_differ_stateless_mock,
    MockStatelessRenderFun = render_page,

    MockStatefulModuleCode = merl:qquote(~""""
    -module('@module').
    -behavior(arizona_view).

    -export([mount/1]).
    -export([render/1]).
    -export([handle_event/3]).

    mount(Req) ->
        {Bindings, _BindingsReq} = arizona_request:get_bindings(Req),
        arizona_stateful:new('@module', Bindings#{
            id => ~"stateful",
            name => ~"Arizona"
        }).

    render(Bindings) ->
        arizona_template:from_string(~"""
        <div>Stateful: {arizona_template:get_binding(name, Bindings)}</div>
        {arizona_template:render_stateless(arizona_differ_stateless_mock, render_page, #{
            title => arizona_template:get_binding(stateless_title, Bindings, fun() ->
                arizona_template:get_binding(name, Bindings)
            end)
        })}
        """).

    handle_event(~"update_name", #{~"name" := Name}, State) ->
        {noreply, arizona_stateful:put_binding(name, Name, State)};
    handle_event(~"update_stateless_title", #{~"title" := Title}, State) ->
        {noreply, arizona_stateful:put_binding(stateless_title, Title, State)}.
    """", [{module, merl:term(MockStatefulModule)}]),

    MockStatelessModuleCode = merl:qquote(~""""
    -module('@module').
    -export(['@render_fun'/1]).

    '@render_fun'(Bindings) ->
        arizona_template:from_string(~"""
        <span>Stateless: {arizona_template:get_binding(title, Bindings)}</span>
        """).
    """", [
        {module, merl:term(MockStatelessModule)},
        {render_fun, merl:term(MockStatelessRenderFun)}
    ]),

    {ok, _StatefulBinary} = merl:compile_and_load(MockStatefulModuleCode),
    {ok, _StatelessBinary} = merl:compile_and_load(MockStatelessModuleCode),

    [
        {mock_stateful_module, MockStatefulModule},
        {mock_stateless_module, MockStatelessModule},
        {mock_stateless_render_fun, MockStatelessRenderFun}
        | Config
    ].

end_per_suite(Config) ->
    {mock_stateful_module, MockStatefulModule} = proplists:lookup(mock_stateful_module, Config),
    {mock_stateless_module, MockStatelessModule} = proplists:lookup(mock_stateless_module, Config),

    code:purge(MockStatefulModule),
    code:purge(MockStatelessModule),

    code:delete(MockStatefulModule),
    code:delete(MockStatelessModule),

    ok.

%% --------------------------------------------------------------------
%% Diff tests
%% --------------------------------------------------------------------

diff_stateful_no_changes_test(Config) when is_list(Config) ->
    ct:comment("diff_stateful/3 should return empty diff when no bindings changed"),
    {mock_stateful_module, MockModule} = proplists:lookup(mock_stateful_module, Config),
    {_LivePid, Bindings, MockView} = create_mock_view(MockModule, #{}),
    {Diff, _UpdatedView} = arizona_differ:diff_stateful(MockModule, Bindings, MockView),
    ?assertEqual([], Diff).

diff_stateful_with_changes_test(Config) when is_list(Config) ->
    ct:comment("diff_stateful/3 should return diff when bindings changed and dependencies exist"),
    {mock_stateful_module, MockModule} = proplists:lookup(mock_stateful_module, Config),
    {LivePid, Bindings, _MockView} = create_mock_view(MockModule, #{}),
    _ = arizona_live:handle_event(LivePid, undefined, ~"update_name", #{
        ~"name" => ~"Arizona Framework"
    }),
    EventView = arizona_live:get_view(LivePid),
    {Diff, _DiffView} = arizona_differ:diff_stateful(MockModule, Bindings, EventView),
    ?assertEqual(
        [
            {1, ~"Arizona Framework"},
            {2, [{1, ~"Arizona Framework"}]}
        ],
        Diff
    ).

diff_stateful_with_stateless_changes_test(Config) when is_list(Config) ->
    ct:comment(
        "diff_stateful/3 should return diff when stateteless bindings changed and dependencies exist"
    ),
    {mock_stateful_module, MockModule} = proplists:lookup(mock_stateful_module, Config),
    {LivePid, Bindings, _MockView} = create_mock_view(MockModule, #{
        bindings => #{stateless_title => ~"Arizona"}
    }),
    _ = arizona_live:handle_event(LivePid, undefined, ~"update_stateless_title", #{
        ~"title" => ~"Arizona Framework"
    }),
    EventView = arizona_live:get_view(LivePid),
    {Diff, _DiffView} = arizona_differ:diff_stateful(MockModule, Bindings, EventView),
    ?assertEqual([{2, [{1, ~"Arizona Framework"}]}], Diff).

diff_stateless_test(Config) when is_list(Config) ->
    ct:comment("diff_stateless/4 should re-render entire stateless component"),
    {mock_stateful_module, MockStatefulModule} = proplists:lookup(mock_stateful_module, Config),
    {mock_stateless_module, MockStatelessModule} = proplists:lookup(mock_stateless_module, Config),
    {mock_stateless_render_fun, MockStatelessRenderFun} = proplists:lookup(
        mock_stateless_render_fun, Config
    ),
    {_LivePid, _Bindings, MockView} = create_mock_view(MockStatefulModule, #{}),
    Bindings = #{title => ~"Arizona"},
    {Html, _UpdatedView} = arizona_differ:diff_stateless(
        MockStatelessModule, MockStatelessRenderFun, Bindings, MockView
    ),
    ?assertEqual([{1, ~"Arizona"}], Html).

%% --------------------------------------------------------------------
%% Mock helpers
%% --------------------------------------------------------------------

create_mock_view(MockModule, ReqOpts) ->
    ok = arizona_tracker_dict:new(),
    {ok, LivePid} = arizona_live:start_link(),
    ArizonaRequest = arizona_request:new(foo, undefined, ReqOpts),
    ViewState = arizona_view:call_mount_callback(MockModule, ArizonaRequest),
    View = arizona_view:new(ViewState, hierarchical, LivePid),
    Bindings = arizona_stateful:get_bindings(ViewState),
    {_HierarchicalStructure, RenderView} = arizona_hierarchical:hierarchical_stateful(
        MockModule, Bindings, View
    ),
    DiffModeView = arizona_view:set_render_mode(diff, RenderView),
    ok = arizona_live:set_view(LivePid, DiffModeView),
    {LivePid, Bindings, DiffModeView}.
