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
            diff_view_no_changes_test,
            diff_view_with_changes_test,
            diff_stateful_no_changes_test,
            diff_stateful_with_changes_test,
            diff_stateful_with_stateless_changes_test,
            diff_stateless_test
        ]}
    ].

init_per_suite(Config) ->
    MockViewModule = arizona_differ_view_mock,
    MockStatefulModule = arizona_differ_stateful_mock,
    MockStatelessModule = arizona_differ_stateless_mock,
    MockStatelessRenderFun = render_page,

    MockViewModuleCode = merl:qquote(~""""
    -module('@module').
    -behavior(arizona_view).
    -compile({parse_transform, arizona_parse_transform}).

    -export([mount/1]).
    -export([render/1]).

    mount(Req) ->
        {Bindings, _BindingsReq} = arizona_request:get_bindings(Req),
        arizona_view:new('@module', Bindings#{
            id => ~"view",
            name => ~"Arizona",
            stateful_name => ~"Arizona"
        }, none).

    render(Bindings) ->
        arizona_template:from_string(~"""
        <div id="{arizona_template:get_binding(id, Bindings)}">
            <div>View: {arizona_template:get_binding(name, Bindings)}</div>
            {arizona_template:render_stateful(arizona_differ_stateful_mock, #{
                name => arizona_template:get_binding(stateful_name, Bindings)
            })}
        </div>
        """).
    """", [{module, merl:term(MockViewModule)}]),

    MockStatefulModuleCode = merl:qquote(~""""
    -module('@module').
    -behavior(arizona_stateful).
    -compile({parse_transform, arizona_parse_transform}).

    -export([mount/1]).
    -export([render/1]).
    -export([handle_event/3]).

    mount(Bindings) ->
        arizona_stateful:new('@module', Bindings#{
            id => arizona_binder:get(id, Bindings, fun() -> ~"stateful" end)
        }).

    render(Bindings) ->
        arizona_template:from_string(~"""
        <div id="{arizona_template:get_binding(id, Bindings)}">
            <div>Stateful: {arizona_template:get_binding(name, Bindings)}</div>
            {arizona_template:render_stateless(arizona_differ_stateless_mock, render_page, #{
                title => arizona_template:get_binding(stateless_title, Bindings, fun() ->
                    arizona_template:get_binding(name, Bindings)
                end)
            })}
        </div>
        """).

    handle_event(~"update_name", #{~"name" := Name}, State) ->
        {noreply, arizona_stateful:put_binding(name, Name, State)};
    handle_event(~"update_stateless_title", #{~"title" := Title}, State) ->
        {noreply, arizona_stateful:put_binding(stateless_title, Title, State)}.
    """", [{module, merl:term(MockStatefulModule)}]),

    MockStatelessModuleCode = merl:qquote(~""""
    -module('@module').
    -compile({parse_transform, arizona_parse_transform}).

    -export(['@render_fun'/1]).

    '@render_fun'(Bindings) ->
        arizona_template:from_string(~"""
        <span>Stateless: {arizona_template:get_binding(title, Bindings)}</span>
        """).
    """", [
        {module, merl:term(MockStatelessModule)},
        {render_fun, merl:term(MockStatelessRenderFun)}
    ]),

    {ok, _ViewBinary} = merl:compile_and_load(MockViewModuleCode),
    {ok, _StatefulBinary} = merl:compile_and_load(MockStatefulModuleCode),
    {ok, _StatelessBinary} = merl:compile_and_load(MockStatelessModuleCode),

    [
        {mock_view_module, MockViewModule},
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

diff_view_no_changes_test(Config) when is_list(Config) ->
    ct:comment("diff_view/1 should return empty diff when no bindings changed"),
    {mock_view_module, ViewModule} = proplists:lookup(mock_view_module, Config),
    View = create_mock_view(ViewModule, #{}),
    {Diff, _DiffView} = arizona_differ:diff_view(View),
    ?assertEqual([], Diff).

diff_view_with_changes_test(Config) when is_list(Config) ->
    ct:comment("diff_view/1 should return diff when bindings changed and dependencies exist"),
    {mock_view_module, ViewModule} = proplists:lookup(mock_view_module, Config),
    View = create_mock_view(ViewModule, #{}),
    ViewState = arizona_view:get_state(View),
    UpdatedState = arizona_stateful:put_binding(name, ~"Arizona Framework", ViewState),
    UpdatedView = arizona_view:update_state(UpdatedState, View),
    {Diff, _DiffView} = arizona_differ:diff_view(UpdatedView),
    ?assertEqual([{2, ~"Arizona Framework"}], Diff).

diff_stateful_no_changes_test(Config) when is_list(Config) ->
    ct:comment("diff_stateful/3 should return empty diff when no bindings changed"),
    {mock_view_module, ViewModule} = proplists:lookup(mock_view_module, Config),
    {mock_stateful_module, StatefulModule} = proplists:lookup(mock_stateful_module, Config),
    View = create_mock_view(ViewModule, #{}),
    Bindings = #{},
    {Diff, _DiffView} = arizona_differ:diff_stateful(StatefulModule, Bindings, View),
    ?assertEqual([], Diff).

diff_stateful_with_changes_test(Config) when is_list(Config) ->
    ct:comment("diff_stateful/3 should return diff when bindings changed and dependencies exist"),
    {mock_view_module, ViewModule} = proplists:lookup(mock_view_module, Config),
    {mock_stateful_module, StatefulModule} = proplists:lookup(mock_stateful_module, Config),
    View = create_mock_view(ViewModule, #{}),
    Id = ~"stateful",
    State = arizona_view:get_stateful_state(Id, View),
    Bindings = arizona_stateful:get_bindings(State),
    {noreply, UpdatedState} = arizona_stateful:call_handle_event_callback(
        ~"update_name",
        #{
            ~"name" => ~"Arizona Framework"
        },
        State
    ),
    UpdatedView = arizona_view:put_stateful_state(Id, UpdatedState, View),
    {Diff, _DiffView} = arizona_differ:diff_stateful(StatefulModule, Bindings, UpdatedView),
    ?assertEqual(
        [
            {2, ~"Arizona Framework"},
            {3, [{1, ~"Arizona Framework"}]}
        ],
        Diff
    ).

diff_stateful_with_stateless_changes_test(Config) when is_list(Config) ->
    ct:comment(
        "diff_stateful/3 should return diff when stateteless bindings changed and dependencies exist"
    ),
    {mock_view_module, ViewModule} = proplists:lookup(mock_view_module, Config),
    {mock_stateful_module, StatefulModule} = proplists:lookup(mock_stateful_module, Config),
    View = create_mock_view(ViewModule, #{}),
    Id = ~"stateful",
    State = arizona_view:get_stateful_state(Id, View),
    Bindings = arizona_stateful:get_bindings(State),
    {noreply, UpdatedState} = arizona_stateful:call_handle_event_callback(
        ~"update_stateless_title",
        #{
            ~"title" => ~"Arizona Framework"
        },
        State
    ),
    UpdatedView = arizona_view:put_stateful_state(Id, UpdatedState, View),
    {Diff, _DiffView} = arizona_differ:diff_stateful(StatefulModule, Bindings, UpdatedView),
    ?assertEqual([{3, [{1, ~"Arizona Framework"}]}], Diff).

diff_stateless_test(Config) when is_list(Config) ->
    ct:comment("diff_stateless/4 should re-render entire stateless component"),
    {mock_view_module, ViewModule} = proplists:lookup(mock_view_module, Config),
    {mock_stateless_module, StatelessModule} = proplists:lookup(mock_stateless_module, Config),
    {mock_stateless_render_fun, StatelessRenderFun} = proplists:lookup(
        mock_stateless_render_fun, Config
    ),
    View = create_mock_view(ViewModule, #{}),
    Bindings = #{title => ~"Arizona"},
    {Html, _UpdatedView} = arizona_differ:diff_stateless(
        StatelessModule, StatelessRenderFun, Bindings, View
    ),
    ?assertEqual([{1, ~"Arizona"}], Html).

%% --------------------------------------------------------------------
%% Mock helpers
%% --------------------------------------------------------------------

create_mock_view(Module, ReqOpts) ->
    undefined = arizona_tracker_dict:set_tracker(arizona_tracker:new()),
    undefined = arizona_hierarchical_dict:set_structure(#{}),
    ArizonaRequest = arizona_request:new(?MODULE, undefined, ReqOpts),
    View = arizona_view:call_mount_callback(Module, ArizonaRequest),
    {_HierarchicalStruct, HierarchicalView} = arizona_hierarchical:hierarchical_view(View),
    HierarchicalView.
