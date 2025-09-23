-module(arizona_renderer_SUITE).
-behaviour(ct_suite).
-include_lib("stdlib/include/assert.hrl").
-compile([export_all, nowarn_export_all]).

%% --------------------------------------------------------------------
%% Behaviour (ct_suite) callbacks
%% --------------------------------------------------------------------

all() ->
    [
        {group, rendering_tests}
    ].

groups() ->
    [
        {rendering_tests, [parallel], [
            render_stateful_test,
            render_stateless_test,
            render_list_test,
            render_map_test,
            render_dynamic_test
        ]}
    ].

init_per_suite(Config) ->
    MockStatefulModule = arizona_renderer_stateful_mock,
    MockStatelessModule = arizona_renderer_stateless_mock,
    MockStatelessRenderFun = render_stateless,

    MockStatefulModuleCode = merl:qquote(~""""
    -module('@module').
    -behaviour(arizona_stateful).

    -export([mount/1]).
    -export([render/1]).

    mount(Bindings) ->
        arizona_stateful:new('@module', Bindings).

    render(_Bindings) ->
        arizona_template:from_html(~"""
        <h1>Stateful Template</h1>
        """).
    """", [{module, merl:term(MockStatefulModule)}]),

    MockStatelessModuleCode = merl:qquote(~""""
    -module('@module').

    -export(['@render_fun'/1]).

    '@render_fun'(_Bindings) ->
        arizona_template:from_html(~"""
        <div>Stateless Template</div>
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
%% Rendering tests
%% --------------------------------------------------------------------

render_stateful_test(Config) when is_list(Config) ->
    ct:comment("render_stateful/3 should render stateful component"),
    {mock_stateful_module, MockStatefulModule} = proplists:lookup(mock_stateful_module, Config),
    Bindings = #{id => ~"foo", title => ~"Test"},
    MockView = create_mock_view(MockStatefulModule, Bindings),
    {Html, _UpdatedView} = arizona_renderer:render_stateful(MockStatefulModule, Bindings, MockView),
    ?assertEqual([~"<h1>Stateful Template</h1>"], Html).

render_stateless_test(Config) when is_list(Config) ->
    ct:comment("render_stateless/4 should render stateless component"),
    {mock_stateful_module, MockStatefulModule} = proplists:lookup(mock_stateful_module, Config),
    {mock_stateless_module, MockStatelessModule} = proplists:lookup(mock_stateless_module, Config),
    {mock_stateless_render_fun, MockStatelessRenderFun} = proplists:lookup(
        mock_stateless_render_fun, Config
    ),
    Bindings = #{id => ~"foo", content => ~"Test Content"},
    MockView = create_mock_view(MockStatefulModule, Bindings),
    {Html, _UpdatedView} = arizona_renderer:render_stateless(
        MockStatelessModule, MockStatelessRenderFun, Bindings, ~"foo", MockView
    ),
    ?assertEqual([~"<div>Stateless Template</div>"], Html).

render_list_test(Config) when is_list(Config) ->
    ct:comment("render_list should transform to optimized template structure"),
    {mock_stateful_module, MockStatefulModule} = proplists:lookup(mock_stateful_module, Config),
    MockView = create_mock_view(MockStatefulModule, #{id => ~"list"}),

    % Test the parse transform by using render_list in template
    TemplateAST = arizona_parse_transform:extract_callback_function_body(
        ?MODULE, ?LINE, merl:quote(~""""
        fun(Item) ->
            arizona_template:from_html(~"""
            <li>{Item}</li>
            """)
        end
        """"), []
    ),
    {value, Template, #{}} = erl_eval:expr(erl_syntax:revert(TemplateAST), #{}),

    List = [~"first", ~"second", ~"third"],
    {Html, _UpdatedView} = arizona_renderer:render_list(Template, List, ~"list", MockView),

    % The result should contain the list items
    ExpectedHtml = [
        [~"<li>", ~"first", ~"</li>"],
        [~"<li>", ~"second", ~"</li>"],
        [~"<li>", ~"third", ~"</li>"]
    ],
    ?assertEqual(ExpectedHtml, Html).

render_map_test(Config) when is_list(Config) ->
    ct:comment("render_map should transform to optimized template structure"),
    {mock_stateful_module, MockStatefulModule} = proplists:lookup(mock_stateful_module, Config),
    MockView = create_mock_view(MockStatefulModule, #{id => ~"map"}),

    % Test the parse transform by using render_map in template
    TemplateAST = arizona_parse_transform:extract_callback_function_body(
        ?MODULE, ?LINE, merl:quote(~""""
        fun({Key, Value}) ->
            arizona_template:from_html(~"""
            <li>Key: {Key}, Value: {Value}</li>
            """)
        end
        """"), []
    ),
    {value, Template, #{}} = erl_eval:expr(erl_syntax:revert(TemplateAST), #{}),

    Map = #{~"first" => ~"1", ~"second" => ~"2"},
    {Html, _UpdatedView} = arizona_renderer:render_map(Template, Map, ~"map", MockView),

    % The result should contain the map entries (order may vary)
    ?assertEqual(2, length(Html)),
    % Check that both expected items are present
    ExpectedItems = [
        [~"<li>Key: ", ~"first", ~", Value: ", ~"1", ~"</li>"],
        [~"<li>Key: ", ~"second", ~", Value: ", ~"2", ~"</li>"]
    ],
    lists:foreach(
        fun(ExpectedItem) ->
            ?assert(lists:member(ExpectedItem, Html))
        end,
        ExpectedItems
    ).

render_dynamic_test(Config) when is_list(Config) ->
    ct:comment("render_dynamic/2 should render template dynamic parts"),
    Template = arizona_template:from_html(~"<span>Simple</span>"),
    {mock_stateful_module, MockStatefulModule} = proplists:lookup(mock_stateful_module, Config),
    MockView = create_mock_view(MockStatefulModule, #{id => ~"foo"}),
    DynamicSequence = arizona_template:get_dynamic_sequence(Template),
    Dynamic = arizona_template:get_dynamic(Template),
    {DynamicRender, _UpdatedView} = arizona_renderer:render_dynamic(
        DynamicSequence, Dynamic, ~"test_parent_id", MockView
    ),
    ?assertEqual([], DynamicRender).

%% --------------------------------------------------------------------
%% Mock helpers
%% --------------------------------------------------------------------

create_mock_view(MockModule, Bindings) ->
    % Create a minimal mock view that satisfies the arizona_view interface
    % This is a simplified version that returns the view itself for most operations
    arizona_view:new(MockModule, Bindings, none).
