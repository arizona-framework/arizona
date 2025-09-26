-module(arizona_differ_SUITE).
-behaviour(ct_suite).
-include_lib("stdlib/include/assert.hrl").
-compile([export_all, nowarn_export_all]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------

-define(RAND_MODULE_NAME,
    binary_to_atom(<<
        ?MODULE_STRING,
        "_",
        (atom_to_binary(?FUNCTION_NAME))/binary,
        "_",
        (integer_to_binary(?LINE))/binary,
        "_",
        (integer_to_binary(erlang:unique_integer([positive])))/binary
    >>)
).

%% --------------------------------------------------------------------
%% Ignore elvis warnings
%% --------------------------------------------------------------------

-elvis([
    {elvis_style, no_macros, #{
        allow => [
            'RAND_MODULE_NAME',
            'assert',
            'assertEqual',
            'assertMatch'
        ]
    }}
]).

%% --------------------------------------------------------------------
%% Behaviour (ct_suite) callbacks
%% --------------------------------------------------------------------

all() ->
    [
        {group, diff_tests},
        {group, tracker_context_tests}
    ].

groups() ->
    [
        {diff_tests, [parallel], [
            diff_view_without_changes,
            diff_view_with_changes,
            diff_stateful_fingerprint_match_with_changes,
            diff_stateful_fingerprint_match_no_changes,
            diff_stateful_fingerprint_mismatch,
            diff_root_stateful_with_changes,
            diff_stateless_fingerprint_match,
            diff_stateless_fingerprint_mismatch,
            diff_list_fingerprint_match,
            diff_list_fingerprint_mismatch,
            diff_map_fingerprint_match,
            diff_map_fingerprint_mismatch
        ]},
        {tracker_context_tests, [parallel], [
            tracker_context_restoration_fix
        ]}
    ].

%% --------------------------------------------------------------------
%% Diff tests
%% --------------------------------------------------------------------

diff_view_without_changes(Config) when is_list(Config) ->
    ct:comment("diff_view should return empty diff when view has no changes"),
    {Module, _Id, _StatefulModule, _StatefulId, _StatefulElementIndex, _StatelessModule,
        _StatelessFunction, _StatelessElementIndex} = mock_modules(
        ?RAND_MODULE_NAME, ?RAND_MODULE_NAME, ?RAND_MODULE_NAME
    ),
    View = mount_view(Module, #{}),
    {Diff, _DiffView} = arizona_differ:diff_view(View),
    ?assertEqual([], Diff).

diff_view_with_changes(Config) when is_list(Config) ->
    ct:comment("diff_view should return diff when view state changes"),
    {Module, _Id, _StatefulModule, _StatefulId, _StatefulElementIndex, _StatelessModule,
        _StatelessFunction, _StatelessElementIndex} = mock_modules(
        ?RAND_MODULE_NAME, ?RAND_MODULE_NAME, ?RAND_MODULE_NAME
    ),
    View = mount_view(Module, #{}),
    State = arizona_view:get_state(View),
    UpdatedState = arizona_stateful:put_binding(title, ~"Arizona Framework", State),
    UpdatedView = arizona_view:update_state(UpdatedState, View),
    {Diff, _DiffView} = arizona_differ:diff_view(UpdatedView),
    ?assertMatch(
        [
            {2, ~"Arizona Framework"},
            {3, [{2, ~"Arizona Framework"}, {3, [{1, ~"Arizona Framework"}, {2, ~""}]}]}
        ],
        Diff
    ).

diff_stateful_fingerprint_match_with_changes(Config) when is_list(Config) ->
    ct:comment("diff_stateful should return diff when fingerprint matches and bindings change"),
    {ViewModule, ViewId, StatefulModule, StatefulId, StatefulElementIndex, _StatelessModule,
        _StatelessFunction, _StatelessElementIndex} = mock_modules(
        ?RAND_MODULE_NAME, ?RAND_MODULE_NAME, ?RAND_MODULE_NAME
    ),
    View = mount_view(ViewModule, #{}),

    % Get the stateful component and change its state
    StatefulState = arizona_view:get_stateful_state(StatefulId, View),
    UpdatedState = arizona_stateful:put_binding(title, ~"Updated Title", StatefulState),
    UpdatedView = arizona_view:put_stateful_state(StatefulId, UpdatedState, View),
    UpdatedBindings = arizona_stateful:get_bindings(UpdatedState),
    DiffBindings = arizona_binder:to_map(UpdatedBindings),

    % Test diff_stateful/5 with fingerprint match and changes
    {Result, _DiffView} = arizona_differ:diff_stateful(
        StatefulModule, DiffBindings, ViewId, StatefulElementIndex, UpdatedView
    ),

    % Should return a diff (not nodiff) since bindings changed
    ?assertMatch([{2, ~"Updated Title"}, {3, [{1, ~"Updated Title"}, {2, ~""}]}], Result).

diff_stateful_fingerprint_match_no_changes(Config) when is_list(Config) ->
    ct:comment(
        "diff_stateful should return nodiff when fingerprint matches and bindings unchanged"
    ),
    {ViewModule, ViewId, StatefulModule, StatefulId, StatefulElementIndex, _StatelessModule,
        _StatelessFunction, _StatelessElementIndex} = mock_modules(
        ?RAND_MODULE_NAME, ?RAND_MODULE_NAME, ?RAND_MODULE_NAME
    ),
    View = mount_view(ViewModule, #{}),

    % Get the stateful component without changing its state
    StatefulState = arizona_view:get_stateful_state(StatefulId, View),
    Bindings = arizona_stateful:get_bindings(StatefulState),
    DiffBindings = arizona_binder:to_map(Bindings),

    % Test diff_stateful/5 with fingerprint match but no changes
    {Result, _DiffView} = arizona_differ:diff_stateful(
        StatefulModule, DiffBindings, ViewId, StatefulElementIndex, View
    ),

    % Should return nodiff since no bindings changed
    ?assertEqual(nodiff, Result).

diff_stateful_fingerprint_mismatch(Config) when is_list(Config) ->
    ct:comment("diff_stateful should return hierarchical struct when fingerprint mismatch"),
    {ViewModule, ViewId, StatefulModule, StatefulId, StatefulElementIndex, _StatelessModule,
        _StatelessFunction, _StatelessElementIndex} = mock_modules(
        ?RAND_MODULE_NAME, ?RAND_MODULE_NAME, ?RAND_MODULE_NAME
    ),
    % Mount view with show_stateful = false, which creates a template without
    % the stateful component. This establishes a fingerprint for StatefulElementIndex
    % that doesn't include the stateful component
    View1 = mount_view(ViewModule, #{show_stateful => false}),

    % Create bindings for the stateful component we want to diff
    Bindings = #{
        id => StatefulId,
        title => ~"Arizona"
    },

    % Test diff_stateful/5 with view template that has different fingerprint
    % Since the view was mounted with show_stateful=false, the template fingerprint
    % at element index won't match what diff_stateful expects for a stateful component,
    % causing fallback to hierarchical
    {Result, _DiffView} = arizona_differ:diff_stateful(
        StatefulModule, Bindings, ViewId, StatefulElementIndex, View1
    ),

    % Should return a hierarchical struct (not a diff) since fingerprint doesn't match
    ?assertEqual(#{type => stateful, id => StatefulId}, Result).

diff_root_stateful_with_changes(Config) when is_list(Config) ->
    ct:comment("diff_root_stateful should return diff when root stateful component changes"),
    {ViewModule, _ViewId, StatefulModule, StatefulId, _StatefulElementIndex, _StatelessModule,
        _StatelessFunction, _StatelessElementIndex} = mock_modules(
        ?RAND_MODULE_NAME, ?RAND_MODULE_NAME, ?RAND_MODULE_NAME
    ),
    View = mount_view(ViewModule, #{}),

    % Get the stateful component's current bindings and update them
    StatefulState = arizona_view:get_stateful_state(StatefulId, View),
    CurrentBindings = arizona_stateful:get_bindings(StatefulState),
    CurrentBindingsMap = arizona_binder:to_map(CurrentBindings),
    UpdatedBindings = CurrentBindingsMap#{title => ~"Root Updated Title"},

    % Test diff_root_stateful/3 with changed bindings (bypasses fingerprint checking)
    {Diff, _DiffView} = arizona_differ:diff_root_stateful(
        StatefulModule, UpdatedBindings, View
    ),

    % Should return a diff since bindings changed
    ?assertMatch([{2, ~"Root Updated Title"}, {3, [{1, ~"Root Updated Title"}, {2, ~""}]}], Diff).

diff_stateless_fingerprint_match(Config) when is_list(Config) ->
    ct:comment("diff_stateless should return diff when fingerprint matches"),
    {ViewModule, _ViewId, _StatefulModule, StatefulId, _StatefulElementIndex, StatelessModule,
        StatelessFunction, StatelessElementIndex} = mock_modules(
        ?RAND_MODULE_NAME, ?RAND_MODULE_NAME, ?RAND_MODULE_NAME
    ),
    View = mount_view(ViewModule, #{}),

    % Create bindings for the stateless component
    Bindings = #{title => ~"Stateless Title"},

    % Test diff_stateless/6 with fingerprint match
    {Result, _DiffView} = arizona_differ:diff_stateless(
        StatelessModule, StatelessFunction, Bindings, StatefulId, StatelessElementIndex, View
    ),

    % Should return a diff containing the stateless component's rendered content
    ?assertEqual([{1, ~"Stateless Title"}, {2, ~""}], Result).

diff_stateless_fingerprint_mismatch(Config) when is_list(Config) ->
    ct:comment("diff_stateless should return hierarchical struct when fingerprint mismatch"),
    {ViewModule, _ViewId, StatefulModule, StatefulId, _StatefulElementIndex, _StatelessModule,
        _StatelessFunction, _StatelessElementIndex} = mock_modules(
        ?RAND_MODULE_NAME, ?RAND_MODULE_NAME, ?RAND_MODULE_NAME
    ),
    % Mount view with show_stateless = false, creating a stateful template WITHOUT
    % stateless component. This establishes a fingerprint at element index 3 that
    % doesn't include the stateless component
    View = mount_view(ViewModule, #{show_stateless => false}),

    % Test diff_root_stateful with show_stateless = true, which creates a template
    % WITH stateless component. This creates a fingerprint mismatch: stored
    % fingerprint (no stateless) vs new template (with stateless)
    Bindings = #{id => StatefulId, title => ~"Arizona", show_stateless => true},
    {Diff, _DiffView} = arizona_differ:diff_root_stateful(
        StatefulModule, Bindings, View
    ),

    % Should return hierarchical stateless struct (not a diff) due to fingerprint mismatch
    % When diff_stateless detects fingerprint mismatch, it falls back to hierarchical rendering
    ?assertEqual(
        [
            {3, #{
                type => stateless,
                dynamic => [~"Arizona", ~""],
                static => [~"<h1>", ~"</h1>\n"]
            }}
        ],
        Diff
    ).

diff_list_fingerprint_match(Config) when is_list(Config) ->
    ct:comment("diff_list should return diff when fingerprint matches and list items change"),
    {ViewModule, _ViewId, _StatefulModule, StatefulId, _StatefulElementIndex, StatelessModule,
        StatelessFunction, StatelessElementIndex} = mock_modules(
        ?RAND_MODULE_NAME, ?RAND_MODULE_NAME, ?RAND_MODULE_NAME
    ),
    View = mount_view(ViewModule, #{stateless_items => [~"Item 1", ~"Item 2"]}),

    % Test diff_stateless first to render the stateless component with list items
    % This will create the list template through arizona_template:render_list
    Bindings = #{title => ~"Arizona", items => [~"Item 1", ~"Item 2", ~"Item 3"]},
    {Diff, _ViewWithList} = arizona_differ:diff_stateless(
        StatelessModule, StatelessFunction, Bindings, StatefulId, StatelessElementIndex, View
    ),

    % Should return a list of rendered HTML for each item
    ?assertEqual(
        [
            {1, ~"Arizona"},
            {2, [[~"Item 1"], [~"Item 2"], [~"Item 3"]]}
        ],
        Diff
    ).

diff_list_fingerprint_mismatch(Config) when is_list(Config) ->
    ct:comment("diff_list should return hierarchical struct when fingerprint mismatch"),
    {ViewModule, _ViewId, _StatefulModule, StatefulId, _StatefulElementIndex, StatelessModule,
        StatelessFunction, StatelessElementIndex} = mock_modules(
        ?RAND_MODULE_NAME, ?RAND_MODULE_NAME, ?RAND_MODULE_NAME
    ),
    % Mount view WITHOUT list items, creating different template fingerprint
    % This establishes a fingerprint for StatelessElementIndex without list rendering
    View = mount_view(ViewModule, #{stateless_items => []}),

    % Create bindings with list items for the stateless component
    % This will create a template with list rendering, different from stored fingerprint
    Bindings = #{title => ~"Arizona", items => [~"Item 1", ~"Item 2", ~"Item 3"]},
    {Diff, _ViewWithList} = arizona_differ:diff_stateless(
        StatelessModule, StatelessFunction, Bindings, StatefulId, StatelessElementIndex, View
    ),

    % Should return hierarchical list struct due to fingerprint mismatch
    % When diff_list detects mismatch, it falls back to hierarchical rendering
    ?assertEqual(
        [
            {1, ~"Arizona"},
            {2, #{
                type => list,
                dynamic => [[~"Item 1"], [~"Item 2"], [~"Item 3"]],
                static => [~"<li>", ~"</li>"]
            }}
        ],
        Diff
    ).

diff_map_fingerprint_match(Config) when is_list(Config) ->
    ct:comment("diff_map should return diff when fingerprint matches"),

    % Create a proper map template using parse transform like the renderer test does
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

    % Create a mock view to provide context
    ViewModule = ?RAND_MODULE_NAME,
    View = create_simple_view(ViewModule, ~"map_test"),

    % Test diff_map directly - first establish fingerprint by setting it
    ParentId = ~"parent",
    ElementIndex = 1,
    Fingerprint = arizona_template:get_fingerprint(Template),
    ViewWithFingerprint = arizona_view:put_fingerprint(
        ParentId, ElementIndex, Fingerprint, stateless, View
    ),

    % Now call diff_map - should return diff since fingerprints match
    {Diff, _UpdatedView} = arizona_differ:diff_map(
        Template, Map, ParentId, ElementIndex, ViewWithFingerprint
    ),

    % Should return list of dynamic content (not hierarchical struct)
    ?assertEqual(2, length(Diff)),

    % Check that expected dynamic content is present (map order may vary)
    ExpectedItems = [
        [~"first", ~"1"],
        [~"second", ~"2"]
    ],
    lists:foreach(
        fun(ExpectedItem) ->
            ?assert(lists:member(ExpectedItem, Diff))
        end,
        ExpectedItems
    ).

diff_map_fingerprint_mismatch(Config) when is_list(Config) ->
    ct:comment("diff_map should return hierarchical struct when fingerprint mismatch"),

    % Create a proper map template using parse transform
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

    % Create mock view and set a DIFFERENT fingerprint to force mismatch
    ViewModule = ?RAND_MODULE_NAME,
    View = create_simple_view(ViewModule, ~"map_mismatch_test"),

    ParentId = ~"parent",
    ElementIndex = 1,
    % Different from template's actual fingerprint
    WrongFingerprint = 12345,
    ViewWithWrongFingerprint = arizona_view:put_fingerprint(
        ParentId, ElementIndex, WrongFingerprint, stateless, View
    ),

    % Call diff_map - should return hierarchical struct due to fingerprint mismatch
    {HierarchicalStruct, _UpdatedView} = arizona_differ:diff_map(
        Template, Map, ParentId, ElementIndex, ViewWithWrongFingerprint
    ),

    % Should return hierarchical list struct (not a diff list)
    ?assertEqual(list, maps:get(type, HierarchicalStruct)),
    ?assertEqual([~"<li>Key: ", ~", Value: ", ~"</li>"], maps:get(static, HierarchicalStruct)),
    ?assertEqual(2, length(maps:get(dynamic, HierarchicalStruct))).

%% --------------------------------------------------------------------
%% Tracker context tests
%% --------------------------------------------------------------------

tracker_context_restoration_fix(Config) when is_list(Config) ->
    ct:comment("Variables after stateful components should be tracked correctly"),

    % Template layout: title -> render_stateful -> footer
    % Tests that 'footer' variable (positioned after stateful component) is tracked
    {Module, _Id, _StatefulModule, _StatefulId, _StatefulElementIndex, _StatelessModule,
        _StatelessFunction, _StatelessElementIndex} = mock_modules(
        ?RAND_MODULE_NAME, ?RAND_MODULE_NAME, ?RAND_MODULE_NAME
    ),

    % Initial mount with footer having default empty value
    View = mount_view(Module, #{}),
    State = arizona_view:get_state(View),

    % Change footer variable - this tests tracker context restoration
    % If context isn't restored after processing stateful component,
    % footer dependency won't be tracked and no diff will be generated
    UpdatedState = arizona_stateful:put_binding(footer, ~"Changed footer", State),
    ViewWithChanges = arizona_view:update_state(UpdatedState, View),

    % Generate diff - should include footer change if tracking works correctly
    {Diff, _UpdatedView} = arizona_differ:diff_view(ViewWithChanges),

    % Verify footer change was tracked and included in diff
    ?assert(length(Diff) > 0),
    DiffStr = lists:flatten(io_lib:format("~p", [Diff])),
    ?assert(string:find(DiffStr, "Changed footer") =/= nomatch).

%% --------------------------------------------------------------------
%% Helper functions
%% --------------------------------------------------------------------

mock_modules(ViewModule, StatefulModule, StatelessModule) ->
    ViewId = ~"view",
    StatefulId = ~"stateful",
    StatelessRenderFun = render,
    {ViewModule, StatefulElementIndex} =
        mock_view_module(ViewModule, ViewId, StatefulModule, StatefulId, StatelessModule),
    {StatefulModule, StatelessElementIndex} = mock_stateful_module(
        StatefulModule, StatelessModule, StatelessRenderFun
    ),
    {StatelessModule, StatelessRenderFun} = mock_stateless_module(
        StatelessModule, StatelessRenderFun
    ),
    {ViewModule, ViewId, StatefulModule, StatefulId, StatefulElementIndex, StatelessModule,
        StatelessRenderFun, StatelessElementIndex}.

mock_view_module(ViewModule, ViewId, StatefulModule, StatefulId, StatelessModule) ->
    maybe
        % Element index where stateful component is rendered in template
        StatefulElementIndex = 3,
        {ok, _} ?=
            merl:compile_and_load(
                merl:qquote(~""""
                -module('@view_module').
                -behaviour(arizona_view).
                -compile({parse_transform, arizona_parse_transform}).
                -export([mount/2]).
                -export([render/1]).

                mount(_Arg, Req) ->
                    {ReqBindings, _Req1} = arizona_request:get_bindings(Req),
                    arizona_view:new('@view_module', maps:merge(#{
                        id => ~"'@view_id",
                        stateful_id => ~"'@stateful_id",
                        title => ~"Arizona"
                    }, ReqBindings), none).

                render(Bindings) ->
                    StatefulModule = '@stateful_module',
                    arizona_template:from_html(~"""
                    <div {arizona_template:get_binding(id, Bindings)}>
                        {arizona_template:get_binding(title, Bindings)}
                        {case arizona_template:get_binding(show_stateful, Bindings, true) of
                            true ->
                                arizona_template:render_stateful(StatefulModule, #{
                                    id => arizona_template:get_binding(stateful_id, Bindings),
                                    title => arizona_template:get_binding(title, Bindings),
                                    show_stateless => arizona_template:get_binding(
                                        show_stateless, Bindings, true
                                    ),
                                    stateless_items => arizona_template:get_binding(
                                        stateless_items, Bindings, []
                                    )
                                });
                            false ->
                                ~""
                        end})
                        {arizona_template:get_binding(footer, Bindings, ~"")}
                    </div>
                    """).
                """", [
                    {view_module, merl:term(ViewModule)},
                    {view_id, merl:term(binary_to_list(ViewId))},
                    {stateful_module, merl:term(StatefulModule)},
                    {stateful_id, merl:term(binary_to_list(StatefulId))}
                ])
            ),
        {ViewModule, StatefulElementIndex}
    else
        Error ->
            error(Error, [ViewModule, ViewId, StatefulModule, StatefulId, StatelessModule])
    end.

mock_stateful_module(StatefulModule, StatelessModule, StatelessFun) ->
    maybe
        {ok, _} ?=
            merl:compile_and_load(
                merl:qquote(~""""
                -module('@stateful_module').
                -behaviour(arizona_stateful).
                -compile({parse_transform, arizona_parse_transform}).
                -export([mount/1]).
                -export([render/1]).

                mount(Bindings) ->
                    arizona_stateful:new('@stateful_module', Bindings).

                render(Bindings) ->
                    StatelessModule = '@stateless_module',
                    StatelessFun = '@stateless_fun',
                    arizona_template:from_html(~"""
                    <div {arizona_template:get_binding(id, Bindings)}>
                        {arizona_template:get_binding(title, Bindings)}
                        {case arizona_template:get_binding(
                            show_stateless, Bindings, true
                        ) of
                            true ->
                                arizona_template:render_stateless(StatelessModule, StatelessFun, #{
                                    title => arizona_template:get_binding(title, Bindings),
                                    items => arizona_template:get_binding(
                                        stateless_items, Bindings, []
                                    )
                                });
                            false ->
                                ~""
                        end})
                    </div>
                    """).
                """", [
                    {stateful_module, merl:term(StatefulModule)},
                    {stateless_module, merl:term(StatelessModule)},
                    {stateless_fun, merl:term(StatelessFun)}
                ])
            ),
        % Stateless component is at element 3 within stateful template
        StatelessElementIndex = 3,
        {StatefulModule, StatelessElementIndex}
    else
        Error ->
            error(Error, [StatefulModule, StatelessModule, StatelessFun])
    end.

mock_stateless_module(Module, RenderFun) ->
    maybe
        {ok, _} ?=
            merl:compile_and_load(
                merl:qquote(~"""""
                -module('@module').
                -compile({parse_transform, arizona_parse_transform}).
                -export(['@render_fun'/1]).

                '@render_fun'(Bindings) ->
                    arizona_template:from_html(~""""
                    <h1>{arizona_template:get_binding(title, Bindings)}</h1>
                    {case arizona_template:get_binding(items, Bindings, []) of
                        [] ->
                            ~"";
                        Items ->
                            arizona_template:render_list(fun(Item) ->
                                arizona_template:from_html(~"""
                                <li>{Item}</li>
                                """)
                            end, Items)
                    end}
                    """").
                """"", [
                    {module, merl:term(Module)},
                    {render_fun, merl:term(RenderFun)}
                ])
            ),
        {Module, RenderFun}
    else
        Error ->
            error(Error, [Module, RenderFun])
    end.

mount_view(Module, Bindings) ->
    % Init process dictionaries
    undefined = arizona_tracker_dict:set_tracker(arizona_tracker:new()),
    undefined = arizona_hierarchical_dict:set_structure(#{}),

    % Hierarchical struct is required for diffing
    ArizonaRequest = arizona_request:new(?MODULE, undefined, #{
        bindings => Bindings
    }),
    View = arizona_view:call_mount_callback(Module, #{}, ArizonaRequest),
    {_Struct, HierarchicalView} = arizona_hierarchical:hierarchical_view(View),
    HierarchicalView.

create_simple_view(Module, Id) ->
    % Create a minimal view for testing without complex mock setup
    undefined = arizona_tracker_dict:set_tracker(arizona_tracker:new()),
    undefined = arizona_hierarchical_dict:set_structure(#{}),
    arizona_view:new(Module, #{id => Id}, none).
