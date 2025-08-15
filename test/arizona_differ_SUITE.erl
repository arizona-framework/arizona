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
%% Behaviour (ct_suite) callbacks
%% --------------------------------------------------------------------

all() ->
    [
        {group, diff_tests}
    ].

groups() ->
    [
        {diff_tests, [parallel], [
            diff_view_without_changes,
            diff_view_with_changes,
            diff_stateful_fingerprint_match_with_changes,
            diff_stateful_fingerprint_match_no_changes
        ]}
    ].

%% --------------------------------------------------------------------
%% Diff tests
%% --------------------------------------------------------------------

diff_view_without_changes(Config) when is_list(Config) ->
    {ok, Module, _Id, _StatefulModule, _StatefulId} = mock_view_module(
        ?RAND_MODULE_NAME, ?RAND_MODULE_NAME, ?RAND_MODULE_NAME
    ),
    View = mount_view(Module),
    {Diff, _DiffView} = arizona_differ:diff_view(View),
    ?assertEqual([], Diff).

diff_view_with_changes(Config) when is_list(Config) ->
    {ok, Module, _Id, _StatefulModule, _StatefulId} = mock_view_module(
        ?RAND_MODULE_NAME, ?RAND_MODULE_NAME, ?RAND_MODULE_NAME
    ),
    View = mount_view(Module),
    State = arizona_view:get_state(View),
    UpdatedState = arizona_stateful:put_binding(title, ~"Arizona Framework", State),
    UpdatedView = arizona_view:update_state(UpdatedState, View),
    {Diff, _DiffView} = arizona_differ:diff_view(UpdatedView),
    ?assertEqual(
        [
            {2, ~"Arizona Framework"},
            {3, [{2, ~"Arizona Framework"}, {3, [{1, ~"Arizona Framework"}]}]}
        ],
        Diff
    ).

diff_stateful_fingerprint_match_with_changes(Config) when is_list(Config) ->
    {ok, _ViewModule, ViewId, StatefulModule, StatefulId} = mock_view_module(
        ?RAND_MODULE_NAME, ?RAND_MODULE_NAME, ?RAND_MODULE_NAME
    ),
    View = mount_view(_ViewModule),

    % Get the stateful component and change its state
    StatefulState = arizona_view:get_stateful_state(StatefulId, View),
    UpdatedState = arizona_stateful:put_binding(title, ~"Updated Title", StatefulState),
    UpdatedView = arizona_view:put_stateful_state(StatefulId, UpdatedState, View),
    UpdatedBindings = arizona_stateful:get_bindings(UpdatedState),

    % Test diff_stateful/5 with fingerprint match and changes
    {Result, _DiffView} = arizona_differ:diff_stateful(
        StatefulModule, UpdatedBindings, ViewId, 3, UpdatedView
    ),

    % Should return a diff (not nodiff) since bindings changed
    ?assertMatch([{2, ~"Updated Title"}, {3, [{1, ~"Updated Title"}]}], Result).

diff_stateful_fingerprint_match_no_changes(Config) when is_list(Config) ->
    {ok, _ViewModule, ViewId, StatefulModule, StatefulId} = mock_view_module(
        ?RAND_MODULE_NAME, ?RAND_MODULE_NAME, ?RAND_MODULE_NAME
    ),
    View = mount_view(_ViewModule),

    % Get the stateful component without changing its state
    StatefulState = arizona_view:get_stateful_state(StatefulId, View),
    Bindings = arizona_stateful:get_bindings(StatefulState),

    % Test diff_stateful/5 with fingerprint match but no changes
    {Result, _DiffView} = arizona_differ:diff_stateful(
        StatefulModule, Bindings, ViewId, 3, View
    ),

    % Should return nodiff since no bindings changed
    ?assertEqual(nodiff, Result).

%% --------------------------------------------------------------------
%% Helper functions
%% --------------------------------------------------------------------

mock_view_module(ViewModule, StatefulModule, StatelessModule) ->
    maybe
        ViewId = ~"view",
        StatefulId = ~"stateful",
        {ok, _} ?=
            merl:compile_and_load(
                merl:qquote(~""""
                -module('@view_module').
                -behaviour(arizona_view).
                -compile({parse_transform, arizona_parse_transform}).
                -export([mount/1]).
                -export([render/1]).

                mount(_Req) ->
                    arizona_view:new('@view_module', #{
                        id => ~"'@view_id",
                        stateful_id => ~"'@stateful_id",
                        title => ~"Arizona"
                    }, none).

                render(Bindings) ->
                    StatefulModule = '@stateful_module',
                    arizona_template:from_string(~"""
                    <div {arizona_template:get_binding(id, Bindings)}>
                        {arizona_template:get_binding(title, Bindings)}
                        {arizona_template:render_stateful(StatefulModule, #{
                            id => arizona_template:get_binding(stateful_id, Bindings),
                            title => arizona_template:get_binding(title, Bindings)
                        })}
                    </div>
                    """).
                """", [
                    {view_module, merl:term(ViewModule)},
                    {view_id, merl:term(binary_to_list(ViewId))},
                    {stateful_module, merl:term(StatefulModule)},
                    {stateful_id, merl:term(binary_to_list(StatefulId))}
                ])
            ),
        {ok, StatefulModule, StatelessModule} ?=
            mock_stateful_module(StatefulModule, StatelessModule, render),
        {ok, ViewModule, ViewId, StatefulModule, StatefulId}
    else
        error ->
            error(ViewModule);
        {error, Reason} ->
            error({ViewModule, Reason})
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
                    arizona_template:from_string(~"""
                    <div {arizona_template:get_binding(id, Bindings)}>
                        {arizona_template:get_binding(title, Bindings)}
                        {arizona_template:render_stateless(StatelessModule, StatelessFun, #{
                            title => arizona_template:get_binding(title, Bindings)
                        })}
                    </div>
                    """).
                """", [
                    {stateful_module, merl:term(StatefulModule)},
                    {stateless_module, merl:term(StatelessModule)},
                    {stateless_fun, merl:term(StatelessFun)}
                ])
            ),
        {ok, StatelessModule} ?= mock_stateless_module(StatelessModule, StatelessFun),
        {ok, StatefulModule, StatelessModule}
    else
        error ->
            error(StatefulModule);
        {error, Reason} ->
            error({StatefulModule, Reason})
    end.

mock_stateless_module(Module, RenderFun) ->
    maybe
        {ok, _} ?=
            merl:compile_and_load(
                merl:qquote(~""""
                -module('@module').
                -compile({parse_transform, arizona_parse_transform}).
                -export(['@render_fun'/1]).

                '@render_fun'(Bindings) ->
                    arizona_template:from_string(~"""
                    <h1>{arizona_template:get_binding(title, Bindings)}</h1>
                    """).
                """", [
                    {module, merl:term(Module)},
                    {render_fun, merl:term(RenderFun)}
                ])
            ),
        {ok, Module}
    else
        error ->
            error(Module);
        {error, Reason} ->
            error({Module, Reason})
    end.

mount_view(Module) ->
    % Init process dictionaries
    undefined = arizona_tracker_dict:set_tracker(arizona_tracker:new()),
    undefined = arizona_hierarchical_dict:set_structure(#{}),

    % Hierarchical struct is required for diffing
    ArizonaRequest = arizona_request:new(?MODULE, undefined, #{}),
    View = arizona_view:call_mount_callback(Module, ArizonaRequest),
    {_Struct, HierarchicalView} = arizona_hierarchical:hierarchical_view(View),
    HierarchicalView.
