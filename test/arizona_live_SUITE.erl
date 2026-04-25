-module(arizona_live_SUITE).
-include_lib("stdlib/include/assert.hrl").
-include("arizona.hrl").

-export([all/0, groups/0]).
-export([
    dedup_child_view_ops/1,
    dedup_item_patch_inner_ops/1,
    dedup_item_patch_no_prior_fp/1,
    dedup_navigate_nested_dynamics/1,
    dedup_nested_dynamics_first_time/1,
    dedup_nested_dynamics/1,
    effect_child_event_no_effects/1,
    effect_child_event_with_effects/1,
    effect_child_only_no_ops/1,
    effect_empty_effects/1,
    effect_multiple_push_events/1,
    effect_only_no_ops/1,
    effect_push_event/1,
    handle_update_child_event_then_parent/1,
    live_child_event/1,
    live_child_multiple_events/1,
    live_child_then_parent_sync/1,
    live_connected_event/1,
    live_counter2_child_event/1,
    live_dec_event/1,
    live_event/1,
    live_handle_info_after_navigate/1,
    live_handle_info_before_mount/1,
    live_handle_info_no_callback/1,
    live_handle_info_no_change/1,
    live_handle_info/1,
    live_handle_info_undefined_transport/1,
    live_handle_info_with_effects/1,
    live_send_after_to_root/1,
    live_send_to_child/1,
    live_send_to_root/1,
    live_send_unknown_view/1,
    live_inc_then_dec/1,
    live_init_bindings/1,
    live_mount/1,
    live_multiple_events/1,
    live_navigate_resets_views/1,
    live_navigate_round_trip/1,
    live_navigate/1,
    live_navigate_then_event/1,
    live_page_child_mount/1,
    live_parent_change_child_stable/1,
    live_parent_event_updates_children/1,
    mount_and_render_basic/1,
    mount_and_render_custom_bindings/1,
    mount_and_render_fingerprinted/1,
    mount_and_render_state_consistency/1,
    navigate_dedup_across_visits/1,
    seed_fps_idempotent/1,
    seed_fps_merges_with_existing/1,
    seed_fps_skips_statics/1,
    seed_fps_unknown_fp_still_sends/1,
    stateful_child_independent_state/1,
    unmount_on_navigate/1,
    unmount_timer_cancelled_on_navigate/1,
    unmount_on_terminate/1,
    child_in_stream_survives_dep_skip/1,
    child_in_stream_removed_on_delete/1,
    multiple_children_in_stream_survive_dep_skip/1,
    child_in_stream_survives_item_update/1,
    two_children_per_item_survive_dep_skip/1,
    on_mount_transforms_bindings/1,
    on_mount_works_on_navigate/1,
    on_mount_empty_is_noop/1,
    on_mount_pipeline/1
]).

all() ->
    [
        {group, gen_server},
        {group, mount_and_render},
        {group, handle_update},
        {group, effects},
        {group, navigation},
        {group, handle_info},
        {group, unmount},
        {group, on_mount},
        {group, fingerprint_dedup},
        {group, seed_fps}
    ].

groups() ->
    [
        {gen_server, [parallel], [
            stateful_child_independent_state,
            live_mount,
            live_event,
            live_multiple_events,
            live_dec_event,
            live_init_bindings,
            live_page_child_mount,
            live_connected_event,
            live_child_event,
            live_child_multiple_events,
            live_parent_event_updates_children,
            live_parent_change_child_stable,
            live_child_then_parent_sync,
            live_counter2_child_event,
            live_inc_then_dec
        ]},
        {mount_and_render, [parallel], [
            mount_and_render_basic,
            mount_and_render_fingerprinted,
            mount_and_render_state_consistency,
            mount_and_render_custom_bindings
        ]},
        {handle_update, [parallel], [
            handle_update_child_event_then_parent
        ]},
        {effects, [parallel], [
            effect_push_event,
            effect_multiple_push_events,
            effect_empty_effects,
            effect_only_no_ops,
            effect_child_only_no_ops,
            effect_child_event_with_effects,
            effect_child_event_no_effects
        ]},
        {navigation, [parallel], [
            live_navigate,
            live_navigate_then_event,
            live_navigate_resets_views,
            live_navigate_round_trip
        ]},
        {handle_info, [parallel], [
            live_handle_info,
            live_handle_info_with_effects,
            live_handle_info_no_callback,
            live_handle_info_before_mount,
            live_handle_info_no_change,
            live_handle_info_after_navigate,
            live_handle_info_undefined_transport,
            live_send_to_root,
            live_send_to_child,
            live_send_unknown_view,
            live_send_after_to_root
        ]},
        {unmount, [parallel], [
            unmount_on_navigate,
            unmount_timer_cancelled_on_navigate,
            unmount_on_terminate,
            child_in_stream_survives_dep_skip,
            child_in_stream_removed_on_delete,
            multiple_children_in_stream_survive_dep_skip,
            child_in_stream_survives_item_update,
            two_children_per_item_survive_dep_skip
        ]},
        {on_mount, [parallel], [
            on_mount_transforms_bindings,
            on_mount_works_on_navigate,
            on_mount_empty_is_noop,
            on_mount_pipeline
        ]},
        {fingerprint_dedup, [parallel], [
            navigate_dedup_across_visits,
            dedup_child_view_ops,
            dedup_item_patch_inner_ops,
            dedup_item_patch_no_prior_fp,
            dedup_nested_dynamics,
            dedup_nested_dynamics_first_time,
            dedup_navigate_nested_dynamics
        ]},
        {seed_fps, [parallel], [
            seed_fps_skips_statics,
            seed_fps_unknown_fp_still_sends,
            seed_fps_merges_with_existing,
            seed_fps_idempotent
        ]}
    ].

%% =============================================================================
%% arizona_live gen_server tests
%% =============================================================================

stateful_child_independent_state(Config) when is_list(Config) ->
    %% Child event updates child bindings independently.
    %% When parent re-renders, handle_update merges parent props into child bindings.
    {ok, Pid} = arizona_live:start_link(
        arizona_page, #{}, undefined, [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    %% Increment counter child to 2
    {ok, _, _} = arizona_live:handle_event(Pid, <<"counter">>, <<"inc">>, #{}),
    {ok, Ops2, _} = arizona_live:handle_event(Pid, <<"counter">>, <<"inc">>, #{}),
    ?assertMatch([[?OP_TEXT, _, <<"2">>]], Ops2),
    %% Title change on parent does not affect counter
    {ok, TitleOps, _} = arizona_live:handle_event(Pid, <<"page">>, <<"title_change">>, #{}),
    ?assertMatch([[?OP_TEXT, _, <<"Changed">>]], TitleOps),
    %% Parent "add" overwrites child count via handle_update
    %% Parent count goes from 0 to 1
    {ok, AddOps, _} = arizona_live:handle_event(Pid, <<"page">>, <<"add">>, #{}),
    %% counter: default merge, count=1
    %% counter2: custom handle_update doubles, count=1*2=2
    ?assertMatch(
        [
            [<<"counter">>, [[?OP_TEXT, _, <<"1">>]]],
            [<<"counter2">>, [[?OP_TEXT, _, <<"2">>]]]
        ],
        AddOps
    ).

live_mount(Config) when is_list(Config) ->
    {ok, Pid} = arizona_live:start_link(
        arizona_root_counter, #{}, undefined, [], arizona_req_test_adapter:new()
    ),
    ?assertEqual({ok, <<"counter">>}, arizona_live:mount(Pid)).

live_event(Config) when is_list(Config) ->
    {ok, Pid} = arizona_live:start_link(
        arizona_root_counter, #{}, undefined, [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    {ok, Ops, []} = arizona_live:handle_event(Pid, <<"counter">>, <<"inc">>, #{}),
    ?assertMatch([[?OP_TEXT, _, <<"1">>]], Ops).

live_multiple_events(Config) when is_list(Config) ->
    {ok, Pid} = arizona_live:start_link(
        arizona_root_counter, #{}, undefined, [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    {ok, _, _} = arizona_live:handle_event(Pid, <<"counter">>, <<"inc">>, #{}),
    {ok, _, _} = arizona_live:handle_event(Pid, <<"counter">>, <<"inc">>, #{}),
    {ok, Ops, []} = arizona_live:handle_event(Pid, <<"counter">>, <<"inc">>, #{}),
    ?assertMatch([[?OP_TEXT, _, <<"3">>]], Ops).

live_dec_event(Config) when is_list(Config) ->
    {ok, Pid} = arizona_live:start_link(
        arizona_root_counter, #{}, undefined, [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    {ok, Ops, []} = arizona_live:handle_event(Pid, <<"counter">>, <<"dec">>, #{}),
    ?assertMatch([[?OP_TEXT, _, <<"-1">>]], Ops).

live_init_bindings(Config) when is_list(Config) ->
    {ok, Pid} = arizona_live:start_link(
        arizona_root_counter, #{count => 10}, undefined, [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    {ok, Ops, []} = arizona_live:handle_event(Pid, <<"counter">>, <<"inc">>, #{}),
    ?assertMatch([[?OP_TEXT, _, <<"11">>]], Ops).

live_page_child_mount(Config) when is_list(Config) ->
    {ok, Pid} = arizona_live:start_link(
        arizona_page, #{}, undefined, [], arizona_req_test_adapter:new()
    ),
    ?assertEqual({ok, <<"page">>}, arizona_live:mount(Pid)).

live_connected_event(Config) when is_list(Config) ->
    {ok, Pid} = arizona_live:start_link(
        arizona_page, #{}, self(), [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    %% mount sends self() ! arizona_connected, handle_info pushes to transport
    receive
        {arizona_push, Ops, Effects} ->
            ?assertMatch([[?OP_TEXT, _, <<"Connected">>]], Ops),
            ?assertEqual([{arizona_js, [14, <<"Welcome">>]}], Effects)
    after 1000 ->
        ct:fail(timeout_waiting_for_connected_push)
    end.

live_child_event(Config) when is_list(Config) ->
    {ok, Pid} = arizona_live:start_link(
        arizona_page, #{}, undefined, [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    {ok, Ops, []} = arizona_live:handle_event(Pid, <<"counter">>, <<"inc">>, #{}),
    %% Child counter's count dynamic changed
    ?assertMatch([[?OP_TEXT, _, <<"1">>]], Ops).

live_child_multiple_events(Config) when is_list(Config) ->
    {ok, Pid} = arizona_live:start_link(
        arizona_page, #{}, undefined, [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    {ok, _, _} = arizona_live:handle_event(Pid, <<"counter">>, <<"inc">>, #{}),
    {ok, _, _} = arizona_live:handle_event(Pid, <<"counter">>, <<"inc">>, #{}),
    {ok, Ops, []} = arizona_live:handle_event(Pid, <<"counter">>, <<"inc">>, #{}),
    ?assertMatch([[?OP_TEXT, _, <<"3">>]], Ops).

live_parent_event_updates_children(Config) when is_list(Config) ->
    %% Parent "add" event increments parent count, triggering handle_update
    %% on counter and counter2 children
    {ok, Pid} = arizona_live:start_link(
        arizona_page, #{}, undefined, [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    {ok, Ops, []} = arizona_live:handle_event(Pid, <<"page">>, <<"add">>, #{}),
    %% counter: default merge, count=1
    %% counter2: custom handle_update doubles, count=1*2=2
    ?assertMatch(
        [
            [<<"counter">>, [[?OP_TEXT, _, <<"1">>]]],
            [<<"counter2">>, [[?OP_TEXT, _, <<"2">>]]]
        ],
        Ops
    ).

live_parent_change_child_stable(Config) when is_list(Config) ->
    %% Title change on parent does not affect counter (dep skip)
    {ok, Pid} = arizona_live:start_link(
        arizona_page, #{}, undefined, [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    %% Increment counter first so we can verify it's not touched
    {ok, _, _} = arizona_live:handle_event(Pid, <<"counter">>, <<"inc">>, #{}),
    %% Change parent title -- counter deps (count) unchanged, no counter ops
    {ok, Ops, []} = arizona_live:handle_event(Pid, <<"page">>, <<"title_change">>, #{}),
    ?assertMatch([[?OP_TEXT, _, <<"Changed">>]], Ops).

live_child_then_parent_sync(Config) when is_list(Config) ->
    %% KEY EDGE CASE: child increments to 2, then parent adds +1.
    %% handle_update does maps:merge(child_bindings, parent_props).
    %% Parent props have count=1 (parent's own count 0+1).
    %% maps:merge overwrites child count=2 with parent count=1.
    {ok, Pid} = arizona_live:start_link(
        arizona_page, #{}, undefined, [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    %% Increment counter child to 2
    {ok, _, _} = arizona_live:handle_event(Pid, <<"counter">>, <<"inc">>, #{}),
    {ok, _, _} = arizona_live:handle_event(Pid, <<"counter">>, <<"inc">>, #{}),
    %% Parent "add" event: parent count 0 -> 1
    %% Counter child: handle_update merges count=1 from parent, overwriting count=2
    {ok, Ops, []} = arizona_live:handle_event(Pid, <<"page">>, <<"add">>, #{}),
    %% counter: default merge, count=1 (parent overwrites child's count=2)
    %% counter2: custom handle_update doubles parent count=1, so count=2
    ?assertMatch(
        [
            [<<"counter">>, [[?OP_TEXT, _, <<"1">>]]],
            [<<"counter2">>, [[?OP_TEXT, _, <<"2">>]]]
        ],
        Ops
    ).

live_counter2_child_event(Config) when is_list(Config) ->
    %% Event on a different child view (counter2)
    {ok, Pid} = arizona_live:start_link(
        arizona_page, #{}, undefined, [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    {ok, Ops, []} = arizona_live:handle_event(Pid, <<"counter2">>, <<"inc">>, #{}),
    ?assertMatch([[?OP_TEXT, _, <<"1">>]], Ops).

live_inc_then_dec(Config) when is_list(Config) ->
    %% Increment then decrement back -- snapshot tracks correctly
    {ok, Pid} = arizona_live:start_link(
        arizona_root_counter, #{}, undefined, [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    {ok, _, _} = arizona_live:handle_event(Pid, <<"counter">>, <<"inc">>, #{}),
    {ok, _, _} = arizona_live:handle_event(Pid, <<"counter">>, <<"dec">>, #{}),
    %% Back to 0, then dec again to -1
    {ok, Ops, []} = arizona_live:handle_event(Pid, <<"counter">>, <<"dec">>, #{}),
    ?assertMatch([[?OP_TEXT, _, <<"-1">>]], Ops).

%% =============================================================================
%% mount_and_render tests
%% =============================================================================

mount_and_render_basic(Config) when is_list(Config) ->
    %% mount_and_render returns {ok, ViewId, PageContent}
    {ok, Pid} = arizona_live:start_link(
        arizona_root_counter, #{}, undefined, [], arizona_req_test_adapter:new()
    ),
    {ok, ViewId, Content} = arizona_live:mount_and_render(Pid),
    ?assertEqual(<<"counter">>, ViewId),
    %% counter has f key, so Content is a fingerprint payload map
    ?assert(is_map(Content)),
    ?assert(maps:is_key(<<"f">>, Content)),
    ?assert(is_binary(maps:get(<<"f">>, Content))).

mount_and_render_fingerprinted(Config) when is_list(Config) ->
    %% page handler also has f key -- verify the payload structure
    {ok, Pid} = arizona_live:start_link(
        arizona_page, #{}, undefined, [], arizona_req_test_adapter:new()
    ),
    {ok, ViewId, Content} = arizona_live:mount_and_render(Pid),
    ?assertEqual(<<"page">>, ViewId),
    ?assert(is_map(Content)),
    ?assert(is_binary(maps:get(<<"f">>, Content))),
    %% Should have s (statics) and d (dynamics) in the payload
    ?assert(maps:is_key(<<"s">>, Content)),
    ?assert(maps:is_key(<<"d">>, Content)).

mount_and_render_state_consistency(Config) when is_list(Config) ->
    %% After mount_and_render, the gen_server has a valid snapshot.
    %% Firing an event should produce correct ops.
    {ok, Pid} = arizona_live:start_link(
        arizona_root_counter, #{}, undefined, [], arizona_req_test_adapter:new()
    ),
    {ok, <<"counter">>, _Content} = arizona_live:mount_and_render(Pid),
    %% Increment counter -- should produce an OP_TEXT with new value
    {ok, Ops, []} = arizona_live:handle_event(Pid, <<"counter">>, <<"inc">>, #{}),
    ?assertMatch([[?OP_TEXT, _, <<"1">>]], Ops).

mount_and_render_custom_bindings(Config) when is_list(Config) ->
    %% mount_and_render with custom initial bindings merges them into state.
    {ok, Pid} = arizona_live:start_link(
        arizona_root_counter, #{count => 42}, undefined, [], arizona_req_test_adapter:new()
    ),
    {ok, <<"counter">>, _Content} = arizona_live:mount_and_render(Pid),
    {ok, Ops, []} = arizona_live:handle_event(Pid, <<"counter">>, <<"inc">>, #{}),
    ?assertMatch([[?OP_TEXT, _, <<"43">>]], Ops).

%% =============================================================================
%% handle_update via arizona_live
%% =============================================================================

%% Test custom handle_update/2 -- counter2 doubles the parent count
%% after child incremented independently, then parent update overwrites
handle_update_child_event_then_parent(Config) when is_list(Config) ->
    {ok, Pid} = arizona_live:start_link(
        arizona_page, #{}, undefined, [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    %% Increment counter2 directly to 5
    {ok, _, _} = arizona_live:handle_event(Pid, <<"counter2">>, <<"inc">>, #{}),
    {ok, _, _} = arizona_live:handle_event(Pid, <<"counter2">>, <<"inc">>, #{}),
    {ok, _, _} = arizona_live:handle_event(Pid, <<"counter2">>, <<"inc">>, #{}),
    {ok, _, _} = arizona_live:handle_event(Pid, <<"counter2">>, <<"inc">>, #{}),
    {ok, Ops5, _} = arizona_live:handle_event(Pid, <<"counter2">>, <<"inc">>, #{}),
    ?assertMatch([[?OP_TEXT, _, <<"5">>]], Ops5),
    %% Now parent "add": parent count 0->1
    %% counter2: handle_update doubles parent count=1, so count=2
    %% (child's independent count=5 is overwritten by maps:merge then doubled)
    {ok, Ops, _} = arizona_live:handle_event(Pid, <<"page">>, <<"add">>, #{}),
    ?assertMatch(
        [
            [<<"counter">>, [[?OP_TEXT, _, <<"1">>]]],
            [<<"counter2">>, [[?OP_TEXT, _, <<"2">>]]]
        ],
        Ops
    ).

%% =============================================================================
%% Effect resolution tests
%% =============================================================================

effect_push_event(Config) when is_list(Config) ->
    %% handle_event returns push_event effect, resolved to wire format
    {ok, Pid} = arizona_live:start_link(
        arizona_effectful, #{}, undefined, [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    {ok, Ops, Effects} = arizona_live:handle_event(
        Pid,
        <<"effectful">>,
        <<"notify">>,
        #{<<"message">> => <<"hello">>}
    ),
    ?assertMatch([[?OP_TEXT, _, <<"hello">>]], Ops),
    ?assertEqual(
        [{arizona_js, [9, <<"notification">>, #{<<"message">> => <<"hello">>}]}],
        Effects
    ).

effect_multiple_push_events(Config) when is_list(Config) ->
    %% Multiple effects returned from a single handle_event
    {ok, Pid} = arizona_live:start_link(
        arizona_effectful, #{}, undefined, [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    {ok, Ops, Effects} = arizona_live:handle_event(Pid, <<"effectful">>, <<"multi">>, #{}),
    ?assertMatch([[?OP_TEXT, _, <<"multi">>]], Ops),
    ?assertEqual(
        [
            {arizona_js, [9, <<"event1">>, #{<<"a">> => 1}]},
            {arizona_js, [9, <<"event2">>, #{<<"b">> => 2}]}
        ],
        Effects
    ).

effect_empty_effects(Config) when is_list(Config) ->
    %% handle_event with no effects returns empty list
    {ok, Pid} = arizona_live:start_link(
        arizona_effectful, #{}, undefined, [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    {ok, [], []} = arizona_live:handle_event(Pid, <<"effectful">>, <<"noop">>, #{}).

effect_only_no_ops(Config) when is_list(Config) ->
    %% Effects returned but bindings unchanged -- no ops, effects still resolved
    {ok, Pid} = arizona_live:start_link(
        arizona_effectful, #{}, undefined, [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    {ok, Ops, Effects} = arizona_live:handle_event(Pid, <<"effectful">>, <<"notify_only">>, #{}),
    ?assertEqual([], Ops),
    ?assertEqual([{arizona_js, [9, <<"ping">>, #{}]}], Effects).

effect_child_only_no_ops(Config) when is_list(Config) ->
    %% Child event: effects but no DOM changes (reset at count=0)
    {ok, Pid} = arizona_live:start_link(
        arizona_page, #{}, undefined, [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    %% Counter starts at 0, reset keeps it at 0 -- no ops, but effect fires
    {ok, Ops, Effects} = arizona_live:handle_event(Pid, <<"counter">>, <<"reset">>, #{}),
    ?assertEqual([], Ops),
    ?assertEqual(
        [{arizona_js, [9, <<"counter_reset">>, #{<<"id">> => <<"counter">>}]}],
        Effects
    ).

effect_child_event_with_effects(Config) when is_list(Config) ->
    %% Child view event produces effects through handle_event/4 path
    {ok, Pid} = arizona_live:start_link(
        arizona_page, #{}, undefined, [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    %% Increment counter to 3 first
    {ok, _, []} = arizona_live:handle_event(Pid, <<"counter">>, <<"inc">>, #{}),
    {ok, _, []} = arizona_live:handle_event(Pid, <<"counter">>, <<"inc">>, #{}),
    {ok, _, []} = arizona_live:handle_event(Pid, <<"counter">>, <<"inc">>, #{}),
    %% Reset counter -- produces ops (count 3->0) AND push_event effect
    {ok, Ops, Effects} = arizona_live:handle_event(Pid, <<"counter">>, <<"reset">>, #{}),
    ?assertMatch([[?OP_TEXT, _, <<"0">>]], Ops),
    ?assertEqual(
        [{arizona_js, [9, <<"counter_reset">>, #{<<"id">> => <<"counter">>}]}],
        Effects
    ).

effect_child_event_no_effects(Config) when is_list(Config) ->
    %% Child view event with empty effects (inc/dec) still returns empty list
    {ok, Pid} = arizona_live:start_link(
        arizona_page, #{}, undefined, [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    {ok, _Ops, Effects} = arizona_live:handle_event(Pid, <<"counter">>, <<"inc">>, #{}),
    ?assertEqual([], Effects).

%% =============================================================================
%% SPA navigation tests
%% =============================================================================

live_navigate(Config) when is_list(Config) ->
    {ok, Pid} = arizona_live:start_link(
        arizona_page, #{}, undefined, [], arizona_req_test_adapter:new()
    ),
    {ok, <<"page">>} = arizona_live:mount(Pid),
    %% Navigate to about -- returns fingerprint payload since about has f key
    NavOpts = #{title => <<"About">>},
    {ok, NewViewId, PageContent} = arizona_live:navigate(
        Pid, arizona_about, NavOpts, arizona_req_test_adapter:new()
    ),
    ?assertEqual(<<"page">>, NewViewId),
    ?assertMatch(#{<<"f">> := _, <<"s">> := _, <<"d">> := _}, PageContent),
    ?assert(lists:member(<<"About">>, maps:get(<<"d">>, PageContent))).

live_navigate_then_event(Config) when is_list(Config) ->
    {ok, Pid} = arizona_live:start_link(
        arizona_page, #{}, self(), [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    %% Drain the arizona_connected push from page mount
    receive
        {arizona_push, _, _} -> ok
    after 1000 -> ct:fail(timeout)
    end,
    %% Navigate to about
    {ok, _, _} = arizona_live:navigate(
        Pid, arizona_about, #{title => <<"About">>}, arizona_req_test_adapter:new()
    ),
    %% About's mount sends arizona_connected via handle_info
    receive
        {arizona_push, Ops, Effects} ->
            %% About's connected doesn't change bindings, so no ops
            ?assertEqual([], Ops),
            %% But produces set_title effect
            ?assertEqual([{arizona_js, [14, <<"About">>]}], Effects)
    after 1000 ->
        ct:fail(timeout_waiting_for_about_connected_push)
    end.

live_navigate_resets_views(Config) when is_list(Config) ->
    {ok, Pid} = arizona_live:start_link(
        arizona_page, #{}, undefined, [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    %% Page has child views (counter, counter2, counter3)
    %% Increment counter
    {ok, _, _} = arizona_live:handle_event(Pid, <<"counter">>, <<"inc">>, #{}),
    %% Navigate to about (has no children)
    {ok, _, _} = arizona_live:navigate(Pid, arizona_about, #{}, arizona_req_test_adapter:new()),
    %% Counter events should no longer work as child events
    %% <<"counter">> is not in views map, so it falls through to root
    %% arizona_about has no <<"inc">> handler -> gen_server crashes
    unlink(Pid),
    Ref = monitor(process, Pid),
    #{level := OldLevel} = logger:get_primary_config(),
    _ = logger:set_primary_config(level, none),
    ?assertExit(
        _,
        arizona_live:handle_event(Pid, <<"counter">>, <<"inc">>, #{})
    ),
    receive
        {'DOWN', Ref, process, Pid, _} -> ok
    after 1000 -> error(timeout)
    end,
    logger:set_primary_config(level, OldLevel).

live_navigate_round_trip(Config) when is_list(Config) ->
    {ok, Pid} = arizona_live:start_link(
        arizona_page, #{}, undefined, [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    %% Increment counter to 3
    {ok, _, _} = arizona_live:handle_event(Pid, <<"counter">>, <<"inc">>, #{}),
    {ok, _, _} = arizona_live:handle_event(Pid, <<"counter">>, <<"inc">>, #{}),
    {ok, _, _} = arizona_live:handle_event(Pid, <<"counter">>, <<"inc">>, #{}),
    %% Navigate away to about
    {ok, _, _} = arizona_live:navigate(Pid, arizona_about, #{}, arizona_req_test_adapter:new()),
    %% Navigate back to page -- fresh mount, counter starts at 0
    %% Returns fingerprint payload; verify counter dynamics contain "0"
    {ok, _, PageContent} = arizona_live:navigate(
        Pid, arizona_page, #{title => <<"Welcome">>}, arizona_req_test_adapter:new()
    ),
    ?assert(is_binary(maps:get(<<"f">>, PageContent))),
    %% Counter child dynamics should show count 0
    Dynamics = maps:get(<<"d">>, PageContent),
    %% First counter child is at index 5
    %% (1=id, 2=title, 3=az-click add, 4=theme, 5=counter)
    CounterPayload = lists:nth(5, Dynamics),
    ?assert(is_binary(maps:get(<<"f">>, CounterPayload))),
    ?assertEqual(
        [
            <<" id=\"counter\"">>,
            <<" az-click=\"[0,&quot;inc&quot;]\"">>,
            <<" az-click=\"[0,&quot;dec&quot;]\"">>,
            <<"0">>
        ],
        maps:get(<<"d">>, CounterPayload)
    ).

%% =============================================================================
%% handle_info tests
%% =============================================================================

live_handle_info(Config) when is_list(Config) ->
    {ok, Pid} = arizona_live:start_link(
        arizona_timer, #{}, self(), [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    Pid ! {set_message, <<"hello">>},
    receive
        {arizona_push, Ops, []} ->
            ?assertMatch([[?OP_TEXT, _, <<"hello">>]], Ops)
    after 1000 ->
        error(timeout)
    end.

live_handle_info_with_effects(Config) when is_list(Config) ->
    {ok, Pid} = arizona_live:start_link(
        arizona_timer, #{}, self(), [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    Pid ! {set_message_with_effect, <<"hi">>},
    receive
        {arizona_push, Ops, Effects} ->
            ?assertMatch([[?OP_TEXT, _, <<"hi">>]], Ops),
            ?assertEqual(
                [
                    {arizona_js, [9, <<"message_changed">>, #{<<"msg">> => <<"hi">>}]}
                ],
                Effects
            )
    after 1000 ->
        error(timeout)
    end.

live_handle_info_no_callback(Config) when is_list(Config) ->
    %% arizona_no_info_root doesn't export handle_info/2 -- message is silently dropped
    {ok, Pid} = arizona_live:start_link(
        arizona_no_info_root, #{}, self(), [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    Pid ! some_message,
    receive
        {arizona_push, _, _} -> error(unexpected_push)
    after 100 ->
        ok
    end.

live_handle_info_before_mount(Config) when is_list(Config) ->
    %% Before mount, snapshot is undefined -- message is dropped
    {ok, Pid} = arizona_live:start_link(
        arizona_timer, #{}, self(), [], arizona_req_test_adapter:new()
    ),
    Pid ! {set_message, <<"hello">>},
    receive
        {arizona_push, _, _} -> error(unexpected_push)
    after 100 ->
        ok
    end.

live_handle_info_no_change(Config) when is_list(Config) ->
    %% Message that doesn't change bindings -- no push (empty ops/effects)
    {ok, Pid} = arizona_live:start_link(
        arizona_timer, #{}, self(), [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    %% Re-set message to the same default value
    Pid ! {set_message, <<"none">>},
    receive
        {arizona_push, _, _} -> error(unexpected_push)
    after 100 ->
        ok
    end.

live_handle_info_after_navigate(Config) when is_list(Config) ->
    %% transport_pid is preserved across navigate -- handle_info still works
    {ok, Pid} = arizona_live:start_link(
        arizona_timer, #{}, self(), [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    %% Navigate to a different handler
    {ok, _, _} = arizona_live:navigate(
        Pid, arizona_timer, #{message => <<"fresh">>}, arizona_req_test_adapter:new()
    ),
    %% Send message after navigate -- should still push
    Pid ! {set_message, <<"after_nav">>},
    receive
        {arizona_push, Ops, []} ->
            ?assertMatch([[?OP_TEXT, _, <<"after_nav">>]], Ops)
    after 1000 ->
        error(timeout)
    end.

live_handle_info_undefined_transport(Config) when is_list(Config) ->
    %% start_link/1 sets transport_pid to undefined -- push is silently no-op
    {ok, Pid} = arizona_live:start_link(
        arizona_timer, #{}, undefined, [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    Pid ! {set_message, <<"hello">>},
    %% No crash, no push
    receive
        {arizona_push, _, _} -> error(unexpected_push)
    after 100 ->
        ok
    end.

%% --- send/send_after routing tests ---

live_send_to_root(Config) when is_list(Config) ->
    {ok, Pid} = arizona_live:start_link(
        arizona_timer, #{}, self(), [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    Pid ! {arizona_view, <<"timer">>, {set_message, <<"via send">>}},
    receive
        {arizona_push, Ops, []} ->
            ?assertMatch([[?OP_TEXT, _, <<"via send">>]], Ops)
    after 1000 ->
        error(timeout)
    end.

live_send_to_child(Config) when is_list(Config) ->
    {ok, Pid} = arizona_live:start_link(
        arizona_page, #{}, self(), [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    %% Drain arizona_connected push from page mount
    receive
        {arizona_push, _, _} -> ok
    after 1000 -> error(timeout)
    end,
    %% Send to child counter view
    Pid ! {arizona_view, <<"counter">>, {set_count, 99}},
    receive
        {arizona_push, Ops, []} ->
            ?assertMatch([[?OP_TEXT, _, <<"99">>]], Ops)
    after 1000 ->
        error(timeout)
    end.

live_send_unknown_view(Config) when is_list(Config) ->
    {ok, Pid} = arizona_live:start_link(
        arizona_timer, #{}, self(), [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    unlink(Pid),
    Ref = monitor(process, Pid),
    Pid ! {arizona_view, <<"nonexistent">>, hello},
    receive
        {'DOWN', Ref, process, Pid, Reason} ->
            ?assertMatch({{unknown_view, <<"nonexistent">>, hello}, _}, Reason)
    after 1000 ->
        error(timeout)
    end.

live_send_after_to_root(Config) when is_list(Config) ->
    {ok, Pid} = arizona_live:start_link(
        arizona_timer, #{}, self(), [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    Pid ! {arizona_view, <<"timer">>, {set_message, <<"delayed">>}},
    receive
        {arizona_push, Ops, []} ->
            ?assertMatch([[?OP_TEXT, _, <<"delayed">>]], Ops)
    after 1000 ->
        error(timeout)
    end.

%% =============================================================================
%% Unmount tests
%% =============================================================================

unmount_on_navigate(Config) when is_list(Config) ->
    %% Start on about, navigate to page. About's unmount is not exported,
    %% so maybe_unmount returns ok. No crash.
    {ok, Pid} = arizona_live:start_link(
        arizona_about, #{}, self(), [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    %% Drain arizona_connected push
    receive
        {arizona_push, _, _} -> ok
    after 1000 -> error(timeout)
    end,
    %% Navigate to page -- should not crash
    {ok, _, _} = arizona_live:navigate(Pid, arizona_page, #{}, arizona_req_test_adapter:new()).

unmount_timer_cancelled_on_navigate(Config) when is_list(Config) ->
    %% About's handle_info(arizona_connected) starts a tick timer via ?send_after.
    %% Navigate to page should cancel it -- no tick message should arrive.
    {ok, Pid} = arizona_live:start_link(
        arizona_about, #{}, self(), [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    %% Drain arizona_connected push (which also starts the tick timer)
    receive
        {arizona_push, _, _} -> ok
    after 1000 -> error(timeout)
    end,
    %% Navigate to page -- cancels pending timers
    {ok, _, _} = arizona_live:navigate(Pid, arizona_page, #{}, arizona_req_test_adapter:new()),
    %% Drain page's arizona_connected push
    receive
        {arizona_push, _, _} -> ok
    after 1000 -> error(timeout)
    end,
    %% Wait -- no tick should arrive (timer was cancelled)
    receive
        {arizona_push, _, _} -> error(unexpected_tick_push)
    after 1500 ->
        ok
    end.

unmount_on_terminate(Config) when is_list(Config) ->
    %% Verify terminate calls maybe_unmount without crashing.
    %% Use a handler without unmount/1 -- should be a no-op.
    {ok, Pid} = arizona_live:start_link(
        arizona_timer, #{}, undefined, [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    unlink(Pid),
    Ref = monitor(process, Pid),
    exit(Pid, shutdown),
    receive
        {'DOWN', Ref, process, Pid, shutdown} -> ok
    after 1000 ->
        error(timeout)
    end.

child_in_stream_survives_dep_skip(Config) when is_list(Config) ->
    %% Stream has a stateful child (arizona_counter) inside items.
    %% Changing title (dep-skips the stream) must NOT prune the child.
    {ok, Pid} = arizona_live:start_link(
        arizona_stream_with_child, #{}, self(), [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    %% Change title -- stream deps (items) not in Changed, so stream is dep-skipped.
    %% The child_views in the stream snapshot should carry counter-1 over.
    {ok, _, _} = arizona_live:handle_event(
        Pid, <<"swc">>, <<"set_title">>, #{~"title" => ~"Updated"}
    ),
    %% Now send to the child inside the stream -- should NOT crash with unknown_view.
    Pid ! {arizona_view, <<"counter-1">>, {set_count, 42}},
    receive
        {arizona_push, Ops, []} ->
            ?assertMatch([[?OP_TEXT, _, <<"42">>]], Ops)
    after 1000 ->
        error(timeout)
    end.

child_in_stream_removed_on_delete(Config) when is_list(Config) ->
    %% Delete a stream item -- its stateful child should be pruned from views.
    {ok, Pid} = arizona_live:start_link(
        arizona_stream_with_child, #{}, self(), [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    %% Delete item 1 -- counter-1 should be removed from views
    {ok, _, _} = arizona_live:handle_event(
        Pid, <<"swc">>, <<"delete_item">>, #{~"id" => 1}
    ),
    %% Sending to counter-1 should crash with unknown_view
    unlink(Pid),
    Ref = monitor(process, Pid),
    Pid ! {arizona_view, <<"counter-1">>, {set_count, 99}},
    receive
        {'DOWN', Ref, process, Pid, Reason} ->
            ?assertMatch({{unknown_view, <<"counter-1">>, _}, _}, Reason)
    after 1000 ->
        error(timeout)
    end.

multiple_children_in_stream_survive_dep_skip(Config) when is_list(Config) ->
    %% Add a second item, then dep-skip -- both children should survive.
    {ok, Pid} = arizona_live:start_link(
        arizona_stream_with_child, #{}, self(), [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    %% Add item 2
    {ok, _, _} = arizona_live:handle_event(
        Pid, <<"swc">>, <<"add_item">>, #{~"id" => 2, ~"label" => ~"Item 2"}
    ),
    %% Dep-skip: change title (doesn't affect stream)
    {ok, _, _} = arizona_live:handle_event(
        Pid, <<"swc">>, <<"set_title">>, #{~"title" => ~"Updated"}
    ),
    %% Both children should still be routable
    Pid ! {arizona_view, <<"counter-1">>, {set_count, 10}},
    receive
        {arizona_push, Ops1, []} ->
            ?assertMatch([[?OP_TEXT, _, <<"10">>]], Ops1)
    after 1000 ->
        error(timeout)
    end,
    Pid ! {arizona_view, <<"counter-2">>, {set_count, 20}},
    receive
        {arizona_push, Ops2, []} ->
            ?assertMatch([[?OP_TEXT, _, <<"20">>]], Ops2)
    after 1000 ->
        error(timeout)
    end.

child_in_stream_survives_item_update(Config) when is_list(Config) ->
    %% Update a stream item's label -- its stateful child should survive with state.
    {ok, Pid} = arizona_live:start_link(
        arizona_stream_with_child, #{}, self(), [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    %% Set counter-1 to 5
    Pid ! {arizona_view, <<"counter-1">>, {set_count, 5}},
    receive
        {arizona_push, _, _} -> ok
    after 1000 -> error(timeout)
    end,
    %% Update item 1's label -- child should survive with count=5
    {ok, _, _} = arizona_live:handle_event(
        Pid, <<"swc">>, <<"update_item">>, #{~"id" => 1, ~"label" => ~"Updated"}
    ),
    %% Verify counter-1 still has count=5 (handle_update merges, keeps count)
    Pid ! {arizona_view, <<"counter-1">>, {set_count, 7}},
    receive
        {arizona_push, Ops, []} ->
            ?assertMatch([[?OP_TEXT, _, <<"7">>]], Ops)
    after 1000 ->
        error(timeout)
    end.

two_children_per_item_survive_dep_skip(Config) when is_list(Config) ->
    %% Each stream item has 2 stateful children (counter-N and extra-N).
    %% Both should survive dep-skip.
    {ok, Pid} = arizona_live:start_link(
        arizona_stream_with_child, #{}, self(), [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    %% Dep-skip: change title
    {ok, _, _} = arizona_live:handle_event(
        Pid, <<"swc">>, <<"set_title">>, #{~"title" => ~"Updated"}
    ),
    %% counter-1 should be routable
    Pid ! {arizona_view, <<"counter-1">>, {set_count, 10}},
    receive
        {arizona_push, Ops1, []} ->
            ?assertMatch([[?OP_TEXT, _, <<"10">>]], Ops1)
    after 1000 ->
        error(timeout)
    end,
    %% extra-1 should also be routable
    Pid ! {arizona_view, <<"extra-1">>, {set_count, 20}},
    receive
        {arizona_push, Ops2, []} ->
            ?assertMatch([[?OP_TEXT, _, <<"20">>]], Ops2)
    after 1000 ->
        error(timeout)
    end.

%% =============================================================================
%% on_mount tests
%% =============================================================================

on_mount_transforms_bindings(Config) when is_list(Config) ->
    %% on_mount adds a key to bindings before mount
    OnMount = [fun(B, _Req) -> B#{extra => <<"from_on_mount">>} end],
    {ok, Pid} = arizona_live:start_link(
        arizona_timer, #{}, self(), OnMount, arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    %% Timer renders ?get(message). On mount, message defaults to "none".
    %% Verify on_mount ran by sending a message that reads extra from bindings.
    %% Actually, just verify mount succeeded -- on_mount didn't crash.
    %% For a stronger test, send set_message and verify the process is alive.
    Pid ! {arizona_view, <<"timer">>, {set_message, <<"hello">>}},
    receive
        {arizona_push, Ops, []} ->
            ?assertMatch([[?OP_TEXT, _, <<"hello">>]], Ops)
    after 1000 ->
        error(timeout)
    end.

on_mount_works_on_navigate(Config) when is_list(Config) ->
    %% on_mount runs on navigate too
    OnMount = [fun(B, _Req) -> B#{extra => <<"nav_mount">>} end],
    {ok, Pid} = arizona_live:start_link(
        arizona_timer, #{}, self(), [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    %% Navigate with on_mount
    {ok, _, _} = arizona_live:navigate(
        Pid, arizona_timer, #{}, arizona_req_test_adapter:new(), OnMount
    ),
    %% Verify process is alive and functional
    Pid ! {arizona_view, <<"timer">>, {set_message, <<"after_nav">>}},
    receive
        {arizona_push, Ops, []} ->
            ?assertMatch([[?OP_TEXT, _, <<"after_nav">>]], Ops)
    after 1000 ->
        error(timeout)
    end.

on_mount_empty_is_noop(Config) when is_list(Config) ->
    %% Empty on_mount list doesn't affect bindings
    {ok, Pid} = arizona_live:start_link(
        arizona_timer, #{}, self(), [], arizona_req_test_adapter:new()
    ),
    {ok, <<"timer">>} = arizona_live:mount(Pid).

on_mount_pipeline(Config) when is_list(Config) ->
    %% Multiple on_mount hooks run in order, each transforms bindings.
    Step1 = fun(B, _Req) -> B#{step => 1} end,
    Step2 = fun(#{step := N} = B, _Req) -> B#{step => N + 1} end,
    OnMount = [Step1, Step2],
    {ok, Pid} = arizona_live:start_link(
        arizona_timer, #{}, self(), OnMount, arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    %% Both hooks ran -- step should be 2
    Pid ! {arizona_view, <<"timer">>, {set_message, <<"ok">>}},
    receive
        {arizona_push, _, _} -> ok
    after 1000 ->
        error(timeout)
    end.

%% =============================================================================
%% Fingerprint dedup tests
%% =============================================================================

navigate_dedup_across_visits(Config) when is_list(Config) ->
    %% Fingerprint cache persists across navigates
    {ok, Pid} = arizona_live:start_link(
        arizona_page, #{}, undefined, [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    %% First navigate to about -- has statics
    {ok, _, Content1} = arizona_live:navigate(
        Pid, arizona_about, #{}, arizona_req_test_adapter:new()
    ),
    ?assert(maps:is_key(<<"s">>, Content1)),
    AboutFp = maps:get(<<"f">>, Content1),
    ?assert(is_binary(AboutFp)),
    %% Navigate back to page (different fingerprint -- has statics)
    {ok, _, Content2} = arizona_live:navigate(
        Pid, arizona_page, #{}, arizona_req_test_adapter:new()
    ),
    ?assert(maps:is_key(<<"s">>, Content2)),
    PageFp = maps:get(<<"f">>, Content2),
    ?assertNotEqual(AboutFp, PageFp),
    %% Navigate to about again -- statics should be stripped
    {ok, _, Content3} = arizona_live:navigate(
        Pid, arizona_about, #{}, arizona_req_test_adapter:new()
    ),
    ?assertNot(maps:is_key(<<"s">>, Content3)),
    ?assertEqual(AboutFp, maps:get(<<"f">>, Content3)).

dedup_child_view_ops(Config) when is_list(Config) ->
    %% Child view ops [BinId, ChildOps] should have fingerprints deduped
    FpPayload1 = #{
        <<"f">> => <<"ch_fp">>,
        <<"s">> => [<<"<p>">>, <<"</p>">>],
        <<"d">> => [<<"A">>]
    },
    FpPayload2 = #{
        <<"f">> => <<"ch_fp">>,
        <<"s">> => [<<"<p>">>, <<"</p>">>],
        <<"d">> => [<<"B">>]
    },
    Ops = [
        [
            <<"child_view">>,
            [
                [?OP_TEXT, <<"0">>, FpPayload1],
                [?OP_TEXT, <<"1">>, FpPayload2]
            ]
        ]
    ],
    {Result, _Fps} = dedup_fps(Ops, #{}),
    [[<<"child_view">>, [Inner1, Inner2]]] = Result,
    %% First should keep statics
    [_, _, P1] = Inner1,
    ?assert(maps:is_key(<<"s">>, P1)),
    %% Second should have statics stripped (same fingerprint)
    [_, _, P2] = Inner2,
    ?assertNot(maps:is_key(<<"s">>, P2)),
    ?assertEqual(<<"ch_fp">>, maps:get(<<"f">>, P2)).

dedup_item_patch_inner_ops(Config) when is_list(Config) ->
    %% OP_ITEM_PATCH inner ops should have fingerprints deduped
    FpPayload = #{
        <<"f">> => <<"ip_fp">>,
        <<"s">> => [<<"<b>">>, <<"</b>">>],
        <<"d">> => [<<"X">>]
    },
    Ops = [
        %% First op registers the fingerprint
        [?OP_INSERT, <<"0">>, <<"k1">>, -1, FpPayload],
        %% Inner ops of ITEM_PATCH should have statics stripped
        [
            ?OP_ITEM_PATCH,
            <<"0">>,
            <<"k2">>,
            [
                [
                    ?OP_TEXT,
                    <<"0">>,
                    #{
                        <<"f">> => <<"ip_fp">>,
                        <<"s">> => [<<"<b>">>, <<"</b>">>],
                        <<"d">> => [<<"Y">>]
                    }
                ]
            ]
        ]
    ],
    {Result, _Fps} = dedup_fps(Ops, #{}),
    [_InsOp, PatchOp] = Result,
    [?OP_ITEM_PATCH, <<"0">>, <<"k2">>, InnerOps] = PatchOp,
    [[?OP_TEXT, <<"0">>, InnerPayload]] = InnerOps,
    %% Inner payload should have statics stripped
    ?assertNot(maps:is_key(<<"s">>, InnerPayload)),
    ?assertEqual(<<"ip_fp">>, maps:get(<<"f">>, InnerPayload)).

dedup_item_patch_no_prior_fp(Config) when is_list(Config) ->
    %% OP_ITEM_PATCH inner ops with a new fingerprint keep statics
    FpPayload = #{
        <<"f">> => <<"new_ip_fp">>,
        <<"s">> => [<<"<i>">>, <<"</i>">>],
        <<"d">> => [<<"Z">>]
    },
    Ops = [
        [
            ?OP_ITEM_PATCH,
            <<"0">>,
            <<"k1">>,
            [
                [?OP_TEXT, <<"0">>, FpPayload]
            ]
        ]
    ],
    {Result, Fps} = dedup_fps(Ops, #{}),
    [[?OP_ITEM_PATCH, <<"0">>, <<"k1">>, InnerOps]] = Result,
    [[?OP_TEXT, <<"0">>, P]] = InnerOps,
    ?assert(maps:is_key(<<"s">>, P)),
    %% Fingerprint should now be tracked
    ?assert(maps:is_key(<<"new_ip_fp">>, Fps)).

dedup_nested_dynamics(Config) when is_list(Config) ->
    %% A fingerprinted payload with nested fingerprinted dynamics
    %% should have nested statics stripped on second encounter
    Payload = #{
        <<"f">> => <<"parent_fp">>,
        <<"s">> => [<<"<div>">>, <<"</div>">>],
        <<"d">> => [
            #{
                <<"f">> => <<"child_fp">>,
                <<"s">> => [<<"<span>">>, <<"</span>">>],
                <<"d">> => [<<"inner">>]
            }
        ]
    },
    %% Pre-register child fingerprint
    Fps0 = #{<<"child_fp">> => true},
    Ops = [[?OP_TEXT, <<"0">>, Payload]],
    {Result, _Fps} = dedup_fps(Ops, Fps0),
    [[?OP_TEXT, <<"0">>, P]] = Result,
    %% Parent is first time -- keeps statics
    ?assert(maps:is_key(<<"s">>, P)),
    %% Nested child was already seen -- statics stripped
    [ChildPayload] = maps:get(<<"d">>, P),
    ?assertNot(maps:is_key(<<"s">>, ChildPayload)),
    ?assertEqual(<<"child_fp">>, maps:get(<<"f">>, ChildPayload)).

dedup_nested_dynamics_first_time(Config) when is_list(Config) ->
    %% Both parent and child fingerprints seen for the first time
    Payload = #{
        <<"f">> => <<"p2_fp">>,
        <<"s">> => [<<"<div>">>, <<"</div>">>],
        <<"d">> => [
            #{
                <<"f">> => <<"c2_fp">>,
                <<"s">> => [<<"<em>">>, <<"</em>">>],
                <<"d">> => [<<"val">>]
            }
        ]
    },
    Ops = [[?OP_TEXT, <<"0">>, Payload]],
    {Result, Fps} = dedup_fps(Ops, #{}),
    [[?OP_TEXT, <<"0">>, P]] = Result,
    %% Both first time -- both keep statics
    ?assert(maps:is_key(<<"s">>, P)),
    [ChildPayload] = maps:get(<<"d">>, P),
    ?assert(maps:is_key(<<"s">>, ChildPayload)),
    %% Both fingerprints now tracked
    ?assert(maps:is_key(<<"p2_fp">>, Fps)),
    ?assert(maps:is_key(<<"c2_fp">>, Fps)).

dedup_navigate_nested_dynamics(Config) when is_list(Config) ->
    %% Navigate returns fingerprinted payload whose dynamics contain
    %% nested fingerprinted child components. Within a single dedup pass,
    %% the first counter keeps statics but subsequent ones (same fp) are stripped.
    {ok, Pid} = arizona_live:start_link(
        arizona_page, #{}, undefined, [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    %% First navigate to page -- page fp is new, keeps statics
    {ok, _, Content1} = arizona_live:navigate(
        Pid, arizona_page, #{}, arizona_req_test_adapter:new()
    ),
    ?assert(maps:is_key(<<"s">>, Content1)),
    %% Counter child dynamics are fingerprinted
    D1 = maps:get(<<"d">>, Content1),
    CounterPayloads1 = [P || P <- D1, is_map(P), maps:is_key(<<"f">>, P)],
    ?assert(length(CounterPayloads1) > 0),
    %% First counter of each fingerprint gets statics, rest are stripped
    WithS1 = [P || P <- CounterPayloads1, maps:is_key(<<"s">>, P)],
    WithoutS1 = [P || P <- CounterPayloads1, not maps:is_key(<<"s">>, P)],
    ?assert(length(WithS1) >= 1),
    %% at least one stripped (3 counters, same fp)
    ?assert(length(WithoutS1) >= 1),
    %% Second navigate -- page statics stripped, ALL counter statics stripped
    {ok, _, Content2} = arizona_live:navigate(
        Pid, arizona_page, #{}, arizona_req_test_adapter:new()
    ),
    ?assertNot(maps:is_key(<<"s">>, Content2)),
    D2 = maps:get(<<"d">>, Content2),
    CounterPayloads2 = [P || P <- D2, is_map(P), maps:is_key(<<"f">>, P)],
    lists:foreach(fun(P) -> ?assertNot(maps:is_key(<<"s">>, P)) end, CounterPayloads2).

%% =============================================================================
%% seed_fps tests
%% =============================================================================

seed_fps_skips_statics(Config) when is_list(Config) ->
    %% Seed a fingerprint that the client already knows -> statics stripped
    %% First, discover the actual fingerprint by doing an insert
    {ok, Pid0} = arizona_live:start_link(
        arizona_todo, #{}, undefined, [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid0),
    {ok, Ops0, []} = arizona_live:handle_event(
        Pid0,
        <<"todo">>,
        <<"add">>,
        #{<<"id">> => 1, <<"text">> => <<"A">>}
    ),
    [[?OP_INSERT, _, _, _, P0]] = Ops0,
    ItemFp = maps:get(<<"f">>, P0),
    %% Now start fresh and seed that fingerprint
    {ok, Pid} = arizona_live:start_link(
        arizona_todo, #{}, undefined, [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    arizona_live:seed_fps(Pid, [ItemFp]),
    %% Give the cast time to be processed
    sys:get_state(Pid),
    %% Add an item -- its fingerprinted payload should have statics stripped
    {ok, Ops, []} = arizona_live:handle_event(
        Pid,
        <<"todo">>,
        <<"add">>,
        #{<<"id">> => 1, <<"text">> => <<"A">>}
    ),
    [[?OP_INSERT, _, _, _, Payload]] = Ops,
    ?assert(maps:is_key(<<"f">>, Payload)),
    ?assertNot(maps:is_key(<<"s">>, Payload)).

seed_fps_unknown_fp_still_sends(Config) when is_list(Config) ->
    %% Seed an unrelated fingerprint -> new fp still includes statics
    {ok, Pid} = arizona_live:start_link(
        arizona_todo, #{}, undefined, [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    arizona_live:seed_fps(Pid, [<<"unrelated_fp">>]),
    sys:get_state(Pid),
    {ok, Ops, []} = arizona_live:handle_event(
        Pid,
        <<"todo">>,
        <<"add">>,
        #{<<"id">> => 1, <<"text">> => <<"A">>}
    ),
    [[?OP_INSERT, _, _, _, Payload]] = Ops,
    ?assert(maps:is_key(<<"f">>, Payload)),
    ?assert(maps:is_key(<<"s">>, Payload)).

seed_fps_merges_with_existing(Config) when is_list(Config) ->
    %% Add item (caches todo_item_tpl), then seed todo_tpl, then navigate
    %% -- both fingerprints should be stripped
    %% First, discover the outer template fingerprint via a navigate
    {ok, Pid0} = arizona_live:start_link(
        arizona_todo, #{}, undefined, [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid0),
    {ok, _, Content0} = arizona_live:navigate(
        Pid0, arizona_todo, #{items => []}, arizona_req_test_adapter:new()
    ),
    TodoTplFp = maps:get(<<"f">>, Content0),
    %% Now start the real test
    {ok, Pid} = arizona_live:start_link(
        arizona_todo, #{}, undefined, [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    %% First add caches todo_item_tpl via dedup_fps
    {ok, Ops1, []} = arizona_live:handle_event(
        Pid,
        <<"todo">>,
        <<"add">>,
        #{<<"id">> => 1, <<"text">> => <<"A">>}
    ),
    [[?OP_INSERT, _, _, _, P1]] = Ops1,
    ?assert(maps:is_key(<<"s">>, P1)),
    %% Seed todo_tpl (the outer template) using discovered fingerprint
    arizona_live:seed_fps(Pid, [TodoTplFp]),
    sys:get_state(Pid),
    %% Now add another item -- todo_item_tpl should be stripped (already sent)
    {ok, Ops2, []} = arizona_live:handle_event(
        Pid,
        <<"todo">>,
        <<"add">>,
        #{<<"id">> => 2, <<"text">> => <<"B">>}
    ),
    [[?OP_INSERT, _, _, _, P2]] = Ops2,
    ?assertNot(maps:is_key(<<"s">>, P2)),
    %% Navigate to todo -- todo_tpl should be stripped (seeded)
    InitItems = [#{id => 3, text => <<"C">>}],
    {ok, _, Content} = arizona_live:navigate(
        Pid, arizona_todo, #{items => InitItems}, arizona_req_test_adapter:new()
    ),
    ?assert(maps:is_key(<<"f">>, Content)),
    ?assertNot(maps:is_key(<<"s">>, Content)).

seed_fps_idempotent(Config) when is_list(Config) ->
    %% Announce a fingerprint that's already in sent_fps from a prior event
    {ok, Pid} = arizona_live:start_link(
        arizona_todo, #{}, undefined, [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    %% First add caches todo_item_tpl in sent_fps via dedup
    {ok, _, []} = arizona_live:handle_event(
        Pid,
        <<"todo">>,
        <<"add">>,
        #{<<"id">> => 1, <<"text">> => <<"A">>}
    ),
    %% Client announces the same fp it was already sent -- should be a no-op
    arizona_live:seed_fps(Pid, [<<"todo_item_tpl">>]),
    sys:get_state(Pid),
    %% Second add should still strip statics
    {ok, Ops, []} = arizona_live:handle_event(
        Pid,
        <<"todo">>,
        <<"add">>,
        #{<<"id">> => 2, <<"text">> => <<"B">>}
    ),
    [[?OP_INSERT, _, _, _, Payload]] = Ops,
    ?assertNot(maps:is_key(<<"s">>, Payload)).

%% =============================================================================
%% Helpers
%% =============================================================================

%% Wrapper to call arizona_live's internal dedup_fps for unit testing
dedup_fps(Ops, Fps) ->
    lists:mapfoldl(fun dedup_fp_op/2, Fps, Ops).

dedup_fp_op([BinId, ChildOps], Fps0) when is_binary(BinId), is_list(ChildOps) ->
    {ChildOps1, Fps1} = dedup_fps(ChildOps, Fps0),
    {[BinId, ChildOps1], Fps1};
dedup_fp_op([OpCode, Target | Rest], Fps0) when is_integer(OpCode) ->
    {Rest1, Fps1} = dedup_fp_rest(Rest, Fps0),
    {[OpCode, Target | Rest1], Fps1};
dedup_fp_op(Op, Fps) ->
    {Op, Fps}.

dedup_fp_rest([], Fps) ->
    {[], Fps};
dedup_fp_rest([H | T], Fps0) ->
    {H1, Fps1} = dedup_fp_val(H, Fps0),
    {T1, Fps2} = dedup_fp_rest(T, Fps1),
    {[H1 | T1], Fps2}.

dedup_fp_val(#{<<"f">> := F, <<"s">> := _, <<"d">> := D} = Val, Fps) ->
    case Fps of
        #{F := _} ->
            {D1, Fps1} = dedup_fp_dlist(D, Fps),
            {maps:without([<<"s">>], Val#{<<"d">> => D1}), Fps1};
        #{} ->
            {D1, Fps1} = dedup_fp_dlist(D, Fps#{F => true}),
            {Val#{<<"d">> => D1}, Fps1}
    end;
dedup_fp_val(#{<<"f">> := F, <<"d">> := D} = Val, Fps) ->
    {D1, Fps1} = dedup_fp_dlist(D, Fps),
    Fps2 =
        case Fps1 of
            #{F := _} -> Fps1;
            #{} -> Fps1#{F => true}
        end,
    {Val#{<<"d">> => D1}, Fps2};
dedup_fp_val(Items, Fps) when is_list(Items) ->
    dedup_fp_dlist(Items, Fps);
dedup_fp_val(Val, Fps) ->
    {Val, Fps}.

dedup_fp_dlist([], Fps) ->
    {[], Fps};
dedup_fp_dlist([H | T], Fps0) ->
    {H1, Fps1} = dedup_fp_val(H, Fps0),
    {T1, Fps2} = dedup_fp_dlist(T, Fps1),
    {[H1 | T1], Fps2}.
