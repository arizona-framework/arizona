-module(arizona_stream_SUITE).
-include_lib("stdlib/include/assert.hrl").
-include("arizona.hrl").

-export([all/0, groups/0]).
-export([
    datatable_live_add_row_sequence/1,
    datatable_live_add_row/1,
    datatable_live_connected/1,
    datatable_live_delete_all_then_reset/1,
    datatable_live_delete_row_middle/1,
    datatable_live_delete_row/1,
    datatable_live_mount/1,
    datatable_live_move_top_already_first/1,
    datatable_live_move_top/1,
    datatable_live_reset_restores_next_id/1,
    datatable_live_reset/1,
    datatable_live_shuffle/1,
    datatable_live_sort_asc/1,
    datatable_live_sort_col_change/1,
    datatable_live_sort_dom_simulation/1,
    datatable_live_sort_name_no_move/1,
    datatable_live_sort_toggle/1,
    datatable_mount/1,
    datatable_ssr/1,
    dedup_stream_insert_then_item_patch/1,
    dedup_stream_items_in_dynamics/1,
    dedup_stream_preserves_t/1,
    list_type_switch_list_to_stream/1,
    list_type_switch_stream_to_list/1,
    navigate_with_stream_items/1,
    nested_stream_render/1,
    page_add_todo_live/1,
    page_add_todo_then_title_change_then_add_todo/1,
    page_clear_todos_live/1,
    page_connected_event_skips_stream/1,
    page_remove_todo_live/1,
    page_title_change_skips_stream/1,
    render_fp_val_empty_stream/1,
    render_fp_val_stream_items_no_fp/1,
    render_fp_val_stream_items/1,
    render_to_iolist_with_stream_items/1,
    stream_dedup_strips_statics/1,
    stream_delete_nonexistent_key/1,
    stream_deps_skip/1,
    stream_update_identical_emits_no_ops/1,
    stream_diff_insert/1,
    stream_diff_mixed/1,
    stream_diff_no_change/1,
    stream_diff_remove/1,
    stream_diff_reset/1,
    stream_diff_update/1,
    stream_duplicate_key_insert/1,
    stream_empty_pending_deps_changed/1,
    stream_empty_stream_operations/1,
    stream_get3_existing_key/1,
    stream_get3_missing_key_returns_default/1,
    stream_get_existing_key/1,
    stream_get_lazy_existing_key/1,
    stream_get_lazy_missing_key_calls_fun/1,
    stream_get_missing_key_crashes/1,
    stream_insert_at_middle_position/1,
    stream_insert_at_position/1,
    stream_keys_empty/1,
    stream_keys_multiple/1,
    stream_keys/1,
    stream_limit_drop_delete/1,
    stream_limit_drop_insert/1,
    stream_limit_halt_diff/1,
    stream_limit_halt_render/1,
    stream_limit_one/1,
    stream_limit_reset/1,
    stream_limit_ssr/1,
    stream_live_delete_last_then_readd/1,
    stream_live_insert_at_position/1,
    stream_live_insert_no_stale_pending/1,
    stream_live_insert/1,
    stream_live_mount_with_items/1,
    stream_live_mount_with_items_then_delete/1,
    stream_live_mount_with_items_then_insert/1,
    stream_live_mount_with_items_then_reset/1,
    stream_live_multi_event/1,
    stream_live_remove/1,
    stream_live_reset/1,
    stream_live_reset_with_items/1,
    stream_live_three_sequential_inserts/1,
    stream_live_update_no_change/1,
    stream_live_update/1,
    stream_move_after_key_first/1,
    stream_move_after_key_last/1,
    stream_move_after_key_middle/1,
    stream_move_nonexistent_key/1,
    stream_move_op/1,
    stream_move/1,
    stream_move_to_same_position/1,
    stream_new_stream_opts/1,
    stream_order_after_complex_sequence/1,
    stream_render/1,
    stream_render_with_items/1,
    stream_reset_full_overlap/1,
    stream_reset_overlap_changed_content/1,
    stream_reset_overlap_same_content/1,
    stream_reset_reorder/1,
    stream_reset_same_order_no_moves/1,
    stream_reset_with_new_items/1,
    stream_snapshot_after_multiple_cycles/1,
    stream_sort_already_sorted/1,
    stream_sort_basic/1,
    stream_sort_emits_reorder_ops/1,
    stream_sort_reverse/1,
    stream_ssr/1,
    stream_to_list_empty/1,
    stream_to_list_preserves_order/1,
    update_pending_op_carries_changed/1,
    update_on_missing_key_yields_empty_changed/1,
    reset_pending_op_carries_old_items/1,
    stream_to_list/1,
    stream_update_no_change/1,
    stream_update_nonexistent_key/1,
    %% --- shuffle crash reproducers ----------------------------------------
    repro_insert_dup_breaks_invariant/1,
    repro_insert_at_dup_breaks_invariant/1,
    repro_reset_with_dup_keys_breaks_invariant/1,
    repro_update_missing_key_breaks_invariant/1,
    repro_insert_dup_then_delete_yields_stale_order/1,
    repro_to_list_crash_matches_production_stack/1,
    repro_shuffle_after_insert_dup_then_delete_crashes/1,
    datatable_navigate_from_page_then_add_row_no_crash/1,
    format_error_missing_stream_key_suggests_close_match/1,
    format_error_missing_stream_key_no_suggestion_for_far_match/1
]).

all() ->
    [
        {group, stream_unit},
        {group, stream_edge_unit},
        {group, stream_live},
        {group, page_todo},
        {group, stream_regression},
        {group, stream_api},
        {group, stream_edge_cases},
        {group, stream_to_list},
        {group, stream_sort},
        {group, stream_reset_reorder},
        {group, stream_move_after_key},
        {group, nested_stream},
        {group, datatable},
        {group, list_type_switch},
        {group, shuffle_crash_repro}
    ].

groups() ->
    [
        %% 12. Stream unit tests (via arizona_todo handler)
        {stream_unit, [parallel], [
            stream_render,
            stream_render_with_items,
            stream_ssr,
            stream_diff_no_change,
            stream_diff_insert,
            stream_diff_remove,
            stream_diff_update,
            stream_diff_reset,
            stream_diff_mixed,
            stream_deps_skip,
            stream_update_identical_emits_no_ops
        ]},
        %% 12b. Stream edge case tests (unit-level)
        {stream_edge_unit, [parallel], [
            stream_insert_at_position,
            stream_insert_at_middle_position,
            stream_reset_with_new_items,
            stream_reset_overlap_same_content,
            stream_reset_overlap_changed_content,
            stream_reset_full_overlap,
            stream_update_no_change,
            stream_snapshot_after_multiple_cycles,
            stream_empty_pending_deps_changed,
            stream_move,
            stream_move_nonexistent_key
        ]},
        %% 13. Stream E2E tests (via arizona_live)
        {stream_live, [parallel], [
            stream_live_insert,
            stream_live_remove,
            stream_live_update,
            stream_live_reset,
            stream_live_multi_event
        ]},
        %% 14. Page todo E2E tests (arizona_page with todo section)
        {page_todo, [parallel], [
            page_add_todo_live,
            page_remove_todo_live,
            page_clear_todos_live,
            stream_live_mount_with_items_then_insert,
            stream_live_mount_with_items_then_delete,
            stream_live_mount_with_items_then_reset,
            stream_live_delete_last_then_readd,
            stream_live_update_no_change,
            stream_live_three_sequential_inserts,
            stream_live_insert_at_position,
            stream_live_reset_with_items,
            page_title_change_skips_stream,
            page_add_todo_then_title_change_then_add_todo,
            page_connected_event_skips_stream
        ]},
        %% 11. Stream bug regression tests
        {stream_regression, [parallel], [
            stream_live_mount_with_items,
            stream_live_insert_no_stale_pending
        ]},
        %% 16. Stream limit + move + new API tests
        {stream_api, [parallel], [
            stream_new_stream_opts,
            stream_keys,
            stream_keys_multiple,
            stream_keys_empty,
            stream_get_existing_key,
            stream_get_missing_key_crashes,
            stream_get3_existing_key,
            stream_get3_missing_key_returns_default,
            stream_get_lazy_existing_key,
            stream_get_lazy_missing_key_calls_fun,
            stream_limit_halt_render,
            stream_limit_halt_diff,
            stream_limit_drop_delete,
            stream_limit_drop_insert,
            stream_move_op,
            stream_limit_ssr,
            stream_limit_reset,
            update_pending_op_carries_changed,
            update_on_missing_key_yields_empty_changed,
            reset_pending_op_carries_old_items
        ]},
        %% Edge case tests
        {stream_edge_cases, [parallel], [
            stream_delete_nonexistent_key,
            stream_update_nonexistent_key,
            stream_duplicate_key_insert,
            format_error_missing_stream_key_suggests_close_match,
            format_error_missing_stream_key_no_suggestion_for_far_match,
            stream_limit_one,
            stream_move_to_same_position,
            stream_empty_stream_operations,
            stream_order_after_complex_sequence,
            render_to_iolist_with_stream_items,
            stream_dedup_strips_statics,
            render_fp_val_stream_items,
            render_fp_val_stream_items_no_fp,
            render_fp_val_empty_stream,
            dedup_stream_items_in_dynamics,
            dedup_stream_preserves_t,
            navigate_with_stream_items,
            dedup_stream_insert_then_item_patch
        ]},
        %% stream_to_list tests
        {stream_to_list, [parallel], [
            stream_to_list,
            stream_to_list_empty,
            stream_to_list_preserves_order
        ]},
        %% stream_sort tests
        {stream_sort, [parallel], [
            stream_sort_basic,
            stream_sort_already_sorted,
            stream_sort_emits_reorder_ops,
            stream_sort_reverse
        ]},
        %% stream_reset reorder tests
        {stream_reset_reorder, [parallel], [
            stream_reset_reorder,
            stream_reset_same_order_no_moves
        ]},
        %% OP_MOVE after_key semantics tests
        {stream_move_after_key, [parallel], [
            stream_move_after_key_first,
            stream_move_after_key_last,
            stream_move_after_key_middle
        ]},
        %% Nested stream tests
        {nested_stream, [parallel], [
            nested_stream_render
        ]},
        %% DataTable handler tests
        {datatable, [parallel], [
            datatable_mount,
            datatable_ssr,
            datatable_live_mount,
            datatable_live_connected,
            datatable_live_add_row,
            datatable_live_add_row_sequence,
            datatable_live_delete_row,
            datatable_live_delete_row_middle,
            datatable_live_sort_asc,
            datatable_live_sort_name_no_move,
            datatable_live_sort_toggle,
            datatable_live_sort_col_change,
            datatable_live_move_top,
            datatable_live_move_top_already_first,
            datatable_live_reset,
            datatable_live_reset_restores_next_id,
            datatable_live_shuffle,
            datatable_live_sort_dom_simulation,
            datatable_live_delete_all_then_reset,
            datatable_navigate_from_page_then_add_row_no_crash
        ]},
        %% List type switch tests
        {list_type_switch, [parallel], [
            list_type_switch_stream_to_list,
            list_type_switch_list_to_stream
        ]},
        %% Reproducers for the `arizona_datatable` shuffle crash.
        %% Each case exercises a single public-API misuse and asserts the
        %% items <-> order invariant that the production crash violated.
        {shuffle_crash_repro, [parallel], [
            repro_insert_dup_breaks_invariant,
            repro_insert_at_dup_breaks_invariant,
            repro_reset_with_dup_keys_breaks_invariant,
            repro_update_missing_key_breaks_invariant,
            repro_insert_dup_then_delete_yields_stale_order,
            repro_to_list_crash_matches_production_stack,
            repro_shuffle_after_insert_dup_then_delete_crashes
        ]}
    ].

%% =============================================================================
%% 12. Stream unit tests (via arizona_todo handler)
%% =============================================================================

stream_render(Config) when is_list(Config) ->
    %% First render of empty stream produces empty <ul>
    {B, _} = arizona_todo:mount(#{}, arizona_req_test_adapter:new(#{})),
    Tmpl = arizona_todo:render(B),
    {HTML, _Snap, _Views} = arizona_render:render(Tmpl, #{}),
    HTMLBin = iolist_to_binary(HTML),
    ?assertNotEqual(nomatch, binary:match(HTMLBin, <<"<ul">>)),
    ?assertNotEqual(nomatch, binary:match(HTMLBin, <<"<!--/az--></ul>">>)).

stream_render_with_items(Config) when is_list(Config) ->
    %% Render with initial items produces correct HTML
    Items = [#{id => 1, text => <<"A">>}, #{id => 2, text => <<"B">>}],
    {B, _} = arizona_todo:mount(#{items => Items}, arizona_req_test_adapter:new(#{})),
    Tmpl = arizona_todo:render(B),
    {HTML, _Snap, _Views} = arizona_render:render(Tmpl, #{}),
    HTMLBin = iolist_to_binary(HTML),
    ?assertNotEqual(nomatch, binary:match(HTMLBin, <<"az-key=\"1\"">>)),
    ?assertNotEqual(nomatch, binary:match(HTMLBin, <<"A<!--/az-->">>)),
    ?assertNotEqual(nomatch, binary:match(HTMLBin, <<"az-key=\"2\"">>)),
    ?assertNotEqual(nomatch, binary:match(HTMLBin, <<"B<!--/az-->">>)).

stream_ssr(Config) when is_list(Config) ->
    %% SSR path produces correct HTML for stream
    Items = [#{id => 1, text => <<"A">>}],
    HTML = iolist_to_binary(
        arizona_render:render_view_to_iolist(
            arizona_todo,
            arizona_req_test_adapter:new(#{}),
            #{bindings => #{items => Items}}
        )
    ),
    ?assertNotEqual(nomatch, binary:match(HTML, <<"az-key=\"1\"">>)),
    ?assertNotEqual(nomatch, binary:match(HTML, <<"A<!--/az-->">>)).

stream_diff_no_change(Config) when is_list(Config) ->
    %% No ops when stream unchanged
    {B, _} = arizona_todo:mount(
        #{items => [#{id => 1, text => <<"A">>}]}, arizona_req_test_adapter:new(#{})
    ),
    Tmpl0 = arizona_todo:render(B),
    {_, Snap0, V0} = arizona_render:render(Tmpl0, #{}),
    %% Same bindings, no pending ops -- clear pending first
    B1 = arizona_stream:clear_stream_pending(B, arizona_stream:stream_keys(B)),
    Tmpl1 = arizona_todo:render(B1),
    Changed = #{},
    {Ops, _, _} = arizona_diff:diff(Tmpl1, Snap0, V0, Changed),
    ?assertEqual([], Ops).

stream_diff_insert(Config) when is_list(Config) ->
    %% OP_INSERT at correct position
    {B0, _} = arizona_todo:mount(#{}, arizona_req_test_adapter:new(#{})),
    Tmpl0 = arizona_todo:render(B0),
    {_, Snap0, V0} = arizona_render:render(Tmpl0, #{}),
    B1 = arizona_stream:clear_stream_pending(B0, arizona_stream:stream_keys(B0)),
    %% Add an item
    {B2, _, _} = arizona_todo:handle_event(
        <<"add">>, #{<<"id">> => 1, <<"text">> => <<"First">>}, B1
    ),
    Tmpl1 = arizona_todo:render(B2),
    Changed = arizona_live_compute_changed(B1, B2),
    {Ops, _, _} = arizona_diff:diff(Tmpl1, Snap0, V0, Changed),
    ?assertMatch([[?OP_INSERT, _, <<"1">>, -1, _]], Ops),
    [[_, _, _, _, Payload]] = Ops,
    ?assert(is_binary(maps:get(<<"f">>, Payload))),
    %% Statics contain fingerprint-scoped az values
    StaticsBin = iolist_to_binary(maps:get(<<"s">>, Payload)),
    %% az-key is now in dynamics (full attr string), not baked into statics
    ?assertNotEqual(nomatch, binary:match(StaticsBin, <<"<!--/az--></li>">>)),
    ?assertEqual([<<" az-key=\"1\"">>, <<"First">>], maps:get(<<"d">>, Payload)).

stream_diff_remove(Config) when is_list(Config) ->
    %% OP_REMOVE with correct key
    Items = [#{id => 1, text => <<"A">>}, #{id => 2, text => <<"B">>}],
    {B0, _} = arizona_todo:mount(#{items => Items}, arizona_req_test_adapter:new(#{})),
    Tmpl0 = arizona_todo:render(B0),
    {_, Snap0, V0} = arizona_render:render(Tmpl0, #{}),
    B1 = arizona_stream:clear_stream_pending(B0, arizona_stream:stream_keys(B0)),
    %% Remove first item
    {B2, _, _} = arizona_todo:handle_event(<<"remove">>, #{<<"id">> => 1}, B1),
    Tmpl1 = arizona_todo:render(B2),
    Changed = arizona_live_compute_changed(B1, B2),
    {Ops, _, _} = arizona_diff:diff(Tmpl1, Snap0, V0, Changed),
    ?assertMatch([[?OP_REMOVE, _, <<"1">>]], Ops).

stream_diff_update(Config) when is_list(Config) ->
    %% OP_ITEM_PATCH with inner TEXT op
    Items = [#{id => 1, text => <<"Old">>}],
    {B0, _} = arizona_todo:mount(#{items => Items}, arizona_req_test_adapter:new(#{})),
    Tmpl0 = arizona_todo:render(B0),
    {_, Snap0, V0} = arizona_render:render(Tmpl0, #{}),
    B1 = arizona_stream:clear_stream_pending(B0, arizona_stream:stream_keys(B0)),
    %% Update item text
    {B2, _, _} = arizona_todo:handle_event(
        <<"update">>, #{<<"id">> => 1, <<"text">> => <<"New">>}, B1
    ),
    Tmpl1 = arizona_todo:render(B2),
    Changed = arizona_live_compute_changed(B1, B2),
    {Ops, _, _} = arizona_diff:diff(Tmpl1, Snap0, V0, Changed),
    ?assertMatch([[?OP_ITEM_PATCH, _, <<"1">>, _]], Ops),
    [[_, _, _, InnerOps]] = Ops,
    ?assertMatch([[?OP_TEXT, _, <<"New">>]], InnerOps).

stream_diff_reset(Config) when is_list(Config) ->
    %% OP_REMOVE for each old + OP_INSERT for each new (if any)
    Items = [#{id => 1, text => <<"A">>}, #{id => 2, text => <<"B">>}],
    {B0, _} = arizona_todo:mount(#{items => Items}, arizona_req_test_adapter:new(#{})),
    Tmpl0 = arizona_todo:render(B0),
    {_, Snap0, V0} = arizona_render:render(Tmpl0, #{}),
    B1 = arizona_stream:clear_stream_pending(B0, arizona_stream:stream_keys(B0)),
    %% Clear all
    {B2, _, _} = arizona_todo:handle_event(<<"clear">>, #{}, B1),
    Tmpl1 = arizona_todo:render(B2),
    Changed = arizona_live_compute_changed(B1, B2),
    {Ops, _, _} = arizona_diff:diff(Tmpl1, Snap0, V0, Changed),
    ?assertEqual(2, length(Ops)),
    ?assertMatch([?OP_REMOVE, _, <<"1">>], lists:nth(1, Ops)),
    ?assertMatch([?OP_REMOVE, _, <<"2">>], lists:nth(2, Ops)).

stream_diff_mixed(Config) when is_list(Config) ->
    %% insert + delete + update in one event cycle
    Items = [#{id => 1, text => <<"A">>}, #{id => 2, text => <<"B">>}],
    {B0, _} = arizona_todo:mount(#{items => Items}, arizona_req_test_adapter:new(#{})),
    Tmpl0 = arizona_todo:render(B0),
    {_, Snap0, V0} = arizona_render:render(Tmpl0, #{}),
    B1 = arizona_stream:clear_stream_pending(B0, arizona_stream:stream_keys(B0)),
    %% Manually mutate: delete id=1, update id=2, insert id=3
    S0 = maps:get(items, B1),
    S1 = arizona_stream:delete(S0, 1),
    S2 = arizona_stream:update(S1, 2, #{id => 2, text => <<"B2">>}),
    S3 = arizona_stream:insert(S2, #{id => 3, text => <<"C">>}),
    B2 = B1#{items => S3},
    Tmpl1 = arizona_todo:render(B2),
    Changed = arizona_live_compute_changed(B1, B2),
    {Ops, _, _} = arizona_diff:diff(Tmpl1, Snap0, V0, Changed),
    %% Expect: delete 1, patch 2, insert 3
    ?assertEqual(3, length(Ops)),
    ?assertMatch([?OP_REMOVE, _, <<"1">>], lists:nth(1, Ops)),
    ?assertMatch([?OP_ITEM_PATCH, _, <<"2">>, _], lists:nth(2, Ops)),
    ?assertMatch([?OP_INSERT, _, <<"3">>, -1, _], lists:nth(3, Ops)),
    %% Key dynamic unchanged (id=2), only text changed
    [_, _, _, InnerOps] = lists:nth(2, Ops),
    ?assertMatch([[?OP_TEXT, _, <<"B2">>]], InnerOps).

stream_deps_skip(Config) when is_list(Config) ->
    %% diff/4 skips stream when deps unchanged
    Items = [#{id => 1, text => <<"A">>}],
    {B0, _} = arizona_todo:mount(#{items => Items}, arizona_req_test_adapter:new(#{})),
    Tmpl0 = arizona_todo:render(B0),
    {_, Snap0, V0} = arizona_render:render(Tmpl0, #{}),
    B1 = arizona_stream:clear_stream_pending(B0, arizona_stream:stream_keys(B0)),
    %% No changes to items binding
    Tmpl1 = arizona_todo:render(B1),
    %% nothing changed
    Changed = #{},
    {Ops, _, _} = arizona_diff:diff(Tmpl1, Snap0, V0, Changed),
    ?assertEqual([], Ops).

%% Update with an item identical to the existing one -- Changed is empty,
%% the per-item skipping renderer short-circuits, no closure runs, no ops.
stream_update_identical_emits_no_ops(Config) when is_list(Config) ->
    Items = [#{id => 1, text => <<"A">>}],
    {B0, _} = arizona_todo:mount(#{items => Items}, arizona_req_test_adapter:new(#{})),
    Tmpl0 = arizona_todo:render(B0),
    {_, Snap0, V0} = arizona_render:render(Tmpl0, #{}),
    B1 = arizona_stream:clear_stream_pending(B0, arizona_stream:stream_keys(B0)),
    %% Update with a value-equal item.
    {B2, _, _} = arizona_todo:handle_event(
        <<"update">>, #{<<"id">> => 1, <<"text">> => <<"A">>}, B1
    ),
    Tmpl1 = arizona_todo:render(B2),
    Changed = arizona_live_compute_changed(B1, B2),
    {Ops, _, _} = arizona_diff:diff(Tmpl1, Snap0, V0, Changed),
    ?assertEqual([], Ops).

%% =============================================================================
%% 12b. Stream edge case tests (unit-level)
%% =============================================================================

stream_insert_at_position(Config) when is_list(Config) ->
    %% stream_insert/3 at position 0 (beginning)
    Items = [#{id => 1, text => <<"A">>}],
    {B0, _} = arizona_todo:mount(#{items => Items}, arizona_req_test_adapter:new(#{})),
    Tmpl0 = arizona_todo:render(B0),
    {_, Snap0, V0} = arizona_render:render(Tmpl0, #{}),
    B1 = arizona_stream:clear_stream_pending(B0, arizona_stream:stream_keys(B0)),
    %% Insert at position 0
    S = arizona_stream:insert(maps:get(items, B1), #{id => 2, text => <<"B">>}, 0),
    B2 = B1#{items => S},
    Tmpl1 = arizona_todo:render(B2),
    Changed = arizona_live_compute_changed(B1, B2),
    {Ops, _, _} = arizona_diff:diff(Tmpl1, Snap0, V0, Changed),
    ?assertMatch([[?OP_INSERT, _, <<"2">>, 0, _]], Ops).

stream_insert_at_middle_position(Config) when is_list(Config) ->
    %% 2 initial items, insert at position 1
    Items = [#{id => 1, text => <<"A">>}, #{id => 2, text => <<"B">>}],
    {B0, _} = arizona_todo:mount(#{items => Items}, arizona_req_test_adapter:new(#{})),
    Tmpl0 = arizona_todo:render(B0),
    {_, Snap0, V0} = arizona_render:render(Tmpl0, #{}),
    B1 = arizona_stream:clear_stream_pending(B0, arizona_stream:stream_keys(B0)),
    %% Insert at position 1 (between items 1 and 2)
    S = arizona_stream:insert(maps:get(items, B1), #{id => 3, text => <<"C">>}, 1),
    B2 = B1#{items => S},
    Tmpl1 = arizona_todo:render(B2),
    Changed = arizona_live_compute_changed(B1, B2),
    {Ops, Snap1, _} = arizona_diff:diff(Tmpl1, Snap0, V0, Changed),
    ?assertMatch([[?OP_INSERT, _, <<"3">>, 1, _]], Ops),
    %% Verify snapshot item ordering is [1, 3, 2]
    [{_, #{order := SnapOrder}}] = [
        {Az, V}
     || {Az, V} <- maps:get(d, Snap1), is_map(V), maps:is_key(order, V)
    ],
    ?assertEqual([1, 3, 2], SnapOrder).

stream_reset_with_new_items(Config) when is_list(Config) ->
    %% 2 initial items, stream_reset/2 with 2 new items
    Items = [#{id => 1, text => <<"A">>}, #{id => 2, text => <<"B">>}],
    {B0, _} = arizona_todo:mount(#{items => Items}, arizona_req_test_adapter:new(#{})),
    Tmpl0 = arizona_todo:render(B0),
    {_, Snap0, V0} = arizona_render:render(Tmpl0, #{}),
    B1 = arizona_stream:clear_stream_pending(B0, arizona_stream:stream_keys(B0)),
    %% Reset with new items
    S = arizona_stream:reset(maps:get(items, B1), [
        #{id => 3, text => <<"C">>},
        #{id => 4, text => <<"D">>}
    ]),
    B2 = B1#{items => S},
    Tmpl1 = arizona_todo:render(B2),
    Changed = arizona_live_compute_changed(B1, B2),
    {Ops, Snap1, _} = arizona_diff:diff(Tmpl1, Snap0, V0, Changed),
    %% 2 removes (old) + 2 inserts (new)
    RemOps = [Op || [?OP_REMOVE | _] = Op <- Ops],
    InsOps = [Op || [?OP_INSERT | _] = Op <- Ops],
    ?assertEqual(2, length(RemOps)),
    ?assertEqual(2, length(InsOps)),
    %% Snapshot has only new item keys
    [{_, #{order := SnapOrder}}] = [
        {Az, V}
     || {Az, V} <- maps:get(d, Snap1), is_map(V), maps:is_key(order, V)
    ],
    ?assertEqual([3, 4], SnapOrder).

stream_reset_overlap_same_content(Config) when is_list(Config) ->
    %% Old=[1,2,3], New=[2,3,4] -- keys 2,3 overlap with same content
    %% Expect: 1 remove (key 1), 0 patches, 1 insert (key 4)
    Items = [
        #{id => 1, text => <<"A">>},
        #{id => 2, text => <<"B">>},
        #{id => 3, text => <<"C">>}
    ],
    {B0, _} = arizona_todo:mount(#{items => Items}, arizona_req_test_adapter:new(#{})),
    Tmpl0 = arizona_todo:render(B0),
    {_, Snap0, V0} = arizona_render:render(Tmpl0, #{}),
    B1 = arizona_stream:clear_stream_pending(B0, arizona_stream:stream_keys(B0)),
    S = arizona_stream:reset(
        maps:get(items, B1),
        [
            #{id => 2, text => <<"B">>},
            #{id => 3, text => <<"C">>},
            #{id => 4, text => <<"D">>}
        ]
    ),
    B2 = B1#{items => S},
    Tmpl1 = arizona_todo:render(B2),
    Changed = arizona_live_compute_changed(B1, B2),
    {Ops, _, _} = arizona_diff:diff(Tmpl1, Snap0, V0, Changed),
    RemOps = [Op || [?OP_REMOVE | _] = Op <- Ops],
    InsOps = [Op || [?OP_INSERT | _] = Op <- Ops],
    PatchOps = [Op || [?OP_ITEM_PATCH | _] = Op <- Ops],
    ?assertEqual(1, length(RemOps)),
    ?assertMatch([[?OP_REMOVE, _, <<"1">>]], RemOps),
    ?assertEqual(0, length(PatchOps)),
    ?assertEqual(1, length(InsOps)),
    ?assertMatch([[?OP_INSERT, _, <<"4">>, -1, _]], InsOps).

stream_reset_overlap_changed_content(Config) when is_list(Config) ->
    %% Old=[1,2], New=[2(modified),3] -- key 2 overlaps but content changed
    %% Expect: 1 remove (key 1), 1 OP_ITEM_PATCH (key 2), 1 insert (key 3)
    Items = [#{id => 1, text => <<"A">>}, #{id => 2, text => <<"B">>}],
    {B0, _} = arizona_todo:mount(#{items => Items}, arizona_req_test_adapter:new(#{})),
    Tmpl0 = arizona_todo:render(B0),
    {_, Snap0, V0} = arizona_render:render(Tmpl0, #{}),
    B1 = arizona_stream:clear_stream_pending(B0, arizona_stream:stream_keys(B0)),
    S = arizona_stream:reset(
        maps:get(items, B1),
        [
            #{id => 2, text => <<"B2">>},
            #{id => 3, text => <<"C">>}
        ]
    ),
    B2 = B1#{items => S},
    Tmpl1 = arizona_todo:render(B2),
    Changed = arizona_live_compute_changed(B1, B2),
    {Ops, _, _} = arizona_diff:diff(Tmpl1, Snap0, V0, Changed),
    RemOps = [Op || [?OP_REMOVE | _] = Op <- Ops],
    InsOps = [Op || [?OP_INSERT | _] = Op <- Ops],
    PatchOps = [Op || [?OP_ITEM_PATCH | _] = Op <- Ops],
    ?assertEqual(1, length(RemOps)),
    ?assertMatch([[?OP_REMOVE, _, <<"1">>]], RemOps),
    ?assertEqual(1, length(PatchOps)),
    ?assertMatch([[?OP_ITEM_PATCH, _, <<"2">>, _]], PatchOps),
    ?assertEqual(1, length(InsOps)),
    ?assertMatch([[?OP_INSERT, _, <<"3">>, -1, _]], InsOps).

stream_reset_full_overlap(Config) when is_list(Config) ->
    %% Old=[1,2], New=[1,2] same items -- expect 0 ops
    Items = [#{id => 1, text => <<"A">>}, #{id => 2, text => <<"B">>}],
    {B0, _} = arizona_todo:mount(#{items => Items}, arizona_req_test_adapter:new(#{})),
    Tmpl0 = arizona_todo:render(B0),
    {_, Snap0, V0} = arizona_render:render(Tmpl0, #{}),
    B1 = arizona_stream:clear_stream_pending(B0, arizona_stream:stream_keys(B0)),
    S = arizona_stream:reset(
        maps:get(items, B1),
        [
            #{id => 1, text => <<"A">>},
            #{id => 2, text => <<"B">>}
        ]
    ),
    B2 = B1#{items => S},
    Tmpl1 = arizona_todo:render(B2),
    Changed = arizona_live_compute_changed(B1, B2),
    {Ops, _, _} = arizona_diff:diff(Tmpl1, Snap0, V0, Changed),
    ?assertEqual([], Ops).

stream_update_no_change(Config) when is_list(Config) ->
    %% 1 initial item, stream_update/3 with identical data → Ops = []
    Items = [#{id => 1, text => <<"A">>}],
    {B0, _} = arizona_todo:mount(#{items => Items}, arizona_req_test_adapter:new(#{})),
    Tmpl0 = arizona_todo:render(B0),
    {_, Snap0, V0} = arizona_render:render(Tmpl0, #{}),
    B1 = arizona_stream:clear_stream_pending(B0, arizona_stream:stream_keys(B0)),
    %% Update with identical data
    S = arizona_stream:update(maps:get(items, B1), 1, #{id => 1, text => <<"A">>}),
    B2 = B1#{items => S},
    Tmpl1 = arizona_todo:render(B2),
    Changed = arizona_live_compute_changed(B1, B2),
    {Ops, _, _} = arizona_diff:diff(Tmpl1, Snap0, V0, Changed),
    ?assertEqual([], Ops).

stream_snapshot_after_multiple_cycles(Config) when is_list(Config) ->
    %% 3 sequential diff cycles: insert 2 items → update item 1 → delete item 2
    {B0, _} = arizona_todo:mount(#{}, arizona_req_test_adapter:new(#{})),
    Tmpl0 = arizona_todo:render(B0),
    {_, Snap0, V0} = arizona_render:render(Tmpl0, #{}),
    B1 = arizona_stream:clear_stream_pending(B0, arizona_stream:stream_keys(B0)),
    %% Cycle 1: insert 2 items
    S1a = arizona_stream:insert(maps:get(items, B1), #{id => 1, text => <<"A">>}),
    S1b = arizona_stream:insert(S1a, #{id => 2, text => <<"B">>}),
    B2 = B1#{items => S1b},
    Tmpl1 = arizona_todo:render(B2),
    Changed1 = arizona_live_compute_changed(B1, B2),
    {Ops1, Snap1, V1} = arizona_diff:diff(Tmpl1, Snap0, V0, Changed1),
    ?assertEqual(2, length([Op || [?OP_INSERT | _] = Op <- Ops1])),
    B3 = arizona_stream:clear_stream_pending(B2, arizona_stream:stream_keys(B2)),
    %% Cycle 2: update item 1
    S2 = arizona_stream:update(maps:get(items, B3), 1, #{id => 1, text => <<"A2">>}),
    B4 = B3#{items => S2},
    Tmpl2 = arizona_todo:render(B4),
    Changed2 = arizona_live_compute_changed(B3, B4),
    {Ops2, Snap2, V2} = arizona_diff:diff(Tmpl2, Snap1, V1, Changed2),
    ?assertMatch([[?OP_ITEM_PATCH, _, _, _]], Ops2),
    B5 = arizona_stream:clear_stream_pending(B4, arizona_stream:stream_keys(B4)),
    %% Cycle 3: delete item 2
    S3 = arizona_stream:delete(maps:get(items, B5), 2),
    B6 = B5#{items => S3},
    Tmpl3 = arizona_todo:render(B6),
    Changed3 = arizona_live_compute_changed(B5, B6),
    {Ops3, Snap3, _} = arizona_diff:diff(Tmpl3, Snap2, V2, Changed3),
    ?assertMatch([[?OP_REMOVE, _, <<"2">>]], Ops3),
    %% Final snapshot has item 1 with updated text "A2"
    [{_, #{items := FinalItems, order := FinalOrder}}] = [
        {Az, V}
     || {Az, V} <- maps:get(d, Snap3), is_map(V), maps:is_key(order, V)
    ],
    ?assertEqual([1], FinalOrder),
    #{1 := ItemD} = FinalItems,
    [{_, {attr, <<"az-key">>, 1}, _}, {_, <<"A2">>, _}] = ItemD.

stream_empty_pending_deps_changed(Config) when is_list(Config) ->
    %% Mount with 1 item, clear pending, re-render with same stream but Changed = #{items => true}
    %% Exercises Bug 4 fix (empty pending passthrough) and Bug 1 fix
    Items = [#{id => 1, text => <<"A">>}],
    {B0, _} = arizona_todo:mount(#{items => Items}, arizona_req_test_adapter:new(#{})),
    Tmpl0 = arizona_todo:render(B0),
    {_, Snap0, V0} = arizona_render:render(Tmpl0, #{}),
    B1 = arizona_stream:clear_stream_pending(B0, arizona_stream:stream_keys(B0)),
    %% Re-render with same bindings, but force deps changed
    Tmpl1 = arizona_todo:render(B1),
    Changed = #{items => true},
    {Ops, _, _} = arizona_diff:diff(Tmpl1, Snap0, V0, Changed),
    ?assertEqual([], Ops).

stream_move(Config) when is_list(Config) ->
    %% Move item from position 2 to position 0
    Items = [#{id => 1, text => <<"A">>}, #{id => 2, text => <<"B">>}, #{id => 3, text => <<"C">>}],
    {B0, _} = arizona_todo:mount(#{items => Items}, arizona_req_test_adapter:new(#{})),
    Tmpl0 = arizona_todo:render(B0),
    {_, Snap0, V0} = arizona_render:render(Tmpl0, #{}),
    B1 = arizona_stream:clear_stream_pending(B0, arizona_stream:stream_keys(B0)),
    %% Move item 3 to position 0
    S = arizona_stream:move(maps:get(items, B1), 3, 0),
    B2 = B1#{items => S},
    Tmpl1 = arizona_todo:render(B2),
    Changed = arizona_live_compute_changed(B1, B2),
    {Ops, _, _} = arizona_diff:diff(Tmpl1, Snap0, V0, Changed),
    %% Should produce a single OP_MOVE with after_key null (3 is first in new order [3,1,2])
    ?assertMatch([[?OP_MOVE, _, <<"3">>, null]], Ops).

stream_move_nonexistent_key(Config) when is_list(Config) ->
    %% Move with nonexistent key is a no-op
    Items = [#{id => 1, text => <<"A">>}],
    {B0, _} = arizona_todo:mount(#{items => Items}, arizona_req_test_adapter:new(#{})),
    Tmpl0 = arizona_todo:render(B0),
    {_, Snap0, V0} = arizona_render:render(Tmpl0, #{}),
    B1 = arizona_stream:clear_stream_pending(B0, arizona_stream:stream_keys(B0)),
    %% Move nonexistent key 99
    S = arizona_stream:move(maps:get(items, B1), 99, 0),
    B2 = B1#{items => S},
    Tmpl1 = arizona_todo:render(B2),
    Changed = arizona_live_compute_changed(B1, B2),
    {Ops, _, _} = arizona_diff:diff(Tmpl1, Snap0, V0, Changed),
    ?assertEqual([], Ops).

%% =============================================================================
%% 13. Stream E2E tests (via arizona_live)
%% =============================================================================

stream_live_insert(Config) when is_list(Config) ->
    %% mount, add item, assert OP_INSERT with fingerprint payload
    {ok, Pid} = arizona_live:start_link(
        arizona_todo, #{}, undefined, [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    {ok, Ops, []} = arizona_live:handle_event(
        Pid,
        <<"todo">>,
        <<"add">>,
        #{<<"id">> => 1, <<"text">> => <<"First">>}
    ),
    ?assertMatch([[?OP_INSERT, _, <<"1">>, -1, _]], Ops),
    [[_, _, _, _, Payload]] = Ops,
    %% First insert includes statics (fingerprint not yet sent)
    ?assertMatch(#{<<"f">> := _, <<"s">> := _, <<"d">> := _}, Payload),
    ?assert(is_binary(maps:get(<<"f">>, Payload))),
    ?assertEqual([<<" az-key=\"1\"">>, <<"First">>], maps:get(<<"d">>, Payload)).

stream_live_remove(Config) when is_list(Config) ->
    %% add items, remove one, assert OP_REMOVE
    {ok, Pid} = arizona_live:start_link(
        arizona_todo, #{}, undefined, [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    {ok, _, _} = arizona_live:handle_event(
        Pid,
        <<"todo">>,
        <<"add">>,
        #{<<"id">> => 1, <<"text">> => <<"A">>}
    ),
    {ok, _, _} = arizona_live:handle_event(
        Pid,
        <<"todo">>,
        <<"add">>,
        #{<<"id">> => 2, <<"text">> => <<"B">>}
    ),
    {ok, Ops, []} = arizona_live:handle_event(
        Pid,
        <<"todo">>,
        <<"remove">>,
        #{<<"id">> => 1}
    ),
    ?assertMatch([[?OP_REMOVE, _, <<"1">>]], Ops).

stream_live_update(Config) when is_list(Config) ->
    %% add item, update text, assert OP_ITEM_PATCH
    {ok, Pid} = arizona_live:start_link(
        arizona_todo, #{}, undefined, [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    {ok, _, _} = arizona_live:handle_event(
        Pid,
        <<"todo">>,
        <<"add">>,
        #{<<"id">> => 1, <<"text">> => <<"Old">>}
    ),
    {ok, Ops, []} = arizona_live:handle_event(
        Pid,
        <<"todo">>,
        <<"update">>,
        #{<<"id">> => 1, <<"text">> => <<"New">>}
    ),
    ?assertMatch([[?OP_ITEM_PATCH, _, <<"1">>, _]], Ops),
    [[_, _, _, InnerOps]] = Ops,
    ?assertMatch([[?OP_TEXT, _, <<"New">>]], InnerOps).

stream_live_reset(Config) when is_list(Config) ->
    %% add items, clear, assert OP_REMOVE for each
    {ok, Pid} = arizona_live:start_link(
        arizona_todo, #{}, undefined, [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    {ok, _, _} = arizona_live:handle_event(
        Pid,
        <<"todo">>,
        <<"add">>,
        #{<<"id">> => 1, <<"text">> => <<"A">>}
    ),
    {ok, _, _} = arizona_live:handle_event(
        Pid,
        <<"todo">>,
        <<"add">>,
        #{<<"id">> => 2, <<"text">> => <<"B">>}
    ),
    {ok, Ops, []} = arizona_live:handle_event(Pid, <<"todo">>, <<"clear">>, #{}),
    ?assertEqual(2, length(Ops)),
    ?assertMatch([?OP_REMOVE, _, <<"1">>], lists:nth(1, Ops)),
    ?assertMatch([?OP_REMOVE, _, <<"2">>], lists:nth(2, Ops)).

stream_live_multi_event(Config) when is_list(Config) ->
    %% multiple events in sequence, snapshots track correctly
    {ok, Pid} = arizona_live:start_link(
        arizona_todo, #{}, undefined, [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    %% Add three items
    {ok, Ops1, _} = arizona_live:handle_event(
        Pid,
        <<"todo">>,
        <<"add">>,
        #{<<"id">> => 1, <<"text">> => <<"A">>}
    ),
    ?assertMatch([[?OP_INSERT, _, _, _, _]], Ops1),
    {ok, Ops2, _} = arizona_live:handle_event(
        Pid,
        <<"todo">>,
        <<"add">>,
        #{<<"id">> => 2, <<"text">> => <<"B">>}
    ),
    ?assertMatch([[?OP_INSERT, _, _, _, _]], Ops2),
    {ok, Ops3, _} = arizona_live:handle_event(
        Pid,
        <<"todo">>,
        <<"add">>,
        #{<<"id">> => 3, <<"text">> => <<"C">>}
    ),
    ?assertMatch([[?OP_INSERT, _, _, _, _]], Ops3),
    %% Remove middle item
    {ok, Ops4, _} = arizona_live:handle_event(
        Pid,
        <<"todo">>,
        <<"remove">>,
        #{<<"id">> => 2}
    ),
    ?assertMatch([[?OP_REMOVE, _, <<"2">>]], Ops4),
    %% Update remaining item
    {ok, Ops5, _} = arizona_live:handle_event(
        Pid,
        <<"todo">>,
        <<"update">>,
        #{<<"id">> => 1, <<"text">> => <<"A2">>}
    ),
    ?assertMatch([[?OP_ITEM_PATCH, _, <<"1">>, _]], Ops5).

%% =============================================================================
%% 14. Page todo E2E tests (arizona_page with todo section)
%% =============================================================================

page_add_todo_live(Config) when is_list(Config) ->
    %% add_todo event produces OP_INSERT with fingerprint payload
    {ok, Pid} = arizona_live:start_link(
        arizona_page, #{}, undefined, [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    {ok, Ops, []} = arizona_live:handle_event(Pid, <<"page">>, <<"add_todo">>, #{}),
    ?assertMatch([[?OP_INSERT, _, <<"1">>, -1, _]], Ops),
    [[_, _, _, _, Payload]] = Ops,
    ?assertMatch(#{<<"f">> := _, <<"s">> := _, <<"d">> := _}, Payload),
    ?assert(is_binary(maps:get(<<"f">>, Payload))),
    ?assertEqual(
        [
            <<" az-key=\"1\"">>,
            <<" az-focusout=\"[0,&quot;update_todo&quot;,{&quot;id&quot;:1}]\"">>,
            <<
                " az-keydown=\"[16,[&quot;enter&quot;],"
                "[0,&quot;update_todo&quot;,{&quot;id&quot;:1}]]\""
            >>,
            <<" value=\"Todo 1\"">>,
            <<" az-click=\"[0,&quot;remove_todo&quot;,{&quot;id&quot;:1}]\"">>
        ],
        maps:get(<<"d">>, Payload)
    ).

page_remove_todo_live(Config) when is_list(Config) ->
    %% remove_todo produces OP_REMOVE
    {ok, Pid} = arizona_live:start_link(
        arizona_page, #{}, undefined, [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    {ok, _, _} = arizona_live:handle_event(Pid, <<"page">>, <<"add_todo">>, #{}),
    {ok, _, _} = arizona_live:handle_event(Pid, <<"page">>, <<"add_todo">>, #{}),
    %% Remove first todo (id=1)
    {ok, Ops, []} = arizona_live:handle_event(
        Pid,
        <<"page">>,
        <<"remove_todo">>,
        #{<<"id">> => 1}
    ),
    ?assertMatch([[?OP_REMOVE, _, <<"1">>]], Ops).

page_clear_todos_live(Config) when is_list(Config) ->
    %% clear_todos produces OP_REMOVE for each
    {ok, Pid} = arizona_live:start_link(
        arizona_page, #{}, undefined, [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    {ok, _, _} = arizona_live:handle_event(Pid, <<"page">>, <<"add_todo">>, #{}),
    {ok, _, _} = arizona_live:handle_event(Pid, <<"page">>, <<"add_todo">>, #{}),
    {ok, Ops, []} = arizona_live:handle_event(Pid, <<"page">>, <<"clear_todos">>, #{}),
    ?assertEqual(2, length(Ops)),
    ?assertMatch([?OP_REMOVE, _, <<"1">>], lists:nth(1, Ops)),
    ?assertMatch([?OP_REMOVE, _, <<"2">>], lists:nth(2, Ops)).

stream_live_mount_with_items_then_insert(Config) when is_list(Config) ->
    %% Mount with 1 initial item, then add a new item
    %% Only 1 OP_INSERT for the new item (no stale inserts from mount)
    InitItems = [#{id => 1, text => <<"First">>}],
    {ok, Pid} = arizona_live:start_link(
        arizona_todo, #{items => InitItems}, undefined, [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    {ok, Ops, []} = arizona_live:handle_event(
        Pid,
        <<"todo">>,
        <<"add">>,
        #{<<"id">> => 2, <<"text">> => <<"Second">>}
    ),
    InsOps = [Op || [?OP_INSERT | _] = Op <- Ops],
    ?assertMatch([[?OP_INSERT, _, <<"2">>, -1, _]], InsOps).

stream_live_mount_with_items_then_delete(Config) when is_list(Config) ->
    %% Mount with 2 initial items, then remove one
    InitItems = [#{id => 1, text => <<"A">>}, #{id => 2, text => <<"B">>}],
    {ok, Pid} = arizona_live:start_link(
        arizona_todo, #{items => InitItems}, undefined, [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    {ok, Ops, []} = arizona_live:handle_event(Pid, <<"todo">>, <<"remove">>, #{<<"id">> => 1}),
    ?assertMatch([[?OP_REMOVE, _, <<"1">>]], Ops),
    %% No OP_INSERT ops
    InsOps = [Op || [?OP_INSERT | _] = Op <- Ops],
    ?assertEqual([], InsOps).

stream_live_mount_with_items_then_reset(Config) when is_list(Config) ->
    %% Mount with 2 initial items, then clear
    InitItems = [#{id => 1, text => <<"A">>}, #{id => 2, text => <<"B">>}],
    {ok, Pid} = arizona_live:start_link(
        arizona_todo, #{items => InitItems}, undefined, [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    {ok, Ops, []} = arizona_live:handle_event(Pid, <<"todo">>, <<"clear">>, #{}),
    RemOps = [Op || [?OP_REMOVE | _] = Op <- Ops],
    InsOps = [Op || [?OP_INSERT | _] = Op <- Ops],
    ?assertEqual(2, length(RemOps)),
    ?assertEqual([], InsOps).

stream_live_delete_last_then_readd(Config) when is_list(Config) ->
    %% Add 1 item, delete it (empty stream), add another -- recovery from emptiness
    {ok, Pid} = arizona_live:start_link(
        arizona_todo, #{}, undefined, [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    {ok, _, _} = arizona_live:handle_event(
        Pid,
        <<"todo">>,
        <<"add">>,
        #{<<"id">> => 1, <<"text">> => <<"A">>}
    ),
    {ok, _, _} = arizona_live:handle_event(Pid, <<"todo">>, <<"remove">>, #{<<"id">> => 1}),
    %% Stream is now empty -- add another item
    {ok, Ops, []} = arizona_live:handle_event(
        Pid,
        <<"todo">>,
        <<"add">>,
        #{<<"id">> => 2, <<"text">> => <<"B">>}
    ),
    ?assertMatch([[?OP_INSERT, _, <<"2">>, -1, _]], Ops).

stream_live_update_no_change(Config) when is_list(Config) ->
    %% Add item, update with identical data → Ops = []
    {ok, Pid} = arizona_live:start_link(
        arizona_todo, #{}, undefined, [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    {ok, _, _} = arizona_live:handle_event(
        Pid,
        <<"todo">>,
        <<"add">>,
        #{<<"id">> => 1, <<"text">> => <<"A">>}
    ),
    {ok, Ops, []} = arizona_live:handle_event(
        Pid,
        <<"todo">>,
        <<"update">>,
        #{<<"id">> => 1, <<"text">> => <<"A">>}
    ),
    ?assertEqual([], Ops).

stream_live_three_sequential_inserts(Config) when is_list(Config) ->
    %% 3 sequential inserts, each must produce exactly 1 OP_INSERT
    {ok, Pid} = arizona_live:start_link(
        arizona_todo, #{}, undefined, [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    {ok, Ops1, _} = arizona_live:handle_event(
        Pid,
        <<"todo">>,
        <<"add">>,
        #{<<"id">> => 1, <<"text">> => <<"A">>}
    ),
    ?assertMatch([[?OP_INSERT, _, <<"1">>, -1, _]], Ops1),
    {ok, Ops2, _} = arizona_live:handle_event(
        Pid,
        <<"todo">>,
        <<"add">>,
        #{<<"id">> => 2, <<"text">> => <<"B">>}
    ),
    ?assertMatch([[?OP_INSERT, _, <<"2">>, -1, _]], Ops2),
    {ok, Ops3, _} = arizona_live:handle_event(
        Pid,
        <<"todo">>,
        <<"add">>,
        #{<<"id">> => 3, <<"text">> => <<"C">>}
    ),
    ?assertMatch([[?OP_INSERT, _, <<"3">>, -1, _]], Ops3).

stream_live_insert_at_position(Config) when is_list(Config) ->
    %% Add 2 items, insert_at position 0
    {ok, Pid} = arizona_live:start_link(
        arizona_todo, #{}, undefined, [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    {ok, _, _} = arizona_live:handle_event(
        Pid,
        <<"todo">>,
        <<"add">>,
        #{<<"id">> => 1, <<"text">> => <<"A">>}
    ),
    {ok, _, _} = arizona_live:handle_event(
        Pid,
        <<"todo">>,
        <<"add">>,
        #{<<"id">> => 2, <<"text">> => <<"B">>}
    ),
    {ok, Ops, []} = arizona_live:handle_event(
        Pid,
        <<"todo">>,
        <<"insert_at">>,
        #{<<"id">> => 3, <<"text">> => <<"C">>, <<"pos">> => 0}
    ),
    ?assertMatch([[?OP_INSERT, _, <<"3">>, 0, _]], Ops).

stream_live_reset_with_items(Config) when is_list(Config) ->
    %% Add 2 items, reset_with 1 new item
    {ok, Pid} = arizona_live:start_link(
        arizona_todo, #{}, undefined, [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    {ok, _, _} = arizona_live:handle_event(
        Pid,
        <<"todo">>,
        <<"add">>,
        #{<<"id">> => 1, <<"text">> => <<"A">>}
    ),
    {ok, _, _} = arizona_live:handle_event(
        Pid,
        <<"todo">>,
        <<"add">>,
        #{<<"id">> => 2, <<"text">> => <<"B">>}
    ),
    {ok, Ops, []} = arizona_live:handle_event(
        Pid,
        <<"todo">>,
        <<"reset_with">>,
        #{<<"items">> => [#{id => 3, text => <<"C">>}]}
    ),
    RemOps = [Op || [?OP_REMOVE | _] = Op <- Ops],
    InsOps = [Op || [?OP_INSERT | _] = Op <- Ops],
    ?assertEqual(2, length(RemOps)),
    ?assertEqual(1, length(InsOps)),
    ?assertMatch([[?OP_INSERT, _, <<"3">>, -1, _]], InsOps).

page_title_change_skips_stream(Config) when is_list(Config) ->
    %% title_change produces only OP_TEXT for title, zero stream ops
    {ok, Pid} = arizona_live:start_link(
        arizona_page, #{}, undefined, [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    %% Add 1 todo first
    {ok, _, _} = arizona_live:handle_event(Pid, <<"page">>, <<"add_todo">>, #{}),
    %% Now change title -- should not produce stream ops
    {ok, Ops, []} = arizona_live:handle_event(Pid, <<"page">>, <<"title_change">>, #{}),
    StreamOps = [
        Op
     || [Code | _] = Op <- Ops,
        Code =:= ?OP_INSERT orelse Code =:= ?OP_REMOVE orelse
            Code =:= ?OP_ITEM_PATCH
    ],
    ?assertEqual([], StreamOps),
    %% Should have OP_TEXT for the title
    TextOps = [Op || [?OP_TEXT | _] = Op <- Ops],
    ?assertMatch([[?OP_TEXT, _, <<"Changed">>]], TextOps).

page_add_todo_then_title_change_then_add_todo(Config) when is_list(Config) ->
    %% Interleave: add_todo → title_change → add_todo
    {ok, Pid} = arizona_live:start_link(
        arizona_page, #{}, undefined, [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    %% Event 1: add_todo
    {ok, Ops1, _} = arizona_live:handle_event(Pid, <<"page">>, <<"add_todo">>, #{}),
    InsOps1 = [Op || [?OP_INSERT | _] = Op <- Ops1],
    ?assertMatch([[?OP_INSERT, _, _, _, _]], InsOps1),
    %% Event 2: title_change -- no stream ops
    {ok, Ops2, _} = arizona_live:handle_event(Pid, <<"page">>, <<"title_change">>, #{}),
    StreamOps2 = [
        Op
     || [Code | _] = Op <- Ops2,
        Code =:= ?OP_INSERT orelse Code =:= ?OP_REMOVE orelse
            Code =:= ?OP_ITEM_PATCH
    ],
    ?assertEqual([], StreamOps2),
    %% Event 3: add_todo again -- stream survives dep-skip cycle
    {ok, Ops3, _} = arizona_live:handle_event(Pid, <<"page">>, <<"add_todo">>, #{}),
    InsOps3 = [Op || [?OP_INSERT | _] = Op <- Ops3],
    ?assertMatch([[?OP_INSERT, _, _, _, _]], InsOps3).

page_connected_event_skips_stream(Config) when is_list(Config) ->
    %% connected event produces only OP_TEXT for status span, zero stream ops
    {ok, Pid} = arizona_live:start_link(
        arizona_page, #{}, self(), [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    %% Drain the arizona_connected push from mount
    receive
        {arizona_push, _, _} -> ok
    after 1000 -> ct:fail(timeout)
    end,
    %% Add 1 todo
    {ok, _, _} = arizona_live:handle_event(Pid, <<"page">>, <<"add_todo">>, #{}),
    %% connected already fired -- no stream ops were emitted during connected push
    ok.

%% =============================================================================
%% 11. Stream bug regression tests
%% =============================================================================

stream_live_mount_with_items(Config) when is_list(Config) ->
    %% Bug 2 regression: mount with initial items via new_stream/2 should clear
    %% pending. A subsequent update event must produce only OP_ITEM_PATCH ops,
    %% not spurious OP_INSERT ops for items already in the DOM.
    InitItems = [
        #{id => <<"a">>, text => <<"Alpha">>},
        #{id => <<"b">>, text => <<"Beta">>}
    ],
    {ok, Pid} = arizona_live:start_link(
        arizona_todo, #{items => InitItems}, undefined, [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    %% Update item "a" -- should only produce OP_ITEM_PATCH, not OP_INSERT
    {ok, Ops, []} = arizona_live:handle_event(
        Pid,
        <<"todo">>,
        <<"update">>,
        #{<<"id">> => <<"a">>, <<"text">> => <<"Alpha2">>}
    ),
    %% No OP_INSERT ops should be present
    InsertOps = [Op || [?OP_INSERT | _] = Op <- Ops],
    ?assertEqual([], InsertOps),
    %% Should have exactly one OP_ITEM_PATCH for key "a"
    PatchOps = [Op || [?OP_ITEM_PATCH | _] = Op <- Ops],
    ?assertMatch([[?OP_ITEM_PATCH, _, <<"a">>, _]], PatchOps).

stream_live_insert_no_stale_pending(Config) when is_list(Config) ->
    %% Bug 3 regression (child event path, but tested on root):
    %% After inserting an item (event 1), a second insert (event 2) should
    %% produce exactly 1 OP_INSERT -- not 2 from stale pending accumulation.
    {ok, Pid} = arizona_live:start_link(
        arizona_todo, #{}, undefined, [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    %% First insert
    {ok, Ops1, []} = arizona_live:handle_event(
        Pid,
        <<"todo">>,
        <<"add">>,
        #{<<"id">> => <<"x">>, <<"text">> => <<"X">>}
    ),
    Insert1 = [Op || [?OP_INSERT | _] = Op <- Ops1],
    ?assertMatch([[?OP_INSERT, _, <<"x">>, -1, _]], Insert1),
    %% Second insert -- must produce exactly 1 OP_INSERT (for "y" only)
    {ok, Ops2, []} = arizona_live:handle_event(
        Pid,
        <<"todo">>,
        <<"add">>,
        #{<<"id">> => <<"y">>, <<"text">> => <<"Y">>}
    ),
    Insert2 = [Op || [?OP_INSERT | _] = Op <- Ops2],
    ?assertMatch([[?OP_INSERT, _, <<"y">>, -1, _]], Insert2).

%% =============================================================================
%% 16. Stream limit + move + new API tests
%% =============================================================================

stream_new_stream_opts(Config) when is_list(Config) ->
    %% new_stream/3 with limit/on_limit options sets fields correctly
    S = arizona_stream:new(
        fun(#{id := Id}) -> Id end,
        [#{id => 1, text => <<"A">>}],
        #{limit => 5, on_limit => drop}
    ),
    %% Verify items and API works
    S2 = arizona_stream:insert(S, #{id => 2, text => <<"B">>}),
    %% Should not crash -- opts carried through
    _ = arizona_stream:delete(S2, 1),
    ok.

stream_keys(Config) when is_list(Config) ->
    %% stream_keys returns keys of bindings that are streams
    S = arizona_stream:new(fun(#{id := Id}) -> Id end),
    Bindings = #{items => S, title => <<"hello">>, count => 42},
    Keys = arizona_stream:stream_keys(Bindings),
    ?assertEqual([items], Keys).

stream_keys_multiple(Config) when is_list(Config) ->
    %% Multiple streams in bindings
    S1 = arizona_stream:new(fun(X) -> X end),
    S2 = arizona_stream:new(fun(X) -> X end),
    Bindings = #{a => S1, b => 42, c => S2},
    Keys = lists:sort(arizona_stream:stream_keys(Bindings)),
    ?assertEqual([a, c], Keys).

stream_keys_empty(Config) when is_list(Config) ->
    %% No streams in bindings
    ?assertEqual([], arizona_stream:stream_keys(#{x => 1, y => 2})).

stream_get_existing_key(Config) when is_list(Config) ->
    S = arizona_stream:new(
        fun(#{id := Id}) -> Id end,
        [#{id => 1, text => <<"A">>}]
    ),
    ?assertEqual(#{id => 1, text => <<"A">>}, arizona_stream:get(S, 1)).

stream_get_missing_key_crashes(Config) when is_list(Config) ->
    S = arizona_stream:new(
        fun(#{id := Id}) -> Id end,
        [#{id => 1, text => <<"A">>}, #{id => 2, text => <<"B">>}]
    ),
    Stack =
        try arizona_stream:get(S, 99) of
            _ -> ct:fail(expected_missing_stream_key)
        catch
            error:missing_stream_key:ST -> ST
        end,
    [{arizona_stream, get, [S, 99], Info} | _] = Stack,
    ?assertEqual(
        #{module => arizona_stream},
        proplists:get_value(error_info, Info)
    ),
    %% format_error/2 yields a sentence with the missing key + available keys.
    #{general := Msg} = arizona_stream:format_error(missing_stream_key, Stack),
    Bin = unicode:characters_to_binary(Msg),
    ?assertNotEqual(nomatch, binary:match(Bin, <<"stream key 99 not found">>)),
    ?assertNotEqual(nomatch, binary:match(Bin, <<"1,2">>)).

%% --- did-you-mean hints in missing_stream_key messages ---

format_error_missing_stream_key_suggests_close_match(Config) when is_list(Config) ->
    %% Binary keys that differ by one character should suggest the close match.
    KeyFun = fun(#{id := Id}) -> Id end,
    S = arizona_stream:new(KeyFun, [
        #{id => <<"alpha">>, n => 1},
        #{id => <<"beta">>, n => 2}
    ]),
    Stack =
        try arizona_stream:get(S, <<"alpah">>) of
            _ -> ct:fail(expected_missing_stream_key)
        catch
            error:missing_stream_key:ST -> ST
        end,
    #{general := Msg} = arizona_stream:format_error(missing_stream_key, Stack),
    Bin = unicode:characters_to_binary(Msg),
    ?assertNotEqual(nomatch, binary:match(Bin, <<"Did you mean">>)),
    ?assertNotEqual(nomatch, binary:match(Bin, <<"alpha">>)).

format_error_missing_stream_key_no_suggestion_for_far_match(Config) when is_list(Config) ->
    KeyFun = fun(#{id := Id}) -> Id end,
    S = arizona_stream:new(KeyFun, [
        #{id => <<"alpha">>, n => 1},
        #{id => <<"beta">>, n => 2}
    ]),
    Stack =
        try arizona_stream:get(S, <<"qwerty">>) of
            _ -> ct:fail(expected_missing_stream_key)
        catch
            error:missing_stream_key:ST -> ST
        end,
    #{general := Msg} = arizona_stream:format_error(missing_stream_key, Stack),
    Bin = unicode:characters_to_binary(Msg),
    ?assertEqual(nomatch, binary:match(Bin, <<"Did you mean">>)).

stream_get3_existing_key(Config) when is_list(Config) ->
    S = arizona_stream:new(
        fun(#{id := Id}) -> Id end,
        [#{id => 1, text => <<"A">>}]
    ),
    ?assertEqual(#{id => 1, text => <<"A">>}, arizona_stream:get(S, 1, default)).

stream_get3_missing_key_returns_default(Config) when is_list(Config) ->
    S = arizona_stream:new(fun(#{id := Id}) -> Id end),
    ?assertEqual(default, arizona_stream:get(S, 99, default)).

stream_get_lazy_existing_key(Config) when is_list(Config) ->
    S = arizona_stream:new(
        fun(#{id := Id}) -> Id end,
        [#{id => 1, text => <<"A">>}]
    ),
    ?assertEqual(
        #{id => 1, text => <<"A">>},
        arizona_stream:get_lazy(S, 1, fun() -> error(should_not_be_called) end)
    ).

stream_get_lazy_missing_key_calls_fun(Config) when is_list(Config) ->
    S = arizona_stream:new(fun(#{id := Id}) -> Id end),
    ?assertEqual(computed, arizona_stream:get_lazy(S, 99, fun() -> computed end)).

stream_limit_halt_render(Config) when is_list(Config) ->
    %% new_stream with limit=2 and 3 items: only 2 rendered in HTML
    KeyFun = fun(#{id := Id}) -> Id end,
    Items = [
        #{id => 1, text => <<"A">>},
        #{id => 2, text => <<"B">>},
        #{id => 3, text => <<"C">>}
    ],
    S = arizona_stream:new(KeyFun, Items, #{limit => 2}),
    Tmpl = #{
        s => [<<"<ul az=\"0\">">>, <<"</ul>">>],
        d => [
            {<<"0">>, fun() ->
                arizona_template:each(
                    S,
                    #{
                        t => 0,
                        s => [
                            <<"<li az-key=\"">>,
                            <<"\" az=\"1\"><!--az:1-->">>,
                            <<"<!--/az--></li>">>
                        ],
                        d => fun(Item, Key) ->
                            [
                                {<<"0">>, fun() -> Key end},
                                {<<"1">>, fun() -> maps:get(text, Item) end}
                            ]
                        end,
                        f => <<"test">>
                    }
                )
            end}
        ],
        f => <<"test">>
    },
    {HTML, _Snap, _Views} = arizona_render:render(Tmpl, #{}),
    HTMLBin = iolist_to_binary(HTML),
    %% Items 1 and 2 are visible, item 3 is not
    ?assertMatch({_, _}, binary:match(HTMLBin, <<"A">>)),
    ?assertMatch({_, _}, binary:match(HTMLBin, <<"B">>)),
    ?assertEqual(nomatch, binary:match(HTMLBin, <<"C">>)).

stream_limit_halt_diff(Config) when is_list(Config) ->
    %% Insert 4th item when limit=3 and 3 already visible → no OP_INSERT (halted)
    KeyFun = fun(#{id := Id}) -> Id end,
    Items = [
        #{id => 1, text => <<"A">>},
        #{id => 2, text => <<"B">>},
        #{id => 3, text => <<"C">>}
    ],
    S0 = arizona_stream:new(KeyFun, Items, #{limit => 3}),
    Bindings0 = #{id => <<"test">>, items => S0},
    Tmpl0 = #{
        s => [<<"<ul az=\"0\">">>, <<"</ul>">>],
        d => [
            {<<"0">>, fun() ->
                arizona_template:each(
                    arizona_template:get(items, Bindings0),
                    #{
                        t => 0,
                        s => [<<"<li az-key=\"">>, <<"\">">>, <<"</li>">>],
                        d => fun(Item, Key) ->
                            [
                                {<<"0">>, fun() -> Key end},
                                {<<"1">>, fun() -> maps:get(text, Item) end}
                            ]
                        end,
                        f => <<"test">>
                    }
                )
            end}
        ],
        f => <<"test">>
    },
    {_, Snap0, V0} = arizona_render:render(Tmpl0, #{}),
    B1 = arizona_stream:clear_stream_pending(Bindings0, arizona_stream:stream_keys(Bindings0)),
    %% Insert 4th item -- limit=3 with halt mode
    S1 = arizona_stream:insert(maps:get(items, B1), #{id => 4, text => <<"D">>}),
    B2 = B1#{items => S1},
    Tmpl1 = #{
        s => [<<"<ul az=\"0\">">>, <<"</ul>">>],
        d => [
            {<<"0">>, fun() ->
                arizona_template:each(
                    arizona_template:get(items, B2),
                    #{
                        t => 0,
                        s => [<<"<li az-key=\"">>, <<"\">">>, <<"</li>">>],
                        d => fun(Item, Key) ->
                            [
                                {<<"0">>, fun() -> Key end},
                                {<<"1">>, fun() -> maps:get(text, Item) end}
                            ]
                        end,
                        f => <<"test">>
                    }
                )
            end}
        ],
        f => <<"test">>
    },
    Changed = arizona_live_compute_changed(B1, B2),
    {Ops, _, _} = arizona_diff:diff(Tmpl1, Snap0, V0, Changed),
    %% The insert op is emitted (pending is processed), but then apply_limit
    %% truncates the snapshot to 3 items, removing the 4th with OP_REMOVE.
    %% Net result: one INSERT + one REMOVE (or just empty if implementation
    %% is smarter). Let's check: 3 visible before + 1 insert = 4 in snap,
    %% then truncate to 3 removes 1.
    InsOps = [Op || [?OP_INSERT | _] = Op <- Ops],
    RemOps = [Op || [?OP_REMOVE | _] = Op <- Ops],
    ?assertEqual(1, length(InsOps)),
    ?assertEqual(1, length(RemOps)).

stream_limit_drop_delete(Config) when is_list(Config) ->
    %% on_limit => drop: delete visible item → next slides in
    KeyFun = fun(#{id := Id}) -> Id end,
    Items = [
        #{id => 1, text => <<"A">>},
        #{id => 2, text => <<"B">>},
        #{id => 3, text => <<"C">>}
    ],
    S0 = arizona_stream:new(KeyFun, Items, #{limit => 2, on_limit => drop}),
    Bindings0 = #{id => <<"test">>, items => S0},
    Tmpl0 = #{
        s => [<<"<ul az=\"0\">">>, <<"</ul>">>],
        d => [
            {<<"0">>, fun() ->
                arizona_template:each(
                    arizona_template:get(items, Bindings0),
                    #{
                        t => 0,
                        s => [<<"<li az-key=\"">>, <<"\">">>, <<"</li>">>],
                        d => fun(Item, Key) ->
                            [
                                {<<"0">>, fun() -> Key end},
                                {<<"1">>, fun() -> maps:get(text, Item) end}
                            ]
                        end,
                        f => <<"test">>
                    }
                )
            end}
        ],
        f => <<"test">>
    },
    {_, Snap0, V0} = arizona_render:render(Tmpl0, #{}),
    B1 = arizona_stream:clear_stream_pending(Bindings0, arizona_stream:stream_keys(Bindings0)),
    %% Delete item 1 -- visible window should now show items 2 and 3
    S1 = arizona_stream:delete(maps:get(items, B1), 1),
    B2 = B1#{items => S1},
    Tmpl1 = #{
        s => [<<"<ul az=\"0\">">>, <<"</ul>">>],
        d => [
            {<<"0">>, fun() ->
                arizona_template:each(
                    arizona_template:get(items, B2),
                    #{
                        t => 0,
                        s => [<<"<li az-key=\"">>, <<"\">">>, <<"</li>">>],
                        d => fun(Item, Key) ->
                            [
                                {<<"0">>, fun() -> Key end},
                                {<<"1">>, fun() -> maps:get(text, Item) end}
                            ]
                        end,
                        f => <<"test">>
                    }
                )
            end}
        ],
        f => <<"test">>
    },
    Changed = arizona_live_compute_changed(B1, B2),
    {Ops, _, _} = arizona_diff:diff(Tmpl1, Snap0, V0, Changed),
    %% Should have OP_REMOVE for item 1 and OP_INSERT for item 3 (slides in)
    RemOps = [Op || [?OP_REMOVE | _] = Op <- Ops],
    InsOps = [Op || [?OP_INSERT | _] = Op <- Ops],
    ?assert(length(RemOps) >= 1),
    ?assert(length(InsOps) >= 1),
    %% Item 3 should be inserted (it was not visible before, now it is)
    InsKeys = [K || [?OP_INSERT, _, K | _] <- InsOps],
    ?assert(lists:member(<<"3">>, InsKeys)).

stream_limit_drop_insert(Config) when is_list(Config) ->
    %% on_limit => drop: insert at pos 0 → last visible item removed
    KeyFun = fun(#{id := Id}) -> Id end,
    Items = [#{id => 1, text => <<"A">>}, #{id => 2, text => <<"B">>}],
    S0 = arizona_stream:new(KeyFun, Items, #{limit => 2, on_limit => drop}),
    Bindings0 = #{id => <<"test">>, items => S0},
    Tmpl0 = #{
        s => [<<"<ul az=\"0\">">>, <<"</ul>">>],
        d => [
            {<<"0">>, fun() ->
                arizona_template:each(
                    arizona_template:get(items, Bindings0),
                    #{
                        t => 0,
                        s => [<<"<li az-key=\"">>, <<"\">">>, <<"</li>">>],
                        d => fun(Item, Key) ->
                            [
                                {<<"0">>, fun() -> Key end},
                                {<<"1">>, fun() -> maps:get(text, Item) end}
                            ]
                        end,
                        f => <<"test">>
                    }
                )
            end}
        ],
        f => <<"test">>
    },
    {_, Snap0, V0} = arizona_render:render(Tmpl0, #{}),
    B1 = arizona_stream:clear_stream_pending(Bindings0, arizona_stream:stream_keys(Bindings0)),
    %% Insert at pos 0 -- order becomes [3, 1, 2], visible [3, 1]
    S1 = arizona_stream:insert(maps:get(items, B1), #{id => 3, text => <<"C">>}, 0),
    B2 = B1#{items => S1},
    Tmpl1 = #{
        s => [<<"<ul az=\"0\">">>, <<"</ul>">>],
        d => [
            {<<"0">>, fun() ->
                arizona_template:each(
                    arizona_template:get(items, B2),
                    #{
                        t => 0,
                        s => [<<"<li az-key=\"">>, <<"\">">>, <<"</li>">>],
                        d => fun(Item, Key) ->
                            [
                                {<<"0">>, fun() -> Key end},
                                {<<"1">>, fun() -> maps:get(text, Item) end}
                            ]
                        end,
                        f => <<"test">>
                    }
                )
            end}
        ],
        f => <<"test">>
    },
    Changed = arizona_live_compute_changed(B1, B2),
    {Ops, _, _} = arizona_diff:diff(Tmpl1, Snap0, V0, Changed),
    %% Should have insert for item 3 and remove for item 2 (dropped)
    InsOps = [Op || [?OP_INSERT | _] = Op <- Ops],
    RemOps = [Op || [?OP_REMOVE | _] = Op <- Ops],
    InsKeys = [K || [?OP_INSERT, _, K | _] <- InsOps],
    RemKeys = [K || [?OP_REMOVE, _, K] <- RemOps],
    ?assert(lists:member(<<"3">>, InsKeys)),
    ?assert(lists:member(<<"2">>, RemKeys)).

stream_move_op(Config) when is_list(Config) ->
    %% stream_move generates OP_REMOVE + OP_INSERT pair
    KeyFun = fun(#{id := Id}) -> Id end,
    Items = [
        #{id => 1, text => <<"A">>},
        #{id => 2, text => <<"B">>},
        #{id => 3, text => <<"C">>}
    ],
    S0 = arizona_stream:new(KeyFun, Items),
    Bindings0 = #{id => <<"test">>, items => S0},
    Tmpl0 = #{
        s => [<<"<ul az=\"0\">">>, <<"</ul>">>],
        d => [
            {<<"0">>, fun() ->
                arizona_template:each(
                    arizona_template:get(items, Bindings0),
                    #{
                        t => 0,
                        s => [<<"<li az-key=\"">>, <<"\">">>, <<"</li>">>],
                        d => fun(Item, Key) ->
                            [
                                {<<"0">>, fun() -> Key end},
                                {<<"1">>, fun() -> maps:get(text, Item) end}
                            ]
                        end,
                        f => <<"test">>
                    }
                )
            end}
        ],
        f => <<"test">>
    },
    {_, Snap0, V0} = arizona_render:render(Tmpl0, #{}),
    B1 = arizona_stream:clear_stream_pending(Bindings0, arizona_stream:stream_keys(Bindings0)),
    %% Move item 3 to position 0
    S1 = arizona_stream:move(maps:get(items, B1), 3, 0),
    B2 = B1#{items => S1},
    Tmpl1 = #{
        s => [<<"<ul az=\"0\">">>, <<"</ul>">>],
        d => [
            {<<"0">>, fun() ->
                arizona_template:each(
                    arizona_template:get(items, B2),
                    #{
                        t => 0,
                        s => [<<"<li az-key=\"">>, <<"\">">>, <<"</li>">>],
                        d => fun(Item, Key) ->
                            [
                                {<<"0">>, fun() -> Key end},
                                {<<"1">>, fun() -> maps:get(text, Item) end}
                            ]
                        end,
                        f => <<"test">>
                    }
                )
            end}
        ],
        f => <<"test">>
    },
    Changed = arizona_live_compute_changed(B1, B2),
    {Ops, _, _} = arizona_diff:diff(Tmpl1, Snap0, V0, Changed),
    %% Should produce a single OP_MOVE with after_key null (3 is first in new order [3,1,2])
    ?assertMatch([[?OP_MOVE, <<"0">>, <<"3">>, null]], Ops).

stream_limit_ssr(Config) when is_list(Config) ->
    %% SSR with limit renders only visible items
    KeyFun = fun(#{id := Id}) -> Id end,
    Items = [
        #{id => 1, text => <<"A">>},
        #{id => 2, text => <<"B">>},
        #{id => 3, text => <<"C">>}
    ],
    S = arizona_stream:new(KeyFun, Items, #{limit => 2}),
    Tmpl = #{
        s => [<<"<ul>">>, <<"</ul>">>],
        d => [
            {<<"0">>, fun() ->
                arizona_template:each(
                    S,
                    #{
                        t => 0,
                        s => [<<"<li az-key=\"">>, <<"\">">>, <<"</li>">>],
                        d => fun(Item, Key) ->
                            [
                                {<<"0">>, fun() -> Key end},
                                {<<"1">>, fun() -> maps:get(text, Item) end}
                            ]
                        end,
                        f => <<"test">>
                    }
                )
            end}
        ],
        f => <<"test">>
    },
    {HTML, _Snap} = arizona_render:render(Tmpl),
    HTMLBin = iolist_to_binary(HTML),
    ?assertMatch({_, _}, binary:match(HTMLBin, <<"A">>)),
    ?assertMatch({_, _}, binary:match(HTMLBin, <<"B">>)),
    ?assertEqual(nomatch, binary:match(HTMLBin, <<"C">>)).

stream_limit_reset(Config) when is_list(Config) ->
    %% Reset with limit: only limit items get OP_INSERT
    KeyFun = fun(#{id := Id}) -> Id end,
    Items = [#{id => 1, text => <<"A">>}],
    S0 = arizona_stream:new(KeyFun, Items, #{limit => 2}),
    Bindings0 = #{id => <<"test">>, items => S0},
    Tmpl0 = #{
        s => [<<"<ul az=\"0\">">>, <<"</ul>">>],
        d => [
            {<<"0">>, fun() ->
                arizona_template:each(
                    arizona_template:get(items, Bindings0),
                    #{
                        t => 0,
                        s => [<<"<li az-key=\"">>, <<"\">">>, <<"</li>">>],
                        d => fun(Item, Key) ->
                            [
                                {<<"0">>, fun() -> Key end},
                                {<<"1">>, fun() -> maps:get(text, Item) end}
                            ]
                        end,
                        f => <<"test">>
                    }
                )
            end}
        ],
        f => <<"test">>
    },
    {_, Snap0, V0} = arizona_render:render(Tmpl0, #{}),
    B1 = arizona_stream:clear_stream_pending(Bindings0, arizona_stream:stream_keys(Bindings0)),
    %% Reset with 3 new items, but limit=2
    NewItems = [
        #{id => 10, text => <<"X">>},
        #{id => 20, text => <<"Y">>},
        #{id => 30, text => <<"Z">>}
    ],
    S1 = arizona_stream:reset(maps:get(items, B1), NewItems),
    B2 = B1#{items => S1},
    Tmpl1 = #{
        s => [<<"<ul az=\"0\">">>, <<"</ul>">>],
        d => [
            {<<"0">>, fun() ->
                arizona_template:each(
                    arizona_template:get(items, B2),
                    #{
                        t => 0,
                        s => [<<"<li az-key=\"">>, <<"\">">>, <<"</li>">>],
                        d => fun(Item, Key) ->
                            [
                                {<<"0">>, fun() -> Key end},
                                {<<"1">>, fun() -> maps:get(text, Item) end}
                            ]
                        end,
                        f => <<"test">>
                    }
                )
            end}
        ],
        f => <<"test">>
    },
    Changed = arizona_live_compute_changed(B1, B2),
    {Ops, _, _} = arizona_diff:diff(Tmpl1, Snap0, V0, Changed),
    %% Should have OP_REMOVE for old item 1
    RemOps = [Op || [?OP_REMOVE | _] = Op <- Ops],
    ?assert(length(RemOps) >= 1),
    %% OP_INSERT: reset inserts visible_items (limit=2), so only 2 inserts
    InsOps = [Op || [?OP_INSERT | _] = Op <- Ops],
    ?assertEqual(2, length(InsOps)).

%% --- update/3 captures Changed in pending op --------------------------------
update_pending_op_carries_changed(Config) when is_list(Config) ->
    KeyFun = fun(#{id := Id}) -> Id end,
    S0 = arizona_stream:new(KeyFun, [#{id => 1, text => <<"old">>, other => v}]),
    #stream{pending = P1} =
        arizona_stream:update(S0, 1, #{id => 1, text => <<"new">>, other => v}),
    %% First op is the initial insert from new/2; the second is our update.
    [_Insert, {update, 1, _NewItem, Changed}] = queue:to_list(P1),
    ?assert(maps:is_key(text, Changed)),
    ?assertNot(maps:is_key(other, Changed)),
    ?assertNot(maps:is_key(id, Changed)).

%% --- update/3 on a missing key emits empty Changed --------------------------
update_on_missing_key_yields_empty_changed(Config) when is_list(Config) ->
    KeyFun = fun(#{id := Id}) -> Id end,
    S0 = arizona_stream:new(KeyFun),
    #stream{pending = P1} =
        arizona_stream:update(S0, 999, #{id => 999, text => <<"new">>}),
    [{update, 999, _NewItem, Changed}] = queue:to_list(P1),
    ?assertEqual(#{}, Changed).

%% --- reset/1,2 captures the pre-mutation items map in the pending op -------
reset_pending_op_carries_old_items(Config) when is_list(Config) ->
    KeyFun = fun(#{id := Id}) -> Id end,
    OldItem1 = #{id => 1, text => <<"A">>},
    OldItem2 = #{id => 2, text => <<"B">>},
    S0 = arizona_stream:new(KeyFun, [OldItem1, OldItem2]),
    %% reset/2 with a fresh set
    #stream{pending = P1} = arizona_stream:reset(S0, [#{id => 1, text => <<"A2">>}]),
    %% Pending starts with two inserts (initial population) and ends with reset.
    Ops1 = queue:to_list(P1),
    {reset, OldItems1} = lists:last(Ops1),
    ?assertEqual(#{1 => OldItem1, 2 => OldItem2}, OldItems1),
    %% reset/1 (full clear) also captures old items
    #stream{pending = P2} = arizona_stream:reset(S0),
    Ops2 = queue:to_list(P2),
    {reset, OldItems2} = lists:last(Ops2),
    ?assertEqual(#{1 => OldItem1, 2 => OldItem2}, OldItems2).

%% =============================================================================
%% Edge case tests
%% =============================================================================

%% --- stream_delete nonexistent key ------------------------------------------
stream_delete_nonexistent_key(Config) when is_list(Config) ->
    %% Deleting a key not in the stream is a no-op (size preserved)
    KeyFun = fun(#{id := Id}) -> Id end,
    S0 = arizona_stream:new(KeyFun, [#{id => 1, text => <<"A">>}]),
    S1 = arizona_stream:delete(S0, 99),
    %% Stream unchanged -- mount, render, and verify 1 item still there
    {B0, _} = arizona_todo:mount(
        #{items => [#{id => 1, text => <<"A">>}]}, arizona_req_test_adapter:new(#{})
    ),
    Tmpl0 = arizona_todo:render(B0),
    {HTML, _, _} = arizona_render:render(Tmpl0, #{}),
    HTMLBin = iolist_to_binary(HTML),
    ?assertMatch({_, _}, binary:match(HTMLBin, <<"A">>)),
    %% Verify delete of nonexistent key on fresh stream doesn't corrupt
    S2 = arizona_stream:delete(S1, 99),
    %% Ensure S2 is the same as S1 (no mutation)
    ?assertEqual(S1, S2).

%% --- stream_update nonexistent key in snapshot ------------------------------
stream_update_nonexistent_key(Config) when is_list(Config) ->
    %% Update key not in snapshot falls through to OP_INSERT
    Items = [#{id => 1, text => <<"A">>}],
    {B0, _} = arizona_todo:mount(#{items => Items}, arizona_req_test_adapter:new(#{})),
    Tmpl0 = arizona_todo:render(B0),
    {_, Snap0, V0} = arizona_render:render(Tmpl0, #{}),
    B1 = arizona_stream:clear_stream_pending(B0, arizona_stream:stream_keys(B0)),
    S = arizona_stream:update(maps:get(items, B1), 99, #{id => 99, text => <<"New">>}),
    B2 = B1#{items => S},
    Tmpl1 = arizona_todo:render(B2),
    Changed = arizona_live_compute_changed(B1, B2),
    {Ops, _, _} = arizona_diff:diff(Tmpl1, Snap0, V0, Changed),
    ?assertMatch([[?OP_INSERT, _, <<"99">>, -1, _]], Ops).

%% --- stream_duplicate_key_insert --------------------------------------------
stream_duplicate_key_insert(Config) when is_list(Config) ->
    %% Inserting a key that already exists overwrites the item in the map
    KeyFun = fun(#{id := Id}) -> Id end,
    S0 = arizona_stream:new(KeyFun, [#{id => 1, text => <<"A">>}]),
    B0 = #{items => S0},
    B1 = arizona_stream:clear_stream_pending(B0, arizona_stream:stream_keys(B0)),
    S1 = arizona_stream:insert(maps:get(items, B1), #{id => 1, text => <<"B">>}),
    %% The items map has the new value for key 1
    B2 = B1#{items => S1},
    %% Verify it doesn't crash when diffing
    Tmpl0 = arizona_todo:render(#{id => <<"todo">>, items => S0}),
    {_, Snap0, V0} = arizona_render:render(Tmpl0, #{}),
    Tmpl1 = arizona_todo:render(B2#{id => <<"todo">>}),
    Changed = arizona_live_compute_changed(
        #{id => <<"todo">>, items => S0},
        B2#{id => <<"todo">>}
    ),
    {Ops, _, _} = arizona_diff:diff(Tmpl1, Snap0, V0, Changed),
    %% Should produce an OP_INSERT for the duplicate key
    InsOps = [Op || [?OP_INSERT | _] = Op <- Ops],
    ?assertEqual(1, length(InsOps)).

%% --- stream_limit_one_test --------------------------------------------------
stream_limit_one(Config) when is_list(Config) ->
    %% limit=1 shows only the first item
    KeyFun = fun(#{id := Id}) -> Id end,
    Items = [
        #{id => 1, text => <<"A">>},
        #{id => 2, text => <<"B">>},
        #{id => 3, text => <<"C">>}
    ],
    S = arizona_stream:new(KeyFun, Items, #{limit => 1}),
    Tmpl = #{
        s => [<<"<ul az=\"0\">">>, <<"</ul>">>],
        d => [
            {<<"0">>, fun() ->
                arizona_template:each(
                    S,
                    #{
                        t => 0,
                        s => [
                            <<"<li az-key=\"">>,
                            <<"\" az=\"1\"><!--az:1-->">>,
                            <<"<!--/az--></li>">>
                        ],
                        d => fun(Item, Key) ->
                            [
                                {<<"0">>, fun() -> Key end},
                                {<<"1">>, fun() -> maps:get(text, Item) end}
                            ]
                        end,
                        f => <<"test">>
                    }
                )
            end}
        ],
        f => <<"test">>
    },
    {HTML, _, _} = arizona_render:render(Tmpl, #{}),
    HTMLBin = iolist_to_binary(HTML),
    ?assertMatch({_, _}, binary:match(HTMLBin, <<"A">>)),
    ?assertEqual(nomatch, binary:match(HTMLBin, <<"B">>)),
    ?assertEqual(nomatch, binary:match(HTMLBin, <<"C">>)).

%% --- stream_move_to_same_position -------------------------------------------
stream_move_to_same_position(Config) when is_list(Config) ->
    %% Move item to its current position -- generates ops but no visual change
    Items = [#{id => 1, text => <<"A">>}, #{id => 2, text => <<"B">>}],
    {B0, _} = arizona_todo:mount(#{items => Items}, arizona_req_test_adapter:new(#{})),
    Tmpl0 = arizona_todo:render(B0),
    {_, Snap0, V0} = arizona_render:render(Tmpl0, #{}),
    B1 = arizona_stream:clear_stream_pending(B0, arizona_stream:stream_keys(B0)),
    %% Move item 2 to position 1 (already at index 1)
    S = arizona_stream:move(maps:get(items, B1), 2, 1),
    B2 = B1#{items => S},
    Tmpl1 = arizona_todo:render(B2),
    Changed = arizona_live_compute_changed(B1, B2),
    {Ops, _, _} = arizona_diff:diff(Tmpl1, Snap0, V0, Changed),
    %% Move generates OP_MOVE with after_key <<"1">> (item 1 precedes 2 in [1,2])
    ?assertMatch([[?OP_MOVE, _, <<"2">>, <<"1">>]], Ops).

%% --- stream_empty_stream_operations ----------------------------------------
stream_empty_stream_operations(Config) when is_list(Config) ->
    %% All operations on empty stream -- no crash
    KeyFun = fun(#{id := Id}) -> Id end,
    S0 = arizona_stream:new(KeyFun),
    %% Delete from empty
    S1 = arizona_stream:delete(S0, 1),
    ?assertEqual(S0, S1),
    %% Update on empty
    _S2 = arizona_stream:update(S0, 1, #{id => 1, text => <<"A">>}),
    %% Move on empty
    S3 = arizona_stream:move(S0, 1, 0),
    ?assertEqual(S0, S3),
    %% Reset empty
    S4 = arizona_stream:reset(S0),
    %% Reset should produce a stream with empty items
    %% Insert into previously reset stream works
    S5 = arizona_stream:insert(S4, #{id => 1, text => <<"A">>}),
    %% Verify no crash by rendering
    Tmpl = #{
        s => [<<"<ul az=\"0\">">>, <<"</ul>">>],
        d => [
            {<<"0">>, fun() ->
                arizona_template:each(
                    S5,
                    #{
                        t => 0,
                        s => [<<"<li az-key=\"">>, <<"\">">>, <<"</li>">>],
                        d => fun(Item, Key) ->
                            [
                                {<<"0">>, fun() -> Key end},
                                {<<"1">>, fun() -> maps:get(text, Item) end}
                            ]
                        end,
                        f => <<"test">>
                    }
                )
            end}
        ],
        f => <<"test">>
    },
    {HTML, _, _} = arizona_render:render(Tmpl, #{}),
    HTMLBin = iolist_to_binary(HTML),
    ?assertMatch({_, _}, binary:match(HTMLBin, <<"A">>)).

%% --- stream_order_after_complex_sequence ------------------------------------
stream_order_after_complex_sequence(Config) when is_list(Config) ->
    %% Start with [1, 2, 3], insert 4 at pos 1, delete 2, move 3 to pos 0
    %% Expected final order: [3, 1, 4]
    Items = [
        #{id => 1, text => <<"A">>},
        #{id => 2, text => <<"B">>},
        #{id => 3, text => <<"C">>}
    ],
    {B0, _} = arizona_todo:mount(#{items => Items}, arizona_req_test_adapter:new(#{})),
    Tmpl0 = arizona_todo:render(B0),
    {_, Snap0, V0} = arizona_render:render(Tmpl0, #{}),
    B1 = arizona_stream:clear_stream_pending(B0, arizona_stream:stream_keys(B0)),
    %% Insert 4 at position 1
    S1 = arizona_stream:insert(maps:get(items, B1), #{id => 4, text => <<"D">>}, 1),
    %% Delete 2
    S2 = arizona_stream:delete(S1, 2),
    %% Move 3 to position 0
    S3 = arizona_stream:move(S2, 3, 0),
    B2 = B1#{items => S3},
    Tmpl1 = arizona_todo:render(B2),
    Changed = arizona_live_compute_changed(B1, B2),
    {Ops, Snap1, _} = arizona_diff:diff(Tmpl1, Snap0, V0, Changed),
    %% Verify ops were generated (at least insert, delete, and move ops)
    ?assert(length(Ops) >= 3),
    %% Verify final snapshot order is [3, 1, 4]
    [{_, #{order := FinalOrder}}] = [
        {Az, V}
     || {Az, V} <- maps:get(d, Snap1), is_map(V), maps:is_key(order, V)
    ],
    ?assertEqual([3, 1, 4], FinalOrder).

%% --- render_to_iolist with stream items -------------------------------------
render_to_iolist_with_stream_items(Config) when is_list(Config) ->
    %% SSR via render_view_to_iolist with non-empty stream
    HTML = iolist_to_binary(
        arizona_render:render_view_to_iolist(
            arizona_todo,
            arizona_req_test_adapter:new(#{}),
            #{
                bindings => #{items => [#{id => 1, text => <<"Hello">>}]},
                layouts => [{arizona_layout, render}]
            }
        )
    ),
    %% Verify stream item appears in SSR output
    ?assertMatch({_, _}, binary:match(HTML, <<"Hello">>)),
    ?assertMatch({_, _}, binary:match(HTML, <<"az-key=\"1\"">>)).

stream_dedup_strips_statics(Config) when is_list(Config) ->
    %% Second insert with same fingerprint gets statics stripped by dedup
    {ok, Pid} = arizona_live:start_link(
        arizona_todo, #{}, undefined, [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    %% First insert -- should have statics
    {ok, Ops1, []} = arizona_live:handle_event(
        Pid,
        <<"todo">>,
        <<"add">>,
        #{<<"id">> => 1, <<"text">> => <<"A">>}
    ),
    [[_, _, _, _, P1]] = Ops1,
    ?assert(maps:is_key(<<"s">>, P1)),
    %% Second insert -- statics should be stripped (same fingerprint)
    {ok, Ops2, []} = arizona_live:handle_event(
        Pid,
        <<"todo">>,
        <<"add">>,
        #{<<"id">> => 2, <<"text">> => <<"B">>}
    ),
    [[_, _, _, _, P2]] = Ops2,
    ?assertNot(maps:is_key(<<"s">>, P2)),
    ?assert(is_binary(maps:get(<<"f">>, P2))),
    ?assertEqual([<<" az-key=\"2\"">>, <<"B">>], maps:get(<<"d">>, P2)).

%% --- B4: render_fp_val uses zip_item for stream items ----------------------

render_fp_val_stream_items(Config) when is_list(Config) ->
    %% Stream items with f key produce fingerprinted payloads, not plain HTML
    Snap = #{
        t => 0,
        items => #{
            1 => [{<<"0">>, <<"A">>, #{}}],
            2 => [{<<"0">>, <<"B">>, #{}}]
        },
        template => #{
            t => 0,
            s => [<<"<li>">>, <<"</li>">>],
            d => fun(_) -> [] end,
            f => <<"item_fp">>
        },
        order => [1, 2]
    },
    %% Use fingerprint_payload on a parent template that contains this stream
    ParentSnap = #{
        s => [<<"<ul>">>, <<"</ul>">>],
        d => [{<<"0">>, Snap}],
        f => <<"parent_fp">>
    },
    Result = arizona_render:fingerprint_payload(ParentSnap),
    [StreamResult] = maps:get(<<"d">>, Result),
    %% Stream with f key produces a map with f, t, s, and d (list-of-lists)
    ?assertMatch(
        #{
            <<"f">> := <<"item_fp">>,
            <<"t">> := 0,
            <<"s">> := [<<"<li>">>, <<"</li>">>]
        },
        StreamResult
    ),
    StreamItems = maps:get(<<"d">>, StreamResult),
    ?assertEqual(2, length(StreamItems)),
    ?assertEqual([<<"A">>], lists:nth(1, StreamItems)),
    ?assertEqual([<<"B">>], lists:nth(2, StreamItems)).

render_fp_val_stream_items_no_fp(Config) when is_list(Config) ->
    %% Stream items without f key on template produce HTML binaries
    Snap = #{
        t => 0,
        items => #{
            1 => [{<<"0">>, <<"A">>, #{}}]
        },
        template => #{
            t => 0,
            s => [<<"<li>">>, <<"</li>">>],
            d => fun(_) -> [] end
        },
        order => [1]
    },
    ParentSnap = #{
        s => [<<"<ul>">>, <<"</ul>">>],
        d => [{<<"0">>, Snap}],
        f => <<"parent_fp">>
    },
    Result = arizona_render:fingerprint_payload(ParentSnap),
    [StreamResult] = maps:get(<<"d">>, Result),
    %% Without f key, item should be zipped to binary
    ?assertMatch([B] when is_binary(B), StreamResult).

render_fp_val_empty_stream(Config) when is_list(Config) ->
    %% Empty fingerprinted stream still includes s, t, f with empty d
    Snap = #{
        t => 0,
        items => #{},
        order => [],
        template => #{
            t => 0,
            s => [<<"<li>">>, <<"</li>">>],
            d => fun(_) -> [] end,
            f => <<"empty_fp">>
        }
    },
    ParentSnap = #{
        s => [<<"<ul>">>, <<"</ul>">>],
        d => [{<<"0">>, Snap}],
        f => <<"par_fp">>
    },
    Result = arizona_render:fingerprint_payload(ParentSnap),
    [StreamResult] = maps:get(<<"d">>, Result),
    ?assert(is_map(StreamResult)),
    ?assertEqual([], maps:get(<<"d">>, StreamResult)),
    ?assertEqual([<<"<li>">>, <<"</li>">>], maps:get(<<"s">>, StreamResult)),
    ?assertEqual(0, maps:get(<<"t">>, StreamResult)),
    ?assertEqual(<<"empty_fp">>, maps:get(<<"f">>, StreamResult)).

%% --- C1: dedup_fp_val recurses stream item lists correctly -----------------

dedup_stream_items_in_dynamics(Config) when is_list(Config) ->
    %% Stream items in <<"d">> list are fingerprinted maps -- they should
    %% be deduped (not passed through as ops via dedup_fps catch-all)
    StreamItems = [
        #{
            <<"f">> => <<"si_fp">>,
            <<"s">> => [<<"<li>">>, <<"</li>">>],
            <<"d">> => [<<"A">>]
        },
        #{
            <<"f">> => <<"si_fp">>,
            <<"s">> => [<<"<li>">>, <<"</li>">>],
            <<"d">> => [<<"B">>]
        }
    ],
    Payload = #{
        <<"f">> => <<"parent_si">>,
        <<"s">> => [<<"<ul>">>, <<"</ul>">>],
        <<"d">> => [StreamItems]
    },
    Ops = [[0, <<"0">>, Payload]],
    {Result, Fps} = arizona_live_dedup_fps(Ops, #{}),
    [[0, <<"0">>, P]] = Result,
    [Items] = maps:get(<<"d">>, P),
    %% First item keeps statics, second stripped (same fingerprint)
    [I1, I2] = Items,
    ?assert(maps:is_key(<<"s">>, I1)),
    ?assertNot(maps:is_key(<<"s">>, I2)),
    ?assert(maps:is_key(<<"si_fp">>, Fps)),
    ?assert(maps:is_key(<<"parent_si">>, Fps)).

dedup_stream_preserves_t(Config) when is_list(Config) ->
    %% Stream payload dedup strips <<"s">> but preserves <<"t">>
    Payload = #{
        <<"f">> => <<"st_fp">>,
        <<"t">> => 0,
        <<"s">> => [<<"<li>">>, <<"</li>">>],
        <<"d">> => [<<"A">>]
    },
    Ops1 = [[0, <<"0">>, Payload]],
    {_, Fps} = arizona_live_dedup_fps(Ops1, #{}),
    %% Second encounter -- same fingerprint, statics stripped
    Ops2 = [[0, <<"0">>, Payload]],
    {[[0, <<"0">>, P2]], _} = arizona_live_dedup_fps(Ops2, Fps),
    ?assertNot(maps:is_key(<<"s">>, P2)),
    ?assertEqual(0, maps:get(<<"t">>, P2)),
    ?assertEqual(<<"st_fp">>, maps:get(<<"f">>, P2)),
    ?assertEqual([<<"A">>], maps:get(<<"d">>, P2)).

%% --- S3: navigate with stream in fingerprinted page -------------------------

navigate_with_stream_items(Config) when is_list(Config) ->
    %% Navigate to a todo page with initial items -- the fingerprinted
    %% payload should contain stream items that are properly structured
    InitItems = [#{id => 1, text => <<"First">>}, #{id => 2, text => <<"Second">>}],
    {ok, Pid} = arizona_live:start_link(
        arizona_todo, #{items => InitItems}, undefined, [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    %% Navigate back to todo with same items -- produces fingerprinted payload
    {ok, _, Content} = arizona_live:navigate(
        Pid, arizona_todo, #{items => InitItems}, arizona_req_test_adapter:new()
    ),
    ?assert(maps:is_key(<<"f">>, Content)),
    ?assert(is_binary(maps:get(<<"f">>, Content))),
    %% The dynamics should contain the id attr and a stream map with f key and list-of-lists d
    [_IdDynamic, StreamDynamic] = maps:get(<<"d">>, Content),
    ?assert(is_map(StreamDynamic)),
    ?assert(is_binary(maps:get(<<"f">>, StreamDynamic))),
    ?assert(maps:is_key(<<"t">>, StreamDynamic)),
    ?assert(maps:is_key(<<"s">>, StreamDynamic)),
    StreamItems = maps:get(<<"d">>, StreamDynamic),
    ?assert(is_list(StreamItems)),
    ?assertEqual(2, length(StreamItems)).

%% --- Integration: dedup across stream inserts with nested child views ------

dedup_stream_insert_then_item_patch(Config) when is_list(Config) ->
    %% Insert an item (registers fingerprint), then update it
    %% -- the item patch inner ops should NOT contain fingerprinted payloads
    %% for arizona_todo's simple items (text only), but verify the dedup
    %% pipeline processes them without crashing
    {ok, Pid} = arizona_live:start_link(
        arizona_todo, #{}, undefined, [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    %% Insert
    {ok, Ops1, []} = arizona_live:handle_event(
        Pid,
        <<"todo">>,
        <<"add">>,
        #{<<"id">> => 1, <<"text">> => <<"A">>}
    ),
    [[?OP_INSERT, _, _, _, P1]] = Ops1,
    ?assert(maps:is_key(<<"s">>, P1)),
    %% Update same item -- produces ITEM_PATCH with text ops
    {ok, Ops2, []} = arizona_live:handle_event(
        Pid,
        <<"todo">>,
        <<"update">>,
        #{<<"id">> => 1, <<"text">> => <<"B">>}
    ),
    ?assertMatch([[?OP_ITEM_PATCH, _, _, _]], Ops2),
    [[_, _, _, InnerOps]] = Ops2,
    ?assertMatch([[?OP_TEXT, _, <<"B">>]], InnerOps).

%% =============================================================================
%% stream_to_list tests
%% =============================================================================

stream_to_list(Config) when is_list(Config) ->
    KeyFun = fun(#{id := Id}) -> Id end,
    Items = [#{id => 1, text => <<"A">>}, #{id => 2, text => <<"B">>}],
    S = arizona_stream:new(KeyFun, Items),
    ?assertEqual(Items, arizona_stream:to_list(S)).

stream_to_list_empty(Config) when is_list(Config) ->
    KeyFun = fun(#{id := Id}) -> Id end,
    S = arizona_stream:new(KeyFun),
    ?assertEqual([], arizona_stream:to_list(S)).

stream_to_list_preserves_order(Config) when is_list(Config) ->
    KeyFun = fun(#{id := Id}) -> Id end,
    Items = [
        #{id => 3, text => <<"C">>},
        #{id => 1, text => <<"A">>},
        #{id => 2, text => <<"B">>}
    ],
    S = arizona_stream:new(KeyFun, Items),
    ?assertEqual(Items, arizona_stream:to_list(S)).

%% =============================================================================
%% stream_sort tests
%% =============================================================================

stream_sort_basic(Config) when is_list(Config) ->
    KeyFun = fun(#{id := Id}) -> Id end,
    Items = [
        #{id => 3, text => <<"C">>},
        #{id => 1, text => <<"A">>},
        #{id => 2, text => <<"B">>}
    ],
    S0 = arizona_stream:new(KeyFun, Items),
    S1 = arizona_stream:sort(S0, fun(A, B) ->
        maps:get(id, A) =< maps:get(id, B)
    end),
    ?assertEqual(
        [
            #{id => 1, text => <<"A">>},
            #{id => 2, text => <<"B">>},
            #{id => 3, text => <<"C">>}
        ],
        arizona_stream:to_list(S1)
    ).

stream_sort_already_sorted(Config) when is_list(Config) ->
    %% No change when already sorted -- stream is unchanged (no pending op)
    KeyFun = fun(#{id := Id}) -> Id end,
    Items = [#{id => 1, text => <<"A">>}, #{id => 2, text => <<"B">>}],
    S0 = arizona_stream:new(KeyFun, Items),
    S1 = arizona_stream:clear_stream_pending(#{s => S0}, [s]),
    #{s := S2} = S1,
    S3 = arizona_stream:sort(S2, fun(A, B) ->
        maps:get(id, A) =< maps:get(id, B)
    end),
    %% Should be identical (same order)
    ?assertEqual(S2, S3).

stream_sort_emits_reorder_ops(Config) when is_list(Config) ->
    %% Sort a 3-item stream [3,1,2] → [1,2,3], verify OP_MOVE ops
    KeyFun = fun(#{id := Id}) -> Id end,
    Items = [
        #{id => 3, text => <<"C">>},
        #{id => 1, text => <<"A">>},
        #{id => 2, text => <<"B">>}
    ],
    S0 = arizona_stream:new(KeyFun, Items),
    Bindings0 = #{id => <<"test">>, items => S0},
    Tmpl0 = #{
        s => [<<"<ul az=\"0\">">>, <<"</ul>">>],
        d => [
            {<<"0">>, fun() ->
                arizona_template:each(
                    arizona_template:get(items, Bindings0),
                    #{
                        t => 0,
                        s => [<<"<li az-key=\"">>, <<"\">">>, <<"</li>">>],
                        d => fun(Item, Key) ->
                            [
                                {<<"0">>, fun() -> Key end},
                                {<<"1">>, fun() -> maps:get(text, Item) end}
                            ]
                        end,
                        f => <<"test">>
                    }
                )
            end}
        ],
        f => <<"test">>
    },
    {_, Snap0, V0} = arizona_render:render(Tmpl0, #{}),
    B1 = arizona_stream:clear_stream_pending(Bindings0, arizona_stream:stream_keys(Bindings0)),
    %% Sort ascending by id
    S1 = arizona_stream:sort(maps:get(items, B1), fun(A, B) ->
        maps:get(id, A) =< maps:get(id, B)
    end),
    B2 = B1#{items => S1},
    Tmpl1 = #{
        s => [<<"<ul az=\"0\">">>, <<"</ul>">>],
        d => [
            {<<"0">>, fun() ->
                arizona_template:each(
                    arizona_template:get(items, B2),
                    #{
                        t => 0,
                        s => [<<"<li az-key=\"">>, <<"\">">>, <<"</li>">>],
                        d => fun(Item, Key) ->
                            [
                                {<<"0">>, fun() -> Key end},
                                {<<"1">>, fun() -> maps:get(text, Item) end}
                            ]
                        end,
                        f => <<"test">>
                    }
                )
            end}
        ],
        f => <<"test">>
    },
    Changed = arizona_live_compute_changed(B1, B2),
    {Ops, Snap1, _} = arizona_diff:diff(Tmpl1, Snap0, V0, Changed),
    %% Should have OP_MOVE ops (not empty like the old bug)
    MoveOps = [Op || [?OP_MOVE | _] = Op <- Ops],
    ?assert(length(MoveOps) > 0),
    %% No removes or inserts -- just moves
    RemOps = [Op || [?OP_REMOVE | _] = Op <- Ops],
    InsOps = [Op || [?OP_INSERT | _] = Op <- Ops],
    ?assertEqual(0, length(RemOps)),
    ?assertEqual(0, length(InsOps)),
    %% Verify final snapshot order is [1,2,3]
    {<<"0">>, #{order := FinalOrder}} = lists:keyfind(<<"0">>, 1, maps:get(d, Snap1)),
    ?assertEqual([1, 2, 3], FinalOrder).

stream_sort_reverse(Config) when is_list(Config) ->
    %% Sort [1,2,3] → [3,2,1] -- reverse order
    KeyFun = fun(#{id := Id}) -> Id end,
    Items = [
        #{id => 1, text => <<"A">>},
        #{id => 2, text => <<"B">>},
        #{id => 3, text => <<"C">>}
    ],
    S0 = arizona_stream:new(KeyFun, Items),
    Bindings0 = #{id => <<"test">>, items => S0},
    Tmpl0 = #{
        s => [<<"<ul az=\"0\">">>, <<"</ul>">>],
        d => [
            {<<"0">>, fun() ->
                arizona_template:each(
                    arizona_template:get(items, Bindings0),
                    #{
                        t => 0,
                        s => [<<"<li az-key=\"">>, <<"\">">>, <<"</li>">>],
                        d => fun(Item, Key) ->
                            [
                                {<<"0">>, fun() -> Key end},
                                {<<"1">>, fun() -> maps:get(text, Item) end}
                            ]
                        end,
                        f => <<"test">>
                    }
                )
            end}
        ],
        f => <<"test">>
    },
    {_, Snap0, V0} = arizona_render:render(Tmpl0, #{}),
    B1 = arizona_stream:clear_stream_pending(Bindings0, arizona_stream:stream_keys(Bindings0)),
    %% Sort descending
    S1 = arizona_stream:sort(maps:get(items, B1), fun(A, B) ->
        maps:get(id, A) >= maps:get(id, B)
    end),
    B2 = B1#{items => S1},
    Tmpl1 = #{
        s => [<<"<ul az=\"0\">">>, <<"</ul>">>],
        d => [
            {<<"0">>, fun() ->
                arizona_template:each(
                    arizona_template:get(items, B2),
                    #{
                        t => 0,
                        s => [<<"<li az-key=\"">>, <<"\">">>, <<"</li>">>],
                        d => fun(Item, Key) ->
                            [
                                {<<"0">>, fun() -> Key end},
                                {<<"1">>, fun() -> maps:get(text, Item) end}
                            ]
                        end,
                        f => <<"test">>
                    }
                )
            end}
        ],
        f => <<"test">>
    },
    Changed = arizona_live_compute_changed(B1, B2),
    {Ops, Snap1, _} = arizona_diff:diff(Tmpl1, Snap0, V0, Changed),
    MoveOps = [Op || [?OP_MOVE | _] = Op <- Ops],
    %% LIS of [3,2,1] is length 1 (any single element), so 2 moves needed
    ?assertEqual(2, length(MoveOps)),
    {<<"0">>, #{order := FinalOrder}} = lists:keyfind(<<"0">>, 1, maps:get(d, Snap1)),
    ?assertEqual([3, 2, 1], FinalOrder).

%% =============================================================================
%% stream_reset reorder tests
%% =============================================================================

stream_reset_reorder(Config) when is_list(Config) ->
    %% Reset with same items in reversed order -- should produce move ops
    Items = [
        #{id => 1, text => <<"A">>},
        #{id => 2, text => <<"B">>},
        #{id => 3, text => <<"C">>}
    ],
    {B0, _} = arizona_todo:mount(#{items => Items}, arizona_req_test_adapter:new(#{})),
    Tmpl0 = arizona_todo:render(B0),
    {_, Snap0, V0} = arizona_render:render(Tmpl0, #{}),
    B1 = arizona_stream:clear_stream_pending(B0, arizona_stream:stream_keys(B0)),
    %% Reset with reversed order
    S = arizona_stream:reset(
        maps:get(items, B1),
        [
            #{id => 3, text => <<"C">>},
            #{id => 2, text => <<"B">>},
            #{id => 1, text => <<"A">>}
        ]
    ),
    B2 = B1#{items => S},
    Tmpl1 = arizona_todo:render(B2),
    Changed = arizona_live_compute_changed(B1, B2),
    {Ops, Snap1, _} = arizona_diff:diff(Tmpl1, Snap0, V0, Changed),
    %% No removes or inserts -- all items are kept
    RemOps = [Op || [?OP_REMOVE | _] = Op <- Ops],
    InsOps = [Op || [?OP_INSERT | _] = Op <- Ops],
    ?assertEqual(0, length(RemOps)),
    ?assertEqual(0, length(InsOps)),
    %% Should have move ops for reordering
    MoveOps = [Op || [?OP_MOVE | _] = Op <- Ops],
    ?assert(length(MoveOps) > 0),
    %% Verify final snapshot order
    [{_, #{order := FinalOrder}}] = [
        {Az, V}
     || {Az, V} <- maps:get(d, Snap1), is_map(V), maps:is_key(order, V)
    ],
    ?assertEqual([3, 2, 1], FinalOrder).

stream_reset_same_order_no_moves(Config) when is_list(Config) ->
    %% Reset with same items in same order -- no moves
    Items = [#{id => 1, text => <<"A">>}, #{id => 2, text => <<"B">>}],
    {B0, _} = arizona_todo:mount(#{items => Items}, arizona_req_test_adapter:new(#{})),
    Tmpl0 = arizona_todo:render(B0),
    {_, Snap0, V0} = arizona_render:render(Tmpl0, #{}),
    B1 = arizona_stream:clear_stream_pending(B0, arizona_stream:stream_keys(B0)),
    S = arizona_stream:reset(
        maps:get(items, B1),
        [
            #{id => 1, text => <<"A">>},
            #{id => 2, text => <<"B">>}
        ]
    ),
    B2 = B1#{items => S},
    Tmpl1 = arizona_todo:render(B2),
    Changed = arizona_live_compute_changed(B1, B2),
    {Ops, _, _} = arizona_diff:diff(Tmpl1, Snap0, V0, Changed),
    ?assertEqual([], Ops).

%% =============================================================================
%% OP_MOVE after_key semantics tests
%% =============================================================================

stream_move_after_key_first(Config) when is_list(Config) ->
    %% Move item to first position -- after_key is null (no predecessor)
    Items = [
        #{id => 1, text => <<"A">>},
        #{id => 2, text => <<"B">>},
        #{id => 3, text => <<"C">>}
    ],
    {B0, _} = arizona_todo:mount(#{items => Items}, arizona_req_test_adapter:new(#{})),
    Tmpl0 = arizona_todo:render(B0),
    {_, Snap0, V0} = arizona_render:render(Tmpl0, #{}),
    B1 = arizona_stream:clear_stream_pending(B0, arizona_stream:stream_keys(B0)),
    %% Move item 3 to position 0 → order becomes [3,1,2], after_key = null
    S = arizona_stream:move(maps:get(items, B1), 3, 0),
    B2 = B1#{items => S},
    Tmpl1 = arizona_todo:render(B2),
    Changed = arizona_live_compute_changed(B1, B2),
    {Ops, _, _} = arizona_diff:diff(Tmpl1, Snap0, V0, Changed),
    ?assertMatch([[?OP_MOVE, _, <<"3">>, null]], Ops).

stream_move_after_key_last(Config) when is_list(Config) ->
    %% Move item to last position -- after_key is the item before it
    Items = [
        #{id => 1, text => <<"A">>},
        #{id => 2, text => <<"B">>},
        #{id => 3, text => <<"C">>}
    ],
    {B0, _} = arizona_todo:mount(#{items => Items}, arizona_req_test_adapter:new(#{})),
    Tmpl0 = arizona_todo:render(B0),
    {_, Snap0, V0} = arizona_render:render(Tmpl0, #{}),
    B1 = arizona_stream:clear_stream_pending(B0, arizona_stream:stream_keys(B0)),
    %% Move item 1 to position 2 (last) → order becomes [2,3,1], after_key = 3
    S = arizona_stream:move(maps:get(items, B1), 1, 2),
    B2 = B1#{items => S},
    Tmpl1 = arizona_todo:render(B2),
    Changed = arizona_live_compute_changed(B1, B2),
    {Ops, _, _} = arizona_diff:diff(Tmpl1, Snap0, V0, Changed),
    ?assertMatch([[?OP_MOVE, _, <<"1">>, <<"3">>]], Ops).

stream_move_after_key_middle(Config) when is_list(Config) ->
    %% Move item to middle -- after_key is the item before it in the new order
    Items = [
        #{id => 1, text => <<"A">>},
        #{id => 2, text => <<"B">>},
        #{id => 3, text => <<"C">>},
        #{id => 4, text => <<"D">>}
    ],
    {B0, _} = arizona_todo:mount(#{items => Items}, arizona_req_test_adapter:new(#{})),
    Tmpl0 = arizona_todo:render(B0),
    {_, Snap0, V0} = arizona_render:render(Tmpl0, #{}),
    B1 = arizona_stream:clear_stream_pending(B0, arizona_stream:stream_keys(B0)),
    %% Move item 4 to position 1 → order becomes [1,4,2,3], after_key = 1
    S = arizona_stream:move(maps:get(items, B1), 4, 1),
    B2 = B1#{items => S},
    Tmpl1 = arizona_todo:render(B2),
    Changed = arizona_live_compute_changed(B1, B2),
    {Ops, _, _} = arizona_diff:diff(Tmpl1, Snap0, V0, Changed),
    ?assertMatch([[?OP_MOVE, _, <<"4">>, <<"1">>]], Ops).

%% =============================================================================
%% Nested stream tests
%% =============================================================================

nested_stream_render(Config) when is_list(Config) ->
    %% Stream items containing a nested stream -- verify render doesn't crash
    KeyFun = fun(#{id := Id}) -> Id end,
    InnerItems1 = [#{id => <<"a">>, val => <<"X">>}, #{id => <<"b">>, val => <<"Y">>}],
    InnerItems2 = [#{id => <<"c">>, val => <<"Z">>}],
    InnerKeyFun = fun(#{id := Id}) -> Id end,
    OuterItems = [
        #{id => 1, cells => arizona_stream:new(InnerKeyFun, InnerItems1)},
        #{id => 2, cells => arizona_stream:new(InnerKeyFun, InnerItems2)}
    ],
    S = arizona_stream:new(KeyFun, OuterItems),
    Bindings = #{id => <<"test">>, rows => S},
    Tmpl = #{
        s => [<<"<table az=\"0\">">>, <<"</table>">>],
        d => [
            {<<"0">>, fun() ->
                arizona_template:each(
                    arizona_template:get(rows, Bindings),
                    #{
                        t => 0,
                        s => [<<"<tr az-key=\"">>, <<"\" az=\"1\">">>, <<"</tr>">>],
                        d => fun(Row, RowKey) ->
                            [
                                {<<"0">>, fun() -> RowKey end},
                                {<<"1">>, fun() ->
                                    arizona_template:each(
                                        maps:get(cells, Row),
                                        #{
                                            t => 0,
                                            s => [<<"<td az-key=\"">>, <<"\">">>, <<"</td>">>],
                                            d => fun(Cell, CellKey) ->
                                                [
                                                    {<<"0">>, fun() -> CellKey end},
                                                    {<<"1">>, fun() -> maps:get(val, Cell) end}
                                                ]
                                            end,
                                            f => <<"test">>
                                        }
                                    )
                                end}
                            ]
                        end,
                        f => <<"test">>
                    }
                )
            end}
        ],
        f => <<"test">>
    },
    {HTML, _, _} = arizona_render:render(Tmpl, #{}),
    HTMLBin = iolist_to_binary(HTML),
    ?assertMatch({_, _}, binary:match(HTMLBin, <<"X">>)),
    ?assertMatch({_, _}, binary:match(HTMLBin, <<"Y">>)),
    ?assertMatch({_, _}, binary:match(HTMLBin, <<"Z">>)).

%% =============================================================================
%% DataTable handler tests
%% =============================================================================

datatable_mount(Config) when is_list(Config) ->
    {B, _} = arizona_datatable:mount(#{}, arizona_req_test_adapter:new(#{})),
    ?assertEqual(<<"page">>, maps:get(id, B)),
    ?assertEqual(<<"DataTable">>, maps:get(title, B)),
    ?assertEqual(false, maps:get(connected, B)),
    ?assertEqual(6, maps:get(next_id, B)),
    ?assertEqual(id, maps:get(sort_col, B)),
    ?assertEqual(asc, maps:get(sort_dir, B)).

datatable_ssr(Config) when is_list(Config) ->
    HTML = iolist_to_binary(
        arizona_render:render_view_to_iolist(
            arizona_datatable,
            arizona_req_test_adapter:new(#{}),
            #{bindings => #{title => <<"DataTable">>}, layouts => [{arizona_layout, render}]}
        )
    ),
    ?assertMatch({_, _}, binary:match(HTML, <<"<title>DataTable</title>">>)),
    ?assertMatch({_, _}, binary:match(HTML, <<"az-view id=\"page\"">>)),
    ?assertMatch({_, _}, binary:match(HTML, <<"az-key=\"1\"">>)),
    ?assertMatch({_, _}, binary:match(HTML, <<"az-key=\"2\"">>)),
    ?assertMatch({_, _}, binary:match(HTML, <<"az-key=\"3\"">>)),
    ?assertMatch({_, _}, binary:match(HTML, <<"az-key=\"4\"">>)),
    ?assertMatch({_, _}, binary:match(HTML, <<"az-key=\"5\"">>)),
    ?assertMatch({_, _}, binary:match(HTML, <<"Alice">>)),
    ?assertMatch({_, _}, binary:match(HTML, <<"Bob">>)),
    ?assertMatch({_, _}, binary:match(HTML, <<"Charlie">>)),
    ?assertMatch({_, _}, binary:match(HTML, <<"Diana">>)),
    ?assertMatch({_, _}, binary:match(HTML, <<"Eve">>)),
    ?assertMatch({_, _}, binary:match(HTML, <<"az-click=\"[0,&quot;sort&quot;">>)),
    ?assertMatch({_, _}, binary:match(HTML, <<"az-click=\"[0,&quot;add_row&quot;]\"">>)),
    ?assertMatch({_, _}, binary:match(HTML, <<"az-click=\"[0,&quot;delete_row&quot;">>)),
    ?assertMatch({_, _}, binary:match(HTML, <<"az-click=\"[0,&quot;move_top&quot;">>)).

datatable_live_mount(Config) when is_list(Config) ->
    {ok, Pid} = arizona_live:start_link(
        arizona_datatable, #{}, undefined, [], arizona_req_test_adapter:new()
    ),
    {ok, <<"page">>} = arizona_live:mount(Pid).

datatable_live_connected(Config) when is_list(Config) ->
    {ok, Pid} = arizona_live:start_link(
        arizona_datatable, #{}, self(), [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    %% mount sends self() ! arizona_connected, handle_info pushes to transport
    receive
        {arizona_push, Ops, Effects} ->
            ?assertEqual([], Ops),
            ?assertEqual([{arizona_js, [14, <<"DataTable">>]}], Effects)
    after 1000 ->
        ct:fail(timeout_waiting_for_connected_push)
    end.

datatable_live_add_row(Config) when is_list(Config) ->
    {ok, Pid} = arizona_live:start_link(
        arizona_datatable, #{}, undefined, [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    {ok, Ops, []} = arizona_live:handle_event(Pid, <<"page">>, <<"add_row">>, #{}),
    ?assertMatch([[?OP_INSERT, _, <<"6">>, -1, _]], Ops),
    [[_, _, _, _, Payload]] = Ops,
    ?assert(is_binary(maps:get(<<"f">>, Payload))),
    D = maps:get(<<"d">>, Payload),
    ?assertEqual(<<" az-key=\"6\"">>, lists:nth(1, D)),
    ?assertEqual(<<"6">>, lists:nth(2, D)),
    ?assertEqual(<<"New 6">>, lists:nth(3, D)),
    ?assertEqual(<<"20">>, lists:nth(4, D)),
    ?assertEqual(
        <<" az-click=\"[0,&quot;delete_row&quot;,{&quot;id&quot;:6}]\"">>, lists:nth(5, D)
    ),
    ?assertEqual(<<" az-click=\"[0,&quot;move_top&quot;,{&quot;id&quot;:6}]\"">>, lists:nth(6, D)).

datatable_live_add_row_sequence(Config) when is_list(Config) ->
    {ok, Pid} = arizona_live:start_link(
        arizona_datatable, #{}, undefined, [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    {ok, Ops1, []} = arizona_live:handle_event(Pid, <<"page">>, <<"add_row">>, #{}),
    ?assertMatch([[?OP_INSERT, _, <<"6">>, -1, _]], Ops1),
    {ok, Ops2, []} = arizona_live:handle_event(Pid, <<"page">>, <<"add_row">>, #{}),
    ?assertMatch([[?OP_INSERT, _, <<"7">>, -1, _]], Ops2).

datatable_live_delete_row(Config) when is_list(Config) ->
    {ok, Pid} = arizona_live:start_link(
        arizona_datatable, #{}, undefined, [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    {ok, Ops, []} = arizona_live:handle_event(
        Pid,
        <<"page">>,
        <<"delete_row">>,
        #{<<"id">> => 1}
    ),
    ?assertMatch([[?OP_REMOVE, _, <<"1">>]], Ops).

datatable_live_delete_row_middle(Config) when is_list(Config) ->
    {ok, Pid} = arizona_live:start_link(
        arizona_datatable, #{}, undefined, [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    {ok, Ops, []} = arizona_live:handle_event(
        Pid,
        <<"page">>,
        <<"delete_row">>,
        #{<<"id">> => 3}
    ),
    ?assertMatch([[?OP_REMOVE, _, <<"3">>]], Ops).

datatable_live_sort_asc(Config) when is_list(Config) ->
    {ok, Pid} = arizona_live:start_link(
        arizona_datatable, #{}, undefined, [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    {ok, Ops, []} = arizona_live:handle_event(
        Pid,
        <<"page">>,
        <<"sort">>,
        #{<<"col">> => <<"age">>}
    ),
    MoveOps = [Op || [?OP_MOVE | _] = Op <- Ops],
    InsOps = [Op || [?OP_INSERT | _] = Op <- Ops],
    RemOps = [Op || [?OP_REMOVE | _] = Op <- Ops],
    ?assert(length(MoveOps) > 0),
    ?assertEqual([], InsOps),
    ?assertEqual([], RemOps).

datatable_live_sort_name_no_move(Config) when is_list(Config) ->
    %% Default data is already sorted by name (Alice, Bob, Charlie, Diana, Eve)
    %% so sorting by name asc should produce no moves
    {ok, Pid} = arizona_live:start_link(
        arizona_datatable, #{}, undefined, [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    {ok, Ops, []} = arizona_live:handle_event(
        Pid,
        <<"page">>,
        <<"sort">>,
        #{<<"col">> => <<"name">>}
    ),
    MoveOps = [Op || [?OP_MOVE | _] = Op <- Ops],
    ?assertEqual([], MoveOps).

datatable_live_sort_toggle(Config) when is_list(Config) ->
    {ok, Pid} = arizona_live:start_link(
        arizona_datatable, #{}, undefined, [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    %% First sort by age asc
    {ok, _, []} = arizona_live:handle_event(
        Pid,
        <<"page">>,
        <<"sort">>,
        #{<<"col">> => <<"age">>}
    ),
    %% Second sort by age toggles to desc
    {ok, Ops2, []} = arizona_live:handle_event(
        Pid,
        <<"page">>,
        <<"sort">>,
        #{<<"col">> => <<"age">>}
    ),
    MoveOps = [Op || [?OP_MOVE | _] = Op <- Ops2],
    ?assert(length(MoveOps) > 0).

datatable_live_sort_col_change(Config) when is_list(Config) ->
    {ok, Pid} = arizona_live:start_link(
        arizona_datatable, #{}, undefined, [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    %% Sort by age
    {ok, _, []} = arizona_live:handle_event(
        Pid,
        <<"page">>,
        <<"sort">>,
        #{<<"col">> => <<"age">>}
    ),
    %% Sort by id returns to original order
    {ok, Ops2, []} = arizona_live:handle_event(
        Pid,
        <<"page">>,
        <<"sort">>,
        #{<<"col">> => <<"id">>}
    ),
    MoveOps = [Op || [?OP_MOVE | _] = Op <- Ops2],
    ?assert(length(MoveOps) > 0).

datatable_live_move_top(Config) when is_list(Config) ->
    {ok, Pid} = arizona_live:start_link(
        arizona_datatable, #{}, undefined, [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    {ok, Ops, []} = arizona_live:handle_event(
        Pid,
        <<"page">>,
        <<"move_top">>,
        #{<<"id">> => 5}
    ),
    ?assertMatch([[?OP_MOVE, _, <<"5">>, null]], Ops).

datatable_live_move_top_already_first(Config) when is_list(Config) ->
    %% Moving the first item to position 0 -- no crash, no effects
    {ok, Pid} = arizona_live:start_link(
        arizona_datatable, #{}, undefined, [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    {ok, _Ops, []} = arizona_live:handle_event(
        Pid,
        <<"page">>,
        <<"move_top">>,
        #{<<"id">> => 1}
    ).

datatable_live_reset(Config) when is_list(Config) ->
    {ok, Pid} = arizona_live:start_link(
        arizona_datatable, #{}, undefined, [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    %% Add a row
    {ok, _, _} = arizona_live:handle_event(Pid, <<"page">>, <<"add_row">>, #{}),
    %% Delete a row
    {ok, _, _} = arizona_live:handle_event(
        Pid,
        <<"page">>,
        <<"delete_row">>,
        #{<<"id">> => 2}
    ),
    %% Reset
    {ok, Ops, []} = arizona_live:handle_event(Pid, <<"page">>, <<"reset_data">>, #{}),
    RemOps = [Op || [?OP_REMOVE | _] = Op <- Ops],
    InsOps = [Op || [?OP_INSERT | _] = Op <- Ops],
    %% Should remove the added row (6) and insert the deleted row (2)
    ?assertEqual(1, length(RemOps)),
    ?assertEqual(1, length(InsOps)).

datatable_live_reset_restores_next_id(Config) when is_list(Config) ->
    {ok, Pid} = arizona_live:start_link(
        arizona_datatable, #{}, undefined, [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    %% Add 2 rows (ids 6, 7)
    {ok, _, _} = arizona_live:handle_event(Pid, <<"page">>, <<"add_row">>, #{}),
    {ok, _, _} = arizona_live:handle_event(Pid, <<"page">>, <<"add_row">>, #{}),
    %% Reset
    {ok, _, _} = arizona_live:handle_event(Pid, <<"page">>, <<"reset_data">>, #{}),
    %% Add again -- should use id 6
    {ok, Ops, []} = arizona_live:handle_event(Pid, <<"page">>, <<"add_row">>, #{}),
    ?assertMatch([[?OP_INSERT, _, <<"6">>, -1, _]], Ops).

datatable_live_shuffle(Config) when is_list(Config) ->
    {ok, Pid} = arizona_live:start_link(
        arizona_datatable, #{}, undefined, [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    {ok, Ops, []} = arizona_live:handle_event(Pid, <<"page">>, <<"shuffle">>, #{}),
    InsOps = [Op || [?OP_INSERT | _] = Op <- Ops],
    RemOps = [Op || [?OP_REMOVE | _] = Op <- Ops],
    ?assertEqual([], InsOps),
    ?assertEqual([], RemOps).

datatable_live_sort_dom_simulation(Config) when is_list(Config) ->
    %% Verify OP_MOVE ops produce correct DOM order when applied left-to-right
    %% (the way the client applies them). Tests sort asc then sort desc toggle.
    {ok, Pid} = arizona_live:start_link(
        arizona_datatable, #{}, undefined, [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    %% Sort age asc: expected DOM order [2,4,1,5,3]
    {ok, Ops1, []} = arizona_live:handle_event(
        Pid,
        <<"page">>,
        <<"sort">>,
        #{<<"col">> => <<"age">>}
    ),
    MoveOps1 = [Op || [?OP_MOVE | _] = Op <- Ops1],
    Dom1 = simulate_dom_moves([1, 2, 3, 4, 5], MoveOps1),
    ?assertEqual([2, 4, 1, 5, 3], Dom1),
    %% Sort age desc (toggle): expected DOM order [3,5,1,4,2]
    {ok, Ops2, []} = arizona_live:handle_event(
        Pid,
        <<"page">>,
        <<"sort">>,
        #{<<"col">> => <<"age">>}
    ),
    MoveOps2 = [Op || [?OP_MOVE | _] = Op <- Ops2],
    Dom2 = simulate_dom_moves([2, 4, 1, 5, 3], MoveOps2),
    ?assertEqual([3, 5, 1, 4, 2], Dom2).

datatable_live_delete_all_then_reset(Config) when is_list(Config) ->
    {ok, Pid} = arizona_live:start_link(
        arizona_datatable, #{}, undefined, [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    %% Delete all 5 rows
    {ok, _, _} = arizona_live:handle_event(
        Pid,
        <<"page">>,
        <<"delete_row">>,
        #{<<"id">> => 1}
    ),
    {ok, _, _} = arizona_live:handle_event(
        Pid,
        <<"page">>,
        <<"delete_row">>,
        #{<<"id">> => 2}
    ),
    {ok, _, _} = arizona_live:handle_event(
        Pid,
        <<"page">>,
        <<"delete_row">>,
        #{<<"id">> => 3}
    ),
    {ok, _, _} = arizona_live:handle_event(
        Pid,
        <<"page">>,
        <<"delete_row">>,
        #{<<"id">> => 4}
    ),
    {ok, _, _} = arizona_live:handle_event(
        Pid,
        <<"page">>,
        <<"delete_row">>,
        #{<<"id">> => 5}
    ),
    %% Reset
    {ok, Ops, []} = arizona_live:handle_event(Pid, <<"page">>, <<"reset_data">>, #{}),
    InsOps = [Op || [?OP_INSERT | _] = Op <- Ops],
    RemOps = [Op || [?OP_REMOVE | _] = Op <- Ops],
    ?assertEqual(5, length(InsOps)),
    ?assertEqual(0, length(RemOps)).

%% --- datatable_navigate_from_page_then_add_row_no_crash -------------------
%% End-to-end check that the example handlers' explicit-bindings mounts
%% prevent the production shuffle crash trigger:
%%   1. Mount /  (arizona_page) -- its own next_id=1.
%%   2. add_todo 3x -- bumps page's next_id to 4.
%%   3. Navigate to /datatable. Each handler's mount picks only the
%%      keys it accepts (title), so the carried next_id=4 does not
%%      override the datatable default of 6.
%%   4. Click Add Row -- uses datatable's own next_id=6, no dup with
%%      the existing rows {1..5} stream.
%%   5. Click Shuffle -- works.
%% This is the "test modules should not reproduce this error, keep
%% them fixed" assertion: navigation + clicks stay clean.
datatable_navigate_from_page_then_add_row_no_crash(Config) when is_list(Config) ->
    {ok, Pid} = arizona_live:start_link(
        arizona_page, #{}, undefined, [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    ok = lists:foreach(
        fun(_) ->
            {ok, _, _} = arizona_live:handle_event(
                Pid, <<"page">>, <<"add_todo">>, #{<<"text">> => <<"t">>}
            ),
            ok
        end,
        lists:seq(1, 3)
    ),
    {ok, _, _} = arizona_live:navigate(
        Pid, arizona_datatable, #{title => <<"DataTable">>}, arizona_req_test_adapter:new()
    ),
    {ok, _, _} = arizona_live:handle_event(Pid, <<"page">>, <<"add_row">>, #{}),
    LiveState = sys:get_state(Pid),
    [B] = [El || El <- tuple_to_list(LiveState), is_map(El), is_map_key(rows, El)],
    Rows = maps:get(rows, B),
    {Items, Flat, Size} = stream_invariant_components(Rows),
    %% Initial 5 items + 1 just added = 6, no dups.
    ?assertEqual(6, map_size(Items)),
    ?assertEqual(6, length(Flat)),
    ?assertEqual(6, Size),
    ?assertEqual(lists:sort(maps:keys(Items)), lists:sort(lists:usort(Flat))),
    %% Diana stayed at id=4 (was not overwritten by a dup-keyed New 4).
    ?assertMatch(#{name := <<"Diana">>}, maps:get(4, Items)),
    %% Shuffle path is clean.
    {ok, _, _} = arizona_live:handle_event(Pid, <<"page">>, <<"shuffle">>, #{}).

%% =============================================================================
%% Shuffle crash reproducers
%%
%% The production crash:
%%   {badkey,1},
%%   [{erlang,map_get,[1, #{4=>...,5=>...,7=>...,8=>...}], ...},
%%    {arizona_stream,'-to_list/1-lc$^0/1-0-',2,
%%                    [{file,"src/arizona_stream.erl"},{line,406}]},
%%    {arizona_datatable,handle_event,3,
%%                       [{file,"test/support/arizona_datatable.erl"},{line,127}]}]
%%
%% State at crash:
%%   items = #{4=>"New 4",5=>"New 5",7=>"New 7",8=>"New 8"}
%%   order = {[7,4,5,1,2,3,4,5,8],[]}
%%   size  = 9
%%
%% These cases isolate each public-API misuse that can break the
%% items <-> order invariant. The invariant is:
%%   set(flat_order(Order)) =:= maps:keys(Items)
%%   length(flat_order(Order)) =:= map_size(Items)
%%   Size =:= map_size(Items)
%% Once any op breaks it, every later op compounds the damage; finally
%% `to_list/1` (or any iterator that does `maps:get(K, Items)`) crashes.
%% =============================================================================

%% --- helpers ---------------------------------------------------------------

%% Read the (private) #stream record fields. The record is in arizona.hrl
%% (already included), so we can pattern-match without reaching into internals.
stream_invariant_components(#stream{items = Items, order = Order, size = Size}) ->
    {Front, Back} = Order,
    Flat = Front ++ lists:reverse(Back),
    {Items, Flat, Size}.

%% --- repro_insert_dup_breaks_invariant -------------------------------------
%% insert/2 with a key that's already in items: items dedups, order grows,
%% size grows. Pure invariant violation, no crash yet.
repro_insert_dup_breaks_invariant(Config) when is_list(Config) ->
    KeyFun = fun(#{id := Id}) -> Id end,
    S0 = arizona_stream:new(KeyFun, [#{id => 1, n => <<"a">>}, #{id => 2, n => <<"b">>}]),
    S1 = arizona_stream:insert(S0, #{id => 1, n => <<"dup">>}),
    %% Demonstrate the corruption: items count != order length != size
    {Items, Flat, Size} = stream_invariant_components(S1),
    ?assertEqual(2, map_size(Items)),
    ?assertEqual(3, length(Flat)),
    ?assertEqual(3, Size),
    ?assertEqual([1, 2, 1], Flat),
    %% Items <-> order invariant is broken (length(Flat) > map_size(Items)).
    ?assertNotEqual(map_size(Items), length(Flat)).

%% --- repro_insert_at_dup_breaks_invariant ----------------------------------
%% insert/3 has the same problem as insert/2 — order_insert_at/3 blindly
%% adds, regardless of whether the key already lives in the order list.
repro_insert_at_dup_breaks_invariant(Config) when is_list(Config) ->
    KeyFun = fun(#{id := Id}) -> Id end,
    S0 = arizona_stream:new(KeyFun, [#{id => 1, n => <<"a">>}, #{id => 2, n => <<"b">>}]),
    S1 = arizona_stream:insert(S0, #{id => 1, n => <<"dup">>}, 0),
    {Items, Flat, Size} = stream_invariant_components(S1),
    ?assertEqual(2, map_size(Items)),
    ?assertEqual(3, length(Flat)),
    ?assertEqual(3, Size),
    ?assertEqual([1, 1, 2], Flat),
    ?assertNotEqual(map_size(Items), length(Flat)).

%% --- repro_reset_with_dup_keys_breaks_invariant ----------------------------
%% reset/2's items map dedups via comprehension (last-wins) but
%% order_from_keyed/1 keeps duplicates. So passing a list with dup keys
%% directly produces an inconsistent state — no crash yet, but a delete
%% afterwards turns it into the production crash shape.
repro_reset_with_dup_keys_breaks_invariant(Config) when is_list(Config) ->
    KeyFun = fun(#{id := Id}) -> Id end,
    S0 = arizona_stream:new(KeyFun, []),
    DupItems = [
        #{id => 1, n => <<"first">>},
        #{id => 1, n => <<"second">>},
        #{id => 2, n => <<"two">>}
    ],
    S1 = arizona_stream:reset(S0, DupItems),
    {Items, Flat, Size} = stream_invariant_components(S1),
    ?assertEqual(2, map_size(Items)),
    ?assertEqual(3, length(Flat)),
    %% Note: in this branch `size = map_size(ItemsMap)` (not length(Flat)),
    %% so size matches items but disagrees with order length.
    ?assertEqual(2, Size),
    ?assertEqual([1, 1, 2], Flat),
    ?assertNotEqual(map_size(Items), length(Flat)).

%% --- repro_update_missing_key_breaks_invariant -----------------------------
%% update/3 silently inserts the key into items if it wasn't there, but
%% never touches order. The asymmetric mismatch (items has key, order
%% doesn't) doesn't crash to_list but does break the invariant and will
%% trip up sort/2 when its comparator is called on a key not in the
%% order-iterated subset (and vice versa for any caller that walks items).
repro_update_missing_key_breaks_invariant(Config) when is_list(Config) ->
    KeyFun = fun(#{id := Id}) -> Id end,
    S0 = arizona_stream:new(KeyFun, [#{id => 1, n => <<"a">>}]),
    S1 = arizona_stream:update(S0, 99, #{id => 99, n => <<"phantom">>}),
    {Items, Flat, Size} = stream_invariant_components(S1),
    ?assertEqual(2, map_size(Items)),
    ?assertEqual(1, length(Flat)),
    ?assertEqual(1, Size),
    ?assertEqual([1], Flat),
    %% Items <-> order invariant is broken (items has 99, order doesn't).
    ?assertNotEqual(lists:sort(maps:keys(Items)), lists:sort(lists:usort(Flat))).

%% --- repro_insert_dup_then_delete_yields_stale_order ----------------------
%% This is the minimal sequence that produces the production crash shape:
%% insert/2 dup → delete the dup'd key. delete/2 calls order_delete which
%% strips only the FIRST match, but maps:take fully removes the items
%% entry. Result: order keeps a stale key not in items.
repro_insert_dup_then_delete_yields_stale_order(Config) when is_list(Config) ->
    KeyFun = fun(#{id := Id}) -> Id end,
    S0 = arizona_stream:new(KeyFun, [#{id => 1, n => <<"a">>}, #{id => 2, n => <<"b">>}]),
    S1 = arizona_stream:insert(S0, #{id => 1, n => <<"dup">>}),
    S2 = arizona_stream:delete(S1, 1),
    {Items, Flat, Size} = stream_invariant_components(S2),
    %% items lost key 1 entirely; order kept a stale 1
    ?assertEqual(1, map_size(Items)),
    ?assert(is_map_key(2, Items)),
    ?assertNot(is_map_key(1, Items)),
    ?assertEqual([2, 1], Flat),
    ?assertEqual(2, Size),
    %% This is the same shape as production: items is a strict subset of
    %% the keys in flat_order(Order).
    ?assertNotEqual(lists:sort(maps:keys(Items)), lists:sort(lists:usort(Flat))).

%% --- repro_to_list_crash_matches_production_stack -------------------------
%% Same setup as the previous case, then call to_list/1 — this fires the
%% production crash, now wrapped as a named reason carrying the
%% `error_info` annotation that routes through `format_error/2`.
repro_to_list_crash_matches_production_stack(Config) when is_list(Config) ->
    KeyFun = fun(#{id := Id}) -> Id end,
    S0 = arizona_stream:new(KeyFun, [#{id => 1, n => <<"a">>}, #{id => 2, n => <<"b">>}]),
    S1 = arizona_stream:insert(S0, #{id => 1, n => <<"dup">>}),
    S2 = arizona_stream:delete(S1, 1),
    {Reason, Stack} =
        try arizona_stream:to_list(S2) of
            _ -> ct:fail(expected_invariant_break)
        catch
            error:R:ST -> {R, ST}
        end,
    %% Reason is the semantic, named-tag form (not the raw {badkey,_}).
    ?assertMatch({stream_order_stale_key, 1, [2, 1], [2]}, Reason),
    %% The top stack frame is arizona_stream's `error/3` site, carrying
    %% the {error_info, #{module => arizona_stream}} annotation.
    [{arizona_stream, to_list, _Args, Info} | _] = Stack,
    ?assertEqual(
        #{module => arizona_stream},
        proplists:get_value(error_info, Info)
    ),
    %% format_error/2 returns the human-readable sentence the dev page
    %% will surface via erl_error:format_exception/3.
    #{general := Msg} = arizona_stream:format_error(Reason, Stack),
    Bin = unicode:characters_to_binary(Msg),
    ?assertNotEqual(nomatch, binary:match(Bin, <<"stream order references key 1">>)).

%% --- repro_shuffle_after_insert_dup_then_delete_crashes -------------------
%% End-to-end demonstration: drive arizona_datatable through the live
%% process. We can't trigger insert/2 with a dup via the public handlers
%% (add_row uses next_id), so we splice a corrupted stream into the
%% bindings and then send a `shuffle` event — same path the production
%% crash took.
repro_shuffle_after_insert_dup_then_delete_crashes(Config) when is_list(Config) ->
    {ok, Pid} = arizona_live:start_link(
        arizona_datatable, #{}, undefined, [], arizona_req_test_adapter:new()
    ),
    {ok, _} = arizona_live:mount(Pid),
    %% Reach into the live state, corrupt the rows stream the same way
    %% insert(dup) + delete would, and put it back. This simulates whatever
    %% production trigger we haven't pinned down — the crash is downstream
    %% of the corruption, so the trigger doesn't change the crash shape.
    KeyFun = fun(#{id := Id}) -> Id end,
    S0 = arizona_stream:new(KeyFun, [
        #{id => 4, name => <<"New 4">>, age => 20},
        #{id => 5, name => <<"New 5">>, age => 20},
        #{id => 7, name => <<"New 7">>, age => 20},
        #{id => 8, name => <<"New 8">>, age => 20}
    ]),
    %% Inject the same shape as the production state: stale 1,2,3 +
    %% duplicates of 4,5 in order.
    Corrupt = inject_bad_order(S0, [7, 4, 5, 1, 2, 3, 4, 5, 8]),
    _NewState = sys:replace_state(Pid, fun(LiveState) ->
        replace_rows_in_live_state(LiveState, Corrupt)
    end),
    %% Now click Shuffle. The gen_server crashes; arizona_live:handle_event
    %% propagates the exception via gen_server:call. Trap exits so the
    %% test process survives the linked live process going down.
    process_flag(trap_exit, true),
    ?assertExit(
        {{{stream_order_stale_key, _, _, _}, _Stack}, _CallCtx},
        arizona_live:handle_event(Pid, <<"page">>, <<"shuffle">>, #{})
    ).

%% Helpers for the live-state corruption injection above.
inject_bad_order(#stream{} = S, Flat) ->
    S#stream{order = {Flat, []}, size = length(Flat)}.

replace_rows_in_live_state(LiveState, Stream) ->
    %% arizona_live's #state{} record stores its handler bindings; the
    %% datatable handler keeps its rows stream under `rows`. Tuple-update
    %% rather than including the (private) #state{} record from arizona_live.
    %% Layout: state record → bindings map at a fixed position. We find
    %% the tuple element that's a map containing `rows` and update it.
    list_to_tuple([
        case El of
            #{rows := _} = B -> B#{rows => Stream};
            _ -> El
        end
     || El <- tuple_to_list(LiveState)
    ]).

%% Mirrors arizona_live:compute_changed/2 for unit tests
arizona_live_compute_changed(OldBindings, NewBindings) ->
    maps:filter(
        fun(K, V) ->
            case OldBindings of
                #{K := V} -> false;
                #{} -> true
            end
        end,
        NewBindings
    ).

%% Simulate DOM OP_MOVE operations left-to-right (as the client does).
%% afterKey semantics: null = prepend, ref = insert after ref.
simulate_dom_moves(Dom, []) ->
    Dom;
simulate_dom_moves(Dom, [[?OP_MOVE, _Az, KeyBin, RefBin] | Rest]) ->
    Key = binary_to_integer(KeyBin),
    Ref =
        case RefBin of
            null -> null;
            _ -> binary_to_integer(RefBin)
        end,
    DomWithout = lists:delete(Key, Dom),
    NewDom =
        case Ref of
            null -> [Key | DomWithout];
            _ -> insert_after(Key, Ref, DomWithout)
        end,
    simulate_dom_moves(NewDom, Rest).

insert_after(Key, Ref, [Ref | T]) -> [Ref, Key | T];
insert_after(Key, Ref, [H | T]) -> [H | insert_after(Key, Ref, T)].

list_type_switch_stream_to_list(Config) when is_list(Config) ->
    %% Old was a stream, new is a plain list → OP_UPDATE
    KeyFun = fun(#{id := Id}) -> Id end,
    Stream0 = arizona_stream:new(KeyFun, [#{id => 1, text => <<"A">>}]),
    StreamTmpl = #{
        t => 0,
        s => [<<"<li az-key=\"">>, <<"\"><!--az:1-->">>, <<"<!--/az--></li>">>],
        d => fun(Item, Key) ->
            [{<<"0">>, fun() -> Key end}, {<<"1">>, fun() -> maps:get(text, Item) end}]
        end,
        f => <<"test">>
    },
    ListTmpl = #{
        t => 0,
        s => [<<"<li><!--az:0-->">>, <<"<!--/az--></li>">>],
        d => fun(Item) ->
            [{<<"0">>, fun() -> maps:get(text, Item) end}]
        end,
        f => <<"listtest">>
    },
    Bindings0 = #{id => <<"test">>, items => Stream0},
    Tmpl0 = #{
        s => [<<"<ul az=\"0\">">>, <<"</ul>">>],
        d => [
            {<<"0">>, fun() ->
                arizona_template:each(arizona_template:get(items, Bindings0), StreamTmpl)
            end}
        ],
        f => <<"test">>
    },
    {_, Snap0, V0} = arizona_render:render(Tmpl0, #{}),
    %% Switch to plain list
    Items1 = [#{id => 1, text => <<"X">>}],
    Bindings1 = Bindings0#{items => Items1},
    Changed = #{items => true},
    Tmpl1 = #{
        s => [<<"<ul az=\"0\">">>, <<"</ul>">>],
        d => [
            {<<"0">>, fun() ->
                arizona_template:each(arizona_template:get(items, Bindings1), ListTmpl)
            end}
        ],
        f => <<"test">>
    },
    {Ops, _Snap1, _V1} = arizona_diff:diff(Tmpl1, Snap0, V0, Changed),
    ?assertEqual(1, length(Ops)),
    [[3, <<"0">>, _HTML]] = Ops.

list_type_switch_list_to_stream(Config) when is_list(Config) ->
    %% Old was a plain list, new is a stream → OP_UPDATE
    KeyFun = fun(#{id := Id}) -> Id end,
    Items0 = [#{id => 1, text => <<"A">>}],
    ListTmpl = #{
        t => 0,
        s => [<<"<li><!--az:0-->">>, <<"<!--/az--></li>">>],
        d => fun(Item) ->
            [{<<"0">>, fun() -> maps:get(text, Item) end}]
        end,
        f => <<"listtest">>
    },
    StreamTmpl = #{
        t => 0,
        s => [<<"<li az-key=\"">>, <<"\"><!--az:1-->">>, <<"<!--/az--></li>">>],
        d => fun(Item, Key) ->
            [{<<"0">>, fun() -> Key end}, {<<"1">>, fun() -> maps:get(text, Item) end}]
        end,
        f => <<"test">>
    },
    Bindings0 = #{id => <<"test">>, items => Items0},
    Tmpl0 = #{
        s => [<<"<ul az=\"0\">">>, <<"</ul>">>],
        d => [
            {<<"0">>, fun() ->
                arizona_template:each(arizona_template:get(items, Bindings0), ListTmpl)
            end}
        ],
        f => <<"test">>
    },
    {_, Snap0, V0} = arizona_render:render(Tmpl0, #{}),
    %% Switch to stream
    Stream1 = arizona_stream:new(KeyFun, [#{id => 1, text => <<"X">>}]),
    Bindings1 = Bindings0#{items => Stream1},
    Changed = #{items => true},
    Tmpl1 = #{
        s => [<<"<ul az=\"0\">">>, <<"</ul>">>],
        d => [
            {<<"0">>, fun() ->
                arizona_template:each(arizona_template:get(items, Bindings1), StreamTmpl)
            end}
        ],
        f => <<"test">>
    },
    {Ops, _Snap1, _V1} = arizona_diff:diff(Tmpl1, Snap0, V0, Changed),
    ?assertEqual(1, length(Ops)),
    [[3, <<"0">>, _HTML]] = Ops.

%% Wrapper to call arizona_live's internal dedup_fps for unit testing
arizona_live_dedup_fps(Ops, Fps) ->
    %% Use lists:mapfoldl with the same logic as arizona_live
    lists:mapfoldl(fun arizona_live_dedup_fp_op/2, Fps, Ops).

arizona_live_dedup_fp_op([BinId, ChildOps], Fps0) when is_binary(BinId), is_list(ChildOps) ->
    {ChildOps1, Fps1} = arizona_live_dedup_fps(ChildOps, Fps0),
    {[BinId, ChildOps1], Fps1};
arizona_live_dedup_fp_op([OpCode, Target | Rest], Fps0) when is_integer(OpCode) ->
    {Rest1, Fps1} = arizona_live_dedup_fp_rest(Rest, Fps0),
    {[OpCode, Target | Rest1], Fps1};
arizona_live_dedup_fp_op(Op, Fps) ->
    {Op, Fps}.

arizona_live_dedup_fp_rest([], Fps) ->
    {[], Fps};
arizona_live_dedup_fp_rest([H | T], Fps0) ->
    {H1, Fps1} = arizona_live_dedup_fp_val(H, Fps0),
    {T1, Fps2} = arizona_live_dedup_fp_rest(T, Fps1),
    {[H1 | T1], Fps2}.

arizona_live_dedup_fp_val(#{<<"f">> := F, <<"s">> := _, <<"d">> := D} = Val, Fps) ->
    case Fps of
        #{F := _} ->
            {D1, Fps1} = arizona_live_dedup_fp_dlist(D, Fps),
            {maps:without([<<"s">>], Val#{<<"d">> => D1}), Fps1};
        #{} ->
            {D1, Fps1} = arizona_live_dedup_fp_dlist(D, Fps#{F => true}),
            {Val#{<<"d">> => D1}, Fps1}
    end;
arizona_live_dedup_fp_val(#{<<"f">> := F, <<"d">> := D} = Val, Fps) ->
    {D1, Fps1} = arizona_live_dedup_fp_dlist(D, Fps),
    Fps2 =
        case Fps1 of
            #{F := _} -> Fps1;
            #{} -> Fps1#{F => true}
        end,
    {Val#{<<"d">> => D1}, Fps2};
arizona_live_dedup_fp_val(Items, Fps) when is_list(Items) ->
    arizona_live_dedup_fp_dlist(Items, Fps);
arizona_live_dedup_fp_val(Val, Fps) ->
    {Val, Fps}.

arizona_live_dedup_fp_dlist([], Fps) ->
    {[], Fps};
arizona_live_dedup_fp_dlist([H | T], Fps0) ->
    {H1, Fps1} = arizona_live_dedup_fp_val(H, Fps0),
    {T1, Fps2} = arizona_live_dedup_fp_dlist(T, Fps1),
    {[H1 | T1], Fps2}.
