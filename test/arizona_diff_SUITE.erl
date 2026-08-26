-module(arizona_diff_SUITE).
-include_lib("stdlib/include/assert.hrl").
-include("arizona.hrl").

-export([all/0, groups/0]).
-export([
    diff_attr_op/1,
    diff_bool_attr_add/1,
    diff_bool_attr_remove/1,
    diff_bool_attr_no_change/1,
    diff_mixed_op/1,
    diff_nested_text_op/1,
    diff_no_change_op/1,
    diff_no_diff_stateful_child_diff4/1,
    diff_only_changed_emits_ops/1,
    diff_remove_node_op/1,
    diff_replace_with_template_op/1,
    diff_empty_to_template_uses_text_op/1,
    diff_list_content_change_full_update/1,
    diff_list_first_item_change_full_update/1,
    diff_list_grew_full_update/1,
    diff_list_shrank_full_update/1,
    diff_list_invisible_change_emits_no_ops/1,
    diff_list_esc_marker_mismatch_not_suppressed/1,
    diff_list_esc_wrapped_invisible_change_suppressed/1,
    diff_list_visible_change_still_full_updates/1,
    diff_list_no_change_no_ops/1,
    diff_stream_unchanged_snapshot_pair_no_ops/1,
    diff_stream_nested_in_template_is_incremental/1,
    diff_stream_nested_with_cleared_log_reconciles/1,
    diff_stream_pure_append_emits_no_moves/1,
    diff_stream_nested_content_change_delivers/1,
    diff_stream_nested_renders_items_once/1,
    diff_list_content_change_positional/1,
    diff_list_first_item_change_positional/1,
    diff_list_grew_positional/1,
    diff_list_shrank_positional/1,
    diff_list_middle_insert_positional/1,
    diff_list_middle_delete_positional/1,
    diff_list_no_change_positional_no_ops/1,
    diff_map_value_change/1,
    diff_map_grew/1,
    diff_map_no_change_no_ops/1,
    diff_map_single_insert_is_one_op/1,
    diff_map_late_insert_stays_positional/1,
    diff_map_tail_removal_patches_positionally/1,
    diff_list_head_insert_is_one_op/1,
    diff_list_large_shrink_falls_back_to_wholesale/1,
    diff_list_small_shrink_stays_positional/1,
    diff_list_ops_round_trip_every_shape/1,
    diff_list_late_insert_stays_positional/1,
    diff_each_among_siblings_uses_text_op/1,
    diff_each_among_siblings_to_empty_uses_text_op/1,
    diff_stream_among_siblings_uses_text_op/1,
    diff_stream_among_siblings_child_view_uses_text_op/1,
    diff_text_op/1,
    no_diff_diff3/1,
    no_diff_diff4_top_level/1,
    no_diff_nested/1,
    no_diff_ops/1,
    no_diff_skips_eval/1,
    no_diff_stateful_child/1,
    local_dep_aware_skip/1,
    conditional_case_branch_tracks/1,
    conditional_if_branch_tracks/1,
    conditional_maybe_branch_tracks/1,
    conditional_nested_tracks/1,
    conditional_over_track_op_free/1,
    conditional_missing_binding_safe/1,
    conditional_deep_nesting_tracks/1,
    conditional_attr_in_branch_tracks/1,
    conditional_try_branch_tracks/1,
    conditional_block_branch_tracks/1,
    conditional_receive_branch_tracks/1,
    conditional_get_lazy_branch_tracks/1,
    conditional_with_branch_tracks/1,
    conditional_with_macro_branch_tracks/1,
    conditional_computed_key_not_tracked/1,
    conditional_two_slot_fine_grained/1,
    conditional_nested_element_recurses/1,
    wire_get_value_raw/1,
    wire_raw_value_tagged_html/1,
    wire_helper_raw_value_escaped/1,
    markerless_stream_item_update_no_dangling_op/1,
    markerless_list_item_falls_back_to_full_update/1,
    markerless_list_no_change_no_ops/1,
    markerless_stateful_child_update_targets_element_az/1
]).

all() ->
    [
        {group, basic_ops},
        {group, no_diff},
        {group, conditional_dep},
        {group, wire_payload},
        {group, markerless_slots}
    ].

groups() ->
    [
        {basic_ops, [parallel], [
            diff_text_op,
            diff_attr_op,
            diff_no_change_op,
            diff_nested_text_op,
            diff_mixed_op,
            diff_remove_node_op,
            diff_replace_with_template_op,
            diff_empty_to_template_uses_text_op,
            diff_list_content_change_full_update,
            diff_list_first_item_change_full_update,
            diff_list_grew_full_update,
            diff_list_shrank_full_update,
            diff_list_invisible_change_emits_no_ops,
            diff_list_esc_marker_mismatch_not_suppressed,
            diff_list_esc_wrapped_invisible_change_suppressed,
            diff_list_visible_change_still_full_updates,
            diff_list_no_change_no_ops,
            diff_stream_unchanged_snapshot_pair_no_ops,
            diff_stream_nested_in_template_is_incremental,
            diff_stream_nested_with_cleared_log_reconciles,
            diff_stream_pure_append_emits_no_moves,
            diff_stream_nested_content_change_delivers,
            diff_stream_nested_renders_items_once,
            diff_list_content_change_positional,
            diff_list_first_item_change_positional,
            diff_list_grew_positional,
            diff_list_shrank_positional,
            diff_list_middle_insert_positional,
            diff_list_middle_delete_positional,
            diff_list_no_change_positional_no_ops,
            diff_map_value_change,
            diff_map_grew,
            diff_map_no_change_no_ops,
            diff_map_single_insert_is_one_op,
            diff_map_late_insert_stays_positional,
            diff_map_tail_removal_patches_positionally,
            diff_list_head_insert_is_one_op,
            diff_list_large_shrink_falls_back_to_wholesale,
            diff_list_small_shrink_stays_positional,
            diff_list_ops_round_trip_every_shape,
            diff_list_late_insert_stays_positional,
            diff_each_among_siblings_uses_text_op,
            diff_each_among_siblings_to_empty_uses_text_op,
            diff_stream_among_siblings_uses_text_op,
            diff_stream_among_siblings_child_view_uses_text_op,
            diff_only_changed_emits_ops,
            diff_bool_attr_add,
            diff_bool_attr_remove,
            diff_bool_attr_no_change
        ]},
        {no_diff, [parallel], [
            no_diff_ops,
            no_diff_nested,
            no_diff_skips_eval,
            no_diff_stateful_child,
            no_diff_diff3,
            no_diff_diff4_top_level,
            diff_no_diff_stateful_child_diff4,
            local_dep_aware_skip
        ]},
        {conditional_dep, [parallel], [
            conditional_case_branch_tracks,
            conditional_if_branch_tracks,
            conditional_maybe_branch_tracks,
            conditional_nested_tracks,
            conditional_over_track_op_free,
            conditional_missing_binding_safe,
            conditional_deep_nesting_tracks,
            conditional_attr_in_branch_tracks,
            conditional_try_branch_tracks,
            conditional_block_branch_tracks,
            conditional_receive_branch_tracks,
            conditional_get_lazy_branch_tracks,
            conditional_with_branch_tracks,
            conditional_with_macro_branch_tracks,
            conditional_computed_key_not_tracked,
            conditional_two_slot_fine_grained,
            conditional_nested_element_recurses
        ]},
        {wire_payload, [parallel], [
            wire_get_value_raw,
            wire_raw_value_tagged_html,
            wire_helper_raw_value_escaped
        ]},
        {markerless_slots, [parallel], [
            markerless_stream_item_update_no_dangling_op,
            markerless_list_item_falls_back_to_full_update,
            markerless_list_no_change_no_ops,
            markerless_stateful_child_update_targets_element_az
        ]}
    ].

%% --- diff/2 basic ops ---

diff_text_op(Config) when is_list(Config) ->
    OldSnap = #{
        s => [<<"<p az=\"0\">Hello, ">>, <<"!</p>">>],
        d => [{<<"0">>, <<"World">>}]
    },
    NewTmpl = #{
        s => [<<"<p az=\"0\">Hello, ">>, <<"!</p>">>],
        d => [{<<"0">>, fun() -> <<"Alice">> end}],
        f => <<"test">>
    },
    {Ops, NewSnap} = arizona_diff:diff(NewTmpl, OldSnap),
    ?assertEqual([[?OP_TEXT, <<"0">>, <<"Alice">>]], Ops),
    ?assertEqual([{<<"0">>, <<"Alice">>}], maps:get(d, NewSnap)).

diff_attr_op(Config) when is_list(Config) ->
    OldSnap = #{
        s => [<<"<div az=\"0\" class=\"">>, <<"\">ok</div>">>],
        d => [{<<"0">>, {attr, <<"class">>, <<"active">>}}]
    },
    NewTmpl = #{
        s => [<<"<div az=\"0\" class=\"">>, <<"\">ok</div>">>],
        d => [{<<"0">>, {attr, <<"class">>, fun() -> <<"inactive">> end}}],
        f => <<"test">>
    },
    {Ops, _} = arizona_diff:diff(NewTmpl, OldSnap),
    ?assertEqual([[?OP_SET_ATTR, <<"0">>, <<"class">>, <<"inactive">>]], Ops).

%% Bool attr: false -> true produces OP_SET_ATTR with empty value.
diff_bool_attr_add(Config) when is_list(Config) ->
    OldSnap = #{
        s => [<<"<input az=\"0\"">>, <<" />">>],
        d => [{<<"0">>, {attr, <<"checked">>, false}}]
    },
    NewTmpl = #{
        s => [<<"<input az=\"0\"">>, <<" />">>],
        d => [{<<"0">>, {attr, <<"checked">>, fun() -> true end}}],
        f => <<"test">>
    },
    {Ops, _} = arizona_diff:diff(NewTmpl, OldSnap),
    ?assertEqual([[?OP_SET_ATTR, <<"0">>, <<"checked">>, <<>>]], Ops).

%% Bool attr: true -> false produces OP_REM_ATTR.
diff_bool_attr_remove(Config) when is_list(Config) ->
    OldSnap = #{
        s => [<<"<input az=\"0\"">>, <<" />">>],
        d => [{<<"0">>, {attr, <<"checked">>, true}}]
    },
    NewTmpl = #{
        s => [<<"<input az=\"0\"">>, <<" />">>],
        d => [{<<"0">>, {attr, <<"checked">>, fun() -> false end}}],
        f => <<"test">>
    },
    {Ops, _} = arizona_diff:diff(NewTmpl, OldSnap),
    ?assertEqual([[?OP_REM_ATTR, <<"0">>, <<"checked">>]], Ops).

%% Bool attr: true -> true produces no ops.
diff_bool_attr_no_change(Config) when is_list(Config) ->
    OldSnap = #{
        s => [<<"<input az=\"0\"">>, <<" />">>],
        d => [{<<"0">>, {attr, <<"checked">>, true}}]
    },
    NewTmpl = #{
        s => [<<"<input az=\"0\"">>, <<" />">>],
        d => [{<<"0">>, {attr, <<"checked">>, fun() -> true end}}],
        f => <<"test">>
    },
    {Ops, _} = arizona_diff:diff(NewTmpl, OldSnap),
    ?assertEqual([], Ops).

diff_no_change_op(Config) when is_list(Config) ->
    OldSnap = #{
        s => [<<"<p az=\"0\">">>, <<"</p>">>],
        d => [{<<"0">>, <<"same">>}]
    },
    NewTmpl = #{
        s => [<<"<p az=\"0\">">>, <<"</p>">>],
        d => [{<<"0">>, fun() -> <<"same">> end}],
        f => <<"test">>
    },
    {Ops, _} = arizona_diff:diff(NewTmpl, OldSnap),
    ?assertEqual([], Ops).

%% A nested template re-rendered to the same statics diffs its inner dynamics: only
%% the changed inner slot (`i`) patches, addressed by its own az -- not a wholesale
%% re-render of the whole nested template at the outer slot az.
diff_nested_text_op(Config) when is_list(Config) ->
    %% The nested template sits in content slot `0`, so its inner az is
    %% namespaced by the slot to `0-i` (scope_slot, same as a stateless child).
    %% The old snapshot models what render/2 produced, so it carries the scoped id.
    OldSnap = #{
        s => [<<"<p az=\"0\">">>, <<"</p>">>],
        d => [{<<"0">>, #{s => [<<"Hello, ">>, <<"!">>], d => [{<<"0-i">>, <<"World">>}]}}]
    },
    NewTmpl = #{
        s => [<<"<p az=\"0\">">>, <<"</p>">>],
        d => [
            {<<"0">>, #{
                s => [<<"Hello, ">>, <<"!">>],
                d => [{<<"i">>, fun() -> <<"Alice">> end}],
                f => <<"test">>
            }}
        ],
        f => <<"test">>
    },
    {Ops, _} = arizona_diff:diff(NewTmpl, OldSnap),
    ?assertEqual([[?OP_TEXT, <<"0-i">>, <<"Alice">>]], Ops).

diff_mixed_op(Config) when is_list(Config) ->
    OldSnap = #{
        s => [<<"<div az=\"0\" class=\"">>, <<"\"><p az=\"0.0\">">>, <<"</p></div>">>],
        d => [{<<"0">>, {attr, <<"class">>, <<"active">>}}, {<<"0.0">>, <<"hello">>}]
    },
    NewTmpl = #{
        s => [<<"<div az=\"0\" class=\"">>, <<"\"><p az=\"0.0\">">>, <<"</p></div>">>],
        d => [
            {<<"0">>, {attr, <<"class">>, fun() -> <<"inactive">> end}},
            {<<"0.0">>, fun() -> <<"goodbye">> end}
        ],
        f => <<"test">>
    },
    {Ops, _} = arizona_diff:diff(NewTmpl, OldSnap),
    ?assertEqual(
        [
            [?OP_SET_ATTR, <<"0">>, <<"class">>, <<"inactive">>],
            [?OP_TEXT, <<"0.0">>, <<"goodbye">>]
        ],
        Ops
    ).

diff_remove_node_op(Config) when is_list(Config) ->
    OldSnap = #{
        s => [<<"<div az=\"0\">">>, <<"</div>">>],
        d => [{<<"0">>, <<"visible">>}]
    },
    NewTmpl = #{
        s => [<<"<div az=\"0\">">>, <<"</div>">>],
        d => [{<<"0">>, fun() -> remove end}],
        f => <<"test">>
    },
    {Ops, _} = arizona_diff:diff(NewTmpl, OldSnap),
    ?assertEqual([[?OP_REMOVE_NODE, <<"0">>]], Ops).

%% A content slot whose value changes from a plain binary to a nested template
%% patches the slot's marker content with ?OP_TEXT -- never a whole-element
%% innerHTML write, which would clobber the enclosing element when the slot's az
%% is the element's own az. See diff_empty_to_template_uses_text_op for the
%% empty(~"") -> ?stateful descriptor case this protects.
diff_replace_with_template_op(Config) when is_list(Config) ->
    OldSnap = #{
        s => [<<"<div az=\"0\"><!--az:0-->">>, <<"<!--/az--></div>">>],
        d => [{<<"0">>, <<"plain">>}]
    },
    NewTmpl = #{
        s => [<<"<div az=\"0\"><!--az:0-->">>, <<"<!--/az--></div>">>],
        d => [
            {<<"0">>, fun() ->
                #{s => [<<"<b>">>, <<"</b>">>], d => [{<<"i">>, <<"bold">>}], f => <<"test">>}
            end}
        ],
        f => <<"test">>
    },
    {Ops, _} = arizona_diff:diff(NewTmpl, OldSnap),
    %% The new nested template is namespaced by content slot `0`, so its
    %% fingerprint carries the slot prefix (`0-test`). The statics have no baked
    %% az, so they are unchanged; the wire `d` is the rendered value.
    ?assertEqual(
        [
            [
                ?OP_TEXT,
                <<"0">>,
                #{
                    <<"f">> => <<"0-test">>,
                    <<"s">> => [<<"<b>">>, <<"</b>">>],
                    <<"d">> => [<<"bold">>]
                }
            ]
        ],
        Ops
    ).

%% Regression: a content slot transitioning from the empty string (`~""`) to a
%% nested template -- the shape `case ?get(flag) of true -> ?stateful(...);
%% false -> ~"" end` produces -- must patch the slot's marker content via
%% ?OP_TEXT, leaving its siblings (and the enclosing element) intact. The bug
%% was emitting a whole-element innerHTML write here, which clobbered the whole
%% enclosing element when the slot's az equalled the element's own az (a
%% conditional ?stateful child directly under the view root).
diff_empty_to_template_uses_text_op(Config) when is_list(Config) ->
    %% Statics model: <main az="X-0" id="app"><h1>..</h1><!--az:X-0-->SLOT
    %% <!--/az--><footer>..</footer></main> -- the slot's az (X-0) is the same
    %% as the enclosing <main>'s az, exactly as a view root + conditional child.
    Statics = [
        <<"<main az=\"X-0\" id=\"app\"><h1 az=\"X-1\">t</h1><!--az:X-0-->">>,
        <<"<!--/az--><footer az=\"X-2\">f</footer></main>">>
    ],
    OldSnap = #{
        s => Statics,
        d => [{<<"X-1">>, <<"t">>}, {<<"X-0">>, <<>>}, {<<"X-2">>, <<"f">>}]
    },
    NewTmpl = #{
        s => Statics,
        d => [
            {<<"X-1">>, fun() -> <<"t">> end},
            {<<"X-0">>, fun() ->
                #{s => [<<"<div>child</div>">>], d => [], f => <<"child_fp">>}
            end},
            {<<"X-2">>, fun() -> <<"f">> end}
        ],
        f => <<"X">>
    },
    {Ops, _} = arizona_diff:diff(NewTmpl, OldSnap),
    %% Exactly one op, an ?OP_TEXT on the slot -- not a whole-element write on
    %% X-0 (which the client resolves to the <main> root and would innerHTML-wipe).
    %% The nested template is namespaced by the slot az, so its fingerprint
    %% carries the `X-0-` prefix; the op still targets the unchanged slot az X-0.
    ?assertMatch([[?OP_TEXT, <<"X-0">>, #{<<"f">> := <<"X-0-child_fp">>}]], Ops).

%% Plain-list `?each` FALLBACK diffing: a non-single-root item template (the
%% `each_list_diff/2` fixture below omits `single_root`, modelling a
%% multi-root/fragment item) re-renders the whole list with a single OP_TEXT (the
%% marker-aware container patch) -- never a per-item OP_ITEM_PATCH (there is no
%% per-position DOM node to address). The marker-aware OP_TEXT (not a
%% whole-element write) because a plain-list each is anchored by content-slot
%% comment markers; see diff_each_among_siblings_uses_text_op. A *single-root*
%% list instead patches
%% items positionally with OP_LIST_PATCH -- see the `_positional` cases and
%% `each_list_diff_sr/2`. These cover every fallback branch of the diff:
%%   diff_list_positional:  InnerOps =/= [] (head)  |  rest (tail)  |  neither
%%   diff_list:             content change  |  grew (insert)  |  shrank (remove)

%% Diff a plain-list `?each` of `Old` vs `New` (items are #{name => Bin}); the
%% each dynamic depends on `names`, which is marked changed so it isn't skipped.
each_list_diff(Old, New) ->
    ItemTmpl = #{
        t => ?EACH,
        s => [<<"<li az=\"0\">">>, <<"</li>">>],
        d => fun(I) -> [{<<"0">>, maps:get(name, I)}] end,
        f => <<"item">>
    },
    {OldItems, _} = arizona_eval:render_list_items(Old, ItemTmpl, {#{}, #{}}),
    OldSnap = #{
        s => [<<"<ul az=\"0\">">>, <<"</ul>">>],
        d => [{<<"0">>, #{t => ?EACH, items => OldItems, template => ItemTmpl}}],
        deps => [#{names => true}],
        f => <<"parent">>
    },
    NewTmpl = #{
        s => [<<"<ul az=\"0\">">>, <<"</ul>">>],
        d => [{<<"0">>, fun() -> arizona_template:each(New, ItemTmpl) end, {m, 1}}],
        f => <<"parent">>
    },
    {Ops, _Snap, _Views} = arizona_diff:diff(NewTmpl, OldSnap, #{}, #{names => true}),
    Ops.

%% Assert exactly one OP_TEXT (full marker-aware re-render), no per-item
%% OP_ITEM_PATCH, and return the rendered item-dynamics list from the
%% (fingerprinted) payload.
assert_full_update(Ops) ->
    ?assertMatch([[?OP_TEXT, <<"0">>, #{<<"t">> := ?EACH}]], Ops),
    ?assertEqual([], [Op || Op <- Ops, hd(Op) =:= ?OP_ITEM_PATCH]),
    [[?OP_TEXT, <<"0">>, #{<<"d">> := ItemDs}]] = Ops,
    ItemDs.

%% Last item's content changes: head item unchanged (InnerOps == []) so the
%% change is detected via RestChanged from the tail.
diff_list_content_change_full_update(Config) when is_list(Config) ->
    Ops = each_list_diff([#{name => <<"a">>}, #{name => <<"b">>}], [
        #{name => <<"a">>}, #{name => <<"B">>}
    ]),
    ItemDs = assert_full_update(Ops),
    ?assertEqual([[<<"a">>], [<<"B">>]], ItemDs).

%% First item's content changes: detected via InnerOps =/= [] on the head.
diff_list_first_item_change_full_update(Config) when is_list(Config) ->
    Ops = each_list_diff([#{name => <<"a">>}, #{name => <<"b">>}], [
        #{name => <<"A">>}, #{name => <<"b">>}
    ]),
    ?assertEqual([[<<"A">>], [<<"b">>]], assert_full_update(Ops)).

%% List grew: NewTail =/= [] drives the full update.
diff_list_grew_full_update(Config) when is_list(Config) ->
    Ops = each_list_diff([#{name => <<"a">>}], [#{name => <<"a">>}, #{name => <<"b">>}]),
    ?assertEqual([[<<"a">>], [<<"b">>]], assert_full_update(Ops)).

%% List shrank: OldTail =/= [] drives the full update.
diff_list_shrank_full_update(Config) when is_list(Config) ->
    Ops = each_list_diff([#{name => <<"a">>}, #{name => <<"b">>}], [#{name => <<"a">>}]),
    ?assertEqual([[<<"a">>]], assert_full_update(Ops)).

%% No item changed and same length -> no ops (every boolean false).
%% `to_bin/1` formats floats to 10 decimals, so 0.1 + 0.2 and 0.3 both render
%% "0.3". Answering "changed" on term inequality alone would re-render the whole
%% container, tearing down the DOM to rebuild byte-identical markup and losing
%% focus, scroll position and every `?local` inside it for nothing.
diff_list_invisible_change_emits_no_ops(Config) when is_list(Config) ->
    %% Summed through a call so the compiler cannot constant-fold the drift away.
    Drift = lists:sum([0.1, 0.2]),
    ?assertNotEqual(Drift, 0.3),
    ?assertEqual([], each_list_diff([#{name => 0.3}], [#{name => Drift}])).

%% `to_bin/1` unwraps an escape marker, so an esc-wrapped value and a bare one look
%% identical to it -- but the wholesale re-render ESCAPES the wrapped one and not the
%% bare one. Suppressing that would drop a visible change, and in this direction leave
%% unescaped markup on screen.
diff_list_esc_marker_mismatch_not_suppressed(Config) when is_list(Config) ->
    Esc = {arizona_esc, ~"<b>"},
    Bare = ~"<b>",
    ?assertEqual(arizona_template:to_bin(Esc), arizona_template:to_bin(Bare)),
    ?assertMatch(
        [[?OP_TEXT, <<"0">>, #{~"t" := ?EACH}]],
        each_list_diff([#{name => Esc}], [#{name => Bare}])
    ).

%% A pair sharing the marker IS unwrapped and compared, so the float suppression
%% still applies to what the parse transform actually emits (every content slot
%% value is marker-wrapped).
diff_list_esc_wrapped_invisible_change_suppressed(Config) when is_list(Config) ->
    Drift = lists:sum([0.1, 0.2]),
    ?assertNotEqual(Drift, 0.3),
    ?assertEqual(
        [],
        each_list_diff([#{name => {arizona_esc, 0.3}}], [#{name => {arizona_esc, Drift}}])
    ).

%% The suppression must not swallow a change that does alter the bytes.
diff_list_visible_change_still_full_updates(Config) when is_list(Config) ->
    ?assertMatch(
        [[?OP_TEXT, <<"0">>, #{~"t" := ?EACH}]],
        each_list_diff([#{name => 0.3}], [#{name => 0.4}])
    ).

diff_list_no_change_no_ops(Config) when is_list(Config) ->
    Items = [#{name => <<"a">>}, #{name => <<"b">>}],
    ?assertEqual([], each_list_diff(Items, Items)).

%% Single-root plain-list `?each` positional diffing. The item template carries
%% `single_root => true` (what the parse transform stamps for a one-element item
%% body), so a content change patches items in place via OP_LIST_PATCH instead of
%% the wholesale OP_TEXT -- no container childList churn (the WebKit scroll fix).

%% Diff a single-root plain-list `?each` of `Old` vs `New`. Same shape as
%% each_list_diff/2 but the item template is `single_root`, so it takes the
%% positional path.
each_list_diff_sr(Old, New) ->
    ItemTmpl = #{
        t => ?EACH,
        s => [<<"<li az=\"0\">">>, <<"</li>">>],
        d => fun(I) -> [{<<"0">>, maps:get(name, I)}] end,
        f => <<"item">>,
        single_root => true
    },
    {OldItems, _} = arizona_eval:render_list_items(Old, ItemTmpl, {#{}, #{}}),
    OldSnap = #{
        s => [<<"<ul az=\"0\">">>, <<"</ul>">>],
        d => [{<<"0">>, #{t => ?EACH, items => OldItems, template => ItemTmpl}}],
        deps => [#{names => true}],
        f => <<"parent">>
    },
    NewTmpl = #{
        s => [<<"<ul az=\"0\">">>, <<"</ul>">>],
        d => [{<<"0">>, fun() -> arizona_template:each(New, ItemTmpl) end, {m, 1}}],
        f => <<"parent">>
    },
    {Ops, _Snap, _Views} = arizona_diff:diff(NewTmpl, OldSnap, #{}, #{names => true}),
    Ops.

%% Assert exactly one OP_LIST_PATCH on the slot and return its sub-ops.
assert_list_patch(Ops) ->
    ?assertMatch([[?OP_LIST_PATCH, <<"0">>, _]], Ops),
    [[?OP_LIST_PATCH, <<"0">>, SubOps]] = Ops,
    SubOps.

%% Last item content changes: one positional ITEM_PATCH at index 1, addressing
%% the item's inner slot in place.
diff_list_content_change_positional(Config) when is_list(Config) ->
    Ops = each_list_diff_sr([#{name => <<"a">>}, #{name => <<"b">>}], [
        #{name => <<"a">>}, #{name => <<"B">>}
    ]),
    ?assertEqual([[?OP_ITEM_PATCH, 1, [[?OP_TEXT, <<"0">>, <<"B">>]]]], assert_list_patch(Ops)).

%% First item content changes: positional ITEM_PATCH at index 0.
diff_list_first_item_change_positional(Config) when is_list(Config) ->
    Ops = each_list_diff_sr([#{name => <<"a">>}, #{name => <<"b">>}], [
        #{name => <<"A">>}, #{name => <<"b">>}
    ]),
    ?assertEqual([[?OP_ITEM_PATCH, 0, [[?OP_TEXT, <<"0">>, <<"A">>]]]], assert_list_patch(Ops)).

%% List grew: a single tail INSERT sub-op (the one childList op), carrying the new
%% item's fingerprinted payload; existing items untouched.
diff_list_grew_positional(Config) when is_list(Config) ->
    Ops = each_list_diff_sr([#{name => <<"a">>}], [#{name => <<"a">>}, #{name => <<"b">>}]),
    ?assertMatch(
        [[?OP_INSERT, 1, #{<<"f">> := <<"item">>, <<"d">> := [<<"b">>]}]],
        assert_list_patch(Ops)
    ).

%% List shrank: a single tail REMOVE sub-op at the dropped index.
diff_list_shrank_positional(Config) when is_list(Config) ->
    Ops = each_list_diff_sr([#{name => <<"a">>}, #{name => <<"b">>}], [#{name => <<"a">>}]),
    ?assertEqual([[?OP_REMOVE, 1]], assert_list_patch(Ops)).

%% Middle insert reproduces the new list exactly with a content-patch cascade plus
%% ONE tail INSERT (the sole childList op). [a,c] -> [a,x,c]: patch index 1 (c->x),
%% insert index 2 (c). Surviving nodes (index 0) are reused -> scroll-safe.
diff_list_middle_insert_positional(Config) when is_list(Config) ->
    Ops = each_list_diff_sr([#{name => <<"a">>}, #{name => <<"c">>}], [
        #{name => <<"a">>}, #{name => <<"x">>}, #{name => <<"c">>}
    ]),
    %% The unchanged head and tail are stripped first, so this is ONE insert at the
    %% position the item actually goes -- not "patch every later item, then append".
    ?assertMatch([[?OP_INSERT, 1, #{<<"d">> := [<<"x">>]}]], assert_list_patch(Ops)).

%% Middle delete: a content-patch cascade plus ONE tail REMOVE. [a,b,c] -> [a,c]:
%% patch index 1 (b->c), remove index 2.
diff_list_middle_delete_positional(Config) when is_list(Config) ->
    Ops = each_list_diff_sr([#{name => <<"a">>}, #{name => <<"b">>}, #{name => <<"c">>}], [
        #{name => <<"a">>}, #{name => <<"c">>}
    ]),
    ?assertEqual([[?OP_REMOVE, 1]], assert_list_patch(Ops)).

%% No item changed and same length -> no ops at all (empty sub-ops -> no LIST_PATCH).
diff_list_no_change_positional_no_ops(Config) when is_list(Config) ->
    Items = [#{name => <<"a">>}, #{name => <<"b">>}],
    ?assertEqual([], each_list_diff_sr(Items, Items)).

%% Map-source `?each` diffing. A map renders to the same snapshot shape as a list
%% (`items => [ItemD]`, keyed by position in map-iteration order), so it takes the
%% same single-root positional path -- these guard that a map source diffs at all
%% (it used to crash with `function_clause`: `diff_each` had no map clause) and
%% that a changed entry patches the right index.

%% Diff a single-root map `?each` of `Old` vs `New` (entries are Key => Value
%% binaries). Mirrors each_list_diff_sr/2 but with a map source and a 2-arg item
%% callback. `#{~"a" => _, ~"b" => _}` iterates a, then b, so index 1 is `b`.
each_map_diff(Old, New) ->
    ItemTmpl = #{
        t => ?EACH,
        s => [<<"<li az=\"0\">">>, <<"</li>">>],
        d => fun(K, V) -> [{<<"0">>, <<K/binary, ":", V/binary>>}] end,
        f => <<"item">>,
        single_root => true
    },
    {OldItems, _} = arizona_eval:render_map_items(Old, ItemTmpl, {#{}, #{}}),
    OldSnap = #{
        s => [<<"<ul az=\"0\">">>, <<"</ul>">>],
        d => [{<<"0">>, #{t => ?EACH, items => OldItems, template => ItemTmpl}}],
        deps => [#{entries => true}],
        f => <<"parent">>
    },
    NewTmpl = #{
        s => [<<"<ul az=\"0\">">>, <<"</ul>">>],
        d => [{<<"0">>, fun() -> arizona_template:each(New, ItemTmpl) end, {m, 1}}],
        f => <<"parent">>
    },
    {Ops, _Snap, _Views} = arizona_diff:diff(NewTmpl, OldSnap, #{}, #{entries => true}),
    Ops.

%% A value change on one entry: positional ITEM_PATCH at that entry's index.
diff_map_value_change(Config) when is_list(Config) ->
    Ops = each_map_diff(
        #{<<"a">> => <<"1">>, <<"b">> => <<"2">>},
        #{<<"a">> => <<"1">>, <<"b">> => <<"9">>}
    ),
    ?assertEqual(
        [[?OP_ITEM_PATCH, 1, [[?OP_TEXT, <<"0">>, <<"b:9">>]]]],
        assert_list_patch(Ops)
    ).

%% A new key added: the extra entry appends via a positional INSERT.
diff_map_grew(Config) when is_list(Config) ->
    Ops = each_map_diff(
        #{<<"a">> => <<"1">>},
        #{<<"a">> => <<"1">>, <<"b">> => <<"2">>}
    ),
    ?assertMatch([[?OP_INSERT, 1, _]], assert_list_patch(Ops)).

%% A key inserted at the HEAD shifts every later position, so each one would patch
%% with its neighbour's content -- one op per item, against wholesale's one op
%% total. Erlang iterates a small map in term order, so "0" sorts in front of "a".
%% One added key is one op wherever it lands. Past 32 keys a map is a hashmap, so
%% iteration is hash order rather than term order and the insert position is not
%% predictable -- which is exactly why the position is left unasserted here.
diff_map_single_insert_is_one_op(Config) when is_list(Config) ->
    Base = maps:from_list([
        {integer_to_binary(I), integer_to_binary(I)}
     || I <- lists:seq(100, 300)
    ]),
    Ops = each_map_diff(Base, Base#{~"0" => ~"0"}),
    ?assertMatch([[?OP_INSERT, _Idx, _]], assert_list_patch(Ops)).

%% The same insert near the TAIL shifts almost nothing, so it stays on the cheap
%% path -- re-sending every item to patch the last one is the amplification the
%% head-insert fallback exists to avoid, not a rule to apply everywhere.
diff_map_late_insert_stays_positional(Config) when is_list(Config) ->
    Base = maps:from_list([{K, K} || K <- [~"a", ~"b", ~"c", ~"d", ~"e", ~"f", ~"g", ~"h"]]),
    Ops = each_map_diff(Base, Base#{~"g0" => ~"x"}),
    ?assertMatch([[?OP_INSERT, 7, _]], assert_list_patch(Ops)).

%% Dropping the tail key leaves the survivors' positions intact, so the shared
%% head needs no ops and the tail is a positional remove.
diff_map_tail_removal_patches_positionally(Config) when is_list(Config) ->
    Ops = each_map_diff(
        #{<<"a">> => <<"1">>, <<"b">> => <<"2">>},
        #{<<"a">> => <<"1">>}
    ),
    ?assertEqual([[?OP_REMOVE, 1]], assert_list_patch(Ops)).

%% A list has no per-item key to compare, so the same head-insert amplification
%% is caught the same way: by how much of the list the positional walk had to
%% patch. This is the case a key-order gate could never cover for a list.
%% A head insert used to be the worst case -- every item shifted, so every item was
%% patched with its neighbour's content and the whole container was re-rendered
%% instead. Stripping the common suffix makes it one op, at any container size.
diff_list_head_insert_is_one_op(Config) when is_list(Config) ->
    Old = [#{name => integer_to_binary(I)} || I <- lists:seq(1, 200)],
    Ops = each_list_diff_sr(Old, [#{name => ~"0"} | Old]),
    ?assertMatch([[?OP_INSERT, 0, #{<<"d">> := [~"0"]}]], assert_list_patch(Ops)).

%% A DOM teardown costs focus, scroll position and every `?local` in the container.
%% That is not worth a hundred-odd bytes, so a short container keeps its per-item ops
%% even where the wholesale render would be smaller.
%% The invariant that matters, over every mutation shape rather than a hand-picked
%% few: applying the ops to the OLD list must reproduce the NEW list exactly. The
%% expectation is the new list itself, so a wrong op cannot make its own assertion
%% pass, and the ops are generated live here so this cannot go stale against the diff.
%%
%% `replay_list_patch/2` models `applyListPatch` in arizona.js: indices address the
%% OLD positions because the client snapshots the item roots before applying anything,
%% an INSERT lands before whatever was at that index (or at the end past it), and
%% repeated inserts at one index keep their emitted order. Those client semantics are
%% pinned separately by the non-tail insert/remove tests in arizona-slots.test.js.
diff_list_ops_round_trip_every_shape(Config) when is_list(Config) ->
    L = [integer_to_binary(I) || I <- lists:seq(1, 6)],
    L20 = [integer_to_binary(I) || I <- lists:seq(1, 20)],
    Cases =
        [
            {insert, P, L, lists:sublist(L, P) ++ [~"NEW"] ++ lists:nthtail(P, L)}
         || P <- lists:seq(0, 6)
        ] ++
            [{remove, P, L, lists:sublist(L, P) ++ lists:nthtail(P + 1, L)} || P <- lists:seq(0, 5)] ++
            [
                {change, P, L, lists:sublist(L, P) ++ [~"CH"] ++ lists:nthtail(P + 1, L)}
             || P <- lists:seq(0, 5)
            ] ++
            [
                {reverse, 0, L, lists:reverse(L)},
                {swap_ends, 0, L, [lists:last(L)] ++ tl(lists:droplast(L)) ++ [hd(L)]},
                {shrink, 0, L20, lists:sublist(L20, 2)},
                {grow, 0, L20, L20 ++ [~"x", ~"y", ~"z"]},
                {empty_to_3, 0, [], [~"a", ~"b", ~"c"]},
                {to_empty, 0, [~"a", ~"b", ~"c"], []},
                {two_mid, 0, L, lists:sublist(L, 3) ++ [~"X", ~"Y"] ++ lists:nthtail(3, L)},
                {insert_and_change, 0, L, [~"Q", ~"CH"] ++ tl(L)},
                {no_change, 0, L, L}
            ],
    [
        begin
            Ops = each_list_diff_sr(vals_to_items(Old), vals_to_items(New)),
            ?assertEqual(
                {Kind, Pos, New},
                {Kind, Pos, replay_list_patch(Old, Ops)}
            )
        end
     || {Kind, Pos, Old, New} <- Cases
    ].

vals_to_items(Vs) -> [#{name => V} || V <- Vs].

%% Model of the client. `[]` means nothing changed; a wholesale `?OP_TEXT` re-render
%% replaces the container outright, so the new list is whatever the payload carries.
replay_list_patch(Old, []) ->
    Old;
replay_list_patch(_Old, [[?OP_TEXT, _Az, #{~"d" := Rows}]]) ->
    [V || [V] <- Rows];
replay_list_patch(Old, [[?OP_LIST_PATCH, _Az, Subs]]) ->
    Removed = [I || [?OP_REMOVE, I] <- Subs],
    Patched = #{I => V || [?OP_ITEM_PATCH, I, [[?OP_TEXT, _, V]]] <- Subs},
    Inserts = lists:foldl(
        fun([?OP_INSERT, I, #{~"d" := [V]}], Acc) ->
            maps:update_with(I, fun(Vs) -> Vs ++ [V] end, [V], Acc)
        end,
        #{},
        [Op || [?OP_INSERT, _, _] = Op <- Subs]
    ),
    Len = length(Old),
    Body = [
        maps:get(I, Inserts, []) ++
            case lists:member(I, Removed) of
                true -> [];
                false -> [maps:get(I, Patched, lists:nth(I + 1, Old))]
            end
     || I <- lists:seq(0, Len - 1)
    ],
    lists:append(Body) ++ maps:get(Len, Inserts, []).

%% Where the gate still hands over: a large shrink is a long run of removes, which
%% no amount of prefix/suffix stripping collapses, and one re-render of the few
%% survivors is smaller.
diff_list_large_shrink_falls_back_to_wholesale(Config) when is_list(Config) ->
    Old = [#{name => integer_to_binary(I)} || I <- lists:seq(1, 200)],
    ?assertMatch(
        [[?OP_TEXT, <<"0">>, #{~"t" := ?EACH}]],
        each_list_diff_sr(Old, lists:sublist(Old, 2))
    ).

%% And where it does not: a short container is not torn down for a trivial saving.
diff_list_small_shrink_stays_positional(Config) when is_list(Config) ->
    Old = [#{name => integer_to_binary(I)} || I <- lists:seq(1, 10)],
    ?assertMatch(
        [[?OP_LIST_PATCH, <<"0">>, _]],
        each_list_diff_sr(Old, lists:sublist(Old, 2))
    ).

%% And a late insert into the same list stays positional.
diff_list_late_insert_stays_positional(Config) when is_list(Config) ->
    Old = [#{name => integer_to_binary(I)} || I <- lists:seq(1, 10)],
    New = lists:sublist(Old, 9) ++ [#{name => ~"x"}] ++ lists:nthtail(9, Old),
    ?assertMatch([[?OP_INSERT, 9, _]], assert_list_patch(each_list_diff_sr(Old, New))).

%% Same map twice: no ops.
diff_map_no_change_no_ops(Config) when is_list(Config) ->
    Map = #{<<"a">> => <<"1">>, <<"b">> => <<"2">>},
    ?assertEqual([], each_map_diff(Map, Map)).

%% Regression: a plain-list `?each` sitting *among static sibling content* in the
%% same content slot. SSR anchors the each by its `<!--az:X-->...<!--/az-->`
%% comment markers (like every dynamic-text child) -- there is NO wrapper element
%% carrying `az="X"`. So the container op must be the marker-aware ?OP_TEXT: the
%% client's resolveEl can't find an element for the each's marker az and falls
%% back to the *enclosing* element, where a whole-element innerHTML write would
%% wipe the static sibling content. The mixed-siblings shape is what breaks; a
%% sole-child each only "works" with such a write by coincidence (the fallback
%% element is the right one). diff_each_among_siblings_to_empty_uses_text_op
%% covers the reverse
%% (non-empty -> []) toggle. Build the snapshot/template with sibling dynamics
%% before the each so the each's az is a marker slot distinct from the parent.

%% Diff a plain-list `?each` placed after two static sibling dynamics (the
%% sibling dynamics are unchanged, so only the each should emit an op). The each
%% transitions `Old` -> `New`. Returns the full op list.
each_among_siblings_diff(Old, New) ->
    ItemTmpl = #{
        t => ?EACH,
        s => [<<"<div class=\"item\" az=\"strip:2:0\"><span>">>, <<"</span></div>">>],
        d => fun(I) -> [{<<"strip:2:0">>, maps:get(name, I)}] end,
        f => <<"item">>
    },
    {OldItems, _} = arizona_eval:render_list_items(Old, ItemTmpl, {#{}, #{}}),
    %% Statics model:
    %%   <div class="strip" az="strip">
    %%     <div class="item" az="strip:0"><!--az:strip:0-->A<!--/az--></div>
    %%     <div class="item" az="strip:1"><!--az:strip:1-->B<!--/az--></div>
    %%     <!--az:strip:2-->EACH<!--/az-->
    %%   </div>
    %% The each's az (strip:2) is a marker slot, NOT any element's own az.
    Statics = [
        <<"<div class=\"strip\" az=\"strip\">",
            "<div class=\"item\" az=\"strip:0\"><!--az:strip:0-->">>,
        <<"<!--/az--></div><div class=\"item\" az=\"strip:1\"><!--az:strip:1-->">>,
        <<"<!--/az--></div><!--az:strip:2-->">>,
        <<"<!--/az--></div>">>
    ],
    OldSnap = #{
        s => Statics,
        d => [
            {<<"strip:0">>, <<"A">>},
            {<<"strip:1">>, <<"B">>},
            {<<"strip:2">>, #{t => ?EACH, items => OldItems, template => ItemTmpl}}
        ],
        deps => [#{a => true}, #{b => true}, #{rows => true}],
        f => <<"strip">>
    },
    NewTmpl = #{
        s => Statics,
        d => [
            {<<"strip:0">>, fun() -> <<"A">> end},
            {<<"strip:1">>, fun() -> <<"B">> end},
            {<<"strip:2">>, fun() -> arizona_template:each(New, ItemTmpl) end, {m, 1}}
        ],
        f => <<"strip">>
    },
    {Ops, _Snap, _Views} = arizona_diff:diff(NewTmpl, OldSnap, #{}, #{rows => true}),
    Ops.

%% `[]` -> non-empty: the each must patch its marker slot via ?OP_TEXT, never a
%% whole-element write (which would innerHTML-wipe the static sibling .item
%% divs). The unchanged sibling dynamics (strip:0/strip:1) must emit no ops.
diff_each_among_siblings_uses_text_op(Config) when is_list(Config) ->
    Ops = each_among_siblings_diff([], [#{name => <<"k">>}]),
    ?assertMatch([[?OP_TEXT, <<"strip:2">>, #{<<"t">> := ?EACH}]], Ops),
    %% Siblings untouched: no op targets strip:0 or strip:1.
    ?assertEqual(
        [],
        [Op || Op <- Ops, lists:member(lists:nth(2, Op), [<<"strip:0">>, <<"strip:1">>])]
    ),
    [[?OP_TEXT, <<"strip:2">>, #{<<"d">> := ItemDs}]] = Ops,
    ?assertEqual([[<<"k">>]], ItemDs).

%% non-empty -> `[]`: the reverse toggle must also use ?OP_TEXT (clearing only
%% the marker content), leaving the static siblings intact.
diff_each_among_siblings_to_empty_uses_text_op(Config) when is_list(Config) ->
    Ops = each_among_siblings_diff([#{name => <<"k">>}], []),
    ?assertMatch([[?OP_TEXT, <<"strip:2">>, #{<<"t">> := ?EACH}]], Ops),
    [[?OP_TEXT, <<"strip:2">>, #{<<"d">> := ItemDs}]] = Ops,
    ?assertEqual([], ItemDs).

%% The same rule for a STREAM `?each` among static siblings, against real
%% compiled-template SSR (`arizona_stream_siblings`) rather than a hand-built
%% snapshot. The container full-render is the one op the stream path emits at the
%% slot's own az; it must be the marker-aware ?OP_TEXT.
%%
%% Why it matters here more than for a sole-child each: SSR gives the slot the
%% COMPOUND az `<Root>:1` and no element carries it, so the client's element
%% lookups both miss -- the compound base az is the view ROOT's own az, which a
%% descendant-only `querySelector` cannot return -- and only the `<!--az:X-->`
%% marker resolves, to the ROOT. A whole-element innerHTML write there wipes the
%% header, the title slot's markers and the footer, and the view never recovers.
%% The load-bearing assertions are that the op is the marker-aware ?OP_TEXT, that
%% it targets a marker SSR actually anchored, and that NO element carries that az.
stream_siblings_ssr_and_ops(Old, New) ->
    B = #{id => <<"siblings">>, title => <<"T">>},
    T0 = arizona_stream_siblings:render(B#{items => Old}),
    {HTML, Snap0, Views0} = arizona_render:render(T0, #{}),
    T1 = arizona_stream_siblings:render(B#{items => New}),
    {Ops, _Snap1, _Views1} = arizona_diff:diff(T1, Snap0, Views0, #{items => true}),
    {iolist_to_binary(HTML), Ops}.

%% First emitter: `diff_stream/4`'s no-`order` clause -- the old snapshot was a
%% map-source each, so there is nothing to diff incrementally and the whole
%% container re-renders.
diff_stream_among_siblings_uses_text_op(Config) when is_list(Config) ->
    Old = #{<<"1">> => #{id => 1, label => <<"a">>}},
    New = arizona_stream:new(
        fun(#{id := Id}) -> integer_to_binary(Id) end,
        [#{id => 1, label => <<"a">>}, #{id => 2, label => <<"b">>}]
    ),
    {SSR, Ops} = stream_siblings_ssr_and_ops(Old, New),
    ?assertMatch([[?OP_TEXT, _, #{<<"t">> := ?EACH}]], Ops),
    [[?OP_TEXT, Az, _]] = Ops,
    %% The slot is marker-anchored and element-less: the exact shape that makes
    %% a whole-element write resolve to (and wipe) the view root.
    ?assertNotEqual(nomatch, binary:match(SSR, <<"<!--az:", Az/binary, "-->">>)),
    ?assertEqual(nomatch, binary:match(SSR, <<" az=\"", Az/binary, "\"">>)),
    %% The static siblings the wipe would have taken are really there.
    ?assertNotEqual(nomatch, binary:match(SSR, <<"class=\"header\"">>)),
    ?assertNotEqual(nomatch, binary:match(SSR, <<"class=\"footer\"">>)),
    %% ...and the re-render actually carries both items.
    [[?OP_TEXT, _, #{<<"d">> := ItemDs}]] = Ops,
    ?assertEqual(2, length(ItemDs)).

%% Second emitter: `make_op/3`'s stream `?EACH` clause, reached through the
%% child-view path (`diff_child_dynamics/2`) when the each lives in an embedded
%% `?stateful` child. Same slot shape, same requirement.
diff_stream_among_siblings_child_view_uses_text_op(Config) when is_list(Config) ->
    KeyFun = fun(#{id := Id}) -> integer_to_binary(Id) end,
    S0 = arizona_stream:new(KeyFun, [#{id => 1, label => <<"a">>}]),
    S1 = arizona_stream:new(KeyFun, [#{id => 1, label => <<"a">>}, #{id => 2, label => <<"b">>}]),
    B = #{id => <<"siblings-parent">>, title => <<"T">>},
    T0 = arizona_stream_siblings_parent:render(B#{items => S0}),
    {HTML, Snap0, Views0} = arizona_render:render(T0, #{}),
    SSR = iolist_to_binary(HTML),
    T1 = arizona_stream_siblings_parent:render(B#{title => <<"T2">>, items => S1}),
    {Ops, _Snap1, _Views1} = arizona_diff:diff(
        T1, Snap0, Views0, #{title => true, items => true}
    ),
    %% One child-view wrapper carrying the title patch plus the container
    %% re-render; the latter must be marker-aware.
    ?assertMatch([[<<"siblings">>, [[?OP_TEXT, _, <<"T2">>], [_, _, #{<<"t">> := ?EACH}]]]], Ops),
    [[<<"siblings">>, [_TitleOp, [Code, Az, _]]]] = Ops,
    ?assertEqual(?OP_TEXT, Code),
    ?assertNotEqual(nomatch, binary:match(SSR, <<"<!--az:", Az/binary, "-->">>)),
    ?assertEqual(nomatch, binary:match(SSR, <<" az=\"", Az/binary, "\"">>)).

diff_only_changed_emits_ops(Config) when is_list(Config) ->
    OldSnap = #{
        s => [<<"<div az=\"0\">">>, <<" ">>, <<"</div>">>],
        d => [{<<"0">>, <<"stay">>}, {<<"0">>, <<"change">>}]
    },
    NewTmpl = #{
        s => [<<"<div az=\"0\">">>, <<" ">>, <<"</div>">>],
        d => [{<<"0">>, fun() -> <<"stay">> end}, {<<"0">>, fun() -> <<"changed">> end}],
        f => <<"test">>
    },
    {Ops, _} = arizona_diff:diff(NewTmpl, OldSnap),
    ?assertEqual([[?OP_TEXT, <<"0">>, <<"changed">>]], Ops).

%% --- diff => false ---

no_diff_ops(Config) when is_list(Config) ->
    %% diff/2 produces no ops when old snapshot has diff => false
    OldSnap = #{
        s => [<<"<p az=\"0\">">>, <<"</p>">>],
        d => [{<<"0">>, <<"old">>}],
        diff => false
    },
    NewTmpl = #{
        s => [<<"<p az=\"0\">">>, <<"</p>">>],
        d => [{<<"0">>, fun() -> <<"new">> end}],
        diff => false,
        f => <<"test">>
    },
    {Ops, _} = arizona_diff:diff(NewTmpl, OldSnap),
    ?assertEqual([], Ops).

no_diff_nested(Config) when is_list(Config) ->
    %% Only the non-ignored dynamic produces ops
    OldSnap = #{
        s => [<<"<div az=\"0\">">>, <<" ">>, <<"</div>">>],
        d => [
            {<<"0">>, <<"title">>},
            {<<"1">>, #{
                s => [<<"<p>">>, <<"</p>">>],
                d => [{<<"i">>, <<"old">>}],
                diff => false
            }}
        ]
    },
    NewTmpl = #{
        s => [<<"<div az=\"0\">">>, <<" ">>, <<"</div>">>],
        d => [
            {<<"0">>, fun() -> <<"new title">> end},
            {<<"1">>, #{
                s => [<<"<p>">>, <<"</p>">>],
                d => [{<<"i">>, fun() -> <<"new">> end}],
                diff => false,
                f => <<"test">>
            }}
        ],
        f => <<"test">>
    },
    {Ops, _} = arizona_diff:diff(NewTmpl, OldSnap),
    ?assertEqual([[?OP_TEXT, <<"0">>, <<"new title">>]], Ops).

no_diff_skips_eval(Config) when is_list(Config) ->
    %% diff/4 reuses old value (no eval) for diff => false dynamic
    Bindings0 = #{title => <<"A">>, content => <<"old">>},
    T0 = #{
        s => [<<"<div az=\"0\">">>, <<" ">>, <<"</div>">>],
        d => [
            {<<"0">>, fun() -> arizona_template:get(title, Bindings0) end},
            {<<"1">>, #{
                s => [<<"<span>">>, <<"</span>">>],
                d => [{<<"i">>, fun() -> arizona_template:get(content, Bindings0) end}],
                diff => false,
                f => <<"test">>
            }}
        ],
        f => <<"test">>
    },
    {_, Snap0, V0} = arizona_render:render(T0, #{}),
    %% Change both title and content
    Bindings1 = #{title => <<"B">>, content => <<"new">>},
    T1 = #{
        s => [<<"<div az=\"0\">">>, <<" ">>, <<"</div>">>],
        d => [
            {<<"0">>, fun() -> arizona_template:get(title, Bindings1) end},
            {<<"1">>, #{
                s => [<<"<span>">>, <<"</span>">>],
                d => [{<<"i">>, fun() -> arizona_template:get(content, Bindings1) end}],
                diff => false,
                f => <<"test">>
            }}
        ],
        f => <<"test">>
    },
    Changed = compute_changed(Bindings0, Bindings1),
    {Ops, Snap1, _} = arizona_diff:diff(T1, Snap0, V0, Changed),
    %% Only title produces an op
    ?assertEqual([[?OP_TEXT, <<"0">>, <<"B">>]], Ops),
    %% The ignored dynamic retains old value in snapshot. Its inner az is
    %% namespaced by content slot `1` (scope_slot), so it reads back as `1-i`.
    [{<<"0">>, _}, {<<"1">>, IgnoredSnap}] = maps:get(d, Snap1),
    [{<<"1-i">>, ContentVal}] = maps:get(d, IgnoredSnap),
    ?assertEqual(<<"old">>, ContentVal).

no_diff_stateful_child(Config) when is_list(Config) ->
    %% Stateful child whose render/1 returns diff => false.
    %% After initial render, diff/3 with changed parent props emits no child ops
    %% and the child snapshot preserves diff => false.
    Bindings0 = #{title => <<"T">>, count => 1},
    T0 = #{
        s => [<<"<main az=\"0\">">>, <<" ">>, <<"</main>">>],
        d => [
            {<<"0">>, fun() -> arizona_template:get(title, Bindings0) end},
            {<<"1">>,
                arizona_template:stateful(
                    arizona_no_diff_counter,
                    #{id => <<"nd_counter">>, count => arizona_template:get(count, Bindings0)}
                )}
        ],
        f => <<"test">>
    },
    {_HTML0, Snap0, Views0} = arizona_render:render(T0, #{}),
    %% Verify child is in views and snapshot has diff => false
    ?assertMatch(#{<<"nd_counter">> := #{handler := arizona_no_diff_counter}}, Views0),
    [{<<"0">>, _}, {<<"1">>, ChildSnap0}] = maps:get(d, Snap0),
    ?assertEqual(false, maps:get(diff, ChildSnap0)),
    %% Now diff/3 with changed count -- child should produce no ops
    Bindings1 = #{title => <<"T">>, count => 99},
    T1 = #{
        s => [<<"<main az=\"0\">">>, <<" ">>, <<"</main>">>],
        d => [
            {<<"0">>, fun() -> arizona_template:get(title, Bindings1) end},
            {<<"1">>,
                arizona_template:stateful(
                    arizona_no_diff_counter,
                    #{id => <<"nd_counter">>, count => arizona_template:get(count, Bindings1)}
                )}
        ],
        f => <<"test">>
    },
    {Ops1, Snap1, _Views1} = arizona_diff:diff(T1, Snap0, Views0),
    %% No child ops -- only empty child op list or none at all
    lists:foreach(
        fun(Op) ->
            case Op of
                [<<"nd_counter">>, ChildOps] -> ?assertEqual([], ChildOps);
                _ -> ok
            end
        end,
        Ops1
    ),
    %% Child snapshot still has diff => false
    [{<<"0">>, _}, {<<"1">>, ChildSnap1}] = maps:get(d, Snap1),
    ?assertEqual(false, maps:get(diff, ChildSnap1)).

no_diff_diff3(Config) when is_list(Config) ->
    %% diff/3 path: nested diff => false dynamic produces no ops.
    %% Note: diff/3 evaluates new dynamics (funs are called) but diff_dynamics
    %% skips op generation when the OLD snapshot has diff => false. The new
    %% snapshot contains the freshly evaluated values (unlike diff/4 which
    %% reuses old values entirely).
    Bindings0 = #{title => <<"A">>, content => <<"old">>},
    T0 = #{
        s => [<<"<div az=\"0\">">>, <<" ">>, <<"</div>">>],
        d => [
            {<<"0">>, fun() -> arizona_template:get(title, Bindings0) end},
            {<<"1">>, #{
                s => [<<"<span>">>, <<"</span>">>],
                d => [{<<"i">>, fun() -> arizona_template:get(content, Bindings0) end}],
                diff => false,
                f => <<"test">>
            }}
        ],
        f => <<"test">>
    },
    {_HTML0, Snap0, Views0} = arizona_render:render(T0, #{}),
    %% Change both title and content, diff/3 (no Changed map)
    Bindings1 = #{title => <<"B">>, content => <<"new">>},
    T1 = #{
        s => [<<"<div az=\"0\">">>, <<" ">>, <<"</div>">>],
        d => [
            {<<"0">>, fun() -> arizona_template:get(title, Bindings1) end},
            {<<"1">>, #{
                s => [<<"<span>">>, <<"</span>">>],
                d => [{<<"i">>, fun() -> arizona_template:get(content, Bindings1) end}],
                diff => false,
                f => <<"test">>
            }}
        ],
        f => <<"test">>
    },
    {Ops, Snap1, _Views1} = arizona_diff:diff(T1, Snap0, Views0),
    %% Only title produces an op -- the diff => false nested dynamic is skipped
    ?assertEqual([[?OP_TEXT, <<"0">>, <<"B">>]], Ops),
    %% Nested snapshot has diff => false and freshly evaluated content
    [{<<"0">>, _}, {<<"1">>, NestedSnap}] = maps:get(d, Snap1),
    ?assertEqual(false, maps:get(diff, NestedSnap)).

no_diff_diff4_top_level(Config) when is_list(Config) ->
    %% diff/4 with top-level diff => false short-circuits entirely
    OldSnap = #{
        s => [<<"<div az=\"0\">">>, <<"</div>">>],
        d => [{<<"0">>, <<"old">>}],
        deps => [#{title => true}],
        diff => false
    },
    NewTmpl = #{
        s => [<<"<div az=\"0\">">>, <<"</div>">>],
        d => [{<<"0">>, fun() -> <<"new">> end}],
        diff => false,
        f => <<"test">>
    },
    Views = #{},
    Changed = #{title => <<"new">>},
    {Ops, ResultSnap, ResultViews} = arizona_diff:diff(NewTmpl, OldSnap, Views, Changed),
    %% Complete short-circuit: no ops, old snapshot unchanged, views unchanged
    ?assertEqual([], Ops),
    ?assertEqual(OldSnap, ResultSnap),
    ?assertEqual(Views, ResultViews).

diff_no_diff_stateful_child_diff4(Config) when is_list(Config) ->
    %% diff/4 skips eval entirely for stateful child with diff => false
    %% when the child's deps haven't changed
    Bindings0 = #{title => <<"T">>, count => 1},
    T0 = #{
        s => [<<"<main az=\"0\">">>, <<" ">>, <<"</main>">>],
        d => [
            {<<"0">>, fun() -> arizona_template:get(title, Bindings0) end},
            {<<"1">>,
                arizona_template:stateful(
                    arizona_no_diff_counter,
                    #{id => <<"nd_counter">>, count => arizona_template:get(count, Bindings0)}
                )}
        ],
        f => <<"test">>
    },
    {_HTML0, Snap0, Views0} = arizona_render:render(T0, #{}),
    %% Change only title (not count) -- child deps haven't changed
    Bindings1 = #{title => <<"New Title">>, count => 1},
    T1 = #{
        s => [<<"<main az=\"0\">">>, <<" ">>, <<"</main>">>],
        d => [
            {<<"0">>, fun() -> arizona_template:get(title, Bindings1) end},
            {<<"1">>,
                arizona_template:stateful(
                    arizona_no_diff_counter,
                    #{id => <<"nd_counter">>, count => arizona_template:get(count, Bindings1)}
                )}
        ],
        f => <<"test">>
    },
    Changed = compute_changed(Bindings0, Bindings1),
    {Ops, Snap1, _Views1} = arizona_diff:diff(T1, Snap0, Views0, Changed),
    %% Only title op, no child ops
    ?assertEqual([[?OP_TEXT, <<"0">>, <<"New Title">>]], Ops),
    %% Child snapshot preserved with diff => false
    [{<<"0">>, _}, {<<"1">>, ChildSnap}] = maps:get(d, Snap1),
    ?assertEqual(false, maps:get(diff, ChildSnap)).

%% A ?local whose init reads a server binding (?get) is seeded once: even in the
%% dep-aware diff with that binding in the Changed set, the slot is skipped. The
%% `#{diff := false}` clause precedes the deps-changed check, so a server-side
%% change to the init's dependency never re-renders the client-owned slot.
local_dep_aware_skip(Config) when is_list(Config) ->
    B0 = #{foo => <<"a">>},
    T0 = #{
        s => [<<"<span>">>, <<"</span>">>],
        d => [
            {<<"0">>, fun() -> arizona_template:local(<<"k">>, arizona_template:get(foo, B0)) end}
        ],
        f => <<"test">>,
        backend => arizona_html
    },
    {_HTML, Snap0, V0} = arizona_render:render(T0, #{}),
    B1 = #{foo => <<"b">>},
    T1 = #{
        s => [<<"<span>">>, <<"</span>">>],
        d => [
            {<<"0">>, fun() -> arizona_template:local(<<"k">>, arizona_template:get(foo, B1)) end}
        ],
        f => <<"test">>,
        backend => arizona_html
    },
    Changed = compute_changed(B0, B1),
    {Ops, _Snap1, _V1} = arizona_diff:diff(T1, Snap0, V0, Changed),
    ?assertEqual([], Ops).

%% --- content-slot conditional dependency tracking ---

%% A `case` in a content slot with a constant scrutinee (`flag`) and a branch that
%% reads `val`: changing `val` must re-render the branch (was frozen -- `[]`). The
%% hoisted-variable form (`case_var`, inlined back into the slot) produces the same
%% diff.
conditional_case_branch_tracks(Config) when is_list(Config) ->
    B0 = #{flag => true, val => <<"A">>},
    B1 = #{flag => true, val => <<"B">>},
    Changed = #{val => true},
    OpsInline = cond_diff(case_branch, B0, B1, Changed),
    OpsVar = cond_diff(case_var, B0, B1, Changed),
    ?assertMatch([[?OP_TEXT, _, _]], OpsInline),
    ?assert(cond_payload_has(OpsInline, <<"B">>)),
    ?assertEqual(OpsInline, OpsVar).

conditional_if_branch_tracks(Config) when is_list(Config) ->
    B0 = #{flag => true, val => <<"A">>},
    B1 = #{flag => true, val => <<"B">>},
    Changed = #{val => true},
    OpsInline = cond_diff(if_branch, B0, B1, Changed),
    OpsVar = cond_diff(if_var, B0, B1, Changed),
    ?assertMatch([[?OP_TEXT, _, _]], OpsInline),
    ?assert(cond_payload_has(OpsInline, <<"B">>)),
    ?assertEqual(OpsInline, OpsVar).

conditional_maybe_branch_tracks(Config) when is_list(Config) ->
    B0 = #{flag => true, val => <<"A">>},
    B1 = #{flag => true, val => <<"B">>},
    Changed = #{val => true},
    OpsInline = cond_diff(maybe_branch, B0, B1, Changed),
    OpsVar = cond_diff(maybe_var, B0, B1, Changed),
    ?assertMatch([[?OP_TEXT, _, _]], OpsInline),
    ?assert(cond_payload_has(OpsInline, <<"B">>)),
    ?assertEqual(OpsInline, OpsVar).

%% A read in an inner branch of a nested conditional, both scrutinees constant.
conditional_nested_tracks(Config) when is_list(Config) ->
    B0 = #{outer => true, inner => true, val => <<"A">>},
    B1 = #{outer => true, inner => true, val => <<"B">>},
    Changed = #{val => true},
    OpsInline = cond_diff(nested, B0, B1, Changed),
    OpsVar = cond_diff(nested_var, B0, B1, Changed),
    ?assertMatch([[?OP_TEXT, _, _]], OpsInline),
    ?assert(cond_payload_has(OpsInline, <<"B">>)),
    ?assertEqual(OpsInline, OpsVar).

%% Over-tracking is op-free: changing a key read only in the non-taken branch
%% re-evaluates the slot but the equal snapshot emits no op. Changing the taken
%% branch's key emits one op.
conditional_over_track_op_free(Config) when is_list(Config) ->
    Base = #{flag => true, a => <<"A">>, b => <<"B">>},
    NonTaken = cond_diff(over_track, Base, Base#{b => <<"B2">>}, #{b => true}),
    ?assertEqual([], NonTaken),
    ?assertEqual([], cond_diff(over_track_var, Base, Base#{b => <<"B2">>}, #{b => true})),
    Taken = cond_diff(over_track, Base, Base#{a => <<"A2">>}, #{a => true}),
    ?assertMatch([[?OP_TEXT, _, _]], Taken),
    ?assert(cond_payload_has(Taken, <<"A2">>)).

%% The injected touch records the key via track/1 (no read), so rendering the taken
%% branch never raises missing_binding when the other branch's binding is absent.
%% Switching to the branch that reads it (now present) re-renders.
conditional_missing_binding_safe(Config) when is_list(Config) ->
    M = arizona_conditional_freeze,
    %% A plain match is the assertion: rendering the taken (`_`) branch must not raise
    %% missing_binding even though the admin branch reads the absent `secret`. A crash
    %% would fail the test here.
    {_H1, _S1, _V1} = arizona_render:render(M:optional_key(#{mode => user}), #{}),
    {_H2, _S2, _V2} = arizona_render:render(M:optional_key_var(#{mode => user}), #{}),
    B0 = #{mode => user},
    B1 = #{mode => admin, secret => <<"S">>},
    Ops = cond_diff(optional_key, B0, B1, #{mode => true}),
    ?assertMatch([[?OP_TEXT, _, _]], Ops),
    ?assert(cond_payload_has(Ops, <<"S">>)).

%% A read three scrutinees deep must still re-render (recursion through nested
%% control flow in collect_branch_keys/2).
conditional_deep_nesting_tracks(Config) when is_list(Config) ->
    B0 = #{a => true, b => true, c => true, val => <<"A">>},
    B1 = B0#{val => <<"B">>},
    Ops = cond_diff(deep, B0, B1, #{val => true}),
    ?assertMatch([[?OP_TEXT, _, _]], Ops),
    ?assert(cond_payload_has(Ops, <<"B">>)).

%% A read in a branch element's attribute (not its content) is tracked: the whole
%% element subtree is walked, attributes included. The fine-grained inner diff emits a
%% precise ?OP_SET_ATTR on the inner element, not a whole-branch ?OP_TEXT.
conditional_attr_in_branch_tracks(Config) when is_list(Config) ->
    B0 = #{flag => true, cls => <<"c1">>},
    B1 = #{flag => true, cls => <<"c2">>},
    Ops = cond_diff(attr_in_branch, B0, B1, #{cls => true}),
    ?assertMatch([[?OP_SET_ATTR, _, <<"class">>, <<"c2">>]], Ops).

%% A `try ... of` body is a walked tail position: the `of` element branch's read is
%% tracked.
conditional_try_branch_tracks(Config) when is_list(Config) ->
    B0 = #{flag => true, val => <<"A">>},
    B1 = #{flag => true, val => <<"B">>},
    Ops = cond_diff(try_branch, B0, B1, #{val => true}),
    ?assertMatch([[?OP_TEXT, _, _]], Ops),
    ?assert(cond_payload_has(Ops, <<"B">>)).

%% A `begin ... end` block is a walked tail position: the block's last-expression element
%% branch reads `val`, which must be tracked so a `val` change re-renders the slot.
conditional_block_branch_tracks(Config) when is_list(Config) ->
    B0 = #{cls => <<"c">>, val => <<"A">>},
    B1 = #{cls => <<"c">>, val => <<"B">>},
    Ops = cond_diff(block_branch, B0, B1, #{val => true}),
    ?assertMatch([[?OP_TEXT, _, <<"B">>]], Ops).

%% A `receive ... after` body is a walked tail position: the `after 0` element branch
%% reads `val`, which must be tracked. `after 0` never blocks the render.
conditional_receive_branch_tracks(Config) when is_list(Config) ->
    B0 = #{val => <<"A">>},
    B1 = #{val => <<"B">>},
    Ops = cond_diff(receive_branch, B0, B1, #{val => true}),
    ?assertMatch([[?OP_TEXT, _, <<"B">>]], Ops).

%% A branch reading via `?get_lazy` (not `?get`): collect_call_keys extracts the key from
%% a get_lazy call, so a `val` change re-renders the slot.
conditional_get_lazy_branch_tracks(Config) when is_list(Config) ->
    B0 = #{flag => true, val => <<"A">>},
    B1 = #{flag => true, val => <<"B">>},
    Ops = cond_diff(get_lazy_branch, B0, B1, #{val => true}),
    ?assertMatch([[?OP_TEXT, _, <<"B">>]], Ops).

%% A branch projecting a bindings subset via `az:with`: collect_call_keys extracts each
%% key from the `with` list, so a `val` change re-renders the slot.
conditional_with_branch_tracks(Config) when is_list(Config) ->
    B0 = #{flag => true, val => <<"A">>},
    B1 = #{flag => true, val => <<"B">>},
    Ops = cond_diff(with_branch, B0, B1, #{val => true}),
    ?assertMatch([[?OP_TEXT, _, <<"B">>]], Ops).

%% The same projection written with the `?with` macro (expanding to
%% `arizona_template:with/2`, not the `az` alias): the macro must expand into the shape
%% collect_call_keys matches, so a `val` change re-renders the slot here too.
conditional_with_macro_branch_tracks(Config) when is_list(Config) ->
    B0 = #{flag => true, val => <<"A">>},
    B1 = #{flag => true, val => <<"B">>},
    Ops = cond_diff(with_macro_branch, B0, B1, #{val => true}),
    ?assertMatch([[?OP_TEXT, _, <<"B">>]], Ops).

%% Documents the known limitation: a computed-key branch read (`?get(Key)`, Key a variable)
%% is not auto-tracked, so changing the binding it names freezes the slot (no op). The
%% literal `which` read still tracks; only the computed read is missed. The value-form
%% workaround remains for authors who need reactivity here.
conditional_computed_key_not_tracked(Config) when is_list(Config) ->
    B0 = #{flag => true, which => val, val => <<"A">>},
    B1 = #{flag => true, which => val, val => <<"B">>},
    Ops = cond_diff(computed_key, B0, B1, #{val => true}),
    ?assertEqual([], Ops).

%% Phase 2 payoff: a branch with two text slots, changing one emits exactly one inner
%% op at that slot's az (the sibling slot is left untouched -- no whole-branch
%% re-render). Changing the other slot targets a different az.
conditional_two_slot_fine_grained(Config) when is_list(Config) ->
    Base = #{flag => true, a => <<"A">>, b => <<"B">>},
    OpsA = cond_diff(two_slot, Base, Base#{a => <<"A2">>}, #{a => true}),
    ?assertMatch([[?OP_TEXT, _, <<"A2">>]], OpsA),
    OpsB = cond_diff(two_slot, Base, Base#{b => <<"B2">>}, #{b => true}),
    ?assertMatch([[?OP_TEXT, _, <<"B2">>]], OpsB),
    [[?OP_TEXT, AzA, _]] = OpsA,
    [[?OP_TEXT, AzB, _]] = OpsB,
    ?assertNotEqual(AzA, AzB).

%% Nested-nested templates (an element branch wrapping another conditional whose branch
%% is an element): fine-graining recurses (make_ops -> diff_dynamics -> make_ops) to
%% emit a single op at the deepest changed slot, not a wholesale re-render at any wrapper
%% level.
conditional_nested_element_recurses(Config) when is_list(Config) ->
    B0 = #{flag => true, inner => true, val => <<"A">>},
    B1 = B0#{val => <<"B">>},
    Ops = cond_diff(nested_element, B0, B1, #{val => true}),
    ?assertMatch([[?OP_TEXT, _, <<"B">>]], Ops).

%% A scalar `?get` value is sent RAW on the diff wire (a bare binary) -- escaping happens
%% client-side (a text node shows `<` literally, matching SSR). It is NOT HTML-tagged, so
%% the client text-nodes it and a value containing markup cannot inject.
wire_get_value_raw(Config) when is_list(Config) ->
    M = arizona_conditional_freeze,
    {_HTML, S0, V0} = arizona_render:render(M:top_text(#{name => <<"<b>a</b>">>}), #{}),
    {Ops, _, _} = arizona_diff:diff(M:top_text(#{name => <<"<b>z</b>">>}), S0, V0, #{name => true}),
    ?assertMatch([[?OP_TEXT, _, <<"<b>z</b>">>]], Ops).

%% A `?raw` trusted-HTML value is tagged `#{~"raw" => Html}` (a map, not a bare string) so
%% the wire marks it HTML; the client unwraps and innerHTMLs it, keeping the escape opt-out
%% across the live diff.
wire_raw_value_tagged_html(Config) when is_list(Config) ->
    M = arizona_conditional_freeze,
    {_HTML, S0, V0} = arizona_render:render(M:raw_text(#{html => <<"<b>a</b>">>}), #{}),
    {Ops, _, _} = arizona_diff:diff(M:raw_text(#{html => <<"<b>z</b>">>}), S0, V0, #{html => true}),
    ?assertMatch([[?OP_TEXT, _, #{~"raw" := <<"<b>z</b>">>}]], Ops).

%% A raw tuple returned by a HELPER (not a literal `?raw` at the slot) is treated as a
%% plain scalar and ESCAPED: the wire value is a bare text-node string (the client
%% text-nodes it, so markup shows literally), never the `#{~"raw" => _}` innerHTML tag
%% that a literal `?raw` gets above. This enforces the documented literal-only rule --
%% before the fix, `mark_esc/1` passed the raw tuple through and the first live update
%% injected the markup as trusted HTML.
wire_helper_raw_value_escaped(Config) when is_list(Config) ->
    M = arizona_conditional_freeze,
    {HTML0, S0, V0} = arizona_render:render(M:helper_raw(#{html => <<"<b>a</b>">>}), #{}),
    %% First load (SSR): the helper-returned raw is escaped, no trusted markup.
    HTML = iolist_to_binary(HTML0),
    ?assertNotEqual(nomatch, binary:match(HTML, <<"&lt;b&gt;a&lt;/b&gt;">>)),
    ?assertEqual(nomatch, binary:match(HTML, <<"<b>a</b>">>)),
    %% First live update: still a bare text-node string, never a `#{~"raw" => _}` tag.
    {Ops, _, _} = arizona_diff:diff(
        M:helper_raw(#{html => <<"<b>z</b>">>}), S0, V0, #{html => true}
    ),
    ?assertMatch([[?OP_TEXT, _, <<"<b>z</b>">>]], Ops).

%% =============================================================================
%% Markerless (raw-text) slots inside items and child views
%% =============================================================================
%% A raw-text element's content slot (script/style/textarea/title) has
%% Az = undefined -- markerless, render-once (see the arizona_diff moduledoc).
%% The top-level walkers (diff_dynamics/3, diff_dynamics_v/5) always skipped
%% it; these cases guard the per-item and child-view walkers, which used to
%% emit ops targeting `undefined` (a dangling target the client resolves to
%% the item element itself, wiping its static children).

%% Build a limit-free stream page whose item template holds a markerless
%% (textarea) content slot, render it, apply `Mutate` to the stream, and diff.
markerless_stream_diff(Mutate) ->
    KeyFun = fun(#{id := Id}) -> Id end,
    ItemTmpl = #{
        t => ?EACH,
        s => [<<"<li az-key=\"">>, <<"\"><textarea>">>, <<"</textarea></li>">>],
        d => fun(Item, Key) ->
            [{<<"0">>, fun() -> Key end}, {undefined, fun() -> maps:get(text, Item) end}]
        end,
        f => <<"item">>
    },
    B0 = #{id => <<"t">>, items => arizona_stream:new(KeyFun, [#{id => 1, text => <<"a">>}])},
    Tmpl0 = #{
        s => [<<"<ul az=\"0\">">>, <<"</ul>">>],
        d => [
            {<<"0">>, fun() -> arizona_template:each(arizona_template:get(items, B0), ItemTmpl) end}
        ],
        f => <<"p">>
    },
    {_, Snap0, V0} = arizona_render:render(Tmpl0, #{}),
    B1 = arizona_stream:clear_stream_pending(B0, arizona_stream:stream_keys(B0)),
    B2 = B1#{items => Mutate(maps:get(items, B1))},
    Tmpl1 = #{
        s => [<<"<ul az=\"0\">">>, <<"</ul>">>],
        d => [
            {<<"0">>, fun() -> arizona_template:each(arizona_template:get(items, B2), ItemTmpl) end,
                {m, 1}}
        ],
        f => <<"p">>
    },
    Changed = compute_changed(B1, B2),
    {Ops, _Snap1, _V1} = arizona_diff:diff(Tmpl1, Snap0, V0, Changed),
    Ops.

%% Updating a stream item's markerless slot emits nothing at all: the slot is
%% render-once (no comment marker to target). Previously the per-item walker
%% emitted an ?OP_ITEM_PATCH whose inner op targeted `undefined`.
markerless_stream_item_update_no_dangling_op(Config) when is_list(Config) ->
    Ops = markerless_stream_diff(fun(S) ->
        arizona_stream:update(S, 1, #{id => 1, text => <<"b">>})
    end),
    ?assertEqual([], Ops).

%% Diff a single-root plain-list `?each` whose item template holds a markerless
%% (textarea) content slot beside a normal marker slot.
markerless_list_diff(Old, New) ->
    ItemTmpl = #{
        t => ?EACH,
        s => [<<"<li az=\"0\">">>, <<": <textarea>">>, <<"</textarea></li>">>],
        d => fun(I) -> [{<<"0">>, maps:get(name, I)}, {undefined, maps:get(text, I)}] end,
        f => <<"item">>,
        single_root => true
    },
    {OldItems, _} = arizona_eval:render_list_items(Old, ItemTmpl, {#{}, #{}}),
    OldSnap = #{
        s => [<<"<ul az=\"0\">">>, <<"</ul>">>],
        d => [{<<"0">>, #{t => ?EACH, items => OldItems, template => ItemTmpl}}],
        deps => [#{rows => true}],
        f => <<"parent">>
    },
    NewTmpl = #{
        s => [<<"<ul az=\"0\">">>, <<"</ul>">>],
        d => [{<<"0">>, fun() -> arizona_template:each(New, ItemTmpl) end, {m, 1}}],
        f => <<"parent">>
    },
    {Ops, _Snap, _Views} = arizona_diff:diff(NewTmpl, OldSnap, #{}, #{rows => true}),
    Ops.

%% A change to the markerless slot cannot ride a per-item patch (no op target),
%% so the list is not positionally patchable: the wholesale marker-aware
%% ?OP_TEXT re-render delivers the raw-text content instead. Previously an
%% OP_LIST_PATCH carried an inner op targeting `undefined` and the change was
%% lost (or destructive) client-side.
markerless_list_item_falls_back_to_full_update(Config) when is_list(Config) ->
    Ops = markerless_list_diff(
        [#{name => <<"a">>, text => <<"x">>}],
        [#{name => <<"a">>, text => <<"y">>}]
    ),
    ?assertMatch(
        [[?OP_TEXT, <<"0">>, #{<<"t">> := ?EACH, <<"d">> := [[<<"a">>, <<"y">>]]}]], Ops
    ).

%% The wholesale fallback fires only on an actual change: an identical list
%% emits nothing.
markerless_list_no_change_no_ops(Config) when is_list(Config) ->
    Items = [#{name => <<"a">>, text => <<"x">>}],
    ?assertEqual([], markerless_list_diff(Items, Items)).

%% A stateful child whose only change is its markerless textarea slot emits no
%% ops at all -- render-once, same as a root-level raw-text slot. Previously
%% the child walker emitted `[ViewId, [[?OP_TEXT, undefined, _]]]` and the
%% client dropped the dangling update with a console warning.
markerless_stateful_child_update_targets_element_az(Config) when is_list(Config) ->
    B0 = #{text => <<"one">>},
    T0 = #{
        s => [<<"<main az=\"0\"><!--az:0-->">>, <<"<!--/az--></main>">>],
        d => [
            {<<"0">>,
                arizona_template:stateful(
                    arizona_textarea_child,
                    #{id => <<"c">>, text => arizona_template:get(text, B0)}
                )}
        ],
        f => <<"test">>
    },
    {_HTML, Snap0, V0} = arizona_render:render(T0, #{}),
    B1 = #{text => <<"two">>},
    T1 = #{
        s => [<<"<main az=\"0\"><!--az:0-->">>, <<"<!--/az--></main>">>],
        d => [
            {<<"0">>,
                arizona_template:stateful(
                    arizona_textarea_child,
                    #{id => <<"c">>, text => arizona_template:get(text, B1)}
                )}
        ],
        f => <<"test">>
    },
    {Ops, _Snap1, _V1} = arizona_diff:diff(T1, Snap0, V0),
    %% The `<textarea>` carries no markers, so before raw-text elements were given
    %% their own az there was nothing to target and the change was dropped. Now the
    %% child ships one op scoped to that element, not a re-render of the whole child.
    ?assertMatch([[~"c", [[?OP_TEXT, _Az, #{~"d" := [~"two"]}]]]], Ops).

%% =============================================================================
%% Helpers
%% =============================================================================

%% Render arizona_conditional_freeze:Fn(B0), then diff against Fn(B1) with Changed.
cond_diff(Fn, B0, B1, Changed) ->
    M = arizona_conditional_freeze,
    {_HTML, Snap0, V0} = arizona_render:render(M:Fn(B0), #{}),
    {Ops, _Snap1, _V1} = arizona_diff:diff(M:Fn(B1), Snap0, V0, Changed),
    Ops.

%% The OP_TEXT payload re-rendering a nested template is a zip map
%% (#{<<"s">> => Statics, <<"d">> => DynValues, <<"f">> => Fp}); the changed value
%% lives in <<"d">>. Search the whole term for the needle (statics are lowercase
%% base-36 fingerprints + markup, so an uppercase value needle can't false-match).
cond_payload_has([[?OP_TEXT, _Az, Payload]], Needle) ->
    cond_term_has(Payload, Needle).

cond_term_has(Bin, Needle) when is_binary(Bin) ->
    binary:match(Bin, Needle) =/= nomatch;
cond_term_has(List, Needle) when is_list(List) ->
    lists:any(fun(E) -> cond_term_has(E, Needle) end, List);
cond_term_has(Map, Needle) when is_map(Map) ->
    cond_term_has(maps:values(Map), Needle);
cond_term_has(_Other, _Needle) ->
    false.

%% Mirrors arizona_live:compute_changed/2 for unit tests
compute_changed(OldBindings, NewBindings) ->
    maps:filter(
        fun(K, V) ->
            case OldBindings of
                #{K := V} -> false;
                #{} -> true
            end
        end,
        NewBindings
    ).

diff_stream_unchanged_snapshot_pair_no_ops(Config) when is_list(Config) ->
    %% Two stream-`?each` snapshots compared snapshot-against-snapshot rather
    %% than against the each descriptor. That happens whenever an embedded child
    %% view's inner dynamics are diffed from its parent: the freshly evaluated
    %% side carries `source`, while the stored side does not -- the incremental
    %% stream path settles its snapshot without it -- so the two never compare
    %% term-equal even when they render identically. Emitting the container-level
    %% `?OP_TEXT` there re-renders a list the client already holds, destroying
    %% focus, scroll, uncontrolled input state and every `?local` in the items,
    %% so an unchanged pair must produce nothing at all.
    ItemTmpl = #{
        t => ?EACH,
        s => [<<"<li az=\"0\">">>, <<"</li>">>],
        d => fun(I, _Key) -> [{<<"0">>, maps:get(text, I)}] end,
        f => <<"item">>
    },
    Tmpl = stream_each_tmpl(ItemTmpl, [
        #{id => <<"a">>, text => <<"A">>}, #{id => <<"b">>, text => <<"B">>}
    ]),
    {Az, EachSnap, DepsList} = eval_stream_each(Tmpl),
    Statics = maps:get(s, Tmpl),
    %% Same items, same order, same item template -- only `source` differs.
    Settled = maps:remove(source, EachSnap),
    OldSnap = #{s => Statics, d => [{Az, Settled}], deps => DepsList},
    ?assertEqual([], element(1, arizona_diff:diff(Tmpl, OldSnap, #{}))),
    %% Control: a genuinely different list produces the real per-item delta, not a
    %% container re-render. This pair reaches the nested-template walk, which used
    %% to have no stream route and fell back to `?OP_TEXT` -- the very re-render
    %% the comment above argues against. One `?OP_INSERT` for the item the stale
    %% snapshot lacks is both cheaper and non-destructive.
    OtherTmpl = stream_each_tmpl(ItemTmpl, [#{id => <<"a">>, text => <<"A">>}]),
    {OtherAz, OtherEach, OtherDepsList} = eval_stream_each(OtherTmpl),
    StaleSnap = #{
        s => Statics, d => [{OtherAz, maps:remove(source, OtherEach)}], deps => OtherDepsList
    },
    ?assertMatch(
        [[?OP_INSERT, _, <<"b">>, -1, _]], element(1, arizona_diff:diff(Tmpl, StaleSnap, #{}))
    ).

%% A stream `?each` sitting inside a NESTED template -- what a `?stateless` child
%% renders to -- must still diff per item. The incremental path was wired only
%% into a template's own top-level dynamics (`diff_changed_dynamic`) and into
%% stream items; the nested walk had no stream route, so it fell through to the
%% container `?OP_TEXT` and re-sent the whole list. That is O(N) on the wire for
%% an O(1) change, with no symptom except payload size, and it silently degrades
%% the documented habit of factoring a container into a `?stateless`.
diff_stream_nested_in_template_is_incremental(Config) when is_list(Config) ->
    ItemTmpl = #{
        t => ?EACH,
        s => [<<"<li az=\"0\">">>, <<"</li>">>],
        d => fun(I, _Key) -> [{<<"0">>, maps:get(text, I)}] end,
        f => <<"item">>
    },
    Items = [#{id => <<"a">>, text => <<"A">>}, #{id => <<"b">>, text => <<"B">>}],
    Inner = stream_each_tmpl(ItemTmpl, Items),
    %% Wrap the each's template in an outer template, so its dynamics are reached
    %% through the nested-template walk rather than the top-level one.
    Nest = fun(T) ->
        #{
            s => [<<"<div az=\"n\">">>, <<"</div>">>],
            d => [{<<"n">>, fun() -> T end}],
            f => <<"outer">>
        }
    end,
    {_, Snap, _} = arizona_render:render(Nest(Inner), #{}),
    %% Append one item and diff: one insert, not a container re-render.
    Grown = stream_each_tmpl(ItemTmpl, Items ++ [#{id => <<"c">>, text => <<"C">>}]),
    {Ops, _, _} = arizona_diff:diff(Nest(Grown), Snap, #{}),
    ?assertMatch([[?OP_INSERT, _, <<"c">>, -1, _]], Ops).

%% The nested-template stream route derives its ops purely by draining the
%% stream's pending log. A container reached through a `?stateful` child has an
%% empty log -- `arizona_eval` clears it before the child renders, to stop a
%% prop-fed child accumulating one entry per root update -- so draining yields
%% NOTHING while the order has in fact changed, and the container silently never
%% updates. That is worse than the wholesale render it replaced: zero delivered
%% rather than everything delivered expensively. When the log cannot account for
%% the order difference, reconcile the two key orders instead -- semantically a
%% reset to the current state -- so the change is delivered AND stays per-item.
diff_stream_nested_with_cleared_log_reconciles(Config) when is_list(Config) ->
    ItemTmpl = #{
        t => ?EACH,
        s => [<<"<li az=\"0\">">>, <<"</li>">>],
        d => fun(I, _Key) -> [{<<"0">>, maps:get(text, I)}] end,
        f => <<"item">>
    },
    Inner = stream_each_tmpl(ItemTmpl, [#{id => <<"a">>, text => <<"A">>}]),
    Nest = fun(T) ->
        #{
            s => [<<"<div az=\"n\">">>, <<"</div>">>],
            d => [{<<"n">>, fun() -> T end}],
            f => <<"outer">>
        }
    end,
    {_, Snap, _} = arizona_render:render(Nest(Inner), #{}),
    %% A stream whose order grew but whose pending log is empty -- exactly the
    %% state a `?stateful` child hands down.
    Grown = stream_each_tmpl(ItemTmpl, [
        #{id => <<"a">>, text => <<"A">>}, #{id => <<"b">>, text => <<"B">>}
    ]),
    #{d := [{_, GrownFun}]} = Grown,
    Cleared = maps:get(source, GrownFun()),
    #{st := Drained} = arizona_stream:clear_stream_pending(#{st => Cleared}, [st]),
    Tmpl = #{
        s => maps:get(s, Grown),
        d => [{<<"0">>, fun() -> #{t => ?EACH, source => Drained, template => ItemTmpl} end}],
        f => maps:get(f, Grown)
    },
    {Ops, _, _} = arizona_diff:diff(Nest(Tmpl), Snap, #{}),
    %% Delivered, and delivered per item: one insert for the key the client
    %% lacks, not a re-render of the whole container.
    ?assertMatch([[?OP_INSERT, _, <<"b">>, -1, _] | _], Ops),
    ?assertEqual([], [Op || Op <- Ops, hd(Op) =:= ?OP_TEXT]).

%% A pure tail append needs inserts and nothing else: the inserts land at the tail
%% in the new order, so the DOM already matches and every move the LIS would emit
%% is a node moved onto itself. The reconciliation treats an inserted key as
%% unplaced, so without the prefix check an append-only list paid one redundant
%% move per insert. A genuine reorder must still move.
diff_stream_pure_append_emits_no_moves(Config) when is_list(Config) ->
    ItemTmpl = #{
        t => ?EACH,
        s => [<<"<li az=\"0\">">>, <<"</li>">>],
        d => fun(I, _Key) -> [{<<"0">>, maps:get(text, I)}] end,
        f => <<"item">>
    },
    Item = fun(K) -> #{id => K, text => K} end,
    Nest = fun(T) ->
        #{
            s => [<<"<div az=\"n\">">>, <<"</div>">>],
            d => [{<<"n">>, fun() -> T end}],
            f => <<"outer">>
        }
    end,
    Drained = fun(Tmpl) ->
        #{d := [{_, F}]} = Tmpl,
        Src = maps:get(source, F()),
        #{st := D} = arizona_stream:clear_stream_pending(#{st => Src}, [st]),
        #{
            s => maps:get(s, Tmpl),
            d => [{<<"0">>, fun() -> #{t => ?EACH, source => D, template => ItemTmpl} end}],
            f => maps:get(f, Tmpl)
        }
    end,
    Base = [Item(<<"a">>), Item(<<"b">>)],
    {_, Snap, _} = arizona_render:render(Nest(stream_each_tmpl(ItemTmpl, Base)), #{}),
    %% Append: inserts only, no moves.
    Appended = Drained(stream_each_tmpl(ItemTmpl, Base ++ [Item(<<"c">>), Item(<<"d">>)])),
    {AppendOps, _, _} = arizona_diff:diff(Nest(Appended), Snap, #{}),
    ?assertEqual([], [Op || Op <- AppendOps, hd(Op) =:= ?OP_MOVE]),
    ?assertEqual(2, length([Op || Op <- AppendOps, hd(Op) =:= ?OP_INSERT])),
    %% Reorder: moves are still required.
    Reordered = Drained(stream_each_tmpl(ItemTmpl, [Item(<<"b">>), Item(<<"a">>)])),
    {ReorderOps, _, _} = arizona_diff:diff(Nest(Reordered), Snap, #{}),
    ?assertNotEqual([], [Op || Op <- ReorderOps, hd(Op) =:= ?OP_MOVE]).

%% An item whose CONTENT changed in place leaves the key order identical. The
%% empty-log guard used to read that as "no ops needed" and hand the slot to the
%% drain, which found nothing and emitted nothing -- the server snapshot advanced
%% while the client kept the old value forever, with no self-heal. An empty log
%% carries no information about WHETHER anything changed (it is cleared before a
%% `?stateful` child renders), so it must go to the reconciliation, which compares
%% item dynamics and patches exactly what differs.
diff_stream_nested_content_change_delivers(Config) when is_list(Config) ->
    ItemTmpl = #{
        t => ?EACH,
        s => [<<"<li az=\"0\">">>, <<"</li>">>],
        d => fun(I, _Key) -> [{<<"0">>, maps:get(text, I)}] end,
        f => <<"item">>
    },
    Items = fun(BText) ->
        [#{id => <<"a">>, text => <<"A">>}, #{id => <<"b">>, text => BText}]
    end,
    Nest = fun(T) ->
        #{
            s => [<<"<div az=\"n\">">>, <<"</div>">>],
            d => [{<<"n">>, fun() -> T end}],
            f => <<"outer">>
        }
    end,
    {_, Snap, _} = arizona_render:render(Nest(stream_each_tmpl(ItemTmpl, Items(<<"B">>))), #{}),
    %% Same keys, same order, one item's text changed, and an empty log.
    Changed = stream_each_tmpl(ItemTmpl, Items(<<"B2">>)),
    #{d := [{_, F}]} = Changed,
    Src = maps:get(source, F()),
    #{st := Drained} = arizona_stream:clear_stream_pending(#{st => Src}, [st]),
    Tmpl = #{
        s => maps:get(s, Changed),
        d => [{<<"0">>, fun() -> #{t => ?EACH, source => Drained, template => ItemTmpl} end}],
        f => maps:get(f, Changed)
    },
    {Ops, _, _} = arizona_diff:diff(Nest(Tmpl), Snap, #{}),
    ?assertMatch([[?OP_ITEM_PATCH, _, <<"b">>, [[?OP_TEXT, _, <<"B2">>]]]], Ops).

%% The nested walk already renders every item to build the new value, so the
%% reconciliation must diff against THAT rather than render the list a second
%% time. The second render was not just wasted work: it re-ran every item child's
%% `mount/1` / `handle_update/3`, so a child that subscribes or arms a timer in
%% mount did it twice per diff.
diff_stream_nested_renders_items_once(Config) when is_list(Config) ->
    Counter = counters:new(1, []),
    ItemTmpl = #{
        t => ?EACH,
        s => [<<"<li az=\"0\">">>, <<"</li>">>],
        d => fun(I, _Key) ->
            counters:add(Counter, 1, 1),
            [{<<"0">>, maps:get(text, I)}]
        end,
        f => <<"item">>
    },
    Items = [
        #{id => <<"a">>, text => <<"A">>},
        #{id => <<"b">>, text => <<"B">>},
        #{id => <<"c">>, text => <<"C">>}
    ],
    Nest = fun(T) ->
        #{
            s => [<<"<div az=\"n\">">>, <<"</div>">>],
            d => [{<<"n">>, fun() -> T end}],
            f => <<"outer">>
        }
    end,
    {_, Snap, _} = arizona_render:render(Nest(stream_each_tmpl(ItemTmpl, Items)), #{}),
    Grown = stream_each_tmpl(ItemTmpl, Items ++ [#{id => <<"d">>, text => <<"D">>}]),
    #{d := [{_, F}]} = Grown,
    Src = maps:get(source, F()),
    #{st := Drained} = arizona_stream:clear_stream_pending(#{st => Src}, [st]),
    Tmpl = #{
        s => maps:get(s, Grown),
        d => [{<<"0">>, fun() -> #{t => ?EACH, source => Drained, template => ItemTmpl} end}],
        f => maps:get(f, Grown)
    },
    counters:put(Counter, 1, 0),
    {Ops, _, _} = arizona_diff:diff(Nest(Tmpl), Snap, #{}),
    %% Four items, rendered once each by the enclosing eval -- not eight.
    ?assertEqual(4, counters:get(Counter, 1)),
    ?assertMatch([[?OP_INSERT, _, <<"d">>, -1, _] | _], Ops).

stream_each_tmpl(ItemTmpl, Items) ->
    Stream = arizona_stream:new(fun(#{id := Id}) -> Id end, Items),
    #{
        s => [<<"<ul az=\"0\">">>, <<"</ul>">>],
        d => [{<<"0">>, fun() -> #{t => ?EACH, source => Stream, template => ItemTmpl} end}],
        f => <<"parent">>
    }.

%% Evaluate the template's single each dynamic exactly as a render would, so the
%% snapshot under test is the real thing rather than a hand-built lookalike.
eval_stream_each(Tmpl) ->
    {Triples, _Views} = arizona_eval:eval_dynamics_v(maps:get(d, Tmpl), {#{}, #{}}),
    {[{Az, EachSnap}], DepsList} = arizona_template:split_triples(Triples),
    {Az, EachSnap, DepsList}.
