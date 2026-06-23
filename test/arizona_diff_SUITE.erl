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
    diff_list_no_change_no_ops/1,
    diff_each_among_siblings_uses_text_op/1,
    diff_each_among_siblings_to_empty_uses_text_op/1,
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
    conditional_two_slot_fine_grained/1,
    conditional_nested_element_recurses/1,
    wire_get_value_raw/1,
    wire_raw_value_tagged_html/1
]).

all() ->
    [{group, basic_ops}, {group, no_diff}, {group, conditional_dep}, {group, wire_payload}].

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
            diff_list_no_change_no_ops,
            diff_each_among_siblings_uses_text_op,
            diff_each_among_siblings_to_empty_uses_text_op,
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
            conditional_two_slot_fine_grained,
            conditional_nested_element_recurses
        ]},
        {wire_payload, [parallel], [
            wire_get_value_raw,
            wire_raw_value_tagged_html
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
    OldSnap = #{
        s => [<<"<p az=\"0\">">>, <<"</p>">>],
        d => [{<<"0">>, #{s => [<<"Hello, ">>, <<"!">>], d => [{<<"i">>, <<"World">>}]}}]
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
    ?assertEqual([[?OP_TEXT, <<"i">>, <<"Alice">>]], Ops).

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
%% patches the slot's marker content with ?OP_TEXT -- never ?OP_UPDATE
%% (innerHTML), which would clobber the enclosing element when the slot's az is
%% the element's own az. See diff_empty_to_template_uses_text_op for the
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
    ?assertEqual(
        [
            [
                ?OP_TEXT,
                <<"0">>,
                #{
                    <<"f">> => <<"test">>,
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
%% was emitting ?OP_UPDATE here, whose innerHTML write clobbered the whole
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
    %% Exactly one op, an ?OP_TEXT on the slot -- not an ?OP_UPDATE on X-0
    %% (which the client resolves to the <main> root and would innerHTML-wipe).
    ?assertMatch([[?OP_TEXT, <<"X-0">>, #{<<"f">> := <<"child_fp">>}]], Ops),
    ?assertNotMatch([[?OP_UPDATE, <<"X-0">>, _]], Ops).

%% Plain-list `?each` diffing: any change re-renders the whole list with a
%% single OP_TEXT (the marker-aware container patch) -- never a per-item
%% OP_ITEM_PATCH (plain lists are unkeyed, so there is no `az-key` to patch).
%% Regression for the "stream item az-key=... not found for patch" bug. OP_TEXT
%% (not OP_UPDATE) because a plain-list each is anchored by content-slot comment
%% markers; see diff_each_among_siblings_uses_text_op. These cover every boolean
%% branch of the diff:
%%   diff_list_zip:  InnerOps =/= [] (head)  |  RestChanged (tail)  |  neither
%%   diff_list:      Changed  |  NewTail =/= [] (grew)  |  OldTail =/= [] (shrank)

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
diff_list_no_change_no_ops(Config) when is_list(Config) ->
    Items = [#{name => <<"a">>}, #{name => <<"b">>}],
    ?assertEqual([], each_list_diff(Items, Items)).

%% Regression: a plain-list `?each` sitting *among static sibling content* in the
%% same content slot. SSR anchors the each by its `<!--az:X-->...<!--/az-->`
%% comment markers (like every dynamic-text child) -- there is NO wrapper element
%% carrying `az="X"`. So the container op must be the marker-aware ?OP_TEXT: the
%% client's resolveEl can't find an element for the each's marker az and falls
%% back to the *enclosing* element, where ?OP_UPDATE's innerHTML would wipe the
%% static sibling content. The mixed-siblings shape is what breaks; a sole-child
%% each only "works" with ?OP_UPDATE by coincidence (the fallback element is the
%% right one). diff_each_among_siblings_to_empty_uses_text_op covers the reverse
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

%% `[]` -> non-empty: the each must patch its marker slot via ?OP_TEXT, never
%% ?OP_UPDATE (which would innerHTML-wipe the static sibling .item divs). The
%% unchanged sibling dynamics (strip:0/strip:1) must emit no ops.
diff_each_among_siblings_uses_text_op(Config) when is_list(Config) ->
    Ops = each_among_siblings_diff([], [#{name => <<"k">>}]),
    ?assertMatch([[?OP_TEXT, <<"strip:2">>, #{<<"t">> := ?EACH}]], Ops),
    ?assertNotMatch([[?OP_UPDATE, <<"strip:2">>, _]], Ops),
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
    ?assertNotMatch([[?OP_UPDATE, <<"strip:2">>, _]], Ops),
    [[?OP_TEXT, <<"strip:2">>, #{<<"d">> := ItemDs}]] = Ops,
    ?assertEqual([], ItemDs).

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
    %% The ignored dynamic retains old value in snapshot
    [{<<"0">>, _}, {<<"1">>, IgnoredSnap}] = maps:get(d, Snap1),
    [{<<"i">>, ContentVal}] = maps:get(d, IgnoredSnap),
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
        f => <<"test">>
    },
    {_HTML, Snap0, V0} = arizona_render:render(T0, #{}),
    B1 = #{foo => <<"b">>},
    T1 = #{
        s => [<<"<span>">>, <<"</span>">>],
        d => [
            {<<"0">>, fun() -> arizona_template:local(<<"k">>, arizona_template:get(foo, B1)) end}
        ],
        f => <<"test">>
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
