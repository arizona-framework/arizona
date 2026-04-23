-module(arizona_SUITE).
-include_lib("stdlib/include/assert.hrl").
-include("arizona.hrl").

-export([all/0, groups/0]).
-export([
    about_connected_event/1,
    about_handle_info_tick/1,
    about_mount/1,
    about_render/1,
    about_tick_started_event/1,
    counter_diff_id_change/1,
    counter_diff/1,
    counter_handle_event_dec/1,
    counter_handle_event_inc/1,
    counter_handle_event_reset/1,
    counter_mount/1,
    counter_mount_with_props/1,
    counter_no_change/1,
    counter_render_custom_id/1,
    counter_render/1,
    deps_all_changed/1,
    deps_count_change_skips_title/1,
    deps_count_tracks_correctly/1,
    deps_dedup_and_multi_key/1,
    deps_page_tracks_correctly/1,
    deps_skip_unchanged/1,
    deps_theme_change_only/1,
    diff_with_views_no_children/1,
    list_diff2/1,
    list_diff3/1,
    list_diff_deps_skip/1,
    list_diff_different_length/1,
    list_diff_empty_to_populated/1,
    list_diff_populated_to_empty/1,
    list_diff_same_length_no_change/1,
    list_diff_same_length/1,
    list_diff_shorter/1,
    list_empty/1,
    list_fingerprint/1,
    list_render/1,
    list_render_v/1,
    list_ssr/1,
    mixed_children_card_update/1,
    mixed_children_initial_render/1,
    mixed_children_roundtrip/1,
    mixed_children_show_event/1,
    page_diff_all_changes/1,
    page_diff_count_counter3_stable/1,
    page_diff_count_recursive/1,
    page_diff_theme_attr/1,
    page_diff_title_only/1,
    page_handle_event_add/1,
    page_handle_event_connected/1,
    page_handle_event_title_change/1,
    page_mount/1,
    page_render_with_views/1,
    stateful_bindings_sync/1,
    stateful_first_mount/1,
    stateful_handle_update/1,
    stateless_diff/1,
    stateless_fun_render/1,
    stateless_no_change/1,
    stateless_render/1
]).

all() ->
    [
        {group, counter},
        {group, page},
        {group, stateful_child},
        {group, stateless_child},
        {group, dep_tracking},
        {group, edge_cases},
        {group, list_comprehension},
        {group, mixed_children}
    ].

groups() ->
    [
        {counter, [parallel], [
            counter_mount,
            counter_mount_with_props,
            counter_render,
            counter_render_custom_id,
            counter_diff,
            counter_diff_id_change,
            counter_no_change
        ]},
        {page, [parallel], [
            page_mount,
            page_render_with_views,
            page_diff_title_only,
            page_diff_theme_attr,
            page_diff_count_recursive,
            page_diff_count_counter3_stable,
            page_diff_all_changes
        ]},
        {stateful_child, [parallel], [
            stateful_first_mount,
            stateful_handle_update,
            stateful_bindings_sync
        ]},
        {stateless_child, [parallel], [
            stateless_render,
            stateless_diff,
            stateless_fun_render,
            stateless_no_change
        ]},
        {dep_tracking, [parallel], [
            deps_skip_unchanged,
            deps_count_tracks_correctly,
            deps_dedup_and_multi_key,
            deps_page_tracks_correctly,
            deps_count_change_skips_title,
            deps_theme_change_only,
            deps_all_changed
        ]},
        {edge_cases, [parallel], [
            counter_handle_event_inc,
            counter_handle_event_dec,
            counter_handle_event_reset,
            page_handle_event_title_change,
            page_handle_event_add,
            page_handle_event_connected,
            diff_with_views_no_children,
            about_mount,
            about_render,
            about_connected_event,
            about_handle_info_tick,
            about_tick_started_event
        ]},
        {list_comprehension, [parallel], [
            list_render,
            list_render_v,
            list_ssr,
            list_empty,
            list_fingerprint,
            list_diff_same_length,
            list_diff_same_length_no_change,
            list_diff_different_length,
            list_diff_shorter,
            list_diff_empty_to_populated,
            list_diff_populated_to_empty,
            list_diff_deps_skip,
            list_diff2,
            list_diff3
        ]},
        {mixed_children, [parallel], [
            mixed_children_initial_render,
            mixed_children_show_event,
            mixed_children_card_update,
            mixed_children_roundtrip
        ]}
    ].

%% =============================================================================
%% 3. Counter module tests
%% =============================================================================

counter_mount(Config) when is_list(Config) ->
    {Bindings, _} = arizona_counter:mount(#{}),
    ?assertEqual(0, maps:get(count, Bindings)).

counter_mount_with_props(Config) when is_list(Config) ->
    {Bindings, _} = arizona_counter:mount(#{id => <<"c1">>, count => 5}),
    ?assertEqual(5, maps:get(count, Bindings)),
    ?assertEqual(<<"c1">>, maps:get(id, Bindings)).

counter_render(Config) when is_list(Config) ->
    {B, _} = arizona_counter:mount(#{}),
    Tmpl = arizona_counter:render(B),
    {HTML, _Snap} = arizona_render:render(Tmpl),
    HTMLBin = iolist_to_binary(HTML),
    ?assertNotEqual(nomatch, binary:match(HTMLBin, <<"az-click=\"[0,&quot;inc&quot;]\"">>)),
    ?assertNotEqual(nomatch, binary:match(HTMLBin, <<"az-click=\"[0,&quot;dec&quot;]\"">>)),
    ?assertNotEqual(nomatch, binary:match(HTMLBin, <<"Count: ">>)),
    ?assertNotEqual(nomatch, binary:match(HTMLBin, <<"0<!--/az-->">>)).

counter_render_custom_id(Config) when is_list(Config) ->
    {B, _} = arizona_counter:mount(#{id => <<"c1">>, count => 7}),
    Tmpl = arizona_counter:render(B),
    {HTML, _Snap} = arizona_render:render(Tmpl),
    HTMLBin = iolist_to_binary(HTML),
    ?assertNotEqual(nomatch, binary:match(HTMLBin, <<"7<!--/az-->">>)).

counter_diff(Config) when is_list(Config) ->
    {B0, _} = arizona_counter:mount(#{}),
    Tmpl0 = arizona_counter:render(B0),
    {_, Snap0} = arizona_render:render(Tmpl0),
    %% Count changes from 0 to 5
    B1 = B0#{count => 5},
    Tmpl1 = arizona_counter:render(B1),
    {Ops, _} = arizona_diff:diff(Tmpl1, Snap0),
    %% Count dynamic changed
    ?assertMatch([[?OP_TEXT, _, <<"5">>]], Ops).

counter_diff_id_change(Config) when is_list(Config) ->
    {B0, _} = arizona_counter:mount(#{}),
    Tmpl0 = arizona_counter:render(B0),
    {_, Snap0} = arizona_render:render(Tmpl0),
    %% Change id -- counter now renders id as a dynamic attr, so there IS an op
    B1 = B0#{id => <<"c1">>},
    Tmpl1 = arizona_counter:render(B1),
    {Ops, _} = arizona_diff:diff(Tmpl1, Snap0),
    ?assertMatch([[?OP_SET_ATTR, _, <<"id">>, <<"c1">>]], Ops).

counter_no_change(Config) when is_list(Config) ->
    {B0, _} = arizona_counter:mount(#{}),
    Tmpl0 = arizona_counter:render(B0),
    {_, Snap0} = arizona_render:render(Tmpl0),
    Tmpl1 = arizona_counter:render(B0),
    {Ops, _} = arizona_diff:diff(Tmpl1, Snap0),
    ?assertEqual([], Ops).

%% =============================================================================
%% 5. Page module tests with stateful children (render/2 and diff/3,4)
%% =============================================================================

page_mount(Config) when is_list(Config) ->
    {Bindings, _} = arizona_page:mount(#{}, arizona_req_test_adapter:new(#{})),
    ?assertEqual(<<"page">>, maps:get(id, Bindings)),
    ?assertEqual(<<"Welcome">>, maps:get(title, Bindings)),
    ?assertEqual(<<"light">>, maps:get(theme, Bindings)),
    ?assertEqual(0, maps:get(count, Bindings)),
    ?assertEqual(false, maps:get(connected, Bindings)).

page_render_with_views(Config) when is_list(Config) ->
    {B, _} = arizona_page:mount(#{}, arizona_req_test_adapter:new(#{})),
    Tmpl = arizona_page:render(B),
    {HTML, Snap, Views} = arizona_render:render(Tmpl, #{}),
    %% HTML includes all three counters
    HTMLBin = iolist_to_binary(HTML),
    ?assertNotEqual(nomatch, binary:match(HTMLBin, <<"az-view id=\"page\"">>)),
    ?assertNotEqual(nomatch, binary:match(HTMLBin, <<"Welcome<!--/az-->">>)),
    ?assertNotEqual(nomatch, binary:match(HTMLBin, <<"az-view id=\"counter\"">>)),
    ?assertNotEqual(nomatch, binary:match(HTMLBin, <<"az-view id=\"counter2\"">>)),
    ?assertNotEqual(nomatch, binary:match(HTMLBin, <<"az-view id=\"counter3\"">>)),
    ?assertNotEqual(nomatch, binary:match(HTMLBin, <<"42<!--/az-->">>)),
    ?assertNotEqual(nomatch, binary:match(HTMLBin, <<"Connecting...<!--/az-->">>)),
    ?assertNotEqual(nomatch, binary:match(HTMLBin, <<"az-submit=\"[0,&quot;add_todo&quot;]\"">>)),
    ?assertNotEqual(nomatch, binary:match(HTMLBin, <<"az-drop=\"[0,&quot;reorder_todo&quot;]\"">>)),
    %% Snapshot includes deps
    ?assert(maps:is_key(deps, Snap)),
    %% Views contain all three counters
    ?assertEqual(
        [<<"counter">>, <<"counter2">>, <<"counter3">>],
        lists:sort(maps:keys(Views))
    ),
    %% Each view entry has handler, bindings, snapshot
    #{<<"counter">> := CView} = Views,
    ?assertEqual(arizona_counter, maps:get(handler, CView)),
    ?assertEqual(#{count => 0, id => <<"counter">>}, maps:get(bindings, CView)),
    ?assert(maps:is_key(snapshot, CView)).

page_diff_title_only(Config) when is_list(Config) ->
    {B0, _} = arizona_page:mount(#{}, arizona_req_test_adapter:new(#{})),
    Tmpl0 = arizona_page:render(B0),
    {_, Snap0, V0} = arizona_render:render(Tmpl0, #{}),
    %% Change title only
    B1 = B0#{title => <<"About">>},
    Tmpl1 = arizona_page:render(B1),
    {Ops, _, _} = arizona_diff:diff(Tmpl1, Snap0, V0),
    ?assertMatch([[?OP_TEXT, _, <<"About">>]], Ops).

page_diff_theme_attr(Config) when is_list(Config) ->
    {B0, _} = arizona_page:mount(#{}, arizona_req_test_adapter:new(#{})),
    Tmpl0 = arizona_page:render(B0),
    {_, Snap0, V0} = arizona_render:render(Tmpl0, #{}),
    %% Change theme only
    B1 = B0#{theme => <<"dark">>},
    Tmpl1 = arizona_page:render(B1),
    {Ops, _, _} = arizona_diff:diff(Tmpl1, Snap0, V0),
    ?assertMatch([[?OP_SET_ATTR, _, <<"class">>, <<"dark">>]], Ops).

page_diff_count_recursive(Config) when is_list(Config) ->
    {B0, _} = arizona_page:mount(#{}, arizona_req_test_adapter:new(#{})),
    Tmpl0 = arizona_page:render(B0),
    {_, Snap0, V0} = arizona_render:render(Tmpl0, #{}),
    %% Change count -- counter and counter2 produce recursive ops,
    %% counter3 has hardcoded count=42 so stays unchanged.
    B1 = B0#{count => 3},
    Tmpl1 = arizona_page:render(B1),
    {Ops, _, _} = arizona_diff:diff(Tmpl1, Snap0, V0),
    %% Recursive child ops: [ViewId, [child_ops]]
    %% counter: default merge, count=3
    %% counter2: custom handle_update doubles, count=3*2=6
    ?assertMatch(
        [
            [<<"counter">>, [[?OP_TEXT, _, <<"3">>]]],
            [<<"counter2">>, [[?OP_TEXT, _, <<"6">>]]]
        ],
        Ops
    ).

page_diff_count_counter3_stable(Config) when is_list(Config) ->
    {B0, _} = arizona_page:mount(#{}, arizona_req_test_adapter:new(#{})),
    Tmpl0 = arizona_page:render(B0),
    {_, Snap0, V0} = arizona_render:render(Tmpl0, #{}),
    %% Change count to 99 -- counter3 has hardcoded count=42, no ops for it
    B1 = B0#{count => 99},
    Tmpl1 = arizona_page:render(B1),
    {Ops, _, V1} = arizona_diff:diff(Tmpl1, Snap0, V0),
    %% Only counter and counter2 produce ops
    ?assertEqual(2, length(Ops)),
    ?assertMatch([<<"counter">>, _], lists:nth(1, Ops)),
    ?assertMatch([<<"counter2">>, _], lists:nth(2, Ops)),
    %% counter3 bindings unchanged
    C3 = maps:get(<<"counter3">>, V1),
    ?assertEqual(#{count => 42, id => <<"counter3">>}, maps:get(bindings, C3)).

page_diff_all_changes(Config) when is_list(Config) ->
    {B0, _} = arizona_page:mount(#{}, arizona_req_test_adapter:new(#{})),
    Tmpl0 = arizona_page:render(B0),
    {_, Snap0, V0} = arizona_render:render(Tmpl0, #{}),
    %% Change title + theme + count
    B1 = B0#{title => <<"Home">>, theme => <<"dark">>, count => 1},
    Tmpl1 = arizona_page:render(B1),
    {Ops, _, _} = arizona_diff:diff(Tmpl1, Snap0, V0),
    %% 4 ops: title text, theme attr, counter recursive, counter2 recursive
    ?assertEqual(4, length(Ops)),
    ?assertMatch([?OP_TEXT, _, <<"Home">>], lists:nth(1, Ops)),
    ?assertMatch([?OP_SET_ATTR, _, <<"class">>, <<"dark">>], lists:nth(2, Ops)),
    ?assertMatch([<<"counter">>, [[?OP_TEXT, _, <<"1">>]]], lists:nth(3, Ops)),
    %% counter2: custom handle_update doubles, count=1*2=2
    ?assertMatch([<<"counter2">>, [[?OP_TEXT, _, <<"2">>]]], lists:nth(4, Ops)).

%% =============================================================================
%% 6. Stateful child lifecycle tests (render/2 path)
%% =============================================================================

stateful_first_mount(Config) when is_list(Config) ->
    %% First render calls mount, stores view in Views map
    {B, _} = arizona_page:mount(#{}, arizona_req_test_adapter:new(#{})),
    Tmpl = arizona_page:render(B),
    {_, _, Views} = arizona_render:render(Tmpl, #{}),
    %% All three counters are mounted
    ?assert(maps:is_key(<<"counter">>, Views)),
    ?assert(maps:is_key(<<"counter2">>, Views)),
    ?assert(maps:is_key(<<"counter3">>, Views)),
    %% Verify counter was mounted with correct bindings
    #{<<"counter">> := CView} = Views,
    ?assertEqual(arizona_counter, maps:get(handler, CView)),
    ?assertEqual(#{count => 0, id => <<"counter">>}, maps:get(bindings, CView)),
    %% Snapshot has view_id and deps
    CSnap = maps:get(snapshot, CView),
    ?assertEqual(<<"counter">>, maps:get(view_id, CSnap)),
    ?assert(maps:is_key(deps, CSnap)).

stateful_handle_update(Config) when is_list(Config) ->
    %% Second render (via diff/3) calls handle_update (default: maps:merge)
    {B0, _} = arizona_page:mount(#{}, arizona_req_test_adapter:new(#{})),
    Tmpl0 = arizona_page:render(B0),
    {_, Snap0, V0} = arizona_render:render(Tmpl0, #{}),
    %% Change count -- counter already exists in Views, triggers handle_update
    B1 = B0#{count => 5},
    Tmpl1 = arizona_page:render(B1),
    {_, _, V1} = arizona_diff:diff(Tmpl1, Snap0, V0),
    %% Counter bindings now have count=5 (merged from parent props)
    CView = maps:get(<<"counter">>, V1),
    ?assertEqual(5, maps:get(count, maps:get(bindings, CView))).

stateful_bindings_sync(Config) when is_list(Config) ->
    %% After handle_update, bindings and snapshot stay in sync
    {B0, _} = arizona_page:mount(#{}, arizona_req_test_adapter:new(#{})),
    Tmpl0 = arizona_page:render(B0),
    {_, Snap0, V0} = arizona_render:render(Tmpl0, #{}),
    %% Update count
    B1 = B0#{count => 5},
    Tmpl1 = arizona_page:render(B1),
    {_, _, V1} = arizona_diff:diff(Tmpl1, Snap0, V0),
    CView = maps:get(<<"counter">>, V1),
    CBindings = maps:get(bindings, CView),
    CSnap = maps:get(snapshot, CView),
    %% Bindings count matches snapshot dynamic value
    ?assertEqual(5, maps:get(count, CBindings)),
    %% Snapshot d: [{az0,{attr,id,val}}, {az1,{attr,az-click,cmd}},
    %%              {az2,{attr,az-click,cmd}}, {az3,count_val}]
    [
        {_, {attr, <<"id">>, _}},
        {_, {attr, <<"az-click">>, _}},
        {_, {attr, <<"az-click">>, _}},
        {_, CountVal}
    ] = maps:get(d, CSnap),
    ?assertEqual(5, CountVal).

%% =============================================================================
%% 7. Stateless child tests
%% =============================================================================

stateless_render(Config) when is_list(Config) ->
    %% arizona_template:stateless(Mod, Fun, Props) returns a descriptor #{callback, props}
    Desc = arizona_template:stateless(arizona_counter, render, #{count => 7, id => <<"sl">>}),
    ?assertMatch(#{callback := _, props := #{count := 7, id := <<"sl">>}}, Desc),
    %% Engine calls the callback during render/2
    Tmpl = #{
        s => [<<"<div az=\"0\">">>, <<"</div>">>],
        d => [
            {<<"0">>, fun() ->
                arizona_template:stateless(
                    arizona_counter,
                    render,
                    #{count => 7, id => <<"sl">>}
                )
            end}
        ],
        f => <<"test">>
    },
    {HTML, _Snap, _Views} = arizona_render:render(Tmpl, #{}),
    HTMLBin = iolist_to_binary(HTML),
    %% The stateless child renders the counter template inline
    ?assertNotEqual(nomatch, binary:match(HTMLBin, <<"az-click=\"[0,&quot;inc&quot;]\"">>)),
    ?assertNotEqual(nomatch, binary:match(HTMLBin, <<"7<!--/az-->">>)).

stateless_diff(Config) when is_list(Config) ->
    %% Stateless children produce OP_TEXT with re-zipped HTML when value changes
    Tmpl0 = #{
        s => [<<"<div az=\"0\">">>, <<"</div>">>],
        d => [
            {<<"0">>, fun() ->
                arizona_template:stateless(
                    arizona_counter,
                    render,
                    #{count => 0, id => <<"sl">>}
                )
            end}
        ],
        f => <<"test">>
    },
    {_, Snap0, V0} = arizona_render:render(Tmpl0, #{}),
    %% Now diff with count=5
    Tmpl1 = #{
        s => [<<"<div az=\"0\">">>, <<"</div>">>],
        d => [
            {<<"0">>, fun() ->
                arizona_template:stateless(
                    arizona_counter,
                    render,
                    #{count => 5, id => <<"sl">>}
                )
            end}
        ],
        f => <<"test">>
    },
    {Ops, _, _} = arizona_diff:diff(Tmpl1, Snap0, V0),
    %% Both old and new are nested templates with fingerprint, so OP_TEXT
    %% carries a structured payload instead of zipped HTML.
    ?assertMatch([[?OP_TEXT, <<"0">>, #{<<"f">> := _, <<"s">> := _, <<"d">> := _}]], Ops),
    [[_, _, Payload]] = Ops,
    ?assert(is_binary(maps:get(<<"f">>, Payload))),
    %% Statics contain fingerprint-scoped az values, just check key fragments
    Statics = maps:get(<<"s">>, Payload),
    ?assertEqual(5, length(Statics)),
    StaticsBin = iolist_to_binary(Statics),
    %% Attr names are no longer baked into statics -- check az-view and Count:
    ?assertNotEqual(nomatch, binary:match(StaticsBin, <<"az-view">>)),
    ?assertNotEqual(nomatch, binary:match(StaticsBin, <<"Count: ">>)),
    %% Dynamics now carry full attribute strings via unwrap_val
    ?assertEqual(
        [
            <<" id=\"sl\"">>,
            <<" az-click=\"[0,&quot;inc&quot;]\"">>,
            <<" az-click=\"[0,&quot;dec&quot;]\"">>,
            <<"5">>
        ],
        maps:get(<<"d">>, Payload)
    ).

stateless_fun_render(Config) when is_list(Config) ->
    %% arizona:stateless/2 with fun/1 works the same as stateless/3
    Tmpl = #{
        s => [<<"<div az=\"0\">">>, <<"</div>">>],
        d => [
            {<<"0">>, fun() ->
                arizona_template:stateless(
                    fun arizona_counter:render/1,
                    #{count => 7, id => <<"sl">>}
                )
            end}
        ],
        f => <<"test">>
    },
    {HTML, _Snap, _Views} = arizona_render:render(Tmpl, #{}),
    HTMLBin = iolist_to_binary(HTML),
    ?assertNotEqual(nomatch, binary:match(HTMLBin, <<"az-click=\"[0,&quot;inc&quot;]\"">>)),
    ?assertNotEqual(nomatch, binary:match(HTMLBin, <<"7<!--/az-->">>)).

stateless_no_change(Config) when is_list(Config) ->
    %% Stateless child with same props produces no ops
    Tmpl = #{
        s => [<<"<div az=\"0\">">>, <<"</div>">>],
        d => [
            {<<"0">>, fun() ->
                arizona_template:stateless(
                    arizona_counter,
                    render,
                    #{count => 3, id => <<"sl">>}
                )
            end}
        ],
        f => <<"test">>
    },
    {_, Snap0, V0} = arizona_render:render(Tmpl, #{}),
    {Ops, _, _} = arizona_diff:diff(Tmpl, Snap0, V0),
    ?assertEqual([], Ops).

%% =============================================================================
%% 10. Dep tracking edge cases
%% =============================================================================

deps_skip_unchanged(Config) when is_list(Config) ->
    %% diff/4 with Changed map -- unchanged dynamics are skipped entirely
    {B0, _} = arizona_page:mount(#{}, arizona_req_test_adapter:new(#{})),
    Tmpl0 = arizona_page:render(B0),
    {_, Snap0, V0} = arizona_render:render(Tmpl0, #{}),
    %% Only title changed -- theme and count deps not in Changed map
    B1 = B0#{title => <<"New">>},
    Tmpl1 = arizona_page:render(B1),
    Changed = #{title => <<"New">>},
    {Ops, _, _} = arizona_diff:diff(Tmpl1, Snap0, V0, Changed),
    %% Only title op, theme and counter dynamics skipped
    ?assertMatch([[?OP_TEXT, _, <<"New">>]], Ops).

deps_count_tracks_correctly(Config) when is_list(Config) ->
    %% Verify that counter render produces correct deps via render/2
    {B, _} = arizona_counter:mount(#{}),
    Tmpl = arizona_counter:render(B),
    {_, Snap, _} = arizona_render:render(Tmpl, #{}),
    %% Deps: #{id} for id attr, #{} for az-click inc/dec, #{count} for text
    ?assertEqual([#{id => true}, #{}, #{}, #{count => true}], maps:get(deps, Snap)).

deps_dedup_and_multi_key(Config) when is_list(Config) ->
    %% A dynamic that reads the same key twice should have one entry (dedup),
    %% and a dynamic reading multiple keys should collect them all in one map.
    B = #{a => 1, b => 2},
    Tmpl = #{
        s => [<<"">>, <<"">>, <<"">>],
        d => [
            {<<"0">>, fun() ->
                %% Read 'a' twice -- should deduplicate to #{a => true}
                arizona_template:get(a, B) + arizona_template:get(a, B)
            end},
            {<<"1">>, fun() ->
                %% Read both 'a' and 'b' -- should produce #{a => true, b => true}
                arizona_template:get(a, B) + arizona_template:get(b, B)
            end}
        ],
        f => <<"test">>
    },
    {_, Snap, _} = arizona_render:render(Tmpl, #{}),
    ?assertEqual([#{a => true}, #{a => true, b => true}], maps:get(deps, Snap)).

deps_page_tracks_correctly(Config) when is_list(Config) ->
    %% Verify page deps: id, title, az-click (static), theme, count (counter1),
    %% count (counter2), counter3 (static), connected (status), form statics, todos
    {B, _} = arizona_page:mount(#{}, arizona_req_test_adapter:new(#{})),
    Tmpl = arizona_page:render(B),
    {_, Snap, _} = arizona_render:render(Tmpl, #{}),
    ?assertEqual(
        [
            #{id => true},
            #{title => true},
            #{},
            #{theme => true},
            #{count => true},
            #{count => true},
            #{},
            #{connected => true},
            #{},
            #{},
            #{},
            #{todos => true}
        ],
        maps:get(deps, Snap)
    ).

deps_count_change_skips_title(Config) when is_list(Config) ->
    %% diff/4 with count change -- title dep not in Changed, title skipped
    {B0, _} = arizona_page:mount(#{}, arizona_req_test_adapter:new(#{})),
    Tmpl0 = arizona_page:render(B0),
    {_, Snap0, V0} = arizona_render:render(Tmpl0, #{}),
    B1 = B0#{count => 10},
    Tmpl1 = arizona_page:render(B1),
    Changed = #{count => 10},
    {Ops, _, _} = arizona_diff:diff(Tmpl1, Snap0, V0, Changed),
    %% Only counter ops, no title or theme ops
    ?assertEqual(2, length(Ops)),
    ?assertMatch([<<"counter">>, _], lists:nth(1, Ops)),
    ?assertMatch([<<"counter2">>, _], lists:nth(2, Ops)).

deps_theme_change_only(Config) when is_list(Config) ->
    %% diff/4 with theme change -- only OP_SET_ATTR on theme
    {B0, _} = arizona_page:mount(#{}, arizona_req_test_adapter:new(#{})),
    Tmpl0 = arizona_page:render(B0),
    {_, Snap0, V0} = arizona_render:render(Tmpl0, #{}),
    B1 = B0#{theme => <<"dark">>},
    Tmpl1 = arizona_page:render(B1),
    Changed = #{theme => <<"dark">>},
    {Ops, _, _} = arizona_diff:diff(Tmpl1, Snap0, V0, Changed),
    ?assertMatch([[?OP_SET_ATTR, _, <<"class">>, <<"dark">>]], Ops).

deps_all_changed(Config) when is_list(Config) ->
    %% diff/4 with all bindings changed
    {B0, _} = arizona_page:mount(#{}, arizona_req_test_adapter:new(#{})),
    Tmpl0 = arizona_page:render(B0),
    {_, Snap0, V0} = arizona_render:render(Tmpl0, #{}),
    B1 = B0#{title => <<"X">>, theme => <<"dark">>, count => 7, connected => true},
    Tmpl1 = arizona_page:render(B1),
    Changed = #{title => <<"X">>, theme => <<"dark">>, count => 7, connected => true},
    {Ops, _, _} = arizona_diff:diff(Tmpl1, Snap0, V0, Changed),
    %% title + theme + counter + counter2 + status = 5 ops
    ?assertEqual(5, length(Ops)).

%% =============================================================================
%% Additional edge case tests
%% =============================================================================

%% Test counter handle_event
counter_handle_event_inc(Config) when is_list(Config) ->
    {B0, _} = arizona_counter:mount(#{}),
    {B1, _, Effects} = arizona_counter:handle_event(<<"inc">>, #{}, B0),
    ?assertEqual(1, maps:get(count, B1)),
    ?assertEqual([], Effects).

counter_handle_event_dec(Config) when is_list(Config) ->
    {B0, _} = arizona_counter:mount(#{}),
    {B1, _, Effects} = arizona_counter:handle_event(<<"dec">>, #{}, B0),
    ?assertEqual(-1, maps:get(count, B1)),
    ?assertEqual([], Effects).

counter_handle_event_reset(Config) when is_list(Config) ->
    {B0, _} = arizona_counter:mount(#{id => <<"c1">>, count => 5}),
    {B1, _, Effects} = arizona_counter:handle_event(<<"reset">>, #{}, B0),
    ?assertEqual(0, maps:get(count, B1)),
    ?assertEqual([{arizona_js, [9, <<"counter_reset">>, #{<<"id">> => <<"c1">>}]}], Effects).

%% Test page handle_event
page_handle_event_title_change(Config) when is_list(Config) ->
    {B0, _} = arizona_page:mount(#{}, arizona_req_test_adapter:new(#{})),
    {B1, _, _} = arizona_page:handle_event(<<"title_change">>, #{}, B0),
    ?assertEqual(<<"Changed">>, maps:get(title, B1)).

page_handle_event_add(Config) when is_list(Config) ->
    {B0, _} = arizona_page:mount(#{}, arizona_req_test_adapter:new(#{})),
    {B1, _, _} = arizona_page:handle_event(<<"add">>, #{}, B0),
    ?assertEqual(1, maps:get(count, B1)).

page_handle_event_connected(Config) when is_list(Config) ->
    {B0, _} = arizona_page:mount(#{}, arizona_req_test_adapter:new(#{})),
    ?assertEqual(false, maps:get(connected, B0)),
    {B1, _, Effects} = arizona_page:handle_info(arizona_connected, B0),
    ?assertEqual(true, maps:get(connected, B1)),
    ?assertEqual([{arizona_js, [14, <<"Welcome">>]}], Effects).

%% Test diff/3 returns empty views when no stateful children
diff_with_views_no_children(Config) when is_list(Config) ->
    Tmpl0 = #{
        s => [<<"<p az=\"0\">">>, <<"</p>">>],
        d => [{<<"0">>, fun() -> <<"hello">> end}],
        f => <<"test">>
    },
    {_, Snap0, V0} = arizona_render:render(Tmpl0, #{}),
    Tmpl1 = #{
        s => [<<"<p az=\"0\">">>, <<"</p>">>],
        d => [{<<"0">>, fun() -> <<"world">> end}],
        f => <<"test">>
    },
    {Ops, _, V1} = arizona_diff:diff(Tmpl1, Snap0, V0),
    ?assertEqual([[?OP_TEXT, <<"0">>, <<"world">>]], Ops),
    ?assertEqual(#{}, V1).

about_mount(Config) when is_list(Config) ->
    {B, _} = arizona_about:mount(#{}, arizona_req_test_adapter:new(#{})),
    ?assertEqual(<<"page">>, maps:get(id, B)),
    ?assertEqual(<<"About">>, maps:get(title, B)),
    ?assertEqual(0, maps:get(tick, B)).

about_render(Config) when is_list(Config) ->
    {B, _} = arizona_about:mount(#{}, arizona_req_test_adapter:new(#{})),
    Tmpl = arizona_about:render(B),
    {HTML, _Snap} = arizona_render:render(Tmpl),
    HTMLBin = iolist_to_binary(HTML),
    ?assertNotEqual(nomatch, binary:match(HTMLBin, <<"az-view id=\"page\"">>)),
    ?assertNotEqual(nomatch, binary:match(HTMLBin, <<"About<!--/az-->">>)),
    ?assertNotEqual(nomatch, binary:match(HTMLBin, <<"az-hook=\"Tick\"">>)),
    ?assertNotEqual(nomatch, binary:match(HTMLBin, <<"0<!--/az-->">>)),
    ?assertNotEqual(nomatch, binary:match(HTMLBin, <<"erlang">>)),
    ?assertNotEqual(nomatch, binary:match(HTMLBin, <<"otp">>)),
    ?assertNotEqual(nomatch, binary:match(HTMLBin, <<"arizona">>)).

about_connected_event(Config) when is_list(Config) ->
    {B, _} = arizona_about:mount(#{}, arizona_req_test_adapter:new(#{})),
    {B2, _, Effects} = arizona_about:handle_info(arizona_connected, B),
    ?assertEqual(B, B2),
    ?assertEqual([{arizona_js, [14, <<"About">>]}], Effects).

about_handle_info_tick(Config) when is_list(Config) ->
    {B, _} = arizona_about:mount(#{}, arizona_req_test_adapter:new(#{})),
    ?assertEqual(0, maps:get(tick, B)),
    {B1, _, E1} = arizona_about:handle_info(tick, B),
    ?assertEqual(1, maps:get(tick, B1)),
    ?assertEqual([], E1),
    {B2, _, _} = arizona_about:handle_info(tick, B1),
    ?assertEqual(2, maps:get(tick, B2)).

about_tick_started_event(Config) when is_list(Config) ->
    {B, _} = arizona_about:mount(#{}, arizona_req_test_adapter:new(#{})),
    {B2, _, Effects} = arizona_about:handle_event(<<"tick_started">>, #{}, B),
    ?assertEqual(B, B2),
    ?assertEqual([{arizona_js, [9, <<"tick_ack">>, #{<<"status">> => <<"ok">>}]}], Effects).

%% =============================================================================
%% List comprehension tests
%% =============================================================================

list_render(Config) when is_list(Config) ->
    %% Render a plain list, verify HTML output
    Items = [#{id => 1, text => <<"A">>}, #{id => 2, text => <<"B">>}],
    Tmpl = #{
        s => [<<"<ul az=\"0\">">>, <<"</ul>">>],
        d => [
            {<<"0">>, fun() ->
                arizona_template:each(
                    Items,
                    #{
                        t => 0,
                        s => [
                            <<"<li az-key=\"">>,
                            <<"\"><!--az:1-->">>,
                            <<"<!--/az--></li>">>
                        ],
                        d => fun(Item) ->
                            [
                                {<<"0">>, fun() -> maps:get(id, Item) end},
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
    ?assertEqual(
        <<
            "<ul az=\"0\">"
            "<li az-key=\"1\"><!--az:1-->A<!--/az--></li>"
            "<li az-key=\"2\"><!--az:1-->B<!--/az--></li>"
            "</ul>"
        >>,
        HTMLBin
    ).

list_render_v(Config) when is_list(Config) ->
    %% Render a plain list with views map (render/2)
    Items = [#{id => 1, text => <<"A">>}, #{id => 2, text => <<"B">>}],
    Bindings = #{id => <<"test">>, items => Items},
    Tmpl = #{
        s => [<<"<ul az=\"0\">">>, <<"</ul>">>],
        d => [
            {<<"0">>, fun() ->
                arizona_template:each(
                    arizona_template:get(items, Bindings),
                    #{
                        t => 0,
                        s => [
                            <<"<li az-key=\"">>,
                            <<"\"><!--az:1-->">>,
                            <<"<!--/az--></li>">>
                        ],
                        f => <<"test">>,
                        d => fun(Item) ->
                            [
                                {<<"0">>, fun() -> maps:get(id, Item) end},
                                {<<"1">>, fun() -> maps:get(text, Item) end}
                            ]
                        end
                    }
                )
            end}
        ],
        f => <<"test">>
    },
    {HTML, _Snap, _Views} = arizona_render:render(Tmpl, #{}),
    HTMLBin = iolist_to_binary(HTML),
    ?assertEqual(
        <<
            "<ul az=\"0\">"
            "<li az-key=\"1\"><!--az:1-->A<!--/az--></li>"
            "<li az-key=\"2\"><!--az:1-->B<!--/az--></li>"
            "</ul>"
        >>,
        HTMLBin
    ).

list_diff_same_length(Config) when is_list(Config) ->
    %% Same count, changed values → OP_ITEM_PATCH ops
    Items1 = [#{id => 1, text => <<"A">>}, #{id => 2, text => <<"B">>}],
    Items2 = [#{id => 1, text => <<"X">>}, #{id => 2, text => <<"B">>}],
    Bindings0 = #{id => <<"test">>, items => Items1},
    ListTmpl = #{
        t => 0,
        s => [
            <<"<li az-key=\"">>,
            <<"\"><!--az:1-->">>,
            <<"<!--/az--></li>">>
        ],
        d => fun(Item) ->
            [
                {<<"0">>, fun() -> maps:get(id, Item) end},
                {<<"1">>, fun() -> maps:get(text, Item) end}
            ]
        end,
        f => <<"test">>
    },
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
    Bindings1 = Bindings0#{items => Items2},
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
    %% Only item 1 changed text from A→X, item 2 unchanged
    ?assertEqual(1, length(Ops)),
    [[7, <<"0">>, <<"1">>, InnerOps]] = Ops,
    %% Inner op should update text of the changed dynamic
    ?assertEqual(1, length(InnerOps)).

list_diff_same_length_no_change(Config) when is_list(Config) ->
    %% Same count, same values → no ops
    Items = [#{id => 1, text => <<"A">>}, #{id => 2, text => <<"B">>}],
    Bindings0 = #{id => <<"test">>, items => Items},
    ListTmpl = #{
        t => 0,
        s => [
            <<"<li az-key=\"">>,
            <<"\"><!--az:1-->">>,
            <<"<!--/az--></li>">>
        ],
        d => fun(Item) ->
            [
                {<<"0">>, fun() -> maps:get(id, Item) end},
                {<<"1">>, fun() -> maps:get(text, Item) end}
            ]
        end,
        f => <<"test">>
    },
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
    %% Same items
    Changed = #{items => true},
    Tmpl1 = #{
        s => [<<"<ul az=\"0\">">>, <<"</ul>">>],
        d => [
            {<<"0">>, fun() ->
                arizona_template:each(arizona_template:get(items, Bindings0), ListTmpl)
            end}
        ],
        f => <<"test">>
    },
    {Ops, _Snap1, _V1} = arizona_diff:diff(Tmpl1, Snap0, V0, Changed),
    ?assertEqual([], Ops).

list_diff_different_length(Config) when is_list(Config) ->
    %% Different count → single OP_UPDATE
    Items1 = [#{id => 1, text => <<"A">>}, #{id => 2, text => <<"B">>}],
    Items2 = [
        #{id => 1, text => <<"A">>},
        #{id => 2, text => <<"B">>},
        #{id => 3, text => <<"C">>}
    ],
    Bindings0 = #{id => <<"test">>, items => Items1},
    ListTmpl = #{
        t => 0,
        s => [
            <<"<li az-key=\"">>,
            <<"\"><!--az:1-->">>,
            <<"<!--/az--></li>">>
        ],
        d => fun(Item) ->
            [
                {<<"0">>, fun() -> maps:get(id, Item) end},
                {<<"1">>, fun() -> maps:get(text, Item) end}
            ]
        end,
        f => <<"test">>
    },
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
    Bindings1 = Bindings0#{items => Items2},
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
    %% Length mismatch → single OP_UPDATE (opcode 3)
    ?assertEqual(1, length(Ops)),
    [[3, <<"0">>, _HTML]] = Ops.

list_ssr(Config) when is_list(Config) ->
    %% SSR with list comprehension
    Items = [#{id => 1, text => <<"A">>}, #{id => 2, text => <<"B">>}],
    Tmpl = #{
        s => [<<"<ul az=\"0\">">>, <<"</ul>">>],
        d => [
            {<<"0">>, fun() ->
                arizona_template:each(
                    Items,
                    #{
                        t => 0,
                        s => [
                            <<"<li az-key=\"">>,
                            <<"\">">>,
                            <<"</li>">>
                        ],
                        d => fun(Item) ->
                            [
                                {<<"0">>, fun() -> maps:get(id, Item) end},
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
    ?assertEqual(
        <<
            "<ul az=\"0\">"
            "<li az-key=\"1\">A</li>"
            "<li az-key=\"2\">B</li>"
            "</ul>"
        >>,
        HTMLBin
    ).

list_fingerprint(Config) when is_list(Config) ->
    %% Fingerprinted payload with list
    Items = [#{id => 1, text => <<"A">>}, #{id => 2, text => <<"B">>}],
    Tmpl = #{
        s => [<<"<ul az=\"0\">">>, <<"</ul>">>],
        f => <<"list_tpl">>,
        d => [
            {<<"0">>, fun() ->
                arizona_template:each(
                    Items,
                    #{
                        t => 0,
                        s => [
                            <<"<li az-key=\"">>,
                            <<"\">">>,
                            <<"</li>">>
                        ],
                        f => <<"list_item_tpl">>,
                        d => fun(Item) ->
                            [
                                {<<"0">>, fun() -> maps:get(id, Item) end},
                                {<<"1">>, fun() -> maps:get(text, Item) end}
                            ]
                        end
                    }
                )
            end}
        ]
    },
    {_HTML, Snap} = arizona_render:render(Tmpl),
    Payload = arizona_render:fingerprint_payload(Snap),
    ?assertMatch(#{<<"f">> := <<"list_tpl">>, <<"s">> := _, <<"d">> := _}, Payload),
    [ListPayload] = maps:get(<<"d">>, Payload),
    %% List items should be a fingerprinted object with t, f, s, d
    ?assertMatch(#{<<"t">> := 0, <<"f">> := <<"list_item_tpl">>}, ListPayload),
    ItemDs = maps:get(<<"d">>, ListPayload),
    ?assertEqual(2, length(ItemDs)).

list_empty(Config) when is_list(Config) ->
    %% Empty list renders correctly
    Tmpl = #{
        s => [<<"<ul az=\"0\">">>, <<"</ul>">>],
        d => [
            {<<"0">>, fun() ->
                arizona_template:each(
                    [],
                    #{
                        t => 0,
                        s => [<<"<li>">>, <<"</li>">>],
                        d => fun(Item) ->
                            [{<<"0">>, fun() -> maps:get(text, Item) end}]
                        end,
                        f => <<"test">>
                    }
                )
            end}
        ],
        f => <<"test">>
    },
    {HTML, _Snap} = arizona_render:render(Tmpl),
    ?assertEqual(<<"<ul az=\"0\"></ul>">>, iolist_to_binary(HTML)).

list_diff_empty_to_populated(Config) when is_list(Config) ->
    %% [] → [a, b] -- length mismatch → OP_UPDATE
    ListTmpl = #{
        t => 0,
        s => [
            <<"<li az-key=\"">>,
            <<"\"><!--az:1-->">>,
            <<"<!--/az--></li>">>
        ],
        d => fun(Item) ->
            [
                {<<"0">>, fun() -> maps:get(id, Item) end},
                {<<"1">>, fun() -> maps:get(text, Item) end}
            ]
        end,
        f => <<"test">>
    },
    Bindings0 = #{id => <<"test">>, items => []},
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
    Items2 = [#{id => 1, text => <<"A">>}, #{id => 2, text => <<"B">>}],
    Bindings1 = Bindings0#{items => Items2},
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

list_diff_populated_to_empty(Config) when is_list(Config) ->
    %% [a, b] → [] -- length mismatch → OP_UPDATE
    Items1 = [#{id => 1, text => <<"A">>}, #{id => 2, text => <<"B">>}],
    ListTmpl = #{
        t => 0,
        s => [
            <<"<li az-key=\"">>,
            <<"\"><!--az:1-->">>,
            <<"<!--/az--></li>">>
        ],
        d => fun(Item) ->
            [
                {<<"0">>, fun() -> maps:get(id, Item) end},
                {<<"1">>, fun() -> maps:get(text, Item) end}
            ]
        end,
        f => <<"test">>
    },
    Bindings0 = #{id => <<"test">>, items => Items1},
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
    Bindings1 = Bindings0#{items => []},
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

list_diff_shorter(Config) when is_list(Config) ->
    %% [a, b, c] → [a, b] -- length decrease → OP_UPDATE
    Items1 = [
        #{id => 1, text => <<"A">>},
        #{id => 2, text => <<"B">>},
        #{id => 3, text => <<"C">>}
    ],
    Items2 = [#{id => 1, text => <<"A">>}, #{id => 2, text => <<"B">>}],
    ListTmpl = #{
        t => 0,
        s => [
            <<"<li az-key=\"">>,
            <<"\"><!--az:1-->">>,
            <<"<!--/az--></li>">>
        ],
        d => fun(Item) ->
            [
                {<<"0">>, fun() -> maps:get(id, Item) end},
                {<<"1">>, fun() -> maps:get(text, Item) end}
            ]
        end,
        f => <<"test">>
    },
    Bindings0 = #{id => <<"test">>, items => Items1},
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
    Bindings1 = Bindings0#{items => Items2},
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

list_diff_deps_skip(Config) when is_list(Config) ->
    %% diff/4 with unchanged deps skips list eval entirely
    Items = [#{id => 1, text => <<"A">>}],
    ListTmpl = #{
        t => 0,
        s => [
            <<"<li az-key=\"">>,
            <<"\"><!--az:1-->">>,
            <<"<!--/az--></li>">>
        ],
        d => fun(Item) ->
            [
                {<<"0">>, fun() -> maps:get(id, Item) end},
                {<<"1">>, fun() -> maps:get(text, Item) end}
            ]
        end,
        f => <<"test">>
    },
    Bindings0 = #{id => <<"test">>, items => Items, other => <<"x">>},
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
    %% Only 'other' changed, not 'items' -- list should be skipped
    Changed = #{other => true},
    Tmpl1 = #{
        s => [<<"<ul az=\"0\">">>, <<"</ul>">>],
        d => [
            {<<"0">>, fun() ->
                arizona_template:each(arizona_template:get(items, Bindings0), ListTmpl)
            end}
        ],
        f => <<"test">>
    },
    {Ops, _Snap1, _V1} = arizona_diff:diff(Tmpl1, Snap0, V0, Changed),
    ?assertEqual([], Ops).

list_diff2(Config) when is_list(Config) ->
    %% diff/2 path -- tests make_op fix for EACH snapshots
    %% diff/2 has no EACH-aware path, so make_op emits OP_UPDATE
    Items1 = [#{id => 1, text => <<"A">>}],
    Items2 = [#{id => 1, text => <<"X">>}],
    ListTmpl = #{
        t => 0,
        s => [
            <<"<li az-key=\"">>,
            <<"\"><!--az:1-->">>,
            <<"<!--/az--></li>">>
        ],
        d => fun(Item) ->
            [
                {<<"0">>, fun() -> maps:get(id, Item) end},
                {<<"1">>, fun() -> maps:get(text, Item) end}
            ]
        end,
        f => <<"test">>
    },
    Tmpl0 = #{
        s => [<<"<ul az=\"0\">">>, <<"</ul>">>],
        d => [
            {<<"0">>, fun() ->
                arizona_template:each(Items1, ListTmpl)
            end}
        ],
        f => <<"test">>
    },
    {_, Snap0} = arizona_render:render(Tmpl0),
    Tmpl1 = #{
        s => [<<"<ul az=\"0\">">>, <<"</ul>">>],
        d => [
            {<<"0">>, fun() ->
                arizona_template:each(Items2, ListTmpl)
            end}
        ],
        f => <<"test">>
    },
    {Ops, _Snap1} = arizona_diff:diff(Tmpl1, Snap0),
    %% diff/2 falls through to make_op → OP_UPDATE (opcode 3)
    ?assertEqual(1, length(Ops)),
    [[3, <<"0">>, _HTML]] = Ops.

list_diff3(Config) when is_list(Config) ->
    %% diff/3 path -- tests make_op fix via views path
    %% diff/3 has no EACH-aware path, so make_op emits OP_UPDATE
    Items1 = [#{id => 1, text => <<"A">>}],
    Items2 = [#{id => 1, text => <<"X">>}],
    ListTmpl = #{
        t => 0,
        s => [
            <<"<li az-key=\"">>,
            <<"\"><!--az:1-->">>,
            <<"<!--/az--></li>">>
        ],
        d => fun(Item) ->
            [
                {<<"0">>, fun() -> maps:get(id, Item) end},
                {<<"1">>, fun() -> maps:get(text, Item) end}
            ]
        end,
        f => <<"test">>
    },
    Tmpl0 = #{
        s => [<<"<ul az=\"0\">">>, <<"</ul>">>],
        d => [
            {<<"0">>, fun() ->
                arizona_template:each(Items1, ListTmpl)
            end}
        ],
        f => <<"test">>
    },
    {_, Snap0} = arizona_render:render(Tmpl0),
    Tmpl1 = #{
        s => [<<"<ul az=\"0\">">>, <<"</ul>">>],
        d => [
            {<<"0">>, fun() ->
                arizona_template:each(Items2, ListTmpl)
            end}
        ],
        f => <<"test">>
    },
    {Ops, _Snap1, _V1} = arizona_diff:diff(Tmpl1, Snap0, #{}),
    %% diff/3 falls through to make_op → OP_UPDATE (opcode 3)
    ?assertEqual(1, length(Ops)),
    [[3, <<"0">>, _HTML]] = Ops.

%% =============================================================================
%% Mixed stateless + dynamic children
%% =============================================================================
%%
%% Reproduces a bug where a parent template mixes arizona:stateless children
%% with regular dynamic children (arizona:get). After an event that changes
%% the regular dynamic, the diff op may target the wrong az slot because the
%% stateless child shifts the az numbering.

mixed_children_initial_render(Config) when is_list(Config) ->
    {ok, Pid} = arizona_live:start_link(arizona_mixed_children),
    {ok, _ViewId, HTML} = arizona_live:mount_and_render(Pid),
    HTMLBin = iolist_to_binary(mixed_render_html(HTML)),
    ?assertNotEqual(nomatch, binary:match(HTMLBin, <<"Empty">>)),
    ?assertNotEqual(nomatch, binary:match(HTMLBin, <<"Hello">>)),
    ?assertNotEqual(nomatch, binary:match(HTMLBin, <<"Initial">>)),
    ?assertNotEqual(nomatch, binary:match(HTMLBin, <<"rack absent">>)).

mixed_children_show_event(Config) when is_list(Config) ->
    %% After "show" event, the section class, content, AND message <p> should
    %% update. The ops must target the correct az slots -- not the stateless
    %% child's inner slots.
    {ok, Pid} = arizona_live:start_link(arizona_mixed_children),
    {ok, _} = arizona_live:mount(Pid),
    {ok, Ops, []} = arizona_live:handle_event(
        Pid, <<"mixed">>, <<"show">>, #{<<"text">> => <<"Widget">>}
    ),
    ?assertNotEqual([], Ops),
    ?assert(
        lists:any(
            fun
                ([?OP_TEXT, _, <<"Showing: Widget">>]) -> true;
                (_) -> false
            end,
            Ops
        )
    ),
    ?assert(
        lists:any(
            fun
                ([?OP_SET_ATTR, _, <<"class">>, <<"rack present">>]) -> true;
                (_) -> false
            end,
            Ops
        )
    ),
    ?assert(
        lists:any(
            fun
                ([?OP_TEXT, _, <<"Widget">>]) -> true;
                (_) -> false
            end,
            Ops
        )
    ).

mixed_children_card_update(Config) when is_list(Config) ->
    {ok, Pid} = arizona_live:start_link(arizona_mixed_children),
    {ok, _} = arizona_live:mount(Pid),
    {ok, Ops, []} = arizona_live:handle_event(
        Pid,
        <<"mixed">>,
        <<"update_card">>,
        #{<<"class">> => <<"card active">>, <<"label">> => <<"Updated">>}
    ),
    ?assertNotEqual([], Ops).

mixed_children_roundtrip(Config) when is_list(Config) ->
    {ok, Pid} = arizona_live:start_link(arizona_mixed_children),
    {ok, _} = arizona_live:mount(Pid),
    {ok, _, _} = arizona_live:handle_event(
        Pid, <<"mixed">>, <<"show">>, #{<<"text">> => <<"Test">>}
    ),
    {ok, Ops, []} = arizona_live:handle_event(
        Pid, <<"mixed">>, <<"hide">>, #{}
    ),
    ?assertNotEqual([], Ops),
    ?assert(
        lists:any(
            fun
                ([?OP_SET_ATTR, _, <<"class">>, <<"rack absent">>]) -> true;
                (_) -> false
            end,
            Ops
        )
    ),
    ?assert(
        lists:any(
            fun
                ([?OP_TEXT, _, <<"Empty">>]) -> true;
                (_) -> false
            end,
            Ops
        )
    ).

mixed_render_html(#{<<"f">> := _, <<"s">> := S, <<"d">> := D}) ->
    arizona_render:zip(S, [mixed_render_html(X) || X <:- D]);
mixed_render_html(#{<<"f">> := _, <<"d">> := D}) ->
    [mixed_render_html(X) || X <:- D];
mixed_render_html(V) ->
    V.
