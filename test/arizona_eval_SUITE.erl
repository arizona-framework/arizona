-module(arizona_eval_SUITE).
-include_lib("stdlib/include/assert.hrl").
-include("arizona.hrl").
-dialyzer({nowarn_function, eval_each_def_3tuple/1}).

-export([all/0, groups/0]).
-export([
    eval_each_def_3tuple/1,
    render_map_items_order_matches_ssr/1,
    eval_val_stateless_descriptor/1,
    stateless_callback_does_not_leak_deps/1,
    stateless_callback_with_no_eager_reads/1,
    stateless_callback_eager_reads_dropped_outer_reads_kept/1,
    stateless_callback_multiple_eager_reads_all_dropped/1,
    adjacent_dynamics_have_independent_deps/1,
    deeply_nested_stateless_no_leak_at_any_layer/1,
    stateless_inside_each_no_leak/1,
    stateless_callback_returning_fun_no_leak/1,
    stateful_mount_eager_read_no_leak/1,
    render_stream_item_persists_deps/1,
    render_stream_item_skipping_reuses_unchanged/1,
    render_stream_item_skipping_full_eval_on_empty_deps/1,
    render_stream_item_skipping_short_circuits_on_empty_changed/1,
    child_stream_pending_cleared_on_eval_path/1,
    child_stream_emits_no_op_on_unchanged_ticks/1,
    child_stream_untouched_by_prop_bump_emits_nothing/1,
    child_stream_replayed_drain_emits_no_duplicate_insert/1
]).

all() ->
    [
        {group, eval_api},
        {group, dep_isolation},
        {group, per_item_optimization},
        {group, child_streams}
    ].

groups() ->
    [
        {eval_api, [parallel], [
            eval_each_def_3tuple,
            render_map_items_order_matches_ssr,
            eval_val_stateless_descriptor
        ]},
        {per_item_optimization, [parallel], [
            render_stream_item_persists_deps,
            render_stream_item_skipping_reuses_unchanged,
            render_stream_item_skipping_full_eval_on_empty_deps,
            render_stream_item_skipping_short_circuits_on_empty_changed
        ]},
        {dep_isolation, [parallel], [
            stateless_callback_does_not_leak_deps,
            stateless_callback_with_no_eager_reads,
            stateless_callback_eager_reads_dropped_outer_reads_kept,
            stateless_callback_multiple_eager_reads_all_dropped,
            adjacent_dynamics_have_independent_deps,
            deeply_nested_stateless_no_leak_at_any_layer,
            stateless_inside_each_no_leak,
            stateless_callback_returning_fun_no_leak,
            stateful_mount_eager_read_no_leak
        ]},
        {child_streams, [parallel], [
            child_stream_pending_cleared_on_eval_path,
            child_stream_emits_no_op_on_unchanged_ticks,
            child_stream_untouched_by_prop_bump_emits_nothing,
            child_stream_replayed_drain_emits_no_duplicate_insert
        ]}
    ].

%% --- child streams ---

%% A stateful CHILD's stream is fed by parent props, so its queue grows on the
%% EVAL path (eval_stateful/3), which stored the post-handle_update bindings
%% verbatim -- unlike arizona_live's root/child-event paths, which clear. One
%% queue entry per ROOT update accumulated for the process lifetime (memory,
%% plus a drain that then walks the whole history to emit a couple of ops).
child_stream_pending_cleared_on_eval_path(Config) when is_list(Config) ->
    B0 = #{id => ~"p", tick => 0},
    {_HTML, Snap0, V0} = arizona_render:render(child_stream_tmpl(B0), #{}),
    Lens = child_stream_tick_cycles(1, 6, B0, Snap0, V0, []),
    ?assertEqual([0, 0, 0, 0, 0, 0], Lens).

%% The drain's insert dup guard (`stream_insert`'s `not is_map_key(Key, SnapAcc)`)
%% is what stops a REPLAYED insert becoming a second DOM node under one az-key.
%% Reaching it needs a drain that actually replays: a linear re-drain no longer
%% does, because the drain mark filters the replay before `stream_insert` runs.
%% A DIVERGENT fork does -- its mark is absent, so the whole queue re-drains,
%% including the insert of a key the snapshot already holds. With the guard
%% removed this emits [~"a", ~"c2"] instead of [~"c2"].
child_stream_replayed_drain_emits_no_duplicate_insert(Config) when is_list(Config) ->
    KeyFun = fun(#{id := Id}) -> Id end,
    Base = arizona_stream:new(KeyFun, []),
    ItemA = #{id => ~"a", text => ~"A"},
    B0 = #{id => ~"c", items => arizona_stream:insert(Base, ItemA)},
    Tmpl0 = arizona_stateful:call_render(arizona_stream_child, B0),
    {_HTML, Snap0, V0} = arizona_render:render(Tmpl0, #{}),
    %% Re-derive from the pristine base rather than from the rendered stream: a
    %% divergent fork carrying its own insert of "a" plus a new key.
    Forked = arizona_stream:insert(
        arizona_stream:insert(Base, ItemA), #{id => ~"c2", text => ~"C"}
    ),
    B1 = B0#{items => Forked},
    Tmpl1 = arizona_stateful:call_render(arizona_stream_child, B1),
    {Ops, _Snap1, _V1} = arizona_diff:diff(Tmpl1, Snap0, V0, #{items => true}),
    ?assertEqual([~"c2"], [K || [?OP_INSERT, _, K, _, _] <- Ops]).

%% The stored child bindings and the snapshot built during the SAME eval must
%% agree about the stream. Clearing the pending queue AFTER `call_render` left
%% the snapshot holding the uncleared stream and the bindings the cleared one, so
%% the next eval's source comparison differed purely because of the queue and the
%% whole container re-rendered -- on every tick, including the ones where the
%% child's stream did not change at all. That wholesale re-render destroys and
%% recreates every item node: focus/selection lost, `?local` slots reset to their
%% SSR initials, CSS transitions restarted, per-item child views re-mounted.
child_stream_emits_no_op_on_unchanged_ticks(Config) when is_list(Config) ->
    B0 = #{id => ~"p", tick => 0},
    {_HTML, Snap0, V0} = arizona_render:render(child_stream_tmpl(B0), #{}),
    %% arizona_stream_child mutates on ODD ticks only, so the even ticks are
    %% genuinely unchanged and must emit nothing.
    ?assertEqual(
        [{1, true}, {2, false}, {3, true}, {4, false}, {5, true}, {6, false}],
        child_stream_tick_ops(1, 6, B0, Snap0, V0, [])
    ).

%% The same defect seen from its sharpest angle: a child whose stream is
%% pre-populated at mount (so its queue is non-empty from the very first eval)
%% and NEVER mutated afterwards. Every parent prop bump must emit nothing at all.
%% Only even ticks are used here, which arizona_stream_child leaves untouched.
child_stream_untouched_by_prop_bump_emits_nothing(Config) when is_list(Config) ->
    Seed = [#{id => 1, text => ~"one"}, #{id => 2, text => ~"two"}],
    B0 = #{id => ~"p", tick => 0, seed => Seed},
    {_HTML, Snap0, V0} = arizona_render:render(child_stream_seeded_tmpl(B0), #{}),
    {Ops1, Snap1, V1} = arizona_diff:diff(
        child_stream_seeded_tmpl(B0#{tick => 2}), Snap0, V0, #{tick => true}
    ),
    ?assertEqual([], Ops1),
    {Ops2, _Snap2, _V2} = arizona_diff:diff(
        child_stream_seeded_tmpl(B0#{tick => 4}), Snap1, V1, #{tick => true}
    ),
    ?assertEqual([], Ops2).

%% Parent template that also forwards a mount-time `seed` to the child.
child_stream_seeded_tmpl(B) ->
    #{
        s => [~"<div az=\"0\">", ~"</div>"],
        d => [
            {~"0", fun() ->
                arizona_template:stateful(arizona_stream_child, #{
                    id => ~"c",
                    tick => arizona_template:get(tick, B),
                    seed => arizona_template:get(seed, B)
                })
            end}
        ],
        f => ~"seeded"
    }.

%% Runs `Max` root update cycles, returning `{Tick, EmittedAnyOp}` per tick.
child_stream_tick_ops(N, Max, _B, _Snap, _V, Acc) when N > Max ->
    lists:reverse(Acc);
child_stream_tick_ops(N, Max, B, Snap, V, Acc) ->
    B1 = B#{tick => N},
    {Ops, Snap1, V1} = arizona_diff:diff(child_stream_tmpl(B1), Snap, V, #{tick => true}),
    child_stream_tick_ops(N + 1, Max, B1, Snap1, V1, [{N, Ops =/= []} | Acc]).

%% Runs `Max` root update cycles, returning the child's pending queue length
%% after each one. The parent's only changing binding is `tick`, delivered to
%% the child as a prop -- the child appends it to its stream in handle_update/3.
child_stream_tick_cycles(N, Max, _B, _Snap, _V, Acc) when N > Max ->
    lists:reverse(Acc);
child_stream_tick_cycles(N, Max, B, Snap, V, Acc) ->
    B1 = B#{tick => N},
    {_Ops, Snap1, V1} = arizona_diff:diff(child_stream_tmpl(B1), Snap, V, #{tick => true}),
    #{~"c" := #{bindings := #{items := Items}}} = V1,
    Pending = length(arizona_stream:pending_ops(Items)),
    child_stream_tick_cycles(N + 1, Max, B1, Snap1, V1, [Pending | Acc]).

%% Parent template embedding arizona_stream_child and passing `tick` as a prop.
child_stream_tmpl(B) ->
    #{
        s => [~"<div az=\"0\">", ~"</div>"],
        d => [
            {~"0", fun() ->
                arizona_template:stateful(arizona_stream_child, #{
                    id => ~"c", tick => arizona_template:get(tick, B)
                })
            end}
        ],
        f => ~"parent"
    }.

%% --- eval API ---

eval_each_def_3tuple(Config) when is_list(Config) ->
    %% eval_each_def with 3-tuple location
    Items = [#{name => <<"a">>}],
    Tmpl = #{
        t => 0, s => [<<"<li>">>], d => fun(I) -> [{<<"0">>, maps:get(name, I)}] end, f => <<"x">>
    },
    Def = {<<"0">>, fun() -> arizona_template:each(Items, Tmpl) end, {handler, 15}},
    {Az, Val, _Deps} = arizona_eval:eval_each_def(Def),
    ?assertEqual(<<"0">>, Az),
    ?assertMatch(#{t := 0, source := _, template := _}, Val).

%% The tracked/diff render path (render_map_items) must yield entries in the same
%% order as the SSR path (render_map_items_simple); otherwise the live snapshot
%% disagrees with the SSR DOM item-for-item on connect. render_map_items folded
%% with a prepend and so used to return the entries reversed.
render_map_items_order_matches_ssr(Config) when is_list(Config) ->
    Map = #{<<"a">> => <<"1">>, <<"b">> => <<"2">>, <<"c">> => <<"3">>},
    Tmpl = #{
        t => 0,
        s => [<<"<li>">>, <<"</li>">>],
        d => fun(K, V) -> [{<<"0">>, <<K/binary, ":", V/binary>>}] end,
        f => <<"x">>
    },
    {Tracked, _Views} = arizona_eval:render_map_items(Map, Tmpl, {#{}, #{}}),
    Simple = arizona_eval:render_map_items_simple(Map, Tmpl),
    ?assertEqual(Simple, Tracked).

%% eval_val processes stateless descriptors (#{callback, props}), and the
%% enclosing dynamic namespaces the child's inner az ids (and fingerprint) by
%% the slot az -- here slot <<"0">> prefixes the child's <<"0">> -> <<"0-0">>
%% and <<"t1">> -> <<"0-t1">>, so repeated same-function renders don't collide.
eval_val_stateless_descriptor(Config) when is_list(Config) ->
    Callback = fun(Props) ->
        Title = maps:get(title, Props),
        #{s => [<<"<b>">>, <<"</b>">>], d => [{<<"0">>, Title}], f => <<"t1">>}
    end,
    Descriptor = #{callback => Callback, props => #{title => <<"hello">>}},
    Dyn = {<<"0">>, fun() -> Descriptor end},
    [{<<"0">>, Result}] = arizona_eval:eval_dynamics([Dyn]),
    ?assertMatch(
        #{s := [<<"<b>">>, <<"</b>">>], d := [{<<"0-0">>, <<"hello">>}], f := <<"0-t1">>}, Result
    ).

%% --- dep isolation in nested templates ---

%% A stateless callback that eagerly reads a binding BEFORE returning the
%% inner template must not pollute the outer dynamic's deps.
stateless_callback_does_not_leak_deps(Config) when is_list(Config) ->
    Callback = fun(_Props) ->
        eager_value = arizona_template:get(eager_key, #{eager_key => eager_value}),
        #{s => [<<"<b>">>, <<"</b>">>], d => [{<<"0">>, <<"hi">>}], f => <<"t1">>}
    end,
    Descriptor = #{callback => Callback, props => #{}},
    Dyn = {<<"0">>, fun() -> Descriptor end, {?MODULE, ?LINE}},
    {[{<<"0">>, _Val, OuterDeps}], _Views} =
        arizona_eval:eval_dynamics_v([Dyn], {#{}, #{}}),
    ?assertNot(maps:is_key(eager_key, OuterDeps)).

%% Happy-path sanity: a callback that does no eager reads still produces an
%% empty deps map for the outer dynamic and renders correctly.
stateless_callback_with_no_eager_reads(Config) when is_list(Config) ->
    Callback = fun(Props) ->
        Title = maps:get(title, Props),
        #{s => [<<"<b>">>, <<"</b>">>], d => [{<<"0">>, Title}], f => <<"t1">>}
    end,
    Descriptor = #{callback => Callback, props => #{title => <<"hi">>}},
    Dyn = {<<"0">>, fun() -> Descriptor end, {?MODULE, ?LINE}},
    {[{<<"0">>, Val, OuterDeps}], _Views} =
        arizona_eval:eval_dynamics_v([Dyn], {#{}, #{}}),
    ?assertEqual(#{}, OuterDeps),
    ?assertMatch(#{s := [<<"<b>">>, <<"</b>">>]}, Val).

%% Outer-level reads (those happening in the outer dynamic's closure body
%% before the descriptor is constructed) MUST be tracked. Only callback-body
%% eager reads should be discarded.
stateless_callback_eager_reads_dropped_outer_reads_kept(Config) when is_list(Config) ->
    Dyn =
        {<<"0">>,
            fun() ->
                outer_value = arizona_template:get(outer_key, #{outer_key => outer_value}),
                Callback = fun(_Props) ->
                    eager_value = arizona_template:get(eager_key, #{eager_key => eager_value}),
                    #{
                        s => [<<"<b>">>, <<"</b>">>],
                        d => [{<<"0">>, <<"hi">>}],
                        f => <<"t1">>
                    }
                end,
                #{callback => Callback, props => #{}}
            end,
            {?MODULE, ?LINE}},
    {[{<<"0">>, _Val, OuterDeps}], _Views} =
        arizona_eval:eval_dynamics_v([Dyn], {#{}, #{}}),
    ?assert(maps:is_key(outer_key, OuterDeps)),
    ?assertNot(maps:is_key(eager_key, OuterDeps)).

%% Multiple eager reads in a single callback body are all discarded together.
stateless_callback_multiple_eager_reads_all_dropped(Config) when is_list(Config) ->
    Callback = fun(_Props) ->
        a = arizona_template:get(eager_a, #{eager_a => a}),
        b = arizona_template:get(eager_b, #{eager_b => b}),
        c = arizona_template:get(eager_c, #{eager_c => c}),
        #{s => [<<"<b>">>, <<"</b>">>], d => [{<<"0">>, <<"hi">>}], f => <<"t1">>}
    end,
    Descriptor = #{callback => Callback, props => #{}},
    Dyn = {<<"0">>, fun() -> Descriptor end, {?MODULE, ?LINE}},
    {[{<<"0">>, _Val, OuterDeps}], _Views} =
        arizona_eval:eval_dynamics_v([Dyn], {#{}, #{}}),
    ?assertEqual(#{}, OuterDeps).

%% Two adjacent dynamics: one with a leaky callback, one normal. The leaky
%% callback must not pollute the second dynamic's deps slot either.
adjacent_dynamics_have_independent_deps(Config) when is_list(Config) ->
    LeakyCallback = fun(_Props) ->
        leak_value = arizona_template:get(leak_key, #{leak_key => leak_value}),
        #{s => [<<"<b>">>, <<"</b>">>], d => [{<<"0">>, <<"hi">>}], f => <<"t1">>}
    end,
    Dyn1 =
        {<<"0">>, fun() -> #{callback => LeakyCallback, props => #{}} end, {?MODULE, ?LINE}},
    Dyn2 =
        {<<"1">>,
            fun() ->
                normal_value = arizona_template:get(normal_key, #{normal_key => normal_value}),
                <<"static-value">>
            end,
            {?MODULE, ?LINE}},
    {[{<<"0">>, _, Deps0}, {<<"1">>, _, Deps1}], _Views} =
        arizona_eval:eval_dynamics_v([Dyn1, Dyn2], {#{}, #{}}),
    ?assertEqual(#{}, Deps0),
    ?assertEqual(#{normal_key => true}, Deps1).

%% Three layers of stateless nesting, each with its own eager read in the
%% callback body. None of the eager reads should leak to any ancestor's deps.
deeply_nested_stateless_no_leak_at_any_layer(Config) when is_list(Config) ->
    InnerMost = fun(_P) ->
        c = arizona_template:get(inner_c_key, #{inner_c_key => c}),
        #{s => [<<"<i>">>, <<"</i>">>], d => [{<<"0">>, <<"deep">>}], f => <<"t-c">>}
    end,
    Middle = fun(_P) ->
        b = arizona_template:get(inner_b_key, #{inner_b_key => b}),
        #{
            s => [<<"<m>">>, <<"</m>">>],
            d => [{<<"0">>, fun() -> #{callback => InnerMost, props => #{}} end, {?MODULE, ?LINE}}],
            f => <<"t-b">>
        }
    end,
    Outer = fun(_P) ->
        a = arizona_template:get(inner_a_key, #{inner_a_key => a}),
        #{
            s => [<<"<o>">>, <<"</o>">>],
            d => [{<<"0">>, fun() -> #{callback => Middle, props => #{}} end, {?MODULE, ?LINE}}],
            f => <<"t-a">>
        }
    end,
    Dyn =
        {<<"0">>, fun() -> #{callback => Outer, props => #{}} end, {?MODULE, ?LINE}},
    {[{<<"0">>, _Val, OuterDeps}], _Views} =
        arizona_eval:eval_dynamics_v([Dyn], {#{}, #{}}),
    ?assertNot(maps:is_key(inner_a_key, OuterDeps)),
    ?assertNot(maps:is_key(inner_b_key, OuterDeps)),
    ?assertNot(maps:is_key(inner_c_key, OuterDeps)).

%% A ?each whose item template embeds a stateless with a leaky callback. The
%% outer dynamic's deps should not contain the per-item callback's eager reads.
stateless_inside_each_no_leak(Config) when is_list(Config) ->
    Callback = fun(_Props) ->
        x = arizona_template:get(eager_each_key, #{eager_each_key => x}),
        #{s => [<<"<b>">>, <<"</b>">>], d => [{<<"0">>, <<"hi">>}], f => <<"t1">>}
    end,
    Items = [#{n => 1}, #{n => 2}],
    ItemTmpl = #{
        t => 0,
        s => [<<"<li>">>],
        d => fun(_I) ->
            [{<<"0">>, fun() -> #{callback => Callback, props => #{}} end, {?MODULE, ?LINE}}]
        end,
        f => <<"e">>
    },
    Dyn =
        {<<"0">>, fun() -> arizona_template:each(Items, ItemTmpl) end, {?MODULE, ?LINE}},
    {[{<<"0">>, _Val, OuterDeps}], _Views} =
        arizona_eval:eval_dynamics_v([Dyn], {#{}, #{}}),
    ?assertNot(maps:is_key(eager_each_key, OuterDeps)).

%% A stateful handler whose mount/1 callback does an eager binding read
%% must not pollute the outer dynamic's deps. mount/1 (and handle_update/2)
%% lifecycle methods run inside eval_stateful's bracket.
stateful_mount_eager_read_no_leak(Config) when is_list(Config) ->
    Descriptor = #{stateful => arizona_leaky_mount, props => #{id => ~"leaky"}},
    Dyn = {<<"0">>, fun() -> Descriptor end, {?MODULE, ?LINE}},
    {[{<<"0">>, _Val, OuterDeps}], _Views} =
        arizona_eval:eval_dynamics_v([Dyn], {#{}, #{}}),
    ?assertNot(maps:is_key(eager_mount_key, OuterDeps)).

%% --- per-item optimization (?each Levels 1+2) ---

%% Per-item rendering returns 3-tuples carrying deps captured during eval.
render_stream_item_persists_deps(Config) when is_list(Config) ->
    Tmpl = #{
        t => 0,
        s => [<<"<li>">>, <<"</li>">>],
        d => fun(Item, _Key) ->
            [{<<"0">>, fun() -> arizona_template:get(text, Item) end, {?MODULE, ?LINE}}]
        end,
        f => <<"x">>
    },
    Item = #{text => <<"hello">>},
    {[{<<"0">>, <<"hello">>, Deps}], _Views} =
        arizona_eval:render_stream_item(1, Item, Tmpl, {#{}, #{}}),
    ?assert(maps:is_key(text, Deps)).

%% A dynamic that explicitly tracks `text` is reused (not re-evaluated)
%% when only an unrelated key changes.
render_stream_item_skipping_reuses_unchanged(Config) when is_list(Config) ->
    Counter = counters:new(1, []),
    Tmpl = #{
        t => 0,
        s => [<<"<li>">>, <<"</li>">>],
        d => fun(Item, _Key) ->
            [
                {<<"0">>,
                    fun() ->
                        ok = counters:add(Counter, 1, 1),
                        arizona_template:get(text, Item)
                    end,
                    {?MODULE, ?LINE}}
            ]
        end,
        f => <<"x">>
    },
    OldItem = #{text => <<"hello">>, other => 1},
    NewItem = #{text => <<"hello">>, other => 2},
    {OldD, _} = arizona_eval:render_stream_item(1, OldItem, Tmpl, {#{}, #{}}),
    1 = counters:get(Counter, 1),
    Changed = #{other => true},
    {NewD, _} =
        arizona_eval:render_stream_item_skipping(1, NewItem, OldD, Changed, Tmpl, {#{}, #{}}),
    %% Counter still 1 -- the closure was NOT invoked the second time.
    1 = counters:get(Counter, 1),
    %% New triple is the exact same triple as before (reused as-is).
    ?assertEqual(OldD, NewD).

%% A dynamic without explicit ?get tracking (e.g. pattern destructure) has
%% empty deps -- skipping is unsafe, so re-eval happens on any non-empty
%% Changed.
render_stream_item_skipping_full_eval_on_empty_deps(Config) when is_list(Config) ->
    Tmpl = #{
        t => 0,
        s => [<<"<li>">>, <<"</li>">>],
        d => fun(#{text := Text}, _Key) ->
            [{<<"0">>, fun() -> Text end, {?MODULE, ?LINE}}]
        end,
        f => <<"x">>
    },
    OldItem = #{text => <<"old">>},
    NewItem = #{text => <<"new">>},
    {OldD, _} = arizona_eval:render_stream_item(1, OldItem, Tmpl, {#{}, #{}}),
    [{<<"0">>, <<"old">>, EmptyDeps}] = OldD,
    ?assertEqual(#{}, EmptyDeps),
    Changed = #{text => true},
    {NewD, _} =
        arizona_eval:render_stream_item_skipping(1, NewItem, OldD, Changed, Tmpl, {#{}, #{}}),
    %% Empty deps -> always re-eval, so we get the new value.
    [{<<"0">>, <<"new">>, _}] = NewD.

%% When Changed is empty, the whole item snapshot is reused without invoking
%% any per-item closure.
render_stream_item_skipping_short_circuits_on_empty_changed(Config) when is_list(Config) ->
    Counter = counters:new(1, []),
    Tmpl = #{
        t => 0,
        s => [<<"<li>">>, <<"</li>">>],
        d => fun(Item, _Key) ->
            [
                {<<"0">>,
                    fun() ->
                        ok = counters:add(Counter, 1, 1),
                        arizona_template:get(text, Item)
                    end,
                    {?MODULE, ?LINE}}
            ]
        end,
        f => <<"x">>
    },
    OldItem = #{text => <<"hello">>},
    {OldD, _} = arizona_eval:render_stream_item(1, OldItem, Tmpl, {#{}, #{}}),
    1 = counters:get(Counter, 1),
    {NewD, _} =
        arizona_eval:render_stream_item_skipping(1, OldItem, OldD, #{}, Tmpl, {#{}, #{}}),
    %% Empty Changed -> short-circuit, no closure invocation.
    1 = counters:get(Counter, 1),
    ?assertEqual(OldD, NewD).

%% A callback that returns a 0-arity fun (which eval_val_v then unwraps via
%% its is_function/0 clause) must not leak the fun-body's reads into the
%% outer dynamic's deps.
stateless_callback_returning_fun_no_leak(Config) when is_list(Config) ->
    Callback = fun(_Props) ->
        fun() ->
            late_value = arizona_template:get(late_key, #{late_key => late_value}),
            #{s => [<<"<b>">>, <<"</b>">>], d => [{<<"0">>, <<"hi">>}], f => <<"t1">>}
        end
    end,
    Descriptor = #{callback => Callback, props => #{}},
    Dyn = {<<"0">>, fun() -> Descriptor end, {?MODULE, ?LINE}},
    {[{<<"0">>, _Val, OuterDeps}], _Views} =
        arizona_eval:eval_dynamics_v([Dyn], {#{}, #{}}),
    ?assertNot(maps:is_key(late_key, OuterDeps)).
