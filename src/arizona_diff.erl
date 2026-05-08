-module(arizona_diff).
-moduledoc """
Computes the minimal set of patch operations between an old snapshot and a
freshly evaluated template.

Three entry points, each more powerful than the last:

- `diff/2` -- bare diff: walks new dynamics against old, emits patch ops.
- `diff/3` -- adds a `Views` accumulator so nested stateful children can be
  tracked across the recursion.
- `diff/4` -- adds a `Changed` set of dirty bindings, so dynamics whose
  dependencies didn't change are skipped entirely (the fast path).

Each op is a list whose first element is one of the `?OP_*` codes defined
in `arizona.hrl`; the rest are operands consumed by the JS client. The
op codes:

```
?OP_TEXT, ?OP_SET_ATTR, ?OP_REM_ATTR, ?OP_UPDATE, ?OP_REMOVE_NODE,
?OP_INSERT, ?OP_REMOVE, ?OP_ITEM_PATCH, ?OP_REPLACE, ?OP_MOVE
```

## Stream diffing

Streams ship with a queue of pending mutations (`insert`, `delete`,
`update`, `move`, `reorder`, `reset`). The diff drains that queue into
the corresponding ops, enforces visibility limits, and uses Longest
Increasing Subsequence (LIS) to emit the minimum number of `?OP_MOVE`s
for a reorder.

## Az-nodiff safety

Templates compiled with `az-nodiff` carry `diff => false` and dynamics
with `Az = undefined`. The diff functions short-circuit on `diff => false`
before ever inspecting individual dynamics, so `undefined` Az values
never reach op-code targets.
""".

-include("arizona.hrl").

%% --------------------------------------------------------------------
%% API function exports
%% --------------------------------------------------------------------

-export([diff/2]).
-export([diff/3]).
-export([diff/4]).
-export([deps_changed/2]).

%% --------------------------------------------------------------------
%% Ignore xref warnings
%% --------------------------------------------------------------------

-ignore_xref([diff/3]).

%% --------------------------------------------------------------------
%% Ignore elvis warnings
%% --------------------------------------------------------------------

%% Stream operation helpers (stream_insert, stream_delete, ...) all chain
%% into diff_stream_pending and assemble similar reply tuples. The shared
%% shape is intentional -- it's the queue-draining loop. Op-code clauses
%% in make_op also have inherent structural similarity.
-elvis([{elvis_style, dont_repeat_yourself, disable}]).

%% --------------------------------------------------------------------
%% Types exports
%% --------------------------------------------------------------------

-export_type([op/0]).

%% --------------------------------------------------------------------
%% Types definitions
%% --------------------------------------------------------------------

-nominal op() :: [integer() | binary() | term()].

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

-doc """
Bare diff: returns `{Ops, NewSnapshot}` for a new template against an old
snapshot.

Short-circuits to `{[], OldSnap}` when the old snapshot carries
`diff => false` (set by `az-nodiff`).
""".
-spec diff(Template, OldSnapshot) -> {Ops, NewSnapshot} when
    Template :: arizona_template:template(),
    OldSnapshot :: arizona_template:snapshot(),
    Ops :: [op()],
    NewSnapshot :: arizona_template:snapshot().
diff(_NewTmpl, #{diff := false} = OldSnap) ->
    {[], OldSnap};
diff(#{s := Statics, d := NewDynamics} = Tmpl, #{s := Statics, d := OldEvals} = OldSnap) ->
    EvalNew = arizona_eval:eval_dynamics(NewDynamics),
    Ops = diff_dynamics(EvalNew, OldEvals),
    Snap0 = preserve_view_id(OldSnap, #{s => Statics, d => EvalNew}),
    {Ops, arizona_template:maybe_put_fingerprint(Tmpl, Snap0)}.

-doc """
Diff with view tracking: threads a `Views` map through the recursion so
nested stateful children are accumulated alongside the patch ops.
""".
-spec diff(Template, OldSnapshot, Views) -> {Ops, NewSnapshot, Views1} when
    Template :: arizona_template:template(),
    OldSnapshot :: arizona_template:snapshot(),
    Views :: map(),
    Ops :: [op()],
    NewSnapshot :: arizona_template:snapshot(),
    Views1 :: map().
diff(_NewTmpl, #{diff := false} = OldSnap, Views) ->
    {[], OldSnap, Views};
diff(
    #{s := Statics, d := NewDynamics} = Tmpl,
    #{s := Statics, d := OldEvals} = OldSnap,
    Views0
) ->
    {Triples, {_Old, NewViews}} = arizona_eval:eval_dynamics_v(NewDynamics, {Views0, #{}}),
    {EvalNew, NewDeps, _Vals} = arizona_template:unzip_triples(Triples),
    Ops = diff_dynamics(EvalNew, OldEvals),
    Snap0 = preserve_view_id(OldSnap, #{s => Statics, d => EvalNew, deps => NewDeps}),
    {Ops, arizona_template:maybe_put_fingerprint(Tmpl, Snap0), NewViews}.

-doc """
Dependency-aware diff: takes a `Changed` map of dirty binding keys and
skips dynamics whose stored deps don't intersect with it.

This is the production fast path -- only the dynamics actually affected
by the bindings that changed are re-evaluated.
""".
-spec diff(Template, OldSnapshot, Views, Changed) -> {Ops, NewSnapshot, Views1} when
    Template :: arizona_template:template(),
    OldSnapshot :: arizona_template:snapshot(),
    Views :: map(),
    Changed :: map(),
    Ops :: [op()],
    NewSnapshot :: arizona_template:snapshot(),
    Views1 :: map().
diff(_NewTmpl, #{diff := false} = OldSnap, Views, _Changed) ->
    {[], OldSnap, Views};
diff(
    #{s := Statics, d := NewDynamics} = Tmpl,
    #{s := Statics, d := OldEvals, deps := OldDeps} = OldSnap,
    Views0,
    Changed
) ->
    {Ops, NewD, NewDeps, {_Old, NewViews}} =
        diff_dynamics_v(NewDynamics, OldEvals, OldDeps, Changed, {Views0, #{}}),
    Snap0 = preserve_view_id(OldSnap, #{s => Statics, d => NewD, deps => NewDeps}),
    {Ops, arizona_template:maybe_put_fingerprint(Tmpl, Snap0), NewViews}.

%% `view_id` lives on child-view snapshots (set by `make_child_snap`) and is
%% read by `make_op/3` to detect child diffs. The rebuilt snapshot must carry
%% it forward so subsequent diffs keep matching the child-view clause.
preserve_view_id(#{view_id := VId}, Snap) -> Snap#{view_id => VId};
preserve_view_id(#{}, Snap) -> Snap.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

diff_dynamics([], []) ->
    [];
diff_dynamics([{Az, _} | NR], [{Az, #{diff := false}} | OR]) ->
    diff_dynamics(NR, OR);
diff_dynamics([{Az, Same} | NR], [{Az, Same} | OR]) ->
    diff_dynamics(NR, OR);
diff_dynamics([{Az, New} | NR], [{Az, Old} | OR]) ->
    [make_op(Az, New, Old) | diff_dynamics(NR, OR)].

diff_dynamics_v([], [], [], _Changed, Views) ->
    {[], [], [], Views};
diff_dynamics_v(
    [_Def | DR],
    [{Az, #{diff := false} = Old} | OR],
    [ODeps | DepsR],
    Changed,
    Views0
) ->
    skip_dynamic(Az, Old, ODeps, DR, OR, DepsR, Changed, Views0);
diff_dynamics_v([Def | DR], [{Az, Old} | OR], [ODeps | DepsR], Changed, Views0) ->
    case deps_changed(ODeps, Changed) of
        false ->
            skip_dynamic(Az, Old, ODeps, DR, OR, DepsR, Changed, Views0);
        true ->
            diff_changed_dynamic(Def, Az, Old, DR, OR, DepsR, Changed, Views0)
    end.

%% Skip a dynamic whose deps haven't changed: carry its child views to the
%% new accumulator and continue with the original Az/Old/Deps.
skip_dynamic(Az, Old, ODeps, DR, OR, DepsR, Changed, Views0) ->
    Views1 = carry_skipped_view(Old, Views0),
    {OpsRest, DRest, DepsRest, Views2} =
        diff_dynamics_v(DR, OR, DepsR, Changed, Views1),
    {OpsRest, [{Az, Old} | DRest], [ODeps | DepsRest], Views2}.

%% Re-evaluate a dynamic whose deps have changed. Each-containers take a
%% special path because their child snapshots need merging; everything else
%% goes through eval_one_v_flat and a value comparison.
diff_changed_dynamic(Def, _Az, #{t := ?EACH} = Old, DR, OR, DepsR, Changed, Views0) ->
    {Az2, EachDesc, Deps} = arizona_eval:eval_each_def(Def),
    diff_each(Az2, EachDesc, Deps, Old, DR, OR, DepsR, Changed, Views0);
diff_changed_dynamic(Def, Az, Old, DR, OR, DepsR, Changed, Views0) ->
    {Az, New, NewDeps, Views1} = arizona_eval:eval_one_v_flat(Def, Views0),
    {OpsRest, DRest, DepsRest, Views2} =
        diff_dynamics_v(DR, OR, DepsR, Changed, Views1),
    Ops =
        case New of
            Old -> OpsRest;
            _ -> [make_op(Az, New, Old) | OpsRest]
        end,
    {Ops, [{Az, New} | DRest], [NewDeps | DepsRest], Views2}.

diff_each(
    Az, #{source := #stream{} = Source} = EachDesc, Deps, Old, DR, OR, DepsR, Changed, Views0
) ->
    {Old0, New0} = Views0,
    {StreamOps, NewSnap0, {_, LocalNew}} =
        diff_stream(Az, EachDesc, Old, {Old0, #{}}),
    LocalNew1 = merge_stream_child_views(Source, Old, LocalNew, Old0),
    NewSnap = NewSnap0#{child_views => maps:keys(LocalNew1)},
    Views1 = {Old0, maps:merge(New0, LocalNew1)},
    {OpsRest, DRest, DepsRest, Views2} =
        diff_dynamics_v(DR, OR, DepsR, Changed, Views1),
    {StreamOps ++ OpsRest, [{Az, NewSnap} | DRest], [Deps | DepsRest], Views2};
diff_each(
    Az, #{source := Items} = EachDesc, Deps, Old, DR, OR, DepsR, Changed, Views0
) when is_list(Items) ->
    {Old0, New0} = Views0,
    {ListOps, NewSnap0, {_, LocalNew}} =
        diff_list(Az, EachDesc, Old, {Old0, #{}}),
    NewSnap = NewSnap0#{child_views => maps:keys(LocalNew)},
    Views1 = {Old0, maps:merge(New0, LocalNew)},
    {OpsRest, DRest, DepsRest, Views2} =
        diff_dynamics_v(DR, OR, DepsR, Changed, Views1),
    {ListOps ++ OpsRest, [{Az, NewSnap} | DRest], [Deps | DepsRest], Views2}.

%% Incremental stream child_views: old - deleted + rendered.
merge_stream_child_views(Source, Old, LocalNew, Old0) ->
    OldChildViews = maps:get(child_views, Old, []),
    #{items := OldItems} = Old,
    Deleted = deleted_item_children(Source#stream.pending, OldItems),
    Surviving = OldChildViews -- Deleted -- maps:keys(LocalNew),
    carry_item_children(Surviving, Old0, LocalNew).

%% When a dynamic is skipped (deps unchanged), carry its child views over
%% from OldViews to NewViews so they aren't pruned.
carry_skipped_view(#{view_id := VId}, {Old, New}) ->
    case Old of
        #{VId := View} -> {Old, New#{VId => View}};
        #{} -> {Old, New}
    end;
carry_skipped_view(#{child_views := ChildIds}, {Old, New}) ->
    {Old, maps:merge(New, maps:with(ChildIds, Old))};
carry_skipped_view(_Old, Views) ->
    Views.

%% Extract child view IDs from deleted stream items only.
deleted_item_children(Pending, OldItems) ->
    lists:foldl(
        fun
            ({delete, Key}, Acc) ->
                case OldItems of
                    #{Key := ItemD} -> item_child_views(ItemD, Acc);
                    #{} -> Acc
                end;
            (_, Acc) ->
                Acc
        end,
        [],
        queue:to_list(Pending)
    ).

item_child_views(ItemD, Acc) ->
    lists:foldl(
        fun
            ({_Az, #{view_id := VId}, _Deps}, A) -> [VId | A];
            (_, A) -> A
        end,
        Acc,
        ItemD
    ).

%% Copy child views from OldViews to NewViews for children not already present.
carry_item_children(ChildViewIds, Old, New) ->
    maps:merge(New, maps:with(ChildViewIds, Old)).

-doc """
Returns `true` when any key in `Deps` also appears in `Changed`. Used by
`diff/4` and the per-item skipping renderer to decide whether a dynamic
needs re-evaluation. Walks the smaller map and probes the larger via
`is_map_key/2` -- avoids the allocation of `maps:intersect/2`.
""".
-spec deps_changed(Deps, Changed) -> boolean() when
    Deps :: map(),
    Changed :: map().
deps_changed(Deps, Changed) ->
    case map_size(Deps) =< map_size(Changed) of
        true -> any_key_in(Deps, Changed);
        false -> any_key_in(Changed, Deps)
    end.

any_key_in(Small, Large) ->
    any_key_in_iter(maps:next(maps:iterator(Small)), Large).

any_key_in_iter(none, _Large) ->
    false;
any_key_in_iter({K, _V, Iter}, Large) ->
    case is_map_key(K, Large) of
        true -> true;
        false -> any_key_in_iter(maps:next(Iter), Large)
    end.

diff_stream(
    Az,
    #{source := #stream{} = Source, template := Tmpl},
    #{items := OldItems} = OldSnap,
    Views0
) ->
    case OldSnap of
        #{order := OldOrder} ->
            diff_stream_pending(
                Az,
                Source#stream.pending,
                Source,
                Tmpl,
                OldItems,
                OldOrder,
                Views0
            );
        #{} ->
            VKeys = arizona_template:visible_keys(Source#stream.order, Source#stream.limit),
            {ItemSnaps, Views1} = arizona_eval:eval_stream_items(
                VKeys,
                Source#stream.items,
                Tmpl,
                Views0
            ),
            NewSnap = #{
                t => ?EACH,
                items => ItemSnaps,
                order => VKeys,
                source => Source,
                template => Tmpl
            },
            HTML = arizona_render:zip_stream_fp(Tmpl, ItemSnaps, VKeys),
            {[[?OP_UPDATE, Az, HTML]], NewSnap, Views1}
    end.

diff_stream_pending(Az, Queue, Source, Tmpl, SnapAcc, OldOrder, Views0) ->
    case queue:out(Queue) of
        {empty, _} ->
            apply_limit(Az, Source, Tmpl, SnapAcc, Views0);
        {{value, Op}, Rest} ->
            diff_stream_op(Az, Op, Rest, Source, Tmpl, SnapAcc, OldOrder, Views0)
    end.

diff_stream_op(Az, {insert, Key, Item, Pos}, Rest, Source, Tmpl, SnapAcc, OldOrder, Views) ->
    stream_insert(Az, Key, Item, Pos, Rest, Source, Tmpl, SnapAcc, OldOrder, Views);
diff_stream_op(Az, {delete, Key}, Rest, Source, Tmpl, SnapAcc, OldOrder, Views) ->
    stream_delete(Az, Key, Rest, Source, Tmpl, SnapAcc, OldOrder, Views);
diff_stream_op(Az, {update, Key, NewItem, Changed}, Rest, Source, Tmpl, SnapAcc, OldOrder, Views) ->
    stream_update(Az, Key, NewItem, Changed, Rest, Source, Tmpl, SnapAcc, OldOrder, Views);
diff_stream_op(Az, {move, Key, AfterKey}, Rest, Source, Tmpl, SnapAcc, OldOrder, Views) ->
    stream_move(Az, Key, AfterKey, Rest, Source, Tmpl, SnapAcc, OldOrder, Views);
diff_stream_op(Az, reorder, Rest, Source, Tmpl, SnapAcc, OldOrder, Views) ->
    stream_reorder(Az, Rest, Source, Tmpl, SnapAcc, OldOrder, Views);
diff_stream_op(Az, {reset, OldItems}, Rest, Source, Tmpl, SnapAcc, OldOrder, Views) ->
    stream_reset(Az, OldItems, Rest, Source, Tmpl, SnapAcc, OldOrder, Views).

stream_insert(Az, Key, Item, Pos, Rest, Source, Tmpl, SnapAcc, OldOrder, Views0) ->
    {ItemD, Views1} = arizona_eval:render_stream_item(Key, Item, Tmpl, Views0),
    HTML = arizona_render:zip_item(Tmpl, ItemD),
    InsOp = [?OP_INSERT, Az, arizona_template:to_bin(Key), Pos, HTML],
    NewSnapAcc = SnapAcc#{Key => ItemD},
    {RestOps, FinalSnap, Views2} =
        diff_stream_pending(Az, Rest, Source, Tmpl, NewSnapAcc, OldOrder, Views1),
    {[InsOp | RestOps], FinalSnap, Views2}.

stream_delete(Az, Key, Rest, Source, Tmpl, SnapAcc, OldOrder, Views0) ->
    DelOp = [?OP_REMOVE, Az, arizona_template:to_bin(Key)],
    NewSnapAcc = maps:remove(Key, SnapAcc),
    {RestOps, FinalSnap, Views1} =
        diff_stream_pending(Az, Rest, Source, Tmpl, NewSnapAcc, OldOrder, Views0),
    {[DelOp | RestOps], FinalSnap, Views1}.

stream_update(Az, Key, NewItem, Changed, Rest, Source, Tmpl, SnapAcc, OldOrder, Views0) ->
    case SnapAcc of
        #{Key := OldD} ->
            {NewD, Views1} =
                arizona_eval:render_stream_item_skipping(
                    Key, NewItem, OldD, Changed, Tmpl, Views0
                ),
            stream_update_existing(
                Az, Key, NewD, OldD, Rest, Source, Tmpl, SnapAcc, OldOrder, Views1
            );
        #{} ->
            {NewD, Views1} = arizona_eval:render_stream_item(Key, NewItem, Tmpl, Views0),
            stream_update_missing(Az, Key, NewD, Rest, Source, Tmpl, SnapAcc, OldOrder, Views1)
    end.

stream_update_existing(Az, Key, NewD, OldD, Rest, Source, Tmpl, SnapAcc, OldOrder, Views0) ->
    {InnerOps, Views1} = diff_item_dynamics_v(NewD, OldD, Views0),
    case InnerOps of
        [] ->
            diff_stream_pending(Az, Rest, Source, Tmpl, SnapAcc, OldOrder, Views1);
        _ ->
            PatchOp = [?OP_ITEM_PATCH, Az, arizona_template:to_bin(Key), InnerOps],
            NewSnapAcc = SnapAcc#{Key => NewD},
            {RestOps, FinalSnap, Views2} =
                diff_stream_pending(Az, Rest, Source, Tmpl, NewSnapAcc, OldOrder, Views1),
            {[PatchOp | RestOps], FinalSnap, Views2}
    end.

stream_update_missing(Az, Key, NewD, Rest, Source, Tmpl, SnapAcc, OldOrder, Views0) ->
    HTML = arizona_render:zip_item(Tmpl, NewD),
    InsOp = [?OP_INSERT, Az, arizona_template:to_bin(Key), -1, HTML],
    NewSnapAcc = SnapAcc#{Key => NewD},
    {RestOps, FinalSnap, Views1} =
        diff_stream_pending(Az, Rest, Source, Tmpl, NewSnapAcc, OldOrder, Views0),
    {[InsOp | RestOps], FinalSnap, Views1}.

stream_move(Az, Key, AfterKey, Rest, Source, Tmpl, SnapAcc, OldOrder, Views0) ->
    case SnapAcc of
        #{Key := _} ->
            Ref = move_after_ref(AfterKey),
            MoveOp = [?OP_MOVE, Az, arizona_template:to_bin(Key), Ref],
            {RestOps, FinalSnap, Views1} =
                diff_stream_pending(Az, Rest, Source, Tmpl, SnapAcc, OldOrder, Views0),
            {[MoveOp | RestOps], FinalSnap, Views1};
        #{} ->
            diff_stream_pending(Az, Rest, Source, Tmpl, SnapAcc, OldOrder, Views0)
    end.

move_after_ref(null) -> null;
move_after_ref(AfterKey) -> arizona_template:to_bin(AfterKey).

stream_reorder(Az, Rest, Source, Tmpl, SnapAcc, OldOrder, Views0) ->
    VKeys = arizona_template:visible_keys(Source#stream.order, Source#stream.limit),
    MoveOps = compute_reorder_ops(Az, OldOrder, VKeys, SnapAcc),
    {RestOps, FinalSnap, Views1} =
        diff_stream_pending(Az, Rest, Source, Tmpl, SnapAcc, VKeys, Views0),
    {MoveOps ++ RestOps, FinalSnap, Views1}.

stream_reset(Az, OldItems, Rest, Source, Tmpl, SnapAcc, OldOrder, Views0) ->
    VKeys = arizona_template:visible_keys(Source#stream.order, Source#stream.limit),
    VSet = maps:from_keys(VKeys, true),
    RemOps = [
        [?OP_REMOVE, Az, arizona_template:to_bin(K)]
     || K := _ <- SnapAcc, not is_map_key(K, VSet)
    ],
    Kept = maps:with(VKeys, SnapAcc),
    {DiffOps, NewSnaps, Views1} =
        smart_reset_items(Az, VKeys, Kept, OldItems, Source#stream.items, Tmpl, Views0, #{}),
    MoveOps = compute_reorder_ops(Az, OldOrder, VKeys, Kept),
    {RestOps, FinalSnap, Views2} =
        diff_stream_pending(Az, Rest, Source, Tmpl, NewSnaps, VKeys, Views1),
    {RemOps ++ DiffOps ++ MoveOps ++ RestOps, FinalSnap, Views2}.

diff_list(Az, #{source := Items, template := Tmpl}, #{items := OldItemsList}, Views0) ->
    {NewItemsList, Views1} = arizona_eval:render_list_items(Items, Tmpl, Views0),
    NewSnap = #{t => ?EACH, items => NewItemsList, template => Tmpl},
    case is_list(OldItemsList) of
        false ->
            HTML = arizona_render:zip_list_fp(Tmpl, NewItemsList),
            {[[?OP_UPDATE, Az, HTML]], NewSnap, Views1};
        true ->
            {Ops, NewTail, OldTail, Views2} =
                diff_list_zip(Az, NewItemsList, OldItemsList, Views1),
            case {NewTail, OldTail} of
                {[], []} ->
                    {Ops, NewSnap, Views2};
                _ ->
                    HTML = arizona_render:zip_list_fp(Tmpl, NewItemsList),
                    {[[?OP_UPDATE, Az, HTML]], NewSnap, Views2}
            end
    end.

diff_list_zip(Az, [NewD | NR], [OldD | OR], Views0) ->
    {InnerOps, Views1} = diff_item_dynamics_v(NewD, OldD, Views0),
    {RestOps, NewTail, OldTail, Views2} = diff_list_zip(Az, NR, OR, Views1),
    Ops =
        case InnerOps of
            [] ->
                RestOps;
            _ ->
                [{_, OldKey, _} | _] = OldD,
                [[?OP_ITEM_PATCH, Az, arizona_template:to_bin(OldKey), InnerOps] | RestOps]
        end,
    {Ops, NewTail, OldTail, Views2};
diff_list_zip(_Az, NewTail, OldTail, Views) ->
    {[], NewTail, OldTail, Views}.

smart_reset_items(_Az, [], _Kept, _OldItems, _ItemsMap, _Tmpl, Views, Snaps) ->
    {[], Snaps, Views};
smart_reset_items(Az, [K | Rest], Kept, OldItems, ItemsMap, Tmpl, Views0, Snaps) ->
    NewItem = maps:get(K, ItemsMap),
    case Kept of
        #{K := OldD} ->
            {NewD, Views1} =
                render_kept_with_skipping(K, NewItem, OldD, OldItems, Tmpl, Views0),
            NewSnaps = Snaps#{K => NewD},
            {InnerOps, Views2} = diff_item_dynamics_v(NewD, OldD, Views1),
            case InnerOps of
                [] ->
                    smart_reset_items(
                        Az, Rest, Kept, OldItems, ItemsMap, Tmpl, Views2, NewSnaps
                    );
                _ ->
                    PatchOp = [?OP_ITEM_PATCH, Az, arizona_template:to_bin(K), InnerOps],
                    {RestOps, FinalSnaps, Views3} =
                        smart_reset_items(
                            Az, Rest, Kept, OldItems, ItemsMap, Tmpl, Views2, NewSnaps
                        ),
                    {[PatchOp | RestOps], FinalSnaps, Views3}
            end;
        #{} ->
            {NewD, Views1} = arizona_eval:render_stream_item(K, NewItem, Tmpl, Views0),
            NewSnaps = Snaps#{K => NewD},
            HTML = arizona_render:zip_item(Tmpl, NewD),
            InsOp = [?OP_INSERT, Az, arizona_template:to_bin(K), -1, HTML],
            {RestOps, FinalSnaps, Views2} =
                smart_reset_items(
                    Az, Rest, Kept, OldItems, ItemsMap, Tmpl, Views1, NewSnaps
                ),
            {[InsOp | RestOps], FinalSnaps, Views2}
    end.

%% Render a kept item via the per-item skipping path when its old source
%% is recoverable from the captured `OldItems`. Falls back to a full
%% render if the key wasn't in the pre-reset items map (rare: limit-hidden
%% then re-shown).
render_kept_with_skipping(K, NewItem, OldD, OldItems, Tmpl, Views0) ->
    case OldItems of
        #{K := OldItem} ->
            Changed = arizona_stream:compute_item_changed(OldItem, NewItem),
            arizona_eval:render_stream_item_skipping(K, NewItem, OldD, Changed, Tmpl, Views0);
        #{} ->
            arizona_eval:render_stream_item(K, NewItem, Tmpl, Views0)
    end.

apply_limit(
    _Az,
    #stream{limit = infinity, order = Order},
    Tmpl,
    SnapItems,
    Views
) ->
    %% Flush the {Front, BackRev} buffer to a flat list -- the snapshot's
    %% `order` is consumed by `arizona_render:zip/2` as a list iterator,
    %% not by `visible_keys/2`, so we need to materialise here.
    FlatOrder = arizona_template:visible_keys(Order, infinity),
    {[], #{t => ?EACH, items => SnapItems, order => FlatOrder, template => Tmpl}, Views};
apply_limit(
    Az,
    #stream{limit = Limit, on_limit = halt, order = Order},
    Tmpl,
    SnapItems,
    Views
) ->
    VKeys = arizona_template:visible_keys(Order, Limit),
    KeepOnly = maps:with(VKeys, SnapItems),
    DropOps = [
        [?OP_REMOVE, Az, arizona_template:to_bin(K)]
     || K := _ <- SnapItems, not is_map_key(K, KeepOnly)
    ],
    {DropOps, #{t => ?EACH, items => KeepOnly, order => VKeys, template => Tmpl}, Views};
apply_limit(
    Az,
    #stream{
        limit = Limit,
        on_limit = drop,
        items = ItemsMap,
        order = Order
    },
    Tmpl,
    SnapItems,
    Views0
) ->
    VKeys = arizona_template:visible_keys(Order, Limit),
    VSet = maps:from_keys(VKeys, true),
    RemOps = [
        [?OP_REMOVE, Az, arizona_template:to_bin(K)]
     || K := _ <- SnapItems, not is_map_key(K, VSet)
    ],
    Pruned = #{K => V || K := V <- SnapItems, is_map_key(K, VSet)},
    {InsOps, Final, Views1} = snap_add_missing(
        Az,
        VKeys,
        Pruned,
        ItemsMap,
        Tmpl,
        Views0
    ),
    {RemOps ++ InsOps, #{t => ?EACH, items => Final, order => VKeys, template => Tmpl}, Views1}.

snap_add_missing(_Az, [], Snaps, _ItemsMap, _Tmpl, Views) ->
    {[], Snaps, Views};
snap_add_missing(Az, [K | Rest], Snaps, ItemsMap, Tmpl, Views0) ->
    case Snaps of
        #{K := _} ->
            snap_add_missing(Az, Rest, Snaps, ItemsMap, Tmpl, Views0);
        #{} ->
            Item = maps:get(K, ItemsMap),
            {ItemD, Views1} = arizona_eval:render_stream_item(K, Item, Tmpl, Views0),
            HTML = arizona_render:zip_item(Tmpl, ItemD),
            InsOp = [?OP_INSERT, Az, arizona_template:to_bin(K), -1, HTML],
            NewSnaps = Snaps#{K => ItemD},
            {RestOps, FinalSnaps, Views2} =
                snap_add_missing(Az, Rest, NewSnaps, ItemsMap, Tmpl, Views1),
            {[InsOp | RestOps], FinalSnaps, Views2}
    end.

%% Longest Increasing Subsequence -- minimal moves for stream reorder.
lis_indices(NewOrder, OldPosMap) ->
    {Len, Tails, Parent} = lis_scan(NewOrder, OldPosMap, 1, 0, #{}, #{}),
    case Len of
        0 ->
            #{};
        _ ->
            #{(Len - 1) := {_, LastIdx}} = Tails,
            lis_backtrack(LastIdx, Parent, #{})
    end.

lis_scan([], _PosMap, _I, Len, Tails, Parent) ->
    {Len, Tails, Parent};
lis_scan([Key | Rest], PosMap, I, Len, Tails, Parent) ->
    case PosMap of
        #{Key := V} ->
            case Len of
                0 ->
                    lis_scan(Rest, PosMap, I + 1, 1, #{0 => {V, I}}, Parent);
                _ ->
                    #{(Len - 1) := {TailVal, TailIdx}} = Tails,
                    case TailVal < V of
                        true ->
                            lis_scan(
                                Rest,
                                PosMap,
                                I + 1,
                                Len + 1,
                                Tails#{Len => {V, I}},
                                Parent#{I => TailIdx}
                            );
                        false ->
                            Pos = lis_bsearch(Tails, V, 0, Len - 1),
                            Parent2 = update_lis_parent(Pos, I, Parent, Tails),
                            lis_scan(
                                Rest,
                                PosMap,
                                I + 1,
                                Len,
                                Tails#{Pos => {V, I}},
                                Parent2
                            )
                    end
            end;
        #{} ->
            lis_scan(Rest, PosMap, I + 1, Len, Tails, Parent)
    end.

update_lis_parent(0, _I, Parent, _Tails) ->
    Parent;
update_lis_parent(Pos, I, Parent, Tails) ->
    #{(Pos - 1) := {_, PrevIdx}} = Tails,
    Parent#{I => PrevIdx}.

lis_bsearch(_Tails, _V, Lo, Hi) when Lo >= Hi -> Lo;
lis_bsearch(Tails, V, Lo, Hi) ->
    Mid = (Lo + Hi) bsr 1,
    #{Mid := {MidVal, _}} = Tails,
    case MidVal < V of
        true -> lis_bsearch(Tails, V, Mid + 1, Hi);
        false -> lis_bsearch(Tails, V, Lo, Mid)
    end.

lis_backtrack(undefined, _Parent, Acc) ->
    Acc;
lis_backtrack(Idx, Parent, Acc) ->
    Next =
        case Parent of
            #{Idx := V} -> V;
            #{} -> undefined
        end,
    lis_backtrack(Next, Parent, Acc#{Idx => true}).

compute_reorder_ops(_Az, OldOrder, OldOrder, _Kept) ->
    [];
compute_reorder_ops(Az, OldOrder, NewOrder, Kept) ->
    KeptOld = [K || K <- OldOrder, is_map_key(K, Kept)],
    case KeptOld of
        [] ->
            [];
        _ ->
            OldPosMap = pos_map(KeptOld, 1),
            LISSet = lis_indices(NewOrder, OldPosMap),
            emit_move_ops(Az, LISSet, NewOrder, 1, null)
    end.

pos_map([], _I) -> #{};
pos_map([K | Rest], I) -> (pos_map(Rest, I + 1))#{K => I}.

emit_move_ops(_Az, _LIS, [], _I, _Prev) ->
    [];
emit_move_ops(Az, LIS, [Key | Rest], I, Prev) ->
    case LIS of
        #{I := _} ->
            emit_move_ops(Az, LIS, Rest, I + 1, Key);
        #{} ->
            Ref =
                case Prev of
                    null -> null;
                    _ -> arizona_template:to_bin(Prev)
                end,
            [
                [?OP_MOVE, Az, arizona_template:to_bin(Key), Ref]
                | emit_move_ops(Az, LIS, Rest, I + 1, Key)
            ]
    end.

make_op(Az, {attr, Attr, false}, _Old) ->
    [?OP_REM_ATTR, Az, Attr];
make_op(Az, {attr, Attr, true}, _Old) ->
    [?OP_SET_ATTR, Az, Attr, <<>>];
make_op(Az, {attr, Attr, Val}, _Old) ->
    [?OP_SET_ATTR, Az, Attr, arizona_template:to_bin(Val)];
make_op(_Az, #{view_id := VId, s := S, d := NewD}, #{view_id := _, s := S, d := OldD}) ->
    [VId, diff_child_dynamics(NewD, OldD)];
make_op(Az, #{s := _, d := _} = NewNested, #{s := _, d := _}) ->
    [?OP_TEXT, Az, arizona_render:zip_or_fp(NewNested)];
make_op(Az, #{s := _, d := _} = Tmpl, _Old) ->
    [?OP_UPDATE, Az, arizona_render:zip_or_fp(Tmpl)];
make_op(Az, #{t := ?EACH, items := Items, template := Tmpl}, _Old) when
    is_list(Items)
->
    [?OP_UPDATE, Az, arizona_render:zip_list_fp(Tmpl, Items)];
make_op(Az, #{t := ?EACH, items := Items, order := Order, template := Tmpl}, _Old) ->
    [?OP_UPDATE, Az, arizona_render:zip_stream_fp(Tmpl, Items, Order)];
make_op(Az, remove, {attr, Attr, _}) ->
    [?OP_REM_ATTR, Az, Attr];
make_op(Az, remove, _Old) ->
    [?OP_REMOVE_NODE, Az];
make_op(Az, New, _Old) ->
    [?OP_TEXT, Az, arizona_template:to_bin(New)].

diff_item_dynamics_v([], [], Views) ->
    {[], Views};
diff_item_dynamics_v([{Az, _, _} | NR], [{Az, #{diff := false}, _} | OR], Views0) ->
    diff_item_dynamics_v(NR, OR, Views0);
diff_item_dynamics_v([{Az, Same, _} | NR], [{Az, Same, _} | OR], Views0) ->
    diff_item_dynamics_v(NR, OR, Views0);
diff_item_dynamics_v([{Az, New, _} | NR], [{Az, Old, _} | OR], Views0) ->
    case {New, Old} of
        {#{t := ?EACH, source := Src, template := Tmpl}, #{t := ?EACH}} ->
            EachDesc = #{source => Src, template => Tmpl},
            {EachOps, _NewSnap, Views1} =
                case Src of
                    #stream{} ->
                        diff_stream(Az, EachDesc, Old, Views0);
                    _ when is_list(Src) ->
                        diff_list(Az, EachDesc, Old, Views0)
                end,
            {RestOps, Views2} = diff_item_dynamics_v(NR, OR, Views1),
            {EachOps ++ RestOps, Views2};
        _ ->
            {RestOps, Views1} = diff_item_dynamics_v(NR, OR, Views0),
            {[make_op(Az, New, Old) | RestOps], Views1}
    end.

diff_child_dynamics([], []) ->
    [];
diff_child_dynamics([{Az, _New} | NR], [{Az, #{diff := false}} | OR]) ->
    diff_child_dynamics(NR, OR);
diff_child_dynamics([{Az, Same} | NR], [{Az, Same} | OR]) ->
    diff_child_dynamics(NR, OR);
diff_child_dynamics([{Az, New} | NR], [{Az, Old} | OR]) ->
    [make_op(Az, New, Old) | diff_child_dynamics(NR, OR)].
