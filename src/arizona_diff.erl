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

## `?each` list vs. stream diffing

A `?each` over a **plain list** is unkeyed: its items render as bare HTML
with no `az-key`, so there is no per-item DOM address to patch. Any change
(item content or list length) therefore re-renders the whole list with a
single `?OP_TEXT` patching the slot's `<!--az:X-->...<!--/az-->` marker
content; an unchanged list emits nothing. (`?OP_TEXT`, not `?OP_UPDATE`: a
plain-list each is marker-anchored in a content slot, so innerHTML on the
fallback enclosing element would clobber the slot's static siblings.)

Use an `arizona_stream` when you need **keyed, incremental** updates --
per-item `?OP_ITEM_PATCH`/`?OP_INSERT`/`?OP_REMOVE`/`?OP_MOVE` and stable
item identity (e.g. to preserve a stateful child's state across updates). A
plain list's full re-render re-mounts any such children.

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

A diffable template can also carry an individual `Az = undefined` dynamic:
a content slot inside a raw-text element (`script`/`style`/`textarea`/`title`),
where HTML comment markers would become literal content. Such a slot is
render-once -- `diff_dynamics/3` and `diff_dynamics_v/5` skip any `undefined`
Az dynamic, so it is never re-evaluated and never produces an op.
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

%% Public bare-diff entry point, exercised by the test suites but no longer
%% called from production (arizona_live uses diff/3 and diff/4).
-ignore_xref([diff/2]).

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
    {EvalNew, NewDeps} = arizona_template:split_triples(Triples),
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

diff_dynamics(NewEvals, OldEvals) ->
    diff_dynamics(NewEvals, OldEvals, []).

%% Tail-accumulator (difference-list) form: each op is consed straight onto `Tail`,
%% so there is no `++` copying. This matters for the fine-grained nested-template path
%% (`make_ops/4` recurses back here), where a `++` would re-copy the inner ops at every
%% nesting level. `Tail` is the ops that follow these dynamics; order is preserved.
diff_dynamics([], [], Tail) ->
    Tail;
diff_dynamics([{undefined, _} | NR], [{undefined, _} | OR], Tail) ->
    %% Markerless render-once slot (raw-text element content, or az-nodiff): no
    %% comment marker to target, so never emit an op -- carry it forward as-is.
    diff_dynamics(NR, OR, Tail);
diff_dynamics([{Az, _} | NR], [{Az, #{diff := false}} | OR], Tail) ->
    diff_dynamics(NR, OR, Tail);
diff_dynamics([{Az, Same} | NR], [{Az, Same} | OR], Tail) ->
    diff_dynamics(NR, OR, Tail);
diff_dynamics([{Az, New} | NR], [{Az, Old} | OR], Tail) ->
    make_ops(Az, New, Old, diff_dynamics(NR, OR, Tail)).

diff_dynamics_v([], [], [], _Changed, Views) ->
    {[], [], [], Views};
diff_dynamics_v(
    [_Def | DR],
    [{undefined, Old} | OR],
    [ODeps | DepsR],
    Changed,
    Views0
) ->
    %% Markerless render-once slot (raw-text element content, or az-nodiff): no
    %% comment marker to target, so skip it -- never re-evaluate, never emit an op.
    skip_dynamic(undefined, Old, ODeps, DR, OR, DepsR, Changed, Views0);
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
            _ -> make_ops(Az, New, Old, OpsRest)
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
    {ListOps ++ OpsRest, [{Az, NewSnap} | DRest], [Deps | DepsRest], Views2};
diff_each(
    Az, #{source := Source} = EachDesc, Deps, Old, DR, OR, DepsR, Changed, Views0
) when is_map(Source) ->
    %% A map-source `?each` renders to the same snapshot shape as a list-source
    %% one (`items => [ItemD]`), so it diffs through the same list machinery once
    %% the entries are rendered in map-iteration order (see diff_map/4).
    {Old0, New0} = Views0,
    {MapOps, NewSnap0, {_, LocalNew}} =
        diff_map(Az, EachDesc, Old, {Old0, #{}}),
    NewSnap = NewSnap0#{child_views => maps:keys(LocalNew)},
    Views1 = {Old0, maps:merge(New0, LocalNew)},
    {OpsRest, DRest, DepsRest, Views2} =
        diff_dynamics_v(DR, OR, DepsR, Changed, Views1),
    {MapOps ++ OpsRest, [{Az, NewSnap} | DRest], [Deps | DepsRest], Views2}.

%% Incremental stream child_views: old - deleted + rendered.
merge_stream_child_views(Source, Old, LocalNew, Old0) ->
    OldChildViews = maps:get(child_views, Old, []),
    #{items := OldItems} = Old,
    Deleted = deleted_item_children(Source#stream.pending, OldItems),
    Surviving = OldChildViews -- Deleted -- maps:keys(LocalNew),
    carry_item_children(Surviving, Old0, LocalNew).

%% When a dynamic is skipped (deps unchanged), carry its child views over
%% from OldViews to NewViews so they aren't pruned. For a stateful child, carry
%% the child AND its transitive descendants: the child's current (authoritative)
%% snapshot in Old records them as `child_views`, so a depth-2+ grandchild under
%% a skipped child is not dropped from the accumulator and then unmounted/reset.
%% We read `child_views` from Old's live entry (not the possibly-stale cached
%% snapshot passed in) so a grandchild added by the child's own event survives.
carry_skipped_view(#{view_id := VId}, {Old, New}) ->
    ToCarry =
        case Old of
            #{VId := #{snapshot := #{child_views := ChildIds}}} -> [VId | ChildIds];
            #{VId := _} -> [VId];
            #{} -> []
        end,
    {Old, maps:merge(New, maps:with(ToCarry, Old))};
carry_skipped_view(#{child_views := ChildIds}, {Old, New}) ->
    {Old, maps:merge(New, maps:with(ChildIds, Old))};
carry_skipped_view(_Old, Views) ->
    Views.

%% Extract child view IDs from deleted stream items only. The result is
%% used in list subtraction (`OldChildViews -- Deleted`), so order doesn't
%% matter -- safe to use a flat comp instead of a fold-with-prepend.
deleted_item_children(Pending, OldItems) ->
    [
        VId
     || {delete, Key} <- queue:to_list(Pending),
        {_Az, #{view_id := VId}, _Deps} <-
            case OldItems of
                #{Key := ItemD} -> ItemD;
                #{} -> []
            end
    ].

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

diff_list(Az, #{source := Items, template := Tmpl}, OldSnap, Views0) ->
    {NewItemsList, Views1} = arizona_eval:render_list_items(Items, Tmpl, Views0),
    diff_each_items(Az, Tmpl, NewItemsList, OldSnap, Views0, Views1).

%% A map-source `?each` diffs exactly like a list once its entries are rendered:
%% the snapshot is the same `items => [ItemD]` shape, keyed by position (map keys
%% carry no cross-render identity here -- use a stream for keyed diffing).
%% render_map_items yields entries in map-iteration order, matching the SSR path.
diff_map(Az, #{source := Source, template := Tmpl}, OldSnap, Views0) ->
    {NewItemsList, Views1} = arizona_eval:render_map_items(Source, Tmpl, Views0),
    diff_each_items(Az, Tmpl, NewItemsList, OldSnap, Views0, Views1).

%% Shared list/map each diff, given the already-rendered new item list. Views0 is
%% the pre-render accumulator, Views1 the post-render one (their child-view-count
%% delta is what flags a per-item child view).
diff_each_items(Az, Tmpl, NewItemsList, #{items := OldItemsList}, Views0, Views1) ->
    {_, NewLocal0} = Views0,
    {_, NewLocal1} = Views1,
    NewSnap = #{t => ?EACH, items => NewItemsList, template => Tmpl},
    %% Positional per-item patching is sound only when (a) the old slot already
    %% held a list (so positions line up with the live DOM), (b) each item is a
    %% single root element (compile-time `single_root` => DOM position N == item N
    %% between the slot's `<!--az:X-->...<!--/az-->` markers), and (c) this list
    %% rendered no per-item child view (a `?stateful`/`?stateless` child must be
    %% re-mounted by a full re-render -- the existing unsupported case, preserved;
    %% detected by the child-view accumulator growing across the render).
    %% Otherwise the wholesale marker re-render is the only correct patch.
    Patchable =
        is_list(OldItemsList) andalso
            is_single_root(Tmpl) andalso
            map_size(NewLocal1) =:= map_size(NewLocal0),
    case Patchable of
        true ->
            {SubOps, Views2} =
                diff_list_positional(Tmpl, NewItemsList, OldItemsList, 0, Views1),
            case SubOps of
                [] ->
                    {[], NewSnap, Views2};
                _ ->
                    {[[?OP_LIST_PATCH, Az, SubOps]], NewSnap, Views2}
            end;
        false ->
            diff_list_full(Az, Tmpl, NewItemsList, OldItemsList, NewSnap, Views1)
    end.

is_single_root(#{single_root := true}) -> true;
is_single_root(#{}) -> false.

%% Wholesale fallback: a non-single-root (multi-root/fragment) item, a list
%% bearing per-item child views, or a slot that previously held a non-list.
%% Re-render the whole list with one marker-aware OP_TEXT -- but only when
%% something actually changed (an unchanged same-length list emits nothing).
%% `list_changed/3` threads child views exactly as the positional walk would, so
%% a no-op diff still settles their snapshots.
diff_list_full(Az, Tmpl, NewItemsList, OldItemsList, NewSnap, Views0) when
    is_list(OldItemsList)
->
    {Changed, Views1} = list_changed(NewItemsList, OldItemsList, Views0),
    case Changed of
        false -> {[], NewSnap, Views1};
        true -> full_update(Az, Tmpl, NewItemsList, NewSnap, Views1)
    end;
diff_list_full(Az, Tmpl, NewItemsList, _OldItemsList, NewSnap, Views0) ->
    %% Old slot was not a list (first populate / type change): always render.
    full_update(Az, Tmpl, NewItemsList, NewSnap, Views0).

%% Lockstep change detection for the wholesale fallback -- mirrors the per-item
%% walk but emits no ops, just whether any item changed (an inner diff =/= [] or
%% a length difference), threading child views through.
list_changed([NewD | NR], [OldD | OR], Views0) ->
    {InnerOps, Views1} = diff_item_dynamics_v(NewD, OldD, Views0),
    {RestChanged, Views2} = list_changed(NR, OR, Views1),
    {InnerOps =/= [] orelse RestChanged, Views2};
list_changed([], [], Views) ->
    {false, Views};
list_changed(_NewTail, _OldTail, Views) ->
    {true, Views}.

%% A plain-list `?each` is marker-anchored in a content slot (no wrapper element
%% carries the slot az), so the full re-render patches the marker content via
%% `?OP_TEXT` -- never `?OP_UPDATE` (innerHTML), which clobbers the slot's static
%% siblings when resolveEl falls back to the enclosing element. Mirrors the
%% `make_op/3` plain-list each clause and the nested-template content-slot fix.
%% The fallback for a non-single-root (multi-root/fragment) item, a list bearing
%% per-item child views, or a slot that did not previously hold a list.
full_update(Az, Tmpl, NewItemsList, NewSnap, Views) ->
    HTML = arizona_render:zip_list_fp(Tmpl, NewItemsList),
    {[[?OP_TEXT, Az, HTML]], NewSnap, Views}.

%% Lockstep positional diff for a single-root plain list (the `Patchable` path in
%% `diff_list/4`). Overlap: emit an `?OP_ITEM_PATCH` sub-op only where the item's
%% inner dynamics changed -- reusing `diff_item_dynamics_v` (the same per-item
%% diff the stream path uses), so an inner scalar text op rides the client's
%% in-place text write and never churns childList. Tail delta: `?OP_INSERT` (new
%% longer -- append) or `?OP_REMOVE` (old longer). `Idx` is a 0-based position.
%% The client snapshots the item roots once before applying, so sub-op order is
%% immaterial. A middle insert/delete shows up as a cascade of content patches
%% plus a single tail insert/remove -- correct (the new list is reproduced
%% exactly) and minimal in childList churn; identity across reorders is the keyed
%% `arizona_stream`'s job, not a plain list's.
diff_list_positional(Tmpl, [NewD | NR], [OldD | OR], Idx, Views0) ->
    {InnerOps, Views1} = diff_item_dynamics_v(NewD, OldD, Views0),
    {RestOps, Views2} = diff_list_positional(Tmpl, NR, OR, Idx + 1, Views1),
    case InnerOps of
        [] -> {RestOps, Views2};
        _ -> {[[?OP_ITEM_PATCH, Idx, InnerOps] | RestOps], Views2}
    end;
diff_list_positional(Tmpl, [NewD | NR], [], Idx, Views0) ->
    HTML = arizona_render:zip_item(Tmpl, NewD),
    {RestOps, Views1} = diff_list_positional(Tmpl, NR, [], Idx + 1, Views0),
    {[[?OP_INSERT, Idx, HTML] | RestOps], Views1};
diff_list_positional(_Tmpl, [], OldTail, Idx, Views) ->
    {[[?OP_REMOVE, I] || I <- lists:seq(Idx, Idx + length(OldTail) - 1)], Views}.

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
    #stream{limit = Limit, items = ItemsMap, order = Order},
    Tmpl,
    SnapItems,
    Views0
) ->
    %% `halt` and `drop` reconcile the visible window identically here: remove the
    %% DOM items that fell out of it, and insert the newly-visible ones at their
    %% ordered position. (The halt/drop distinction is about source retention,
    %% owned by the stream module; by this point `order` already reflects it.)
    %% The positional back-fill is what lets a delete slide the next item into the
    %% freed slot (previously halt never inserted it) and a sort bring a hidden
    %% item into view at the right spot (previously appended at the end).
    VKeys = arizona_template:visible_keys(Order, Limit),
    VSet = maps:from_keys(VKeys, true),
    RemOps = [
        [?OP_REMOVE, Az, arizona_template:to_bin(K)]
     || K := _ <- SnapItems, not is_map_key(K, VSet)
    ],
    Pruned = #{K => V || K := V <- SnapItems, is_map_key(K, VSet)},
    {InsOps, Final, Views1} = snap_add_missing(Az, VKeys, Pruned, ItemsMap, Tmpl, Views0),
    {RemOps ++ InsOps, #{t => ?EACH, items => Final, order => VKeys, template => Tmpl}, Views1}.

snap_add_missing(Az, VKeys, Snaps, ItemsMap, Tmpl, Views) ->
    snap_add_missing(Az, VKeys, 0, Snaps, ItemsMap, Tmpl, Views).

%% Insert each visible key missing from the DOM at its position in the visible
%% window. Processed left-to-right, so by the time a missing key at index Idx is
%% reached the DOM already holds VKeys[0..Idx-1] (surviving items settled by the
%% preceding moves, earlier back-fills just inserted) -- so inserting at Idx lands
%% it correctly. Appending at -1 only happened to be right when the new item was
%% last in the window (a delete); a sort can place it anywhere.
snap_add_missing(_Az, [], _Idx, Snaps, _ItemsMap, _Tmpl, Views) ->
    {[], Snaps, Views};
snap_add_missing(Az, [K | Rest], Idx, Snaps, ItemsMap, Tmpl, Views0) ->
    case Snaps of
        #{K := _} ->
            snap_add_missing(Az, Rest, Idx + 1, Snaps, ItemsMap, Tmpl, Views0);
        #{} ->
            Item = maps:get(K, ItemsMap),
            {ItemD, Views1} = arizona_eval:render_stream_item(K, Item, Tmpl, Views0),
            HTML = arizona_render:zip_item(Tmpl, ItemD),
            InsOp = [?OP_INSERT, Az, arizona_template:to_bin(K), Idx, HTML],
            NewSnaps = Snaps#{K => ItemD},
            {RestOps, FinalSnaps, Views2} =
                snap_add_missing(Az, Rest, Idx + 1, NewSnaps, ItemsMap, Tmpl, Views1),
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

%% Escape markers don't take part in structural diffing -- unwrap them so the
%% value-shape clauses below match (sentinels like `remove`, nested templates,
%% each containers). The op value stays raw (`to_bin` unwraps too): a scalar `?get`
%% value is sent as a bare binary and the client renders it with a text node (so `<`
%% shows as literal text -- safe, and matching SSR), while an attribute value goes
%% through the client's `setAttribute`. The escape opt-out `?raw` is the exception: it
%% is tagged `#{~"raw" => _}` below so the client can tell trusted markup (innerHTML)
%% from a scalar that merely contains `<` (text node).
make_op(Az, {arizona_esc, New}, {arizona_esc, Old}) ->
    make_op(Az, New, Old);
make_op(Az, {arizona_esc, New}, Old) ->
    make_op(Az, New, Old);
make_op(Az, New, {arizona_esc, Old}) ->
    make_op(Az, New, Old);
make_op(Az, {attr, Attr, false}, _Old) ->
    [?OP_REM_ATTR, Az, Attr];
make_op(Az, {attr, Attr, true}, _Old) ->
    [?OP_SET_ATTR, Az, Attr, <<>>];
make_op(Az, {attr, Attr, Val}, _Old) ->
    [?OP_SET_ATTR, Az, Attr, arizona_template:to_bin(Val)];
make_op(_Az, #{view_id := VId, s := S, d := NewD}, #{view_id := _, s := S, d := OldD}) ->
    [VId, diff_child_dynamics(NewD, OldD)];
%% A nested template/snapshot in a content slot is always anchored by the
%% slot's `<!--az:X-->...<!--/az-->` comment markers, whatever the slot held
%% before (a binary, the empty string, or another template). `?OP_TEXT`
%% replaces only the marker content, leaving the slot's siblings -- and the
%% enclosing element -- intact. `?OP_UPDATE` (innerHTML) would be wrong: when
%% the slot's `Az` is the enclosing element's own `az` (a conditional
%% `?stateful` child rendered directly under the view root), it overwrites the
%% whole element and drops every sibling. That is the empty(`~""`) ->
%% descriptor transition the idiomatic
%% `case ?get(flag) of true -> ?stateful(...); false -> ~"" end` produces.
make_op(Az, #{s := _, d := _} = NewNested, _Old) ->
    [?OP_TEXT, Az, arizona_render:zip_or_fp(NewNested)];
%% A plain-list `?each` in a content slot is anchored by the slot's
%% `<!--az:X-->...<!--/az-->` comment markers, exactly like the nested-template
%% case above (every dynamic-text child is marker-wrapped in SSR -- see
%% arizona_html:text_slot_open/1). There is no wrapper element carrying
%% `az="X"`, so `?OP_TEXT` (replace marker content) is correct and `?OP_UPDATE`
%% (innerHTML) is wrong: when the each sits among static siblings, the client's
%% resolveEl finds no element for the slot az and falls back to the enclosing
%% element, where innerHTML wipes every sibling. The marker is present whether
%% or not the each is the sole child, so `?OP_TEXT` is uniformly correct (the
%% old sole-child each only "worked" with `?OP_UPDATE` by coincidence).
make_op(Az, #{t := ?EACH, items := Items, template := Tmpl}, _Old) when
    is_list(Items)
->
    [?OP_TEXT, Az, arizona_render:zip_list_fp(Tmpl, Items)];
%% Stream (`order`-keyed) each: kept on `?OP_UPDATE` for the container-level
%% full render. Streams address items by `az-key` for incremental ops and are
%% used as a list container's content -- the unkeyed plain-list marker fix does
%% not apply; an analogous mixed-siblings concern for streams is tracked
%% separately (see docs/architecture.md).
make_op(Az, #{t := ?EACH, items := Items, order := Order, template := Tmpl}, _Old) ->
    [?OP_UPDATE, Az, arizona_render:zip_stream_fp(Tmpl, Items, Order)];
make_op(Az, remove, {attr, Attr, _}) ->
    [?OP_REM_ATTR, Az, Attr];
make_op(Az, remove, _Old) ->
    [?OP_REMOVE_NODE, Az];
%% A `?raw` trusted-HTML content value: tag it `#{raw => Html}` so the wire marks it as
%% HTML (an object, not a bare string). The client unwraps and `innerHTML`s it, keeping
%% the escape opt-out across the live diff. A plain (`?get`) value below stays a raw bare
%% string -- the wire is unescaped (the client text-nodes it, which is safe and matches
%% SSR); only `?raw` needs the tag because the client cannot otherwise tell trusted markup
%% from a scalar that merely contains `<`. `?raw` is an HTML-target feature (the JS client
%% applies this tag); `?native`/`?terminal` don't HTML-escape, so `?raw` there is
%% unsupported (see `arizona_template:raw/1`).
make_op(Az, {arizona_raw, V}, _Old) ->
    [?OP_TEXT, Az, #{~"raw" => arizona_template:to_bin(V)}];
make_op(Az, New, _Old) ->
    [?OP_TEXT, Az, arizona_template:to_bin(New)].

%% Like make_op/3 but conses its op(s) onto `Tail` (a difference list -- no `++`). A
%% nested template re-rendered to the **same statics** diffs its inner dynamics instead
%% of re-rendering the whole branch with one wholesale ?OP_TEXT: each inner dynamic is
%% globally Az-addressed and marker-anchored (`<!--az:X-->...<!--/az-->`), so only the
%% changed inner slots patch (and an inner attribute change becomes a precise
%% ?OP_SET_ATTR). This is the same per-inner-dynamic diff the `view_id` child-view path
%% uses (diff_child_dynamics/2), minus the `[VId, ChildOps]` wrapper -- a plain nested
%% template is inline in the parent view, so its inner ops carry the parent view's id and
%% resolve directly. Statics that differ (a different branch, empty<->template, a
%% structure change) fall back to the single wholesale make_op/3 op, as do ?each
%% containers, attrs, scalars, and child views.
%%
%% The guard checks only New's `view_id`, not Old's: the pattern binds both to the
%% same statics S, and a child-view snapshot's statics can never equal a plain nested
%% template's -- a child-view root's statics carry an `az-view` boundary attribute and
%% an az prefix from the child's own fingerprint, neither of which a plain branch has.
%% So a same-S Old is necessarily a plain inline template too; a child New is excluded
%% here and handled by make_op/3's child-view clause (which requires `view_id` on both).
make_ops(_Az, #{s := S, d := NewD} = New, #{s := S, d := OldD}, Tail) when
    not is_map_key(view_id, New)
->
    diff_dynamics(NewD, OldD, Tail);
make_ops(Az, New, Old, Tail) ->
    [make_op(Az, New, Old) | Tail].

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
                        diff_list(Az, EachDesc, Old, Views0);
                    _ when is_map(Src) ->
                        diff_map(Az, EachDesc, Old, Views0)
                end,
            {RestOps, Views2} = diff_item_dynamics_v(NR, OR, Views1),
            {EachOps ++ RestOps, Views2};
        _ ->
            {RestOps, Views1} = diff_item_dynamics_v(NR, OR, Views0),
            {make_ops(Az, New, Old, RestOps), Views1}
    end.

diff_child_dynamics(NewD, OldD) ->
    diff_child_dynamics(NewD, OldD, []).

diff_child_dynamics([], [], Tail) ->
    Tail;
diff_child_dynamics([{Az, _New} | NR], [{Az, #{diff := false}} | OR], Tail) ->
    diff_child_dynamics(NR, OR, Tail);
diff_child_dynamics([{Az, Same} | NR], [{Az, Same} | OR], Tail) ->
    diff_child_dynamics(NR, OR, Tail);
diff_child_dynamics([{Az, New} | NR], [{Az, Old} | OR], Tail) ->
    make_ops(Az, New, Old, diff_child_dynamics(NR, OR, Tail)).
