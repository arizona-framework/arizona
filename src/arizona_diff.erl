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
?OP_TEXT, ?OP_SET_ATTR, ?OP_REM_ATTR, ?OP_REMOVE_NODE, ?OP_INSERT,
?OP_REMOVE, ?OP_ITEM_PATCH, ?OP_REPLACE, ?OP_MOVE, ?OP_LIST_PATCH
```

## `?each` list vs. stream diffing

A `?each` over a **plain list** is unkeyed: its items render as bare HTML
with no `az-key`, so there is no per-item DOM address to patch. Any change
(item content or list length) therefore re-renders the whole list with a
single `?OP_TEXT` patching the slot's `<!--az:X-->...<!--/az-->` marker
content; an unchanged list emits nothing. (The marker-aware `?OP_TEXT` is
what every container full render uses: a plain-list each is anchored in a
content slot, so an innerHTML write on the fallback enclosing element would
clobber the slot's static siblings.)

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
Az dynamic, so it is never re-evaluated and never produces an op. The
per-item and child-view walkers (`diff_item_dynamics_v/3`,
`diff_child_dynamics/3`) skip it the same way; the one place such a change
IS delivered is a plain-list `?each`, whose wholesale `?OP_TEXT` re-render
ships raw-text content as plain HTML (so a markerless item slot routes the
list to that fallback instead of a per-item patch).
""".

-include("arizona.hrl").

%% How much smaller a wholesale container re-render must be before it is worth
%% giving up per-item ops. Those ops preserve the DOM -- the container is never torn
%% down, so focus, scroll position and `?local` values inside it survive -- so
%% wholesale has to be clearly smaller (the ratio) AND save something worth having
%% (the floor). A short container can be twice as cheap to re-render while the saving
%% is a hundred-odd bytes, which is not a trade worth losing a selection over.
-define(RE_RENDER_BIAS_NUM, 3).
-define(RE_RENDER_BIAS_DEN, 2).
-define(RE_RENDER_MIN_SAVING, 512).

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

%% Public diff entry points, exercised by the test suites but no longer
%% called from production (arizona_live uses the dep-gated diff/4).
-ignore_xref([diff/2]).
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
    {Ops, arizona_template:maybe_propagate(Tmpl, Snap0)}.

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
    {Ops, arizona_template:maybe_propagate(Tmpl, Snap0), NewViews}.

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
    {Ops, arizona_template:maybe_propagate(Tmpl, Snap0), NewViews}.

%% `view_id` lives on child-view snapshots (set by `make_child_snap`) and is
%% read by `make_op/3` to detect child diffs. The rebuilt snapshot must carry
%% it forward so subsequent diffs keep matching the child-view clause.
preserve_view_id(#{view_id := VId}, Snap) -> Snap#{view_id => VId};
preserve_view_id(#{}, Snap) -> Snap.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

diff_dynamics(NewEvals, OldEvals) ->
    {Ops, _Views} = diff_dynamics(NewEvals, OldEvals, [], {#{}, #{}}),
    Ops.

%% Tail-accumulator (difference-list) form: each op is consed straight onto `Tail`,
%% so there is no `++` copying. This matters for the fine-grained nested-template path
%% (`make_ops/4` recurses back here), where a `++` would re-copy the inner ops at every
%% nesting level. `Tail` is the ops that follow these dynamics; order is preserved.
diff_dynamics([], [], Tail, Views) ->
    {Tail, Views};
diff_dynamics([{undefined, _} | NR], [{undefined, _} | OR], Tail, Views) ->
    %% Markerless render-once slot (raw-text element content, or az-nodiff): no
    %% comment marker to target, so never emit an op -- carry it forward as-is.
    diff_dynamics(NR, OR, Tail, Views);
diff_dynamics([{Az, _} | NR], [{Az, #{diff := false}} | OR], Tail, Views) ->
    diff_dynamics(NR, OR, Tail, Views);
diff_dynamics([{Az, Same} | NR], [{Az, Same} | OR], Tail, Views) ->
    diff_dynamics(NR, OR, Tail, Views);
%% A stream each nested inside another template (what a `?stateless` child
%% renders to) reaches here rather than `diff_changed_dynamic`, which is where
%% the incremental path is wired for a template's own top-level dynamics. Route
%% it the same way, or the generic clause below re-renders the whole container
%% through `?OP_TEXT` -- O(N) on the wire for an O(1) change, with no symptom but
%% payload size. Only a STREAM snapshot carries `source` (see
%% `diff_item_dynamics_v/3`, which reconstructs the descriptor the same way), so
%% list- and map-source eaches fall through untouched.
diff_dynamics(
    [{Az, #{t := ?EACH, source := #stream{} = Src, template := Tmpl} = New} | NR],
    [{Az, #{t := ?EACH} = Old} | OR],
    Tail,
    Views0
) ->
    {RestOps, Views1} = diff_dynamics(NR, OR, Tail, Views0),
    case stream_drainable(Src, Old) of
        true ->
            {Old0, New0} = Views1,
            {StreamOps, _NewSnap, {_, LocalNew}} =
                diff_stream(Az, #{source => Src, template => Tmpl}, Old, {Old0, #{}}),
            LocalNew1 = merge_stream_child_views(Src, Old, LocalNew, Old0),
            {StreamOps ++ RestOps, {Old0, maps:merge(New0, LocalNew1)}};
        false ->
            stream_relist(Az, Src, Tmpl, New, Old, RestOps, Views1)
    end;
diff_dynamics([{Az, New} | NR], [{Az, Old} | OR], Tail, Views0) ->
    {RestOps, Views1} = diff_dynamics(NR, OR, Tail, Views0),
    maybe_make_ops(Az, New, Old, RestOps, Views1).

%% A slot whose value changed as a TERM has not necessarily changed on screen, and
%% `?OP_TEXT`/`?OP_SET_ATTR` for a value the DOM already holds is not free: an
%% attribute write is a style-recalc trigger, and `applySetAttrOp` additionally
%% assigns the live property for `value`, which can move a caret or drop a selection
%% in an input the user is working in. `collapses_to_same_bytes/2` already answers
%% this for the container fallback; the same question belongs here, on every slot.
%%
%% Equality is matched first so the byte comparison is only ever asked about values
%% that really do differ, which is the precondition it is named for.
maybe_make_ops(_Az, Same, Same, Tail, Views) ->
    {Tail, Views};
maybe_make_ops(Az, New, Old, Tail, Views) ->
    case collapses_to_same_bytes(New, Old) of
        true -> {Tail, Views};
        false -> make_ops(Az, New, Old, Tail, Views)
    end.

%% The log could not explain the change, but the two key orders can: this is
%% semantically a reset to the stream's current state, so reconcile it as one.
%% `stream_reset/8` needs no pending log -- it removes dropped keys, patches kept
%% ones only where their dynamics differ, inserts new ones, and emits the minimal
%% LIS moves. Passing `#{}` for the previous item VALUES costs the dep-skip that a
%% real reset gets (each kept item's dynamics are re-evaluated rather than
%% skipped), but the WIRE stays proportional to what actually changed, which is
%% the cost that was hurting. Falls back to the wholesale render only when the old
%% snapshot is not stream-shaped, where there is no order to reconcile against.
stream_relist(
    Az,
    _Src,
    Tmpl,
    #{items := NewItems, order := NewOrder},
    #{items := OldItems, order := OldOrder},
    Tail,
    Views
) ->
    %% `New` is the freshly evaluated state: the enclosing walk already rendered
    %% every item to build it. Diff against `Old` directly rather than handing the
    %% source to `stream_reset/8`, which would render the whole list a SECOND time
    %% -- twice the work, and it re-runs each item child's `mount/1` /
    %% `handle_update/3`, so a child that subscribes or arms a timer does it twice
    %% per diff.
    NewSet = maps:from_keys(NewOrder, true),
    RemOps = [
        [?OP_REMOVE, Az, arizona_template:to_bin(K)]
     || K <- OldOrder, not is_map_key(K, NewSet)
    ],
    {ItemOps, Views1} = relist_items(Az, NewOrder, NewItems, OldItems, Tmpl, Views),
    Kept = maps:with(NewOrder, OldItems),
    MoveOps = compute_reorder_ops(Az, OldOrder, NewOrder, Kept, NewItems),
    {RemOps ++ ItemOps ++ MoveOps ++ Tail, Views1};
stream_relist(Az, _Src, _Tmpl, New, Old, Tail, Views) ->
    make_ops(Az, New, Old, Tail, Views).

%% Per-key walk for `stream_relist/7`: a key the client already holds is patched
%% only where its dynamics differ, one it lacks is inserted at the tail. Mirrors
%% `smart_reset_items/8`'s op shapes exactly, minus the re-render -- the item
%% dynamics are taken from the already-evaluated `New`.
relist_items(_Az, [], _NewItems, _OldItems, _Tmpl, Views) ->
    {[], Views};
relist_items(Az, [K | Rest], NewItems, OldItems, Tmpl, Views0) ->
    NewD = maps:get(K, NewItems),
    {Ops, Views1} =
        case OldItems of
            #{K := OldD} ->
                {InnerOps, _Markerless, ViewsA} = diff_item_dynamics_v(NewD, OldD, Views0),
                case InnerOps of
                    [] ->
                        {[], ViewsA};
                    _ ->
                        {[[?OP_ITEM_PATCH, Az, arizona_template:to_bin(K), InnerOps]], ViewsA}
                end;
            #{} ->
                HTML = arizona_render:zip_item(Tmpl, NewD),
                {[[?OP_INSERT, Az, arizona_template:to_bin(K), -1, HTML]], Views0}
        end,
    {RestOps, Views2} = relist_items(Az, Rest, NewItems, OldItems, Tmpl, Views1),
    {Ops ++ RestOps, Views2}.

%% Can this stream's change be expressed by draining its pending log?
%%
%% `diff_stream/4` derives its ops purely by draining that log, and the log is
%% cleared before a `?stateful` child renders (arizona_eval, to stop a prop-fed
%% child accumulating one entry per root update). So an empty log carries NO
%% information about whether the container changed: it means either "nothing
%% happened" or "the log was wiped and anything may have happened". Draining it
%% yields no ops either way, which loses a real change silently.
%%
%% Comparing key ORDERS is not enough to tell those apart -- an item whose content
%% changed in place leaves the order identical, so treating an unchanged order as
%% "no ops needed" drops that change and the client never self-heals. Only a
%% non-empty log is evidence the drain can account for the difference; an empty
%% one goes to `stream_relist/7`, which reconciles against the old snapshot and
%% emits an `?OP_ITEM_PATCH` for exactly the items whose dynamics differ.
stream_drainable(#stream{} = Src, Old) ->
    case arizona_stream:undrained_ops(Src, maps:get(drained, Old, none)) of
        [_ | _] -> true;
        [] -> false
    end.

diff_dynamics_v([], [], [], _Changed, Views) ->
    {[], [], [], Views};
diff_dynamics_v(
    [_Def | DR],
    [{undefined, _} = OldD | OR],
    [ODeps | DepsR],
    Changed,
    Views0
) ->
    %% Markerless render-once slot (raw-text element content, or az-nodiff): no
    %% comment marker to target, so skip it -- never re-evaluate, never emit an op.
    skip_dynamic(OldD, ODeps, DR, OR, DepsR, Changed, Views0);
diff_dynamics_v(
    [_Def | DR],
    [{_Az, #{diff := false}} = OldD | OR],
    [ODeps | DepsR],
    Changed,
    Views0
) ->
    skip_dynamic(OldD, ODeps, DR, OR, DepsR, Changed, Views0);
diff_dynamics_v([Def | DR], [{Az, Old} = OldD | OR], [ODeps | DepsR], Changed, Views0) ->
    case deps_changed(ODeps, Changed) of
        false ->
            skip_dynamic(OldD, ODeps, DR, OR, DepsR, Changed, Views0);
        true ->
            diff_changed_dynamic(Def, Az, Old, DR, OR, DepsR, Changed, Views0)
    end.

%% Skip a dynamic whose deps haven't changed: carry its child views to the
%% new accumulator and cons the original `{Az, Old}` tuple onto the new
%% snapshot -- shared with the old one, not rebuilt.
skip_dynamic({_Az, Old} = OldD, ODeps, DR, OR, DepsR, Changed, Views0) ->
    Views1 = carry_skipped_view(Old, Views0),
    {OpsRest, DRest, DepsRest, Views2} =
        diff_dynamics_v(DR, OR, DepsR, Changed, Views1),
    {OpsRest, [OldD | DRest], [ODeps | DepsRest], Views2}.

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
    {OpsFinal, ViewsFinal} = maybe_make_ops(Az, New, Old, OpsRest, Views2),
    {OpsFinal, [{Az, New} | DRest], [NewDeps | DepsRest], ViewsFinal}.

diff_each(
    Az, #{source := #stream{} = Source} = EachDesc, Deps, Old, DR, OR, DepsR, Changed, Views0
) ->
    {Old0, New0} = Views0,
    {StreamOps, NewSnap0, {_, LocalNew}} =
        diff_stream(Az, EachDesc, Old, {Old0, #{}}),
    LocalNew1 = merge_stream_child_views(Source, Old, LocalNew, Old0),
    NewSnap = NewSnap0#{child_views => arizona_eval:child_view_set(LocalNew1)},
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
    NewSnap = NewSnap0#{child_views => arizona_eval:child_view_set(LocalNew)},
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
    NewSnap = NewSnap0#{child_views => arizona_eval:child_view_set(LocalNew)},
    Views1 = {Old0, maps:merge(New0, LocalNew)},
    {OpsRest, DRest, DepsRest, Views2} =
        diff_dynamics_v(DR, OR, DepsR, Changed, Views1),
    {MapOps ++ OpsRest, [{Az, NewSnap} | DRest], [Deps | DepsRest], Views2}.

%% Incremental stream child_views: old - deleted + rendered.
merge_stream_child_views(Source, Old, LocalNew, Old0) ->
    OldChildViews = maps:get(child_views, Old, #{}),
    #{items := OldItems} = Old,
    Deleted = deleted_item_children(arizona_stream:pending_ops(Source), OldItems),
    Surviving = maps:without(Deleted, OldChildViews),
    carry_item_children(Surviving, Old0, LocalNew).

%% When a dynamic is skipped (deps unchanged), carry its child views over
%% from OldViews to NewViews so they aren't pruned.
%%
%% Both the stateful-child form and the container form (a nested template or an
%% `?each`, which record the views rendered inside them as `child_views`) resolve
%% what to carry through `live_subtree/2` rather than trusting the set on the
%% snapshot they were handed. `child_views` on a SNAPSHOT is only accurate as of
%% that snapshot's last evaluation, and a view can come into existence with no
%% enclosing container re-evaluated at all: a grandchild first rendered by a
%% CHILD's own event updates `views` and the child's own snapshot, and nothing
%% above it. Carrying a container's recorded set verbatim therefore dropped that
%% grandchild from the accumulator, and the next UNRELATED root diff then treated
%% it as removed -- unmounting a live view (running its `unmount/1` side effects,
%% releasing pubsub subscriptions and resources) while its DOM was still on the
%% page, after which every event addressed to it was silently swallowed.
carry_skipped_view(#{view_id := VId}, {Old, New}) ->
    {Old, maps:merge(New, live_subtree(#{VId => true}, Old))};
carry_skipped_view(#{child_views := ChildIds}, {Old, New}) ->
    {Old, maps:merge(New, live_subtree(ChildIds, Old))};
carry_skipped_view(_Old, Views) ->
    Views.

%% The live entries of `Old` making up the subtrees rooted at `Ids`, following
%% each entry's own recorded descendants transitively. Every view's snapshot in
%% `Old` is authoritative for ITS subtree at the moment it last rendered, so
%% chaining through them reaches a view created at any depth after the container
%% above it was last evaluated -- one level of expansion is not enough, since a
%% great-grandchild added by a grandchild's own event is recorded only on that
%% grandchild. Ids absent from `Old` (already unmounted) simply drop out.
%%
%% Runs during a root diff, which already walks the tree; nothing here touches
%% the child-event path.
%%
%% `Ids` naming every live view is the shape that matters most -- one container
%% holding the page's views -- and there the answer is `Old` itself: the seed
%% `maps:with(Ids, Old)` is a subset of `Old`, so a seed the size of `Old` IS
%% `Old`, and the expansion below can add nothing to a set that already holds
%% everything. Deciding that by MEMBERSHIP rather than by building the seed and
%% comparing sizes is what the `child_views` set buys here: probing a live id
%% against `Ids` is O(1), so the whole test is one pass over `Old` with no
%% allocation, where building the seed first cost an N-entry map on every
%% dep-skipped container of every root diff -- the single largest line item in a
%% root diff that follows a child change. The size guard runs first because it
%% settles the common negative (a container holding SOME of the page's views)
%% without touching a key.
%%
%% Off that path the bulk `maps:with/2` seed is deliberate: it is the single
%% operation this used to be, and the expansion then only follows entries naming
%% a descendant the seed did not already cover. In the ordinary case -- a
%% container listing its children, none of which has grown since -- that is one
%% lookup and one empty scan per id, with no per-id insert and no list building.
%% Chaining every id unconditionally instead cost a single-key insert plus a `++`
%% per entry on EVERY dep-skipped container, whether or not anything had changed.
live_subtree(Ids, Old) ->
    case names_every_live_view(Ids, Old) of
        true ->
            Old;
        false ->
            IdList = maps:keys(Ids),
            expand_subtree(IdList, Old, maps:with(IdList, Old))
    end.

%% `Ids` can name a view that is no longer mounted, so a count alone does not
%% settle it -- but a count SHORTER than `Old` rules it out for free.
names_every_live_view(Ids, Old) when map_size(Ids) < map_size(Old) ->
    false;
names_every_live_view(Ids, Old) ->
    all_named(maps:next(maps:iterator(Old)), Ids).

all_named(none, _Ids) ->
    true;
all_named({Id, _View, Iter}, Ids) ->
    case Ids of
        #{Id := _} -> all_named(maps:next(Iter), Ids);
        #{} -> false
    end.

expand_subtree([], _Old, Acc) ->
    Acc;
expand_subtree([Id | Rest], Old, Acc) ->
    case Acc of
        %% A leaf -- most of the ids on a list page -- records no descendants, so
        %% it can add nothing. Answering that on the guard skips setting up an
        %% iterator over an empty set once per id.
        #{Id := #{snapshot := #{child_views := ChildIds}}} when map_size(ChildIds) =:= 0 ->
            expand_subtree(Rest, Old, Acc);
        #{Id := #{snapshot := #{child_views := ChildIds}}} ->
            expand_uncovered([C || C := _ <- ChildIds, not is_map_key(C, Acc)], Rest, Old, Acc);
        #{} ->
            expand_subtree(Rest, Old, Acc)
    end.

expand_uncovered([], Rest, Old, Acc) ->
    expand_subtree(Rest, Old, Acc);
expand_uncovered(Missing, Rest, Old, Acc) ->
    expand_subtree(Missing ++ Rest, Old, maps:merge(Acc, maps:with(Missing, Old))).

%% Extract child view IDs from deleted stream items only. The result is
%% only ever `maps:without/2`'s key list (`OldChildViews` minus these), so
%% neither order nor duplicates matter -- safe to use a flat comp instead of
%% a fold-with-prepend.
deleted_item_children(PendingOps, OldItems) ->
    [
        VId
     || {delete, Key} <- PendingOps,
        {_Az, #{view_id := VId}, _Deps} <-
            case OldItems of
                #{Key := ItemD} -> ItemD;
                #{} -> []
            end
    ].

%% Copy child views from OldViews to NewViews for children not already present.
%% `maps:intersect/2` takes its values from the SECOND map, so this is
%% `maps:with(maps:keys(ChildViewIds), Old)` without materializing the key list
%% -- and it iterates whichever map is smaller. `New` is merged on top so a
%% freshly rendered child always beats the carried-over copy of itself.
carry_item_children(ChildViewIds, Old, New) ->
    maps:merge(maps:intersect(ChildViewIds, Old), New).

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
            %% The drain threads `{Source, Vis}` as one term (`SV`): both are
            %% constant for the whole drain, and bundling them keeps each stream
            %% helper's arity in bounds. The window (`Vis`) is computed once here
            %% rather than re-materialised on every insert/update op.
            SV = {Source, visible_set(Source)},
            %% Only the ops queued since this slot's previous drain. A stream the
            %% live process cannot clear (nested inside another value) keeps its
            %% whole history in `pending`, and replaying it re-emitted every
            %% historical intermediate patch -- see arizona_stream's moduledoc.
            Ops = arizona_stream:undrained_ops(Source, maps:get(drained, OldSnap, none)),
            {StreamOps, NewSnap, Views1} = diff_stream_pending(
                Az,
                drainable_ops(Ops),
                SV,
                Tmpl,
                OldItems,
                OldOrder,
                Views0
            ),
            case stream_outgrows_re_render(StreamOps, Tmpl, OldSnap, Source) of
                false -> {StreamOps, NewSnap, Views1};
                true -> stream_full_render(Az, Source, Tmpl, Views0)
            end;
        #{} ->
            %% The slot did not previously hold a stream, so there is no order to
            %% diff against.
            stream_full_render(Az, Source, Tmpl, Views0)
    end.

%% Container full render. Marker-aware `?OP_TEXT` -- see `make_op/3`'s stream
%% `?EACH` clause.
stream_full_render(Az, Source, Tmpl, Views0) ->
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
        template => Tmpl,
        %% A full render reflects the whole post-op state, so every op queued so
        %% far counts as consumed.
        drained => arizona_stream:drain_mark(Source)
    },
    HTML = arizona_render:zip_stream_fp(Tmpl, ItemSnaps, VKeys),
    {[[?OP_TEXT, Az, HTML]], NewSnap, Views1}.

%% The same question `outgrows_re_render/3` asks for a list, with two differences.
%%
%% A stream carrying CHILD VIEWS is never collapsed. The incremental path keeps their
%% bookkeeping (`merge_stream_child_views/4`: old minus deleted plus rendered); the
%% full render has none, so re-rendering would re-mount every child and reset its
%% state. Bytes are not worth that.
%%
%% And the drain does not visit every item, so unlike the list walk there is nothing
%% free to accumulate. The wholesale side is estimated from the OLD items' average
%% size against the NEW visible count, which is close whenever items are alike -- and
%% the bias and the absolute floor absorb the error either way.
stream_outgrows_re_render(Ops, Tmpl, OldSnap, Source) ->
    maps:get(child_views, OldSnap, #{}) =:= #{} andalso
        stream_outgrows_by_bytes(Ops, Tmpl, maps:get(items, OldSnap), Source).

stream_outgrows_by_bytes(Ops, Tmpl, OldItems, Source) when map_size(OldItems) > 0 ->
    NewCount = visible_count(Source),
    AvgItem = sum_item_value_bytes(maps:next(maps:iterator(OldItems)), 0) div map_size(OldItems),
    Whole = re_render_bytes(Tmpl, AvgItem * NewCount, NewCount),
    Positional = wire_bytes(Ops),
    Positional - Whole > ?RE_RENDER_MIN_SAVING andalso
        Positional * ?RE_RENDER_BIAS_DEN > Whole * ?RE_RENDER_BIAS_NUM;
stream_outgrows_by_bytes(_Ops, _Tmpl, _OldItems, _Source) ->
    false.

%% How many keys the visible window holds, from the stream's cached size. The window
%% is the first `Limit` keys of `order`, so its size is settled by two integers --
%% `visible_keys/2` would flatten the append buffer and copy the whole list, only for
%% it to be counted and dropped.
visible_count(#stream{limit = infinity, size = Size}) ->
    Size;
visible_count(#stream{limit = Limit, size = Size}) ->
    min(Size, Limit).

%% Total value bytes over an item map, walked through its iterator: `maps:values/1`
%% would materialise every item snapshot into a list just to sum over it.
sum_item_value_bytes(none, Acc) ->
    Acc;
sum_item_value_bytes({_Key, ItemD, Iter}, Acc) ->
    sum_item_value_bytes(maps:next(Iter), Acc + item_value_bytes(ItemD)).

%% An empty drain carries no information about whether the container changed -- it
%% means either "nothing happened" or "the log was wiped and anything may have
%% happened". Draining nothing silently answers "no change" to both. `diff_dynamics/4`
%% guards against that by asking `stream_drainable/2` first and reconciling through
%% `stream_relist/7`, but the dep-aware walk (`diff_each/9`) and the
%% stream-inside-a-stream-item walk (`diff_item_dynamics_v/3`) call here straight, so
%% on those paths a wiped log dropped the change: replacing a populated stream binding
%% with a cleared one emitted ZERO ops where `diff/3` emitted the removals. Reconcile
%% instead -- a reset against the CURRENT source is the answer the log cannot give, and
%% it costs nothing when nothing did change, since every kept item's dynamics compare
%% equal and emit no op.
drainable_ops([]) -> [{reset, #{}}];
drainable_ops(Ops) -> Ops.

diff_stream_pending(Az, [], {Source, _Vis}, Tmpl, SnapAcc, OldOrder, Views0) ->
    apply_limit(Az, Source, Tmpl, SnapAcc, OldOrder, Views0);
diff_stream_pending(Az, [Op | Rest], SV, Tmpl, SnapAcc, OldOrder, Views0) ->
    diff_stream_op(Az, Op, Rest, SV, Tmpl, SnapAcc, OldOrder, Views0).

diff_stream_op(Az, {insert, Key, Item, Pos}, Rest, SV, Tmpl, SnapAcc, OldOrder, Views) ->
    stream_insert(Az, Key, Item, Pos, Rest, SV, Tmpl, SnapAcc, OldOrder, Views);
diff_stream_op(Az, {delete, Key}, Rest, SV, Tmpl, SnapAcc, OldOrder, Views) ->
    stream_delete(Az, Key, Rest, SV, Tmpl, SnapAcc, OldOrder, Views);
diff_stream_op(Az, {update, Key, NewItem, Changed}, Rest, SV, Tmpl, SnapAcc, OldOrder, Views) ->
    stream_update(Az, Key, NewItem, Changed, Rest, SV, Tmpl, SnapAcc, OldOrder, Views);
diff_stream_op(Az, {move, Key, AfterKey}, Rest, SV, Tmpl, SnapAcc, OldOrder, Views) ->
    stream_move(Az, Key, AfterKey, Rest, SV, Tmpl, SnapAcc, OldOrder, Views);
diff_stream_op(Az, reorder, Rest, SV, Tmpl, SnapAcc, OldOrder, Views) ->
    stream_reorder(Az, Rest, SV, Tmpl, SnapAcc, OldOrder, Views);
diff_stream_op(Az, {reset, OldItems}, Rest, SV, Tmpl, SnapAcc, OldOrder, Views) ->
    stream_reset(Az, OldItems, Rest, SV, Tmpl, SnapAcc, OldOrder, Views).

%% The visibility window as a set, computed once per drain from the (constant)
%% stream record. An unlimited stream keeps every key, so it needs no set. The
%% stream record is already the post-op state, so this is the *final* window --
%% skipping an op for a key outside it is safe in both directions, since
%% `apply_limit/6` back-fills any visible key missing from the snapshot at its
%% ordered position, so a key that slides into view later still lands.
visible_set(#stream{limit = infinity}) ->
    all;
visible_set(#stream{order = Order, limit = Limit}) ->
    maps:from_keys(arizona_template:visible_keys(Order, Limit), true).

is_visible(_Key, all) ->
    true;
is_visible(Key, Vis) ->
    is_map_key(Key, Vis).

%% An insert past the limit used to render the item, ship its HTML, and then have
%% `apply_limit/6` remove it in the same batch -- payload the client mounts and
%% immediately destroys, firing a phantom `mounted()`/`destroyed()` pair.
%%
%% The `SnapAcc` guard makes a REPLAYED insert a no-op: the stream API refuses
%% duplicate-key inserts, so a queued insert whose key the client DOM already
%% holds can only be the re-drain of a pending queue that was never cleared --
%% a `#stream{}` nested inside another value (a field of a parent stream's
%% item) is out of `clear_stream_pending`'s reach, so every re-eval of the
%% enclosing slot re-drains it. Skipping the replay (delete/update/move/
%% reorder/reset replays are already no-ops against the post-drain SnapAcc)
%% makes the whole re-drain idempotent instead of emitting duplicate
%% OP_INSERTs (duplicate DOM nodes under one az-key).
stream_insert(Az, Key, Item, Pos, Rest, SV, Tmpl, SnapAcc, OldOrder, Views0) ->
    {_Source, Vis} = SV,
    case is_visible(Key, Vis) andalso not is_map_key(Key, SnapAcc) of
        true ->
            {ItemD, Views1} = arizona_eval:render_stream_item(Key, Item, Tmpl, Views0),
            HTML = arizona_render:zip_item(Tmpl, ItemD),
            InsOp = [?OP_INSERT, Az, arizona_template:to_bin(Key), Pos, HTML],
            NewSnapAcc = SnapAcc#{Key => ItemD},
            {RestOps, FinalSnap, Views2} =
                diff_stream_pending(Az, Rest, SV, Tmpl, NewSnapAcc, OldOrder, Views1),
            {[InsOp | RestOps], FinalSnap, Views2};
        false ->
            diff_stream_pending(Az, Rest, SV, Tmpl, SnapAcc, OldOrder, Views0)
    end.

%% `SnapAcc` tracks what the client's DOM holds as the ops are applied, so a key
%% absent from it has no node to remove -- the case for a key the limit kept out
%% of the window, whether this batch skipped its insert or an earlier cycle pruned
%% it. Mirrors the guard `stream_move/9` already applies.
stream_delete(Az, Key, Rest, SV, Tmpl, SnapAcc, OldOrder, Views0) ->
    case SnapAcc of
        #{Key := _} ->
            DelOp = [?OP_REMOVE, Az, arizona_template:to_bin(Key)],
            NewSnapAcc = maps:remove(Key, SnapAcc),
            {RestOps, FinalSnap, Views1} =
                diff_stream_pending(Az, Rest, SV, Tmpl, NewSnapAcc, OldOrder, Views0),
            {[DelOp | RestOps], FinalSnap, Views1};
        #{} ->
            diff_stream_pending(Az, Rest, SV, Tmpl, SnapAcc, OldOrder, Views0)
    end.

stream_update(Az, Key, NewItem, Changed, Rest, SV, Tmpl, SnapAcc, OldOrder, Views0) ->
    case SnapAcc of
        #{Key := OldD} ->
            {NewD, Views1} =
                arizona_eval:render_stream_item_skipping(
                    Key, NewItem, OldD, Changed, Tmpl, Views0
                ),
            stream_update_existing(
                Az, Key, NewD, OldD, Rest, SV, Tmpl, SnapAcc, OldOrder, Views1
            );
        #{} ->
            stream_update_missing(Az, Key, NewItem, Rest, SV, Tmpl, SnapAcc, OldOrder, Views0)
    end.

stream_update_existing(Az, Key, NewD, OldD, Rest, SV, Tmpl, SnapAcc, OldOrder, Views0) ->
    %% A changed markerless slot is ignored: a stream item's raw-text content is
    %% render-once (no marker, no per-item re-render op that preserves siblings).
    {InnerOps, _Markerless, Views1} = diff_item_dynamics_v(NewD, OldD, Views0),
    case InnerOps of
        [] ->
            diff_stream_pending(Az, Rest, SV, Tmpl, SnapAcc, OldOrder, Views1);
        _ ->
            PatchOp = [?OP_ITEM_PATCH, Az, arizona_template:to_bin(Key), InnerOps],
            NewSnapAcc = SnapAcc#{Key => NewD},
            {RestOps, FinalSnap, Views2} =
                diff_stream_pending(Az, Rest, SV, Tmpl, NewSnapAcc, OldOrder, Views1),
            {[PatchOp | RestOps], FinalSnap, Views2}
    end.

%% An update whose key the client's DOM (`SnapAcc`) doesn't hold has nothing to
%% patch. On an UNLIMITED stream that key can only be this frame's upsert of a
%% brand-new key, which appended to `order` -- so render it as a tail insert
%% (`-1`), the one rendering path an infinity stream has (its `apply_limit/6`
%% clause does no back-fill). On a LIMITED stream the key is (or was)
%% limit-hidden: leave it entirely to `apply_limit/6`, whose left-to-right
%% back-fill renders the item's CURRENT source state at its exact window index,
%% or keeps it out when it is not in the final window. Inserting here at -1 put
%% a newly-visible mid-window key at the tail and, being in `SnapAcc`, made the
%% back-fill skip it -- permanently diverging the client's order from the
%% server's (e.g. a hidden key moved to the front and updated in one frame).
stream_update_missing(Az, Key, NewItem, Rest, SV, Tmpl, SnapAcc, OldOrder, Views0) ->
    case SV of
        {#stream{limit = infinity}, _Vis} ->
            {NewD, Views1} = arizona_eval:render_stream_item(Key, NewItem, Tmpl, Views0),
            HTML = arizona_render:zip_item(Tmpl, NewD),
            InsOp = [?OP_INSERT, Az, arizona_template:to_bin(Key), -1, HTML],
            NewSnapAcc = SnapAcc#{Key => NewD},
            {RestOps, FinalSnap, Views2} =
                diff_stream_pending(Az, Rest, SV, Tmpl, NewSnapAcc, OldOrder, Views1),
            {[InsOp | RestOps], FinalSnap, Views2};
        {#stream{}, _Vis} ->
            diff_stream_pending(Az, Rest, SV, Tmpl, SnapAcc, OldOrder, Views0)
    end.

stream_move(Az, Key, AfterKey, Rest, SV, Tmpl, SnapAcc, OldOrder, Views0) ->
    case SnapAcc of
        #{Key := _} ->
            case after_ref_in_dom(AfterKey, SnapAcc) of
                true ->
                    Ref = move_after_ref(AfterKey),
                    MoveOp = [?OP_MOVE, Az, arizona_template:to_bin(Key), Ref],
                    {RestOps, FinalSnap, Views1} =
                        diff_stream_pending(Az, Rest, SV, Tmpl, SnapAcc, OldOrder, Views0),
                    {[MoveOp | RestOps], FinalSnap, Views1};
                false ->
                    %% The after-reference is a limit-hidden key the client DOM
                    %% doesn't hold; a MOVE with a missing ref makes the client's
                    %% moveItemEl fall back to appending, scrambling surviving
                    %% order. Remove the item instead and let `apply_limit/6`'s
                    %% left-to-right back-fill re-insert it at its exact final
                    %% window index (or keep it out when it moved past the
                    %% window). Only reachable on a limited stream -- an
                    %% unlimited stream's SnapAcc holds every live key.
                    RemOp = [?OP_REMOVE, Az, arizona_template:to_bin(Key)],
                    NewSnapAcc = maps:remove(Key, SnapAcc),
                    {RestOps, FinalSnap, Views1} =
                        diff_stream_pending(Az, Rest, SV, Tmpl, NewSnapAcc, OldOrder, Views0),
                    {[RemOp | RestOps], FinalSnap, Views1}
            end;
        #{} ->
            diff_stream_pending(Az, Rest, SV, Tmpl, SnapAcc, OldOrder, Views0)
    end.

after_ref_in_dom(null, _SnapAcc) -> true;
after_ref_in_dom(AfterKey, SnapAcc) -> is_map_key(AfterKey, SnapAcc).

move_after_ref(null) -> null;
move_after_ref(AfterKey) -> arizona_template:to_bin(AfterKey).

stream_reorder(Az, Rest, SV, Tmpl, SnapAcc, OldOrder, Views0) ->
    {Source, _Vis} = SV,
    VKeys = arizona_template:visible_keys(Source#stream.order, Source#stream.limit),
    MoveOps = compute_reorder_ops(Az, OldOrder, VKeys, SnapAcc, SnapAcc),
    {RestOps, FinalSnap, Views1} =
        diff_stream_pending(Az, Rest, SV, Tmpl, SnapAcc, VKeys, Views0),
    {MoveOps ++ RestOps, FinalSnap, Views1}.

stream_reset(Az, OldItems, Rest, SV, Tmpl, SnapAcc, OldOrder, Views0) ->
    {Source, _Vis} = SV,
    VKeys = arizona_template:visible_keys(Source#stream.order, Source#stream.limit),
    VSet = maps:from_keys(VKeys, true),
    RemOps = [
        [?OP_REMOVE, Az, arizona_template:to_bin(K)]
     || K := _ <- SnapAcc, not is_map_key(K, VSet)
    ],
    Kept = maps:with(VKeys, SnapAcc),
    {DiffOps, NewSnaps, Views1} =
        smart_reset_items(Az, VKeys, Kept, OldItems, Source#stream.items, Tmpl, Views0, #{}),
    MoveOps = compute_reorder_ops(Az, OldOrder, VKeys, Kept, NewSnaps),
    {RestOps, FinalSnap, Views2} =
        diff_stream_pending(Az, Rest, SV, Tmpl, NewSnaps, VKeys, Views1),
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
    %% A markerless (raw-text content, `Az = undefined`) slot in the item
    %% template is additionally not per-item patchable: it has no op target, so
    %% a positional patch would silently drop (or dangle) its changes. The
    %% wholesale re-render delivers raw-text content correctly as plain HTML.
    Patchable =
        is_list(OldItemsList) andalso
            is_single_root(Tmpl) andalso
            map_size(NewLocal1) =:= map_size(NewLocal0) andalso
            not has_markerless_slot(NewItemsList),
    case Patchable of
        true ->
            {{SubOps, ValueBytes}, Views2} =
                diff_list_positional(Tmpl, NewItemsList, OldItemsList, 0, Views1),
            case SubOps of
                [] ->
                    {[], NewSnap, Views2};
                _ ->
                    maybe_list_patch(
                        Az,
                        Tmpl,
                        {SubOps, ValueBytes},
                        NewItemsList,
                        OldItemsList,
                        NewSnap,
                        Views1,
                        Views2
                    )
            end;
        false ->
            diff_list_full(Az, Tmpl, NewItemsList, OldItemsList, NewSnap, Views1)
    end.

%% Positional patching is always CORRECT (position N is item N in both the DOM and
%% the new render), but it is not always the SMALLER patch. A shift is what makes it
%% lose: insert or remove anywhere but the tail and every later item patches with
%% its neighbour's content, so the ops grow with the list while the wholesale
%% re-render stays one op.
%%
%% The walk's own output names that case without any per-item identity, so it works
%% for a list (which has none) as well as a map. A length change means items were
%% appended or removed; an item patch BESIDE one is the signature of a shift, since
%% those patches are items wearing their neighbour's content. Equal lengths cannot
%% have shifted, so their patches are genuine value changes and always ship.
%%
%% How FAR the list shifted is what decides, and the patch count measures it: an
%% edit at the tail shifts nothing, one at the head shifts everything. That keeps a
%% late insert into a long list on the cheap path -- shipping 1000 items to patch
%% the last two would be the very amplification this guards against.
%%
%% The bias is deliberately toward staying positional, because those ops also
%% preserve the DOM: the container is never torn down, so focus, scroll position
%% and `?local` values inside it survive -- the whole reason `?OP_LIST_PATCH`
%% exists. Measured against the wholesale encoding, the two cross at about half the
%% list shifted, but there they are within ~1% of each other and trading the DOM
%% for that would be a bad deal. Three quarters shifted is where positional turns
%% decisively bigger (30-40% more bytes at 10 and 40 items) while touching nearly
%% every node anyway, so that is where it hands over.
maybe_list_patch(Az, Tmpl, SubOps, NewItemsList, OldItemsList, NewSnap, Views1, Views2) ->
    case outgrows_re_render(SubOps, Tmpl, NewItemsList) of
        false ->
            {[[?OP_LIST_PATCH, Az, element(1, SubOps)]], NewSnap, Views2};
        true ->
            diff_list_full(Az, Tmpl, NewItemsList, OldItemsList, NewSnap, Views1)
    end.

%% Positional patching is always CORRECT -- position N is item N in both the DOM and
%% the new render -- but not always the SMALLER patch, and this is where that is
%% decided. Insert or remove anywhere but the tail and every later item patches with
%% its neighbour's content, so the ops grow with the list while the wholesale
%% re-render stays one op.
%%
%% Counting how much of the container changed cannot decide it, because the three
%% per-op costs are structurally different: an `?OP_INSERT` carries a whole rendered
%% item (statics included), an `?OP_ITEM_PATCH` carries values plus framing, and an
%% `?OP_REMOVE` is a couple of integers. Wholesale, by contrast, ships the statics
%% ONCE plus every item's values. A count therefore assumes a statics-to-value ratio,
%% and the real crossover moves across ordinary shapes -- so a constant tuned on
%% inserts overcharges removes by exactly one statics block, and a shrink whose
%% surviving prefix is unchanged produces no item patches at all and could never trip
%% a count at any threshold.
%%
%% Estimate both sides in bytes instead. The wholesale side costs nothing extra to
%% obtain: `diff_list_positional/5` already visits every new item, so it accumulates
%% their value bytes as it walks.
%%
%% The comparison is biased toward staying positional, because those ops also
%% preserve the DOM: the container is never torn down, so focus, scroll position and
%% `?local` values inside it survive -- the whole reason `?OP_LIST_PATCH` exists.
%% Wholesale has to be clearly smaller, not merely smaller, before it is worth that,
%% and it has to save something worth having in absolute terms too. A small container
%% can be 2x cheaper to re-render while the saving is a hundred-odd bytes, which is
%% not a trade worth losing an in-progress selection over.
outgrows_re_render({SubOps, ValueBytes}, Tmpl, NewItemsList) ->
    Positional = wire_bytes(SubOps),
    Whole = re_render_bytes(Tmpl, ValueBytes, length(NewItemsList)),
    Positional - Whole > ?RE_RENDER_MIN_SAVING andalso
        Positional * ?RE_RENDER_BIAS_DEN > Whole * ?RE_RENDER_BIAS_NUM.

%% Statics once, every item's values, plus per-item list framing.
re_render_bytes(#{s := Statics}, ValueBytes, Count) ->
    iolist_size(Statics) + ValueBytes + Count * 4.

%% Rough JSON-encoded size of a term, without encoding it. Only the ratio against
%% `re_render_bytes/3` matters, so constants approximate quoting and separators.
wire_bytes(B) when is_binary(B) ->
    byte_size(B) + 2;
wire_bytes(I) when is_integer(I) ->
    4;
wire_bytes(L) when is_list(L) ->
    lists:foldl(fun(E, A) -> A + wire_bytes(E) + 1 end, 2, L);
wire_bytes(M) when is_map(M) ->
    maps:fold(fun(K, V, A) -> A + wire_bytes(K) + wire_bytes(V) + 2 end, 2, M);
wire_bytes(_Other) ->
    8.

is_single_root(#{single_root := true}) -> true;
is_single_root(#{}) -> false.

%% Every item shares one template, so probing the first rendered item's
%% dynamics for an `undefined` Az suffices. An empty list has no dynamics to
%% patch, so it stays patchable (tail inserts/removes only).
has_markerless_slot([ItemD | _]) ->
    lists:any(
        fun
            ({undefined, _, _}) -> true;
            ({_, _, _}) -> false
        end,
        ItemD
    );
has_markerless_slot([]) ->
    false.

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
%% walk but emits no ops, just whether any item changed (a slot that renders
%% differently, or a length difference), threading child views through.
list_changed([NewD | NR], [OldD | OR], Views0) ->
    %% The walk still runs for its Views accumulation (this fallback is where a
    %% list bearing per-item child views lands); its ops are discarded, since only
    %% the boolean matters here. A changed markerless slot emits no inner op but IS
    %% a change, and `item_changed/2` sees it like any other slot.
    {_InnerOps, _Markerless, Views1} = diff_item_dynamics_v(NewD, OldD, Views0),
    {RestChanged, Views2} = list_changed(NR, OR, Views1),
    {item_changed(NewD, OldD) orelse RestChanged, Views2};
list_changed([], [], Views) ->
    {false, Views};
list_changed(_NewTail, _OldTail, Views) ->
    {true, Views}.

%% Did any of this item's slots RENDER differently? The fallback re-renders the
%% whole container, so answering on term inequality alone tears the container down
%% and rebuilds byte-identical markup, losing focus, scroll position and every
%% `?local` inside it for nothing. A value can differ as a term yet render the
%% same: `to_bin/1` formats floats to 10 decimals, so accumulated error past that
%% (0.1 + 0.2 against 0.3) renders "0.3" either way, and an integer against its
%% binary is the same bytes too.
%%
%% The byte comparison only runs once the terms already differ, so it costs
%% nothing on the common path, and only for values `to_bin/1` renders without
%% raising. A map (a nested template, each snapshot or child view) or a descriptor
%% tuple is not compared: it counts as changed, which is the conservative answer.
item_changed([], []) ->
    false;
item_changed([{_NewAz, Same, _} | NR], [{_OldAz, Same, _} | OR]) ->
    item_changed(NR, OR);
item_changed([{_NewAz, New, _} | NR], [{_OldAz, Old, _} | OR]) ->
    case collapses_to_same_bytes(New, Old) of
        true -> item_changed(NR, OR);
        false -> true
    end;
item_changed(_NewTail, _OldTail) ->
    true.

%% Do two values KNOWN to differ still render to the same bytes? Asked only from
%% `item_changed/2`, which has already matched the equal case, so these clauses
%% never see equal inputs (hence the name -- "collapses", not "renders same").
%%
%% Same-type scalars collapse for nothing: `to_bin/1` is the identity on binaries,
%% and `integer_to_binary`/`atom_to_binary` are injective, so two distinct ones
%% cannot print the same. That covers nearly every slot without rendering either
%% side, leaving the render for the pairs that genuinely can collapse -- two floats
%% (the 10-decimal format), or a value against a different type.
%% Same marker both sides: unwrap and compare the payloads. The parse transform
%% wraps every content slot value, so without this a real template never reaches the
%% type fast paths below at all -- only hand-built templates do.
collapses_to_same_bytes({arizona_esc, New}, {arizona_esc, Old}) ->
    collapses_to_same_bytes(New, Old);
%% An attribute's value compares like any other, EXCEPT that a boolean is not a
%% value here: `true` renders as a bare attribute and `false` removes it outright
%% (`?OP_REM_ATTR`), so neither may be called equal to a string that happens to print
%% the same -- `true` against `~"true"` really is a change, from a bare attribute to
%% `name="true"`. A differing NAME is a different attribute and never collapses.
collapses_to_same_bytes({attr, Name, New}, {attr, Name, Old}) when
    not is_boolean(New), not is_boolean(Old)
->
    collapses_to_same_bytes(New, Old);
collapses_to_same_bytes({attr, _N1, _V1}, {attr, _N2, _V2}) ->
    false;
collapses_to_same_bytes(New, Old) when is_binary(New), is_binary(Old) ->
    false;
collapses_to_same_bytes(New, Old) when is_integer(New), is_integer(Old) ->
    false;
collapses_to_same_bytes(New, Old) when is_atom(New), is_atom(Old) ->
    false;
collapses_to_same_bytes(New, Old) ->
    byte_comparable(New) andalso byte_comparable(Old) andalso
        arizona_template:to_bin(New) =:= arizona_template:to_bin(Old).

%% An escape marker is deliberately NOT comparable. `to_bin/1` unwraps it, so
%% `{arizona_esc, ~"<b>"}` and a bare `~"<b>"` look identical to it -- but the
%% wholesale re-render escapes the wrapped one (`&lt;b&gt;`) and not the bare one, so
%% treating them as equal would drop a visible change, and in one direction leave
%% unescaped markup on screen. A pair SHARING the marker is unwrapped above; a mixed
%% pair reaches here and counts as changed.
byte_comparable(V) when is_binary(V); is_integer(V); is_float(V); is_atom(V) ->
    true;
byte_comparable(_Other) ->
    false.

%% A plain-list `?each` is marker-anchored in a content slot (no wrapper element
%% carries the slot az), so the full re-render patches the marker content via
%% `?OP_TEXT`. An innerHTML-style whole-element write would clobber the slot's
%% static siblings when resolveEl falls back to the enclosing element. Mirrors
%% the `make_op/3` plain-list each clause and the nested-template content-slot fix.
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
%% Returns `{{SubOps, ValueBytes}, Views}`. `ValueBytes` is the size of every NEW
%% item's values, accumulated here because this walk already visits them -- it is
%% what `outgrows_re_render/3` prices the wholesale alternative with.
%% Entry point. Strip the unchanged head and tail FIRST, then diff only the middle.
%% Without that, a head insert reads as "every position differs" and emits one item
%% patch per item plus a tail append -- each patch carrying the value of the item that
%% merely shifted along. The change is one item, so the patch should be one op.
%%
%% Sub-op indices address the OLD positions, which is exactly what the client resolves
%% them against: `applyListPatch` snapshots the item roots before applying anything, so
%% an `?OP_INSERT` at index N lands before the item that was at N (or the end marker
%% past the end), and repeated inserts at one index keep their emitted order because
%% each lands immediately before the same unmoved reference node.
diff_list_positional(Tmpl, NewItems, OldItems, Idx0, Views0) ->
    AllBytes = lists:foldl(fun(D, A) -> A + item_value_bytes(D) end, 0, NewItems),
    {Common, NewRest, OldRest} = strip_common_prefix(NewItems, OldItems, 0),
    {NewMid, OldMid} = maybe_strip_common_suffix(NewRest, OldRest),
    {Ops, Views1} = diff_list_middle(Tmpl, NewMid, OldMid, Idx0 + Common, Views0),
    {{Ops, AllBytes}, Views1}.

strip_common_prefix([Same | NR], [Same | OR], N) ->
    strip_common_prefix(NR, OR, N + 1);
strip_common_prefix(NewRest, OldRest, N) ->
    {N, NewRest, OldRest}.

%% The tail matters as much as the head: a HEAD insert differs at position 0, so the
%% prefix strip finds nothing, while every remaining item matches one position along.
%% Stripping that shared tail turns it into an empty old middle -- one insert. The
%% middle's starting index is unaffected, since the shared tail sits past it.
%%
%% Only worth doing when the lengths DIFFER. Stripping costs a reverse of both lists,
%% and at equal length it cannot produce a pure insert or remove -- the middle still
%% goes through the lockstep walk, which already skips matching items for nothing. The
%% common case is a value changing in place, so paying two reverses there is the whole
%% cost of this optimisation with none of its benefit.
maybe_strip_common_suffix(New, Old) ->
    case same_length(New, Old) of
        true -> {New, Old};
        false -> strip_common_suffix(New, Old)
    end.

%% Comparing two `length/1` calls walks BOTH lists to the end, even when one ran out
%% a hundred items ago. The lockstep walk stops at the shorter one, and that is the
%% side this guard cares about -- it asks whether the lengths differ, not by how much.
same_length([], []) ->
    true;
same_length([_New | NR], [_Old | OR]) ->
    same_length(NR, OR);
same_length(_New, _Old) ->
    false.

strip_common_suffix(New, Old) ->
    {_N, RevNew, RevOld} = strip_common_prefix(lists:reverse(New), lists:reverse(Old), 0),
    {lists:reverse(RevNew), lists:reverse(RevOld)}.

%% A pure insertion: the old list is exhausted at the same point the new one still has
%% items, and everything before matched. One `?OP_INSERT` per added item, all at the
%% same old index, instead of dragging every later item through a patch.
diff_list_middle(Tmpl, NewRest, [], Idx, Views) ->
    {[[?OP_INSERT, Idx, arizona_render:zip_item(Tmpl, D)] || D <- NewRest], Views};
%% A pure removal: nothing new remains, so drop the old tail by index.
diff_list_middle(_Tmpl, [], OldRest, Idx, Views) ->
    {remove_ops(OldRest, Idx), Views};
%% Both sides still have items: an edit rather than a clean insert or remove. Walk them
%% in lockstep, which is what a same-length content change wants anyway.
diff_list_middle(Tmpl, [NewD | NR], [OldD | OR], Idx, Views0) ->
    %% Markerless slots never reach this walk: `diff_each_items/6` routes any
    %% template carrying one to the wholesale fallback.
    {InnerOps, _Markerless, Views1} = diff_item_dynamics_v(NewD, OldD, Views0),
    {RestOps, Views2} = diff_list_middle(Tmpl, NR, OR, Idx + 1, Views1),
    case InnerOps of
        [] -> {RestOps, Views2};
        _ -> {[[?OP_ITEM_PATCH, Idx, InnerOps] | RestOps], Views2}
    end.

%% One `?OP_REMOVE` per dropped item, indexed from `Idx`. Walking the items
%% themselves keeps this one pass: `lists:seq/2` needs the tail's length first --
%% another walk -- and then builds a list of integers only to walk that too.
remove_ops([], _Idx) ->
    [];
remove_ops([_OldD | Rest], Idx) ->
    [[?OP_REMOVE, Idx] | remove_ops(Rest, Idx + 1)].

item_value_bytes(ItemD) ->
    lists:foldl(fun({_Az, V, _Deps}, A) -> A + wire_bytes(V) end, 0, ItemD).

smart_reset_items(_Az, [], _Kept, _OldItems, _ItemsMap, _Tmpl, Views, Snaps) ->
    {[], Snaps, Views};
smart_reset_items(Az, [K | Rest], Kept, OldItems, ItemsMap, Tmpl, Views0, Snaps) ->
    NewItem = maps:get(K, ItemsMap),
    case Kept of
        #{K := OldD} ->
            {NewD, Views1} =
                render_kept_with_skipping(K, NewItem, OldD, OldItems, Tmpl, Views0),
            NewSnaps = Snaps#{K => NewD},
            %% Render-once markerless slots: same rule as stream_update_existing.
            {InnerOps, _Markerless, Views2} = diff_item_dynamics_v(NewD, OldD, Views1),
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
    #stream{limit = infinity, order = Order} = Source,
    Tmpl,
    SnapItems,
    _OldOrder,
    Views
) ->
    %% Flush the {Front, BackRev} buffer to a flat list -- the snapshot's
    %% `order` is consumed by `arizona_render:zip/2` as a list iterator,
    %% not by `visible_keys/2`, so we need to materialise here.
    FlatOrder = arizona_template:visible_keys(Order, infinity),
    {[], post_drain_snap(SnapItems, FlatOrder, Tmpl, Source), Views};
apply_limit(
    Az,
    #stream{limit = Limit, items = ItemsMap, order = Order} = Source,
    Tmpl,
    SnapItems,
    OldOrder,
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
    case window_unchanged(VKeys, OldOrder, SnapItems, 0) of
        true ->
            %% Fast path -- the frame didn't touch window membership or order
            %% (e.g. a single visible-item content update): nothing fell out,
            %% nothing to back-fill, so skip the VSet/RemOps/Pruned passes and
            %% their map allocations entirely.
            {[], post_drain_snap(SnapItems, VKeys, Tmpl, Source), Views0};
        false ->
            VSet = maps:from_keys(VKeys, true),
            RemOps = [
                [?OP_REMOVE, Az, arizona_template:to_bin(K)]
             || K := _ <- SnapItems, not is_map_key(K, VSet)
            ],
            Pruned = #{K => V || K := V <- SnapItems, is_map_key(K, VSet)},
            {InsOps, Final, Views1} =
                snap_add_missing(Az, VKeys, Pruned, ItemsMap, Tmpl, Views0),
            {
                RemOps ++ InsOps,
                post_drain_snap(Final, VKeys, Tmpl, Source),
                Views1
            }
    end.

%% The post-drain snapshot, marked so the NEXT drain of this slot resumes past the
%% ops this one just consumed (`arizona_stream:undrained_ops/2`). That is what
%% makes a re-drain of a never-cleared queue cheap and stale-patch free. The mark
%% is the stamp of the last op in the post-op queue, so it covers every op the
%% drain walked, including the ones the visibility window skipped. Resuming
%% locates that stamp rather than counting positions, so a queue the mark does not
%% belong to (a divergent successor of the same stream, a reset, a clear) falls
%% back to a full drain instead of dropping its ops.
post_drain_snap(SnapItems, Order, Tmpl, Source) ->
    #{
        t => ?EACH,
        items => SnapItems,
        order => Order,
        template => Tmpl,
        drained => arizona_stream:drain_mark(Source)
    }.

%% Allocation-light equality for the fast path: the new visible window must
%% equal the pre-frame window (`OldOrder`) element-wise, every window key must
%% be in the client-DOM snapshot, and the snapshot must hold EXACTLY those
%% keys (the size check catches keys the drain added or should prune).
window_unchanged([K | VR], [K | OR], SnapItems, N) when is_map_key(K, SnapItems) ->
    window_unchanged(VR, OR, SnapItems, N + 1);
window_unchanged([], [], SnapItems, N) ->
    map_size(SnapItems) =:= N;
window_unchanged(_VKeys, _OldOrder, _SnapItems, _N) ->
    false.

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

%% `Kept` is the LIS base (keys the client held BEFORE this reconciliation --
%% their old positions anchor the stable subsequence). `Present` is what the
%% client DOM holds when the emitted moves apply: for a plain reorder the same
%% as `Kept` (hidden keys are back-filled only later, by `apply_limit/6`); for
%% a reset it also includes the keys `smart_reset_items/8` just inserted, whose
%% tail (-1) inserts precede these moves on the wire.
compute_reorder_ops(_Az, OldOrder, OldOrder, _Kept, _Present) ->
    [];
compute_reorder_ops(Az, OldOrder, NewOrder, Kept, Present) ->
    %% Pure tail append: every old key kept, in order, with the new ones after
    %% them. `smart_reset_items/8` inserts each missing key at -1 walking the new
    %% order, so they land at the tail in that order and the DOM already equals
    %% `NewOrder` -- every move the LIS would emit is a node moved onto itself.
    %% Worth the check: an append-only list (a log, a growing series) hits this on
    %% every update, and the LIS treats each inserted key as unplaced, so it was
    %% emitting one redundant move per insert (360 inserts -> 360 extra moves).
    case lists:prefix(OldOrder, NewOrder) of
        true ->
            [];
        false ->
            KeptOld = [K || K <- OldOrder, is_map_key(K, Kept)],
            case KeptOld of
                [] ->
                    [];
                _ ->
                    OldPosMap = pos_map(KeptOld, 1),
                    LISSet = lis_indices(NewOrder, OldPosMap),
                    emit_move_ops(Az, LISSet, NewOrder, 1, null, Present)
            end
    end.

pos_map([], _I) -> #{};
pos_map([K | Rest], I) -> (pos_map(Rest, I + 1))#{K => I}.

%% Walk NewOrder left-to-right: LIS keys stay put, other PRESENT keys move to
%% sit after the last present key placed (`Prev`). A key absent from `Present`
%% (limit-hidden, back-filled later at its exact index) is skipped entirely:
%% it neither gets a MOVE (the client has no node for it) nor becomes a ref --
%% a MOVE whose ref is missing makes the client's moveItemEl fall back to
%% appending, scrambling the surviving order. Skipping is sound because by the
%% time a present key is placed, every present key to its left in NewOrder has
%% already settled, so "after the last present key placed" IS its position
%% among present keys; the back-fill then interleaves the hidden ones.
emit_move_ops(_Az, _LIS, [], _I, _Prev, _Present) ->
    [];
emit_move_ops(Az, LIS, [Key | Rest], I, Prev, Present) ->
    case LIS of
        #{I := _} ->
            emit_move_ops(Az, LIS, Rest, I + 1, Key, Present);
        #{} when not is_map_key(Key, Present) ->
            emit_move_ops(Az, LIS, Rest, I + 1, Prev, Present);
        #{} ->
            Ref =
                case Prev of
                    null -> null;
                    _ -> arizona_template:to_bin(Prev)
                end,
            [
                [?OP_MOVE, Az, arizona_template:to_bin(Key), Ref]
                | emit_move_ops(Az, LIS, Rest, I + 1, Key, Present)
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
%% enclosing element -- intact. A whole-element innerHTML write would be wrong:
%% when the slot's `Az` is the enclosing element's own `az` (a conditional
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
%% `az="X"`, so `?OP_TEXT` (replace marker content) is correct and a
%% whole-element innerHTML write is wrong: when the each sits among static
%% siblings, the client's resolveEl finds no element for the slot az and falls
%% back to the enclosing element, where innerHTML wipes every sibling. The
%% marker is present whether or not the each is the sole child, so `?OP_TEXT`
%% is uniformly correct (a sole-child each only "worked" with the old
%% innerHTML op by coincidence).
make_op(Az, #{t := ?EACH, items := Items, template := Tmpl}, _Old) when
    is_list(Items)
->
    [?OP_TEXT, Az, arizona_render:zip_list_fp(Tmpl, Items)];
%% Stream (`order`-keyed) each: the container-level FULL render, which the same
%% marker rule governs as the plain-list clause above. SSR anchors a stream each
%% by the identical content-slot markers, and among static siblings the slot az
%% is compound (`<Root>:N`) and carried by no element -- so the client resolves it
%% through the marker to the ENCLOSING element, where a whole-element innerHTML
%% write takes the siblings with it (when the enclosing element is the view root,
%% the whole view). `?OP_TEXT` replaces only the marker content and is uniformly
%% correct, sole-child or not. The INCREMENTAL stream ops (`?OP_INSERT`,
%% `?OP_REMOVE`, `?OP_MOVE`, `?OP_ITEM_PATCH`) keep their own op codes: they carry
%% the SAME container az as the target and name the item by key in a later field,
%% and they mutate one keyed child rather than the container's whole content, so
%% the full-render op code does not govern them. They have their own open
%% limitation -- placement (`?OP_INSERT`'s position, `?OP_MOVE`'s prepend) is
%% relative to the container ELEMENT, not the marker span, so on a marker-only
%% container the client refuses them rather than misplacing the node (see
%% docs/architecture.md).
make_op(Az, #{t := ?EACH, items := Items, order := Order, template := Tmpl}, _Old) ->
    [?OP_TEXT, Az, arizona_render:zip_stream_fp(Tmpl, Items, Order)];
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
%% A stream `?each` container whose rendered items, order, and item-template
%% fingerprint are all unchanged needs no op: the container's HTML is exactly
%% what the client already holds, so the wholesale container `?OP_TEXT` the last
%% clause below would emit (via `make_op/3`'s stream `?EACH` clause) would only
%% re-materialize an identical list -- destroying focus, scroll, uncontrolled
%% input state and every `?local` in the items for nothing.
%%
%% Reached when the two sides are compared snapshot-against-snapshot rather than
%% against the each descriptor (`diff_child_dynamics/3`, i.e. an embedded child
%% view's inner dynamics diffed from its parent). There the freshly evaluated
%% side carries `source` and the stored side does not -- the incremental stream
%% path settles its snapshot without it -- so the two never compare term-equal
%% even when they render identically.
make_ops(
    _Az,
    #{t := ?EACH, items := Items, order := Order, template := #{f := Fp}},
    #{t := ?EACH, items := Items, order := Order, template := #{f := Fp}},
    Tail,
    Views
) ->
    {Tail, Views};
make_ops(Az, #{s := S, d := NewD} = New, #{s := S, d := OldD} = Old, Tail, Views) when
    not is_map_key(view_id, New)
->
    %% A markerless slot has no op target of its own, so `diff_dynamics/4` skips it.
    %% At a template's TOP level that is right -- raw-text content is render-once,
    %% and there is nothing to patch it with. Inside a nested template it is not:
    %% this slot has an `Az` and its own `<!--az:X-->...<!--/az-->` markers, so the
    %% change is deliverable by re-rendering the nested template whole. Without the
    %% escalation the change is dropped silently, which is how an SVG `<title>` in a
    %% `?stateless` child freezes: the child compiles on its own, so it cannot know
    %% it renders in foreign content, classifies `title` as escapable raw text, and
    %% its slot comes out markerless.
    case markerless_changed(NewD, OldD) of
        false -> diff_dynamics(NewD, OldD, Tail, Views);
        true -> {[make_op(Az, New, Old) | Tail], Views}
    end;
make_ops(
    _Az, #{view_id := VId, s := S, d := NewD}, #{view_id := _, s := S, d := OldD}, Tail, Views
) ->
    %% Child view: diff its inner dynamics like make_op/3's child clause, but
    %% suppress the wrapper when no inner op survives (e.g. only a markerless
    %% raw-text slot changed) -- an empty `[VId, []]` op is wire noise.
    case diff_child_dynamics(NewD, OldD) of
        [] -> {Tail, Views};
        ChildOps -> {[[VId, ChildOps] | Tail], Views}
    end;
make_ops(Az, New, Old, Tail, Views) ->
    {[make_op(Az, New, Old) | Tail], Views}.

%% Did a markerless slot at THIS nesting level change value? Only this level: a
%% markerless slot deeper down sits in its own nested template, whose `make_ops/5`
%% escalates it against its own `Az`, which is the tighter patch.
markerless_changed([], []) ->
    false;
markerless_changed([{undefined, Same} | NR], [{undefined, Same} | OR]) ->
    markerless_changed(NR, OR);
markerless_changed([{undefined, _New} | _NR], [{undefined, _Old} | _OR]) ->
    true;
markerless_changed([_New | NR], [_Old | OR]) ->
    markerless_changed(NR, OR).

%% Walks an item's dynamics, returning `{Ops, Markerless, Views}`. `Markerless`
%% is true when a markerless slot (raw-text element content, `Az = undefined`)
%% changed value: such a slot has no op target, so no op is ever emitted for it
%% (mirroring `diff_dynamics/3`), but a plain-list container uses the flag to
%% fall back to the wholesale re-render that CAN deliver the change (see
%% `diff_each_items/6` / `list_changed/3`). Stream items and reset keeps ignore
%% it -- their markerless slots are render-once, like a root-level raw-text slot.
diff_item_dynamics_v([], [], Views) ->
    {[], false, Views};
diff_item_dynamics_v([{Az, _, _} | NR], [{Az, #{diff := false}, _} | OR], Views0) ->
    diff_item_dynamics_v(NR, OR, Views0);
diff_item_dynamics_v([{Az, Same, _} | NR], [{Az, Same, _} | OR], Views0) ->
    diff_item_dynamics_v(NR, OR, Views0);
diff_item_dynamics_v([{undefined, _New, _} | NR], [{undefined, _Old, _} | OR], Views0) ->
    %% Markerless render-once slot whose value changed (equal pairs matched
    %% above): never emit an op -- there is no comment marker to target.
    {Ops, _Markerless, Views1} = diff_item_dynamics_v(NR, OR, Views0),
    {Ops, true, Views1};
diff_item_dynamics_v([{Az, New, _} | NR], [{Az, Old, _} | OR], Views0) ->
    case {New, Old} of
        {#{t := ?EACH, source := #stream{} = Src, template := Tmpl}, #{t := ?EACH}} ->
            %% Only a STREAM each snapshot carries `source` (build_each_snap /
            %% diff_stream's first-render clause): list- and map-source eaches
            %% evaluate to snapshots without it, so they fall to the make_ops
            %% clause below and re-render wholesale via the ?OP_TEXT each clause.
            EachDesc = #{source => Src, template => Tmpl},
            {EachOps, _NewSnap, Views1} = diff_stream(Az, EachDesc, Old, Views0),
            {RestOps, Markerless, Views2} = diff_item_dynamics_v(NR, OR, Views1),
            {EachOps ++ RestOps, Markerless, Views2};
        _ ->
            {RestOps, Markerless, Views1} = diff_item_dynamics_v(NR, OR, Views0),
            {NewOps, Views2} = make_ops(Az, New, Old, RestOps, Views1),
            {NewOps, Markerless, Views2}
    end.

diff_child_dynamics(NewD, OldD) ->
    diff_child_dynamics(NewD, OldD, []).

diff_child_dynamics([], [], Tail) ->
    Tail;
diff_child_dynamics([{undefined, _} | NR], [{undefined, _} | OR], Tail) ->
    %% Markerless render-once slot (raw-text element content): no comment
    %% marker to target, so never emit an op -- mirrors diff_dynamics/3.
    diff_child_dynamics(NR, OR, Tail);
diff_child_dynamics([{Az, _New} | NR], [{Az, #{diff := false}} | OR], Tail) ->
    diff_child_dynamics(NR, OR, Tail);
diff_child_dynamics([{Az, Same} | NR], [{Az, Same} | OR], Tail) ->
    diff_child_dynamics(NR, OR, Tail);
diff_child_dynamics([{Az, New} | NR], [{Az, Old} | OR], Tail) ->
    {Ops, _Views} = make_ops(Az, New, Old, diff_child_dynamics(NR, OR, Tail), {#{}, #{}}),
    Ops.
