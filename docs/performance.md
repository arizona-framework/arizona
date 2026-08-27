# Performance

How to measure Arizona, what measuring it has actually taught us, and what is still
worth trying. Written after a tuning pass over `arizona_diff`, `arizona_stream`,
`arizona_template` and `arizona_render`; the numbers below are from that pass
(12th Gen i9-12900HX, OTP 29) and are there to give a sense of scale, not to be
treated as thresholds.

## Tools

| Command | What it is for |
| ------- | -------------- |
| `make bench ARGS="--only <label>"` | Per-op wall clock for a workload. Catches regressions, not causes. |
| `make bench-ab REFS="<a> <b>" ARGS="--only <label>"` | Paired A/B of one workload across two commits. |
| `make prof ARGS="--only <label>"` | eprof/fprof per-MFA breakdown. Finds hot paths. |
| `make prof-at REF=<ref> ARGS=...` | The same at any commit-ish, in a cached worktree. |
| call-count tracing | Which functions run, and how often. See below. |

None of these are wired into `ci` or `precommit`: shared runners are too noisy for
automatic thresholds, and every one of them needs a human to read it.

## Measuring without fooling yourself

Four traps, each of which produced a confidently wrong answer during the tuning pass.

**A benchmark cannot resolve small changes.** Take a module, add ~640 bytes of code
that is never called, and `stream_update_field_100` moved by **-5.1%**. Nothing about
the work changed; only the layout of the compiled module did. That workload runs
thousands of calls to two tiny functions per event, so instruction-cache placement
moves the number as much as a real optimisation would. Treat anything under ~10% on
such a workload as unresolved rather than as a result, and reach for call counts and
micro-benchmarks to settle it.

**Swapping sources in one checkout can measure the wrong build.** rebar3 compares
source and beam mtimes at 1-second resolution, so a variant copied into place within
the same second as the previous build is silently not recompiled. A bisect run this
way produced numbers that moved non-monotonically across cumulative commits, which is
what gave it away. `bench_ab.sh` builds each ref in its own worktree so this cannot
happen; if you swap files by hand, delete the beam first and verify what you loaded
(`md5sum`, or `beam_lib:chunks/2` for a function only one variant defines).

**A cross-commit comparison compares the benchmarks too.** Each worktree carries its
own `scripts/bench.escript` and `test/support/`, so a workload whose fixture changed
between the refs is measuring two different amounts of work. `git diff <a> <b> --
scripts/ test/support/` before believing a surprising number. This is not
hypothetical: `render_each_100` passed a 100-element `tags` override into a fixture
whose `mount/1` ignored it, so for a long time it measured a **3-item** list under a
name that promised 100.

**eprof over-weights small functions.** Its percentages are call-count weighted, so a
tiny function called hundreds of thousands of times looks dominant. During this pass
it pointed at `arizona_template:track/1`, which turned out to already no-op when
dependency collection is off. Use eprof to find *candidates*, then confirm with a
micro-benchmark of that function in isolation.

**What to do instead.** Prefer the minimum over the mean: the machine is usually
contended, and the minimum is the run that was least disturbed. Interleave the two
variants round by round so drift hits both. Pin with `taskset`. And when the
end-to-end number will not resolve the change, answer the question directly:

```erlang
%% Which functions actually run on this path, and how often?
erlang:trace_pattern({arizona_diff, '_', '_'}, true, [call_count]),
run_the_workload(),
[{F, A, C} || {F, A} <- arizona_diff:module_info(functions),
              {call_count, C} <- [erlang:trace_info({arizona_diff, F, A}, call_count)],
              is_integer(C), C > 0].
```

That is what settled the biggest find in this pass: a workload whose per-event cost
was ~2100 calls to `wire_bytes/1`, all from one estimate that could not affect the
outcome.

## Verifying an optimisation is exact

Tests answer "did anything break". They do not answer "did the output change at all",
which is the question an optimisation should be able to answer with **no**. Three
differential harnesses were used during this pass; each runs the same inputs through
two builds and compares byte for byte. They live outside the repo (they are throwaway
scripts), but the recipes are short:

- **Render** -- for every fixture in `test/support/` exporting `mount/1` and
  `render/1`, call `arizona_render:render_view_to_iolist(Mod, #{bindings => #{}})`
  and dump the HTML. 83 fixtures render request-free.
- **Wire ops** -- for each fixture, extract its event names from the source
  (`handle_event(~"..."`), mount it through `arizona_socket:init/4`, fire each event
  twice through `handle_in/2`, and dump every reply frame. Record crashes too, so a
  change in failure behaviour is caught. 49 fixtures, 212 frames.
- **Fuzz** -- random `arizona_stream` op sequences from a fixed seed (so both builds
  see identical sequences), comparing `to_list/1`, the visible window, undrained ops,
  the pending queue and size after every op. 3000 sequences, ~39k ops.

All three found nothing during this pass, which is the point: they are what makes
"this change is exact" a measurement rather than a claim.

## What worked

Every one of these is exact -- same output, less work. The pattern behind most of
them is the same: **work computed eagerly whose result the common path never reads.**

| Change | Effect |
| ------ | ------ |
| Gate the stream re-render estimate on the patch size | `stream_update_field_100` -72%, `stream_update_unchanged_100` -84% |
| Decide a slot's `az` prefix from the value, not eagerly | render path -33% to -62% |
| Same gate for the plain-list re-render estimate | no bench covers it; removes an O(items x dynamics) walk per list diff |
| Take a stream's visible window from the front | -72% at 100 keys, -97% at 1000 (limit 20) |
| Evict a full `drop` stream by popping the front | -73% per append at limit 2000; O(Limit) becomes amortized O(1) |
| Fuse SSR dynamics into the zip walk | `render_each_100` -8%, `render_view_many_dyn_50` -6% |
| Skip the second `classify_trusted/1` per escaped value | (measured together with the fuse above) |
| Keep the drain's accumulator when it exhausts the queue | halves `undrained_ops/2` on the ordinary path |
| Carry a stream's child views only when it has any | avoids draining the whole pending queue per stream diff |

Two of these deserve their reasoning recorded, because both look like they *should*
be needed:

The **re-render estimate** decides whether a wholesale container re-render would ship
fewer bytes than incremental ops. It weighed every value of every item to do it --
and the wholesale side is never negative, so a patch no bigger than the floor cannot
beat it by the floor, whatever the items weigh. The answer is settled by the ops
alone. It was being computed on every drain, including the overwhelmingly common one
that patches a single field.

The **slot `az` prefix** was built by `binary:replace/4` before looking at the value.
That is a pure-Erlang wrapper: it compiles the pattern, allocates a closure and copies
the binary, ~125ns, even when there is no colon to replace. Only a nested snapshot
ever uses the prefix; a scalar -- which is what nearly every content slot holds --
threw it away. Asking the value first drops that slot to ~6ns.

## What did not work

Recorded so they are not retried blindly.

**Eliminating `++` and `length/1` for their own sake.** A first pass removed nearly
all of them from `arizona_diff` (difference lists, single-pass counting, lockstep
length comparison). Every one is strictly less work and they were kept, but the
end-to-end effect was **not measurable** -- each was linear in a list whose
construction already cost more than the copy. The real win in the same code was the
gate above, which removed the work rather than making it cheaper.

**Replacing `maps:merge/2` in `compute_item_changed/2` with two iterator walks.**
Measured **+49%** at 5 keys and **+30%** at 21 -- stream items are small maps, where
the merge's C path beats stepping a map iterator in Erlang. Rejected.

**Tail recursion with `lists:reverse/2` in the op builders.** `lists:reverse/2`
attaches a tail without `++`, so it is a real alternative to body recursion. It wins
when most items emit an op (100 items, 100 ops: 730ns vs 1200ns) and loses when few
do (10 items, 1 op: 38ns vs 35ns) or when the list is long enough that the extra
allocation bites (1000 of 1000: 28.6us vs 16.2us). Op builders here usually emit few
ops, so body recursion stayed. Worth re-measuring for any builder whose output is
dense.

**Stepping a map's own iterator instead of `maps:values/1`.** Tried inside the
re-render estimate. `maps:values/1` is one pass in C and won at 10 entries, tied at
1000; the comprehension plus `lists:sum/1` it replaced was the slowest of the three
(~30% worse at 100). Summing a `maps:values/1` walk directly is the shape that won.

## Still open

Ranked by expected value. Nothing here has been measured end to end.

1. **`render_list_items_simple/2`'s value tree** -- 16.4% of a 100-item render, split
   across two comprehensions. It builds `[[{Az, Val, Deps}]]` for every item, which
   the zip walk then turns into HTML and discards on the SSR path. Emitting HTML
   directly per item would remove it, but the tree is load-bearing for nested
   snapshots and `maybe_propagate/2`, so this is a redesign of the SSR path rather
   than a local change.
2. **`arizona_stream:clear_stream_pending/2`** -- called as
   `clear_stream_pending(B, stream_keys(B))`, which walks the bindings map twice and
   rebuilds every stream record even when its queue is already empty. Runs per
   stateful child per evaluation.
3. **`arizona_stream:sort/2`** -- `lists:sort/2` with a comparator that does two
   `maps:get/2` per comparison, so ~2*N*log2(N) lookups. Decorate-sort-undecorate
   makes it N.
4. **`arizona_stream:reset/2` and `new/3`** -- build an intermediate `[{Key, Item}]`
   list and walk it twice. `reset/2` is the documented bulk-mutation path.
5. **`arizona_render:render/1`** -- builds a list of unwrapped values and then zips
   it, although `zip_d/3` already walks pairs in one pass. On the connect/navigate
   path, not per event.
6. **`arizona_diff`'s four remaining appends** -- three are stream containers whose
   drain runs before the walk that would supply a tail (the drain feeds it the views
   it rendered, and reordering would reorder `$arizona_update_effects`, which ships
   in evaluation order); the fourth is in `stream_reset/8`, where the moves and the
   rest of the drain both need the snapshot the item walk produces. All four cons
   onto an empty tail in the common case, which `append_ops/2` already answers for
   free.
