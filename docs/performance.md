# Performance

How to measure Arizona, what measuring it has actually taught us, and what is still
worth trying. Written after a tuning pass over `arizona_diff`, `arizona_stream`,
`arizona_template` and `arizona_render`; the numbers below are from that pass
(12th Gen i9-12900HX, OTP 29) and are there to give a sense of scale, not to be
treated as thresholds.

## Tools

| Command | What it is for |
| ------- | -------------- |
| `make bench ARGS="--only <label>"` | Per-op wall clock + deterministic `red/op` reductions for a workload. Catches regressions, not causes. |
| `make bench-ab REFS="<a> <b>" ARGS="--only <label>"` | Paired A/B of one workload across two commits. |
| `make bench-client ARGS="--only <label>"` | `applyOps` in a real Chromium, against fixtures from a real diff. |
| `make bench-client-connect ARGS="--only <label>"` | The real `connect()` + bfcache reconnect in Chromium, frames from a real socket, WS stubbed to zero latency. |
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

**Per-BYTE recursion is the extreme form of that, and it reads as a hot spot.** A
function that recurses one byte at a time emits one trace event per byte, so eprof
charges it per byte at trace-event prices. Profiling an HTTP GET made header parsing
look like ~11.5% of the request -- more than the whole framework above it. Dividing
gives it away: eprof attributed **41 ns to each byte-step** of a scan whose real cost
is 1-2 ns, an inflation of 20-40x. The true share was well under 1%. Before believing
any profile row, divide its time by its call count and ask whether that per-call
figure is physically plausible for what the function does; a per-byte walker and a
BIF call in the same table are not on the same scale.

**The profiler must trace the process that does the work.** eprof seeds a pid set
and `set_on_spawn` extends it to processes spawned DURING the trace -- not to one
spawned in the workload's setup. Every socket-event profile workload mounted its
live `gen_server` in setup and then profiled only the driver process, so the whole
server-side diff ran untraced and the trace showed the driver's share -- JSON
encoding -- as ~97% of a workload whose real time was overwhelmingly in the live
process. Two things gave it away: `arizona_diff` was absent from a diff-heavy
profile, and a direct micro-benchmark of the encode step measured 37 us where the
profile implied ~300. The workloads now seed `[self(), LivePid]` explicitly
(`profile_loop_server/4`); a profile of a message-passing path should be read with
"whose time is this?" asked first. (This is also a second instance of the
inflation trap above: the tiny json functions were charged ~8x their wall clock,
which is what let the driver's slice masquerade as the whole event.)

**The benchmark's call graph must match production's.** Every direction this can go
wrong has produced a wrong number in practice:

- *An extra caller.* Driving a changed clause through a realistic-looking 3-element
  list showed a 74 ns regression that vanished once the clause was measured alone --
  real cost 8-13 ns. The same setup produced a second phantom with a whole request as
  the caller. The surrounding walk contributes enough variance to manufacture a delta
  several times the size of the thing under test.
- *An extra callee.* A benchmark had the fast-path function delegate to the general one
  on a miss, a hop production does not make -- there the general clause is inline in the
  same function. That inflated the measured miss cost from ~1 ns to ~9 ns, and the wrong
  figure was quoted onward before anyone caught it.

"Isolate the function" is the usual fix, but it is the symptom rather than the rule: a
benchmark can be perfectly isolated and still measure a call graph production never
executes. Check both ends -- what calls it, and what it calls.

**A result does not transfer between modules without re-measuring, and the reason is
not just input size.** Two builders asked the same question -- is a tail-recursive
`<<Acc/binary, B>>` accumulator faster than consing an iolist and flattening once? --
and got different answers, both correct. Both curves cross, but at different places and
with different stakes:

| input size | `arizona_html:escape/2` | lowercasing header names (roadrunner) |
| ---------- | ----------------------- | ------------------------------------- |
| 5B | iolist | iolist |
| 10B | iolist | iolist |
| 20B | append (marginal) | iolist |
| 30B | -- | crossover |
| 50-100B | -- | append |
| 200B | append | append |
| 500B | append | append |

Two differences, and the second is the one that would have misled. The crossover sits
near 20 bytes for one and 30-50 for the other. And past it `escape/2`'s append pulls
away to roughly **twice** as fast (1170 ns against 2261 at 200 bytes) where the
lowercase walk's never exceeds ~16%. The reason is what each builder emits PER INPUT
BYTE: `escape/2` writes a multi-byte entity for a metacharacter, `ascii_lowercase_walk`
conses exactly one cell per byte. Same question, same winner past the crossover,
completely different stakes -- reading either curve off the other predicts the wrong
shape. Re-measure per function, not per question.

The full `escape/2` numbers, including a third variant that batches runs of ordinary
bytes into slices:

| input | binary append | iolist | iolist, sliced runs |
| ----- | ------------- | ------ | ------------------- |
| 1 metachar in 5B | 93 ns | **56** | 90 |
| 1 metachar in 10B | 116 ns | **86** | 115 |
| 1 metachar in 20B | 169 ns | 178 | **148** |
| 1 metachar in 200B | 1170 ns | 2261 | **558** |
| dense 200B | **1867 ns** | 3421 | 7582 |
| markup-ish 500B | **3267 ns** | 6190 | 6050 |

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

**The bench's `red/op` column is the standing form of that answer.** Every workload
row now ends with the per-op VM-wide `exact_reductions` count (minimum across
trials -- background processes only add, never subtract), and `bench-ab` diffs it
beside the wall clock. Reductions count WORK, not time: they are deterministic
where the clock is not (identical to the reduction across repeated runs, against
the ~10% wall floor), so they resolve exactly the "strictly less work" class of
change this document keeps having to file as unresolved. Validating the column on
the previously-unresolved dedup-sharing + walker pair settled all of it at once:
`stream_reset_with_overlap_100` **-10.5%** reductions (wall had said -1.6%),
`stream_update_field_100` -2.2% (confirming the wall reading), `ws_event_e2e`
exactly 0 (those changes don't touch that path -- a clean null control), and
`diff_simple_event` **+5 reductions** -- the sharing walk's changed-flag threading
has a real, tiny cost on single-op frames that no clock could see. The caveat is
the mirror of the strength: a reduction delta cannot see cache placement or
scheduling, so the clock stays the arbiter of whether the work was ever the
bottleneck.

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
| Key a stream's items and order in one walk | `reset/2`/`new/3` -30% at 100 items |
| Skip clearing a stream queue that is already empty | -8% per untouched stream, and no record rebuilt |
| Render a plain-list `?each` straight to output during SSR | `render_each_100` -18% |
| Guard the client's per-element hook scans | a 500-item slot re-render stops doing 1000 subtree queries |
| Encode the ops frame through a specialized walk | encode of a 100-op frame -56% (28.2 -> 12.3 us), byte-identical |
| Materialize the changed keys once per dynamics walk | the per-slot deps probe loses its iterator/key-list setup; -20-30% per check at every small-map shape |
| Ask the re-render estimate's floor before weighing items | skips the O(items x dynamics) weighing when the statics floor alone disqualifies wholesale |
| Fuse `render/2`'s triple unzip into its zip walk | one walk and two lists where there were two walks and four, per connect/navigate |
| Skip the fingerprint dedup walk for fp-less frames | strictly less work on every event workload: red/op -0.5% to -6.1%, resolved by the reductions column |

(The whole 2026-08 arc that added the last four -- PRs #759-#762 -- closed with
one paired `bench-ab` over every workload, benchmarks byte-identical on both
sides. Two labels resolve past the ~10% floor: `stream_reset_with_overlap_100`
**-19.2%** (329 -> 266 us) and `stream_reorder_100` **-18.2%** (4.4 -> 3.6 us)
-- the two whose frames carry enough ops for the encoder, deps-hoist and
estimate work to add up. Everything else sits under the floor, the e2e paths at
~0%, which is exactly what the framework-share bound below predicts: the arc's
changes live inside the 14.1%/9.8% slice. Reading either number off an earlier
partial pairing understates them; only the full-arc pairing shows the encoder
and the walk changes compounding on the ops-heavy labels.)

Two of these deserve their reasoning recorded, because both look like they *should*
be needed:

The **re-render estimate** decides whether a wholesale container re-render would ship
fewer bytes than incremental ops. It weighed every value of every item to do it --
and the wholesale side is never negative, so a patch no bigger than the floor cannot
beat it by the floor, whatever the items weigh. The answer is settled by the ops
alone. It was being computed on every drain, including the overwhelmingly common one
that patches a single field.

The **SSR each tree** was the one item on this list that had been written off as a
redesign, and it took a different kind of check to unblock: not a measurement but a
reachability argument. It built a `{Az, Value, Deps}` triple per dynamic, a list of
those per item and a list of those, which the walk that turns them into output reads
once and drops. That looked load-bearing because the same shape feeds
`render_fp_val/2` and the hydration payload -- until every consumer of it turned out
to live in `arizona_diff` and the LIVE render path, both of which keep a snapshot the
next diff reads. The SSR family keeps none, so on that path alone the tree is pure
overhead, and a `{ssr_rendered, _}` marker lets the items render as the walk reaches
them. Worth remembering as a technique: when a structure looks required, enumerate who
actually reads it before believing it.

The **slot `az` prefix** was built by `binary:replace/4` before looking at the value.
That is a pure-Erlang wrapper: it compiles the pattern, allocates a closure and copies
the binary, ~125ns, even when there is no colon to replace. Only a nested snapshot
ever uses the prefix; a scalar -- which is what nearly every content slot holds --
threw it away. Asking the value first drops that slot to ~6ns.

The **ops encoder** is worth a sentence on where the cost actually was. Generic
`json:encode/2` walked a 5.4 KB / 100-op frame at ~63 ns per output byte -- an
order of magnitude off raw JSON encoding -- because ops are many TINY values, and
each one paid the full dispatch (`op_encoder` -> `encode_value` -> type switch ->
escape setup), plus a fresh `<<ViewId:Az>>` binary built and escaped per op even
though a stream drain's ops all share one target. The specialized walk switches on
the type directly, emits through json's exported per-type functions (so the bytes
cannot drift -- a differential test holds the two encoders together), and memoizes
the scoped target across consecutive ops. Byte escaping itself was never the
problem; dispatch per tiny value was.

## What did not work

Recorded so they are not retried blindly.

**Eliminating `++` and `length/1` for their own sake.** A first pass removed nearly
all of them from `arizona_diff` (difference lists, single-pass counting, lockstep
length comparison). Every one is strictly less work and they were kept, but the
end-to-end effect was **not measurable** -- each was linear in a list whose
construction already cost more than the copy. The real win in the same code was the
gate above, which removed the work rather than making it cheaper.

**Fusing the kept-item reuse walk with the inner-ops diff.** The idea: on a reset,
`eval_or_reuse_per_item/4` already knows which positions it re-evaluated, so the
second lockstep walk (`diff_item_dynamics_v/3`) could run over just those pairs.
Measured before building it: the full 20-pair walk with 19 shared tuples costs
**78 ns per kept item** and a pairs-only walk 8 ns, so the fusion's ceiling is
~7 us on a 100-item reset -- 2.6%, under the noise floor -- at the price of
threading changed-pairs across the eval/diff seam. Not built. What the reading
DID pay for: the walk's threaded `Markerless` boolean was computed and never
read by any caller (the plain-list container routes markerless templates to the
wholesale fallback before per-item patching, and `list_changed/3` compares
values itself), so it is gone and the walk returns `{Ops, Views}`.

**The dedup-sharing walk and the estimate's closure-free walkers landed in the same
bucket.** `dedup_fps/2` now hands back the original term when a sub-walk stripped
nothing (no list cell or map rebuilt on the common all-scalar frame), and
`wire_bytes/1`/`item_value_bytes/1` walk by direct recursion instead of a
`lists:foldl` closure per element -- micro-measured **-30/-40%** on the walks
themselves, with exactness asserted. End to end, a 6-round paired `bench-ab` read
`stream_reset_with_overlap_100` **-1.6%** and `stream_update_field_100` **-2.2%**:
under the floor, unresolved. Kept on the `++` precedent above -- strictly less
work (and strictly less allocation per reply), no maintainability loss (the dedup
rewrite retired a duplicated walker) -- but do not quote them as wins. (The
follow-up skip -- `arizona_render:drain_fp_note/0` gating the walk outright --
later recovered even the sharing walk's +5-reduction cost on single-op frames,
and the reductions column resolved the whole family exactly.)

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

**Decorate-sort-undecorate in `arizona_stream:sort/2`.** Replacing the comparator's
two `maps:get/2` per comparison (~2*N*log2(N) lookups) with one lookup per element
wins only for large streams: **-37%** at N=1000, but **+139%** at N=33 and **+4%** at
N=100. The crossover is where the items map stops being a flat array and the lookups
start to cost, which is not a boundary worth encoding as a magic threshold -- gating
on `map_size > 32` was measured too and is exactly where the +139% lands. Left alone.

**Rewriting `clear_stream_pending/2` as a single iterator pass.** Replacing the
`stream_keys/1` comprehension plus the keyed walk with one `maps:next/1` walk measured
**+27% to +47%** across every shape tried, for the same reason as
`compute_item_changed/2`: a map comprehension is a compiled generator, stepping an
iterator from Erlang is not. Only the narrower half of that idea survived -- skipping
the rebuild when the queue is already empty.

**Shrinking the wire payload, and the roadrunner header walkers.** Both came out of
reading eprof percentages as wall clock; see the per-byte trap above. The header
walkers do contain real redundancy (a compile-time-lowercase literal being lowercased
on every request, a value walked once to validate and again to lowercase, Title-Case
names walked four times -- 46 of 81 per-request calls), but it lives in the
`roadrunner` dependency and is worth low single digits of the HTTP path, not the
11.5% the profile suggested. Its `check_header_safe/3` is the least inflated of the
four AND the security check: leave that one alone.

**Rewriting `arizona_html:escape/2`'s accumulator.** Prompted by a neighbouring
codebase measuring the opposite shape faster; see the transfer note above. The current
tail-recursive binary append is the right choice HERE and stays: it beats a per-byte
iolist everywhere past ~20 bytes, which is where template values live. A third variant
that batches runs of ordinary bytes into slices is genuinely faster for a long value
holding ONE metacharacter (1170 -> 558 ns at 200 bytes) but 4x slower on dense markup
(1867 -> 7582), and escape-dense values are exactly what escaping exists for. Note the
`first_meta/2` guard means none of this runs at all for a clean value.

**Stepping a map's own iterator instead of `maps:values/1`.** Tried inside the
re-render estimate. `maps:values/1` is one pass in C and won at 10 entries, tied at
1000; the comprehension plus `lists:sum/1` it replaced was the slowest of the three
(~30% worse at 100). Summing a `maps:values/1` walk directly is the shape that won.

## The client

The client had never been measured. It has **no layout thrashing** anywhere -- the only
geometry reads are one `scrollTo` per batch, after the op loop -- and its per-batch
caches (`els`, and the stream key maps) are well built. What was left was work
happening in the gaps between those caches, all of it per op rather than per batch:

- **Hook scans.** `mountHooks`/`destroyChildHooks` ran `querySelectorAll('[az-hook]')`
  per ELEMENT per op. A slot re-render of a 500-item list is 1000 subtree queries for
  one op, and an app with no hooks paid every one. Both now answer from the registry
  (`_hooks.size`, and a `for...in` over the defs) before scanning.
- **Discarded work.** `moveItemEl` computed `slotBounds` -- a walk over every child of
  the container -- unconditionally, and the dominant move (an `afterKey` whose element
  resolves) never read it. Now resolved in the branches that use it.
- **Nested containers had no cache.** Item ops passed `streams = null` down, so a
  stream inside a stream item fell back to `querySelector` per op: the O(N^2) the
  top-level key map exists to prevent. The batch's maps are keyed by container element,
  so threading them down is safe and gives nested containers their own entries.
- **No memo for inner lookups.** `applyItemOps` re-ran `resolveInnerEl` (a subtree
  `querySelector`, two on a miss) per op, where two ops on one item commonly target the
  same az. It now memoises like `applyOps` does, re-verifying `isConnected` on a hit.
- **`findMarker` built its comparison string inside the loop**, once per child visited.

Measured with jsdom, so treat the absolute numbers as inflated -- jsdom's
`querySelectorAll` is JS, a browser's is native -- but the shapes are structural. The
hook guard took a 500-item slot re-render from 3.2 ms of scanning to ~1 us.

**Measured and NOT taken:** `Date.now()` per resolved template in `touchFp` costs 23 ns
(11.7 us for a message carrying 500 templates). One reading per message would remove
it, but `resolveHtml` is called directly by tests as well as by the worker, so any
refresh entry point can be bypassed -- and a stale clock silently skews the fingerprint
cache's MRU eviction. Not worth that for 23 ns.

### Benchmarking the client -- `make bench-client`

`make bench-client` times `applyOps` in a real Chromium against fixtures generated from a
**real diff** (`scripts/client_fixture.escript`), printing a per-function breakdown beside a
raw-DOM floor. Not in `ci`, for the same reason `bench` is not.

| workload | ops | total | floor | ratio |
| -------- | --- | ----- | ----- | ----- |
| `stream_patch` (200 of 400 changed) | 200 `OP_ITEM_PATCH` | 0.154 ms | 0.040 ms | 3.8x |
| `stream_render` (400 of 400 changed) | 1 `OP_TEXT` | 1.12 ms | 1.04 ms | 1.1x |

**The full re-render path is already at the floor.** The batch runs at 1.1x a bare
`innerHTML` of the same fragment, with `parseFragmentIn` taking 83% of it. Nothing to win
there, and nothing to move to the worker either: parsing needs a DOM, and parsed nodes are not
transferable.

**Read the per-function breakdown as shares, not milliseconds.** Wrapping every internal costs
two `performance.now()` calls per call, which on `stream_patch` makes that pass ~4x slower than
the plain run it prints under (on `stream_render`, with ~8 wrapped calls, it is ~1.0x). The
proportions survive that overhead; the absolute figures do not, which is why the tool reports
shares -- quoting an instrumented millisecond next to a plain total is its own small version of
the denominator trap above.

It guards two traps, both of which produced confident numbers for work that never happened.

**Hand-written ops measure a workload the engine never emits.** The op shape is not guessable:
a bulk change collapses to ONE container `?OP_TEXT`, while a partial change emits per-item
`?OP_ITEM_PATCH` that all share the container's az. A benchmark inventing one op per element
reported 64% of its time in `querySelector` and implied an O(N^2) lookup problem -- the real
batch resolves its target **once**, and `buildKeyMap` runs once. This is the client form of
"the benchmark's call graph must match production's" above, so fixtures come from
`arizona_diff` rather than from imagination.

**An op that does not resolve is skipped, not failed.** A raw `arizona_diff:diff/4` target is
view-relative, and the client reads a colon-less target as a VIEW ID, so a batch missing the
scoping that `arizona_socket:flatten_ops/2` applies patches nothing and warns once per op.
Timing that measures `console.warn`: it read 2.4 ms/batch where the real figure is 0.158 ms,
and made framework overhead look like 98% of the batch. The harness refuses to print a number
unless the DOM visibly changed and the console stayed silent, and exits non-zero instead.

### Benchmarking the connect path -- `make bench-client-connect`

`make bench-client-connect` runs the real `connect()` and a bfcache-triggered reconnect in
Chromium. Fixtures carry the SSR page plus the exact frames a real `arizona_socket` emitted
for it -- the first-connect `{"a":...}`, the deferred-reconnect connect frame, and the resync
the server sends once the promised `cached_fps` arrives -- and the only substitution is a
stubbed zero-latency `WebSocket` in the worker, so the numbers isolate client-side boot cost.
The reconnect leg goes through the client's own `pagehide`/`pageshow` handlers, not internals.
Same refusal rules as `bench-client` (must reach `az-connected`, the resync must visibly
replace the view root, console must stay silent), plus a fresh browser context per run so the
worker's IndexedDB fingerprint cache is deterministically cold.

What the first measurements said (connect_page / connect_bulk_500, min over 20 runs):

- **The client's own connect work is sub-millisecond.** `connect()` returns in ~0.7 ms, and
  the instrumented breakdown (delegation bind, `handleEvent` per type, `noteAzAttrs`,
  `mountHooks`) is a fraction of that. Time-to-`az-connected` is ~9 ms, of which a bare
  do-nothing worker's spawn-to-first-message floor is ~3 ms -- the rest is the real worker's
  module chain and IndexedDB open, i.e. browser machinery, not framework code walking data.
- **The reconnect resync is parse-bound on big pages**, exactly like the applyOps
  full-render path: `parseFragmentIn` is ~60% of the 500-row resync. Form save/restore shows
  up on small pages only because everything else is tiny -- it is proportional one-shot work
  (~0.1 ms for one form) with nothing to guard.
- **The production bundle halves the source shape's number.** The tool times both:
  the committed `arizona.min.js` (one file, min worker beside it) reaches
  `az-connected` in ~4.8 ms where the two-fetch source shape reads ~9.0 ms -- the
  gap IS the module chain, so read the bundled block for absolute cost and the
  source block for like-for-like comparison across commits. Against the ~2.8 ms
  bare-worker floor, the shipped client's own boot contribution is ~2 ms.

## Where the time actually goes

Profiling the two paths a user experiences, rather than a synthetic workload, bounds
all of this. `arizona_*` modules are **14.1%** of a WebSocket event and **9.8%** of a
page load; the rest is the socket write, JSON, the HTTP server and gen_server
plumbing. Even an infinitely fast framework would remove only that much. Two
consequences, both measured:

- **The socket write is per-call, not per-byte** -- 3.55 us/call for a 59-byte patch
  against 3.20 us/call for a whole-view replace, at one write per event. Shrinking
  ops does not buy CPU. (It still buys client bandwidth: a one-field patch is 59
  bytes of which 27 are the two `az` strings.)
- **Broadcast is BEAM's message copy.** `arizona_pubsub:send_each/2` is 82% of the
  broadcast profile and is already `Pid ! Data` in a loop.

The remaining server-side candidates are all small, and the largest single
`arizona_*` function on the event path is 1.88%.

## Still open

Ranked by expected value. Nothing here has been measured end to end.

1. **Compile-time `{get, Key}` descriptors for per-item dynamics.** A re-rendered
   stream item allocates ~20 closures through the template's `d` fun before the
   reuse walk drops most of them; dynamics that are pure `get(K, Item)` reads
   could compile to a descriptor evaluated directly. Parse-transform + eval
   surgery with an uncertain win -- gate it on a real-app profile showing
   item-eval dominance, not on the synthetic reset workload.
2. **`arizona_diff`'s four remaining appends** -- three are stream containers whose
   drain runs before the walk that would supply a tail (the drain feeds it the views
   it rendered, and reordering would reorder `$arizona_update_effects`, which ships
   in evaluation order); the fourth is in `stream_reset/8`, where the moves and the
   rest of the drain both need the snapshot the item walk produces. All four cons
   onto an empty tail in the common case, which `append_ops/2` already answers for
   free.
