---
name: profile
description: Profile Arizona hot paths with eprof/fprof. Use when investigating performance or picking the next optimization.
argument-hint: [workload_label]
allowed-tools: Read, Bash, Grep, Glob
---

Profile an Arizona workload to find hot paths and propose the next
optimization. If `$ARGUMENTS` is empty, profile the full curated set
(`render_view_page`, `render_each_100`, `diff_simple_event`,
`stream_reorder_100`).

## 1. Run the profile

Pick `--ops` based on the workload's per-op cost so total profile time
is in the 20–100ms range (enough signal, not so much that the analysis
itself dominates):

| Workload | Suggested `--ops` |
|----------|-------------------|
| `render_view_page` | 1000 |
| `render_view_with_layout` | 1000 |
| `render_each_100` | 200 |
| `stream_reorder_100` | 200 |
| `diff_simple_event` | 5000 |

```bash
make prof ARGS="--only $ARGUMENTS --ops 1000 --min-ms 0.5"
```

The `--min-ms 0.5` threshold filters out rows below 0.5ms total — keeps
the table readable. Drop to `--min-ms 0.2` if the workload runs short.
Profile output also lands at `/tmp/arizona_profile_<label>.log`.

## 2. Categorise the top 5–10 rows

Group functions by what they're doing — that's how you pick a target:

- **HTML escape**: `binary:replace/4`, `binary:matches/3`, `binary:do_replace/4`, `binary:part/2` →
  `arizona_js:escape_attr/1` and friends
- **JSON encode**: `json:escape_binary/5`, `json:string/7`, `json:do_encode/2`, `json:list_loop/2` →
  `arizona_socket:encode/1` reply path
- **List comprehension closures**: anything with `'-Fn/Arity-lc$^N/M-K-'/Arity` or `zlc$` →
  per-item iteration overhead
- **Per-dynamic dispatch**: `arizona_eval:eval_one/1`, `eval_val/1`,
  `arizona_render:render_ssr_one/1`, `render_ssr_val/1`
- **Tree walks**: `arizona_render:zip/2`, `arizona_template:to_bin/1`,
  `unwrap_val/1`
- **OTP framework**: `gen:do_call/4`, `erlang:demonitor/2`,
  `erlang:integer_to_binary/1` — usually intrinsic, hard to optimize
- **gen_server roundtrip**: `gen:do_call/4` ≥ 5% in event workloads
  signals the live process call cost; usually not addressable without
  re-architecting

Note the totals row for before/after comparison (you'll re-run after
changes to confirm the win).

## 3. Identify the candidate

Pick the highest-`%` row that's in **Arizona code** (not OTP). For each
candidate, find the call site and decide whether the cost is:

- **Intrinsic** (algorithmic — the function has to do this work) → not
  worth optimizing without changing the algorithm
- **Accidental** (intermediate list, double walk, redundant escape, eager
  eval where compile-time would do) → fixable

Quick lookup:

```bash
grep -rn 'function_name' src/ test/support/
```

Common accidental patterns:

- **Double walk**: `Vals = f(Xs), [g(X, V) || X <- Xs && V <- Vals]` →
  fuse to `[g(X, f_one(X)) || X <- Xs]` (we did this for
  `render_ssr_val/1`).
- **Eager binary build for static input**: `escape(json_encode(Cmd))`
  where `Cmd` is a literal → fold at compile time in
  `arizona_parse_transform.erl` (we did this for static `arizona_js`
  commands).
- **Map allocation in non-tracking path**: `[{Az, V, #{}} || ...]` where
  the `#{}` deps map is never read → drop it.

## 4. Deeper detail (optional)

If the eprof flat profile isn't enough — e.g. a function shows 10% but
you can't see what it's calling — rerun with fprof for OWN/ACC time and
the call hierarchy:

```bash
make prof ARGS="--only $ARGUMENTS --tool fprof --ops 200"
```

fprof output is verbose. Read the `totals` block first, then sort by
`OWN` time and trace upwards from the heaviest leaf MFAs.

## 5. Cross-check with the wall-clock bench

Before claiming a win, confirm the profile-suggested change hits the
wall clock too. Use `make bench` (real ns/op, multi-trial stats):

```bash
make bench ARGS="--only $ARGUMENTS 30"
```

The bench is intentionally NOT auto-gated (variance from shared CI
runners). Compare mean ns/op before vs. after by hand.

## 6. Plan the change

State the hypothesis explicitly:

> Function `M:F/A` is N% of `<workload>` total. The cost is accidental
> because <reason>. Replacing it with `<edit>` should drop it to ~M% and
> shave ~X% off the total.

Then propose **one** concrete edit. Per the user's preference, don't
bundle multiple unrelated optimizations — ship one change, re-measure,
then plan the next.

## Reference files

- Profile script: `scripts/profile.escript`
- Profiler module: `test/support/arizona_profiler.erl`
- Workload registry: `scripts/profile.escript` `profilers/0`
- Bench harness: `test/support/arizona_bench_lib.erl`
- Bench script: `scripts/bench.escript`
- Architecture map: `docs/architecture.md`

## Adding a new workload to profile

If the hot path you want lives outside the four curated workloads:

1. Mirror an existing `bench_*` workload from `scripts/bench.escript`
   (typically the one named identically). The bench setup logic is the
   right shape for profiling too.
2. Add a `prof_<label>/2` function in `scripts/profile.escript` that
   calls `profile_loop/3` with the inner op fun.
3. Register it in `profilers/0`.

Keep the curated set small — this is for hot-path investigation, not
exhaustive coverage.
