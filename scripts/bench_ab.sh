#!/usr/bin/env bash
#
# Compare one benchmark workload across two commits, paired.
#
# Runs `bench.escript` alternately at both refs, round by round, and reports the
# MINIMUM per-op p50 each ref reached. Alternating cancels machine drift (a
# background build, a thermal ramp) that hits whichever ref happens to run first;
# the minimum discards the rounds that lost the CPU rather than averaging them in.
#
# Each ref is built in its own `git worktree` under `_build/bench-at-<sha>/`
# (cached across runs, like `prof_at.sh`). That is not just for isolation: it
# gives each ref its own `_build`, which is what makes the comparison honest.
# Swapping sources in one checkout instead lets rebar3 skip a recompile whenever
# the copy lands in the same wall-clock second as the previous build -- the
# benchmark then measures the ref it already had loaded, silently.
#
# Usage:
#   ./scripts/bench_ab.sh <ref-a> <ref-b> [--rounds N] [--runs N] [bench args...]
#
# Examples:
#   ./scripts/bench_ab.sh main HEAD --only stream_update_field_100
#   ./scripts/bench_ab.sh HEAD~1 HEAD --rounds 6 --runs 120 --only render_each_100
#
# Cleanup:
#   git worktree remove _build/bench-at-<sha>
#   git worktree prune
#
# Read the numbers with the noise floor in mind: a difference under ~10% on a
# workload dominated by many tiny calls is not necessarily real. See
# docs/performance.md.

set -euo pipefail

if [ $# -lt 2 ]; then
    echo "usage: $0 <ref-a> <ref-b> [--rounds N] [--runs N] [bench args...]" >&2
    exit 1
fi

REF_A="$1"
REF_B="$2"
shift 2

ROUNDS=4
RUNS=80
BENCH_ARGS=()
while [ $# -gt 0 ]; do
    case "$1" in
        --rounds) ROUNDS="$2"; shift 2 ;;
        --runs) RUNS="$2"; shift 2 ;;
        *) BENCH_ARGS+=("$1"); shift ;;
    esac
done

ROOT=$(git rev-parse --show-toplevel)
cd "$ROOT"

# Pin to a fixed core set when the platform offers it: scheduler migration is a
# large share of the run-to-run spread on a busy machine.
PIN=()
if command -v taskset >/dev/null 2>&1; then
    PIN=(taskset -c 4,5,6,7)
fi

prepare() {
    local ref="$1" sha worktree
    sha=$(git rev-parse --short "$ref")
    worktree="_build/bench-at-$sha"
    if [ ! -d "$worktree" ]; then
        echo "==> creating worktree $worktree @ $ref" >&2
        git worktree add --detach "$worktree" "$ref" >&2
    fi
    (cd "$worktree" && rebar3 as test compile >&2)
    echo "$worktree"
}

WT_A=$(prepare "$REF_A")
WT_B=$(prepare "$REF_B")

RESULTS=$(mktemp)
trap 'rm -f "$RESULTS"' EXIT

for round in $(seq 1 "$ROUNDS"); do
    echo "==> round $round/$ROUNDS" >&2
    for side in a b; do
        case "$side" in
            a) wt="$WT_A"; ref="$REF_A" ;;
            b) wt="$WT_B"; ref="$REF_B" ;;
        esac
        (cd "$wt" && "${PIN[@]}" ./scripts/bench.escript "$RUNS" "${BENCH_ARGS[@]}" 2>/dev/null) \
            | awk -v ref="$ref" '
                # bench.escript rows: label mean unit stdev unit p50 unit p99 unit ops
                NF >= 9 && $2 ~ /^[0-9.]+$/ {
                    v = $6; if ($7 == "\xc2\xb5s") v = v * 1000; else if ($7 == "ms") v = v * 1000000
                    print ref, $1, v
                }' >> "$RESULTS"
    done
done

echo
awk '
    { key = $2; if (!($1 SUBSEP key in min) || $3 < min[$1, key]) min[$1, key] = $3
      refs[$1] = 1; labels[key] = 1 }
    END {
        n = 0; for (r in refs) { n++; order[n] = r }
        if (n != 2) { print "expected two refs"; exit 1 }
        a = order[1]; b = order[2]
        printf "%-32s %14s %14s %9s\n", "workload", a, b, "delta" > "/dev/stderr"
        for (l in labels) {
            x = min[a, l]; y = min[b, l]
            printf "%-32s %11.0f ns %11.0f ns %8.1f%%\n", l, x, y, (y - x) * 100 / x
        }
    }' "$RESULTS" | sort

# A cross-commit run compares the BENCHMARKS too: each worktree carries its own
# scripts/ and test/support/, so a workload whose fixture or definition changed
# between the refs is measuring two different amounts of work. Check `git diff
# <ref-a> <ref-b> -- scripts/bench.escript test/support/` before trusting a
# surprising number.
