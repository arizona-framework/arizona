#!/usr/bin/env bash
#
# Profile a workload at any git commit without touching the working tree.
#
# Uses a `git worktree` under `_build/prof-at-<sha>/` so the current
# checkout (with its uncommitted edits, build cache, etc.) is left
# alone. The worktree is cached across runs -- subsequent `prof_at` of
# the same commit reuses the compiled artifacts.
#
# Usage:
#   ./scripts/prof_at.sh <commit-ish> [profile.escript args...]
#
# Examples:
#   # Profile a workload at the parent of HEAD (typical A/B baseline).
#   ./scripts/prof_at.sh HEAD~1 --only render_each_100 --ops 200
#
#   # Profile a specific SHA.
#   ./scripts/prof_at.sh 9fe7497 --only render_view_page
#
#   # Run all curated workloads at a tag.
#   ./scripts/prof_at.sh v0.5.0
#
# Cleanup:
#   git worktree remove _build/prof-at-<sha>
#   git worktree prune

set -euo pipefail

if [ $# -lt 1 ]; then
    echo "usage: $0 <commit-ish> [profile.escript args...]" >&2
    exit 1
fi

COMMIT="$1"
shift

SHA=$(git rev-parse --short "$COMMIT")
WORKTREE="_build/prof-at-$SHA"

if [ ! -d "$WORKTREE" ]; then
    echo "==> creating worktree $WORKTREE @ $COMMIT" >&2
    git worktree add --detach "$WORKTREE" "$COMMIT" >&2
else
    echo "==> reusing cached worktree $WORKTREE" >&2
fi

cd "$WORKTREE"
rebar3 as test compile >&2
./scripts/profile.escript "$@"
