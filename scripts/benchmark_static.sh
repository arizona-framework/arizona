#!/usr/bin/env bash

# Arizona Static Site Generator Benchmark Script

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

cd "$PROJECT_ROOT"

echo "🚀 Starting Arizona Static Site Generator Benchmark..."
echo

# Default values
ROUTE_COUNT="${1:-100}"
TIMEOUT="${2:-30000}"
ITERATIONS="${3:-3}"

# Build assets files
if ! npm run build; then
    exit 1
fi

# Compile using the test profile
if ! rebar3 as test compile; then
    exit 1
fi

# Start Erlang shell and run benchmark
echo "⚡ Running benchmark..."
echo "   Routes: $ROUTE_COUNT"
echo "   Timeout: ${TIMEOUT}ms"
echo "   Iterations: $ITERATIONS"
echo

exec erl \
    -pa "_build/test/lib/arizona/ebin" \
    -pa "_build/test/lib/arizona/test" \
    -pa "_build/test/lib/fs/ebin" \
    -pa "_build/test/lib/cowboy/ebin" \
    -pa "_build/test/lib/cowlib/ebin" \
    -pa "_build/test/lib/ranch/ebin" \
    -pa "_build/test/lib/gun/ebin" \
    -pa "_build/test/lib/syntax_tools/ebin" \
    -sname arizona_benchmark \
    -setcookie framework \
    ${ERLANG_EXTRA_ARGS:-} \
    -eval "
        arizona_static_benchmark:run(#{
            route_count => $ROUTE_COUNT,
            timeout => $TIMEOUT,
            iterations => $ITERATIONS
        }),
        init:stop().
    " \
    -noshell

echo "✅ Benchmark completed!"