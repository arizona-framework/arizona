MAKEFLAGS += -j$(shell nproc)

# Force bash so recipes using `set -o pipefail` (check-hank, check-doc) work
# on systems where /bin/sh is dash (Debian/Ubuntu CI runners).
SHELL := /bin/bash


.PHONY: all start ci precommit compile \
	fmt fmt-erl fmt-js \
	lint \
	check check-dirty check-fast check-erl check-fmt check-lint check-hank check-xref check-dialyzer check-js \
	build-js analyze-js \
	test test-eunit test-ct test-erl test-js test-e2e \
	bench bench-client bench-client-connect  \
	cover cover-erl cover-js \
	doc doc-erl doc-js \
	setup-e2e clean

all: start

start: build-js
	./scripts/start_test_server.sh

ci:
	$(MAKE) compile build-js
	$(MAKE) check-dirty
	$(MAKE) check-erl check-js
	$(MAKE) test-erl test-js
	$(MAKE) test-e2e
	$(MAKE) cover
	$(MAKE) doc

check-dirty:
	@git diff --quiet || (echo "Error: uncommitted changes after build (run make precommit and commit artifacts)" && git diff --name-only && exit 1)

precommit:
	$(MAKE) fmt
	$(MAKE) compile build-js
	$(MAKE) check-fmt check-js
	$(MAKE) test-erl test-js

compile: compile-test
	rebar3 compile

compile-test:
	rebar3 as test compile

fmt: fmt-erl fmt-js

fmt-erl:
	rebar3 fmt

fmt-js:
	npx biome format --write

check: check-erl check-js check-md check-yaml check-actions

check-fast: check-fmt check-js check-md

check-erl:
	$(MAKE) check-fmt
	$(MAKE) check-lint
	$(MAKE) check-hank
	$(MAKE) check-xref
	$(MAKE) check-dialyzer

check-fmt:
	rebar3 fmt --check

check-lint:
	rebar3 lint

check-hank:
	@# NOT `| tee /dev/stderr |`: when stderr is a redirected regular file
	@# (`make ci > build.log 2>&1`), tee opens it with O_TRUNC and wipes every
	@# line logged before this point, leaving a NUL-filled hole. Capture instead,
	@# so the output is both shown and grepped without touching the log's fd.
	@# Fails on any finding (rebar3 hank exits non-zero) and on a stale ignore,
	@# which hank only warns about.
	@out=$$(rebar3 hank 2>&1); status=$$?; printf '%s\n' "$$out"; \
		[ $$status -eq 0 ] && ! printf '%s\n' "$$out" | grep -q "no longer needed"

check-xref:
	rebar3 xref

check-dialyzer:
	rebar3 as test dialyzer

check-js:
	npx biome check
	npx -p typescript tsc --noEmit --allowJs --checkJs --strict --target es2020 --module node16 --moduleResolution node16 assets/js/arizona.js assets/js/arizona-static.js assets/js/arizona-core.js assets/js/arizona-worker.js assets/js/arizona-reloader.js

check-md:
	npm run check:md

check-yaml:
	npm run check:yaml

check-actions:
	npm run check:actions

build-js:
	npx vite build

analyze-js:
	ANALYZE=true npx vite build

test: test-erl test-js test-e2e

test-eunit:
	rebar3 as test eunit

test-ct:
	rebar3 as test ct

test-erl:
	rebar3 as test test

test-js:
	npx vitest run

test-e2e:
	npx playwright test

test-e2e-parallel:
	npx playwright test --project parallel

test-e2e-sequential:
	npx playwright test --project sequential --no-deps

test-e2e-json:
	npx playwright test --project json

# Performance bench. Intentionally NOT wired into ci/precommit:
# numbers are noisy under shared CI runners and need human comparison.
# Compile under the test profile so test/support/ fixtures are on the path.
# Pass extra args via ARGS, e.g.:
#   make bench ARGS="--only diff_no_change --only diff_simple_event"
bench: compile-test
	./scripts/bench.escript $(ARGS)

# Client-side benchmark: `applyOps` timed in a real Chromium, against fixtures
# generated from a REAL diff rather than hand-written ops (the op SHAPE is not
# guessable -- a bulk change collapses to one container OP_TEXT, a partial one
# emits per-item OP_ITEM_PATCH sharing the container's az). Refuses to report
# unless the patch visibly applied with no warnings, because an op whose target
# does not resolve is skipped with a console.warn and would otherwise be timed as
# if it were work. Not in ci, for the same reason `bench` is not.
#   make bench-client ARGS="--only stream_patch"
bench-client: compile-test
	@mkdir -p _build/client_fixtures
	./scripts/client_fixture.escript _build/client_fixtures
	node ./scripts/bench_client.mjs _build/client_fixtures $(ARGS)

# Client connect/hydration benchmark: the real connect() + bfcache reconnect in
# Chromium against fixtures from a real socket, with only the WebSocket stubbed
# (zero latency), so the numbers isolate client-side boot cost. Refuses to
# report a boot that never reached az-connected or logged to the console.
#   make bench-client-connect ARGS="--only connect_page"
bench-client-connect: compile-test
	@mkdir -p _build/client_fixtures
	./scripts/client_fixture.escript _build/client_fixtures
	node ./scripts/bench_client_connect.mjs _build/client_fixtures $(ARGS)

# Paired A/B of one workload across two commits. Builds each ref in its own
# worktree (so neither can silently measure the other's beams), interleaves the
# rounds, and reports the minimum each ref reached. Also not in ci.
#   make bench-ab REFS="main HEAD" ARGS="--only stream_update_field_100"
bench-ab:
	./scripts/bench_ab.sh $(REFS) $(ARGS)

# Performance profile (eprof/fprof). Same caveat as bench: developer
# tool, not auto-gated. Pass extra args via ARGS, e.g.:
#   make prof ARGS="--only diff_simple_event"
#   make prof ARGS="--only render_view_page --tool fprof"
prof: compile-test
	./scripts/profile.escript $(ARGS)

# Profile any commit-ish (branch, tag, SHA, HEAD~N) without touching
# the working tree, via git worktree cached under _build/prof-at-<sha>/.
# Use for A/B comparisons against an uncommitted change without manual
# stash/pop. Examples:
#   make prof-at REF=HEAD~1 ARGS="--only render_each_100 --ops 200"
#   make prof-at REF=main   ARGS="--only render_view_page --ops 1000"
prof-at:
	./scripts/prof_at.sh $(REF) $(ARGS)

cover: cover-erl cover-js

cover-erl:
	rebar3 as test check_cover

cover-js:
	npx vitest run --coverage

doc: doc-erl doc-js

doc-erl:
	@# Same O_TRUNC hazard as check-hank -- and worse here, because `doc` is the
	@# LAST stage of `ci`, so a redirected full-CI log kept only these few lines
	@# and read like the pipeline had short-circuited. Capture instead of tee.
	@# Compile first, outside the capture: on a fresh _build `rebar3 doc` would
	@# build the deps inside it, and a dep's own compiler warnings (which our
	@# warnings_as_errors does not cover) would fail the doc check.
	rebar3 compile
	@set -o pipefail; out=$$(rebar3 doc 2>&1); printf '%s\n' "$$out"; \
		printf '%s\n' "$$out" | (! grep -q "warning")

doc-js:
	@echo "No JS docs configured yet"

setup-e2e:
	npm install
	npx playwright install chromium

clean:
	rebar3 clean
	rm -rf _build
	rm -rf priv/static
