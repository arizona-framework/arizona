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
	bench \
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
	@set -o pipefail; rebar3 hank 2>&1 | tee /dev/stderr | \
		(! grep -q "no longer needed")

check-xref:
	rebar3 xref

check-dialyzer:
	rebar3 as test dialyzer

check-js:
	npx biome check
	npx -p typescript tsc --noEmit --allowJs --checkJs --strict --target es2020 --module node16 --moduleResolution node16 assets/js/arizona.js assets/js/arizona-core.js assets/js/arizona-worker.js assets/js/arizona-reloader.js

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
	npx playwright test --project sequential

# Performance bench. Intentionally NOT wired into ci/precommit:
# numbers are noisy under shared CI runners and need human comparison.
# Compile under the test profile so test/support/ fixtures are on the path.
# Pass extra args via ARGS, e.g.:
#   make bench ARGS="--only diff_no_change --only diff_simple_event"
bench: compile-test
	./scripts/bench.escript $(ARGS)

# Performance profile (eprof/fprof). Same caveat as bench: developer
# tool, not auto-gated. Pass extra args via ARGS, e.g.:
#   make prof ARGS="--only diff_simple_event"
#   make prof ARGS="--only render_view_page --tool fprof"
prof: compile-test
	./scripts/profile.escript $(ARGS)

cover: cover-erl cover-js

cover-erl:
	rebar3 as test check_cover

cover-js:
	npx vitest run --coverage

doc: doc-erl doc-js

doc-erl:
	@set -o pipefail; rebar3 doc 2>&1 | tee /dev/stderr | \
		(! grep -q "warning")

doc-js:
	@echo "No JS docs configured yet"

setup-e2e:
	npm install
	npx playwright install chromium

clean:
	rebar3 clean
	rm -rf _build
	rm -rf priv/static
