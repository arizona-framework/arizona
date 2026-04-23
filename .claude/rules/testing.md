---
description: Test structure, handler modules, CT suites, and E2E conventions
paths:
  - "test/**"
  - "e2e/**"
---

# Testing Conventions

## EUnit

All unit tests use plain `_test()` functions (no generators/fixtures). Parse transform tests in `test/arizona_parse_transform_test.erl`. Render/diff tests in inline `-ifdef(TEST)` blocks within source modules.

## E2E tests

Playwright, split into two directories:
- `e2e/parallel/` -- `arizona_page.spec.js`, `arizona_datatable.spec.js`, `arizona_mixed_children.spec.js` (run in parallel)
- `e2e/sequential/` -- `arizona_chat.spec.js` (runs with `workers: 1` to avoid pg channel leaks between tests)

## Common Test suites

- `arizona_app_SUITE.erl` -- application boot: env-driven server + reloader wiring
- `arizona_watcher_SUITE.erl` -- file watcher: events, debounce, patterns, broadcast
- `arizona_reloader_SUITE.erl` -- dev reloader: pubsub join/broadcast, watcher integration
- `arizona_pubsub_SUITE.erl` -- pubsub: subscribe, broadcast, isolation, cleanup

## Test handler modules (in `test/support/`)

Handlers pick one of the three header forms:

- `arizona_view.hrl` -- route-level pages (`mount/2` taking Bindings + Request)
- `arizona_stateful.hrl` -- embeddable components (`mount/1`, instantiated via `?stateful(...)` in a parent template)
- `arizona_stateless.hrl` -- pure template modules

### Route-level views (`arizona_view.hrl`)
- `arizona_page.erl` -- page with 3 stateful counter children, connected status
- `arizona_about.erl` -- about page with `handle_info/2` tick timer, `az-hook="Tick"`, SPA navigation
- `arizona_crashable.erl` -- crash fixtures: `mount`, `handle_event`, and `handle_info` crash paths
- `arizona_chat.erl` -- pubsub cross-tab messaging, stream-based, owner-guarded delete
- `arizona_datatable.erl` -- stream sort/move/reset, 5 initial rows
- `arizona_mixed_children.erl` -- stateless + dynamic children, az numbering correctness
- `arizona_scroll_home.erl` / `arizona_scroll_about.erl` -- dedicated E2E scroll fixtures (tall content + hash anchor + replace-nav button). Routed at `/scroll-home` and `/scroll-about`; kept off the demo nav to avoid polluting the other E2E cases.
- `arizona_todo.erl` -- stream operations (add/remove/update/clear/move/insert_at/reset_with)
- `arizona_timer.erl` -- `handle_info/2` with set_message/set_message_with_effect
- `arizona_effectful.erl` -- dispatch_event effects (notify/multi/noop)

### Embeddable stateful components (`arizona_stateful.hrl`)
- `arizona_counter.erl` -- simple counter (inc/dec/reset), `handle_update/2` with counter2 special case
- `arizona_no_diff_counter.erl` -- `az-nodiff` directive with stateful children; embedded via `arizona_template:stateful/2` in `arizona_diff_SUITE`
- `arizona_stream_with_child.erl` -- stream items each wrap an embedded `arizona_counter`

### Stateless templates (`arizona_stateless.hrl`)
- `arizona_layout.erl` -- stateless layout with `?html`/`az_nodiff`, render/2 with InnerContent

### Other
- `arizona_parse_transform_test.erl` -- parse transform EUnit tests
- `arizona_req_test_adapter.erl` -- test double for `arizona_req` used in view SSR tests
