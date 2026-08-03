---
description: Test structure, handler modules, CT suites, and E2E conventions
paths:
  - "test/**"
  - "e2e/**"
---

# Testing Conventions

Four layers, in the order to reach for them:

| Layer | Where | Run |
|-------|-------|-----|
| Common Test (default) | `test/*_SUITE.erl` (47 suites) | `make test-ct`, `rebar3 ct --suite=arizona_diff_SUITE` |
| Inline EUnit (private fns only) | `-ifdef(TEST)` blocks in `src/*.erl` | `make test-eunit`, `rebar3 eunit --module=arizona_socket` |
| Playwright E2E | `e2e/parallel`, `e2e/sequential`, `e2e/native` | `make test-e2e` |
| Vitest (client JS) | `assets/js/*.test.js` | `make test-js` |

**New tests go in a CT suite.** Inline EUnit exists only to reach module-private functions;
today that is seven modules (`arizona_js`, `arizona_render`, `arizona_crypto`, `arizona_eval`,
`arizona_error_page`, `arizona_socket`, `arizona_effect`). There is no `test/*_test.erl`
module -- parse-transform tests live in `test/arizona_parse_transform_SUITE.erl`, a CT suite.

## Suite conventions

- One suite per module under test, named `<module>_SUITE`. Group related cases with
  `groups/0`, and mark a group `[parallel]` when its cases share no global state.
- Test helpers expose values; assert with the stdlib macros (`?assertMatch` with a `when`
  guard for anything structured, `?assertEqual` for scalars).
- `test/support/` is on the code path under the `test` profile. Fixtures live there, never
  inside a suite.

### Binding a listener -- always `arizona_test_port:pick/0`

Any suite that starts a real listener (HTTP, WebSocket, MCP) **must** take its port from
`arizona_test_port:pick/0`. It binds an ephemeral port (`0`), reads it back, and releases it,
so nothing is deterministic across VM restarts. Do **not** hand-roll a `Base + counter`
scheme: the counter restarts at 1 in every fresh VM, so the first listener of every run lands
on the same port -- colliding with a prior run's socket in `TIME_WAIT` and with any parallel
checkout of this repo, which surfaces as `eaddrinuse` in `init_per_suite` and then cascades
into unrelated suites through a half-started application. `pick/0` also re-rolls past the
WHATWG "bad ports" ceiling (10080), since a browser -- and the MCP SDK client the conformance
suite drives -- refuses to connect to one of those.

Current users: `arizona_app_SUITE`, `arizona_ws_SUITE`, `arizona_mcp_e2e_SUITE`,
`arizona_mcp_conformance_SUITE`, `arizona_bench_lib`. A suite whose server assigns the port
itself (`arizona_terminal_ssh_SUITE` reads it back from `ssh:daemon_info/1`) needs nothing.

## E2E tests

Playwright, split into projects/directories, all served by
`scripts/start_test_server.sh` on `$PORT` (default 4041):

- `e2e/parallel/` -- 18 specs, `fullyParallel`: `arizona_page`, `arizona_datatable`,
  `arizona_mixed_children`, `arizona_inline`, `arizona_params`, `arizona_patch`,
  `arizona_session`, `arizona_transition`, `arizona_middleware_halt`,
  `arizona_form_submitter`, `bfcache`, the fetch specs (`arizona_fetch_push`,
  `arizona_fetch_error`), the `?local` specs (`arizona_local`, `arizona_local_app`,
  `arizona_local_nested`), `arizona_stream_siblings` (a stream `?each` among static
  siblings -- the shape whose container render must patch through the slot marker), and
  `arizona_os` -- the native-shell (OS) capability seam, driven
  against the real client with a fake `window.__arizona_os__` installed via
  `page.addInitScript` (the Electron-preload equivalent).
- `e2e/sequential/` -- 3 specs, `workers: 1`: `arizona_chat`, `arizona_drain`,
  `arizona_fetch_account`. The serialization is for the **drain** spec, which soft-drains the
  whole listener and remounts every live view on the server, so it must not overlap the
  others. Serializing buys ordering, not state isolation: the e2e server starts once for the
  whole run, so anything keyed globally outlives every test -- `arizona_chat` therefore scopes
  its pubsub channel to the `:room` path segment and each test visits a fresh random room.
- `e2e/native/` -- 11 specs: the `?native` (JSON) wire e2e. A real WebSocket client, no browser
  (`e2e/utils/native_client.js`), driving the native views over the live server.

`make test-e2e-parallel` / `test-e2e-sequential` / `test-e2e-native` run one project.

## `test/support/` (128 modules)

Do not read it as a list -- read it as categories. A new fixture joins one of these and
follows its naming.

| Category | Naming | Header | Notes |
|----------|--------|--------|-------|
| Route-level live pages | `arizona_<page>` | `arizona_stateful.hrl` | Routed in `arizona_test_server:routes/0` |
| Embeddable stateful components | `arizona_<thing>` | `arizona_stateful.hrl` | Instantiated via `?stateful(...)`; never their own live process |
| Stateless templates / layouts | `arizona_<name>`, `arizona_*_layout` | `arizona_stateless.hrl` | Pure `render/1` |
| HTTP controllers | `arizona_*_controller` | plain module | Reached by `arizona_js:fetch/2`; export the action (default `handle/1`) |
| `?native` render target | `arizona_native_*` | `arizona_stateful.hrl` | Drive the `e2e/native` specs and `arizona_native_SUITE` |
| Terminal render target | `arizona_term_*` | `arizona_stateful.hrl` | `?terminal` views plus drivers for the TTY/SSH suites |
| MCP servers | `arizona_mcp_*_server` | `-behaviour(arizona_mcp)` | Minimal / crashing / unencodable variants for the MCP suites |
| `?local` fixtures | `arizona_local*`, `*_local` | either | Client-owned slots: content, attribute, nested, XSS |
| Parse-transform + diff shapes | `arizona_conditional_*`, `arizona_each_*`, `arizona_nested_*`, ... | either | One module per compile/diff shape under test |
| Benchmarks | `arizona_bench_*` | `arizona_stateful.hrl` | Workloads for `make bench` (`scripts/bench.escript`) |
| Test infrastructure | see below | plain modules | Machinery, not fixtures |

Test infrastructure, by what each is for:

- `arizona_test_port` -- free-port picker (mandatory, see above).
- `arizona_test_server` -- the E2E/dev server: `start/0`, `stop/0`, and `routes/0` (exposed so
  `arizona_dev_mcp`'s `list_routes` tool can introspect the live routes).
- `arizona_test_log_handler` -- a `logger` handler that forwards each event to a test pid, so
  a suite can assert a specific log was emitted.
- `arizona_req_test_adapter` -- stub `arizona_req` adapter. `new/0` returns the canonical test
  request used for every `arizona_live:start_link` call in the suites, and its
  `resolve_route/3` reads a `routes` map off the raw value, so route wiring is testable
  without a listener.
- `arizona_failing_session_store` -- an `arizona_session_store` whose read always fails, for
  asserting that a store failure is observable and distinct from a genuinely absent session.
- `arizona_bench_lib` / `arizona_profiler` -- harnesses for `scripts/bench.escript` and
  `scripts/profile.escript` (`make bench`, `make prof`).

## Handler fixture rules

Handlers pick one of the two header forms:

- `arizona_stateful.hrl` -- live handlers: route-level pages and embeddable components
  (`mount/1`; pages get request data as bindings via `arizona_middleware:extract/1`
  middlewares, components are instantiated via `?stateful(...)` in a parent template).
- `arizona_stateless.hrl` -- pure template modules (`render/1` only).

**Mount rule:** `mount/1` builds a **fresh** map literal and pulls each accepted override via
`maps:get/3`. Never `maps:merge(Defaults, Bindings)` or `Bindings#{...}` -- a navigate carries
arbitrary keys from previously visited pages, and merging lets a foreign key collide with a
handler-owned default.

### Route-level pages (`arizona_stateful.hrl`)
- `arizona_page.erl` -- page with 3 stateful counter children, connected status
- `arizona_about.erl` -- about page with `handle_info/2` tick timer, `az-hook="Tick"`, SPA navigation
- `arizona_crashable.erl` -- crash fixtures: `mount`, `handle_event`, and `handle_info` crash paths
- `arizona_chat.erl` -- pubsub cross-tab messaging, stream-based, owner-guarded delete; channel scoped per `:room` path segment
- `arizona_datatable.erl` -- stream sort/move/reset, 5 initial rows
- `arizona_mixed_children.erl` -- stateless + dynamic children, az numbering correctness
- `arizona_os_demo.erl` -- native-shell (OS) capability seam e2e fixture (route `/os`): capability-gated UI via the `?connected` binding pattern, server-emitted + client-triggered `arizona_os` commands, inbound OS events into `handle_event/3`
- `arizona_scroll_home.erl` / `arizona_scroll_about.erl` -- dedicated E2E scroll fixtures (tall content + hash anchor + replace-nav button). Routed at `/scroll-home` and `/scroll-about`; kept off the demo nav to avoid polluting the other E2E cases.
- `arizona_todo.erl` -- stream operations (add/remove/update/clear/move/insert_at/reset_with)
- `arizona_timer.erl` -- `handle_info/2` with set_message/set_message_with_effect
- `arizona_effectful.erl` -- dispatch_event effects (notify/multi/noop)
- `arizona_stream_with_child.erl` -- stream items each wrap an embedded `arizona_counter`; used to test child views surviving dep-skip
- `arizona_root_counter.erl` -- minimal counter view (inc/dec/noop) for `arizona_live` machinery tests
- `arizona_no_info_root.erl` -- view without `handle_info/2`; asserts inbox messages are silently dropped

**Spawn rule:** `arizona_live:start_link/4,5` spawns a handler as a **route root**. A stateful
handler embedded as a child (via `?stateful`) is never spawned as its own live process -- drive
embedded-handler behaviour either via direct calls (`arizona_stateful:call_mount/2`,
`arizona_render:render_to_iolist/2`) or via a parent that embeds it and dispatches events to it.

**Callback shapes that bite:** `handle_update/3` -- `(Params, Bindings, Effects) -> {Bindings,
Resets, Effects}` (e.g. `arizona_counter`), not the 2-arg form; `handle_event/3`;
`handle_info/2`; and a layout is a stateless `render/1` that splices the page through
`?inner_content` (e.g. `arizona_layout`), not a `render/2` taking the inner content as an
argument.
