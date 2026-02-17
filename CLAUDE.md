# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with
code in this repository.

## Project Overview

Arizona is a real-time web framework for Erlang (OTP 28+). It uses compile-time
parse transforms to optimize templates and a WebSocket-based differential
rendering engine to push minimal updates to clients.

## Build & Development Commands

```bash
# Build
rebar3 compile                  # Compile Erlang
npm run build                   # Build JavaScript client

# Run all checks + tests (CI equivalent)
rebar3 ci

# Code quality (runs fmt --check, lint, hank, xref, dialyzer)
rebar3 check

# Format code
rebar3 fmt                      # Erlang
npm run format                  # JavaScript/JSON/YAML

# Lint (beyond rebar3 check)
npm run lint:md                 # Markdown
npm run lint:js                 # JavaScript (ESLint)
npm run ci                      # Node.js CI (format:check + lint:check + test)

# Tests
rebar3 ct                       # Common Test suites
rebar3 eunit --cover            # EUnit tests
rebar3 ct --suite test/arizona_template_SUITE  # Single CT suite
npm run test:unit               # JS unit tests (Vitest)
npm run test:e2e                # E2E tests (Playwright)

# Start test server at localhost:8080
./scripts/start_test_server.sh
```

## Architecture

### Component Model

Three component types with distinct lifecycles:

- **Views** (`arizona_view` behaviour) — top-level page components.
  Required: `mount/2`, `render/1`.
  Optional: `handle_event/3`, `handle_info/2`, `terminate/2`.
- **Stateful components** (`arizona_stateful` behaviour) — nested components
  with their own state.
  Required: `mount/1`, `render/1`.
  Optional: `handle_event/3`, `unmount/1`.
- **Stateless components** — pure render functions (no behaviour):
  `render(Bindings) -> Template`.
  Executed via `arizona_stateless:call_render_callback/3`.

### Template System

Templates support three syntaxes via `arizona_template`:

- HTML: `arizona_template:from_html(~"<h1>{Title}</h1>")`
- Erlang terms:
  `arizona_template:from_erl({'div', [{id, Id}], [Content]})`
- Markdown:
  `arizona_template:from_markdown(~"# Hello {Name}!")`

A parse transform (`arizona_parse_transform`) compiles templates at build time
into optimized records with static parts, dynamic callback tuples, and
fingerprints for cache validation.

### Rendering Pipeline

1. **Scanner** (`arizona_scanner`) tokenizes template strings
2. **Parser** (`arizona_parser`) produces an AST from tokens
3. **Renderer** (`arizona_renderer`) executes templates to HTML
4. **Tracker** (`arizona_tracker`) records which bindings affect which
   dynamic elements
5. **Differ** (`arizona_differ`) generates minimal diffs by comparing
   fingerprints and tracking changed bindings

### Live Connections

`arizona_live` is a GenServer per WebSocket connection. It renders the initial
hierarchical structure, then on state changes produces diffs sent to the JS
client. Events from the client are routed to the appropriate view or stateful
component by ID.

### Server & Routing

Built on Cowboy (`arizona_server`). Four route types (all include a trailing
`Middlewares` list):

- `{view, Path, ViewModule, MountArg, Middlewares}` — renders a view to HTML
- `{websocket, Path, WebSocketOpts, Middlewares}` — live connection endpoint
- `{controller, Path, Handler, State, Middlewares}` — custom Cowboy handler
- `{asset, Path, AssetConfig, Middlewares}` — static file serving
  (`cowboy_static` opts)

Middleware (`arizona_middleware` behaviour) and plugins (`arizona_plugin`
behaviour) extend request handling and configuration.

### JavaScript Client

JavaScript source in `assets/js/`. Handles WebSocket communication, DOM
patching from server diffs (via morphdom), and event binding. Built with Vite;
output goes to `priv/static/assets/`.

### Key Supporting Modules

- `arizona_pubsub` — topic-based pub/sub via Erlang `pg`
- `arizona_action` — action types returned from callbacks
  (dispatch, reply, redirect, reload)
- `arizona_config` — application configuration loading
- `arizona_watcher` / `arizona_reloader` — file watching and hot reload
  in development

## Compilation Order

These files must compile first (parse transform dependencies):
`arizona_token` → `arizona_scanner` → `arizona_markdown` →
`arizona_markdown_processor` → `arizona_parser`

## Code Conventions

- All modules use the `arizona_` prefix. Test modules end with `_SUITE`.
- Compiler flags: `debug_info`, `warnings_as_errors`, `warn_missing_spec`
  — all exports require type specs.
- Module layout: `-moduledoc`, behaviour, exports (grouped by section),
  types, then implementations.
- Types use `-nominal` for complex definitions; maps use `:=` for
  required keys.
- Macros are restricted in source code (elvis config). Use EUnit assertion
  macros only in tests.
- Tests use Common Test with EUnit assertions
  (`-include_lib("stdlib/include/assert.hrl")`). Test groups run in parallel.
- Formatting: `erlfmt` for Erlang, Prettier for JS. 4-space indentation,
  no tabs, ≤100 character lines.
- No trailing whitespace. Comment tricky or non-obvious decisions.
- Minimum 80% code coverage enforced.
- Commit messages: first line must be ≤72 characters.
