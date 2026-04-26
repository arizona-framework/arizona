# Arizona

Erlang/OTP real-time web framework with server-side diffing. The project is in the `0.x` series -- the API may change between minor versions, so changes don't need to preserve backward compatibility for now.

Dependencies: `cowboy` (optional -- HTTP/WS server), `fs` (file system events for dev-mode watcher).

## Build & Test

```bash
rebar3 compile                            # compile Erlang
rebar3 ct                                 # all CT suites
rebar3 ct --suite=arizona_SUITE           # single suite
rebar3 eunit --module=my_module           # inline EUnit tests (private fn testing)
npx vitest run                            # JS unit tests (Vitest + jsdom)
```

**Always run `make precommit` before committing** if any `.erl` or `.js` files were touched -- it formats, runs fast checks (no dialyzer), and runs unit/JS tests.

### Makefile targets

| Target | Description |
|--------|-------------|
| `make` / `make start` | Start the test server (default) |
| `make ci` | Full CI: check + test + cover + E2E + doc |
| `make precommit` | Fast pre-commit: fmt, check-fast, test-erl, test-js |
| `make compile` | Compile Erlang sources |
| `make fmt` | Format all (`fmt-erl` `fmt-js`) |
| `make check` | All checks incl. dialyzer (`check-erl` `check-js`) |
| `make check-fast` | Fast checks -- no dialyzer (`check-fmt` `check-lint` `check-hank` `check-xref` `check-js`) |
| `make test` | All tests -- no coverage (`test-erl` `test-js` `test-e2e`) |
| `make test-erl` | Common Test + EUnit |
| `make test-ct` | Common Test only |
| `make test-eunit` | EUnit only (inline private fn tests) |
| `make test-js` | JS unit tests (Vitest) |
| `make test-e2e` | Playwright E2E tests |
| `make cover` | Coverage check (`cover-erl` `cover-js`) |
| `make cover-erl` | Erlang coverage (min 80%) |
| `make doc` | Generate docs (`doc-erl` `doc-js`) |
| `make doc-erl` | Erlang docs (ex_doc) |
| `make setup-e2e` | Install E2E deps |
| `make clean` | Remove build artifacts |

## Architecture reference

Full architecture documentation (modules, APIs, data flow, op codes, etc.) is in [docs/architecture.md](docs/architecture.md).

## Event attributes & effects -- `arizona_js`

Event attributes (`az-click`, `az-submit`, etc.) use `arizona_js` commands. Handler effects use the same module. All functions return `{arizona_js, [OpCode, ...Args]}`.

```erlang
%% Template: event attribute
{'button', [{az_click, arizona_js:push_event(~"inc")}], [<<"+">>]}

%% Template: multiple commands
{'button', [{az_click, [arizona_js:push_event(~"inc"), arizona_js:toggle(~"#modal")]}], [<<"Both">>]}

%% Handler: effects
handle_event(~"inc", _P, B) ->
    {B#{count => Count + 1}, #{}, [arizona_js:set_title(~"Updated")]}.
```

`push_event` auto-collects payload from inputs/forms. Explicit payload merges on top. Op codes in `include/arizona_js.hrl`.

## What's missing

### Engine

- **Upload support** -- file upload handling
