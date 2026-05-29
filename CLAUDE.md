# Arizona

Erlang/OTP real-time web framework with server-side diffing. The project is in the `0.x` series -- the API may change between minor versions, so changes don't need to preserve backward compatibility for now.

Dependencies: `roadrunner` (HTTP/WS server); `fs` (file system events for dev-mode watcher).

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
| `make test-e2e` | Playwright E2E tests (incl. the `native` JSON-wire project) |
| `make test-android` | Android client e2e (opt-in; needs Android SDK + emulator; **not** in `ci`) |
| `make cover` | Coverage check (`cover-erl` `cover-js`) |
| `make cover-erl` | Erlang coverage (min 80%) |
| `make doc` | Generate docs (`doc-erl` `doc-js`) |
| `make doc-erl` | Erlang docs (ex_doc) |
| `make setup-e2e` | Install E2E deps |
| `make clean` | Remove build artifacts |

## Architecture reference

Full architecture documentation (modules, APIs, data flow, op codes, etc.) is in [docs/architecture.md](docs/architecture.md).

## Clients

Client runtimes that consume the wire protocol live in-repo so they evolve atomically with it:
the **web** client (browser) is `assets/js/` (built into `priv/static` by Vite); the **Android**
client for the `?native` target is `clients/android/` (Kotlin/Compose, its own Gradle build --
**not** part of `make ci`; see `make test-android`).

## Event attributes & effects -- `arizona_js` / `arizona_android`

Web event attributes (`az-click`, `az-submit`, etc.) use `arizona_js` commands; `?native` views use `arizona_android`. Both build the same neutral effect tuple `{arizona_effect, [OpCode, ...Args]}` (encoded by `arizona_effect`). Handler effects use the same builders.

```erlang
%% Template: event attribute
{'button', [{az_click, arizona_js:push_event(~"inc")}], [<<"+">>]}

%% Template: multiple commands
{'button', [{az_click, [arizona_js:push_event(~"inc"), arizona_js:toggle(~"#modal")]}], [<<"Both">>]}

%% Handler: effects
handle_event(~"inc", _P, B) ->
    {B#{count => Count + 1}, #{}, [arizona_js:set_title(~"Updated")]}.
```

`push_event` auto-collects payload from inputs/forms. Explicit payload merges on top. Op codes in `include/arizona_effect.hrl`.

## Client-owned slots -- `?local`

`?local(Key, Init)` declares a slot the server renders **once** at SSR and then **never diffs** -- the browser owns the value (keyed by `Key`, a binary or atom literal) and updates it locally with **no WebSocket round-trip**. For UI-only state (dialog open/close, tabs, toggles). It binds either content or an attribute value. A content `?local` is **not** restricted to being the sole child -- an element can hold several content slots, freely mixed with static text and other dynamic children. An attribute value may also interpolate **one** `?local` with static prefix/suffix (two locals, or a local mixed with a server-owned dynamic, in one attribute are compile errors). Interpolation is for **string-valued** attributes only; boolean attributes must use a whole-value `?local` (an interpolated value always renders `name="value"`, so an interpolated boolean stays present):

```erlang
{'span', [], [?local(~"title", ~"Hello")]}            %% content
{'dialog', [{open, ?local(~"modal_open", false)}], [...]} %% attribute
{'p', [], [~"Name: ", ?local(~"first", ~"Ada"), ~" ", ?local(~"last", ~"Lovelace")]} %% many content slots
{'a', [{href, [~"/u/", ?local(~"id", ~"1"), ~"/edit"]}], [...]} %% interpolated attribute
```

Update it from an event attribute (or handler effect) -- never reaches the server:

- `arizona_js:set(Key, Value)` -- closest view of the trigger (event attributes only; a no-op as a handler effect -- no trigger element, so handlers use `set/3` or `set_all/2`)
- `arizona_js:set(ViewId, Key, Value)` -- a named view
- `arizona_js:set_all(Key, Value)` -- every view

Client JS: `arizona.set(viewId, key, value)` (always 3-arg -- the 2-arg `arizona_js:set/2` is template-only), `arizona.setAll(key, value)`, `arizona.get(key)` / `arizona.get(viewId, key)`. `get` returns DOM strings (no type preservation; absent/bare boolean attrs read back as `false`/`true`).

**Caveat:** a `?local` value survives normal per-slot diffs, but resets to its SSR initial if an enclosing region is re-rendered wholesale (`OP_UPDATE`/`OP_REPLACE`/`?each` swap) or on a forced reconnect. Inside `?each`, every item shares the slot **key** -- keys are compile-time literals (no `?local(<<"open_", Id/binary>>, ...)`), so `set`/`set_all` updates **all** items at once; `?local` can't hold per-item independent client state in a list/stream (use server state for that). The server never reads it back -- to use it server-side, send it in a `push_event` payload. See [docs/architecture.md](docs/architecture.md).

## What's missing

### Engine

- **Upload support** -- file upload handling
