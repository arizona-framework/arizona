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
| `make test-ios` | iOS client tests (opt-in; `swift test` anywhere + Simulator e2e on macOS; **not** in `ci`) |
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
**not** part of `make ci`; see `make test-android`); the **iOS** client is `clients/ios/`
(Swift/SwiftUI -- a SwiftPM package whose logic tests run on any platform via `swift test`, plus an
XcodeGen sample app for the Simulator e2e -- also **not** part of `make ci`; see `make test-ios`).

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

### HTTP fetch -- `arizona_js:fetch/2`

Three form-submission modes, by what each can do: a plain `{method, post}` form does a native POST (can `Set-Cookie`, but full reload); `az_submit` sends a WebSocket event (no reload, but WS can't set cookies); `arizona_js:fetch/2` issues a `fetch()` request (no reload **and** a real `Set-Cookie`, HttpOnly honored). `fetch` is the only mode that gives cookies without a reload, so it suits password-change / login / logout flows that rotate the session cookie while keeping the typed fields and showing inline validation.

```erlang
%% az_submit command: submit the form via fetch, no reload, fields preserved
{'form', [{az_submit, arizona_js:fetch(~"/account", #{method => post})}], [...]}
```

It hits a controller route -- declared with a verb tag (e.g. `{post, ~"/account", Handler, #{}}`), dispatched to the controller's action (default `handle`) -- that reads the body with `roadrunner_req:body/1` (buffered + unit-testable; not the streaming `read_body/1`), may `roadrunner_resp:set_cookie/4`, and returns the effects wire payload via `arizona_controller:reply_effects/1` (or `reply_redirect/1`, which delivers a `navigate` effect -- a fetch-followed HTTP 3xx can't drive a SPA nav). The browser applies `Set-Cookie` natively.

**Showing content is server-authoritative, not a response effect** -- there is deliberately no `set_text`/`set_html` (it would fight the diff engine), and the response is effects-only because a stateless controller has no diff snapshot to make ops from (hence `accept: application/json`, no ops). To update **the submitting view**, return an `arizona_js:push_event` in the response: the client relays it over the existing WebSocket and the view re-renders through its normal `handle_event/3` -- no subscription, works for anonymous forms. It targets the form's enclosing view (root **or** a child view) and does **not** echo the form fields into the event payload (the effects run against the view element, not the form, so sensitive inputs aren't dragged along) -- pass the controller's result as an explicit payload. Success uses `reply_effects/1` (200, the form auto-resets); an error leg uses `reply_effects(Status, Effects)` with a non-2xx (e.g. `422`) so the typed fields survive while the effects still apply. Use `arizona_pubsub` instead when the change must reach **other** views/users (broadcast), scoping the topic by user/session (e.g. `{account, UserId}`). Either way the view renders from state via the diff; the response effects themselves are for *request-local imperative UI* only (toggling a pre-rendered element, focus). The two mechanisms have working e2e fixtures: `arizona_fetch_push` (push_event) and `arizona_fetch_account` (pubsub).

Response effects apply whenever the body parses **even on a `4xx`** (so the controller can use a real status); `on_error` (plus an `arizona:fetch-error` DOM event) fires only when there is no usable effects body -- a non-JSON page, an empty non-2xx, or a network failure. `az-form-reset` on a fetch form resets **only on a 2xx success** (so a validation error keeps the typed fields). `Opts`: `method` (default form's, else `post`), `body` (default the form, urlencoded; a GET form's fields go in the query), `headers`, `credentials` (default `same_origin`), `on_error`. The default Origin check (below) already refuses cross-origin fetches; the **CSRF token** is still missing -- a signed double-submit token modeled on `arizona_flash` (which would also let an anonymous form like login scope a pubsub topic to its submitter) is the next follow-up.

### View transitions

A view transition wraps **any** DOM change in `document.startViewTransition` (not tied to navigation). Request one per-trigger (no global switch) with `arizona_js:transition(Cmd[, Opts])` -- wraps the command (or list) whose change should animate, like `on_key/2` -- or the `az_transition` attribute on any triggering element (bare = cross-fade, `{az_transition, ~"slide back"}` = space-separated `types`). A sync effect animates in place; `navigate`/`push_event` animate the resulting server diff. Guarded by feature-detect + `prefers-reduced-motion`; back/forward replays via history state. Real `<a href>` navigations transition through user CSS (`@view-transition { navigation: auto }`). All styling is user-owned CSS. See [.claude/rules/js.md](.claude/rules/js.md).

## CSRF / Origin checking

CSRF defense is an **Origin check**, not a token: `arizona_origin:check(Origin, Host)` rejects a request whose `Origin` header is neither same-origin (its authority equals the `Host`) nor in an allowlist. A missing `Origin` (native `?native` clients, CLI tools, top-level GET navigations) is allowed.

It's the `arizona_middleware:check_origin/2` middleware step, and the router **applies it by default** to `{live, ...}` routes -- so it runs on both the page render (GET, usually no `Origin`) and the **WebSocket upgrade** (`arizona_ws:prepare/3` runs the resolved route's middlewares; a cross-origin upgrade gets `403`). Off by exception, not omission:

- `check_origin` app env (`boolean()`, default `true`) -- global switch.
- `csrf_origins` app env (`[binary()]`, default `[]`) -- extra trusted origins (proxy/multi-origin).
- `check_origin => false` in a route's `Opts` -- opt a single route out (a deliberately cross-origin endpoint).

This closes the WS-CSRF vector (a cross-origin page opening a WS as the victim). It is **not** CORS: CORS gates response-reading + preflight and lets simple cross-site requests through; the Origin check rejects the request outright. Controller routes (verb-tags like `{post, ...}` and `{match, ...}`) get the same default: they dispatch through `arizona_roadrunner_controller`, which runs the middleware pipeline (check_origin first) before the action -- so a cross-origin POST to a fetch endpoint is also refused with `403`.

**Boundary -- mutate only on non-GET (the check's one inherent limit).** The Origin check protects state-changing **POST/PUT/DELETE** and the WS upgrade; browsers always send `Origin` on those when cross-site. It **cannot** protect a state-changing **GET**: a top-level cross-site GET navigation (`<a href>`, `window.location`, a prefetcher) sends *no* `Origin` (so the check allows it) yet still carries a `SameSite=Lax` cookie -- so a cookie-only GET that mutates state (e.g. `GET /logout`) is CSRF-able regardless. This is the textbook limit of Origin-checking and isn't framework-closable (a cross-site GET nav is indistinguishable from a user following a link). Method routing makes "don't mutate on GET" enforced by **routing** rather than handler discipline: declare a mutating endpoint with a non-GET verb (`{post, ...}`/`{delete, ...}`) and a cross-site `GET` to it gets `405` from the router before the action runs. The limit only bites a mutation you **deliberately** route as `{get, ...}` -- so never put state changes behind GET (mirroring Phoenix scoping logout to DELETE/POST). Also set `SameSite=Lax` (or `Strict`, which closes even the GET-logout case) on app auth/session cookies. `Origin: null` is rejected; the missing-Origin allowance is for native clients. A signed double-submit **token** (which would additionally close the missing-Origin gap) is a documented follow-up; with `arizona_session` now present, the token can bind to the session.

## Sessions -- `arizona_session`

Durable, **encrypted** cookie-store session state: a small map carried across requests in the `az_session` cookie, encrypted with AES-256-GCM (via `arizona_crypto:encrypt/decrypt`, keyed off `secret_key`) and stamped with an absolute expiry, so a client can neither read, forge, nor outlive it. It generalizes `arizona_flash` from a one-request message to durable state, but with the opposite read semantics: a **read never consumes** (the cookie is re-emitted only on a write). Cookie store only (no server-side store yet); keep sessions small (an id plus light state, well under ~4KB), and note a cookie store cannot be revoked before its expiry.

`arizona_req` owns the stash + API: `put_session/3` / `delete_session/2` (write, fail-fast on missing `secret_key`; the first write seeds from the incoming cookie so writes merge onto existing state), `clear_session/1` (logout, no key needed), `session/1` (the effective map including this request's writes), `get_session/2` (`{ok, V} | error`) / `get_session/3` (with default), and `read_session/1` (lazy, idempotent, non-consuming). The response re-emits the cookie via `resp_cookies/1`: a written non-empty session sets it, a cleared one clears it, an untouched one emits nothing. The opt-in `{arizona_middleware, fetch_session}` step reads it into the `session` binding (`?get(session)`) and runs on both the GET render and the WS upgrade, so a live view is seeded at mount.

**Read everywhere; write through a controller.** A live process holds a session *snapshot* and cannot `Set-Cookie` over the WebSocket. To change the session from a live view, submit via `arizona_js:fetch(~"/path", #{method => post})` to a controller route; the action computes the next session and calls `roadrunner_resp:set_cookie(Resp, arizona_session:cookie_name(), arizona_session:encode(New), Opts)` directly (the controller flushes the *pre-action* request cookies, so `put_session/3` inside an action that returns a `roadrunner_resp` is not flushed). The view re-renders via the controller's `push_event`/pubsub (the `arizona_fetch_account` pattern). Config: `session_max_age` (seconds, default 7 days) and `session_secure` app envs. The CSRF double-submit token is now unblocked (a signed token can bind to the session); still a follow-up. A server-side store and encryption-at-rest alternatives are documented non-goals for now.

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

## Static generation

`arizona_static:generate/2,3` renders route handlers to HTML files offline (no server/WS), building
on the request-free SSR path, and returns `{Written, Failed}` (paths plus per-spec failures). A spec is `{Handler, Outfile}` or
`{Handler, Outfile, Opts}` (`Outfile` relative to `OutDir`; `Opts` = `bindings`/`on_mount`/`layouts`).
`generate/3` takes a `DefaultOpts` map each spec's `Opts` override (e.g. a shared layout).

```erlang
arizona_static:generate(~"_site", [
    {home_page, ~"index.html"},
    {about_page, ~"about/index.html", #{bindings => #{title => ~"About"}}}
], #{layouts => [{site_layout, render}]}).
```

## What's missing

### Engine

- **Upload support** -- file upload handling

### Sessions & security (follow-ups to `arizona_session`)

- **CSRF double-submit token** -- now unblocked: a signed token (from `arizona_crypto`) can bind to the session, closing the missing-Origin gap the Origin check leaves open.
- **Server-side session store** -- a pluggable `get/put/delete` store behaviour (ETS/Redis/DB) behind an opaque session-id cookie, for instant revocation and large/secret state the cookie store can't hold.
- **Key rotation grace** -- the encrypt/sign wire format carries no key version, so rotating `secret_key` invalidates every existing cookie (logs everyone out). Add a multi-key decrypt path (accept the previous key) for zero-downtime rotation.
- **`Secure` cookie default** -- `session_secure`/`flash_secure` default `false` (dev-friendly); apps must enable them in prod. Consider defaulting to `true` or auto-enabling under HTTPS.
- **Cookie size guard** -- an oversized session (> ~4KB) is silently dropped by the browser; optionally have `arizona_session:encode/1` error on overflow instead.
- **Sliding-window expiry** -- the session TTL is fixed per write; optionally renew it on read.
- **Auth / `current_user` helpers** -- a login/identity layer built on top of `get_session`.
