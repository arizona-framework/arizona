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
| `make build-tauri` | Build the reference Tauri desktop shell (opt-in; needs Rust + webview deps; **not** in `ci`) |
| `make test-tauri` | Tauri shell Rust tests (opt-in; compiles the shell; full UI run manually; **not** in `ci`) |
| `make dev-tauri` | Run the reference Tauri shell in dev mode (devtools) against a running `make start`; opt-in, **not** in `ci` |
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
OS-level capabilities for a browser renderer wrapped in a **native shell** (Tauri, Electron, ...)
go through the host-neutral `arizona_os` seam; the reference shell is `clients/tauri/` (Tauri v2,
**not** part of `make ci`; see `make test-tauri` and [docs/os.md](docs/os.md)).

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

Response effects apply whenever the body parses **even on a `4xx`** (so the controller can use a real status); `on_error` (plus an `arizona:fetch-error` DOM event) fires only when there is no usable effects body -- a non-JSON page, an empty non-2xx, or a network failure. `az-form-reset` on a fetch form resets **only on a 2xx success** (so a validation error keeps the typed fields). `Opts`: `method` (default form's, else `post`), `body` (default the form, urlencoded; a GET form's fields go in the query), `headers`, `credentials` (default `same_origin`), `on_error`. The default Origin check (below) already refuses cross-origin fetches, so there is **deliberately no CSRF token** -- it would be redundant with the Origin check and can't cover the WS event path the check already protects (see the CSRF section). An anonymous form that needs to scope a pubsub topic to its submitter can use an anonymous `arizona_session` id instead.

### SPA navigation -- `navigate` (replace) vs `patch` (in-place)

Two SPA navigation modes, chosen per-link by the caller. **`navigate`** (`az_navigate` attribute / `arizona_js:navigate/1,2`) *replaces* the root view: the live process unmounts the old root, mounts the new handler, and ships one `OP_REPLACE` of the whole view element -- a fresh page. **`patch`** (`az_patch` / `arizona_js:patch/1,2`) *keeps* the root view: the live process delivers the new route's params to the current root's `handle_update/3` and re-renders through the diff, shipping only the changed slots. The process, view id, child views, and DOM (live chrome, scroll position, open menus) all survive. `arizona_socket` decides per frame -- it tracks the current root `handler` and applies a patch in place (`arizona_live:patch/2`) only when the patched path resolves to the **same** handler; a patch to a **different** handler can't keep the view, so it degrades to a full navigate/replace.

So "persistent live chrome across navigation" (a sidebar with a live badge, a media player) is an ordinary app pattern, **not** a framework feature: write one view whose chrome is persistent and whose content is a swappable `?stateful(?get(page), ...)` child, point several routes at it, and link between them with `az_patch`. There is deliberately no `shell`/`?outlet` concept -- that would bake a UI shape into the framework; `patch` is the generic primitive. The client tags the history entry (`_azNav`) and stamps the outgoing entry so back/forward replays the patch. Demo: `arizona_patch_demo` (`/patch-demo/:section`).

**The reaction is `handle_update/3`, widened to thread effects.** A patch calls the root's `handle_update(Params, Bindings, Effects) -> {NewBindings, Resets, Effects}` -- the **same** callback an embedded child gets on a parent prop update, now also fired on a route root (navigation is the root's prop source). `Effects` is the accumulator of effects already queued for this update cycle; return it extended with this callback's own (the default returns it verbatim), so a patch reaction can `arizona_js:set_title/1`, redirect, etc., threaded with no caller-side concatenation. A view without `handle_update/3` gets the default merge-and-re-render (efficient param-driven nav). The callback (and `arizona_stateful:call_handle_update/4`) being 3-arg is the one breaking shape vs the old 2-arg `{Bindings, Resets}`.

**Lifecycle: a patch runs no mount step.** `mount/1` and `on_mount` run whenever the view **(re)mounts** -- initial GET, reconnect, `navigate` -- and **not** on a patch (which by definition keeps the view alive). This is deliberate, not a gap: `on_mount` is a *mount input* (its output is fed into `mount/1`); with no `mount/1` on a patch, re-running it would dump a mount-time transform straight onto the live bindings and clobber the very state the patch preserves (a stock `fun(B) -> B#{count => 0} end` default-seeder would reset an evolved counter every nav). What legitimately must run on every arrival -- request-shaped derivation (session, path params) -- is the **middlewares**, which **do** run on a patch (their output arrives as `Params`); handler-specific per-navigation logic goes in `handle_update/3`, which uniquely sees both the new `Params` and the live state. So: mount-time setup -> `mount/1`/`on_mount`; every-arrival setup -> middleware; per-navigation reaction -> `handle_update/3`.

### View transitions

A view transition wraps **any** DOM change in `document.startViewTransition` (not tied to navigation). Request one per-trigger (no global switch) with `arizona_js:transition(Cmd[, Opts])` -- wraps the command (or list) whose change should animate, like `on_key/2` -- or the `az_transition` attribute on any triggering element (bare = cross-fade, `{az_transition, ~"slide back"}` = space-separated `types`). A sync effect animates in place; `navigate`/`push_event` animate the resulting server diff. Guarded by feature-detect + `prefers-reduced-motion`; back/forward replays via history state. Real `<a href>` navigations transition through user CSS (`@view-transition { navigation: auto }`). All styling is user-owned CSS. See [.claude/rules/js.md](.claude/rules/js.md).

## Configuration -- env-var references (`arizona_config`)

Any `arizona` app-env value (sys.config) may be declared as an environment-variable reference instead of a literal, resolved at startup by `arizona_config`. Two forms: `{env, "VAR"}` -- **required**, returns `$VAR` as a binary and crashes (`env_not_set`) if unset; `{env, "VAR", Default}` -- **optional**, coerces `$VAR` to the type of `Default` and falls back to `Default` when unset. Env vars are strings, so coercion is driven by the default's type: integer (`list_to_integer`), float, boolean (`"true"`/`"false"`, case-insensitive), binary, atom (`list_to_existing_atom`), or list (a comma-split into trimmed binaries, the `csrf_origins` shape). The required form has no default and yields a binary (suits `secret_key`).

```erlang
{arizona, [
    {secret_key, {env, "SECRET_KEY"}},
    {session_secure, {env, "SESSION_SECURE", false}},
    {server, #{
        scheme => {env, "SCHEME", http},
        transport_opts => [{port, {env, "PORT", 8080}}],
        routes => [ ... ]
    }}
]}.
```

References resolve **anywhere** in the config surface: the scalar keys (`secret_key`, `check_origin`, `csrf_origins`, `session_max_age`, `session_max_bytes`, `session_secure`, `flash_secure`, `session_store_sweep_ms`, ...) go through `arizona_config:get_env/1,2` at their read sites; the nested `server` map (`port`, `scheme`, `tls` cert paths, `proto_opts` tunables) is resolved once in `arizona_roadrunner_server:start/2`. `resolve/1` recurses into maps, lists, and 2-tuples (proplist pairs) only, so route tuples (`{live, Path, Handler, Opts}` -- all >=3-tuples) pass through untouched: an operator-supplied `{env, _, _}` term sitting inside a route's `bindings`/`state` is never rewritten.

## CSRF / Origin checking

CSRF defense is an **Origin check**, not a token: `arizona_origin:check(Origin, Host)` rejects a request whose `Origin` header is neither same-origin (its authority equals the `Host`) nor in an allowlist. A missing `Origin` (native `?native` clients, CLI tools, top-level GET navigations) is allowed.

It's the `arizona_middleware:check_origin/2` middleware step, and the router **applies it by default** to `{live, ...}` routes -- so it runs on both the page render (GET, usually no `Origin`) and the **WebSocket upgrade** (`arizona_ws:prepare/3` runs the resolved route's middlewares; a cross-origin upgrade gets `403`). Off by exception, not omission:

- `check_origin` app env (`boolean()`, default `true`) -- global switch.
- `csrf_origins` app env (`[binary()]`, default `[]`) -- extra trusted origins (proxy/multi-origin).
- `check_origin => false` in a route's `Opts` -- opt a single route out (a deliberately cross-origin endpoint).

This closes the WS-CSRF vector (a cross-origin page opening a WS as the victim). It is **not** CORS: CORS gates response-reading + preflight and lets simple cross-site requests through; the Origin check rejects the request outright. Controller routes (verb-tags like `{post, ...}` and `{match, ...}`) get the same default: they dispatch through `arizona_roadrunner_controller`, which runs the middleware pipeline (check_origin first) before the action -- so a cross-origin POST to a fetch endpoint is also refused with `403`.

**Boundary -- mutate only on non-GET (the check's one inherent limit).** The Origin check protects state-changing **POST/PUT/DELETE** and the WS upgrade; browsers always send `Origin` on those when cross-site. It **cannot** protect a state-changing **GET**: a top-level cross-site GET navigation (`<a href>`, `window.location`, a prefetcher) sends *no* `Origin` (so the check allows it) yet still carries a `SameSite=Lax` cookie -- so a cookie-only GET that mutates state (e.g. `GET /logout`) is CSRF-able regardless. This is the textbook limit of Origin-checking and isn't framework-closable (a cross-site GET nav is indistinguishable from a user following a link). Method routing makes "don't mutate on GET" enforced by **routing** rather than handler discipline: declare a mutating endpoint with a non-GET verb (`{post, ...}`/`{delete, ...}`) and a cross-site `GET` to it gets `405` from the router before the action runs. The limit only bites a mutation you **deliberately** route as `{get, ...}` -- so never put state changes behind GET (scope logout and similar mutations to DELETE/POST). Also set `SameSite=Lax` (or `Strict`, which closes even the GET-logout case) on app auth/session cookies. `Origin: null` is rejected; the missing-Origin allowance is for native clients. A signed double-submit **token** was evaluated and **deliberately not added**: it is redundant with this Origin check (which already gates POST/PUT/DELETE and the WS upgrade), can't cover the WS event path at all, and the only thing it would add -- closing the missing-Origin gap -- is for non-browser clients that carry no ambient cookie authority and so are not a CSRF vector.

## Sessions -- `arizona_session`

Durable, **encrypted** cookie-store session state: a small map carried across requests in the `az_session` cookie, encrypted with AES-256-GCM (via `arizona_crypto:encrypt/decrypt`, keyed off `secret_key`) and stamped with an absolute expiry, so a client can neither read, forge, nor outlive it. It generalizes `arizona_flash` from a one-request message to durable state, but with the opposite read semantics: a **read never consumes** (the cookie is re-emitted only on a write). The cookie store is the default; keep these sessions small (an id plus light state, well under ~4KB), and note a cookie store cannot be revoked before its expiry. An opt-in server-side store (below) lifts both limits.

`arizona_req` owns the stash + API: `put_session/3` / `delete_session/2` (write, fail-fast on missing `secret_key`; the first write seeds from the incoming cookie so writes merge onto existing state), `clear_session/1` (logout, no key needed), `session/1` (the effective map including this request's writes), `get_session/2` (`{ok, V} | error`) / `get_session/3` (with default), and `read_session/1` (lazy, idempotent, non-consuming). The response re-emits the cookie via `resp_cookies/1`: a written non-empty session sets it, a cleared one clears it, an untouched one emits nothing. The opt-in `{arizona_middleware, fetch_session}` step reads it into the `session` binding (`?get(session)`) and runs on both the GET render and the WS upgrade, so a live view is seeded at mount.

**Read everywhere; write through a controller.** A live process holds a session *snapshot* and cannot `Set-Cookie` over the WebSocket. To change the session from a live view, submit via `arizona_js:fetch(~"/path", #{method => post})` to a controller route; the action computes the next session and calls `roadrunner_resp:set_cookie(Resp, arizona_session:cookie_name(), arizona_session:encode(New), Opts)` directly (the controller flushes the *pre-action* request cookies, so `put_session/3` inside an action that returns a `roadrunner_resp` is not flushed). The view re-renders via the controller's `push_event`/pubsub (the `arizona_fetch_account` pattern). Config: `session_max_age` (seconds, default 7 days); `session_secure`/`flash_secure` (the `Secure` flag, default `false` -- **set `true` in production**; it defaults `false` because a `Secure` cookie is silently dropped over plain-HTTP dev, and a TLS-terminating proxy can't be auto-detected); and `session_max_bytes` (encoded-cookie cap, default 4096; `encode/1` errors `{session_too_large, ...}` past it rather than letting the browser drop the cookie). A CSRF double-submit token is **deliberately not built** (redundant with the default-on Origin check; see the CSRF section).

**Flash, by contrast, does survive a WebSocket navigate.** Unlike a session write, a one-request flash set by a halting middleware (`put_flash/3` before `{halt, redirect}`) or by an in-view handler (an `arizona_js:navigate`/`patch` `flash` opt) reaches the target page even when the redirect is a mid-session SPA navigate with no `Set-Cookie` leg: `arizona_socket` carries it in-process on the socket to the follow-up navigate frame, delivered **exactly once, no cookie** -- each navigation kind arms one mechanism (the signed `az_flash` cookie for a full-page HTTP redirect, the in-process carry for a live navigate). So flash needs **no** controller round-trip; `put_flash/3` + redirect works over both HTTP and WS. The app-facing flash API stays just `put_flash/3` (set) + `flash/1` (read); the carry (`pending_flash/1`, `put_flash_in/2`) and the cookie codec (`read_flash/1`) are internal plumbing. The HTTP cookie is signed with a baked-in TTL, so a replayed cookie kept past its `Max-Age` is rejected, not just soft-expired.

**Server-side store (opt-in).** Set the `session_store` app env to an `arizona_session_store` module (the default `arizona_session_store_ets`, or your own Redis/Mnesia backend for multi-node) and the `az_session` cookie carries only a **signed opaque id** while the map lives in the store -- enabling **revocation** (`Store:delete(Id)`), large/secret state, and data the client never sees. The mode switch lives in `arizona_req` (`read_session` resolves the id through the store; `resp_cookies` persists on the response); the map-level API (`put_session`/`get_session`/`session`) is unchanged. `arizona_req:session_id/1` exposes the id so an app can record it to revoke later. The ETS store is a supervised table owner (started via `arizona_sup` when configured) with a periodic sweep (`session_store_sweep_ms`, default 60000) plus lazy expiry; mirrors `Plug.Session.Store`. The `resp_cookies` persist is **commit-on-success** -- the store write happens at response flush, so a handler that crashes after `put_session/3` leaves the store untouched. **Preconditions:** store mode needs `secret_key` (the id is *signed*, not just random) and the `session_store` env set **before the app boots** (it's read at `arizona_sup` init; setting it later leaves the store unsupervised and session ops crash on the missing table). From a controller (store mode), write directly: after `arizona_session_store_ets:put(Id, New, arizona_session:max_age())`, set `{N, V, O} = arizona_session:set_cookie_id(Id)` and `roadrunner_resp:set_cookie(Resp, N, V, O)`. Fixed per-write TTL (`session_max_age`); **WS-activity (sliding) renewal and per-user revocation are deferred** -- the store makes them possible (renew the entry on a live event, no cookie needed) but they need live-process wiring, a separate change.

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

## Native-shell capabilities -- `arizona_os`

When a browser renderer is wrapped in a **native shell** (Tauri, Electron, a webview wrapper), the server can drive OS capabilities the browser sandbox forbids (window control, native notifications, screen-capture protection). The renderer is the **same** web (`?html`) client over the **same** WebSocket -- the shell adds only a native side-channel, so an existing app runs in a shell unchanged and the whole seam is a **safe no-op in a plain browser**. (This is **not** the `?native` JSON render target; see [docs/os.md](docs/os.md).)

The seam is defined at one boundary: a JS object `globalThis.__arizona_os__` the shell installs **before** the page scripts run, with `capabilities` (a map), `invoke(name, args)` (to the shell's native layer), and `onEvent(cb)` (inbound OS events). Arizona has **zero shell-specific code**; a new shell is a new adapter providing that object.

**Negotiation + detection.** At connect the client serializes `__arizona_os__.capabilities` into the `_az_caps` WS query param; the live process reads it with `?capability(Name)` / `?capabilities` (mirrors `?connected`: `false` at SSR). Gate **rendering** on a binding flipped on connect (the `?connected` self-cast pattern), **not** raw `?capability` in `render/1` (which freezes at SSR's `false`). **`?capability` is an unauthenticated client claim -- a UI/effect hint only, NEVER a server-side authorization input.**

**Commands -- `arizona_os`.** The per-shell command builder (beside `arizona_js`/`arizona_android`). Unlike those, **every** command funnels through one generic op `?EFFECT_OS` carrying a capability **name** plus args -- the engine is a pass-through, the shell owns the vocabulary, so new capabilities are new names not new op codes. Typed sugars (`set_title/1`, `focus/0`, `minimize/0`, `maximize/0`, `fullscreen/1`, `notify/1,2`, `capture_protection/1`) are the documented path; `command/2` is the unchecked escape hatch. Issue from an event attribute (client-triggered, no round-trip) or as a handler effect (server-emitted). Re-assert **declarative/idempotent** capabilities (`set_title`, `fullscreen`, capture protection) from server state on reconnect; do **not** re-fire **one-shot** ones (`notify`, `focus`). Inbound OS events arrive via `onEvent`, are relayed as a `push_event`, and land in the view's `handle_event/3` like any event.

**Shell-neutral; Tauri is the reference.** The same contract is satisfied by Tauri (`initialization_script` + `window.__TAURI__.window.getCurrentWindow()` core window commands / `event.listen`) and Electron (`preload` + `contextBridge` + `ipcMain`/`webContents.send`). The reference shell is `clients/tauri/` (Tauri v2), opt-in via `make build-tauri`/`make test-tauri` (not in `make ci`). The **thin** shape (renderer loads a remote Arizona URL; same-origin WS) is primary; **fat** (bundled local BEAM) is deferred. CI proves the seam without a shell via a real-browser e2e with a fake `__arizona_os__` (`arizona_os_demo` / `arizona_os.spec.js`). Full contract in [docs/os.md](docs/os.md).

## What's missing

### Engine

- **Upload support** -- file upload handling

### Sessions & security (follow-ups to `arizona_session`)

- **Session activity (sliding) renewal** -- now unblocked by the server-side store: renew a store entry's TTL on live WebSocket activity (needs the live process wired to the store; the cookie store can't slide over WS at all). Per-user revocation (a user->ids index) is the related follow-up.
- **Auth / `current_user` helpers** -- a login/identity layer built on top of `get_session`.
