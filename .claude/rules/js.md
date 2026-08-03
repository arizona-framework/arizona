---
description: Client runtime and element hooks conventions
paths:
  - "assets/**"
  - "e2e/**"
---

# Client Runtime

`assets/js/arizona.js` -- ES module, zero dependencies. Built by Vite to `priv/static/assets/js/arizona.min.js`.

Exports (the full list, alphabetical as in the module): `applyEffects`, `applyOps`, `connect`, `executeJS`, `exitPip`, `get`, `hooks`, `mountHooks`, `OP`, `pushEvent`, `pushEventTo`, `requestPip`, `resolveEl`, `restoreFormState`, `saveFormState`, `set`, `setAll`.

- `connect(endpoint, params?)` -- bootstrap: spawns Worker, installs document/window-level event delegation, takes over `history.scrollRestoration`. **Returns a `disconnect` function** that aborts every listener it registered, terminates the Worker, clears module state (`_connected`, `_pendingScroll`, saved forms), and restores the previous `scrollRestoration`. Idempotent. Use for teardown in tests and for host apps that want to unmount Arizona on route change. **One live connection at a time:** the connection owns module-level state, so calling `connect` again warns and retires the previous one first (otherwise its Worker keeps a second socket patching the same document, and its `disconnect` -- reading the same module state -- would tear down the newer connection instead).
- `applyOps(ops)` -- applies opcodes. `OP_TEXT` uses comment markers (`<!--az:X-->...<!--/az-->`). `OP_UPDATE` does innerHTML, `OP_REPLACE` swaps the target for the parsed fragment and mounts hooks on the nodes it inserted (the target's id names the *outgoing* view, so it cannot be re-resolved after the swap). Triggers hook lifecycle callbacks.
  - **`OP_TEXT` text vs HTML.** A scalar `?get` value arrives on the WS wire as a bare string; an HTML fragment (a nested-template / plain-list-`?each` zip-map, or a `?raw` `{raw}` tag) arrives as an object. The worker (`resolveOps`) records which -- `isHtml = typeof payload !== 'string'` -- *before* `resolveHtml` flattens both to a string, and sets `op[3] = true` only for HTML (the common text case stays a 3-element op; a missing `op[3]` is falsy). The main thread then renders text via a **text node** (`createTextNode` / `textContent` -- a `<` shows as literal text, never parsed, so a `?get` value can't inject and matches SSR) and HTML via **innerHTML**. `?raw` is the only scalar that needs the object tag, so the client can tell trusted markup from a value that merely contains `<`. This is the diff-side completion of server-side auto-escaping (`render_dyn`/`escape_value` at SSR): the wire stays raw, the client is the escaping boundary.
- `applyEffects(effects)` -- runs **every** server-sent effect through the one `executeJS` interpreter (`executeJS(document.documentElement, null, eff)` per effect), so a handler effect and an event-attribute command share a single implementation of all the op codes in `include/arizona_effect.hrl`. There is no separate server-effect subset: `push_event` sends over the **WebSocket** (`workerPost(W_SEND, ...)`) -- it is `dispatch_event` that fires a `CustomEvent`, on every hosting document -- `set_title` writes `document.title`, `reload` is dev-mode, and the selector effects behave exactly as under an event attribute. The trigger element is `document.documentElement`, so the ops that resolve a view from the trigger degrade: `resolveTarget` returns null, `sendTarget` falls back to the **root** view (`push_event`), and `set_local` without an explicit view id (`arizona_js:set/2`) finds no view and is a no-op (handlers use `set/3` or `set_all/2`). Ops needing a user gesture (`copy_to_clipboard`, `request_pip`) are event commands in practice -- a server-pushed one is caught and logged, not thrown.
- `resolveEl(target)` -- splits `"viewId:az"`, finds `getElementById(viewId)`, then tries three lookups in order: an element carrying the az (the view root itself or a descendant); for a compound `"X:n"` slot az, the base `[az="X"]` element; and finally the slot's `<!--az:X-->` comment marker anywhere in the view subtree, returning its **parent element**.
  - **The marker arm is what makes a marker-only slot patchable.** A template whose whole body is a bare dynamic (`?html(case ... end)`, `?html(?get(x))`, `?html(?each(...))`, `?html(?stateless(...))`, a mixed top-level fragment) and a stream/list `?each` among static siblings anchor their slot with a marker pair that **no element carries**, so without this arm every op addressed to them resolves to nothing and is dropped. Arm 2 does not subsume it: `querySelector` searches descendants, so a compound az whose base is the view root's own az finds nothing.
  - **A marker hit is refused by the ops that act on the whole element.** `resolveEl` is a thin wrapper over `resolveOpTarget`, which reports `{el, marker}` -- `marker: true` meaning `el` is only the slot's PARENT. `applyOps` skips `MARKER_UNSAFE_OPS` (`OP_UPDATE`, `OP_REPLACE`, `OP_REMOVE_NODE`, `OP_INSERT`, `OP_MOVE`) on such a hit, warning and skipping like an unresolved target: the first three would `innerHTML`/`replaceWith`/`remove` the parent and take the slot's siblings (the whole view when the parent is the live root), and the last two place a node by container position, landing outside the marker span. The marker-aware ops keep the resolution -- `OP_TEXT`, `OP_LIST_PATCH`, and the position-independent `OP_REMOVE`/`OP_ITEM_PATCH`, which find their item by `az-key`.
  - **The per-batch memo re-validates a marker hit.** `applyOps` caches target -> resolution; `isConnected` only proves the parent survived, so a marker hit is additionally re-checked with `findMarker` before reuse -- an earlier op in the same batch can re-render the enclosing slot and destroy the inner marker, and the stale hit would otherwise reach `OP_TEXT`'s whole-element fallback and wipe the parent.
- `pushEvent(event, payload)` / `pushEventTo(view, event, payload)` -- send over WebSocket.
- `executeJS(el, event, cmds)` -- the shared command interpreter (`execOne` per command); exported so a hook can run an `arizona_js` command list itself.
- `set(viewId, key, value)` / `setAll(key, value)` / `get(key)` / `get(viewId, key)` -- the client-owned slot (`?local`) API; see below.
- `requestPip(viewId, opts?)` / `exitPip(viewId)` -- pop a view out into a document-picture-in-picture window / close it.
- `saveFormState()` / `restoreFormState()` -- snapshot and re-apply typed form fields around a resync that rebuilds the DOM (an abnormal close, a bfcache round-trip).

## Client-owned slots (`?local`)

`?local(Key, Init)` renders once at SSR and is then owned by the browser -- the server never diffs it and never reads it back. The client side is three functions:

- `set(viewId, key, value)` -- `findViewRoot(viewId)`, then `forEachLocal` writes every matching slot in that view. Always 3-arg; the 2-arg `arizona_js:set/2` ("closest view of the trigger") is a template-only form the client resolves from the trigger element.
- `setAll(key, value)` -- every matching slot in **every** hosting document (main + popped-out PiP documents), no view scoping.
- `get(key)` -> first match anywhere; `get(viewId, key)` -> first match in that view. Reads come back as DOM strings (no type preservation); an absent boolean attribute reads `false` and a bare one `true`.

A slot binds either element **content** or an **attribute value**; the descriptor rides on the reserved `az-local` attribute, which is why a template may not write `az-local` (or `az`) itself. Both writers go through `writeLocalValue(el, target, value)`, so a content slot and an attribute slot update the same way from the caller's view. `set`/`setAll` on a missing view or key are silent no-ops.

Every item of an `?each` shares the slot **key** (keys are compile-time literals), so a `set` hits all of them; `?local` cannot hold per-item independent client state.

## Navigation scroll behavior

- Push nav (`az-navigate` click, `arizona_js:navigate/1,2` without replace): saves outgoing scroll onto the current history entry via `replaceState`, `pushState`s the new URL, resets scroll to top (or `#hash` target) after OP_REPLACE. Opt out with `az-noscroll` on the link or `{noscroll: true}` on the effect.
- Replace nav (`arizona_js:navigate(Path, {replace: true})`): `replaceState` only. Does NOT save outgoing, does NOT reset.
- Popstate (back): restores `e.state._azScroll` after OP_REPLACE, falls through to `#hash` target or top.
- Forward-after-back: destination entry has null state -> scroll to top. Documented non-goal; future restore would need a state-ID-keyed map backed by sessionStorage, not `replaceState`-on-scroll.
- Modifier clicks (ctrl/cmd/shift/alt, non-primary button) on `az-navigate` links fall through to the browser.

### Patch nav (`az-patch`) and the `_azNav` tag

`navigateTo` takes a `kind` -- `'navigate'` (default) or `'patch'` -- which is both the WS frame verb (`[kind, {path, qs}]`) and a history tag. A patch push writes `_azNav: 'patch'` onto **both** the new entry (in the `pushState` state) and the outgoing entry (via `saveCurrentScroll(kind)`), so a later back/forward across that edge replays the same mode; popstate reads `e.state?._azNav === 'patch' ? 'patch' : 'navigate'`. The tag is only a hint -- the server still corrects the verb, degrading a cross-handler patch to a navigate. Tagging the *outgoing* entry matters when it was a full page load, which carries no state of its own.

Scroll timing differs because a patch emits **no `OP_REPLACE`**. `applyOps` scrolls when `didReplace` (navigate) **or** when the armed intent is a patch and the batch is non-empty (`_pendingScroll.patch && ops.length > 0`). The worker message handler clears an armed patch intent after a frame that carried no ops (identity-checked, so an intent armed by *this* frame's own `JS_PATCH` effect survives to its own reply), otherwise a silent patch would leave the intent armed for an unrelated later diff to yank the scroll. Residual race: a truly silent patch is indistinguishable from a slow patch reply, so one unrelated ops-frame can still scroll -- there is no reply id on the wire to do better client-side. Scroll, the patch tag, and any pending view transition all ride **one** `replaceState` per outgoing entry (Safari rate-limits history writes to ~100/30s, then throws `SecurityError`).

## View transitions

A view transition wraps **any** DOM change in `document.startViewTransition` -- it is not tied to navigation. The API is mostly CSS; the framework only *starts* the transition for changes the browser can't see on its own. Two ways to request one (opt-in per-trigger, no global switch):

- **`arizona_js:transition(Cmd)` / `transition(Cmd, Opts)`** -- wraps the command (or list of commands) whose DOM change should animate, exactly like `on_key/2` wraps a command. `Opts` is `#{types => [binary()]}`.

  ```erlang
  {az_click, arizona_js:transition(arizona_js:toggle(~"#panel"))}                      %% sync client effect
  {az_click, arizona_js:transition(arizona_js:navigate(~"/x"), #{types => [~"slide"]})} %% navigation
  {az_click, arizona_js:transition(arizona_js:push_event(~"load_more"))}                %% server diff
  ```

- **`az_transition` attribute** on any element with a trigger (an `az-navigate` link **or** an `az_click`/`az_submit`/... element) -- bare (`az_transition`) = default cross-fade; `{az_transition, ~"slide back"}` = a space-separated list of view-transition `types` (tokens trimmed, empties dropped). It wraps whatever the element's trigger does.

The client picks sync vs async from the wrapped command:
- **Sync effect** (`toggle`/`add_class`/...): wrapped in place immediately.
- **`navigate`**: the page swap arrives a round-trip later; the worker message handler wraps the `OP_REPLACE` batch (a stray text/attr tick in between is ignored).
- **`push_event`**: the resulting server diff arrives later; the handler wraps the first response batch (then drops the intent, so a no-diff event can't bleed onto a later one). Caveat: on a page with frequent concurrent server pushes -- e.g. a timer -- an interleaving diff could be the one animated; navigation and sync effects are race-free.

Wrapping a **mix** of sync and async commands (`transition([toggle(...), push_event(...)])`) animates the async result; sync siblings apply immediately, unwrapped. Wrap one kind per call.

The wrap is applied at the **worker message handler**, so a message's ops **and** effects animate together, in order.

Behaviour:
- **Guards:** no-ops (instant swap) when `document.startViewTransition` is absent or `prefers-reduced-motion: reduce` matches. `types` use the object form `startViewTransition({update, types})` only when `CSS.supports('selector(:active-view-transition-type(x))')`; otherwise the bare-callback form (older engines still cross-fade, ignore types).
- **Back/forward:** a transitioned nav stamps `_azTransition` onto both the outgoing and new history entries; popstate replays `e.state._azTransition`, so traversing the edge animates symmetrically. (Direction-aware type reversal is not done yet -- the same opts are reused both ways.)
- **Cross-document** (real `<a href>` navigations, full reloads): pure CSS -- add `@view-transition { navigation: auto; }` to the page. No framework code.
- **Styling** is user CSS. By default the whole root cross-fades; to scope or morph a single element, give it a `view-transition-name` (it then animates independently across the change). `::view-transition-*` and `:active-view-transition-type(<type>)` customize the animation. A `view-transition-name` must be unique among rendered elements during a transition, or the browser skips it.

## HTTP fetch (`arizona_js:fetch`)

`arizona_js:fetch(Url, Opts)` (op `JS_FETCH = 22`) issues a `fetch()` request with **no page reload** -- the only command that can set a real `Set-Cookie` (HttpOnly honored) without navigating. The `case JS_FETCH:` handler in `execOne`:

- **Body.** For a POST/PUT/... when the trigger is/contains a `<form>` (the `az-submit` listener passes the form as the trigger element), the body is `new URLSearchParams(new FormData(form, submitter))` -- `application/x-www-form-urlencoded`, mirroring a normal form POST (the submit button's `name`/`value` rides along, so a multi-submit-button form reports which one fired; the submitter is also carried into a GET form's query string; multipart / file uploads are a non-goal). Otherwise `Opts.body` is JSON-encoded. Explicit `Opts.body` wins over the form.
- **GET/HEAD.** No request body; a form's fields are appended to the URL query string instead (`fetch` is otherwise POST-oriented -- it exists to set cookies).
- **Request.** `method` = `Opts.method` -> the form's `method` -> `POST`. `credentials` maps the atom (`same_origin` -> `'same-origin'`, default) to the fetch mode. Headers default to `accept: application/json` plus `Opts.headers`. `keep_alive` = `Opts.keep_alive === true` (default `false`) -- maps straight to `fetch()`'s `keepalive`, so a request fired just before a navigation completes instead of being cancelled (browser inflight-body cap ~64KB).
- **Effects body.** Whenever the response body parses as `{ e: effects }`, `applyEffects(effects)` runs -- **regardless of status**, so a controller can return a real `4xx` (e.g. `422`). An empty `2xx` body (a `204` cookie-only response) applies nothing. `Set-Cookie` is applied natively by the browser. (No response-effect view-transition wrapping -- a follow-up; server-driven re-renders animate through the normal WS path.)
- **Form reset.** `az-form-reset` is honored via the shared `maybeResetForm` helper. The submit listener resets synchronously for non-fetch commands; for a fetch command it defers (the listener skips the sync reset via `commandsIncludeFetch`) and the fetch handler resets **only on a 2xx success** -- so a validation error (a non-2xx) keeps the typed fields.
- **Redirect.** There is **no HTTP-3xx handling** -- `fetch`'s `redirect: 'manual'` yields an opaque-redirect whose `Location` is unreadable. A redirect is delivered as an `arizona_js:navigate` effect in a 2xx body (`arizona_controller:reply_redirect/1`).
- **Failure.** `Opts.on_error` runs (via `executeJS`) and an `arizona:fetch-error` `CustomEvent` is dispatched on `document` only when there is **no usable effects body**: a non-JSON error page, an empty non-2xx, or a network failure.

The server endpoint is a controller route (e.g. `{post, ...}`) building the response with `arizona_controller:reply_effects/1`. **Showing content is server-authoritative, not a response effect** -- there is deliberately no `set_text`/`set_html` (it would fight the diff engine) and the response is effects-only (a stateless controller has no diff snapshot for ops, hence `accept: application/json`). Two ways to update the rendered page, both server-authoritative (the view renders from state, the WS diff patches):

- **`push_event` in the response (default, the submitting view).** Return `arizona_js:push_event(~"...")`; the fetch handler runs response effects against the **enclosing view element** (not the form), so `resolveTarget` finds the form's view (root **or** a child view) and the client relays the event over its WS -> `handle_event/3` re-renders -- **without** scraping the form's fields into the payload (the view element isn't a form, so sensitive inputs aren't echoed). Pass the controller's result as an explicit payload. Success leg: `reply_effects/1` (200, the form auto-resets). Error leg: `reply_effects(Status, Effects)` with a non-2xx (e.g. `422`) so the typed fields survive while the effects still apply. No subscription, works for anonymous forms. Fixture: `arizona_fetch_push`.
- **`arizona_pubsub` (broadcast, other views/users).** The view subscribes to a topic (scoped by user/session) in `mount/1`; the controller broadcasts to it. Use when the change must reach views beyond the submitter. Fixture: `arizona_fetch_account`.

The response effects themselves are for request-local imperative UI only. CSRF is handled by the default-on Origin check (cross-origin fetches and the WS upgrade are refused); a signed double-submit **token is deliberately not added** -- redundant with the Origin check and unable to cover the WS event path it already protects. An anonymous form that needs to scope a pubsub topic to its submitter can use an anonymous `arizona_session` id.

## Fingerprint cache (`arizona-core.js` / `arizona-worker.js`)

Template statics are cached by fingerprint in memory and in IndexedDB, and the keys are announced to the server on every socket open (`["cached_fps", [...]]`) so it can omit statics it knows the client has.

**It is bounded.** A fingerprint is a hash of a template's statics, so editing a template mints a new key and orphans the old one: unbounded, the store accumulates one generation per deploy for the lifetime of the origin's storage. Each entry carries a coarse last-used stamp (`u`, re-stamped at most hourly on a cache hit and flushed back to the store), and hydration keeps only the `FP_CACHE_MAX` (1000) most recently used, deleting the rest. The announcement is capped at the same number -- it ships on every reconnect, and 1000 keys of at most 7 base-36 characters is ~10 KB. The server caps its own side at `?MAX_SENT_FPS` (10000, `arizona_live`) and drops the overflow.

Evicting is never wrong: the cache is content-addressed, so the server re-sends the statics for any fingerprint the client did not announce, and a miss costs bytes only. It happens at **hydration** only -- once a connection has announced a key, the server stops shipping that template's statics, so dropping it mid-connection would leave a payload the client cannot resolve. The in-memory map is therefore free to grow past the cap within a session; the next hydration prunes it.

## Connection detection

Server-side: handlers use the `?connected` macro (delegates to `arizona_live:connected()`) in `mount/1` to detect WS vs SSR context, and `?reconnected` (`arizona_live:reconnected()`) to tell a re-opened socket from a first connect. No `az-connect` HTML attribute -- connection is fully server-driven.

To run something on connect, self-send and handle it in `handle_info/2`. Use the **`?send` macro**, not a bare `self() ! Msg`: live-process messages are view-id tagged (`arizona_live:send/2` sends `{arizona_view, ViewId, Msg}`) and `handle_info/2` routes a tagged message to the matching root **or child** view. A bare untagged message still reaches the root through the catch-all clause, but it can never address an embedded child, and a message arriving before the first render (`snapshot = undefined`) is dropped -- with two exceptions matched by earlier clauses: the transport's `{'DOWN', ...}` monitor and `{arizona_drain, _}`, whose pre-mount leg deliberately stops with `{shutdown, drain}`.

```erlang
mount(Bindings0) ->
    Bindings = #{id => ~"demo", connected => false},
    ?connected andalso ?send(arizona_connected),
    {Bindings, #{}}.
```

`?send(Msg)` expands to `arizona_live:send(?get(id), Msg)`, so `mount/1` must bind a fresh map **named `Bindings`** carrying `id` for the macro to resolve against (fixture: `arizona_os_demo`). `?send_after/2,3` additionally registers its timer ref in the `$arizona_timers` process-dictionary map (keyed by view id) so unmount and `navigate` can cancel it.

## Reconnect handshake (`_az_fps_follow`)

A reconnect must resync an already-rendered DOM, and the resync payload is a whole-page `OP_REPLACE` -- exactly the payload that benefits most from the fingerprint cache. But the cache keys are announced in a `cached_fps` **frame**, which normally arrives *after* the server has already rendered. The handshake closes that gap:

1. The worker sets `_fpsFollow = _reconnecting` in `openSocket` and appends `&_az_reconnect=1&_az_fps_follow=1` to the URL. That is a promise: on this open the `cached_fps` frame is **mandatory** and is sent first. A first connect never flags (SSR already delivered the page, nothing to defer); the fingerprint *list* deliberately stays off the URL, since it can grow past URL length limits.
2. `arizona_ws:prepare/3` reads `_az_fps_follow` off the query string and threads it into `arizona_socket:init/4`.
3. `arizona_socket:init_view(true, true, ...)` mounts **nothing**: it arms a `?RESYNC_TIMEOUT_MS` (1000 ms) backstop timer, stores the ref in `pending_resync`, and returns. Deferring the whole mount+render matters -- the mount must not run twice, and no frame may reach a still-unmounted live process.
4. The **first** inbound frame settles it. The promised `[<<"cached_fps">>, FpList]` seeds the fingerprints (`arizona_live:seed_fps/2`, a cast, ordered ahead of the following call) and the resync replies deduped. **Any** other frame (an event racing the announcement, a ping) flushes the resync undeduped first and its own reply follows on the wire in the same `{reply_many, [Frame], Socket}` result.
5. If nothing arrives, `handle_info(arizona_resync_timeout, ...)` flushes the resync undeduped after the 1 s backstop.

An unflagged reconnect (`init_view(true, false, ...)`) keeps the old behavior: mount + render + `OP_REPLACE` immediately, undeduped.

## bfcache (worker teardown and respawn)

A live WebSocket makes a page ineligible for the back/forward cache, so `connect` tears the transport down on `pagehide` and re-establishes it on `pageshow`:

- **`pagehide`**: `saveFormState()` first (the pageshow resync's `OP_REPLACE` rebuilds the DOM the bfcache preserved), then `_worker.terminate()` -- **synchronous**, because by the time `pagehide` fires the socket is still open and an async `workerPost(W_CLOSE)` may not run before the page freezes. The connection classes are flipped to `az-disconnected` by hand, since the abrupt terminate produces no `[2, closeCode]` message. (Closing popped-out PiP windows is `disconnect()`'s job, not `pagehide`'s -- each PiP window has its own `pagehide` handler that moves its view back inline.)
- **`pageshow`**: only on a real restore (`e.persisted`) and only when no worker exists, `spawnWorker(true)` -- the `true` flags it as a reconnect, because the restored DOM already carries the live state it had at `pagehide`, so the server must resync rather than mount fresh. The respawned worker starts with an empty in-memory fingerprint cache, so `sendCachedFps` gates the mandatory announcement on `_hydrated` (IDB hydration settled); announcing before that would claim an empty cache and forfeit the dedup.

E2E: `e2e/parallel/bfcache.spec.js`.

## Element hooks (`az-hook`)

Register hooks in `hooks` object before `connect()`. Elements with `az-hook="HookName"` get lifecycle callbacks.

**Hook instance:** `{ el, __name, pushEvent(name, payload) }`. Callbacks called with `this = instance`. The instance's prototype **is the hook definition**, so a hook's own helper methods are callable as `this.method()` from any lifecycle callback, and per-instance state assigned to `this` (e.g. `this.chart` in `mounted`) is an own property -- isolated per element, never shared across instances or written back to the def.

**Contract:** `updated()` fires on a hooked element when the framework mutates **that element itself** -- its attributes, its child node list, or its position among its siblings. It does **not** bubble: an ancestor hook is not told that a descendant changed. A newly inserted element gets `mounted()`, not `updated()`; a removed one gets `destroyed()` before detachment. Within one op the order is every `destroyed()` -> DOM mutation -> every `mounted()` -> `updated()`.

`az-hook` is read **once, at mount**. An element that gains the attribute later never mounts, and one that loses it keeps its instance (and its callbacks) until `disconnect()`.

**Lifecycle:**
- `mounted()` -- on a **first** connect (`mountHooks(document)` runs on the open message only when it is not flagged a reconnect), after `OP_INSERT`/`OP_UPDATE`/`OP_REPLACE`/`OP_TEXT` (marker path **and** markerless-HTML path)/`OP_LIST_PATCH` inserts. Guarded -- never double-fires.
- `updated()` -- after `OP_SET_ATTR`/`OP_REM_ATTR`/`OP_UPDATE`/`OP_TEXT` (all three sub-paths; **not** when the slot is unterminated and the op is refused), on the **container** for `OP_INSERT`/`OP_REMOVE`/`OP_MOVE` and for an `OP_LIST_PATCH` that changed the child list, on the **moved item** as well for `OP_MOVE` (its position is its own state), and on the element holding a `?local` slot after `arizona.set`/`setAll`. A pure `OP_ITEM_PATCH` notifies whatever its inner ops touch, not the container -- it mutated a descendant. Only for persisting elements. Every DOM-mutating `arizona_js` effect also fires `updated()` on every element it touches, so a client-driven change is observable to a hook exactly like the server-driven diff: the attribute effects (`set_attr`/`remove_attr`/`toggle_attr`) go through the canonical writers `applySetAttrOp`/`applyRemAttrOp` (which also sync the form-control `value` property like `OP_SET_ATTR`/`OP_REM_ATTR`), and the class/visibility effects (`add_class`/`remove_class`/`toggle_class`/`toggle`/`show`/`hide`) call `notifyUpdated` after mutating `classList`/`hidden`.

**Selector targeting:** the broadcast effects (`toggle`/`show`/`hide`/`add_class`/`remove_class`/`toggle_class`/`set_attr`/`remove_attr`/`toggle_attr`/`reset_form`) act on **all** elements matching the selector, via `withQueryAll` (`querySelectorAll` across the main + PiP documents). `reset_form` calls each match's native `reset()` (a non-form match is a safe no-op) and fires `updated()` like the class/visibility effects. The single-target effects (`focus`/`blur`/`scroll_to`/`select`/`copy_to_clipboard`/`show_modal`/`close_modal`) act on the **first** match only, via `withQuery`. `select` calls the match's `select()` and `copy_to_clipboard` writes its `value`/`textContent` via `navigator.clipboard.writeText` (neither is a DOM mutation, so no `updated()`); `show_modal`/`close_modal` call the match's `showModal()`/`close()` and fire `updated()` like the class/visibility effects (a non-dialog match is a safe no-op). `dispatch_event` takes no selector but follows the same reach: it dispatches a fresh `CustomEvent` on **every** hosting document, so a listener registered inside a popped-out view receives it too.
- `destroyed()` -- before `OP_REMOVE_NODE`/`OP_REMOVE`/`OP_REPLACE`/`OP_UPDATE`/`OP_TEXT`/`OP_LIST_PATCH` removals, and before a `?local` write (`arizona.set`/`setAll`) replaces a slot's content. Called BEFORE DOM mutation, while the element is still attached.

**Key distinction:** `OP_UPDATE` and `OP_TEXT`'s **markerless** path use `destroyChildHooks` (descendants only) -- target stays, gets `updated()`. `OP_TEXT`'s **marker** path is narrower still: `forEachElementBetweenMarkers(marker, destroyHooks)` tears down only the elements inside the slot span, root-inclusive for each, so a hooked sibling *outside* the markers survives a slot re-render. `OP_REPLACE`/`OP_REMOVE_NODE` use `destroyHooks` (root + descendants).

**A hook on the view root sees slot changes non-uniformly**, and this is correct under the contract rather than a gap: it is notified for a marker-anchored slot that is its own direct child (its child nodes really did change), and stays silent for a sibling slot anchored on a descendant element (that element's children changed, not the root's).
