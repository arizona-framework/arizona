---
description: Client runtime and element hooks conventions
paths:
  - "assets/**"
  - "e2e/**"
---

# Client Runtime

`assets/js/arizona.js` -- ES module, zero dependencies. Built by Vite to `priv/static/assets/js/arizona.min.js`.

Exports: `connect`, `applyOps`, `applyEffects`, `resolveEl`, `pushEvent`, `pushEventTo`, `OP`, `hooks`, `mountHooks`.

- `connect(endpoint, params?)` -- bootstrap: spawns Worker, installs document/window-level event delegation, takes over `history.scrollRestoration`. **Returns a `disconnect` function** that aborts every listener it registered, terminates the Worker, clears module state (`_connected`, `_pendingScroll`, saved forms), and restores the previous `scrollRestoration`. Idempotent. Use for teardown in tests and for host apps that want to unmount Arizona on route change.
- `applyOps(ops)` -- applies opcodes. `OP_TEXT` uses comment markers (`<!--az:X-->...<!--/az-->`). `OP_UPDATE` does innerHTML, `OP_REPLACE` does outerHTML. Triggers hook lifecycle callbacks.
- `applyEffects(effects)` -- `push_event` (CustomEvent on document), `set_title`, `reload` (dev-mode).
- `resolveEl(target)` -- splits `"viewId:az"`, finds `getElementById(viewId)` then `[az="az"]`.
- `pushEvent(event, payload)` / `pushEventTo(view, event, payload)` -- send over WebSocket.

## Navigation scroll behavior

- Push nav (`az-navigate` click, `arizona_js:navigate/1,2` without replace): saves outgoing scroll onto the current history entry via `replaceState`, `pushState`s the new URL, resets scroll to top (or `#hash` target) after OP_REPLACE. Opt out with `az-noscroll` on the link or `{noscroll: true}` on the effect.
- Replace nav (`arizona_js:navigate(Path, {replace: true})`): `replaceState` only. Does NOT save outgoing, does NOT reset.
- Popstate (back): restores `e.state._azScroll` after OP_REPLACE, falls through to `#hash` target or top.
- Forward-after-back: destination entry has null state -> scroll to top. Documented non-goal; future restore would need a state-ID-keyed map backed by sessionStorage, not `replaceState`-on-scroll.
- Modifier clicks (ctrl/cmd/shift/alt, non-primary button) on `az-navigate` links fall through to the browser.

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

## Connection detection

Server-side: handlers use `?connected` macro (delegates to `arizona_live:connected()`) in `mount/1` to detect WS vs SSR context. For effects, use `self() ! arizona_connected` and handle in `handle_info/2`. No `az-connect` HTML attribute -- connection is fully server-driven.

## Element hooks (`az-hook`)

Register hooks in `hooks` object before `connect()`. Elements with `az-hook="HookName"` get lifecycle callbacks.

**Hook instance:** `{ el, __name, pushEvent(name, payload) }`. Callbacks called with `this = instance`.

**Lifecycle:**
- `mounted()` -- on `ws.onopen`, after `OP_INSERT`/`OP_UPDATE`/`OP_REPLACE`/`OP_TEXT` marker path. Guarded -- never double-fires.
- `updated()` -- after `OP_SET_ATTR`/`OP_REM_ATTR`/`OP_UPDATE`/`OP_TEXT` textContent. Only for persisting elements. Every DOM-mutating `arizona_js` effect also fires `updated()` on the element it touches, so a client-driven change is observable to a hook exactly like the server-driven diff: the attribute effects (`set_attr`/`remove_attr`/`toggle_attr`) go through the canonical writers `applySetAttrOp`/`applyRemAttrOp` (which also sync the form-control `value` property like `OP_SET_ATTR`/`OP_REM_ATTR`), and the class/visibility effects (`add_class`/`remove_class`/`toggle_class`/`toggle`/`show`/`hide`) call `notifyUpdated` after mutating `classList`/`hidden`.
- `destroyed()` -- before `OP_REMOVE_NODE`/`OP_REMOVE`/`OP_REPLACE`/`OP_UPDATE`/`OP_TEXT`. Called BEFORE DOM mutation.

**Key distinction:** `OP_UPDATE`/`OP_TEXT` use `destroyChildHooks` (descendants only) -- target stays, gets `updated()`. `OP_REPLACE`/`OP_REMOVE_NODE` use `destroyHooks` (root + descendants).
