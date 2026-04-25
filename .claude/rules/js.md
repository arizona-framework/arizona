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

## Connection detection

Server-side: handlers use `?connected` macro (delegates to `arizona_live:connected()`) in `mount/1` to detect WS vs SSR context. For effects, use `self() ! arizona_connected` and handle in `handle_info/2`. No `az-connect` HTML attribute -- connection is fully server-driven.

## Element hooks (`az-hook`)

Register hooks in `hooks` object before `connect()`. Elements with `az-hook="HookName"` get lifecycle callbacks.

**Hook instance:** `{ el, __name, pushEvent(name, payload) }`. Callbacks called with `this = instance`.

**Lifecycle:**
- `mounted()` -- on `ws.onopen`, after `OP_INSERT`/`OP_UPDATE`/`OP_REPLACE`/`OP_TEXT` marker path. Guarded -- never double-fires.
- `updated()` -- after `OP_SET_ATTR`/`OP_REM_ATTR`/`OP_UPDATE`/`OP_TEXT` textContent. Only for persisting elements.
- `destroyed()` -- before `OP_REMOVE_NODE`/`OP_REMOVE`/`OP_REPLACE`/`OP_UPDATE`/`OP_TEXT`. Called BEFORE DOM mutation.

**Key distinction:** `OP_UPDATE`/`OP_TEXT` use `destroyChildHooks` (descendants only) -- target stays, gets `updated()`. `OP_REPLACE`/`OP_REMOVE_NODE` use `destroyHooks` (root + descendants).
