---
name: trace-event
description: Trace an event through the full Arizona stack from client click to DOM patch. Use when debugging event handling.
argument-hint: [event_name]
allowed-tools: Read, Grep, Glob
---

Trace the `$ARGUMENTS` event through the full stack:

## 1. Client-side dispatch

How the event is triggered in `assets/js/arizona.js`:
- Which `az-*` attribute binds it (e.g., `az-click`, `az-submit`, `az-change`)
- Target resolution: `resolveEl` splits `"viewId:az"` to find the element
- Wire format sent: `[target, eventName, payload]`

## 2. WebSocket handler

How `src/arizona_cowboy_ws.erl` receives and `src/arizona_socket.erl` routes:
- `websocket_handle` forwards the text frame to `arizona_socket:handle_in/2`
- `handle_in` decodes the JSON array and calls `arizona_live:handle_event/4` with
  `(Pid, ViewId, Event, Payload)`

## 3. Live process

How `src/arizona_live.erl` dispatches:
- Root vs child view routing (checks `views` map for `ViewId`)
- Calls `arizona_handler:call_handle_event(H, Event, Payload, Bindings)` which invokes the
  handler's `handle_event/3`
- Gets back `{NewBindings, Resets, Effects}`
- Computes changed keys via `compute_changed/2`
- Re-renders via `arizona_handler:call_render/2`
- Diffs: `arizona_diff:diff/4(NewTmpl, OldSnap, Views, Changed)`

## 4. Diff & ops

What ops are produced:
- Which dynamics changed based on dep tracking
- Op codes generated (OP_TEXT, OP_SET_ATTR, etc.)
- How ops are scoped with view ID by `scope_ops/2`

## 5. Response & client patch

How the response reaches the browser:
- JSON envelope: `{"o": scopedOps, "e": effects}`
- `applyOps(ops)` in arizona.js applies each op
- `applyEffects(effects)` processes push_event/set_title
- Hook lifecycle callbacks triggered

Find the handler that processes `$ARGUMENTS` using: `Grep for handle_event.*$ARGUMENTS`
