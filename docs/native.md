# Native (JSON) render target

Arizona renders HTML to browsers via `?html`. The **native** target renders the
same server-side-diffing model to a **JSON widget tree** that a thin native
client (iOS/SwiftUI, Android/Jetpack Compose, or any other) turns into real
widgets. The server stays the source of truth: it ships JSON + diff ops over the
WebSocket and receives events back, reusing the *entire* engine — the diff,
the fingerprint cache, the op codes, and the transport — unchanged.

The web (`?html`) path is untouched: same HTML statics, SSR, SEO, and browser
client.

## Authoring

Use `?native(...)` instead of `?html(...)`. The element-tuple form, macros, and
handlers are identical:

```erlang
-include_lib("arizona/include/arizona_view.hrl").

mount(Bindings, _Req) ->
    {#{id => ~"counter", count => maps:get(count, Bindings, 0)}, #{}}.

render(Bindings) ->
    ?native({'Column', [{id, ?get(id)}, {padding, ~"16"}], [
        {'Text', [], [?get(count)]},
        {'Button', [{on_tap, arizona_android:push_event(~"inc")}], [~"+"]},
        {'List', [], [
            ?each(fun item/1, ?get(items))
        ]}
    ]}).

item(Item) ->
    {'Text', [], [maps:get(label, Item)]}.

handle_event(~"inc", _Payload, Bindings) ->
    {Bindings#{count => maps:get(count, Bindings) + 1}, #{}, []}.
```

- **The tag is the widget type, verbatim.** `'Column'`, `'VStack'`, `'Text'`,
  `'Button'`, … The framework is **vocabulary-agnostic** — it never interprets
  the tag; the client engine maps the `type` string to a real widget. There is
  no fixed widget list and no server-side validation: an unknown widget is the
  client's / your tests' concern, not a compile error.
- **Attributes are widget props.** Static and dynamic both supported. Values are
  string-encoded on the wire; the client coerces (typed props are a future
  addition).
- **Event handlers are props** whose value is an effect command built with the
  platform module: `{on_tap, arizona_android:push_event(~"inc")}`. Events flow
  back exactly as on the web (`[ViewId, Event, Payload]` → `handle_event/3`).
- **`?each` works uniformly.** A `?each` inside `?native` is compiled for the
  native target automatically — no separate macro. `?stateful`/`?stateless`
  children also work; the embedded module must use the same target (see
  *Constraints*).

## Serving HTML and native from one view

The framework injects nothing and decides nothing. To serve browsers HTML and
native apps a JSON tree from a single view, read the connecting client in
`mount/2` and branch in `render/1`.

`arizona_req:user_agent/1` returns the raw `User-Agent` header (no custom header
needed — most clients send one), or `<<>>` if absent. `arizona_user_agent`
turns it into a coarse, best-effort classification through **pure, composable**
helpers — call only the one(s) you need, no precomputed map:

- `browser/1` — true when the UA carries `Mozilla` (browsers/webviews do; a
  native app's HTTP stack typically does not).
- `os/1` — `ios | android | windows | macos | linux | other`.
- `mobile/1` — best-effort mobile-device guess.

```erlang
mount(_Bindings, Req) ->
    {UA, _Req1} = arizona_req:user_agent(Req),
    {#{id => ~"page", user_agent => UA}, #{}}.

%% A browser/webview carries "Mozilla" -> HTML; a native app's HTTP stack
%% typically does not -> native. Reach for arizona_user_agent:os/1 or mobile/1,
%% or match the raw UA (e.g. your app's custom UA), only when you need to.
render(#{user_agent := UA} = Bindings) ->
    case arizona_user_agent:browser(UA) of
        true -> ?html({'div', [{id, ?get(id)}], [...]});
        false -> ?native({'Column', [{id, ?get(id)}], [...]})
    end.
```

These helpers are heuristic and request-level; `user_agent/1` works at `mount/2`
on both the HTTP and WS paths. You own the binding name and the branch — if you
prefer an explicit signal, your client can send a query param and you read it
with `arizona_req:params/1` instead.

## The JSON widget tree

Each element compiles to a flat JSON object — `type`, `az`, and `children` are
reserved keys; every other key is a prop:

```json
{
  "type": "Column",
  "az": "f1-0",
  "az-view": "counter",
  "id": "counter",
  "padding": "16",
  "children": [
    { "type": "Text", "az": "f1-1",
      "children": [ { "type": "#slot", "az": "f1-1t0", "children": ["42"] } ] },
    { "type": "Button", "on_tap": [0, "inc"], "children": ["+"] }
  ]
}
```

- **`az`** is the diff-target token. Ops address `"ViewId:az"`; the client keeps
  an `az` → node registry.
- **`az-view`** marks a view root; its value is the `ViewId` (the `id` returned
  by `mount`). Events from a node route to the nearest enclosing `az-view`.
- **`#slot`** is the addressable, *transparent* wrapper for any **dynamic** child
  (dynamic text, an `?each`, or a child component). The client renders a `#slot`
  by splicing its `children` into the parent — a fragment. One `#slot` per
  dynamic child (the JSON analogue of the HTML comment markers).
- **No void elements** — a childless widget is just `"children": []`.

## Wire protocol — reuses the fingerprint cache

Native uses the **same** op codes and the **same** statics-deduplication as the
browser, so statics are sent once and never re-sent:

- The first time a fingerprint `f` is seen, the payload is
  `{"f": F, "s": [...statics...], "d": [...values...]}`.
- Once the client has cached `f` (it reports them via `["cached_fps", [...]]` on
  connect), the server omits `s`: `{"f": F, "d": [...]}`.
- `d` carries **raw** values (strings/numbers). The browser concatenates them;
  the native client `JSON.stringify`s each one as it interleaves.
- Op codes (`OP_TEXT`, `OP_SET_ATTR`, `OP_UPDATE`, `OP_INSERT`/`OP_MOVE`/
  `OP_REMOVE`/`OP_ITEM_PATCH`, `OP_REPLACE`, …) are identical; only the payload
  *content* is a JSON widget skeleton instead of HTML.
- There is no SSR/HTTP page for native: the first frame is the live process's
  `mount_and_render` output wrapped as `OP_REPLACE` over the WebSocket.

## Native client contract

Reference implementations live in-repo: `e2e/utils/native_client.js` (a JS client used
by the `native` e2e project) and `clients/android/` (the Kotlin/Compose Android client).
Both are near-copies of the browser worker (`assets/js/arizona-worker.js` + `arizona-core.js`):

1. **Cache statics by fingerprint** in platform storage (browser uses
   IndexedDB); send `["cached_fps", [...]]` on connect.
2. **Interleave** `s` + `d` to reconstruct content — identical to the browser's
   `zipTemplate`, except each `d` value is **JSON-encoded** (string → quoted,
   number → as-is) rather than text-concatenated. Recurse into nested
   `{f,s,d}` payloads and each `{t:0,...}` item-lists.
3. **Flatten fragments:** render a `#slot` by splicing its `children` into the
   parent, and splice a nested array (an `?each` expansion) one level up. So
   `["#slot children"]` and `[[item, item]]` both flatten into the parent's
   children.
4. **Apply the op codes** against an `az` → node registry (the browser uses the
   DOM): `OP_TEXT` sets a `#slot`'s content, `OP_SET_ATTR`/`OP_REM_ATTR` set/drop
   a prop, and the stream ops manage a container's keyed children — items keyed
   by `az_key`, with `OP_INSERT`/`OP_REMOVE`/`OP_MOVE` reordering the list,
   `OP_ITEM_PATCH` applying inner ops scoped to one item, and `OP_UPDATE`
   re-rendering the whole list.
5. **Run effects and navigate.** A tap fires its node's command prop; the
   server's `"e"` (effects) array is dispatched after the ops. The portable
   commands are handled — `push_event`, and `navigate` (which sends
   `["navigate", {path, qs}]` on the same socket; the server's `handle_navigate`
   re-mounts the target view and replies with `OP_REPLACE`). Web-only effects
   (`set_title`, `dispatch_event`, …) are skipped on native.
6. **Reconnect with backoff.** On a dropped socket — any close other than a
   normal `1000`, or an abrupt failure — reopen with exponential backoff
   (`[1, 2, 5, 10]`s steps, capped at 10s, ±20% jitter), resetting the attempt
   counter once a frame arrives. Because the native URL carries `_az_reconnect=1`,
   each reopen re-mounts and replies with a fresh `OP_REPLACE` (native has no
   form state to preserve — that path is browser-only), so a server restart
   self-heals. An intentional `close()` (code `1000`) does not reconnect. The
   browser's 30s heartbeat ping/pong (`'0'`/`'1'`) for silent half-open detection
   is a follow-up; a TCP-level drop already surfaces via the socket's failure
   callback.

The `native` e2e exercises each example over the real socket: a counter
(`/native/counter`), a keyed list (`/native/list`), conditional tab switching
(`/native/tabs`), server-pushed ticks (`/native/ticker`), independent counters
(`/native/multi`), navigation (`/native/menu`), and reconnect-after-drop. The in-repo Android sample
(`clients/android`) is a launcher that opens `/native/menu` and navigates to each
on a device.

An iOS/SwiftUI client would follow the same contract (e.g. `mob`'s SwiftUI renderer is a
natural starting point — point it at an Arizona WebSocket instead of an on-device NIF).

## Effect commands

Event-attribute values and `handle_event/3` effects are built **per platform**,
all producing the same neutral wire format (`{arizona_effect, [OpCode | Args]}`,
op codes in `include/arizona_effect.hrl`, encoded by `arizona_effect`):

- **`arizona_js`** — web/browser builders. `push_event`, `navigate`, plus
  DOM-specific ones: `toggle`/`show`/`hide`, the class ops, `set_attr`/
  `remove_attr`, `dispatch_event`, `focus`/`blur`/`scroll_to` (CSS selectors),
  `on_key` (keyboard), `set_title`, `reload`.
- **`arizona_android`** — native builders for `?native` views. `push_event` and
  `navigate` (duplicated; same wire as the web ones), and the home for
  Android-specific commands as they're added.

Only `push_event`/`navigate` are portable enough to exist on both — they're pure
client→server actions. The DOM/selector/keyboard commands (`toggle`, `focus`,
`scroll_to`, `on_key`, …) are web-only by nature and live only in `arizona_js`.

The reference native clients implement both portable commands: `push_event`
(from a tap or the `"e"` array) and `navigate` (a same-socket view transition).
Web-only effects appearing in the `"e"` array are skipped, so a handler that
returns them won't crash a native client.

## Constraints

- **One target per view tree.** A `?native` view's `?stateful`/`?stateless`/
  `?each` children must also be `?native` (and vice-versa for `?html`). Mixing
  targets in one tree mismatches the statics format. This is a documented rule,
  not a checked one.
- **Events route to the view root.** A native client sends events to the
  `OP_REPLACE` ViewId; routing to a nested `?stateful` child (which needs the
  child's own view id) isn't supported yet — the multi-counter example uses
  per-region events on a single view. Stateful children still *render*; only
  event routing to them is the gap.
- **No server-side widget validation** (let-it-crash / the client's concern).
- **Props are string-encoded**; the client coerces. Typed props, opt-in
  per-platform compile-time validators, and a portable cross-platform
  vocabulary helper are possible future additions.
