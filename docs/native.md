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
        {'Button', [{on_tap, arizona_js:push_event(~"inc")}], [~"+"]},
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
- **Event handlers are props** whose value is an `arizona_js` command:
  `{on_tap, arizona_js:push_event(~"inc")}`. Events flow back exactly as on the
  web (`[ViewId, Event, Payload]` → `handle_event/3`).
- **`?each` works uniformly.** A `?each` inside `?native` is compiled for the
  native target automatically — no separate macro. `?stateful`/`?stateless`
  children also work; the embedded module must use the same target (see
  *Constraints*).

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

## Native client contract (out of scope to build here)

A native client is a near-copy of the browser worker
(`assets/js/arizona-worker.js` + `arizona-core.js`):

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
   DOM): `OP_TEXT` sets a `#slot`'s content, `OP_SET_ATTR` sets a prop,
   `OP_INSERT`/`OP_MOVE`/`OP_REMOVE` manage a keyed list, etc.

`mob`'s SwiftUI/Compose renderers are a natural starting point — point them at
an Arizona WebSocket instead of an on-device NIF.

## `arizona_js` effects

Handler effects use the same `arizona_js` module. Some are universal; some are
browser-only (a native client ignores what it doesn't implement):

- **Universal:** `push_event/1,2`, `navigate/1,2`, `focus/1`, `blur/1`,
  `scroll_to/1,2`, `on_key/2`.
- **Browser-only:** `toggle`/`show`/`hide`, the class ops, `set_attr`/
  `remove_attr` (use props), `dispatch_event`, `set_title`, `reload`.

## Constraints

- **One target per view tree.** A `?native` view's `?stateful`/`?stateless`/
  `?each` children must also be `?native` (and vice-versa for `?html`). Mixing
  targets in one tree mismatches the statics format. This is a documented rule,
  not a checked one.
- **No server-side widget validation** (let-it-crash / the client's concern).
- **Props are string-encoded**; the client coerces. Typed props, opt-in
  per-platform compile-time validators, and a portable cross-platform
  vocabulary helper are possible future additions.
