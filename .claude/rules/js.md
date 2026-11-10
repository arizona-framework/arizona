---
description: Client runtime and element hooks conventions
paths:
  - "assets/**"
  - "e2e/**"
---

# Client Runtime

`assets/js/arizona.js` -- ES module, zero dependencies. Built by Vite to `priv/static/assets/js/arizona.min.js`.

It is the entry point only: it re-exports the public API from the modules beside it, split by concern.
The **shared** runtime -- `arizona-hooks.js`, `-docs.js` (multi-document / PiP), `-slots.js` (comment
markers), `-attrs.js`, `-locals.js` (`?local`), `-transitions.js`, `-commands.js` (`executeJS`),
`-events.js` (delegation), `-util.js` -- never imports the **live** side (`arizona-connection.js`: the
socket, the Worker and navigation; `-ops.js`: the diff ops; `-scroll.js`). Where it needs one -- send an
event, navigate, ask whether a socket is open -- it calls through `host` (`arizona-host.js`), which
`arizona-connection.js` fills in when it is loaded. Keep that direction: a shared module that imports a
live one ties every page to the socket. Mutable module state is written only in the module that
declares it (an ES import is read-only), through a setter when another module must change it.

**Static entry.** `assets/js/arizona-static.js` is a second entry, built to
`priv/static/assets/js/arizona-static.min.js` (a second Vite pass, `ARIZONA_BUILD_PASS=static`) and
exported as `./static`. It imports only the shared modules and installs its own `host`: `ready` is
always true, `sendEvent` warns and drops (`push_event "<name>" dropped: this page has no server`),
`navigate`/`leavePage` load the URL as a page (`loadPage`, http(s) only), and a transition waits for
no reply. It exports `start`, `executeJS`, `exitPip`, `requestPip`, `mountHooks`, `hooks`, `get`,
`set`, `setAll`. `start({attrs}?)` binds delegation, notes the event names (`attrs`, else the JSON
list `arizona_static` embedded in `#arizona-attrs`, read by the shared `embeddedAttrs`), mounts hooks, and returns an idempotent `stop`
(unbinds, closes PiP windows, destroys hooks); a second `start()` stops the first. The host member
`ready` -- whether delegated commands run now -- is the socket being open on a live page. The static
bundle must never grow a Worker or a socket: anything it needs from the live side goes through
`host`.

Exports (the full list, alphabetical as in the module): `applyEffects`, `applyOps`, `connect`, `executeJS`, `exitPip`, `get`, `hooks`, `mountHooks`, `OP`, `pushEvent`, `pushEventTo`, `requestPip`, `resolveEl`, `restoreFormState`, `saveFormState`, `set`, `setAll`.

- `connect(endpoint, params?)` -- bootstrap: spawns Worker, installs document/window-level event delegation, takes over `history.scrollRestoration`. **Returns a `disconnect` function** that aborts every listener it registered, terminates the Worker, clears module state (`_connected`, `_pendingScroll`, saved forms), and restores the previous `scrollRestoration`. Idempotent. Use for teardown in tests and for host apps that want to unmount Arizona on route change. **One live connection at a time:** the connection owns module-level state, so calling `connect` again warns and retires the previous one first (otherwise its Worker keeps a second socket patching the same document, and its `disconnect` -- reading the same module state -- would tear down the newer connection instead).
- `applyOps(ops, resync?)` -- applies opcodes (`resync` marks a reconnect's resync batch, whose `OP_REPLACE` keeps az-nodiff elements -- see "Kept az-nodiff elements" under element hooks). `OP_TEXT` uses comment markers (`<!--az:X-->...<!--/az-->`), falling back to `textContent`/`innerHTML` on the resolved element when the slot has none. `OP_REPLACE` swaps the target for the parsed fragment and mounts hooks on the nodes it inserted (the target's id names the *outgoing* view, so it cannot be re-resolved after the swap). Triggers hook lifecycle callbacks. **Op code 3 is unassigned** -- it was the innerHTML op, removed once every container full render became `OP_TEXT`; it is free to reuse, but only by changing `include/arizona_wire.hrl` and every client together (the web client, `json_client.js`, and the `arizona_android`/`arizona_ios` repos).
  - **`OP_TEXT` text vs HTML.** A scalar `?get` value arrives on the WS wire as a bare string; an HTML fragment (a nested-template / plain-list-`?each` zip-map, or a `?raw` `{raw}` tag) arrives as an object. The worker (`resolveOps`) records which -- `isHtml = typeof payload !== 'string'` -- *before* `resolveHtml` flattens both to a string, and sets `op[3] = true` only for HTML (the common text case stays a 3-element op; a missing `op[3]` is falsy). The main thread then renders text via a **text node** (`createTextNode` / `textContent` -- a `<` shows as literal text, never parsed, so a `?get` value can't inject and matches SSR) and HTML via **innerHTML**. `?raw` is the only scalar that needs the object tag, so the client can tell trusted markup from a value that merely contains `<`. This is the diff-side completion of server-side auto-escaping (`render_dyn`/`escape_value` at SSR): the wire stays raw, the client is the escaping boundary. `OP_SET_ATTR` follows the same rule: a value is a bare string set verbatim with `setAttribute`, and a `?raw` attribute value arrives tagged `{raw}` -- it is attribute markup, so `decodeRawAttr` parses it inside `name="..."` in an inert `<template>` to get the value SSR's parse produced (`&amp;` -> `&`). A command value is the one array.
- `applyEffects(effects)` -- runs **every** server-sent effect through the one `executeJS` interpreter (`executeJS(document.documentElement, null, eff)` per effect), so a handler effect and an event-attribute command share a single implementation of all the op codes in `include/arizona_effect.hrl`. There is no separate server-effect subset: `push_event` sends over the **WebSocket** (`workerPost(W_SEND, ...)`) -- it is `dispatch_event` that fires a `CustomEvent`, on every hosting document -- `set_title` writes `document.title`, `reload` is dev-mode, and the selector effects behave exactly as under an event attribute. The trigger element is `document.documentElement`, so the ops that resolve a view from the trigger degrade: `resolveTarget` returns null, `sendTarget` falls back to the **root** view (`push_event`), and `set_local` without an explicit view id (`arizona_js:set/2`) finds no view and is a no-op (handlers use `set/3` or `set_all/2`). Ops needing a user gesture (`copy_to_clipboard`, `request_pip`) are event commands in practice -- a server-pushed one is caught and logged, not thrown.
- `resolveEl(target)` -- splits `"viewId:az"`, finds `getElementById(viewId)`, then tries three lookups in order: an element carrying the az (the view root itself or a descendant); for a compound `"X:n"` slot az, the base `[az="X"]` element; and finally the slot's `<!--az:X-->` comment marker anywhere in the view subtree, returning its **parent element**.
  - **The marker arm is what makes a marker-only slot patchable.** A template whose whole body is a bare dynamic (`?html(case ... end)`, `?html(?get(x))`, `?html(?each(...))`, `?html(?stateless(...))`, a mixed top-level fragment) and a stream/list `?each` among static siblings anchor their slot with a marker pair that **no element carries**, so without this arm every op addressed to them resolves to nothing and is dropped. Arm 2 does not subsume it: `querySelector` searches descendants, so a compound az whose base is the view root's own az finds nothing.
  - **A marker hit is refused by the ops that act on the whole element.** `resolveEl` is a thin wrapper over `resolveOpTarget`, which reports `{el, marker}` -- `marker: true` meaning `el` is only the slot's PARENT. `applyOps` skips `MARKER_UNSAFE_OPS` (`OP_REPLACE`, `OP_REMOVE_NODE`) on such a hit, warning and skipping like an unresolved target: they would `replaceWith`/`remove` the parent and take the slot's siblings (the whole view when the parent is the live root). The marker-aware ops keep the resolution -- `OP_TEXT`, `OP_LIST_PATCH`, the position-independent `OP_REMOVE`/`OP_ITEM_PATCH` (which find their item by `az-key`), and the stream placement ops `OP_INSERT`/`OP_MOVE`, which place inside the slot span through `slotBounds` (the markers are the parent's direct children).
  - **The per-batch memo re-validates a marker hit.** `applyOps` caches target -> resolution; `isConnected` only proves the parent survived, so a marker hit is additionally re-checked with `findMarker` before reuse -- an earlier op in the same batch can re-render the enclosing slot and destroy the inner marker, and the stale hit would otherwise reach `OP_TEXT`'s whole-element fallback and wipe the parent.
  - **Item ops resolve the same three arms, with no fallback to the item.** `applyItemOps` resolves an inner az through `resolveInnerEl`, which mirrors `resolveOpTarget` within the item (the item itself or a descendant, the compound base, then the slot marker anywhere in the item, flagged `marker`). A miss warns and skips: falling back to the item would let `OP_TEXT` rewrite the whole item and `OP_INSERT` append after the item's content when the slot's marker sits in a deeper element (a stream behind a conditional fragment inside the item).
  - **Stream lookups are scoped to the slot span.** Two streams can share one parent element (a second `?each` among siblings takes the compound `<El>:1` az and resolves to the same element) with overlapping keys, so `itemByKey`, the per-batch key maps and positional indexing read only the keyed children between the stream's own markers -- otherwise `OP_REMOVE`/`OP_ITEM_PATCH` for one stream hits the other's item. The batch cache (`StreamCache`) keys the maps by (element, az) and memoizes each (element, az) span it finds. A cached span is reused only while both its markers are still children of the element: no op moves a marker, a re-render around the span (an enclosing `OP_TEXT`, a wholesale write to the element) removes them, and what it inserts is balanced, so that check alone is exact. Recomputing the span per op instead would re-walk the list after every row's inner `OP_TEXT` and make a whole-table update quadratic. A miss is not memoized: a later op in the batch can render the slot.
- `pushEvent(event, payload)` / `pushEventTo(view, event, payload)` -- send over WebSocket.
- `executeJS(el, event, cmds)` -- the shared command interpreter (`execOne` per command); exported so a hook can run an `arizona_js` command list itself.
- `set(viewId, key, value)` / `setAll(key, value)` / `get(key)` / `get(viewId, key)` -- the client-owned slot (`?local`) API; see below.
- `requestPip(viewId, opts?)` / `exitPip(viewId)` -- pop a view out into a document-picture-in-picture window / close it.
  - **A popped-out view keeps being patched in its window.** Its root moves into the PiP document and a comment placeholder holds its inline place; `findViewRoot` resolves ops by containment, so the view and any child view inside it are patched there. After every `applyOps` batch, `reconcilePipWindows` brings each window in line with what the batch did (once per batch, after every op, since any op can take a placeholder, a nested `OP_TEXT` in an item patch included):
    - **A window is stale when its view no longer stands for anything in the page**: its placeholder is gone, or lies inside another window's stale copy (a child popped out before its parent leaves its placeholder in the parent's copy), or inside a window already closed. Staleness follows that chain up to the page, so windows are reconciled in any order: a child is judged right whether its parent's window is settled yet or not, and a stale parent copy holds only the child's placeholder, never a copy of it.
    - **A replace in a live window under a new id re-keys it.** A navigate of a popped-out root lands in the window (the root is resolved there) as the new page, so `exitPip`/`requestPip` and `arizona_js:exit_pip/request_pip` from that page name the new view id.
    - **A stale window whose view the batch rendered again gets the fresh element.** A reconnect's resync `OP_REPLACE` re-renders the whole root and a wholesale `OP_TEXT` of an ancestor re-renders the view, in the page -- or in an ancestor's window when that one is popped out too, never in a stale window (a copy there lies inside one the batch superseded, so of two re-renders in one batch the newer wins): the fresh element replaces the stale one in the window and leaves a new placeholder, as a pop-out does, so the window keeps showing the live view. Its az-nodiff elements are carried over from the window's copy by the same identity rules as an inline re-render (`carryPoppedNodiff` inside `keepNodiff`), before any hook mounts, so a widget there is neither remounted nor reset -- but only from a window still live when the op runs: one an earlier op in the batch made stale had its copy retired, so its widgets are dead, and carrying them (or a nested window's placeholder inside one) would revive what is gone. Inline, a view that leaves one slot and lands in another in the same batch remounts its widgets too. Every op that mounts new content first tears down the hooks of every stale window's copy (`retireStaleCopies`, once per op, before its mount loop), the inline order: the op that took a placeholder is the one that would have destroyed the view, and a stale copy never becomes live again, so it is gone whether its view comes back or its window closes. A live window's view is never touched, whatever ids the new content reuses. Only an `az-view` element counts as the view: another element reusing its id stays where it was rendered and the window closes. The window follows the view **id**, so a different handler mounted under the same id by a non-navigate re-render inherits it -- the server treats an id as one view outside a navigate.
    - **Otherwise the view is gone, and its window closes.** A removal took it, or the batch was a navigate (a non-resync `OP_REPLACE`): a new page's views are new, whatever their ids, so a same-id view on it does not inherit the window. A nested window inside a closed one is stale too, so a navigate closes every window it orphaned. A closed window is unregistered at once: a browser fires its `pagehide` after `close()` returns, and until then no later batch may hand it a view or count it as a home for the placeholders in it.
    - **Until the batch ends, the fresh copy is the one patched.** A stale window's copy stays connected until `reconcilePipWindows` runs, so `findViewRoot` searches stale windows last for **any** view (one inside the stale copy too), and `applyOps`' per-batch memo re-resolves a hit in a PiP document. A main-document hit is never stale, and with no window open the extra work is one `Map.size` read per memo hit.
  - **Closing brings back what the window holds now**, not the element moved out: when the window is not stale, its `az-view` children replace the placeholder (a node a hook added to the PiP body stays behind). When it is stale they are discarded with their hooks torn down -- putting them back would resurrect dead server state, or drop them into another window's stale copy.
- `saveFormState()` / `restoreFormState()` -- snapshot and re-apply typed form fields around a resync that rebuilds the DOM (an abnormal close, a bfcache round-trip). A `readonly` field is server-owned, so it keeps the resync's fresh value (its saved value is still consumed, keeping a repeated name's positions aligned). A `type=hidden` input **is** restored, deliberately: a client widget (a rich-text editor, a date picker, a tag input) keeps the user's input in one, and losing that on a reconnect is worse than the cost -- a server-owned hidden field (a version stamp, a record id) can come back stale after a reconnect resync. Render a server-owned value that must stay fresh as `readonly`, or outside a form with an `id`.

## Event delegation (`az-<event>`)

An `az-<event>` attribute binds `addEventListener(<event>)` for **any** element event. The suffix is
the listener type verbatim -- no mapping table -- so a custom element's own vocabulary works with
nothing registered anywhere: `{az_sl_change, arizona_js:push_event(~"picked")}` renders
`az-sl-change` and binds `sl-change`. (An atom attribute translates `_` to `-`, so an event name
that genuinely contains an underscore needs the binary form, `{~"az-my_event", ...}`. The name must
be lowercase: the HTML parser lowercases attribute names, so `az-slChange` binds a `slchange` that
never fires -- which is what custom elements use kebab-case for anyway.)

**The set of types comes from proofs, never from guessing.** There are exactly two moments the
command-versus-data question is answerable, and the delivery uses both (`arizona_event_attrs`):

- **Compile time.** `compile_attr` records an `az-*` name when a builder call (a module declaring
  `-behaviour(arizona_command_builder)`: `arizona_js`, `arizona_android`, `arizona_os`,
  `arizona_effect`, or one from any package) is visible anywhere in its value expression --
  builders are the only producers of effect tuples, so the call is proof. A builder whose
  `target/0` names another render target than the template's is a compile error, since that
  client would not know its commands. The names land in an
  `arizona_az_attrs` module attribute beside `arizona_az_deps` (the component modules the templates
  name literally, plus every module the module calls by a literal name -- a remote helper's
  template carries its folded names in statics no render re-proves, so a call such as
  `other_mod:button()` must be an edge; a callee reached only through a variable, a fun value, or a
  module compiled without the headers is not, so declare `-arizona_az_deps([mod]).` by hand);
  `arizona_event_attrs:all/1` walks that graph from the route's handler **and
  layout modules** (layouts render once, at SSR, so no frame can ever carry their markup) and the
  socket ships the union on the connect frame, under JSON key `"a"` alongside `"o"`/`"e"`.
- **Render time.** What compile time cannot prove, evaluation can: an opaque dynamic
  (`{az_close, ?get(on_close)}`) that evaluates to an `{arizona_effect, _}` command names its
  attribute, and a `?stateful`/`?stateless` instantiation names its module -- covering a module
  bound at runtime (`?stateful(?get(page), ...)`), which the walk cannot follow. The socket walks
  newly observed modules, dedups against everything already sent, and ships the per-socket delta as
  `"a"` on the **same frame** that first renders the markup, so the type is bound before anything
  can trigger it. After connect, frames carry no `"a"` at all unless something new was proved.

**The page carries its set too, so it is known before the socket opens.** The connect frame lands
a round trip after the open, and a click in that window -- or an HTTP-form `az-submit` `fetch`,
which runs with no socket at all -- must find its proof. So the HTTP render arms the same
collector for the page and embeds the union (`arizona_event_attrs:embed/2`, the render target's
`static_document/2`: `<script type="application/json" id="arizona-attrs">` appended after the
document, which the HTML parser moves into the body as its last element), exactly as
`arizona_static` does for a static page, and `connect()` reads it (`embeddedAttrs`) before anything
can fire -- and again on `DOMContentLoaded` when it runs while the document is still parsing, as a
classic script before the embed does. The client takes the **last** `script[type="application/json"]`
with that id: an id can come from user input, so an earlier element sharing it would otherwise
win, and escaped content cannot create a script element. A page without the embed (a `?json`
document, a page rendered another way) has no proofs until the connect frame, and runs nothing
before it.
There is **no bootstrap set**: delegating common types (`click`, `change`, ...) ahead of any proof
used to cover that window, and it ran app data under those names as commands -- an
`az-change='[22,"https://evil.example",{}]'` posted the form's fields to another origin.

The worker relays each frame's `"a"` as field 4 of
`postMessage([0, ops, effects, firstAfterReconnect, azNames])`; the main thread decides which names
are events and unions monotonically, over what the page embedded.

**Every executor gates on the proven set (`_eventTypes`), never on which listeners exist.** The
generic listeners are bound only for proven types, which is the only way one gets bound. The
dedicated `submit` and `drop` listeners are bound on every page (they carry bookkeeping), so each
asks `proven` before parsing its attribute; an unproven `az-submit` is still `preventDefault`ed
(see below) and runs nothing. The reconnect replay proposes a form's `az-change` or a field's
`az-change`/`az-input` only under a proven name (`proposing`). A `submit` or `drop` proof is
recorded beside the others but binds no generic listener (`DEDICATED`).

Deriving the set from rendered bytes instead was tried and abandoned. A regex over serialized HTML
cannot tell an attribute NAME from `az-` text inside an attribute VALUE, so user content echoed
into a `title` could register document listeners and disable the page's scroll fast path; and it
truncated on a `>` inside an attribute value, silently dropping real events. Both failure modes are
structural, not bugs to patch.

**An unproven value is data, and is deliberately not delegated.** A static binary is data by
construction (an effect is always a call, never a literal), a bare or boolean attribute has no
value to hold a command, and an opaque dynamic that never evaluates to a command is data too --
`{az_select, ?get(ids)}` rendering `az-select="[1,2,3]"` never enters the set, `select` is never
delegated, and the value never reaches the command interpreter -- nor when it changes: a diff op
writing it, like a `?local` attribute write, proves nothing, since the server ships a command a
diff turns up as a JSON array and proves its name on that same frame, so a string value in an op
is always data. That matters because a command list
is a bare JSON array (`arizona_effect:encode/1` emits a single command unwrapped, `[0,"inc"]`), so
`[1,2,3]` is a structurally valid command -- `toggle` with selector `2` -- and **no client-side
inspection of the value can separate the two**. Delegating on guesswork would both leave declared
events dead and hand app data (possibly attacker-influenced) to the command interpreter; the typed
proofs above are what make neither happen. The proof is per **name**, not per element: a name
proven anywhere on the page runs a same-named data value elsewhere on it (`az-change` proven on one
form makes another form's `az-change` data a command list), so never render untrusted data under
an `az-*` name the page also uses for commands. Per-element marking would be a separate design. The
delegated dispatch still wraps parse AND execution in a warn-once containment for the same reason
(a runtime `set_attr`, host markup).

**Two listeners per type, split on the event's own `bubbles` flag**, because one phase cannot serve
both kinds:

- **Bubbling** events take the bubble phase and resolve with `closest()`, so an ancestor that opted
  in handles a descendant's event and an inner `stopPropagation()` still suppresses it.
- **Non-bubbling** events (`toggle`, `beforetoggle`, `close`, `cancel`, `scroll`, `scrollend`,
  `play`, `load`, `error`, `invalid`, ...) are reachable **only** in the capture phase. They resolve
  by **exact target**, not `closest()`: an event with no propagation path has no ancestor to
  delegate to, so matching one would run a command for an event that never reached it. That is also
  why `mouseenter`/`mouseleave` -- which the platform dispatches separately to each nesting level --
  fire once rather than once per level.

Capture is skipped for types in `NATIVELY_BUBBLING`. Registering it for a type the platform always
bubbles would change what the pre-existing types respond to (a shim's `new Event('change')` defaults
to `bubbles: false` and would start round-tripping on views that opted into nothing), and on
`wheel`/`touchmove` it would put a listener that can never do work in front of every scroll frame.

`e.target` is not always an Element -- a viewport `scroll`, the page-level `mouseenter`, and a
server-sent `dispatch_event` all target the Document -- so the handler guards on `nodeType` before
resolving. The attribute name and its `CSS.escape`d selector are built once per type at bind time,
not per dispatch (`CSS.escape` because the parser keeps `.` and `:` in an attribute name, so
`az-turbo:load` would otherwise be an invalid selector and throw on every dispatch).

**Passive is decided at bind time, and revisited exactly once.** Chrome makes a document-level
`wheel`, `mousewheel`, `touchstart` or `touchmove` listener passive **by default**, so opting out
is the only way `az-prevent-default` works on them -- but opting out unconditionally disables the
browser's scroll fast path for the whole page, so an app that only wants to *observe* wheel events
would pay scroll jank it never asked for. `az-prevent-default` is recorded in every attribute form
(it is the one directive the client reads out of the declared set), so on the common path its
presence is known before the first listener is registered. When the declaration arrives **after** a
forced-passive type was bound (a later delta frame, a `set_attr` effect), the client
re-registers those listeners non-passive (`declarePreventDefault`) -- passive is fixed at
registration, so without the rebind the page would keep ignoring the `preventDefault` it declared.
The flag must be **stated in both directions**: omitting it entirely makes Chromium register
`passive: true` anyway (verified against Chromium's own `DOMDebugger.getEventListeners`).

**`submit` and `drop` are deliberately not delegated generically.** They name real DOM events *and*
have dedicated listeners (submit flushes pending debounced inputs, honors `az-form-reset`, and
defers the reset to a `fetch` response; drop carries the drag bookkeeping), so delegating them as
well would run their commands twice. They sit in `DEDICATED`: recorded as proven like any event,
never bound generically, beside the `AZ_DIRECTIVES` that name no event at all. The flush covers
the form's own slots too (a form-level `az-debounce` paces the field events it delegates), and a submit is never debounced itself --
`az-debounce="blur"` on a form would otherwise never send it -- while `az-throttle` still paces
a `push_event` submit (a second submit inside the window becomes its trailing send). No flush
touches the submit's own slot -- not the submit flush, nor a blur or event-name flush on the
form: its only pending send is that trailing submission, which its tick delivers on time, so a
flush would only reopen the window early. Pacing is a `push_event` mechanism: an
`arizona_js:fetch` is issued as it is raised, on a submit or any event, and `az-debounce`/
`az-throttle` do not defer it (a deferred request would need its own answer to the navigate
epoch and the replay hold, both defined for view events, and a paced double submit still posts
twice). A debounce mode the server switches mid-window sends the slot's latest raise in the new
mode: the old timer neither fires a raise the new one replaced nor drops it.

**An `az-submit` form is never submitted natively, connected or not.** Let through, the browser
submits it with the form's own method -- a GET by default, putting every field (a password
included) in the URL -- which is what happened before the first open and during reconnect backoff.
That holds for a form with an explicit `method`/`action` too: it no longer falls back to a native
submission while disconnected. The listener always calls `preventDefault`, then decides by what
the commands need (`socketNeeds`) -- when `az-submit` is proven. An unproven one runs nothing and
is a drop like the disconnected one below (a warning and `arizona:submit-dropped` with
`detail: {reason: 'unproven'}`): its value is app data, or its proof has not arrived yet.

- **An SPA `navigate`/`patch` alone** (top level or under `transition`) degrades to a full load of
  its URL (`location.assign`, or `location.replace` for a `replace` navigate), exactly what a
  disconnected `az-navigate` click does.
- **Anything else needing the socket** -- a `push_event`, a navigation under `on_key` (it runs only
  on a matching key), or a navigation beside a `fetch` (the load would cancel the request) -- drops
  the submission **whole**, with no `az-form-reset`: the worker drops pre-open sends, so running
  the rest would apply half a submission and discard the typed fields. Every drop warns and
  dispatches a non-cancelable, bubbling `arizona:submit-dropped` `CustomEvent` on the form,
  `detail: {reason: 'disconnected'}`, so the app can tell the user.
- **A `fetch` or client-only list** runs: an HTTP login form needs no socket. It runs under the
  page's embedded proof of `az-submit`, so on a page without the embed it is dropped as
  `unproven` until the connect frame proves the name.

Queueing for the reconnect was rejected: a submit replayed seconds later, against a view the resync
may have remounted, is not what the user pressed. To disable the submit button while down, style it
off `:not(.az-connected)` rather than `.az-disconnected`: before the first open `<html>` carries
neither class. Likewise `az-prevent-default` on a delegated event is honored before the connection
check, since a declared suppression needs no server.

The listener cannot act before `arizona.js` has run (Enter while the deferred module still loads),
so the server closes that window: the `?html` target renders `method="post"` on a `form` carrying
`az-submit` and no `method` of its own (`arizona_renderer:implied_attrs/2`). Such a submit is then a
POST with the fields in the body -- a live route answers `405` -- never a GET with them in the URL.
The client's `fetch` already treats a method-less form as POST, so nothing else changes; an
author's `method` (static, dynamic, any casing) is never overridden, and `?json`/`?terminal`
imply nothing.

**Three cases the compile-time set cannot cover, and how each is handled:**

- **A runtime `set_attr`/`toggle_attr` effect** (`arizona_js:set_attr(sel, "az-toggle", ...)`)
  uses a name no template contains. The effect records the name before writing it: the command
  that wrote it was itself proven (it ran under a proven name, or a handler returned it). It is
  the only attribute write that proves a name -- `applySetAttrOp` itself records nothing, so the
  diff path (`OP_SET_ATTR`, an `OP_ITEM_PATCH` inner op) and a `?local` attribute slot, which
  carry app data, never do.
- **`?raw` markup and host-inserted DOM** declare nothing the compiler saw. Call the exported
  `mountHooks` on it, the same contract hooks already have; it walks with `getAttributeNames()`,
  which is exact because the parser already did the parsing. That call **is** the proof: every
  `az-*` name on the markup becomes a proven type for the page, so call it only on markup whose
  `az-*` attributes the host wrote itself, never on markup holding untrusted data. Pass only that
  subtree, never `document` or a server-rendered view: a scan there proves the names of every
  data value on it (`{az_select, ?get(ids)}`). Calling it on already-mounted markup is
  safe (`mountHook` returns early for an element it already tracks).
- **Window-targeted events** (`resize`, `online`/`offline`, `hashchange`, `popstate`, `storage`)
  are not delegated at all: delegation binds on Documents, and their propagation path does not
  include the Document. Use a hook.

**Delegation is asymmetric, and the rule is the event's `bubbles` flag.** `az-click` on a parent
catches every descendant's click; `az-load` or `az-toggle` on a parent never fires. Two consequences
readers assume away: an app cannot `stopPropagation` a non-bubbling `az-*` command (the framework's
capture listener runs at the document, before any listener on the target), and a `composed: false`
custom-element event cannot be delegated **when it originates inside a shadow root** -- a
shadow-less custom element is unaffected, since `composed` only matters at a boundary.

## Client-owned slots (`?local`)

`?local(Key, Init)` renders once at SSR and is then owned by the browser -- the server never diffs it and never reads it back. The client side is three functions:

- `set(viewId, key, value)` -- `findViewRoot(viewId)`, then `forEachLocal` writes every matching slot in that view. Always 3-arg; the 2-arg `arizona_js:set/2` ("closest view of the trigger") is a template-only form the client resolves from the trigger element.
- `setAll(key, value)` -- every matching slot in **every** hosting document (main + popped-out PiP documents), no view scoping.
- `get(key)` -> first match anywhere; `get(viewId, key)` -> first match in that view. Reads come back as DOM strings (no type preservation); an absent boolean attribute reads `false` and a bare one `true`. A form control's binding is the exception: it reads the live property -- `checked` on an `<input>` and `selected` on an `<option>` as a boolean, `value` on a `<select multiple>` as the list of the selected values the local owns (all of them, but those a static part beside the local names -- a part keeps its pick), `value` on a `<textarea>`/`<select>` or an `<input>` in the HTML value mode "value" (text-like types) as a string -- since after user interaction the dirty value/checkedness flags leave the attribute holding only the default. An interpolated binding of such a live `value` (text-like `<input>`, `<textarea>`, single `<select>`) reads the live value as well, its static affixes stripped, or `null` when it no longer carries them; any other interpolated attribute reads the attribute. Which element/attribute pairs sync is `syncsProp`, matched by local name (a PiP document's elements come from another realm, so `instanceof` would miss them). Anywhere else `value` only reflects the attribute, coerced, so it is neither read nor written as a property: a write would put a removed attribute back (`"0"` on `<li>`/`<progress>`/`<meter>`, `""` on `<button>`/`<data>` and on a hidden/checkbox/radio/button `<input>`) or coerce a set one.

A `<select>` carrying a `value` attribute is one whose options the server could not all mark
`selected` in the template (a component, a runtime value or raw markup renders some; see "A `value`
on a `select`" in [erlang.md](erlang.md)). The page arrives with them marked (the SSR render marks
that child's output), but a live update renders them unmarked, and HTML ignores the attribute, so
`applyChooser` (`arizona-attrs.js`) applies it: every option's `selected` attribute (its
`defaultSelected`, what a form reset returns to and what an untouched option shows) follows whether
the value names it -- the value, or under `multiple` the JSON array of values the server renders (and a `?local`
`set` writes: a list is the picks, `false`/`null` none, any other value one pick, and beside static
parts those parts stay picks around the local's own; the descriptor's `ap` for a select value holds
them as lists, `[[before...], [after...]]`, joined into affixes without `multiple`) --
and when the value itself changes (a `SET_ATTR`/`REM_ATTR` `value` on the select, a `?local` `set`)
the live selection follows too, the user's pick included. Under a `?local` value the server marks
only the page, so an option a later batch inserts there arrives unmarked and this pass is the only
one that marks it, from the value the client owns. A `multiple` the server flips changes what that
value means, and nothing re-renders it, so the `multiple` write itself carries it across
(`heldChooserLocal`/`carryChooserLocal`, from `applySetAttrOp`/`applyRemAttrOp`): the local's value
as the select held it, written back in the new form -- a single select taking the last of several
picks, as the browser keeps the last selected option. It runs on every such select when the page
starts (`connect` and the static `start`), on the select an op writes the value of, and after any
batch in which an op, at any depth (an item or list patch's inner ops, a child view's batch inside
an item), targeted a select/option/optgroup or carried markup holding one (`noteChoices`,
`arizona-ops.js`): an option the value names may arrive later. Option markup counts only when the
page has a valued select (asked once per batch); otherwise only markup bringing a select does, so a
page without one never searches for one. Picks are read as the parser reads an attribute value
(newlines normalized, NUL replaced; a live write or a JSON array skipped the parser), as the server
compares them. Options sharing a value are all marked, and the last one is selected, as the parser
selects the last `selected` option (the `.value` setter would pick the first), also when an update
brings one before it. The `arizona_js` attribute effects (`set_attr`, `remove_attr`, `toggle_attr`)
go through the same writers, so `value` on a select means the same from an effect: it picks the
option(s) it names, and removing it names none, which leaves a single select on its first option
(the HTML default) and a multiple one on none. A select without a `value` is never touched by a
batch.

A slot binds either element **content** or an **attribute value**; the descriptor rides on the reserved `az-local` attribute, which is why a template may not write `az-local` (or `az`) itself. Both writers go through `writeLocalValue(el, target, value)`, so a content slot and an attribute slot update the same way from the caller's view. An attribute write goes through the canonical `applySetAttrOp`/`applyRemAttrOp` in both directions -- `false`/`null` removes through `applyRemAttrOp`, so it unchecks, deselects or clears a control the user already changed, exactly as `true` checks it -- and an interpolated slot renders `null`/`undefined` as empty. `set`/`setAll` on a missing view or key are silent no-ops.

Every item of an `?each` shares the slot **key** (keys are compile-time literals), so a `set` hits all of them; `?local` cannot hold per-item independent client state.

## Navigation scroll behavior

- Push nav (`az-navigate` click, `arizona_js:navigate/1,2` without replace): saves outgoing scroll onto the current history entry via `replaceState`, `pushState`s the new URL, resets scroll to top (or `#hash` target) after OP_REPLACE. Opt out with `az-noscroll` on the link or `{noscroll: true}` on the effect.
- Replace nav (`arizona_js:navigate(Path, {replace: true})`): `replaceState` only. Does NOT save outgoing, does NOT reset.
- Popstate (back): restores `e.state._azScroll` after OP_REPLACE, falls through to `#hash` target or top.
- Full-document load of an existing entry (reload, Back/Forward to a page the bfcache did not keep): `'manual'` switches off the browser's own restore here too, so the `pagehide` listener stamps the current scroll onto the entry being left (`_azScroll`, one `replaceState` per unload) and `connect` restores it when `history.state._azScroll` is present -- ahead of the `#hash` jump, as the browser's own restore would be. A new entry (a fresh visit) carries no stamp and lands at the hash or the top. Two unloads the client starts itself skip the stamp, because the entry history now shows is not the page on screen: a full load the server degraded a navigate/patch frame to (`leavePage` with `replace` -- the new URL was already pushed; a full load without `replace` leaves this page's entry in place and is stamped, for Back to restore) and a cross-page popstate served by `location.reload()` while disconnected (the browser already moved to the destination, whose own stamp must survive).
- Forward-after-back: destination entry has null state -> scroll to top. Documented non-goal; future restore would need a state-ID-keyed map backed by sessionStorage, not `replaceState`-on-scroll.
- Modifier clicks (ctrl/cmd/shift/alt, non-primary button) on `az-navigate`/`az-patch` links fall through to the browser, as do a link with a `target` other than `_self` (another browsing context), one with `download`, and a click an app listener already `preventDefault`ed.
- A `navigate`/`patch` effect URL resolves against the **current page** (`navigateEffect`), as `pushState` resolves it, so the frame asks for the page the address bar shows (`?page=2` on `/users/1/` is `/users/1/?page=2`). An `az-navigate`/`az-patch` click goes through the same `navigateEffect` (with the link's base-resolved `href`), so the two agree: only a new fragment (a bare `#` included) on the current path + query is a hash jump -- it pushes (or, with `replace`, replaces) the entry and scrolls, with no server round trip. A link to the current URL with no fragment is a real navigate/patch (`navRoute` decides).
- Full nav (`{full: true}`, or any `navigate`/`patch` http(s) URL on another origin -- `pushState` refuses one -- and an `az-navigate` link to another origin, which falls through to the browser): `location.assign`, or `location.replace` when the effect also carries `replace`. The server sends `replace` on every full load it degrades a navigate/patch frame to, since the client already pushed that entry; the browser folds a same-URL load into the current entry by itself, but not one whose URL differs (a halt replayed through the gated path, a dropped fragment). Only `http:`/`https:` ever loads: `leavePage` drops any other scheme (`javascript:`, `data:`) with a warning, since loading it would run it in the page.

### Reply refs

A frame that changes nothing gets no reply, and frames carry no id, so a client cannot tell "handled" from "slow" or "lost". A frame that waits on its reply therefore carries a **reply ref**, a page-unique integer (`nextRef`) appended to it, and the server answers it with exactly one frame carrying `"r"` back (see the `arizona_socket` moduledoc); the worker forwards it as the last element of the resolved message. Only frames something waits on are tagged: every `navigate`/`patch` frame (`sendNav`), and a `push_event` raised inside an `arizona_js:transition` (the command interpreter's `_transitionScope`, captured when the command runs, so a debounced event still binds to its own reply). Every other event goes out untagged, as before.

### The navigate page-swap window (`_navRefs`)

Every view event goes out through `sendEvent` (delegated `push_event`, a hook's `pushEvent`, the exported `pushEvent`/`pushEventTo`, a relayed `push_event` effect). It drops the event while a navigate is in flight. The server replaced the root when it read the frame, so an event from the old DOM would reach the NEW root by id, and nothing on the wire lets the server tell it from a fresh one.

The window is the **set of reply refs** of the navigates in flight, so back-to-back navigates keep it open until the last one is answered. `sendNav` adds the ref when it posts a `navigate` frame on a live socket. The server answers each one with exactly one frame carrying its ref -- its `OP_REPLACE`, or a navigate effect (a middleware redirect, which re-arms through `sendNav`, or a full-page load, where the server kept the old view) -- and `settleNavReplies` removes that ref when the frame is applied (inside the worker handler's `apply`, so a deferred transition keeps the old DOM gated until the swap). A frame naming no navigate ref settles nothing: a navigate effect in an **old-page push** (a timer redirect) and the `OP_REPLACE` of a patch the server degraded to a replace look like replies but answer something else. A close (or a `pagehide` teardown) loses every reply still owed (`dropOwedReplies`): the window moves to `_navAwaitResync`, which the reconnect's resync frame clears, since the reconnect mounts the new path under the old DOM. A client-triggered navigate (`executeJS`, `applyEffects` called directly) only adds a navigate; only server frames settle one.

A dropped event is reported with a `console.warn` naming the event (never the payload), like the worker's socket-down drop. There is deliberately no timeout: every server answer to a navigate frame settles it and a crash closes the socket into the resync, so a server change that could leave a navigate frame unanswered must add one.

A `patch` does not arm: it usually keeps the view, and a cross-handler patch that the server degrades to a replace cannot be predicted client-side. A debounced/throttled `push_event` captures `_navEpoch` when raised, so one that fires after the swap is dropped too; `disconnect()` bumps the same epoch, so a timer that outlives its connection (the `WeakMap` of timers is not iterable) never sends the old view's event over the next one.

### Server-owned controls (`az-ctl`, `_owned`)

A control the parse transform marked `az-ctl` (see [erlang.md](erlang.md)) has a default the
server renders, and once the user edits it the dirty flags make that attribute invisible. The
guarantee: **once the server has answered an event that carried a control's value, and the
user has not changed the control since, the control shows what the server rendered.** That
covers a refused or normalised edit, which changes no binding and so ships no op.

**It applies to every event that carries the control, not just a submit.** An `az-change`
validator on a form carries the form's controls, so a handler that does not store a field
back reverts it: a login form whose error path re-renders without the typed email shows the
old (empty) email. The fix is to store what should stay -- `{B#{email => Email}, ...}` -- since
the field shows the server's state, which is the point.

- **Marked parts.** `az-ctl` lists the parts the server renders (`value`, `checked`,
  `selected`, `content`); `isCtl` takes a control only for the part it shows -- `checked` on a
  checkbox or radio, `value` on another input, `content` on a textarea (it has no `value`
  attribute), a select's `value` or an `option[az-ctl~="selected"]`. A checkbox whose only rendered part is its `value`
  is not held or settled: the server does not render its checkedness.
- **Carry.** `JS_PUSH_EVENT` asks `host.carry(el, event)` when the command is raised (not when
  a debounce fires): the trigger if it is a marked control (a radio brings its marked group --
  same name, same form or same root without one, looked up by name, so a big radio grid
  costs one attribute query per group), or a form's marked controls it submits (named, not
  disabled, a disabled fieldset's included) -- what `autoPayload` sends. A drop, and an event
  whose default was prevented (`az-prevent-default` on a click: the browser undoes the
  toggle, so nothing was edited), carry nothing; a submit is always prevented by the client
  and still carries. Each is snapshotted (`ctlState`: checkedness, a select's selected values,
  else `value`) and is **pending** while any raised, unsent event carries it (`_pending`, a
  count per control): `scheduleSend` releases a raise when it sends it or when a later raise
  replaces it in its slot (event type and command position, every debounce/throttle mode,
  a prevented raise included), and a dropped send releases too. A release counts once (a raise
  a reconnect left in its slot releases nothing of the new connection), and a raise whose
  trigger left the page (a blur-debounced field removed) holds nothing, so no raise that will
  never be sent can hold a control. So a debounce or throttle window
  is guarded, focused or not (a Safari click does not focus), and a form-level
  `az-debounce`, whose blur flush never runs for its fields, keeps them guarded past a blur. A
  hook's `pushEvent`, the exported `pushEvent`/`pushEventTo` and a handler effect carry
  nothing; the static host's `carry` returns `null`.
- **Ref.** `sendEvent` tags such a frame with one reply ref (the one a waiting transition takes
  too) only when it is sent on a live socket: `_owned` maps each control to `{ref, snap}`
  (latest event wins), `_ownedRefs` maps the ref to its controls. A dropped or disconnected
  send registers nothing. Strings go out well-formed (`wellFormed`: a lone surrogate, half an
  astral character, becomes U+FFFD), since the server refuses one; and a frame it still
  cannot decode is answered by the ref it ends in (`arizona_socket`), so nothing is held for
  a reply that never comes.
- **Hold -- server writes only.** `applySetAttrOp`/`applyRemAttrOp`, and `applyTextOp` for a
  marked `<textarea>`'s content (its default, as an input's `value` attribute is), ask
  `host.holds(el, name, val)` (an option asks for its select) **before** the write, since the
  default it changes is what an edit is compared with. Only a write a server frame applies
  (its ops, and the effects it carries -- `_inFrame`) is ever held: a held control takes the
  attribute only, a chooser `value` runs `applyChooser` without its live leg, and an unheld
  textarea content write syncs `value` to the new default. A command run from the user's own
  event (`az-keydown` `set_attr(#q, value, "")` to clear on Escape, or
  `[push_event(add), set_attr(...)]` to clear on Enter) writes through at once; if it changes a
  control in flight, that is the user's change since the send, so the reply leaves it. A
  `fetch` the user starts applies its response effects as write-through; one a server effect
  started applies them as server writes (the origin rides `holdReplay`). A write to a part
  the control does not show (a checkbox's `value`) is not held; a textarea's `value` write
  is its live value, so it is. A `?local` control carries
  no `az-ctl`, so it is never held. The static host holds nothing.
- **Settle.** Inside the worker handler's `apply`, after the ops and effects (so a
  transition-deferred frame settles after its DOM): for each control of the frame's ref still
  owned by that ref, drop the hold and, if connected and it still shows its snapshot,
  `showDefault` it -- text and textarea `value = defaultValue` only when it differs (a focused
  one keeps its selection, clamped); a checkbox or radio `checked = defaultChecked` (a group
  settles to its default-checked member, else none); a select with a `value` attribute through
  `applyChooser(sel, true)`, else each option to `defaultSelected` -- and fire `updated()` on
  every control whose state that changed (a radio it unchecked included). A control changed
  since is left alone; its next event settles it. That keeps a quick `az-input` sequence at
  `123` and never `13`: an earlier reply's op lands on the attribute and its settle skips the
  control a later event owns.
- **Unsent typing.** A marked control that is focused (any member of its radio group) and
  `edited` (shows something other than its default) is held too. A held write that is not the
  reply to an event carrying the control -- a push, or an effect (even in that reply: a command
  a handler returns is a server statement) -- is noted (`_guardWrote`). Which frame is the
  reply is told by the ref of the frame being applied (`_applying`, against `_ownedRefs`);
  the server ships the pushes it drains ahead of a reply in their own frame first, so a push
  never passes for the reply's echo. (That frame carries no ref, so a pending `'any'` view
  transition does not wrap it: only the reply after it animates.) A command a frame runs
  later -- a handler's `transition(set_attr(...))` runs in the view transition's update
  callback -- keeps the frame's origin (`holdReplay` carries `_inFrame`), so it is held like
  the frame's own writes. A focused but untouched control is not held: it shows
  live updates. The guard ends when an event carries the control (pending, then in flight,
  the note dropped) or when focus leaves it (`focusout` -> `host.ctlEvent`) with nothing
  pending or in flight: a noted write wins then -- the server changed what the user was
  editing -- and with none the edit is kept for the event or submit that sends it. Only focus
  moving within the document ends it: a focusout while the document has no focus (the window
  was left, alt-tab) keeps the guard, and the field is still the active element when the
  window comes back. The order makes a sent value safe: `change` fires before
  `blur`/`focusout`, an element `az-debounce` flushes on `blur`, and anything else is pending,
  so an edit that is sent is settled by its reply, never by the focus loss.
- **Bad input.** A control showing bad input (`validity.badInput`: `1e-` in a number, a
  partial date) reads `""`. Its settle is noted instead of written, and a server `value` write
  while the user is on it is deferred (`_deferred`; Firefox clears a partial date on the
  attribute write alone), then written as the attribute when the input is good again or focus
  leaves -- where a noted write wins as above.
- **IME.** Nothing writes a control's property while a composition is in progress
  (`compositionstart`/`compositionend`, `host.ctlEvent`): it is held, and a settle that would
  write is noted instead, so focus leaving the field settles it.
- **Teardown.** `dropOwedReplies` (close, `pagehide`) clears `_owned`/`_ownedRefs`;
  `disconnect()` clears the pending, guard and deferred state too.

Out of scope: restoring a control across a reconnect beyond the existing `az-change` replay
(`saveFormState`/`restoreFormState`), a `fetch` form (its response has no diff to settle
against; render from state via `push_event`). An unmarked `<textarea>` keeps the user's text
over a content update, as before.

### Patch nav (`az-patch`) and the `_azNav` tag

`navigateTo` takes a `kind` -- `'navigate'` (default) or `'patch'` -- which is both the WS frame verb (`[kind, {path, qs}]`) and a history tag. A patch push writes `_azNav: 'patch'` onto **both** the new entry (in the `pushState` state) and the outgoing entry (via `saveCurrentScroll(kind)`), so a later back/forward across that edge replays the same mode; popstate reads `e.state?._azNav === 'patch' ? 'patch' : 'navigate'`. The tag is only a hint -- the server still corrects the verb, degrading a cross-handler patch to a navigate. Tagging the *outgoing* entry matters when it was a full page load, which carries no state of its own.

Scroll timing differs because a patch emits **no `OP_REPLACE`**. `applyOps` scrolls on `didReplace` (a navigate, or a patch the server degraded to a replace). A patch kept in place scrolls on its **own reply**: the intent records the patch frame's reply ref, and the worker message handler, on the frame carrying it, scrolls when that frame has ops and drops the intent either way -- so a patch that changes nothing (answered by its ref alone) never leaves it armed, and a push landing ahead of the reply never takes it. Identity-checked, so an intent the reply's own `OP_REPLACE` already consumed is not applied twice. Scroll, the patch tag, and any pending view transition all ride **one** `replaceState` per outgoing entry (Safari rate-limits history writes to ~100/30s, then throws `SecurityError`).

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
- **A `navigate`/`patch` that sends no frame** -- a new fragment on the current page, a full load -- has no reply to wait for, so it never sets the pending intent (it would animate the next unrelated batch): a hash jump is wrapped in place like a sync effect, and an `az-navigate`/`az-patch` link's `az_transition` is dropped for a hash jump (`navigateEffect` sets `_pendingTransition` only on the frame-sending branch).
- **`push_event`** / **`patch`**: the resulting server diff arrives later, in the reply to the frame the transition wraps, which is tagged with a reply ref (`_transitionRef`); the handler wraps that frame when it has ops and drops the intent either way, so a reply with no diff can't bleed onto a later one and a concurrent server push (a timer) landing first is not the one animated. The frame is bound by **identity**: a command running under the transition (`_transitionScope` in the interpreter, captured when the command runs, so a debounced `push_event` still binds when it fires) hands it to `sendEvent`/`navigateEffect`, and only a frame issued under the pending transition binds it -- an unrelated patch or event in between neither takes it nor clears it. Until it is bound the intent dies with its frame: a `push_event` the page-swap window drops, one raised while disconnected (the worker drops it, so no reply would come), and any navigate not issued under it (its page, and its debounced event, are going away). A plain `az-patch` link runs under no transition, so it neither takes nor clears a pending one, and an `'any'` transition is not armed at all inside the page-swap window, whose event it would wait for is about to be dropped (arming it would displace the navigate's own transition). A command that defers its own send outside that call stack (anything but `scheduleSend`) loses the binding; a nested transition replaces the outer one, so only the inner one animates. History stamps (`_azTransitionPrev`/`_azTransitionNext`) carry only the navigation's **own** transition, never a pending one it was not issued under.

Wrapping a **mix** of sync and async commands (`transition([toggle(...), push_event(...)])`) animates the async result; sync siblings apply immediately, unwrapped. Wrap one kind per call.

The wrap is applied at the **worker message handler**, so a message's ops **and** effects animate together, in order. The browser runs the update callback later, so frames arriving meanwhile are held (`_heldFrames`) and applied after it, in arrival order: each frame is a delta against the one before, and an older frame landing last would silently overwrite a newer one. Each is applied on its own, so one that throws does not drop the frames after it; the first error is rethrown once they have all run. `disconnect()` drops the deferred frame and those held behind it, also when a frame being applied tears the connection down (an effect whose listener reconnects): the held frames after it are not applied.

Behaviour:
- **Guards:** no-ops (instant swap) when `document.startViewTransition` is absent or `prefers-reduced-motion: reduce` matches. `types` use the object form `startViewTransition({update, types})` only when `CSS.supports('selector(:active-view-transition-type(x))')`; otherwise the bare-callback form (older engines still cross-fade, ignore types).
- **Back/forward:** a transition belongs to the **edge** between two history entries, not to an entry -- each entry has two edges, so a per-entry flag would animate Back into an entry that was merely *reached* by a transition (`A -(transition)-> B -(plain)-> C`, Back to B). A transitioned push stamps the outgoing entry's `_azTransitionNext` and the new entry's `_azTransitionPrev`; a plain push clears the outgoing entry's `_azTransitionNext`. Every SPA entry also carries its position `_azIdx` (written in the same `replaceState`/`pushState`, resumed by `connect` after a reload), and popstate compares the destination's index with the current one: Back replays the destination's `_azTransitionNext`, Forward its `_azTransitionPrev`, so traversing a transitioned edge animates in both directions and a plain edge never does. A multi-step `history.go(n)` (|n| > 1) crosses several edges and does not animate. A replace nav keeps the replaced entry's `_azTransitionPrev`/`_azTransitionNext` (the edges into and out of that position are unchanged, so Forward into the new page animates exactly like Back out of it) and stamps no edge of its own. (Direction-aware type reversal is not done yet -- the same opts are reused both ways.)
- **Cross-document** (real `<a href>` navigations, full reloads): pure CSS -- add `@view-transition { navigation: auto; }` to the page. No framework code.
- **Styling** is user CSS. By default the whole root cross-fades; to scope or morph a single element, give it a `view-transition-name` (it then animates independently across the change). `::view-transition-*` and `:active-view-transition-type(<type>)` customize the animation. A `view-transition-name` must be unique among rendered elements during a transition, or the browser skips it.

## HTTP fetch (`arizona_js:fetch`)

`arizona_js:fetch(Url, Opts)` (op `JS_FETCH = 22`) issues a `fetch()` request with **no page reload** -- the only command that can set a real `Set-Cookie` (HttpOnly honored) without navigating. The `case JS_FETCH:` handler in `execOne`:

- **Body.** For a POST/PUT/... when the trigger is/contains a `<form>` (the `az-submit` listener passes the form as the trigger element), the body is `new URLSearchParams(new FormData(form, submitter))` -- `application/x-www-form-urlencoded`, mirroring a normal form POST (the submit button's `name`/`value` rides along, so a multi-submit-button form reports which one fired; the submitter is also carried into a GET form's query string; multipart / file uploads are a non-goal). Otherwise `Opts.body` is JSON-encoded. Explicit `Opts.body` wins over the form.
- **GET/HEAD.** No request body; the payload is appended to the URL query string instead (`fetch` is otherwise POST-oriented -- it exists to set cookies), spliced in **before** any `#fragment` (`appendQuery`) -- after it the query would be part of the fragment, which `fetch` never sends. The same precedence as a body: an explicit `Opts.body` wins over the form. A form contributes its fields; a `body` map contributes one parameter per key (`bodyQuery`: a list repeats its key, `null` is left out, a nested map is JSON-encoded); a `body` that is not a map has no keys to name parameters by, so it is not sent and a console warning says so.
- **Request.** `method` = `Opts.method` -> the form's `method` -> `POST`. `credentials` maps the atom (`same_origin` -> `'same-origin'`, default) to the fetch mode. Headers default to `accept: application/json` plus `Opts.headers`. `keep_alive` = `Opts.keep_alive === true` (default `false`) -- maps straight to `fetch()`'s `keepalive`, so a request fired just before a navigation completes instead of being cancelled (browser inflight-body cap ~64KB).
- **Effects body.** Whenever the response body parses as `{ e: effects }`, `applyEffects(effects)` runs -- **regardless of status**, so a controller can return a real `4xx` (e.g. `422`). An empty `2xx` body (a `204` cookie-only response) applies nothing. `Set-Cookie` is applied natively by the browser. (No response-effect view-transition wrapping -- a follow-up; server-driven re-renders animate through the normal WS path.)
- **Form reset.** `az-form-reset` is honored via the shared `maybeResetForm` helper. The submit listener resets synchronously for non-fetch commands; for a fetch command it defers (the listener skips the sync reset via `commandsIncludeFetch`) and the fetch handler resets **only on a 2xx success** -- so a validation error (a non-2xx) keeps the typed fields.
- **Redirect.** There is **no HTTP-3xx handling** -- `fetch`'s `redirect: 'manual'` yields an opaque-redirect whose `Location` is unreadable. A redirect is delivered as an `arizona_js:navigate` effect in a 2xx body (`arizona_controller:reply_redirect/1`).
- **Failure.** `Opts.on_error` runs (via `executeJS`) and an `arizona:fetch-error` `CustomEvent` is dispatched on `document` only when there is **no usable effects body**: a non-JSON error page, an empty non-2xx, or a network failure.

The server endpoint is a controller route (e.g. `{post, ...}`) building the response with `arizona_controller:reply_effects/1`. **Showing content is server-authoritative, not a response effect** -- there is deliberately no `set_text`/`set_html` (it would fight the diff engine) and the response is effects-only (a stateless controller has no diff snapshot for ops, hence `accept: application/json`). Two ways to update the rendered page, both server-authoritative (the view renders from state, the WS diff patches):

- **`push_event` in the response (default, the submitting view).** Return `arizona_js:push_event(~"...")`; the fetch handler runs response effects against a **detached stand-in** for the trigger, carrying `az-target` set to `resolveTarget(trigger)` -- the trigger's own `az-target`, else its enclosing view (root **or** a child view) -- so the event goes where the trigger's own events go (a `set/2` in the response updates that view's `?local` too) and the client relays it over its WS -> `handle_event/3` re-renders -- **without** scraping the form's fields into the payload (the stand-in is no form or field, so sensitive inputs aren't echoed), and a `fetch` in the response finds no enclosing `<form>` to post (a view nested in a form would otherwise hand it the outer form's fields). Pass the controller's result as an explicit payload. Success leg: `reply_effects/1` (200, the form auto-resets). Error leg: `reply_effects(Status, Effects)` with a non-2xx (e.g. `422`) so the typed fields survive while the effects still apply. No subscription, works for anonymous forms. Fixture: `arizona_fetch_push`.
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

## Crash and mount-failure recovery

The client classifies a close by its code alone. **4500** (the view crashed after it mounted): no reload; the worker's backoff reconnect remounts the view in place. **4501** (the view failed to mount): reload after a random 5-10 s delay, 30 s from the 10th consecutive reload, never giving up. The count lives in `sessionStorage` under one key, `arizona:mount-reloads`, holding `{path, count}`: per tab, surviving that tab's reload (all the count needs) and cleared when the tab closes, so a failing path -- which may carry a secret such as a reset token or invite code -- never outlives the tab. A reload increments the count when the stored path is the one being reloaded and starts a new count of 1 otherwise; an unavailable or corrupt value reads as 0. Tabs do not share it: each tab's jittered delay already spreads them out. Only a successful mount removes it: the first frame of a first connect (the server sends nothing before it mounts), the resync frame (`firstAfterReconnect`) of a reconnect, or a navigate's `OP_REPLACE` reply. The main thread is the one source of that signal: it also posts `[5]` to the worker, whose backoff resets on a close only after a mount, and on a 4500 only when that mount is at least 10 s old. A successful mount also cancels a pending reload; `pagehide` and `disconnect()` only cancel it. Fields saved on a 4501 are lost by the reload unless a reconnect mounts first.

Typed form fields are saved on any non-1000 close (and re-saved just before the resync replaces the view) and restored after the remount, replaying `az-change` once. The resync frame also keeps the user's place: `captureFocus` records each hosting document's focused element by what the fresh markup carries again (its `id`, else its form's `id` plus its `name` and position among that form's same-named fields) with its selection range, and `restoreFocus` -- after the values are restored, since setting a value moves the caret to the end -- refocuses the replacement (`preventScroll`) and restores the range. A field inside a kept az-nodiff element is the same node after the resync, but moving it through the parsed fragment detached it and dropped focus, so it is refocused by reference (no id or form needed). It does nothing when the element still holds focus, when the field is gone, or when something else already holds focus (a resync effect's `focus` wins). The refocus is a real `focus()`, so it fires `focus`/`focusin`: an `az-focus`/`az-focusin` command on that field (a `push_event`) runs again on reconnect. Once every replayed event is posted -- a command that defers its work (a view transition's update, a fetch's response effects) holds the replay open until it ran -- the main thread posts `[4]` to the worker, which sends a ping; the server handles a socket's frames in order and an event synchronously, so that ping's pong (reported back as `[3]`, matched to its ping because pongs come back in ping order) proves the replay was handled. A 4500 before it restores the replayed forms next time **without** the replay -- the values are kept, never dropped; after it, nothing is suspect.

## Connection events (`arizona:connected` / `arizona:disconnected`)

Two `CustomEvent`s on `document`, like `arizona:fetch-error` -- no hook callbacks. Both fire from the main thread's worker message handler, at points it already tracks. Added state: `_outage` (a `disconnected` went out and no `connected` since), `_hadMount` (a `connected` went out on this page load; never reset), and `_closeCount` (closes seen, to void a deferred `connected`).

- **`arizona:disconnected` `{code, reloadInMs}`** -- from the `[2, closeCode]` handler, for any code but `1000`: **once per outage, and again when a page reload gets scheduled during it**. The dedup matters because the worker posts a `[2, code]` for **every** failed reconnect attempt (a handshake that never opens reports `1006`, the browser's code for a close with no close frame; so does a first connect that never opened, which is an outage too). `reloadInMs` is what `scheduleMountReload()` returned -- the delay of the reload this `4501` scheduled, `null` when one was already pending -- and a non-null one fires even mid-outage: that is the app's cue to save state before the reload. Listeners must be idempotent. `pagehide` and `disconnect()` terminate the worker synchronously, so no `[2, ...]` arrives and nothing fires; `disconnect()` also clears `_outage` so a later `connect()` starts clean.
- **`arizona:connected` `{reconnect}`** -- when a frame proves a mount **while `_mountProof` is still set**, i.e. the first proof on a socket. Dispatched from the end of the frame's `apply`, so it follows the ops even when a view transition defers them; a close in between (`_closeCount` moved) voids it. `reconnect` is `_hadMount` -- whether this page load already announced a mount -- not the socket's reconnect flag: a first connect that never opened reconnects with `_az_reconnect=1`, yet its mount is the page's first. A navigate's `OP_REPLACE` on an already-mounted socket proves a mount too (`mountSucceeded()` still runs) but is not a connection event; if a navigate reply beats a reconnect's resync, it is that socket's first proof and fires `connected` itself. A bfcache restore (`spawnWorker(true)`) fires `connected{reconnect: true}` with no `disconnected` before it.

Tests: `connection events` in `arizona.test.js`; the per-attempt close premise in `arizona-worker-integration.test.js`; e2e in `e2e/parallel/arizona_crash.spec.js`. Full contract and an example in [docs/architecture.md](../../docs/architecture.md) "Connection events".

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
- `mounted()` -- on a **first** connect (`mountHooks(document)` runs on the open message only when it is not flagged a reconnect), after `OP_INSERT`/`OP_REPLACE`/`OP_TEXT` (marker path **and** markerless-HTML path)/`OP_LIST_PATCH` inserts. Guarded -- never double-fires.
- `updated()` -- after `OP_SET_ATTR`/`OP_REM_ATTR`/`OP_TEXT` (all three sub-paths; **not** when the slot is unterminated and the op is refused), on the **container** for `OP_INSERT`/`OP_REMOVE`/`OP_MOVE` and for an `OP_LIST_PATCH` that changed the child list, on the **moved item** as well for `OP_MOVE` (its position is its own state), and on the element holding a `?local` slot after `arizona.set`/`setAll`. A pure `OP_ITEM_PATCH` notifies whatever its inner ops touch, not the container -- it mutated a descendant. Only for persisting elements. Every DOM-mutating `arizona_js` effect also fires `updated()` on every element it touches, so a client-driven change is observable to a hook exactly like the server-driven diff: the attribute effects (`set_attr`/`remove_attr`/`toggle_attr`) go through the canonical writers `applySetAttrOp`/`applyRemAttrOp` (which also sync the form-control `value` property like `OP_SET_ATTR`/`OP_REM_ATTR`), and the class/visibility effects (`add_class`/`remove_class`/`toggle_class`/`toggle`/`show`/`hide`) call `notifyUpdated` after mutating `classList`/`hidden`.

**Selector targeting:** the broadcast effects (`toggle`/`show`/`hide`/`add_class`/`remove_class`/`toggle_class`/`set_attr`/`remove_attr`/`toggle_attr`/`reset_form`) act on **all** elements matching the selector, via `withQueryAll` (`querySelectorAll` across the main + PiP documents). `reset_form` calls each match's native `reset()` (a non-form match is a safe no-op) and fires `updated()` like the class/visibility effects. The single-target effects (`focus`/`blur`/`scroll_to`/`select`/`copy_to_clipboard`/`show_modal`/`close_modal`) act on the **first** match only, via `withQuery`. `select` calls the match's `select()` and `copy_to_clipboard` writes its `value`/`textContent` via `navigator.clipboard.writeText` (neither is a DOM mutation, so no `updated()`); `show_modal`/`close_modal` call the match's `showModal()`/`close()` and fire `updated()` like the class/visibility effects (a non-dialog match is a safe no-op). `dispatch_event` takes no selector but follows the same reach: it dispatches a fresh `CustomEvent` on **every** hosting document, so a listener registered inside a popped-out view receives it too.
- `destroyed()` -- before `OP_REMOVE_NODE`/`OP_REMOVE`/`OP_REPLACE`/`OP_TEXT`/`OP_LIST_PATCH` removals, and before a `?local` write (`arizona.set`/`setAll`) replaces a slot's content. Called BEFORE DOM mutation, while the element is still attached.
- **Kept az-nodiff elements get neither.** An HTML `OP_TEXT` and the resync `OP_REPLACE` (the first frame after a reconnect, `applyOps(ops, true)`; a navigate's keeps nothing) carry over every outermost az-nodiff element of the old content that the new markup names again (`az-nodiff="TOKEN"`, the server's identity token; a bare `az-nodiff` is never kept, nor is anything inside one) at the same place -- the enclosing slot marker plus the az-nodiff element's own and each ancestor's `az-key` and view id. Only a one-to-one match is kept: two az-nodiff elements under one identity (unkeyed list items) are ambiguous, so neither is. `keepNodiff` moves the old element into the parsed fragment in place of the new copy BEFORE the teardown, so `destroyHooks` no longer reaches it, and `mountHooksOutside` skips it (and the client-owned markup inside it) after insertion. On a resync an az-nodiff element holding a live view (`[az-view]`) is not kept: the reconnect remounted that view fresh. The `mayNameNodiff` substring check (`` ` az-nodiff="` ``) keeps every other `OP_TEXT` on its original path. `nodiff` is in `AZ_DIRECTIVES`, so a scan of host markup never takes the attribute for an event.

**Key distinction:** `OP_TEXT`'s **markerless** path uses `destroyChildHooks` (descendants only) -- target stays, gets `updated()`. `OP_TEXT`'s **marker** path is narrower still: `forEachElementBetweenMarkers(marker, destroyHooks)` tears down only the elements inside the slot span, root-inclusive for each, so a hooked sibling *outside* the markers survives a slot re-render. `OP_REPLACE`/`OP_REMOVE_NODE` use `destroyHooks` (root + descendants).

**A hook on the view root sees slot changes non-uniformly**, and this is correct under the contract rather than a gap: it is notified for a marker-anchored slot that is its own direct child (its child nodes really did change), and stays silent for a sibling slot anchored on a descendant element (that element's children changed, not the root's).
