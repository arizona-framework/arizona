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
- `applyOps(ops, resync?)` -- applies opcodes (`resync` marks a reconnect's resync batch, whose `OP_REPLACE` keeps az-nodiff elements -- see "Kept az-nodiff elements" under element hooks). `OP_TEXT` uses comment markers (`<!--az:X-->...<!--/az-->`), falling back to `textContent`/`innerHTML` on the resolved element when the slot has none. `OP_REPLACE` swaps the target for the parsed fragment and mounts hooks on the nodes it inserted (the target's id names the *outgoing* view, so it cannot be re-resolved after the swap). Triggers hook lifecycle callbacks. **Op code 3 is unassigned** -- it was the innerHTML op, removed once every container full render became `OP_TEXT`; it is free to reuse, but only by changing `arizona.hrl` and all four clients together.
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

- **Compile time.** `compile_attr` records an `az-*` name when a builder call
  (`arizona_js`/`arizona_android`/`arizona_os`) is visible anywhere in its value expression --
  builders are the only producers of effect tuples, so the call is proof. The names land in an
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

The worker relays each frame's `"a"` as field 4 of
`postMessage([0, ops, effects, firstAfterReconnect, azNames])`; the main thread decides which names
are events and unions monotonically.

Deriving the set from rendered bytes instead was tried and abandoned. A regex over serialized HTML
cannot tell an attribute NAME from `az-` text inside an attribute VALUE, so user content echoed
into a `title` could register document listeners and disable the page's scroll fast path; and it
truncated on a `>` inside an attribute value, silently dropping real events. Both failure modes are
structural, not bugs to patch.

**An unproven value is data, and is deliberately not delegated.** A static binary is data by
construction (an effect is always a call, never a literal), a bare or boolean attribute has no
value to hold a command, and an opaque dynamic that never evaluates to a command is data too --
`{az_select, ?get(ids)}` rendering `az-select="[1,2,3]"` never enters the set, `select` is never
delegated, and the value never reaches the command interpreter. That matters because a command list
is a bare JSON array (`arizona_effect:encode/1` emits a single command unwrapped, `[0,"inc"]`), so
`[1,2,3]` is a structurally valid command -- `toggle` with selector `2` -- and **no client-side
inspection of the value can separate the two**. Delegating on guesswork would both leave declared
events dead and hand app data (possibly attacker-influenced) to the command interpreter; the typed
proofs above are what make neither happen. The delegated dispatch still wraps parse AND execution
in a warn-once containment, because a proven name can share the DOM with same-named app data (a
runtime `set_attr`, host markup).

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
forced-passive type was bound (a later delta frame, a runtime attribute write), the client
re-registers those listeners non-passive (`declarePreventDefault`) -- passive is fixed at
registration, so without the rebind the page would keep ignoring the `preventDefault` it declared.
The flag must be **stated in both directions**: omitting it entirely makes Chromium register
`passive: true` anyway (verified against Chromium's own `DOMDebugger.getEventListeners`).

**`submit` and `drop` are deliberately not delegated generically.** They name real DOM events *and*
have dedicated listeners (submit flushes pending debounced inputs, honors `az-form-reset`, and
defers the reset to a `fetch` response; drop carries the drag bookkeeping), so delegating them as
well would run their commands twice. They sit in `AZ_DIRECTIVES` beside the `az-*` names that are
directives rather than events.

**An `az-submit` form is never submitted natively, connected or not.** Let through, the browser
submits it with the form's own method -- a GET by default, putting every field (a password
included) in the URL -- which is what happened before the first open and during reconnect backoff.
That holds for a form with an explicit `method`/`action` too: it no longer falls back to a native
submission while disconnected. The listener always calls `preventDefault`, then decides by what
the commands need (`socketNeeds`):

- **An SPA `navigate`/`patch` alone** (top level or under `transition`) degrades to a full load of
  its URL (`location.assign`, or `location.replace` for a `replace` navigate), exactly what a
  disconnected `az-navigate` click does.
- **Anything else needing the socket** -- a `push_event`, a navigation under `on_key` (it runs only
  on a matching key), or a navigation beside a `fetch` (the load would cancel the request) -- drops
  the submission **whole**, with no `az-form-reset`: the worker drops pre-open sends, so running
  the rest would apply half a submission and discard the typed fields. Every drop warns and
  dispatches a non-cancelable, bubbling `arizona:submit-dropped` `CustomEvent` on the form,
  `detail: {reason: 'disconnected'}`, so the app can tell the user.
- **A `fetch` or client-only list** runs: an HTTP login form needs no socket.

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
author's `method` (static, dynamic, any casing) is never overridden, and `?native`/`?terminal`
imply nothing.

**Three cases the compile-time set cannot cover, and how each is handled:**

- **A runtime attribute write** (`arizona_js:set_attr(sel, "az-toggle", ...)`) uses a name no
  template contains. Every such write funnels through `applySetAttrOp`, which records the name it
  just wrote.
- **`?raw` markup and host-inserted DOM** declare nothing the compiler saw. Call the exported
  `mountHooks` on it, the same contract hooks already have; it walks with `getAttributeNames()`,
  which is exact because the parser already did the parsing. Calling it on already-mounted markup is
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
- `get(key)` -> first match anywhere; `get(viewId, key)` -> first match in that view. Reads come back as DOM strings (no type preservation); an absent boolean attribute reads `false` and a bare one `true`. A form control's binding is the exception: it reads the live property -- `checked` on an `<input>` and `selected` on an `<option>` as a boolean, `value` on a `<textarea>`/`<select>` or an `<input>` in the HTML value mode "value" (text-like types) as a string -- since after user interaction the dirty value/checkedness flags leave the attribute holding only the default. Which element/attribute pairs sync is `syncsProp`, matched by local name (a PiP document's elements come from another realm, so `instanceof` would miss them). Anywhere else `value` only reflects the attribute, coerced, so it is neither read nor written as a property: a write would put a removed attribute back (`"0"` on `<li>`/`<progress>`/`<meter>`, `""` on `<button>`/`<data>` and on a hidden/checkbox/radio/button `<input>`) or coerce a set one.

A slot binds either element **content** or an **attribute value**; the descriptor rides on the reserved `az-local` attribute, which is why a template may not write `az-local` (or `az`) itself. Both writers go through `writeLocalValue(el, target, value)`, so a content slot and an attribute slot update the same way from the caller's view. An attribute write goes through the canonical `applySetAttrOp`/`applyRemAttrOp` in both directions -- `false`/`null` removes through `applyRemAttrOp`, so it unchecks, deselects or clears a control the user already changed, exactly as `true` checks it -- and an interpolated slot renders `null`/`undefined` as empty. `set`/`setAll` on a missing view or key are silent no-ops.

Every item of an `?each` shares the slot **key** (keys are compile-time literals), so a `set` hits all of them; `?local` cannot hold per-item independent client state.

## Navigation scroll behavior

- Push nav (`az-navigate` click, `arizona_js:navigate/1,2` without replace): saves outgoing scroll onto the current history entry via `replaceState`, `pushState`s the new URL, resets scroll to top (or `#hash` target) after OP_REPLACE. Opt out with `az-noscroll` on the link or `{noscroll: true}` on the effect.
- Replace nav (`arizona_js:navigate(Path, {replace: true})`): `replaceState` only. Does NOT save outgoing, does NOT reset.
- Popstate (back): restores `e.state._azScroll` after OP_REPLACE, falls through to `#hash` target or top.
- Full-document load of an existing entry (reload, Back/Forward to a page the bfcache did not keep): `'manual'` switches off the browser's own restore here too, so the `pagehide` listener stamps the current scroll onto the entry being left (`_azScroll`, one `replaceState` per unload) and `connect` restores it when `history.state._azScroll` is present -- ahead of the `#hash` jump, as the browser's own restore would be. A new entry (a fresh visit) carries no stamp and lands at the hash or the top.
- Forward-after-back: destination entry has null state -> scroll to top. Documented non-goal; future restore would need a state-ID-keyed map backed by sessionStorage, not `replaceState`-on-scroll.
- Modifier clicks (ctrl/cmd/shift/alt, non-primary button) on `az-navigate`/`az-patch` links fall through to the browser, as do a link with a `target` other than `_self` (another browsing context), one with `download`, and a click an app listener already `preventDefault`ed.
- A `navigate`/`patch` effect URL resolves against the **current page** (`navigateEffect`), as `pushState` resolves it, so the frame asks for the page the address bar shows (`?page=2` on `/users/1/` is `/users/1/?page=2`). A new fragment on the current path + query just pushes (or, with `replace`, replaces) the entry and scrolls, with no server round trip -- the effect form of the in-page `az-navigate` click.
- Full nav (`{full: true}`, or any `navigate`/`patch` http(s) URL on another origin -- `pushState` refuses one -- and an `az-navigate` link to another origin, which falls through to the browser): `location.assign`, or `location.replace` when the effect also carries `replace`. The server sends `replace` on every full load it degrades a navigate/patch frame to, since the client already pushed that entry; the browser folds a same-URL load into the current entry by itself, but not one whose URL differs (a halt replayed through the gated path, a dropped fragment). Only `http:`/`https:` ever loads: `leavePage` drops any other scheme (`javascript:`, `data:`) with a warning, since loading it would run it in the page.

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
- **GET/HEAD.** No request body; the payload is appended to the URL query string instead (`fetch` is otherwise POST-oriented -- it exists to set cookies), spliced in **before** any `#fragment` (`appendQuery`) -- after it the query would be part of the fragment, which `fetch` never sends. The same precedence as a body: an explicit `Opts.body` wins over the form. A form contributes its fields; a `body` map contributes one parameter per key (`bodyQuery`: a list repeats its key, `null` is left out, a nested map is JSON-encoded); a `body` that is not a map has no keys to name parameters by, so it is not sent and a console warning says so.
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

## Crash and mount-failure recovery

The client classifies a close by its code alone. **4500** (the view crashed after it mounted): no reload; the worker's backoff reconnect remounts the view in place. **4501** (the view failed to mount): reload after a random 5-10 s delay, 30 s from the 10th consecutive reload, never giving up. The count lives in `sessionStorage` under one key, `arizona:mount-reloads`, holding `{path, count}`: per tab, surviving that tab's reload (all the count needs) and cleared when the tab closes, so a failing path -- which may carry a secret such as a reset token or invite code -- never outlives the tab. A reload increments the count when the stored path is the one being reloaded and starts a new count of 1 otherwise; an unavailable or corrupt value reads as 0. Tabs do not share it: each tab's jittered delay already spreads them out. Only a successful mount removes it: the first frame of a first connect (the server sends nothing before it mounts), the resync frame (`firstAfterReconnect`) of a reconnect, or a navigate's `OP_REPLACE` reply. The main thread is the one source of that signal: it also posts `[5]` to the worker, whose backoff resets on a close only after a mount, and on a 4500 only when that mount is at least 10 s old. A successful mount also cancels a pending reload; `pagehide` and `disconnect()` only cancel it. Fields saved on a 4501 are lost by the reload unless a reconnect mounts first.

Typed form fields are saved on any non-1000 close (and re-saved just before the resync replaces the view) and restored after the remount, replaying `az-change` once. Once every replayed event is posted -- a command that defers its work (a view transition's update, a fetch's response effects) holds the replay open until it ran -- the main thread posts `[4]` to the worker, which sends a ping; the server handles a socket's frames in order and an event synchronously, so that ping's pong (reported back as `[3]`, matched to its ping because pongs come back in ping order) proves the replay was handled. A 4500 before it restores the replayed forms next time **without** the replay -- the values are kept, never dropped; after it, nothing is suspect.

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
