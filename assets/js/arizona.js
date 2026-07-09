/**
 * @module arizona
 *
 * Arizona -- Client Runtime
 *
 * Thin DOM patcher for server-rendered views. A Web Worker owns the WebSocket
 * connection and resolves template payloads; this module receives pre-computed
 * DOM-ready ops, applies them, collects user events, and sends them to the
 * Worker for transmission.
 *
 * Wire protocol (Worker -> Main):
 *   [0, ops|null, effects|null, firstAfterReconnect] -- resolved message
 *   [1, isReconnect]                                  -- WS opened
 *   [2, closeCode]                                    -- WS closed
 *
 * Wire protocol (Main -> Worker):
 *   [0, wsUrl]      -- connect
 *   [1, jsonString] -- send data
 *   [2, code]       -- close WS
 */

/**
 * Op codes -- each op is a flat array: [opcode, "viewId:az", ...args].
 * Codes match the server-side OP_* constants in arizona.erl.
 * @enum {number}
 * @property {number} TEXT - [0, target, value]
 * @property {number} SET_ATTR - [1, target, attr, value]
 * @property {number} REM_ATTR - [2, target, attr]
 * @property {number} UPDATE - [3, target, html] -- innerHTML
 * @property {number} REMOVE_NODE - [4, target]
 * @property {number} INSERT - [5, target, key, pos, html]
 * @property {number} REMOVE - [6, target, key]
 * @property {number} ITEM_PATCH - [7, target, key, innerOps]
 * @property {number} REPLACE - [8, target, html] -- outerHTML (navigate)
 * @property {number} MOVE - [9, target, key, afterKey] -- move keyed child
 */
const OP = {
    TEXT: 0,
    SET_ATTR: 1,
    REM_ATTR: 2,
    UPDATE: 3,
    REMOVE_NODE: 4,
    INSERT: 5,
    REMOVE: 6,
    ITEM_PATCH: 7,
    REPLACE: 8,
    MOVE: 9,
    LIST_PATCH: 10,
};

/** Worker protocol opcodes -- must match arizona-worker.js. */
const W_CONNECT = 0;
const W_SEND = 1;
const W_CLOSE = 2;
const W_UPDATE_PATH = 3;

/** WebSocket close codes used by Arizona (must match `arizona_socket.erl`). */
const WS_CLOSE_NORMAL = 1000;
const WS_CLOSE_CRASH = 4500;

// ---------------------------------------------------------------------------
// Hook system -- element lifecycle hooks via az-hook attribute
//
// Hooks let client-side JS interact with server-managed elements. Register
// hook definitions before calling connect(). Any element with az-hook="Name"
// is tracked and receives lifecycle callbacks as the server patches the DOM.
//
// Each callback receives `this` bound to the hook instance:
//   this.el           -- the DOM element
//   this.pushEvent(name, payload) -- send an event to the server's
//       handle_event/3, routed to the element's enclosing az-view
//
// Lifecycle:
//   mounted()   -- element entered the DOM (SSR hydration, OP_INSERT,
//                 OP_UPDATE, OP_REPLACE, or OP_TEXT with new content)
//   updated()   -- element stayed in the DOM but its attributes or inner
//                 content changed (OP_SET_ATTR, OP_REM_ATTR, OP_UPDATE,
//                 OP_TEXT)
//   destroyed() -- element is about to be removed (OP_REMOVE_NODE,
//                 OP_REMOVE, OP_REPLACE, OP_UPDATE, OP_TEXT). Called
//                 before the DOM mutation -- this.el is still attached
//
// A hook's own methods are reachable as `this.method()` from any lifecycle
// callback (the instance's prototype is the hook def), and state assigned to
// `this` is per-instance -- so shared logic factors into a helper. Example -- a
// chart hook that draws on mount and redraws on update via its own draw(),
// cleans up on destroy, and notifies the server it's ready:
//
//   import { hooks, connect } from './arizona.js';
//   hooks.Chart = {
//       mounted() {
//           this.chart = new ChartLib(this.el);   // per-instance state
//           this.draw();
//           this.pushEvent('chart_ready', { width: this.el.offsetWidth });
//       },
//       updated()   { this.draw(); },
//       destroyed() { this.chart.destroy(); },
//       draw()      { this.chart.render(this.el.dataset); }, // own helper method
//   };
//   connect('/ws');
//
// On the server, handle_event/3 receives the pushed event:
//
//   handle_event(<<"chart_ready">>, #{<<"width">> := W}, Bindings) ->
//       {Bindings#{chart_width => W}, []}.
// ---------------------------------------------------------------------------

/** @type {Object<string, {mounted?: function, updated?: function, destroyed?: function} & Object<string, *>>} */
const hooks = {};

// The instance's prototype is the hook def (see mountHook), so beyond the base
// fields it also exposes the def's helper methods plus arbitrary per-instance state.
/** @type {Map<Element, {el: Element, __name: string, pushEvent: function} & Object<string, *>>} */
const _hooks = new Map();

/** @type {Worker|null} */
let _worker = null;

/** @type {boolean} */
let _connected = false;

// Path + query the server last rendered. Tracked so the popstate handler can
// tell a real cross-page back/forward (which needs a server navigate) from a
// same-page fragment change (hash only -- scroll, no round-trip). Mirrors the
// same-page fast path in the az-navigate click handler.
/** @type {string} */
let _currentPath = '';
/** @type {string} */
let _currentQs = '';

/** @type {Map<string, {fields: Object<string, string|string[]>, azChange: string|null}>} */
const _savedForms = new Map();

// --------------------------------------------------------------------------
// Multi-document support (Document Picture-in-Picture)
// --------------------------------------------------------------------------
// A view's root element normally lives in the main `document`, but `requestPip`
// can move it into a floating PiP window with its own `document`. Patches are
// applied by resolving the view root in its OWNING document, so server diffs
// keep flowing after the move. Registers the popped view's id against its PiP
// Document so `allDocs` knows which windows to search; resolution itself goes
// by DOM containment (see `findViewRoot`), not this map's keys.
/** @type {Map<string, Document>} */
const _viewDocs = new Map();

/**
 * Every document currently hosting Arizona views: the main document plus any
 * popped-out PiP documents. Used by document-wide local-slot ops.
 * @returns {Set<Document>}
 */
function allDocs() {
    const docs = new Set([document]);
    for (const d of _viewDocs.values()) docs.add(d);
    return docs;
}

/**
 * Find a view's root element by id, searching the main document first and then
 * any popped-out PiP windows, returning whichever one actually owns it.
 * Resolving by DOM containment -- rather than a flat viewId -> Document map --
 * is what routes a NESTED stateful child correctly: `requestPip` registers only
 * the popped root's id, but a stateful child inside the moved subtree emits ops
 * under its own view id and gets physically moved into the PiP document with the
 * subtree. Containment finds it there with no per-child bookkeeping (and the
 * same for children inserted after pop-out). The PiP scan is skipped entirely
 * when no window is open, so the common (main-document) path stays a single
 * allocation-free `getElementById` -- this runs per diff op. Returns null when
 * no hosting document holds the element.
 * @param {string} viewId
 * @returns {Element|null}
 */
function findViewRoot(viewId) {
    const el = document.getElementById(viewId);
    if (el || _viewDocs.size === 0) return el;
    for (const doc of _viewDocs.values()) {
        const found = doc.getElementById(viewId);
        if (found) return found;
    }
    return null;
}

// --------------------------------------------------------------------------
// Scroll on SPA navigation
// --------------------------------------------------------------------------
// Semantics:
//   push (az-navigate click, arizona_js:navigate/1,2 without replace)
//     -> save outgoing scroll to history entry; scroll to top (or #hash)
//        after OP_REPLACE. Opt out with az-noscroll / {noscroll: true}.
//   replace (arizona_js:navigate with #{replace => true})
//     -> in-place URL swap; do NOT save outgoing scroll; do NOT reset.
//   popstate (back)
//     -> restore the saved scroll stored on the destination entry.
//   popstate (forward, after back)
//     -> destination entry has no saved scroll (we only save on push),
//        so falls through to #hash-or-top. Restoring forward-nav scroll
//        is a deliberate non-goal for this release; adding it later
//        should use a state-ID-keyed Map + sessionStorage, not
//        replaceState-on-scroll.

/**
 * Pending scroll intent set when az-navigate/az-patch/popstate is handled. A
 * navigate applies it after its OP_REPLACE; a patch (`patch: true`) applies it
 * after the first non-empty diff batch (it has no OP_REPLACE).
 * @type {{kind: 'push'|'pop', hash: string, saved?: {x:number,y:number}|null, patch?: boolean}|null}
 */
let _pendingScroll = null;

/**
 * Apply a scroll intent. pop+saved restores prior position; otherwise scroll
 * to #hash target if present, else to top.
 * @param {{kind: 'push'|'pop', hash: string, saved?: {x:number,y:number}|null}} p
 */
function applyScroll(p) {
    if (p.kind === 'pop' && p.saved) {
        window.scrollTo(p.saved.x, p.saved.y);
        return;
    }
    if (p.hash) {
        const el = document.getElementById(p.hash);
        if (el) {
            el.scrollIntoView();
            return;
        }
    }
    window.scrollTo(0, 0);
}

/**
 * Save the current scroll position onto the current (outgoing) history entry so
 * back/popstate can restore it. When leaving via a patch, also tag the outgoing
 * entry `_azNav: 'patch'` so a later back-navigation to it replays as a patch
 * (keeping the view) rather than the default navigate -- important when the
 * entry was a full page load, which carries no tag of its own. The server still
 * corrects the verb (a cross-handler patch falls back to navigate), so the tag
 * is only a hint.
 * @param {string} [navKind]
 */
function saveCurrentScroll(navKind) {
    const st = history.state || {};
    const next = { ...st, _azScroll: { x: window.scrollX, y: window.scrollY } };
    if (navKind === 'patch') next._azNav = 'patch';
    history.replaceState(next, '', location.href);
}

// --------------------------------------------------------------------------
// View transitions
// --------------------------------------------------------------------------
// A view transition wraps a DOM change in document.startViewTransition so the
// browser cross-fades (or, with user CSS, morphs) old -> new. Transitions are
// not tied to navigation -- they wrap any DOM change the framework drives:
//   - a synchronous client effect (toggle/add_class/...) -- wrapped immediately
//   - an az-navigate round-trip -- the OP_REPLACE arrives a message later
//   - a push_event -- the resulting server diff arrives a message later
// Real <a href> navigations animate via the user's `@view-transition` CSS, no
// code here.
//
// For the async cases (navigate/push_event) `_pendingTransition` holds the
// intent until the server response lands; the worker message handler then wraps
// that message's ops+effects together. `kind` says which batch to wait for:
// 'replace' (a navigation's page swap -- a stray text/attr tick is ignored) or
// 'any' (the next non-empty diff from a push_event). Set by the az-transition
// attribute, an arizona_js:transition command, or replayed from history state
// on back/forward. Synchronous effects never set it -- they wrap in place.

/** @type {{types?: string[], kind: 'replace'|'any'}|null} */
let _pendingTransition = null;

/**
 * Whether a view transition should run now: the API exists and the user has
 * not asked to reduce motion. Guarded for jsdom, which lacks matchMedia.
 * @returns {boolean}
 */
function canTransition() {
    if (typeof document.startViewTransition !== 'function') return false;
    if (!window.matchMedia) return true;
    return !window.matchMedia('(prefers-reduced-motion: reduce)').matches;
}

/**
 * Whether the browser supports the view-transition `types` option (the
 * object-form startViewTransition + :active-view-transition-type selector).
 * Older View-Transition browsers still cross-fade; they just ignore types.
 * @returns {boolean}
 */
function supportsVTTypes() {
    return (
        typeof CSS !== 'undefined' &&
        !!CSS.supports &&
        CSS.supports('selector(:active-view-transition-type(x))')
    );
}

/**
 * Run `fn` (a DOM mutation) inside a view transition when possible, else run
 * it directly. Uses the object form to pass `types` when supported.
 * @param {{types?: string[]}|null} opts
 * @param {() => void} fn
 */
function runTransition(opts, fn) {
    if (!canTransition()) {
        fn();
        return;
    }
    const types = opts?.types;
    const vt =
        types?.length && supportsVTTypes()
            ? document.startViewTransition({ update: fn, types })
            : document.startViewTransition(fn);
    // `ready` rejects when the transition is skipped -- a duplicate
    // view-transition-name, or interruption by a newer transition (rapid nav).
    // The DOM still updates; swallow it so it isn't an unhandled rejection.
    vt?.ready?.catch(() => {});
}

/**
 * Whether this op batch is the one a pending transition is waiting for. 'replace'
 * matches only a page-swap OP_REPLACE (so a concurrent text/attr tick does not
 * consume the intent); 'any' matches any non-empty batch (a push_event result).
 * @param {Array<Array<*>>} ops
 * @param {'replace'|'any'} kind
 * @returns {boolean}
 */
function opsMatchTransition(ops, kind) {
    if (!ops?.length) return false;
    return kind === 'replace' ? ops.some((op) => op[0] === OP.REPLACE) : true;
}

/**
 * Read the `az-transition` attribute into a transition opts object, or null if
 * absent. The value is a space-separated list of view-transition type names
 * (like `class`); tokens are trimmed and empties dropped, so a bare attribute
 * or stray whitespace yields an empty `types` list (plain cross-fade).
 * @param {Element} el
 * @returns {{types: string[]}|null}
 */
function parseTransitionAttr(el) {
    if (!el.hasAttribute('az-transition')) return null;
    const raw = el.getAttribute('az-transition') || '';
    return { types: raw.split(/\s+/).filter(Boolean) };
}

/**
 * Wrap an element's parsed event commands in a synthetic transition command when
 * the element carries `az-transition`, so the attribute animates whatever those
 * commands do (a client effect, a navigate, or a push_event) -- not just links.
 * A no-op without the attribute.
 * @param {Element} el
 * @param {Array<*>} cmds
 * @returns {Array<*>}
 */
function withTransitionAttr(el, cmds) {
    const t = parseTransitionAttr(el);
    return t ? [JS_TRANSITION, t, cmds] : cmds;
}

/**
 * Stamp the pending transition opts onto the current (outgoing) history entry,
 * preserving existing state (e.g. _azScroll), so back/forward across this edge
 * replays the transition.
 */
function stampOutgoingTransition() {
    const st = history.state || {};
    history.replaceState({ ...st, _azTransition: _pendingTransition }, '', location.href);
}

/**
 * Perform an SPA navigation. Shared code path for `az-navigate` clicks and
 * `arizona_js:navigate/1,2` effects.
 *
 * @param {string} path     Path portion (no hash, no query), sent to the server.
 * @param {string} qs       Query string without leading `?`, sent to the server.
 * @param {string} hash     Fragment without leading `#`; used client-side
 *                          to scroll to the target after OP_REPLACE.
 * @param {{replace?: boolean, noscroll?: boolean, fullUrl?: string, kind?: string}} opts
 *   - replace  Use `replaceState` instead of `pushState`. Does not save
 *              outgoing scroll, does not reset scroll.
 *   - noscroll Push only: skip the scroll-to-top/hash after REPLACE.
 *   - fullUrl  Exact URL to write to history (defaults to
 *              `path + '?' + qs + '#' + hash`). Lets the click handler
 *              preserve the original href verbatim.
 *   - kind     `'navigate'` (default, replaces the root view) or `'patch'`
 *              (in-place: the server keeps the view and re-renders it). The
 *              kind is the WS frame verb and is tagged onto the history entry
 *              (`_azNav`) so popstate replays the same mode on back/forward.
 *
 * Reads `_pendingTransition` (set by the caller from the az-transition attribute
 * or a transition command): when set, it is stamped onto both the outgoing and
 * the new history entry so popstate can replay the transition across this edge.
 */
function navigateTo(path, qs, hash, opts) {
    const kind = opts.kind === 'patch' ? 'patch' : 'navigate';
    const pathAndQs = qs ? `${path}?${qs}` : path;
    const fullUrl = opts.fullUrl || (hash ? `${pathAndQs}#${hash}` : pathAndQs);
    /** @type {{_azTransition?: typeof _pendingTransition, _azNav?: string}} */
    const navState = {};
    if (_pendingTransition) navState._azTransition = _pendingTransition;
    if (kind === 'patch') navState._azNav = 'patch';
    const state = Object.keys(navState).length ? navState : null;
    if (opts.replace) {
        history.replaceState(state, '', fullUrl);
    } else {
        saveCurrentScroll(kind);
        if (_pendingTransition) stampOutgoingTransition();
        history.pushState(state, '', fullUrl);
        if (!opts.noscroll) _pendingScroll = { kind: 'push', hash, patch: kind === 'patch' };
    }
    workerPost(W_SEND, JSON.stringify([kind, { path, qs }]));
    workerPost(W_UPDATE_PATH, path);
    _currentPath = path;
    _currentQs = qs;
}

/**
 * Post a control message to the Worker. No-op if the Worker isn't
 * spawned. The Worker is the authority on transport state -- pre-open
 * `W_SEND` messages are dropped by the Worker itself, so the main
 * thread doesn't gate on `_connected` before posting.
 * @param {number} opcode
 * @param {...*} args
 */
function workerPost(opcode, ...args) {
    if (_worker) _worker.postMessage([opcode, ...args]);
}

/**
 * Mount a hook on an element with az-hook. Creates an instance, stores it,
 * and calls mounted() if defined. Skips if already tracked or hook not registered.
 *
 * The instance's prototype IS the hook definition, so a hook's own helper
 * methods are reachable as `this.method()` from any lifecycle callback.
 * el/__name/pushEvent are assigned as OWN properties -- they shadow the
 * def and stay framework-owned -- and any per-instance state a hook assigns to
 * `this` (e.g. `this.chart` in mounted) is an own property too, so it never
 * writes through to the shared def or leaks across instances.
 * @param {Element} el
 */
function mountHook(el) {
    if (_hooks.has(el)) return;
    const name = el.getAttribute('az-hook');
    if (!name || !hooks[name]) return;
    const def = hooks[name];
    const instance = Object.create(def);
    instance.el = el;
    instance.__name = name;
    /** @param {string} eventName @param {*} payload */
    instance.pushEvent = (eventName, payload) => {
        workerPost(W_SEND, JSON.stringify([resolveTarget(el), eventName, payload || {}]));
    };
    _hooks.set(el, instance);
    if (def.mounted) def.mounted.call(instance);
}

/**
 * Run hook lifecycle callback `phase` on `el` if it is tracked and the
 * hook def exports that phase.
 * @param {Element} el
 * @param {'mounted'|'updated'|'destroyed'} phase
 */
function runHookPhase(el, phase) {
    const instance = _hooks.get(el);
    if (!instance) return;
    const def = hooks[instance.__name];
    if (def?.[phase]) def[phase].call(instance);
}

/**
 * Destroy a hook on an element. Calls destroyed() if defined, then removes from tracking.
 * @param {Element} el
 */
function destroyHook(el) {
    runHookPhase(el, 'destroyed');
    _hooks.delete(el);
}

/**
 * Call updated() on a hooked element if it is tracked.
 * @param {Element} el
 */
function notifyUpdated(el) {
    runHookPhase(el, 'updated');
}

/**
 * Mount hooks on all [az-hook] elements within root (inclusive). `nodeType`, not
 * `instanceof Element`: root may come from a PiP document, whose realm has its own
 * Element constructor (a Document is nodeType 9, so it still skips the self-check).
 * @param {Element|Document} root
 */
function mountHooks(root) {
    if (root.nodeType === 1 && /** @type {Element} */ (root).hasAttribute('az-hook'))
        mountHook(/** @type {Element} */ (root));
    root.querySelectorAll('[az-hook]').forEach(mountHook);
}

/**
 * Destroy hooks on descendant [az-hook] elements only (not root itself).
 * Used by ops that replace inner content but keep the element (UPDATE, TEXT).
 * @param {Element} root
 */
function destroyChildHooks(root) {
    root.querySelectorAll('[az-hook]').forEach((el) => {
        destroyHook(el);
    });
}

/**
 * Destroy hooks on root and all descendant [az-hook] elements.
 * Used by ops that remove the element itself (REPLACE, REMOVE_NODE, REMOVE).
 * @param {Element} root
 */
function destroyHooks(root) {
    destroyHook(root);
    destroyChildHooks(root);
}

/**
 * Walk elements between a start marker and its closing <!--/az--> marker,
 * applying `fn` to each Element-typed node.
 * @param {Comment} startMarker
 * @param {(el: Element) => void} fn
 */
function forEachElementBetweenMarkers(startMarker, fn) {
    let node = startMarker.nextSibling;
    while (node && !(node.nodeType === 8 && /** @type {Comment} */ (node).data === '/az')) {
        if (node.nodeType === 1) fn(/** @type {Element} */ (node));
        node = node.nextSibling;
    }
}

/**
 * Apply a TEXT op: replace marker content (or el.textContent if no marker)
 * and walk hook lifecycle. `isHtml` distinguishes an HTML fragment (innerHTML) from a
 * scalar value (text node / textContent) -- see updateMarkerContent.
 * @param {Element} el
 * @param {string} az
 * @param {string} val
 * @param {boolean} [isHtml]
 */
function applyTextOp(el, az, val, isHtml) {
    const marker = findMarker(el, az);
    if (marker) {
        forEachElementBetweenMarkers(marker, destroyHooks);
        updateMarkerContent(marker, val, isHtml);
        forEachElementBetweenMarkers(marker, mountHooks);
    } else {
        destroyChildHooks(el);
        if (isHtml) {
            el.innerHTML = val;
        } else {
            // In-place when the element holds exactly one text node: `textContent =`
            // would remove + reinsert it (childList churn), and that forces a layout
            // recompute that reverts an in-progress scroll on WebKitGTK (it has no
            // CSS scroll anchoring). Writing the text node's data directly does not.
            // Mirrors updateLoneTextNode for the marker path. This is the per-tick hot
            // path for a single-value element (e.g. a live stat/price span).
            const child = el.firstChild;
            if (child && child.nodeType === 3 && !child.nextSibling) {
                /** @type {Text} */ (child).data = val;
            } else {
                el.textContent = val;
            }
        }
    }
    notifyUpdated(el);
}

/**
 * Apply a SET_ATTR op: setAttribute and sync the DOM `value` property for
 * form elements (setAttribute alone doesn't update the live value).
 * @param {Element} el
 * @param {string} name
 * @param {string} val
 */
function applySetAttrOp(el, name, val) {
    el.setAttribute(name, val);
    if (name === 'value' && 'value' in el) el.value = val;
    notifyUpdated(el);
}

/**
 * Apply a REM_ATTR op: removeAttribute and run the hook `updated` phase. The
 * canonical attribute-removal write shared by diff ops, item patches, and the
 * `arizona_js` attribute effects, so a removal behaves the same whatever drove it.
 * @param {Element} el
 * @param {string} name
 */
function applyRemAttrOp(el, name) {
    el.removeAttribute(name);
    notifyUpdated(el);
}

/**
 * Apply an UPDATE op: replace innerHTML, walking hook lifecycle.
 * @param {Element} el
 * @param {string} html
 */
function applyUpdateOp(el, html) {
    destroyChildHooks(el);
    el.innerHTML = html;
    mountHooks(el);
    notifyUpdated(el);
}

/**
 * Remove an element from the DOM, running its hook teardown first. The canonical
 * destroy+remove used by every node removal (diff `OP_REMOVE_NODE`, stream
 * `OP_REMOVE`, and plain-list `OP_LIST_PATCH` item removal) so teardown can never
 * be skipped.
 * @param {Element} el
 */
function removeEl(el) {
    destroyHooks(el);
    el.remove();
}

/**
 * Apply a batch of ops to the DOM. Ops arrive pre-resolved from the Worker --
 * all template payloads are already HTML strings. A view transition, when one is
 * pending, wraps this call (plus the message's effects) at the message handler,
 * not here -- so `applyOps` itself is synchronous.
 * @param {Array<Array<*>>} ops
 */
function applyOps(ops) {
    let didReplace = false;
    for (const op of ops) {
        const el = resolveEl(op[1]);
        if (!el) continue;
        const az = op[1].substring(op[1].indexOf(':') + 1);
        switch (op[0]) {
            case OP.TEXT:
                applyTextOp(el, az, op[2], op[3]);
                break;
            case OP.SET_ATTR:
                applySetAttrOp(el, op[2], op[3]);
                break;
            case OP.REM_ATTR:
                applyRemAttrOp(el, op[2]);
                break;
            case OP.UPDATE:
                applyUpdateOp(el, op[2]);
                break;
            case OP.REPLACE: {
                destroyHooks(el);
                // Hold the replacement's roots BEFORE inserting them: a navigate mounts a
                // view whose id differs, so re-resolving `op[1]` (which names the OUTGOING
                // view) after the swap finds nothing and the destination's hooks would
                // never mount. Same parse-then-mount shape as OP_INSERT.
                const tpl = el.ownerDocument.createElement('template');
                tpl.innerHTML = op[2];
                const added = Array.from(tpl.content.children);
                el.replaceWith(tpl.content);
                for (const e of added) mountHooks(e);
                didReplace = true;
                break;
            }
            case OP.REMOVE_NODE:
                removeEl(el);
                break;
            case OP.INSERT:
                insertItem(op[1], op[2], op[3], op[4]);
                break;
            case OP.REMOVE:
                removeItem(op[1], op[2]);
                break;
            case OP.ITEM_PATCH:
                patchItem(op[1], op[2], op[3]);
                break;
            case OP.MOVE:
                moveItem(op[1], op[2], op[3]);
                break;
            case OP.LIST_PATCH:
                applyListPatch(el, az, op[2]);
                break;
        }
    }
    // Navigate scrolls on its OP_REPLACE (robust: only a navigation emits one).
    // A patch has no OP_REPLACE, so it scrolls on the first non-empty diff batch
    // after the patch frame (tagged `patch` on _pendingScroll); same concurrent-
    // push race as patch transitions, and navigate is left strictly on didReplace.
    if (_pendingScroll && (didReplace || (_pendingScroll.patch && ops.length > 0))) {
        applyScroll(_pendingScroll);
        _pendingScroll = null;
    }
}

/**
 * Resolve a patch target to a DOM element. Bare targets (no colon) resolve to
 * the view root element itself -- used by OP_REPLACE for navigation. Scoped
 * targets ("viewId:az") find the view root, then the element within it.
 * @param {string} target
 * @returns {Element|null}
 */
function resolveEl(target) {
    const i = target.indexOf(':');
    if (i === -1) return findViewRoot(target);
    const viewId = target.substring(0, i);
    const az = target.substring(i + 1);
    const view = findViewRoot(viewId);
    if (!view) return null;
    if (view.getAttribute('az') === az) return view;
    let el = view.querySelector(`[az="${az}"]`);
    if (!el) {
        const j = az.indexOf(':');
        if (j !== -1) el = view.querySelector(`[az="${az.substring(0, j)}"]`);
    }
    return el;
}

/**
 * Dynamic text is bracketed by HTML comment markers: <!--az:X-->...<!--/az-->.
 * This lets us update inline text without a wrapper element. We scan direct
 * children only (nodeType 8 = Comment) to find the opening marker.
 * @param {Element} el
 * @param {string} az
 * @returns {Comment|null}
 */
function findMarker(el, az) {
    for (const node of el.childNodes) {
        if (node.nodeType === 8 && /** @type {Comment} */ (node).data === `az:${az}`) {
            return /** @type {Comment} */ (node);
        }
    }
    return null;
}

/**
 * If the slot between `startMarker` and its `<!--/az-->` already holds exactly one
 * text node, update it in place (`node.data = value`, a characterData write) and
 * return true. The reason this matters: a childList remove+insert of the text node
 * forces a layout recompute, and WebKitGTK reverts an IN-PROGRESS / uncommitted
 * scroll offset to a remembered position when that happens (it implements no CSS
 * scroll anchoring; Chromium/Firefox do not exhibit this). So a live per-tick text
 * update -- the common case -- would yank a user who is mid-scroll backward. An
 * in-place data write carries no such side effect and is also cheaper. Returns
 * false when the slot is not a lone text node (empty, an HTML fragment, or several
 * nodes), so callers fall back to the general remove+insert path.
 * @param {Comment} startMarker
 * @param {string} value
 * @returns {boolean}
 */
function updateLoneTextNode(startMarker, value) {
    const first = startMarker.nextSibling;
    const after = first?.nextSibling;
    if (
        first &&
        first.nodeType === 3 &&
        after &&
        after.nodeType === 8 &&
        /** @type {Comment} */ (after).data === '/az'
    ) {
        /** @type {Text} */ (first).data = value;
        return true;
    }
    return false;
}

/**
 * Replace everything between <!--az:X--> and <!--/az--> with new content.
 * `isHtml` (carried on the op by the worker) selects the renderer: a <template> that
 * parses tags for an HTML fragment (nested template, plain-list `?each`, or a `?raw`
 * value), or a text node for a scalar value. A scalar is ALWAYS a text node -- never
 * sniffed for `<` -- so a `?get` value containing markup is shown as literal text and
 * cannot inject (matching SSR, which escapes the same value).
 * @param {Comment} startMarker
 * @param {string} value
 * @param {boolean} [isHtml]
 */
function updateMarkerContent(startMarker, value, isHtml) {
    const doc = startMarker.ownerDocument;
    // Scalar fast path: a lone text node is updated in place (no childList churn,
    // which would revert an in-progress scroll on WebKitGTK -- see updateLoneTextNode).
    if (!isHtml && updateLoneTextNode(startMarker, value)) return;
    let node = startMarker.nextSibling;
    while (node && !(node.nodeType === 8 && /** @type {Comment} */ (node).data === '/az')) {
        const next = node.nextSibling;
        node.remove();
        node = next;
    }
    // Insert new content before the closing marker
    if (isHtml) {
        const tpl = doc.createElement('template');
        tpl.innerHTML = value;
        startMarker.after(tpl.content);
    } else {
        startMarker.after(doc.createTextNode(value));
    }
}

// ---------------------------------------------------------------------------
// Client-owned slots (?local): the browser owns a slot the server renders once
// and never diffs. Discovery is self-describing -- elements carry an `az-local`
// descriptor (JSON: {c: contentKey, a: {attrName: key}}) -- so set/get query
// the live DOM directly; no persistent index, nothing to sync across renders.
// ---------------------------------------------------------------------------

/**
 * Replace the content between <!--az:X--> and <!--/az--> with a TEXT node.
 * Unlike updateMarkerContent it never interprets the value as HTML, so a
 * client-set value containing `<` can't inject markup.
 * @param {Comment} startMarker
 * @param {*} value
 */
function setMarkerText(startMarker, value) {
    const str = value == null ? '' : String(value);
    // Same in-place fast path as updateMarkerContent: avoid childList churn that
    // reverts an in-progress scroll on WebKitGTK (see updateLoneTextNode).
    if (updateLoneTextNode(startMarker, str)) return;
    let node = startMarker.nextSibling;
    while (node && !(node.nodeType === 8 && /** @type {Comment} */ (node).data === '/az')) {
        const next = node.nextSibling;
        node.remove();
        node = next;
    }
    startMarker.after(startMarker.ownerDocument.createTextNode(str));
}

/**
 * Reconstruct a content slot's comment-marker az from the element's runtime az
 * and the slot index. Mirrors arizona_html:text_az/2 (slot 0 reuses the element
 * az; slot N appends ":N") -- a cross-language wire contract.
 * @param {Element} el
 * @param {string} slot
 * @returns {string}
 */
function localMarkerAz(el, slot) {
    const elAz = el.getAttribute('az') || '';
    return slot === '0' ? elAz : `${elAz}:${slot}`;
}

/**
 * Write a value to one bound slot.
 * @param {Element} el
 * @param {string[]} target -- ['content', slot], ['attr', name], or
 *   ['attr', name, prefix, suffix] (interpolated attribute)
 * @param {*} value
 */
function writeLocalValue(el, target, value) {
    if (target[0] === 'content') {
        const marker = findMarker(el, localMarkerAz(el, target[1]));
        if (marker) setMarkerText(marker, value);
        else el.textContent = value == null ? '' : String(value);
        notifyUpdated(el);
        return;
    }
    const name = target[1];
    if (target.length === 4) {
        // Interpolated: recompose prefix + value + suffix (always a string attr).
        applySetAttrOp(el, name, target[2] + String(value) + target[3]);
    } else if (value === false || value == null) {
        el.removeAttribute(name);
        notifyUpdated(el);
    } else if (value === true) {
        applySetAttrOp(el, name, '');
    } else {
        applySetAttrOp(el, name, String(value));
    }
}

/**
 * Read the current value of one bound slot from the DOM.
 * @param {Element} el
 * @param {string[]} target -- ['content', slot], ['attr', name], or
 *   ['attr', name, prefix, suffix] (interpolated attribute)
 * @returns {*}
 */
function readLocalValue(el, target) {
    if (target[0] === 'content') {
        const marker = findMarker(el, localMarkerAz(el, target[1]));
        if (!marker) return el.textContent;
        let text = '';
        let node = marker.nextSibling;
        while (node && !(node.nodeType === 8 && /** @type {Comment} */ (node).data === '/az')) {
            text += node.textContent ?? '';
            node = node.nextSibling;
        }
        return text;
    }
    const name = target[1];
    if (target.length === 4) {
        // Interpolated: strip the known prefix/suffix to recover the local value.
        const v = el.getAttribute(name) ?? '';
        return v.slice(target[2].length, v.length - target[3].length);
    }
    if (!el.hasAttribute(name)) return false;
    const v = el.getAttribute(name);
    return v === '' ? true : v;
}

/**
 * Visit each bound slot for `key` under `root`. When `viewId` is non-null only
 * slots whose nearest view is `viewId` are visited (per-view isolation).
 * @param {Element|Document} root
 * @param {string} key
 * @param {string|null} viewId
 * @param {(el: Element, target: string[]) => void} fn
 */
function forEachLocal(root, key, viewId, fn) {
    /** @param {Element} el */
    const visit = (el) => {
        const desc = el.getAttribute('az-local');
        if (!desc) return;
        if (viewId !== null && resolveTarget(el) !== viewId) return;
        // The descriptor is always framework-generated valid JSON.
        const parsed = JSON.parse(desc);
        // c maps each content slot index -> key; a maps each attr name -> key;
        // ap (optional) carries [prefix, suffix] for interpolated attributes.
        if (parsed.c) {
            for (const [slot, k] of Object.entries(parsed.c)) {
                if (k === key) fn(el, ['content', slot]);
            }
        }
        if (parsed.a) {
            for (const [attr, k] of Object.entries(parsed.a)) {
                if (k === key) {
                    const aff = parsed.ap?.[attr];
                    fn(el, aff ? ['attr', attr, aff[0], aff[1]] : ['attr', attr]);
                }
            }
        }
    };
    if (root instanceof Element && root.hasAttribute('az-local')) visit(root);
    root.querySelectorAll('[az-local]').forEach(visit);
}

/**
 * Set a client-owned slot (`?local`) in one view, locally -- no server
 * round-trip. Use setAll for document-wide.
 * @param {string} viewId
 * @param {string} key
 * @param {*} value
 */
function set(viewId, key, value) {
    const root = findViewRoot(viewId);
    if (!root) return;
    forEachLocal(root, key, viewId, (el, target) => writeLocalValue(el, target, value));
}

/**
 * Set a client-owned slot (`?local`) in every view on the page (document-wide).
 * @param {string} key
 * @param {*} value
 */
function setAll(key, value) {
    for (const doc of allDocs())
        forEachLocal(doc, key, null, (el, target) => writeLocalValue(el, target, value));
}

/**
 * Read a client-owned slot (`?local`) from the DOM.
 *   get(key) -- first match anywhere | get(viewId, key) -- one view
 * @param {string} a
 * @param {string} [b]
 * @returns {*}
 */
function get(a, b) {
    const scoped = b !== undefined;
    const viewId = scoped ? a : null;
    const key = scoped ? /** @type {string} */ (b) : a;
    /** @type {*} */
    let result;
    /** @param {Element|Document} root */
    const scan = (root) =>
        forEachLocal(root, key, viewId, (el, target) => {
            if (result === undefined) result = readLocalValue(el, target);
        });
    if (viewId) {
        const root = findViewRoot(viewId);
        if (root) scan(root);
    } else {
        for (const doc of allDocs()) scan(doc);
    }
    return result;
}

/**
 * Insert a keyed child into a container element.
 * @param {Element} el -- container element
 * @param {string} key
 * @param {number} pos -- -1 means append, otherwise insert before child at index
 * @param {string} html
 */
function insertItemEl(el, key, pos, html) {
    const tpl = el.ownerDocument.createElement('template');
    tpl.innerHTML = html;
    const fragment = tpl.content;
    if (pos === -1) {
        el.appendChild(fragment);
    } else {
        const children = el.querySelectorAll(':scope > [az-key]');
        if (pos < children.length) {
            el.insertBefore(fragment, children[pos]);
        } else {
            el.appendChild(fragment);
        }
    }
    const item = el.querySelector(`[az-key="${key}"]`);
    if (item) mountHooks(item);
    else console.warn(`[arizona] stream item missing az-key="${key}" after insert`);
}

/**
 * If `target` resolves to an element, call `fn` with it. Otherwise no-op.
 * @param {string} target
 * @param {(el: Element) => void} fn
 */
function withTarget(target, fn) {
    const el = resolveEl(target);
    if (el) fn(el);
}

/**
 * Stream ops -- insert, remove, and patch operate on keyed children (az-key)
 * within a container element. pos=-1 means append; otherwise insert before
 * the child at that index.
 * @param {string} target
 * @param {string} key
 * @param {number} pos
 * @param {string} html
 */
function insertItem(target, key, pos, html) {
    withTarget(target, (el) => insertItemEl(el, key, pos, html));
}

/**
 * Remove a keyed child from a container element.
 * @param {Element} el -- container element
 * @param {string} key
 */
function removeItemEl(el, key) {
    const item = el.querySelector(`:scope > [az-key="${key}"]`);
    if (!item) {
        console.warn(`[arizona] stream item az-key="${key}" not found for remove`);
        return;
    }
    removeEl(item);
}

/**
 * Remove a keyed child element from its container.
 * @param {string} target
 * @param {string} key
 */
function removeItem(target, key) {
    withTarget(target, (el) => removeItemEl(el, key));
}

/**
 * Move a keyed child after another keyed element within a container.
 * @param {Element} el -- container element
 * @param {string} key
 * @param {string|null} afterKey -- key of preceding sibling, or null for prepend
 */
function moveItemEl(el, key, afterKey) {
    const item = el.querySelector(`:scope > [az-key="${key}"]`);
    if (!item) {
        console.warn(`[arizona] stream item az-key="${key}" not found for move`);
        return;
    }
    if (afterKey === null) {
        el.prepend(item);
    } else {
        const ref = el.querySelector(`:scope > [az-key="${afterKey}"]`);
        if (ref) ref.after(item);
        else el.appendChild(item);
    }
    notifyUpdated(item);
}

/**
 * Move a keyed child element after another keyed element (or prepend if
 * afterKey is null). Preserves form state, focus, scroll position, CSS
 * animations, and hook instances.
 * @param {string} target
 * @param {string} key
 * @param {string|null} afterKey -- key of preceding sibling, or null for prepend
 */
function moveItem(target, key, afterKey) {
    withTarget(target, (el) => moveItemEl(el, key, afterKey));
}

/**
 * Apply `innerOps` to the keyed child of `container`. Warns and no-ops if
 * the key isn't present.
 * @param {Element} container
 * @param {string} key
 * @param {Array<Array<*>>} innerOps
 */
function applyItemPatch(container, key, innerOps) {
    const item = container.querySelector(`:scope > [az-key="${key}"]`);
    if (!item) {
        console.warn(`[arizona] stream item az-key="${key}" not found for patch`);
        return;
    }
    applyItemOps(item, innerOps);
}

/**
 * Resolve a nested item for patching: find the inner container by az,
 * then apply innerOps scoped to its keyed child.
 * @param {Element} parentEl -- parent item or container element
 * @param {string} az -- az attribute to find the container within parentEl
 * @param {string} key -- az-key of the item to patch
 * @param {Array<Array<*>>} innerOps -- ops scoped to the item
 */
function patchItemEl(parentEl, az, key, innerOps) {
    applyItemPatch(resolveInnerEl(parentEl, az), key, innerOps);
}

/**
 * Apply ops scoped to a single keyed item. innerOps use bare az indices
 * (not "viewId:az"), resolved relative to the item element.
 * @param {string} target
 * @param {string} key
 * @param {Array<Array<*>>} innerOps
 */
function patchItem(target, key, innerOps) {
    withTarget(target, (el) => applyItemPatch(el, key, innerOps));
}

/**
 * Resolve an element within a parent by az attribute, with compound fallback.
 * For compound az like "0:1", tries exact match first, then base az "0".
 * @param {Element} parent
 * @param {string} az
 * @returns {Element}
 */
function resolveInnerEl(parent, az) {
    let el = parent.querySelector(`[az="${az}"]`);
    if (!el && az.includes(':')) {
        el = parent.querySelector(`[az="${az.substring(0, az.indexOf(':'))}"]`);
    }
    return el || parent;
}

/**
 * Apply inner ops to an item element. Ops arrive pre-resolved from the Worker.
 * Per-op behaviour matches `applyOps` via the shared `apply*Op` helpers,
 * differing only in element resolution: bare `az` resolved against `item`
 * via `resolveInnerEl/2`, with a separate fallback for `REMOVE_NODE`.
 * @param {Element} item
 * @param {Array<Array<*>>} innerOps
 */
function applyItemOps(item, innerOps) {
    for (const op of innerOps) {
        const az = op[1];
        switch (op[0]) {
            case OP.TEXT:
                applyTextOp(resolveInnerEl(item, az), az, op[2], op[3]);
                break;
            case OP.SET_ATTR:
                applySetAttrOp(resolveInnerEl(item, az), op[2], op[3]);
                break;
            case OP.REM_ATTR:
                applyRemAttrOp(resolveInnerEl(item, az), op[2]);
                break;
            case OP.UPDATE:
                applyUpdateOp(resolveInnerEl(item, az), op[2]);
                break;
            case OP.REMOVE_NODE: {
                const innerEl = item.querySelector(`[az="${az}"]`);
                if (innerEl) removeEl(innerEl);
                break;
            }
            case OP.INSERT:
                insertItemEl(resolveInnerEl(item, az), op[2], op[3], op[4]);
                break;
            case OP.REMOVE:
                removeItemEl(resolveInnerEl(item, az), op[2]);
                break;
            case OP.ITEM_PATCH:
                patchItemEl(item, az, op[2], op[3]);
                break;
            case OP.MOVE:
                moveItemEl(resolveInnerEl(item, az), op[2], op[3]);
                break;
            case OP.LIST_PATCH:
                applyListPatch(resolveInnerEl(item, az), az, op[2]);
                break;
        }
    }
}

/**
 * Apply a LIST_PATCH op: positional in-place patch of a single-root plain-list
 * `?each` slot. Unlike a stream (keyed by `az-key`), plain-list items are
 * addressed by DOM-order position between the slot's `<!--az:X-->...<!--/az-->`
 * markers. We snapshot the item element-roots ONCE up front, so a batch of
 * sub-ops can reference stable positions regardless of order, and a content
 * patch never touches the container's `childList` (which would revert an
 * in-progress scroll on WebKit -- the whole point). Sub-ops:
 *   [OP.ITEM_PATCH, idx, innerOps] -- patch item `idx` in place (no childList)
 *   [OP.REMOVE,     idx]           -- remove item `idx`
 *   [OP.INSERT,     idx, html]     -- insert a new item at position `idx` (before
 *                                     the item currently there, else the end
 *                                     marker -- the server only inserts at the tail)
 * @param {Element} el -- the element holding the slot markers
 * @param {string} az -- the slot's az (marker id)
 * @param {Array<Array<*>>} subOps
 */
function applyListPatch(el, az, subOps) {
    const marker = findMarker(el, az);
    if (!marker) {
        console.warn(`[arizona] list-patch slot marker az:${az} not found`);
        return;
    }
    // Snapshot the item roots (Element children) and locate the end marker.
    const roots = [];
    let node = marker.nextSibling;
    while (node && !(node.nodeType === 8 && /** @type {Comment} */ (node).data === '/az')) {
        if (node.nodeType === 1) roots.push(/** @type {Element} */ (node));
        node = node.nextSibling;
    }
    const endMarker = node;
    for (const sub of subOps) {
        switch (sub[0]) {
            case OP.ITEM_PATCH: {
                const item = roots[sub[1]];
                if (item) applyItemOps(item, sub[2]);
                break;
            }
            case OP.REMOVE: {
                const item = roots[sub[1]];
                if (item) removeEl(item);
                break;
            }
            case OP.INSERT: {
                const tpl = el.ownerDocument.createElement('template');
                tpl.innerHTML = sub[2];
                const added = Array.from(tpl.content.children);
                // Insert at position idx -- before the item currently there, or the
                // end marker for a tail insert (the server only inserts at the tail,
                // but honoring idx keeps this correct for any sub-op ordering).
                const ref = roots[sub[1]] ?? endMarker;
                if (ref) ref.before(tpl.content);
                else marker.after(tpl.content);
                for (const e of added) mountHooks(e);
                break;
            }
        }
    }
    notifyUpdated(el);
}

/**
 * Process server-side effects. Same op codes as JS commands (arizona_js.hrl).
 * Each effect is executed through the unified executeJS interpreter.
 * @param {Array<Array<*>>} effects
 */
function applyEffects(effects) {
    for (const eff of effects) {
        executeJS(document.documentElement, null, eff);
    }
}

/**
 * Send an event to the root view (first [az-view] element).
 * @param {string} event
 * @param {*} [payload]
 */
function pushEvent(event, payload) {
    const view = document.querySelector('[az-view]')?.id;
    pushEventTo(view, event, payload);
}

/**
 * Send an event to a specific view by id.
 * @param {string|null|undefined} view
 * @param {string} event
 * @param {*} [payload]
 */
function pushEventTo(view, event, payload) {
    workerPost(W_SEND, JSON.stringify([view, event, payload]));
}

/**
 * Determine which server-side view handles this element's events.
 * Explicit az-target takes priority; otherwise walk up to the nearest az-view.
 * @param {Element} el
 * @returns {string|null}
 */
function resolveTarget(el) {
    return el.getAttribute('az-target') || el.closest('[az-view]')?.id || null;
}

/**
 * Auto-collect payload from an element based on its type and event context.
 * Drop -> {data_transfer, drop_index}, Forms -> FormData,
 * inputs/selects/textareas -> {value}, otherwise -> {}.
 * @param {Element} el
 * @param {Event|null} event
 * @returns {Object<string, *>}
 */
function autoPayload(el, event) {
    if (event && /** @type {any} */ (event).dataTransfer) {
        const dropTarget = /** @type {Element} */ (event.target).closest('[az-key]');
        const children = Array.from(el.querySelectorAll(':scope > [az-key]'));
        return {
            data_transfer: /** @type {any} */ (event).dataTransfer.getData('text/plain'),
            drop_index: dropTarget ? children.indexOf(dropTarget) : -1,
        };
    }
    const tag = el.tagName;
    if (tag === 'FORM')
        return Object.fromEntries(new FormData(/** @type {HTMLFormElement} */ (el)));
    if (tag === 'INPUT' || tag === 'SELECT' || tag === 'TEXTAREA')
        return { value: /** @type {any} */ (el).value || '' };
    return {};
}

// JS command op codes -- must match include/arizona_effect.hrl
const JS_PUSH_EVENT = 0,
    JS_TOGGLE = 1,
    JS_SHOW = 2,
    JS_HIDE = 3,
    JS_ADD_CLASS = 4,
    JS_REMOVE_CLASS = 5,
    JS_TOGGLE_CLASS = 6,
    JS_SET_ATTR = 7,
    JS_REMOVE_ATTR = 8,
    JS_DISPATCH_EVENT = 9,
    JS_NAVIGATE = 10,
    JS_FOCUS = 11,
    JS_BLUR = 12,
    JS_SCROLL_TO = 13,
    JS_SET_TITLE = 14,
    JS_RELOAD = 15,
    JS_ON_KEY = 16,
    JS_SET_LOCAL = 17,
    JS_REQUEST_PIP = 18,
    JS_EXIT_PIP = 19,
    JS_TRANSITION = 20,
    JS_TOGGLE_ATTR = 21,
    JS_FETCH = 22,
    JS_OS = 23,
    JS_PATCH = 24,
    JS_RESET_FORM = 25,
    JS_SELECT = 26,
    JS_COPY_TO_CLIPBOARD = 27,
    JS_SHOW_MODAL = 28,
    JS_CLOSE_MODAL = 29;

// arizona_js credentials atoms -> fetch() credentials mode
/** @type {Record<string, RequestCredentials>} */
const CREDENTIALS = { same_origin: 'same-origin', include: 'include', omit: 'omit' };

/**
 * The native-shell bridge the embedding shell (Electron/Tauri/...) installs on
 * the page before connect(); `undefined` in a plain browser, where every OS
 * command/capability is a safe no-op.
 * @returns {{
 *   capabilities?: Record<string, unknown>,
 *   invoke?: (name: string, args: unknown[]) => Promise<unknown>,
 *   onEvent?: (cb: (name: string, payload: any) => void) => void,
 * } | undefined}
 */
function osHost() {
    return /** @type {any} */ (globalThis).__arizona_os__;
}

/**
 * Reset a form after a successful az-submit, when it opted in with `az-form-reset`.
 * Shared by the submit listener (synchronous, for push_event-style commands) and the
 * `fetch` command (deferred to a 2xx response, so the fields survive a validation error).
 * @param {Element} form
 */
function maybeResetForm(form) {
    if (form.hasAttribute('az-form-reset')) /** @type {HTMLFormElement} */ (form).reset();
}

/**
 * True when a parsed az-submit command (single or list) is/contains a `fetch`, so the
 * submit listener can defer `az-form-reset` to the fetch response instead of resetting
 * synchronously on submit.
 * @param {Array<*>} cmds
 */
function commandsIncludeFetch(cmds) {
    const list = Array.isArray(cmds[0]) ? cmds : [cmds];
    return list.some((c) => c[0] === JS_FETCH);
}

/**
 * Call `fn` with the FIRST element matching `sel` (cast to `HTMLElement`),
 * searched across the main document and any PiP documents. First-match only --
 * the single-target effects `focus`/`blur`/`scroll_to` use this, since those act
 * on one element by definition. The broadcast effects use `withQueryAll`.
 * @param {string} sel
 * @param {(el: HTMLElement) => void} fn
 */
function withQuery(sel, fn) {
    for (const doc of allDocs()) {
        const t = /** @type {HTMLElement|null} */ (doc.querySelector(sel));
        if (t) {
            fn(t);
            return;
        }
    }
}

/**
 * Call `fn` for EVERY element matching `sel`, across the main document and any
 * PiP documents. Used by the broadcast effects (`toggle`/`show`/`hide`/
 * `*_class`/`*_attr`), so a selector matching several elements affects them all
 * -- honoring the builders' "all elements matching the selector" contract.
 * @param {string} sel
 * @param {(el: HTMLElement) => void} fn
 */
function withQueryAll(sel, fn) {
    for (const doc of allDocs()) {
        for (const t of doc.querySelectorAll(sel)) {
            fn(/** @type {HTMLElement} */ (t));
        }
    }
}

/**
 * Execute JS commands from an az-* attribute value.
 * Single command: [opcode, ...args]
 * Multiple commands: [[opcode, ...args], [opcode, ...args]]
 * @param {Element} el - the element that triggered the event
 * @param {Event|null} event - the DOM event (null for programmatic dispatch)
 * @param {Array<*>} cmds - parsed JSON command(s)
 */
function executeJS(el, event, cmds) {
    const commands = Array.isArray(cmds[0]) ? cmds : [cmds];
    for (const cmd of commands) execOne(el, event, cmd);
}

/**
 * Execute a single JS command (the per-command body extracted from executeJS).
 * @param {Element} el
 * @param {Event|null} event
 * @param {Array<*>} cmd
 */
function execOne(el, event, cmd) {
    const op = cmd[0];
    switch (op) {
        case JS_TRANSITION: {
            // Wrap the inner command(s)' DOM change in a view transition. A sync
            // effect (toggle/add_class/...) wraps in place; navigate/push_event
            // produce a future server diff, so stash the intent and let the
            // worker message handler wrap the matching batch.
            const opts = cmd[1] || {};
            const inner = cmd[2];
            const innerCmds = /** @type {Array<Array<*>>} */ (
                Array.isArray(inner[0]) ? inner : [inner]
            );
            const kind = innerCmds.some((c) => c[0] === JS_NAVIGATE)
                ? 'replace'
                : innerCmds.some((c) => c[0] === JS_PUSH_EVENT || c[0] === JS_PATCH)
                  ? 'any'
                  : null;
            if (kind) {
                _pendingTransition = { types: opts.types, kind };
                executeJS(el, event, inner);
            } else {
                runTransition(opts, () => executeJS(el, event, inner));
            }
            break;
        }
        case JS_PUSH_EVENT: {
            const evt = cmd[1];
            const payload =
                cmd.length > 2 ? { ...autoPayload(el, event), ...cmd[2] } : autoPayload(el, event);
            const msg = JSON.stringify([resolveTarget(el), evt, payload]);
            if (event) {
                scheduleSend(el, event, () => {
                    workerPost(W_SEND, msg);
                });
            } else {
                workerPost(W_SEND, msg);
            }
            break;
        }
        case JS_TOGGLE:
            withQueryAll(cmd[1], (t) => {
                t.hidden = !t.hidden;
                notifyUpdated(t);
            });
            break;
        case JS_SHOW:
            withQueryAll(cmd[1], (t) => {
                t.hidden = false;
                notifyUpdated(t);
            });
            break;
        case JS_HIDE:
            withQueryAll(cmd[1], (t) => {
                t.hidden = true;
                notifyUpdated(t);
            });
            break;
        case JS_ADD_CLASS:
            withQueryAll(cmd[1], (t) => {
                t.classList.add(cmd[2]);
                notifyUpdated(t);
            });
            break;
        case JS_REMOVE_CLASS:
            withQueryAll(cmd[1], (t) => {
                t.classList.remove(cmd[2]);
                notifyUpdated(t);
            });
            break;
        case JS_TOGGLE_CLASS:
            withQueryAll(cmd[1], (t) => {
                t.classList.toggle(cmd[2]);
                notifyUpdated(t);
            });
            break;
        case JS_SET_ATTR:
            withQueryAll(cmd[1], (t) => applySetAttrOp(t, cmd[2], cmd[3]));
            break;
        case JS_REMOVE_ATTR:
            withQueryAll(cmd[1], (t) => applyRemAttrOp(t, cmd[2]));
            break;
        case JS_TOGGLE_ATTR:
            // 3 args: presence toggle (remove if present, else set bare). 5 args:
            // value toggle (cmd[3] <-> cmd[4]; any other current value -> cmd[3]).
            // Each match is toggled on its own current state.
            withQueryAll(cmd[1], (t) => {
                if (cmd.length === 3) {
                    if (t.hasAttribute(cmd[2])) applyRemAttrOp(t, cmd[2]);
                    else applySetAttrOp(t, cmd[2], '');
                } else {
                    applySetAttrOp(t, cmd[2], t.getAttribute(cmd[2]) === cmd[3] ? cmd[4] : cmd[3]);
                }
            });
            break;
        case JS_DISPATCH_EVENT:
            document.dispatchEvent(new CustomEvent(cmd[1], { detail: cmd[2] || {} }));
            break;
        case JS_NAVIGATE: {
            const full = cmd[1];
            const u = new URL(full, location.origin);
            const hash = u.hash ? u.hash.slice(1) : '';
            const qs = u.search ? u.search.slice(1) : '';
            navigateTo(u.pathname, qs, hash, { ...(cmd[2] || {}), fullUrl: full });
            break;
        }
        case JS_PATCH: {
            // In-place SPA navigation: keep the view, re-render via handle_update.
            const full = cmd[1];
            const u = new URL(full, location.origin);
            const hash = u.hash ? u.hash.slice(1) : '';
            const qs = u.search ? u.search.slice(1) : '';
            navigateTo(u.pathname, qs, hash, { ...(cmd[2] || {}), fullUrl: full, kind: 'patch' });
            break;
        }
        case JS_FETCH: {
            // HTTP request via fetch() with no page reload. Unlike push_event (WS,
            // can't set cookies), the response can carry a real Set-Cookie, applied
            // natively by the browser. The controller returns the {e:[...]} effects
            // wire payload; we apply it against the enclosing view element below, so a
            // push_event in the response resolves to (and re-renders) the submitting
            // view without scraping the form -- pubsub is for broadcasting to other views.
            const url = cmd[1];
            const opts = cmd[2] || {};
            const form = /** @type {HTMLFormElement|null} */ (el?.closest?.('form') ?? null);
            const method = (opts.method || form?.getAttribute('method') || 'post').toUpperCase();
            const headers = { accept: 'application/json', ...(opts.headers || {}) };
            let target = url;
            let body;
            if (method === 'GET' || method === 'HEAD') {
                // No request body for GET/HEAD -- carry a form's fields in the query
                // string instead (fetch is otherwise POST-oriented: it sets cookies).
                if (form) {
                    const fd = /** @type {any} */ (new FormData(form));
                    const qs = new URLSearchParams(fd).toString();
                    if (qs) target += (url.includes('?') ? '&' : '?') + qs;
                }
            } else if (opts.body !== undefined) {
                body = JSON.stringify(opts.body);
                headers['content-type'] = 'application/json';
            } else if (form) {
                // Mirror a normal form POST: application/x-www-form-urlencoded.
                // (multipart / file uploads are a documented non-goal.)
                body = new URLSearchParams(/** @type {any} */ (new FormData(form)));
            }
            const onError = (/** @type {object} */ detail) => {
                if (opts.on_error) executeJS(el, event, opts.on_error);
                document.dispatchEvent(new CustomEvent('arizona:fetch-error', { detail }));
            };
            fetch(target, {
                method,
                body,
                credentials: CREDENTIALS[opts.credentials] || 'same-origin',
                headers,
                keepalive: opts.keep_alive === true,
            })
                .then((resp) =>
                    resp.text().then((text) => {
                        // Apply the effects body whenever it parses -- even on a 4xx, so
                        // the server can drive inline validation with a real status. The
                        // effects run against the enclosing view element (not the form, not
                        // document), so a `push_event` in the response resolves to the
                        // submitting view and re-renders it via handle_event (no pubsub) --
                        // without scraping the form's fields into the event payload (the
                        // view element isn't a form, so autoPayload is empty; the controller
                        // passes any result explicitly). An empty 2xx body (a cookie-only
                        // response) applies nothing. on_error runs only when there is no
                        // usable effects body: a non-JSON page or an empty non-2xx.
                        let effects = null;
                        if (text) {
                            try {
                                effects = JSON.parse(text).e || [];
                            } catch {
                                effects = null;
                            }
                        } else if (resp.ok) {
                            effects = [];
                        }
                        if (effects !== null)
                            executeJS(el?.closest?.('[az-view]') ?? el, null, effects);
                        else onError({ url, status: resp.status });
                        // Honor az-form-reset only on a 2xx success, so a validation
                        // error (a non-2xx) keeps the typed fields.
                        if (resp.ok && form) maybeResetForm(form);
                    }),
                )
                .catch((error) => onError({ url, error }));
            break;
        }
        case JS_FOCUS:
            withQuery(cmd[1], (t) => t.focus());
            break;
        case JS_BLUR:
            withQuery(cmd[1], (t) => t.blur());
            break;
        case JS_RESET_FORM:
            // Broadcast (all matches, main + PiP docs), like the class/visibility
            // effects: reset every matching form. A non-form match (no reset()) is
            // a safe no-op. Fires updated() so a hook observes it like a diff.
            withQueryAll(cmd[1], (t) => {
                const f = /** @type {any} */ (t);
                if (typeof f.reset === 'function') {
                    f.reset();
                    notifyUpdated(t);
                }
            });
            break;
        case JS_SELECT:
            // First match only, like focus/blur/scroll_to. Selection is not a DOM
            // mutation, so no notifyUpdated. A non-input/textarea match (no
            // select()) is a safe no-op.
            withQuery(cmd[1], (t) => {
                const f = /** @type {any} */ (t);
                if (typeof f.select === 'function') f.select();
            });
            break;
        case JS_COPY_TO_CLIPBOARD:
            // First match only. Copy the matched element's value (form control) or
            // textContent to the clipboard. Requires a secure context + user gesture
            // (event command only); a missing/blocked clipboard is a safe no-op. Not
            // a DOM mutation, so no notifyUpdated.
            withQuery(cmd[1], (t) => {
                const text = /** @type {any} */ (t).value ?? t.textContent ?? '';
                navigator.clipboard?.writeText?.(text);
            });
            break;
        case JS_SHOW_MODAL:
            // First match only. Open the matched <dialog> as a true modal (top
            // layer, ::backdrop, ESC-to-close). A non-dialog match (no showModal())
            // is a safe no-op. Fires updated() so a hook observes it like a diff.
            withQuery(cmd[1], (t) => {
                const f = /** @type {any} */ (t);
                if (typeof f.showModal === 'function') {
                    f.showModal();
                    notifyUpdated(t);
                }
            });
            break;
        case JS_CLOSE_MODAL:
            // First match only. Close the matched <dialog>. A non-dialog match (no
            // close()) is a safe no-op. Fires updated() so a hook observes it.
            withQuery(cmd[1], (t) => {
                const f = /** @type {any} */ (t);
                if (typeof f.close === 'function') {
                    f.close();
                    notifyUpdated(t);
                }
            });
            break;
        case JS_SCROLL_TO:
            withQuery(cmd[1], (t) => t.scrollIntoView(cmd[2] || { behavior: 'smooth' }));
            break;
        case JS_SET_TITLE:
            document.title = cmd[1];
            break;
        case JS_RELOAD:
            location.reload();
            break;
        case JS_ON_KEY: {
            const f = cmd[1];
            const lk =
                event && /** @type {any} */ (event).key
                    ? /** @type {any} */ (event).key.toLowerCase()
                    : '';
            if (Array.isArray(f) ? f.includes(lk) : new RegExp(f).test(lk))
                executeJS(el, event, cmd[2]);
            break;
        }
        case JS_SET_LOCAL: {
            // Client-owned slot update -- never sent to the server. cmd[3]:
            // absent => closest view (the trigger); a viewId string => that
            // view; true => all views.
            const scope = cmd[3];
            if (scope === true) {
                setAll(cmd[1], cmd[2]);
            } else {
                const viewId = scope ?? resolveTarget(el);
                if (viewId) set(viewId, cmd[1], cmd[2]);
            }
            break;
        }
        case JS_REQUEST_PIP:
            requestPip(cmd[1], cmd[2] || {});
            break;
        case JS_EXIT_PIP:
            exitPip(cmd[1]);
            break;
        case JS_OS: {
            // Native-shell (OS) command -- delegate to the embedding shell's
            // invoke if one is present (Electron/Tauri/...). A plain browser has
            // no `__arizona_os__`, so this is a safe no-op. invoke() is async;
            // log (don't crash) on rejection so a failing OS command is visible.
            const args = cmd.slice(2);
            const r = osHost()?.invoke?.(cmd[1], args);
            r?.catch?.((err) => console.error('[arizona] OS command failed:', cmd[1], args, err));
            break;
        }
    }
}

/**
 * @typedef {{
 *   id?: ReturnType<typeof setTimeout>,
 *   pending?: Function|null,
 *   eventBound?: boolean,
 *   blurBound?: boolean,
 *   prevKey?: string
 * }} TimerState
 */

/**
 * Per-element timer state, keyed by DOM element. WeakMap so entries are
 * GC'd when elements are removed from the document.
 * @type {WeakMap<Element, TimerState>}
 */
const _timers = new WeakMap();

/**
 * Three send modes controlled by az-debounce and az-throttle attributes:
 *
 * 1. Immediate -- no attributes: sendFn fires right away.
 *
 * 2. Numeric debounce/throttle -- az-debounce="300" or az-throttle="300":
 *    debounce resets the timer on every event; throttle sends immediately
 *    then suppresses for the interval (trailing send if events arrived).
 *    For keydown throttle, a different key resets the interval so each
 *    distinct key gets through promptly.
 *
 * 3. Event-name debounce -- az-debounce="blur": stores the latest sendFn
 *    but only flushes when the named DOM event fires on the element.
 *    Useful for sending input values only when the field loses focus.
 *
 * For modes 2 and 3, blur auto-flushes pending sends so data isn't lost
 * when the user tabs away or clicks elsewhere.
 * @param {Element} el
 * @param {Event|KeyboardEvent} event
 * @param {Function} sendFn
 */
function scheduleSend(el, event, sendFn) {
    const debounceAttr = el.getAttribute('az-debounce') || '';
    const debounceMs = parseInt(debounceAttr, 10);
    const debounceEvent = Number.isNaN(debounceMs) && debounceAttr !== '' ? debounceAttr : '';
    const throttleMs = parseInt(el.getAttribute('az-throttle') || '', 10);
    if (!debounceMs && !throttleMs && !debounceEvent) {
        sendFn();
        return;
    }
    if (!_timers.has(el)) _timers.set(el, {});
    const t = /** @type {TimerState} */ (_timers.get(el));
    // Event-name debounce: just store the latest send, flush on the named event.
    if (debounceEvent) {
        t.pending = sendFn;
        if (!t.eventBound) {
            t.eventBound = true;
            el.addEventListener(debounceEvent, () => flushTimer(el));
        }
        return;
    }
    if (debounceMs > 0) {
        // Classic debounce: reset timer on every event.
        clearTimeout(t.id);
        t.pending = sendFn;
        t.id = setTimeout(() => {
            t.id = undefined;
            t.pending = null;
            sendFn();
        }, debounceMs);
    } else {
        // Throttle: send immediately, suppress duplicates for the interval.
        // For keydown, a different key resets the cooldown so the new key
        // isn't swallowed by the previous key's throttle window.
        if (event.type === 'keydown' && t.prevKey !== /** @type {KeyboardEvent} */ (event).key) {
            clearTimeout(t.id);
            t.id = undefined;
        }
        if (event.type === 'keydown') t.prevKey = /** @type {KeyboardEvent} */ (event).key;
        t.pending = sendFn;
        if (t.id) return;
        sendFn();
        t.pending = null;
        t.id = setTimeout(() => {
            const pending = t.pending;
            t.id = undefined;
            t.pending = null;
            if (pending) pending();
        }, throttleMs);
    }
    // Auto-flush on blur so pending data isn't lost when focus leaves.
    if (!t.blurBound) {
        t.blurBound = true;
        el.addEventListener('blur', () => flushTimer(el));
    }
}

/**
 * Immediately send any pending debounced/throttled message for this element.
 * Used by blur auto-flush and form submit (to capture in-flight input values).
 * @param {Element} el
 */
function flushTimer(el) {
    const t = _timers.get(el);
    if (!t) return;
    if (t.id) clearTimeout(t.id);
    const pending = t.pending;
    t.id = undefined;
    t.pending = null;
    if (pending) pending();
}

/**
 * Save form state for all forms with id attributes. Called on disconnect
 * to preserve user input across reconnections.
 */
function saveFormState() {
    _savedForms.clear();
    document.querySelectorAll('form[id]').forEach((form) => {
        const fd = new FormData(/** @type {HTMLFormElement} */ (form));
        /** @type {Object<string, string|string[]>} */
        const data = {};
        for (const [k, v] of fd.entries()) {
            if (k in data) {
                const prev = data[k];
                data[k] = Array.isArray(prev)
                    ? prev.concat(/** @type {string} */ (v))
                    : [prev, /** @type {string} */ (v)];
            } else {
                data[k] = /** @type {string} */ (v);
            }
        }
        const azChange = form.getAttribute('az-change') || null;
        _savedForms.set(form.id, { fields: data, azChange });
    });
}

/**
 * Restore form state after reconnection. Sets field values on matching forms
 * and replays az-change events to sync server state.
 */
function restoreFormState() {
    for (const [formId, { fields, azChange }] of _savedForms) {
        const form = document.getElementById(formId);
        if (!form) continue;
        const formEl = /** @type {HTMLFormElement} */ (form);
        for (const el of formEl.elements) {
            if (el instanceof HTMLInputElement) {
                if (!el.name || el.type === 'file') continue;
                if (el.type === 'checkbox') {
                    el.checked = el.name in fields;
                } else if (el.type === 'radio') {
                    el.checked = fields[el.name] === el.value;
                } else if (el.name in fields) {
                    el.value = /** @type {string} */ (fields[el.name]);
                }
            } else if (el instanceof HTMLSelectElement) {
                if (!el.name || !(el.name in fields)) continue;
                if (el.multiple) {
                    const val = fields[el.name];
                    const arr = Array.isArray(val) ? val : [val];
                    for (const opt of el.options) opt.selected = arr.includes(opt.value);
                } else {
                    el.value = /** @type {string} */ (fields[el.name]);
                }
            } else if (el instanceof HTMLTextAreaElement) {
                if (el.name && el.name in fields) {
                    el.value = /** @type {string} */ (fields[el.name]);
                }
            }
        }
        if (azChange && _connected) {
            executeJS(form, null, JSON.parse(azChange));
        }
    }
    _savedForms.clear();
}

/**
 * Delegate a DOM event type on `target` (a Document) via a delegated listener.
 * Bound to the supplied AbortSignal so all delegated listeners can be torn down
 * together. Key filtering is handled by the JS_ON_KEY command inside executeJS,
 * not by attribute name suffixes.
 * @param {Document} target
 * @param {string} eventType
 * @param {AbortSignal} signal
 */
function handleEvent(target, eventType, signal) {
    target.addEventListener(
        eventType,
        (e) => {
            const el = /** @type {Element} */ (e.target).closest(`[az-${eventType}]`);
            if (!el || !_connected) return;
            if (el.hasAttribute('az-prevent-default')) e.preventDefault();
            const raw = el.getAttribute(`az-${eventType}`);
            if (!raw) return;
            executeJS(el, e, withTransitionAttr(el, JSON.parse(raw)));
        },
        { signal },
    );
}

/** The az-* DOM events Arizona delegates per document (main + any PiP window). */
const DELEGATED_EVENTS = ['click', 'change', 'input', 'keydown', 'keyup', 'focusin', 'focusout'];

/**
 * Bind every delegated DOM event on `target` (a Document): the az-* event types
 * plus form submit and drag-and-drop. Called for the main document on connect,
 * and for a PiP document when a view is popped out. Page-level concerns
 * (az-navigate, popstate, scroll) are wired only for the main document.
 * @param {Document} target
 * @param {AbortSignal} signal
 */
function bindDocumentEvents(target, signal) {
    for (const type of DELEGATED_EVENTS) handleEvent(target, type, signal);

    // Form submission: flush any pending debounced/throttled inputs first so
    // the server sees final values, then execute JS commands from az-submit.
    // az-form-reset opts in to clearing the form after submit.
    target.addEventListener(
        'submit',
        (e) => {
            const form = /** @type {Element} */ (e.target).closest('[az-submit]');
            if (!form || !_connected) return;
            e.preventDefault();
            form.querySelectorAll('[az-debounce],[az-throttle]').forEach(flushTimer);
            const raw = form.getAttribute('az-submit');
            const cmds = raw ? JSON.parse(raw) : null;
            if (cmds) executeJS(form, e, withTransitionAttr(form, cmds));
            // A fetch command resets on its own 2xx response (so a validation error
            // keeps the typed fields); everything else resets synchronously here.
            if (!(cmds && commandsIncludeFetch(cmds))) maybeResetForm(form);
        },
        { signal },
    );

    // Drag-and-drop: uses az-key on draggable items and az-drop on the
    // container. dragstart stores the item's key; drop executes the az-drop
    // command with auto-collected {data_transfer, drop_index} payload.
    target.addEventListener(
        'dragstart',
        (e) => {
            const keyEl = /** @type {Element} */ (e.target).closest('[az-key]');
            if (keyEl && e.dataTransfer)
                e.dataTransfer.setData('text/plain', keyEl.getAttribute('az-key') || '');
        },
        { signal },
    );
    target.addEventListener(
        'dragover',
        (e) => {
            if (/** @type {Element} */ (e.target).closest('[az-key]')) e.preventDefault();
        },
        { signal },
    );
    target.addEventListener(
        'drop',
        (e) => {
            const dropTarget = /** @type {Element} */ (e.target).closest('[az-key]');
            if (!dropTarget) return;
            e.preventDefault();
            const container = dropTarget.closest('[az-drop]');
            if (!container || !_connected) return;
            const raw = container.getAttribute('az-drop');
            if (!raw) return;
            executeJS(container, e, withTransitionAttr(container, JSON.parse(raw)));
        },
        { signal },
    );
}

/**
 * Bootstrap: spawn Worker, set up document-level event delegation for all
 * supported event types, and wire up form submission and drag-and-drop.
 *
 * Returns a `disconnect` function that tears down every listener this call
 * registered and terminates the Worker. Idempotent -- calling it twice is
 * a no-op. Useful for tests that spin Arizona up and down repeatedly, and
 * for host apps that need to shut Arizona down on route change.
 *
 * @param {string} endpoint
 * @param {Object<string, unknown>} [params]
 * @returns {() => void} disconnect
 */
function connect(endpoint, params = {}) {
    const controller = new AbortController();
    const signal = controller.signal;
    const prevScrollRestoration = /** @type {any} */ (history).scrollRestoration;

    // Event delegation on the main document: az-* events, form submit, and
    // drag-and-drop. (requestPip binds the same set on a PiP window's document.)
    bindDocumentEvents(document, signal);

    // Take over scroll restoration so the browser doesn't scroll to a stale
    // position before OP_REPLACE swaps in the new content. See the block
    // comment above applyScroll for the full model.
    if ('scrollRestoration' in history) history.scrollRestoration = 'manual';

    // Seed the rendered path/qs so the popstate handler can distinguish a
    // fragment-only change (scroll, no server round-trip) from a real
    // cross-page navigation.
    _currentPath = location.pathname;
    _currentQs = location.search ? location.search.slice(1) : '';

    // On initial load with a URL hash, honor it -- with scrollRestoration set
    // to 'manual', the browser may have skipped or raced its native anchor
    // jump, so we take care of it ourselves.
    if (location.hash) {
        const hash = location.hash.slice(1);
        requestAnimationFrame(() => applyScroll({ kind: 'push', hash }));
    }

    // SPA navigation: az-navigate (replace the view) or az-patch (keep the view,
    // re-render in place) -- both boolean attrs on <a>. The path is read from
    // href (hash stripped before sending). az-navigate sends ["navigate", ...]
    // and the server replies OP_REPLACE; az-patch sends ["patch", ...] and the
    // server replies a diff. Scroll resets to top (or #hash) on new nav; opt out
    // with az-noscroll.
    document.addEventListener(
        'click',
        (e) => {
            const me = /** @type {MouseEvent} */ (e);
            // Let the browser handle modifier-key and non-primary clicks (open in
            // new tab/window, etc.) so the link isn't hijacked.
            if (me.button !== 0 || me.ctrlKey || me.metaKey || me.shiftKey || me.altKey) return;
            const el = /** @type {HTMLAnchorElement} */ (
                /** @type {Element} */ (e.target).closest('[az-navigate], [az-patch]')
            );
            if (!el || !_connected) return;
            const href = el.getAttribute('href');
            if (!href) return;
            const isPatch = el.hasAttribute('az-patch');
            // Anchors implement the URL interface -- use browser-parsed parts.
            const path = el.pathname;
            const qs = el.search ? el.search.slice(1) : '';
            const hash = el.hash ? el.hash.slice(1) : '';
            const noscroll = el.hasAttribute('az-noscroll');

            e.preventDefault();

            // Same-page hash nav: update URL + scroll, no server round-trip.
            if (path === location.pathname && qs === location.search.slice(1)) {
                history.pushState(null, '', href);
                if (!noscroll) applyScroll({ kind: 'push', hash });
                return;
            }

            // az-transition opts this navigation into a view transition. A patch
            // produces a diff (no OP_REPLACE), so its intent is consumed by the
            // first response batch ('any'); a navigate waits for the OP_REPLACE.
            const t = parseTransitionAttr(el);
            _pendingTransition = t ? { types: t.types, kind: isPatch ? 'any' : 'replace' } : null;
            navigateTo(path, qs, hash, {
                noscroll,
                fullUrl: href,
                kind: isPatch ? 'patch' : 'navigate',
            });
        },
        { signal },
    );

    // Browser back/forward: send navigate on popstate so the server
    // renders the correct page for the current URL. Restore the saved
    // scroll position (or #hash target) after REPLACE applies.
    window.addEventListener(
        'popstate',
        (e) => {
            if (!_connected) return;
            const path = location.pathname;
            const qs = location.search ? location.search.slice(1) : '';
            const hash = location.hash ? location.hash.slice(1) : '';
            const saved = e.state?._azScroll || null;
            // Fragment-only change (path + query unchanged): a same-page hash
            // jump fires popstate in some browsers, but it needs no server
            // round-trip -- just scroll. Mirrors the click handler's same-page
            // fast path so in-page anchors don't trigger a full OP_REPLACE.
            if (path === _currentPath && qs === _currentQs) {
                applyScroll({ kind: 'pop', hash, saved });
                return;
            }
            // Replay the same mode the edge was navigated with (tagged `_azNav`),
            // so back/forward over a patch re-patches rather than replacing.
            const navKind = e.state?._azNav === 'patch' ? 'patch' : 'navigate';
            _pendingScroll = { kind: 'pop', hash, saved, patch: navKind === 'patch' };
            // Replay the transition stamped onto this entry when the edge was
            // navigated with a transition, so back/forward animate symmetrically.
            _pendingTransition = e.state?._azTransition || null;
            workerPost(W_SEND, JSON.stringify([navKind, { path, qs }]));
            workerPost(W_UPDATE_PATH, path);
            _currentPath = path;
            _currentQs = qs;
        },
        { signal },
    );

    // Build full WS URL -- Worker can't access location.*
    // Framework keys are `_az_`-prefixed so they can't collide with user params
    // or the page's own query string; user-page qs and connect params ride as
    // regular URL query keys (server reaches them via arizona_req:params/1).
    const protocol = location.protocol === 'https:' ? 'wss:' : 'ws:';
    const pagePath = encodeURIComponent(location.pathname);
    const pageQs = location.search ? location.search.slice(1) : '';
    /** @type {Record<string, string>} */
    const stringParams = {};
    for (const k of Object.keys(params)) stringParams[k] = String(params[k]);
    const extraQs = new URLSearchParams(stringParams).toString();
    const userQs = [pageQs, extraQs].filter(Boolean).join('&');
    const qs = userQs ? `_az_path=${pagePath}&${userQs}` : `_az_path=${pagePath}`;
    // Native-shell capabilities (Electron/Tauri/...) advertised at the WS
    // handshake so the live process can answer ?capability(...). Absent in a
    // plain browser (no `__arizona_os__`), so capsQs is empty and nothing
    // changes. Rides reconnect because the Worker reuses this URL's search.
    const osCaps = osHost()?.capabilities;
    const capsQs = osCaps ? `&_az_caps=${encodeURIComponent(JSON.stringify(osCaps))}` : '';
    const wsUrl = `${protocol}//${location.host}${endpoint}?${qs}${capsQs}`;

    /** @type {Function|null} */
    let _onmessageHook = null;

    // Spawn the Worker, wire its message handler, and open the socket. Extracted
    // so a bfcache restore (`pageshow`) can re-establish the connection that
    // `pagehide` tore down.
    const spawnWorker = () => {
        // Worker is co-located with this script. This static
        // `new Worker(new URL(..., import.meta.url), { type: 'module' })` shape is
        // what bundlers (Vite/Rollup/rolldown) statically detect: Arizona's own
        // build emits the sibling `arizona-worker.min.js` next to
        // `arizona.min.js` and rewrites this reference to it, and a consumer that
        // re-bundles the built client gets the worker auto-emitted,
        // content-hashed, and its URL rewritten (no runtime-string 404).
        _worker = new Worker(new URL('./arizona-worker.js', import.meta.url), { type: 'module' });

        _worker.onmessage = (e) => {
            const msg = e.data;
            switch (msg[0]) {
                case 0: {
                    // [0, ops|null, effects|null, firstAfterReconnect]
                    const apply = () => {
                        if (msg[1]) applyOps(msg[1]);
                        if (msg[2]) applyEffects(msg[2]);
                        if (msg[3]) restoreFormState();
                    };
                    // A pending transition wraps its batch -- ops and effects
                    // together, in order, so the swap and any effect fall inside
                    // one snapshot. 'replace' (navigate) waits for the page-swap
                    // batch, ignoring stray ticks; 'any' (push_event) takes its
                    // first response message and is then consumed either way, so a
                    // no-diff event can't leave the intent dangling onto a later one.
                    const pt = _pendingTransition;
                    const wrap = pt && opsMatchTransition(msg[1], pt.kind);
                    if (pt && (wrap || pt.kind === 'any')) _pendingTransition = null;
                    if (wrap) {
                        runTransition({ types: pt.types }, apply);
                    } else {
                        apply();
                    }
                    if (_onmessageHook) {
                        _onmessageHook({
                            data: JSON.stringify({
                                ...(msg[1] ? { o: msg[1] } : {}),
                                ...(msg[2] ? { e: msg[2] } : {}),
                            }),
                        });
                    }
                    break;
                }
                case 1: {
                    // [1, isReconnect]
                    _connected = true;
                    document.documentElement.classList.add('az-connected');
                    document.documentElement.classList.remove('az-disconnected');
                    if (!msg[1]) {
                        mountHooks(document);
                    }
                    break;
                }
                case 2: {
                    // [2, closeCode]
                    _connected = false;
                    document.documentElement.classList.add('az-disconnected');
                    document.documentElement.classList.remove('az-connected');
                    if (msg[1] === WS_CLOSE_CRASH) {
                        location.reload();
                        return;
                    }
                    if (msg[1] !== WS_CLOSE_NORMAL) saveFormState();
                    break;
                }
            }
        };

        // Send connect message to Worker
        workerPost(W_CONNECT, wsUrl);
    };

    spawnWorker();

    // Let the native shell (if any) inject OS events (window focus/blur, capture
    // state, ...) into the ROOT view's normal event handling. The shell calls
    // cb(name, payload); we relay it as an ordinary pushEvent so the view's
    // handle_event/3 sees it like any other event. Registered ONCE (not per
    // spawnWorker / bfcache restore): the shell's listener is persistent, so
    // re-registering would leak duplicate listeners. A bare callback is the
    // documented contextBridge/Tauri-listen form (an object of functions can't
    // cross contextBridge).
    osHost()?.onEvent?.((name, payload) => pushEvent(name, payload));

    // window._ws proxy for E2E test compatibility
    if (typeof window !== 'undefined') {
        /** @type {any} */ (window)._ws = {
            get readyState() {
                return _connected ? 1 : 3;
            },
            /** @param {string} data */ send(data) {
                workerPost(W_SEND, data);
            },
            /** @param {number} [code] */ close(code) {
                workerPost(W_CLOSE, code || WS_CLOSE_NORMAL);
            },
            set onmessage(fn) {
                _onmessageHook = fn;
            },
            get onmessage() {
                return _onmessageHook;
            },
        };
    }

    // Back/forward cache: a live WebSocket makes the page ineligible for the
    // bfcache, so tear the worker (and its socket) down when the page is hidden,
    // then re-establish it if the page is later restored from the cache. The
    // close must be synchronous -- by the time `pagehide` fires the socket is
    // still open (precisely why the page would otherwise be excluded), and an
    // async `workerPost(W_CLOSE)` may not run before the page is frozen.
    window.addEventListener(
        'pagehide',
        () => {
            if (_worker) {
                _worker.terminate();
                _worker = null;
                _connected = false;
                // Reflect the dropped connection so a bfcache-restored snapshot
                // shows disconnected until `pageshow` reconnects (the worker is
                // killed abruptly, so no [2, closeCode] arrives to do this).
                document.documentElement.classList.remove('az-connected');
                document.documentElement.classList.add('az-disconnected');
            }
        },
        { signal },
    );
    window.addEventListener(
        'pageshow',
        (e) => {
            // Only on a real bfcache restore; a normal load already has a worker.
            if (e.persisted && !_worker) spawnWorker();
        },
        { signal },
    );

    let disconnected = false;
    return function disconnect() {
        if (disconnected) return;
        disconnected = true;
        controller.abort();
        if (_worker) {
            _worker.terminate();
            _worker = null;
        }
        _connected = false;
        _pendingScroll = null;
        _pendingTransition = null;
        _savedForms.clear();
        // Close any floating (PiP) view windows; each window's pagehide handler
        // moves its view back inline and unregisters it.
        for (const d of [..._viewDocs.values()]) {
            try {
                d.defaultView?.close();
            } catch {
                /* window already gone */
            }
        }
        _viewDocs.clear();
        // Run destroyed() on every tracked hook and clear the map. Without
        // this, hook instances leak when host code removes the DOM by means
        // arizona didn't observe (third-party libs, test teardown via
        // `document.body.innerHTML = ''`).
        for (const el of [..._hooks.keys()]) destroyHook(el);
        // Debounce/throttle timers live in a WeakMap keyed by element and
        // aren't iterable. They all guard on `_connected` inside their fun,
        // so after setting `_connected = false` above a late-firing timer
        // won't actually send anything -- leave them to expire naturally.
        document.documentElement.classList.remove('az-connected');
        document.documentElement.classList.remove('az-disconnected');
        if ('scrollRestoration' in history) {
            /** @type {any} */ (history).scrollRestoration = prevScrollRestoration;
        }
        if (typeof window !== 'undefined') {
            delete (/** @type {any} */ (window)._ws);
        }
    };
}

/**
 * Copy <style> and <link rel="stylesheet"> from `src` into `dst`'s <head> so a
 * Picture-in-Picture document (which starts empty) renders with the page styles.
 * @param {Document} src
 * @param {Document} dst
 */
function copyStyles(src, dst) {
    for (const sheet of src.styleSheets) {
        try {
            const css = Array.from(sheet.cssRules, (r) => r.cssText).join('');
            const style = dst.createElement('style');
            style.textContent = css;
            dst.head.appendChild(style);
        } catch {
            // Cross-origin sheet: rules aren't readable -- re-link it by href.
            const href = /** @type {any} */ (sheet).href;
            if (!href) continue;
            const link = dst.createElement('link');
            link.rel = 'stylesheet';
            link.href = href;
            dst.head.appendChild(link);
        }
    }
}

/**
 * Move a view's root element into a floating Document Picture-in-Picture window,
 * keeping it live: server diffs keep patching it (resolved in the PiP document)
 * and az-* / form / drag events fired inside it are delegated there too. Page
 * styles are copied in. Must be called from a user gesture. Resolves to the PiP
 * `Window`, or `null` if the browser lacks Document PiP, the view isn't found,
 * or it's already popped out.
 * @param {string} viewId
 * @param {{width?: number, height?: number, onClose?: () => void}} [opts]
 * @returns {Promise<Window|null>}
 */
async function requestPip(viewId, opts = {}) {
    const pipApi = /** @type {any} */ (window).documentPictureInPicture;
    if (!pipApi) return null;
    if (_viewDocs.has(viewId)) return null;
    const view = document.getElementById(viewId);
    if (!view) return null;

    // Remember where the view sat so it can be restored in place on close.
    const placeholder = document.createComment(`az-pip:${viewId}`);
    view.before(placeholder);

    // Forward the caller's options straight to the browser (no framework defaults);
    // requestWindow ignores dictionary members it doesn't know (e.g. onClose).
    const pip = await pipApi.requestWindow(opts);

    copyStyles(document, pip.document);
    pip.document.body.append(view);
    _viewDocs.set(viewId, pip.document);

    // Delegate events fired inside the floating window; torn down on close.
    const controller = new AbortController();
    bindDocumentEvents(pip.document, controller.signal);

    pip.addEventListener(
        'pagehide',
        () => {
            controller.abort();
            _viewDocs.delete(viewId);
            if (placeholder.parentNode) placeholder.replaceWith(view);
            else document.body.append(view);
            if (opts.onClose) opts.onClose();
        },
        { once: true },
    );

    return pip;
}

/**
 * Close a view's floating window (if open); its pagehide handler moves the view
 * back inline. No-op when the view isn't popped out.
 * @param {string} viewId
 */
function exitPip(viewId) {
    const doc = _viewDocs.get(viewId);
    doc?.defaultView?.close();
}

export {
    applyEffects,
    applyOps,
    connect,
    executeJS,
    exitPip,
    get,
    hooks,
    mountHooks,
    OP,
    pushEvent,
    pushEventTo,
    requestPip,
    resolveEl,
    restoreFormState,
    saveFormState,
    set,
    setAll,
};
