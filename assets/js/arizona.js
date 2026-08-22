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
 *   [0, wsUrl, isReconnect] -- connect
 *   [1, jsonString]         -- send data
 *   [2, code]               -- close WS
 */

/**
 * Op codes -- each op is a flat array: [opcode, "viewId:az", ...args].
 * Codes match the server-side OP_* constants in arizona.hrl, 3 included: it is
 * unassigned there too (the removed innerHTML op), so nothing emits it and no
 * handler here claims it. Reusing it for a new op means changing both sides
 * together.
 * @enum {number}
 * @property {number} TEXT - [0, target, value, isHtml]
 * @property {number} SET_ATTR - [1, target, attr, value]
 * @property {number} REM_ATTR - [2, target, attr]
 * @property {number} REMOVE_NODE - [4, target]
 * @property {number} INSERT - [5, target, key, pos, html]
 * @property {number} REMOVE - [6, target, key]
 * @property {number} ITEM_PATCH - [7, target, key, innerOps]
 * @property {number} REPLACE - [8, target, html] -- outerHTML (navigate)
 * @property {number} MOVE - [9, target, key, afterKey] -- move keyed child
 * @property {number} LIST_PATCH - [10, target, subOps] -- positional list patch
 */
const OP = {
    TEXT: 0,
    SET_ATTR: 1,
    REM_ATTR: 2,
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

/**
 * Crash-reload loop guard: a WS_CLOSE_CRASH triggers a full-page reload, so a view
 * that crashes deterministically on mount would reload forever. Allow at most
 * CRASH_RELOAD_MAX reloads inside CRASH_RELOAD_WINDOW_MS (tracked in sessionStorage,
 * reset on a clean connect) before giving up.
 */
const CRASH_RELOAD_MAX = 3;
const CRASH_RELOAD_WINDOW_MS = 10000;
const CRASH_RELOAD_KEY = 'arizona:crash-reloads';

/**
 * Reload after a server-side crash close, unless we have already reloaded
 * CRASH_RELOAD_MAX times within the window (a deterministic crash loop): then log
 * an error and stay put with the `az-disconnected` state already set by the caller.
 */
function crashReload() {
    /** @type {{count?: number, first?: number}} */
    let rec = {};
    try {
        const raw = sessionStorage.getItem(CRASH_RELOAD_KEY);
        if (raw) rec = JSON.parse(raw);
    } catch {
        /* no/invalid record -- treat as a fresh window */
    }
    const now = Date.now();
    const count = rec.first && now - rec.first < CRASH_RELOAD_WINDOW_MS ? rec.count || 0 : 0;
    if (count >= CRASH_RELOAD_MAX) {
        console.error(
            `[arizona] WebSocket closed with a crash ${count} times within ` +
                `${CRASH_RELOAD_WINDOW_MS}ms; not reloading again -- the view likely ` +
                `crashes deterministically on mount server-side.`,
        );
        return;
    }
    try {
        sessionStorage.setItem(
            CRASH_RELOAD_KEY,
            JSON.stringify({ count: count + 1, first: count === 0 ? now : rec.first }),
        );
    } catch {
        /* sessionStorage unavailable -- reload anyway, just without the guard */
    }
    location.reload();
}

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
//                 OP_REPLACE, or OP_TEXT with new content)
//   updated()   -- element stayed in the DOM but its attributes or inner
//                 content changed (OP_SET_ATTR, OP_REM_ATTR, OP_TEXT)
//   destroyed() -- element is about to be removed (OP_REMOVE_NODE, OP_REMOVE,
//                 OP_REPLACE, OP_TEXT). Called before the DOM mutation --
//                 this.el is still attached
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
// Extra query params passed to `connect(endpoint, params)`. Unlike the page's
// own query string they are connection-level config, constant across SPA
// navigations, so they are re-merged into every reconnect URL (the page qs is
// replaced by the navigated-to qs; these ride along).
/** @type {string} */
let _connectQs = '';

/** @type {Map<string, {fields: Object<string, string|string[]>, azChange: string|null}>} */
const _savedForms = new Map();

// Teardown for the connection currently owning the module state, so a second
// `connect()` can retire the first instead of orphaning its Worker. Cleared by
// the teardown itself.
/** @type {(() => void)|null} */
let _teardown = null;

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

// The PiP window and inline placeholder per popped-out view, so a navigate's
// OP_REPLACE -- which destroys the placeholder with the outgoing subtree -- can
// close the now-orphaned window instead of leaving it floating over dead server
// state. Entries live and die with `_viewDocs` (set in requestPip, removed by
// the window's pagehide handler).
/** @type {Map<string, {win: Window, placeholder: Comment}>} */
const _pipWindows = new Map();

/**
 * Close any popped-out (PiP) window whose inline placeholder is no longer in
 * the DOM -- its home was destroyed (a navigate OP_REPLACE swapped the page),
 * so the floating view shows dead server state. Closing fires the window's
 * pagehide handler, which discards the stale view and unregisters the maps.
 * Same reconciliation `disconnect` performs, driven here by the page swap.
 */
function closeOrphanedPipWindows() {
    for (const pip of [..._pipWindows.values()]) {
        if (!pip.placeholder.isConnected) {
            try {
                pip.win.close();
            } catch {
                /* window already gone */
            }
        }
    }
}

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
 * after the first non-empty diff batch (it has no OP_REPLACE), and is dropped
 * at the end of the first worker frame after the patch either way (see the
 * worker message handler), so a no-op patch can't leave it armed indefinitely.
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
 * is only a hint. A pending view transition is stamped (`_azTransition`) in the
 * SAME write, so back/forward across the edge replays it -- one replaceState per
 * outgoing entry, not one per concern (Safari rate-limits history writes to
 * ~100/30s, then throws SecurityError).
 * @param {string} [navKind]
 */
function saveCurrentScroll(navKind) {
    const st = history.state || {};
    const next = { ...st, _azScroll: { x: window.scrollX, y: window.scrollY } };
    if (navKind === 'patch') next._azNav = 'patch';
    if (_pendingTransition) next._azTransition = _pendingTransition;
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

// The user-facing query string carried to the worker for the next reconnect
// after an SPA navigation: the navigated-to page qs plus the connection-level
// `connect()` extras (constant across navigations). The worker replaces the
// previous page's user params with this, preserving the framework `_az_*` keys.
/**
 * @param {string} qs navigated-to page query string (no leading `?`)
 * @returns {string}
 */
function reconnectUserQs(qs) {
    return [qs, _connectQs].filter(Boolean).join('&');
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
        // One replaceState: saveCurrentScroll stamps scroll, patch tag, AND any
        // pending transition onto the outgoing entry together.
        saveCurrentScroll(kind);
        history.pushState(state, '', fullUrl);
        if (!opts.noscroll) _pendingScroll = { kind: 'push', hash, patch: kind === 'patch' };
    }
    workerPost(W_SEND, JSON.stringify([kind, { path, qs }]));
    workerPost(W_UPDATE_PATH, { path, qs: reconnectUserQs(qs) });
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
    if (def.mounted) runHookCallback(instance, def.mounted, 'mounted');
}

/**
 * Invoke a hook lifecycle callback in isolation: a throwing user hook is logged
 * and swallowed so it can't abort the op batch it runs inside (the server
 * snapshot has already advanced, so a bubbling throw would desync the DOM).
 * @param {{__name: string} & Object<string, *>} instance
 * @param {Function} cb
 * @param {string} phase
 */
function runHookCallback(instance, cb, phase) {
    try {
        cb.call(instance);
    } catch (err) {
        // Static format string: a hook name / op code goes in an ARGUMENT, never in
        // the format position, where a `%s` in it would consume the `err` that
        // follows (and `%c` would restyle the line) -- see native_client.js.
        console.error('[arizona] hook %s %s() threw', instance.__name, phase, err);
    }
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
    if (def?.[phase]) runHookCallback(instance, def[phase], phase);
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
 * Used by ops that replace inner content but keep the element (TEXT).
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
 * A framework slot-opening comment: `az:` followed by an az of the shape the
 * compiler emits.
 *
 * Every framework-emitted az is `<Fp>-<id>` -- a base-36 `phash2` fingerprint
 * (upper-case alphanumerics) and a numeric id, repeated for each nesting level
 * and optionally suffixed `:<slot>` for a second content slot on one element.
 * The fingerprint is the anchor that separates a real marker from user-authored
 * bytes, exactly as `arizona_html:scope_static/3` states for the server side:
 * static text is spliced verbatim and `?raw` splices trusted stored HTML, so a
 * comment written by a CMS or markdown pipeline reaches slot content as ordinary
 * bytes. Matching bare `az:` would let such a decoy pose as a nested opener and
 * make the walker below swallow the slot's own closer.
 */
const MARKER_OPEN = /^az:[0-9A-Z]+-\d+(?:-[0-9A-Z]+-\d+)*(?::\d+)?$/;

/**
 * The `<!--/az-->` closing the slot `startMarker` opens, or null when the slot is
 * not terminated inside its parent.
 *
 * Marker pairs nest -- a template whose whole body is a bare dynamic anchors its
 * root slot with its own pair inside the enclosing slot's pair -- so this tracks
 * depth rather than stopping at the first closer, incrementing on a nested
 * framework opener and decrementing on each closer.
 * @param {Comment} startMarker
 * @returns {Comment|null}
 */
function findSlotEnd(startMarker) {
    let depth = 0;
    for (let node = startMarker.nextSibling; node; node = node.nextSibling) {
        if (node.nodeType !== 8) continue;
        const data = /** @type {Comment} */ (node).data;
        if (data === '/az') {
            if (depth === 0) return /** @type {Comment} */ (node);
            depth--;
        } else if (MARKER_OPEN.test(data)) {
            depth++;
        }
    }
    return null;
}

/**
 * Walk every sibling node inside the slot `startMarker` opens, applying `fn`, and
 * return the slot's closing marker. Every walker over a slot's contents goes
 * through here, so nesting is handled in one place.
 *
 * The delimiter is found FIRST and nothing is touched when it is missing: `fn`
 * typically removes the node it is handed, so a walk that mutated as it searched
 * would empty the rest of the parent whenever the slot could not be delimited --
 * turning a mis-parse into data loss. Callers get null and skip instead.
 *
 * `fn` may remove the node it is handed -- the next sibling is read first.
 * @param {Comment} startMarker
 * @param {(node: ChildNode) => void} fn
 * @returns {Comment|null} the matching closing marker, or null if unterminated
 */
function forEachNodeInSlot(startMarker, fn) {
    const end = findSlotEnd(startMarker);
    if (!end) return null;
    // `end` is a following sibling of `startMarker`, so the walk always reaches
    // it; the null check is the type-level stop.
    let node = startMarker.nextSibling;
    while (node && node !== end) {
        const next = node.nextSibling;
        fn(node);
        node = next;
    }
    return end;
}

/**
 * Walk elements inside the slot `startMarker` opens (nesting-aware, see
 * `forEachNodeInSlot`), applying `fn` to each Element-typed node.
 * @param {Comment} startMarker
 * @param {(el: Element) => void} fn
 */
function forEachElementBetweenMarkers(startMarker, fn) {
    forEachNodeInSlot(startMarker, (node) => {
        if (node.nodeType === 1) fn(/** @type {Element} */ (node));
    });
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
        if (!updateMarkerContent(marker, val, isHtml)) {
            // Undelimited slot (stored HTML carrying a marker-shaped comment with
            // no closer). Nothing was touched -- say so rather than write the
            // value somewhere arbitrary.
            console.warn(`[arizona] slot az:${az} has no closing marker; skipping`);
            return;
        }
        forEachElementBetweenMarkers(marker, mountHooks);
    } else {
        destroyChildHooks(el);
        if (isHtml) {
            el.innerHTML = val;
            mountHooks(el);
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
 * Apply a SET_ATTR op: setAttribute and sync the live DOM property for form
 * controls. Once the user has interacted, the browser's dirty value/checkedness
 * flags make attribute writes stop affecting `.value`/`.checked`/`.selected`,
 * so without the property write a server-driven change would silently stop
 * rendering. The server is authoritative; a SET_ATTR of a boolean attribute
 * always means true (false diffs to REM_ATTR).
 * @param {Element} el
 * @param {string} name
 * @param {string} val
 */
function applySetAttrOp(el, name, val) {
    el.setAttribute(name, val);
    if (name === 'value' && 'value' in el) el.value = val;
    else if (name === 'checked' && 'checked' in el) el.checked = true;
    else if (name === 'selected' && 'selected' in el) el.selected = true;
    notifyUpdated(el);
}

/**
 * Apply a REM_ATTR op: removeAttribute and run the hook `updated` phase. The
 * canonical attribute-removal write shared by diff ops, item patches, and the
 * `arizona_js` attribute effects, so a removal behaves the same whatever drove it.
 * Mirrors applySetAttrOp's form-control property sync: a boolean removal means
 * false, and removing `value` clears the default value, so the live value resets
 * to that empty default (the dirty flags would otherwise keep the stale state).
 * @param {Element} el
 * @param {string} name
 */
function applyRemAttrOp(el, name) {
    el.removeAttribute(name);
    if (name === 'value' && 'value' in el) el.value = '';
    else if (name === 'checked' && 'checked' in el) el.checked = false;
    else if (name === 'selected' && 'selected' in el) el.selected = false;
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
 *
 * Two per-batch resolution caches keep a K-op batch from re-scanning the view
 * subtree per op:
 * - `els` memoizes target -> resolution. Every hit is verified live
 *   (`isConnected`) before use: an op that replaced/re-rendered a subtree
 *   (REPLACE, TEXT) leaves the old elements disconnected, so a stale entry
 *   re-resolves itself -- no per-op invalidation bookkeeping. (A
 *   connected-but-wrong hit would need a duplicate az within one view, which the
 *   compiler prevents.) Nulls are not cached, so an element created mid-batch is
 *   found.
 * - `streams` maps a stream container to a key -> item map of its direct keyed
 *   children, built on first keyed lookup and maintained by the batch's
 *   INSERT/REMOVE ops -- an N-op reorder is O(N), not O(N^2) scans (mirrors
 *   applyListPatch's one-snapshot-per-batch shape).
 * @param {Array<Array<*>>} ops
 */
function applyOps(ops) {
    let didReplace = false;
    /** @type {Map<string, Resolution>} */
    const els = new Map();
    /** @type {Map<Element, Map<string, Element>>} */
    const streams = new Map();
    /** @param {string} target @returns {Resolution|null} */
    const resolve = (target) => {
        const hit = els.get(target);
        // `isConnected` proves the resolved ELEMENT survived, which for a
        // marker-only hit is only the slot's parent -- an earlier op in this very
        // batch can re-render the enclosing slot and destroy the inner marker
        // while leaving that parent connected. The stale hit would then reach a
        // marker-aware op, find no marker, and fall through to its whole-element
        // fallback (`textContent`), wiping the parent -- on `OP_TEXT`, the op code
        // `MARKER_UNSAFE_OPS` deliberately does not cover. So re-check the marker
        // itself; a failed check falls through to a fresh resolve, which finds
        // nothing and warns exactly as an unresolvable target does.
        if (hit?.el.isConnected && (!hit.marker || findMarker(hit.el, azOf(target)))) return hit;
        const found = resolveOpTarget(target);
        if (found) els.set(target, found);
        return found;
    };
    for (const op of ops) {
        // Isolate each op: a bad selector or a throwing hook must not abort the
        // rest of the batch, or the DOM desyncs from the already-advanced server
        // snapshot until a reload.
        try {
            const found = resolve(op[1]);
            if (!found) {
                // Loud like the stream-item warns: a silently dropped op (a
                // server op addressed to a slot SSR never anchored) reads as
                // "nothing happened" and costs a debugging round trip.
                console.warn(`[arizona] op ${op[0]} target "${op[1]}" not found; skipping`);
                continue;
            }
            if (found.marker && MARKER_UNSAFE_OPS.has(op[0])) {
                // A marker-only hit is the slot's PARENT element, not the slot:
                // it exists to give the marker-aware ops something to scan. An op
                // that rewrites the element or places a node by container position
                // would destroy or misplace content outside the slot -- see
                // MARKER_UNSAFE_OPS. Refuse it as an unresolved target is refused.
                console.warn(
                    `[arizona] op ${op[0]} target "${op[1]}" resolves only to a slot marker; ` +
                        `refusing an op that acts on the enclosing element`,
                );
                continue;
            }
            const el = found.el;
            const az = azOf(op[1]);
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
                    // A popped-out (PiP) view whose placeholder just went with
                    // the outgoing subtree is orphaned -- close its window.
                    closeOrphanedPipWindows();
                    didReplace = true;
                    break;
                }
                case OP.REMOVE_NODE:
                    removeEl(el);
                    break;
                case OP.INSERT:
                    insertItemEl(el, op[2], op[3], op[4], streams, az);
                    break;
                case OP.REMOVE:
                    removeItemEl(el, op[2], streams);
                    break;
                case OP.ITEM_PATCH:
                    applyItemPatch(el, op[2], op[3], streams);
                    break;
                case OP.MOVE:
                    moveItemEl(el, op[2], op[3], streams, az);
                    break;
                case OP.LIST_PATCH:
                    applyListPatch(el, az, op[2]);
                    break;
                default:
                    // Silence here would let a version-skewed or retired op (code 3,
                    // for one) diverge the DOM from server state with no symptom but
                    // "it didn't update". Mirrors applyItemOps, which already warns.
                    console.warn(`[arizona] op ${op[0]} not recognized; skipping`);
            }
        } catch (err) {
            console.error('[arizona] op %s failed; skipping', op[0], err);
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
 * A resolved patch target. `marker` is true when NO element carries the az and
 * `el` is only the element the slot's comment marker hangs under -- see
 * `resolveOpTarget`. Callers must not treat such an `el` as the target itself.
 * @typedef {{el: Element, marker: boolean}} Resolution
 */

/**
 * Ops that act on the element they resolve to as a whole, so they cannot be
 * applied to a marker-only resolution -- whose element is the slot's PARENT, not
 * the slot. Two kinds, both refused:
 *
 * - DESTRUCTIVE (`REPLACE`, `REMOVE_NODE`): `replaceWith` / `remove` on the
 *   parent takes the slot's static siblings with it, and when the parent is the
 *   live root, the whole view. The server never addresses these to a
 *   marker-anchored slot (the diff emits the marker-aware `?OP_TEXT`), so this
 *   is a guard against a stray op, not a supported path.
 * - CONTAINER-RELATIVE PLACEMENT (`INSERT`, `MOVE`): a tail insert appends to the
 *   parent and a null-`afterKey` move prepends to it, both landing OUTSIDE the
 *   slot's marker span (after the footer, before the header). Silent misplacement
 *   is worse than the drop-and-warn these got before the marker fallback existed,
 *   so they warn too. Making stream items marker-relative is the tracked
 *   follow-up (see docs/architecture.md); until then this refuses rather than
 *   corrupts. The position-INDEPENDENT item ops (`REMOVE`, `ITEM_PATCH`) find
 *   their target by `az-key` and stay correct, so they are NOT refused -- they
 *   are a strict gain over dropping every stream op at this shape.
 * @type {Set<number>}
 */
const MARKER_UNSAFE_OPS = new Set([OP.REPLACE, OP.REMOVE_NODE, OP.INSERT, OP.MOVE]);

/**
 * The az half of a `"viewId:az"` patch target (the whole string when it carries
 * no view scope, which never names a slot).
 * @param {string} target
 * @returns {string}
 */
function azOf(target) {
    return target.substring(target.indexOf(':') + 1);
}

/**
 * Resolve a patch target to a DOM element. Bare targets (no colon) resolve to
 * the view root element itself -- used by OP_REPLACE for navigation. Scoped
 * targets ("viewId:az") find the view root, then the element within it.
 *
 * Three lookups, cheapest and most common first:
 * 1. an element carrying the az (the view root itself, or a descendant);
 * 2. for a compound "X:n" slot az, the base element `[az="X"]`;
 * 3. the slot's `<!--az:X-->` comment marker, whose PARENT element is returned
 *    -- flagged `marker: true`, because that element is NOT the target.
 *
 * (3) is what makes a MARKER-ONLY slot patchable: a template whose whole body is
 * a bare dynamic -- `?html(case ... end)`, `?html(?get(x))`, `?html(?each(...))`,
 * `?html(?stateless(...))`, or a mixed top-level fragment -- anchors its root slot
 * with its own marker pair and NO element ever carries that az. The returned
 * parent is exactly the element the marker is a direct child of, so the
 * `findMarker(el, az)` every marker-aware op then runs finds it. Without it the
 * op resolves to nothing and `applyOps` drops it, so such a component never
 * updates after SSR.
 *
 * Arm 2 does not subsume arm 3 even for a compound az: `querySelector` searches
 * DESCENDANTS, so when the base az belongs to the view root itself (a stream
 * `?each` among static siblings under the root) it finds nothing. That is exactly
 * the case where the marker's parent IS the root, which is why `applyOps` refuses
 * the destructive ops on a marker hit.
 * @param {string} target
 * @returns {Resolution|null}
 */
function resolveOpTarget(target) {
    const i = target.indexOf(':');
    if (i === -1) {
        const root = findViewRoot(target);
        return root && { el: root, marker: false };
    }
    const viewId = target.substring(0, i);
    const az = target.substring(i + 1);
    const view = findViewRoot(viewId);
    if (!view) return null;
    if (view.getAttribute('az') === az) return { el: view, marker: false };
    const el = view.querySelector(`[az="${az}"]`);
    if (el) return { el, marker: false };
    const j = az.indexOf(':');
    if (j !== -1) {
        const base = view.querySelector(`[az="${az.substring(0, j)}"]`);
        if (base) return { el: base, marker: false };
    }
    const parent = findMarkerDeep(view, az)?.parentElement;
    return parent ? { el: parent, marker: true } : null;
}

/**
 * The element a patch target resolves to, dropping the provenance flag. The
 * exported form; `applyOps` uses `resolveOpTarget` so it can refuse a destructive
 * op on a marker-only hit.
 * @param {string} target
 * @returns {Element|null}
 */
function resolveEl(target) {
    return resolveOpTarget(target)?.el ?? null;
}

/**
 * Find the `<!--az:X-->` opening marker anywhere in `root`'s subtree. The
 * fallback arm of `resolveEl` -- element lookups run first, so this walk only
 * happens for a slot no element anchors, and `applyOps`' per-batch `els` memo
 * keeps it to once per target per batch. Same view-wide scope as the
 * `[az="..."]` query it backs up.
 * @param {Element} root
 * @param {string} az
 * @returns {Comment|null}
 */
function findMarkerDeep(root, az) {
    const data = `az:${az}`;
    const walker = root.ownerDocument.createTreeWalker(root, NodeFilter.SHOW_COMMENT);
    for (let node = walker.nextNode(); node; node = walker.nextNode()) {
        if (/** @type {Comment} */ (node).data === data) return /** @type {Comment} */ (node);
    }
    return null;
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
 * Returns false (touching nothing) when the slot has no closing marker.
 * @param {Comment} startMarker
 * @param {string} value
 * @param {boolean} [isHtml]
 * @returns {boolean}
 */
function updateMarkerContent(startMarker, value, isHtml) {
    const doc = startMarker.ownerDocument;
    // Scalar fast path: a lone text node is updated in place (no childList churn,
    // which would revert an in-progress scroll on WebKitGTK -- see updateLoneTextNode).
    if (!isHtml && updateLoneTextNode(startMarker, value)) return true;
    if (!forEachNodeInSlot(startMarker, (node) => node.remove())) return false;
    // Insert new content before the closing marker
    if (isHtml) {
        const tpl = doc.createElement('template');
        tpl.innerHTML = value;
        startMarker.after(tpl.content);
    } else {
        startMarker.after(doc.createTextNode(value));
    }
    return true;
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
 * client-set value containing `<` can't inject markup. Returns false (touching
 * nothing) when the slot has no closing marker.
 * @param {Comment} startMarker
 * @param {*} value
 * @returns {boolean}
 */
function setMarkerText(startMarker, value) {
    const str = value == null ? '' : String(value);
    // Same in-place fast path as updateMarkerContent: avoid childList churn that
    // reverts an in-progress scroll on WebKitGTK (see updateLoneTextNode).
    if (updateLoneTextNode(startMarker, str)) return true;
    if (!forEachNodeInSlot(startMarker, (node) => node.remove())) return false;
    startMarker.after(startMarker.ownerDocument.createTextNode(str));
    return true;
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
        // Both writes below drop whatever the slot held, so they run the same
        // teardown applyTextOp does -- `destroyHooks` over the slot span, or
        // `destroyChildHooks` when there is no marker to delimit and the write
        // takes the element's whole content. A `?local` slot's SSR initial is a
        // scalar and `set` only ever writes a text node, so the framework itself
        // never puts an element in reach; a hook that renders into the element
        // can, and it would otherwise be detached with its instance left in
        // `_hooks` and `destroyed()` never called.
        const marker = findMarker(el, localMarkerAz(el, target[1]));
        if (marker) {
            forEachElementBetweenMarkers(marker, destroyHooks);
            setMarkerText(marker, value);
        } else {
            destroyChildHooks(el);
            el.textContent = value == null ? '' : String(value);
        }
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
        // Comment nodes are skipped: a nested slot's own markers are structure,
        // never part of the value the browser owns.
        forEachNodeInSlot(marker, (node) => {
            if (node.nodeType !== 8) text += node.textContent ?? '';
        });
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
 * A parsed `az-local` descriptor: content slots, attribute bindings, and
 * interpolated-attribute affixes.
 * @typedef {{c?: Object<string, string>, a?: Object<string, string>, ap?: Object<string, [string, string]>}} LocalDesc
 */

// Parsed `az-local` descriptors, cached per element. NOT a persistent index --
// discovery still queries the live DOM every call; only the JSON.parse of each
// element's descriptor is memoized. The attribute is written once at SSR and a
// ?local slot is never diffed, so a descriptor is immutable for its element's
// lifetime; a re-rendered slot is a NEW element (a fresh cache key) and dropped
// elements fall out of the WeakMap with GC.
/** @type {WeakMap<Element, LocalDesc>} */
const _localDescs = new WeakMap();

/**
 * Visit each bound slot for `key` under `root`. When `viewId` is non-null only
 * slots whose nearest view is `viewId` are visited (per-view isolation). The
 * callback may return `true` to stop the scan (first-match reads); the return
 * value says whether it did.
 * @param {Element|Document} root
 * @param {string} key
 * @param {string|null} viewId
 * @param {(el: Element, target: string[]) => boolean|void} fn
 * @returns {boolean}
 */
function forEachLocal(root, key, viewId, fn) {
    /** @param {Element} el @returns {boolean} */
    const visit = (el) => {
        let parsed = _localDescs.get(el);
        if (parsed === undefined) {
            const desc = el.getAttribute('az-local');
            if (!desc) return false;
            // The descriptor is always framework-generated valid JSON.
            parsed = /** @type {LocalDesc} */ (JSON.parse(desc));
            _localDescs.set(el, parsed);
        }
        if (viewId !== null && resolveTarget(el) !== viewId) return false;
        // c maps each content slot index -> key; a maps each attr name -> key;
        // ap (optional) carries [prefix, suffix] for interpolated attributes.
        if (parsed.c) {
            for (const [slot, k] of Object.entries(parsed.c)) {
                if (k === key && fn(el, ['content', slot]) === true) return true;
            }
        }
        if (parsed.a) {
            for (const [attr, k] of Object.entries(parsed.a)) {
                if (k === key) {
                    const aff = parsed.ap?.[attr];
                    const t = aff ? ['attr', attr, aff[0], aff[1]] : ['attr', attr];
                    if (fn(el, t) === true) return true;
                }
            }
        }
        return false;
    };
    // `nodeType`, not `instanceof Element`: a popped-out (Document PiP) root
    // lives in another realm with its own Element constructor, so `instanceof`
    // is false cross-realm and would skip the root's own slot (a Document is
    // nodeType 9, so it still skips the self-check). Mirrors `mountHooks`.
    if (
        root.nodeType === 1 &&
        /** @type {Element} */ (root).hasAttribute('az-local') &&
        visit(/** @type {Element} */ (root))
    ) {
        return true;
    }
    for (const el of root.querySelectorAll('[az-local]')) {
        if (visit(el)) return true;
    }
    return false;
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
    // First match wins -- the callback returns true so the scan stops there
    // instead of visiting (and parsing) every remaining slot.
    /** @param {Element|Document} root */
    const scan = (root) =>
        forEachLocal(root, key, viewId, (el, target) => {
            result = readLocalValue(el, target);
            return true;
        });
    if (viewId) {
        const root = findViewRoot(viewId);
        if (root) scan(root);
    } else {
        for (const doc of allDocs()) {
            if (scan(doc)) break;
        }
    }
    return result;
}

/**
 * Build a key -> element map of a container's direct keyed (az-key) children,
 * in DOM order. Under a duplicate key the FIRST child wins, matching what a
 * `:scope > [az-key="..."]` query would return.
 * @param {Element} el
 * @returns {Map<string, Element>}
 */
/**
 * Boundary of a stream container's slot when the container shares its parent's
 * content slot with siblings. There the each's slot az is compound and carried by no
 * element, so `resolveEl` falls back to the BASE az -- the enclosing element -- and
 * `el` is that enclosing element, not the list. Appending or prepending to it puts the
 * item outside the slot span (after a footer, before a header) instead of in the list.
 *
 * Lookup by key needs none of this: the items are real children of `el` either way.
 * Only PLACEMENT does, so this is used solely to anchor inserts and moves.
 *
 * Returns null for the ordinary case (a container that owns its az), which keeps the
 * plain element placement.
 * @param {Element} el
 * @param {string} [az]
 * @returns {{start: Comment, end: Comment}|null}
 */
function slotBounds(el, az) {
    const start = az ? findMarker(el, az) : null;
    if (!start) return null;
    const end = findSlotEnd(start);
    return end ? { start, end } : null;
}

/**
 * Keyed children of a stream container, restricted to its slot span when it has one.
 * @param {Element} el
 * @param {{start: Comment, end: Comment}|null} bounds
 * @returns {Element[]}
 */
function keyedChildren(el, bounds) {
    if (!bounds) return Array.from(el.querySelectorAll(':scope > [az-key]'));
    const out = [];
    for (let n = bounds.start.nextSibling; n && n !== bounds.end; n = n.nextSibling) {
        if (n.nodeType === 1 && /** @type {Element} */ (n).getAttribute('az-key') !== null) {
            out.push(/** @type {Element} */ (n));
        }
    }
    return out;
}

/**
 * Build a key -> element map of a container's direct keyed (az-key) children,
 * in DOM order. Under a duplicate key the FIRST child wins, matching what a
 * `:scope > [az-key="..."]` query would return.
 * @param {Element} el
 * @returns {Map<string, Element>}
 */
function buildKeyMap(el) {
    const map = new Map();
    for (const child of el.children) {
        const k = child.getAttribute('az-key');
        if (k !== null && !map.has(k)) map.set(k, child);
    }
    return map;
}

/**
 * Look up a stream container's direct keyed child. With a per-batch `streams`
 * cache (top-level applyOps) the container's children are scanned once and the
 * map is maintained by the batch's inserts/removes, so an N-op stream batch is
 * O(N) instead of one full child scan per op. Without one (nested item ops) it
 * falls back to a direct query. A cached entry that went stale -- a non-stream
 * op (TEXT) rewrote the container's children under the map -- triggers one
 * rebuild and retry, so the cache can never return a disconnected element.
 * @param {Map<Element, Map<string, Element>>|null} streams
 * @param {Element} el
 * @param {string} key
 * @returns {Element|null}
 */
function itemByKey(streams, el, key) {
    if (!streams) return el.querySelector(`:scope > [az-key="${CSS.escape(key)}"]`);
    let map = streams.get(el);
    if (!map) {
        map = buildKeyMap(el);
        streams.set(el, map);
    }
    let item = map.get(key);
    if (item === undefined || !item.isConnected) {
        map = buildKeyMap(el);
        streams.set(el, map);
        item = map.get(key);
    }
    return item ?? null;
}

/**
 * Insert a keyed child into a container element.
 * @param {Element} el -- container element
 * @param {string} key
 * @param {number} pos -- -1 means append, otherwise insert before child at index
 * @param {string} html
 * @param {Map<Element, Map<string, Element>>|null} [streams] -- per-batch key maps
 * @param {string} [az] -- the op's slot az, used to anchor placement to the slot span
 */
function insertItemEl(el, key, pos, html, streams = null, az) {
    const tpl = el.ownerDocument.createElement('template');
    tpl.innerHTML = html;
    const bounds = slotBounds(el, az);
    // Grab the keyed item from the payload BEFORE inserting it: re-querying the
    // container by key afterwards would find a PRE-EXISTING element first under
    // a duplicate key, mounting hooks on the wrong element and skipping the new
    // item's own.
    const item = Array.from(tpl.content.children).find((e) => e.getAttribute('az-key') === key);
    // Tail placement goes before the slot's CLOSING marker when there is one, so the
    // item lands inside the list rather than after the slot's static siblings.
    const atEnd = () =>
        bounds ? el.insertBefore(tpl.content, bounds.end) : el.appendChild(tpl.content);
    if (pos === -1) {
        atEnd();
    } else {
        // Positional: the live child list, not the key map -- MOVE ops change
        // DOM order without touching the map, so only the DOM knows position.
        const children = keyedChildren(el, bounds);
        if (pos < children.length) {
            el.insertBefore(tpl.content, children[pos]);
        } else {
            atEnd();
        }
    }
    if (item) {
        streams?.get(el)?.set(key, item);
        mountHooks(item);
    } else {
        console.warn(`[arizona] stream item missing az-key="${key}" after insert`);
    }
    // The container's child list changed, so a hook on it is notified -- after the
    // new item's `mounted()`, matching applyUpdateOp/applyListPatch ordering. A
    // container hook has no other channel: nothing bubbles and there is no
    // MutationObserver fallback, so without this a stream container hook fires only
    // when the server happens to full-render, making it depend on the diff's op-code
    // choice for a change that is semantically identical.
    notifyUpdated(el);
}

/**
 * Remove a keyed child from a container element.
 * @param {Element} el -- container element
 * @param {string} key
 * @param {Map<Element, Map<string, Element>>|null} [streams] -- per-batch key maps
 */
function removeItemEl(el, key, streams = null) {
    const item = itemByKey(streams, el, key);
    if (!item) {
        console.warn(`[arizona] stream item az-key="${key}" not found for remove`);
        return;
    }
    removeEl(item);
    streams?.get(el)?.delete(key);
    notifyUpdated(el);
}

/**
 * Move a keyed child after another keyed element within a container (or prepend
 * if afterKey is null). Preserves form state, focus, scroll position, CSS
 * animations, and hook instances.
 * @param {Element} el -- container element
 * @param {string} key
 * @param {string|null} afterKey -- key of preceding sibling, or null for prepend
 * @param {Map<Element, Map<string, Element>>|null} [streams] -- per-batch key maps
 * @param {string} [az] -- the op's slot az, used to anchor placement to the slot span
 */
function moveItemEl(el, key, afterKey, streams = null, az) {
    const item = itemByKey(streams, el, key);
    if (!item) {
        console.warn(`[arizona] stream item az-key="${key}" not found for move`);
        return;
    }
    // Head and tail placement are relative to the slot span when there is one:
    // `el.prepend` would put the item before the slot's static siblings (a header),
    // and `el.appendChild` after them (a footer).
    const bounds = slotBounds(el, az);
    if (afterKey === null) {
        if (bounds) bounds.start.after(item);
        else el.prepend(item);
    } else {
        const ref = itemByKey(streams, el, afterKey);
        if (ref) ref.after(item);
        else if (bounds) el.insertBefore(item, bounds.end);
        else el.appendChild(item);
    }
    // Both: the item's position among its siblings is its own observable state (a row
    // hook may animate its move), and the container's child ORDER changed.
    notifyUpdated(item);
    notifyUpdated(el);
}

/**
 * Apply `innerOps` to the keyed child of `container`. Warns and no-ops if
 * the key isn't present.
 * @param {Element} container
 * @param {string} key
 * @param {Array<Array<*>>} innerOps
 * @param {Map<Element, Map<string, Element>>|null} [streams] -- per-batch key maps
 */
function applyItemPatch(container, key, innerOps, streams = null) {
    const item = itemByKey(streams, container, key);
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
 *
 * An op whose head is NOT an op code is the `[ChildViewId, ChildOps]` child-view
 * wrapper: `arizona_socket:flatten_ops/2` unwraps it only at top level, so a
 * `?stateful` child inside a stream `?each` item ships it here still wrapped. Its
 * ops belong to the child's own view root, resolved by id like the top level does
 * for `viewId:az` -- and they are the same bare-`az` shape, so they recurse
 * through this function (which also covers a grandchild wrapper).
 * @param {Element} item
 * @param {Array<Array<*>>} innerOps
 */
function applyItemOps(item, innerOps) {
    for (const op of innerOps) {
        // Same per-op isolation as applyOps: a throwing inner op must not abort
        // the rest of the item's patch batch.
        try {
            if (typeof op[0] !== 'number') {
                const childRoot = findViewRoot(op[0]);
                // Loud like the top-level miss: a dropped child batch reads as
                // "the child just stopped updating".
                if (childRoot) applyItemOps(childRoot, op[1]);
                else console.warn(`[arizona] item op child view "${op[0]}" not found; skipping`);
                continue;
            }
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
                case OP.REMOVE_NODE: {
                    const innerEl = item.querySelector(`[az="${az}"]`);
                    if (innerEl) removeEl(innerEl);
                    break;
                }
                case OP.INSERT:
                    insertItemEl(resolveInnerEl(item, az), op[2], op[3], op[4], null, az);
                    break;
                case OP.REMOVE:
                    removeItemEl(resolveInnerEl(item, az), op[2]);
                    break;
                case OP.ITEM_PATCH:
                    patchItemEl(item, az, op[2], op[3]);
                    break;
                case OP.MOVE:
                    moveItemEl(resolveInnerEl(item, az), op[2], op[3], null, az);
                    break;
                case OP.LIST_PATCH:
                    applyListPatch(resolveInnerEl(item, az), az, op[2]);
                    break;
                default:
                    console.warn(`[arizona] item op ${op[0]} not recognized; skipping`);
            }
        } catch (err) {
            console.error('[arizona] item op %s failed; skipping', op[0], err);
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
    /** @type {Element[]} */
    const roots = [];
    const endMarker = forEachNodeInSlot(marker, (node) => {
        if (node.nodeType === 1) roots.push(/** @type {Element} */ (node));
    });
    if (!endMarker) {
        // Undelimited slot: positions are meaningless and an insert would land
        // outside the list. Same refusal as a missing opening marker.
        console.warn(`[arizona] list-patch slot az:${az} has no closing marker; skipping`);
        return;
    }
    let childListChanged = false;
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
                childListChanged = true;
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
                ref.before(tpl.content);
                for (const e of added) mountHooks(e);
                childListChanged = true;
                break;
            }
        }
    }
    // Only when a sub-op actually changed the child list. A pure ITEM_PATCH batch
    // mutated a descendant, not `el`, and an empty batch mutated nothing -- neither
    // is an update TO the container.
    if (childListChanged) notifyUpdated(el);
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
 * Send an event to the root view. The target goes out as null and the server
 * resolves it against the root view id it already holds -- see pushEventTo.
 * @param {string} event
 * @param {*} [payload]
 */
function pushEvent(event, payload) {
    pushEventTo(null, event, payload);
}

/**
 * Send an event to a specific view by id, or to the root view when `view` is
 * null/undefined.
 *
 * A null target is the wire value for "the root view", NOT a missing value: the
 * server maps a non-binary target to the root view id it is already tracking
 * (`arizona_socket:event_target/2`). Resolving it here from the DOM instead
 * would be a guess -- `document.querySelector('[az-view]')` returns the FIRST
 * marker in document order, which is not the root view when the page carries a
 * marker for a component that was never registered as a live view (a ?stateful
 * rendered outside the live tree). Such a guess sends a plausible-looking id the
 * server cannot recognize, and it drops the event; null is always recoverable.
 * @param {string|null|undefined} view
 * @param {string} event
 * @param {*} [payload]
 */
function pushEventTo(view, event, payload) {
    workerPost(W_SEND, JSON.stringify([view ?? null, event, payload]));
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
 * The submit button that initiated a submit event, or null. Passed as the
 * second arg to `new FormData(form, submitter)` so a form with multiple named
 * submit buttons reports which one fired (its name/value in the field data),
 * matching a native form POST. A non-submit event (a click) has no submitter,
 * so a plain button never drags the form's fields along -- gathering stays a
 * property of submitting the form, not of the trigger element.
 * @param {Event|null} event
 * @returns {HTMLElement|null}
 */
function submitter(event) {
    return /** @type {any} */ (event)?.submitter ?? null;
}

/**
 * Auto-collect payload from an element based on its type and event context.
 * Drop -> {data_transfer, drop_index}, Forms -> FormData (incl. the submitter),
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
        return Object.fromEntries(
            new FormData(/** @type {HTMLFormElement} */ (el), submitter(event)),
        );
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
 * @returns {boolean}
 */
function commandsIncludeFetch(cmds) {
    const list = Array.isArray(cmds[0]) ? cmds : [cmds];
    return list.some((c) => {
        if (c[0] === JS_FETCH) return true;
        // transition(...)/on_key(...) wrap their inner command(s) in c[2]; a fetch
        // nested there still runs (inside the transition callback), so it must also
        // defer the reset -- recurse instead of only checking the top level.
        if (c[0] === JS_TRANSITION || c[0] === JS_ON_KEY) return commandsIncludeFetch(c[2]);
        return false;
    });
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
            // Every hosting document, like the selector effects: a popped-out
            // (PiP) view is a first-class Arizona document (it gets the same
            // event delegation and the same server patches), so a listener there
            // is as legitimate a target as one on the main document. A fresh
            // event per document -- an Event carries its own dispatch state.
            for (const doc of allDocs()) {
                doc.dispatchEvent(new CustomEvent(cmd[1], { detail: cmd[2] || {} }));
            }
            break;
        case JS_NAVIGATE: {
            const full = cmd[1];
            const opts = cmd[2] || {};
            if (opts.full) {
                // Full-page navigation: let the browser load the URL normally.
                // Used when the target isn't a live route (a controller/asset
                // path, or a 404) and so can't be SPA-navigated.
                location.assign(full);
                break;
            }
            const u = new URL(full, location.origin);
            const hash = u.hash ? u.hash.slice(1) : '';
            const qs = u.search ? u.search.slice(1) : '';
            navigateTo(u.pathname, qs, hash, { ...opts, fullUrl: full });
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
                    const fd = /** @type {any} */ (new FormData(form, submitter(event)));
                    const qs = new URLSearchParams(fd).toString();
                    if (qs) target += (url.includes('?') ? '&' : '?') + qs;
                }
            } else if (opts.body !== undefined) {
                body = JSON.stringify(opts.body);
                headers['content-type'] = 'application/json';
            } else if (form) {
                // Mirror a normal form POST: application/x-www-form-urlencoded.
                // (multipart / file uploads are a documented non-goal.)
                body = new URLSearchParams(
                    /** @type {any} */ (new FormData(form, submitter(event))),
                );
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
                        // Effect application is isolated from the trailing network
                        // .catch: a throw here is an app bug on a request that
                        // SUCCEEDED (the body parsed), so it is logged -- letting it
                        // flow into the .catch would fire on_error/arizona:fetch-error
                        // as a phantom network failure, after effects partially applied.
                        if (effects !== null) {
                            try {
                                executeJS(el?.closest?.('[az-view]') ?? el, null, effects);
                            } catch (err) {
                                console.error('[arizona] fetch response effect threw', err);
                            }
                        } else onError({ url, status: resp.status });
                        // Honor az-form-reset only on a 2xx success, so a validation
                        // error (a non-2xx) keeps the typed fields. Same isolation as
                        // the effects above.
                        if (resp.ok && form) {
                            try {
                                maybeResetForm(form);
                            } catch (err) {
                                console.error('[arizona] az-form-reset failed', err);
                            }
                        }
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
                // writeText rejects without permission / a secure context; catch it
                // (like JS_OS) so a blocked copy is a logged no-op, not an unhandled
                // rejection.
                navigator.clipboard
                    ?.writeText?.(text)
                    ?.catch?.((err) => console.error('[arizona] copy_to_clipboard failed:', err));
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
            // Smooth is the default, and opts merge onto it rather than replacing
            // it: passing only an alignment (`#{block => center}`) would otherwise
            // fall back to scrollIntoView's own `behavior: 'auto'` and scroll
            // instantly. An explicit `behavior` still wins.
            withQuery(cmd[1], (t) => t.scrollIntoView({ behavior: 'smooth', ...cmd[2] }));
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
            // requestWindow rejects without a user gesture; catch it (like JS_OS)
            // so a server-pushed request_pip is a logged no-op, not an unhandled
            // rejection.
            requestPip(cmd[1], cmd[2] || {}).catch((err) =>
                console.error('[arizona] request_pip failed:', cmd[1], err),
            );
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
        // A duplicate name (a repeated text input, a checkbox group) was saved as
        // an array. Value-setting fields consume it positionally, so track how many
        // values each name has handed out; checkbox groups match by value instead.
        /** @type {Object<string, number>} */
        const cursor = {};
        /** @param {string} name @returns {string|undefined} */
        const nextValue = (name) => {
            const val = fields[name];
            if (!Array.isArray(val)) return /** @type {string} */ (val);
            const i = cursor[name] || 0;
            cursor[name] = i + 1;
            return val[i];
        };
        for (const el of formEl.elements) {
            if (el instanceof HTMLInputElement) {
                if (!el.name || el.type === 'file') continue;
                if (el.type === 'checkbox') {
                    // Check by value membership: a checkbox group saves only the
                    // checked boxes' values, so `name in fields` would tick them all.
                    const val = fields[el.name];
                    el.checked = Array.isArray(val) ? val.includes(el.value) : val === el.value;
                } else if (el.type === 'radio') {
                    el.checked = fields[el.name] === el.value;
                } else if (el.name in fields) {
                    const v = nextValue(el.name);
                    if (v !== undefined) el.value = v;
                }
            } else if (el instanceof HTMLSelectElement) {
                if (!el.name || !(el.name in fields)) continue;
                if (el.multiple) {
                    const val = fields[el.name];
                    const arr = Array.isArray(val) ? val : [val];
                    for (const opt of el.options) opt.selected = arr.includes(opt.value);
                } else {
                    const v = nextValue(el.name);
                    if (v !== undefined) el.value = v;
                }
            } else if (el instanceof HTMLTextAreaElement) {
                if (el.name && el.name in fields) {
                    const v = nextValue(el.name);
                    if (v !== undefined) el.value = v;
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
 * The connection owns module-level state (the Worker handle, the connected flag,
 * the saved forms), so only one can be live: calling `connect` again retires the
 * previous one first. Left unretired, its Worker would keep a second socket
 * applying ops to the same document, and its `disconnect` -- reading the same
 * module state -- would terminate THIS connection's Worker instead of its own.
 *
 * @param {string} endpoint
 * @param {Object<string, unknown>} [params]
 * @returns {() => void} disconnect
 */
function connect(endpoint, params = {}) {
    if (_teardown) {
        console.warn('[arizona] connect() called twice; disconnecting the previous connection');
        _teardown();
    }
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
            // Save the outgoing scroll first (scrollRestoration is 'manual'),
            // matching the push branch of navigateTo -- otherwise Back after an
            // in-page anchor click lands at the top.
            if (path === location.pathname && qs === location.search.slice(1)) {
                saveCurrentScroll();
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
            const path = location.pathname;
            const qs = location.search ? location.search.slice(1) : '';
            const hash = location.hash ? location.hash.slice(1) : '';
            const saved = e.state?._azScroll || null;
            // Fragment-only change (path + query unchanged): a same-page hash
            // jump fires popstate in some browsers, but it needs no server
            // round-trip -- just scroll. Mirrors the click handler's same-page
            // fast path so in-page anchors don't trigger a full OP_REPLACE.
            // Needs no connection either, so it runs even while disconnected.
            if (path === _currentPath && qs === _currentQs) {
                applyScroll({ kind: 'pop', hash, saved });
                return;
            }
            // A cross-page popstate cannot be served over a down WebSocket, and
            // the browser has already changed the URL -- returning would desync
            // URL and content and leave the reconnect URL stale (a reconnect
            // would resync to the pre-back path, and a later Forward would look
            // fragment-only). Degrade to a full load of the URL the browser now
            // shows, like a disconnected az-navigate click falls through to the
            // browser.
            if (!_connected) {
                location.reload();
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
            workerPost(W_UPDATE_PATH, { path, qs: reconnectUserQs(qs) });
            _currentPath = path;
            _currentQs = qs;
        },
        { signal },
    );

    // The WS URL is (re)built at every worker spawn -- the Worker can't access
    // location.*. Framework keys are `_az_`-prefixed so they can't collide with
    // user params or the page's own query string; the user-facing qs is the
    // tracked page qs plus the connect() extras (server reaches them via
    // arizona_req:params/1). Building from `_currentPath`/`_currentQs` rather
    // than a connect-time snapshot matters for the bfcache respawn: an SPA
    // navigation moves the path only inside the worker (W_UPDATE_PATH), the
    // worker dies on pagehide, and the respawned one must target the route the
    // restored page actually shows.
    /** @type {Record<string, string>} */
    const stringParams = {};
    for (const k of Object.keys(params)) stringParams[k] = String(params[k]);
    _connectQs = new URLSearchParams(stringParams).toString();
    const buildWsUrl = () => {
        const protocol = location.protocol === 'https:' ? 'wss:' : 'ws:';
        const pagePath = encodeURIComponent(_currentPath);
        const userQs = reconnectUserQs(_currentQs);
        const qs = userQs ? `_az_path=${pagePath}&${userQs}` : `_az_path=${pagePath}`;
        // Native-shell capabilities (Electron/Tauri/...) advertised at the WS
        // handshake so the live process can answer ?capability(...). Absent in a
        // plain browser (no `__arizona_os__`), so capsQs is empty and nothing
        // changes. Rides reconnect because the Worker reuses this URL's search.
        const osCaps = osHost()?.capabilities;
        const capsQs = osCaps ? `&_az_caps=${encodeURIComponent(JSON.stringify(osCaps))}` : '';
        return `${protocol}//${location.host}${endpoint}?${qs}${capsQs}`;
    };

    /** @type {Function|null} */
    let _onmessageHook = null;

    // Spawn the Worker, wire its message handler, and open the socket. Extracted
    // so a bfcache restore (`pageshow`) can re-establish the connection that
    // `pagehide` tore down. `reconnect` flags that spawn: the restored page's
    // DOM already exists with the live state it had at pagehide, so the worker
    // must open with `_az_reconnect=1` -- only that makes the server send the
    // full-page resync (and the first frame restore any saved form state).
    /** @param {boolean} reconnect */
    const spawnWorker = (reconnect) => {
        // Worker is co-located with this script. This static
        // `new Worker(new URL(..., import.meta.url), { type: 'module' })` shape is
        // what bundlers (Vite/Rollup/rolldown) statically detect: Arizona's own
        // build emits the sibling `arizona-worker.min.js` next to
        // `arizona.min.js` and rewrites this reference to it, and a consumer that
        // re-bundles the built client gets the worker auto-emitted,
        // content-hashed, and its URL rewritten (no runtime-string 404).
        _worker = new Worker(new URL('./arizona-worker.js', import.meta.url), { type: 'module' });

        // A Worker that fails to load or throws at top level never posts [1, ...],
        // so without this the page just silently never connects.
        _worker.onerror = (e) => {
            console.error('[arizona] worker error:', e.message || e);
        };

        _worker.onmessage = (e) => {
            const msg = e.data;
            switch (msg[0]) {
                case 0: {
                    // [0, ops|null, effects|null, firstAfterReconnect]
                    const apply = () => {
                        // A patch-scroll intent lives exactly until the first frame
                        // after the patch request. applyOps consumes it when the
                        // frame carries ops (the patch reply diff -- scroll applies);
                        // otherwise it is cleared here, so a no-op patch (the server
                        // sends nothing back) can't leave the intent armed for an
                        // unrelated later diff to yank the scroll. Identity-checked
                        // so an intent armed by THIS frame's effects (a JS_PATCH
                        // command) survives to its own reply. Residual race: for a
                        // truly silent patch the next frame -- whatever it is -- is
                        // indistinguishable from a slow patch reply, so one
                        // unrelated ops-frame can still scroll; there is no reply
                        // id on the wire to do better client-side.
                        const armedScroll = _pendingScroll;
                        if (msg[1]) applyOps(msg[1]);
                        if (msg[2]) applyEffects(msg[2]);
                        if (msg[3]) restoreFormState();
                        if (armedScroll?.patch && _pendingScroll === armedScroll) {
                            _pendingScroll = null;
                        }
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
                    // A clean connect means any prior crash loop is broken -- reset
                    // the crash-reload guard so a future genuine crash reloads again.
                    try {
                        sessionStorage.removeItem(CRASH_RELOAD_KEY);
                    } catch {
                        /* sessionStorage unavailable -- nothing to reset */
                    }
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
                        crashReload();
                        return;
                    }
                    if (msg[1] !== WS_CLOSE_NORMAL) saveFormState();
                    break;
                }
            }
        };

        // Send connect message to Worker, rebuilding the URL from the
        // currently tracked route (see buildWsUrl).
        workerPost(W_CONNECT, buildWsUrl(), reconnect);
    };

    spawnWorker(false);

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
                // Preserve typed form fields first: the pageshow reconnect asks
                // the server for a full resync, whose OP_REPLACE rebuilds the
                // DOM bfcache had preserved -- the same save/restore that
                // covers an abnormal close covers this.
                saveFormState();
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
            // Only on a real bfcache restore; a normal load already has a
            // worker. A restore is semantically a reconnect: the DOM exists
            // with evolved live state, so the server must resync it.
            if (e.persisted && !_worker) spawnWorker(true);
        },
        { signal },
    );

    let disconnected = false;
    _teardown = function disconnect() {
        if (disconnected) return;
        disconnected = true;
        _teardown = null;
        controller.abort();
        if (_worker) {
            _worker.terminate();
            _worker = null;
        }
        _connected = false;
        _pendingScroll = null;
        _pendingTransition = null;
        _connectQs = '';
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
        _pipWindows.clear();
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
    return _teardown;
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

    // Open the floating window FIRST. requestWindow rejects without a user
    // gesture (a server-pushed request_pip has none); await before touching the
    // DOM so a rejection leaves no orphaned placeholder behind.
    // Forward the caller's options straight to the browser (no framework defaults);
    // requestWindow ignores dictionary members it doesn't know (e.g. onClose).
    const pip = await pipApi.requestWindow(opts);

    // Remember where the view sat so it can be restored in place on close.
    const placeholder = document.createComment(`az-pip:${viewId}`);
    view.before(placeholder);

    copyStyles(document, pip.document);
    pip.document.body.append(view);
    _viewDocs.set(viewId, pip.document);
    _pipWindows.set(viewId, { win: pip, placeholder });

    // Delegate events fired inside the floating window; torn down on close.
    const controller = new AbortController();
    bindDocumentEvents(pip.document, controller.signal);

    pip.addEventListener(
        'pagehide',
        () => {
            controller.abort();
            _viewDocs.delete(viewId);
            _pipWindows.delete(viewId);
            // isConnected, not parentNode: a replaced (navigate) outgoing
            // subtree is detached wholesale, so the placeholder still HAS a
            // parent -- inside a dead tree.
            if (placeholder.isConnected) {
                placeholder.replaceWith(view);
            } else {
                // The view's inline home was destroyed while it was popped out
                // (a navigate OP_REPLACE swapped the page): the new page has its
                // own content, so discard the stale element -- appending it to
                // the new page's body would resurrect dead server state. Tear
                // its hooks down like any removed subtree.
                destroyHooks(view);
                view.remove();
            }
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
