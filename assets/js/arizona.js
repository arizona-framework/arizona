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
// Example -- a chart hook that initializes a library on mount, resizes on
// update, cleans up on destroy, and notifies the server it's ready:
//
//   import { hooks, connect } from './arizona.js';
//   hooks.Chart = {
//       mounted() {
//           this.chart = new ChartLib(this.el);
//           this.pushEvent('chart_ready', { width: this.el.offsetWidth });
//       },
//       updated()   { this.chart.resize(); },
//       destroyed() { this.chart.destroy(); },
//   };
//   connect('/ws');
//
// On the server, handle_event/3 receives the pushed event:
//
//   handle_event(<<"chart_ready">>, #{<<"width">> := W}, Bindings) ->
//       {Bindings#{chart_width => W}, []}.
// ---------------------------------------------------------------------------

/** @type {Object<string, {mounted?: function, updated?: function, destroyed?: function}>} */
const hooks = {};

/** @type {Map<Element, {el: Element, __name: string, pushEvent: function}>} */
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
// keep flowing after the move. Maps a viewId -> the Document that owns it.
/** @type {Map<string, Document>} */
const _viewDocs = new Map();

/**
 * The Document that owns a view's root element (the main document by default).
 * @param {string} viewId
 * @returns {Document}
 */
function docFor(viewId) {
    return _viewDocs.get(viewId) || document;
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
 * Pending scroll intent set when az-navigate / popstate is handled, applied
 * after OP_REPLACE renders the new page.
 * @type {{kind: 'push'|'pop', hash: string, saved?: {x:number,y:number}|null}|null}
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
 * Save the current scroll position onto the current history entry so
 * back/popstate can restore it.
 */
function saveCurrentScroll() {
    const st = history.state || {};
    history.replaceState(
        { ...st, _azScroll: { x: window.scrollX, y: window.scrollY } },
        '',
        location.href,
    );
}

/**
 * Perform an SPA navigation. Shared code path for `az-navigate` clicks and
 * `arizona_js:navigate/1,2` effects.
 *
 * @param {string} path     Path portion (no hash, no query), sent to the server.
 * @param {string} qs       Query string without leading `?`, sent to the server.
 * @param {string} hash     Fragment without leading `#`; used client-side
 *                          to scroll to the target after OP_REPLACE.
 * @param {{replace?: boolean, noscroll?: boolean, fullUrl?: string}} opts
 *   - replace  Use `replaceState` instead of `pushState`. Does not save
 *              outgoing scroll, does not reset scroll.
 *   - noscroll Push only: skip the scroll-to-top/hash after REPLACE.
 *   - fullUrl  Exact URL to write to history (defaults to
 *              `path + '?' + qs + '#' + hash`). Lets the click handler
 *              preserve the original href verbatim.
 */
function navigateTo(path, qs, hash, opts) {
    const pathAndQs = qs ? `${path}?${qs}` : path;
    const fullUrl = opts.fullUrl || (hash ? `${pathAndQs}#${hash}` : pathAndQs);
    if (opts.replace) {
        history.replaceState(null, '', fullUrl);
    } else {
        saveCurrentScroll();
        history.pushState(null, '', fullUrl);
        if (!opts.noscroll) _pendingScroll = { kind: 'push', hash };
    }
    workerPost(W_SEND, JSON.stringify(['navigate', { path, qs }]));
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
 * @param {Element} el
 */
function mountHook(el) {
    if (_hooks.has(el)) return;
    const name = el.getAttribute('az-hook');
    if (!name || !hooks[name]) return;
    const def = hooks[name];
    const instance = {
        el,
        __name: name,
        /** @param {string} eventName @param {*} payload */
        pushEvent(eventName, payload) {
            workerPost(W_SEND, JSON.stringify([resolveTarget(el), eventName, payload || {}]));
        },
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
 * Mount hooks on all [az-hook] elements within root (inclusive).
 * @param {Element|Document} root
 */
function mountHooks(root) {
    if (root instanceof Element && root.hasAttribute && root.hasAttribute('az-hook'))
        mountHook(root);
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
 * and walk hook lifecycle.
 * @param {Element} el
 * @param {string} az
 * @param {string} val
 */
function applyTextOp(el, az, val) {
    const marker = findMarker(el, az);
    if (marker) {
        forEachElementBetweenMarkers(marker, destroyHooks);
        updateMarkerContent(marker, val);
        forEachElementBetweenMarkers(marker, mountHooks);
    } else {
        destroyChildHooks(el);
        el.textContent = val;
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
 * Apply a batch of ops to the DOM. Ops arrive pre-resolved from the Worker --
 * all template payloads are already HTML strings.
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
                applyTextOp(el, az, op[2]);
                break;
            case OP.SET_ATTR:
                applySetAttrOp(el, op[2], op[3]);
                break;
            case OP.REM_ATTR:
                el.removeAttribute(op[2]);
                notifyUpdated(el);
                break;
            case OP.UPDATE:
                applyUpdateOp(el, op[2]);
                break;
            case OP.REPLACE: {
                destroyHooks(el);
                el.outerHTML = op[2];
                const newEl = resolveEl(op[1]);
                if (newEl) mountHooks(newEl);
                didReplace = true;
                break;
            }
            case OP.REMOVE_NODE:
                destroyHooks(el);
                el.remove();
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
        }
    }
    if (didReplace && _pendingScroll) {
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
    if (i === -1) return docFor(target).getElementById(target);
    const viewId = target.substring(0, i);
    const az = target.substring(i + 1);
    const view = docFor(viewId).getElementById(viewId);
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
 * Replace everything between <!--az:X--> and <!--/az--> with new content.
 * Uses a <template> for HTML strings (to parse tags) or a text node for plain text.
 * @param {Comment} startMarker
 * @param {string} value
 */
function updateMarkerContent(startMarker, value) {
    const doc = startMarker.ownerDocument;
    let node = startMarker.nextSibling;
    while (node && !(node.nodeType === 8 && /** @type {Comment} */ (node).data === '/az')) {
        const next = node.nextSibling;
        node.remove();
        node = next;
    }
    // Insert new content before the closing marker
    if (value.includes('<')) {
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
    let node = startMarker.nextSibling;
    while (node && !(node.nodeType === 8 && /** @type {Comment} */ (node).data === '/az')) {
        const next = node.nextSibling;
        node.remove();
        node = next;
    }
    startMarker.after(startMarker.ownerDocument.createTextNode(value == null ? '' : String(value)));
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
                    const aff = parsed.ap && parsed.ap[attr];
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
    const root = docFor(viewId).getElementById(viewId);
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
        const root = docFor(viewId).getElementById(viewId);
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
    destroyHooks(item);
    item.remove();
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
                applyTextOp(resolveInnerEl(item, az), az, op[2]);
                break;
            case OP.SET_ATTR:
                applySetAttrOp(resolveInnerEl(item, az), op[2], op[3]);
                break;
            case OP.REM_ATTR: {
                const innerEl = resolveInnerEl(item, az);
                innerEl.removeAttribute(op[2]);
                notifyUpdated(innerEl);
                break;
            }
            case OP.UPDATE:
                applyUpdateOp(resolveInnerEl(item, az), op[2]);
                break;
            case OP.REMOVE_NODE: {
                const innerEl = item.querySelector(`[az="${az}"]`);
                if (innerEl) {
                    destroyHooks(innerEl);
                    innerEl.remove();
                }
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
        }
    }
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
    JS_EXIT_PIP = 19;

/**
 * If `sel` matches an element, call `fn` with it cast to `HTMLElement`.
 * Used by the executeJS targeted commands; some need HTMLElement-only
 * properties (`hidden`, `focus`, `blur`).
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
 * Execute JS commands from an az-* attribute value.
 * Single command: [opcode, ...args]
 * Multiple commands: [[opcode, ...args], [opcode, ...args]]
 * @param {Element} el - the element that triggered the event
 * @param {Event|null} event - the DOM event (null for programmatic dispatch)
 * @param {Array<*>} cmds - parsed JSON command(s)
 */
function executeJS(el, event, cmds) {
    const commands = Array.isArray(cmds[0]) ? cmds : [cmds];
    for (const cmd of commands) {
        const op = cmd[0];
        switch (op) {
            case JS_PUSH_EVENT: {
                const evt = cmd[1];
                const payload =
                    cmd.length > 2
                        ? { ...autoPayload(el, event), ...cmd[2] }
                        : autoPayload(el, event);
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
                withQuery(cmd[1], (t) => (t.hidden = !t.hidden));
                break;
            case JS_SHOW:
                withQuery(cmd[1], (t) => (t.hidden = false));
                break;
            case JS_HIDE:
                withQuery(cmd[1], (t) => (t.hidden = true));
                break;
            case JS_ADD_CLASS:
                withQuery(cmd[1], (t) => t.classList.add(cmd[2]));
                break;
            case JS_REMOVE_CLASS:
                withQuery(cmd[1], (t) => t.classList.remove(cmd[2]));
                break;
            case JS_TOGGLE_CLASS:
                withQuery(cmd[1], (t) => t.classList.toggle(cmd[2]));
                break;
            case JS_SET_ATTR:
                withQuery(cmd[1], (t) => t.setAttribute(cmd[2], cmd[3]));
                break;
            case JS_REMOVE_ATTR:
                withQuery(cmd[1], (t) => t.removeAttribute(cmd[2]));
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
            case JS_FOCUS:
                withQuery(cmd[1], (t) => t.focus());
                break;
            case JS_BLUR:
                withQuery(cmd[1], (t) => t.blur());
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
                requestPip(cmd[1]);
                break;
            case JS_EXIT_PIP:
                exitPip(cmd[1]);
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
            executeJS(el, e, JSON.parse(raw));
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
            if (raw) executeJS(form, e, JSON.parse(raw));
            if (form.hasAttribute('az-form-reset')) /** @type {HTMLFormElement} */ (form).reset();
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
            executeJS(container, e, JSON.parse(raw));
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

    // SPA navigation: az-navigate (boolean attr) on <a> triggers client-side
    // navigation. The path is read from href (hash stripped before sending
    // to the server). Sends ["navigate", {path}] to the server, which renders
    // the new page and sends OP_REPLACE on the content slot. Scroll resets to
    // top (or #hash target) on new nav; opt out with az-noscroll.
    document.addEventListener(
        'click',
        (e) => {
            const me = /** @type {MouseEvent} */ (e);
            // Let the browser handle modifier-key and non-primary clicks (open in
            // new tab/window, etc.) so az-navigate doesn't hijack them.
            if (me.button !== 0 || me.ctrlKey || me.metaKey || me.shiftKey || me.altKey) return;
            const el = /** @type {HTMLAnchorElement} */ (
                /** @type {Element} */ (e.target).closest('[az-navigate]')
            );
            if (!el || !_connected) return;
            const href = el.getAttribute('href');
            if (!href) return;
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

            navigateTo(path, qs, hash, { noscroll, fullUrl: href });
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
            _pendingScroll = { kind: 'pop', hash, saved };
            workerPost(W_SEND, JSON.stringify(['navigate', { path, qs }]));
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
    const wsUrl = `${protocol}//${location.host}${endpoint}?${qs}`;

    // Worker is co-located with this script.
    const baseUrl = new URL(/* @vite-ignore */ '.', import.meta.url).href;

    /** @type {Function|null} */
    let _onmessageHook = null;

    // Spawn the Worker, wire its message handler, and open the socket. Extracted
    // so a bfcache restore (`pageshow`) can re-establish the connection that
    // `pagehide` tore down.
    const spawnWorker = () => {
        _worker = new Worker(`${baseUrl}arizona-worker.min.js`, { type: 'module' });

        _worker.onmessage = (e) => {
            const msg = e.data;
            switch (msg[0]) {
                case 0: {
                    // [0, ops|null, effects|null, firstAfterReconnect]
                    if (msg[1]) applyOps(msg[1]);
                    if (msg[2]) applyEffects(msg[2]);
                    if (msg[3]) restoreFormState();
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

    const pip = await pipApi.requestWindow({
        width: opts.width || 360,
        height: opts.height || 240,
    });

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
