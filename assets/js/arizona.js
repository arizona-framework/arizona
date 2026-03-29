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

/** @type {Map<string, {fields: Object<string, string|string[]>, azChange: string|null}>} */
const _savedForms = new Map();

/**
 * Send a pre-stringified JSON message to the server via the Worker.
 * @param {string} jsonString
 */
function workerSend(jsonString) {
    if (_worker && _connected) _worker.postMessage([1, jsonString]);
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
            workerSend(JSON.stringify([resolveTarget(el), eventName, payload || {}]));
        },
    };
    _hooks.set(el, instance);
    if (def.mounted) def.mounted.call(instance);
}

/**
 * Destroy a hook on an element. Calls destroyed() if defined, then removes from tracking.
 * @param {Element} el
 */
function destroyHook(el) {
    const instance = _hooks.get(el);
    if (!instance) return;
    const def = hooks[instance.__name];
    if (def && def.destroyed) def.destroyed.call(instance);
    _hooks.delete(el);
}

/**
 * Call updated() on a hooked element if it is tracked.
 * @param {Element} el
 */
function notifyUpdated(el) {
    const instance = _hooks.get(el);
    if (!instance) return;
    const def = hooks[instance.__name];
    if (def && def.updated) def.updated.call(instance);
}

/**
 * Mount hooks on all [az-hook] elements within root (inclusive).
 * @param {Element|Document} root
 */
function mountHooks(root) {
    if (root instanceof Element && root.hasAttribute && root.hasAttribute('az-hook')) mountHook(root);
    root.querySelectorAll('[az-hook]').forEach(mountHook);
}

/**
 * Destroy hooks on descendant [az-hook] elements only (not root itself).
 * Used by ops that replace inner content but keep the element (UPDATE, TEXT).
 * @param {Element} root
 */
function destroyChildHooks(root) {
    root.querySelectorAll('[az-hook]').forEach((el) => destroyHook(el));
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
 * Destroy hooks on elements between a start marker and its closing <!--/az--> marker.
 * @param {Comment} startMarker
 */
function destroyHooksBetweenMarkers(startMarker) {
    let node = startMarker.nextSibling;
    while (node && !(node.nodeType === 8 && /** @type {Comment} */ (node).data === "/az")) {
        if (node.nodeType === 1) destroyHooks(/** @type {Element} */(node));
        node = node.nextSibling;
    }
}

/**
 * Mount hooks on elements between a start marker and its closing <!--/az--> marker.
 * @param {Comment} startMarker
 */
function mountHooksBetweenMarkers(startMarker) {
    let node = startMarker.nextSibling;
    while (node && !(node.nodeType === 8 && /** @type {Comment} */ (node).data === "/az")) {
        if (node.nodeType === 1) mountHooks(/** @type {Element} */(node));
        node = node.nextSibling;
    }
}

/**
 * Apply a batch of ops to the DOM. Ops arrive pre-resolved from the Worker --
 * all template payloads are already HTML strings.
 * @param {Array<Array<*>>} ops
 */
function applyOps(ops) {
    for (const op of ops) {
        const el = resolveEl(op[1]);
        if (!el) continue;
        const az = op[1].substring(op[1].indexOf(":") + 1);
        switch (op[0]) {
            case OP.TEXT: {
                const val = op[2];
                const marker = findMarker(el, az);
                if (marker) {
                    destroyHooksBetweenMarkers(marker);
                    updateMarkerContent(marker, val);
                    mountHooksBetweenMarkers(marker);
                    notifyUpdated(el);
                } else {
                    destroyChildHooks(el);
                    el.textContent = val;
                    notifyUpdated(el);
                }
                break;
            }
            case OP.SET_ATTR:
                el.setAttribute(op[2], op[3]);
                // Sync the DOM property for form elements -- setAttribute alone
                // doesn't update the live value of <input>/<select>/<textarea>.
                if (op[2] === 'value' && 'value' in el) el.value = op[3];
                notifyUpdated(el);
                break;
            case OP.REM_ATTR:
                el.removeAttribute(op[2]);
                notifyUpdated(el);
                break;
            case OP.UPDATE:
                destroyChildHooks(el);
                el.innerHTML = op[2];
                mountHooks(el);
                notifyUpdated(el);
                break;
            case OP.REPLACE: {
                destroyHooks(el);
                el.outerHTML = op[2];
                const newEl = resolveEl(op[1]);
                if (newEl) mountHooks(newEl);
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
}

/**
 * Resolve a patch target to a DOM element. Bare targets (no colon) resolve to
 * the view root element itself -- used by OP_REPLACE for navigation. Scoped
 * targets ("viewId:az") find the view root, then the element within it.
 * @param {string} target
 * @returns {Element|null}
 */
function resolveEl(target) {
    const i = target.indexOf(":");
    if (i === -1) return document.getElementById(target);
    const viewId = target.substring(0, i);
    const az = target.substring(i + 1);
    const view = document.getElementById(viewId);
    if (!view) return null;
    if (view.getAttribute('az') === az) return view;
    let el = view.querySelector('[az="' + az + '"]');
    if (!el) {
        const j = az.indexOf(':');
        if (j !== -1) el = view.querySelector('[az="' + az.substring(0, j) + '"]');
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
        if (node.nodeType === 8 && /** @type {Comment} */ (node).data === "az:" + az) {
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
    let node = startMarker.nextSibling;
    while (node && !(node.nodeType === 8 && /** @type {Comment} */ (node).data === "/az")) {
        const next = node.nextSibling;
        node.remove();
        node = next;
    }
    // Insert new content before the closing marker
    if (value.includes("<")) {
        const tpl = document.createElement("template");
        tpl.innerHTML = value;
        startMarker.after(tpl.content);
    } else {
        startMarker.after(document.createTextNode(value));
    }
}

/**
 * Insert a keyed child into a container element.
 * @param {Element} el -- container element
 * @param {string} key
 * @param {number} pos -- -1 means append, otherwise insert before child at index
 * @param {string} html
 */
function insertItemEl(el, key, pos, html) {
    const tpl = document.createElement("template");
    tpl.innerHTML = html;
    const fragment = tpl.content;
    if (pos === -1) {
        el.appendChild(fragment);
    } else {
        const children = el.querySelectorAll(":scope > [az-key]");
        if (pos < children.length) {
            el.insertBefore(fragment, children[pos]);
        } else {
            el.appendChild(fragment);
        }
    }
    const item = el.querySelector('[az-key="' + key + '"]');
    if (item) mountHooks(item);
    else console.warn('[arizona] stream item missing az-key="' + key + '" after insert');
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
    const el = resolveEl(target);
    if (!el) return;
    insertItemEl(el, key, pos, html);
}

/**
 * Remove a keyed child from a container element.
 * @param {Element} el -- container element
 * @param {string} key
 */
function removeItemEl(el, key) {
    const item = el.querySelector(':scope > [az-key="' + key + '"]');
    if (!item) { console.warn('[arizona] stream item az-key="' + key + '" not found for remove'); return; }
    destroyHooks(item);
    item.remove();
}

/**
 * Remove a keyed child element from its container.
 * @param {string} target
 * @param {string} key
 */
function removeItem(target, key) {
    const el = resolveEl(target);
    if (!el) return;
    removeItemEl(el, key);
}

/**
 * Move a keyed child after another keyed element within a container.
 * @param {Element} el -- container element
 * @param {string} key
 * @param {string|null} afterKey -- key of preceding sibling, or null for prepend
 */
function moveItemEl(el, key, afterKey) {
    const item = el.querySelector(':scope > [az-key="' + key + '"]');
    if (!item) { console.warn('[arizona] stream item az-key="' + key + '" not found for move'); return; }
    if (afterKey === null) {
        el.prepend(item);
    } else {
        const ref = el.querySelector(':scope > [az-key="' + afterKey + '"]');
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
    const el = resolveEl(target);
    if (!el) return;
    moveItemEl(el, key, afterKey);
}

/**
 * Resolve a nested item for patching: find the item element within a parent
 * element, then apply innerOps scoped to it.
 * @param {Element} parentEl -- parent item or container element
 * @param {string} az -- az attribute to find the container within parentEl
 * @param {string} key -- az-key of the item to patch
 * @param {Array<Array<*>>} innerOps -- ops scoped to the item
 */
function patchItemEl(parentEl, az, key, innerOps) {
    const container = resolveInnerEl(parentEl, az);
    const item = container.querySelector(':scope > [az-key="' + key + '"]');
    if (!item) { console.warn('[arizona] stream item az-key="' + key + '" not found for patch'); return; }
    applyItemOps(item, innerOps);
}

/**
 * Apply ops scoped to a single keyed item. innerOps use bare az indices
 * (not "viewId:az"), resolved relative to the item element.
 * @param {string} target
 * @param {string} key
 * @param {Array<Array<*>>} innerOps
 */
function patchItem(target, key, innerOps) {
    const el = resolveEl(target);
    if (!el) return;
    const item = el.querySelector(':scope > [az-key="' + key + '"]');
    if (!item) { console.warn('[arizona] stream item az-key="' + key + '" not found for patch'); return; }
    applyItemOps(item, innerOps);
}

/**
 * Resolve an element within a parent by az attribute, with compound fallback.
 * For compound az like "0:1", tries exact match first, then base az "0".
 * @param {Element} parent
 * @param {string} az
 * @returns {Element}
 */
function resolveInnerEl(parent, az) {
    let el = parent.querySelector('[az="' + az + '"]');
    if (!el && az.includes(':')) {
        el = parent.querySelector('[az="' + az.substring(0, az.indexOf(':')) + '"]');
    }
    return el || parent;
}

/**
 * Apply inner ops to an item element. Ops arrive pre-resolved from the Worker.
 * @param {Element} item
 * @param {Array<Array<*>>} innerOps
 */
function applyItemOps(item, innerOps) {
    for (const op of innerOps) {
        const az = op[1];
        switch (op[0]) {
            case OP.TEXT: {
                const val = op[2];
                const innerEl = resolveInnerEl(item, az);
                const marker = findMarker(innerEl, az);
                if (marker) {
                    destroyHooksBetweenMarkers(marker);
                    updateMarkerContent(marker, val);
                    mountHooksBetweenMarkers(marker);
                    notifyUpdated(innerEl);
                } else {
                    destroyChildHooks(innerEl);
                    innerEl.textContent = val;
                    notifyUpdated(innerEl);
                }
                break;
            }
            case OP.SET_ATTR: {
                const innerEl = resolveInnerEl(item, az);
                innerEl.setAttribute(op[2], op[3]);
                if (op[2] === 'value' && 'value' in innerEl) innerEl.value = op[3];
                notifyUpdated(innerEl);
                break;
            }
            case OP.REM_ATTR: {
                const innerEl = resolveInnerEl(item, az);
                innerEl.removeAttribute(op[2]);
                notifyUpdated(innerEl);
                break;
            }
            case OP.UPDATE: {
                const innerEl = resolveInnerEl(item, az);
                destroyChildHooks(innerEl);
                innerEl.innerHTML = op[2];
                mountHooks(innerEl);
                notifyUpdated(innerEl);
                break;
            }
            case OP.REMOVE_NODE: {
                const innerEl = item.querySelector('[az="' + az + '"]');
                if (innerEl) {
                    destroyHooks(innerEl);
                    innerEl.remove();
                }
                break;
            }
            case OP.INSERT: {
                const container = resolveInnerEl(item, az);
                insertItemEl(container, op[2], op[3], op[4]);
                break;
            }
            case OP.REMOVE: {
                const container = resolveInnerEl(item, az);
                removeItemEl(container, op[2]);
                break;
            }
            case OP.ITEM_PATCH: {
                patchItemEl(item, az, op[2], op[3]);
                break;
            }
            case OP.MOVE: {
                const container = resolveInnerEl(item, az);
                moveItemEl(container, op[2], op[3]);
                break;
            }
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
    pushEventTo(view, event, payload)
}

/**
 * Send an event to a specific view by id.
 * @param {string|null|undefined} view
 * @param {string} event
 * @param {*} [payload]
 */
function pushEventTo(view, event, payload) {
    workerSend(JSON.stringify([view, event, payload]));
}

/**
 * Determine which server-side view handles this element's events.
 * Explicit az-target takes priority; otherwise walk up to the nearest az-view.
 * @param {Element} el
 * @returns {string|null}
 */
function resolveTarget(el) {
    return el.getAttribute('az-target')
        || el.closest('[az-view]')?.id
        || null;
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
            drop_index: dropTarget ? children.indexOf(dropTarget) : -1
        };
    }
    const tag = el.tagName;
    if (tag === 'FORM') return Object.fromEntries(new FormData(/** @type {HTMLFormElement} */(el)));
    if (tag === 'INPUT' || tag === 'SELECT' || tag === 'TEXTAREA') return { value: /** @type {any} */ (el).value || '' };
    return {};
}

// JS command op codes -- must match include/arizona_js.hrl
const JS_PUSH_EVENT = 0, JS_TOGGLE = 1, JS_SHOW = 2, JS_HIDE = 3,
      JS_ADD_CLASS = 4, JS_REMOVE_CLASS = 5, JS_TOGGLE_CLASS = 6,
      JS_SET_ATTR = 7, JS_REMOVE_ATTR = 8, JS_DISPATCH_EVENT = 9,
      JS_NAVIGATE = 10, JS_FOCUS = 11, JS_BLUR = 12, JS_SCROLL_TO = 13,
      JS_SET_TITLE = 14, JS_RELOAD = 15, JS_ON_KEY = 16;

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
                const payload = cmd.length > 2 ? {...autoPayload(el, event), ...cmd[2]} : autoPayload(el, event);
                const msg = JSON.stringify([resolveTarget(el), evt, payload]);
                if (event) {
                    scheduleSend(el, event, () => { if (_connected) workerSend(msg); });
                } else {
                    workerSend(msg);
                }
                break;
            }
            case JS_TOGGLE: { const t = document.querySelector(cmd[1]); if (t) t.hidden = !t.hidden; break; }
            case JS_SHOW: { const t = document.querySelector(cmd[1]); if (t) t.hidden = false; break; }
            case JS_HIDE: { const t = document.querySelector(cmd[1]); if (t) t.hidden = true; break; }
            case JS_ADD_CLASS: { const t = document.querySelector(cmd[1]); if (t) t.classList.add(cmd[2]); break; }
            case JS_REMOVE_CLASS: { const t = document.querySelector(cmd[1]); if (t) t.classList.remove(cmd[2]); break; }
            case JS_TOGGLE_CLASS: { const t = document.querySelector(cmd[1]); if (t) t.classList.toggle(cmd[2]); break; }
            case JS_SET_ATTR: { const t = document.querySelector(cmd[1]); if (t) t.setAttribute(cmd[2], cmd[3]); break; }
            case JS_REMOVE_ATTR: { const t = document.querySelector(cmd[1]); if (t) t.removeAttribute(cmd[2]); break; }
            case JS_DISPATCH_EVENT: { document.dispatchEvent(new CustomEvent(cmd[1], { detail: cmd[2] || {} })); break; }
            case JS_NAVIGATE: { const path = cmd[1]; const opts = cmd[2] || {}; if (opts.replace) { history.replaceState(null, '', path); } else { history.pushState(null, '', path); } workerSend(JSON.stringify(['navigate', { path }])); if (_worker) _worker.postMessage([3, path]); break; }
            case JS_FOCUS: { const t = document.querySelector(cmd[1]); if (t) /** @type {HTMLElement} */ (t).focus(); break; }
            case JS_BLUR: { const t = document.querySelector(cmd[1]); if (t) /** @type {HTMLElement} */ (t).blur(); break; }
            case JS_SCROLL_TO: { const t = document.querySelector(cmd[1]); if (t) t.scrollIntoView(cmd[2] || { behavior: 'smooth' }); break; }
            case JS_SET_TITLE: { document.title = cmd[1]; break; }
            case JS_RELOAD: { location.reload(); break; }
            case JS_ON_KEY: { const f = cmd[1]; const lk = event && /** @type {any} */ (event).key ? /** @type {any} */ (event).key.toLowerCase() : ''; if (Array.isArray(f) ? f.includes(lk) : new RegExp(f).test(lk)) executeJS(el, event, cmd[2]); break; }
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
    const debounceEvent = isNaN(debounceMs) && debounceAttr !== '' ? debounceAttr : '';
    const throttleMs = parseInt(el.getAttribute('az-throttle') || '', 10);
    if (!debounceMs && !throttleMs && !debounceEvent) { sendFn(); return; }
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
        const fd = new FormData(/** @type {HTMLFormElement} */(form));
        /** @type {Object<string, string|string[]>} */
        const data = {};
        for (const [k, v] of fd.entries()) {
            if (k in data) {
                const prev = data[k];
                data[k] = Array.isArray(prev)
                    ? prev.concat(/** @type {string} */(v))
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
            workerSend(JSON.stringify([resolveTarget(form), azChange, fields]));
        }
    }
    _savedForms.clear();
}

/**
 * Delegate a DOM event type via document-level listener.
 * Key filtering is handled by the JS_ON_KEY command inside executeJS,
 * not by attribute name suffixes.
 * @param {string} eventType
 */
function handleEvent(eventType) {
    document.addEventListener(eventType, (e) => {
        const el = /** @type {Element} */ (e.target).closest('[az-' + eventType + ']');
        if (!el || !_connected) return;
        if (el.hasAttribute('az-prevent-default')) e.preventDefault();
        const raw = el.getAttribute('az-' + eventType);
        if (!raw) return;
        executeJS(el, e, JSON.parse(raw));
    });
}

/**
 * Bootstrap: spawn Worker, set up document-level event delegation for all
 * supported event types, and wire up form submission and drag-and-drop.
 * @param {string} endpoint
 */
function connect(endpoint, params = {}) {
    // Event delegation -- one document listener per event type, attributes on
    // elements opt them in (e.g. az-click="increment", az-keydown-enter="submit").
    handleEvent('click');
    handleEvent('change');
    handleEvent('input');
    handleEvent('keydown');
    handleEvent('keyup');
    handleEvent('focusin');
    handleEvent('focusout');

    // Form submission: flush any pending debounced/throttled inputs first so
    // the server sees final values, then execute JS commands from az-submit.
    // az-form-reset opts in to clearing the form after submit.
    document.addEventListener('submit', (e) => {
        const form = /** @type {Element} */ (e.target).closest('[az-submit]');
        if (!form || !_connected) return;
        e.preventDefault();
        form.querySelectorAll('[az-debounce],[az-throttle]').forEach(flushTimer);
        const raw = form.getAttribute('az-submit');
        if (raw) executeJS(form, e, JSON.parse(raw));
        if (form.hasAttribute('az-form-reset')) /** @type {HTMLFormElement} */ (form).reset();
    });

    // SPA navigation: az-navigate (boolean attr) on <a> triggers client-side
    // navigation. The path is read from href. Sends ["navigate", {path}]
    // to the server, which renders the new page and sends OP_REPLACE on the
    // content slot.
    document.addEventListener('click', (e) => {
        const el = /** @type {Element} */ (e.target).closest('[az-navigate]');
        if (!el || !_connected) return;
        const path = el.getAttribute('href');
        if (!path || path === location.pathname) return;
        e.preventDefault();
        history.pushState(null, '', path);
        workerSend(JSON.stringify(['navigate', { path }]));
        if (_worker) _worker.postMessage([3, path]);
    });

    // Browser back/forward: send navigate on popstate so the server
    // renders the correct page for the current URL.
    window.addEventListener('popstate', () => {
        if (!_connected) return;
        const path = location.pathname;
        workerSend(JSON.stringify(['navigate', { path }]));
        if (_worker) _worker.postMessage([3, path]);
    });

    // Drag-and-drop: uses az-key on draggable items and az-drop on the
    // container. dragstart stores the item's key; drop executes the az-drop
    // command with auto-collected {data_transfer, drop_index} payload.
    document.addEventListener('dragstart', (e) => {
        const keyEl = /** @type {Element} */ (e.target).closest('[az-key]');
        if (keyEl && e.dataTransfer) e.dataTransfer.setData('text/plain', keyEl.getAttribute('az-key') || '');
    });
    document.addEventListener('dragover', (e) => {
        if (/** @type {Element} */ (e.target).closest('[az-key]')) e.preventDefault();
    });
    document.addEventListener('drop', (e) => {
        const dropTarget = /** @type {Element} */ (e.target).closest('[az-key]');
        if (!dropTarget) return;
        e.preventDefault();
        const container = dropTarget.closest('[az-drop]');
        if (!container || !_connected) return;
        const raw = container.getAttribute('az-drop');
        if (!raw) return;
        executeJS(container, e, JSON.parse(raw));
    });

    // Build full WS URL -- Worker can't access location.*
    const protocol = location.protocol === 'https:' ? 'wss:' : 'ws:';
    const path = encodeURIComponent(location.pathname);
    const merged = { ...Object.fromEntries(new URLSearchParams(location.search)), ...params };
    const paramsStr = Object.keys(merged).length ? '&params=' + encodeURIComponent(JSON.stringify(merged)) : '';
    const wsUrl = protocol + '//' + location.host + endpoint + '?path=' + path + paramsStr;

    // Spawn the Worker -- co-located with this script
    const baseUrl = new URL(/* @vite-ignore */ '.', import.meta.url).href;
    _worker = new Worker(baseUrl + 'arizona-worker.min.js', { type: 'module' });

    /** @type {Function|null} */
    let _onmessageHook = null;

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
                if (msg[1] === 4500) {
                    location.reload();
                    return;
                }
                if (msg[1] !== 1000) saveFormState();
                break;
            }
        }
    };

    // Send connect message to Worker
    _worker.postMessage([0, wsUrl]);

    // window._ws proxy for E2E test compatibility
    if (typeof window !== 'undefined') {
        /** @type {any} */ (window)._ws = {
            get readyState() { return _connected ? 1 : 3; },
            /** @param {string} data */ send(data) { workerSend(data); },
            /** @param {number} [code] */ close(code) { if (_worker) _worker.postMessage([2, code || 1000]); },
            set onmessage(fn) { _onmessageHook = fn; },
            get onmessage() { return _onmessageHook; },
        };
    }
}

export { connect, applyOps, applyEffects, executeJS, resolveEl, pushEvent, pushEventTo, OP, hooks, mountHooks, saveFormState, restoreFormState };
