import { applyRemAttrOp, applySetAttrOp } from './arizona-attrs.js';
import { closeOrphanedPipWindows, findViewRoot } from './arizona-docs.js';
import {
    anyHookDefs,
    destroyChildHooks,
    destroyHooks,
    mountHook,
    mountHooks,
    notifyUpdated,
} from './arizona-hooks.js';
import { _pendingScroll, applyScroll, setPendingScroll } from './arizona-scroll.js';
import {
    findMarker,
    findSlotEnd,
    forEachElementBetweenMarkers,
    forEachNodeInSlot,
    MARKER_OPEN,
    parseFragmentIn,
    updateMarkerContent,
} from './arizona-slots.js';

/**
 * Op codes -- each op is a flat array: [opcode, "viewId:az", ...args].
 * Codes match the server-side OP_* constants in include/arizona_wire.hrl, 3 included: it is
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
export const OP = {
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

/**
 * Whether this op batch is the one a pending transition is waiting for. 'replace'
 * matches only a page-swap OP_REPLACE (so a concurrent text/attr tick does not
 * consume the intent); 'any' matches any non-empty batch (a push_event result).
 * @param {Array<Array<*>>} ops
 * @param {'replace'|'any'} kind
 * @returns {boolean}
 */
export function opsMatchTransition(ops, kind) {
    if (!ops?.length) return false;
    return kind === 'replace' ? ops.some((op) => op[0] === OP.REPLACE) : true;
}

// ---------------------------------------------------------------------------
// Kept az-nodiff elements. An az-nodiff element is rendered by the server once and then owned
// by the client (a widget mounted from its `az-hook`). When something around it is
// re-rendered wholesale -- a container `OP_TEXT`, or the `OP_REPLACE` that resyncs
// a reconnected view -- the new markup holds a fresh copy of every az-nodiff element in it.
// The server names each az-nodiff element's identity as the value of the `az-nodiff` attribute
// on its root (built by `arizona_template:nodiff_token/3` from the fingerprint,
// branch and `az_key` the diff keeps an az-nodiff element by); a bare `az-nodiff` marks
// client-owned markup that cannot be told apart from its siblings (an unkeyed list
// item) and is never kept. Here an az-nodiff element whose old and new copies sit at the
// same place under the same name is carried over: the old DOM moves into the new
// markup in place of the new copy, and its hooks see neither `destroyed()` nor
// `mounted()`. Anything else is replaced as before.
//
// "The same place" is read off the DOM, where the addresses already are: the slot marker enclosing
// each element on the way up, and the `az-key` / view id of the az-nodiff element and each
// ancestor. A slot holds one value, so two az-nodiff elements under the same name and place are
// unkeyed items of a plain list -- which nothing can match up, so they are never kept.
// ---------------------------------------------------------------------------

/** No az-nodiff element kept. Shared, never written. */
const NO_KEPT = /** @type {ReadonlySet<Element>} */ (new Set());

const NODIFF_ATTR = 'az-nodiff';

/**
 * Cheap pre-check: can this markup name an az-nodiff element at all? Keeps the common
 * re-render on its original path.
 * @param {string} html
 * @returns {boolean}
 */
function mayNameNodiff(html) {
    return html.includes(` ${NODIFF_ATTR}="`);
}

/**
 * Carry the az-nodiff elements in `oldNodes` (the content about to be replaced) over into
 * `frag` (its replacement, not yet inserted): each new az-nodiff element with an old
 * counterpart of the same identity is swapped for it. Returns the kept az-nodiff elements.
 *
 * An az-nodiff element holding a live view is not kept on a resync: the reconnect mounted that
 * view afresh, so the DOM it left behind no longer matches the server's state.
 * @param {ArrayLike<Node>} oldNodes
 * @param {DocumentFragment} frag
 * @param {boolean} resync
 * @returns {ReadonlySet<Element>}
 */
function keepNodiff(oldNodes, frag, resync) {
    const tops = Array.from(oldNodes);
    // An empty old slot (a conditional switching from `<<>>` to an az-nodiff element)
    // holds nothing to keep, and has no node to find its parent from.
    if (tops.length === 0) return NO_KEPT;
    const fresh = nodiffByIdentity(Array.from(frag.childNodes), frag);
    if (fresh.size === 0) return NO_KEPT;
    const old = nodiffByIdentity(tops, /** @type {ParentNode} */ (tops[0].parentNode));
    const kept = new Set();
    for (const [id, news] of fresh) {
        const olds = old.get(id);
        // Two az-nodiff elements under one name at one place are items the server could not
        // tell apart (it re-renders a list wholesale for exactly that reason), so
        // which old one a new one is cannot be known: keep neither.
        if (olds?.length !== 1 || news.length !== 1) continue;
        const o = olds[0];
        if (resync && (o.hasAttribute('az-view') || o.querySelector('[az-view]'))) continue;
        news[0].replaceWith(o);
        kept.add(o);
    }
    return kept;
}

/**
 * The outermost az-nodiff elements under the sibling range `tops` (children of `parent`),
 * grouped by identity in document order. An az-nodiff element inside another is part of it
 * and is not listed. One query over `parent`, not one per top: a list re-render
 * has as many tops as items.
 * @param {Node[]} tops
 * @param {ParentNode} parent
 * @returns {Map<string, Element[]>}
 */
function nodiffByIdentity(tops, parent) {
    /** @type {Map<string, Element[]>} */
    const byId = new Map();
    /** @type {Map<Node, string>} */
    const slots = new Map();
    slotsOf(tops, slots);
    // Every marked root, bare ones too: a named az-nodiff element inside one is part of it.
    for (const r of parent.querySelectorAll(`[${NODIFF_ATTR}]`)) {
        const id = nodiffIdentity(r, parent, slots);
        if (id === null) continue;
        const list = byId.get(id);
        if (list) list.push(r);
        else byId.set(id, [r]);
    }
    return byId;
}

/**
 * An az-nodiff element's identity: its name plus, for it and each ancestor below `parent`,
 * the slot it sits in and its `az-key` / view id -- the az-nodiff element's own included,
 * since a child view whose root is the az-nodiff element is that view only under its id.
 * Null when the az-nodiff element is bare (never kept), when an ancestor is itself marked
 * (the outer one carries this one), or when the az-nodiff element is outside the range
 * (`slots` holds the range's tops).
 * @param {Element} marked
 * @param {ParentNode} parent
 * @param {Map<Node, string>} slots
 * @returns {string|null}
 */
function nodiffIdentity(marked, parent, slots) {
    let id = /** @type {string} */ (marked.getAttribute(NODIFF_ATTR));
    if (id === '') return null;
    /** @type {Element} */
    let node = marked;
    for (;;) {
        if (node !== marked && node.hasAttribute(NODIFF_ATTR)) return null;
        const key = node.getAttribute('az-key') ?? '';
        const view = node.hasAttribute('az-view') ? node.id : '';
        id = `${key}#${view}/${id}`;
        const up = /** @type {Element} */ (node.parentNode);
        if (up === parent) {
            const slot = slots.get(node);
            return slot === undefined ? null : `${slot}>${id}`;
        }
        if (!slots.has(node)) slotsOf(Array.from(up.childNodes), slots);
        id = `${slots.get(node)}>${id}`;
        node = up;
    }
}

/**
 * Record, for each element in the sibling range `nodes`, the az of the innermost
 * slot marker pair enclosing it within that range ('' for none).
 * @param {Node[]} nodes
 * @param {Map<Node, string>} slots
 */
function slotsOf(nodes, slots) {
    /** @type {string[]} */
    const open = [];
    for (const n of nodes) {
        if (n.nodeType === 8) {
            const data = /** @type {Comment} */ (n).data;
            if (data === '/az') open.pop();
            else if (MARKER_OPEN.test(data)) open.push(data.slice(3));
        } else if (n.nodeType === 1) {
            slots.set(n, open.length > 0 ? open[open.length - 1] : '');
        }
    }
}

/**
 * `mountHooks` over `root`, skipping the kept az-nodiff elements: their hooks never left,
 * and what is inside them is the client's own markup.
 * @param {Element} root
 * @param {ReadonlySet<Element>} kept
 */
function mountHooksOutside(root, kept) {
    if (kept.size === 0) {
        mountHooks(root);
        return;
    }
    if (!anyHookDefs()) return;
    // Up from each hook to `root`: a kept az-nodiff element on the way means it is inside one.
    // Each hook costs its depth, where asking every kept az-nodiff element `contains()` cost
    // the number of az-nodiff elements -- quadratic over a list of widgets.
    const inKept = (/** @type {Element} */ e) => {
        for (let n = /** @type {Element|null} */ (e); n; n = n === root ? null : n.parentElement)
            if (kept.has(n)) return true;
        return false;
    };
    if (root.hasAttribute('az-hook') && !inKept(root)) mountHook(root);
    for (const e of root.querySelectorAll('[az-hook]')) if (!inKept(e)) mountHook(e);
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
        // A re-rendered slot keeps the az-nodiff elements its new content names again:
        // they move into the new content before the old is torn down.
        let frag = null;
        let kept = NO_KEPT;
        if (isHtml && mayNameNodiff(val)) {
            /** @type {Node[]} */
            const old = [];
            if (forEachNodeInSlot(marker, (n) => old.push(n))) {
                frag = parseFragmentIn(marker.parentNode, val);
                kept = keepNodiff(old, frag, false);
            }
        }
        forEachElementBetweenMarkers(marker, destroyHooks);
        if (!updateMarkerContent(marker, val, isHtml, frag)) {
            // Undelimited slot (stored HTML carrying a marker-shaped comment with
            // no closer). Nothing was touched -- say so rather than write the
            // value somewhere arbitrary.
            console.warn(`[arizona] slot az:${az} has no closing marker; skipping`);
            return;
        }
        forEachElementBetweenMarkers(marker, (e) => mountHooksOutside(e, kept));
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
 * - `streams` is the batch's StreamCache (`newStreamCache`): key -> item maps per
 *   container and slot span, plus the slot spans themselves, built on first
 *   lookup and maintained by the batch's INSERT/REMOVE ops -- an N-op reorder is
 *   O(N), not O(N^2) scans (mirrors applyListPatch's one-snapshot-per-batch
 *   shape).
 *
 * `resync` marks the batch that resyncs a reconnected view (the first frame after
 * a reconnect): its `OP_REPLACE` keeps the az-nodiff elements the new render names
 * again (`keepNodiff`). A navigate's `OP_REPLACE` keeps none -- it is a new page.
 * @param {Array<Array<*>>} ops
 * @param {boolean} [resync]
 */
export function applyOps(ops, resync = false) {
    let didReplace = false;
    /** @type {Map<string, Resolution>} */
    const els = new Map();
    const streams = newStreamCache();
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
                // that rewrites the element would destroy content outside the slot
                // -- see MARKER_UNSAFE_OPS. Refuse it as an unresolved target is
                // refused.
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
                    // Hold the replacement's roots BEFORE inserting them: a navigate mounts a
                    // view whose id differs, so re-resolving `op[1]` (which names the OUTGOING
                    // view) after the swap finds nothing and the destination's hooks would
                    // never mount. Same parse-then-mount shape as OP_INSERT.
                    const frag = parseFragmentIn(el.parentNode, op[2]);
                    // Kept az-nodiff elements move into the replacement first, so the teardown below
                    // no longer reaches them.
                    const kept = resync ? keepNodiff([el], frag, true) : NO_KEPT;
                    destroyHooks(el);
                    const added = Array.from(frag.children);
                    el.replaceWith(frag);
                    for (const e of added) mountHooksOutside(e, kept);
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
                    removeItemEl(el, op[2], streams, az);
                    break;
                case OP.ITEM_PATCH:
                    applyItemPatch(el, op[2], op[3], streams, az);
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
        setPendingScroll(null);
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
 * the slot. `replaceWith` / `remove` on the parent takes the slot's static
 * siblings with it, and when the parent is the live root, the whole view. The
 * server never addresses these to a marker-anchored slot (the diff emits the
 * marker-aware `?OP_TEXT`), so this is a guard against a stray op, not a
 * supported path.
 *
 * The stream placement ops (`INSERT`, `MOVE`) are NOT here: on a marker-only hit
 * the slot's markers are direct children of the resolved parent, so
 * `insertItemEl` / `moveItemEl` place inside the span through `slotBounds`.
 * @type {Set<number>}
 */
const MARKER_UNSAFE_OPS = new Set([OP.REPLACE, OP.REMOVE_NODE]);

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
export function resolveEl(target) {
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
 * Boundary of a stream container's slot when the container shares its parent's
 * content slot with siblings. There the each's slot az is compound and carried by no
 * element, so `resolveEl` falls back to the BASE az -- the enclosing element -- and
 * `el` is that enclosing element, not the list. Appending or prepending to it puts the
 * item outside the slot span (after a footer, before a header) instead of in the list.
 *
 * It anchors inserts and moves, and scopes lookup by key: two streams can share `el`
 * (a second `?each` among siblings takes the compound `<El>:1` az) with overlapping
 * keys, so only the children between this slot's markers belong to it.
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
 * Per-batch stream lookup state, so a K-op batch scans a container's children
 * once rather than once per op:
 * - `keys`: a key -> item map per container AND slot scope. Two streams can share
 *   one parent element (siblings in a content-list fragment, or compound slots
 *   under one element) with overlapping keys, so a map is scoped to the stream's
 *   slot span when it has one -- otherwise an op for one stream finds the other's
 *   item.
 * - `spans`: the slot span each (container, az) resolves to. `slotBounds` walks the
 *   slot to its closing marker, so without this every lookup by key paid an O(items)
 *   walk -- and the common batch interleaves a TEXT inside every row with lookups in
 *   the list, so re-walking after each one would make it quadratic. A cached span is
 *   exact while both its markers are still children of the container: no op moves a
 *   marker, an op that rewrites the span's surroundings removes them, and whatever
 *   it inserts is balanced, so the closer pairs with the same opener. Only a found
 *   span is kept; a miss is looked up again, since a later re-render can add one.
 * @typedef {{keys: Map<Element, Map<string, Map<string, Element>>>, spans: Map<Element, Map<string, SlotSpan>>}} StreamCache
 */

/** @typedef {{start: Comment, end: Comment}} SlotSpan */

/** @returns {StreamCache} */
function newStreamCache() {
    return { keys: new Map(), spans: new Map() };
}

/**
 * The slot span of `az` within `el`, memoized per batch (see `StreamCache`).
 * @param {StreamCache|null} streams
 * @param {Element} el
 * @param {string} [az]
 * @returns {SlotSpan|null}
 */
function spanOf(streams, el, az) {
    if (!az) return null;
    if (!streams) return slotBounds(el, az);
    let byAz = streams.spans.get(el);
    if (!byAz) {
        byAz = new Map();
        streams.spans.set(el, byAz);
    }
    const hit = byAz.get(az);
    if (hit && hit.start.parentNode === el && hit.end.parentNode === el) return hit;
    const span = slotBounds(el, az);
    if (span) byAz.set(az, span);
    return span;
}

/**
 * Build a key -> element map of a container's keyed (az-key) children, in DOM
 * order, restricted to the slot span when there is one. Under a duplicate key the
 * FIRST child wins, matching what a `:scope > [az-key="..."]` query would return.
 * @param {Element} el
 * @param {SlotSpan|null} span
 * @returns {Map<string, Element>}
 */
function buildKeyMap(el, span) {
    const map = new Map();
    for (const child of keyedChildren(el, span)) {
        const k = child.getAttribute('az-key');
        if (k !== null && !map.has(k)) map.set(k, child);
    }
    return map;
}

/**
 * The cached key map of a stream, if this batch built one.
 * @param {StreamCache|null} streams
 * @param {Element} el
 * @param {string} [az]
 * @returns {Map<string, Element>|undefined}
 */
function cachedKeyMap(streams, el, az) {
    if (!streams) return undefined;
    const span = spanOf(streams, el, az);
    return streams.keys.get(el)?.get(span && az ? az : '');
}

/**
 * Look up a stream's keyed child, within the stream's slot span when it has one.
 * With a per-batch `streams` cache the children are scanned once and the map is
 * maintained by the batch's inserts/removes, so an N-op stream batch is O(N)
 * instead of one full child scan per op. A cached entry that went stale -- a
 * non-stream op (TEXT) rewrote the container's children under the map -- triggers
 * one rebuild and retry, so the cache can never return a disconnected element.
 * @param {StreamCache|null} streams
 * @param {Element} el
 * @param {string} key
 * @param {string} [az] -- the stream slot's az, scoping the lookup to its span
 * @returns {Element|null}
 */
function itemByKey(streams, el, key, az) {
    const span = spanOf(streams, el, az);
    if (!streams) {
        return keyedChildren(el, span).find((c) => c.getAttribute('az-key') === key) ?? null;
    }
    const scope = span && az ? az : '';
    let byScope = streams.keys.get(el);
    if (!byScope) {
        byScope = new Map();
        streams.keys.set(el, byScope);
    }
    let map = byScope.get(scope);
    if (!map) {
        map = buildKeyMap(el, span);
        byScope.set(scope, map);
    }
    let item = map.get(key);
    if (item === undefined || !item.isConnected) {
        map = buildKeyMap(el, span);
        byScope.set(scope, map);
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
 * @param {StreamCache|null} [streams] -- per-batch stream lookups
 * @param {string} [az] -- the op's slot az, used to anchor placement to the slot span
 */
function insertItemEl(el, key, pos, html, streams = null, az) {
    const frag = parseFragmentIn(el, html);
    const bounds = spanOf(streams, el, az);
    // Grab the keyed item from the payload BEFORE inserting it: re-querying the
    // container by key afterwards would find a PRE-EXISTING element first under
    // a duplicate key, mounting hooks on the wrong element and skipping the new
    // item's own.
    const item = Array.from(frag.children).find((e) => e.getAttribute('az-key') === key);
    // Tail placement goes before the slot's CLOSING marker when there is one, so the
    // item lands inside the list rather than after the slot's static siblings.
    const atEnd = () => (bounds ? el.insertBefore(frag, bounds.end) : el.appendChild(frag));
    if (pos === -1) {
        atEnd();
    } else {
        // Positional: the live child list, not the key map -- MOVE ops change
        // DOM order without touching the map, so only the DOM knows position.
        const children = keyedChildren(el, bounds);
        if (pos < children.length) {
            el.insertBefore(frag, children[pos]);
        } else {
            atEnd();
        }
    }
    if (item) {
        cachedKeyMap(streams, el, az)?.set(key, item);
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
 * @param {StreamCache|null} [streams] -- per-batch stream lookups
 * @param {string} [az] -- the stream slot's az
 */
function removeItemEl(el, key, streams = null, az) {
    const item = itemByKey(streams, el, key, az);
    if (!item) {
        console.warn(`[arizona] stream item az-key="${key}" not found for remove`);
        return;
    }
    removeEl(item);
    cachedKeyMap(streams, el, az)?.delete(key);
    notifyUpdated(el);
}

/**
 * Move a keyed child after another keyed element within a container (or prepend
 * if afterKey is null). Preserves form state, focus, scroll position, CSS
 * animations, and hook instances.
 * @param {Element} el -- container element
 * @param {string} key
 * @param {string|null} afterKey -- key of preceding sibling, or null for prepend
 * @param {StreamCache|null} [streams] -- per-batch stream lookups
 * @param {string} [az] -- the op's slot az, used to anchor placement to the slot span
 */
function moveItemEl(el, key, afterKey, streams = null, az) {
    const item = itemByKey(streams, el, key, az);
    if (!item) {
        console.warn(`[arizona] stream item az-key="${key}" not found for move`);
        return;
    }
    // Head and tail placement are relative to the slot span when there is one:
    // `el.prepend` would put the item before the slot's static siblings (a header),
    // and `el.appendChild` after them (a footer).
    //
    // Resolved lazily: `slotBounds` walks every child of the container looking for the
    // slot's marker, and the dominant move -- one with an `afterKey` whose element is
    // present -- never reads it. Computing it up front made a reorder pay that walk
    // once per moved item for a result it discarded.
    if (afterKey === null) {
        const bounds = spanOf(streams, el, az);
        if (bounds) bounds.start.after(item);
        else el.prepend(item);
    } else {
        const ref = itemByKey(streams, el, afterKey, az);
        if (ref) {
            ref.after(item);
        } else {
            const bounds = spanOf(streams, el, az);
            if (bounds) el.insertBefore(item, bounds.end);
            else el.appendChild(item);
        }
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
 * @param {StreamCache|null} [streams] -- per-batch stream lookups
 * @param {string} [az] -- the stream slot's az
 */
function applyItemPatch(container, key, innerOps, streams = null, az) {
    const item = itemByKey(streams, container, key, az);
    if (!item) {
        console.warn(`[arizona] stream item az-key="${key}" not found for patch`);
        return;
    }
    applyItemOps(item, innerOps, streams);
}

/**
 * Resolve an az within an item, mirroring `resolveOpTarget`'s three arms: an
 * element carrying the az (the item itself or a descendant), for a compound
 * "X:n" az the base element, then the slot's comment marker -- whose PARENT is
 * returned flagged `marker: true`, since that element is not the target. A miss
 * is null: falling back to the item would let a marker-aware op rewrite the whole
 * item (an `OP_TEXT` whole-element write) or place a node outside the slot.
 * @param {Element} parent
 * @param {string} az
 * @returns {Resolution|null}
 */
function resolveInnerEl(parent, az) {
    if (parent.getAttribute('az') === az) return { el: parent, marker: false };
    const el = parent.querySelector(`[az="${az}"]`);
    if (el) return { el, marker: false };
    const j = az.indexOf(':');
    if (j !== -1) {
        const baseAz = az.substring(0, j);
        if (parent.getAttribute('az') === baseAz) return { el: parent, marker: false };
        const base = parent.querySelector(`[az="${baseAz}"]`);
        if (base) return { el: base, marker: false };
    }
    const markerParent = findMarkerDeep(parent, az)?.parentElement;
    return markerParent ? { el: markerParent, marker: true } : null;
}

/**
 * Apply inner ops to an item element. Ops arrive pre-resolved from the Worker.
 * Per-op behaviour matches `applyOps` via the shared `apply*Op` helpers,
 * differing only in element resolution: bare `az` resolved against `item` via
 * `resolveInnerEl/2` (an unresolvable target is skipped, never retargeted at the
 * item), with `REMOVE_NODE` looked up directly.
 *
 * An op whose head is NOT an op code is the `[ChildViewId, ChildOps]` child-view
 * wrapper: `arizona_socket:flatten_ops/2` unwraps it only at top level, so a
 * `?stateful` child inside a stream `?each` item ships it here still wrapped. Its
 * ops belong to the child's own view root, resolved by id like the top level does
 * for `viewId:az` -- and they are the same bare-`az` shape, so they recurse
 * through this function (which also covers a grandchild wrapper).
 * @param {Element} item
 * @param {Array<Array<*>>} innerOps
 * @param {StreamCache|null} [streams] -- per-batch stream lookups
 */
function applyItemOps(item, innerOps, streams = null) {
    // Same memo as `applyOps`' `els`, for the same reason: two ops in one item batch
    // commonly target the same az (a TEXT plus a SET_ATTR), and each `resolveInnerEl`
    // is a full subtree `querySelector` -- two on a miss.
    /** @type {Map<string, Resolution>} */
    const innerEls = new Map();
    /** @param {string} az @returns {Resolution|null} */
    const inner = (az) => {
        const hit = innerEls.get(az);
        // A hit is only reusable while it is still in the document (an earlier op in
        // this batch can re-render the slot that held it) and, for a marker hit,
        // while the marker itself survives -- the same check `applyOps` makes.
        if (hit?.el.isConnected && (!hit.marker || findMarker(hit.el, az))) return hit;
        const found = resolveInnerEl(item, az);
        if (found) innerEls.set(az, found);
        return found;
    };
    for (const op of innerOps) {
        // Same per-op isolation as applyOps: a throwing inner op must not abort
        // the rest of the item's patch batch.
        try {
            if (typeof op[0] !== 'number') {
                const childRoot = findViewRoot(op[0]);
                // Loud like the top-level miss: a dropped child batch reads as
                // "the child just stopped updating".
                if (childRoot) applyItemOps(childRoot, op[1], streams);
                else console.warn(`[arizona] item op child view "${op[0]}" not found; skipping`);
                continue;
            }
            const az = op[1];
            if (op[0] === OP.REMOVE_NODE) {
                const innerEl = item.querySelector(`[az="${az}"]`);
                if (innerEl) removeEl(innerEl);
                continue;
            }
            const found = inner(az);
            if (!found) {
                console.warn(`[arizona] item op ${op[0]} target "${az}" not found; skipping`);
                continue;
            }
            const el = found.el;
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
                case OP.INSERT:
                    insertItemEl(el, op[2], op[3], op[4], streams, az);
                    break;
                case OP.REMOVE:
                    removeItemEl(el, op[2], streams, az);
                    break;
                case OP.ITEM_PATCH:
                    applyItemPatch(el, op[2], op[3], streams, az);
                    break;
                case OP.MOVE:
                    moveItemEl(el, op[2], op[3], streams, az);
                    break;
                case OP.LIST_PATCH:
                    applyListPatch(el, az, op[2]);
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
                const frag = parseFragmentIn(el, sub[2]);
                const added = Array.from(frag.children);
                // Insert at position idx -- before the item currently there, or the
                // end marker for a tail insert (the server only inserts at the tail,
                // but honoring idx keeps this correct for any sub-op ordering).
                const ref = roots[sub[1]] ?? endMarker;
                ref.before(frag);
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
