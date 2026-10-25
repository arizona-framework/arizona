/**
 * A framework slot-opening comment: `az:` followed by an az of the shape the
 * compiler emits.
 *
 * A framework az is a run of `-`-joined upper-case alphanumeric segments, with
 * an optional `:<slot>` suffix for a second content slot on one element. The
 * base unit is `<Fp>-<id>` (a base-36 `phash2` fingerprint and a numeric id),
 * but a nested one is NOT simply those units concatenated:
 * `arizona_template:scope_slot/2` namespaces a slot's value by the slot az made
 * colon-free (`:` -> `-`), so a slot that is not the FIRST content slot of its
 * element contributes a bare numeric segment -- `<Fp>-0:1` becomes the prefix
 * `<Fp>-0-1`, and every marker inside it reads `<Fp>-0-1-<Fp2>-<id>`. Requiring
 * strict `<Fp>-<id>` alternation rejected exactly those, so the walker below
 * stopped counting them as openers and under-walked the slot: a clear op emptied
 * only up to the first nested closer, leaving the rest of the content stranded
 * outside the slot to be drawn a second time by the next fill.
 *
 * The segment run is what separates a real marker from user-authored bytes,
 * exactly as `arizona_html:scope_static/3` states for the server side: static
 * text is spliced verbatim and `?raw` splices trusted stored HTML, so a comment
 * written by a CMS or markdown pipeline reaches slot content as ordinary bytes.
 * Matching bare `az:` would let such a decoy pose as a nested opener and make the
 * walker swallow the slot's own closer.
 *
 * Deliberately NOT the exact grammar (`<Fp>-<id>` pairs each optionally followed
 * by a colon-free slot number). That grammar is ambiguous once a fingerprint is
 * all digits -- `-1-1` tiles as one pair or as a bare number plus the head of the
 * next -- so a regex for it backtracks exponentially on a non-matching run of
 * numeric segments, which is attacker-reachable through `?raw` stored HTML. One
 * segment per repetition is unambiguous and linear, and pays for it only by
 * admitting a decoy that already had to spell `az:` plus dash-joined upper-case
 * segments.
 */
export const MARKER_OPEN = /^az:[0-9A-Z]+(?:-[0-9A-Z]+)+(?::\d+)?$/;

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
export function findSlotEnd(startMarker) {
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
export function forEachNodeInSlot(startMarker, fn) {
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
export function forEachElementBetweenMarkers(startMarker, fn) {
    forEachNodeInSlot(startMarker, (node) => {
        if (node.nodeType === 1) fn(/** @type {Element} */ (node));
    });
}

/**
 * Dynamic text is bracketed by HTML comment markers: <!--az:X-->...<!--/az-->.
 * This lets us update inline text without a wrapper element. We scan direct
 * children only (nodeType 8 = Comment) to find the opening marker.
 * @param {Element} el
 * @param {string} az
 * @returns {Comment|null}
 */
export function findMarker(el, az) {
    // `want` is built once, not once per child: this walks every child of the container
    // (a stream container has as many as it has items) and is the innermost helper of
    // the marker lookups that run per op.
    const want = `az:${az}`;
    for (let node = el.firstChild; node !== null; node = node.nextSibling) {
        if (node.nodeType === 8 && /** @type {Comment} */ (node).data === want) {
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

const SVG_NS = 'http://www.w3.org/2000/svg';

/**
 * Parse `html` into a fragment, using the element the nodes will land INSIDE as
 * the parsing context.
 *
 * A detached `<template>` always parses in HTML context, so a `<rect>` inserted
 * by a patch comes out an `HTMLUnknownElement` in the XHTML namespace and renders
 * nothing -- with every attribute correct, which is why it reads as bad data
 * rather than a DOM bug. SSR markup escapes this because the page parser
 * namespace-adjusts inside `<svg>`; only nodes the diff creates are affected, so
 * it surfaces exactly when a list is empty at mount and fills in later.
 *
 * Parsing into a shallow clone of the real parent fixes it without a tag list:
 * the fragment-parsing algorithm takes its namespace from the context element,
 * and `<a>`/`<title>`/`<script>` are legal in both namespaces so the tag alone
 * cannot decide. Cloning the ACTUAL parent (rather than a synthetic `<svg>`
 * container) is what makes `<foreignObject>` come out right too: it sits in the
 * SVG namespace but its children are HTML, and using it as the context yields
 * exactly that, with no special case.
 *
 * HTML parents keep the `<template>` path. Cloning is scoped to SVG because a
 * shallow clone upgrades a custom element, running its constructor for a node
 * that exists only to be thrown away.
 *
 * @param {Node|null} parent -- the node the parsed content is inserted into
 * @param {string} html
 * @returns {DocumentFragment}
 */
export function parseFragmentIn(parent, html) {
    const doc = parent?.ownerDocument ?? document;
    if (parent instanceof Element && parent.namespaceURI === SVG_NS) {
        const ctx = /** @type {Element} */ (parent.cloneNode(false));
        ctx.innerHTML = html;
        const frag = doc.createDocumentFragment();
        frag.append(...ctx.childNodes);
        return frag;
    }
    const tpl = doc.createElement('template');
    tpl.innerHTML = html;
    return tpl.content;
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
 * @param {DocumentFragment|null} [frag] -- `value` already parsed (see applyTextOp)
 * @returns {boolean}
 */
export function updateMarkerContent(startMarker, value, isHtml, frag = null) {
    const doc = startMarker.ownerDocument;
    // Scalar fast path: a lone text node is updated in place (no childList churn,
    // which would revert an in-progress scroll on WebKitGTK -- see updateLoneTextNode).
    if (!isHtml && updateLoneTextNode(startMarker, value)) return true;
    if (!forEachNodeInSlot(startMarker, (node) => node.remove())) return false;
    // Insert new content before the closing marker
    if (isHtml) {
        startMarker.after(frag ?? parseFragmentIn(startMarker.parentNode, value));
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
export function setMarkerText(startMarker, value) {
    const str = value == null ? '' : String(value);
    // Same in-place fast path as updateMarkerContent: avoid childList churn that
    // reverts an in-progress scroll on WebKitGTK (see updateLoneTextNode).
    if (updateLoneTextNode(startMarker, str)) return true;
    if (!forEachNodeInSlot(startMarker, (node) => node.remove())) return false;
    startMarker.after(startMarker.ownerDocument.createTextNode(str));
    return true;
}
