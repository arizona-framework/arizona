import {
    applyRemAttrOp,
    applySetAttrOp,
    isMultipleChooser,
    pageText,
    sanitized,
    syncsProp,
} from './arizona-attrs.js';
import { resolveTarget } from './arizona-commands.js';
import { allDocs, findViewRoot } from './arizona-docs.js';
import { destroyChildHooks, destroyHooks, notifyUpdated } from './arizona-hooks.js';
import {
    findMarker,
    forEachElementBetweenMarkers,
    forEachNodeInSlot,
    setMarkerText,
} from './arizona-slots.js';
import { warnOnce } from './arizona-util.js';

/**
 * Reconstruct a content slot's comment-marker az from the element's runtime az
 * and the slot index. Mirrors arizona_html:content_slot_az/2 (slot 0 reuses the element
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
 * One bound slot: `['content', slot]`, `['attr', name]`, or `['attr', name, prefix,
 * suffix]` for an interpolated attribute (a select value's affixes are lists of
 * static parts).
 * @typedef {['content', string] | ['attr', string] | ['attr', string, string | string[], string | string[]]} LocalTarget
 */

/**
 * Write a value to one bound slot.
 * @param {Element} el
 * @param {LocalTarget} target
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
        // Held as the page holds the initial: a NUL as U+FFFD.
        const text = value == null ? '' : pageText(String(value));
        const marker = findMarker(el, localMarkerAz(el, target[1]));
        if (marker) {
            forEachElementBetweenMarkers(marker, destroyHooks);
            setMarkerText(marker, text);
        } else {
            destroyChildHooks(el);
            el.textContent = text;
        }
        notifyUpdated(el);
        return;
    }
    const name = target[1];
    if (target.length === 4 && isMultipleChooser(el, name)) {
        // A select's value beside static parts (lists, see `forEachLocal`): under
        // `multiple` each part is a pick, kept around the picks the local owns.
        applySetAttrOp(el, name, [...target[2], ...ownPicks(value), ...target[3]]);
    } else if (target.length === 4) {
        // Interpolated: recompose prefix + value + suffix (always a string attr). An
        // absent value interpolates as empty, as a content slot renders it.
        const text = affix(target[2]) + (value == null ? '' : String(value)) + affix(target[3]);
        applySetAttrOp(el, name, text);
    } else if (isMultipleChooser(el, name) && value !== false && value != null) {
        // The JSON array of the texts it picks, as the server renders it: a list holds
        // the picks, anything else is one, a string reading as JSON included.
        applySetAttrOp(el, name, ownPicks(value));
    } else if (value === false || value == null) {
        // Through the canonical writer, like a set: after user interaction the
        // dirty value/checkedness flags detach `.checked`/`.selected`/`.value` from
        // the attribute, so a bare removal would leave the control showing the old
        // state.
        applyRemAttrOp(el, name);
    } else if (value === true) {
        applySetAttrOp(el, name, '');
    } else {
        applySetAttrOp(el, name, String(value));
    }
}

/**
 * Read the current value of one bound slot from the DOM.
 * @param {Element} el
 * @param {LocalTarget} target
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
    // A multiple select holds a list: the values of the options the user sees picked,
    // but those a static part beside the local names -- they are the part's.
    if (isMultipleChooser(el, name)) {
        const fixed = target.length === 4 ? [...target[2], ...target[3]].map(pageText) : [];
        return Array.from(
            /** @type {HTMLSelectElement} */ (el).selectedOptions,
            (o) => o.value,
        ).filter((v) => !fixed.includes(v));
    }
    if (target.length === 4) {
        // Interpolated: strip the known prefix/suffix to recover the local value. A
        // control's live `value` (`syncsProp`) is what the user sees -- typed, or the
        // option they picked -- read as a whole-value binding reads it; one the
        // affixes cannot compose holds no value of the local's.
        const pre = affix(target[2]);
        const suf = affix(target[3]);
        if (name === 'value' && syncsProp(el, name)) {
            const v = /** @type {HTMLInputElement} */ (el).value;
            // The live value is sanitized, so it holds the affixes as sanitization
            // leaves them: as a prefix (its start a value's), as a suffix (its end
            // one), or as one value when the local's part sanitizes away.
            const p = sanitized(el, pre, false, true);
            const s = sanitized(el, suf, true, false);
            if (v.length >= p.length + s.length && v.startsWith(p) && v.endsWith(s))
                return v.slice(p.length, v.length - s.length);
            return v === sanitized(el, pre + suf, false, false) ? '' : null;
        }
        const v = el.getAttribute(name) ?? '';
        return v.slice(pre.length, v.length - suf.length);
    }
    // The form-control properties hold what the user sees once they interacted
    // (the attributes are only the defaults by then) -- the same three
    // applySetAttrOp/applyRemAttrOp keep in step on a write.
    if (syncsProp(el, name)) return /** @type {any} */ (el)[name];
    if (!el.hasAttribute(name)) return false;
    const v = el.getAttribute(name);
    return v === '' ? true : v;
}

/**
 * The picks a local value owns in a multiple select's JSON array, as the server reads
 * them: a list is its picks, `false`/`null` none, anything else one; `true` renders a
 * bare attribute, the empty text.
 * @param {*} value
 * @returns {string[]}
 */
function ownPicks(value) {
    if (value === false || value == null) return [];
    return (Array.isArray(value) ? value : [value]).map((p) => (p === true ? '' : String(p)));
}

/**
 * An affix as one text: a select value's static parts arrive as a list (one pick each
 * under `multiple`), any other attribute's as the text itself. It is the text the page
 * holds (the server escapes a CR as `&#13;`), so the joined parts are exact text, a
 * NUL as U+FFFD, as the server compares them.
 * @param {string | string[]} a
 * @returns {string}
 */
function affix(a) {
    return pageText(Array.isArray(a) ? a.join('') : a);
}

/**
 * A parsed `az-local` descriptor: content slots, attribute bindings, and
 * interpolated-attribute affixes (a select value's as lists of static parts).
 * @typedef {{c?: Object<string, string>, a?: Object<string, string>, ap?: Object<string, [string | string[], string | string[]]>}} LocalDesc
 */

// Parsed `az-local` descriptors, cached per element. NOT a persistent index --
// discovery still queries the live DOM every call; only the JSON.parse of each
// element's descriptor is memoized, with the text it was parsed from: the server
// writes the attribute once at SSR and never diffs a ?local slot, but a
// `set_attr` effect or `?raw` markup can write it too, with any value. A
// re-rendered slot is a NEW element (a fresh cache key) and dropped elements fall
// out of the WeakMap with GC.
/** @type {WeakMap<Element, {raw: string, desc: LocalDesc|null}>} */
const _localDescs = new WeakMap();

/**
 * The `az-local` descriptor of `el`, parsed once per value; `null` when it has
 * none, or one that is not JSON or not of the descriptor's shape -- skipped with
 * a warning, so the locals after it are still served.
 * @param {Element} el
 * @returns {LocalDesc|null}
 */
function localDesc(el) {
    const raw = el.getAttribute('az-local');
    if (!raw) return null;
    const cached = _localDescs.get(el);
    if (cached?.raw === raw) return cached.desc;
    /** @type {LocalDesc|null} */
    let desc = null;
    let fault = null;
    try {
        const parsed = JSON.parse(raw);
        if (isLocalDesc(parsed)) desc = parsed;
        else fault = 'not a ?local descriptor';
    } catch (err) {
        fault = err instanceof Error ? err.message : String(err);
    }
    // Once per page load, and not the value: it can be long, and app data.
    if (fault !== null) warnOnce(`[arizona] an az-local was skipped: ${fault}`, 'az-local');
    _localDescs.set(el, { raw, desc });
    return desc;
}

/** @param {*} v @returns {boolean} a JSON object, not an array or null */
const isObject = (v) => typeof v === 'object' && v !== null && !Array.isArray(v);

/** @param {*} v @returns {boolean} an object of keys, as `c` and `a` hold */
const isKeyMap = (v) => isObject(v) && Object.values(v).every((k) => typeof k === 'string');

/** @param {*} v @returns {boolean} an affix: text, or a select value's static parts */
const isAffix = (v) =>
    typeof v === 'string' || (Array.isArray(v) && v.every((p) => typeof p === 'string'));

/**
 * Whether `v` has the shape the server writes (`LocalDesc`): a value of any other
 * shape (a `set_attr` effect or `?raw` markup can write one) would throw partway
 * through a scan, leaving the locals after it unserved.
 * @param {*} v
 * @returns {v is LocalDesc}
 */
function isLocalDesc(v) {
    return (
        isObject(v) &&
        (v.c === undefined || isKeyMap(v.c)) &&
        (v.a === undefined || isKeyMap(v.a)) &&
        (v.ap === undefined ||
            (isObject(v.ap) &&
                Object.values(v.ap).every(
                    (p) => Array.isArray(p) && p.length === 2 && isAffix(p[0]) && isAffix(p[1]),
                )))
    );
}

/**
 * The value of the `?local` a select's `value` is bound to, read as the select holds it
 * now, before its `multiple` flips (`carryChooserLocal`); `null` for a value that is no
 * local's, which the server renders again in the new form itself.
 * @param {HTMLSelectElement} sel
 * @returns {[LocalTarget, *] | null}
 */
export function heldChooserLocal(sel) {
    const parsed = localDesc(sel);
    if (parsed?.a?.value === undefined) return null;
    const aff = parsed.ap?.value;
    /** @type {LocalTarget} */
    const target = aff ? ['attr', 'value', aff[0], aff[1]] : ['attr', 'value'];
    return [target, readLocalValue(sel, target)];
}

/**
 * Write the value `heldChooserLocal` read back once `multiple` flipped: the server never
 * renders a local again, and the value means one text or a pick per part by `multiple`.
 * A single select holds one pick, the last, as the browser keeps the last selected
 * option when `multiple` goes; no pick is no value.
 * @param {HTMLSelectElement} sel
 * @param {[LocalTarget, *]} held
 */
export function carryChooserLocal(sel, [target, value]) {
    const single = Array.isArray(value) && !sel.multiple;
    writeLocalValue(sel, target, single ? (value[value.length - 1] ?? null) : value);
}

/**
 * Visit each bound slot for `key` under `root`. When `viewId` is non-null only
 * slots whose nearest view is `viewId` are visited (per-view isolation). The
 * callback may return `true` to stop the scan (first-match reads); the return
 * value says whether it did.
 * @param {Element|Document} root
 * @param {string} key
 * @param {string|null} viewId
 * @param {(el: Element, target: LocalTarget) => boolean|void} fn
 * @returns {boolean}
 */
function forEachLocal(root, key, viewId, fn) {
    /** @param {Element} el @returns {boolean} */
    const visit = (el) => {
        const parsed = localDesc(el);
        if (!parsed) return false;
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
                    /** @type {LocalTarget} */
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
export function set(viewId, key, value) {
    const root = findViewRoot(viewId);
    if (!root) return;
    forEachLocal(root, key, viewId, (el, target) => writeLocalValue(el, target, value));
}

/**
 * Set a client-owned slot (`?local`) in every view on the page (document-wide).
 * @param {string} key
 * @param {*} value
 */
export function setAll(key, value) {
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
export function get(a, b) {
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
