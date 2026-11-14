import { notifyUpdated } from './arizona-hooks.js';
import { host } from './arizona-host.js';
import { carryChooserLocal, heldChooserLocal } from './arizona-locals.js';

/** @type {HTMLTemplateElement | null} */
let rawAttrTpl = null;

/**
 * Decode a `?raw` attribute value. SSR splices it verbatim inside `name="..."`, so the
 * page's value is what the HTML parser made of that text (`&amp;` -> `&`); setting the
 * markup with `setAttribute` would keep the entities literal. Parsing it in the same
 * double-quoted context, inside an inert template, yields the value SSR produced.
 *
 * No DOM API decodes character references without a parse, and the attribute-context
 * rules (a legacy entity without `;` is left alone before `=` or an alphanumeric) rule
 * out an RCDATA decode or a hand-written table, so the parse stays. Every `"` becomes
 * `&quot;` first: in the double-quoted value state that is the only character that ends
 * the value, so the text can never leave the attribute and the parse can only decode
 * character references. A bare `"` is invalid in this context (at SSR it would end the
 * value), so the live path shows it literally instead of breaking out.
 * @param {string} raw
 * @returns {string}
 */
function decodeRawAttr(raw) {
    if (!rawAttrTpl) rawAttrTpl = document.createElement('template');
    rawAttrTpl.innerHTML = `<i a="${raw.replace(/"/g, '&quot;')}"></i>`;
    const el = /** @type {Element} */ (rawAttrTpl.content.firstChild);
    return el.getAttribute('a') ?? '';
}

/**
 * Apply a SET_ATTR op: setAttribute and sync the live DOM property for form
 * controls. Once the user has interacted, the browser's dirty value/checkedness
 * flags make attribute writes stop affecting `.value`/`.checked`/`.selected`,
 * so without the property write a server-driven change would silently stop
 * rendering. The server is authoritative; a SET_ATTR of a boolean attribute
 * always means true (false diffs to REM_ATTR). A command value arrives as its
 * JSON array, not attribute text, so it is written back as JSON. A `?raw` value
 * arrives tagged `{raw}`: it is attribute markup, decoded the way SSR's parse did.
 * A server-owned control the client holds (`host.holds`: an event carrying its state
 * is pending or awaits its reply, or the user is editing it) takes the attribute
 * only, and a `value` the user is mid bad input in waits.
 * A write proves nothing about `name` (see `JS_SET_ATTR`): the diff path's value
 * may be app data, and a `?local` value is.
 * @param {Element} el
 * @param {string} name
 * @param {string | unknown[] | {raw: string}} val
 */
export function applySetAttrOp(el, name, val) {
    if (Array.isArray(val)) val = JSON.stringify(val);
    else if (typeof val === 'string') val = pageText(val);
    else val = decodeRawAttr(val.raw);
    const local = name === 'multiple' ? heldLocal(el) : null;
    // Asked before the write: the default it changes is what the user's edit is
    // compared against.
    const hold = (isChooserValue(el, name) || syncsProp(el, name)) && host.holds(el, name, val);
    if (hold === 'defer') return;
    const live = !hold && (isChooserValue(el, name) || syncsProp(el, name));
    el.setAttribute(name, val);
    if (local) carryChooserLocal(/** @type {HTMLSelectElement} */ (el), local);
    else if (isChooserValue(el, name)) applyChooser(/** @type {HTMLSelectElement} */ (el), live);
    else if (live) /** @type {any} */ (el)[name] = name === 'value' ? val : true;
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
export function applyRemAttrOp(el, name) {
    const local = name === 'multiple' ? heldLocal(el) : null;
    const hold = (isChooserValue(el, name) || syncsProp(el, name)) && host.holds(el, name, null);
    if (hold === 'defer') return;
    const live = !hold && (isChooserValue(el, name) || syncsProp(el, name));
    el.removeAttribute(name);
    if (local) carryChooserLocal(/** @type {HTMLSelectElement} */ (el), local);
    else if (isChooserValue(el, name)) applyChooser(/** @type {HTMLSelectElement} */ (el), live);
    else if (live) /** @type {any} */ (el)[name] = name === 'value' ? '' : false;
    notifyUpdated(el);
}

/**
 * The `?local` value of a select whose `multiple` is about to flip (`heldChooserLocal`):
 * the value means one text or a pick per part by `multiple`, and the server never renders
 * a local again, so the client carries it across. Asked only on a `multiple` write.
 * @param {Element} el
 * @returns {ReturnType<typeof heldChooserLocal>}
 */
function heldLocal(el) {
    return isChooserValue(el, 'value')
        ? heldChooserLocal(/** @type {HTMLSelectElement} */ (el))
        : null;
}

/**
 * Is `name` the `value` of a `<select>`? HTML has no such attribute: a select the
 * server left one on holds options it could not mark in the template (from a
 * component, a runtime value, raw markup), so the value is applied to the options
 * instead -- see `applyChooser`. An `arizona_js` attribute effect writing `value` on
 * any select means the same: it picks the option(s) it names, and a removal names
 * none (a single select then shows its first option, as HTML does with no pick).
 * @param {Element} el
 * @param {string} name
 * @returns {boolean}
 */
function isChooserValue(el, name) {
    return (
        name === 'value' &&
        el.localName === 'select' &&
        el.namespaceURI === 'http://www.w3.org/1999/xhtml'
    );
}

/**
 * Is `name` the `value` of a `<select multiple>`? Its value is the JSON array of the
 * texts it picks, the form every writer gives it (see `applyChooser`).
 * @param {Element} el
 * @param {string} name
 * @returns {boolean}
 */
export function isMultipleChooser(el, name) {
    return isChooserValue(el, name) && /** @type {HTMLSelectElement} */ (el).multiple;
}

/**
 * Mark `selected` the options of `sel` its `value` attribute names: the value, or
 * under `multiple` the JSON array of values the server renders (no attribute names
 * none). The `selected` attribute is what a form reset returns to and what an
 * option the user has not touched shows, so a newly rendered option the value names
 * is picked as one the server marked would be. `force` also moves the live
 * selection, the user's pick included: the server changed the value. Options sharing a
 * value are all marked, so the last of them is selected, as the parser selects the
 * last `selected` option.
 * @param {HTMLSelectElement} sel
 * @param {boolean} force
 */
export function applyChooser(sel, force) {
    const raw = sel.getAttribute('value');
    const picks = raw === null ? [] : chooserPicks(sel, raw);
    const before = sel.options[sel.selectedIndex];
    /** @type {HTMLOptionElement|null} */
    let last = null;
    let wrote = false;
    for (const o of sel.options) {
        const on = picks.includes(o.value);
        if (on) last = o;
        if (o.defaultSelected !== on) {
            o.defaultSelected = on;
            wrote = true;
        }
        if (force && o.selected !== on) o.selected = on;
    }
    // Marking an option selects it, so one sharing the value that arrives before the
    // selected one would take over; the parser selects the last. Only a selection this
    // marking moved goes there: one it left alone (the user's) stays.
    const now = sel.options[sel.selectedIndex];
    if (
        wrote &&
        last &&
        !sel.multiple &&
        now &&
        now !== before &&
        now !== last &&
        picks.includes(now.value)
    )
        last.selected = true;
}

/**
 * @param {HTMLSelectElement} sel
 * @param {string} raw
 * @returns {string[]}
 */
function chooserPicks(sel, raw) {
    if (sel.multiple) {
        try {
            const picks = JSON.parse(raw);
            if (Array.isArray(picks)) return picks.map((p) => pageText(String(p)));
        } catch {
            // Not the server's array: one value, as the server compares it.
        }
    }
    return [pageText(raw)];
}

/**
 * A value as the page holds it: a NUL, which no HTML can hold, as U+FFFD (the server
 * renders it so). Every other character, a CR included, is held as it is. A server
 * value written live, and a pick decoded from JSON, go through it so they hold what
 * the server's render of the same value does.
 * @param {string} text
 * @returns {string}
 */
export function pageText(text) {
    return text.includes('\0') ? text.replace(/\0/g, '\uFFFD') : text;
}

// The <input> types whose value sanitization strips newlines; a url or email value
// is also stripped of the whitespace around it, an email multiple's around each
// comma-separated value.
const LINE_INPUTS = new Set(['text', 'search', 'url', 'tel', 'email', 'password']);
const LEAD_WS = /^[\t\n\f\r ]+/;
const TRAIL_WS = /[\t\n\f\r ]+$/;

/**
 * `text`, part of an input's value, as the input's value sanitization leaves it.
 * `keepStart`/`keepEnd`: the text's start/end lies inside a value the rest of the
 * input's value continues, so no whitespace is stripped there.
 * @param {Element} el
 * @param {string} text
 * @param {boolean} keepStart
 * @param {boolean} keepEnd
 * @returns {string}
 */
export function sanitized(el, text, keepStart, keepEnd) {
    const input = /** @type {HTMLInputElement} */ (el);
    if (el.localName !== 'input' || !LINE_INPUTS.has(input.type)) return text;
    const line = text.replace(/[\r\n]/g, '');
    if (input.type !== 'url' && input.type !== 'email') return line;
    const values = input.type === 'email' && input.multiple ? line.split(',') : [line];
    const last = values.length - 1;
    return values
        .map((t, i) => {
            const s = i === 0 && keepStart ? t : t.replace(LEAD_WS, '');
            return i === last && keepEnd ? s : s.replace(TRAIL_WS, '');
        })
        .join(',');
}

// The <input> attributes its value sanitization reads (a color's `alpha` and
// `colorspace` too).
const SANITIZING_ATTRS = ['type', 'min', 'max', 'step', 'multiple', 'alpha', 'colorspace'];

/**
 * What a control's `value` shows once `text`, its default, is written to it, as the
 * browser computes it: a textarea's API value reads a CR as a LF, and an input's value
 * is sanitized by its type (newlines stripped, a color lowercased, a range clamped and
 * snapped, an invalid number or date emptied). A fresh, never-edited copy carrying only
 * the attributes that decide the sanitization holds `text` as its default and shows it:
 * a clone would carry the control's dirty value instead, and any other attribute copied
 * (a `style` a strict CSP reports) would be a write of its own. No `value` setter runs.
 * Compared with `value` to tell an edit from the default.
 * @param {Element} el
 * @param {string} text
 * @returns {string}
 */
export function shownDefault(el, text) {
    const probe = /** @type {HTMLInputElement} */ (el.ownerDocument.createElement(el.localName));
    if (el.localName === 'textarea') probe.textContent = text;
    else {
        for (const name of SANITIZING_ATTRS) {
            const v = el.getAttribute(name);
            if (v !== null) probe.setAttribute(name, v);
        }
        probe.setAttribute('value', text);
    }
    return probe.value;
}

/**
 * `applyChooser` over every select under `root` that carries a value: the page as
 * it loads, and the documents a batch of ops rendered options or selects into.
 * @param {Document|Element} root
 */
export function applyChoosers(root) {
    for (const sel of root.querySelectorAll('select[value]'))
        applyChooser(/** @type {HTMLSelectElement} */ (sel), false);
}

// The <input> types in the "value" value mode, the only ones whose `value` property
// holds user state apart from the attribute. The rest reflect it ("default" and
// "default/on": hidden, checkbox, radio, the buttons) or refuse a write ("filename":
// file), and an unknown type reads back as `text`.
const VALUE_MODE_INPUTS = new Set([
    'text',
    'search',
    'url',
    'tel',
    'email',
    'password',
    'date',
    'month',
    'week',
    'time',
    'datetime-local',
    'number',
    'range',
    'color',
]);

/**
 * Does `name` on `el` have a live property that user interaction detaches from the
 * attribute (the dirty value/checkedness/selectedness flags)? Only then must a write
 * set the property too, and a `?local` read report it. Elsewhere `value` merely
 * reflects the attribute, coerced -- a property write would put a removed attribute
 * back (`"0"` on `<li>`/`<progress>`/`<meter>`, `""` on `<button>`/`<data>` and on a
 * hidden/checkbox/radio/button `<input>`, and an `<option>` loses its fallback to its
 * text) or coerce a set one. Matched by local
 * name, not `instanceof`: a popped-out (PiP) document's elements come from another
 * realm.
 * @param {Element} el
 * @param {string} name
 * @returns {boolean}
 */
export function syncsProp(el, name) {
    if (el.namespaceURI !== 'http://www.w3.org/1999/xhtml') return false;
    if (name === 'value') {
        if (el.localName === 'input')
            return VALUE_MODE_INPUTS.has(/** @type {HTMLInputElement} */ (el).type);
        return el.localName === 'textarea' || el.localName === 'select';
    }
    if (name === 'checked') return el.localName === 'input';
    if (name === 'selected') return el.localName === 'option';
    return false;
}
