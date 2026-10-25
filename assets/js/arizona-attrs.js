import { noteAzAttr } from './arizona-events.js';
import { notifyUpdated } from './arizona-hooks.js';

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
 * @param {Element} el
 * @param {string} name
 * @param {string | unknown[] | {raw: string}} val
 */
export function applySetAttrOp(el, name, val) {
    if (Array.isArray(val)) val = JSON.stringify(val);
    else if (typeof val !== 'string') val = decodeRawAttr(val.raw);
    // A runtime `arizona_js:set_attr(sel, "az-toggle", ...)` writes a name no
    // template contains, so the compile-time set cannot hold it. Every such write
    // funnels here -- the SET_ATTR op, an ITEM_PATCH inner op, `set_attr`/
    // `toggle_attr`, and a `?local` attribute slot -- so one call covers them all.
    // Without it the attribute lands correctly and its event silently never
    // fires. Before the write: a custom element's attributeChangedCallback may
    // synchronously dispatch the very event the attribute names.
    noteAzAttr(name);
    el.setAttribute(name, val);
    if (syncsProp(el, name)) /** @type {any} */ (el)[name] = name === 'value' ? val : true;
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
    el.removeAttribute(name);
    if (syncsProp(el, name)) /** @type {any} */ (el)[name] = name === 'value' ? '' : false;
    notifyUpdated(el);
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
