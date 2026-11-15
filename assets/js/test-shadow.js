// A browser's form is [LegacyOverrideBuiltIns]: a control associated with it (one of
// its listed elements, `form="id"` ones outside it included, or an <img> in it) named
// or with the id `id`, `elements`, `reset`, `hidden`, `value`, ... is what the form's
// own property of that name returns, ahead of the prototype. Assigning to such a name
// still runs the prototype's setter where there is one (`form.hidden = true` sets the
// attribute) and throws where there is none (`value`, `reset`) in module code. jsdom
// does none of this, so tests that must hold on a real page give their forms the same
// named properties.

const NAMES = ['id', 'elements', 'reset', 'hidden', 'value'];

/** The real `elements` getter: what a form's listed elements are. */
const ELEMENTS = /** @type {() => HTMLFormControlsCollection} */ (
    Object.getOwnPropertyDescriptor(HTMLFormElement.prototype, 'elements')?.get
);

/**
 * @param {object} proto
 * @param {string} k
 * @returns {PropertyDescriptor|undefined}
 */
function inherited(proto, k) {
    for (let p = proto; p; p = Object.getPrototypeOf(p)) {
        const d = Object.getOwnPropertyDescriptor(p, k);
        if (d) return d;
    }
    return undefined;
}

/**
 * The element a browser's form names `k`: an associated listed element or an <img>
 * in it, by id or name.
 * @param {Element} form
 * @param {string} k
 * @returns {Element|undefined}
 */
function named(form, k) {
    const cands = [...ELEMENTS.call(form), ...form.querySelectorAll('img')];
    return cands.find((c) => c.getAttribute('id') === k || c.getAttribute('name') === k);
}

/** @param {Element} form */
function shadowForm(form) {
    if (Object.hasOwn(form, 'elements')) return;
    for (const k of NAMES) {
        const d = inherited(Object.getPrototypeOf(form), k);
        Object.defineProperty(form, k, {
            configurable: true,
            get() {
                const el = named(this, k);
                if (el) return el;
                return d?.get ? d.get.call(this) : d?.value;
            },
            set(v) {
                // A setter on the prototype runs; otherwise the assignment defines an
                // own property, which a named property refuses.
                if (d?.set) d.set.call(this, v);
                else if (named(this, k))
                    throw new TypeError(`Cannot assign to read only property '${k}'`);
                else
                    Object.defineProperty(this, k, {
                        value: v,
                        writable: true,
                        configurable: true,
                        enumerable: true,
                    });
            },
        });
    }
}

/** @param {ParentNode} root */
function shadowIn(root) {
    // An SVG `form` is no HTML form: it has no named properties.
    for (const f of root.querySelectorAll('form'))
        if (f.namespaceURI === 'http://www.w3.org/1999/xhtml') shadowForm(f);
}

/**
 * Give every form on the page, and every one markup parses (`innerHTML`, which the
 * client's fragment parsing goes through) until the returned undo, its controls as
 * named properties.
 * @returns {() => void}
 */
export function shadowNamedControls() {
    const desc = /** @type {PropertyDescriptor} */ (
        Object.getOwnPropertyDescriptor(Element.prototype, 'innerHTML')
    );
    Object.defineProperty(Element.prototype, 'innerHTML', {
        configurable: true,
        get: desc.get,
        set(v) {
            desc.set?.call(this, v);
            shadowIn(this.localName === 'template' ? this.content : this);
        },
    });
    shadowIn(document);
    return () => Object.defineProperty(Element.prototype, 'innerHTML', desc);
}
