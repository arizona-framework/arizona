// A browser's form is [LegacyOverrideBuiltIns]: a control named (or with the id)
// `id`, `elements`, `reset`, `hidden`, `value`, ... is what the form's own property
// of that name returns, ahead of the prototype. jsdom does not do this, so tests
// that must hold on a real page give their forms the same named properties.

const NAMES = ['id', 'elements', 'reset', 'hidden', 'value'];

/**
 * @param {object} proto
 * @param {string} k
 * @returns {PropertyDescriptor}
 */
function inherited(proto, k) {
    for (let p = proto; p; p = Object.getPrototypeOf(p)) {
        const d = Object.getOwnPropertyDescriptor(p, k);
        if (d) return d;
    }
    // None (a form has no `value`): an expando is a plain property.
    return { value: undefined, writable: true };
}

/** @param {Element} form */
function shadowForm(form) {
    if (Object.hasOwn(form, 'elements')) return;
    for (const k of NAMES) {
        const d = inherited(Object.getPrototypeOf(form), k);
        Object.defineProperty(form, k, {
            configurable: true,
            get() {
                const named = this.querySelector(`[name="${k}"],[id="${k}"]`);
                if (named) return named;
                return d.get ? d.get.call(this) : d.value;
            },
            // A named property is read-only: assigning over it throws in module
            // (strict) code, as a browser's does.
            set(v) {
                if (this.querySelector(`[name="${k}"],[id="${k}"]`))
                    throw new TypeError(`Cannot assign to read only property '${k}'`);
                d.set?.call(this, v);
            },
        });
    }
}

/** @param {ParentNode} root */
function shadowIn(root) {
    for (const f of root.querySelectorAll('form')) shadowForm(f);
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
