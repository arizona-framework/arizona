// Vitest setup: jsdom omits the `CSS` interface, but every browser the client
// targets ships `CSS.escape`. The client uses it to build `[az-key="..."]`
// selectors from arbitrary server keys, so provide the spec-compliant algorithm
// (CSSOM "serialize an identifier") when it is missing, keeping tests faithful to
// a real browser without a runtime guard in the shipped code.
if (typeof globalThis.CSS === 'undefined') globalThis.CSS = {};
if (typeof globalThis.CSS.escape !== 'function') {
    globalThis.CSS.escape = (value) => {
        const string = String(value);
        const length = string.length;
        const firstCodeUnit = string.charCodeAt(0);
        let index = -1;
        let result = '';
        while (++index < length) {
            const codeUnit = string.charCodeAt(index);
            if (codeUnit === 0x0000) {
                result += '�';
                continue;
            }
            if (
                (codeUnit >= 0x0001 && codeUnit <= 0x001f) ||
                codeUnit === 0x007f ||
                (index === 0 && codeUnit >= 0x0030 && codeUnit <= 0x0039) ||
                (index === 1 &&
                    codeUnit >= 0x0030 &&
                    codeUnit <= 0x0039 &&
                    firstCodeUnit === 0x002d)
            ) {
                result += `\\${codeUnit.toString(16)} `;
                continue;
            }
            if (index === 0 && length === 1 && codeUnit === 0x002d) {
                result += `\\${string.charAt(index)}`;
                continue;
            }
            if (
                codeUnit >= 0x0080 ||
                codeUnit === 0x002d ||
                codeUnit === 0x005f ||
                (codeUnit >= 0x0030 && codeUnit <= 0x0039) ||
                (codeUnit >= 0x0041 && codeUnit <= 0x005a) ||
                (codeUnit >= 0x0061 && codeUnit <= 0x007a)
            ) {
                result += string.charAt(index);
                continue;
            }
            result += `\\${string.charAt(index)}`;
        }
        return result;
    };
}

// jsdom caches a form's `elements` and, after some moves of a form between
// documents and re-renders inside it, keeps radios that left the form there while
// missing the ones that replaced them. Browsers keep the collection live, so give
// it a spec-faithful replacement here: the listed elements of the form's tree whose
// form owner is the form, in tree order, with `namedItem` (an element for one
// match, a list for several).
{
    const LISTED = 'button, fieldset, input, object, output, select, textarea';
    Object.defineProperty(HTMLFormElement.prototype, 'elements', {
        configurable: true,
        get() {
            const els = [...this.getRootNode().querySelectorAll(LISTED)].filter(
                (e) => e.form === this && !(e.localName === 'input' && e.type === 'image'),
            );
            return Object.assign(Object.create(null), els, {
                length: els.length,
                [Symbol.iterator]: () => els[Symbol.iterator](),
                item: (i) => els[i] ?? null,
                namedItem(name) {
                    const hits = els.filter(
                        (e) => e.id === name || e.getAttribute('name') === name,
                    );
                    if (hits.length === 0) return null;
                    if (hits.length === 1) return hits[0];
                    return Object.assign(Object.create(null), hits, {
                        length: hits.length,
                        [Symbol.iterator]: () => hits[Symbol.iterator](),
                        item: (i) => hits[i] ?? null,
                    });
                },
            });
        },
    });
}
