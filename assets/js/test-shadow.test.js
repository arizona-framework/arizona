import { afterEach, describe, expect, it } from 'vitest';
import { shadowNamedControls } from './test-shadow.js';

// The helper must shadow exactly as Chromium and Firefox do, or a test built on it
// proves a bug no browser has (or misses one every browser has).
describe('shadowNamedControls mirrors a browser form', () => {
    /** @type {(() => void)|null} */
    let undo = null;
    afterEach(() => {
        undo?.();
        undo = null;
        document.body.innerHTML = '';
    });
    /** @param {string} html */
    function page(html) {
        document.body.innerHTML = html;
        undo = shadowNamedControls();
        return /** @type {any} */ (document.getElementById('f'));
    }

    it('names an associated control by its name or id', () => {
        const f = page('<form id="f"><input name="hidden"><input id="value"></form>');
        expect(f.hidden).toBe(document.querySelector('[name=hidden]'));
        expect(f.value).toBe(document.getElementById('value'));
    });

    it('runs the prototype setter where there is one', () => {
        const f = page('<form id="f"><input name="hidden"><input name="id"></form>');
        f.hidden = true;
        expect(f.hasAttribute('hidden')).toBe(true);
        f.id = 'z';
        expect(f.getAttribute('id')).toBe('z');
    });

    it('refuses an assignment over a name with no setter, and allows an expando', () => {
        const f = page('<form id="f"><input name="value"></form><form id="g"></form>');
        expect(() => {
            f.value = 1;
        }).toThrow(TypeError);
        const g = /** @type {any} */ (document.getElementById('g'));
        g.value = 1;
        expect(g.value).toBe(1);
    });

    it('is shadowed by a control form="id" links from outside, not by one it links away', () => {
        const f = page(
            '<form id="f"><input name="reset" form="other"></form><input name="value" form="f">',
        );
        expect(typeof f.reset).toBe('function');
        expect(f.value).toBe(document.querySelector('[name=value]'));
    });

    it('is not shadowed by an element that is no control, but is by an img', () => {
        const f = page(
            '<form id="f"><div id="reset"></div><span name="value"></span><img name="hidden"></form>',
        );
        expect(typeof f.reset).toBe('function');
        expect(f.value).toBeUndefined();
        expect(f.hidden).toBe(document.querySelector('img'));
    });

    it('leaves an SVG form alone, as no HTML form', () => {
        page('<svg><form id="s"><rect id="value"></rect></form></svg>');
        const s = /** @type {any} */ (document.getElementById('s'));
        expect(s.id).toBe('s');
        expect(s.value).toBeUndefined();
    });
});
