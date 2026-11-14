import { afterEach, describe, expect, it } from 'vitest';
import { applyOps, get, set } from './arizona.js';
import { resolveHtml } from './arizona-core.js';

// The HTML parser rewrites a CR (alone or before a LF) into a LF and a NUL into
// U+FFFD (in body text it drops it) before the page exists, while a live update
// writes its value through the DOM. The markup and ops below are what the server
// renders for `arizona_cr_nul` (`cr_and_nul_render_as_the_page_holds_them` in
// arizona_render_SUITE asserts them): the page and a live update must agree.

const LOCAL =
    '<span az="YK753W-3" az-local="{&quot;a&quot;:{&quot;title&quot;:&quot;k&quot;},' +
    '&quot;ap&quot;:{&quot;title&quot;:[&quot;&quot;,&quot;\\ny&quot;]}}" title="x&#13;\ny"></span>' +
    '<input az="YK753W-4" az-local="{&quot;a&quot;:{&quot;value&quot;:&quot;q&quot;},' +
    '&quot;ap&quot;:{&quot;value&quot;:[&quot;&quot;,&quot;\\r\\ny&quot;]}}" value="x&#13;\ny" />' +
    '<input az="YK753W-5" az-local="{&quot;a&quot;:{&quot;value&quot;:&quot;u&quot;},' +
    '&quot;ap&quot;:{&quot;value&quot;:[&quot; &quot;,&quot;/ &quot;]}}" type="url" value=" x/ " />' +
    '<input az="YK753W-6" az-local="{&quot;a&quot;:{&quot;value&quot;:&quot;m&quot;},' +
    '&quot;ap&quot;:{&quot;value&quot;:[&quot; a@b.c ,\\n &quot;,&quot; , g@h.i &quot;]}}" ' +
    'type="email" multiple value=" a@b.c ,\n d@e.f , g@h.i " />';

/** @param {string} v */
const page = (v) =>
    `<div><p az="YK753W-0" title="${v}"><!--az:YK753W-0-->${v}<!--/az--></p>` +
    `<input az="YK753W-1" value="${v}" az-ctl="value" />` +
    `<textarea az="YK753W-2" az-ctl="content">${v}</textarea>${LOCAL}</div>`;

const FFFD = '�';

/** @param {string} html */
function load(html) {
    document.body.innerHTML = `<div id="v" az-view>${html}</div>`;
}

function read() {
    const p = /** @type {HTMLElement} */ (document.querySelector('p'));
    const input = /** @type {HTMLInputElement} */ (document.querySelector('input'));
    const textarea = /** @type {HTMLTextAreaElement} */ (document.querySelector('textarea'));
    return {
        title: p.getAttribute('title'),
        text: p.textContent,
        input: input.getAttribute('value'),
        textarea: textarea.defaultValue,
    };
}

afterEach(() => {
    document.body.innerHTML = '';
});

describe('CR and NUL in rendered values', () => {
    it('holds a CR on the page as a live update writes it', () => {
        load(page('a&#13;\nb'));
        const ssr = read();
        expect(ssr).toEqual({
            title: 'a\r\nb',
            text: 'a\r\nb',
            input: 'a\r\nb',
            textarea: 'a\r\nb',
        });
        load(page('i'));
        applyOps([
            [1, 'v:YK753W-0', 'title', 'a\r\nb'],
            [0, 'v:YK753W-0', 'a\r\nb'],
            [1, 'v:YK753W-1', 'value', 'a\r\nb'],
            [0, 'v:YK753W-2', resolveHtml({ d: ['a&#13;\nb'], f: 'x', s: ['', ''] }), true],
        ]);
        expect(read()).toEqual(ssr);
    });

    it('writes a NUL as U+FFFD, as the page holds it', () => {
        load(page(`a${FFFD}b`));
        const ssr = read();
        expect(ssr.text).toBe(`a${FFFD}b`);
        load(page('i'));
        applyOps([
            [1, 'v:YK753W-0', 'title', 'a\0b'],
            [0, 'v:YK753W-0', 'a\0b'],
            [1, 'v:YK753W-1', 'value', 'a\0b'],
            [0, 'v:YK753W-2', resolveHtml({ d: [`a${FFFD}b`], f: 'x', s: ['', ''] }), true],
        ]);
        expect(read()).toEqual(ssr);
    });

    it('reads a ?local initial ending in a CR back beside a suffix starting with a LF', () => {
        load(page('i'));
        expect(get('k')).toBe('x\r');
        set('v', 'k', 'z\r');
        expect(get('k')).toBe('z\r');
    });

    // A select's value and its options hold their exact text, so a CR and a LF name
    // different options, on the page and after a live write alike.
    it('picks the option whose value holds the same CR', () => {
        load(
            '<select az="0" value="a&#13;\nb"><option value="z">z</option>' +
                '<option value="a\nb">n</option><option value="a&#13;\nb">rn</option></select>',
        );
        const sel = /** @type {HTMLSelectElement} */ (document.querySelector('select'));
        applyOps([[1, 'v:0', 'value', 'a\r\nb']]);
        expect(sel.selectedOptions[0].text).toBe('rn');
        applyOps([[1, 'v:0', 'value', 'a\nb']]);
        expect(sel.value).toBe('a\nb');
        sel.multiple = true;
        applyOps([[1, 'v:0', 'value', '["a\\r\\nb","z\\u0000"]']]);
        expect([...sel.selectedOptions].map((o) => o.text)).toEqual(['rn']);
    });
});
