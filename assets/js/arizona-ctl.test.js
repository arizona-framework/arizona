import { afterEach, describe, expect, it, vi } from 'vitest';

// Server-owned controls (`az-ctl`): the attribute a server write changes is only a
// control's default, and once the user edits the control what it shows is the live
// property. These tests drive a connected page through a mock worker, as
// arizona.test.js does.

/** @type {any} */
let mock = null;
/** @type {any} */
let mod = null;

afterEach(() => {
    if (mock) mock.restore();
    mock = null;
    document.body.innerHTML = '';
});

async function connect(html) {
    vi.resetModules();
    mod = await import('./arizona.js');
    document.body.innerHTML = `<div id="page" az-view>${html}</div>`;
    const posted = [];
    let onmessage = null;
    const worker = {
        postMessage: (d) => posted.push(d),
        set onmessage(fn) {
            onmessage = fn;
        },
        get onmessage() {
            return onmessage;
        },
        terminate: vi.fn(),
    };
    const OrigWorker = globalThis.Worker;
    globalThis.Worker = function () {
        return worker;
    };
    const disconnect = mod.connect('/ws');
    mock = {
        open: () => onmessage({ data: [1, false] }),
        close: (code = 1006) => onmessage({ data: [2, code] }),
        reply: (ops, ref) => onmessage({ data: [0, ops, null, false, null, ref] }),
        sent: () => posted.filter((d) => d[0] === 1).map((d) => JSON.parse(d[1])),
        lastRef() {
            const ref = this.sent().at(-1)?.at(3);
            return typeof ref === 'number' ? ref : null;
        },
        restore() {
            disconnect();
            globalThis.Worker = OrigWorker;
        },
    };
    mock.open();
    return mock;
}

const CHANGE = `az-change='[0,"set"]'`;
const INPUT = `az-input='[0,"set"]'`;

/** The user edits `el`: its live state changes, then `type` fires. */
function edit(el, change, type = 'change') {
    change(el);
    el.dispatchEvent(new Event(type, { bubbles: true }));
}

const setAttr = (az, name, val) => [mod.OP.SET_ATTR, `page:${az}`, name, val];

describe('server-owned controls: an answered event shows what the server rendered', () => {
    it('a refused edit shows the stored value again', async () => {
        await connect(`<input az="0" name="qty" value="5" ${CHANGE} az-ctl />`);
        const el = document.querySelector('input');
        edit(el, (e) => {
            e.value = '999';
        });
        const ref = mock.lastRef();
        expect(ref).not.toBeNull();
        expect(mock.sent().at(-1).slice(0, 3)).toEqual(['page', 'set', { value: '999' }]);
        // The binding did not change: the reply carries no op.
        mock.reply(null, ref);
        expect(el.value).toBe('5');
    });

    it('a normalised value replaces what the user typed', async () => {
        await connect(`<input az="0" value="abc" ${CHANGE} az-ctl />`);
        const el = document.querySelector('input');
        edit(el, (e) => {
            e.value = 'abc ';
        });
        mock.reply(null, mock.lastRef());
        expect(el.value).toBe('abc');
    });

    it('an accepted value is left as typed, caret and all', async () => {
        await connect(`<input az="0" value="5" ${INPUT} az-ctl />`);
        const el = document.querySelector('input');
        el.focus();
        edit(
            el,
            (e) => {
                e.value = '999';
                e.setSelectionRange(1, 1);
            },
            'input',
        );
        mock.reply([setAttr(0, 'value', '999')], mock.lastRef());
        expect(el.value).toBe('999');
        expect(el.getAttribute('value')).toBe('999');
        expect([el.selectionStart, el.selectionEnd]).toEqual([1, 1]);
    });

    it('typing while the event is in flight is kept', async () => {
        await connect(`<input az="0" value="5" ${CHANGE} az-ctl />`);
        const el = document.querySelector('input');
        edit(el, (e) => {
            e.value = '99';
        });
        const ref = mock.lastRef();
        el.value = '999';
        mock.reply([setAttr(0, 'value', '99')], ref);
        expect(el.value).toBe('999');
        expect(el.getAttribute('value')).toBe('99');
    });

    it('an earlier reply does not overwrite later typing (123, not 13)', async () => {
        await connect(`<input az="0" value="" ${INPUT} az-ctl />`);
        const el = document.querySelector('input');
        const refs = [];
        for (const v of ['1', '12', '123']) {
            edit(
                el,
                (e) => {
                    e.value = v;
                },
                'input',
            );
            refs.push(mock.lastRef());
        }
        expect(new Set(refs).size).toBe(3);
        mock.reply([setAttr(0, 'value', '1')], refs[0]);
        expect(el.value).toBe('123');
        mock.reply([setAttr(0, 'value', '12')], refs[1]);
        expect(el.value).toBe('123');
        mock.reply([setAttr(0, 'value', '123')], refs[2]);
        expect(el.value).toBe('123');
        // Settled: a later server write shows again.
        mock.reply([setAttr(0, 'value', '7')], null);
        expect(el.value).toBe('7');
    });

    it('a quick edit refused after a later one is sent keeps the later one in flight', async () => {
        await connect(`<input az="0" value="5" ${CHANGE} az-ctl />`);
        const el = document.querySelector('input');
        edit(el, (e) => {
            e.value = '1';
        });
        const r1 = mock.lastRef();
        edit(el, (e) => {
            e.value = '2';
        });
        const r2 = mock.lastRef();
        mock.reply(null, r1);
        expect(el.value).toBe('2');
        mock.reply(null, r2);
        expect(el.value).toBe('5');
    });

    it('an unmarked control is not held and its frame carries no ref', async () => {
        await connect(`<input az="0" value="5" ${CHANGE} />`);
        const el = document.querySelector('input');
        edit(el, (e) => {
            e.value = '9';
        });
        expect(mock.sent().at(-1)).toEqual(['page', 'set', { value: '9' }]);
        mock.reply([setAttr(0, 'value', '6')], null);
        expect(el.value).toBe('6');
    });

    it('a ?local control is the client own: no ref, nothing reverted', async () => {
        await connect(`<input az="0" az-local='{"a":{"value":"k"}}' value="x" ${CHANGE} />`);
        const el = document.querySelector('input');
        edit(el, (e) => {
            e.value = 'typed';
        });
        expect(mock.sent().at(-1)).toHaveLength(3);
        mock.reply(null, null);
        expect(el.value).toBe('typed');
    });

    it('a refused checkbox shows its stored state', async () => {
        await connect(`<input az="0" type="checkbox" checked ${CHANGE} az-ctl />`);
        const el = document.querySelector('input');
        edit(el, (e) => {
            e.checked = false;
        });
        mock.reply(null, mock.lastRef());
        expect(el.checked).toBe(true);
    });

    it('a refused radio pick restores its group', async () => {
        await connect(
            `<input az="0" type="radio" name="r" value="a" ${CHANGE} az-ctl />` +
                `<input az="1" type="radio" name="r" value="b" checked ${CHANGE} az-ctl />`,
        );
        const [a, b] = document.querySelectorAll('input');
        edit(a, (e) => {
            e.checked = true;
        });
        expect(b.checked).toBe(false);
        mock.reply(null, mock.lastRef());
        expect([a.checked, b.checked]).toEqual([false, true]);
    });

    it('a refused single select shows its stored option', async () => {
        await connect(
            `<select ${CHANGE}><option az="0" value="a" az-ctl>A</option>` +
                `<option az="1" value="b" selected az-ctl>B</option></select>`,
        );
        const sel = document.querySelector('select');
        edit(sel, (e) => {
            e.value = 'a';
        });
        mock.reply(null, mock.lastRef());
        expect(sel.value).toBe('b');
    });

    it('a refused multiple select shows its stored options', async () => {
        await connect(
            `<select multiple ${CHANGE}><option az="0" value="a" selected az-ctl>A</option>` +
                `<option az="1" value="b" az-ctl>B</option></select>`,
        );
        const sel = document.querySelector('select');
        edit(sel, (e) => {
            e.options[0].selected = false;
            e.options[1].selected = true;
        });
        expect(mock.sent().at(-1)[2]).toEqual({ value: ['b'] });
        mock.reply(null, mock.lastRef());
        expect(Array.from(sel.selectedOptions, (o) => o.value)).toEqual(['a']);
    });

    it('a refused select that kept its value shows the value it names', async () => {
        await connect(
            `<select az="0" value="b" ${CHANGE} az-ctl><option value="a">A</option>` +
                `<option value="b" selected>B</option></select>`,
        );
        const sel = document.querySelector('select');
        edit(sel, (e) => {
            e.value = 'a';
        });
        mock.reply(null, mock.lastRef());
        expect(sel.value).toBe('b');
    });

    it('a refused textarea shows its stored content', async () => {
        await connect(`<textarea az="0" ${CHANGE} az-ctl>hi</textarea>`);
        const el = document.querySelector('textarea');
        edit(el, (e) => {
            e.value = 'hello';
        });
        mock.reply(null, mock.lastRef());
        expect(el.value).toBe('hi');
    });

    it('a refused submit reverts its server-owned fields only', async () => {
        await connect(
            `<form az-submit='[0,"save"]'><input az="0" name="q" value="5" az-ctl />` +
                `<input name="free" value="" /></form>`,
        );
        const [q, free] = document.querySelectorAll('input');
        q.value = '999';
        free.value = 'note';
        document
            .querySelector('form')
            .dispatchEvent(new Event('submit', { bubbles: true, cancelable: true }));
        mock.reply(null, mock.lastRef());
        expect([q.value, free.value]).toEqual(['5', 'note']);
    });

    it('a close before the reply holds nothing afterwards', async () => {
        await connect(`<input az="0" value="5" ${CHANGE} az-ctl />`);
        const el = document.querySelector('input');
        edit(el, (e) => {
            e.value = '999';
        });
        mock.close();
        mock.open();
        mock.reply([setAttr(0, 'value', '7')], null);
        expect(el.value).toBe('7');
    });

    it('a normalised focused value keeps the caret, clamped', async () => {
        await connect(`<input az="0" value="abcdef" ${CHANGE} az-ctl />`);
        const el = document.querySelector('input');
        el.focus();
        edit(el, (e) => {
            e.value = 'abcdefgh';
            e.setSelectionRange(2, 7);
        });
        mock.reply([setAttr(0, 'value', 'abcd')], mock.lastRef());
        expect(el.value).toBe('abcd');
        expect([el.selectionStart, el.selectionEnd]).toEqual([2, 4]);
    });
});

describe('server-owned controls: unsent typing is not clobbered', () => {
    it('a push while the user types unsent keeps the typing', async () => {
        await connect(`<input az="0" value="a" ${CHANGE} az-ctl />`);
        const el = document.querySelector('input');
        el.focus();
        el.value = 'hel';
        mock.reply([setAttr(0, 'value', 'server')], null);
        expect(el.value).toBe('hel');
        expect(el.getAttribute('value')).toBe('server');
    });

    it('a push to a focused untouched field is shown', async () => {
        await connect(`<input az="0" value="a" ${CHANGE} az-ctl />`);
        const el = document.querySelector('input');
        el.focus();
        mock.reply([setAttr(0, 'value', 'server')], null);
        expect(el.value).toBe('server');
    });

    it('leaving the field after a held push shows the server value', async () => {
        await connect(`<input az="0" value="a" az-ctl />`);
        const el = document.querySelector('input');
        el.focus();
        el.value = 'hel';
        mock.reply([setAttr(0, 'value', 'server')], null);
        el.blur();
        expect(el.value).toBe('server');
    });

    it('leaving the field with no push keeps the typing, which a change then sends', async () => {
        await connect(`<input az="0" value="a" ${CHANGE} az-ctl />`);
        const el = document.querySelector('input');
        el.focus();
        el.value = 'hel';
        el.blur();
        expect(el.value).toBe('hel');
        el.dispatchEvent(new Event('change', { bubbles: true }));
        expect(mock.sent().at(-1).slice(0, 3)).toEqual(['page', 'set', { value: 'hel' }]);
        mock.reply(null, mock.lastRef());
        expect(el.value).toBe('a');
    });

    it('a change sent before focus leaves settles on its reply, not on the blur', async () => {
        await connect(`<input az="0" value="a" ${CHANGE} az-ctl />`);
        const el = document.querySelector('input');
        el.focus();
        el.value = 'hel';
        mock.reply([setAttr(0, 'value', 'server')], null);
        el.dispatchEvent(new Event('change', { bubbles: true }));
        el.blur();
        expect(el.value).toBe('hel');
        mock.reply([setAttr(0, 'value', 'hel')], mock.lastRef());
        expect(el.value).toBe('hel');
    });

    it('never writes mid-composition; leaving the field settles it', async () => {
        await connect(`<input az="0" value="a" ${CHANGE} az-ctl />`);
        const el = document.querySelector('input');
        el.focus();
        el.dispatchEvent(new CompositionEvent('compositionstart', { bubbles: true }));
        el.value = 'あ';
        mock.reply([setAttr(0, 'value', 'server')], null);
        expect(el.value).toBe('あ');
        el.dispatchEvent(new CompositionEvent('compositionend', { bubbles: true }));
        expect(el.value).toBe('あ');
        el.blur();
        expect(el.value).toBe('server');
    });

    it('a reply settling mid-composition waits for focus to leave', async () => {
        await connect(`<input az="0" value="5" ${INPUT} az-ctl />`);
        const el = document.querySelector('input');
        el.focus();
        edit(
            el,
            (e) => {
                e.value = '12';
            },
            'input',
        );
        el.dispatchEvent(new CompositionEvent('compositionstart', { bubbles: true }));
        mock.reply(null, mock.lastRef());
        expect(el.value).toBe('12');
        el.dispatchEvent(new CompositionEvent('compositionend', { bubbles: true }));
        el.blur();
        expect(el.value).toBe('5');
    });
});
