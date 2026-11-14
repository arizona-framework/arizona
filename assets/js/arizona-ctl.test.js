import { afterEach, describe, expect, it, vi } from 'vitest';
import { embedAttrs } from './test-proofs.js';

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
    const unembed = embedAttrs();
    const disconnect = mod.connect('/ws');
    unembed();
    mock = {
        open: () => onmessage({ data: [1, false] }),
        close: (code = 1006) => onmessage({ data: [2, code] }),
        reply: (ops, ref) => onmessage({ data: [0, ops, null, false, null, ref] }),
        effects: (effects, ref) => onmessage({ data: [0, null, effects, false, null, ref] }),
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
        await connect(`<input az="0" name="qty" value="5" ${CHANGE} az-ctl="value" />`);
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
        await connect(`<input az="0" value="abc" ${CHANGE} az-ctl="value" />`);
        const el = document.querySelector('input');
        edit(el, (e) => {
            e.value = 'abc ';
        });
        mock.reply(null, mock.lastRef());
        expect(el.value).toBe('abc');
    });

    it('an accepted value is left as typed, caret and all', async () => {
        await connect(`<input az="0" value="5" ${INPUT} az-ctl="value" />`);
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
        await connect(`<input az="0" value="5" ${CHANGE} az-ctl="value" />`);
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
        await connect(`<input az="0" value="" ${INPUT} az-ctl="value" />`);
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
        await connect(`<input az="0" value="5" ${CHANGE} az-ctl="value" />`);
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
        await connect(`<input az="0" type="checkbox" checked ${CHANGE} az-ctl="checked" />`);
        const el = document.querySelector('input');
        edit(el, (e) => {
            e.checked = false;
        });
        mock.reply(null, mock.lastRef());
        expect(el.checked).toBe(true);
    });

    it('a refused radio pick restores its group', async () => {
        await connect(
            `<input az="0" type="radio" name="r" value="a" ${CHANGE} az-ctl="checked" />` +
                `<input az="1" type="radio" name="r" value="b" checked ${CHANGE} az-ctl="checked" />`,
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
            `<select ${CHANGE}><option az="0" value="a" az-ctl="selected">A</option>` +
                `<option az="1" value="b" selected az-ctl="selected">B</option></select>`,
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
            `<select multiple ${CHANGE}><option az="0" value="a" selected az-ctl="selected">A</option>` +
                `<option az="1" value="b" az-ctl="selected">B</option></select>`,
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
            `<select az="0" value="b" ${CHANGE} az-ctl="value"><option value="a">A</option>` +
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
        await connect(`<textarea az="0" ${CHANGE} az-ctl="content">hi</textarea>`);
        const el = document.querySelector('textarea');
        edit(el, (e) => {
            e.value = 'hello';
        });
        mock.reply(null, mock.lastRef());
        expect(el.value).toBe('hi');
    });

    it('a refused submit reverts its server-owned fields only', async () => {
        await connect(
            `<form az-submit='[0,"save"]'><input az="0" name="q" value="5" az-ctl="value" />` +
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
        await connect(`<input az="0" value="5" ${CHANGE} az-ctl="value" />`);
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
        await connect(`<input az="0" value="abcdef" ${CHANGE} az-ctl="value" />`);
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
        await connect(`<input az="0" value="a" ${CHANGE} az-ctl="value" />`);
        const el = document.querySelector('input');
        el.focus();
        el.value = 'hel';
        mock.reply([setAttr(0, 'value', 'server')], null);
        expect(el.value).toBe('hel');
        expect(el.getAttribute('value')).toBe('server');
    });

    it('a push to a focused untouched field is shown', async () => {
        await connect(`<input az="0" value="a" ${CHANGE} az-ctl="value" />`);
        const el = document.querySelector('input');
        el.focus();
        mock.reply([setAttr(0, 'value', 'server')], null);
        expect(el.value).toBe('server');
    });

    it('leaving the field after a held push shows the server value', async () => {
        await connect(`<input az="0" value="a" az-ctl="value" />`);
        const el = document.querySelector('input');
        el.focus();
        el.value = 'hel';
        mock.reply([setAttr(0, 'value', 'server')], null);
        el.blur();
        expect(el.value).toBe('server');
    });

    it('leaving the field with no push keeps the typing, which a change then sends', async () => {
        await connect(`<input az="0" value="a" ${CHANGE} az-ctl="value" />`);
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
        await connect(`<input az="0" value="a" ${CHANGE} az-ctl="value" />`);
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
        await connect(`<input az="0" value="a" ${CHANGE} az-ctl="value" />`);
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
        await connect(`<input az="0" value="5" ${INPUT} az-ctl="value" />`);
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

describe('server-owned controls: a push during an in-flight edit', () => {
    it('a push past which the user typed wins when focus leaves', async () => {
        await connect(`<input az="0" value="0" ${CHANGE} az-ctl="value" />`);
        const el = document.querySelector('input');
        el.focus();
        edit(el, (e) => {
            e.value = '1';
        });
        const r1 = mock.lastRef();
        el.value = '12';
        mock.reply([setAttr(0, 'value', '9')], null);
        mock.reply(null, r1);
        expect(el.value).toBe('12');
        el.blur();
        expect(el.value).toBe('9');
    });

    it('the reply echoing the sent value keeps the typing past it for its own event', async () => {
        await connect(`<input az="0" value="0" ${CHANGE} az-ctl="value" />`);
        const el = document.querySelector('input');
        el.focus();
        edit(el, (e) => {
            e.value = '1';
        });
        const r1 = mock.lastRef();
        el.value = '12';
        mock.reply([setAttr(0, 'value', '1')], r1);
        expect(el.value).toBe('12');
        el.blur();
        expect(el.value).toBe('12');
        el.dispatchEvent(new Event('change', { bubbles: true }));
        expect(mock.sent().at(-1).slice(0, 3)).toEqual(['page', 'set', { value: '12' }]);
    });

    it('a push that a settled reply already showed is not replayed on a later blur', async () => {
        await connect(`<input az="0" value="0" ${CHANGE} az-ctl="value" />`);
        const el = document.querySelector('input');
        el.focus();
        edit(el, (e) => {
            e.value = '1';
        });
        const r1 = mock.lastRef();
        mock.reply([setAttr(0, 'value', '9')], null);
        mock.reply(null, r1);
        expect(el.value).toBe('9');
        el.value = '95';
        el.blur();
        expect(el.value).toBe('95');
    });
});

describe('server-owned controls: textarea content', () => {
    it('a content update shows on a textarea the user edited and left', async () => {
        await connect(`<textarea az="0" az-ctl="content">a</textarea>`);
        const el = document.querySelector('textarea');
        el.value = 'user';
        mock.reply([[mod.OP.TEXT, 'page:0', 'B']], null);
        expect(el.value).toBe('B');
    });

    it('a content update holds off unsent typing until focus leaves', async () => {
        await connect(`<textarea az="0" az-ctl="content">a</textarea>`);
        const el = document.querySelector('textarea');
        el.focus();
        el.value = 'user';
        mock.reply([[mod.OP.TEXT, 'page:0', 'B']], null);
        expect(el.value).toBe('user');
        expect(el.defaultValue).toBe('B');
        el.blur();
        expect(el.value).toBe('B');
    });

    it('a content update during an in-flight edit is the reply when it answers it', async () => {
        await connect(`<textarea az="0" ${CHANGE} az-ctl="content">a</textarea>`);
        const el = document.querySelector('textarea');
        edit(el, (e) => {
            e.value = 'hello ';
        });
        mock.reply([[mod.OP.TEXT, 'page:0', 'hello']], mock.lastRef());
        expect(el.value).toBe('hello');
    });

    it('an unmarked textarea keeps the user text, as before', async () => {
        await connect(`<textarea az="0">a</textarea>`);
        const el = document.querySelector('textarea');
        el.value = 'user';
        mock.reply([[mod.OP.TEXT, 'page:0', 'B']], null);
        expect(el.value).toBe('user');
    });
});

describe('server-owned controls: audit gaps', () => {
    it('a form-level debounced send keeps the control guarded past a blur', async () => {
        vi.useFakeTimers();
        try {
            await connect(
                `<form az-input='[0,"v"]' az-debounce="300"><input az="0" name="t" value="" az-ctl="value" /></form>`,
            );
            const el = document.querySelector('input');
            el.focus();
            el.value = 'abc';
            el.dispatchEvent(new Event('input', { bubbles: true }));
            mock.reply([setAttr(0, 'value', 'P')], null);
            el.blur();
            expect(el.value).toBe('abc');
            vi.advanceTimersByTime(400);
            const f = mock.sent().at(-1);
            expect(f.slice(0, 3)).toEqual(['page', 'v', { t: 'abc' }]);
            mock.reply([setAttr(0, 'value', 'abc')], f[3]);
            expect([el.getAttribute('value'), el.value]).toEqual(['abc', 'abc']);
        } finally {
            vi.useRealTimers();
        }
    });

    it('a debounced unfocused checkbox is guarded from its raise', async () => {
        vi.useFakeTimers();
        try {
            await connect(
                `<input az="0" type="checkbox" name="c" az-ctl="checked" az-debounce="300" ${CHANGE} />`,
            );
            const el = document.querySelector('input');
            el.click();
            mock.reply([setAttr(0, 'checked', '')], null);
            mock.reply([[mod.OP.REM_ATTR, 'page:0', 'checked']], null);
            expect(el.checked).toBe(true);
            vi.advanceTimersByTime(400);
            mock.reply(null, mock.lastRef());
            expect(el.checked).toBe(false);
        } finally {
            vi.useRealTimers();
        }
    });

    it('a window losing focus keeps the guard; leaving the field ends it', async () => {
        await connect(`<input az="0" value="a" az-ctl="value" />`);
        const el = document.querySelector('input');
        el.focus();
        el.value = 'typed';
        mock.reply([setAttr(0, 'value', 'srv')], null);
        const hasFocus = vi.spyOn(document, 'hasFocus').mockReturnValue(false);
        el.dispatchEvent(new FocusEvent('focusout', { bubbles: true }));
        expect(el.value).toBe('typed');
        hasFocus.mockReturnValue(true);
        el.blur();
        expect(el.value).toBe('srv');
        hasFocus.mockRestore();
    });

    it('sends a lone surrogate as U+FFFD', async () => {
        await connect(`<input az="0" value="" ${INPUT} az-ctl="value" />`);
        edit(
            document.querySelector('input'),
            (e) => {
                e.value = 'a\ud800';
            },
            'input',
        );
        const sent = mock.sent().at(-1);
        expect(sent[2]).toEqual({ value: 'a�' });
        expect(typeof sent[3]).toBe('number');
    });

    it('tells a hook on the control it corrected, and on the radio it unchecked', async () => {
        const updated = vi.fn();
        await connect(
            `<input az-hook="H" az="0" value="5" ${CHANGE} az-ctl="value" />` +
                `<input type="radio" name="r" value="a" checked ${CHANGE} az-ctl="checked" />` +
                `<input az-hook="H" type="radio" name="r" value="b" ${CHANGE} az-ctl="checked" />`,
        );
        mod.hooks.H = { updated };
        mod.mountHooks(document);
        const [text, , b] = document.querySelectorAll('input');
        edit(text, (e) => {
            e.value = '9';
        });
        mock.reply(null, mock.lastRef());
        expect(updated).toHaveBeenCalledTimes(1);
        b.click();
        mock.reply(null, mock.lastRef());
        expect(b.checked).toBe(false);
        expect(updated).toHaveBeenCalledTimes(2);
    });

    it('a form carries only the controls it submits', async () => {
        await connect(
            `<form ${CHANGE}><input id="q" name="q" value="" az-ctl="value" />` +
                `<input id="d" name="d" value="" disabled az-ctl="value" />` +
                `<input id="n" value="" az-ctl="value" />` +
                `<fieldset disabled><input id="f" name="f" value="" az-ctl="value" /></fieldset></form>`,
        );
        const $ = (id) => document.getElementById(id);
        $('d').value = 'x';
        $('n').value = 'y';
        $('f').value = 'w';
        edit($('q'), (e) => {
            e.value = 'z';
        });
        mock.reply(null, mock.lastRef());
        expect(['q', 'd', 'n', 'f'].map((id) => $(id).value)).toEqual(['', 'x', 'y', 'w']);
    });

    it('an event whose default was prevented edited nothing', async () => {
        await connect(
            `<input az="0" type="checkbox" name="c" az-ctl="checked" az-prevent-default az-click='[0,"tick"]' />`,
        );
        const el = document.querySelector('input');
        el.click();
        expect(el.checked).toBe(false);
        expect(mock.sent().at(-1)).toEqual(['page', 'tick', { value: 'on', checked: true }]);
        mock.reply([setAttr(0, 'checked', '')], null);
        expect(el.checked).toBe(true);
    });

    it('a checkbox whose rendered part is only its value keeps its checkedness', async () => {
        await connect(
            `<input az="0" type="checkbox" name="c" value="1" az-ctl="value" ${CHANGE} />`,
        );
        const el = document.querySelector('input');
        el.click();
        expect(mock.sent().at(-1)).toHaveLength(3);
        mock.reply([setAttr(0, 'value', '2')], null);
        expect(el.checked).toBe(true);
    });

    it('a settle waits while the control shows bad input', async () => {
        await connect(`<input az="0" type="number" value="5" ${INPUT} az-ctl="value" />`);
        const el = document.querySelector('input');
        Object.defineProperty(el, 'validity', { value: { badInput: true } });
        edit(
            el,
            (e) => {
                e.value = '';
            },
            'input',
        );
        mock.reply(null, mock.lastRef());
        expect(el.value).toBe('');
    });

    it('a value write waits while the user is mid bad input, then lands', async () => {
        await connect(`<input az="0" type="number" value="5" az-ctl="value" />`);
        const el = document.querySelector('input');
        const bad = { badInput: true };
        Object.defineProperty(el, 'validity', { get: () => bad });
        el.focus();
        el.value = '';
        mock.reply([setAttr(0, 'value', '9')], null);
        expect(el.getAttribute('value')).toBe('5');
        el.dispatchEvent(new Event('input', { bubbles: true }));
        expect(el.getAttribute('value')).toBe('5');
        bad.badInput = false;
        el.blur();
        expect([el.getAttribute('value'), el.value]).toEqual(['9', '9']);
    });

    it('a set_attr effect in the reply wins on blur over typing past the sent value', async () => {
        await connect(`<input id="i" az="0" value="" ${INPUT} az-ctl="value" />`);
        const el = document.querySelector('input');
        el.focus();
        edit(
            el,
            (e) => {
                e.value = 'ab';
            },
            'input',
        );
        const ref = mock.lastRef();
        el.value = 'abc';
        mock.effects([[7, '#i', 'value', '']], ref);
        el.blur();
        expect(el.value).toBe('');
    });
});

describe('server-owned controls: final audit', () => {
    it('a textarea value write is no control part: an accepted edit stays', async () => {
        await connect(`<textarea az="0" name="t" az-ctl="value" ${CHANGE}></textarea>`);
        const el = document.querySelector('textarea');
        edit(el, (e) => {
            e.value = 'hi';
        });
        expect(mock.sent().at(-1)).toHaveLength(3);
        mock.reply([setAttr(0, 'value', 'hi')], null);
        expect(el.value).toBe('hi');
    });

    it('a handler transition(set_attr) effect is held off unsent typing like the frame', async () => {
        /** @type {Array<() => void>} */
        const queue = [];
        document.startViewTransition = (fn) => {
            queue.push(typeof fn === 'object' ? fn.update : fn);
            return { ready: Promise.resolve(), finished: Promise.resolve() };
        };
        try {
            await connect(`<input id="q" az="0" name="q" value="a" az-ctl="value" />`);
            const el = document.querySelector('input');
            el.focus();
            el.value = 'ab';
            mock.effects([[20, {}, [7, '#q', 'value', 'srv']]], null);
            for (const f of queue.splice(0)) f();
            expect(el.value).toBe('ab');
            el.blur();
            expect(el.value).toBe('srv');
        } finally {
            delete document.startViewTransition;
        }
    });

    it('a field a superseded form debounce carried, the next raise not, is let go', async () => {
        vi.useFakeTimers();
        try {
            await connect(
                `<form az-input='[0,"v"]' az-debounce="300"><input id="a" az="0" name="a" value="" az-ctl="value" />` +
                    `<input id="b" az="1" name="b" value="" az-ctl="value" /></form>`,
            );
            const a = document.getElementById('a');
            const b = document.getElementById('b');
            a.value = 'x';
            b.value = 'bb';
            a.dispatchEvent(new Event('input', { bubbles: true }));
            b.disabled = true;
            a.value = 'xy';
            a.dispatchEvent(new Event('input', { bubbles: true }));
            mock.reply([setAttr(1, 'value', 'srv')], null);
            expect(b.value).toBe('srv');
        } finally {
            vi.useRealTimers();
        }
    });
});

describe('server-owned controls: pending by debounce slot', () => {
    it('a raise a prevented raise replaces in its slot is let go', async () => {
        vi.useFakeTimers();
        try {
            await connect(
                `<input id="q" az="0" name="q" value="" az-ctl="value" az-debounce="300" az-keydown='[0,"k"]' />`,
            );
            const el = document.querySelector('input');
            el.addEventListener('keydown', (e) => {
                if (e.key === 'Enter') e.preventDefault();
            });
            el.focus();
            el.value = 'x';
            el.dispatchEvent(new KeyboardEvent('keydown', { key: 'a', bubbles: true }));
            el.dispatchEvent(
                new KeyboardEvent('keydown', { key: 'Enter', bubbles: true, cancelable: true }),
            );
            el.blur();
            expect(mock.sent().at(-1)).toHaveLength(3);
            mock.reply([setAttr(0, 'value', 'srv')], null);
            expect(el.value).toBe('srv');
        } finally {
            vi.useRealTimers();
        }
    });

    it('a control stays pending while any slot still carries it', async () => {
        vi.useFakeTimers();
        try {
            await connect(
                `<form az-input='[0,"i"]' az-change='[0,"c"]' az-debounce="300">` +
                    `<input id="a" az="0" name="a" value="" az-ctl="value" />` +
                    `<input id="b" az="1" name="b" value="" az-ctl="value" /></form>`,
            );
            const a = document.getElementById('a');
            const b = document.getElementById('b');
            b.value = 'bb';
            b.dispatchEvent(new Event('input', { bubbles: true }));
            b.disabled = true;
            a.value = 'aa';
            a.dispatchEvent(new Event('change', { bubbles: true }));
            mock.reply([setAttr(1, 'value', 'srv')], null);
            expect(b.value).toBe('bb');
        } finally {
            vi.useRealTimers();
        }
    });

    it('a textarea value write is its live value: held off unsent typing', async () => {
        await connect(`<textarea az="0" name="t" az-ctl="content">a</textarea>`);
        const el = document.querySelector('textarea');
        el.focus();
        el.value = 'typed';
        mock.reply([setAttr(0, 'value', 'x')], null);
        expect(el.value).toBe('typed');
    });
});

describe('server-owned controls: pending across reconnects and removals', () => {
    it('a raise a reconnect left in its slot releases nothing of the new connection', async () => {
        vi.useFakeTimers();
        let again = () => {};
        try {
            await connect(
                `<input id="q" az="0" name="q" value="" az-ctl="value" az-debounce="300" ${INPUT} />`,
            );
            const el = document.querySelector('input');
            edit(
                el,
                (e) => {
                    e.value = 'a';
                },
                'input',
            );
            again = mod.connect('/ws');
            mock.open();
            edit(
                el,
                (e) => {
                    e.value = 'ab';
                },
                'input',
            );
            mock.reply([setAttr(0, 'value', 'srv')], null);
            expect(el.value).toBe('ab');
        } finally {
            again();
            vi.useRealTimers();
        }
    });

    it('a pending raise whose trigger left the page holds its radio group no more', async () => {
        await connect(
            `<input type="radio" id="r1" az="0" name="g" value="1" az-ctl="checked" az-debounce="blur" ${CHANGE} />` +
                `<input type="radio" id="r2" az="1" name="g" value="2" az-ctl="checked" ${CHANGE} />`,
        );
        const r1 = document.getElementById('r1');
        const r2 = document.getElementById('r2');
        edit(r1, (e) => {
            e.checked = true;
        });
        r1.remove();
        // The user's own state on r2 (dirty), so only a property write shows the push.
        r2.checked = false;
        mock.reply([setAttr(1, 'checked', '')], null);
        expect(r2.checked).toBe(true);
    });
});

describe('event pacing (az-debounce)', () => {
    it('a mode the server switches mid-window sends the latest raise in the new mode', async () => {
        vi.useFakeTimers();
        try {
            await connect(`<input id="q" az="0" name="q" value="" az-debounce="300" ${INPUT} />`);
            const el = document.querySelector('input');
            el.focus();
            edit(
                el,
                (e) => {
                    e.value = 'a';
                },
                'input',
            );
            el.setAttribute('az-debounce', 'blur');
            edit(
                el,
                (e) => {
                    e.value = 'ab';
                },
                'input',
            );
            vi.advanceTimersByTime(400);
            expect(mock.sent()).toEqual([]);
            el.blur();
            expect(mock.sent().map((f) => f[2])).toEqual([{ value: 'ab' }]);
        } finally {
            vi.useRealTimers();
        }
    });

    it("a form's az-debounce paces its field events, not its submit", async () => {
        await connect(
            `<form az-input='[0,"i"]' az-submit='[0,"s"]' az-debounce="blur">` +
                `<input id="a" az="0" name="a" value="" az-ctl="value" /></form>`,
        );
        const a = document.getElementById('a');
        edit(
            a,
            (e) => {
                e.value = 'x';
            },
            'input',
        );
        expect(mock.sent()).toEqual([]);
        document
            .querySelector('form')
            .dispatchEvent(new Event('submit', { bubbles: true, cancelable: true }));
        expect(mock.sent().map((f) => f[1])).toEqual(['i', 's']);
    });
    it("a form's az-throttle paces a second submit", async () => {
        vi.useFakeTimers();
        try {
            await connect(
                `<form id="f" az-throttle="1000" az-submit='[0,"s"]'><input name="q" value="x" /></form>`,
            );
            const f = document.getElementById('f');
            const submit = () =>
                f.dispatchEvent(new Event('submit', { bubbles: true, cancelable: true }));
            submit();
            submit();
            expect(mock.sent().filter((m) => m[1] === 's')).toHaveLength(1);
            vi.advanceTimersByTime(1000);
            expect(mock.sent().filter((m) => m[1] === 's')).toHaveLength(2);
        } finally {
            vi.useRealTimers();
        }
    });

    it("a form's az-throttle paces its submit beside an az-debounce", async () => {
        vi.useFakeTimers();
        try {
            await connect(
                `<form id="f" az-debounce="300" az-throttle="1000" az-submit='[0,"s"]' az-input='[0,"i"]'><input name="q" value="x" /></form>`,
            );
            const f = document.getElementById('f');
            const submit = () =>
                f.dispatchEvent(new Event('submit', { bubbles: true, cancelable: true }));
            submit();
            submit();
            expect(mock.sent().filter((m) => m[1] === 's')).toHaveLength(1);
        } finally {
            vi.useRealTimers();
        }
    });

    it("an event-name az-debounce on the submit leaves the submit's throttle window", async () => {
        vi.useFakeTimers();
        try {
            await connect(
                `<form id="f" az-debounce="submit" az-throttle="1000" az-input='[0,"i"]' az-submit='[0,"s"]'><input id="q" name="q" value="" /></form>`,
            );
            const f = document.getElementById('f');
            const q = document.getElementById('q');
            const submit = () =>
                f.dispatchEvent(new Event('submit', { bubbles: true, cancelable: true }));
            q.value = 'a';
            q.dispatchEvent(new Event('input', { bubbles: true }));
            submit();
            submit();
            expect(mock.sent().map((m) => m[1])).toEqual(['i', 's']);
            vi.advanceTimersByTime(1000);
            expect(mock.sent().map((m) => m[1])).toEqual(['i', 's', 's']);
        } finally {
            vi.useRealTimers();
        }
    });

    it("a form's blur debounce flush leaves the submit's throttle window", async () => {
        vi.useFakeTimers();
        try {
            await connect(
                `<form id="f" tabindex="-1" az-debounce="blur" az-throttle="1000" az-input='[0,"i"]' az-submit='[0,"s"]'><input id="q" name="q" value="" /></form>`,
            );
            const f = document.getElementById('f');
            const q = document.getElementById('q');
            const submit = () =>
                f.dispatchEvent(new Event('submit', { bubbles: true, cancelable: true }));
            q.value = 'a';
            q.dispatchEvent(new Event('input', { bubbles: true }));
            submit();
            f.dispatchEvent(new Event('blur'));
            submit();
            expect(mock.sent().map((m) => m[1])).toEqual(['i', 's']);
            vi.advanceTimersByTime(1000);
            expect(mock.sent().map((m) => m[1])).toEqual(['i', 's', 's']);
        } finally {
            vi.useRealTimers();
        }
    });

    it("a form's blur auto-flush neither reopens nor hurries the submit's throttle", async () => {
        vi.useFakeTimers();
        try {
            await connect(
                `<form id="f" tabindex="-1" az-throttle="1000" az-submit='[0,"s"]'><input name="q" value="x" /></form>`,
            );
            const f = document.getElementById('f');
            const submit = () =>
                f.dispatchEvent(new Event('submit', { bubbles: true, cancelable: true }));
            const sent = () => mock.sent().filter((m) => m[1] === 's');
            submit();
            f.dispatchEvent(new Event('blur'));
            submit();
            expect(sent()).toHaveLength(1);
            // The trailing submit waits for its window, blur or not.
            f.dispatchEvent(new Event('blur'));
            expect(sent()).toHaveLength(1);
            vi.advanceTimersByTime(1000);
            expect(sent()).toHaveLength(2);
        } finally {
            vi.useRealTimers();
        }
    });

    it('a form-level debounced input goes out before the submit, once', async () => {
        vi.useFakeTimers();
        try {
            await connect(
                `<form id="f" az-debounce="300" az-input='[0,"i"]' az-submit='[0,"s"]'><input id="q" name="q" value="" /></form>`,
            );
            const q = document.getElementById('q');
            q.value = 'a';
            q.dispatchEvent(new Event('input', { bubbles: true }));
            document
                .getElementById('f')
                .dispatchEvent(new Event('submit', { bubbles: true, cancelable: true }));
            expect(mock.sent().map((m) => m[1])).toEqual(['i', 's']);
            vi.advanceTimersByTime(1000);
            expect(mock.sent().map((m) => m[1])).toEqual(['i', 's']);
        } finally {
            vi.useRealTimers();
        }
    });

    it('a debounce switched to throttle mid-window sends the latest raise once', async () => {
        vi.useFakeTimers();
        try {
            await connect(`<input id="q" az="0" name="q" value="" az-debounce="300" ${INPUT} />`);
            const el = document.querySelector('input');
            edit(
                el,
                (e) => {
                    e.value = 'a';
                },
                'input',
            );
            el.removeAttribute('az-debounce');
            el.setAttribute('az-throttle', '300');
            edit(
                el,
                (e) => {
                    e.value = 'b';
                },
                'input',
            );
            vi.advanceTimersByTime(1000);
            expect(mock.sent().map((m) => m[2].value)).toEqual(['b']);
        } finally {
            vi.useRealTimers();
        }
    });
});
