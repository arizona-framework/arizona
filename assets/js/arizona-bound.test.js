import { afterEach, describe, expect, it, vi } from 'vitest';

// A server-owned form control (`az-bound`) shows the server's value. The browser
// keeps the attribute (the default, what the server writes) apart from the property
// (what is shown), and the diff only ships changes, so an edit the server refuses,
// or the echo of an older keystroke, would otherwise leave the property wrong.

const SET_ATTR = 1;
const REM_ATTR = 2;
const TEXT = 0;

/** @type {ReturnType<typeof start> extends Promise<infer T> ? T : never} */
let conn;

afterEach(() => {
    conn?.restore();
    conn = /** @type {any} */ (null);
    vi.useRealTimers();
    vi.restoreAllMocks();
    document.body.innerHTML = '';
});

/**
 * A fresh module connected through a mocked Worker, with `html` on the page and
 * the event names delegated.
 * @param {string} html
 * @param {{open?: boolean, before?: (mod: any) => void}} [opts]
 */
async function start(html, opts = {}) {
    vi.resetModules();
    const mod = await import('./arizona.js');
    document.body.innerHTML = html;
    opts.before?.(mod);
    /** @type {any[]} */
    const posted = [];
    /** @type {any} */
    let onmsg = null;
    const inst = {
        postMessage: (/** @type {any} */ d) => posted.push(d),
        set onmessage(fn) {
            onmsg = fn;
        },
        get onmessage() {
            return onmsg;
        },
        terminate: vi.fn(),
    };
    const Orig = globalThis.Worker;
    globalThis.Worker = /** @type {any} */ (
        function () {
            return inst;
        }
    );
    const disconnect = mod.connect('/ws');
    const frame = (/** @type {any[]} */ data) => inst.onmessage({ data });
    const c = {
        mod,
        disconnect,
        frame,
        open: (reconnect = false) => frame([1, reconnect]),
        /** A server frame; `ref` answers the client frame carrying it. */
        msg: (ops = null, effects = null, ref = null, resync = false) =>
            frame([0, ops, effects, resync, null, ref]),
        close: (code = 1006) => frame([2, code]),
        sent: () => posted.filter((d) => d[0] === 1).map((d) => JSON.parse(d[1])),
        /** The reply ref the last frame sent carries, or null. */
        lastRef() {
            const ref = c.sent().at(-1)?.at(-1);
            return typeof ref === 'number' ? ref : null;
        },
        restore() {
            disconnect();
            globalThis.Worker = Orig;
        },
    };
    if (opts.open !== false) {
        c.open();
        c.msg(null, null); // mounted
        frame([0, null, null, false, ['az-input', 'az-change', 'az-click'], null]);
    }
    conn = c;
    return c;
}

const $ = (/** @type {string} */ sel) => /** @type {any} */ (document.querySelector(sel));

/** Type `v` into a text control, as the user would. */
function type(/** @type {any} */ el, /** @type {string} */ v) {
    el.value = v;
    el.dispatchEvent(new Event('input', { bubbles: true }));
}

/**
 * Make what a test set on control `el` the user's edit: an `input` on it, or a
 * `change` where an `input` would propose.
 */
function touch(/** @type {any} */ el) {
    el.dispatchEvent(new Event(el.closest('[az-input]') ? 'change' : 'input', { bubbles: true }));
}

function change(/** @type {any} */ el) {
    el.dispatchEvent(new Event('change', { bubbles: true }));
}

const INPUT = `<div id="v" az-view><input az="0" name="q" value="" az-bound="value" az-input='[0,"edit"]'></div>`;

describe('a refused edit', () => {
    it('shows the server value again when the reply changes nothing', async () => {
        const c = await start(INPUT);
        type($('input'), 'Capital');
        c.msg(null, null, c.lastRef());
        expect($('input').value).toBe('');
    });

    // A lone surrogate is escaped by JSON.stringify as `\\ud800`, which the server
    // cannot decode, so it drops the frame and its ref is never answered.
    it('sends a lone surrogate the server can read', async () => {
        const c = await start(INPUT);
        type($('input'), 'a\ud800');
        expect(c.sent().at(-1)).toEqual(['v', 'edit', { value: 'a\ufffd' }, expect.any(Number)]);
    });

    it('tags every proposal frame with a reply ref', async () => {
        const c = await start(INPUT);
        type($('input'), 'a');
        expect(c.sent().at(-1)).toEqual(['v', 'edit', { value: 'a' }, expect.any(Number)]);
    });

    it('resets a select whose option the server kept', async () => {
        const c = await start(
            `<div id="v" az-view><select name="s" az-change='[0,"pick"]'>` +
                `<option value="a" selected az-bound="selected">A</option>` +
                `<option value="b" az-bound="selected">B</option></select></div>`,
        );
        $('select').value = 'b';
        change($('select'));
        c.msg(null, null, c.lastRef());
        expect($('select').value).toBe('a');
    });

    // A select whose server-rendered value picks among options the compiler could not
    // mark (a component renders them) owns them through `az-bound="value"`.
    const valued = (/** @type {string} */ slot = '') =>
        `<div id="v" az-view><select az="s" name="s" value="a" az-bound="value" az-change='[0,"pick"]'>` +
        `<option value="a" selected>A</option><option value="b">B</option>${slot}</select></div>`;
    it('resets a select its value owns when the server refuses the pick', async () => {
        const c = await start(valued());
        $('select').value = 'b';
        change($('select'));
        c.msg(null, null, c.lastRef());
        expect($('select').value).toBe('a');
    });
    it('holds a value write to a select a proposal waits on, then shows it', async () => {
        const c = await start(valued());
        $('select').value = 'b';
        change($('select'));
        const ref = c.lastRef();
        c.msg([[SET_ATTR, 'v:s', 'value', 'b']]);
        expect([$('select').getAttribute('value'), $('select').value]).toEqual(['a', 'b']);
        c.msg(null, null, ref);
        expect([$('select').getAttribute('value'), $('select').value]).toEqual(['b', 'b']);
    });
    // An option a batch renders while a proposal waits is marked by the select's value
    // only once the select is released: nothing writes a held control.
    it('marks an option a batch adds to a held select only at the release', async () => {
        const c = await start(valued('<!--az:s:0--><!--/az-->'));
        $('select').value = 'b';
        change($('select'));
        const ref = c.lastRef();
        c.msg([[TEXT, 'v:s:0', '<option value="a">A2</option>', true]]);
        const added = /** @type {any} */ ($('select').options[2]);
        expect(added.defaultSelected).toBe(false);
        c.msg(null, null, ref);
        expect(added.defaultSelected).toBe(true);
    });

    it('resets the bound controls of a form trigger', async () => {
        const c = await start(
            `<div id="v" az-view><form id="f" az-change='[0,"validate"]'>` +
                `<input name="q" value="" az-bound="value"></form></div>`,
        );
        $('input').value = 'x';
        change($('input'));
        c.msg(null, null, c.lastRef());
        expect($('input').value).toBe('');
    });

    // The submit listener prevents the native submission, which reverts nothing.
    it('resets the bound controls of a submitted form', async () => {
        const c = await start(
            `<div id="v" az-view><form id="f" az-submit='[0,"save"]'>` +
                `<input name="q" value="" az-bound="value"></form></div>`,
        );
        $('input').value = 'x';
        $('form').dispatchEvent(new Event('submit', { bubbles: true, cancelable: true }));
        expect(c.sent().at(-1)).toEqual(['v', 'save', { q: 'x' }, expect.any(Number)]);
        c.msg(null, null, c.lastRef());
        expect($('input').value).toBe('');
    });

    it('restores the default radio of a group the server kept', async () => {
        const c = await start(
            `<div id="v" az-view>` +
                `<input type="radio" name="r" value="a" checked az-bound="checked" az-change='[0,"pick"]'>` +
                `<input type="radio" name="r" value="b" az-bound="checked" az-change='[0,"pick"]'>` +
                `</div>`,
        );
        $('[value=b]').click();
        expect($('[value=b]').checked).toBe(true);
        c.msg(null, null, c.lastRef());
        expect($('[value=a]').checked).toBe(true);
        expect($('[value=b]').checked).toBe(false);
    });

    it('takes a radio associated by form="id" into its group, and no other form', async () => {
        const c = await start(
            `<div id="v" az-view>` +
                `<form id="f"><input type="radio" name="r" value="b" az-bound="checked" az-change='[0,"pick"]'></form>` +
                `<input type="radio" form="f" name="r" value="a" checked az-bound="checked">` +
                `<form id="g"><input type="radio" name="r" value="c" checked az-bound="checked">` +
                `<input type="radio" name="r" value="d" az-bound="checked"></form>` +
                `</div>`,
        );
        $('[value=d]').checked = true; // another form's group, never proposed
        $('[value=b]').click();
        c.msg(null, null, c.lastRef());
        expect($('[value=a]').checked).toBe(true);
        expect($('[value=b]').checked).toBe(false);
        expect($('[value=d]').checked).toBe(true);
    });

    it('notifies a hook on the control it corrected', async () => {
        const updated = vi.fn();
        const c = await start(
            `<div id="v" az-view><input az-hook="H" name="q" value="" az-bound="value" az-input='[0,"edit"]'></div>`,
            { before: (mod) => (mod.hooks.H = { updated }) },
        );
        type($('input'), 'x');
        c.msg(null, null, c.lastRef());
        expect(updated).toHaveBeenCalled();
    });
});

describe('a reconciled radio group', () => {
    // Checking the default radio unchecks the user's choice implicitly, and an
    // implicit change notifies no hook: the choice is unchecked first, explicitly.
    it('notifies a hook on the radio it unchecked', async () => {
        const updated = vi.fn();
        const c = await start(
            `<div id="v" az-view>` +
                `<input type="radio" name="r" value="a" checked az-bound="checked" az-change='[0,"pick"]'>` +
                `<input az-hook="H" type="radio" name="r" value="b" az-bound="checked" az-change='[0,"pick"]'>` +
                `</div>`,
            { before: (mod) => (mod.hooks.H = { updated }) },
        );
        $('[value=b]').click();
        c.msg(null, null, c.lastRef());
        expect($('[value=a]').checked).toBe(true);
        expect(updated).toHaveBeenCalled();
    });

    // A browser groups no radio without a name: each is its own unit, so the
    // reconcile of one leaves another's state alone.
    it('is not every nameless radio', async () => {
        const c = await start(
            `<div id="v" az-view>` +
                `<input id="a" az="0" type="radio" az-bound="checked" az-change='[0,"a"]'>` +
                `<input id="b" type="radio" az-bound="checked"></div>`,
        );
        $('#b').checked = true;
        $('#a').click();
        c.msg([[SET_ATTR, 'v:0', 'checked', '']]);
        c.msg(null, null, c.lastRef());
        expect($('#a').checked).toBe(true);
        expect($('#b').checked).toBe(true);
    });
});

describe('an older echo', () => {
    it('does not overwrite newer typing', async () => {
        const c = await start(INPUT);
        type($('input'), 'a');
        const r1 = c.lastRef();
        type($('input'), 'ab');
        const r2 = c.lastRef();
        c.msg([[SET_ATTR, 'v:0', 'value', 'a']], null, r1);
        expect($('input').value).toBe('ab');
        c.msg([[SET_ATTR, 'v:0', 'value', 'ab']], null, r2);
        expect($('input').value).toBe('ab');
        expect($('input').defaultValue).toBe('ab');
    });

    it('does not overwrite newer typing in a stream item', async () => {
        const c = await start(
            `<div id="v" az-view><ul az="0"><li az-key="k1">` +
                `<input az="1" name="q" value="" az-bound="value" az-input='[0,"edit"]'></li></ul></div>`,
        );
        type($('input'), 'a');
        const r1 = c.lastRef();
        type($('input'), 'ab');
        c.msg([[7, 'v:0', 'k1', [[SET_ATTR, '1', 'value', 'a']]]], null, r1);
        expect($('input').value).toBe('ab');
        expect($('input').defaultValue).toBe('');
    });

    it('does not undo a newer checkbox toggle with an older removal in a stream item', async () => {
        const c = await start(
            `<div id="v" az-view><ul az="0"><li az-key="k1"><input az="1" type="checkbox" name="c" ` +
                `checked az-bound="checked" az-change='[0,"tick"]'></li></ul></div>`,
        );
        const cb = $('input');
        cb.click(); // off
        const r1 = c.lastRef();
        cb.click(); // on again
        c.msg([[7, 'v:0', 'k1', [[REM_ATTR, '1', 'checked']]]], null, r1);
        expect(cb.checked).toBe(true);
    });

    it('does not overwrite newer typing in a textarea', async () => {
        const c = await start(
            `<div id="v" az-view><textarea az="0" name="t" az-bound="value" az-input='[0,"edit"]'></textarea></div>`,
        );
        const ta = $('textarea');
        type(ta, 'a');
        const r1 = c.lastRef();
        type(ta, 'ab');
        const r2 = c.lastRef();
        c.msg([[TEXT, 'v:0', 'a']], null, r1);
        expect(ta.value).toBe('ab');
        c.msg([[TEXT, 'v:0', 'ab']], null, r2);
        expect(ta.value).toBe('ab');
    });

    it('does not undo a newer checkbox toggle with an older removal', async () => {
        const c = await start(
            `<div id="v" az-view><input az="0" type="checkbox" name="c" checked az-bound="checked" az-change='[0,"tick"]'></div>`,
        );
        const cb = $('input');
        cb.click(); // off
        const r1 = c.lastRef();
        cb.click(); // on again
        const r2 = c.lastRef();
        c.msg([[REM_ATTR, 'v:0', 'checked']], null, r1);
        expect(cb.checked).toBe(true);
        c.msg([[SET_ATTR, 'v:0', 'checked', '']], null, r2);
        expect(cb.checked).toBe(true);
    });

    it('does not move a newer radio choice back with an older one', async () => {
        const c = await start(
            `<div id="v" az-view>` +
                `<input az="0" type="radio" name="r" value="a" checked az-bound="checked" az-change='[0,"pick"]'>` +
                `<input az="1" type="radio" name="r" value="b" az-bound="checked" az-change='[0,"pick"]'>` +
                `<input az="2" type="radio" name="r" value="c" az-bound="checked" az-change='[0,"pick"]'>` +
                `</div>`,
        );
        $('[value=b]').click();
        const r1 = c.lastRef();
        $('[value=c]').click();
        c.msg(
            [
                [REM_ATTR, 'v:0', 'checked'],
                [SET_ATTR, 'v:1', 'checked', ''],
            ],
            null,
            r1,
        );
        expect($('[value=c]').checked).toBe(true);
    });
});

describe('what is a proposal', () => {
    // A form proposes what FormData would submit: a disabled or unnamed control is
    // not sent, so the server never answers for it.
    it('is not a disabled or unnamed control of a form trigger', async () => {
        const c = await start(
            `<div id="v" az-view><form id="f" az-change='[0,"validate"]'>` +
                `<input id="q" name="q" value="" az-bound="value">` +
                `<input id="d" name="d" value="" disabled az-bound="value">` +
                `<input id="n" value="" az-bound="value"></form></div>`,
        );
        $('#d').value = 'x';
        $('#n').value = 'y';
        $('#q').value = 'z';
        change($('#q'));
        c.msg(null, null, c.lastRef());
        expect($('#q').value).toBe('');
        expect($('#d').value).toBe('x');
        expect($('#n').value).toBe('y');
    });

    it('is not a control of a disabled fieldset in a form trigger', async () => {
        const c = await start(
            `<div id="v" az-view><form id="f" az-change='[0,"validate"]'>` +
                `<input id="q" name="q" value="" az-bound="value">` +
                `<fieldset disabled><input id="d" name="d" value="" az-bound="value"></fieldset>` +
                `</form></div>`,
        );
        $('#d').value = 'x';
        $('#q').value = 'z';
        change($('#q'));
        c.msg(null, null, c.lastRef());
        expect($('#d').value).toBe('x');
    });

    it('is not a div trigger holding bound controls', async () => {
        const c = await start(
            `<div id="v" az-view><div az-click='[0,"go"]'>` +
                `<input name="q" value="" az-bound="value"></div></div>`,
        );
        $('input').value = 'x';
        $('[az-click]').click();
        expect(c.sent().at(-1)).toHaveLength(3);
        c.msg(null, null, null);
        expect($('input').value).toBe('x');
    });

    // The controlled checkbox: the payload says checked, the browser reverts the
    // click, and the server's SET_ATTR must reach the property.
    it('is not an event whose default was prevented', async () => {
        const c = await start(
            `<div id="v" az-view><input az="0" type="checkbox" name="c" az-bound="checked" ` +
                `az-prevent-default az-click='[0,"tick"]'></div>`,
        );
        const cb = $('input');
        cb.click();
        expect(cb.checked).toBe(false);
        expect(c.sent().at(-1)).toEqual(['v', 'tick', { value: 'on', checked: true }]);
        c.msg([[SET_ATTR, 'v:0', 'checked', '']]);
        expect(cb.checked).toBe(true);
    });

    it('is not a hook pushEvent', async () => {
        /** @type {any} */
        let inst = null;
        const c = await start(INPUT.replace('<input', '<input az-hook="H"'), {
            before: (mod) =>
                (mod.hooks.H = {
                    mounted() {
                        inst = this;
                    },
                }),
        });
        $('input').value = 'x';
        inst.pushEvent('edit', { value: 'x' });
        expect(c.sent().at(-1)).toHaveLength(3);
    });

    it('is not a push_event run against a view element (a fetch response, an effect)', async () => {
        const c = await start(INPUT);
        $('input').value = 'x';
        c.mod.executeJS($('#v'), null, [0, 'done']);
        c.msg(null, [[0, 'done']]);
        expect(c.sent().slice(-2)).toEqual([
            ['v', 'done', {}],
            [null, 'done', {}],
        ]);
        expect($('input').value).toBe('x');
    });

    // A checkbox's `value` merely reflects its attribute, so `az-bound="value"`
    // leaves its checkedness client-owned.
    it('is not a control with no reconcilable bound property', async () => {
        const c = await start(
            `<div id="v" az-view><input type="checkbox" name="c" value="1" az-bound="value" az-change='[0,"tick"]'></div>`,
        );
        $('input').click();
        expect(c.sent().at(-1)).toHaveLength(3);
        c.msg(null, null, null);
        expect($('input').checked).toBe(true);
    });

    it('is not a control in an az-nodiff element or outside every view', async () => {
        const c = await start(
            `<input id="out" name="o" value="" az-bound="value" az-input='[0,"edit"]'>` +
                `<div id="v" az-view><div az-nodiff><input id="nd" name="n" value="" az-bound="value" az-input='[0,"edit"]'></div></div>`,
        );
        type($('#out'), 'x');
        expect(c.sent().at(-1)).toHaveLength(3);
        type($('#nd'), 'y');
        expect(c.sent().at(-1)).toHaveLength(3);
        c.msg(null, null, null);
        expect($('#out').value).toBe('x');
        expect($('#nd').value).toBe('y');
    });

    // A child view under an az-nodiff element is registered and diffs its own template;
    // a view whose own root is az-nodiff never diffs.
    it('is a control of a live child view inside an az-nodiff element', async () => {
        const c = await start(
            `<div id="v" az-view><div az-nodiff><div id="c" az-view>` +
                `<input az="0" name="q" value="" az-bound="value" az-input='[0,"edit"]'>` +
                `</div></div></div>`,
        );
        type($('input'), 'x');
        expect(c.sent().at(-1)).toEqual(['c', 'edit', { value: 'x' }, expect.any(Number)]);
        c.msg(null, null, c.lastRef());
        expect($('input').value).toBe('');
    });

    it('is not a control of a view whose root is az-nodiff', async () => {
        const c = await start(
            `<div id="v" az-view><div id="c" az-view az-nodiff>` +
                `<input name="q" value="" az-bound="value" az-input='[0,"edit"]'></div></div>`,
        );
        type($('input'), 'x');
        expect(c.sent().at(-1)).toHaveLength(3);
        c.msg(null, null, null);
        expect($('input').value).toBe('x');
    });

    it('shares one reply ref with the view transition it is raised under', async () => {
        const c = await start(
            `<div id="v" az-view><input az="0" name="q" value="" az-bound="value" az-input='[20,{},[0,"edit"]]'></div>`,
        );
        type($('input'), 'a');
        const sent = c.sent().at(-1);
        expect(sent).toHaveLength(4);
        expect(typeof sent[3]).toBe('number');
        c.msg(null, null, sent[3]);
        expect($('input').value).toBe('');
    });
});

describe('the capture lifetime', () => {
    const DEBOUNCED = INPUT.replace('<input', '<input az-debounce="300"');

    it('covers the debounce window before the send', async () => {
        vi.useFakeTimers();
        const c = await start(DEBOUNCED);
        type($('input'), 'ab');
        c.msg([[SET_ATTR, 'v:0', 'value', 'srv']]);
        expect($('input').value).toBe('ab');
    });

    it('ends for a superseded debounced send', async () => {
        vi.useFakeTimers();
        const c = await start(DEBOUNCED);
        type($('input'), 'a');
        type($('input'), 'ab');
        vi.advanceTimersByTime(300);
        expect(c.sent().filter((s) => s[1] === 'edit')).toHaveLength(1);
        c.msg(null, null, c.lastRef());
        expect($('input').value).toBe('');
    });

    it('ends for a debounced send dropped while disconnected', async () => {
        vi.useFakeTimers();
        const c = await start(DEBOUNCED);
        type($('input'), 'ab');
        c.close();
        vi.spyOn(console, 'warn').mockImplementation(() => {});
        vi.advanceTimersByTime(300);
        c.mod.applyOps([[SET_ATTR, 'v:0', 'value', 'srv']]);
        expect($('input').value).toBe('srv');
    });

    // A send parked for a named event never fires once its control is gone, and a
    // radio group outlives one radio: its stale capture would block every later
    // reconcile of the group.
    it('ends for a send parked on a named event when its control is removed', async () => {
        const c = await start(
            `<div id="v" az-view>` +
                `<input az="0" type="radio" name="r" value="a" checked az-bound="checked" az-change='[0,"pick"]'>` +
                `<input az="1" type="radio" name="r" value="b" az-bound="checked" az-debounce="commit" az-change='[0,"pick"]'>` +
                `<input az="2" type="radio" name="r" value="c" az-bound="checked" az-change='[0,"pick"]'>` +
                `</div>`,
        );
        $('[value=b]').click();
        c.msg([[4, 'v:1']]);
        $('[value=c]').click();
        c.msg(null, null, c.lastRef());
        expect($('[value=a]').checked).toBe(true);
        expect($('[value=c]').checked).toBe(false);
    });

    it('keeps a send parked on a named event when its control only moved', async () => {
        const c = await start(
            `<div id="v" az-view><input az="0" name="q" value="" az-bound="value" ` +
                `az-debounce="commit" az-input='[0,"edit"]'><p id="p"></p></div>`,
        );
        type($('input'), 'ab');
        $('#p').append($('input')); // a move: out and back in within one batch
        c.msg(null);
        $('input').dispatchEvent(new Event('commit'));
        expect(c.sent().at(-1)).toEqual(['v', 'edit', { value: 'ab' }, expect.any(Number)]);
    });

    it('ends for an event the page-swap window drops', async () => {
        const c = await start(INPUT);
        c.mod.executeJS($('#v'), null, [10, '/next']);
        vi.spyOn(console, 'warn').mockImplementation(() => {});
        type($('input'), 'ab');
        c.mod.applyOps([[SET_ATTR, 'v:0', 'value', 'srv']]);
        expect($('input').value).toBe('srv');
    });

    it('ends when the socket closes', async () => {
        const c = await start(INPUT);
        type($('input'), 'ab');
        c.close();
        c.mod.applyOps([[SET_ATTR, 'v:0', 'value', 'srv']]);
        expect($('input').value).toBe('srv');
    });

    it('ends on pagehide', async () => {
        const c = await start(INPUT);
        type($('input'), 'ab');
        window.dispatchEvent(new PageTransitionEvent('pagehide', { persisted: true }));
        c.mod.applyOps([[SET_ATTR, 'v:0', 'value', 'srv']]);
        expect($('input').value).toBe('srv');
    });

    it('ends on disconnect()', async () => {
        const c = await start(INPUT);
        type($('input'), 'ab');
        c.disconnect();
        c.mod.applyOps([[SET_ATTR, 'v:0', 'value', 'srv']]);
        expect($('input').value).toBe('srv');
    });
});

// While a unit is held, a diff write to its bound attributes or a textarea's content
// is not applied at all (Firefox clears a partial date when its value ATTRIBUTE
// changes): the latest one per element and attribute is applied when the unit is
// released, before the reconcile.
describe('a held write', () => {
    it('is applied on the reply, and the reconcile shows it', async () => {
        const c = await start(INPUT);
        type($('input'), 'ab');
        const ref = c.lastRef();
        c.msg([[SET_ATTR, 'v:0', 'value', 'srv']]);
        expect($('input').getAttribute('value')).toBe('');
        c.msg([[SET_ATTR, 'v:0', 'value', 'srv2']]);
        c.msg(null, null, ref);
        expect($('input').getAttribute('value')).toBe('srv2');
        expect($('input').value).toBe('srv2');
    });

    it('defers a removal', async () => {
        const c = await start(
            `<div id="v" az-view><input az="0" type="checkbox" name="c" checked az-bound="checked" az-change='[0,"tick"]'></div>`,
        );
        const cb = $('input');
        cb.click(); // off
        const ref = c.lastRef();
        cb.click(); // on again, still proposed by ref's capture
        const ref2 = c.lastRef();
        c.msg([[REM_ATTR, 'v:0', 'checked']], null, ref);
        expect(cb.hasAttribute('checked')).toBe(true);
        c.msg(null, null, ref2);
        expect(cb.hasAttribute('checked')).toBe(false);
        expect(cb.checked).toBe(false); // the reply kept the server's unchecked state
    });

    it("defers a textarea's content", async () => {
        const c = await start(
            `<div id="v" az-view><textarea az="0" name="t" az-bound="value" az-input='[0,"edit"]'>a</textarea></div>`,
        );
        const ta = $('textarea');
        type(ta, 'ab');
        const ref = c.lastRef();
        c.msg([[TEXT, 'v:0', 'srv']]);
        expect(ta.textContent).toBe('a');
        c.msg(null, null, ref);
        expect(ta.textContent).toBe('srv');
        expect(ta.value).toBe('srv');
    });

    it('in a stream item', async () => {
        const c = await start(
            `<div id="v" az-view><ul az="0"><li az-key="k1">` +
                `<input az="1" name="q" value="" az-bound="value" az-input='[0,"edit"]'></li></ul></div>`,
        );
        type($('input'), 'ab');
        const ref = c.lastRef();
        c.msg([[7, 'v:0', 'k1', [[SET_ATTR, '1', 'value', 'srv']]]]);
        expect($('input').getAttribute('value')).toBe('');
        c.msg(null, null, ref);
        expect($('input').value).toBe('srv');
    });

    it('leaves the attributes the server does not own immediate', async () => {
        const c = await start(INPUT);
        type($('input'), 'ab');
        c.msg([[SET_ATTR, 'v:0', 'class', 'err']]);
        expect($('input').getAttribute('class')).toBe('err');
    });

    it('runs the hook update when it is applied', async () => {
        const updated = vi.fn();
        const c = await start(
            `<div id="v" az-view><input az="0" az-hook="H" name="q" value="" az-bound="value" az-input='[0,"edit"]'></div>`,
            { before: (mod) => (mod.hooks.H = { updated }) },
        );
        type($('input'), 'ab');
        const ref = c.lastRef();
        c.msg([[SET_ATTR, 'v:0', 'value', 'ab']]);
        expect(updated).not.toHaveBeenCalled();
        c.msg(null, null, ref);
        expect(updated).toHaveBeenCalled();
        expect($('input').getAttribute('value')).toBe('ab');
    });

    it('is applied when the proposal is dropped, with no reconcile', async () => {
        const c = await start(INPUT);
        type($('input'), 'ab');
        c.msg([[SET_ATTR, 'v:0', 'value', 'srv']]);
        c.close();
        expect($('input').getAttribute('value')).toBe('srv');
        expect($('input').value).toBe('ab');
    });

    it('is dropped with an element removed while held', async () => {
        const updated = vi.fn();
        const c = await start(
            `<div id="v" az-view><input az="0" az-hook="H" name="q" value="" az-bound="value" az-input='[0,"edit"]'></div>`,
            { before: (mod) => (mod.hooks.H = { updated }) },
        );
        const el = $('input');
        type(el, 'ab');
        const ref = c.lastRef();
        c.msg([[SET_ATTR, 'v:0', 'value', 'srv']]);
        c.msg([[4, 'v:0']]);
        c.msg(null, null, ref);
        expect(el.isConnected).toBe(false);
        expect(el.getAttribute('value')).toBe('');
        expect(updated).not.toHaveBeenCalled();
    });

    // A partial date or a number mid-"1e-": an attribute write could clear what is
    // being typed, so the write waits for the next release.
    it('waits past a release while the control shows bad input', async () => {
        const c = await start(INPUT.replace('value=""', 'type="number" value="5"'));
        const el = $('input');
        const bad = { badInput: true };
        Object.defineProperty(el, 'validity', { get: () => bad });
        type(el, '');
        c.msg([[SET_ATTR, 'v:0', 'value', '']], null, c.lastRef());
        expect(el.getAttribute('value')).toBe('5');
        bad.badInput = false;
        type(el, '7');
        c.msg([[SET_ATTR, 'v:0', 'value', '7']], null, c.lastRef());
        expect(el.getAttribute('value')).toBe('7');
        expect(el.value).toBe('7');
    });

    // Nothing proposes again after the bad input clears (an az-change field): the
    // kept writes go out on the field's next input, else the default stays stale
    // and a form reset shows a value the server no longer has.
    it('kept past a release is applied on the next input once the input is good', async () => {
        const c = await start(
            `<div id="v" az-view><form id="f"><input az="0" type="number" name="q" value="5" ` +
                `az-bound="value" az-change='[0,"edit"]'></form></div>`,
        );
        const el = $('input');
        const bad = { badInput: false };
        Object.defineProperty(el, 'validity', { get: () => bad });
        el.value = '6';
        change(el);
        const ref = c.lastRef();
        c.msg([[SET_ATTR, 'v:0', 'value', '6']]);
        bad.badInput = true;
        el.value = '';
        c.msg(null, null, ref);
        expect(el.getAttribute('value')).toBe('5');
        el.dispatchEvent(new Event('input', { bubbles: true }));
        expect(el.getAttribute('value')).toBe('5'); // still bad
        bad.badInput = false;
        type(el, '7');
        expect(el.getAttribute('value')).toBe('6');
        expect(el.value).toBe('7');
        $('form').reset();
        expect(el.value).toBe('6');
    });

    it('kept past a release is applied on focusout once the input is good', async () => {
        const c = await start(INPUT.replace('value=""', 'type="number" value="5"'));
        const el = $('input');
        const bad = { badInput: true };
        Object.defineProperty(el, 'validity', { get: () => bad });
        type(el, '');
        c.msg([[SET_ATTR, 'v:0', 'value', '6']], null, c.lastRef());
        bad.badInput = false;
        el.dispatchEvent(new FocusEvent('focusout', { bubbles: true }));
        expect(el.getAttribute('value')).toBe('6');
    });

    /** Whether an op to a bound control no proposal holds asks anything of it. */
    async function opPathIdle(/** @type {any} */ c) {
        document.body.insertAdjacentHTML(
            'beforeend',
            `<div id="w" az-view><input id="other" az="0" name="o" value="" az-bound="value"></div>`,
        );
        const spy = vi.spyOn($('#other'), 'getAttribute');
        c.msg([[SET_ATTR, 'w:0', 'value', 'x']]);
        return !spy.mock.calls.some(([n]) => n === 'az-bound');
    }

    it('is let go with an element removed while it shows bad input', async () => {
        const c = await start(INPUT.replace('value=""', 'type="number" value="5"'));
        const el = $('input');
        Object.defineProperty(el, 'validity', { get: () => ({ badInput: true }) });
        type(el, '');
        const ref = c.lastRef();
        c.msg([[SET_ATTR, 'v:0', 'value', '6']]);
        c.msg([[4, 'v:0']]);
        c.msg(null, null, ref);
        expect(await opPathIdle(c)).toBe(true);
    });

    it('kept past a release is let go when its element is removed', async () => {
        const c = await start(INPUT.replace('value=""', 'type="number" value="5"'));
        const el = $('input');
        Object.defineProperty(el, 'validity', { get: () => ({ badInput: true }) });
        type(el, '');
        c.msg([[SET_ATTR, 'v:0', 'value', '6']], null, c.lastRef());
        c.msg([[4, 'v:0']]);
        expect(el.getAttribute('value')).toBe('5');
        expect(await opPathIdle(c)).toBe(true);
    });

    // A dirty control ignores a default write, so only the reconcile waits for a
    // composition: the writes go out on the reply.
    it('is applied on the reply during an IME composition, the reconcile after it', async () => {
        const c = await start(INPUT);
        const el = $('input');
        el.dispatchEvent(new Event('compositionstart', { bubbles: true }));
        type(el, 'か');
        c.msg([[SET_ATTR, 'v:0', 'value', 'srv']], null, c.lastRef());
        expect(el.getAttribute('value')).toBe('srv');
        expect(el.value).toBe('か');
        el.dispatchEvent(new Event('compositionend', { bubbles: true }));
        expect(el.value).toBe('srv');
    });

    it('kept past a release is superseded by a later write', async () => {
        const c = await start(INPUT.replace('value=""', 'type="number" value="5"'));
        const el = $('input');
        const bad = { badInput: true };
        Object.defineProperty(el, 'validity', { get: () => bad });
        type(el, '');
        c.msg([[SET_ATTR, 'v:0', 'value', '']], null, c.lastRef());
        c.msg([[SET_ATTR, 'v:0', 'value', '9']]); // a server push, nothing held
        bad.badInput = false;
        type(el, '9');
        c.msg(null, null, c.lastRef());
        expect(el.getAttribute('value')).toBe('9');
    });
});

describe('only the diff path is held', () => {
    it('still lets a set_attr effect write the property', async () => {
        const c = await start(INPUT.replace('<input', '<input id="i"'));
        type($('input'), 'ab');
        c.msg(null, [[7, '#i', 'value', 'eff']]);
        expect($('input').value).toBe('eff');
    });
});

describe('the reconcile', () => {
    it('keeps what the user typed after the captured value', async () => {
        const c = await start(INPUT.replace('az-input', 'az-change'));
        $('input').value = 'ab';
        change($('input'));
        $('input').value = 'abc';
        c.msg(null, null, c.lastRef());
        expect($('input').value).toBe('abc');
    });

    // A reply may add or remove a radio or an option: the members the capture saw
    // and that are still there decide; a new one counts as unchanged.
    it('resets a select whose reply adds an option', async () => {
        const c = await start(
            `<div id="v" az-view><select name="s" az-change='[0,"pick"]'>` +
                `<option value="a" selected az-bound="selected">A</option>` +
                `<option value="b" az-bound="selected">B</option></select></div>`,
        );
        $('select').value = 'b';
        change($('select'));
        $('select').insertAdjacentHTML(
            'beforeend',
            `<option value="c" az-bound="selected">C</option>`,
        );
        c.msg(null, null, c.lastRef());
        expect($('select').value).toBe('a');
    });

    it('resets a select whose reply removes an option', async () => {
        const c = await start(
            `<div id="v" az-view><select name="s" az-change='[0,"pick"]'>` +
                `<option value="a" selected az-bound="selected">A</option>` +
                `<option value="b" az-bound="selected">B</option>` +
                `<option value="c" az-bound="selected">C</option></select></div>`,
        );
        $('select').value = 'b';
        change($('select'));
        $('[value=c]').remove();
        c.msg(null, null, c.lastRef());
        expect($('select').value).toBe('a');
    });

    it('restores the default radio of a group whose reply adds a radio', async () => {
        const c = await start(
            `<div id="v" az-view>` +
                `<input type="radio" name="r" value="a" checked az-bound="checked" az-change='[0,"pick"]'>` +
                `<input type="radio" name="r" value="b" az-bound="checked" az-change='[0,"pick"]'>` +
                `</div>`,
        );
        $('[value=b]').click();
        $('#v').insertAdjacentHTML(
            'beforeend',
            `<input type="radio" name="r" value="c" az-bound="checked">`,
        );
        c.msg(null, null, c.lastRef());
        expect($('[value=a]').checked).toBe(true);
        expect($('[value=b]').checked).toBe(false);
    });

    it('keeps a choice made after the capture in a group whose reply adds a radio', async () => {
        const c = await start(
            `<div id="v" az-view>` +
                `<input type="radio" name="r" value="a" checked az-bound="checked" az-change='[0,"pick"]'>` +
                `<input type="radio" name="r" value="b" az-bound="checked">` +
                `<input type="radio" name="r" value="d" az-bound="checked">` +
                `</div>`,
        );
        $('[value=a]').dispatchEvent(new Event('change', { bubbles: true }));
        $('[value=d]').checked = true; // the user moved on, unproposed
        $('#v').insertAdjacentHTML(
            'beforeend',
            `<input type="radio" name="r" value="c" az-bound="checked">`,
        );
        c.msg(null, null, c.lastRef());
        expect($('[value=d]').checked).toBe(true);
    });

    // A keydown proposes the value before the key's input lands: a key that edits
    // never matches the live value, but one that inserts nothing (Enter) does.
    it('reconciles on a key that inserts nothing, not on one that edits', async () => {
        const c = await start(
            `<div id="v" az-view><input name="q" value="" az-bound="value" ` +
                `az-keydown='[16,["enter","a"],[[0,"key"]]]'></div>`,
        );
        const el = $('input');
        el.value = 'x';
        el.dispatchEvent(new KeyboardEvent('keydown', { key: 'Enter', bubbles: true }));
        c.msg(null, null, c.lastRef());
        expect(el.value).toBe('');
        el.value = 'y';
        el.dispatchEvent(new KeyboardEvent('keydown', { key: 'a', bubbles: true }));
        el.value = 'ya'; // the key's input
        c.msg(null, null, c.lastRef());
        expect(el.value).toBe('ya');
    });

    it('leaves a control showing bad input alone', async () => {
        const c = await start(INPUT.replace('value=""', 'type="number" value="5"'));
        // Mid-typing "1e-": the value reads "", so writing the default would
        // replace the text the user is still typing.
        Object.defineProperty($('input'), 'validity', { value: { badInput: true } });
        type($('input'), '');
        c.msg(null, null, c.lastRef());
        expect($('input').value).toBe('');
    });

    it('writes nothing for an echo', async () => {
        const c = await start(INPUT);
        type($('input'), 'ab');
        const setter = vi.spyOn(HTMLInputElement.prototype, 'value', 'set');
        c.msg([[SET_ATTR, 'v:0', 'value', 'ab']], null, c.lastRef());
        expect(setter).not.toHaveBeenCalled();
        expect($('input').value).toBe('ab');
    });

    // A single select none of whose options is selected shows its first enabled one:
    // that is its default, not an edit to undo.
    it('writes nothing to a select showing its first option for none selected', async () => {
        const c = await start(
            `<div id="v" az-view><form id="f" az-change='[0,"edit"]'>` +
                `<input name="q" value="">` +
                `<select name="s"><option value="a" az-bound="selected">A</option>` +
                `<option value="b" az-bound="selected">B</option></select></form></div>`,
        );
        change($('[name=q]'));
        const setter = vi.spyOn(HTMLOptionElement.prototype, 'selected', 'set');
        c.msg(null, null, c.lastRef());
        expect(setter).not.toHaveBeenCalled();
        setter.mockRestore();
        $('select').value = 'b';
        change($('select'));
        c.msg(null, null, c.lastRef());
        expect($('select').selectedIndex).toBe(0);
    });

    it('keeps the caret of a focused control it corrects', async () => {
        const c = await start(INPUT);
        const el = $('input');
        el.focus();
        type(el, 'hello');
        el.setSelectionRange(2, 2);
        c.msg([[SET_ATTR, 'v:0', 'value', 'HELLO']], null, c.lastRef());
        expect(el.value).toBe('HELLO');
        expect([el.selectionStart, el.selectionEnd]).toEqual([2, 2]);
    });

    it('waits for an IME composition to end', async () => {
        const c = await start(INPUT);
        const el = $('input');
        el.dispatchEvent(new Event('compositionstart', { bubbles: true }));
        type(el, 'か');
        c.msg(null, null, c.lastRef());
        expect(el.value).toBe('か');
        el.dispatchEvent(new Event('compositionend', { bubbles: true }));
        expect(el.value).toBe('');
    });
});

describe('a reconnect', () => {
    const view = (/** @type {string} */ change) =>
        `<div id="page" az-view><form id="f"${change}><input name="q" value="" az-bound="value"></form></div>`;

    it('keeps the server value of a bound field no replay will propose', async () => {
        const c = await start(view(''));
        $('input').value = 'typed';
        c.close();
        c.open(true);
        c.msg([[8, 'page', view('')]], null, null, true);
        expect($('input').value).toBe('');
    });

    it('keeps the server value of a bound field in a child view under an az-nodiff element', async () => {
        const v =
            `<div id="page" az-view><div az-nodiff><div id="cv" az-view>` +
            `<form id="f"><input name="q" value="" az-bound="value"></form></div></div></div>`;
        const c = await start(v);
        $('input').value = 'typed';
        c.close();
        c.open(true);
        c.msg([[8, 'page', v]], null, null, true);
        expect($('input').value).toBe('');
    });

    it('keeps the server state of a bound checkbox, radio and select no replay proposes', async () => {
        const v =
            `<div id="page" az-view><form id="f">` +
            `<input type="checkbox" name="c" az-bound="checked">` +
            `<input type="radio" name="r" value="a" checked az-bound="checked">` +
            `<input type="radio" name="r" value="b" az-bound="checked">` +
            `<select name="s"><option value="a" selected az-bound="selected">A</option>` +
            `<option value="b" az-bound="selected">B</option></select>` +
            `<select name="m" multiple><option value="a" az-bound="selected">A</option></select>` +
            `</form></div>`;
        const c = await start(v);
        $('[name=c]').checked = true;
        $('[value=b]').checked = true;
        $('[name=s]').value = 'b';
        $('[name=m] option').selected = true;
        c.close();
        c.open(true);
        c.msg([[8, 'page', v]], null, null, true);
        expect($('[name=c]').checked).toBe(false);
        expect($('[value=a]').checked).toBe(true);
        expect($('[name=s]').value).toBe('a');
        expect($('[name=m] option').selected).toBe(false);
    });

    /** A reconnect with `v` as the resynced view. */
    function resync(/** @type {any} */ c, /** @type {string} */ v, code = 1006) {
        c.close(code);
        c.open(true);
        c.msg([[8, 'page', v]], null, null, true);
    }

    // The field's own event proposes what was typed while disconnected, and the
    // reply reconciles it, as the form's az-change replay does for its fields.
    it('restores a bound field its own event proposes, in a form with no az-change', async () => {
        const v =
            `<div id="page" az-view><form id="f">` +
            `<input name="q" value="" az-bound="value" az-input='[0,"edit"]'></form></div>`;
        const c = await start(v);
        $('input').value = 'typed';
        touch($('input'));
        resync(c, v);
        expect($('input').value).toBe('typed');
        const replay = c.sent().at(-1);
        expect(replay.slice(0, 3)).toEqual(['page', 'edit', { value: 'typed' }]);
        c.msg(null, null, replay[3]);
        expect($('input').value).toBe('');
    });

    // Two fields of one form whose own replays throw are two failures, each warned.
    it('warns for each field whose own replay throws', async () => {
        const bad = `az-input='[[1,"#["],[0,"edit"]]'`;
        const v =
            `<div id="page" az-view><form id="f">` +
            `<input name="a" value="" az-bound="value" ${bad}>` +
            `<input name="b" value="" az-bound="value" ${bad}></form></div>`;
        const warn = vi.spyOn(console, 'warn').mockImplementation(() => {});
        const c = await start(v);
        $('[name=a]').value = 'x';
        touch($('[name=a]'));
        $('[name=b]').value = 'y';
        touch($('[name=b]'));
        resync(c, v);
        const replayWarns = warn.mock.calls.filter(([msg]) => String(msg).includes('replayed'));
        expect(replayWarns).toHaveLength(2);
        warn.mockRestore();
    });

    // Two id-less formless radio groups are two units, so two failures.
    it('warns for each id-less formless group whose own replay throws', async () => {
        const bad = `az-bound="checked" az-change='[[1,"#["],[0,"pick"]]'`;
        const g = (/** @type {string} */ n) =>
            `<input type="radio" name="${n}" value="a" checked ${bad}>` +
            `<input type="radio" name="${n}" value="b" ${bad}>`;
        const v = `<div id="page" az-view>${g('g1')}${g('g2')}</div>`;
        const warn = vi.spyOn(console, 'warn').mockImplementation(() => {});
        const c = await start(v);
        for (const n of ['g1', 'g2']) {
            $(`[name=${n}][value=b]`).click();
        }
        resync(c, v);
        const replayWarns = warn.mock.calls.filter(([msg]) => String(msg).includes('replayed'));
        expect(replayWarns).toHaveLength(2);
        warn.mockRestore();
    });

    it('restores a formless bound field with an id its own event proposes', async () => {
        const v =
            `<div id="page" az-view>` +
            `<input id="q" name="q" value="" az-bound="value" az-change='[0,"edit"]'>` +
            `<input id="cb" type="checkbox" name="c" az-bound="checked" az-change='[0,"tick"]'></div>`;
        const c = await start(v);
        $('#q').value = 'typed';
        touch($('#q'));
        $('#cb').checked = true;
        touch($('#cb'));
        resync(c, v);
        expect($('#q').value).toBe('typed');
        expect($('#cb').checked).toBe(true);
        const sent = c.sent().slice(-2);
        expect(sent.map((f) => f.slice(0, 2))).toEqual([
            ['page', 'edit'],
            ['page', 'tick'],
        ]);
    });

    // The restore writes the edit into a fresh element no `input` has landed on: a
    // second drop before the replay's reply must still save it as the user's.
    it('keeps a restored edit across a second drop before the replay is answered', async () => {
        const v =
            `<div id="page" az-view>` +
            `<form id="f" az-change='[0,"changed"]'><input name="q" value="" az-bound="value"></form>` +
            `<input id="solo" value="" az-bound="value" az-change='[0,"edit"]'></div>`;
        const c = await start(v);
        $('[name=q]').value = 'typed';
        touch($('[name=q]'));
        $('#solo').value = 'mine';
        touch($('#solo'));
        resync(c, v);
        resync(c, v);
        expect($('[name=q]').value).toBe('typed');
        expect($('#solo').value).toBe('mine');
    });

    // The `change` a blur fires after a reconcile showed the server's value is no
    // edit: a later dropped capture writes the next server value as the attribute
    // only, and a reconnect would replay the one still shown as the user's.
    it('replays no field whose blur came after the server value was shown', async () => {
        const v = (/** @type {string} */ x) =>
            `<div id="page" az-view><form id="f" az-submit='[0,"save"]'>` +
            `<input az="0" name="q" value="${x}" az-bound="value" az-input='[0,"edit"]'></form></div>`;
        const c = await start(v(''));
        type($('input'), 'a');
        c.msg([[SET_ATTR, 'page:0', 'value', 'a']], null, c.lastRef());
        change($('input'));
        $('form').dispatchEvent(new Event('submit', { bubbles: true, cancelable: true }));
        c.msg([[SET_ATTR, 'page:0', 'value', 'b']]);
        const before = c.sent().length;
        resync(c, v('b'));
        expect($('input').value).toBe('b');
        expect(
            c
                .sent()
                .slice(before)
                .filter((f) => f[1] === 'edit'),
        ).toEqual([]);
    });

    // A hook that sets a bound field when the resync mounts it is no user's edit,
    // so no replay proposes what it set.
    it('replays no bound field a hook set when the resync mounted it', async () => {
        const v =
            `<div id="page" az-view><form id="f">` +
            `<input name="q" value="" az-bound="value" az-hook="Fill" az-change='[0,"edit"]'>` +
            `</form></div>`;
        const c = await start(v, {
            before: (mod) => {
                mod.hooks.Fill = {
                    mounted() {
                        this.el.value = 'widget';
                    },
                };
            },
        });
        const before = c.sent().length;
        resync(c, v);
        expect(
            c
                .sent()
                .slice(before)
                .filter((f) => f[1] === 'edit'),
        ).toEqual([]);
    });

    // A formless field the restore cannot write (readonly at the resync) keeps
    // what the resync mounted, a hook's value included: no user's edit, so neither
    // replayed nor saved again by a second drop.
    it('replays no formless bound field the restore could not write', async () => {
        const v = (/** @type {string} */ ro) =>
            `<div id="page" az-view><input id="q" value="" az-bound="value" az-hook="Fill" az-change='[0,"edit"]'${ro}></div>`;
        const c = await start(v(''), {
            before: (mod) => {
                mod.hooks.Fill = {
                    mounted() {
                        if (this.el.readOnly) this.el.value = 'widget';
                    },
                };
            },
        });
        $('#q').value = 'typed';
        touch($('#q'));
        const before = c.sent().length;
        const edits = () =>
            c
                .sent()
                .slice(before)
                .filter((f) => f[1] === 'edit');
        resync(c, v(' readonly'));
        expect($('#q').value).toBe('widget');
        expect(edits()).toEqual([]);
        resync(c, v(''));
        expect($('#q').value).toBe('');
        expect(edits()).toEqual([]);
    });

    // A resync may insert a field of the same name before an edited one: a field
    // with an id keeps its edit by id, bound or not, not by position.
    it.each([
        ['a bound', ` az-bound="value"`],
        ['an unbound', ''],
    ])('keeps %s field edit by id when the resync inserts one before it', async (_l, b) => {
        const v = (/** @type {string[]} */ xs) =>
            `<div id="page" az-view><form id="f" az-change='[0,"changed"]'>` +
            xs.map((x) => `<input id="i-${x}" name="item" value="${x}"${b}>`).join('') +
            `</form></div>`;
        const c = await start(v(['a', 'b']));
        $('#i-b').value = 'B!';
        touch($('#i-b'));
        const before = c.sent().length;
        resync(c, v(['new', 'a', 'b']));
        const shown = [...document.querySelectorAll('input')].map((e) => e.value);
        expect(shown).toEqual(['new', 'a', 'B!']);
        expect(
            c
                .sent()
                .slice(before)
                .map((f) => f.slice(0, 3)),
        ).toEqual([['page', 'changed', { item: ['new', 'a', 'B!'] }]]);
    });

    // An id minted in mount/1 changes on the remount a reconnect runs, so it names
    // no field of the fresh form: the edit is restored by position.
    it('restores a field by position when the resync gives it another id', async () => {
        const v = (/** @type {string} */ id) =>
            `<div id="page" az-view><form id="f"><label for="${id}">T</label>` +
            `<input id="${id}" name="title" value=""><textarea id="${id}-b" name="body"></textarea>` +
            `</form></div>`;
        const c = await start(v('u1'));
        $('[name=title]').value = 'my title';
        $('[name=body]').value = 'my body';
        resync(c, v('u2'));
        expect([$('[name=title]').value, $('[name=body]').value]).toEqual(['my title', 'my body']);
    });

    // A field that gains an id is no longer among the id-less fields, so pairing
    // those by position would move each edit onto its neighbour: the name pairs
    // by position.
    it('restores fields by position when an id-less one gains an id', async () => {
        const v = (/** @type {string} */ id) =>
            `<div id="page" az-view><form id="f"><input name="n" id="a" value="">` +
            `<input name="n"${id} value=""><input name="n" value=""></form></div>`;
        const c = await start(v(''));
        const [a, x, y] = /** @type {any[]} */ ([...document.querySelectorAll('input')]);
        a.value = 'A';
        x.value = 'X';
        y.value = 'Y';
        resync(c, v(' id="nx"'));
        expect([...document.querySelectorAll('input')].map((e) => e.value)).toEqual([
            'A',
            'X',
            'Y',
        ]);
    });

    // Pairing a name's controls is linear in them, so a resync of a form holding
    // thousands of same-name fields (a grid, a long radio list) does not stall.
    // Counted, not timed: the work is every `some`/`every` callback and `id` read.
    it.each([
        ['fields with ids', (/** @type {number} */ i) => `<input name="q" id="q${i}" value="">`],
        ['radios', (/** @type {number} */ i) => `<input type="radio" name="r" value="${i}">`],
    ])('restores %s in work linear in their number', async (_l, field) => {
        const work = async (/** @type {number} */ n) => {
            const v = `<div id="page" az-view><form id="f">${Array.from({ length: n }, (_, i) => field(i)).join('')}</form></div>`;
            const c = await start(v);
            let calls = 0;
            const some = Array.prototype.some;
            const every = Array.prototype.every;
            const idGet = /** @type {any} */ (
                Object.getOwnPropertyDescriptor(Element.prototype, 'id')
            ).get;
            /** @param {Function} orig */
            const wrap = (orig) =>
                function (
                    /** @type {any} */ this_,
                    /** @type {Function} */ cb,
                    /** @type {any} */ t,
                ) {
                    return orig.call(this_, (/** @type {any[]} */ ...a) => {
                        calls++;
                        return cb.apply(t, a);
                    });
                };
            Array.prototype.some = function (cb, t) {
                return wrap(some)(this, cb, t);
            };
            Array.prototype.every = function (cb, t) {
                return wrap(every)(this, cb, t);
            };
            Object.defineProperty(Element.prototype, 'id', {
                configurable: true,
                get() {
                    calls++;
                    return idGet.call(this);
                },
            });
            try {
                resync(c, v);
            } finally {
                Array.prototype.some = some;
                Array.prototype.every = every;
                Object.defineProperty(Element.prototype, 'id', {
                    configurable: true,
                    get: idGet,
                });
            }
            c.restore();
            conn = /** @type {any} */ (null);
            return calls;
        };
        const small = await work(200);
        const big = await work(800);
        expect(big / small).toBeLessThan(6);
    });

    // An id two fields share names neither: both are restored by position.
    it('restores fields sharing an id by position', async () => {
        const v =
            `<div id="page" az-view><form id="f">` +
            `<input id="qty" name="qty[]" value=""><input id="qty" name="qty[]" value="">` +
            `</form></div>`;
        const c = await start(v);
        const [a, b] = /** @type {any[]} */ ([...document.querySelectorAll('input')]);
        a.value = '1';
        b.value = '2';
        resync(c, v);
        expect([...document.querySelectorAll('input')].map((e) => e.value)).toEqual(['1', '2']);
    });

    // A disabled control submits nothing, so nothing was saved for it: it keeps
    // the resync's state rather than reading as unchecked.
    it('keeps the resync state of a checkbox and radio disabled at the drop', async () => {
        const v =
            `<div id="page" az-view><form id="f">` +
            `<input type="checkbox" name="agree" value="1" checked disabled>` +
            `<input type="radio" name="r" value="a" checked disabled>` +
            `<input type="radio" name="r" value="b" disabled>` +
            `</form></div>`;
        const c = await start(v);
        resync(c, v);
        const checked = [...document.querySelectorAll('input')].map((e) => e.checked);
        expect(checked).toEqual([true, true, false]);
    });

    // A checkbox or radio is restored by value, so whether it was disabled at the
    // drop is decided by value too: a resync that inserts one before it must not
    // shift that onto its neighbour.
    it('keeps a checkbox edit and a box disabled at the drop when the resync inserts one before them', async () => {
        const box = (/** @type {string} */ x, /** @type {string} */ a = '') =>
            `<input type="checkbox" name="cb" value="${x}"${a}>`;
        const v = (/** @type {string} */ inner) =>
            `<div id="page" az-view><form id="f">${inner}</form></div>`;
        const c = await start(v(box('1') + box('2', ' checked disabled') + box('3')));
        $('[value="1"]').checked = true;
        resync(c, v(box('0') + box('1') + box('2', ' checked disabled') + box('3')));
        const checked = [...document.querySelectorAll('input')].map((e) => e.checked);
        expect(checked).toEqual([false, true, true, false]);
    });

    it('keeps a radio disabled at the drop when the resync inserts one before it', async () => {
        const v = (/** @type {string} */ inner) =>
            `<div id="page" az-view><form id="f">${inner}` +
            `<input type="radio" name="r" value="a">` +
            `<input type="radio" name="r" value="b" checked disabled></form></div>`;
        const c = await start(v(''));
        resync(c, v(`<input type="radio" name="r" value="z">`));
        const checked = [...document.querySelectorAll('input')].map((e) => e.checked);
        expect(checked).toEqual([false, false, true]);
    });

    // The user edited it while it was enabled: when the server enables it again,
    // only `disabled` diffs, so the user's state would be lost for good.
    it('restores the state of an unbound checkbox and radio the resync disabled', async () => {
        const v = (/** @type {string} */ d) =>
            `<div id="page" az-view><form id="f">` +
            `<input type="checkbox" name="cb" value="1"${d}>` +
            `<input type="radio" name="r" value="a" checked${d}>` +
            `<input type="radio" name="r" value="b"${d}></form></div>`;
        const c = await start(v(''));
        $('[name=cb]').checked = true;
        $('[value=b]').checked = true;
        resync(c, v(' disabled'));
        const checked = [...document.querySelectorAll('input')].map((e) => e.checked);
        expect(checked).toEqual([true, false, true]);
    });

    // Boxes of one name sharing a value (value-less ones all submit "on") pair by
    // their order among those, so the one the user checked is the one restored.
    it('restores checkboxes of one name sharing a value by their order among those', async () => {
        const v = (/** @type {string} */ a) =>
            `<div id="page" az-view><form id="f" az-change='[0,"chg"]'>` +
            `<input type="checkbox" name="t"${a}><input type="checkbox" name="t"${a}>` +
            `<input type="checkbox" name="u" value="1" az-bound="checked">` +
            `<input type="checkbox" name="u" value="1" az-bound="checked"></form></div>`;
        const c = await start(v(''));
        for (const name of ['t', 'u']) {
            $(`[name=${name}]`).click();
        }
        const before = c.sent().length;
        resync(c, v(''));
        const checked = [...document.querySelectorAll('input')].map((e) => e.checked);
        expect(checked).toEqual([true, false, true, false]);
        expect(
            c
                .sent()
                .slice(before)
                .map((/** @type {any[]} */ f) => f.slice(0, 3)),
        ).toEqual([['page', 'chg', { t: ['on'], u: ['1'] }]]);
    });

    // A checkbox and a radio of one name sharing a value are two controls.
    it('restores a checkbox and a radio of one name sharing a value apart', async () => {
        const v =
            `<div id="page" az-view><form id="f">` +
            `<input type="checkbox" name="k" value="x"><input type="radio" name="k" value="x">` +
            `</form></div>`;
        const c = await start(v);
        $('[type=checkbox]').checked = true;
        resync(c, v);
        const checked = [...document.querySelectorAll('input')].map((e) => e.checked);
        expect(checked).toEqual([true, false]);
    });

    // A checkbox takes its state by value, so whether the user edited it is read
    // by value too: a box inserted before the one the user checked must not lend
    // the user's edit to its neighbour.
    it('restores a bound checkbox the user edited by value when the resync inserts one before it', async () => {
        const box = (/** @type {string} */ x, /** @type {string} */ a = '') =>
            `<input type="checkbox" name="cb" value="${x}" az-bound="checked"${a}>`;
        const v = (/** @type {string} */ inner) =>
            `<div id="page" az-view><form id="f" az-change='[0,"changed"]'>${inner}</form></div>`;
        const c = await start(v(box('1') + box('2')));
        $('[value="2"]').checked = true;
        touch($('[value="2"]'));
        resync(c, v(box('0') + box('1', ' checked') + box('2')));
        const checked = [...document.querySelectorAll('input')].map((e) => e.checked);
        expect(checked).toEqual([false, true, true]);
    });

    // A server-owned field the resync disabled shows the server's state and is not
    // replayed: it submits nothing, and the server disabled it. An unbound one keeps
    // what the user typed while it was enabled, which the server cannot write back.
    it('keeps the resync value of a bound field the resync disabled, and restores an unbound one', async () => {
        const v = (/** @type {string} */ d) =>
            `<div id="page" az-view><form id="f" az-change='[0,"chg"]'><input name="t" value=""${d}>` +
            `<select name="s"${d}><option value="a">a</option><option value="b">b</option></select>` +
            `<input name="o" value="" az-bound="value"${d}></form>` +
            `<input id="q" value="" az-bound="value" az-input='[0,"edit"]'${d}></div>`;
        const c = await start(v(''));
        $('[name=t]').value = 'typed';
        $('[name=s]').value = 'b';
        type($('[name=o]'), 'owned');
        type($('#q'), 'mine');
        const before = c.sent().length;
        resync(c, v(' disabled'));
        expect([
            $('[name=t]').value,
            $('[name=s]').value,
            $('[name=o]').value,
            $('#q').value,
        ]).toEqual(['typed', 'b', '', '']);
        expect(
            c
                .sent()
                .slice(before)
                .map((/** @type {any[]} */ f) => f.slice(0, 3)),
        ).toEqual([['page', 'chg', {}]]);
    });

    // A saved choice the resync no longer offers would leave the select showing
    // none: it keeps the resync's selection.
    it('keeps the resync selection of a select that no longer offers the saved value', async () => {
        const v = (/** @type {string[]} */ os) =>
            `<div id="page" az-view><form id="f"><select name="s">` +
            os.map((x) => `<option value="${x}">${x}</option>`).join('') +
            `</select></form></div>`;
        const c = await start(v(['a', 'b']));
        $('select').value = 'b';
        resync(c, v(['a', 'c']));
        expect([$('select').selectedIndex, $('select').value]).toEqual([0, 'a']);
    });

    // Nothing was saved for a checkbox or radio whose name or value the form did
    // not have at the drop: the user never saw it, so it keeps the resync's state.
    it('keeps the resync state of a checkbox and radio the saved form did not have', async () => {
        const v = (/** @type {string} */ extra) =>
            `<div id="page" az-view><form id="f"><input name="t" value="">` +
            `<input type="checkbox" name="cb" value="1">${extra}</form></div>`;
        const c = await start(v(''));
        $('[name=t]').value = 'T';
        resync(
            c,
            v(
                `<input type="checkbox" name="cb" value="2" checked>` +
                    `<input type="checkbox" name="nb" value="1" checked>` +
                    `<input type="radio" name="nr" value="a" checked>`,
            ),
        );
        const checked = [...document.querySelectorAll('[type=checkbox],[type=radio]')].map(
            (e) => /** @type {any} */ (e).checked,
        );
        expect([$('[name=t]').value, ...checked]).toEqual(['T', false, true, true, true]);
    });

    // Restoring only the saved choices still offered would select, and replay, a set
    // the user never chose: a server-owned multiple select keeps the resync's
    // selection unless every saved choice is still offered.
    it.each([
        ['none', ['x']],
        ['only some', ['x', 'y']],
    ])(
        'keeps the resync selection of a bound multiple select offering %s of the saved choices',
        async (_l, picks) => {
            const o = (/** @type {string} */ x, /** @type {string} */ a = '') =>
                `<option value="${x}" az-bound="selected"${a}>${x}</option>`;
            const v = (/** @type {string} */ os) =>
                `<div id="page" az-view><form id="f" az-change='[0,"chg"]'>` +
                `<select name="m" multiple>${os}</select></form></div>`;
            const c = await start(v(o('x') + o('y') + o('w')));
            for (const p of picks) $(`option[value=${p}]`).selected = true;
            change($('select'));
            resync(c, v(o('y') + o('z', ' selected')));
            const sel = [...$('select').selectedOptions].map((/** @type {any} */ e) => e.value);
            expect(sel).toEqual(['z']);
            expect(c.sent().at(-1).slice(0, 3)).toEqual(['page', 'chg', { m: ['z'] }]);
        },
    );

    // An unbound multiple select keeps the choices still offered, as it does live when
    // a diff removes one of its options; a server-owned one is restored whole or not
    // at all, since a replay would propose a set the user never chose.
    it('keeps the still-offered choices of an unbound multiple select, and none of a bound one', async () => {
        const opts = (/** @type {string[]} */ os, /** @type {string} */ a) =>
            os
                .map((x) => `<option value="${x}"${a}${x === 'c' ? ' selected' : ''}>${x}</option>`)
                .join('');
        const v = (/** @type {string[]} */ os) =>
            `<div id="page" az-view><form id="f" az-change='[0,"chg"]'>` +
            `<select name="u" multiple>${opts(os, '')}</select>` +
            `<select name="m" multiple>${opts(os, ' az-bound="selected"')}</select></form></div>`;
        const c = await start(v(['a', 'b', 'c']));
        for (const name of ['u', 'm']) {
            const s = $(`[name=${name}]`);
            for (const o of s.options) o.selected = o.value !== 'c';
            change(s);
        }
        const before = c.sent().length;
        resync(c, v(['a', 'c']));
        const picked = (/** @type {string} */ name) =>
            [...$(`[name=${name}]`).selectedOptions].map((o) => o.value);
        expect([picked('u'), picked('m')]).toEqual([['a'], ['c']]);
        expect(
            c
                .sent()
                .slice(before)
                .map((/** @type {any[]} */ f) => f.slice(0, 3)),
        ).toEqual([['page', 'chg', { u: ['a'], m: ['c'] }]]);
    });

    it('restores a multiple select whose saved choices the resync reorders', async () => {
        const o = (/** @type {string} */ x, /** @type {string} */ a = '') =>
            `<option value="${x}"${a}>${x}</option>`;
        const v = (/** @type {string} */ os) =>
            `<div id="page" az-view><form id="f"><select name="m" multiple>${os}</select></form></div>`;
        const c = await start(v(o('x') + o('y') + o('w')));
        $('option[value=x]').selected = true;
        $('option[value=w]').selected = true;
        resync(c, v(o('w') + o('z', ' selected') + o('x')));
        const sel = [...$('select').selectedOptions].map((/** @type {any} */ e) => e.value);
        expect(sel).toEqual(['w', 'x']);
    });

    // Two multiple selects sharing a name each take their own saved choices.
    it('restores two multiple selects sharing a name apart', async () => {
        const o = (/** @type {string} */ x) => `<option value="${x}">${x}</option>`;
        const s = `<select name="m" multiple>${o('x') + o('y')}</select>`;
        const v = `<div id="page" az-view><form id="f">${s}${s}</form></div>`;
        const c = await start(v);
        const [a, b] = /** @type {any[]} */ ([...document.querySelectorAll('select')]);
        a.options[0].selected = true;
        b.options[1].selected = true;
        resync(c, v);
        const sel = [...document.querySelectorAll('select')].map((e) =>
            [.../** @type {any} */ (e).selectedOptions].map((/** @type {any} */ x) => x.value),
        );
        expect(sel).toEqual([['x'], ['y']]);
    });

    it('keeps the resync selection of a formless multiple select offering none of the saved choices', async () => {
        const v = (/** @type {string} */ os) =>
            `<div id="page" az-view><select id="ms" multiple az-change='[0,"pick"]'>${os}</select></div>`;
        const o = (/** @type {string} */ x, /** @type {string} */ a = '') =>
            `<option az-bound="selected" value="${x}"${a}>${x}</option>`;
        const c = await start(v(o('x') + o('y')));
        $('option[value=x]').selected = true;
        change($('#ms'));
        const before = c.sent().length;
        resync(c, v(o('y', ' selected') + o('z')));
        const sel = [...$('#ms').selectedOptions].map((/** @type {any} */ e) => e.value);
        expect(sel).toEqual(['y']);
        expect(
            c
                .sent()
                .slice(before)
                .filter((f) => f[1] === 'pick'),
        ).toEqual([]);
    });

    // A formless field is restored by id; what it holds is restored by value, since
    // the resync may reorder the options or give the id another value.
    it('restores a formless select by option value when the resync reorders them', async () => {
        const opts = (/** @type {string[]} */ vs, /** @type {string} */ def) =>
            vs
                .map(
                    (x) =>
                        `<option value="${x}"${x === def ? ' selected' : ''} az-bound="selected">${x}</option>`,
                )
                .join('');
        const v = (/** @type {string} */ o) =>
            `<div id="page" az-view><select id="s" name="s" az-change='[0,"pick"]'>${o}</select></div>`;
        const c = await start(v(opts(['a', 'b', 'c'], 'a')));
        $('#s').value = 'b';
        touch($('#s'));
        resync(c, v(opts(['c', 'a', 'b'], 'c')));
        expect($('#s').value).toBe('b');
        expect(c.sent().at(-1).slice(0, 3)).toEqual(['page', 'pick', { value: 'b' }]);
    });

    it('restores a formless multiple select by option values', async () => {
        const v = (/** @type {string} */ o) =>
            `<div id="page" az-view><select id="m" name="m" multiple az-change='[0,"pick"]'>${o}</select></div>`;
        const o = (/** @type {string} */ x) =>
            `<option value="${x}" az-bound="selected">${x}</option>`;
        const c = await start(v(o('a') + o('b') + o('c')));
        $('#m').options[1].selected = true;
        touch($('#m'));
        resync(c, v(o('c') + o('a') + o('b')));
        expect([...$('#m').selectedOptions].map((x) => x.value)).toEqual(['b']);
    });

    it('keeps the server state of a formless checkbox whose value the resync changed', async () => {
        const v = (/** @type {string} */ x) =>
            `<div id="page" az-view><input id="cb" type="checkbox" name="c" value="${x}" az-bound="checked" az-change='[0,"tick"]'></div>`;
        const c = await start(v('a'));
        const before = c.sent().length;
        $('#cb').checked = true;
        resync(c, v('b'));
        expect($('#cb').checked).toBe(false);
        expect(
            c
                .sent()
                .slice(before)
                .filter((f) => f[1] === 'tick'),
        ).toEqual([]);
    });

    // Only a push_event proposes: a field whose own event only toggles, or pushes
    // under on_key (which a replay presses no key for), is neither replayed nor
    // restored.
    it('replays no field whose own event pushes nothing a replay can send', async () => {
        const v =
            `<div id="page" az-view>` +
            `<input id="t" name="t" value="" az-bound="value" az-change='[1,"#x"]'>` +
            `<input id="k" name="k" value="" az-bound="value" az-input='[16,["enter"],[0,"edit"]]'>` +
            `<p id="x"></p></div>`;
        const c = await start(v);
        const before = c.sent().length;
        $('#t').value = 'typed t';
        $('#k').value = 'typed k';
        resync(c, v);
        expect($('#x').hidden).toBe(false);
        expect($('#t').value).toBe('');
        expect($('#k').value).toBe('');
        expect(c.sent().slice(before)).toEqual([]);
    });

    it('replays a field whose push_event a view transition wraps', async () => {
        const v =
            `<div id="page" az-view>` +
            `<input id="t" name="t" value="" az-bound="value" az-change='[20,{},[0,"edit"]]'></div>`;
        const c = await start(v);
        $('#t').value = 'typed';
        touch($('#t'));
        resync(c, v);
        expect($('#t').value).toBe('typed');
        expect(c.sent().at(-1).slice(0, 3)).toEqual(['page', 'edit', { value: 'typed' }]);
    });

    it('replays a field once when its form proposes it too', async () => {
        const v =
            `<div id="page" az-view><form id="f" az-change='[0,"changed"]'>` +
            `<input name="q" value="" az-bound="value" az-input='[0,"edit"]'></form></div>`;
        const c = await start(v);
        const before = c.sent().length;
        $('input').value = 'typed';
        resync(c, v);
        expect(
            c
                .sent()
                .slice(before)
                .map((f) => f[1]),
        ).toEqual(['changed']);
    });

    it('replays no field the user left as the server has it', async () => {
        const v =
            `<div id="page" az-view><form id="f">` +
            `<input name="q" value="" az-bound="value" az-input='[0,"edit"]'></form></div>`;
        const c = await start(v);
        const before = c.sent().length;
        resync(c, v);
        expect(
            c
                .sent()
                .slice(before)
                .filter((f) => f[1] === 'edit'),
        ).toEqual([]);
    });

    // A single select none of whose options is selected shows its first enabled
    // one, which the user did not choose.
    it('replays no select the user left showing its first option for none selected', async () => {
        const o = (/** @type {string} */ x, /** @type {string} */ attrs = '') =>
            `<option value="${x}"${attrs} az-bound="selected">${x}</option>`;
        const v =
            `<div id="page" az-view>` +
            `<select id="s" az-change='[0,"pick"]'>${o('a')}${o('b')}</select>` +
            `<form id="f"><select name="n" az-change='[0,"pick"]'>` +
            `<optgroup disabled>${o('x')}</optgroup>${o('y', ' disabled')}${o('z')}</select></form>` +
            `</div>`;
        const c = await start(v);
        const before = c.sent().length;
        resync(c, v);
        expect($('#s').value).toBe('a');
        expect($('[name=n]').value).toBe('z');
        expect(
            c
                .sent()
                .slice(before)
                .filter((f) => f[1] === 'pick'),
        ).toEqual([]);
    });

    // A diff inserting an option before the one a select shows for none selected
    // moves its default, and the browser keeps showing the old one: not an edit.
    it('replays no select whose default a diff moved under it', async () => {
        const o = (/** @type {string} */ x) =>
            `<option value="${x}" az-bound="selected">${x}</option>`;
        const v = (/** @type {string} */ os) =>
            `<div id="page" az-view>` +
            `<select id="s" az-change='[0,"pick"]'>${os}</select>` +
            `<form id="f"><select name="n" az-change='[0,"pick"]'>${os}</select></form></div>`;
        const c = await start(v(o('b') + o('c')));
        for (const s of document.querySelectorAll('select'))
            s.insertAdjacentHTML('afterbegin', o('a'));
        expect($('#s').value).toBe('b');
        expect($('[name=n]').value).toBe('b');
        const before = c.sent().length;
        resync(c, v(o('a') + o('b') + o('c')));
        expect($('#s').value).toBe('a');
        expect($('[name=n]').value).toBe('a');
        expect(
            c
                .sent()
                .slice(before)
                .filter((f) => f[1] === 'pick'),
        ).toEqual([]);
    });

    // A refused pick shows the server's choice again: the user's change is answered,
    // so a default the diff later moves is no edit either.
    it('replays no select a refused pick left before a diff moved its default', async () => {
        const o = (/** @type {string} */ x) =>
            `<option value="${x}" az-bound="selected">${x}</option>`;
        const v = (/** @type {string} */ os) =>
            `<div id="page" az-view><select id="s" az-change='[0,"pick"]'>${os}</select></div>`;
        const c = await start(v(o('b') + o('c')));
        $('#s').value = 'c';
        change($('#s'));
        c.msg(null, null, c.lastRef());
        expect($('#s').value).toBe('b');
        $('#s').insertAdjacentHTML('afterbegin', o('a'));
        const before = c.sent().length;
        resync(c, v(o('a') + o('b') + o('c')));
        expect($('#s').value).toBe('a');
        expect(
            c
                .sent()
                .slice(before)
                .filter((f) => f[1] === 'pick'),
        ).toEqual([]);
    });

    // What a field the user left alone saved is an older server value: the resync's
    // is newer, and replaying the saved one would revert it.
    it('keeps the resync value of each bound field the user left alone', async () => {
        const v = (/** @type {string} */ x, /** @type {boolean} */ on) => {
            const ck = (/** @type {boolean} */ b) => (b ? ' checked' : '');
            return (
                `<div id="page" az-view>` +
                `<input id="q" name="q" value="${x}" az-bound="value" az-change='[0,"edit"]'>` +
                `<input id="cb" type="checkbox" name="cb" az-bound="checked"${ck(on)} az-change='[0,"tick"]'>` +
                `<form id="f"><input name="t" value="${x}" az-bound="value" az-input='[0,"edit"]'>` +
                `<input type="checkbox" name="c" az-bound="checked"${ck(on)} az-change='[0,"tick"]'>` +
                `<input type="radio" name="r" value="a" az-bound="checked"${ck(on)} az-change='[0,"rad"]'>` +
                `<input type="radio" name="r" value="b" az-bound="checked"${ck(!on)} az-change='[0,"rad"]'>` +
                `</form>` +
                `<form id="g" az-change='[0,"changed"]'><input name="u" value="${x}" az-bound="value"></form>` +
                `</div>`
            );
        };
        const c = await start(v('x', true));
        const before = c.sent().length;
        resync(c, v('y', false));
        expect($('#q').value).toBe('y');
        expect($('#cb').checked).toBe(false);
        expect($('[name=t]').value).toBe('y');
        expect($('[name=c]').checked).toBe(false);
        expect($('[value=b]').checked).toBe(true);
        expect($('[name=u]').value).toBe('y');
        expect(
            c
                .sent()
                .slice(before)
                .map((f) => f.slice(0, 3)),
        ).toEqual([['page', 'changed', { u: 'y' }]]);
    });

    // Of several selected options a single select shows the last; a multiple select,
    // or a single one showing more than one row, may show none selected.
    it('replays no select the user left as the browser shows its defaults', async () => {
        const o = (/** @type {string} */ x, /** @type {string} */ attrs = '') =>
            `<option value="${x}"${attrs} az-bound="selected">${x}</option>`;
        const v =
            `<div id="page" az-view>` +
            `<select id="s" az-change='[0,"pick"]'>${o('a', ' selected')}${o('b', ' selected')}</select>` +
            `<select id="m" multiple az-change='[0,"pick"]'>${o('c')}${o('d')}</select>` +
            `<select id="r" size="2" az-change='[0,"pick"]'>${o('e')}${o('f')}</select>` +
            `</div>`;
        const c = await start(v);
        const before = c.sent().length;
        resync(c, v);
        expect($('#s').value).toBe('b');
        expect($('#m').selectedIndex).toBe(-1);
        expect($('#r').selectedIndex).toBe(-1);
        expect(
            c
                .sent()
                .slice(before)
                .filter((f) => f[1] === 'pick'),
        ).toEqual([]);
    });

    it('replays a select the user moved off its first option for none selected', async () => {
        const v =
            `<div id="page" az-view><select id="s" az-change='[0,"pick"]'>` +
            `<option value="a" az-bound="selected">A</option>` +
            `<option value="b" az-bound="selected">B</option></select></div>`;
        const c = await start(v);
        $('#s').value = 'b';
        touch($('#s'));
        resync(c, v);
        expect($('#s').value).toBe('b');
        expect(c.sent().at(-1).slice(0, 3)).toEqual(['page', 'pick', { value: 'b' }]);
    });

    // A form az-change that proposes nothing (a toggle, a fetch) leaves its bound
    // fields unproposed: they show the server's value, unless their own event proposes.
    it('keeps the server value of a bound field a form replay does not propose', async () => {
        const v =
            `<div id="page" az-view><form id="f" az-change='[1,"#x"]'>` +
            `<input id="a" name="a" value="" az-bound="value">` +
            `<input id="b" name="b" value="" az-bound="value" az-input='[0,"edit"]'></form>` +
            `<p id="x"></p></div>`;
        const c = await start(v);
        $('#a').value = 'typed a';
        touch($('#a'));
        $('#b').value = 'typed b';
        touch($('#b'));
        resync(c, v);
        expect($('#a').value).toBe('');
        expect($('#b').value).toBe('typed b');
        expect(c.sent().at(-1).slice(0, 3)).toEqual(['page', 'edit', { value: 'typed b' }]);
    });

    it('keeps the server value when the last replay may have crashed the view', async () => {
        const v =
            `<div id="page" az-view><form id="f">` +
            `<input name="q" value="" az-bound="value" az-input='[0,"edit"]'></form>` +
            `<input id="q2" name="q2" value="" az-bound="value" az-input='[0,"edit2"]'></div>`;
        const c = await start(v);
        $('input').value = 'typed';
        touch($('input'));
        $('#q2').value = 'typed 2';
        touch($('#q2'));
        resync(c, v);
        expect($('input').value).toBe('typed');
        expect($('#q2').value).toBe('typed 2');
        const before = c.sent().length;
        resync(c, v, 4500); // before the server acknowledged the replay
        expect($('input').value).toBe('');
        expect($('#q2').value).toBe('');
        expect(
            c
                .sent()
                .slice(before)
                .filter((f) => f[1] === 'edit' || f[1] === 'edit2'),
        ).toEqual([]);
    });

    // The resync saves again what was typed while disconnected, also when the drop
    // found nothing to save.
    it('restores a formless field edited only while disconnected on a page the drop saved nothing of', async () => {
        const v = `<div id="page" az-view><input id="q" value="" az-bound="value" az-input='[0,"edit"]'></div>`;
        const c = await start(v);
        c.close();
        c.open(true);
        type($('#q'), 'typed');
        const before = c.sent().length;
        c.msg([[8, 'page', v]], null, null, true);
        expect($('#q').value).toBe('typed');
        expect(
            c
                .sent()
                .slice(before)
                .map((f) => f.slice(0, 3)),
        ).toEqual([['page', 'edit', { value: 'typed' }]]);
    });

    // A field the crash may have come from stays withheld even when it held the
    // server's value at the crash, so nothing of it was saved then, and the user
    // edits it again before the resync.
    it('withholds the replay of a formless field edited only after the crash', async () => {
        const v =
            `<div id="page" az-view><form id="f"></form>` +
            `<input id="q" value="" az-bound="value" az-input='[0,"edit"]'></div>`;
        const c = await start(v);
        type($('#q'), 'typed');
        resync(c, v);
        expect($('#q').value).toBe('typed');
        type($('#q'), '');
        c.close(4500); // before the server acknowledged the replay
        c.open(true);
        type($('#q'), 'again');
        const before = c.sent().length;
        c.msg([[8, 'page', v]], null, null, true);
        expect($('#q').value).toBe('');
        expect(
            c
                .sent()
                .slice(before)
                .filter((f) => f[1] === 'edit'),
        ).toEqual([]);
    });

    it('restores a bound field its replay proposes, and reconciles on the reply', async () => {
        const v = view(` az-change='[0,"changed"]'`);
        const c = await start(v);
        $('input').value = 'typed';
        touch($('input'));
        c.close();
        c.open(true);
        c.msg([[8, 'page', v]], null, null, true);
        expect($('input').value).toBe('typed');
        const replay = c.sent().at(-1);
        expect(replay.slice(0, 3)).toEqual(['page', 'changed', { q: 'typed' }]);
        c.msg(null, null, replay[3]);
        expect($('input').value).toBe('');
    });

    // A saved value is an answer to the control it was saved from: a select's
    // choice is no text, and a checkbox's value and checkedness are none either.
    describe('a control the resync renders as another kind', () => {
        const sel = (/** @type {string} */ a = '') =>
            `<select${a}><option value="o1">o1</option><option value="o2">o2</option></select>`;

        it('keeps the resync value of a text field paired with a saved select', async () => {
            const v = (/** @type {string} */ inner) =>
                `<div id="page" az-view><form id="f">${inner}</form></div>`;
            const c = await start(v(sel(' name="q"')));
            $('select').value = 'o2';
            resync(c, v(`<input name="q" value="">`));
            expect($('input').value).toBe('');
        });

        // A password is shown by no other field, and an input that sanitizes its value
        // would show something else: text in a range shows its midpoint.
        it.each([
            ['text', 'password', 'hunter2', ''],
            ['password', 'text', 'hunter2', ''],
            ['range', 'text', 'hello', '10'],
            ['number', 'text', '1x', ''],
            ['range', 'number', '7', '10'],
            ['text', 'date', '2024-01-01', ''],
        ])(
            'keeps the resync value of a %s input paired with a saved %s',
            async (to, from, typed, server) => {
                const v = (/** @type {string} */ t) =>
                    `<div id="page" az-view><form id="f">` +
                    `<input type="${t}" name="q" value="${t === to ? server : ''}"></form></div>`;
                const c = await start(v(from));
                $('input').value = typed;
                change($('input'));
                resync(c, v(to));
                expect($('input').value).toBe(server);
            },
        );

        // A text input drops a textarea's line breaks, so it would show text the
        // user never typed.
        it('keeps the resync value of a text input paired with a textarea holding a line break', async () => {
            const v = (/** @type {string} */ inner) =>
                `<div id="page" az-view><form id="f">${inner}</form></div>`;
            const c = await start(v(`<textarea name="q"></textarea>`));
            $('textarea').value = 'one\ntwo';
            resync(c, v(`<input name="q" value="">`));
            expect($('input').value).toBe('');
        });

        it('keeps the resync value of a formless text field whose id a saved select had', async () => {
            const own = ` az-bound="value" az-change='[0,"edit"]'`;
            const opts = (/** @type {string} */ x) =>
                `<option value="o1" az-bound="selected">o1</option>` +
                `<option value="${x}" az-bound="selected">${x}</option>`;
            const c = await start(
                `<div id="page" az-view><select id="q" az-change='[0,"edit"]'>${opts('o2')}</select>` +
                    `<input id="b" type="checkbox" value="v" az-bound="checked" az-change='[0,"tick"]'></div>`,
            );
            $('#q').value = 'o2';
            change($('#q'));
            $('#b').click();
            const before = c.sent().length;
            resync(
                c,
                `<div id="page" az-view><input id="q" value=""${own}>` +
                    `<input id="b" value=""${own}></div>`,
            );
            expect([$('#q').value, $('#b').value]).toEqual(['', '']);
            expect(c.sent().slice(before)).toEqual([]);
        });
    });

    // A radio group is restored as a unit: the user's choice is its checked radio,
    // so when that one cannot be checked again, unchecking the rest would show (and
    // replay) a choice nobody made, an empty group.
    describe('a radio group whose saved choice cannot be restored', () => {
        const radio = (/** @type {string} */ x, /** @type {string} */ a = '') =>
            `<input type="radio" name="r" value="${x}"${a}>`;
        const form = (/** @type {string} */ inner, /** @type {string} */ ch = '') =>
            `<div id="page" az-view><form id="f"${ch}>${inner}</form></div>`;
        const checked = () => [...document.querySelectorAll('input')].map((e) => e.checked);
        const b = ' az-bound="checked"';
        const ch = ` az-change='[0,"chg"]'`;

        it('keeps the resync state of a bound group whose chosen radio the resync disabled', async () => {
            const c = await start(
                form(radio('a', ` checked${b}`) + radio('b', b) + radio('c', b), ch),
            );
            $('[value=c]').checked = true;
            touch($('[value=c]'));
            const before = c.sent().length;
            resync(
                c,
                form(radio('a', ` checked${b}`) + radio('b', b) + radio('c', ` disabled${b}`), ch),
            );
            expect(checked()).toEqual([true, false, false]);
            const sent = c
                .sent()
                .slice(before)
                .map((/** @type {any[]} */ f) => f.slice(0, 3));
            expect(sent).toEqual([['page', 'chg', { r: 'a' }]]);
        });

        it('keeps the resync state of a bound group the resync removed the chosen radio of', async () => {
            const c = await start(
                form(radio('a', ` checked${b}`) + radio('b', b) + radio('c', b), ch),
            );
            $('[value=c]').checked = true;
            touch($('[value=c]'));
            const before = c.sent().length;
            resync(c, form(radio('a', ` checked${b}`) + radio('b', b), ch));
            expect(checked()).toEqual([true, false]);
            const sent = c
                .sent()
                .slice(before)
                .map((/** @type {any[]} */ f) => f.slice(0, 3));
            expect(sent).toEqual([['page', 'chg', { r: 'a' }]]);
        });

        it('keeps the resync state of an unbound group the resync removed the chosen radio of', async () => {
            const c = await start(form(radio('a', ' checked') + radio('b') + radio('c')));
            $('[value=c]').checked = true;
            resync(c, form(radio('a', ' checked') + radio('b')));
            expect(checked()).toEqual([true, false]);
        });

        // A user cannot uncheck a radio, only check another: a group whose checked
        // radio at the drop was disabled, or that had none checked, holds no choice
        // of the user's, and unchecking the radio the resync checks would show one.
        it.each([
            [
                'its checked radio was disabled at the drop',
                radio('a') + radio('b', ' checked disabled'),
            ],
            ['none of its radios was checked at the drop', radio('a') + radio('b')],
        ])('keeps the resync state of a group when %s', async (_l, inner) => {
            const c = await start(form(inner));
            resync(c, form(radio('a', ' checked') + radio('b', ' disabled')));
            expect(checked()).toEqual([true, false]);
        });

        it('keeps the resync state of a formless bound group whose chosen radio the resync disabled', async () => {
            const r = (/** @type {string} */ x, /** @type {string} */ a = '') =>
                `<input type="radio" id="r${x}" name="r" value="${x}"${b}` +
                ` az-change='[0,"pick"]'${a}>`;
            const v = (/** @type {string} */ d) =>
                `<div id="page" az-view>${r('a', ' checked')}${r('b')}${r('c', d)}</div>`;
            const c = await start(v(''));
            $('#rc').checked = true;
            touch($('#rc'));
            const before = c.sent().length;
            resync(c, v(' disabled'));
            expect(checked()).toEqual([true, false, false]);
            expect(
                c
                    .sent()
                    .slice(before)
                    .filter((/** @type {any[]} */ f) => f[1] === 'pick'),
            ).toEqual([]);
        });
    });

    // What a server-owned control the user left alone held at the drop was the
    // server's, not the user's: the resync rendering its place unbound must not
    // bring that stale value back.
    it('restores nothing a server-owned control the user left alone held into an unbound one', async () => {
        const opt = (/** @type {string} */ x, /** @type {string} */ a) =>
            `<option value="${x}"${a}>${x}</option>`;
        const v = (/** @type {string} */ b, /** @type {string} */ s1, /** @type {string} */ s2) =>
            `<div id="page" az-view><form id="f">` +
            `<select name="s">${opt('o1', `${b}${s1}`)}${opt('o2', `${b}${s2}`)}</select>` +
            `<input type="checkbox" name="c" value="1"${b ? ' az-bound="checked"' : ''}${s2}>` +
            `<input type="radio" name="r" value="a"${b ? ' az-bound="checked"' : ''}${s1}>` +
            `<input type="radio" name="r" value="b"${b ? ' az-bound="checked"' : ''}${s2}>` +
            `</form></div>`;
        const c = await start(v(' az-bound="selected"', '', ' selected checked'));
        resync(c, v('', ' selected checked', ''));
        const checked = [...document.querySelectorAll('input')].map((e) => e.checked);
        expect([$('select').value, ...checked]).toEqual(['o1', false, true, false]);
    });

    // A radio group's edit is read off its radios, not off another control sharing
    // their name.
    it('keeps the server choice of a bound group when the user edited only a field of its name', async () => {
        const v = (/** @type {string} */ radios) =>
            `<div id="page" az-view><form id="f" az-change='[0,"chg"]'>${radios}` +
            `<textarea name="q" az-bound="value"></textarea></form></div>`;
        const c = await start(
            v(
                `<input type="radio" name="q" value="a" checked>` +
                    `<input type="radio" name="q" value="b" az-bound="checked">`,
            ),
        );
        type($('textarea'), 'typed');
        resync(
            c,
            v(
                `<input type="radio" name="q" value="a">` +
                    `<input type="radio" name="q" value="b" checked az-bound="checked">`,
            ),
        );
        const checked = [...document.querySelectorAll('input')].map((e) => e.checked);
        expect([checked, $('textarea').value]).toEqual([[false, true], 'typed']);
    });

    // A restored choice no replay proposes shows the server's again, for the whole
    // group: unchecking the radio the user chose alone would leave the group empty
    // when the server's radio is one the restore did not write.
    it.each([
        ['a bound group', ' az-bound="checked"'],
        ['a group mixing bound and unbound radios', ''],
    ])('shows the server choice of %s whose restored choice no replay proposes', async (_l, z) => {
        const b = ' az-bound="checked"';
        const v = (/** @type {string} */ extra, /** @type {string} */ a) =>
            `<div id="page" az-view><form id="f">${extra}` +
            `<input type="radio" name="r" value="a"${a}${b}>` +
            `<input type="radio" name="r" value="b"${b}></form></div>`;
        const c = await start(v('', ' checked'));
        $('[value=b]').click();
        resync(c, v(`<input type="radio" name="r" value="z" checked${z}>`, ''));
        const checked = [...document.querySelectorAll('input')].map((e) => e.checked);
        expect(checked).toEqual([true, false, false]);
    });

    // The server owns a radio group as a unit: one bound radio makes the whole group
    // server-owned, an unbound radio in it included.
    describe('a radio group mixing bound and unbound radios', () => {
        const v = (/** @type {string} */ d = '') =>
            `<div id="page" az-view><form id="f" az-change='[0,"chg"]'>` +
            `<input type="radio" name="plan" value="free" checked az-bound="checked">` +
            `<input type="radio" name="plan" value="pro"${d}></form></div>`;
        const checked = () => [...document.querySelectorAll('input')].map((e) => e.checked);

        // Whether the restore showed the user's choice is read off the whole group:
        // the unbound radio it checked differs from no default of a bound one, but
        // the bound radio that unchecked does.
        it('shows the server choice when no replay proposes the unbound radio chosen', async () => {
            const w =
                `<div id="page" az-view><form id="f">` +
                `<input type="radio" name="plan" value="free" checked disabled az-bound="checked">` +
                `<input type="radio" name="plan" value="pro"></form></div>`;
            const c = await start(w);
            $('[value=pro]').click();
            resync(c, w);
            expect(checked()).toEqual([true, false]);
        });

        // An unbound radio of a server-owned group is the server's too: the group
        // shows the user's choice while any of its radios differs from what the
        // server renders, a bound one or not.
        it('shows the server state when no replay proposes a choice that moved only unbound radios', async () => {
            const w = (/** @type {string} */ a) =>
                `<div id="page" az-view><form id="f">` +
                `<input type="radio" name="plan" value="free"${a} disabled az-bound="checked">` +
                `<input type="radio" name="plan" value="pro"></form></div>`;
            const c = await start(w(' checked'));
            $('[value=pro]').click();
            resync(c, w(''));
            expect(checked()).toEqual([false, false]);
        });

        // The form's replay proposes the group through its unbound radio when the
        // bound one is disabled: the proposal holds the group, so it shows the choice.
        it('restores a choice the form replay proposes through an unbound radio', async () => {
            const w =
                `<div id="page" az-view><form id="f" az-change='[0,"chg"]'>` +
                `<input type="radio" name="plan" value="free" disabled az-bound="checked">` +
                `<input type="radio" name="plan" value="pro"></form></div>`;
            const c = await start(w);
            $('[value=pro]').click();
            const before = c.sent().length;
            resync(c, w);
            expect(checked()).toEqual([false, true]);
            expect(
                c
                    .sent()
                    .slice(before)
                    .map((/** @type {any[]} */ f) => f.slice(0, 3)),
            ).toEqual([['page', 'chg', { plan: 'pro' }]]);
        });

        it('keeps the server state when the last replay may have crashed the view', async () => {
            const c = await start(v());
            $('[value=pro]').click();
            resync(c, v());
            expect(checked()).toEqual([false, true]);
            const before = c.sent().length;
            resync(c, v(), 4500); // before the server acknowledged the replay
            expect(checked()).toEqual([true, false]);
            expect(
                c
                    .sent()
                    .slice(before)
                    .filter((/** @type {any[]} */ f) => f[1] === 'chg'),
            ).toEqual([]);
        });

        it('keeps the server state when the resync disabled the unbound radio the user chose', async () => {
            const c = await start(v());
            $('[value=pro]').click();
            resync(c, v(' disabled'));
            expect(checked()).toEqual([true, false]);
        });
    });

    // A control inside an az-nodiff element is never diffed, so the server owns none
    // of it, `az-bound` or not: one the resync renders there takes the user's state
    // as an unbound control does, and nothing reverts it to a default the server
    // will never change.
    describe('a bound control the resync renders inside an az-nodiff element', () => {
        const page = (/** @type {string} */ inner, /** @type {boolean} */ nodiff) =>
            `<div id="page" az-view><form id="f" az-change='[0,"chg"]'>` +
            `${nodiff ? `<span az-nodiff>${inner}</span>` : inner}</form></div>`;
        const b = ' az-bound="checked"';

        it('keeps the user state of a checkbox', async () => {
            const box = `<input type="checkbox" name="c" value="v"${b}>`;
            const c = await start(page(box, false));
            $('input').click();
            resync(c, page(box, true));
            expect($('input').checked).toBe(true);
        });

        it('keeps the user choice of a radio group', async () => {
            const radios =
                `<input type="radio" name="r" value="a" checked${b}>` +
                `<input type="radio" name="r" value="b"${b}>`;
            const c = await start(page(radios, false));
            $('[value=b]').click();
            resync(c, page(radios, true));
            expect([$('[value=a]').checked, $('[value=b]').checked]).toEqual([false, true]);
        });
    });
    // Options sharing a value are told apart by their order among those: the user's
    // choice is that option, not every option of its value, nor the first.
    describe('a select whose options share a value', () => {
        const opts = (/** @type {string} */ a = '') =>
            `<option value="a"${a}>A1</option><option value="a"${a}>A2</option>` +
            `<option value="b"${a}>B</option>`;
        const page = (/** @type {string} */ inner, /** @type {string} */ ch = '') =>
            `<div id="page" az-view><form id="f"${ch}>${inner}</form></div>`;
        const selected = (/** @type {string} */ sel) =>
            [...$(sel).options].map((/** @type {any} */ o) => o.selected);

        it('restores the option a single select showed', async () => {
            const v = page(`<select name="s">${opts()}</select>`);
            const c = await start(v);
            $('select').selectedIndex = 1;
            change($('select'));
            resync(c, v);
            expect(selected('select')).toEqual([false, true, false]);
        });

        it('restores the options a multiple select showed', async () => {
            const v = page(`<select name="m" multiple>${opts()}</select>`);
            const c = await start(v);
            $('select').options[1].selected = true;
            change($('select'));
            resync(c, v);
            expect(selected('select')).toEqual([false, true, false]);
        });

        it('replays the options a bound multiple select showed', async () => {
            const v = page(
                `<select name="m" multiple>${opts(' az-bound="selected"')}</select>`,
                ` az-change='[0,"chg"]'`,
            );
            const c = await start(v);
            $('select').options[1].selected = true;
            change($('select'));
            const before = c.sent().length;
            resync(c, v);
            expect(selected('select')).toEqual([false, true, false]);
            expect(
                c
                    .sent()
                    .slice(before)
                    .map((/** @type {any[]} */ f) => f.slice(0, 3)),
            ).toEqual([['page', 'chg', { m: ['a'] }]]);
        });
    });

    // Outside every form a radio group is a unit too: it pairs by name, through its
    // checked radio's value, and any radio of a server-owned group proposes it.
    describe('a formless radio group', () => {
        const radio = (/** @type {string} */ x, /** @type {string} */ a = '') =>
            `<input type="radio" name="r" id="${x}" value="${x}"${a}>`;
        const page = (/** @type {string} */ inner) => `<div id="page" az-view>${inner}</div>`;
        const b = ' az-bound="checked"';
        const own = ` az-change='[0,"pick"]'`;
        const checked = () => [...document.querySelectorAll('input')].map((e) => e.checked);

        it('restores and proposes a choice of an unbound radio of a server-owned group', async () => {
            const v = page(radio('a', ` checked${b}`) + radio('b', own));
            const c = await start(v);
            $('#b').click();
            const before = c.sent().length;
            resync(c, v);
            expect(checked()).toEqual([false, true]);
            const replay = c.sent().slice(before);
            expect(replay.map((/** @type {any[]} */ f) => f.slice(0, 3))).toEqual([
                ['page', 'pick', { value: 'b', checked: true }],
            ]);
            c.msg(null, null, replay[0][3]);
            expect(checked()).toEqual([true, false]);
        });

        // A crash before the server acknowledged a group's replay withholds the
        // group's next one, whichever radio the user chooses meanwhile.
        it('keeps the server choice of a group whose last replay may have crashed the view', async () => {
            const v = page(
                radio('a', `${b}${own}`) +
                    radio('b', `${b}${own}`) +
                    radio('c', ` checked${b}${own}`),
            );
            const c = await start(v);
            $('#b').click();
            resync(c, v);
            expect(checked()).toEqual([false, true, false]);
            $('#a').click();
            const before = c.sent().length;
            resync(c, v, 4500);
            expect(checked()).toEqual([false, false, true]);
            expect(c.sent().slice(before)).toEqual([]);
        });

        // A group is keyed by its name, so id-less groups are withheld apart: the
        // crash withholds the one that replayed, not every group with no ids.
        it('withholds only the replay of the id-less group that replayed', async () => {
            const g = (/** @type {string} */ n, /** @type {string} */ on) =>
                ['a', 'b']
                    .map(
                        (x) =>
                            `<input type="radio" name="${n}" value="${x}"${x === on ? ' checked' : ''}` +
                            `${b} az-change='[0,"${n}"]'>`,
                    )
                    .join('');
            const v = page(g('g1', 'a') + g('g2', 'a'));
            const c = await start(v);
            const r = (/** @type {string} */ n, /** @type {string} */ x) =>
                $(`input[name=${n}][value=${x}]`);
            r('g1', 'b').click();
            resync(c, v);
            r('g2', 'b').click();
            const before = c.sent().length;
            resync(c, v, 4500); // before the server acknowledged g1's replay
            expect(checked()).toEqual([true, false, false, true]);
            expect(
                c
                    .sent()
                    .slice(before)
                    .map((/** @type {any[]} */ f) => f[1]),
            ).toEqual(['g2']);
        });

        it('restores a choice whose radio the resync gave another id', async () => {
            const v = (/** @type {string} */ id) =>
                page(
                    radio('a', ` checked${b}${own}`) +
                        radio('b', `${b}${own}`).replace('id="b"', `id="${id}"`),
                );
            const c = await start(v('b'));
            $('#b').click();
            resync(c, v('b2'));
            expect(checked()).toEqual([false, true]);
        });
    });
});

describe('a formless field in a popped-out (PiP) document', () => {
    const field = (/** @type {string} */ ev) =>
        `<input id="q" value="" az-bound="value" az-change='[0,"${ev}"]'>`;
    const main = `<div id="page" az-view>${field('main')}</div>`;
    const pv = `<div id="pv" az-view>${field('pip')}</div>`;
    /** The replayed events since `before`. @param {any} c @param {number} before */
    const replayed = (c, before) =>
        c
            .sent()
            .slice(before)
            .map((/** @type {any[]} */ f) => f.slice(0, 3));
    /** @param {any} el @param {string} v */
    const edit = (el, v) => {
        el.value = v;
        change(el);
    };
    /** The resync of both views, wherever they are. @param {any} c */
    const resyncBoth = (c, code = 1006) => {
        c.close(code);
        c.open(true);
        c.msg(
            [
                [8, 'page', main],
                [8, 'pv', pv],
            ],
            null,
            null,
            true,
        );
    };

    /** @type {Record<string, Function>} */
    let handlers = {};
    /** The `pagehide` of each window still open. @type {Function[]} */
    let open = [];
    /**
     * Close every window still open, bringing its view back inline. A resync keeps a
     * popped-out view in its window (see `arizona-pip.test.js`); a test of a view the
     * resync renders inline closes the window first.
     */
    const closeAll = () => {
        for (const hide of open.splice(0)) hide();
    };
    /** @type {Document} */
    let doc;

    afterEach(() => {
        /** @type {any} */ (window).documentPictureInPicture = undefined;
    });

    /**
     * Open view `pv` of connection `c` in a PiP window; `close()` brings it back, at
     * once or, `later`, on a task of its own, as a real window's `pagehide` fires.
     */
    async function popOut(/** @type {any} */ c, later = false, id = 'pv') {
        handlers = {};
        doc = document.implementation.createHTMLDocument('pip');
        const pip = {
            document: doc,
            addEventListener(/** @type {string} */ t, /** @type {Function} */ fn) {
                handlers[t] = fn;
                if (t === 'pagehide') open.push(once);
            },
            close() {
                if (later) setTimeout(once, 0);
                else once();
            },
        };
        const h = handlers;
        let done = false;
        function once() {
            if (done) return;
            done = true;
            open = open.filter((f) => f !== once);
            h.pagehide?.();
        }
        /** @type {any} */ (window).documentPictureInPicture = {
            requestWindow: vi.fn().mockResolvedValue(pip),
        };
        await c.mod.requestPip(id);
        return pip;
    }
    const q = (/** @type {Document} */ d) => /** @type {any} */ (d.getElementById('q'));

    // A restore writes each choice it restores with the time of the act it came from:
    // two documents' parts of a formless group, restored apart, merge on a pop-back
    // and the choice made later stands at the next drop, whichever restore ran last.
    it('keeps the later choice of two parts a restore wrote apart once they merge', async () => {
        const r = (/** @type {string} */ v) =>
            `<input type="radio" name="g" value="${v}" az-bound="checked" az-change='[0,"${v}"]'>`;
        const pg = `<div id="page" az-view>${r('a') + r('b')}</div>`;
        const ch = `<div id="pv" az-view>${r('c') + r('d')}</div>`;
        const both = (/** @type {any} */ c) => {
            c.close();
            c.open(true);
            c.msg(
                [
                    [8, 'page', pg],
                    [8, 'pv', ch],
                ],
                null,
                null,
                true,
            );
        };
        const c = await start(pg + ch);
        await popOut(c);
        /** @type {any} */ (doc.querySelector('[value=c]')).click();
        /** @type {any} */ ($('[value=a]')).click(); // the later choice
        both(c);
        expect([$('[value=a]').checked, doc.querySelector('[value=c]').checked]).toEqual([
            true,
            true,
        ]);
        closeAll(); // one document again: one of the two stays checked
        both(c);
        expect([...document.querySelectorAll('input:checked')].map((e) => e.value)).toEqual(['a']);
    });

    // Each document is saved apart: a formless field is saved by its id, which a
    // field in another document may carry too.
    it('restores each document its own edit of a formless field sharing an id', async () => {
        const c = await start(main + pv);
        await popOut(c);
        edit(q(document), 'typed in main');
        edit(q(doc), 'typed in pip');
        const before = c.sent().length;
        resyncBoth(c);
        expect(q(document).value).toBe('typed in main');
        expect(q(doc).value).toBe('typed in pip');
        expect(replayed(c, before)).toEqual([
            ['page', 'main', { value: 'typed in main' }],
            ['pv', 'pip', { value: 'typed in pip' }],
        ]);
    });

    // A save follows its view: a resync that re-renders the page holding a popped-out
    // child view keeps the child in its window, edit and all.
    it('restores the edit of a popped-out child view the resync keeps in its window', async () => {
        const page = `<div id="page" az-view>${pv}</div>`;
        const c = await start(page);
        await popOut(c);
        edit(q(doc), 'typed in pip');
        const before = c.sent().length;
        c.close();
        c.open(true);
        c.msg([[8, 'page', page]], null, null, true);
        expect(q(doc).value).toBe('typed in pip');
        expect(replayed(c, before)).toEqual([['pv', 'pip', { value: 'typed in pip' }]]);
    });

    // The window closing between the drop and the resync brings the view, and the
    // edit made in it, back to the page.
    it('restores the edit of a view that came back from PiP before the resync', async () => {
        const c = await start(`${main.replace(field('main'), '')}${pv}`);
        const pip = await popOut(c);
        edit(q(doc), 'typed in pip');
        const before = c.sent().length;
        c.close();
        pip.close();
        c.open(true);
        c.msg([[8, 'pv', pv]], null, null, true);
        expect(q(document).value).toBe('typed in pip');
        expect(replayed(c, before)).toEqual([['pv', 'pip', { value: 'typed in pip' }]]);
    });

    it('restores the edit of a view popped out before the resync', async () => {
        const c = await start(`${main.replace(field('main'), '')}${pv}`);
        edit(q(document), 'typed inline');
        const before = c.sent().length;
        c.close();
        await popOut(c);
        c.open(true);
        c.msg([[8, 'pv', pv]], null, null, true);
        expect(q(doc).value).toBe('typed inline');
        expect(replayed(c, before)).toEqual([['pv', 'pip', { value: 'typed inline' }]]);
    });

    // A crash withholds the replay of the field that replayed, not of a field with
    // the same id in another view.
    it('withholds only the replay of the view whose field replayed', async () => {
        const c = await start(main + pv);
        await popOut(c);
        edit(q(document), 'typed in main');
        resyncBoth(c);
        edit(q(doc), 'typed in pip');
        const before = c.sent().length;
        resyncBoth(c, 4500); // before the server acknowledged the main field's replay
        expect(q(doc).value).toBe('typed in pip');
        expect(replayed(c, before)).toEqual([['pv', 'pip', { value: 'typed in pip' }]]);
    });

    // Two fields sharing an id in two views are two failures, each warned.
    it('warns for each view whose formless field replay throws', async () => {
        const bad = (/** @type {string} */ id) =>
            `<div id="${id}" az-view><input id="q" value="" az-bound="value" ` +
            `az-change='[[1,"#["],[0,"x"]]'></div>`;
        const warn = vi.spyOn(console, 'warn').mockImplementation(() => {});
        const c = await start(bad('page') + bad('pv'));
        await popOut(c);
        edit(q(document), 'a');
        edit(q(doc), 'b');
        c.close();
        c.open(true);
        c.msg(
            [
                [8, 'page', bad('page')],
                [8, 'pv', bad('pv')],
            ],
            null,
            null,
            true,
        );
        const replayWarns = warn.mock.calls.filter(([msg]) => String(msg).includes('replayed'));
        expect(replayWarns).toHaveLength(2);
        warn.mockRestore();
    });

    /** A page view holding formless `q` and the child view `pv` inline. */
    const parent = `<div id="page" az-view>${field('main')}${pv}</div>`;
    /** The view and value of every `q` in `d`. */
    const qs = (d = document) =>
        [...d.querySelectorAll('#q')].map((e) => [
            /** @type {any} */ (e.closest('[az-view]')).id,
            /** @type {any} */ (e).value,
        ]);
    /** Resync the page view of `c` to `html`. */
    const resyncPage = (/** @type {any} */ c, /** @type {string} */ html, code = 1006) => {
        c.close(code);
        c.open(true);
        closeAll();
        c.msg([[8, 'page', html]], null, null, true);
    };

    // A page and its popped-out child, both with a formless `q`: the resync renders
    // the child inline, and each `q` takes its own view's edit.
    for (const later of [false, true])
        it(`restores each view its own edit of an id both brought back inline${later ? ' (pagehide later)' : ''}`, async () => {
            const c = await start(parent);
            await popOut(c, later);
            edit(q(document), 'typed in main');
            edit(q(doc), 'typed in pip');
            const before = c.sent().length;
            resyncPage(c, parent);
            expect(qs()).toEqual([
                ['page', 'typed in main'],
                ['pv', 'typed in pip'],
            ]);
            expect(replayed(c, before)).toEqual([
                ['page', 'main', { value: 'typed in main' }],
                ['pv', 'pip', { value: 'typed in pip' }],
            ]);
        });

    // A unit is restored into the views it was saved from: two views of the page
    // sharing an id are one unit, beside the popped-out child's.
    it('restores a unit spanning two views beside another of its name', async () => {
        const ch = `<div id="ch" az-view>${field('ch')}</div>`;
        const page = `<div id="page" az-view>${field('main')}${ch}${pv}</div>`;
        const c = await start(page);
        await popOut(c);
        const [a, b] = [...document.querySelectorAll('#q')];
        edit(a, 'A');
        edit(b, 'B');
        edit(q(doc), 'P');
        resyncPage(c, page);
        expect(qs()).toEqual([
            ['page', 'A'],
            ['ch', 'B'],
            ['pv', 'P'],
        ]);
    });

    // A view is its id: a popped-out child the resync remounts under another id (a
    // `?stateful` child whose id changed) is another view, which takes nothing.
    it('restores nothing of a popped-out child the resync remounts inline under another id', async () => {
        const c = await start(`<div id="page" az-view>${pv}</div>`);
        await popOut(c);
        edit(q(doc), 'typed in pip');
        const before = c.sent().length;
        resyncPage(c, `<div id="page" az-view>${pv.replace('id="pv"', 'id="pv2"')}</div>`);
        expect(qs()).toEqual([['pv2', '']]);
        expect(replayed(c, before)).toEqual([]);
    });

    const radio = (/** @type {string} */ ev, /** @type {string} */ v) =>
        `<input type="radio" name="g" value="${v}" az-bound="checked" az-change='[0,"${ev}"]'>`;
    const radios = (/** @type {string} */ ev) => radio(ev, 'x') + radio(ev, 'y');
    /** The view, value and checkedness of every radio of the page. */
    const group = () =>
        [...document.querySelectorAll('input[type=radio]')].map((e) => [
            /** @type {any} */ (e.closest('[az-view]')).id,
            /** @type {any} */ (e).value,
            /** @type {any} */ (e).checked,
        ]);
    const pick = (/** @type {any} */ el) => {
        el.click();
    };
    const inline = `<div id="page" az-view>${radios('main')}<div id="pv" az-view>${radios('pip')}</div></div>`;

    // A formless radio name is one group per document: the page's group and the
    // popped-out child's were two, each with its choice, and the resync renders them
    // as one, which shows one: the choice the user made last.
    for (const [first, last, on, event] of [
        ['page', 'pv', [false, false, false, true], ['pv', 'pip', { value: 'y', checked: true }]],
        [
            'pv',
            'page',
            [true, false, false, false],
            ['page', 'main', { value: 'x', checked: true }],
        ],
    ])
        it(`restores the later choice of two groups brought into one document (${last} last)`, async () => {
            const c = await start(inline);
            await popOut(c);
            const choose = (/** @type {string} */ where) =>
                pick(
                    where === 'page'
                        ? document.querySelector('[value=x]')
                        : doc.querySelector('[value=y]'),
                );
            choose(/** @type {string} */ (first));
            choose(/** @type {string} */ (last));
            const before = c.sent().length;
            resyncPage(c, inline);
            expect(group().map(([, , checked]) => checked)).toEqual(on);
            expect(replayed(c, before)).toEqual([event]);
        });

    // A restore keeps when the user made the choice it shows: two groups restored
    // apart, then brought together, show the one the user chose last, not the one
    // restored last.
    it('restores the later choice of two groups restored apart before being brought together', async () => {
        const c = await start(inline);
        await popOut(c);
        pick(doc.querySelector('[value=y]'));
        pick(document.querySelector('[value=x]'));
        // A resync of the popped-out view alone: each group is restored in its own
        // document, the page's first.
        c.close();
        c.open(true);
        c.msg([[8, 'pv', `<div id="pv" az-view>${radios('pip')}</div>`]], null, null, true);
        const before = c.sent().length;
        resyncPage(c, inline);
        expect(group().map(([, , on]) => on)).toEqual([true, false, false, false]);
        expect(replayed(c, before)).toEqual([['page', 'main', { value: 'x', checked: true }]]);
    });

    // A restore re-stamps the group it fills with when the user made the choice, on
    // the fresh radios too: two groups both restored apart, then brought together,
    // still compare by the user's acts.
    it('restores the later choice of two groups both re-rendered apart before being brought together', async () => {
        const m = `<div id="m" az-view>${radios('main')}</div>`;
        const pvOnly = `<div id="pv" az-view>${radios('pip')}</div>`;
        const page = `<div id="page" az-view>${m}${pvOnly}</div>`;
        const c = await start(page);
        await popOut(c);
        pick(doc.querySelector('[value=y]'));
        pick(document.querySelector('[value=x]'));
        c.close();
        c.open(true);
        c.msg(
            [
                [8, 'm', m],
                [8, 'pv', pvOnly],
            ],
            null,
            null,
            true,
        );
        const before = c.sent().length;
        resyncPage(c, page);
        expect(group().map(([, , on]) => on)).toEqual([true, false, false, false]);
        expect(replayed(c, before)).toEqual([['m', 'main', { value: 'x', checked: true }]]);
    });

    // A later choice with no radio left to pair with is no choice the group can show:
    // the older one that still has its radio shows.
    it('restores the older choice when the later one lost its radio', async () => {
        const c = await start(inline);
        await popOut(c);
        pick(document.querySelector('[value=x]'));
        pick(doc.querySelector('[value=y]'));
        const before = c.sent().length;
        resyncPage(c, inline.replace(radios('pip'), radio('pip', 'x')));
        expect(group().map(([, , on]) => on)).toEqual([true, false, false]);
        expect(replayed(c, before)).toEqual([['page', 'main', { value: 'x', checked: true }]]);
    });

    // A return to the server's choice made after choices in both groups shows the
    // server's, whichever group was chosen in first.
    it("shows the server's choice the user went back to last, after a choice in the other group", async () => {
        const page =
            `<div id="page" az-view>${radio('main', 'x').replace('>', ' checked>')}` +
            `${radio('main', 'y')}<div id="pv" az-view>${radios('pip')}</div></div>`;
        const c = await start(page);
        await popOut(c);
        pick(document.querySelector('[value=y]'));
        pick(doc.querySelector('[value=y]'));
        pick(document.querySelector('[value=x]'));
        const before = c.sent().length;
        resyncPage(c, page);
        expect(group().map(([, , on]) => on)).toEqual([true, false, false, false]);
        expect(replayed(c, before)).toEqual([]);
    });

    // Three documents, two choices: the later one, whichever window holds it.
    for (const [first, last] of [
        ['a', 'b'],
        ['b', 'a'],
    ])
        it(`restores the later of two choices from two windows (${last} last)`, async () => {
            const page =
                `<div id="page" az-view>${radios('main')}<div id="a" az-view>${radios('a')}</div>` +
                `<div id="b" az-view>${radios('b')}</div></div>`;
            const c = await start(page);
            const docs = {
                a: (await popOut(c, false, 'a')).document,
                b: (await popOut(c, false, 'b')).document,
            };
            pick(docs[/** @type {'a'} */ (first)].querySelector('[value=y]'));
            pick(docs[/** @type {'a'} */ (last)].querySelector('[value=x]'));
            const before = c.sent().length;
            resyncPage(c, page);
            expect(group().filter(([, , checked]) => checked)).toEqual([[last, 'x', true]]);
            expect(replayed(c, before)).toEqual([[last, last, { value: 'x', checked: true }]]);
        });

    it('restores the one choice of two groups brought into one document', async () => {
        const c = await start(inline);
        await popOut(c);
        pick(doc.querySelector('[value=y]'));
        const before = c.sent().length;
        resyncPage(c, inline);
        expect(group()).toEqual([
            ['page', 'x', false],
            ['page', 'y', false],
            ['pv', 'x', false],
            ['pv', 'y', true],
        ]);
        expect(replayed(c, before)).toEqual([['pv', 'pip', { value: 'y', checked: true }]]);
    });

    // A group brought from another document pairs among the radios of its views: a
    // radio of the page sharing the chosen value is another group's.
    it('restores a choice brought inline onto its own radio, not a page radio of its value', async () => {
        const page =
            `<div id="page" az-view><input type="radio" name="g" value="v" checked>` +
            `<div id="pv" az-view>${radio('pip', 'v')}</div></div>`;
        const c = await start(page);
        await popOut(c);
        pick(doc.querySelector('[value=v]'));
        const before = c.sent().length;
        resyncPage(c, page);
        expect(group()).toEqual([
            ['page', 'v', false],
            ['pv', 'v', true],
        ]);
        expect(replayed(c, before)).toEqual([['pv', 'pip', { value: 'v', checked: true }]]);
    });

    // A group that replayed and was then split by a pop-out is withheld whole.
    it('withholds both parts of a group a pop-out split after it replayed', async () => {
        const c = await start(inline);
        pick(document.querySelector('#pv [value=y]'));
        resyncPage(c, inline);
        await popOut(c);
        pick(document.querySelector('[value=x]'));
        const before = c.sent().length;
        resyncPage(c, inline, 4500);
        expect(group().map(([, , on]) => on)).toEqual([false, false, false, false]);
        expect(replayed(c, before)).toEqual([]);
    });

    // Every view the group spans replayed, not only the chosen radio's or the first:
    // the part a pop-out takes away is withheld too, also when the user's later
    // choice is in it.
    it('withholds the popped-out part of a group whose choice was in the page', async () => {
        const c = await start(inline);
        pick(document.querySelector('[value=x]'));
        resyncPage(c, inline);
        await popOut(c);
        pick(doc.querySelector('[value=y]'));
        const before = c.sent().length;
        resyncPage(c, inline, 4500);
        expect(group().map(([, , on]) => on)).toEqual([false, false, false, false]);
        expect(replayed(c, before)).toEqual([]);
    });

    // The user's last act on a name decides: a group whose chosen radio a diff
    // disabled before the drop holds the server's state, and that later act
    // overrides the other group's older choice.
    it("shows the server's choice when the group chosen last had its choice disabled", async () => {
        const c = await start(inline);
        await popOut(c);
        pick(document.querySelector('[value=x]'));
        pick(doc.querySelector('[value=y]'));
        /** @type {any} */ (doc.querySelector('[value=y]')).disabled = true;
        const before = c.sent().length;
        resyncPage(c, inline);
        expect(group().map(([, , on]) => on)).toEqual([false, false, false, false]);
        expect(replayed(c, before)).toEqual([]);
    });

    // Going back to the server's choice is a choice too: made last, it shows.
    it("shows the server's choice the user went back to after choosing in two groups", async () => {
        const page =
            `<div id="page" az-view>${radio('main', 'x').replace('>', ' checked>')}` +
            `${radio('main', 'y')}<div id="pv" az-view>${radios('pip')}</div></div>`;
        const c = await start(page);
        await popOut(c);
        pick(doc.querySelector('[value=y]'));
        pick(document.querySelector('[value=y]'));
        pick(document.querySelector('[value=x]'));
        const before = c.sent().length;
        resyncPage(c, page);
        expect(group().map(([, , on]) => on)).toEqual([true, false, false, false]);
        expect(replayed(c, before)).toEqual([]);
    });

    // A part of a group whose view the resync remounted under another id is gone, so
    // its choice is placed nowhere: the latest of the parts still placeable shows.
    const remounted = inline.replace('id="pv"', 'id="pv2"');
    for (const [order, on, event] of [
        [['pip'], [false, false, false, false], null],
        [
            ['page', 'pip'],
            [true, false, false, false],
            ['page', 'main', { value: 'x', checked: true }],
        ],
        [
            ['pip', 'page'],
            [true, false, false, false],
            ['page', 'main', { value: 'x', checked: true }],
        ],
    ])
        it(`restores only the page's part when the popped-out child comes back under another id (${order})`, async () => {
            const c = await start(inline);
            await popOut(c);
            for (const where of /** @type {string[]} */ (order))
                pick(
                    where === 'page'
                        ? document.querySelector('[value=x]')
                        : doc.querySelector('[value=y]'),
                );
            const before = c.sent().length;
            resyncPage(c, remounted);
            expect(group().map(([, , checked]) => checked)).toEqual(on);
            expect(replayed(c, before)).toEqual(event ? [event] : []);
        });

    // An event that changes no group's choice is no act: code dispatching `change`
    // on an unchecked radio, or on the server's choice nobody left, decides nothing.
    for (const [what, noise] of [
        ['an unchecked radio of the other group', () => change(doc.querySelector('[value=x]'))],
        ["the other group's unchanged choice", () => change(doc.querySelector('[value=y]'))],
    ])
        it(`keeps the user's choice when code fires a change on ${what}`, async () => {
            const c = await start(inline);
            await popOut(c);
            pick(doc.querySelector('[value=y]'));
            pick(document.querySelector('[value=x]'));
            noise();
            const before = c.sent().length;
            resyncPage(c, inline);
            expect(group().map(([, , on]) => on)).toEqual([true, false, false, false]);
            expect(replayed(c, before)).toEqual([['page', 'main', { value: 'x', checked: true }]]);
        });

    it("keeps the user's choice when code fires a change on the server's choice of the other group", async () => {
        const page =
            `<div id="page" az-view>${radio('main', 'x').replace('>', ' checked>')}` +
            `${radio('main', 'y')}<div id="pv" az-view>${radios('pip')}</div></div>`;
        const c = await start(page);
        await popOut(c);
        pick(doc.querySelector('[value=y]'));
        change(document.querySelector('[value=x]'));
        const before = c.sent().length;
        resyncPage(c, page);
        expect(group().map(([, , on]) => on)).toEqual([false, false, false, true]);
        expect(replayed(c, before)).toEqual([['pv', 'pip', { value: 'y', checked: true }]]);
    });

    // What a group showed is remembered on its radios: a pop-out that splits a group
    // whose choice unchecked the default leaves the window's part showing none, so
    // picking the default there is an act -- made last, it shows the server's.
    it("shows the server's choice the user picked in a window's part of a split group", async () => {
        const page =
            `<div id="page" az-view>${radios('main')}` +
            `<div id="pv" az-view>${radio('pip', 'z').replace('>', ' checked>')}</div></div>`;
        const c = await start(page);
        pick(document.querySelector('[value=y]'));
        await popOut(c);
        pick(doc.querySelector('[value=z]'));
        const before = c.sent().length;
        resyncPage(c, page);
        expect(group().map(([, , on]) => on)).toEqual([false, false, true]);
        expect(replayed(c, before)).toEqual([]);
    });

    // A view is its id: views the resync remounted under other ids take nothing, so
    // no edit lands in a view it was not made in.
    it('restores nothing of views remounted under other ids', async () => {
        const ch = `<div id="ch" az-view>${field('ch')}</div>`;
        const page = `<div id="page" az-view>${ch}${pv}</div>`;
        const c = await start(page);
        await popOut(c);
        edit(q(document), 'A');
        edit(q(doc), 'P');
        const before = c.sent().length;
        resyncPage(c, page.replace('id="ch"', 'id="ch2"').replace('id="pv"', 'id="pv2"'));
        expect(qs()).toEqual([
            ['ch2', ''],
            ['pv2', ''],
        ]);
        expect(replayed(c, before)).toEqual([]);
    });

    it("restores a group's part in its own view, not in a remount of another view", async () => {
        const page =
            `<div id="page" az-view><div id="a" az-view>${radios('main')}</div>` +
            `<div id="b" az-view>${radios('main')}</div><div id="pv" az-view>${radios('pip')}</div></div>`;
        const c = await start(page);
        await popOut(c);
        pick(document.querySelector('#a [value=x]'));
        pick(doc.querySelector('[value=y]'));
        const before = c.sent().length;
        resyncPage(c, page.replace('id="b"', 'id="b2"').replace('id="pv"', 'id="pv2"'));
        expect(group().filter(([, , on]) => on)).toEqual([['a', 'x', true]]);
        expect(replayed(c, before)).toEqual([['a', 'main', { value: 'x', checked: true }]]);
    });

    it('restores nothing of two groups whose views were remounted', async () => {
        const page =
            `<div id="page" az-view><div id="ch" az-view>${radios('ch')}</div>` +
            `<div id="pv" az-view>${radios('pip')}</div></div>`;
        const c = await start(page);
        await popOut(c);
        pick(document.querySelector('#ch [value=x]'));
        pick(doc.querySelector('[value=y]'));
        const before = c.sent().length;
        resyncPage(c, page.replace('id="ch"', 'id="ch2"').replace('id="pv"', 'id="pv2"'));
        expect(group().filter(([, , on]) => on)).toEqual([]);
        expect(replayed(c, before)).toEqual([]);
    });

    // A unit brought from another document pairs among the fields that came with
    // it, not a field of the page sharing its id.
    it('restores a popped-out field into its own view when the page renders another of its id', async () => {
        const page = `<div id="page" az-view><div id="a" az-view>${field('main')}</div>${pv}</div>`;
        const c = await start(page);
        await popOut(c);
        edit(q(doc), 'P');
        const before = c.sent().length;
        resyncPage(c, `<div id="page" az-view>${field('main')}${pv}</div>`);
        expect(qs()).toEqual([
            ['page', ''],
            ['pv', 'P'],
        ]);
        expect(replayed(c, before)).toEqual([['pv', 'pip', { value: 'P' }]]);
    });

    // A field the resync renders in another view is that view's: even alone of its
    // id, it takes nothing of what was typed in the view it left.
    it('restores nothing of a popped-out field the resync moves into the page', async () => {
        const c = await start(`<div id="page" az-view>${pv}</div>`);
        await popOut(c);
        edit(q(doc), 'P');
        const before = c.sent().length;
        resyncPage(c, `<div id="page" az-view>${field('pip')}<div id="pv" az-view></div></div>`);
        expect(qs()).toEqual([['page', '']]);
        expect(replayed(c, before)).toEqual([]);
    });

    // The group acted on last decides even when its disabled choice lost its radio:
    // it holds no choice, so the server's shows, not the older group's.
    it("shows the server's choice when the later group's disabled choice lost its radio", async () => {
        const c = await start(inline);
        await popOut(c);
        pick(document.querySelector('[value=x]'));
        pick(doc.querySelector('[value=y]'));
        /** @type {any} */ (doc.querySelector('[value=y]')).disabled = true;
        const before = c.sent().length;
        resyncPage(c, inline.replace(radios('pip'), radio('pip', 'x')));
        expect(group().map(([, , on]) => on)).toEqual([false, false, false]);
        expect(replayed(c, before)).toEqual([]);
    });

    // A refusal the reconcile showed resets what the group shows: a later `change`
    // code fires on the server's choice there is no act.
    it('keeps the later choice when code fires a change on a group a refusal reset', async () => {
        const page =
            `<div id="page" az-view>${radio('main', 'x').replace('>', ' checked>')}` +
            `${radio('main', 'y')}<div id="pv" az-view>${radios('pip')}</div></div>`;
        const c = await start(page);
        await popOut(c);
        pick(document.querySelector('[value=y]'));
        c.msg(null, null, c.lastRef()); // refused: the page group shows x again
        pick(doc.querySelector('[value=y]'));
        change(document.querySelector('[value=x]'));
        const before = c.sent().length;
        resyncPage(c, page);
        expect(group().map(([, , on]) => on)).toEqual([false, false, false, true]);
        expect(replayed(c, before)).toEqual([['pv', 'pip', { value: 'y', checked: true }]]);
    });

    // A restore sets what the group shows: a `change` code fires on the restored
    // choice is no act either.
    it('keeps the later choice when code fires a change on a restored choice', async () => {
        const m = `<div id="m" az-view>${radios('main')}</div>`;
        const pvOnly = `<div id="pv" az-view>${radios('pip')}</div>`;
        const page = `<div id="page" az-view>${m}${pvOnly}</div>`;
        const c = await start(page);
        await popOut(c);
        pick(document.querySelector('[value=y]'));
        c.close();
        c.open(true);
        c.msg(
            [
                [8, 'm', m],
                [8, 'pv', pvOnly],
            ],
            null,
            null,
            true,
        );
        pick(doc.querySelector('[value=x]'));
        change(document.querySelector('#m [value=y]'));
        const before = c.sent().length;
        resyncPage(c, page);
        expect(group().filter(([, , on]) => on)).toEqual([['pv', 'x', true]]);
        expect(replayed(c, before)).toEqual([['pv', 'pip', { value: 'x', checked: true }]]);
    });

    // A pop-back merges two groups each seen apart: the group shows the radio the
    // window's part held (the browser keeps the inserted choice), so the user's
    // pick of the page's radio after it is an act.
    it('counts a pick after a pop-back merged two parts of a group as an act', async () => {
        const c = await start(inline);
        const pip = await popOut(c);
        pick(document.querySelector('[value=x]'));
        pick(doc.querySelector('[value=y]'));
        pip.close();
        expect(group().map(([, , on]) => on)).toEqual([false, false, false, true]);
        pick(document.querySelector('[value=x]'));
        const before = c.sent().length;
        resyncPage(c, inline);
        expect(group().map(([, , on]) => on)).toEqual([true, false, false, false]);
        expect(replayed(c, before)).toEqual([['page', 'main', { value: 'x', checked: true }]]);
    });

    // The user's choice in the window, unbound there, is theirs in the server-owned
    // group a pop-back merges it into: the part acted on last, its choice shows and is
    // proposed. A `change` code fires on it after is no act.
    it("restores the window's choice a pop-back merged into a server-owned group", async () => {
        const page =
            `<div id="page" az-view>${radio('main', 'x').replace('>', ' checked>')}` +
            `${radio('main', 'y')}<div id="pv" az-view>` +
            `<input type="radio" name="g" value="z" az-change='[0,"pip"]'></div></div>`;
        const c = await start(page);
        const pip = await popOut(c);
        pick(doc.querySelector('[value=z]'));
        pip.close();
        change(document.querySelector('[value=x]'));
        const before = c.sent().length;
        resyncPage(c, page);
        expect(group().map(([, , on]) => on)).toEqual([false, false, true]);
        expect(replayed(c, before).map((f) => f[1])).toEqual(['pip']);
    });

    // A part no act touched that holds a choice -- the window's server default a
    // pop-back brought into a group the user had acted on -- does not win by its
    // place: the part acted on last decides, here a return to the server's.
    it("shows the server's choice over an un-acted part a pop-back brought in", async () => {
        const page =
            `<div id="page" az-view>${radio('main', 'x').replace('>', ' checked>')}${radio('main', 'y')}` +
            `<div id="pv" az-view>${radio('pip', 'z').replace('>', ' checked>')}</div></div>`;
        const c = await start(page);
        const pip = await popOut(c);
        pick(document.querySelector('[value=y]'));
        pick(document.querySelector('[value=x]'));
        pip.close();
        // A browser keeps the inserted checked radio and unchecks the rest of the
        // group (jsdom does the reverse): do what the browser does.
        /** @type {any} */ (document.querySelector('[value=z]')).checked = true;
        const before = c.sent().length;
        resyncPage(c, page);
        expect(replayed(c, before)).toEqual([]);
    });

    // A write of `checked` on a radio no proposal holds and the server does not own
    // looks nothing up: a page of such radios pays nothing per write.
    it('looks nothing up for a checked removal on an unbound radio', async () => {
        const n = 200;
        const rs = Array.from(
            { length: n },
            (_, i) => `<input az="${i}" type="radio" name="g${i}" value="v" checked>`,
        ).join('');
        const c = await start(`<div id="page" az-view>${rs}</div>`);
        const qsa = vi.spyOn(Document.prototype, 'querySelectorAll');
        const byName = vi.spyOn(Document.prototype, 'getElementsByName');
        c.msg(Array.from({ length: n }, (_, i) => [REM_ATTR, `page:${i}`, 'checked']));
        expect(qsa.mock.calls.length + byName.mock.calls.length).toBe(0);
        qsa.mockRestore();
        byName.mockRestore();
    });

    // A view is its id: the remounted view is another one, so
    // nothing is restored rather than something into the wrong view.
    it('restores nothing of a remounted view whose place the resync cannot prove', async () => {
        const page = `<div id="page" az-view><div id="a" az-view></div>${pv}</div>`;
        const c = await start(page);
        await popOut(c);
        edit(q(doc), 'P');
        const before = c.sent().length;
        resyncPage(c, `<div id="page" az-view>${pv.replace('id="pv"', 'id="pv2"')}</div>`);
        expect(qs()).toEqual([['pv2', '']]);
        expect(replayed(c, before)).toEqual([]);
    });

    // The restore reads each field's view once: fields sharing ids across documents
    // cost it linear work, not a pass over every field per saved unit.
    it('restores fields sharing ids across documents in work linear in their number', async () => {
        const calls = async (/** @type {number} */ n) => {
            const fs = Array.from(
                { length: n },
                (_, i) => `<input id="f${i}" value="" az-bound="value">`,
            ).join('');
            const page = `<div id="page" az-view>${fs}<div id="pv" az-view>${fs}</div></div>`;
            const c = await start(page);
            await popOut(c);
            c.close();
            c.open(true);
            const spy = vi.spyOn(Element.prototype, 'closest');
            c.msg([[8, 'page', page]], null, null, true);
            const k = spy.mock.calls.length;
            spy.mockRestore();
            c.restore();
            return k;
        };
        const [small, large] = [await calls(100), await calls(200)];
        expect(large / small).toBeLessThan(2.5);
    });
});

describe('a formless field whose view changes', () => {
    const field = `<input id="q" value="" az-bound="value" az-change='[0,"ev"]'>`;
    /** @param {any} el @param {string} v */
    const edit = (el, v) => {
        el.value = v;
        change(el);
    };
    const resyncPage = (/** @type {any} */ c, /** @type {string} */ html, code = 1006) => {
        c.close(code);
        c.open(true);
        c.msg([[8, 'page', html]], null, null, true);
    };
    const qs = () => [...document.querySelectorAll('#q')].map((e) => /** @type {any} */ (e).value);
    const q = (/** @type {Document} */ d) => /** @type {any} */ (d.getElementById('q'));

    // A view is its id: a field the resync renders in another view -- a remount
    // under another id, or another view altogether -- takes nothing of the edit.
    for (const [what, v1, v2] of [
        [
            'remounted under another id',
            `<div id="page" az-view><div id="c1" az-view>${field}</div></div>`,
            `<div id="page" az-view><div id="c2" az-view>${field}</div></div>`,
        ],
        [
            'moved from a child view into the page',
            `<div id="page" az-view><div id="ch" az-view>${field}</div></div>`,
            `<div id="page" az-view>${field}<div id="ch" az-view></div></div>`,
        ],
    ])
        it(`restores nothing of a field ${what}`, async () => {
            const c = await start(v1);
            edit(q(document), 'typed');
            const before = c.sent().length;
            resyncPage(c, v2);
            expect(qs()).toEqual(['']);
            expect(c.sent().slice(before)).toEqual([]);
        });

    // A view the resync removed is gone: its field, rendered elsewhere, is
    // another view's and takes nothing.
    it('restores nothing of a field whose view the resync removed', async () => {
        const c = await start(`<div id="page" az-view><div id="ch" az-view>${field}</div></div>`);
        edit(q(document), 'typed');
        const before = c.sent().length;
        resyncPage(c, `<div id="page" az-view>${field}</div>`);
        expect(qs()).toEqual(['']);
        expect(c.sent().slice(before)).toEqual([]);
    });

    // A remounted view is another view: it takes nothing, and neither does a field of its id
    // the page renders elsewhere.
    it('restores nothing into a remount or a page field sharing the id', async () => {
        const c = await start(`<div id="page" az-view><div id="ch" az-view>${field}</div></div>`);
        edit(q(document), 'typed');
        const before = c.sent().length;
        resyncPage(c, `<div id="page" az-view>${field}<div id="ch2" az-view>${field}</div></div>`);
        expect(qs()).toEqual(['', '']);
        expect(c.sent().slice(before)).toEqual([]);
    });

    // A remount beside a removed sibling is another view too.
    it('restores nothing of a remounted view whose sibling the resync removed', async () => {
        const c = await start(
            `<div id="page" az-view><div id="ch" az-view>${field}</div><div id="b" az-view></div></div>`,
        );
        edit(q(document), 'typed');
        const before = c.sent().length;
        resyncPage(c, `<div id="page" az-view><div id="ch2" az-view>${field}</div></div>`);
        expect(qs()).toEqual(['']);
        expect(c.sent().slice(before)).toEqual([]);
    });

    it('withholds the replays of two views sharing a formless id after a crash', async () => {
        const v = `<div id="page" az-view>${field}<div id="ch" az-view>${field}</div></div>`;
        const c = await start(v);
        const [a, b] = [...document.querySelectorAll('#q')];
        edit(a, 'A');
        edit(b, 'B');
        let before = c.sent().length;
        resyncPage(c, v);
        expect(qs()).toEqual(['A', 'B']);
        expect(c.sent().length).toBe(before + 2);
        before = c.sent().length;
        resyncPage(c, v, 4500);
        expect(qs()).toEqual(['', '']);
        expect(c.sent().slice(before)).toEqual([]);
    });

    const radio = (/** @type {string} */ v) =>
        `<input type="radio" name="g" value="${v}" az-bound="checked" az-change='[0,"g"]'>`;
    const checked = () =>
        [...document.querySelectorAll('input')].map((e) => /** @type {any} */ (e).checked);

    // A radio group is one unit wherever its radios are: a crash withholds the
    // group that replayed, whichever view the radio it replayed from is in.
    it('withholds the replay of a group spanning two views after a crash', async () => {
        const v =
            `<div id="page" az-view><div id="A" az-view>${radio('x')}</div>` +
            `<div id="B" az-view>${radio('y')}${radio('z')}</div></div>`;
        const c = await start(v);
        const z = /** @type {any} */ (document.querySelector('[value=z]'));
        z.click();
        let before = c.sent().length;
        resyncPage(c, v);
        expect(checked()).toEqual([false, false, true]);
        expect(c.sent().length).toBe(before + 1);
        before = c.sent().length;
        resyncPage(c, v, 4500);
        expect(checked()).toEqual([false, false, false]);
        expect(c.sent().slice(before)).toEqual([]);
    });

    // A diff after the replay can add a radio of the group in another view, ahead
    // of the ones that replayed: the crash still withholds the group.
    it('withholds a group a diff gave a radio in another view since it replayed', async () => {
        const v =
            `<div id="page" az-view><div id="A" az-view></div>` +
            `<div id="B" az-view>${radio('y')}${radio('z')}</div></div>`;
        const c = await start(v);
        const z = /** @type {any} */ (document.querySelector('[value=z]'));
        z.click();
        resyncPage(c, v);
        c.msg([[8, 'A', `<div id="A" az-view>${radio('x')}</div>`]]);
        const before = c.sent().length;
        resyncPage(
            c,
            v.replace('<div id="A" az-view></div>', `<div id="A" az-view>${radio('x')}</div>`),
            4500,
        );
        expect(checked()).toEqual([false, false, false]);
        expect(c.sent().slice(before)).toEqual([]);
    });
});

describe('a formless field restores only into its own view id', () => {
    const field = (/** @type {string} */ ev, id = 'q') =>
        `<input id="${id}" value="" az-bound="value" az-change='[0,"${ev}"]'>`;
    const edit = (/** @type {any} */ el, /** @type {string} */ v) => {
        el.value = v;
        change(el);
    };
    const replayed = (/** @type {any} */ c, /** @type {number} */ before) =>
        c
            .sent()
            .slice(before)
            .map((/** @type {any[]} */ f) => f.slice(0, 3));
    const resyncPage = (/** @type {any} */ c, /** @type {string} */ html, code = 1006) => {
        c.close(code);
        c.open(true);
        c.msg([[8, 'page', html]], null, null, true);
    };
    const qs = () =>
        [...document.querySelectorAll('#q')].map((e) => [
            /** @type {any} */ (e.closest('[az-view]')).id,
            /** @type {any} */ (e).value,
        ]);
    const radio = (/** @type {string} */ ev, /** @type {string} */ v) =>
        `<input type="radio" name="g" value="${v}" az-bound="checked" az-change='[0,"${ev}"]'>`;
    const radios = (/** @type {string} */ ev) => radio(ev, 'x') + radio(ev, 'y');
    const group = () =>
        [...document.querySelectorAll('input[type=radio]')].map((e) => [
            /** @type {any} */ (e.closest('[az-view]')).id,
            /** @type {any} */ (e).value,
            /** @type {any} */ (e).checked,
        ]);
    const pick = (/** @type {any} */ el) => {
        el.click();
    };
    const view = (/** @type {string} */ id, /** @type {string} */ body) =>
        `<div id="${id}" az-view>${body}</div>`;

    // a record child view at the same place, now showing another record.
    it('edits of item-5 are not put into item-6 at its place', async () => {
        const c = await start(view('page', view('item-5', field('save'))));
        edit($('#q'), 'title of record 5');
        const before = c.sent().length;
        resyncPage(c, view('page', view('item-6', field('save'))));
        expect({ qs: qs(), sent: replayed(c, before) }).toEqual({ qs: [['item-6', '']], sent: [] });
    });
    // another handler (tab switch) at the same place.
    it('a group of tab B is not replayed to tab A', async () => {
        const c = await start(view('page', view('tab-b', radios('b'))));
        pick(document.querySelector('[value=y]'));
        const before = c.sent().length;
        resyncPage(c, view('page', view('tab-a', radios('a'))));
        expect({ g: group(), sent: replayed(c, before) }).toEqual({
            g: [
                ['tab-a', 'x', false],
                ['tab-a', 'y', false],
            ],
            sent: [],
        });
    });
    // keyed sibling views reordered, kept ids, sharing a formless id.
    it('reordered sibling views keep their own text', async () => {
        const items = (/** @type {string[]} */ ids) =>
            view('page', ids.map((i) => view(i, field(i))).join(''));
        const c = await start(items(['i1', 'i2']));
        const [a, b] = [...document.querySelectorAll('#q')];
        edit(a, 'A');
        edit(b, 'B');
        const before = c.sent().length;
        resyncPage(c, items(['i2', 'i1']));
        expect(qs()).toEqual([
            ['i2', 'B'],
            ['i1', 'A'],
        ]);
        expect(replayed(c, before)).toEqual([
            ['i2', 'i2', { value: 'B' }],
            ['i1', 'i1', { value: 'A' }],
        ]);
    });
    // a sibling view inserted at the front, ids kept.
    it('a view inserted before keeps edits in their own views', async () => {
        const items = (/** @type {string[]} */ ids) =>
            view('page', ids.map((i) => view(i, field(i))).join(''));
        const c = await start(items(['i1', 'i2']));
        const [a] = [...document.querySelectorAll('#q')];
        edit(a, 'A');
        const before = c.sent().length;
        resyncPage(c, items(['i0', 'i1', 'i2']));
        expect(qs()).toEqual([
            ['i0', ''],
            ['i1', 'A'],
            ['i2', ''],
        ]);
        expect(replayed(c, before)).toEqual([['i1', 'i1', { value: 'A' }]]);
    });
    // one formless group spanning reordered views.
    it('a group spanning reordered views keeps its choice in its view', async () => {
        const items = (/** @type {string[]} */ ids) =>
            view('page', ids.map((i) => view(i, radios(i))).join(''));
        const c = await start(items(['i1', 'i2']));
        pick(document.querySelector('#i1 [value=x]'));
        const before = c.sent().length;
        resyncPage(c, items(['i2', 'i1']));
        expect(group().filter(([, , on]) => on)).toEqual([['i1', 'x', true]]);
        expect(replayed(c, before)).toEqual([['i1', 'i1', { value: 'x', checked: true }]]);
    });
    // the one view holding the id is removed; a new view elsewhere holds it.
    it('a unit spanning two views: one removed, another view added holding the id', async () => {
        const c = await start(view('page', view('a', field('a')) + view('b', field('b'))));
        const [a] = [...document.querySelectorAll('#q')];
        edit(a, 'A');
        const before = c.sent().length;
        resyncPage(c, view('page', view('c', field('c')) + view('b', field('b'))));
        expect(qs()).toEqual([
            ['c', ''],
            ['b', ''],
        ]);
        expect(replayed(c, before)).toEqual([]);
    });

    it('a unit spanning two views: one removed, a view appended holding the id', async () => {
        const c = await start(view('page', view('a', field('a')) + view('b', field('b'))));
        const [a] = [...document.querySelectorAll('#q')];
        edit(a, 'A');
        const before = c.sent().length;
        resyncPage(c, view('page', view('b', field('b')) + view('c', field('c'))));
        expect({ qs: qs(), sent: replayed(c, before) }).toEqual({
            qs: [
                ['b', ''],
                ['c', ''],
            ],
            sent: [],
        });
    });
    it('a child field removed: the page own field of that id takes nothing', async () => {
        const c = await start(view('page', view('ch', field('ch'))));
        edit($('#q'), 'typed in child');
        const before = c.sent().length;
        resyncPage(c, view('page', field('page') + view('ch', '')));
        expect({ qs: qs(), sent: replayed(c, before) }).toEqual({ qs: [['page', '']], sent: [] });
    });
    it('a child group removed: the page own group takes nothing', async () => {
        const c = await start(view('page', view('ch', radios('ch'))));
        pick(document.querySelector('[value=y]'));
        const before = c.sent().length;
        resyncPage(c, view('page', radios('page') + view('ch', '')));
        expect({ g: group(), sent: replayed(c, before) }).toEqual({
            g: [
                ['page', 'x', false],
                ['page', 'y', false],
            ],
            sent: [],
        });
    });

    it('a gone group: a choice made in a lands not in b successor', async () => {
        const pg = (/** @type {string} */ a, /** @type {string} */ ab, /** @type {string} */ b) =>
            view('page', view(a, ab) + view(b, radios('b')));
        const c = await start(pg('a', radios('a'), 'b'));
        pick(document.querySelector('#a [value=x]'));
        const before = c.sent().length;
        resyncPage(c, pg('a2', radio('a', 'y'), 'b2'));
        expect(group().filter(([, , on]) => on)).toEqual([]);
        expect(replayed(c, before)).toEqual([]);
    });
    it('a kept group: a choice made in a, whose x is gone, lands not in b', async () => {
        const pg = (/** @type {string} */ ab) =>
            view('page', view('a', ab) + view('b', radios('b')));
        const c = await start(pg(radios('a')));
        pick(document.querySelector('#a [value=x]'));
        const before = c.sent().length;
        resyncPage(c, pg(radio('a', 'y')));
        expect(group().filter(([, , on]) => on)).toEqual([]);
        expect(replayed(c, before)).toEqual([]);
    });

    describe('acts', () => {
        /** @type {Record<string, Function>} */
        let handlers = {};
        /** @type {Document} */
        let doc;
        afterEach(() => {
            /** @type {any} */ (window).documentPictureInPicture = undefined;
        });
        async function popOut(/** @type {any} */ c, id = 'pv') {
            handlers = {};
            doc = document.implementation.createHTMLDocument('pip');
            const pip = {
                document: doc,
                addEventListener(/** @type {string} */ t, /** @type {Function} */ fn) {
                    handlers[t] = fn;
                },
                close() {
                    handlers.pagehide?.();
                },
            };
            /** @type {any} */ (window).documentPictureInPicture = {
                requestWindow: vi.fn().mockResolvedValue(pip),
            };
            await c.mod.requestPip(id);
            return pip;
        }
        const azr = (/** @type {string} */ v, /** @type {number} */ i) =>
            `<input az="${i}" type="radio" name="g" value="${v}" az-bound="checked" az-change='[0,"main"]'>`;
        const inline = `<div id="page" az-view>${azr('x', 0)}${azr('y', 1)}<div id="pv" az-view>${radios('pip')}</div></div>`;

        it('re-choosing after a server push without a capture is an act', async () => {
            const c = await start(inline);
            await popOut(c);
            pick(document.querySelector('[value=x]'));
            c.msg([[SET_ATTR, 'page:0', 'checked', '']], null, c.lastRef());
            expect(/** @type {any} */ (document.querySelector('[value=x]')).checked).toBe(true);
            // the server moves the main group to y on its own (another user's change)
            c.msg([
                [REM_ATTR, 'page:0', 'checked'],
                [SET_ATTR, 'page:1', 'checked', ''],
            ]);
            expect(/** @type {any} */ (document.querySelector('[value=y]')).checked).toBe(true);
            pick(doc.querySelector('[value=y]'));
            // the user re-chooses x in main, last
            pick(document.querySelector('[value=x]'));
            resyncPage(c, inline);
            expect(group().filter(([, , on]) => on)).toEqual([['page', 'x', true]]);
        });
        it('re-choosing after a set_attr effect moved the group is an act', async () => {
            const c = await start(inline);
            await popOut(c);
            pick(document.querySelector('[value=y]'));
            c.msg([[SET_ATTR, 'page:1', 'checked', '']], null, c.lastRef());
            pick(doc.querySelector('[value=y]')); // unanswered at the drop
            expect(/** @type {any} */ (document.querySelector('[value=y]')).checked).toBe(true);
            // a server handler effect moves the main group to x
            c.msg(null, [[7, '#page [value=x]', 'checked', '']]);
            expect(/** @type {any} */ (document.querySelector('[value=x]')).checked).toBe(true);
            // the user re-chooses y in main, last
            pick(document.querySelector('[value=y]'));
            resyncPage(c, inline);
            expect(group().filter(([, , on]) => on)).toEqual([['page', 'y', true]]);
        });
        it('re-choosing after an OP_TEXT re-render moved the group is an act', async () => {
            const slot = (/** @type {string} */ inner) =>
                `<span az="1"><!--az:1-->${inner}<!--/az--></span>`;
            const r = (/** @type {string} */ v, on = false) =>
                `<input type="radio" name="g" value="${v}" az-bound="checked"${on ? ' checked' : ''} az-change='[0,"main"]'>`;
            const pg = `<div id="page" az-view><input az="0" type="radio" name="g" value="x" az-bound="checked" az-change='[0,"main"]'>${slot(r('y'))}<div id="pv" az-view>${radios('pip')}</div></div>`;
            const c = await start(pg);
            await popOut(c);
            pick(document.querySelector('[value=x]'));
            c.msg([[SET_ATTR, 'page:0', 'checked', '']], null, c.lastRef());
            pick(doc.querySelector('[value=y]')); // unanswered at the drop
            // the server re-renders the slot holding y, now checked
            c.msg([[TEXT, 'page:1', r('y', true), true]]);
            expect(/** @type {any} */ (document.querySelector('[value=y]')).checked).toBe(true);
            // jsdom does not uncheck the group when a checked radio becomes connected
            // (the HTML spec and browsers do): do what the browser does.
            /** @type {any} */ (document.querySelector('[value=x]')).checked = false;
            /** @type {any} */ (document.querySelector('[value=y]')).checked = true;
            // the user re-chooses x in main, last
            pick(document.querySelector('[value=x]'));
            resyncPage(c, pg);
            expect(group().filter(([, , on]) => on)).toEqual([['page', 'x', true]]);
        });
    });
    const form = (/** @type {string} */ ev, /** @type {string} */ inner) =>
        `<form id="f" az-change='[0,"${ev}"]'>${inner}</form>`;
    const named = (/** @type {string} */ ev) =>
        `<input name="title" value="" az-bound="value" az-change='[0,"${ev}"]'>`;
    const titles = () =>
        [...document.querySelectorAll('[name=title]')].map((e) => [
            /** @type {any} */ (e.closest('[az-view]')).id,
            /** @type {any} */ (e).value,
        ]);

    it('a bound form field of item-5 is not put into item-6 form of the same id', async () => {
        const c = await start(view('page', view('item-5', form('v5', named('v5')))));
        edit($('[name=title]'), 'title of 5');
        const before = c.sent().length;
        resyncPage(c, view('page', view('item-6', form('v6', named('v6')))));
        expect({ t: titles(), sent: replayed(c, before) }).toEqual({
            t: [['item-6', '']],
            sent: [],
        });
    });
    it('an unbound form field of item-5 is not put into item-6 form of the same id', async () => {
        const c = await start(
            view('page', view('item-5', form('v5', '<input name="title" value="">'))),
        );
        edit($('[name=title]'), 'title of 5');
        resyncPage(c, view('page', view('item-6', form('v6', '<input name="title" value="">'))));
        expect(titles()).toEqual([['item-6', '']]);
    });

    it('a restore replays the az-change the resync renders, not the dropped page one', async () => {
        const c = await start(view('page', form('validate', named('validate'))));
        edit($('[name=title]'), 'T');
        const before = c.sent().length;
        // the resync renders the form with another az-change (no longer validating)
        resyncPage(
            c,
            view('page', `<form id="f" az-change='[0,"other"]'>${named('other')}</form>`),
        );
        expect(replayed(c, before).map((f) => f[1])).not.toContain('validate');
    });
    it('a form="f" control in another view takes no state of view a', async () => {
        const pg = (/** @type {string} */ where) =>
            view(
                'page',
                view('a', form('a', where === 'a' ? named('a') : '')) +
                    view(
                        'b',
                        where === 'b'
                            ? `<input form="f" name="title" value="" az-bound="value">`
                            : '',
                    ),
            );
        const c = await start(pg('a'));
        edit($('[name=title]'), 'typed in a');
        resyncPage(c, pg('b'));
        expect(titles()).toEqual([['b', '']]);
    });

    // A form the resync renders again in its view replays its `az-change` whatever
    // controls it holds now, as one it renders with the same controls does.
    it('replays a form found again in its view that holds no control now', async () => {
        const c = await start(view('page', view('a', form('v', named('v')))));
        edit($('[name=title]'), 'T');
        const before = c.sent().length;
        resyncPage(c, view('page', view('a', form('v', ''))));
        expect(replayed(c, before)).toEqual([['a', 'v', {}]]);
    });
    it('replays a form that held no control at the drop', async () => {
        const c = await start(view('page', view('a', form('v', ''))));
        const before = c.sent().length;
        resyncPage(c, view('page', view('a', form('v', named('v')))));
        expect(replayed(c, before)).toEqual([['a', 'v', { title: '' }]]);
    });
    // A control naming a form of an id two views render (`form=`) names the first
    // in the document: a crash withholds the form that replayed, also when such a
    // control named another at the drop and names it again at the resync.
    it('withholds a crashed form a control naming its id reaches only after the drop', async () => {
        const slot = (/** @type {string} */ inner) =>
            `<span az="1"><!--az:1-->${inner}<!--/az--></span>`;
        const pg = `<input form="f" name="t" value="">${slot('')}${view('b', form('vb', named('vb')))}`;
        const c = await start(view('page', pg));
        edit($('[name=title]'), 'B');
        let before = c.sent().length;
        resyncPage(c, view('page', pg));
        expect(replayed(c, before).map((f) => f[1])).toEqual(['vb']);
        // the server renders another view with a form of that id before b
        c.msg([[TEXT, 'page:1', view('a', form('va', '')), true]]);
        edit($('[name=t]'), 'T');
        before = c.sent().length;
        resyncPage(c, view('page', pg), 4500); // before the server acknowledged vb
        expect(replayed(c, before)).toEqual([]);
    });

    // The server unchecking an unbound radio of a server-owned group moves what the
    // group shows, so the user choosing it again is an act, and an edit.
    it('re-choosing an unbound radio the server unchecked in an owned group is an edit', async () => {
        const g = (/** @type {boolean} */ on) =>
            view(
                'page',
                form(
                    'v',
                    `<input az="1" type="radio" name="g" value="x"${on ? ' checked' : ''}>` +
                        `<input type="radio" name="g" value="y" az-bound="checked">`,
                ),
            );
        const c = await start(g(false));
        const x0 = /** @type {any} */ ($('[value=x]'));
        x0.click();
        // the server takes x: the reconcile looks at the group, x
        c.msg([[SET_ATTR, 'page:1', 'checked', '']], null, c.lastRef());
        c.msg(null, [[8, '[value=x]', 'checked']]);
        expect(/** @type {any} */ ($('[value=x]')).checked).toBe(false);
        const x = /** @type {any} */ ($('[value=x]'));
        x.click();
        const before = c.sent().length;
        resyncPage(c, g(false));
        expect({
            x: /** @type {any} */ ($('[value=x]')).checked,
            sent: replayed(c, before),
        }).toEqual({ x: true, sent: [['page', 'v', { g: 'x' }]] });
    });

    // A crash withholds the form that replayed, every control of it: one naming it
    // from another view (`form=`), which the replayed restore had not held, too.
    it('withholds a form replayed before a crash, a control naming it from elsewhere too', async () => {
        const a = view('a', form('v', named('v')));
        const c = await start(view('page', a));
        edit($('[name=title]'), 'T');
        const note = `<input form="f" name="note" value="" az-bound="value">`;
        const pg = view('page', note + a);
        let before = c.sent().length;
        resyncPage(c, pg);
        expect(replayed(c, before).map((f) => f[1])).toEqual(['v']);
        edit($('[name=note]'), 'N');
        before = c.sent().length;
        resyncPage(c, pg, 4500); // before the server acknowledged the replay
        expect(replayed(c, before)).toEqual([]);
    });

    it('two views each rendering form id f: an edit in i2 stays in i2', async () => {
        const items = view(
            'page',
            view('i1', form('i1', named('i1'))) + view('i2', form('i2', named('i2'))),
        );
        const c = await start(items);
        edit(document.querySelectorAll('[name=title]')[1], 'typed in i2');
        const before = c.sent().length;
        resyncPage(c, items);
        expect({ t: titles(), sent: replayed(c, before) }).toEqual({
            t: [
                ['i1', ''],
                ['i2', 'typed in i2'],
            ],
            // Each view's form replays its own `az-change`: i1's the server's value, i2's
            // the user's edit, each to its own view.
            sent: [
                ['i1', 'i1', { title: '' }],
                ['i2', 'i2', { title: 'typed in i2' }],
            ],
        });
    });
    it('a page field the resync moves into a new child view takes nothing', async () => {
        const c = await start(view('page', field('page')));
        edit($('#q'), 'typed in page');
        const before = c.sent().length;
        resyncPage(c, view('page', view('ch', field('ch'))));
        expect({ qs: qs(), sent: replayed(c, before) }).toEqual({ qs: [['ch', '']], sent: [] });
    });
});

describe('a reload or back/forward', () => {
    const PAGE =
        `<input id="out" name="o" value="srv" az-bound="value">` +
        `<div id="v" az-view><input id="in" name="q" value="srv" az-bound="value">` +
        `<input id="cb" type="checkbox" name="c" az-bound="checked">` +
        `<div az-nodiff><input id="nd" name="n" value="srv" az-bound="value">` +
        `<div az-view id="cv"><input id="cin" name="c" value="srv" az-bound="value"></div></div></div>`;

    /** @param {string} kind */
    function restored(kind) {
        vi.spyOn(performance, 'getEntriesByType').mockReturnValue(
            /** @type {any} */ ([{ type: kind }]),
        );
        return start(PAGE, {
            open: false,
            before: () => {
                for (const id of ['out', 'in', 'nd', 'cin']) $(`#${id}`).value = 'old';
                $('#cb').checked = true;
            },
        });
    }

    it('drops the values the browser restored into live bound controls', async () => {
        await restored('reload');
        expect($('#in').value).toBe('srv');
        expect($('#cb').checked).toBe(false);
        expect($('#out').value).toBe('old');
        expect($('#nd').value).toBe('old');
        expect($('#cin').value).toBe('srv');
    });

    it('drops them on back/forward too', async () => {
        await restored('back_forward');
        expect($('#in').value).toBe('srv');
    });

    it('leaves a fresh navigation alone', async () => {
        await restored('navigate');
        expect($('#in').value).toBe('old');
    });

    // Chromium refills controls on a back/forward only after `load`, just before
    // `pageshow`, so connect() is too early to see what it restored.
    describe('restored after connect, before pageshow', () => {
        /** @param {string} kind */
        async function late(kind) {
            vi.spyOn(document, 'readyState', 'get').mockReturnValue('interactive');
            return restored(kind);
        }

        const pageshow = () => window.dispatchEvent(new Event('pageshow'));

        it('drops them at pageshow', async () => {
            await late('back_forward');
            $('#in').value = 'old';
            pageshow();
            expect($('#in').value).toBe('srv');
        });

        it('keeps a value a proposal still waits on', async () => {
            await late('back_forward');
            const c = conn;
            $('#in').setAttribute('az-input', '[0,"edit"]');
            c.open();
            c.msg(null, null);
            c.frame([0, null, null, false, ['az-input'], null]);
            type($('#in'), 'typed');
            expect(c.lastRef()).toEqual(expect.any(Number));
            pageshow();
            expect($('#in').value).toBe('typed');
        });

        it('leaves a fresh navigation alone', async () => {
            await late('navigate');
            $('#in').value = 'old';
            pageshow();
            expect($('#in').value).toBe('old');
        });

        // Only a back/forward restores after `load`: on a reload, pageshow would
        // reset what the user typed while the page loaded.
        it('leaves a reload alone at pageshow', async () => {
            await late('reload');
            type($('#in'), 'typed');
            pageshow();
            expect($('#in').value).toBe('typed');
        });

        it('keeps what the user typed since connect', async () => {
            await late('back_forward');
            type($('#in'), 'typed');
            pageshow();
            expect($('#in').value).toBe('typed');
        });

        it('keeps a radio group the user picked in since connect', async () => {
            vi.spyOn(document, 'readyState', 'get').mockReturnValue('interactive');
            vi.spyOn(performance, 'getEntriesByType').mockReturnValue(
                /** @type {any} */ ([{ type: 'back_forward' }]),
            );
            await start(
                `<div id="v" az-view>` +
                    `<input type="radio" name="r" value="a" checked az-bound="checked">` +
                    `<input type="radio" name="r" value="b" az-bound="checked"></div>`,
                { open: false },
            );
            $('[value=b]').click();
            pageshow();
            expect($('[value=b]').checked).toBe(true);
        });
    });

    // The navigation type stays `reload` for the document's lifetime, but only the
    // first connect() sees what the browser restored; a later one would reset typing.
    it('runs once per document, not on a later connect()', async () => {
        const c = await restored('reload');
        c.disconnect();
        $('#in').value = 'typed';
        const again = c.mod.connect('/ws');
        expect($('#in').value).toBe('typed');
        again();
    });

    it('leaves an autofilled control alone', async () => {
        vi.spyOn(performance, 'getEntriesByType').mockReturnValue(
            /** @type {any} */ ([{ type: 'reload' }]),
        );
        await start(PAGE, {
            open: false,
            before: () => {
                const el = $('#in');
                el.value = 'auto';
                const m = el.matches.bind(el);
                el.matches = (/** @type {string} */ s) => s === ':autofill' || m(s);
            },
        });
        expect($('#in').value).toBe('auto');
    });
});

describe('a form across views and documents', () => {
    afterEach(() => {
        /** @type {any} */ (window).documentPictureInPicture = undefined;
    });
    const replayed = (/** @type {any} */ c, /** @type {number} */ before) =>
        c
            .sent()
            .slice(before)
            .map((/** @type {any[]} */ f) => f.slice(0, 3));
    const resyncPage = (/** @type {any} */ c, /** @type {string} */ html, code = 1006) => {
        c.close(code);
        c.open(true);
        c.msg([[8, 'page', html]], null, null, true);
    };
    const view = (/** @type {string} */ id, /** @type {string} */ body) =>
        `<div id="${id}" az-view>${body}</div>`;
    const edit = (/** @type {any} */ el, /** @type {string} */ v) => {
        el.value = v;
        change(el);
    };
    /** Close the window the last `popOut` opened, bringing its view back. */
    let closePip = () => {};
    /** Pop view `id` out to a second document, as `requestPip` does. */
    async function popOut(/** @type {any} */ c, /** @type {string} */ id) {
        const doc = document.implementation.createHTMLDocument('pip');
        /** @type {Record<string, Function>} */
        const handlers = {};
        const pip = {
            document: doc,
            addEventListener(/** @type {string} */ t, /** @type {Function} */ fn) {
                handlers[t] = fn;
            },
            close() {
                handlers.pagehide?.();
            },
        };
        /** @type {any} */ (window).documentPictureInPicture = {
            requestWindow: vi.fn().mockResolvedValue(pip),
        };
        await c.mod.requestPip(id);
        closePip = () => pip.close();
        return doc;
    }

    // A resync keeps a popped-out view in its window (see `arizona-pip.test.js`): the
    // restore puts each control's state back there.
    // A view the form renders is still the form's while popped out, where its
    // controls have no form owner: what the user typed there is restored into the
    // same view's controls of that form when the resync brings it back.
    it('restores what was typed in a popped-out view inside a form', async () => {
        const pg = view(
            'page',
            `<form id="f">${view('ch', '<input name="t" value=""><input name="b" value="" az-bound="value" az-change=\'[0,"b"]\'>')}</form>`,
        );
        const c = await start(pg);
        const doc = await popOut(c, 'ch');
        edit(doc.querySelector('[name=t]'), 'typed in pip');
        edit(doc.querySelector('[name=b]'), 'bound in pip');
        const before = c.sent().length;
        resyncPage(c, pg);
        expect({
            t: doc.querySelector('[name=t]').value,
            b: doc.querySelector('[name=b]').value,
            sent: replayed(c, before).map((f) => f[1]),
        }).toEqual({ t: 'typed in pip', b: 'bound in pip', sent: ['b'] });
    });
    // A control naming its form from the page is that form's though the form is now
    // in a window: the resync rendered it into a view kept popped out.
    it('restores a control naming its form a resync rendered into a popped-out view', async () => {
        const k = '<input type="checkbox" name="k" value="v" form="f1" checked>';
        const before = view('page', `${k}<form id="f1"></form>${view('ch', '')}`);
        const after = view('page', `${k}${view('ch', '<form id="f1"></form>')}`);
        const c = await start(before);
        await popOut(c, 'ch');
        const box = $('[name=k]');
        box.checked = false;
        change(box);
        resyncPage(c, after);
        expect($('[name=k]').checked).toBe(false);
    });
    // A form's replay posts the fields a window holds for it too: a server-owned field
    // with no event of its own, in a view the resync kept popped out, is proposed by
    // the form, and keeps the user's edit.
    // A form's fields in a window post as a native submit of the form inline would:
    // in tree order as if the view were back, an explicit empty value as "", and a
    // disabled option left out.
    it('posts the fields a window holds for a form as a native submit would', async () => {
        const pg = view(
            'page',
            `<form id="f" az-change='[0,"f"]'><input name="n" value="1">${view(
                'ch',
                '<input name="n" value="2"><input type="checkbox" name="c" value="" checked>' +
                    '<select name="s" multiple><option value="x" selected disabled>x</option>' +
                    '<option value="y" selected>y</option></select>',
            )}<input name="n" value="3"></form>`,
        );
        const c = await start(pg);
        await popOut(c, 'ch');
        const before = c.sent().length;
        change($('[name=n]'));
        expect(replayed(c, before)).toEqual([
            ['page', 'f', { n: ['1', '2', '3'], c: '', s: ['y'] }],
        ]);
    });
    // With a window open the form's own fields still go through the browser's own
    // FormData: an image submitter's coordinates, named or not, and what a `formdata`
    // listener adds are kept, and the window's fields join where their view sits.
    for (const named of [true, false])
        it(`keeps an image submitter${named ? '' : ' with no name'} with a window open`, async () => {
            const img = `<input type="image" ${named ? 'name="go" ' : ''}alt="go">`;
            const pg = view(
                'page',
                `<form id="f" az-submit='[0,"f"]'><input name="n" value="1">${view(
                    'ch',
                    '<input name="n" value="2">',
                )}${img}</form>`,
            );
            const c = await start(pg);
            await popOut(c, 'ch');
            const before = c.sent().length;
            const form = /** @type {any} */ ($('#f'));
            form.requestSubmit($('[type=image]'));
            const sent = replayed(c, before);
            expect(sent).toEqual([
                [
                    'page',
                    'f',
                    named
                        ? { n: ['1', '2'], 'go.x': '0', 'go.y': '0' }
                        : { n: ['1', '2'], x: '0', y: '0' },
                ],
            ]);
        });
    it("keeps a formdata listener's entry with a window open", async () => {
        const pg = view(
            'page',
            `<form id="f" az-change='[0,"f"]'><input name="n" value="1">${view(
                'ch',
                '<input name="n" value="2">',
            )}<input name="n" value="3"></form>`,
        );
        const c = await start(pg);
        // jsdom fires no `formdata` event: stand in for the browser adding the entry a
        // listener appends to the FormData of a form.
        const Native = globalThis.FormData;
        globalThis.FormData = /** @type {any} */ (
            class extends Native {
                constructor(/** @type {any} */ f, /** @type {any} */ s) {
                    super(f, s);
                    if (f) this.append('extra', 'yes');
                }
            }
        );
        try {
            await popOut(c, 'ch');
            const before = c.sent().length;
            change($('[name=n]'));
            expect(replayed(c, before)).toEqual([
                ['page', 'f', { n: ['1', '2', '3'], extra: 'yes' }],
            ]);
        } finally {
            globalThis.FormData = Native;
        }
    });
    // A disabled fieldset around the view's place disables its fields, as inline.
    it("leaves out a window's fields a disabled fieldset around its view disables", async () => {
        const pg = view(
            'page',
            `<form id="f" az-change='[0,"f"]'><input name="n" value="1"><fieldset disabled>${view(
                'ch',
                '<input name="d" value="2">',
            )}</fieldset></form>`,
        );
        const c = await start(pg);
        await popOut(c, 'ch');
        const before = c.sent().length;
        change($('[name=n]'));
        expect(replayed(c, before)).toEqual([['page', 'f', { n: '1' }]]);
    });
    /** The payload a change of the form's first field sends, `ch` popped out. */
    const sendsWith = async (/** @type {string} */ body) => {
        const c = await start(view('page', `<form id="f" az-change='[0,"f"]'>${body}</form>`));
        await popOut(c, 'ch');
        const before = c.sent().length;
        change($('#f [name]'));
        return replayed(c, before)[0]?.[2];
    };
    /**
     * Run `fn` with a `FormData` that builds a form's entries as a browser does where
     * jsdom does not: no `dirname` entry for a field other than a textarea or a
     * text-like input, the direction inherited from the nearest valid `dir` (ASCII
     * case-insensitive), and a hidden `_charset_` field submitting the charset. Every
     * one built from a form is kept in `built`.
     * @template T
     * @param {(built: FormData[]) => Promise<T>} fn
     */
    const asBrowser = async (fn) => {
        const Native = globalThis.FormData;
        /** @type {FormData[]} */
        const built = [];
        const texty = ['hidden', 'text', 'search', 'tel', 'url', 'email', 'password'];
        /** @param {Element} el */
        const dirOf = (el) => {
            for (let n = /** @type {Element|null} */ (el); n; n = n.parentElement) {
                const d = n.getAttribute('dir')?.toLowerCase();
                if (d === 'ltr' || d === 'rtl') return d;
            }
            return 'ltr';
        };
        globalThis.FormData = /** @type {any} */ (
            class extends Native {
                constructor(/** @type {any} */ f, /** @type {any} */ sub) {
                    super(f, sub);
                    if (!f) return;
                    const entries = [...this.entries()];
                    for (const [k] of entries) this.delete(k);
                    const fields = [...f.elements];
                    for (const [k, v] of entries) {
                        const owner = fields.find((e) => e.getAttribute('dirname') === k);
                        if (owner) {
                            if (
                                owner.localName === 'textarea' ||
                                (owner.localName === 'input' && texty.includes(owner.type))
                            )
                                this.append(k, dirOf(owner));
                            continue;
                        }
                        const charset = fields.some(
                            (e) => e.type === 'hidden' && e.name.toLowerCase() === k.toLowerCase(),
                        );
                        this.append(k, charset && k.toLowerCase() === '_charset_' ? 'UTF-8' : v);
                    }
                    built.push(this);
                }
            }
        );
        try {
            return await fn(built);
        } finally {
            globalThis.FormData = Native;
        }
    };
    // The browser places a window's fields: a stand-in for each sits at its view's
    // place for the build, so entries no field of the window's can be told from --
    // another field's `dirname`, a field in a `datalist` -- cannot shift them.
    for (const [what, own] of [
        [
            'a select with a dirname',
            '<select name="n" dirname="d"><option selected>1</option></select>',
        ],
        ['a radio with a dirname', '<input type="radio" name="n" value="1" dirname="d" checked>'],
        [
            'a field in a datalist',
            '<input name="n" value="1"><datalist><input name="n" value="x"></datalist>',
        ],
    ])
        it(`places a window's fields after ${what}`, async () => {
            const got = await asBrowser(() =>
                sendsWith(
                    `${own}${view('ch', '<input name="n" value="2">')}<input name="n" value="3">`,
                ),
            );
            expect(got).toEqual({ n: ['1', '2', '3'] });
        });
    it("orders a window's fields around an image submitter as inline", async () => {
        const c = await start(
            view(
                'page',
                `<form id="f" az-submit='[22,"/x",{"method":"post"}]'><input name="a" value="1">${view(
                    'ch',
                    '<input name="w" value="2">',
                )}<input type="image" name="go" alt="go"><input name="b" value="3"></form>`,
            ),
        );
        await popOut(c, 'ch');
        const orig = globalThis.fetch;
        const f = vi.fn().mockResolvedValue({
            ok: true,
            status: 200,
            text: () => Promise.resolve('{"e":[]}'),
        });
        /** @type {any} */ (globalThis).fetch = f;
        try {
            /** @type {any} */ ($('#f')).requestSubmit($('[type=image]'));
            await new Promise((r) => setTimeout(r, 0));
            expect(String(f.mock.calls[0]?.[1]?.body)).toBe('a=1&w=2&go.x=0&go.y=0&b=3');
        } finally {
            globalThis.fetch = orig;
        }
    });
    it("orders a formdata listener's entry after a window's fields", async () => {
        const Native = globalThis.FormData;
        // jsdom fires no `formdata` event: stand in for the listener's append.
        globalThis.FormData = /** @type {any} */ (
            class extends Native {
                constructor(/** @type {any} */ f, /** @type {any} */ s) {
                    super(f, s);
                    if (f) this.append('n', 'L');
                }
            }
        );
        try {
            const got = await sendsWith(
                `<input name="n" value="1">${view('ch', '<input name="n" value="2">')}`,
            );
            expect(got).toEqual({ n: ['1', '2', 'L'] });
        } finally {
            globalThis.FormData = Native;
        }
    });
    it("lets a formdata listener remove a window's field", async () => {
        const Native = globalThis.FormData;
        globalThis.FormData = /** @type {any} */ (
            class extends Native {
                constructor(/** @type {any} */ f, /** @type {any} */ s) {
                    super(f, s);
                    if (f) this.delete('secret');
                }
            }
        );
        try {
            const got = await sendsWith(
                `<input name="n" value="1">${view('ch', '<input name="secret" value="pw">')}`,
            );
            expect(got).toEqual({ n: '1' });
        } finally {
            globalThis.FormData = Native;
        }
    });
    // A window field's `dirname` reports the direction it has inline: its own
    // ancestors in the window first (a valid `dir`, any case), else the page's.
    for (const [what, page, win, dir] of [
        ['the page', 'rtl', '', 'rtl'],
        ['its window, any case', 'ltr', 'RTL', 'rtl'],
        ['the page past an invalid dir', 'rtl', 'sideways', 'rtl'],
    ])
        it(`gives a window field's dirname the direction from ${what}`, async () => {
            document.documentElement.setAttribute('dir', page);
            try {
                const got = await asBrowser(() =>
                    sendsWith(
                        `<input name="n" value="1">${view(
                            'ch',
                            `<p${win ? ` dir="${win}"` : ''}><input name="t" value="x" dirname="t.dir"></p>`,
                        )}`,
                    ),
                );
                expect(got).toEqual({ n: '1', t: 'x', 't.dir': dir });
            } finally {
                document.documentElement.removeAttribute('dir');
            }
        });
    it("keeps a window's fields its view's place in a disabled fieldset's first legend", async () => {
        const got = await sendsWith(
            `<input name="n" value="1"><fieldset disabled><legend>${view(
                'ch',
                '<input name="n" value="2">',
            )}</legend></fieldset>`,
        );
        expect(got).toEqual({ n: ['1', '2'] });
    });
    // A submit bubbling from a form nested in the one the command reads (built by
    // script) has a submitter that form does not own: the browser's FormData would
    // throw for it, so the form's fields go without it.
    it('reads a form without a submitter it does not own', async () => {
        const c = await start(
            view('page', `<form id="f" az-submit='[0,"f"]'><input name="n" value="1"></form>`),
        );
        const inner = document.createElement('form');
        inner.innerHTML = '<button name="b" value="v">go</button>';
        $('#f').append(inner);
        const before = c.sent().length;
        inner.requestSubmit(inner.querySelector('button'));
        expect(replayed(c, before)).toEqual([['page', 'f', { n: '1' }]]);
    });
    // A window's fields build as the browser builds them inline: a hard-wrapped
    // textarea wrapped, an empty file input an empty `application/octet-stream`
    // file, a hidden `_charset_` the charset.
    it("builds a window's fields as the browser builds them", async () => {
        const built = await asBrowser(async (b) => {
            await sendsWith(
                `<input name="n" value="1">${view(
                    'ch',
                    '<textarea name="ta" wrap="hard" cols="3">abcdefg</textarea>' +
                        '<input type="file" name="up"><input type="hidden" name="_charset_">',
                )}`,
            );
            return b;
        });
        const entries = [...(built.at(-1)?.entries() ?? [])].map(([k, v]) =>
            typeof v === 'string' ? [k, v] : [k, v.name, v.type, v.size],
        );
        expect(entries).toEqual([
            ['n', '1'],
            ['ta', 'abc\ndef\ng'],
            ['up', '', 'application/octet-stream', 0],
            ['_charset_', 'UTF-8'],
        ]);
    });
    // What a window's field would not submit inline it does not submit here either --
    // disabled, in a datalist, an unchecked box, a button (no submitter outside its
    // form's document) -- and a field's `dirname` counts only on a text-like field.
    it("leaves out what a window's fields would not submit inline", async () => {
        const got = await asBrowser(() =>
            sendsWith(
                `<input name="n" value="1">${view(
                    'ch',
                    '<input name="n" value="2" disabled><datalist><input name="n" value="x"></datalist>' +
                        '<input type="checkbox" name="c" value="off"><button name="b" value="v">go</button>' +
                        '<input type="submit" name="s" value="v">' +
                        '<input type="number" name="num" value="5" dirname="num.dir">' +
                        '<input name="_charset_" value="typed">',
                )}`,
            ),
        );
        expect(got).toEqual({ n: '1', num: '5', _charset_: 'typed' });
    });
    it("leaves no stand-in behind once a window's fields are read", async () => {
        const c = await start(
            view(
                'page',
                `<form id="f" az-change='[0,"f"]'><input name="n" value="1">${view(
                    'ch',
                    '<input name="n" value="2">',
                )}</form>`,
            ),
        );
        await popOut(c, 'ch');
        const html = $('#f').outerHTML;
        change($('[name=n]'));
        expect($('#f').outerHTML).toBe(html);
    });
    it("counts no window field a disabled fieldset around its view disables toward a name's arity", async () => {
        const got = await sendsWith(
            `<input type="checkbox" name="k" value="a" checked><fieldset disabled>${view(
                'ch',
                '<input type="checkbox" name="k" value="b" checked>',
            )}</fieldset>`,
        );
        expect(got).toEqual({ k: 'a' });
    });
    // Several fields after a window's view keep their order: each goes after the one
    // before it, from the page or from a sibling window.
    for (const sibling of [false, true])
        it(`orders several fields after a window holding their form${sibling ? ', one in a sibling window' : ''}`, async () => {
            const a = '<input name="n" value="A" form="f">';
            const c = await start(
                view(
                    'page',
                    `${view(
                        'ch',
                        `<form id="f" az-change='[0,"f"]'><input name="n" value="1"></form>`,
                    )}${sibling ? view('sib', a) : a}<input name="n" value="B" form="f"><input name="n" value="C" form="f">`,
                ),
            );
            const doc = await popOut(c, 'ch');
            if (sibling) await popOut(c, 'sib');
            const before = c.sent().length;
            change(doc.querySelector('#f [name=n]'));
            expect(replayed(c, before)).toEqual([['ch', 'f', { n: ['1', 'A', 'B', 'C'] }]]);
        });
    // A throw while placing the stand-ins leaves none of them behind.
    it('leaves no stand-in behind when placing them throws', async () => {
        const c = await start(
            view(
                'page',
                `<form id="f" az-change='[0,"f"]'><input name="n" value="1">${view(
                    'ch',
                    '<input name="n" value="2"><input type="file" name="up">',
                )}</form>`,
            ),
        );
        const doc = await popOut(c, 'ch');
        Object.defineProperty(doc.querySelector('[name=up]'), 'files', {
            get: () => ['not a FileList'],
        });
        const html = $('#f').outerHTML;
        const log = vi.spyOn(console, 'warn').mockImplementation(() => {});
        try {
            change($('[name=n]'));
            expect({ thrown: log.mock.calls.length > 0, html: $('#f').outerHTML }).toEqual({
                thrown: true,
                html,
            });
        } finally {
            log.mockRestore();
        }
    });
    it('orders the fields of a window inside a window as inline', async () => {
        const c = await start(
            view(
                'page',
                `<form id="f" az-change='[0,"f"]'><input name="n" value="1">${view(
                    'ch',
                    `<input name="n" value="2">${view('gc', '<input name="n" value="3">')}<input name="n" value="4">`,
                )}<input name="n" value="5"></form>`,
            ),
        );
        // Popped out first, its placeholder then goes with `ch` into that window.
        await popOut(c, 'gc');
        const doc = await popOut(c, 'ch');
        expect(doc.querySelector('#ch')?.innerHTML).toContain('az-pip:gc');
        const before = c.sent().length;
        change($('[name=n]'));
        expect(replayed(c, before)).toEqual([['page', 'f', { n: ['1', '2', '3', '4', '5'] }]]);
    });
    // A form a window holds takes a page field naming it where the field sits from the
    // window's view inline.
    for (const after of [false, true])
        it(`orders a page field naming a form a window holds ${after ? 'after' : 'before'} its fields`, async () => {
            const k = '<input name="n" value="0" form="f">';
            const c = await start(
                view(
                    'page',
                    `${after ? '' : k}${view(
                        'ch',
                        `<form id="f" az-change='[0,"f"]'><input name="n" value="1"></form>`,
                    )}${after ? k : ''}`,
                ),
            );
            const doc = await popOut(c, 'ch');
            const before = c.sent().length;
            change(doc.querySelector('#f [name=n]'));
            expect(replayed(c, before)).toEqual([
                ['ch', 'f', { n: after ? ['1', '0'] : ['0', '1'] }],
            ]);
        });
    it('replays a form with the fields of a view the resync kept popped out', async () => {
        const pg = view(
            'page',
            `<form id="f" az-change='[0,"f"]'>${view('ch', '<input name="b" value="" az-bound="value">')}</form>`,
        );
        const c = await start(pg);
        const doc = await popOut(c, 'ch');
        edit(doc.querySelector('[name=b]'), 'bound in pip');
        const before = c.sent().length;
        resyncPage(c, pg);
        expect({
            b: doc.querySelector('[name=b]').value,
            sent: replayed(c, before),
        }).toEqual({ b: 'bound in pip', sent: [['page', 'f', { b: 'bound in pip' }]] });
    });
    it('restores into a popped-out view a control naming its form from outside it', async () => {
        const pg = view(
            'page',
            `<form id="f"></form>${view('ch', '<input form="f" name="t" value="">')}`,
        );
        const c = await start(pg);
        const doc = await popOut(c, 'ch');
        edit(doc.querySelector('[name=t]'), 'typed in pip');
        resyncPage(c, pg);
        expect(doc.querySelector('[name=t]').value).toBe('typed in pip');
    });

    // A control there that belongs to the form inline is no formless one: a formless
    // control of the view sharing its id keeps its own state, not the form's.
    it('saves no control of a form a popped-out view is in as formless', async () => {
        const pg = view(
            'page',
            `<form id="f" az-change='[0,"f"]'>${view(
                'ch',
                '<input id="q" name="a" value="" az-bound="value">' +
                    `<input id="q" form="none" value="" az-bound="value" az-change='[0,"q"]'>`,
            )}</form>`,
        );
        const c = await start(pg);
        const doc = await popOut(c, 'ch');
        const [a, q] = doc.querySelectorAll('#q');
        edit(a, 'of the form');
        edit(q, 'formless');
        resyncPage(c, pg);
        expect([...doc.querySelectorAll('#q')].map((e) => /** @type {any} */ (e).value)).toEqual([
            'of the form',
            'formless',
        ]);
    });

    // A pop-back merges two parts of a radio name into one group, which then shows
    // the choice the window held: code dispatching `change` on it after that is no
    // act, so the part the user acted on last still decides, with the choice that act
    // left it, which only the pop-back unchecked.
    it('counts no act on a choice a pop-back brought in', async () => {
        const r = (/** @type {string} */ v, on = false) =>
            `<input type="radio" name="g" value="${v}"${on ? ' checked' : ''} az-bound="checked" az-change='[0,"g"]'>`;
        const pg = view('page', r('x', true) + r('z') + view('pv', r('y')));
        const c = await start(pg);
        const doc = await popOut(c, 'pv');
        const pick = (/** @type {any} */ el) => {
            el.click();
        };
        pick(doc.querySelector('[value=y]'));
        pick($('[value=z]'));
        const y = doc.querySelector('[value=y]');
        closePip();
        // A browser keeps the checked radio it inserts and unchecks the rest.
        /** @type {any} */ (y).checked = true;
        expect([$('[value=x]').checked, $('[value=z]').checked]).toEqual([false, false]);
        change(y); // code's, on the group as it is
        resyncPage(c, pg);
        const on = [...document.querySelectorAll('[name=g]')]
            .filter((e) => /** @type {any} */ (e).checked)
            .map((e) => /** @type {any} */ (e).value);
        expect(on).toEqual(['z']);
    });

    // Code's `change` is untrusted: no act, whatever a pop-back and a server write
    // left the group showing.
    it('a server check after a pop-back, then code change on it, is no act', async () => {
        const r = (v, az, on = false) =>
            `<input az="${az}" type="radio" name="g" value="${v}"${on ? ' checked' : ''}>`;
        const pg = view('page', r('x', 0, true) + view('pv', r('y', 0)));
        const c = await start(pg);
        const bound = await import('./arizona-bound.js');
        const doc = await popOut(c, 'pv');
        const y = doc.querySelector('[value=y]');
        y.click(); // user act in pip
        closePip();
        y.checked = true; // browser keeps the moved checked radio
        // the server checks x now
        c.msg([[SET_ATTR, 'page:0', 'checked', '']]);
        const x = $('[value=x]');
        expect([x.checked, y.checked]).toEqual([true, false]);
        const before = bound.actedOn(x);
        change(x); // code's, on the choice the group shows
        expect(bound.actedOn(x)).toBe(before);
    });

    it('round trip end to end: the reconnect shows the choice the user made last', async () => {
        const r = (v) =>
            `<input type="radio" name="g" value="${v}" az-bound="checked" az-change='[0,"g"]'>`;
        const pg = view('page', r('b') + view('pv', r('a')));
        const c = await start(pg);
        const b = $('[value=b]');
        b.click();
        let doc = await popOut(c, 'pv');
        const a = doc.querySelector('[value=a]');
        a.click();
        closePip();
        a.checked = true;
        doc = await popOut(c, 'pv');
        b.click(); // the user's last act: b
        closePip();
        c.close(1006);
        c.open(true);
        c.msg([[8, 'page', pg]], null, null, true);
        const on = [...document.querySelectorAll('[name=g]')]
            .filter((e) => e.checked)
            .map((e) => e.value);
        expect(on).toEqual(['b']);
    });

    // Code dispatching `change` on the radio rendered checked is untrusted: no act.
    it('counts no act on code changing the radio rendered checked', async () => {
        const r = (/** @type {string} */ v, on = false) =>
            `<input type="radio" name="g" value="${v}"${on ? ' checked' : ''} az-bound="checked">`;
        await start(view('page', r('x', true) + r('y')));
        const bound = await import('./arizona-bound.js');
        const x = $('[value=x]');
        change(x);
        expect(bound.actedOn(x)).toBe(0);
    });

    // A radio pick stands until a later pick in its group, a later server statement
    // about its group (a `checked` write, a radio inserted checked) or a later reset of
    // its form; pops, renames, removals and moves do not end it.
    const settling = () => {
        const r = (/** @type {string} */ v, on = false) =>
            `<input az="${v}" type="radio" name="g" value="${v}"${on ? ' checked' : ''} az-bound="checked" az-change='[0,"${v}"]'>`;
        return {
            r,
            pg: (/** @type {string} */ on = '') =>
                view(
                    'page',
                    '<form id="f2"></form>' +
                        r('b', on === 'b') +
                        r('c', on === 'c') +
                        view('pv', r('a', on === 'a')),
                ),
        };
    };
    /** The user picks a in a popped-out pv, then b, last, whose pv's pop-back unchecks. */
    async function displace(/** @type {any} */ c) {
        const doc = await popOut(c, 'pv');
        /** @type {any} */ (doc.querySelector('[value=a]')).click();
        $('[value=b]').click();
        closePip();
        expect([$('[value=b]').checked, $('[value=a]').checked]).toEqual([false, true]);
    }
    const shown = () =>
        [...document.querySelectorAll('[name=g]')]
            .filter((e) => /** @type {any} */ (e).checked)
            .map((e) => /** @type {any} */ (e).value);
    it('restores the pick a pop-back unchecked, the one the user made last', async () => {
        const { pg } = settling();
        const c = await start(pg());
        await displace(c);
        resyncPage(c, pg());
        expect(shown()).toEqual(['b']);
    });
    it('ends a pick when the user picks again in its group', async () => {
        const { pg } = settling();
        const c = await start(pg());
        await displace(c);
        $('[value=c]').click();
        resyncPage(c, pg());
        expect(shown()).toEqual(['c']);
    });
    it('ends a pick when the user picks in its group after a pop-out', async () => {
        const { pg } = settling();
        const c = await start(pg());
        await displace(c);
        await popOut(c, 'pv'); // a leaves the page's document again
        $('[value=c]').click();
        resyncPage(c, pg());
        expect(shown()).toEqual(['c']);
    });
    // Each part's pick stands on its own: the page's last pick, c, shows when the
    // later pick of a's part has no radio left to restore into.
    it("restores a part's own pick when the later pick of another part cannot be", async () => {
        const r = (/** @type {string} */ v, on = false) =>
            `<input type="radio" name="g" value="${v}"${on ? ' checked' : ''} az-bound="checked" az-change='[0,"${v}"]'>`;
        const pg = (/** @type {string} */ a) => view('page', r('b') + r('c') + view('pv', r(a)));
        const c = await start(pg('a'));
        const doc = await popOut(c, 'pv');
        /** @type {any} */ (doc.querySelector('[value=a]')).click();
        $('[value=b]').click();
        closePip(); // a, moved in checked, unchecks b
        $('[value=c]').click();
        $('[value=a]').click(); // a again, last
        resyncPage(c, pg('a2')); // a's radio gone: its part cannot be restored
        expect(shown()).toEqual(['c']);
    });
    // A server batch leaving the group another checked radio states it, ending the
    // pick: its choice shows, none included. One that changes nothing the group shows
    // states nothing -- a check of the radio already checked (a toggle of a radio
    // checked with no `checked` attribute sets it), or a diff write held while the
    // pick's proposal waits -- and the pick stands.
    for (const [
        how,
        send,
        after,
        want,
    ] of /** @type {[string, (c: any) => void, string, string[]][]} */ ([
        [
            'an effect checking another radio',
            (c) => c.msg(null, [[7, '[value=c]', 'checked', '']]),
            'c',
            ['c'],
        ],
        [
            'an effect unchecking the radio that unchecked it',
            (c) => c.msg(null, [[8, '[value=a]', 'checked']]),
            '',
            [],
        ],
        [
            'an effect checking the radio that unchecked it',
            (c) => c.msg(null, [[7, '[value=a]', 'checked', '']]),
            'a',
            ['b'],
        ],
        [
            'a toggle of the radio that unchecked it',
            (c) => c.msg(null, [[21, '[value=a]', 'checked']]),
            '',
            ['b'],
        ],
        [
            'a held diff unchecking the radio that unchecked it',
            (c) => c.msg([[REM_ATTR, 'pv:a', 'checked']]),
            '',
            ['b'],
        ],
    ]))
        it(`${want.includes('b') ? 'keeps' : 'ends'} a pick on ${how}`, async () => {
            const { pg } = settling();
            const c = await start(pg());
            await displace(c);
            send(c);
            resyncPage(c, pg(after));
            expect(shown()).toEqual(want);
        });
    // The statement is the group's, not the radio's: removing the radio it wrote
    // takes nothing of it away.
    it('ends a pick on a server write to a radio the server then removes', async () => {
        const slot = (/** @type {string} */ d) => `<span az="s"><!--az:s-->${d}<!--/az--></span>`;
        const { r } = settling();
        const pg = (/** @type {string} */ d) => view('page', r('b') + r('c') + slot(d));
        const c = await start(pg(r('d')));
        $('[value=b]').click();
        c.msg(null, [[7, '[value=d]', 'checked', '']]);
        c.msg([[TEXT, 'page:s', '', true]]);
        expect(shown()).toEqual([]);
        resyncPage(c, pg(''));
        expect(shown()).toEqual([]);
    });
    it('ends a pick when a diff inserts a radio of its group checked', async () => {
        const slot = (/** @type {string} */ d) => `<span az="s"><!--az:s-->${d}<!--/az--></span>`;
        const { r } = settling();
        const pg = (/** @type {string} */ d) => view('page', r('b') + r('c') + slot(d));
        const c = await start(pg(''));
        $('[value=b]').click();
        c.msg([[TEXT, 'page:s', r('d', true), true]]);
        resyncPage(c, pg(r('d', true)));
        expect(shown()).toEqual(['d']);
    });
    // A server statement about a group reaches its radios in every document: the
    // pick made in a popped-out window ends on a write in the page.
    it('ends a pick in a popped-out window on a server write in the page', async () => {
        const { pg } = settling();
        const c = await start(pg());
        const doc = await popOut(c, 'pv');
        /** @type {any} */ (doc.querySelector('[value=a]')).click();
        c.msg(null, [[7, '#page [value=c]', 'checked', '']]);
        closePip();
        resyncPage(c, pg('c'));
        expect(shown()).toEqual(['c']);
    });
    // A radio joining the group with a later statement about its old group brings
    // none about this one: the pick stands.
    it('keeps a pick when a radio another group was written about joins its group', async () => {
        const { r } = settling();
        const h = (/** @type {string} */ v) =>
            `<input az="${v}" type="radio" name="h" value="${v}" az-bound="checked">`;
        const pg = view('page', r('b') + r('c') + h('y') + h('z'));
        const c = await start(pg);
        $('[value=b]').click();
        $('[value=z]').click(); // a pick in h, which the statement below ends
        c.msg(null, [[7, '[value=y]', 'checked', '']]); // the server states group h
        c.msg([[SET_ATTR, 'page:z', 'name', 'g']]); // z, unchecked, joins g
        expect(shown()).toEqual(['b']);
        resyncPage(c, pg);
        expect(shown()).toEqual(['b']);
    });
    // A statement about another group leaves the pick alone.
    it('keeps a pick on a server write to another group', async () => {
        const { r } = settling();
        const h = (/** @type {string} */ v) =>
            `<input az="${v}" type="radio" name="h" value="${v}" az-bound="checked">`;
        const pg = view('page', r('b') + r('c') + h('y'));
        const c = await start(pg);
        $('[value=b]').click();
        c.msg(null, [[7, '[value=y]', 'checked', '']]);
        resyncPage(c, pg);
        expect(shown()).toEqual(['b']);
    });
    // Removing the checked radio leaves the group none: a statement.
    it('ends a pick when a diff removes the radio that unchecked it', async () => {
        const { pg } = settling();
        const c = await start(pg());
        await displace(c);
        c.msg([[4, 'pv:a']]);
        expect(shown()).toEqual([]);
        resyncPage(c, pg());
        expect(shown()).toEqual([]);
    });
    // A pick is for the group it was made in: written out of it (a name, `form` or
    // type write) it counts for no group -- not restored into the one it is in, and
    // the group it left follows its other facts (here a's part: a) -- and written
    // back it counts again.
    for (const [attr, away] of [
        ['name', 'h'],
        ['form', 'f2'],
        ['type', 'checkbox'],
    ])
        it(`restores no pick a diff wrote the ${attr} of out of its group`, async () => {
            const { r, pg } = settling();
            const c = await start(pg());
            await displace(c);
            c.msg([[SET_ATTR, 'page:b', attr, away]]);
            const b =
                `<input az="b" type="${attr === 'type' ? away : 'radio'}" name="${attr === 'name' ? away : 'g'}"` +
                `${attr === 'form' ? ` form="${away}"` : ''} value="b" az-bound="checked" az-change='[0,"b"]'>`;
            resyncPage(c, view('page', '<form id="f2"></form>' + b + r('c') + view('pv', r('a'))));
            expect([shown().filter((v) => v !== 'b'), $('[value=b]').checked]).toEqual([
                ['a'],
                false,
            ]);
        });
    // A radio naming a form a pop-out moved into a window is that form's, as it is
    // inline: the pick made meanwhile is for that group, and stands back inline.
    it('keeps a pick made while the form its radio names was popped out', async () => {
        const rf = (/** @type {string} */ v, form = '') =>
            `<input type="radio" name="g" value="${v}"${form} az-bound="checked" az-change='[0,"${v}"]'>`;
        const pg = view(
            'page',
            rf('y', ' form="f"') + view('pv', `<form id="f">${rf('x')}</form>`),
        );
        const c = await start(pg);
        await popOut(c, 'pv');
        $('[value=y]').click();
        closePip();
        resyncPage(c, pg);
        expect(shown()).toEqual(['y']);
    });
    for (const [attr, away, back] of [
        ['name', 'h', 'g'],
        ['form', 'f2', null],
        ['type', 'checkbox', 'radio'],
    ])
        it(`keeps a pick a diff wrote the ${attr} of away and back`, async () => {
            const { pg } = settling();
            const c = await start(pg());
            await displace(c);
            c.msg([[SET_ATTR, 'page:b', attr, away]]);
            c.msg([back === null ? [REM_ATTR, 'page:b', attr] : [SET_ATTR, 'page:b', attr, back]]);
            resyncPage(c, pg());
            expect(shown()).toEqual(['b']);
        });
    // A reset shows the form's defaults, ending every pick in it made before; one a
    // listener cancels changes nothing, so it ends nothing.
    for (const cancel of [false, true])
        it(`${cancel ? 'keeps' : 'ends'} a pick on a ${cancel ? 'canceled ' : ''}reset of its form after a pop-back`, async () => {
            const rf = (/** @type {string} */ v) =>
                `<input type="radio" name="g" value="${v}" az-bound="checked" az-change='[0,"${v}"]'>`;
            const pg = view(
                'page',
                `<form id="f">${rf('b') + rf('c') + view('pv', rf('a'))}</form>`,
            );
            const c = await start(pg);
            const doc = await popOut(c, 'pv');
            /** @type {any} */ (doc.querySelector('[value=a]')).click();
            $('[value=b]').click();
            closePip();
            if (cancel)
                $('#f').addEventListener('reset', (/** @type {Event} */ e) => e.preventDefault());
            $('#f').reset();
            resyncPage(c, pg);
            expect(shown()).toEqual(cancel ? ['b'] : []);
        });
    it('ends a pick on a reset of its form', async () => {
        const rf = (/** @type {string} */ v) =>
            `<input type="radio" name="g" value="${v}" az-bound="checked" az-change='[0,"${v}"]'>`;
        const pg = view('page', `<form id="f">${rf('b') + rf('c')}</form>`);
        const c = await start(pg);
        $('[value=b]').click();
        $('#f').reset();
        resyncPage(c, pg);
        expect(shown()).toEqual([]);
    });
    // The same for a form's radios a view inside it splits while popped out.
    // A checked radio the server moves into the group unchecks the pick: a statement
    // about the group, ending it. The moved radio's own pick, made in another group,
    // counts for none here, so the server's choice shows.
    for (const [how, html, move] of /** @type {[string, string, (c: any) => void][]} */ ([
        [
            'a name write',
            `<input az="b" type="radio" name="h" value="b" az-bound="checked">`,
            (c) => {
                $('[value=b]').click();
                c.msg([[SET_ATTR, 'page:b', 'name', 'g']]);
            },
        ],
        [
            'a type write',
            `<input az="b" type="checkbox" name="g" value="b" checked az-bound="checked">`,
            (c) => c.msg([[SET_ATTR, 'page:b', 'type', 'radio']]),
        ],
        [
            'a form= removal',
            `<input az="b" type="radio" name="g" value="b" form="f2" checked az-bound="checked">`,
            (c) => c.msg([[REM_ATTR, 'page:b', 'form']]),
        ],
        [
            'a form id write',
            `<input az="b" type="radio" name="g" value="b" form="f2" checked az-bound="checked">`,
            (c) => c.msg([[SET_ATTR, 'page:f', 'id', 'f3']]),
        ],
        [
            'a form removal',
            `<input az="b" type="radio" name="g" value="b" form="f2" checked az-bound="checked">`,
            (c) => c.msg([[TEXT, 'page:s', '', true]]),
        ],
    ]))
        it(`ends a pick when ${how} moves a checked radio into its group`, async () => {
            const { r } = settling();
            const pg = view(
                'page',
                `<span az="s"><!--az:s--><form az="f" id="f2"></form><!--/az--></span>` +
                    html +
                    r('c'),
            );
            const c = await start(pg);
            $('[value=c]').click();
            move(c);
            // A browser unchecks the rest of the group b joins checked, a form owner
            // change included (jsdom does not): do what the browser does.
            /** @type {any} */ ($('[value=c]')).checked = false;
            resyncPage(c, view('page', r('b') + r('c')));
            expect(shown()).toEqual([]);
        });
    // A checked pick written out of its group and back: the server changed the
    // group's checked radio twice, which ends it.
    it('ends a checked pick a diff wrote the name of away and back', async () => {
        const { r } = settling();
        const pg = view('page', r('b') + r('c'));
        const c = await start(pg);
        $('[value=b]').click();
        c.msg([[SET_ATTR, 'page:b', 'name', 'h']]);
        c.msg([[SET_ATTR, 'page:b', 'name', 'g']]);
        resyncPage(c, pg);
        expect(shown()).toEqual([]);
    });
    // An az-nodiff radio a re-render keeps was not inserted: no statement.
    it('keeps a pick when a re-render keeps an az-nodiff radio rendered checked', async () => {
        const { r } = settling();
        const slot = (/** @type {string} */ t) =>
            `<span az="s"><!--az:s-->${t}<div az-nodiff="W"><input type="radio" name="g" value="x" checked></div><!--/az--></span>`;
        const pg = (/** @type {string} */ t) => view('page', r('b') + r('c') + slot(t));
        const c = await start(pg('one'));
        $('[value=b]').click();
        const x = $('[value=x]');
        c.msg([
            [
                TEXT,
                'page:s',
                'two<div az-nodiff="W"><input type="radio" name="g" value="x" checked></div>',
                true,
            ],
        ]);
        expect($('[value=x]')).toBe(x);
        resyncPage(c, pg('two'));
        expect(shown()).toEqual(['b']);
    });
    // What a reconnect frame states ahead of the restore, the restore writes the
    // user's pick over: the pick it restored stands at the next drop.
    it('keeps a restored pick over what its reconnect frame stated', async () => {
        const { r } = settling();
        const pg = view('page', r('b') + r('c'));
        const c = await start(pg);
        $('[value=b]').click();
        c.close(1006);
        c.open(true);
        c.msg([[8, 'page', pg]], [[7, '[value=c]', 'checked', '']], null, true);
        expect(shown()).toEqual(['b']);
        resyncPage(c, pg);
        expect(shown()).toEqual(['b']);
    });
    // An unbound group's part holds what it shows: code moving it after the user's
    // pick is what is restored.
    // (Outside every form an unbound group is not saved at all.)
    for (const script of /** @type {const} */ (['c', 'none']))
        it(`restores what an unbound form group shows after code set ${script}`, async () => {
            const u = (/** @type {string} */ v) => `<input type="radio" name="g" value="${v}">`;
            const pg = view('page', `<form id="f">${u('b') + u('c')}</form>`);
            const c = await start(pg);
            $('[value=b]').click();
            if (script === 'c') $('[value=c]').checked = true;
            else $('[value=b]').checked = false;
            const live = shown();
            resyncPage(c, pg);
            expect(shown()).toEqual(live);
        });
    // A server batch reads the checked radios twice, before and after, only while a
    // pick may still stand: none before an act, none once a statement ended it; and
    // never every input -- here 300 items holding a form each, inserted, then
    // removed, then a reply whose release nests in it.
    for (const acted of [false, true])
        it(`reads the checked radios twice per server batch while a pick stands (acted: ${acted})`, async () => {
            const { r } = settling();
            const c = await start(view('page', r('b') + r('c') + '<ul az="l"></ul>'));
            if (acted) $('[value=c]').click();
            let reads = 0;
            let inputs = 0;
            const qsa = Document.prototype.querySelectorAll;
            const tag = Document.prototype.getElementsByTagName;
            Document.prototype.querySelectorAll = function (/** @type {string} */ sel) {
                if (sel === 'input:checked') reads++;
                return qsa.call(this, sel);
            };
            Document.prototype.getElementsByTagName = function (/** @type {string} */ t) {
                if (t === 'input') inputs++;
                return tag.call(this, t);
            };
            const N = 300;
            const counts = [];
            try {
                c.msg(
                    Array.from({ length: N }, (_, i) => [
                        5,
                        'page:l',
                        `k${i}`,
                        -1,
                        `<li az-key="k${i}"><form id="f${i}"></form></li>`,
                    ]),
                );
                counts.push(reads);
                c.msg(Array.from({ length: N }, (_, i) => [6, 'page:l', `k${i}`]));
                counts.push(reads);
                // A reply releasing the pick's proposal, its release nested in the
                // frame: refused, it reconciles c away, a statement ending the pick.
                c.msg(null, null, acted ? c.lastRef() : null);
                counts.push(reads);
                // No pick may stand now: a batch reads nothing.
                c.msg([[5, 'page:l', 'z', -1, '<li az-key="z"></li>']]);
                counts.push(reads);
            } finally {
                Document.prototype.querySelectorAll = qsa;
                Document.prototype.getElementsByTagName = tag;
            }
            expect({ counts, inputs }).toEqual({
                counts: acted ? [2, 4, 6, 6] : [0, 0, 0, 0],
                inputs: 0,
            });
        });
    // A server effect a view transition defers, and a fetch response's effect, are
    // the server's batches too.
    it('ends a pick on a server effect a view transition runs later', async () => {
        const { r } = settling();
        const pg = (/** @type {string} */ on) =>
            view('page', r('b', on === 'b') + r('c', on === 'c'));
        const c = await start(pg(''));
        /** @type {Function[]} */
        const later = [];
        /** @type {any} */ (document).startViewTransition = (/** @type {any} */ fn) => {
            later.push(typeof fn === 'function' ? fn : fn.update);
            return { ready: Promise.resolve(), finished: Promise.resolve() };
        };
        try {
            $('[value=c]').click();
            c.msg(null, [[20, {}, [[7, '[value=b]', 'checked', '']]]]);
            for (const f of later) f();
            resyncPage(c, pg('b'));
            expect(shown()).toEqual(['b']);
        } finally {
            delete (/** @type {any} */ (document).startViewTransition);
        }
    });
    for (const by of ['server', 'user'])
        it(`ends a pick on a fetch response effect (fetch sent by the ${by})`, async () => {
            const { r } = settling();
            const btn = `<button az-click='[22,"/x",{"method":"post","body":{}}]'>x</button>`;
            const pg = (/** @type {string} */ on) =>
                view('page', r('b', on === 'b') + r('c', on === 'c') + btn);
            const c = await start(pg(''));
            const body = JSON.stringify({ e: [[7, '[value=b]', 'checked', '']] });
            const orig = globalThis.fetch;
            /** @type {any} */ (globalThis).fetch = vi.fn().mockResolvedValue({
                ok: true,
                status: 200,
                text: () => Promise.resolve(body),
            });
            try {
                $('[value=c]').click();
                if (by === 'server') c.msg(null, [[22, '/x', { method: 'post', body: {} }]]);
                else $('button').click();
                await new Promise((res) => setTimeout(res, 0));
                await new Promise((res) => setTimeout(res, 0));
                expect(shown()).toEqual(['b']);
                resyncPage(c, pg('b'));
                expect(shown()).toEqual(['b']);
            } finally {
                globalThis.fetch = orig;
            }
        });
    // A user's event attribute setting `checked` is the page's own: neither an act
    // nor a statement, so the pick stands.
    it('keeps a pick over a set_attr an event attribute runs', async () => {
        const { r } = settling();
        const btn = `<button az-click='[7,"[value=c]","checked",""]'>x</button>`;
        const pg = view('page', r('b') + r('c') + btn);
        const c = await start(pg);
        $('[value=b]').click();
        $('button').click();
        expect(shown()).toEqual(['c']);
        resyncPage(c, pg);
        expect(shown()).toEqual(['b']);
    });
    // A form re-rendered under its id is the same group: its radios' pick stands.
    it('keeps a pick when the form its radios name is re-rendered under its id', async () => {
        const { r } = settling();
        const pg = view(
            'page',
            `<span az="s"><!--az:s--><form id="f"></form><!--/az--></span>` +
                r('b').replace('<input ', '<input form="f" ') +
                r('c').replace('<input ', '<input form="f" '),
        );
        const c = await start(pg);
        $('[value=c]').click();
        c.msg([[TEXT, 'page:s', '<form id="f" class="x"></form>', true]]);
        expect(shown()).toEqual(['c']);
        resyncPage(c, pg);
        expect(shown()).toEqual(['c']);
    });
    // The batch that leaves a group without its checked radio, the pick's radio away
    // in another group meanwhile, states it even when no radio stays in the group.
    for (const keep of [true, false])
        it(`ends an away pick whose group a batch left without its checked radio (a radio stays: ${keep})`, async () => {
            const { r } = settling();
            const pg = (/** @type {string} */ on) =>
                view('page', r('b', on === 'b') + r('c', on === 'c'));
            const c = await start(pg('c'));
            $('[value=b]').click();
            c.msg(null, null, c.lastRef()); // refused: c shows again
            expect(shown()).toEqual(['c']);
            $('[value=b]').click();
            c.msg(null, null, c.lastRef());
            const ops = [
                [SET_ATTR, 'page:b', 'name', 'h'],
                [REM_ATTR, 'page:c', 'checked'],
            ];
            if (!keep) ops.push([4, 'page:c']);
            c.msg(ops);
            c.msg([[SET_ATTR, 'page:b', 'name', 'g']]);
            resyncPage(c, keep ? pg('') : view('page', r('b')));
            expect(shown()).toEqual([]);
        });
    // A form leaving carries a kept az-nodiff checked radio into the pick's group:
    // the group's checked radio is another, a statement.
    it('ends a pick when a re-render carries a kept checked radio out of a form into its group', async () => {
        const { r } = settling();
        const nod = `<div az-nodiff="W"><input type="radio" name="g" value="x" checked></div>`;
        const slot = (/** @type {string} */ wrap) =>
            `<span az="s"><!--az:s--><${wrap}>${nod}</${wrap}><!--/az--></span>`;
        const pg = (/** @type {string} */ wrap) => view('page', r('b') + r('c') + slot(wrap));
        const c = await start(pg('form'));
        $('[value=c]').click();
        const x = $('[value=x]');
        c.msg([[TEXT, 'page:s', `<div>${nod}</div>`, true]]);
        expect($('[value=x]')).toBe(x);
        // A browser unchecks the rest of the group x joins (jsdom does not on a form
        // owner change): do what the browser does.
        /** @type {any} */ ($('[value=c]')).checked = false;
        resyncPage(c, pg('div'));
        expect(shown().filter((v) => v !== 'x')).toEqual([]);
    });
    for (const attr of ['checked', 'CHECKED'])
        it(`ends a pick when content inserts a radio of its group rendered ${attr}`, async () => {
            const { r } = settling();
            const pg = (/** @type {string} */ t) =>
                view('page', r('b') + r('c') + `<span az="s"><!--az:s-->${t}<!--/az--></span>`);
            const c = await start(pg(''));
            $('[value=c]').click();
            const ins = `<input type="radio" name="g" value="x" ${attr}>`;
            c.msg([[TEXT, 'page:s', ins, true]]);
            /** @type {any} */ ($('[value=c]')).checked = false; // as a browser does
            resyncPage(c, pg(ins));
            expect(shown()).toEqual(['x']);
        });
    // A radio that leaves the group in the batch that states it takes the stamp along:
    // written back unchecked, its pick does not stand.
    it('ends a pick whose radio left its group in the batch that stated it', async () => {
        const { pg } = settling();
        const c = await start(pg());
        await displace(c);
        c.msg([[SET_ATTR, 'page:b', 'name', 'h']], [[7, '[value=c]', 'checked', '']]);
        c.msg([[SET_ATTR, 'page:b', 'name', 'g']]);
        resyncPage(c, pg('c'));
        expect(shown()).toEqual(['c']);
    });
    // A held write applied when its proposal ends at a close is the server's too.
    it('ends a pick when a held write applies as its proposal ends', async () => {
        const { r } = settling();
        const pg = view('page', r('b') + r('c'));
        const c = await start(pg);
        $('[value=b]').click();
        c.msg([[SET_ATTR, 'page:c', 'checked', '']]); // held: b's proposal waits
        expect(shown()).toEqual(['b']);
        c.close(1006); // the proposal ends: the held write applies
        expect(shown()).toEqual(['c']);
        c.open(true);
        c.msg([[8, 'page', view('page', r('b') + r('c', true))]], null, null, true);
        expect(shown()).toEqual(['c']);
    });
    // A reconnect frame states nothing, a reset it runs included: the pick its
    // restore writes stands at the next drop.
    it('keeps a restored pick over a reset in its reconnect frame', async () => {
        const { r } = settling();
        const pg = view('page', `<form id="f">${r('b') + r('c')}</form>`);
        const c = await start(pg);
        $('[value=b]').click();
        c.close(1006);
        c.open(true);
        c.msg([[8, 'page', pg]], [[25, '#f']], null, true);
        expect(shown()).toEqual(['b']);
        resyncPage(c, pg);
        expect(shown()).toEqual(['b']);
    });
    // A statement is about a group, and a pick about the group it was made in: a
    // statement about the group its radio joined meanwhile does not end it, and back
    // in its own group it is restored.
    it('keeps a pick whose radio joined another group in the batch that stated that one', async () => {
        const { r } = settling();
        const h = `<input az="z" type="radio" name="h" value="z" az-bound="checked">`;
        const page = view(
            'page',
            '<form id="f2"></form>' + r('b') + r('c') + h + view('pv', r('a')),
        );
        const c = await start(page);
        await displace(c);
        c.msg([[SET_ATTR, 'page:b', 'name', 'h']], [[7, '[value=z]', 'checked', '']]);
        c.msg([[SET_ATTR, 'page:b', 'name', 'g']]);
        resyncPage(c, page);
        expect(shown()).toEqual(['b']);
    });
    // Two views each rendering a form of one id are two groups: a statement about one
    // leaves the pick in the other.
    it('keeps a pick when the server states the same-named group of another view', async () => {
        const { r } = settling();
        const pg = view(
            'page',
            `<form id="f">${r('b') + r('c')}</form>` +
                view(
                    'ch',
                    `<form id="f"><input az="y" type="radio" name="g" value="y" az-bound="checked"></form>`,
                ),
        );
        const c = await start(pg);
        $('[value=b]').click();
        c.msg(null, [[7, '[value=y]', 'checked', '']]);
        resyncPage(c, pg);
        expect(shown().filter((v) => v !== 'y')).toEqual(['b']);
    });
    // A reset the server runs ends the picks of its form even where it changes no
    // radio.
    it('ends a pick a pop-back unchecked on a reset the server runs', async () => {
        const rf = (/** @type {string} */ v) =>
            `<input type="radio" name="g" value="${v}" az-bound="checked" az-change='[0,"${v}"]'>`;
        const pg = view('page', `<form id="f">${rf('b') + rf('c') + view('pv', rf('a'))}</form>`);
        const c = await start(pg);
        const doc = await popOut(c, 'pv');
        /** @type {any} */ (doc.querySelector('[value=a]')).click();
        $('[value=b]').click();
        closePip();
        // Code unchecks a: the group shows its defaults, so the reset changes no radio.
        /** @type {any} */ ($('[value=a]')).checked = false;
        c.msg(null, [[25, '#f']]);
        resyncPage(c, pg);
        expect(shown()).toEqual([]);
    });
    // Without WeakRef (older browsers) a pick is held strongly: an act still notes it,
    // and a statement still ends it.
    it('ends a pick on a statement where the browser has no WeakRef', async () => {
        const { r } = settling();
        const pg = (/** @type {string} */ on) =>
            view('page', r('b', on === 'b') + r('c', on === 'c'));
        const WR = globalThis.WeakRef;
        /** @type {any} */ (globalThis).WeakRef = undefined;
        try {
            const c = await start(pg(''));
            $('[value=c]').click();
            c.msg(null, [[7, '[value=b]', 'checked', '']]);
            resyncPage(c, pg('b'));
            expect(shown()).toEqual(['b']);
        } finally {
            /** @type {any} */ (globalThis).WeakRef = WR;
        }
    });
    // A picked radio out of the page across a server batch (a widget moving it) is
    // still the pick's: a later statement ends it.
    it('ends a pick its radio was detached across a server batch for, on a later statement', async () => {
        const { r } = settling();
        const pg = (/** @type {string} */ on) =>
            view(
                'page',
                `<div id="box">${r('b', on === 'b') + r('c', on === 'c')}</div><ul az="l"></ul>`,
            );
        const c = await start(pg(''));
        $('[value=c]').click();
        const box = $('#box');
        const parent = box.parentNode;
        const next = box.nextSibling;
        box.remove();
        c.msg([[5, 'page:l', 'k', -1, '<li az-key="k"></li>']]);
        parent.insertBefore(box, next);
        c.msg(null, [[7, '[value=b]', 'checked', '']]);
        resyncPage(c, pg('b'));
        expect(shown()).toEqual(['b']);
    });
    // What a reconnect frame renders before the restore states nothing; what it
    // defers past the restore (a view transition's update) is a later statement. The
    // next drop restores what the screen showed, with view transitions or without.
    for (const vt of [false, true])
        it(`restores the pick a reconnect frame's deferred effect leaves standing (view transitions: ${vt})`, async () => {
            const { r } = settling();
            const pg = view('page', r('b') + r('c'));
            const c = await start(pg);
            /** @type {Function[]} */
            const later = [];
            if (vt)
                /** @type {any} */ (document).startViewTransition = (/** @type {any} */ fn) => {
                    later.push(typeof fn === 'function' ? fn : fn.update);
                    return { ready: Promise.resolve(), finished: Promise.resolve() };
                };
            try {
                $('[value=c]').click();
                c.close(1006);
                c.open(true);
                c.msg([[8, 'page', pg]], [[20, {}, [[7, '[value=b]', 'checked', '']]]], null, true);
                for (const f of later) f();
                const screen = shown();
                resyncPage(c, pg);
                // The pick stands where the screen shows it; the server's b ended it,
                // and the server's choice (none) shows.
                expect({ screen, next: shown() }).toEqual(
                    vt ? { screen: ['b'], next: [] } : { screen: ['c'], next: ['c'] },
                );
            } finally {
                delete (/** @type {any} */ (document).startViewTransition);
            }
        });
    // A restored pick no replay proposes shows the server's again: a statement, so
    // the next drop restores it no more, even where a replay could propose it then.
    it('ends a restored pick no replay proposed', async () => {
        const q = (/** @type {string} */ v, ev = '[1,"#x"]') =>
            `<input type="radio" name="g" value="${v}" az-bound="checked" az-change='${ev}'>`;
        const mute = view('page', q('b') + q('c') + '<p id="x"></p>');
        const loud = view('page', q('b', '[0,"b"]') + q('c', '[0,"c"]') + '<p id="x"></p>');
        const c = await start(loud);
        $('[value=b]').click();
        resyncPage(c, mute); // restored, but nothing proposes it: the server's shows
        expect(shown()).toEqual([]);
        resyncPage(c, loud);
        expect(shown()).toEqual([]);
    });
    // A reconnect lets go of the replaced page's picks, a browser without WeakRef
    // included: nothing keeps the old page alive.
    for (const noWR of [false, true])
        it(`keeps no replaced page alive across reconnects (no WeakRef: ${noWR})`, async () => {
            const v8 = await import('node:v8');
            const vm = await import('node:vm');
            v8.setFlagsFromString('--expose-gc');
            const gc = vm.runInNewContext('gc');
            const RealWR = globalThis.WeakRef;
            const { r } = settling();
            const pg = view('page', `<section>${'x'.repeat(1000)}</section>${r('b')}${r('c')}`);
            if (noWR) /** @type {any} */ (globalThis).WeakRef = undefined;
            /** @type {any} */
            let wr;
            try {
                const c = await start(pg);
                (() => {
                    const old = $('[value=c]');
                    old.click();
                    wr = new RealWR(old.parentNode);
                })();
                for (let i = 0; i < 3; i++) resyncPage(c, pg);
                c.msg([[8, 'page', view('page', '<p>other</p>')]]);
            } finally {
                /** @type {any} */ (globalThis).WeakRef = RealWR;
            }
            for (let i = 0; i < 5; i++) {
                await new Promise((res) => setTimeout(res, 0));
                gc();
            }
            expect(wr.deref()).toBeUndefined();
        });
    // A statement about another group does not end a pick, wherever its radio was
    // meanwhile -- moved out of the page and into that group's form, or moved there
    // in the page.
    for (const detach of [true, false])
        it(`keeps a pick over a statement about the group its radio visited (detached: ${detach})`, async () => {
            const { r } = settling();
            const pg = view(
                'page',
                `<form id="f1"><div id="box">${r('b') + r('c')}</div></form><form id="f9">${r('z')}</form><ul az="l"></ul>`,
            );
            const c = await start(pg);
            $('[value=c]').click();
            const cEl = $('[value=c]');
            if (detach) {
                cEl.remove();
                c.msg([[5, 'page:l', 'k', -1, '<li az-key="k"></li>']]);
            }
            /** @type {any} */ (document.getElementById('f9')).appendChild(cEl);
            c.msg(null, [[7, '[value=z]', 'checked', '']]); // the server states f9's g
            /** @type {any} */ (document.getElementById('box')).appendChild(cEl);
            resyncPage(c, pg);
            expect(
                [...document.querySelectorAll('#f1 [name=g]')]
                    .filter((e) => /** @type {any} */ (e).checked)
                    .map((e) => /** @type {any} */ (e).value),
            ).toEqual(['c']);
        });
    // A statement about a pick's group made while its radio is out of the page (a
    // widget holding it) still ends the pick.
    it('ends a pick on a statement made while its radio is out of the page', async () => {
        const { r } = settling();
        const pg = (/** @type {string} */ on) =>
            view('page', `<div id="box">${r('b', on === 'b') + r('c', on === 'c')}</div>`);
        const c = await start(pg(''));
        $('[value=c]').click();
        const cEl = $('[value=c]');
        const box = $('#box');
        cEl.remove();
        c.msg(null, [[7, '[value=b]', 'checked', '']]);
        box.appendChild(cEl);
        /** @type {any} */ (cEl).checked = false; // as the group showed when it left
        resyncPage(c, pg('b'));
        expect(shown()).toEqual(['b']);
    });
    // A reset, like a statement, is about the form a pick was made in: one of the form
    // its radio visits leaves it, one of its own form ends it, wherever the radio is.
    const inF1 = () =>
        [...document.querySelectorAll('#f1 [name=g]')]
            .filter((e) => /** @type {any} */ (e).checked)
            .map((e) => /** @type {any} */ (e).value);
    const twoForms = () => {
        const { r } = settling();
        return view(
            'page',
            `<form id="f1"><div id="box">${r('b') + r('c')}</div></form><form id="f9">${r('z')}</form>`,
        );
    };
    it('keeps a pick over a reset of the form its radio visits', async () => {
        const pg = twoForms();
        const c = await start(pg);
        $('[value=c]').click();
        const cEl = $('[value=c]');
        /** @type {any} */ (document.getElementById('f9')).appendChild(cEl);
        /** @type {any} */ (document.getElementById('f9')).reset();
        /** @type {any} */ (document.getElementById('box')).appendChild(cEl);
        resyncPage(c, pg);
        expect(inF1()).toEqual(['c']);
    });
    for (const where of ['f9', 'detached'])
        it(`ends a pick on a reset of its own form while its radio is ${where === 'f9' ? 'in another' : 'out of the page'}`, async () => {
            const pg = twoForms();
            const c = await start(pg);
            $('[value=c]').click();
            const cEl = $('[value=c]');
            if (where === 'f9') /** @type {any} */ (document.getElementById('f9')).appendChild(cEl);
            else cEl.remove();
            /** @type {any} */ (document.getElementById('f1')).reset();
            /** @type {any} */ (document.getElementById('box')).appendChild(cEl);
            /** @type {any} */ (cEl).checked = false;
            resyncPage(c, pg);
            expect(inF1()).toEqual([]);
        });
    it('ends a pick in a form when the server writes its group', async () => {
        const r = (/** @type {string} */ v, on = false) =>
            `<input type="radio" name="g" value="${v}"${on ? ' checked' : ''}>`;
        const pg = (/** @type {boolean} */ on) =>
            view('page', `<form id="f">${r('b') + r('c', on) + view('pv', r('a'))}</form>`);
        const c = await start(pg(false));
        const doc = await popOut(c, 'pv');
        /** @type {any} */ (doc.querySelector('[value=a]')).click();
        $('[value=b]').click();
        closePip();
        expect($('[value=b]').checked).toBe(false);
        c.msg(null, [[7, '[value=c]', 'checked', '']]); // a handler effect checks c
        resyncPage(c, pg(true));
        expect(shown()).toEqual(['c']);
    });

    // A part acted on last decides even when its view, still there, holds nothing of
    // the name now: that act returned it to the server's choice, which shows.
    it('lets a part acted on last decide when its view holds no control now', async () => {
        const r = (/** @type {string} */ v, on = false) =>
            `<input type="radio" name="g" value="${v}"${on ? ' checked' : ''} az-bound="checked" az-change='[0,"${v}"]'>`;
        const c = await start(view('page', r('x') + view('ch', r('y', true) + r('z'))));
        const pick = (/** @type {any} */ el) => {
            el.click();
        };
        const doc = await popOut(c, 'ch');
        pick($('[value=x]'));
        pick(doc.querySelector('[value=z]'));
        pick(doc.querySelector('[value=y]')); // back to the server's, last
        closePip(); // one document: a window's group is another group
        resyncPage(c, view('page', r('x') + view('ch', '')));
        expect(shown()).toEqual([]);
    });

    // The same for a form's group spanning views: the form's own view, still there,
    // holds no radio of the name now, and the user's last act there went back to the
    // server's choice, so the page's older pick of the form's group does not show.
    it("lets a form's part acted on last decide when its view holds no radio now", async () => {
        const r = (/** @type {string} */ v, on = false, form = '') =>
            `<input type="radio" name="g" value="${v}"${on ? ' checked' : ''}${form} az-bound="checked">`;
        const pg = (/** @type {boolean} */ y) =>
            view(
                'page',
                `<form id="fv" az-view az-change='[0,"f"]'>${y ? r('y', true) : ''}</form>${r('x', false, ' form="fv"')}`,
            );
        const c = await start(pg(true));
        $('[value=x]').click();
        $('[value=y]').click(); // back to the server's, last
        expect($('[value=x]').checked).toBe(false);
        resyncPage(c, pg(false));
        expect(shown()).toEqual([]);
    });

    // The same for a part outside the form's view naming it (`form=`): the page holds
    // none of its radios now, and its last act went back to the server's choice.
    it('lets a part naming a form from another view decide when it holds no radio now', async () => {
        const r = (/** @type {string} */ v, on = false, form = '') =>
            `<input type="radio" name="g" value="${v}"${on ? ' checked' : ''}${form} az-bound="checked">`;
        const pg = (/** @type {boolean} */ p) =>
            view(
                'page',
                `${p ? r('p', true, ' form="fv"') : ''}${view('ch', `<form id="fv" az-change='[0,"f"]'>${r('z')}</form>`)}`,
            );
        const c = await start(pg(true));
        $('[value=z]').click();
        $('[value=p]').click(); // back to the server's, last
        expect($('[value=z]').checked).toBe(false);
        resyncPage(c, pg(false));
        expect(shown()).toEqual([]);
    });

    // Only a choice the user made is kept when a pop-back unchecks it.
    it('keeps no server choice a pop-back unchecked', async () => {
        const r = (/** @type {string} */ v, on = false) =>
            `<input type="radio" name="g" value="${v}"${on ? ' checked' : ''} az-bound="checked" az-change='[0,"${v}"]'>`;
        const c = await start(view('page', r('x', true) + view('ch', r('y'))));
        const doc = await popOut(c, 'ch');
        const y = doc.querySelector('[value=y]');
        /** @type {any} */ (y).click();
        closePip();
        expect($('[value=x]').checked).toBe(false);
        // y is rendered in the page now, its view empty
        resyncPage(c, view('page', r('x') + r('y') + view('ch', '')));
        expect(shown()).toEqual([]);
    });

    // A resync renders fresh radios: the group shows the server's choice, which code
    // dispatching `change` on after it decides nothing.
    it('counts no act on the choice a resync rendered', async () => {
        const r = (/** @type {string} */ v, on = false) =>
            `<input type="radio" name="g" value="${v}"${on ? ' checked' : ''} az-bound="checked">`;
        const c = await start(view('page', r('x') + r('y')));
        const bound = await import('./arizona-bound.js');
        const y = $('[value=y]');
        y.click();
        resyncPage(c, view('page', r('x', true)));
        const x = $('[value=x]');
        expect(x.checked).toBe(true);
        change(x);
        expect(bound.actedOn(x)).toBe(0);
    });

    // An event on a form's radio looks its group up in that form alone, never in the
    // whole document: many forms sharing a radio name cost nothing more per event.
    it('looks a form radio group up in its form only', async () => {
        const n = 300;
        const forms = Array.from(
            { length: n },
            (_, i) =>
                `<form id="f${i}" az-change='[0,"v"]'><input type="radio" name="s" value="a" checked az-bound="checked">` +
                `<input type="radio" name="s" value="b" az-bound="checked"></form>`,
        ).join('');
        await start(`<div id="page" az-view>${forms}</div>`);
        const byName = vi.spyOn(Document.prototype, 'getElementsByName');
        const desc = /** @type {PropertyDescriptor} */ (
            Object.getOwnPropertyDescriptor(HTMLFormElement.prototype, 'elements')
        );
        let reads = 0;
        Object.defineProperty(HTMLFormElement.prototype, 'elements', {
            configurable: true,
            get() {
                reads++;
                return desc.get?.call(this);
            },
        });
        const events = 50;
        try {
            for (const b of [...document.querySelectorAll('[value=b]')].slice(0, events)) {
                /** @type {any} */ (b).click();
            }
        } finally {
            Object.defineProperty(HTMLFormElement.prototype, 'elements', desc);
        }
        const calls = byName.mock.calls.length;
        byName.mockRestore();
        expect({ calls, perEvent: reads / events <= 12 }).toEqual({ calls: 0, perEvent: true });
    });

    // A form that is its view's own element is a form of that view.
    it('replays a view-root form found again holding no control', async () => {
        const fv = (/** @type {string} */ inner) =>
            `<form id="a" az-view az-change='[0,"v"]'>${inner}</form>`;
        const c = await start(view('page', fv('<input name="t" value="" az-bound="value">')));
        edit($('[name=t]'), 'T');
        const before = c.sent().length;
        resyncPage(c, view('page', fv('')));
        expect(replayed(c, before)).toEqual([['a', 'v', {}]]);
    });

    // Content inserting checked radios looks nothing up: the radio inserted is what
    // its group shows now.
    it('reads no form controls for content inserting checked radios', async () => {
        const rows = Array.from(
            { length: 50 },
            (_, i) =>
                `<input type="radio" name="g${i}" value="a" checked><input type="RADIO" name="g${i}" value="b">`,
        ).join('');
        const c = await start(view('page', '<span az="1"><!--az:1--><!--/az--></span>'));
        const desc = /** @type {PropertyDescriptor} */ (
            Object.getOwnPropertyDescriptor(HTMLFormElement.prototype, 'elements')
        );
        let reads = 0;
        Object.defineProperty(HTMLFormElement.prototype, 'elements', {
            configurable: true,
            get() {
                reads++;
                return desc.get?.call(this);
            },
        });
        try {
            c.msg([[TEXT, 'page:1', `<form id="f">${rows}</form>`, true]]);
        } finally {
            Object.defineProperty(HTMLFormElement.prototype, 'elements', desc);
        }
        expect(reads).toBe(0);
    });

    // A form reset moves what a radio group shows with no write: the reconnect shows,
    // and proposes, what the reset left, unless the user chose their radio again.
    for (const again of [false, true])
        it(`after a form reset keeps a radio as the user's choice only if chosen again (${again})`, async () => {
            const f = view(
                'page',
                `<form id="f" az-change='[0,"v"]'>` +
                    `<input type="radio" name="g" value="x" checked az-bound="checked">` +
                    `<input type="radio" name="g" value="y" az-bound="checked"></form>`,
            );
            const c = await start(f);
            const y = $('[value=y]');
            y.click();
            c.msg(null, [[25, '#f']]);
            expect([$('[value=x]').checked, y.checked]).toEqual([true, false]);
            if (again) y.click();
            const before = c.sent().length;
            resyncPage(c, f);
            expect({
                y: $('[value=y]').checked,
                sent: replayed(c, before),
            }).toEqual(
                again
                    ? { y: true, sent: [['page', 'v', { g: 'y' }]] }
                    : { y: false, sent: [['page', 'v', { g: 'x' }]] },
            );
        });

    // Two parts of one form, a control naming it from another view among them, are
    // two failures when their own replays throw: each is warned.
    it('warns for the replay of each part of a form that throws', async () => {
        const bad = `az-change='[[1,"#["],[0,"x"]]'`;
        const pg = view(
            'page',
            `<input form="f" name="t" value="" az-bound="value" ${bad}>` +
                view('a', `<form id="f"><input name="t" value="" az-bound="value" ${bad}></form>`),
        );
        const warn = vi.spyOn(console, 'warn').mockImplementation(() => {});
        const c = await start(pg);
        for (const el of document.querySelectorAll('[name=t]')) edit(el, 'T');
        resyncPage(c, pg);
        const warns = warn.mock.calls.filter(([m]) => String(m).includes('replayed'));
        warn.mockRestore();
        expect(warns).toHaveLength(2);
    });

    // A radio written `type="RADIO"` is a radio: content inserting it checked moves
    // what its group shows, so the user choosing the other again is an act.
    const upper = (/** @type {boolean} */ on) =>
        `<input az="0" type="radio" name="g" value="x" az-bound="checked" az-change='[0,"g"]'>` +
        `<span az="1"><!--az:1--><input type="RADIO" name="g" value="y"${on ? ' checked' : ''} az-bound="checked" az-change='[0,"g"]'><!--/az--></span>`;
    it('notes content inserting a checked radio written in upper case', async () => {
        const c = await start(view('page', upper(false)));
        const x = $('[value=x]');
        x.click();
        c.msg([[SET_ATTR, 'page:0', 'checked', '']], null, c.lastRef());
        // the server re-renders y, now checked
        c.msg([[TEXT, 'page:1', upper(true).split('<!--az:1-->')[1].split('<!--/az-->')[0], true]]);
        expect([$('[value=x]').checked, $('[value=y]').checked]).toEqual([false, true]);
        // the user chooses x again: an act, kept over the server's y on a reconnect
        const x2 = $('[value=x]');
        x2.click();
        resyncPage(c, view('page', upper(true)));
        expect([$('[value=x]').checked, $('[value=y]').checked]).toEqual([true, false]);
    });
});

describe('a radio group across documents and writes', () => {
    afterEach(() => {
        /** @type {any} */ (window).documentPictureInPicture = undefined;
    });
    const view = (/** @type {string} */ id, /** @type {string} */ body) =>
        `<div id="${id}" az-view>${body}</div>`;
    const resyncPage = (/** @type {any} */ c, /** @type {string} */ html, code = 1006) => {
        c.close(code);
        c.open(true);
        c.msg([[8, 'page', html]], null, null, true);
    };
    let closePip = () => {};
    async function popOut(/** @type {any} */ c, /** @type {string} */ id) {
        const doc = document.implementation.createHTMLDocument('pip');
        /** @type {Record<string, Function>} */
        const handlers = {};
        const pip = {
            document: doc,
            addEventListener(/** @type {string} */ t, /** @type {Function} */ fn) {
                handlers[t] = fn;
            },
            close() {
                handlers.pagehide?.();
            },
        };
        /** @type {any} */ (window).documentPictureInPicture = {
            requestWindow: vi.fn().mockResolvedValue(pip),
        };
        await c.mod.requestPip(id);
        closePip = () => pip.close();
        return doc;
    }
    const pick = (/** @type {any} */ el) => {
        el.click();
    };
    const shown = (/** @type {Document} */ d = document) =>
        [...d.querySelectorAll('[name=g]')]
            .filter((e) => /** @type {any} */ (e).checked)
            .map((e) => /** @type {any} */ (e).value);

    // A refused choice is reconciled wherever a pop-back or pop-out moved its group
    // while the proposal waited.
    it('reconciles a refused radio choice whose reply lands after a pop-back', async () => {
        const r = (v, on = false) =>
            `<input type="radio" name="g" value="${v}"${on ? ' checked' : ''} az-bound="checked" az-change='[0,"g"]'>`;
        const c = await start(view('page', view('pv', r('x', true) + r('y'))));
        const doc = await popOut(c, 'pv');
        pick(doc.querySelector('[value=y]'));
        const ref = c.lastRef();
        expect(ref).not.toBeNull();
        closePip();
        expect(shown()).toEqual(['y']);
        c.msg(null, null, ref); // the server refused: nothing changes
        expect(shown()).toEqual(['x']);
    });
    it('reconciles a refused radio choice whose reply lands after a pop-out', async () => {
        const r = (v, on = false) =>
            `<input type="radio" name="g" value="${v}"${on ? ' checked' : ''} az-bound="checked" az-change='[0,"g"]'>`;
        const c = await start(view('page', view('pv', r('x', true) + r('y'))));
        pick($('[value=y]'));
        const ref = c.lastRef();
        const doc = await popOut(c, 'pv');
        c.msg(null, null, ref);
        expect(shown(doc)).toEqual(['x']);
    });
    // A form's radio group a pop-out split, the user choosing in both parts: the
    // restore shows the choice of the part acted on last, whatever the pop-back left
    // checked, as one outside every form does.
    for (const order of [
        ['m', 'a'],
        ['a', 'm'],
    ])
        it(`restores the last act of a form group split across views, acts ${order.join(' then ')}`, async () => {
            const r = (v) => `<input type="radio" name="g" value="${v}" az-bound="checked">`;
            const pg = view(
                'page',
                `<form id="f" az-change='[0,"f"]'>${view('pv', r('a'))}${view('w', r('m'))}</form>`,
            );
            const c = await start(pg);
            const doc = await popOut(c, 'pv');
            const a = doc.querySelector('[value=a]');
            const els = { a, m: $('[value=m]') };
            pick(els[order[0]]);
            pick(els[order[1]]);
            closePip();
            a.checked = true; // a browser keeps the moved-in checked radio
            expect(shown()).toEqual(['a']);
            resyncPage(c, pg);
            expect(shown()).toEqual([order[1]]);
        });
    for (const order of [
        ['m', 'a'],
        ['a', 'm'],
    ])
        it(`restores the last act of a form group split across views, popped view last, acts ${order.join(' then ')}`, async () => {
            const r = (v) => `<input type="radio" name="g" value="${v}" az-bound="checked">`;
            const pg = view(
                'page',
                `<form id="f" az-change='[0,"f"]'>${view('w', r('m'))}${view('pv', r('a'))}</form>`,
            );
            const c = await start(pg);
            const doc = await popOut(c, 'pv');
            const a = doc.querySelector('[value=a]');
            const els = { a, m: $('[value=m]') };
            pick(els[order[0]]);
            pick(els[order[1]]);
            closePip();
            a.checked = true;
            resyncPage(c, pg);
            expect(shown()).toEqual([order[1]]);
        });

    // What an act notes keeps no removed radio, nor the subtree removed with it, alive.
    it('keeps no removed radio the user chose alive', async () => {
        const v8 = await import('node:v8');
        const vm = await import('node:vm');
        v8.setFlagsFromString('--expose-gc');
        const gc = vm.runInNewContext('gc');
        await start(view('page', ''));
        const make = (/** @type {boolean} */ act) => {
            const div = document.createElement('div');
            div.innerHTML = `<input type="radio" name="leak${act ? 1 : 0}">${'<p>x</p>'.repeat(1000)}`;
            /** @type {any} */ ($('#page')).append(div);
            if (act) /** @type {any} */ (div.firstChild).click();
            div.remove();
            return new WeakRef(div);
        };
        const acted = make(true);
        const control = make(false);
        for (let i = 0; i < 5; i++) {
            await new Promise((r) => setTimeout(r, 0));
            gc();
        }
        expect(control.deref()).toBeUndefined();
        expect(acted.deref()).toBeUndefined();
    });
    // A rename moving a checked radio into a group leaves the user's acts counting:
    // choosing another radio after it is a later act.
    it('counts the user choosing back after a rename moved a checked radio in', async () => {
        const c = await start(
            view(
                'page',
                '<input az="0" type="radio" name="g" value="x" checked>' +
                    '<input az="1" type="radio" name="g" value="y">' +
                    '<input az="2" type="radio" name="h" value="z" checked>',
            ),
        );
        const bound = await import('./arizona-bound.js');
        const y = $('[value=y]');
        const z = $('[value=z]');
        pick(y);
        const first = bound.actedOn(y);
        expect(first).toBeGreaterThan(0);
        c.msg([[SET_ATTR, 'page:2', 'name', 'g']]);
        if (y.checked) y.checked = false; // the browser unchecks the rest when a checked radio joins
        expect(z.checked).toBe(true);
        pick(y); // the user's real change, z -> y
        expect(bound.actedOn(y)).toBeGreaterThan(first);
    });

    // A form inserted or removed moves the radios naming it (`form=`) between groups.
    it('counts no act on a choice a form insert moved into its group', async () => {
        const c = await start(
            view(
                'page',
                '<input type="radio" name="g" value="x" checked>' +
                    '<input type="radio" name="g" value="y" form="f">' +
                    '<span az="1"><!--az:1--><!--/az--></span>',
            ),
        );
        const bound = await import('./arizona-bound.js');
        const y = $('[value=y]');
        pick(y);
        c.msg([[TEXT, 'page:1', '<form id="f"></form>', true]]); // y joins f's group
        expect(y.form?.id).toBe('f');
        // The pick was for the formless group: in f's it counts for none.
        const first = bound.actedOn(y);
        expect(first).toBe(0);
        change(y); // code's, on the choice y's group shows
        expect(bound.actedOn(y)).toBe(first);
    });
    it('counts no act on a choice a form removal moved into its group', async () => {
        const c = await start(
            view(
                'page',
                '<input az="0" type="radio" name="g" value="x">' +
                    '<input type="radio" name="g" value="y" form="f" checked>' +
                    '<span az="1"><!--az:1--><form id="f"></form><!--/az--></span>',
            ),
        );
        const bound = await import('./arizona-bound.js');
        pick($('[value=x]'));
        c.msg([[REM_ATTR, 'page:0', 'checked']]); // the page's group shows none
        const y = $('[value=y]');
        c.msg([[TEXT, 'page:1', '', true]]); // y joins the page's group, checked
        expect([$('[value=x]').checked, y.checked, y.form]).toEqual([false, true, null]);
        const first = bound.actedOn(y);
        change(y); // code's, on the choice the group shows
        expect(bound.actedOn(y)).toBe(first);
    });
    it('counts no act on a choice a form node removal moved into its group', async () => {
        const c = await start(
            view(
                'page',
                '<input az="0" type="radio" name="g" value="x">' +
                    '<input type="radio" name="g" value="y" form="f" checked>' +
                    '<form az="1" id="f"></form>',
            ),
        );
        const bound = await import('./arizona-bound.js');
        pick($('[value=x]'));
        c.msg([[REM_ATTR, 'page:0', 'checked']]); // the page's group shows none
        const y = $('[value=y]');
        c.msg([[4, 'page:1']]); // the form goes: y joins the page's group, checked
        expect([$('[value=x]').checked, y.checked, y.form]).toEqual([false, true, null]);
        const first = bound.actedOn(y);
        change(y);
        expect(bound.actedOn(y)).toBe(first);
    });

    // A restore writing an unbound group's choice leaves the user's acts counting:
    // choosing the radio the resync rendered checked after it is an act.
    it('counts choosing the rendered radio after a restore moved an unbound group', async () => {
        const r = (/** @type {string} */ v, on = false) =>
            `<input type="radio" name="g" value="${v}"${on ? ' checked' : ''}>`;
        const pg = view('page', `<form id="f">${r('x', true)}${r('y')}</form>`);
        const c = await start(pg);
        const bound = await import('./arizona-bound.js');
        pick($('[value=y]'));
        resyncPage(c, pg);
        expect(shown()).toEqual(['y']);
        const x = $('[value=x]');
        pick(x);
        expect(bound.actedOn(x)).toBeGreaterThan(0);
    });

    // An unbound form's group spanning views that the user never chose in keeps what
    // each part held at the drop (rule 4): no act ranks them.
    it('keeps what an unbound form group spanning views held when no act ranks it', async () => {
        const r = (/** @type {string} */ v, on = false) =>
            `<input type="radio" name="g" value="${v}"${on ? ' checked' : ''}>`;
        const pg = (/** @type {boolean} */ on) =>
            view('page', `<form id="f">${r('x')}${view('pv', r('y', on))}</form>`);
        const c = await start(pg(true));
        resyncPage(c, pg(false)); // the server no longer checks y
        expect(shown()).toEqual(['y']);
    });

    // A pop-back unchecking the server's choice keeps nothing of it: an unbound
    // form's group the user never chose in shows the server's choice after the
    // resync.
    it('keeps no unchosen radio a pop-back unchecked in an unbound form', async () => {
        const r = (/** @type {string} */ v, on = false) =>
            `<input az="${v}" type="radio" name="g" value="${v}"${on ? ' checked' : ''}>`;
        // The window's view first: a choice restored after it would win the group.
        const pg = (/** @type {string} */ on) =>
            view(
                'page',
                `<form id="f">${view('pv', r('y', on === 'y'))}${r('x', on === 'x')}</form>`,
            );
        const c = await start(pg('x'));
        const doc = await popOut(c, 'pv');
        c.msg(null, [[7, '[value=y]', 'checked', '']]); // the server checks y in the window
        expect(/** @type {any} */ (doc.querySelector('[value=y]')).checked).toBe(true);
        closePip();
        expect(shown()).toEqual(['y']);
        resyncPage(c, pg('y'));
        expect(shown()).toEqual(['y']);
    });

    // The same for a radio outside the form naming it (`form=`), whose part is restored
    // after the form's own view's.
    it('keeps no unchosen form= radio a pop-back of its form unchecked', async () => {
        const r = (/** @type {string} */ v, on = false, attrs = '') =>
            `<input type="radio" name="g" value="${v}"${on ? ' checked' : ''}${attrs}>`;
        const pg = (/** @type {string} */ on) =>
            view(
                'page',
                r('x', on === 'x', ' form="f"') +
                    view('pv', `<form id="f">${r('y', on === 'y')}</form>`),
            );
        const c = await start(pg('x'));
        const doc = await popOut(c, 'pv');
        c.msg(null, [[7, '[value=y]', 'checked', '']]); // the server checks y in the window
        expect(/** @type {any} */ (doc.querySelector('[value=y]')).checked).toBe(true);
        closePip();
        // A browser keeps the radio moved in checked: x, joining its group, unchecks.
        /** @type {any} */ ($('[value=x]')).checked = false;
        expect(shown()).toEqual(['y']);
        resyncPage(c, pg('y'));
        expect(shown()).toEqual(['y']);
    });

    // A script relaying a pick with `checked` and an untrusted `change` makes no act:
    // warned once per radio, and only on a server-owned group's non-default choice.
    describe('a relayed radio pick', () => {
        const page = (/** @type {boolean} */ bound, extra = '') => {
            const r = (/** @type {string} */ v, on = false) =>
                `<input type="radio" name="g" value="${v}"${on ? ' checked' : ''}${bound ? ' az-bound="checked"' : ''}${extra}>`;
            return view('page', r('x', true) + r('y') + r('z'));
        };
        const announce = (/** @type {any} */ el, type = 'change') =>
            el.dispatchEvent(new Event(type, { bubbles: true }));
        const relay = (/** @type {any} */ el, type = 'change') => {
            el.checked = true;
            announce(el, type);
        };
        const warned = (/** @type {any} */ spy) =>
            spy.mock.calls.filter(([m]) => String(m).includes('radio.click()')).length;
        it('warns once for a script checking a server-owned radio', async () => {
            const warn = vi.spyOn(console, 'warn').mockImplementation(() => {});
            await start(page(true));
            relay($('[value=y]'));
            relay($('[value=y]'));
            expect(warned(warn)).toBe(1);
        });
        it('names the event the script dispatched', async () => {
            const warn = vi.spyOn(console, 'warn').mockImplementation(() => {});
            await start(page(true));
            relay($('[value=y]'), 'input');
            const msgs = warn.mock.calls.map(([m]) => String(m));
            expect([msgs.length, msgs[0]?.includes("dispatched 'input'")]).toEqual([1, true]);
        });
        // A library announcing again the pick the user just made (or a restore wrote)
        // relays nothing: the radio holds its group's latest act.
        for (const type of ['change', 'input'])
            it(`warns for no ${type} re-announcing the user's pick`, async () => {
                const warn = vi.spyOn(console, 'warn').mockImplementation(() => {});
                await start(page(true));
                const y = $('[value=y]');
                y.click();
                announce(y, type);
                expect(warned(warn)).toBe(0);
            });
        it('warns for no change re-announcing a choice the restore wrote', async () => {
            const warn = vi.spyOn(console, 'warn').mockImplementation(() => {});
            const pg = page(true, ` az-change='[0,"v"]'`);
            const c = await start(pg);
            $('[value=y]').click();
            resyncPage(c, pg);
            const y = $('[value=y]');
            expect(y.checked).toBe(true);
            announce(y);
            expect(warned(warn)).toBe(0);
        });
        // The radio the user picked before their latest pick holds no latest act.
        for (const before of [false, true])
            it(`warns for a script relaying another radio after the user picked one (z picked before: ${before})`, async () => {
                const warn = vi.spyOn(console, 'warn').mockImplementation(() => {});
                await start(page(true));
                if (before) $('[value=z]').click();
                $('[value=y]').click();
                relay($('[value=z]'));
                expect(warned(warn)).toBe(1);
            });
        it('warns for no user click', async () => {
            const warn = vi.spyOn(console, 'warn').mockImplementation(() => {});
            await start(page(true));
            $('[value=y]').click();
            expect(warned(warn)).toBe(0);
        });
        it('warns for no change on a radio left unchecked', async () => {
            const warn = vi.spyOn(console, 'warn').mockImplementation(() => {});
            await start(page(true));
            $('[value=y]').dispatchEvent(new Event('change', { bubbles: true }));
            expect(warned(warn)).toBe(0);
        });
        it('warns for no change on the default radio', async () => {
            const warn = vi.spyOn(console, 'warn').mockImplementation(() => {});
            await start(page(true));
            relay($('[value=x]'));
            expect(warned(warn)).toBe(0);
        });
        it('warns for no change in an unbound group', async () => {
            const warn = vi.spyOn(console, 'warn').mockImplementation(() => {});
            await start(page(false));
            relay($('[value=y]'));
            expect(warned(warn)).toBe(0);
        });
    });

    // A trusted change on a radio an app listener left unchecked is no act: an act is
    // one on the radio checked after it.
    it('counts no act on a radio a click listener left unchecked', async () => {
        const r = (/** @type {string} */ v, on = false) =>
            `<input type="radio" name="g" value="${v}"${on ? ' checked' : ''} az-bound="checked">`;
        await start(view('page', r('b', true) + r('c')));
        const bound = await import('./arizona-bound.js');
        const cc = $('[value=c]');
        cc.addEventListener('click', () => {
            cc.checked = false;
            $('[value=b]').checked = true;
        });
        cc.click();
        expect([cc.checked, bound.actedOn(cc)]).toEqual([false, 0]);
    });

    // A radio a diff adds to a group a proposal waits on is the unit's too: a refusal
    // reconciles it with the rest.
    it('reconciles a radio a diff added to the group of a refused choice', async () => {
        const r = (/** @type {string} */ v, on = false) =>
            `<input type="radio" name="g" value="${v}"${on ? ' checked' : ''} az-bound="checked" az-change='[0,"g"]'>`;
        const c = await start(
            view('page', `${r('x', true)}${r('y')}<span az="1"><!--az:1--><!--/az--></span>`),
        );
        pick($('[value=y]'));
        const first = c.lastRef();
        c.msg([[TEXT, 'page:1', r('z'), true]]);
        pick($('[value=z]'));
        const second = c.lastRef();
        c.msg(null, null, first); // both refused
        c.msg(null, null, second);
        expect(shown()).toEqual(['x']);
    });

    // the page's part holds the server's choice a; the window's part (no
    // default) gets the user's w2, refused. A pop-back moves w2 in (unchecking a).
    // The reply must show the server's choice a, not an empty group.
    it('reconciles a refused window choice to the server choice of the merged group', async () => {
        const r = (/** @type {string} */ v, on = false) =>
            `<input type="radio" name="g" value="${v}"${on ? ' checked' : ''} az-bound="checked" az-change='[0,"g"]'>`;
        const c = await start(view('page', r('a', true) + view('pv', r('w1') + r('w2'))));
        const doc = await popOut(c, 'pv');
        pick(doc.querySelector('[value=w2]'));
        const ref = c.lastRef();
        expect(ref).not.toBeNull();
        closePip();
        $('[value=w2]').checked = true; // the browser keeps the moved-in checked radio
        expect(shown()).toEqual(['w2']);
        c.msg(null, null, ref); // refused
        expect(shown()).toEqual(['a']);
    });

    // a diff writing a form's id moves the radios naming it (form=) into its group,
    // as a form insert does: code's change on the choice it shows is no act.
    it('counts no act on a choice a form id write moved into its group', async () => {
        const c = await start(
            view(
                'page',
                '<input type="radio" name="g" value="x" checked>' +
                    '<input type="radio" name="g" value="y" form="f">' +
                    '<form az="1" id="f0"></form>',
            ),
        );
        const bound = await import('./arizona-bound.js');
        const y = $('[value=y]');
        pick(y);
        c.msg([[SET_ATTR, 'page:1', 'id', 'f']]);
        const first = bound.actedOn(y);
        expect(y.form?.id).toBe('f');
        change(y);
        expect(bound.actedOn(y)).toBe(first);
    });

    // a pop-out moving a form away moves the radios naming it outside the view to
    // their document's group, as a form removal does.
    it('counts no act on a choice a pop-out of its form moved into the page group', async () => {
        const c = await start(
            view(
                'page',
                '<input az="0" type="radio" name="g" value="x">' +
                    '<input type="radio" name="g" value="y" form="f" checked>' +
                    view('pv', '<form id="f"></form>'),
            ),
        );
        const bound = await import('./arizona-bound.js');
        pick($('[value=x]'));
        c.msg([[REM_ATTR, 'page:0', 'checked']]);
        const y = $('[value=y]');
        await popOut(c, 'pv');
        expect([$('[value=x]').checked, y.checked, y.form]).toEqual([false, true, null]);
        const first = bound.actedOn(y);
        change(y);
        expect(bound.actedOn(y)).toBe(first);
    });

    // a form removed inside a popped-out window: its radios naming it join that
    // window's group, and code's change on one there is untrusted: no act.
    it('counts no act on a choice a form removal in a window moved into its group', async () => {
        const c = await start(
            view(
                'page',
                view(
                    'pv',
                    '<input az="0" type="radio" name="g" value="x">' +
                        '<input type="radio" name="g" value="y" form="f" checked>' +
                        '<form az="1" id="f"></form>',
                ),
            ),
        );
        const bound = await import('./arizona-bound.js');
        const doc = await popOut(c, 'pv');
        pick(doc.querySelector('[value=x]'));
        c.msg([[REM_ATTR, 'pv:0', 'checked']]);
        const y = doc.querySelector('[value=y]');
        c.msg([[4, 'pv:1']]);
        expect([doc.querySelector('[value=x]').checked, y.checked, y.form]).toEqual([
            false,
            true,
            null,
        ]);
        const first = bound.actedOn(y);
        change(y);
        expect(bound.actedOn(y)).toBe(first);
    });
});
