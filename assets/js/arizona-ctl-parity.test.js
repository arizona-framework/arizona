import { afterEach, describe, expect, it, vi } from 'vitest';
import { embedAttrs } from './test-proofs.js';

// Parity with a larger design of server-owned controls: its scenarios for the core
// guarantee, ported to `az-ctl` (a control the server renders shows the server's
// value once the event carrying it is answered, and an older reply never overwrites
// newer typing). Where the larger design withholds the attribute write itself while
// a control is in flight, only what the control shows is asserted here: this design
// writes the attribute (the default) at once and holds the live state.

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
    const unembed = embedAttrs();
    const disconnect = mod.connect('/ws');
    unembed();
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

const INPUT = `<div id="v" az-view><input az="0" name="q" value="" az-ctl="value" az-input='[0,"edit"]'></div>`;

describe('a refused edit', () => {
    it('shows the server value again when the reply changes nothing', async () => {
        const c = await start(INPUT);
        type($('input'), 'Capital');
        c.msg(null, null, c.lastRef());
        expect($('input').value).toBe('');
    });

    it('tags every proposal frame with a reply ref', async () => {
        const c = await start(INPUT);
        type($('input'), 'a');
        expect(c.sent().at(-1)).toEqual(['v', 'edit', { value: 'a' }, expect.any(Number)]);
    });

    it('resets a select whose option the server kept', async () => {
        const c = await start(
            `<div id="v" az-view><select name="s" az-change='[0,"pick"]'>` +
                `<option value="a" selected az-ctl="selected">A</option>` +
                `<option value="b" az-ctl="selected">B</option></select></div>`,
        );
        $('select').value = 'b';
        change($('select'));
        c.msg(null, null, c.lastRef());
        expect($('select').value).toBe('a');
    });

    // A select whose server-rendered value picks among options the compiler could not
    // mark (a component renders them) owns them through `az-ctl` on the select.
    const valued = (/** @type {string} */ slot = '') =>
        `<div id="v" az-view><select az="s" name="s" value="a" az-ctl="value" az-change='[0,"pick"]'>` +
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
        expect($('select').value).toBe('b');
        c.msg(null, null, ref);
        expect([$('select').getAttribute('value'), $('select').value]).toEqual(['b', 'b']);
    });

    it('resets the bound controls of a form trigger', async () => {
        const c = await start(
            `<div id="v" az-view><form id="f" az-change='[0,"validate"]'>` +
                `<input name="q" value="" az-ctl="value"></form></div>`,
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
                `<input name="q" value="" az-ctl="value"></form></div>`,
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
                `<input type="radio" name="r" value="a" checked az-ctl="checked" az-change='[0,"pick"]'>` +
                `<input type="radio" name="r" value="b" az-ctl="checked" az-change='[0,"pick"]'>` +
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
                `<form id="f"><input type="radio" name="r" value="b" az-ctl="checked" az-change='[0,"pick"]'></form>` +
                `<input type="radio" form="f" name="r" value="a" checked az-ctl="checked">` +
                `<form id="g"><input type="radio" name="r" value="c" checked az-ctl="checked">` +
                `<input type="radio" name="r" value="d" az-ctl="checked"></form>` +
                `</div>`,
        );
        $('[value=d]').checked = true; // another form's group, never proposed
        $('[value=b]').click();
        c.msg(null, null, c.lastRef());
        expect($('[value=a]').checked).toBe(true);
        expect($('[value=b]').checked).toBe(false);
        expect($('[value=d]').checked).toBe(true);
    });
});

describe('a reconciled radio group', () => {
    // A browser groups no radio without a name: each is its own unit, so the
    // reconcile of one leaves another's state alone.
    it('is not every nameless radio', async () => {
        const c = await start(
            `<div id="v" az-view>` +
                `<input id="a" az="0" type="radio" az-ctl="checked" az-change='[0,"a"]'>` +
                `<input id="b" type="radio" az-ctl="checked"></div>`,
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
                `<input az="1" name="q" value="" az-ctl="value" az-input='[0,"edit"]'></li></ul></div>`,
        );
        type($('input'), 'a');
        const r1 = c.lastRef();
        type($('input'), 'ab');
        c.msg([[7, 'v:0', 'k1', [[SET_ATTR, '1', 'value', 'a']]]], null, r1);
        expect($('input').value).toBe('ab');
    });

    it('does not undo a newer checkbox toggle with an older removal in a stream item', async () => {
        const c = await start(
            `<div id="v" az-view><ul az="0"><li az-key="k1"><input az="1" type="checkbox" name="c" ` +
                `checked az-ctl="checked" az-change='[0,"tick"]'></li></ul></div>`,
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
            `<div id="v" az-view><textarea az="0" name="t" az-ctl="content" az-input='[0,"edit"]'></textarea></div>`,
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
            `<div id="v" az-view><input az="0" type="checkbox" name="c" checked az-ctl="checked" az-change='[0,"tick"]'></div>`,
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
                `<input az="0" type="radio" name="r" value="a" checked az-ctl="checked" az-change='[0,"pick"]'>` +
                `<input az="1" type="radio" name="r" value="b" az-ctl="checked" az-change='[0,"pick"]'>` +
                `<input az="2" type="radio" name="r" value="c" az-ctl="checked" az-change='[0,"pick"]'>` +
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
    it('is not a div trigger holding bound controls', async () => {
        const c = await start(
            `<div id="v" az-view><div az-click='[0,"go"]'>` +
                `<input name="q" value="" az-ctl="value"></div></div>`,
        );
        $('input').value = 'x';
        $('[az-click]').click();
        expect(c.sent().at(-1)).toHaveLength(3);
        c.msg(null, null, null);
        expect($('input').value).toBe('x');
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

    // A child view under an az-nodiff element is registered and diffs its own template;
    // a view whose own root is az-nodiff never diffs.
    it('is a control of a live child view inside an az-nodiff element', async () => {
        const c = await start(
            `<div id="v" az-view><div az-nodiff><div id="c" az-view>` +
                `<input az="0" name="q" value="" az-ctl="value" az-input='[0,"edit"]'>` +
                `</div></div></div>`,
        );
        type($('input'), 'x');
        expect(c.sent().at(-1)).toEqual(['c', 'edit', { value: 'x' }, expect.any(Number)]);
        c.msg(null, null, c.lastRef());
        expect($('input').value).toBe('');
    });

    it('shares one reply ref with the view transition it is raised under', async () => {
        const c = await start(
            `<div id="v" az-view><input az="0" name="q" value="" az-ctl="value" az-input='[20,{},[0,"edit"]]'></div>`,
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
                `<input az="0" type="radio" name="r" value="a" checked az-ctl="checked" az-change='[0,"pick"]'>` +
                `<input az="1" type="radio" name="r" value="b" az-ctl="checked" az-debounce="commit" az-change='[0,"pick"]'>` +
                `<input az="2" type="radio" name="r" value="c" az-ctl="checked" az-change='[0,"pick"]'>` +
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
            `<div id="v" az-view><input az="0" name="q" value="" az-ctl="value" ` +
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
        expect($('input').value).toBe('ab');
        c.msg([[SET_ATTR, 'v:0', 'value', 'srv2']]);
        c.msg(null, null, ref);
        expect($('input').getAttribute('value')).toBe('srv2');
        expect($('input').value).toBe('srv2');
    });

    it('defers a removal', async () => {
        const c = await start(
            `<div id="v" az-view><input az="0" type="checkbox" name="c" checked az-ctl="checked" az-change='[0,"tick"]'></div>`,
        );
        const cb = $('input');
        cb.click(); // off
        const ref = c.lastRef();
        cb.click(); // on again, still proposed by ref's capture
        const ref2 = c.lastRef();
        c.msg([[REM_ATTR, 'v:0', 'checked']], null, ref);
        expect(cb.checked).toBe(true);
        c.msg(null, null, ref2);
        expect(cb.hasAttribute('checked')).toBe(false);
        expect(cb.checked).toBe(false); // the reply kept the server's unchecked state
    });

    it("defers a textarea's content", async () => {
        const c = await start(
            `<div id="v" az-view><textarea az="0" name="t" az-ctl="content" az-input='[0,"edit"]'>a</textarea></div>`,
        );
        const ta = $('textarea');
        type(ta, 'ab');
        const ref = c.lastRef();
        c.msg([[TEXT, 'v:0', 'srv']]);
        expect(ta.value).toBe('ab');
        c.msg(null, null, ref);
        expect(ta.textContent).toBe('srv');
        expect(ta.value).toBe('srv');
    });

    it('in a stream item', async () => {
        const c = await start(
            `<div id="v" az-view><ul az="0"><li az-key="k1">` +
                `<input az="1" name="q" value="" az-ctl="value" az-input='[0,"edit"]'></li></ul></div>`,
        );
        type($('input'), 'ab');
        const ref = c.lastRef();
        c.msg([[7, 'v:0', 'k1', [[SET_ATTR, '1', 'value', 'srv']]]]);
        expect($('input').value).toBe('ab');
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
            `<div id="v" az-view><input az="0" az-hook="H" name="q" value="" az-ctl="value" az-input='[0,"edit"]'></div>`,
            { before: (mod) => (mod.hooks.H = { updated }) },
        );
        type($('input'), 'ab');
        const ref = c.lastRef();
        c.msg([[SET_ATTR, 'v:0', 'value', 'ab']]);
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

    /** Whether a write to a server-owned control nothing holds reaches what it shows. */
    async function opPathIdle(/** @type {any} */ c) {
        document.body.insertAdjacentHTML(
            'beforeend',
            `<div id="w" az-view><input id="other" az="0" name="o" value="" az-ctl="value"></div>`,
        );
        c.msg([[SET_ATTR, 'w:0', 'value', 'x']]);
        return $('#other').value === 'x';
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
    // A command the user's own event runs is the user's act: it writes through.
    it('a client set_attr clears a focused, edited control (Escape to clear)', async () => {
        await start(
            `<div id="v" az-view><input id="q" az="0" name="q" value="" az-ctl="value" ` +
                `az-keydown='[7,"#q","value",""]' az-change='[0,"set"]'></div>`,
        );
        const el = $('input');
        el.focus();
        el.value = 'abc';
        el.dispatchEvent(new KeyboardEvent('keydown', { key: 'Escape', bubbles: true }));
        expect(el.getAttribute('value')).toBe('');
        expect(el.value).toBe('');
    });

    it('a push then a client clear: typing the next item survives the reply', async () => {
        const c = await start(
            `<div id="v" az-view><input id="q" az="0" name="q" value="" az-ctl="value" ` +
                `az-keydown='[[0,"add"],[7,"#q","value",""]]'></div>`,
        );
        const el = $('input');
        el.focus();
        el.value = 'milk';
        el.dispatchEvent(new KeyboardEvent('keydown', { key: 'Enter', bubbles: true }));
        expect(c.sent().at(-1)[1]).toBe('add');
        expect(el.value).toBe('');
        el.value = 'e';
        c.msg(null, null, c.lastRef());
        expect(el.value).toBe('e');
        el.blur();
        expect(el.value).toBe('e');
    });

    it('a set_attr effect a frame carries is a server write, held off unsent typing', async () => {
        const c = await start(
            `<div id="v" az-view><input id="q" az="0" name="q" value="a" az-ctl="value"></div>`,
        );
        const el = $('input');
        el.focus();
        el.value = 'ab';
        c.msg(null, [[7, '#q', 'value', 'srv']]);
        expect(el.value).toBe('ab');
        el.blur();
        expect(el.value).toBe('srv');
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
                `<option value="a" selected az-ctl="selected">A</option>` +
                `<option value="b" az-ctl="selected">B</option></select></div>`,
        );
        $('select').value = 'b';
        change($('select'));
        $('select').insertAdjacentHTML(
            'beforeend',
            `<option value="c" az-ctl="selected">C</option>`,
        );
        c.msg(null, null, c.lastRef());
        expect($('select').value).toBe('a');
    });

    it('resets a select whose reply removes an option', async () => {
        const c = await start(
            `<div id="v" az-view><select name="s" az-change='[0,"pick"]'>` +
                `<option value="a" selected az-ctl="selected">A</option>` +
                `<option value="b" az-ctl="selected">B</option>` +
                `<option value="c" az-ctl="selected">C</option></select></div>`,
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
                `<input type="radio" name="r" value="a" checked az-ctl="checked" az-change='[0,"pick"]'>` +
                `<input type="radio" name="r" value="b" az-ctl="checked" az-change='[0,"pick"]'>` +
                `</div>`,
        );
        $('[value=b]').click();
        $('#v').insertAdjacentHTML(
            'beforeend',
            `<input type="radio" name="r" value="c" az-ctl="checked">`,
        );
        c.msg(null, null, c.lastRef());
        expect($('[value=a]').checked).toBe(true);
        expect($('[value=b]').checked).toBe(false);
    });

    it('keeps a choice made after the capture in a group whose reply adds a radio', async () => {
        const c = await start(
            `<div id="v" az-view>` +
                `<input type="radio" name="r" value="a" checked az-ctl="checked" az-change='[0,"pick"]'>` +
                `<input type="radio" name="r" value="b" az-ctl="checked">` +
                `<input type="radio" name="r" value="d" az-ctl="checked">` +
                `</div>`,
        );
        $('[value=a]').dispatchEvent(new Event('change', { bubbles: true }));
        $('[value=d]').checked = true; // the user moved on, unproposed
        $('#v').insertAdjacentHTML(
            'beforeend',
            `<input type="radio" name="r" value="c" az-ctl="checked">`,
        );
        c.msg(null, null, c.lastRef());
        expect($('[value=d]').checked).toBe(true);
    });

    // A keydown proposes the value before the key's input lands: a key that edits
    // never matches the live value, but one that inserts nothing (Enter) does.
    it('reconciles on a key that inserts nothing, not on one that edits', async () => {
        const c = await start(
            `<div id="v" az-view><input name="q" value="" az-ctl="value" ` +
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

    it('writes nothing for an echo', async () => {
        const c = await start(INPUT);
        type($('input'), 'ab');
        const setter = vi.spyOn(HTMLInputElement.prototype, 'value', 'set');
        c.msg([[SET_ATTR, 'v:0', 'value', 'ab']], null, c.lastRef());
        expect(setter).not.toHaveBeenCalled();
        expect($('input').value).toBe('ab');
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
});

describe('a reconnect', () => {
    const view = (/** @type {string} */ change) =>
        `<div id="page" az-view><form id="f"${change}><input name="q" value="" az-ctl="value"></form></div>`;

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
});

// The larger design's scenarios below raise an IME composition, or type inside a
// debounce window, in a field that is not focused, which a browser never does; and
// it writes a set_attr effect to a control in flight at once, where this design
// holds it until the reply settles. These keep each scenario's guarantee with the
// field focused, and the effect shown once the reply is applied.
describe('adapted: focus and effects', () => {
    it('covers the debounce window before the send (focused)', async () => {
        vi.useFakeTimers();
        const c = await start(INPUT.replace('<input', '<input az-debounce="300"'));
        $('input').focus();
        type($('input'), 'ab');
        c.msg([[SET_ATTR, 'v:0', 'value', 'srv']]);
        expect($('input').value).toBe('ab');
    });

    it('a reply mid-composition settles on the input that commits it', async () => {
        const c = await start(INPUT);
        const el = $('input');
        el.focus();
        el.dispatchEvent(new Event('compositionstart', { bubbles: true }));
        type(el, 'か');
        c.msg(null, null, c.lastRef());
        expect(el.value).toBe('か');
        el.dispatchEvent(new Event('compositionend', { bubbles: true }));
        type(el, 'か');
        c.msg(null, null, c.lastRef());
        expect(el.value).toBe('');
    });

    it('a reply mid-composition settles when focus leaves', async () => {
        const c = await start(INPUT);
        const el = $('input');
        el.focus();
        el.dispatchEvent(new Event('compositionstart', { bubbles: true }));
        type(el, 'か');
        c.msg(null, null, c.lastRef());
        el.dispatchEvent(new Event('compositionend', { bubbles: true }));
        el.blur();
        expect(el.value).toBe('');
    });

    it('a set_attr effect in the reply shows', async () => {
        const c = await start(INPUT.replace('<input', '<input id="i"'));
        type($('input'), 'ab');
        c.msg(null, [[7, '#i', 'value', 'eff']], c.lastRef());
        expect($('input').value).toBe('eff');
    });

    it('a set_attr effect pushed while in flight shows once the reply settles', async () => {
        const c = await start(INPUT.replace('<input', '<input id="i"'));
        type($('input'), 'ab');
        const ref = c.lastRef();
        c.msg(null, [[7, '#i', 'value', 'eff']]);
        expect($('input').value).toBe('ab');
        c.msg(null, null, ref);
        expect($('input').value).toBe('eff');
    });
});
