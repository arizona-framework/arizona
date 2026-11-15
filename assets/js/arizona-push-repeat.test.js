import { afterEach, describe, expect, it, vi } from 'vitest';
import { embedAttrs } from './test-proofs.js';

// `push_event` under `on_repeat: 'drop'`: a repeat -- the same event to the same view
// from the same holder -- is not sent while the previous send awaits its reply. The
// holder carries `az-busy` meanwhile, counted with its fetches. These tests drive a
// connected page through a mock worker, as arizona.test.js does.

/** @type {any} */
let mock = null;
/** @type {any} */
let mod = null;

afterEach(() => {
    if (mock) mock.restore();
    mock = null;
    document.body.innerHTML = '';
    vi.useRealTimers();
    vi.restoreAllMocks();
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
        open: (reconnect = false) => onmessage({ data: [1, reconnect] }),
        close: (code = 1006) => onmessage({ data: [2, code] }),
        reply: (ref, ops = null, effects = null) =>
            onmessage({ data: [0, ops, effects, false, null, ref] }),
        /** The event frames sent, parsed. */
        sent: () =>
            posted
                .filter((d) => d[0] === 1)
                .map((d) => JSON.parse(d[1]))
                .filter((f) => f[0] !== 'navigate' && f[0] !== 'patch'),
        restore() {
            disconnect();
            globalThis.Worker = OrigWorker;
        },
    };
    mock.open();
    return mock;
}

const cmd = (c) => JSON.stringify(c).replace(/'/g, '&#39;');
const DROP = { on_repeat: 'drop' };
const SAVE = cmd([0, 'save', {}, DROP]);

/** Listen for `arizona:event-dropped` on `document`; returns the events seen. */
function droppedEvents() {
    /** @type {CustomEvent[]} */
    const seen = [];
    const on = (/** @type {Event} */ e) => seen.push(/** @type {CustomEvent} */ (e));
    document.addEventListener('arizona:event-dropped', on);
    return seen;
}

function submit(form) {
    form.dispatchEvent(new Event('submit', { bubbles: true, cancelable: true }));
}

describe('push_event on_repeat: drop', () => {
    it('sends one frame for a double click, with a reply ref, and reports the repeat', async () => {
        await connect(`<button id="b" az-click='${SAVE}'>Save</button>`);
        const btn = /** @type {HTMLElement} */ (document.getElementById('b'));
        const dropped = droppedEvents();
        const warn = vi.spyOn(console, 'warn');
        btn.click();
        btn.click();
        const sent = mock.sent();
        expect(sent).toHaveLength(1);
        expect(sent[0].slice(0, 3)).toEqual(['page', 'save', {}]);
        expect(typeof sent[0][3]).toBe('number');
        expect(btn.hasAttribute('az-busy')).toBe(true);
        expect(dropped).toHaveLength(1);
        // Bubbles from the holder, so a page-level listener sees it.
        expect(dropped[0].target).toBe(btn);
        expect(dropped[0].bubbles).toBe(true);
        expect(dropped[0].cancelable).toBe(false);
        expect(dropped[0].detail).toEqual({ event: 'save', reason: 'in_flight' });
        expect(warn).not.toHaveBeenCalled();
    });

    it('runs the commands beside a dropped event', async () => {
        await connect(
            `<button id="b" az-click='${cmd([
                [0, 'save', {}, DROP],
                [1, '#m'],
            ])}'>Save</button><p id="m"></p>`,
        );
        const btn = /** @type {HTMLElement} */ (document.getElementById('b'));
        const m = /** @type {HTMLElement} */ (document.getElementById('m'));
        btn.click();
        expect(m.hidden).toBe(true);
        btn.click();
        expect(mock.sent()).toHaveLength(1);
        expect(m.hidden).toBe(false);
    });

    it('lets the event out again once its reply lands', async () => {
        await connect(`<button id="b" az-click='${SAVE}'>Save</button>`);
        const btn = /** @type {HTMLElement} */ (document.getElementById('b'));
        btn.click();
        mock.reply(mock.sent()[0][3]);
        expect(btn.hasAttribute('az-busy')).toBe(false);
        btn.click();
        expect(mock.sent()).toHaveLength(2);
        expect(btn.hasAttribute('az-busy')).toBe(true);
    });

    it('releases before the reply effects run, so one may push the event again', async () => {
        await connect(`<button id="b" az-click='${SAVE}'>Save</button>`);
        const btn = /** @type {HTMLElement} */ (document.getElementById('b'));
        /** @type {boolean[]} */
        const busyAtEffect = [];
        document.addEventListener(
            'again',
            () => {
                busyAtEffect.push(btn.hasAttribute('az-busy'));
                btn.click();
            },
            { once: true },
        );
        const dropped = droppedEvents();
        btn.click();
        mock.reply(mock.sent()[0][3], null, [[9, 'again', {}]]);
        expect(busyAtEffect).toEqual([false]);
        expect(mock.sent()).toHaveLength(2);
        expect(dropped).toHaveLength(0);
        expect(btn.hasAttribute('az-busy')).toBe(true);
    });

    it('a reply to another frame releases nothing', async () => {
        await connect(`<button id="b" az-click='${SAVE}'>Save</button>`);
        const btn = /** @type {HTMLElement} */ (document.getElementById('b'));
        btn.click();
        const ref = mock.sent()[0][3];
        mock.reply(null, null, null);
        mock.reply(ref + 100);
        expect(btn.hasAttribute('az-busy')).toBe(true);
        btn.click();
        expect(mock.sent()).toHaveLength(1);
    });

    it.each([
        ['a close', () => mock.close()],
        [
            'a pagehide',
            () => window.dispatchEvent(new PageTransitionEvent('pagehide', { persisted: true })),
        ],
        ['a disconnect', () => mock.restore()],
    ])('releases on %s, whose replies never come', async (_label, end) => {
        await connect(`<button id="b" az-click='${SAVE}'>Save</button>`);
        const btn = /** @type {HTMLElement} */ (document.getElementById('b'));
        btn.click();
        expect(btn.hasAttribute('az-busy')).toBe(true);
        end();
        expect(btn.hasAttribute('az-busy')).toBe(false);
    });

    it('a slot outlives no close: the reconnected page sends the event again', async () => {
        await connect(`<button id="b" az-click='${SAVE}'>Save</button>`);
        const btn = /** @type {HTMLElement} */ (document.getElementById('b'));
        btn.click();
        mock.close();
        mock.open(true);
        btn.click();
        expect(mock.sent()).toHaveLength(2);
    });

    it('takes no slot when the event is not sent', async () => {
        await connect(`<button id="b">Save</button>`);
        const btn = /** @type {HTMLElement} */ (document.getElementById('b'));
        mock.close();
        const dropped = droppedEvents();
        vi.spyOn(console, 'warn').mockImplementation(() => {});
        mod.executeJS(btn, null, [0, 'save', {}, DROP]);
        expect(btn.hasAttribute('az-busy')).toBe(false);
        mod.executeJS(btn, null, [0, 'save', {}, DROP]);
        expect(dropped).toHaveLength(0);
    });

    describe('what counts as a repeat', () => {
        it('is per holder: another trigger of the same event is not one', async () => {
            await connect(
                `<button id="a" az-click='${SAVE}'>A</button><button id="b" az-click='${SAVE}'>B</button>`,
            );
            document.getElementById('a')?.click();
            document.getElementById('b')?.click();
            expect(mock.sent()).toHaveLength(2);
        });

        it('is per event name and per target view', async () => {
            await connect(
                `<button id="b" az-click='${cmd([
                    [0, 'save', {}, DROP],
                    [0, 'delete', {}, DROP],
                ])}'>B</button>
                 <button id="t" az-target="other" az-click='${SAVE}'>T</button>`,
            );
            const b = /** @type {HTMLElement} */ (document.getElementById('b'));
            b.click();
            expect(mock.sent().map((f) => f[1])).toEqual(['save', 'delete']);
            b.click();
            expect(mock.sent()).toHaveLength(2);
            document.getElementById('t')?.click();
            expect(mock.sent().map((f) => f[0])).toEqual(['page', 'page', 'other']);
        });

        it('holds an az-submit event on its form', async () => {
            await connect(
                `<form id="f" az-submit='${SAVE}'><input name="q" /><button id="s">Go</button></form>`,
            );
            const form = /** @type {HTMLFormElement} */ (document.getElementById('f'));
            const dropped = droppedEvents();
            submit(form);
            submit(form);
            expect(mock.sent()).toHaveLength(1);
            expect(form.hasAttribute('az-busy')).toBe(true);
            expect(dropped[0].target).toBe(form);
        });

        it('holds a server effect on the page, never marking <html>', async () => {
            await connect('');
            const dropped = droppedEvents();
            const effect = [0, 'tick', {}, DROP];
            mock.reply(null, null, [effect]);
            mock.reply(null, null, [effect]);
            expect(mock.sent()).toHaveLength(1);
            expect(document.documentElement.hasAttribute('az-busy')).toBe(false);
            expect(dropped).toHaveLength(1);
            expect(dropped[0].target).toBe(document);
            mock.reply(mock.sent()[0][3]);
            mock.reply(null, null, [effect]);
            expect(mock.sent()).toHaveLength(2);
        });

        it('shares az-busy with a fetch the holder has in flight', async () => {
            const originalFetch = globalThis.fetch;
            /** @type {Array<(r: any) => void>} */
            const pending = [];
            globalThis.fetch = vi.fn(() => new Promise((resolve) => pending.push(resolve)));
            try {
                await connect(
                    `<form id="f" method="post" az-submit='${cmd([
                        [22, '/save', {}],
                        [0, 'saved', {}, DROP],
                    ])}'></form>`,
                );
                const form = /** @type {HTMLFormElement} */ (document.getElementById('f'));
                const settleFetch = async (i) => {
                    pending[i]({ ok: true, status: 200, text: () => Promise.resolve('') });
                    await new Promise((r) => setTimeout(r, 0));
                };
                // The event outlives the request...
                submit(form);
                await settleFetch(0);
                expect(form.hasAttribute('az-busy')).toBe(true);
                mock.reply(mock.sent()[0][3]);
                expect(form.hasAttribute('az-busy')).toBe(false);
                // ...and the request outlives the event.
                submit(form);
                mock.reply(mock.sent()[1][3]);
                expect(form.hasAttribute('az-busy')).toBe(true);
                await settleFetch(1);
                expect(form.hasAttribute('az-busy')).toBe(false);
            } finally {
                globalThis.fetch = originalFetch;
            }
        });
    });

    describe('pacing', () => {
        it('judges a debounced event when it fires, not when it is raised', async () => {
            vi.useFakeTimers();
            await connect(
                `<input id="q" name="q" az-debounce="100" az-input='${cmd([0, 'search', {}, DROP])}' />`,
            );
            const q = /** @type {HTMLInputElement} */ (document.getElementById('q'));
            const dropped = droppedEvents();
            const input = (v) => {
                q.value = v;
                q.dispatchEvent(new Event('input', { bubbles: true }));
            };
            // A raise the debounce replaces was never sent: no repeat.
            input('a');
            input('ab');
            vi.advanceTimersByTime(100);
            expect(mock.sent().map((f) => f[2])).toEqual([{ value: 'ab' }]);
            expect(dropped).toHaveLength(0);
            // Fired while the first awaits its reply: a repeat.
            input('abc');
            vi.advanceTimersByTime(100);
            expect(mock.sent()).toHaveLength(1);
            expect(dropped).toHaveLength(1);
            mock.reply(mock.sent()[0][3]);
            input('abcd');
            vi.advanceTimersByTime(100);
            expect(mock.sent().map((f) => f[2])).toEqual([{ value: 'ab' }, { value: 'abcd' }]);
        });
    });

    describe('server-owned controls', () => {
        it('a dropped repeat lets go of the controls it carried', async () => {
            await connect(
                `<form id="f" az-submit='${SAVE}'>
                    <input az="0" name="q" value="5" az-ctl="value" />
                 </form>`,
            );
            const form = /** @type {HTMLFormElement} */ (document.getElementById('f'));
            const el = /** @type {HTMLInputElement} */ (form.querySelector('input'));
            el.value = 'a';
            submit(form);
            el.value = 'ab';
            submit(form);
            expect(mock.sent()).toHaveLength(1);
            // The answer to the first send leaves what the user typed since.
            mock.reply(mock.sent()[0][3]);
            expect(el.value).toBe('ab');
            // Nothing carries the control any more: a server write reaches it.
            mock.reply(null, [[mod.OP.SET_ATTR, 'page:0', 'value', 'srv']]);
            expect(el.value).toBe('srv');
        });
    });
});

describe('push_event on_repeat: allow', () => {
    it('sends every repeat with no reply ref and marks nothing', async () => {
        await connect(`<button id="b" az-click='${cmd([0, 'save', {}])}'>Save</button>`);
        const btn = /** @type {HTMLElement} */ (document.getElementById('b'));
        btn.click();
        btn.click();
        expect(mock.sent()).toEqual([
            ['page', 'save', {}],
            ['page', 'save', {}],
        ]);
        expect(btn.hasAttribute('az-busy')).toBe(false);
    });
});

describe('push_event on_repeat on a page with no server', () => {
    it('takes no slot: no az-busy, and a repeat is not reported as one', async () => {
        vi.resetModules();
        const { start } = await import('./arizona-static.js');
        document.body.innerHTML =
            `<div id="v" az-view><button id="b" az-click='${SAVE}'>Save</button></div>` +
            `<script type="application/json" id="arizona-attrs">["az-click"]</script>`;
        const stop = start();
        try {
            vi.spyOn(console, 'warn').mockImplementation(() => {});
            const dropped = droppedEvents();
            const btn = /** @type {HTMLElement} */ (document.getElementById('b'));
            btn.click();
            expect(btn.hasAttribute('az-busy')).toBe(false);
            btn.click();
            expect(dropped).toHaveLength(0);
        } finally {
            stop();
        }
    });
});
