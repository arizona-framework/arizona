import { afterEach, describe, expect, it, vi } from 'vitest';

/**
 * A module instance with no delegation history: the delegated event registry is
 * module state and only grows, so a shared import would make what a test sees
 * depend on the tests before it.
 * @returns {Promise<typeof import('./arizona-static.js')>}
 */
async function fresh() {
    vi.resetModules();
    return await import('./arizona-static.js');
}

/** The names arizona_static embeds for a page. @param {string[]} names */
function embed(names) {
    return `<script type="application/json" id="arizona-attrs">${JSON.stringify(names)}</script>`;
}

/** A command list as an attribute value. @param {*} cmds */
const attr = (cmds) => JSON.stringify(cmds).replace(/'/g, '&#39;');

/** @param {string} sel */
function dblclick(sel) {
    document.querySelector(sel)?.dispatchEvent(new MouseEvent('dblclick', { bubbles: true }));
}

/** @param {string} sel */
function click(sel) {
    /** @type {HTMLElement} */ (document.querySelector(sel)).click();
}

/** @type {(() => void) | null} */
let stop = null;

afterEach(() => {
    stop?.();
    stop = null;
    document.body.innerHTML = '';
    vi.restoreAllMocks();
});

describe('static client (no server)', () => {
    // A select whose options the server could not all mark keeps its value; the
    // generated page marks them, and start() keeps a form reset on that choice.
    it('applies a select value to the options it names', async () => {
        const { start } = await fresh();
        document.body.innerHTML =
            '<form><select value="b"><option>a</option><option>b</option></select></form>' +
            embed([]);
        stop = start();
        const sel = /** @type {HTMLSelectElement} */ (document.querySelector('select'));
        expect(sel.value).toBe('b');
        sel.value = 'a';
        /** @type {HTMLFormElement} */ (document.querySelector('form')).reset();
        expect(sel.value).toBe('b');
    });

    it('delegates the events the page embeds, running their commands', async () => {
        const { start } = await fresh();
        document.body.innerHTML =
            `<button id="b" az-dblclick='${attr([1, '#panel'])}'>t</button>` +
            '<div id="panel"></div>' +
            embed(['az-dblclick']);
        stop = start();
        dblclick('#b');
        expect(document.getElementById('panel')?.hidden).toBe(true);
    });

    // No server owns a control here, so what the user types into one marked
    // server-owned stays when focus leaves it.
    it('keeps what the user types into a server-owned control', async () => {
        const { start } = await fresh();
        document.body.innerHTML =
            '<div id="v" az-view az="0"><input id="q" az="1" value="5" az-ctl="value"></div>' +
            embed([]);
        stop = start();
        const q = /** @type {HTMLInputElement} */ (document.querySelector('#q'));
        q.focus();
        q.value = '999';
        q.dispatchEvent(new Event('input', { bubbles: true }));
        q.blur();
        expect(q.value).toBe('999');
    });

    // Nothing holds a write here, so a set_attr reaches what the control shows.
    it('writes a control a set_attr command names, typed into or not', async () => {
        const { start } = await fresh();
        document.body.innerHTML =
            '<div id="v" az-view az="0"><input id="q" az="1" value="5" az-ctl="value">' +
            `<button id="b" az-dblclick='${attr([7, '#q', 'value', ''])}'>clear</button></div>` +
            embed(['az-dblclick']);
        stop = start();
        const q = /** @type {HTMLInputElement} */ (document.querySelector('#q'));
        q.focus();
        q.value = 'typed';
        q.dispatchEvent(new Event('input', { bubbles: true }));
        dblclick('#b');
        expect(q.value).toBe('');
    });

    it('switches a ?local tab in the browser', async () => {
        const { start, get } = await fresh();
        document.body.innerHTML =
            '<div id="v" az-view az="0">' +
            `<button id="b" az-click='${attr([17, 'tab', 'code'])}'>Code</button>` +
            `<span az="1" az-local='{"c":{"0":"tab"}}'><!--az:1-->preview<!--/az--></span>` +
            '</div>' +
            embed(['az-click']);
        stop = start();
        click('#b');
        expect(document.querySelector('span')?.textContent).toBe('code');
        expect(get('v', 'tab')).toBe('code');
    });

    it('does not delegate a name the page does not embed', async () => {
        const { start } = await fresh();
        // A valid command list under a name nothing proved: it is data, never run.
        document.body.innerHTML =
            `<button id="b" az-dblclick='${attr([1, '#panel'])}'>t</button>` +
            '<div id="panel"></div>' +
            embed(['az-click']);
        stop = start();
        dblclick('#b');
        expect(document.getElementById('panel')?.hidden).toBe(false);
    });

    it('drops a push_event with a warning: there is no server', async () => {
        const { start } = await fresh();
        const warn = vi.spyOn(console, 'warn').mockImplementation(() => {});
        document.body.innerHTML =
            `<div id="v" az-view az="0"><button id="b" az-click='${attr([0, 'save'])}'>s</button></div>` +
            embed(['az-click']);
        stop = start();
        click('#b');
        expect(warn).toHaveBeenCalledWith(expect.stringContaining('push_event "save" dropped'));
    });

    it('loads a navigate target as a page, refusing a non-web URL', async () => {
        const { start } = await fresh();
        const warn = vi.spyOn(console, 'warn').mockImplementation(() => {});
        document.body.innerHTML =
            `<button id="b" az-click='${attr([10, 'javascript:alert(1)'])}'>n</button>` +
            embed(['az-click']);
        stop = start();
        click('#b');
        expect(warn).toHaveBeenCalledWith(
            '[arizona] refused non-http(s) URL:',
            'javascript:alert(1)',
        );
    });

    it('takes the names from start() over the embedded ones', async () => {
        const { start } = await fresh();
        document.body.innerHTML =
            `<button id="b" az-dblclick='${attr([1, '#panel'])}'>t</button>` +
            '<div id="panel"></div>';
        stop = start({ attrs: ['az-dblclick'] });
        dblclick('#b');
        expect(document.getElementById('panel')?.hidden).toBe(true);
    });

    it('delegates nothing when the embedded names are not a JSON list', async () => {
        const { start } = await fresh();
        const warn = vi.spyOn(console, 'warn').mockImplementation(() => {});
        document.body.innerHTML =
            `<button id="b" az-dblclick='${attr([1, '#panel'])}'>t</button>` +
            '<div id="panel"></div>' +
            '<script type="application/json" id="arizona-attrs">not json</script>';
        stop = start();
        dblclick('#b');
        expect(document.getElementById('panel')?.hidden).toBe(false);
        expect(warn).toHaveBeenCalledWith(expect.stringContaining('not a JSON list'));
    });

    it('delegates nothing extra when the page embeds no names', async () => {
        const { start } = await fresh();
        document.body.innerHTML =
            `<button id="b" az-dblclick='${attr([1, '#panel'])}'>t</button>` +
            '<div id="panel"></div>';
        stop = start();
        dblclick('#b');
        expect(document.getElementById('panel')?.hidden).toBe(false);
    });

    it('reads only the names in an embedded list, ignoring anything else', async () => {
        const { start } = await fresh();
        document.body.innerHTML =
            `<button id="b" az-dblclick='${attr([1, '#panel'])}'>t</button>` +
            `<button id="c" az-mouseup='${attr([1, '#other'])}'>u</button>` +
            '<div id="panel"></div><div id="other"></div>' +
            embed(/** @type {*} */ (['az-dblclick', 42, null]));
        stop = start();
        dblclick('#b');
        expect(document.getElementById('panel')?.hidden).toBe(true);
    });

    it('delegates nothing extra when the embedded JSON is not a list', async () => {
        const { start } = await fresh();
        document.body.innerHTML =
            `<button id="b" az-dblclick='${attr([1, '#panel'])}'>t</button>` +
            '<div id="panel"></div>' +
            '<script type="application/json" id="arizona-attrs">{"az-dblclick":true}</script>';
        stop = start();
        dblclick('#b');
        expect(document.getElementById('panel')?.hidden).toBe(false);
    });

    it('stop() unbinds the events and destroys the hooks', async () => {
        const { start, hooks } = await fresh();
        const mounted = vi.fn();
        const destroyed = vi.fn();
        hooks.H = { mounted, destroyed };
        document.body.innerHTML =
            `<button id="b" az-click='${attr([1, '#panel'])}'>t</button>` +
            '<div id="panel"></div><div az-hook="H"></div>' +
            embed(['az-click']);
        const stopIt = start();
        expect(mounted).toHaveBeenCalledOnce();
        stopIt();
        expect(destroyed).toHaveBeenCalledOnce();
        click('#b');
        expect(document.getElementById('panel')?.hidden).toBe(false);
        stopIt(); // idempotent
        expect(destroyed).toHaveBeenCalledOnce();
    });

    // A view transition waits for the reply a server sends; here none comes, so
    // whatever it wraps runs as it would unwrapped.
    it('runs what a transition wraps: a toggle', async () => {
        const { start } = await fresh();
        document.body.innerHTML =
            `<button id="b" az-click='${attr([20, {}, [1, '#panel']])}'>t</button>` +
            '<div id="panel"></div>' +
            embed(['az-click']);
        stop = start();
        click('#b');
        expect(document.getElementById('panel')?.hidden).toBe(true);
    });

    it('runs what a transition wraps: a push_event, dropped', async () => {
        const { start } = await fresh();
        const warn = vi.spyOn(console, 'warn').mockImplementation(() => {});
        document.body.innerHTML =
            `<div id="v" az-view az="0"><button id="b" az-click='${attr([20, {}, [0, 'go']])}'>s</button></div>` +
            embed(['az-click']);
        stop = start();
        click('#b');
        expect(warn).toHaveBeenCalledWith(expect.stringContaining('push_event "go" dropped'));
    });

    it('runs what a transition wraps: a navigate, loaded as a page', async () => {
        const { start } = await fresh();
        const warn = vi.spyOn(console, 'warn').mockImplementation(() => {});
        document.body.innerHTML =
            `<button id="b" az-click='${attr([20, {}, [10, 'javascript:void(0)']])}'>n</button>` +
            embed(['az-click']);
        stop = start();
        click('#b');
        expect(warn).toHaveBeenCalledWith(
            '[arizona] refused non-http(s) URL:',
            'javascript:void(0)',
        );
    });

    it('a second start() stops the first', async () => {
        const { start } = await fresh();
        const warn = vi.spyOn(console, 'warn').mockImplementation(() => {});
        document.body.innerHTML =
            `<button id="b" az-click='${attr([1, '#panel'])}'>t</button>` +
            '<div id="panel"></div>' +
            embed(['az-click']);
        start();
        stop = start();
        expect(warn).toHaveBeenCalledWith(expect.stringContaining('called twice'));
        // One binding, not two: a click toggles once.
        click('#b');
        expect(document.getElementById('panel')?.hidden).toBe(true);
    });
});
