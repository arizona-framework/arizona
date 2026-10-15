import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';

// Op codes (must match include/arizona.hrl / include/arizona_effect.hrl).
const OP_TEXT = 0;
const OP_REPLACE = 8;
const JS_PUSH_EVENT = 0;
const JS_NAVIGATE = 10;
const JS_PATCH = 24;

// Between posting a navigate frame and receiving its OP_REPLACE the old DOM is
// still on screen, but the server has already replaced the root view. An event
// raised there is dispatched by view id alone and would reach the NEW root (which
// often shares the id), so the client drops it, as a full page load would.

const PAGE = '<main id="page" az-view><button az="0" az-click=\'[0,"bump"]\'></button></main>';
const NEW_PAGE = '<main id="page" az-view><button az="0" az-click=\'[0,"inc"]\'></button></main>';

/** @type {any} */
let mod = null;
/** @type {any} */
let mock = null;

/** Mock the Worker + connect(); mirrors the harness in arizona.test.js. */
function setupMockWorker() {
    /** @type {Array<Array<*>>} */
    const posted = [];
    /** @type {any} */
    let onmessage = null;
    const worker = {
        postMessage: (/** @type {Array<*>} */ d) => posted.push(d),
        set onmessage(fn) {
            onmessage = fn;
        },
        get onmessage() {
            return onmessage;
        },
        terminate: vi.fn(),
    };
    const OrigWorker = globalThis.Worker;
    globalThis.Worker = /** @type {any} */ (
        function () {
            return worker;
        }
    );
    const disconnect = mod.connect('/ws');
    return {
        open: (reconnect = false) => worker.onmessage({ data: [1, reconnect] }),
        close: (/** @type {number} */ code) => worker.onmessage({ data: [2, code] }),
        frame: (/** @type {*} */ ops, /** @type {*} */ effects = null, resync = false) =>
            worker.onmessage({ data: [0, ops, effects, resync] }),
        sent: () => posted.filter((d) => d[0] === 1).map((d) => JSON.parse(d[1])),
        restore: () => {
            disconnect();
            globalThis.Worker = OrigWorker;
        },
    };
}

/** A fresh module, connected, with the old page on screen. */
async function connected() {
    vi.resetModules();
    mod = await import('./arizona.js');
    document.body.innerHTML = PAGE;
    mock = setupMockWorker();
    mock.open();
}

/** The view events sent so far (navigate/patch frames excluded). */
const events = () =>
    mock.sent().filter((/** @type {Array<*>} */ m) => m[0] !== 'navigate' && m[0] !== 'patch');
const click = () => /** @type {HTMLElement} */ (document.querySelector('button')).click();
const navigate = (/** @type {string} */ path) => mod.applyEffects([[JS_NAVIGATE, path, {}]]);
const replace = () => mock.frame([[OP_REPLACE, 'page', NEW_PAGE]]);

beforeEach(() => {
    history.replaceState(null, '', '/start');
    vi.spyOn(window, 'scrollTo').mockImplementation(() => {});
});

afterEach(() => {
    if (mock) mock.restore();
    mock = null;
    vi.useRealTimers();
    vi.restoreAllMocks();
    history.replaceState(null, '', '/');
});

describe('navigate page-swap window', () => {
    it('drops an old-page event sent before the OP_REPLACE, sends one after', async () => {
        await connected();
        navigate('/next');
        expect(mock.sent()).toContainEqual(['navigate', { path: '/next', qs: '' }]);

        click();
        mod.pushEventTo('page', 'bump', {});
        mod.pushEvent('bump', {});
        mod.applyEffects([[JS_PUSH_EVENT, 'relayed', {}]]);
        expect(events()).toEqual([]);

        replace();
        click();
        expect(events()).toEqual([['page', 'inc', {}]]);
    });

    it('an az-navigate click arms the window, and a hook pushEvent is dropped', async () => {
        await connected();
        /** @type {any} */
        let hook = null;
        mod.hooks.Probe = {
            mounted() {
                hook = this;
            },
        };
        document.body.insertAdjacentHTML(
            'beforeend',
            '<div id="h" az-hook="Probe"></div><a id="l" az-navigate href="/next">n</a>',
        );
        mod.mountHooks(document);
        /** @type {HTMLElement} */ (document.getElementById('l')).click();
        expect(mock.sent()).toContainEqual(['navigate', { path: '/next', qs: '' }]);

        click();
        hook.pushEvent('hooked', {});
        expect(events()).toEqual([]);
        replace();
        hook.pushEvent('hooked', {});
        expect(events()).toEqual([[null, 'hooked', {}]]);
    });

    it('an old-page diff frame ahead of the reply does not close the window', async () => {
        await connected();
        navigate('/next');
        mock.frame([[OP_TEXT, 'page:0', 'tick']]);
        click();
        expect(events()).toEqual([]);
    });

    it('a push_event effect in the OP_REPLACE frame goes out', async () => {
        await connected();
        navigate('/next');
        mock.frame([[OP_REPLACE, 'page', NEW_PAGE]], [[JS_PUSH_EVENT, 'ready', {}]]);
        expect(events()).toEqual([[null, 'ready', {}]]);
    });

    it('a patch does not block events', async () => {
        await connected();
        mod.applyEffects([[JS_PATCH, '/start?tab=2', {}]]);
        expect(mock.sent()).toContainEqual(['patch', { path: '/start', qs: 'tab=2' }]);
        click();
        expect(events()).toEqual([['page', 'bump', {}]]);
    });

    it('a full-page navigate reply closes the window (the server kept the view)', async () => {
        await connected();
        navigate('/next');
        const assign = vi.fn();
        vi.stubGlobal('location', { assign });
        mock.frame(null, [[JS_NAVIGATE, '/next', { full: true }]]);
        vi.unstubAllGlobals();
        expect(assign).toHaveBeenCalledWith('/next');
        click();
        expect(events()).toEqual([['page', 'bump', {}]]);
    });

    it('a redirect reply re-arms the window for its own navigate', async () => {
        await connected();
        navigate('/admin');
        mock.frame(null, [[JS_NAVIGATE, '/login']]);
        expect(mock.sent()).toContainEqual(['navigate', { path: '/login', qs: '' }]);
        click();
        expect(events()).toEqual([]);
        replace();
        click();
        expect(events()).toEqual([['page', 'inc', {}]]);
    });

    it('a close keeps the window until the reconnect resync replaces the page', async () => {
        await connected();
        navigate('/next');
        mock.close(4500);
        mock.open(true);
        click();
        expect(events()).toEqual([]);
        mock.frame([[OP_REPLACE, 'page', NEW_PAGE]], null, true);
        click();
        expect(events()).toEqual([['page', 'inc', {}]]);
    });

    it('a navigate posted while disconnected arms nothing', async () => {
        await connected();
        mock.close(1006);
        navigate('/next');
        mock.open(true);
        mod.pushEvent('bump', {});
        expect(events()).toEqual([[null, 'bump', {}]]);
    });

    it('popstate arms the window for a navigate but not for a patch', async () => {
        await connected();
        history.pushState({ _azNav: 'patch' }, '', '/start?tab=3');
        window.dispatchEvent(new PopStateEvent('popstate', { state: { _azNav: 'patch' } }));
        mod.pushEvent('a', {});
        history.pushState(null, '', '/other');
        window.dispatchEvent(new PopStateEvent('popstate', { state: null }));
        mod.pushEvent('b', {});
        expect(mock.sent()).toContainEqual(['navigate', { path: '/other', qs: '' }]);
        expect(events()).toEqual([[null, 'a', {}]]);
    });

    it('a debounced send raised before the navigate is dropped after the swap', async () => {
        vi.useFakeTimers();
        await connected();
        document.body.innerHTML =
            '<main id="page" az-view><input id="inp" az-change=\'[[0,"change"]]\' az-debounce="200" /></main>';
        /** @type {HTMLElement} */ (document.getElementById('inp')).dispatchEvent(
            new Event('change', { bubbles: true }),
        );
        navigate('/next');
        replace();
        vi.advanceTimersByTime(300);
        expect(events()).toEqual([]);
    });

    it('disconnect() clears a pending window', async () => {
        await connected();
        navigate('/next');
        mock.restore();
        mock = setupMockWorker();
        mock.open();
        mod.pushEvent('bump', {});
        expect(events()).toEqual([[null, 'bump', {}]]);
    });
});
