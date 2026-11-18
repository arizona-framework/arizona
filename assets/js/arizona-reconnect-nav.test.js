import { createRequire } from 'node:module';
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';
import { embedAttrs } from './test-proofs.js';

// jsdom's own wrapper-to-impl map: a test cannot make a trusted event through the
// DOM API (dispatchEvent always clears `isTrusted`), so it dispatches the impl.
// This reaches into jsdom internals (`lib/generated/idl/utils.js`'s
// `implForWrapper`, the impl's `isTrusted` and `_dispatch`), as of the jsdom 31.0.0
// the lockfile pins: a jsdom upgrade may move them.
const { implForWrapper } = createRequire(import.meta.url)('jsdom/lib/generated/idl/utils.js');

/**
 * A person's input on `target`: a trusted event, as only the browser makes.
 * @param {'pointerdown'|'keydown'|'click'|'input'|'change'} [type]
 * @param {{repeat?: boolean}} [init]
 * @param {Element} [target]
 */
function person(type = 'pointerdown', init = {}, target = document.body) {
    const e =
        type === 'keydown'
            ? new KeyboardEvent(type, { bubbles: true, composed: true, ...init })
            : new Event(type, { bubbles: true, composed: true });
    implForWrapper(e).isTrusted = true;
    implForWrapper(target)._dispatch(implForWrapper(e));
}

// Op codes (must match include/arizona_wire.hrl / include/arizona_effect.hrl).
const OP_REPLACE = 8;
const JS_NAVIGATE = 10;
const JS_TRANSITION = 20;
const JS_FETCH = 22;
const JS_PATCH = 24;

// A reconnect resyncs the page on screen -- the view the DOM holds -- and a
// navigation the socket could not carry (issued while it was down, or owed a reply
// the close lost) goes out once that page is live again.

const PAGE = '<main id="page" az-view><button az="0" az-click=\'[0,"bump"]\'></button></main>';
const NEXT = '<main id="next" az-view><p id="x">next</p></main>';

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
    let open = false;
    // As the worker keeps them: the navigate/patch frames sent on the open socket
    // (ref -> path).
    /** @type {Map<number, string>} */
    const navRefs = new Map();
    const worker = {
        postMessage: (/** @type {Array<*>} */ d) => {
            posted.push(d);
            if (d[0] === 1 && d[2] && open) navRefs.set(d[2].ref, d[2].path);
        },
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
    const unembed = embedAttrs();
    const disconnect = mod.connect('/ws');
    unembed();
    const sent = () => posted.filter((d) => d[0] === 1).map((d) => JSON.parse(d[1]));
    // As the worker reports it: the reply to a navigate/patch frame sent on the open
    // socket put its page on screen unless it navigates again.
    /** @type {(ops: *, effects?: *, resync?: boolean, ref?: number|null) => void} */
    const frame = (ops, effects = null, resync = false, ref = null) => {
        const again = (effects ?? []).some(
            (/** @type {Array<*>} */ c) => c[0] === JS_NAVIGATE || c[0] === JS_PATCH,
        );
        const path = ref === null ? undefined : navRefs.get(ref);
        const shown = path !== undefined && navRefs.delete(/** @type {number} */ (ref)) && !again;
        worker.onmessage({ data: [0, ops, effects, resync, null, ref, shown] });
    };
    return {
        posted,
        open: (reconnect = false) => {
            open = true;
            worker.onmessage({ data: [1, reconnect] });
        },
        close: (/** @type {number} */ code) => {
            open = false;
            navRefs.clear();
            worker.onmessage({ data: [2, code] });
        },
        frame,
        /** The worker starts a (re)connect attempt. */
        /** The worker's socket is gone, before the main thread hears of it. */
        setOpen: (/** @type {boolean} */ v) => {
            open = v;
        },
        /** A message from the worker, as it is. */
        raw: (/** @type {Array<*>} */ data) => worker.onmessage({ data }),
        dial: (capped = false) => worker.onmessage({ data: [4, capped] }),
        /** A reconnect attempt that starts (at the backoff cap or not) and fails without opening. */
        failAttempt: (capped = false) => {
            worker.onmessage({ data: [4, capped] });
            worker.onmessage({ data: [2, 1006] });
        },
        /** The reconnect's resync: the page on screen, re-rendered. */
        resync: (html = PAGE) => frame([[OP_REPLACE, 'page', html]], null, true),
        sent,
        /** The navigate/patch frames sent so far. */
        navs: () => sent().filter((m) => m[0] === 'navigate' || m[0] === 'patch'),
        restore: () => {
            disconnect();
            globalThis.Worker = OrigWorker;
        },
    };
}

/** A fresh module, connected (and mounted), with the page on screen. */
async function connected() {
    vi.resetModules();
    mod = await import('./arizona.js');
    document.body.innerHTML = PAGE;
    mock = setupMockWorker();
    mock.open();
    mock.frame(null); // the first connect's frame: the view mounted
}

/** Down with a reconnect due. */
async function down() {
    await connected();
    mock.close(1006);
}

const navigate = (/** @type {string} */ path, opts = {}) =>
    mod.applyEffects([[JS_NAVIGATE, path, opts]]);
const events = () =>
    mock.sent().filter((/** @type {Array<*>} */ m) => m[0] !== 'navigate' && m[0] !== 'patch');
const click = () => /** @type {HTMLElement} */ (document.querySelector('button')).click();
/** The URL the reconnect dials, as the worker was last handed it. */
const dialed = () => mock.posted.filter((/** @type {Array<*>} */ d) => d[0] === 0).at(-1)[1];

beforeEach(() => {
    history.replaceState(null, '', '/start');
    vi.spyOn(window, 'scrollTo').mockImplementation(() => {});
});

afterEach(() => {
    if (mock) mock.restore();
    mock = null;
    vi.useRealTimers();
    vi.unstubAllGlobals();
    vi.restoreAllMocks();
    history.replaceState(null, '', '/');
});

describe('a navigation while down', () => {
    it('is not sent, and goes out once the resync puts the page on screen back', async () => {
        await down();
        navigate('/next');
        expect(location.pathname).toBe('/next');
        expect(mock.navs()).toEqual([]);
        // Nothing moves the worker's reconnect URL off the page on screen.
        expect(mock.posted.some((/** @type {Array<*>} */ d) => d[0] === 3)).toBe(false);
        mock.open(true);
        expect(mock.navs()).toEqual([]);
        mock.resync();
        expect(mock.navs()).toEqual([['navigate', { path: '/next', qs: '' }, expect.any(Number)]]);
    });

    it('sends only the last of several, and history keeps them all', async () => {
        await down();
        const len0 = history.length;
        navigate('/a');
        navigate('/b?x=1');
        mod.applyEffects([[JS_PATCH, '/c', {}]]);
        expect(history.length).toBe(len0 + 3);
        mock.open(true);
        mock.resync();
        expect(mock.navs()).toEqual([['patch', { path: '/c', qs: '' }, expect.any(Number)]]);
    });

    it('naming the page on screen is dropped', async () => {
        await down();
        navigate('/next');
        navigate('/start');
        mock.open(true);
        mock.resync();
        expect(mock.navs()).toEqual([]);
    });

    it('goes out in the resync apply, so the window shuts before any user event', async () => {
        await down();
        navigate('/next');
        mock.open(true);
        mock.resync();
        click();
        expect(events()).toEqual([]);
        mock.frame([[OP_REPLACE, 'page', NEXT]], null, false, mock.navs()[0][2]);
        expect(document.getElementById('next')).not.toBeNull();
    });

    it('goes out after a resync whose form holds a bad az-change', async () => {
        // A restore that cannot read a form's az-change warns and moves on: the
        // rest of the resync frame, the pending navigation included, still runs.
        vi.spyOn(console, 'warn').mockImplementation(() => {});
        const view =
            '<main id="page" az-view><form id="f" az-change=\'[0,\'><input name="x" value=""></form></main>';
        await connected();
        document.body.innerHTML = view;
        mock.close(1006);
        navigate('/next');
        mock.open(true);
        mock.resync(view);
        expect(mock.navs().map((/** @type {Array<*>} */ m) => m[1].path)).toEqual(['/next']);
    });

    it('is superseded by a navigation sent live before the resync', async () => {
        await down();
        navigate('/next');
        mock.open(true);
        navigate('/other');
        mock.resync();
        expect(mock.navs().map((/** @type {Array<*>} */ m) => m[1].path)).toEqual(['/other']);
    });

    it('from an az-navigate click is queued, not left to the browser', async () => {
        await down();
        const a = document.createElement('a');
        a.setAttribute('az-navigate', '');
        a.setAttribute('href', '/next');
        document.body.appendChild(a);
        const evt = new MouseEvent('click', { bubbles: true, cancelable: true, button: 0 });
        a.dispatchEvent(evt);
        expect(evt.defaultPrevented).toBe(true);
        expect(location.pathname).toBe('/next');
        mock.open(true);
        mock.resync();
        expect(mock.navs()).toEqual([['navigate', { path: '/next', qs: '' }, expect.any(Number)]]);
    });

    it('keeps its scroll for its own reply, not the resync', async () => {
        await down();
        navigate('/next');
        mock.open(true);
        mock.resync();
        expect(window.scrollTo).not.toHaveBeenCalled();
        mock.frame([[OP_REPLACE, 'page', NEXT]], null, false, mock.navs()[0][2]);
        expect(window.scrollTo).toHaveBeenCalledWith(0, 0);
    });

    it('keeps its view transition for its own reply, not the resync', async () => {
        const vt = vi.fn((/** @type {any} */ arg) => {
            (typeof arg === 'object' ? arg.update : arg)();
            return { ready: Promise.resolve(), finished: Promise.resolve() };
        });
        /** @type {any} */ (document).startViewTransition = vt;
        window.matchMedia = /** @type {any} */ (vi.fn().mockReturnValue({ matches: false }));
        try {
            await down();
            mod.applyEffects([[JS_TRANSITION, {}, [[JS_NAVIGATE, '/next', {}]]]]);
            mock.open(true);
            mock.resync();
            expect(vt).not.toHaveBeenCalled();
            mock.frame([[OP_REPLACE, 'page', NEXT]], null, false, mock.navs()[0][2]);
            expect(vt).toHaveBeenCalledOnce();
        } finally {
            delete (/** @type {any} */ (document).startViewTransition);
        }
    });

    it('after a deliberate end (1000) loads its URL', async () => {
        await connected();
        mock.close(1000);
        const assign = vi.fn();
        vi.stubGlobal('location', {
            assign,
            href: 'http://localhost/start',
            origin: 'http://localhost',
        });
        navigate('/next');
        await Promise.resolve();
        vi.unstubAllGlobals();
        expect(assign).toHaveBeenCalledWith('http://localhost:3000/next');
        expect(mock.navs()).toEqual([]);
    });

    it('a 1000 close with one pending loads the address bar URL', async () => {
        await down();
        navigate('/next');
        mock.open(true);
        const replace = vi.fn();
        const reload = vi.fn();
        vi.stubGlobal('location', { replace, reload, href: 'http://localhost/next' });
        mock.close(1000);
        await Promise.resolve();
        vi.unstubAllGlobals();
        // Loaded by URL: it answers nothing this page sent, so no reload.
        expect(replace).toHaveBeenCalledWith('http://localhost/next');
        expect(reload).not.toHaveBeenCalled();
    });

    it('is cleared by disconnect(), which leaves nothing to run', async () => {
        await down();
        navigate('/next');
        mock.restore();
        history.replaceState(null, '', '/start');
        mock = setupMockWorker();
        mock.open();
        mock.frame(null);
        expect(mock.navs()).toEqual([]);
    });
});

// Until the page has mounted once nothing proves a socket will ever open (a proxy
// that never upgrades), so a navigation is served as with no socket at all: the
// page it names loads, and a link is left to the browser.
describe('a navigation before the page has mounted', () => {
    /** The page, connecting: never opened, or opened and closed before a mount. */
    async function unmounted(opened) {
        vi.resetModules();
        mod = await import('./arizona.js');
        document.body.innerHTML = PAGE;
        mock = setupMockWorker();
        if (opened) {
            mock.open();
            mock.close(1006);
        }
    }
    /** Stub location around `fn`, returning the loads it asked for. */
    async function loads(fn) {
        const assign = vi.fn();
        const replace = vi.fn();
        const reload = vi.fn();
        const { origin, href, pathname, search, hash } = window.location;
        vi.stubGlobal('location', {
            assign,
            replace,
            reload,
            origin,
            href,
            pathname,
            search,
            hash,
        });
        try {
            fn();
        } finally {
            await Promise.resolve();
            vi.unstubAllGlobals();
        }
        return { assign, replace, reload };
    }

    for (const opened of [false, true]) {
        const when = opened ? 'opened and closed' : 'never opened';
        it(`loads the page of a navigate effect (socket ${when})`, async () => {
            await unmounted(opened);
            const { assign } = await loads(() => navigate('/next'));
            expect(assign).toHaveBeenCalledWith('http://localhost:3000/next');
            expect(location.pathname).toBe('/start');
            mock.open(opened);
            mock.frame(opened ? [[OP_REPLACE, 'page', PAGE]] : null, null, opened);
            expect(mock.navs()).toEqual([]);
        });

        it(`leaves an az-navigate click to the browser (socket ${when})`, async () => {
            await unmounted(opened);
            const a = document.createElement('a');
            a.setAttribute('az-navigate', '');
            a.setAttribute('href', '/next');
            document.body.appendChild(a);
            const evt = new MouseEvent('click', { bubbles: true, cancelable: true, button: 0 });
            a.dispatchEvent(evt);
            expect(location.pathname).toBe('/start');
            expect(mock.navs()).toEqual([]);
            // Left alone: the browser follows the link.
            expect(evt.defaultPrevented).toBe(false);
        });

        it(`reloads on a cross-page popstate (socket ${when})`, async () => {
            await unmounted(opened);
            history.pushState(null, '', '/other');
            const { reload } = await loads(() =>
                window.dispatchEvent(new PopStateEvent('popstate', { state: null })),
            );
            expect(reload).toHaveBeenCalledOnce();
        });
    }
});

// A reconnect attempt that started after a navigation was pended and fails without
// opening says the socket will not carry it soon (an upgrade-only outage: an auth
// gate halting the upgrade): the navigation runs as with no socket, a load of its
// URL, leaving the page the user was already leaving.
describe('a pending navigation whose next reconnect attempt fails', () => {
    /** Stub location around `fn`, returning the loads it asked for. */
    async function loads(fn) {
        const assign = vi.fn();
        const replace = vi.fn();
        const reload = vi.fn();
        const { origin, href, pathname, search, hash } = window.location;
        vi.stubGlobal('location', {
            assign,
            replace,
            reload,
            origin,
            href,
            pathname,
            search,
            hash,
        });
        try {
            fn();
        } finally {
            await Promise.resolve();
            vi.unstubAllGlobals();
        }
        return { assign, replace, reload, href };
    }

    it('waits while the attempt in flight when it was pended fails, loads on the next', async () => {
        await down();
        mock.dial(); // an attempt starts...
        navigate('/next'); // ...the navigation is pended while it is in flight...
        let l = await loads(() => mock.close(1006)); // ...and that attempt fails
        expect(l.replace).not.toHaveBeenCalled();
        expect(l.reload).not.toHaveBeenCalled();
        l = await loads(() => mock.failAttempt());
        expect(l.replace).toHaveBeenCalledWith(l.href);
        expect(l.href).toContain('/next');
    });

    it('loads for a click and a popstate alike', async () => {
        let l;
        // A click.
        history.replaceState(null, '', '/start');
        await down();
        const a = document.createElement('a');
        a.setAttribute('az-navigate', '');
        a.setAttribute('href', '/next');
        document.body.appendChild(a);
        a.dispatchEvent(new MouseEvent('click', { bubbles: true, cancelable: true, button: 0 }));
        l = await loads(() => mock.failAttempt());
        expect(l.replace).toHaveBeenCalledOnce();
        mock.restore();
        // A popstate.
        history.replaceState(null, '', '/start');
        await down();
        history.pushState(null, '', '/other');
        window.dispatchEvent(new PopStateEvent('popstate', { state: null }));
        l = await loads(() => mock.failAttempt());
        expect(l.replace).toHaveBeenCalledOnce();
    });

    it('a lost reply loads only once an attempt at the backoff cap fails', async () => {
        // A reply lost to an ordinary restart: the user issued nothing while down,
        // so the server gets the whole backoff to come back before the page loads.
        await connected();
        navigate('/next');
        let l = await loads(() => mock.close(1006));
        l = await loads(() => mock.failAttempt());
        expect(l.replace).not.toHaveBeenCalled();
        expect(l.reload).not.toHaveBeenCalled();
        l = await loads(() => mock.failAttempt());
        expect(l.replace).not.toHaveBeenCalled();
        expect(l.reload).not.toHaveBeenCalled();
        l = await loads(() => mock.failAttempt(true));
        expect(l.replace).toHaveBeenCalledWith(l.href);
    });

    it('a pending navigation to page#fragment reloads its entry: replace would only scroll', async () => {
        // `location.replace` of the address bar's own URL with its fragment is a
        // same-document fragment navigation, which loads nothing.
        await down();
        navigate('/next#sec');
        mock.dial();
        const l = await loads(() => mock.failAttempt());
        expect(l.href).toContain('/next#sec');
        expect(l.reload).toHaveBeenCalledOnce();
        expect(l.replace).not.toHaveBeenCalled();
    });

    it('an attempt that opens and closes before its mount proof loads nothing', async () => {
        // It opened: the socket is carrying traffic again, so the navigation waits
        // for the next attempt rather than loading.
        await down();
        navigate('/next');
        mock.dial(true);
        const l = await loads(() => {
            mock.open(true);
            mock.close(1006);
        });
        expect(l.replace).not.toHaveBeenCalled();
        expect(l.reload).not.toHaveBeenCalled();
    });

    it('does not load when the attempt opens: the navigation goes out after the resync', async () => {
        await down();
        navigate('/next');
        mock.dial();
        const l = await loads(() => {
            mock.open(true);
            mock.resync();
        });
        expect(l.replace).not.toHaveBeenCalled();
        expect(l.reload).not.toHaveBeenCalled();
        expect(mock.navs().map((/** @type {Array<*>} */ m) => m[1].path)).toEqual(['/next']);
    });
});

// Back (or any navigation) to the page on screen while a navigation is pending
// leaves nothing to carry: the pending one is dropped, so no failed attempt loads it.
describe('a navigation back to the page on screen while one is pending', () => {
    /** Stub location around `fn`, returning the loads it asked for. */
    async function loads(fn) {
        const replace = vi.fn();
        const { origin, href, pathname, search, hash } = window.location;
        vi.stubGlobal('location', {
            replace,
            assign: vi.fn(),
            reload: vi.fn(),
            origin,
            href,
            pathname,
            search,
            hash,
        });
        try {
            fn();
        } finally {
            await Promise.resolve();
            vi.unstubAllGlobals();
        }
        return replace;
    }

    for (const [name, pend] of [
        ['issued while down', () => navigate('/next')],
        ['whose reply a close lost', () => undefined],
    ]) {
        it(`drops the pending one ${name}`, async () => {
            await connected();
            if (name.startsWith('whose')) navigate('/next');
            mock.close(1006);
            pend();
            history.back();
            await vi.waitFor(() => expect(location.pathname).toBe('/start'));
            expect(await loads(() => mock.failAttempt(true))).not.toHaveBeenCalled();
            mock.open(true);
            mock.resync();
            expect(
                mock.navs().filter((/** @type {Array<*>} */ m) => m[1].path === '/next').length,
            ).toBe(name.startsWith('whose') ? 1 : 0);
        });
    }
});

// A crash (4500) that loses a navigation is a mount failure's (4501): the view's
// reaction to it may be what crashed, so it is dropped and the counted, delayed
// load of the lost navigation's page decides -- kept past the resync of the page on
// screen, and in the failsafe delay once it recurs. Only a navigation to the page on
// screen's own path (a navigate or a patch, whatever its query) is dialed instead:
// the same route and layouts, its fresh mount restoring the typed fields.
// The server answers a navigation it ran and could not complete (its mount failed,
// the view crashed reacting to it, a middleware raised) with the full load of its
// page, then closes 4500. A navigation still owed a reply at a close is one it never
// ran, so it goes out again once the page on screen is live.
describe('a navigation and a crash', () => {
    // The loads here answer navigations issued with no input: each test starts with
    // an empty record, so none waits out the bound another test filled.
    beforeEach(() => sessionStorage.removeItem('arizona:nav-loads'));

    /** Run `fn` with `location` stubbed, returning the loads it asked for. */
    async function loads(/** @type {() => void | Promise<void>} */ fn) {
        const reload = vi.fn();
        const replace = vi.fn();
        const assign = vi.fn();
        const { origin, href, pathname, search, hash } = window.location;
        vi.stubGlobal('location', {
            reload,
            replace,
            assign,
            origin,
            href,
            pathname,
            search,
            hash,
        });
        try {
            await fn();
            await Promise.resolve();
        } finally {
            vi.unstubAllGlobals();
        }
        return {
            reload,
            replace,
            assign,
            count: reload.mock.calls.length + replace.mock.calls.length + assign.mock.calls.length,
        };
    }

    for (const code of [4500, 4501]) {
        it(`one still owed a reply at a ${code} goes out again after the resync`, async () => {
            vi.useFakeTimers({ toFake: ['setTimeout', 'clearTimeout'] });
            await connected();
            navigate('/next');
            mock.close(code);
            mock.open(true);
            const l = await loads(() => {
                mock.resync();
                vi.advanceTimersByTime(60000);
            });
            // The resync's mount cancels a 4501's reload of the page on screen.
            expect(l.count).toBe(0);
            expect(mock.navs().map((/** @type {Array<*>} */ m) => m[1].path)).toEqual([
                '/next',
                '/next',
            ]);
        });
    }

    it('a patch owed a reply a crash lost goes out again too, its typed fields kept', async () => {
        await connected();
        document.body.innerHTML =
            '<main id="page" az-view><form id="f"><input name="t" value=""></form></main>';
        /** @type {HTMLInputElement} */ (document.querySelector('input')).value = 'typed';
        mod.applyEffects([[JS_PATCH, '/start?b=1', {}]]);
        mock.close(4500);
        mock.open(true);
        mock.resync('<main id="page" az-view><form id="f"><input name="t" value=""></form></main>');
        expect(mock.navs().map((/** @type {Array<*>} */ m) => [m[0], m[1]])).toEqual([
            ['patch', { path: '/start', qs: 'b=1' }],
            ['patch', { path: '/start', qs: 'b=1' }],
        ]);
        expect(/** @type {HTMLInputElement} */ (document.querySelector('input')).value).toBe(
            'typed',
        );
        // Not on screen until its reply says so: the bfcache respawn dials /start.
        window.dispatchEvent(new PageTransitionEvent('pagehide', { persisted: true }));
        window.dispatchEvent(new PageTransitionEvent('pageshow', { persisted: true }));
        expect(dialed()).not.toContain('b=1');
    });

    it('one the server answers with a full load loads its page once, keeping its entry', async () => {
        // The load names the address bar's page: a reload keeps the entry's state
        // (`_azIdx`, the transition edges), which `location.replace` would drop.
        await connected();
        navigate('/next');
        const ref = mock.navs()[0][2];
        const l = await loads(() =>
            mock.frame(null, [[JS_NAVIGATE, '/next', { full: true, replace: true }]], false, ref),
        );
        expect(l.count).toBe(1);
        expect(l.reload).toHaveBeenCalledOnce();
    });

    it('a full load answering a navigation to page#fragment reloads its entry too', async () => {
        // The frame carries no fragment, so the answer names the page without it: a
        // replace would drop the fragment, the reloaded entry keeps it and scrolls there.
        await connected();
        navigate('/next#sec');
        const ref = mock.navs()[0][2];
        const l = await loads(() =>
            mock.frame(null, [[JS_NAVIGATE, '/next', { full: true, replace: true }]], false, ref),
        );
        expect(l.reload).toHaveBeenCalledOnce();
        expect(l.replace).not.toHaveBeenCalled();
    });

    it('a full load answering a navigation to page? reloads its entry too', async () => {
        // An empty query is part of the page the entry names, but the frame asks for
        // `qs: ''`, so the server's answer names the page without the `?`: what the
        // frame asked for decides, not the URL's spelling.
        await connected();
        navigate('/next?');
        const ref = mock.navs()[0][2];
        expect(location.href).toBe('http://localhost:3000/next?');
        const l = await loads(() =>
            mock.frame(null, [[JS_NAVIGATE, '/next', { full: true, replace: true }]], false, ref),
        );
        expect(l.reload).toHaveBeenCalledOnce();
        expect(l.replace).not.toHaveBeenCalled();
    });

    it('a full load of its page inside a view transition in the reply reloads its entry too', async () => {
        // The transition runs the load in its update callback, after the reply has
        // been applied: it still answers that reply's navigation.
        await connected();
        /** @type {Array<() => void>} */
        const updates = [];
        /** @type {any} */ (document).startViewTransition = (/** @type {() => void} */ fn) => {
            updates.push(fn);
        };
        try {
            navigate('/next');
            const ref = mock.navs()[0][2];
            mock.frame(
                null,
                [[JS_TRANSITION, {}, [JS_NAVIGATE, '/next', { full: true }]]],
                false,
                ref,
            );
            expect(updates.length).toBe(1);
            const l = await loads(() => updates[0]());
            expect(l.reload).toHaveBeenCalledOnce();
            expect(l.count).toBe(1);
        } finally {
            delete (/** @type {any} */ (document).startViewTransition);
        }
    });

    it('a full load answering a navigation to another query of its page replaces the entry', async () => {
        // The query is part of the page the frame asked for: `?a=2` is another page.
        await connected();
        navigate('/next?a=1');
        const ref = mock.navs()[0][2];
        const l = await loads(() =>
            mock.frame(
                null,
                [[JS_NAVIGATE, '/next?a=2', { full: true, replace: true }]],
                false,
                ref,
            ),
        );
        expect(l.reload).not.toHaveBeenCalled();
        expect(l.replace).toHaveBeenCalledWith('http://localhost:3000/next?a=2');
    });

    it('a full load of its page once the address bar names another goes by URL', async () => {
        // A reload would reload the entry on screen now, not the one the navigation
        // pushed.
        await connected();
        navigate('/next');
        const ref = mock.navs()[0][2];
        history.replaceState(history.state, '', '/moved');
        const l = await loads(() =>
            mock.frame(null, [[JS_NAVIGATE, '/next', { full: true, replace: true }]], false, ref),
        );
        expect(l.reload).not.toHaveBeenCalled();
        expect(l.replace).toHaveBeenCalledWith('http://localhost:3000/next');
    });

    it("a full load of its page a reply's fetch response asks for goes by URL", async () => {
        // A fetch's response is no reply to the navigation: its loads go by URL,
        // unbounded, whatever page the address bar names when it lands.
        await connected();
        navigate('/next');
        const ref = mock.navs()[0][2];
        /** @type {(r: *) => void} */
        let resolve = () => {};
        const pending = new Promise((r) => {
            resolve = r;
        });
        vi.stubGlobal(
            'fetch',
            vi.fn(() => pending),
        );
        mock.frame(null, [[JS_FETCH, '/api', { method: 'post' }]], false, ref);
        vi.unstubAllGlobals();
        const l = await loads(async () => {
            resolve({
                ok: true,
                status: 200,
                url: 'http://localhost:3000/api',
                text: () =>
                    Promise.resolve(
                        JSON.stringify({ e: [[JS_NAVIGATE, '/next', { full: true }]] }),
                    ),
            });
            await new Promise((r) => setTimeout(r, 0));
            await new Promise((r) => setTimeout(r, 0));
        });
        expect(l.reload).not.toHaveBeenCalled();
        expect(l.assign).toHaveBeenCalledExactlyOnceWith('http://localhost:3000/next');
    });

    it('a full navigate to a fragment of the page on screen goes by URL, as the browser does', async () => {
        await connected();
        const l = await loads(() => navigate('/start#sec', { full: true }));
        expect(l.reload).not.toHaveBeenCalled();
        expect(l.assign).toHaveBeenCalledWith('http://localhost:3000/start#sec');
    });

    it('a full load a reply answers with to a fragment of its own page goes by URL', async () => {
        // Only the answer naming the pushed URL itself reloads the entry.
        await connected();
        navigate('/next');
        const ref = mock.navs()[0][2];
        const l = await loads(() =>
            mock.frame(null, [[JS_NAVIGATE, '/next#x', { full: true, replace: true }]], false, ref),
        );
        expect(l.reload).not.toHaveBeenCalled();
        expect(l.replace).toHaveBeenCalledWith('http://localhost:3000/next#x');
    });

    it('an app full navigate to the address bar URL loads it by URL, never a reload', async () => {
        // It answers no navigation this page sent: on an entry a form POST made, a
        // reload would post the form again.
        await connected();
        const l = await loads(() => navigate(location.href, { full: true }));
        expect(l.reload).not.toHaveBeenCalled();
        expect(l.assign).toHaveBeenCalledWith(location.href);
    });

    it('a full load of another page than the address bar names replaces the entry', async () => {
        await connected();
        navigate('/next');
        const ref = mock.navs()[0][2];
        const l = await loads(() =>
            mock.frame(null, [[JS_NAVIGATE, '/login', { full: true, replace: true }]], false, ref),
        );
        expect(l.count).toBe(1);
        expect(l.replace).toHaveBeenCalledWith('http://localhost:3000/login');
    });

    it('after a load the user cancelled, an unload stamps scroll again once a page is shown', async () => {
        await connected();
        navigate('/next');
        await loads(() =>
            mock.frame(
                null,
                [[JS_NAVIGATE, '/login', { full: true, replace: true }]],
                false,
                mock.navs()[0][2],
            ),
        );
        // Cancelled: the page stays. An unload now would stamp the page on screen's
        // scroll onto the entry naming /next, so it does not...
        window.dispatchEvent(new Event('beforeunload'));
        expect(history.state?._azScroll).toBeUndefined();
        // ...until a reply puts a page on screen, which the address bar names.
        navigate('/c');
        mock.frame([[OP_REPLACE, 'page', NEXT]], null, false, mock.navs().at(-1)[2]);
        window.dispatchEvent(new Event('beforeunload'));
        expect(history.state?._azScroll).toBeDefined();
    });

    it('a load the user cancels leaves the page on screen live, with nothing to send again', async () => {
        // The answered navigation is settled: the 4500 that follows reconnects the
        // page on screen and nothing re-arms -- no second load, no second prompt.
        vi.useFakeTimers({ toFake: ['setTimeout', 'clearTimeout'] });
        await connected();
        navigate('/next');
        const ref = mock.navs()[0][2];
        const l = await loads(() => {
            mock.frame(null, [[JS_NAVIGATE, '/next', { full: true, replace: true }]], false, ref);
        });
        expect(l.count).toBe(1);
        mock.close(4500);
        mock.open(true);
        const after = await loads(() => {
            mock.resync();
            vi.advanceTimersByTime(60000);
        });
        expect(after.count).toBe(0);
        expect(mock.navs().length).toBe(1);
        expect(document.documentElement.classList.contains('az-connected')).toBe(true);
        // The reconnect dialed the page on screen, not the page the load named.
        expect(dialed()).toContain('_az_path=%2Fstart');
    });

    it('a crashing event and a navigate behind it loop over the socket, loading nothing', async () => {
        // The navigate is innocent: the event crashed the view, so the server never
        // ran the navigate. It goes out again after every remount, under the worker's
        // backoff, and nothing loads.
        vi.useFakeTimers({ toFake: ['setTimeout', 'clearTimeout'] });
        await connected();
        const l = await loads(() => {
            for (let i = 0; i < 5; i++) {
                if (i === 0) navigate('/next');
                mock.close(4500);
                mock.open(true);
                mock.resync();
            }
            vi.advanceTimersByTime(60000);
        });
        expect(l.count).toBe(0);
        expect(mock.navs().length).toBe(6);
    });
});

describe('a resync of a view the page does not hold', () => {
    it('loads the address bar URL instead of leaving the old page up', async () => {
        await connected();
        navigate('/next');
        mock.close(1006);
        mock.open(true);
        const replace = vi.fn();
        const { origin, href, pathname, search } = window.location;
        vi.stubGlobal('location', { replace, assign: vi.fn(), origin, href, pathname, search });
        try {
            mock.frame([[OP_REPLACE, 'next', NEXT]], null, true);
        } finally {
            await Promise.resolve();
            vi.unstubAllGlobals();
        }
        expect(replace).toHaveBeenCalledWith(href);
        // The old page was not touched, and nothing was sent on it.
        expect(document.getElementById('page')).not.toBeNull();
        expect(mock.navs().length).toBe(1);
    });

    it("loads the address bar's page without its fragment, which by URL would only scroll", async () => {
        // An entry this client did not push may be a form POST's: no reload.
        history.replaceState(null, '', '/start#x');
        await connected();
        mock.close(1006);
        mock.open(true);
        const replace = vi.fn();
        const reload = vi.fn();
        const { origin, href, pathname, search } = window.location;
        vi.stubGlobal('location', {
            replace,
            reload,
            assign: vi.fn(),
            origin,
            href,
            pathname,
            search,
        });
        try {
            mock.frame([[OP_REPLACE, 'next', NEXT]], null, true);
        } finally {
            await Promise.resolve();
            vi.unstubAllGlobals();
        }
        expect(reload).not.toHaveBeenCalled();
        expect(replace).toHaveBeenCalledWith('http://localhost:3000/start');
    });

    it("reloads the address bar's page with its fragment when this client stamped the entry", async () => {
        // A stamped entry (`_azIdx`) was rewritten by this client: it is no form POST's.
        history.replaceState({ _azIdx: 0 }, '', '/start#x');
        await connected();
        mock.close(1006);
        mock.open(true);
        const replace = vi.fn();
        const reload = vi.fn();
        const { origin, href, pathname, search } = window.location;
        vi.stubGlobal('location', {
            replace,
            reload,
            assign: vi.fn(),
            origin,
            href,
            pathname,
            search,
        });
        try {
            mock.frame([[OP_REPLACE, 'next', NEXT]], null, true);
        } finally {
            await Promise.resolve();
            vi.unstubAllGlobals();
        }
        expect(reload).toHaveBeenCalledOnce();
        expect(replace).not.toHaveBeenCalled();
    });

    it("reloads the address bar's page with its fragment when the entry carries only a scroll stamp", async () => {
        history.replaceState({ _azScroll: { x: 0, y: 0 } }, '', '/start#x');
        await connected();
        mock.close(1006);
        mock.open(true);
        const replace = vi.fn();
        const reload = vi.fn();
        const { origin, href, pathname, search } = window.location;
        vi.stubGlobal('location', {
            replace,
            reload,
            assign: vi.fn(),
            origin,
            href,
            pathname,
            search,
        });
        try {
            mock.frame([[OP_REPLACE, 'next', NEXT]], null, true);
        } finally {
            await Promise.resolve();
            vi.unstubAllGlobals();
        }
        expect(reload).toHaveBeenCalledOnce();
        expect(replace).not.toHaveBeenCalled();
    });

    it('sends no pending navigation from the page it leaves', async () => {
        await down();
        navigate('/other'); // pended while down
        mock.open(true);
        const replace = vi.fn();
        const { origin, href, pathname, search } = window.location;
        vi.stubGlobal('location', { replace, assign: vi.fn(), origin, href, pathname, search });
        try {
            mock.frame([[OP_REPLACE, 'next', NEXT]], null, true);
        } finally {
            await Promise.resolve();
            vi.unstubAllGlobals();
        }
        expect(replace).toHaveBeenCalledOnce();
        expect(mock.navs()).toEqual([]);
    });

    it('applies a resync of the view the page holds, loading nothing', async () => {
        await connected();
        mock.close(1006);
        mock.open(true);
        const replace = vi.fn();
        const { origin, href, pathname, search } = window.location;
        vi.stubGlobal('location', { replace, assign: vi.fn(), origin, href, pathname, search });
        try {
            mock.resync();
        } finally {
            await Promise.resolve();
            vi.unstubAllGlobals();
        }
        expect(replace).not.toHaveBeenCalled();
    });
});

describe('a navigation whose reply is lost', () => {
    it('reconnects to the page on screen, then is sent again', async () => {
        await connected();
        navigate('/next');
        mock.close(1006);
        expect(mock.navs().length).toBe(1);
        mock.open(true);
        mock.resync();
        const navs = mock.navs();
        expect(navs.length).toBe(2);
        expect(navs[1][1]).toEqual({ path: '/next', qs: '' });
        expect(navs[1][2]).not.toBe(navs[0][2]);
    });

    it('naming the page on screen is not sent again: the resync already remounted it', async () => {
        // An az-navigate to the URL on screen is a real navigation, but once its
        // reply is lost the reconnect's resync mounts that very page.
        await connected();
        navigate('/start');
        mock.close(1006);
        mock.open(true);
        mock.resync();
        expect(mock.navs().map((/** @type {Array<*>} */ m) => m[1].path)).toEqual(['/start']);
    });

    it('whose reply arrived is on screen: the bfcache respawn dials it', async () => {
        await connected();
        navigate('/next?tab=2');
        mock.frame([[OP_REPLACE, 'page', NEXT]], null, false, mock.navs()[0][2]);
        window.dispatchEvent(new PageTransitionEvent('pagehide', { persisted: true }));
        window.dispatchEvent(new PageTransitionEvent('pageshow', { persisted: true }));
        expect(dialed()).toContain('_az_path=%2Fnext');
        expect(dialed()).toContain('tab=2');
    });

    it('answered by a redirect is not on screen: the respawn dials the page on screen', async () => {
        await connected();
        navigate('/protected');
        mock.frame(null, [[JS_NAVIGATE, '/login', { replace: true }]], false, mock.navs()[0][2]);
        window.dispatchEvent(new PageTransitionEvent('pagehide', { persisted: true }));
        window.dispatchEvent(new PageTransitionEvent('pageshow', { persisted: true }));
        expect(dialed()).toContain('_az_path=%2Fstart');
        // The redirect's own navigate was lost to the pagehide: it goes out again.
        mock.open(true);
        mock.resync();
        expect(mock.navs().at(-1)[1]).toEqual({ path: '/login', qs: '' });
    });
});

// A 4501 is the page on screen's own mount failure: it arms a reload of the page,
// which the next successful mount cancels, whatever navigation is pending.
describe('the reload of a mount failure', () => {
    /** Advance past any mount reload, returning how many times it reloaded. */
    function reloads() {
        const reload = vi.fn();
        const replace = vi.fn();
        const { origin, href, pathname, search, hash } = window.location;
        vi.stubGlobal('location', { reload, replace, origin, href, pathname, search, hash });
        try {
            vi.advanceTimersByTime(60000);
        } finally {
            vi.unstubAllGlobals();
        }
        return reload.mock.calls.length + replace.mock.calls.length;
    }

    it('is cancelled by a resync with a navigation pending, which then goes out', async () => {
        vi.useFakeTimers({ toFake: ['setTimeout', 'clearTimeout'] });
        await connected();
        mock.close(1006);
        mock.open(true);
        mock.close(4501);
        navigate('/next');
        mock.open(true);
        mock.resync();
        expect(mock.navs().map((/** @type {Array<*>} */ m) => m[1].path)).toEqual(['/next']);
        expect(reloads()).toBe(0);
    });

    it('stays armed while nothing mounts', async () => {
        vi.useFakeTimers({ toFake: ['setTimeout', 'clearTimeout'] });
        await connected();
        navigate('/next');
        mock.close(4501);
        expect(reloads()).toBeGreaterThan(0);
    });
});

// Newer wins: the reply to a navigation older than the last one issued may carry a
// navigation of its own (a redirect, the full load the server answered it with),
// which must not override the newer one.
describe('a reply to an older navigation', () => {
    /** Run `fn` with `location` stubbed, returning the loads it asked for. */
    async function loads(/** @type {() => void} */ fn) {
        const reload = vi.fn();
        const replace = vi.fn();
        const assign = vi.fn();
        const { origin, href, pathname, search, hash } = window.location;
        vi.stubGlobal('location', {
            reload,
            replace,
            assign,
            origin,
            href,
            pathname,
            search,
            hash,
        });
        try {
            fn();
            await Promise.resolve();
        } finally {
            vi.unstubAllGlobals();
        }
        return reload.mock.calls.length + replace.mock.calls.length + assign.mock.calls.length;
    }

    it('loads nothing for a full load the server answered it with', async () => {
        await connected();
        navigate('/b');
        navigate('/c');
        const [b, c] = mock.navs().map((/** @type {Array<*>} */ m) => m[2]);
        expect(
            await loads(() =>
                mock.frame(null, [[JS_NAVIGATE, '/b', { full: true, replace: true }]], false, b),
            ),
        ).toBe(0);
        expect(location.pathname).toBe('/c');
        mock.frame([[OP_REPLACE, 'page', NEXT]], null, false, c);
        expect(document.getElementById('next')).not.toBeNull();
    });

    it('follows no redirect it carries, even inside a transition', async () => {
        await connected();
        navigate('/protected');
        navigate('/c');
        const p = mock.navs()[0][2];
        mock.frame(
            null,
            [
                [JS_NAVIGATE, '/login', { replace: true }],
                [JS_TRANSITION, {}, [[JS_PATCH, '/login', {}]]],
            ],
            false,
            p,
        );
        expect(mock.navs().map((/** @type {Array<*>} */ m) => m[1].path)).toEqual([
            '/protected',
            '/c',
        ]);
        expect(location.pathname).toBe('/c');
    });

    it('still runs the other effects it carries', async () => {
        await connected();
        navigate('/b');
        navigate('/c');
        const b = mock.navs()[0][2];
        mock.frame(
            null,
            [
                [14, 'Stale title'],
                [JS_NAVIGATE, '/login', {}],
            ],
            false,
            b,
        );
        expect(document.title).toBe('Stale title');
        expect(mock.navs().length).toBe(2);
    });

    it('of the last navigation issued follows its redirect', async () => {
        await connected();
        navigate('/protected');
        const p = mock.navs()[0][2];
        mock.frame(null, [[JS_NAVIGATE, '/login', { replace: true }]], false, p);
        expect(mock.navs().at(-1)[1].path).toBe('/login');
        expect(
            await loads(() =>
                mock.frame(
                    null,
                    [[JS_NAVIGATE, '/login', { full: true, replace: true }]],
                    false,
                    mock.navs().at(-1)[2],
                ),
            ),
        ).toBe(1);
    });
});

// The page loads that answer navigations issued with no person acting since the page
// mounted are bounded per tab: past 10 within 5 min, each waits 30 s first, and that
// slow state sticks until 30 s pass with no such load. A load answering a navigation
// issued after a person's pointer or key (or a popstate) is never counted.
describe('the bound on loads answering navigations', () => {
    const KEY = 'arizona:nav-loads';
    beforeEach(() => sessionStorage.clear());
    afterEach(() => sessionStorage.clear());

    /** Stub `location`, run `fn`, flush the load microtask; the loads it asked for. */
    async function loads(/** @type {() => void} */ fn) {
        const replace = vi.fn();
        const assign = vi.fn();
        const reload = vi.fn();
        const { origin, href, pathname, search, hash } = window.location;
        vi.stubGlobal('location', {
            replace,
            assign,
            reload,
            origin,
            href,
            pathname,
            search,
            hash,
        });
        try {
            fn();
            await Promise.resolve();
        } finally {
            vi.unstubAllGlobals();
        }
        return replace.mock.calls.length + assign.mock.calls.length + reload.mock.calls.length;
    }

    /** The last navigation sent answered with the load of `path`. */
    function answerWithLoad(/** @type {string} */ path) {
        mock.frame(
            null,
            [[JS_NAVIGATE, path, { full: true, replace: true }]],
            false,
            mock.navs().at(-1)[2],
        );
    }

    /** A navigate the server issues (a push), no person acting, answered with a load. */
    function serverNavAnsweredWithLoad(/** @type {string} */ path) {
        mock.frame(null, [[JS_NAVIGATE, path, {}]]);
        answerWithLoad(path);
    }

    /** A navigate a person issues, answered with its page's load. */
    function userNavAnsweredWithLoad(/** @type {string} */ path) {
        person();
        navigate(path);
        answerWithLoad(path);
    }

    const stored = () => JSON.parse(sessionStorage.getItem(KEY) ?? '[]');

    async function setup() {
        vi.useFakeTimers({ toFake: ['setTimeout', 'clearTimeout', 'Date'] });
        await connected();
        return vi.spyOn(console, 'error').mockImplementation(() => {});
    }

    it('never counts or delays a load answering a navigation the user issued', async () => {
        const error = await setup();
        for (let i = 0; i < 20; i++) {
            expect(await loads(() => userNavAnsweredWithLoad(`/p${i}`))).toBe(1);
        }
        expect(stored()).toEqual([]);
        expect(error).not.toHaveBeenCalled();
    });

    it('a delayed load goes where its URL named when the server asked for it', async () => {
        // A relative URL resolves against the page the answer came to, not the one
        // the address bar names once the failsafe delay is over.
        await setup();
        sessionStorage.setItem(KEY, JSON.stringify(Array(10).fill({ t: Date.now(), slow: false })));
        mock.frame(null, [[JS_NAVIGATE, '/dir/a', {}]]);
        mock.frame(
            null,
            [[JS_NAVIGATE, 'b', { full: true, replace: true }]],
            false,
            mock.navs().at(-1)[2],
        );
        history.replaceState(history.state, '', '/elsewhere/page');
        const replace = vi.fn();
        const { origin, href, pathname, search, hash } = window.location;
        vi.stubGlobal('location', { replace, origin, href, pathname, search, hash });
        try {
            vi.advanceTimersByTime(30000);
            await Promise.resolve();
        } finally {
            vi.unstubAllGlobals();
        }
        expect(replace).toHaveBeenCalledExactlyOnceWith('http://localhost:3000/dir/b');
    });

    it('bounds a load inside a view transition in the reply too', async () => {
        // The transition runs the load in its update callback, after the reply has
        // been applied: it still answers that reply's navigation.
        await setup();
        /** @type {Array<() => void>} */
        const updates = [];
        /** @type {any} */ (document).startViewTransition = (/** @type {() => void} */ fn) => {
            updates.push(fn);
        };
        try {
            sessionStorage.setItem(
                KEY,
                JSON.stringify(Array(10).fill({ t: Date.now(), slow: false })),
            );
            mock.frame(null, [[JS_NAVIGATE, '/a', {}]]);
            mock.frame(
                null,
                [[JS_TRANSITION, {}, [JS_NAVIGATE, '/b', { full: true }]]],
                false,
                mock.navs().at(-1)[2],
            );
            expect(updates.length).toBe(1);
            expect(await loads(() => updates[0]())).toBe(0);
            expect(await loads(() => vi.advanceTimersByTime(30000))).toBe(1);
        } finally {
            delete (/** @type {any} */ (document).startViewTransition);
        }
    });

    it('lets 10 through, then delays each by 30 s, said once', async () => {
        const error = await setup();
        for (let i = 0; i < 10; i++) {
            expect(await loads(() => serverNavAnsweredWithLoad(`/p${i}`))).toBe(1);
        }
        expect(await loads(() => serverNavAnsweredWithLoad('/p10'))).toBe(0);
        expect(error).toHaveBeenCalledOnce();
        expect(await loads(() => vi.advanceTimersByTime(29999))).toBe(0);
        expect(await loads(() => vi.advanceTimersByTime(1))).toBe(1);
        expect(stored().length).toBe(11);
        expect(stored().at(-1).slow).toBe(true);
    });

    it('stays slow while such loads keep coming, however slowly', async () => {
        await setup();
        // Slow once, long ago: the 5 min window holds a single entry.
        sessionStorage.setItem(KEY, JSON.stringify([{ t: Date.now() - 10000, slow: true }]));
        expect(await loads(() => serverNavAnsweredWithLoad('/a'))).toBe(0);
        expect(await loads(() => vi.advanceTimersByTime(30000))).toBe(1);
        // The next comes 10 s after that load started: still slow.
        vi.advanceTimersByTime(10000);
        expect(await loads(() => serverNavAnsweredWithLoad('/b'))).toBe(0);
    });

    it('leaves the slow state once 30 s pass with no such load', async () => {
        await setup();
        sessionStorage.setItem(KEY, JSON.stringify([{ t: Date.now() - 30000, slow: true }]));
        expect(await loads(() => serverNavAnsweredWithLoad('/a'))).toBe(1);
        expect(stored().at(-1).slow).toBe(false);
    });

    it('counts the whole 5 minutes, not just the last 30 s', async () => {
        await setup();
        const recent = Date.now() - 60000;
        sessionStorage.setItem(KEY, JSON.stringify(Array(10).fill({ t: recent, slow: false })));
        expect(await loads(() => serverNavAnsweredWithLoad('/p'))).toBe(0);
    });

    it('counts only the last 5 minutes: older entries expire', async () => {
        await setup();
        const old = Date.now() - 300000;
        sessionStorage.setItem(KEY, JSON.stringify(Array(10).fill({ t: old, slow: false })));
        expect(await loads(() => serverNavAnsweredWithLoad('/p'))).toBe(1);
        expect(stored().length).toBe(1);
    });

    it('never delays a load the user asked for directly', async () => {
        await setup();
        sessionStorage.setItem(KEY, JSON.stringify(Array(10).fill({ t: Date.now(), slow: true })));
        expect(await loads(() => navigate('https://elsewhere.example/x'))).toBe(1);
        expect(stored().length).toBe(10);
    });

    it('a newer navigation cancels a load waiting out the failsafe', async () => {
        await setup();
        sessionStorage.setItem(KEY, JSON.stringify(Array(10).fill({ t: Date.now(), slow: false })));
        expect(await loads(() => serverNavAnsweredWithLoad('/a'))).toBe(0);
        person('keydown');
        navigate('/b');
        expect(await loads(() => vi.advanceTimersByTime(60000))).toBe(0);
        expect(mock.navs().at(-1)[1].path).toBe('/b');
    });

    it("the page's own sends are no person: a loop stays bounded", async () => {
        // An `arizona:connected` listener's pushEvent, a hook's, a timer's, an app
        // navigate or a script's untrusted click: none of them is a person.
        await setup();
        sessionStorage.setItem(KEY, JSON.stringify(Array(10).fill({ t: Date.now(), slow: false })));
        mod.pushEvent('hello', {});
        click(); // dispatched by script: untrusted
        document.body.dispatchEvent(new Event('pointerdown', { bubbles: true }));
        expect(await loads(() => serverNavAnsweredWithLoad('/a'))).toBe(0);
    });

    it("an app's own navigate answered with a load is bounded", async () => {
        await setup();
        sessionStorage.setItem(KEY, JSON.stringify(Array(10).fill({ t: Date.now(), slow: false })));
        expect(
            await loads(() => {
                navigate('/b');
                answerWithLoad('/b');
            }),
        ).toBe(0);
    });

    it('Back or Forward is a person: the load answering it is never bounded', async () => {
        await setup();
        sessionStorage.setItem(KEY, JSON.stringify(Array(10).fill({ t: Date.now(), slow: false })));
        history.pushState(null, '', '/other');
        window.dispatchEvent(new PopStateEvent('popstate', { state: null }));
        expect(mock.navs().at(-1)[1].path).toBe('/other');
        expect(await loads(() => answerWithLoad('/other'))).toBe(1);
    });

    it("the page's own navigate does not cancel a load waiting out the failsafe", async () => {
        await setup();
        sessionStorage.setItem(KEY, JSON.stringify(Array(10).fill({ t: Date.now(), slow: false })));
        expect(await loads(() => serverNavAnsweredWithLoad('/a'))).toBe(0);
        navigate('/b'); // the page's own code, no person
        expect(await loads(() => vi.advanceTimersByTime(30000))).toBe(1);
    });

    it("a person's chain through a live hop stays theirs", async () => {
        // A person's click navigates live to a page whose view navigates on connect
        // across layouts: that load answers the person, so it is never bounded.
        await setup();
        sessionStorage.setItem(KEY, JSON.stringify(Array(10).fill({ t: Date.now(), slow: false })));
        person();
        navigate('/r');
        mock.frame([[OP_REPLACE, 'page', PAGE]], null, false, mock.navs().at(-1)[2]);
        expect(await loads(() => serverNavAnsweredWithLoad('/w'))).toBe(1);
    });

    it('a navigation the page pended while down keeps its lack of input', async () => {
        await setup();
        sessionStorage.setItem(KEY, JSON.stringify(Array(10).fill({ t: Date.now(), slow: false })));
        mock.close(1006);
        navigate('/a'); // the page's own code, while down: pended
        mock.open(true);
        mock.resync();
        expect(mock.navs().map((/** @type {Array<*>} */ m) => m[1].path)).toEqual(['/a']);
        expect(await loads(() => answerWithLoad('/a'))).toBe(0);
    });

    it('never bounds a user stepping through server redirects, one step every 20 s', async () => {
        // A wizard: each click raises an event whose handler navigates across layouts,
        // which the server answers with a load. A person acted, so it is no loop.
        const error = await setup();
        for (let i = 0; i < 20; i++) {
            mock.frame([[OP_REPLACE, 'page', PAGE]]); // the step's page mounted
            person(); // a person's pointer...
            click(); // ...on the step's button
            mock.frame(null, [[JS_NAVIGATE, `/step${i}`, {}]]); // its handler navigates
            expect(await loads(() => answerWithLoad(`/step${i}`))).toBe(1);
            vi.advanceTimersByTime(20000);
        }
        expect(stored()).toEqual([]);
        expect(error).not.toHaveBeenCalled();
    });

    it('a mount clears the input: a page navigating on its own is bounded again', async () => {
        await setup();
        sessionStorage.setItem(KEY, JSON.stringify(Array(10).fill({ t: Date.now(), slow: false })));
        person();
        mock.frame([[OP_REPLACE, 'page', PAGE]]); // a new view mounted, no input since
        expect(await loads(() => serverNavAnsweredWithLoad('/a'))).toBe(0);
    });

    /** Seeded full: the next no-input load waits. */
    const seedFull = () =>
        sessionStorage.setItem(KEY, JSON.stringify(Array(10).fill({ t: Date.now(), slow: false })));

    for (const type of /** @type {const} */ (['pointerdown', 'keydown', 'click'])) {
        it(`a trusted ${type} is a person`, async () => {
            await setup();
            seedFull();
            person(type);
            expect(await loads(() => serverNavAnsweredWithLoad('/a'))).toBe(1);
        });
    }

    for (const type of /** @type {const} */ (['input', 'change'])) {
        it(`a trusted ${type} is no person: the page's own code fires it trusted`, async () => {
            // A checkbox's scripted click() fires a trusted change, execCommand a
            // trusted input: neither is a person.
            await setup();
            seedFull();
            person(type);
            expect(await loads(() => serverNavAnsweredWithLoad('/a'))).toBe(0);
        });
    }

    it("a held key's autorepeat is no fresh person", async () => {
        await setup();
        seedFull();
        person('keydown', { repeat: true });
        expect(await loads(() => serverNavAnsweredWithLoad('/a'))).toBe(0);
    });

    it('a listener stopping the event cannot hide a person', async () => {
        await setup();
        seedFull();
        document.body.addEventListener('pointerdown', (e) => e.stopPropagation());
        person();
        expect(await loads(() => serverNavAnsweredWithLoad('/a'))).toBe(1);
    });

    it('a fragment-only popstate is no person', async () => {
        // A script writing `location.hash`, or a scroll-spy's own history entries.
        await setup();
        seedFull();
        history.pushState(null, '', '/start#t1');
        window.dispatchEvent(new PopStateEvent('popstate', { state: null }));
        expect(await loads(() => serverNavAnsweredWithLoad('/a'))).toBe(0);
    });

    for (const userInitiated of [false, true]) {
        it(`a cross-page traversal is a person only when the browser started it (${userInitiated})`, async () => {
            // Where the Navigation API says who started it: a script's history.back()
            // is no person, the browser's Back button is.
            /** @type {((e: *) => void)|null} */
            let onNavigate = null;
            vi.stubGlobal('navigation', {
                addEventListener: (/** @type {string} */ t, /** @type {*} */ fn) => {
                    if (t === 'navigate') onNavigate = fn;
                },
            });
            await setup();
            seedFull();
            history.pushState(null, '', '/other');
            /** @type {*} */ (onNavigate)({ navigationType: 'traverse', userInitiated });
            window.dispatchEvent(new PopStateEvent('popstate', { state: null }));
            expect(mock.navs().at(-1)[1].path).toBe('/other');
            expect(await loads(() => answerWithLoad('/other'))).toBe(userInitiated ? 1 : 0);
        });
    }

    /** Stub the Navigation API, returning a dispatcher for its `navigate` events. */
    function stubNavigationApi() {
        /** @type {((e: *) => void)|null} */
        let onNavigate = null;
        vi.stubGlobal('navigation', {
            addEventListener: (/** @type {string} */ t, /** @type {*} */ fn) => {
                if (t === 'navigate') onNavigate = fn;
            },
        });
        return (/** @type {*} */ e) => /** @type {*} */ (onNavigate)(e);
    }

    it('a navigate event other than a traverse resets a traverse the user started', async () => {
        const fire = stubNavigationApi();
        await setup();
        seedFull();
        fire({ navigationType: 'traverse', userInitiated: true });
        fire({ navigationType: 'push', userInitiated: true, destination: { sameDocument: true } });
        history.pushState(null, '', '/other');
        window.dispatchEvent(new PopStateEvent('popstate', { state: null }));
        expect(await loads(() => answerWithLoad('/other'))).toBe(0);
    });

    it('a traverse the user started counts for its own popstate only', async () => {
        const fire = stubNavigationApi();
        await setup();
        seedFull();
        fire({ navigationType: 'traverse', userInitiated: true });
        history.pushState(null, '', '/other');
        window.dispatchEvent(new PopStateEvent('popstate', { state: null }));
        expect(await loads(() => answerWithLoad('/other'))).toBe(1);
        // A later popstate the API reported nothing for is no person.
        mock.frame([[OP_REPLACE, 'page', PAGE]]); // a new view mounted, no input since
        history.pushState(null, '', '/third');
        window.dispatchEvent(new PopStateEvent('popstate', { state: null }));
        expect(await loads(() => answerWithLoad('/third'))).toBe(0);
    });

    it('a bfcache restore forgets a traverse the user started', async () => {
        const fire = stubNavigationApi();
        await setup();
        seedFull();
        fire({ navigationType: 'traverse', userInitiated: true });
        window.dispatchEvent(new PageTransitionEvent('pageshow', { persisted: false }));
        history.pushState(null, '', '/other');
        window.dispatchEvent(new PopStateEvent('popstate', { state: null }));
        expect(await loads(() => answerWithLoad('/other'))).toBe(0);
    });

    it('a no-person live hop keeps no input: the load after it is bounded', async () => {
        await setup();
        seedFull();
        mock.frame(null, [[JS_NAVIGATE, '/r', {}]]); // the server navigates, no person
        mock.frame([[OP_REPLACE, 'page', PAGE]], null, false, mock.navs().at(-1)[2]); // live
        expect(await loads(() => serverNavAnsweredWithLoad('/w'))).toBe(0);
    });

    it('a navigation a frame runs after a person acted does not cancel a waiting load', async () => {
        await setup();
        seedFull();
        expect(await loads(() => serverNavAnsweredWithLoad('/a'))).toBe(0);
        person();
        mock.frame(null, [[JS_NAVIGATE, '/b', {}]]); // the server's, in a frame
        expect(await loads(() => vi.advanceTimersByTime(30000))).toBe(1);
    });

    it('the input listeners go with disconnect()', async () => {
        await setup();
        const add = vi.spyOn(window, 'addEventListener');
        mock.restore();
        vi.resetModules();
        mod = await import('./arizona.js');
        document.body.innerHTML = PAGE;
        mock = setupMockWorker();
        const opts = add.mock.calls
            .filter((c) => c[0] === 'pointerdown')
            .map((c) => /** @type {AddEventListenerOptions} */ (c[2]));
        expect(opts.length).toBe(1);
        expect(opts[0].capture).toBe(true);
        mock.restore();
        expect(opts[0].signal?.aborted).toBe(true);
        mock = null;
    });

    it('the form replay of a reconnect is no user input', async () => {
        // A page that navigates on its own and holds a form: the resync replays the
        // form's az-change event, which no person raised.
        await setup();
        sessionStorage.setItem(KEY, JSON.stringify(Array(10).fill({ t: Date.now(), slow: false })));
        const view =
            '<main id="page" az-view><form id="f" az-change=\'[0,"v"]\'><input name="x" value=""></form></main>';
        document.body.innerHTML = view;
        /** @type {HTMLInputElement} */ (document.querySelector('input')).value = 'typed';
        mock.close(1006);
        mock.open(true);
        mock.resync(view);
        expect(events().map((/** @type {Array<*>} */ e) => e[1])).toContain('v');
        expect(await loads(() => serverNavAnsweredWithLoad('/a'))).toBe(0);
    });

    it('a waiting load is dropped once a newer navigation goes live', async () => {
        await setup();
        sessionStorage.setItem(KEY, JSON.stringify(Array(10).fill({ t: Date.now(), slow: false })));
        expect(await loads(() => serverNavAnsweredWithLoad('/w2'))).toBe(0);
        mock.frame(null, [[JS_NAVIGATE, '/q2', {}]]); // the server navigates again...
        mock.frame([[OP_REPLACE, 'page', NEXT]], null, false, mock.navs().at(-1)[2]); // ...live
        expect(await loads(() => vi.advanceTimersByTime(60000))).toBe(0);
    });

    it('another no-input load while one waits takes its place, keeping its deadline', async () => {
        // A loop keeps navigating while its delayed load waits (the socket remounts
        // and the view navigates again): that must not put the load off forever.
        await setup();
        sessionStorage.setItem(KEY, JSON.stringify(Array(10).fill({ t: Date.now(), slow: false })));
        expect(await loads(() => serverNavAnsweredWithLoad('/a'))).toBe(0);
        vi.advanceTimersByTime(20000);
        expect(await loads(() => serverNavAnsweredWithLoad('/b'))).toBe(0);
        expect(stored().length).toBe(11);
        // The /b entry, the one its navigation pushed, reloaded at /a's deadline.
        expect(location.pathname).toBe('/b');
        expect(await loads(() => vi.advanceTimersByTime(9999))).toBe(0);
        expect(await loads(() => vi.advanceTimersByTime(1))).toBe(1);
    });

    it('a navigation pended while down keeps who issued it across the replay', async () => {
        const error = await setup();
        sessionStorage.setItem(KEY, JSON.stringify(Array(10).fill({ t: Date.now(), slow: false })));
        mock.frame(null, [[JS_NAVIGATE, '/a', {}]]); // the server issues it...
        mock.close(1006); // ...its reply is lost...
        mock.open(true);
        mock.resync(); // ...and it goes out again
        expect(mock.navs().map((/** @type {Array<*>} */ m) => m[1].path)).toEqual(['/a', '/a']);
        expect(await loads(() => answerWithLoad('/a'))).toBe(0);
        expect(error).toHaveBeenCalledOnce();
    });
});

describe('the tag a navigate frame carries for the worker', () => {
    it('names the document base its reply effects resolve against', async () => {
        const base = document.createElement('base');
        base.href = '/';
        document.head.appendChild(base);
        try {
            history.replaceState(null, '', '/docs/intro');
            await connected();
            navigate('/docs/other');
            const tag = mock.posted.filter((/** @type {Array<*>} */ d) => d[0] === 1).at(-1)[2];
            expect(tag.base).toBe(document.baseURI);
            expect(tag.href).toContain('/docs/other');
        } finally {
            base.remove();
        }
    });

    it('names the closes the main thread has seen, for the worker to drop a stale one', async () => {
        await connected();
        const tag = () => mock.posted.filter((/** @type {Array<*>} */ d) => d[0] === 1).at(-1)[2];
        navigate('/a');
        expect(tag().closes).toBe(0);
        mock.close(1006);
        mock.open(true);
        mock.resync();
        mock.close(1006);
        mock.open(true);
        mock.resync();
        navigate('/b');
        expect(tag().closes).toBe(2);
        // A respawned worker starts counting from there.
        expect(mock.posted.find((/** @type {Array<*>} */ d) => d[0] === 0)[3]).toBe(0);
    });
});

describe('popstate', () => {
    it('to the page on screen, with a navigation in flight, is a navigation', async () => {
        await connected();
        navigate('/next');
        history.back();
        await new Promise((r) => window.addEventListener('popstate', r, { once: true }));
        expect(mock.navs().map((/** @type {Array<*>} */ m) => m[1].path)).toEqual([
            '/next',
            '/start',
        ]);
    });

    it('to the address of a page a cancelled full load left on screen only scrolls', async () => {
        await connected();
        // The server degrades the navigate to a full load, which a beforeunload
        // prompt cancels: the page on screen is still /start.
        navigate('/next');
        const replace = vi.fn();
        vi.stubGlobal('location', { replace, href: 'http://localhost/start' });
        mock.frame(
            null,
            [[JS_NAVIGATE, '/next', { full: true, replace: true }]],
            false,
            mock.navs()[0][2],
        );
        await Promise.resolve();
        vi.unstubAllGlobals();
        expect(replace).toHaveBeenCalledOnce();
        history.back();
        await new Promise((r) => window.addEventListener('popstate', r, { once: true }));
        expect(mock.navs().length).toBe(1);
    });
});

describe('a reconnect that never opens', () => {
    // A refused attempt says nothing about the page: a proxy restarting answers for
    // the app, an upgrade middleware may fail once. The worker keeps retrying, and
    // the page stays as it is -- nothing is asked over HTTP, nothing is loaded.
    it('asks nothing over HTTP and loads nothing, however many attempts fail', async () => {
        const fetchMock = vi.fn(() => Promise.resolve({ status: 404, url: location.href }));
        vi.stubGlobal('fetch', fetchMock);
        await connected();
        const assign = vi.fn();
        const replace = vi.fn();
        for (let n = 0; n < 4; n++) mock.close(1006);
        const { origin, href, pathname, search } = window.location;
        vi.stubGlobal('location', { assign, replace, origin, href, pathname, search });
        await new Promise((r) => setTimeout(r, 0));
        await Promise.resolve();
        vi.unstubAllGlobals();
        expect(fetchMock).not.toHaveBeenCalled();
        expect(assign).not.toHaveBeenCalled();
        expect(replace).not.toHaveBeenCalled();
        // The next attempt that opens resyncs the page in place.
        mock.open(true);
        mock.resync();
        expect(document.querySelector('#page')).not.toBeNull();
    });
});
