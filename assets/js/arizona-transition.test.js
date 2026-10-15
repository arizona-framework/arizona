import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';
import { connect, executeJS, OP } from './arizona.js';

// Op codes (must match include/arizona_effect.hrl).
const JS_PUSH_EVENT = 0;
const JS_TOGGLE = 1;
const JS_ADD_CLASS = 4;
const JS_NAVIGATE = 10;
const JS_SET_TITLE = 14;
const JS_TRANSITION = 20;

// ---------------------------------------------------------------------------
// Harness
// ---------------------------------------------------------------------------

let _mock = null;

/**
 * Stub document.startViewTransition. Records each call and runs the update
 * callback synchronously (object form's `.update`, or the bare callback).
 */
function stubViewTransitions() {
    const fn = vi.fn((arg) => {
        (typeof arg === 'object' ? arg.update : arg)();
        return {
            ready: Promise.resolve(),
            finished: Promise.resolve(),
            updateCallbackDone: Promise.resolve(),
            skipTransition: vi.fn(),
        };
    });
    document.startViewTransition = fn;
    return fn;
}

function stubMatchMedia(reduce) {
    window.matchMedia = vi.fn().mockReturnValue({ matches: reduce });
}

function stubCSSSupports(supported) {
    if (typeof globalThis.CSS === 'undefined') globalThis.CSS = {};
    globalThis.CSS.supports = vi.fn().mockReturnValue(supported);
}

/** Mock the Worker + connect(); mirrors the harness in arizona.test.js. */
function setupMockWorker() {
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
    const disconnect = connect('/ws');
    worker.onmessage({ data: [1, false] }); // open -> _connected = true
    return {
        posted,
        simulateMessage: (ops, effects = null) =>
            worker.onmessage({ data: [0, ops, effects, false] }),
        restore: () => {
            disconnect(); // resets _pendingTransition + listeners
            globalThis.Worker = OrigWorker;
        },
    };
}

beforeEach(() => {
    document.body.innerHTML = '';
    // Reset shared jsdom location so a prior nav can't make a link click a
    // same-page (no round-trip) nav.
    history.replaceState(null, '', '/');
    stubViewTransitions();
    stubMatchMedia(false);
    stubCSSSupports(true);
    _mock = setupMockWorker();
});

afterEach(() => {
    _mock.restore();
    _mock = null;
    delete document.startViewTransition;
    delete window.matchMedia;
    document.body.innerHTML = '';
});

/** The page swap an OP_REPLACE on the bare view root produces. */
function replaceOp(text) {
    return [OP.REPLACE, 'page', `<div id="page" az-view><h1 az="0">${text}</h1></div>`];
}

// ---------------------------------------------------------------------------
// Synchronous client effects
// ---------------------------------------------------------------------------

describe('view transitions -- sync client effects', () => {
    it('wraps a single client effect in place', () => {
        document.body.innerHTML = '<div id="panel" hidden>x</div>';
        executeJS(document.body, null, [JS_TRANSITION, { types: ['x'] }, [JS_TOGGLE, '#panel']]);
        expect(document.startViewTransition).toHaveBeenCalledOnce();
        expect(document.getElementById('panel').hidden).toBe(false);
    });

    it('wraps a list of effects', () => {
        document.body.innerHTML = '<div id="a">x</div><div id="b" hidden>y</div>';
        executeJS(document.body, null, [
            JS_TRANSITION,
            {},
            [
                [JS_ADD_CLASS, '#a', 'on'],
                [JS_TOGGLE, '#b'],
            ],
        ]);
        expect(document.startViewTransition).toHaveBeenCalledOnce();
        expect(document.getElementById('a').classList.contains('on')).toBe(true);
        expect(document.getElementById('b').hidden).toBe(false);
    });

    it('runs the effect directly (no wrap) under prefers-reduced-motion', () => {
        stubMatchMedia(true);
        document.body.innerHTML = '<div id="panel" hidden>x</div>';
        executeJS(document.body, null, [JS_TRANSITION, {}, [JS_TOGGLE, '#panel']]);
        expect(document.startViewTransition).not.toHaveBeenCalled();
        expect(document.getElementById('panel').hidden).toBe(false);
    });

    it('runs the effect directly when the API is unsupported', () => {
        delete document.startViewTransition;
        document.body.innerHTML = '<div id="panel" hidden>x</div>';
        expect(() =>
            executeJS(document.body, null, [JS_TRANSITION, {}, [JS_TOGGLE, '#panel']]),
        ).not.toThrow();
        expect(document.getElementById('panel').hidden).toBe(false);
    });
});

// ---------------------------------------------------------------------------
// Navigation (async, awaits the OP_REPLACE)
// ---------------------------------------------------------------------------

describe('view transitions -- navigation', () => {
    it('wraps the page-swap OP_REPLACE with the requested types', () => {
        const fn = document.startViewTransition;
        document.body.innerHTML = '<div id="page" az-view><h1 az="0">Home</h1></div>';
        executeJS(document.body, null, [JS_TRANSITION, { types: ['slide'] }, [JS_NAVIGATE, '/x']]);
        _mock.simulateMessage([replaceOp('Detail')]);
        expect(fn).toHaveBeenCalledOnce();
        expect(fn.mock.calls[0][0].types).toEqual(['slide']);
        expect(document.querySelector('#page h1').textContent).toBe('Detail');
    });

    it('ignores a stray text/attr tick before the page swap', () => {
        const fn = document.startViewTransition;
        document.body.innerHTML = '<div id="page" az-view><h1 az="0">a</h1></div>';
        executeJS(document.body, null, [JS_TRANSITION, {}, [JS_NAVIGATE, '/x']]);
        // A concurrent tick (SET_ATTR) is not the page swap -> not wrapped, intent kept.
        _mock.simulateMessage([[OP.SET_ATTR, 'page:0', 'data-k', '1']]);
        expect(fn).not.toHaveBeenCalled();
        // The real swap then wraps.
        _mock.simulateMessage([replaceOp('b')]);
        expect(fn).toHaveBeenCalledOnce();
    });

    it('swaps instantly under prefers-reduced-motion', () => {
        const fn = document.startViewTransition;
        stubMatchMedia(true);
        document.body.innerHTML = '<div id="page" az-view><h1 az="0">a</h1></div>';
        executeJS(document.body, null, [JS_TRANSITION, {}, [JS_NAVIGATE, '/x']]);
        _mock.simulateMessage([replaceOp('b')]);
        expect(fn).not.toHaveBeenCalled();
        expect(document.querySelector('#page h1').textContent).toBe('b');
    });

    it('wraps ops and effects of the same message together', () => {
        const fn = document.startViewTransition;
        document.body.innerHTML = '<div id="page" az-view><h1 az="0">a</h1></div>';
        executeJS(document.body, null, [JS_TRANSITION, {}, [JS_NAVIGATE, '/x']]);
        _mock.simulateMessage([replaceOp('b')], [[JS_SET_TITLE, 'Detail']]);
        expect(fn).toHaveBeenCalledOnce();
        expect(document.querySelector('#page h1').textContent).toBe('b');
        expect(document.title).toBe('Detail');
    });
});

// ---------------------------------------------------------------------------
// push_event (async, awaits the next diff)
// ---------------------------------------------------------------------------

describe('view transitions -- push_event', () => {
    it('wraps the next server diff, even when it is not a REPLACE', () => {
        const fn = document.startViewTransition;
        document.body.innerHTML = '<div id="page" az-view><p az="0">old</p></div>';
        executeJS(document.body, null, [JS_TRANSITION, {}, [JS_PUSH_EVENT, 'load_more']]);
        _mock.simulateMessage([[OP.TEXT, 'page:0', 'new']]);
        expect(fn).toHaveBeenCalledOnce();
        expect(document.querySelector('#page [az="0"]').innerHTML).toBe('new');
    });

    it('does not let a no-diff push_event linger onto a later diff', () => {
        const fn = document.startViewTransition;
        document.body.innerHTML = '<div id="page" az-view><p az="0">old</p></div>';
        executeJS(document.body, null, [JS_TRANSITION, {}, [JS_PUSH_EVENT, 'noop']]);
        // The event produced no diff (effects-only response) -- the intent is
        // consumed, not held.
        _mock.simulateMessage(null, [[JS_SET_TITLE, 'x']]);
        expect(fn).not.toHaveBeenCalled();
        // A later, unrelated diff must NOT inherit the stale intent.
        _mock.simulateMessage([[OP.TEXT, 'page:0', 'new']]);
        expect(fn).not.toHaveBeenCalled();
    });
});

// ---------------------------------------------------------------------------
// az-transition attribute (general -- any trigger, not just links)
// ---------------------------------------------------------------------------

describe('view transitions -- az-transition attribute', () => {
    it('animates an az-navigate link and parses space-separated types', () => {
        const fn = document.startViewTransition;
        document.body.innerHTML =
            '<div id="page" az-view><a id="lnk" href="/x" az-navigate az-transition="slide back">go</a></div>';
        document
            .getElementById('lnk')
            .dispatchEvent(new MouseEvent('click', { bubbles: true, button: 0 }));
        _mock.simulateMessage([replaceOp('Detail')]);
        expect(fn).toHaveBeenCalledOnce();
        expect(fn.mock.calls[0][0].types).toEqual(['slide', 'back']);
    });

    it('animates a client effect on a non-link az_click element', () => {
        const fn = document.startViewTransition;
        document.body.innerHTML =
            '<div id="page" az-view>' +
            `<button id="btn" az-click='[${JS_TOGGLE},"#panel"]' az-transition>t</button>` +
            '<div id="panel" hidden>x</div>' +
            '</div>';
        document
            .getElementById('btn')
            .dispatchEvent(new MouseEvent('click', { bubbles: true, button: 0 }));
        expect(fn).toHaveBeenCalledOnce();
        expect(document.getElementById('panel').hidden).toBe(false);
    });

    it('does not animate an az-navigate link without az-transition', () => {
        const fn = document.startViewTransition;
        document.body.innerHTML =
            '<div id="page" az-view><a id="lnk" href="/x" az-navigate>go</a></div>';
        document
            .getElementById('lnk')
            .dispatchEvent(new MouseEvent('click', { bubbles: true, button: 0 }));
        _mock.simulateMessage([replaceOp('Detail')]);
        expect(fn).not.toHaveBeenCalled();
    });

    it('stamps outgoing scroll and transition in ONE replaceState (Safari rate-limits)', () => {
        document.body.innerHTML =
            '<div id="page" az-view>' +
            '<a id="lnk" href="/x" az-navigate az-transition="slide">go</a></div>';
        const replaceSpy = vi.spyOn(history, 'replaceState');
        document
            .getElementById('lnk')
            .dispatchEvent(new MouseEvent('click', { bubbles: true, button: 0 }));
        // One merged write: separate _azScroll and _azTransitionNext replaceState
        // calls double the history writes per nav, tripping Safari's ~100/30s
        // SecurityError rate limit twice as fast.
        expect(replaceSpy).toHaveBeenCalledTimes(1);
        const state = replaceSpy.mock.calls[0][0];
        expect(state._azScroll).toEqual({ x: window.scrollX, y: window.scrollY });
        expect(state._azTransitionNext).toEqual({ types: ['slide'], kind: 'replace' });
        replaceSpy.mockRestore();
    });
});

// ---------------------------------------------------------------------------
// Back/forward replay from history state
// ---------------------------------------------------------------------------

describe('view transitions -- back/forward', () => {
    /** Click an SPA link: `href`, with `az-transition` when `types` is given. */
    function nav(href, types) {
        const t = types === undefined ? '' : ` az-transition="${types}"`;
        document.body.innerHTML = `<div id="page" az-view><a id="lnk" href="${href}" az-navigate${t}>go</a></div>`;
        document
            .getElementById('lnk')
            .dispatchEvent(new MouseEvent('click', { bubbles: true, cancelable: true, button: 0 }));
        _mock.simulateMessage([replaceOp(href)]);
    }

    /** Traverse history by `delta` and answer the resulting navigate frame. */
    async function traverse(delta) {
        const popped = new Promise((r) => window.addEventListener('popstate', r, { once: true }));
        history.go(delta);
        await popped;
        _mock.simulateMessage([replaceOp(location.pathname)]);
    }

    it('back and forward across a transitioned edge replay its transition', async () => {
        const fn = document.startViewTransition;
        nav('/b', 'back');
        expect(fn).toHaveBeenCalledTimes(1);
        await traverse(-1); // B -> A crosses the transitioned edge
        expect(fn).toHaveBeenCalledTimes(2);
        expect(fn.mock.calls[1][0].types).toEqual(['back']);
        await traverse(1); // A -> B crosses it again
        expect(fn).toHaveBeenCalledTimes(3);
        expect(fn.mock.calls[2][0].types).toEqual(['back']);
    });

    it('back across a plain edge does not animate, even into a transitioned entry', async () => {
        // A -(transition)-> B -(plain)-> C: B was reached by a transition, but
        // Back from C traverses the plain B-C edge.
        const fn = document.startViewTransition;
        nav('/b', 'slide');
        nav('/c');
        expect(fn).toHaveBeenCalledTimes(1);
        await traverse(-1); // C -> B: plain edge
        expect(fn).toHaveBeenCalledTimes(1);
        await traverse(-1); // B -> A: transitioned edge
        expect(fn).toHaveBeenCalledTimes(2);
        await traverse(1); // A -> B: transitioned edge
        expect(fn).toHaveBeenCalledTimes(3);
        await traverse(1); // B -> C: plain edge
        expect(fn).toHaveBeenCalledTimes(3);
    });

    it('back across a transitioned edge animates when the next one was plain', async () => {
        // A -(plain)-> B -(transition)-> C.
        const fn = document.startViewTransition;
        nav('/b');
        nav('/c', 'slide');
        expect(fn).toHaveBeenCalledTimes(1);
        await traverse(-1); // C -> B: transitioned edge
        expect(fn).toHaveBeenCalledTimes(2);
        await traverse(-1); // B -> A: plain edge
        expect(fn).toHaveBeenCalledTimes(2);
    });

    it('a multi-step traversal does not animate, in either direction', async () => {
        // history.go(+-2) crosses two edges at once; replaying the destination's
        // adjacent one would animate going back but not forward (or vice versa).
        const fn = document.startViewTransition;
        nav('/b', 'slide');
        nav('/c');
        expect(fn).toHaveBeenCalledTimes(1);
        await traverse(-2); // C -> A
        expect(fn).toHaveBeenCalledTimes(1);
        await traverse(2); // A -> C
        expect(fn).toHaveBeenCalledTimes(1);
    });

    it('a replace navigation keeps the edges of the entry it replaces', async () => {
        // A -(transition)-> B, then B is replaced by B2 in place: Back to A and
        // Forward to B2 both cross the same transitioned edge.
        const fn = document.startViewTransition;
        nav('/b', 'slide');
        executeJS(document.body, null, [JS_NAVIGATE, '/b2', { replace: true }]);
        _mock.simulateMessage([replaceOp('/b2')]);
        expect(fn).toHaveBeenCalledTimes(1);
        await traverse(-1); // B2 -> A
        expect(fn).toHaveBeenCalledTimes(2);
        await traverse(1); // A -> B2
        expect(fn).toHaveBeenCalledTimes(3);
        expect(location.pathname).toBe('/b2');
    });

    it('a plain re-navigation from an entry clears its old transitioned edge', async () => {
        // A -(transition)-> B, Back to A, then A -(plain)-> D replaces the
        // forward history: Back from D traverses the new plain edge.
        const fn = document.startViewTransition;
        nav('/b', 'slide');
        await traverse(-1);
        expect(fn).toHaveBeenCalledTimes(2);
        nav('/d');
        await traverse(-1); // D -> A: plain edge
        expect(fn).toHaveBeenCalledTimes(2);
    });
});

describe('containment', () => {
    it('warns once from its own catch when a wrapped command crashes', () => {
        // `[1,"["]` parses but crashes the interpreter (invalid selector). With
        // az-transition the execution runs inside startViewTransition's update
        // callback -- in a real browser that is OUTSIDE runDelegated's try, so
        // the wrapper needs its own containment. The sync stub cannot reproduce
        // the async escape, but it pins WHERE the catch fires: the wrapper's
        // message, not the generic delegated one.
        document.body.innerHTML =
            `<div id="v" az-view>` +
            `<button id="b" az-click='[1,"["]' az-transition>x</button></div>`;
        const warn = vi.spyOn(console, 'warn').mockImplementation(() => {});
        const btn = document.getElementById('b');
        btn.dispatchEvent(new Event('click', { bubbles: true }));
        btn.dispatchEvent(new Event('click', { bubbles: true }));
        const wrapped = warn.mock.calls.filter(([m]) => m.includes('az-transition command failed'));
        expect(wrapped.length).toBe(1);
        warn.mockRestore();
    });
});
