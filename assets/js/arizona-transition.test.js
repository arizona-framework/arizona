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
        _mock.simulateMessage([[OP.UPDATE, 'page:0', 'new']]);
        expect(fn).toHaveBeenCalledOnce();
        expect(document.querySelector('#page [az="0"]').innerHTML).toBe('new');
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
});

// ---------------------------------------------------------------------------
// Back/forward replay from history state
// ---------------------------------------------------------------------------

describe('view transitions -- back/forward', () => {
    it('replays the transition stamped on the history entry', () => {
        const fn = document.startViewTransition;
        document.body.innerHTML = '<div id="page" az-view><h1 az="0">a</h1></div>';
        history.pushState({ _azTransition: { types: ['back'], kind: 'replace' } }, '', '/x');
        window.dispatchEvent(
            new PopStateEvent('popstate', {
                state: { _azTransition: { types: ['back'], kind: 'replace' } },
            }),
        );
        _mock.simulateMessage([replaceOp('popped')]);
        expect(fn).toHaveBeenCalledOnce();
        expect(fn.mock.calls[0][0].types).toEqual(['back']);
    });
});
