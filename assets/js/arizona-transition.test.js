import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';
import { applyOps, connect, executeJS, OP, resolveEl } from './arizona.js';

// Op codes (must match include/arizona_effect.hrl).
const JS_TOGGLE = 1;
const JS_NAVIGATE = 10;
const JS_TRANSITION = 20;

// ---------------------------------------------------------------------------
// Harness
// ---------------------------------------------------------------------------

let _origCSSSupports;
let _activeWorker = null;

/**
 * Stub document.startViewTransition. Records each call's `types` and runs the
 * update callback synchronously (object form's `.update`, or the bare callback).
 */
function stubViewTransitions() {
    const fn = vi.fn((arg) => {
        const isObj = typeof arg === 'object';
        (isObj ? arg.update : arg)();
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

/** Stub prefers-reduced-motion. */
function stubMatchMedia(reduce) {
    window.matchMedia = vi.fn().mockReturnValue({ matches: reduce });
}

/** Stub CSS.supports (view-transition `types` detection). */
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
    _activeWorker = {
        posted,
        simulateOpen: () => worker.onmessage({ data: [1, false] }),
        simulateMessage: (ops, effects = null) =>
            worker.onmessage({ data: [0, ops, effects, false] }),
        restore: () => {
            disconnect();
            globalThis.Worker = OrigWorker;
        },
    };
    return _activeWorker;
}

beforeEach(() => {
    document.body.innerHTML = '';
    // Reset the shared jsdom location so a prior test's navigate (which pushes a
    // new path) cannot turn a link click into a same-page (no round-trip) nav.
    history.replaceState(null, '', '/');
    stubMatchMedia(false);
    stubCSSSupports(true);
});

afterEach(() => {
    if (_activeWorker) {
        _activeWorker.restore();
        _activeWorker = null;
    }
    // Drain any lingering _pendingTransition with the API removed, so a flag set
    // by one test can never leak a transition into the next.
    delete document.startViewTransition;
    applyOps([[OP.REPLACE, '__drain__', '']]);
    delete window.matchMedia;
    if (globalThis.CSS) globalThis.CSS.supports = _origCSSSupports;
    document.body.innerHTML = '';
});

/** The page swap an OP_REPLACE on the bare view root produces. */
function replaceOp(text) {
    return [OP.REPLACE, 'page', `<div id="page" az-view><h1 az="0">${text}</h1></div>`];
}

// ---------------------------------------------------------------------------
// SPA navigation (B)
// ---------------------------------------------------------------------------

describe('view transitions -- SPA navigation', () => {
    it('wraps the navigation OP_REPLACE when a transition command preceded navigate', () => {
        const fn = stubViewTransitions();
        const w = setupMockWorker();
        w.simulateOpen();
        document.body.innerHTML = '<div id="page" az-view><h1 az="0">Home</h1></div>';

        executeJS(document.body, null, [
            [JS_TRANSITION, { types: ['slide'] }],
            [JS_NAVIGATE, '/transitions/detail'],
        ]);
        w.simulateMessage([replaceOp('Detail')]);

        expect(fn).toHaveBeenCalledOnce();
        expect(resolveEl('page:0').textContent).toBe('Detail');
    });

    it('passes types via the object form when supported, callback form otherwise', () => {
        const fn = stubViewTransitions();
        stubCSSSupports(true);
        document.body.innerHTML = '<div id="page" az-view><h1 az="0">a</h1></div>';
        executeJS(document.body, null, [JS_TRANSITION, { types: ['slide'] }]);
        applyOps([replaceOp('b')]);
        expect(typeof fn.mock.calls[0][0]).toBe('object');
        expect(fn.mock.calls[0][0].types).toEqual(['slide']);

        fn.mockClear();
        stubCSSSupports(false);
        executeJS(document.body, null, [JS_TRANSITION, { types: ['slide'] }]);
        applyOps([replaceOp('c')]);
        expect(typeof fn.mock.calls[0][0]).toBe('function');
    });

    it('does not wrap when no transition was triggered', () => {
        const fn = stubViewTransitions();
        document.body.innerHTML = '<div id="page" az-view><h1 az="0">a</h1></div>';
        applyOps([replaceOp('b')]);
        expect(fn).not.toHaveBeenCalled();
        expect(resolveEl('page:0').textContent).toBe('b');
    });

    it('skips the transition (instant swap) under prefers-reduced-motion', () => {
        const fn = stubViewTransitions();
        stubMatchMedia(true);
        document.body.innerHTML = '<div id="page" az-view><h1 az="0">a</h1></div>';
        executeJS(document.body, null, [JS_TRANSITION, {}]);
        applyOps([replaceOp('b')]);
        expect(fn).not.toHaveBeenCalled();
        expect(resolveEl('page:0').textContent).toBe('b');
    });

    it('swaps directly without throwing when the API is unsupported', () => {
        delete document.startViewTransition;
        document.body.innerHTML = '<div id="page" az-view><h1 az="0">a</h1></div>';
        executeJS(document.body, null, [JS_TRANSITION, {}]);
        expect(() => applyOps([replaceOp('b')])).not.toThrow();
        expect(resolveEl('page:0').textContent).toBe('b');
    });

    it('keeps the intent until an OP_REPLACE batch arrives (stray diff first)', () => {
        const fn = stubViewTransitions();
        document.body.innerHTML = '<div id="page" az-view><span az="0">x</span></div>';
        executeJS(document.body, null, [JS_TRANSITION, {}]);
        // A non-REPLACE batch lands first -- must not consume the intent.
        applyOps([[OP.TEXT, 'page:0', 'y']]);
        expect(fn).not.toHaveBeenCalled();
        // The REPLACE batch then wraps.
        applyOps([replaceOp('z')]);
        expect(fn).toHaveBeenCalledOnce();
    });
});

// ---------------------------------------------------------------------------
// az-transition link attribute
// ---------------------------------------------------------------------------

describe('view transitions -- az-transition link', () => {
    it('wraps the navigation and parses space-separated types from the attribute', () => {
        const fn = stubViewTransitions();
        const w = setupMockWorker();
        w.simulateOpen();
        document.body.innerHTML =
            '<div id="page" az-view>' +
            '<a id="lnk" href="/transitions/detail" az-navigate az-transition="slide back">go</a>' +
            '</div>';

        document
            .getElementById('lnk')
            .dispatchEvent(new MouseEvent('click', { bubbles: true, button: 0 }));
        w.simulateMessage([replaceOp('Detail')]);

        expect(fn).toHaveBeenCalledOnce();
        expect(fn.mock.calls[0][0].types).toEqual(['slide', 'back']);
    });

    it('normalizes stray whitespace and a bare attribute to a plain cross-fade', () => {
        const fn = stubViewTransitions();
        const w = setupMockWorker();
        w.simulateOpen();
        document.body.innerHTML =
            '<div id="page" az-view>' +
            '<a id="lnk" href="/transitions/detail" az-navigate az-transition="  slide  ">go</a>' +
            '</div>';

        document
            .getElementById('lnk')
            .dispatchEvent(new MouseEvent('click', { bubbles: true, button: 0 }));
        w.simulateMessage([replaceOp('Detail')]);

        // "  slide  " trims to a single type; no empty tokens.
        expect(fn.mock.calls[0][0].types).toEqual(['slide']);
    });

    it('does not transition an az-navigate link without az-transition', () => {
        const fn = stubViewTransitions();
        const w = setupMockWorker();
        w.simulateOpen();
        document.body.innerHTML =
            '<div id="page" az-view>' +
            '<a id="lnk" href="/transitions/detail" az-navigate>go</a>' +
            '</div>';

        document
            .getElementById('lnk')
            .dispatchEvent(new MouseEvent('click', { bubbles: true, button: 0 }));
        w.simulateMessage([replaceOp('Detail')]);

        expect(fn).not.toHaveBeenCalled();
    });
});

// ---------------------------------------------------------------------------
// Back/forward (popstate) replay from history state
// ---------------------------------------------------------------------------

describe('view transitions -- back/forward', () => {
    it('replays the transition stamped on the history entry', () => {
        const fn = stubViewTransitions();
        const w = setupMockWorker();
        w.simulateOpen();
        document.body.innerHTML = '<div id="page" az-view><h1 az="0">a</h1></div>';

        // Move to a different path so popstate is a real round-trip, and provide
        // the entry state a transitioned push would have stamped.
        history.pushState({ _azTransition: { types: ['back'] } }, '', '/transitions');
        window.dispatchEvent(
            new PopStateEvent('popstate', { state: { _azTransition: { types: ['back'] } } }),
        );
        w.simulateMessage([replaceOp('popped')]);

        expect(fn).toHaveBeenCalledOnce();
        expect(fn.mock.calls[0][0].types).toEqual(['back']);
    });
});

// ---------------------------------------------------------------------------
// Client-side effects (C)
// ---------------------------------------------------------------------------

describe('view transitions -- client-side effects', () => {
    it('wraps a synchronous effect composed after a transition command', () => {
        const fn = stubViewTransitions();
        document.body.innerHTML = '<div id="panel" hidden>x</div>';
        executeJS(document.body, null, [
            [JS_TRANSITION, {}],
            [JS_TOGGLE, '#panel'],
        ]);
        expect(fn).toHaveBeenCalledOnce();
        // The toggle ran inside the transition callback.
        expect(document.getElementById('panel').hidden).toBe(false);
    });

    it('runs the effect directly (no wrap) when the API is unsupported', () => {
        delete document.startViewTransition;
        document.body.innerHTML = '<div id="panel" hidden>x</div>';
        expect(() =>
            executeJS(document.body, null, [
                [JS_TRANSITION, {}],
                [JS_TOGGLE, '#panel'],
            ]),
        ).not.toThrow();
        expect(document.getElementById('panel').hidden).toBe(false);
    });
});
