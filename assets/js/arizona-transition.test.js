import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';
import { connect, executeJS, OP } from './arizona.js';

// Op codes (must match include/arizona_effect.hrl).
const JS_PUSH_EVENT = 0;
const JS_TOGGLE = 1;
const JS_ADD_CLASS = 4;
const JS_NAVIGATE = 10;
const JS_PATCH = 24;
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
        simulateClose: (code = 1006) => worker.onmessage({ data: [2, code] }),
        simulateOpen: (reconnect = true) => worker.onmessage({ data: [1, reconnect] }),
        simulateMessage: (ops, effects = null, ref = null) =>
            worker.onmessage({ data: [0, ops, effects, false, null, ref] }),
        // The reply ref the last frame sent carries, or null when it carries none.
        lastRef: () => {
            const frames = posted.filter((d) => d[0] === 1).map((d) => JSON.parse(d[1]));
            const last = frames.at(-1);
            const ref = last?.at(-1);
            return typeof ref === 'number' ? ref : null;
        },
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

    it('a transition around a hash-only navigate does not wait for a later page swap', () => {
        const fn = document.startViewTransition;
        Element.prototype.scrollIntoView = vi.fn();
        document.body.innerHTML =
            '<div id="page" az-view><h1 az="0">a</h1><section id="sec">s</section></div>';
        executeJS(document.body, null, [JS_TRANSITION, {}, [JS_NAVIGATE, '#sec']]);
        delete Element.prototype.scrollIntoView;
        expect(location.hash).toBe('#sec');
        // No frame was sent, so the jump itself is what animates, in place.
        expect(_mock.posted.some((d) => d[0] === 1)).toBe(false);
        expect(fn).toHaveBeenCalledOnce();
        _mock.simulateMessage([replaceOp('Other')]);
        expect(fn).toHaveBeenCalledOnce();
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
    it('wraps the reply diff, even when it is not a REPLACE', () => {
        const fn = document.startViewTransition;
        document.body.innerHTML = '<div id="page" az-view><p az="0">old</p></div>';
        executeJS(document.body, null, [JS_TRANSITION, {}, [JS_PUSH_EVENT, 'load_more']]);
        const ref = _mock.lastRef();
        expect(ref).toEqual(expect.any(Number));
        _mock.simulateMessage([[OP.TEXT, 'page:0', 'new']], null, ref);
        expect(fn).toHaveBeenCalledOnce();
        expect(document.querySelector('#page [az="0"]').innerHTML).toBe('new');
    });

    it('does not let an effects-only reply linger onto a later diff', () => {
        const fn = document.startViewTransition;
        document.body.innerHTML = '<div id="page" az-view><p az="0">old</p></div>';
        executeJS(document.body, null, [JS_TRANSITION, {}, [JS_PUSH_EVENT, 'noop']]);
        // The event produced no diff (effects-only response) -- the intent is
        // consumed, not held.
        _mock.simulateMessage(null, [[JS_SET_TITLE, 'x']], _mock.lastRef());
        expect(fn).not.toHaveBeenCalled();
        // A later, unrelated diff must NOT inherit the stale intent.
        _mock.simulateMessage([[OP.TEXT, 'page:0', 'new']]);
        expect(fn).not.toHaveBeenCalled();
    });

    // An event that changes nothing used to get no frame at all, so the intent
    // waited for the next frame of any kind -- a push seconds later animated.
    it('is consumed by a reply that changes nothing', () => {
        const fn = document.startViewTransition;
        document.body.innerHTML = '<div id="page" az-view><p az="0">old</p></div>';
        executeJS(document.body, null, [JS_TRANSITION, {}, [JS_PUSH_EVENT, 'noop']]);
        _mock.simulateMessage(null, null, _mock.lastRef());
        _mock.simulateMessage([[OP.TEXT, 'page:0', 'tick']]);
        expect(fn).not.toHaveBeenCalled();
    });

    it('does not wrap a push that lands ahead of the reply', () => {
        const fn = document.startViewTransition;
        document.body.innerHTML = '<div id="page" az-view><p az="0">old</p><p az="1">old</p></div>';
        executeJS(document.body, null, [JS_TRANSITION, {}, [JS_PUSH_EVENT, 'load_more']]);
        const ref = _mock.lastRef();
        _mock.simulateMessage([[OP.TEXT, 'page:1', 'tick']]);
        expect(fn).not.toHaveBeenCalled();
        _mock.simulateMessage([[OP.TEXT, 'page:0', 'new']], null, ref);
        expect(fn).toHaveBeenCalledOnce();
    });

    // Only the frame a transition waits for is tagged: an event sent outside one
    // goes out as it always did.
    it('tags only the event the transition wraps', () => {
        document.body.innerHTML = '<div id="page" az-view><p az="0">old</p></div>';
        executeJS(document.body, null, [JS_PUSH_EVENT, 'plain']);
        expect(_mock.lastRef()).toBeNull();
        executeJS(document.body, null, [JS_TRANSITION, {}, [JS_PUSH_EVENT, 'wrapped']]);
        expect(_mock.lastRef()).toEqual(expect.any(Number));
        executeJS(document.body, null, [JS_PUSH_EVENT, 'after']);
        expect(_mock.lastRef()).toBeNull();
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

    it('animates an az-patch link on its own reply, not a push ahead of it', () => {
        const fn = document.startViewTransition;
        document.body.innerHTML =
            '<div id="page" az-view><p az="0">old</p><p az="1">old</p>' +
            '<a id="lnk" href="/x" az-patch az-transition>go</a></div>';
        document
            .getElementById('lnk')
            .dispatchEvent(new MouseEvent('click', { bubbles: true, cancelable: true, button: 0 }));
        const ref = _mock.lastRef();
        _mock.simulateMessage([[OP.TEXT, 'page:1', 'tick']]);
        expect(fn).not.toHaveBeenCalled();
        _mock.simulateMessage([[OP.TEXT, 'page:0', 'new']], null, ref);
        expect(fn).toHaveBeenCalledOnce();
    });

    // A debounced push_event leaves the transition armed until it is sent; an
    // event sent in between is not the one it waits for.
    it('binds a debounced event to its own reply', () => {
        vi.useFakeTimers();
        try {
            const fn = document.startViewTransition;
            document.body.innerHTML =
                '<div id="page" az-view><p az="0">old</p>' +
                `<button id="b" az-click='[${JS_TRANSITION},{},[${JS_PUSH_EVENT},"slow"]]' ` +
                'az-debounce="100">b</button></div>';
            document.getElementById('b').dispatchEvent(new MouseEvent('click', { bubbles: true }));
            executeJS(document.body, null, [JS_PUSH_EVENT, 'other']);
            expect(_mock.lastRef()).toBeNull();
            vi.advanceTimersByTime(100);
            const ref = _mock.lastRef();
            expect(ref).toEqual(expect.any(Number));
            _mock.simulateMessage([[OP.TEXT, 'page:0', 'new']], null, ref);
            expect(fn).toHaveBeenCalledOnce();
        } finally {
            vi.useRealTimers();
        }
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

    it('animates a hash-less link to the current URL, which reaches the server', () => {
        const fn = document.startViewTransition;
        history.replaceState(null, '', '/x');
        document.body.innerHTML =
            '<div id="page" az-view><a id="lnk" href="/x" az-navigate az-transition>go</a></div>';
        document
            .getElementById('lnk')
            .dispatchEvent(new MouseEvent('click', { bubbles: true, cancelable: true, button: 0 }));
        expect(_mock.posted.some((d) => d[0] === 1 && JSON.parse(d[1])[0] === 'navigate')).toBe(
            true,
        );
        _mock.simulateMessage([replaceOp('Again')]);
        expect(fn).toHaveBeenCalledOnce();
    });

    it('a hash-only link leaves no transition for a later page swap', () => {
        const fn = document.startViewTransition;
        Element.prototype.scrollIntoView = vi.fn();
        document.body.innerHTML =
            '<div id="page" az-view><a id="lnk" href="#sec" az-navigate az-transition>go</a>' +
            '<section id="sec">s</section></div>';
        document
            .getElementById('lnk')
            .dispatchEvent(new MouseEvent('click', { bubbles: true, cancelable: true, button: 0 }));
        delete Element.prototype.scrollIntoView;
        expect(location.hash).toBe('#sec');
        // An unrelated page swap (a server-driven navigate) must not inherit it.
        _mock.simulateMessage([replaceOp('Other')]);
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

// An 'any' transition is bound to the frame issued under it. Until that frame
// goes out it is only an intent, and nothing else may claim it, keep it alive, or
// write it into history.
describe('view transitions -- reply binding', () => {
    const debounced = () => {
        document.body.innerHTML =
            '<div id="page" az-view><p az="0">old</p>' +
            `<button id="b" az-click='[${JS_TRANSITION},{},[${JS_PUSH_EVENT},"slow"]]' ` +
            'az-debounce="100">b</button></div>';
        document.getElementById('b').dispatchEvent(new MouseEvent('click', { bubbles: true }));
    };

    // The push_event is dropped by the page-swap window its own navigate opened;
    // the transition is the navigate's to animate, not the event's to cancel.
    it('a dropped event leaves a navigate transition armed', () => {
        const fn = document.startViewTransition;
        document.body.innerHTML = '<div id="page" az-view><p az="0">old</p></div>';
        const warn = vi.spyOn(console, 'warn').mockImplementation(() => {});
        executeJS(document.body, null, [
            JS_TRANSITION,
            {},
            [
                [JS_NAVIGATE, '/z'],
                [JS_PUSH_EVENT, 'after'],
            ],
        ]);
        warn.mockRestore();
        _mock.simulateMessage([replaceOp('Z')], null, _mock.lastRef());
        expect(fn).toHaveBeenCalledOnce();
    });

    // The debounced event belongs to the page a navigate replaces; its intent
    // must not ride onto the new page and animate whatever patch comes next.
    it('a navigate drops a transition still waiting for its event', () => {
        vi.useFakeTimers();
        try {
            const fn = document.startViewTransition;
            debounced();
            executeJS(document.body, null, [JS_NAVIGATE, '/n']);
            expect(history.state?._azTransitionPrev).toBeUndefined();
            _mock.simulateMessage([replaceOp('N')], null, _mock.lastRef());
            executeJS(document.body, null, [JS_PATCH, '/n?tab=2']);
            _mock.simulateMessage([[OP.TEXT, 'page:0', 'tab']], null, _mock.lastRef());
            expect(fn).not.toHaveBeenCalled();
        } finally {
            vi.useRealTimers();
        }
    });

    // The worker drops a frame sent while disconnected, so its reply never comes:
    // the transition goes with it instead of sitting armed past the resync.
    it('an event raised while disconnected drops its transition', () => {
        document.body.innerHTML = '<div id="page" az-view><p az="0">old</p></div>';
        _mock.simulateClose();
        executeJS(document.body, null, [JS_TRANSITION, {}, [JS_PUSH_EVENT, 'offline']]);
        expect(_mock.lastRef()).toBeNull();
        _mock.simulateOpen();
        _mock.simulateMessage([replaceOp('Back')]);
        executeJS(document.body, null, [JS_NAVIGATE, '/after']);
        expect(history.state?._azTransitionPrev).toBeUndefined();
    });

    // A plain patch link runs under no transition: it neither takes nor clears the
    // one still waiting for its debounced event.
    it('a plain az-patch link leaves a transition waiting for its event', () => {
        vi.useFakeTimers();
        try {
            const fn = document.startViewTransition;
            debounced();
            document.body.insertAdjacentHTML(
                'beforeend',
                '<a id="p" href="/?tab=3" az-patch>p</a>',
            );
            document
                .getElementById('p')
                .dispatchEvent(
                    new MouseEvent('click', { bubbles: true, cancelable: true, button: 0 }),
                );
            _mock.simulateMessage([[OP.TEXT, 'page:0', 'tab']], null, _mock.lastRef());
            expect(fn).not.toHaveBeenCalled();
            vi.advanceTimersByTime(100);
            _mock.simulateMessage([[OP.TEXT, 'page:0', 'slow']], null, _mock.lastRef());
            expect(fn).toHaveBeenCalledOnce();
        } finally {
            vi.useRealTimers();
        }
    });

    // The swap window drops the event, so its transition must not displace the
    // navigate's own, which would then die with it.
    it('an event transition raised in the swap window keeps the navigate one', () => {
        const fn = document.startViewTransition;
        document.body.innerHTML = '<div id="page" az-view><p az="0">old</p></div>';
        executeJS(document.body, null, [JS_TRANSITION, {}, [JS_NAVIGATE, '/z']]);
        const ref = _mock.lastRef();
        const warn = vi.spyOn(console, 'warn').mockImplementation(() => {});
        executeJS(document.body, null, [JS_TRANSITION, {}, [JS_PUSH_EVENT, 'during']]);
        warn.mockRestore();
        _mock.simulateMessage([replaceOp('Z')], null, ref);
        expect(fn).toHaveBeenCalledOnce();
    });

    // A patch not issued under the transition is not the frame it waits for: the
    // debounced event still goes out on this page and is the one animated.
    it('an unrelated patch does not take a transition waiting for its event', () => {
        vi.useFakeTimers();
        try {
            const fn = document.startViewTransition;
            debounced();
            executeJS(document.body, null, [JS_PATCH, '/?tab=2']);
            _mock.simulateMessage([[OP.TEXT, 'page:0', 'tab']], null, _mock.lastRef());
            expect(fn).not.toHaveBeenCalled();
            vi.advanceTimersByTime(100);
            _mock.simulateMessage([[OP.TEXT, 'page:0', 'slow']], null, _mock.lastRef());
            expect(fn).toHaveBeenCalledOnce();
        } finally {
            vi.useRealTimers();
        }
    });
});
