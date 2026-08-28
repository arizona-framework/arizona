import { afterEach, describe, expect, it, vi } from 'vitest';
import {
    applyOps,
    executeJS,
    hooks,
    mountHooks,
    OP,
    requestPip,
    resolveEl,
    restoreFormState,
    saveFormState,
} from './arizona.js';

// A minimal stand-in for a Document Picture-in-Picture window, backed by a real
// (jsdom) Document, so the multi-document patch/effect paths can be exercised
// without a Chromium PiP window (which jsdom can't provide). `close()` fires the
// registered pagehide handler, like a real window closing.
function makePipWindow() {
    const doc = document.implementation.createHTMLDocument('pip');
    /** @type {Record<string, Function>} */
    const handlers = {};
    return {
        document: doc,
        closed: false,
        /** @param {string} type @param {Function} fn */
        addEventListener(type, fn) {
            handlers[type] = fn;
        },
        /** @param {string} type */
        fire(type) {
            handlers[type]?.();
        },
        close() {
            this.closed = true;
            handlers.pagehide?.();
        },
    };
}

/** @param {ReturnType<typeof makePipWindow>} pip */
function stubPip(pip) {
    const requestWindow = vi.fn().mockResolvedValue(pip);
    /** @type {any} */ (window).documentPictureInPicture = { requestWindow };
    return requestWindow;
}

/**
 * A module instance with no discovery history. The delegated event registry is
 * module state and MONOTONIC, so a test sharing the import above inherits every
 * type earlier tests declared: whether a window is bound for a type would then
 * depend on test order, and an earlier test rendering the same `az-*` attribute
 * would decide the outcome before this one runs.
 */
async function fresh() {
    vi.resetModules();
    return await import('./arizona.js');
}

/** The connection the running test opened, torn down in afterEach. */
/** @type {{sent: () => *[], restore: () => void} | null} */
let openConn = null;

/**
 * Connect `mod` through a mocked Worker and open the socket, exposing what the
 * delegated commands pushed. Delegation is gated on a live connection, so a test
 * that fires an event in a PiP document needs one.
 * @param {any} mod
 */
function connectMock(mod) {
    const posted = [];
    let onmsg = null;
    const inst = {
        postMessage: (/** @type {*} */ d) => posted.push(d),
        set onmessage(fn) {
            onmsg = fn;
        },
        get onmessage() {
            return onmsg;
        },
        terminate: vi.fn(),
    };
    const Orig = globalThis.Worker;
    globalThis.Worker = function () {
        return inst;
    };
    const disconnect = mod.connect('/ws');
    inst.onmessage({ data: [1, false] });
    openConn = {
        sent: () => posted.filter((d) => d[0] === 1).map((d) => JSON.parse(d[1])),
        restore: () => {
            disconnect();
            globalThis.Worker = Orig;
        },
    };
    return openConn;
}

afterEach(() => {
    // In the test body a failed assertion skips the teardown, leaving the Worker
    // constructor mocked for every later test in this file.
    openConn?.restore();
    openConn = null;
    /** @type {any} */ (window).documentPictureInPicture = undefined;
    document.body.innerHTML = '';
});

describe('Document Picture-in-Picture (multi-document views)', () => {
    it('returns null when Document PiP is unavailable', async () => {
        document.body.innerHTML = '<div id="v0" az-view az="0"></div>';
        expect(await requestPip('v0')).toBeNull();
    });

    it('moves the view into the PiP document and patches it there', async () => {
        document.body.innerHTML =
            '<div id="v1" az-view az="0"><span az="1" class="a"></span></div>';
        const pip = makePipWindow();
        stubPip(pip);

        const win = await requestPip('v1');
        expect(win).toBe(pip);

        // The view left the main document and now lives in the PiP document.
        expect(document.getElementById('v1')).toBeNull();
        expect(pip.document.getElementById('v1')).not.toBeNull();

        // resolveEl targets the PiP document, and a server patch lands there.
        expect(resolveEl('v1:1')?.ownerDocument).toBe(pip.document);
        applyOps([[1, 'v1:1', 'class', 'b']]); // OP.SET_ATTR
        expect(pip.document.querySelector('[az="1"]')?.getAttribute('class')).toBe('b');

        pip.fire('pagehide'); // restore
    });

    it("preserves a popped-out form's typed values across a reconnect", async () => {
        document.body.innerHTML =
            '<div id="v7" az-view az="0"><form id="pf"><input name="note" /></form></div>';
        const pip = makePipWindow();
        stubPip(pip);
        await requestPip('v7');

        // The form now lives in the PiP document, where the user types into it.
        const typed = pip.document.getElementById('pf').querySelector('input');
        typed.value = 'drafted in the pip window';
        saveFormState();

        // The socket drops and the view re-renders empty, as a reconnect would.
        typed.value = '';
        restoreFormState();

        expect(pip.document.getElementById('pf').querySelector('input').value).toBe(
            'drafted in the pip window',
        );

        pip.fire('pagehide');
    });

    it('delegates a newly discovered event type into an open PiP document', async () => {
        const mod = await fresh();
        document.body.innerHTML =
            '<div id="v6" az-view az="0"><dialog id="d6" az="1"></dialog></div>';
        const conn = connectMock(mod);
        const pip = makePipWindow();
        stubPip(pip);
        await mod.requestPip('v6');

        // The type is discovered after the window opened, so it has to reach every
        // hosting document, not just the main one -- the view now LIVES in the PiP
        // document, so binding only the main document would delegate nothing.
        mod.applyOps([[1, 'v6:1', 'az-close', '[0,"closed"]']]);
        pip.document.getElementById('d6').dispatchEvent(new Event('close'));

        expect(conn.sent()).toEqual([['v6', 'closed', {}]]);

        pip.fire('pagehide');
    });

    it('delegates a type discovered before the window opened', async () => {
        const mod = await fresh();
        document.body.innerHTML =
            '<div id="v10" az-view az="0"><dialog id="d10" az="1"></dialog></div>';
        const conn = connectMock(mod);

        // Discovered while the view is still inline...
        mod.applyOps([[1, 'v10:1', 'az-close', '[0,"closed"]']]);

        const pip = makePipWindow();
        stubPip(pip);
        await mod.requestPip('v10');

        // ...and the window opens afterwards, so it is the RECORD of what has been
        // discovered that binds it, not the discovery itself: a type only pushed to
        // the documents open at the time is delegated nowhere in a window opened
        // later, and every event fired in the floating window is lost.
        pip.document.getElementById('d10').dispatchEvent(new Event('close'));

        expect(conn.sent()).toEqual([['v10', 'closed', {}]]);

        pip.fire('pagehide');
    });

    it('stops delegating new event types into a window that has closed', async () => {
        const mod = await fresh();
        document.body.innerHTML = '<div id="v11" az-view az="0"><b id="b11" az="1"></b></div>';
        const conn = connectMock(mod);
        const pip = makePipWindow();
        stubPip(pip);
        await mod.requestPip('v11');
        pip.fire('pagehide'); // the user closes the floating window

        const bound = [];
        const orig = pip.document.addEventListener.bind(pip.document);
        pip.document.addEventListener = (t, fn, opts) => {
            bound.push(t);
            return orig(t, fn, opts);
        };

        // A type discovered afterwards must reach the documents still hosting a
        // view. The closed window is not one, and only the registry shows it: the
        // window's listeners went with its AbortSignal, which also makes a re-add
        // there a silent no-op -- so a document left in the registry is a dead
        // Document kept alive and re-bound on every discovery, with no symptom.
        mod.applyOps([[1, 'v11:1', 'az-toggle', '[0,"toggled"]']]);
        expect(bound).toEqual([]);

        // ...while the view, restored inline, does get the new type delegated.
        document.getElementById('b11').dispatchEvent(new Event('toggle'));
        expect(conn.sent()).toEqual([['v11', 'toggled', {}]]);
    });

    it('routes ops for a nested stateful child into the PiP document', async () => {
        // A stateful child (its own `az-view`/id boundary) nested inside the
        // popped region. Only the root's id is registered in `_viewDocs`; the
        // child rides along into the PiP document and must keep updating.
        document.body.innerHTML =
            '<div id="p1" az-view az="0"><div id="region">' +
            '<div id="c1" az-view az="0"><span az="1" class="a"></span></div>' +
            '</div></div>';
        const pip = makePipWindow();
        stubPip(pip);

        await requestPip('p1');

        // The nested child moved into the PiP document with its ancestor.
        expect(pip.document.getElementById('c1')).not.toBeNull();

        // A diff for the CHILD (its own view id, never registered) resolves to
        // the PiP document by DOM containment, and the patch lands there.
        expect(resolveEl('c1:1')?.ownerDocument).toBe(pip.document);
        applyOps([[1, 'c1:1', 'class', 'b']]); // OP.SET_ATTR
        expect(pip.document.querySelector('#c1 [az="1"]')?.getAttribute('class')).toBe('b');

        pip.fire('pagehide'); // restore
        // The child routes back to the main document once the region is restored.
        expect(resolveEl('c1:1')?.ownerDocument).toBe(document);
    });

    it('routes selector effects to the PiP document', async () => {
        document.body.innerHTML = '<div id="v2" az-view az="0"><b az="1"></b></div>';
        const pip = makePipWindow();
        stubPip(pip);
        await requestPip('v2');

        // JS_ADD_CLASS (4) targets by CSS selector -- withQuery must search the PiP doc.
        executeJS(document.documentElement, null, [4, '[az="1"]', 'hl']);
        expect(pip.document.querySelector('[az="1"]')?.classList.contains('hl')).toBe(true);

        pip.fire('pagehide');
    });

    it('dispatches a server-driven custom event into the PiP document too', async () => {
        document.body.innerHTML = '<div id="v8" az-view az="0"></div>';
        const pip = makePipWindow();
        stubPip(pip);
        await requestPip('v8');

        let mainSeen = 0;
        let pipSeen = 0;
        const onMain = () => mainSeen++;
        const onPip = () => pipSeen++;
        document.addEventListener('az-test-evt', onMain);
        pip.document.addEventListener('az-test-evt', onPip);

        // JS_DISPATCH_EVENT (9): a listener in a popped-out view is as legitimate
        // a target as one on the main document, like the selector effects.
        executeJS(document.documentElement, null, [9, 'az-test-evt', { n: 1 }]);
        expect(mainSeen).toBe(1);
        expect(pipSeen).toBe(1);

        document.removeEventListener('az-test-evt', onMain);
        pip.fire('pagehide');
    });

    it('carries the effect detail to every document it reaches', async () => {
        document.body.innerHTML = '<div id="v9" az-view az="0"></div>';
        const pip = makePipWindow();
        stubPip(pip);
        await requestPip('v9');

        /** @type {Array<*>} */
        const details = [];
        const onMain = (/** @type {CustomEvent} */ e) => details.push(e.detail);
        const onPip = (/** @type {CustomEvent} */ e) => details.push(e.detail);
        document.addEventListener('az-detail-evt', onMain);
        pip.document.addEventListener('az-detail-evt', onPip);

        executeJS(document.documentElement, null, [9, 'az-detail-evt', { id: 7 }]);
        expect(details).toEqual([{ id: 7 }, { id: 7 }]);

        document.removeEventListener('az-detail-evt', onMain);
        pip.fire('pagehide');
    });

    it('restores the view inline when the window closes', async () => {
        document.body.innerHTML = '<div id="v3" az-view az="0"></div>';
        const pip = makePipWindow();
        stubPip(pip);
        await requestPip('v3');
        expect(document.getElementById('v3')).toBeNull();

        pip.fire('pagehide');
        expect(document.getElementById('v3')).not.toBeNull();
        // Patches resolve against the main document again.
        expect(resolveEl('v3:0')?.ownerDocument).toBe(document);
    });

    it('JS_REQUEST_PIP effect triggers a pop-out at the browser default size', async () => {
        document.body.innerHTML = '<div id="v4" az-view az="0"></div>';
        const pip = makePipWindow();
        const requestWindow = stubPip(pip);

        executeJS(document.documentElement, null, [18, 'v4']); // JS_REQUEST_PIP
        expect(requestWindow).toHaveBeenCalledOnce();
        // No framework default leaks: with no options the user agent picks the size.
        expect(requestWindow).toHaveBeenCalledWith({});

        await Promise.resolve();
        await Promise.resolve();
        pip.fire('pagehide'); // cleanup
    });

    it('does not leak a placeholder when the PiP request is rejected', async () => {
        document.body.innerHTML = '<div id="v6" az-view az="0"></div>';
        const requestWindow = vi.fn().mockRejectedValue(new Error('no user gesture'));
        /** @type {any} */ (window).documentPictureInPicture = { requestWindow };

        await expect(requestPip('v6')).rejects.toThrow();

        // The view stays inline and no `az-pip:` placeholder comment is orphaned.
        expect(document.getElementById('v6')).not.toBeNull();
        const orphaned = [...document.body.childNodes].some(
            (n) => n.nodeType === 8 && /az-pip:/.test(/** @type {Comment} */ (n).data),
        );
        expect(orphaned).toBe(false);
    });

    it('JS_REQUEST_PIP swallows a rejected request (no unhandled rejection)', async () => {
        document.body.innerHTML = '<div id="v7" az-view az="0"></div>';
        const requestWindow = vi.fn().mockRejectedValue(new Error('no user gesture'));
        /** @type {any} */ (window).documentPictureInPicture = { requestWindow };
        const spy = vi.spyOn(console, 'error').mockImplementation(() => {});

        // A server-pushed request_pip has no gesture, so requestWindow rejects; the
        // effect must catch it (a logged no-op), not leave an unhandled rejection.
        executeJS(document.documentElement, null, [18, 'v7']); // JS_REQUEST_PIP
        await Promise.resolve();
        await Promise.resolve();

        expect(spy).toHaveBeenCalled();
        spy.mockRestore();
    });

    it('passes width/height options through to requestWindow', async () => {
        document.body.innerHTML = '<div id="v5" az-view az="0"></div>';
        const pip = makePipWindow();
        const requestWindow = stubPip(pip);

        // JS_REQUEST_PIP with an options map in cmd[2].
        executeJS(document.documentElement, null, [18, 'v5', { width: 440, height: 940 }]);
        expect(requestWindow).toHaveBeenCalledWith({ width: 440, height: 940 });

        await Promise.resolve();
        await Promise.resolve();
        pip.fire('pagehide'); // cleanup
    });

    it('closes an orphaned PiP window when a navigate replaces the outgoing page', async () => {
        // A child view popped out, then the ROOT is replaced (SPA navigate):
        // the placeholder dies with the outgoing subtree, so the floating view
        // shows dead server state and must be closed + discarded.
        document.body.innerHTML =
            '<div id="root" az-view az="0"><div id="player" az-view az="0" az-hook="H"></div></div>';
        const destroyed = vi.fn();
        hooks.H = { destroyed };
        mountHooks(document);
        const pip = makePipWindow();
        stubPip(pip);
        await requestPip('player');
        expect(pip.document.getElementById('player')).not.toBeNull();

        applyOps([[OP.REPLACE, 'root', '<div id="other" az-view az="0">new page</div>']]);

        // The window was closed, the stale element discarded (NOT dumped into
        // the new page's body), and its hook torn down.
        expect(pip.closed).toBe(true);
        expect(document.getElementById('player')).toBeNull();
        expect(destroyed).toHaveBeenCalledOnce();
        // The PiP document is unregistered: the view no longer resolves.
        expect(resolveEl('player')).toBeNull();

        delete hooks.H;
    });

    it('keeps an unrelated PiP window open across a navigate', async () => {
        // Two roots: one is replaced, the other's popped-out child keeps its
        // placeholder -- its window must survive.
        document.body.innerHTML =
            '<div id="root" az-view az="0">page</div>' +
            '<div id="side" az-view az="0"><div id="player" az-view az="0"></div></div>';
        const pip = makePipWindow();
        stubPip(pip);
        await requestPip('player');

        applyOps([[OP.REPLACE, 'root', '<div id="other" az-view az="0">new</div>']]);

        expect(pip.closed).toBe(false);
        expect(resolveEl('player')?.ownerDocument).toBe(pip.document);

        pip.fire('pagehide'); // cleanup
    });

    it('discards the stale view on close when its placeholder is gone', async () => {
        // The placeholder is wiped by an inner re-render (not a REPLACE, so no
        // auto-close); when the user then closes the window, the stale element
        // must be discarded, not appended to the page body.
        document.body.innerHTML =
            '<div id="root" az-view az="0"><div id="player" az-view az="0" az-hook="H"></div></div>';
        const destroyed = vi.fn();
        hooks.H = { destroyed };
        mountHooks(document);
        const pip = makePipWindow();
        stubPip(pip);
        await requestPip('player');

        // A wholesale OP.TEXT on the root wipes the inline placeholder.
        applyOps([[OP.TEXT, 'root:0', 'wiped', true]]);
        pip.fire('pagehide'); // user closes the floating window

        expect(document.getElementById('player')).toBeNull();
        expect(document.body.contains(pip.document.getElementById('player'))).toBe(false);
        expect(destroyed).toHaveBeenCalledOnce();

        delete hooks.H;
    });
});
