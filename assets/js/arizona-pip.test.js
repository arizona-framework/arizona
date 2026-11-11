import { afterEach, describe, expect, it, vi } from 'vitest';
import {
    applyOps,
    executeJS,
    exitPip,
    hooks,
    mountHooks,
    OP,
    requestPip,
    resolveEl,
    restoreFormState,
    saveFormState,
} from './arizona.js';
import { _viewDocs } from './arizona-docs.js';

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

/**
 * Host view `viewId` in a document from ANOTHER realm (an iframe's window), as a
 * real PiP window is: markup parsed there (a resync's `OP_REPLACE`) builds
 * elements from that window's constructors, so the opener's `instanceof` fails
 * on them. Registered directly rather than through `requestPip`, whose listeners
 * jsdom refuses to bind with an opener-realm AbortSignal.
 * @param {string} viewId
 * @param {string} html -- the view root's markup, parsed by the foreign document
 * @returns {{doc: Document, close: () => void}}
 */
function hostInForeignRealm(viewId, html) {
    const frame = document.createElement('iframe');
    document.documentElement.appendChild(frame);
    const doc = /** @type {Document} */ (frame.contentDocument);
    doc.body.innerHTML = html;
    _viewDocs.set(viewId, doc);
    return {
        doc,
        close: () => {
            _viewDocs.delete(viewId);
            frame.remove();
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
 * A module instance with no delegation history. The delegated event registry is
 * module state and MONOTONIC, so a test sharing the import above inherits every
 * type earlier tests declared: whether a window is bound for a type would then
 * depend on test order.
 */
async function fresh() {
    vi.resetModules();
    return await import('./arizona.js');
}

/** The connection the running test opened, torn down in afterEach. */
/** @type {{sent: () => *[], reconnect: () => void, restore: () => void} | null} */
let openConn = null;

/**
 * Connect `mod` through a mocked Worker and open the socket, exposing what the
 * delegated commands pushed. Delegation is gated on a live connection, so a test
 * that fires an event in a PiP document needs one.
 * @param {any} mod
 */
function connectMock(mod) {
    /** @type {*[]} */
    const posted = [];
    /** @type {*} */
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
    let disconnect = mod.connect('/ws');
    inst.onmessage({ data: [1, false] });
    openConn = {
        sent: () => posted.filter((d) => d[0] === 1).map((d) => JSON.parse(d[1])),
        /** Retire this connection and open a fresh one against the same document. */
        reconnect: () => {
            disconnect();
            disconnect = mod.connect('/ws');
            inst.onmessage({ data: [1, false] });
        },
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

    // A replace of the popped-out view (a reconnect's resync re-renders it in its
    // window) leaves the element moved out a stale copy: closing the window brings
    // back the view as it is now.
    it('brings back the view a replace re-rendered in the PiP document', async () => {
        document.body.innerHTML = '<div id="v20" az-view az="0"><span>old</span></div>';
        const pip = makePipWindow();
        stubPip(pip);
        await requestPip('v20');
        applyOps([[OP.REPLACE, 'v20', '<div id="v20" az-view az="0"><span>new</span></div>']]);
        expect(pip.document.querySelector('#v20 span')?.textContent).toBe('new');

        pip.fire('pagehide');

        expect(document.querySelector('#v20 span')?.textContent).toBe('new');
        expect(pip.document.getElementById('v20')).toBeNull();
    });

    // A navigate of a popped-out ROOT view lands in its window (the root is
    // resolved there) under the new page's id, while the placeholder stays inline:
    // closing the window brings back the new page, not the stale element moved out.
    it('brings back the page a navigate replaced a popped-out root with', async () => {
        document.body.innerHTML = '<div id="r1" az-view az="0"><span>old</span></div>';
        const pip = makePipWindow();
        stubPip(pip);
        await requestPip('r1');
        applyOps([[OP.REPLACE, 'r1', '<div id="r2" az-view az="0"><span>new</span></div>']]);
        expect(pip.closed).toBe(false);
        // A node a hook put in the window is not a view: it stays there.
        pip.document.body.append(pip.document.createElement('aside'));

        pip.fire('pagehide');

        expect(document.getElementById('r1')).toBeNull();
        expect(document.querySelector('aside')).toBeNull();
        expect(document.querySelector('#r2 span')?.textContent).toBe('new');
        expect(resolveEl('r2')?.ownerDocument).toBe(document);
    });

    it('re-keys the window to the page a navigate swapped into it', async () => {
        // exit_pip/request_pip from the new page name the new view id.
        document.body.innerHTML = '<div id="r1" az-view az="0"><span>old</span></div>';
        const pip = makePipWindow();
        Object.defineProperty(pip.document, 'defaultView', { value: pip });
        stubPip(pip);
        await requestPip('r1');
        applyOps([[OP.REPLACE, 'r1', '<div id="r2" az-view az="0"><span>new</span></div>']]);

        expect(await requestPip('r2')).toBeNull();
        expect(resolveEl('r2')?.ownerDocument).toBe(pip.document);
        exitPip('r1');
        expect(pip.closed).toBe(false);
        exitPip('r2');
        expect(pip.closed).toBe(true);
        expect(document.querySelector('#r2 span')?.textContent).toBe('new');

        // Closing forgot the window under the new id: the page pops out again.
        const again = makePipWindow();
        stubPip(again);
        expect(await requestPip('r2')).toBe(again);
        expect(again.document.getElementById('r2')).not.toBeNull();
        again.fire('pagehide');
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

    it('restores typed fields a PiP document parsed in its own realm', () => {
        const markup =
            '<input name="t" /><input type="checkbox" name="c" value="y" />' +
            '<select name="s"><option value="a">a</option><option value="b">b</option></select>' +
            '<textarea name="x"></textarea>';
        const pip = hostInForeignRealm(
            'v13',
            `<div id="v13" az-view az="0"><form id="pf13">${markup}</form></div>`,
        );
        try {
            const form = /** @type {any} */ (pip.doc.getElementById('pf13'));
            expect(form.elements.t instanceof HTMLInputElement).toBe(false);
            form.elements.t.value = 'typed';
            form.elements.c.checked = true;
            form.elements.s.value = 'b';
            form.elements.x.value = 'notes';
            saveFormState();

            // The resync re-renders the form, parsed in the PiP document again.
            form.innerHTML = markup;
            restoreFormState();

            expect(form.elements.t.value).toBe('typed');
            expect(form.elements.c.checked).toBe(true);
            expect(form.elements.s.value).toBe('b');
            expect(form.elements.x.value).toBe('notes');
        } finally {
            pip.close();
        }
    });

    it('creates an SVG child in the SVG namespace in a PiP document', () => {
        const pip = hostInForeignRealm(
            'v14',
            '<div id="v14" az-view az="0"><svg><g az="1"><!--az:1--><!--/az--></g></svg></div>',
        );
        try {
            applyOps([[OP.TEXT, 'v14:1', '<rect x="1"></rect>', true]]);
            expect(pip.doc.querySelector('rect')?.namespaceURI).toBe('http://www.w3.org/2000/svg');
        } finally {
            pip.close();
        }
    });

    it('delegates a newly declared event type into an open PiP document', async () => {
        const mod = await fresh();
        document.body.innerHTML =
            '<div id="v6" az-view az="0"><dialog id="d6" az="1"></dialog></div>';
        const conn = connectMock(mod);
        const pip = makePipWindow();
        stubPip(pip);
        await mod.requestPip('v6');

        // The type is declared after the window opened, so it has to reach every
        // hosting document, not just the main one -- the view now LIVES in the PiP
        // document, so binding only the main document would delegate nothing.
        mod.applyOps([[1, 'v6:1', 'az-close', '[0,"closed"]']]);
        pip.document.getElementById('d6').dispatchEvent(new Event('close'));

        expect(conn.sent()).toEqual([['v6', 'closed', {}]]);

        pip.fire('pagehide');
    });

    it('delegates a type declared before the window opened', async () => {
        const mod = await fresh();
        document.body.innerHTML =
            '<div id="v10" az-view az="0"><dialog id="d10" az="1"></dialog></div>';
        const conn = connectMock(mod);

        // Declared while the view is still inline...
        mod.applyOps([[1, 'v10:1', 'az-close', '[0,"closed"]']]);

        const pip = makePipWindow();
        stubPip(pip);
        await mod.requestPip('v10');

        // ...and the window opens afterwards, so it is the RECORD of what has been
        // delegated that binds it, not the declaration itself: a type only pushed
        // to the documents open at the time is bound nowhere in a window opened
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

        /** @type {string[]} */
        const bound = [];
        const orig = pip.document.addEventListener.bind(pip.document);
        pip.document.addEventListener = (
            /** @type {*} */ t,
            /** @type {*} */ fn,
            /** @type {*} */ opts,
        ) => {
            bound.push(t);
            return orig(t, fn, opts);
        };

        // A type declared afterwards must reach the documents still hosting a
        // view. The closed window is not one, and only the registry shows it: the
        // window's listeners went with its AbortSignal, which also makes a re-add
        // there a silent no-op -- so a document left in the registry is a dead
        // Document kept alive and re-bound on every declaration, with no symptom.
        mod.applyOps([[1, 'v11:1', 'az-toggle', '[0,"toggled"]']]);
        expect(bound).toEqual([]);

        // ...while the view, restored inline, does get the new type delegated.
        document.getElementById('b11').dispatchEvent(new Event('toggle'));
        expect(conn.sent()).toEqual([['v11', 'toggled', {}]]);
    });

    it("tears down a popped-out window's listeners when the connection ends", async () => {
        const mod = await fresh();
        document.body.innerHTML =
            '<div id="v12" az-view az="0"><button id="b12" az-click=\'[0,"clicked"]\'>go</button></div>';
        const conn = connectMock(mod);
        const pip = makePipWindow();
        stubPip(pip);
        await mod.requestPip('v12');

        // A PiP document's listeners hang off requestPip's own AbortController, not
        // the connection's signal, so disconnect() cannot reach them on its own.
        // Left armed, they survive into the NEXT connection and push events from an
        // orphaned window under the previous view's id.
        conn.reconnect();
        pip.document.getElementById('b12').click();

        expect(conn.sent()).toEqual([]);
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
        // A real PiP document's window is the PiP window, which exitPip closes.
        Object.defineProperty(pip.document, 'defaultView', { value: pip });
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

    it('exitPip closes the window, which moves the view back inline', async () => {
        document.body.innerHTML = '<div id="v9" az-view az="0"></div>';
        const pip = makePipWindow();
        // A real PiP document's window is the PiP window, which exitPip closes.
        Object.defineProperty(pip.document, 'defaultView', { value: pip });
        stubPip(pip);
        await requestPip('v9');
        expect(document.getElementById('v9')).toBeNull();

        exitPip('v9');
        expect(pip.closed).toBe(true);
        expect(document.getElementById('v9')).not.toBeNull();
    });

    it('exitPip on a view that is not popped out does nothing', () => {
        document.body.innerHTML = '<div id="v10" az-view az="0"></div>';
        expect(() => exitPip('v10')).not.toThrow();
        expect(document.getElementById('v10')).not.toBeNull();
    });

    it('resolves an unknown view to null while a PiP window is open', async () => {
        document.body.innerHTML = '<div id="v11" az-view az="0"></div>';
        const pip = makePipWindow();
        stubPip(pip);
        await requestPip('v11');
        // Searched in the main document and every PiP document, found in none.
        expect(resolveEl('nope:0')).toBeNull();
        pip.fire('pagehide');
    });

    it("gives the PiP document the page's styles, relinking an unreadable sheet", async () => {
        document.body.innerHTML = '<div id="v12" az-view az="0"></div>';
        const readable = { cssRules: [{ cssText: '.a { color: red; }' }] };
        const crossOrigin = {
            href: 'https://cdn.example/site.css',
            get cssRules() {
                throw new DOMException('cross-origin', 'SecurityError');
            },
        };
        const noHref = {
            href: null,
            get cssRules() {
                throw new DOMException('cross-origin', 'SecurityError');
            },
        };
        const spy = vi
            .spyOn(document, 'styleSheets', 'get')
            .mockReturnValue(/** @type {any} */ ([readable, crossOrigin, noHref]));
        const pip = makePipWindow();
        stubPip(pip);
        try {
            await requestPip('v12');
            const head = pip.document.head;
            expect(head.querySelector('style')?.textContent).toBe('.a { color: red; }');
            const links = head.querySelectorAll('link[rel="stylesheet"]');
            // The sheet with no href has nothing to relink and is skipped.
            expect(Array.from(links, (l) => l.getAttribute('href'))).toEqual([
                'https://cdn.example/site.css',
            ]);
        } finally {
            spy.mockRestore();
            pip.fire('pagehide');
        }
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

    it('tears down the view a resync moved into the window when a navigate orphans it', async () => {
        // The resync re-rendered the popped-out child, so the element in the window
        // is not the one moved out: the navigate's teardown must reach it.
        document.body.innerHTML =
            '<div id="root" az-view az="0">' +
            '<div id="v" az-view az="0"><i az-hook="H" data-n="old"></i></div></div>';
        const destroyed = vi.fn();
        hooks.H = {
            destroyed() {
                destroyed(this.el.dataset.n);
            },
        };
        mountHooks(document);
        const pip = makePipWindow();
        stubPip(pip);
        await requestPip('v');
        const resync =
            '<div id="root" az-view az="0">' +
            '<div id="v" az-view az="0"><i az-hook="H" data-n="new"></i></div></div>';
        applyOps([[OP.REPLACE, 'root', resync]], true);

        applyOps([[OP.REPLACE, 'root', '<div id="other" az-view az="0">x</div>']]);

        expect(pip.closed).toBe(true);
        expect(destroyed.mock.calls).toEqual([['old'], ['new']]);
        expect(pip.document.getElementById('v')).toBeNull();

        delete hooks.H;
    });

    it('keeps a popped-out child in its window across a reconnect', async () => {
        // The resync re-renders the whole root, child included, inline: the fresh
        // child moves into the window and leaves a placeholder, as a pop-out does.
        document.body.innerHTML =
            '<div id="root" az-view az="0"><p>a</p>' +
            '<div id="v" az-view az="0"><i az-hook="H" data-n="old"></i></div></div>';
        const destroyed = vi.fn();
        hooks.H = {
            destroyed() {
                destroyed(this.el.dataset.n);
            },
        };
        mountHooks(document);
        const pip = makePipWindow();
        stubPip(pip);
        await requestPip('v');

        const resync =
            '<div id="root" az-view az="0"><p>b</p>' +
            '<div id="v" az-view az="0"><i az-hook="H" data-n="new"></i></div></div>';
        applyOps([[OP.REPLACE, 'root', resync]], true);

        expect(pip.closed).toBe(false);
        expect(destroyed.mock.calls).toEqual([['old']]);
        expect(pip.document.querySelectorAll('#v')).toHaveLength(1);
        expect(pip.document.querySelector('#v i')?.dataset.n).toBe('new');
        expect(document.getElementById('v')).toBeNull();
        expect(resolveEl('v')?.ownerDocument).toBe(pip.document);

        // Closing brings it back where the fresh render put it.
        pip.fire('pagehide');
        expect(document.querySelector('#root > p + #v i')?.dataset.n).toBe('new');
        expect(destroyed.mock.calls).toEqual([['old']]);

        delete hooks.H;
    });

    it('closes the window when the resync no longer renders the popped-out child', async () => {
        document.body.innerHTML =
            '<div id="root" az-view az="0"><div id="v" az-view az="0" az-hook="H"></div></div>';
        const destroyed = vi.fn();
        hooks.H = { destroyed };
        mountHooks(document);
        const pip = makePipWindow();
        stubPip(pip);
        await requestPip('v');

        applyOps([[OP.REPLACE, 'root', '<div id="root" az-view az="0">gone</div>']], true);

        expect(pip.closed).toBe(true);
        expect(document.getElementById('v')).toBeNull();
        expect(destroyed).toHaveBeenCalledOnce();

        delete hooks.H;
    });

    it('closes the window when a navigate renders a view under the same id', async () => {
        // A navigate mounts a new page: its views are new, whatever their ids.
        document.body.innerHTML =
            '<div id="root" az-view az="0"><div id="v" az-view az="0">old</div></div>';
        const pip = makePipWindow();
        stubPip(pip);
        await requestPip('v');

        applyOps([
            [
                OP.REPLACE,
                'root',
                '<div id="p2" az-view az="0"><div id="v" az-view az="0">new</div></div>',
            ],
        ]);

        expect(pip.closed).toBe(true);
        expect(document.getElementById('v')?.textContent).toBe('new');
        expect(pip.document.getElementById('v')).toBeNull();
    });

    it('keeps the window when a parent re-render renders the popped-out view again', async () => {
        // A parent's wholesale re-render wipes the placeholder and renders the
        // child view inline again: the fresh copy moves into the window, so the
        // window shows the live view and the page keeps a placeholder.
        document.body.innerHTML =
            '<div id="root" az-view az="0">' +
            '<div id="player" az-view az="0"><i az-hook="H" data-n="old"></i></div></div>';
        const destroyed = vi.fn();
        hooks.H = {
            destroyed() {
                destroyed(this.el.dataset.n);
            },
        };
        mountHooks(document);
        const pip = makePipWindow();
        stubPip(pip);
        await requestPip('player');

        applyOps([
            [
                OP.TEXT,
                'root:0',
                '<div id="player" az-view az="0"><i az-hook="H" data-n="new"></i></div>',
                true,
            ],
        ]);

        expect(pip.closed).toBe(false);
        expect(destroyed.mock.calls).toEqual([['old']]);
        expect(pip.document.querySelector('#player i')?.dataset.n).toBe('new');
        expect(document.getElementById('player')).toBeNull();
        expect(resolveEl('player')?.ownerDocument).toBe(pip.document);

        pip.fire('pagehide');
        expect(document.querySelector('#root > #player i')?.dataset.n).toBe('new');

        delete hooks.H;
    });

    it('closes the window when a stream remove takes the placeholder with its item', async () => {
        document.body.innerHTML =
            '<div id="root" az-view az="0"><ul az="1">' +
            '<li az-key="a"><div id="player" az-view az="0" az-hook="H"></div></li>' +
            '<li az-key="b">b</li></ul></div>';
        const destroyed = vi.fn();
        hooks.H = { destroyed };
        mountHooks(document);
        const pip = makePipWindow();
        stubPip(pip);
        await requestPip('player');

        applyOps([[OP.REMOVE, 'root:1', 'a']]);

        // Closed and discarded, not dumped into the page.
        expect(pip.closed).toBe(true);
        expect(document.getElementById('player')).toBeNull();
        expect(destroyed).toHaveBeenCalledOnce();
        expect(resolveEl('player')).toBeNull();

        delete hooks.H;
    });

    it('keeps the window when an item patch renders the popped-out view again', async () => {
        // The re-render is nested in a stream item's ops, not a top-level op.
        document.body.innerHTML =
            '<div id="root" az-view az="0"><ul az="1">' +
            '<li az-key="a" az="2"><div id="player" az-view az="0"></div></li></ul></div>';
        const pip = makePipWindow();
        stubPip(pip);
        await requestPip('player');

        applyOps([
            [
                OP.ITEM_PATCH,
                'root:1',
                'a',
                [[OP.TEXT, '2', '<div id="player" az-view az="0">new</div>', true]],
            ],
        ]);

        expect(pip.closed).toBe(false);
        expect(pip.document.getElementById('player')?.textContent).toBe('new');
        expect(document.getElementById('player')).toBeNull();
        pip.fire('pagehide');
        expect(document.querySelector('li[az-key="a"] > #player')?.textContent).toBe('new');
    });

    it('closes the window when a re-render puts a non-view element under its id', async () => {
        // Only a view is the view: a <p> reusing the id stays where it was rendered.
        document.body.innerHTML =
            '<div id="root" az-view az="0"><div id="player" az-view az="0">p</div></div>';
        const pip = makePipWindow();
        stubPip(pip);
        await requestPip('player');

        applyOps([[OP.TEXT, 'root:0', '<p id="player">just a paragraph</p>', true]]);

        expect(pip.closed).toBe(true);
        expect(document.querySelector('#root > p#player')?.textContent).toBe('just a paragraph');
        expect(pip.document.getElementById('player')).toBeNull();
    });

    it('keeps a popped-out child in its window when its popped-out parent re-renders it', async () => {
        // Child C in one window, its parent P in another: P's re-render renders C
        // inside P's window, not in the page, and C's window takes it from there.
        document.body.innerHTML =
            '<div id="root" az-view az="0">' +
            '<div id="P" az-view az="0"><div id="C" az-view az="0">c</div></div></div>';
        const w1 = makePipWindow();
        stubPip(w1);
        await requestPip('C');
        const w2 = makePipWindow();
        stubPip(w2);
        await requestPip('P');

        applyOps([[OP.TEXT, 'P:0', '<div id="C" az-view az="0">c2</div>', true]]);

        expect(w1.closed).toBe(false);
        expect(w1.document.querySelectorAll('#C')).toHaveLength(1);
        expect(w1.document.getElementById('C')?.textContent).toBe('c2');
        expect(w2.document.getElementById('C')).toBeNull();
        expect(resolveEl('C')?.ownerDocument).toBe(w1.document);

        // Closing C's window brings it back into P, in P's window.
        w1.fire('pagehide');
        expect(w2.document.querySelector('#P > #C')?.textContent).toBe('c2');
        w2.fire('pagehide');
    });

    it('patches the fresh view, not the stale one in the window, later in the same batch', async () => {
        document.body.innerHTML =
            '<div id="root" az-view az="0">' +
            '<div id="player" az-view az="0"><b az="1">x</b></div></div>';
        const pip = makePipWindow();
        stubPip(pip);
        await requestPip('player');

        applyOps([
            [OP.SET_ATTR, 'player:1', 'class', 'one'],
            [OP.TEXT, 'root:0', '<div id="player" az-view az="0"><b az="1">x</b></div>', true],
            [OP.SET_ATTR, 'player:1', 'class', 'two'],
        ]);

        expect(pip.closed).toBe(false);
        expect(pip.document.querySelector('#player b')?.getAttribute('class')).toBe('two');
    });

    it('patches the fresh child inside a popped-out parent later in the same batch', async () => {
        document.body.innerHTML =
            '<div id="root" az-view az="0">' +
            '<div id="P" az-view az="0"><div id="C" az-view az="0"><b az="1">c</b></div></div></div>';
        const w1 = makePipWindow();
        stubPip(w1);
        await requestPip('C');
        const w2 = makePipWindow();
        stubPip(w2);
        await requestPip('P');

        applyOps([
            [OP.TEXT, 'P:0', '<div id="C" az-view az="0"><b az="1">c</b></div>', true],
            [OP.SET_ATTR, 'C:1', 'class', 'two'],
        ]);

        expect(w1.document.querySelector('#C b')?.getAttribute('class')).toBe('two');
        w1.fire('pagehide');
        w2.fire('pagehide');
    });
});
