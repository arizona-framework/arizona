import { afterEach, describe, expect, it, vi } from 'vitest';
import { applyOps, executeJS, requestPip, resolveEl } from './arizona.js';

// A minimal stand-in for a Document Picture-in-Picture window, backed by a real
// (jsdom) Document, so the multi-document patch/effect paths can be exercised
// without a Chromium PiP window (which jsdom can't provide).
function makePipWindow() {
    const doc = document.implementation.createHTMLDocument('pip');
    /** @type {Record<string, Function>} */
    const handlers = {};
    return {
        document: doc,
        /** @param {string} type @param {Function} fn */
        addEventListener(type, fn) {
            handlers[type] = fn;
        },
        /** @param {string} type */
        fire(type) {
            handlers[type]?.();
        },
    };
}

/** @param {ReturnType<typeof makePipWindow>} pip */
function stubPip(pip) {
    const requestWindow = vi.fn().mockResolvedValue(pip);
    /** @type {any} */ (window).documentPictureInPicture = { requestWindow };
    return requestWindow;
}

afterEach(() => {
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
});
