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

    it('JS_REQUEST_PIP effect triggers a pop-out', async () => {
        document.body.innerHTML = '<div id="v4" az-view az="0"></div>';
        const pip = makePipWindow();
        const requestWindow = stubPip(pip);

        executeJS(document.documentElement, null, [18, 'v4']); // JS_REQUEST_PIP
        expect(requestWindow).toHaveBeenCalledOnce();

        await Promise.resolve();
        await Promise.resolve();
        pip.fire('pagehide'); // cleanup
    });
});
