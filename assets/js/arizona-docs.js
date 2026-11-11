import { _eventDocs, _passiveBound, bindDocumentEvents } from './arizona-events.js';
import { destroyHooks } from './arizona-hooks.js';

// --------------------------------------------------------------------------
// Multi-document support (Document Picture-in-Picture)
// --------------------------------------------------------------------------
// A view's root element normally lives in the main `document`, but `requestPip`
// can move it into a floating PiP window with its own `document`. Patches are
// applied by resolving the view root in its OWNING document, so server diffs
// keep flowing after the move. Registers the popped view's id against its PiP
// Document so `allDocs` knows which windows to search; resolution itself goes
// by DOM containment (see `findViewRoot`), not this map's keys.
/** @type {Map<string, Document>} */
export const _viewDocs = new Map();

// The PiP window and inline placeholder per popped-out view, keyed by the id of
// the view the window shows, so each op batch can bring the window in line with
// what it did (`reconcilePipWindows`). An entry is mutable: a navigate inside
// the window re-keys it, a re-render inline moves a fresh placeholder in. Entries
// live and die with `_viewDocs` (set in requestPip, removed by the window's
// pagehide handler).
/** @typedef {{id: string, win: Window, placeholder: Comment}} PipEntry */
/** @type {Map<string, PipEntry>} */
export const _pipWindows = new Map();

/**
 * Close every floating (PiP) view window and forget them; each window's pagehide
 * handler moves its view back inline first. A page's teardown.
 */
export function closeAllPip() {
    for (const d of [..._viewDocs.values()]) {
        try {
            d.defaultView?.close();
        } catch {
            /* window already gone */
        }
    }
    _viewDocs.clear();
    _pipWindows.clear();
}

/**
 * The views a PiP document holds: the `az-view` elements of its body, not
 * whatever else a hook may have put there.
 * @param {Document} doc
 * @returns {Element[]}
 */
function viewsIn(doc) {
    return Array.from(doc.body.children).filter((e) => e.hasAttribute('az-view'));
}

/**
 * The view `id` a batch rendered again outside `own` (its window): in the page,
 * or in another window when its parent is popped out too. A view, not just an
 * element under its id: anything else stays where it was rendered.
 * @param {string} id
 * @param {Document} own
 * @returns {Element|null}
 */
function renderedAgain(id, own) {
    for (const doc of allDocs()) {
        if (doc === own) continue;
        const el = doc.getElementById(id);
        if (el?.hasAttribute('az-view')) return el;
    }
    return null;
}

/**
 * Bring each popped-out (PiP) window in line with the op batch just applied:
 * - A replace in the window under a new id (a navigate of a popped-out root)
 *   re-keys it, so `exit_pip`/`request_pip` from the new page name it.
 * - A placeholder the batch took with a re-render that renders the view again
 *   (a reconnect's resync, a parent's wholesale re-render) leaves the fresh
 *   element in the page, or in its parent's window: it moves into the window in
 *   place of the stale one and leaves a new placeholder, as a pop-out does.
 * - Otherwise the view is gone (removed, or the page navigated: a new page's
 *   views are new, whatever their ids), so the window closes; its pagehide
 *   handler discards the stale view.
 * @param {boolean} navigated -- the batch replaced the page (a navigate)
 */
export function reconcilePipWindows(navigated) {
    if (_pipWindows.size === 0) return;
    for (const entry of [..._pipWindows.values()]) {
        const doc = entry.win.document;
        if (entry.placeholder.isConnected) {
            if (doc.getElementById(entry.id)) continue;
            const id = viewsIn(doc)[0]?.id;
            if (!id) continue;
            _viewDocs.delete(entry.id);
            _pipWindows.delete(entry.id);
            entry.id = id;
            _viewDocs.set(id, doc);
            _pipWindows.set(id, entry);
            continue;
        }
        const fresh = navigated ? null : renderedAgain(entry.id, doc);
        if (fresh) {
            for (const e of viewsIn(doc)) {
                destroyHooks(e);
                e.remove();
            }
            entry.placeholder = fresh.ownerDocument.createComment(`az-pip:${entry.id}`);
            fresh.before(entry.placeholder);
            doc.body.append(fresh);
            continue;
        }
        try {
            entry.win.close();
        } catch {
            /* window already gone */
        }
    }
}

/**
 * Every document currently hosting Arizona views: the main document plus any
 * popped-out PiP documents. Used by document-wide local-slot ops.
 * @returns {Set<Document>}
 */
export function allDocs() {
    const docs = new Set([document]);
    for (const d of _viewDocs.values()) docs.add(d);
    return docs;
}

/**
 * Find a view's root element by id, searching the main document first and then
 * any popped-out PiP windows, returning whichever one actually owns it.
 * Resolving by DOM containment -- rather than a flat viewId -> Document map --
 * is what routes a NESTED stateful child correctly: `requestPip` registers only
 * the popped root's id, but a stateful child inside the moved subtree emits ops
 * under its own view id and gets physically moved into the PiP document with the
 * subtree. Containment finds it there with no per-child bookkeeping (and the
 * same for children inserted after pop-out). The PiP scan is skipped entirely
 * when no window is open, so the common (main-document) path stays a single
 * allocation-free `getElementById` -- this runs per diff op. Returns null when
 * no hosting document holds the element.
 * @param {string} viewId
 * @returns {Element|null}
 */
export function findViewRoot(viewId) {
    const el = document.getElementById(viewId);
    if (el || _viewDocs.size === 0) return el;
    // Between a re-render that takes a popped-out view's placeholder and the
    // batch's end (`reconcilePipWindows`), its window still holds the stale copy
    // while the fresh one sits in another window: the fresh one wins.
    const pip = _pipWindows.get(viewId);
    const stale = pip && !pip.placeholder.isConnected ? pip.win.document : null;
    let found = null;
    for (const doc of _viewDocs.values()) {
        const e = doc.getElementById(viewId);
        if (!e) continue;
        if (doc !== stale) return e;
        found = e;
    }
    return found;
}

/**
 * Copy <style> and <link rel="stylesheet"> from `src` into `dst`'s <head> so a
 * Picture-in-Picture document (which starts empty) renders with the page styles.
 * @param {Document} src
 * @param {Document} dst
 */
function copyStyles(src, dst) {
    for (const sheet of src.styleSheets) {
        try {
            const css = Array.from(sheet.cssRules, (r) => r.cssText).join('');
            const style = dst.createElement('style');
            style.textContent = css;
            dst.head.appendChild(style);
        } catch {
            // Cross-origin sheet: rules aren't readable -- re-link it by href.
            const href = /** @type {any} */ (sheet).href;
            if (!href) continue;
            const link = dst.createElement('link');
            link.rel = 'stylesheet';
            link.href = href;
            dst.head.appendChild(link);
        }
    }
}

/**
 * Move a view's root element into a floating Document Picture-in-Picture window,
 * keeping it live: server diffs keep patching it (resolved in the PiP document)
 * and az-* / form / drag events fired inside it are delegated there too. Page
 * styles are copied in. Must be called from a user gesture. Resolves to the PiP
 * `Window`, or `null` if the browser lacks Document PiP, the view isn't found,
 * or it's already popped out.
 * @param {string} viewId
 * @param {{width?: number, height?: number, onClose?: () => void}} [opts]
 * @returns {Promise<Window|null>}
 */
export async function requestPip(viewId, opts = {}) {
    const pipApi = /** @type {any} */ (window).documentPictureInPicture;
    if (!pipApi) return null;
    if (_viewDocs.has(viewId)) return null;
    const view = document.getElementById(viewId);
    if (!view) return null;

    // Open the floating window FIRST. requestWindow rejects without a user
    // gesture (a server-pushed request_pip has none); await before touching the
    // DOM so a rejection leaves no orphaned placeholder behind.
    // Forward the caller's options straight to the browser (no framework defaults);
    // requestWindow ignores dictionary members it doesn't know (e.g. onClose).
    const pip = await pipApi.requestWindow(opts);

    // Remember where the view sat so it can be restored in place on close.
    const placeholder = document.createComment(`az-pip:${viewId}`);
    view.before(placeholder);

    copyStyles(document, pip.document);
    pip.document.body.append(view);
    _viewDocs.set(viewId, pip.document);
    /** @type {PipEntry} */
    const entry = { id: viewId, win: pip, placeholder };
    _pipWindows.set(viewId, entry);

    // Delegate events fired inside the floating window; torn down on close.
    const controller = new AbortController();
    bindDocumentEvents(pip.document, controller.signal, () => controller.abort());

    pip.addEventListener(
        'pagehide',
        () => {
            controller.abort();
            _viewDocs.delete(entry.id);
            _pipWindows.delete(entry.id);
            _eventDocs.delete(pip.document);
            _passiveBound.delete(pip.document);
            // The views the window holds now, not the element moved out: a replace
            // there swaps it for a fresh one, under a new id when it navigates a
            // popped-out root to another page.
            const current = viewsIn(pip.document);
            // isConnected, not parentNode: a replaced (navigate) outgoing
            // subtree is detached wholesale, so the placeholder still HAS a
            // parent -- inside a dead tree.
            if (entry.placeholder.isConnected) {
                entry.placeholder.replaceWith(...current);
            } else {
                // The view's inline home was destroyed while it was popped out
                // (a navigate swapped the page, a removal took it): the page has
                // its own content there now, so discard the stale element --
                // appending it to the body would resurrect dead server state.
                // Tear its hooks down like any removed subtree.
                for (const e of current) {
                    destroyHooks(e);
                    e.remove();
                }
            }
            if (opts.onClose) opts.onClose();
        },
        { once: true },
    );

    return pip;
}

/**
 * Close a view's floating window (if open); its pagehide handler moves the view
 * back inline. No-op when the view isn't popped out.
 * @param {string} viewId
 */
export function exitPip(viewId) {
    const doc = _viewDocs.get(viewId);
    doc?.defaultView?.close();
}
