/**
 * @module arizona/static
 *
 * Arizona's client for a page with no server: a page `arizona_static` generated,
 * served as plain files. `start()` binds the same event delegation `connect()`
 * binds, so every command that runs in the browser works -- `?local` updates
 * (`arizona_js:set/2,3`, `set_all/2`), toggling, classes and attributes, dialogs,
 * view transitions, Picture-in-Picture, `copy_to_clipboard`, `focus`,
 * `dispatch_event`, `on_key`, an HTTP `fetch` -- and `az-hook`s mount. There is no
 * Worker and no WebSocket: a `push_event` has nobody to reach, so it warns and is
 * dropped, and a `navigate`/`patch` loads its URL as a page, as a static site's
 * links do.
 *
 * Which `az-*` attributes are events is never guessed from the markup (a data
 * value such as `az-select="[1,2,3]"` would otherwise run as a command):
 * `arizona_static` embeds the proven names in the page, as the socket's connect
 * frame carries them on a live one.
 */

import { closeAllPip } from './arizona-docs.js';
import { bindDocumentEvents, noteAzAttrs, unbindDocuments } from './arizona-events.js';
import { destroyAllHooks, mountHooks } from './arizona-hooks.js';
import { setHost } from './arizona-host.js';
import { loadPage, warnOnce } from './arizona-util.js';

export { executeJS } from './arizona-commands.js';
export { exitPip, requestPip } from './arizona-docs.js';
export { mountHooksPublic as mountHooks } from './arizona-events.js';
export { hooks } from './arizona-hooks.js';
export { get, set, setAll } from './arizona-locals.js';

/** The element `arizona_static` writes the page's proven `az-*` names into. */
const ATTRS_ID = 'arizona-attrs';

setHost({
    // Nothing to wait for: commands run whenever an event fires.
    ready: () => true,
    epoch: () => 0,
    sendEvent: (name) =>
        warnOnce(`[arizona] push_event "${name}" dropped: this page has no server`, name),
    // A navigate/patch loads a page, so no frame answers it.
    awaitsReply: () => false,
    armTransition: () => {},
    holdReplay: () => (fn) => fn(),
    navigate: (url, opts) => loadPage(url, opts?.replace),
    leavePage: loadPage,
});

/** @type {(() => void) | null} */
let _stop = null;

/**
 * Bind Arizona to this page: delegate its `az-*` events and mount its hooks.
 * `opts.attrs` names the page's proven `az-*` attributes; by default they are
 * read from what `arizona_static` embedded (`<script type="application/json"
 * id="arizona-attrs">`). Returns a `stop` function that unbinds everything,
 * closes popped-out windows and destroys the hooks. Calling `start` again stops
 * the previous one first.
 * @param {{attrs?: string[]}} [opts]
 * @returns {() => void}
 */
function start(opts = {}) {
    if (_stop) {
        console.warn('[arizona] start() called twice; stopping the previous start');
        _stop();
    }
    const controller = new AbortController();
    bindDocumentEvents(document, controller.signal);
    noteAzAttrs(opts.attrs ?? embeddedAttrs());
    mountHooks(document);
    let stopped = false;
    const stop = () => {
        if (stopped) return;
        stopped = true;
        _stop = null;
        controller.abort();
        closeAllPip();
        unbindDocuments();
        destroyAllHooks();
    };
    _stop = stop;
    return stop;
}

/**
 * The names `arizona_static` embedded, or none (a page with no event attributes
 * gets no element).
 * @returns {string[]}
 */
function embeddedAttrs() {
    const el = document.getElementById(ATTRS_ID);
    if (!el) return [];
    try {
        const names = JSON.parse(el.textContent || '[]');
        return Array.isArray(names) ? names.filter((n) => typeof n === 'string') : [];
    } catch {
        console.warn(`[arizona] #${ATTRS_ID} is not a JSON list of names; delegating nothing`);
        return [];
    }
}

export { start };
