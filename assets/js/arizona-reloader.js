/**
 * @module arizona.reloader
 *
 * Arizona dev reloader -- connects to the SSE reload endpoint.
 *
 * Usage:
 *   import { connect } from '/priv/arizona-reloader.min.js';
 *   connect('/dev/reload');
 */

/**
 * Default wait after the last `reload` event before reloading, so a burst
 * collapses into a single navigation. Override per-call via `opts.debounce`.
 */
const RELOAD_DEBOUNCE_MS = 50;

/**
 * Connect to the SSE reload endpoint.
 *
 * @param {string} url - SSE endpoint URL (must match the {reload, Path, _} route)
 * @param {Object} [opts]
 * @param {number} [opts.debounce] - Milliseconds to wait after the last `reload`
 *   event before reloading, so a burst collapses into a single navigation.
 *   Defaults to 50.
 */
function connect(url, opts = {}) {
    const debounceMs = opts.debounce ?? RELOAD_DEBOUNCE_MS;
    const es = new EventSource(url);

    // A rebuild can rewrite and recompile many files, emitting several `reload`
    // events in quick succession (and EventSource auto-reconnects re-deliver the
    // last event). Calling location.reload() per event makes the reloads cancel
    // one another mid-navigation -- a storm of aborted navigations that also
    // skips any in-flight cross-document View Transition (`@view-transition`),
    // logging "AbortError: Transition was skipped". Debounce the burst and latch
    // so the page reloads at most once.
    /** @type {ReturnType<typeof setTimeout>|null} */
    let reloadTimer = null;
    let reloadStarted = false;
    es.addEventListener('reload', () => {
        if (reloadStarted) return;
        if (reloadTimer) clearTimeout(reloadTimer);
        reloadTimer = setTimeout(() => {
            reloadStarted = true;
            location.reload();
        }, debounceMs);
    });

    es.addEventListener('reload_css', () => {
        document.querySelectorAll('link[rel="stylesheet"]').forEach((link) => {
            const url = new URL(/** @type {HTMLLinkElement} */ (link).href);
            url.searchParams.set('_t', String(Date.now()));
            /** @type {HTMLLinkElement} */ (link).href = url.toString();
        });
    });
}

export { connect };
