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
 * Connect to the SSE reload endpoint.
 * @param {string} url - SSE endpoint URL (must match the {reload, Path, _} route)
 */
function connect(url) {
    const es = new EventSource(url);
    es.addEventListener('reload', () => location.reload());
    es.addEventListener('reload_css', () => {
        document.querySelectorAll('link[rel="stylesheet"]').forEach(link => {
            const url = new URL(/** @type {HTMLLinkElement} */ (link).href);
            url.searchParams.set('_t', String(Date.now()));
            /** @type {HTMLLinkElement} */ (link).href = url.toString();
        });
    });
}

export { connect };
