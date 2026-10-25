/** Messages already emitted by `warnOnce`, so a repeating event cannot spam the console. */
const _warned = new Set();

/** @param {string} msg */
export function warnOnce(msg, key = msg) {
    if (_warned.has(key)) return;
    _warned.add(key);
    console.warn(msg);
}

/**
 * Whether `url` may be loaded as a page: only http(s). Any other scheme
 * (`javascript:`, `data:`) would run in this page, and the URL can come from a
 * server that echoed a request parameter -- so it is refused with a warning.
 * @param {string} url
 */
export function isWebUrl(url) {
    const { protocol } = new URL(url, location.href);
    if (protocol === 'http:' || protocol === 'https:') return true;
    console.warn('[arizona] refused non-http(s) URL:', url);
    return false;
}

/**
 * Load `url` as a real page (an http(s) one only, see `isWebUrl`), replacing the
 * current history entry when `replace` is set.
 * @param {string} url
 * @param {boolean} [replace]
 */
export function loadPage(url, replace) {
    if (!isWebUrl(url)) return;
    if (replace) location.replace(url);
    else location.assign(url);
}
