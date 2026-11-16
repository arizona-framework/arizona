/** Messages already emitted by `warnOnce`, so a repeating event cannot spam the console. */
const _warned = new Set();

/** @param {string} msg */
export function warnOnce(msg, key = msg) {
    if (_warned.has(key)) return;
    _warned.add(key);
    console.warn(msg);
}

/**
 * The page a navigation URL names, resolved as the browser resolves an `<a href>`,
 * `history.pushState` or `location.assign`: against the document base
 * (`<base href>`, else the page itself).
 * @param {string} url
 */
export function pageUrl(url) {
    return new URL(url, document.baseURI);
}

/**
 * Whether `url` may be loaded as a page: only http(s). Any other scheme
 * (`javascript:`, `data:`) would run in this page, and the URL can come from a
 * server that echoed a request parameter -- so it is refused with a warning.
 * @param {string} url
 */
export function isWebUrl(url) {
    const { protocol } = pageUrl(url);
    if (protocol === 'http:' || protocol === 'https:') return true;
    console.warn('[arizona] refused non-http(s) URL:', url);
    return false;
}

/**
 * Load `url` as a real page (an http(s) one only, see `isWebUrl`), replacing the
 * current history entry when `replace` is set.
 *
 * The load starts in a microtask, once the running code has returned: what runs
 * as a load starts (an app's unsaved-changes `beforeunload` guard) then sees
 * what that code did after asking for it -- an `az-submit` resetting its form,
 * a nested submit's included -- and the framework's loads asked for in one run
 * start in the order asked. The URL is resolved now (`pageUrl`, as the browser
 * would resolve it now), so an SPA navigation later in the same run, which moves
 * the address, does not change where it goes.
 * @param {string} url
 * @param {boolean} [replace]
 */
export function loadPage(url, replace) {
    if (!isWebUrl(url)) return;
    const href = pageUrl(url).href;
    queueMicrotask(() => {
        if (replace) location.replace(href);
        else location.assign(href);
    });
}

/**
 * Reload the page (a `reload` command), in a microtask as `loadPage` loads: it
 * reloads the page the address bar shows when the reload starts, after the run
 * -- the one an SPA navigation later in the same run moved to, if any.
 */
export function reloadPage() {
    queueMicrotask(() => location.reload());
}

/**
 * Whether `u` names a fragment of the page on screen (a bare `#` too, an empty
 * one, as for the browser): a navigation there only scrolls.
 * @param {URL} u
 * @returns {boolean}
 */
export function isFragmentOfPage(u) {
    const fragment = u.hash !== '' || u.href.endsWith('#');
    return fragment && isSamePage(u, location);
}

/**
 * Whether `u` and `page` name one page, their fragments aside: the same origin,
 * path and query -- an empty query included, since `search` reads '' for `/?` and
 * the browser loads `/?#x` from `/`.
 * @param {URL|Location} u
 * @param {URL|Location} page
 * @returns {boolean}
 */
export function isSamePage(u, page) {
    return (
        u.origin === page.origin &&
        u.pathname === page.pathname &&
        u.search === page.search &&
        hasQuery(u.href) === hasQuery(page.href)
    );
}

/**
 * Whether `href` carries a query, an empty one included.
 * @param {string} href
 * @returns {boolean}
 */
function hasQuery(href) {
    const hash = href.indexOf('#');
    return (hash < 0 ? href : href.slice(0, hash)).includes('?');
}

// HTMLFormElement is [LegacyOverrideBuiltIns]: a control named `elements` or `reset`
// is what the form's own `elements` / `reset` return (the named property sits on the
// form), so they are read from the prototype. (A form's id is read with
// `getAttribute('id')` for the same reason: an edit form's `<input name=id>` is its
// `id`.)

/**
 * A form's listed controls, whatever its controls are named.
 * @param {Element} form
 * @returns {HTMLFormControlsCollection}
 */
export function formControls(form) {
    const get = Object.getOwnPropertyDescriptor(HTMLFormElement.prototype, 'elements')?.get;
    return /** @type {any} */ (get).call(form);
}

/**
 * Reset a form, whatever its controls are named.
 * @param {Element} form
 */
export function resetForm(form) {
    HTMLFormElement.prototype.reset.call(/** @type {HTMLFormElement} */ (form));
}

const XHTML = 'http://www.w3.org/1999/xhtml';

/**
 * Is `el` an HTML <form>? Not by its local name alone: a `form` in SVG (or any
 * other namespace) is an element of that name, and no form. The namespace is a
 * string, so this holds for an element of another window (a PiP document) too.
 * @param {Element} el
 * @returns {boolean}
 */
export function isForm(el) {
    return el.localName === 'form' && el.namespaceURI === XHTML;
}

/**
 * The HTML <form> `el` is in or is, past any SVG `form` between them.
 * @param {Element} el
 * @returns {HTMLFormElement|null}
 */
export function closestForm(el) {
    let f = el.closest('form');
    while (f && !isForm(f)) f = f.parentElement?.closest('form') ?? null;
    return /** @type {HTMLFormElement|null} */ (f);
}

/**
 * How a warning names `el`: its tag, with its namespace when that is not HTML.
 * @param {Element} el
 * @returns {string}
 */
export function describeEl(el) {
    if (el.namespaceURI === XHTML) return `a <${el.localName}>`;
    if (el.namespaceURI === 'http://www.w3.org/2000/svg') return `an SVG <${el.localName}>`;
    return `a <${el.localName}> (${el.namespaceURI})`;
}
