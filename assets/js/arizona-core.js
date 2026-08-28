/**
 * @module arizona.core
 *
 * Pure template resolution -- shared between the Web Worker (runtime) and
 * unit tests (no Worker globals required).
 *
 * Exports: resolveHtml, zipTemplate, backoff, fpCache, loadFpEntries,
 *          setOnPersist, FP_CACHE_MAX, mruFpKeys, takeTouchedFps.
 */

/** Type constant (must match server ?STREAM). */
const EACH = 0;

/**
 * Upper bound on the fingerprints kept in the persistent (IndexedDB) cache and
 * announced to the server at connect.
 *
 * A fingerprint is a hash of a template's statics, so *editing* a template mints
 * a new key and orphans the old one: the store accumulates one generation per
 * deploy, not one per app, and without a bound it grows for the lifetime of the
 * origin's storage. Evicting is never wrong -- the cache is content-addressed and
 * the server re-sends the statics for any fingerprint the client did not announce
 * -- so a miss costs bytes, never correctness.
 *
 * The number is set by the announcement, which ships on EVERY socket open,
 * reconnects included. A key is a base-36 32-bit hash, at most 7 characters, so
 * a full announcement of 1000 keys is ~10 KB. For scale, the framework's own
 * 101-module test app compiles to 135 distinct fingerprints, so 1000 holds
 * several generations of an app that size (or the working set of a much larger
 * one). The server caps its own side at 10000 (`?MAX_SENT_FPS` in
 * `arizona_live`) and silently drops the overflow, so announcing past that is
 * provably wasted.
 */
const FP_CACHE_MAX = 1000;

/**
 * Coarseness of the last-used stamp. A cache hit only re-stamps (and queues a
 * store write for) an entry whose stamp is older than this, so a page load that
 * resolves the same templates over and over writes each entry at most once an
 * hour instead of once per patch. Eviction ranks generations of a deploy, so
 * hour-level resolution is far finer than it needs.
 */
const FP_TOUCH_MS = 3600000;

/**
 * Fingerprint cache -- maps fingerprint IDs to {s, t?, u} entries, where `u` is
 * the last-used epoch stamp that ranks eviction. Shared singleton; the Worker
 * populates it at runtime, tests populate it directly.
 *
 * Unbounded WITHIN a session by design: once a connection has announced a
 * fingerprint the server stops shipping its statics, so dropping the entry would
 * leave an unresolvable payload. The bound is applied where it is safe -- at
 * hydration, and on the announcement itself.
 * @type {Map<string, {s: Array<string>, t?: number, u: number}>}
 */
const fpCache = new Map();

/**
 * Fingerprints whose last-used stamp moved since the last flush, so the Worker
 * can persist just those instead of rewriting the whole store.
 * @type {Set<string>}
 */
const _touchedFps = new Set();

/**
 * Optional callback invoked whenever the fp cache is updated (new statics
 * received). The Worker sets this to post cache entries to the main thread
 * for localStorage persistence. Tests leave it unset.
 * @type {Function|null}
 */
let _onPersist = null;

/**
 * Register a callback to be called when the fp cache is updated.
 * @param {Function|null} fn -- receives (fpId, entry) for each new fingerprint
 */
function setOnPersist(fn) {
    _onPersist = fn;
}

/**
 * Load fp cache entries (e.g. from the Worker's IndexedDB store).
 * @param {Array<[string, {s: Array<string>, t?: number, u: number}]>} entries
 */
function loadFpEntries(entries) {
    for (const [k, v] of entries) {
        fpCache.set(k, v);
    }
}

/**
 * Stamp an entry as used now, and queue it for persistence, when its stamp has
 * gone stale. Keeping the stamp coarse (see FP_TOUCH_MS) is what makes eviction
 * true last-USED rather than last-received: a template that never changes keeps
 * its fingerprint across deploys, so its receipt time says nothing about whether
 * the app still renders it.
 * @param {string} f
 * @param {{u: number}} entry
 */
function touchFp(f, entry) {
    const now = Date.now();
    if (now - entry.u < FP_TOUCH_MS) return;
    entry.u = now;
    _touchedFps.add(f);
}

/**
 * The fingerprints touched since the last call, clearing the set.
 * @returns {Array<string>}
 */
function takeTouchedFps() {
    const keys = [..._touchedFps];
    _touchedFps.clear();
    return keys;
}

/**
 * The `limit` most-recently-used fingerprint keys. Caps the per-connect
 * announcement: the cache may legitimately hold more (see fpCache), and the
 * server simply re-sends statics for whatever was left out.
 * @param {number} limit
 * @returns {Array<string>}
 */
function mruFpKeys(limit) {
    if (fpCache.size <= limit) return [...fpCache.keys()];
    const entries = [...fpCache];
    entries.sort((a, b) => b[1].u - a[1].u);
    return entries.slice(0, limit).map(([k]) => k);
}

/**
 * Resolve a payload that may be a plain text string (`?get` scalar), a `{raw}` tag
 * (a `?raw` trusted-HTML value), or a fingerprinted template object {f, s?, t?, d}
 * (a nested template / plain-list each). Returns the HTML/text string.
 * @param {string|{raw: string}|{f: string, s?: Array<string>, t?: number, d: Array<*>}} payload
 * @returns {string}
 */
function resolveHtml(payload) {
    if (typeof payload === 'string') return payload;
    // `?raw` trusted HTML: a tagged scalar. The worker marks it isHtml (an object, not a
    // string), so the client innerHTMLs the unwrapped markup.
    if ('raw' in payload) return payload.raw;
    const f = payload.f;
    if (payload.s) {
        /** @type {{s: Array<string>, t?: number, u: number}} */
        const entry = { s: payload.s, u: Date.now() };
        if (payload.t !== undefined) entry.t = payload.t;
        fpCache.set(f, entry);
        if (_onPersist) _onPersist(f, entry);
    }
    const cached = fpCache.get(f);
    if (!cached)
        throw new Error(`arizona: unknown template fingerprint "${f}" (statics not cached)`);
    touchFp(f, cached);
    if (payload.t === EACH) {
        return payload.d.map((itemD) => zipTemplate(cached.s, itemD)).join('');
    }
    return zipTemplate(cached.s, payload.d);
}

/**
 * Zip statics and dynamics into an HTML string. Dynamics may themselves be
 * fingerprinted objects (nested components) -- resolved recursively.
 * @param {Array<string>} statics
 * @param {Array<*>} dynamics
 * @returns {string}
 */
function zipTemplate(statics, dynamics) {
    let html = statics[0];
    for (let i = 0; i < dynamics.length; i++) {
        const d = dynamics[i];
        if (Array.isArray(d)) {
            for (let j = 0; j < d.length; j++) html += resolveOrPassthrough(d[j]);
        } else {
            html += resolveOrPassthrough(d);
        }
        html += statics[i + 1];
    }
    return html;
}

/**
 * If `v` is a fingerprinted template object (`{f, ...}`), resolve it to
 * HTML. `null`/`undefined` flatten to empty string so a stray nullish
 * value never lands in the DOM as the literal "null" or "undefined".
 * Everything else passes through (numbers, booleans, plain strings).
 * @param {*} v
 */
function resolveOrPassthrough(v) {
    if (v === null || v === undefined) return '';
    return typeof v === 'object' && v.f !== undefined ? resolveHtml(v) : v;
}

/**
 * Compute reconnection delay with step backoff and ±20% jitter.
 * Caps at 10 s deliberately -- after 4 attempts we keep retrying at the
 * same cadence rather than escalating, so a long server outage doesn't
 * turn into hour-long client gaps.
 * @param {number} attempt
 * @returns {number}
 */
function backoff(attempt) {
    const delays = [1000, 2000, 5000, 10000];
    const base = attempt < delays.length ? delays[attempt] : 10000;
    return Math.round(base * (0.8 + Math.random() * 0.4));
}

export {
    backoff,
    FP_CACHE_MAX,
    fpCache,
    loadFpEntries,
    mruFpKeys,
    resolveHtml,
    setOnPersist,
    takeTouchedFps,
    zipTemplate,
};
