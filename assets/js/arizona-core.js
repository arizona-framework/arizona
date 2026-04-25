/**
 * @module arizona.core
 *
 * Pure template resolution -- shared between the Web Worker (runtime) and
 * unit tests (no Worker globals required).
 *
 * Exports: resolveHtml, zipTemplate, backoff, fpCache, loadFpEntries,
 *          setOnPersist.
 */

/** Type constant (must match server ?STREAM). */
const EACH = 0;

/**
 * Fingerprint cache -- maps fingerprint IDs to {s, t?} entries.
 * Shared singleton; the Worker populates it at runtime, tests populate it
 * directly.
 * @type {Map<string, {s: Array<string>, t?: number}>}
 */
const fpCache = new Map();

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
 * Load fp cache entries (e.g. from localStorage via the main thread).
 * @param {Array<[string, {s: Array<string>, t?: number}]>} entries
 */
function loadFpEntries(entries) {
    for (const [k, v] of entries) {
        fpCache.set(k, v);
    }
}

/**
 * Resolve a payload that may be a plain HTML string or a fingerprinted
 * template object {f, s?, t?, d}. Returns an HTML string.
 * @param {string|{f: string, s?: Array<string>, t?: number, d: Array<*>}} payload
 * @returns {string}
 */
function resolveHtml(payload) {
    if (typeof payload === 'string') return payload;
    const f = payload.f;
    if (payload.s) {
        /** @type {{s: Array<string>, t?: number}} */
        const entry = { s: payload.s };
        if (payload.t !== undefined) entry.t = payload.t;
        fpCache.set(f, entry);
        if (_onPersist) _onPersist(f, entry);
    }
    const cached = fpCache.get(f);
    if (!cached)
        throw new Error(`arizona: unknown template fingerprint "${f}" (statics not cached)`);
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
 * HTML; otherwise return it as-is.
 * @param {*} v
 */
function resolveOrPassthrough(v) {
    return v !== null && typeof v === 'object' && v.f !== undefined ? resolveHtml(v) : v;
}

/**
 * Compute reconnection delay with step backoff and jitter.
 * @param {number} attempt
 * @returns {number}
 */
function backoff(attempt) {
    const delays = [1000, 2000, 5000, 10000];
    const base = attempt < delays.length ? delays[attempt] : 10000;
    return Math.round(base * (0.8 + Math.random() * 0.4));
}

export { backoff, fpCache, loadFpEntries, resolveHtml, setOnPersist, zipTemplate };
