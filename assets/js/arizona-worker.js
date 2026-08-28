/**
 * @module arizona.worker
 *
 * Arizona Web Worker -- owns WebSocket connection, JSON parsing, template
 * resolution, and fingerprint cache persistence (IndexedDB).
 * Sends pre-computed DOM-ready data to the main thread.
 *
 * Worker -> Main protocol (arrays for fast structured clone):
 *   [0, ops|null, effects|null, firstAfterReconnect, azAttrs|null] -- resolved message
 *   [1, isReconnect]                                  -- WS opened
 *   [2, closeCode]                                    -- WS closed
 *
 * Main -> Worker protocol:
 *   [0, wsUrl, isReconnect] -- connect (full URL; isReconnect resyncs an
 *                              already-rendered DOM, e.g. a bfcache restore)
 *   [1, jsonString]         -- send data (pre-stringified on main)
 *   [2, code]               -- close WS
 */

import {
    EACH,
    FP_CACHE_MAX,
    fpCache,
    loadFpEntries,
    mruFpKeys,
    resolveHtml,
    setOnPersist,
    takeTouchedFps,
} from './arizona-core.js';

/** Op codes -- must match server and main thread. */
const OP_TEXT = 0;
const OP_INSERT = 5;
const OP_ITEM_PATCH = 7;
const OP_REPLACE = 8;
const OP_LIST_PATCH = 10;

const SYS_PING = '0';
const SYS_PONG = '1';

// ---------------------------------------------------------------------------
// IndexedDB persistence -- one entry per fingerprint, Worker-owned
// ---------------------------------------------------------------------------

const DB_NAME = 'arizona';
const STORE = 'cache';
const DB_VERSION = 1;

/** @type {Promise<IDBDatabase>|null} */
let _dbReady = null;

function getDB() {
    if (!_dbReady) {
        _dbReady = new Promise((resolve, reject) => {
            const req = indexedDB.open(DB_NAME, DB_VERSION);
            req.onupgradeneeded = () => req.result.createObjectStore(STORE);
            req.onsuccess = () => resolve(req.result);
            req.onerror = () => reject(req.error);
        });
    }
    return _dbReady;
}

/**
 * Read the store for cache hydration, keeping at most `FP_CACHE_MAX` entries and
 * deleting the rest. Pruning happens HERE, before the connection announces its
 * keys: the server stops shipping statics for every fingerprint the client
 * announced, so an eviction mid-connection would leave a payload the client
 * cannot resolve.
 * @returns {Promise<Array<[string, {s: Array<string>, t?: number, u: number}]>>}
 */
function idbLoadPruned() {
    return getDB().then(
        (db) =>
            new Promise((resolve) => {
                /** @type {Array<[string, {s: Array<string>, t?: number, u: number}]>} */
                const entries = [];
                const req = db.transaction(STORE).objectStore(STORE).openCursor();
                req.onsuccess = () => {
                    const c = req.result;
                    if (c) {
                        entries.push([/** @type {string} */ (c.key), c.value]);
                        c.continue();
                    } else resolve(prune(db, entries));
                };
                req.onerror = () => {
                    console.warn('[arizona] idb cache cursor error:', req.error);
                    resolve([]);
                };
            }),
    );
}

/**
 * Drop all but the `FP_CACHE_MAX` most-recently-used entries, returning the kept
 * ones. Evicting is only ever a cache miss (the fingerprint is a hash of the
 * statics, so the server re-sends them), which is what makes a plain cap safe.
 * @param {IDBDatabase} db
 * @param {Array<[string, {s: Array<string>, t?: number, u: number}]>} entries
 */
function prune(db, entries) {
    if (entries.length <= FP_CACHE_MAX) return entries;
    entries.sort((a, b) => b[1].u - a[1].u);
    const store = db.transaction(STORE, 'readwrite').objectStore(STORE);
    for (const [k] of entries.slice(FP_CACHE_MAX)) store.delete(k);
    return entries.slice(0, FP_CACHE_MAX);
}

/**
 * Write a single fingerprint entry.
 * @param {string} fpId
 * @param {{s: Array<string>, t?: number, u: number}} entry
 */
function idbPut(fpId, entry) {
    getDB()
        .then((db) => {
            db.transaction(STORE, 'readwrite').objectStore(STORE).put(entry, fpId);
        })
        .catch(() => {});
}

/**
 * Persist the last-used stamps moved by the resolves just processed, so the next
 * hydration evicts by what the app actually renders rather than by when the
 * statics happened to arrive. A no-op on nearly every message: the stamp is
 * coarse, so a steady stream of patches touches nothing.
 */
function flushTouchedFps() {
    const keys = takeTouchedFps();
    if (keys.length === 0) return;
    getDB()
        .then((db) => {
            const store = db.transaction(STORE, 'readwrite').objectStore(STORE);
            for (const k of keys) store.put(fpCache.get(k), k);
        })
        .catch(() => {});
}

/** @type {WebSocket|null} */
let _ws = null;

/** @type {string|null} */
let _wsUrl = null;

/** @type {boolean} */
let _reconnecting = false;

/** @type {number} */
let _attempt = 0;

/** @type {ReturnType<typeof setTimeout>|null} */
let _reconnectTimer = null;

/** @type {ReturnType<typeof setInterval>|null} */
let _heartbeatInterval = null;

/** @type {boolean} */
let _heartbeatPending = false;

/** @type {boolean} */
let _fpsSent = false;

/**
 * IDB cache hydration settled (loaded or failed). A flagged reconnect must
 * announce the REAL cache -- a fresh worker (bfcache respawn) announcing
 * before hydration would claim an empty cache and forfeit the dedup.
 * @type {boolean}
 */
let _hydrated = false;

/**
 * The current connection carried `_az_fps_follow=1`: it promised the server a
 * `cached_fps` frame (possibly empty) as its first frame, and the server is
 * holding the reconnect resync for it so the payload can dedup against the
 * announced fingerprints.
 * @type {boolean}
 */
let _fpsFollow = false;

/**
 * Send cached fingerprint keys to the server exactly once per connection.
 * Called from ws.onopen and after IDB hydration settles -- whichever finds the
 * preconditions met first actually sends; the other is a no-op. On a flagged
 * reconnect (`_fpsFollow`) the frame is MANDATORY (the server defers its
 * resync for it) and goes out even empty, but only once hydration settled; on
 * a first connect an empty cache sends nothing (no behavior to unlock, no
 * extra frame). Capped at the most-recently-used `FP_CACHE_MAX`: anything left
 * out simply arrives with its statics attached.
 */
function sendCachedFps() {
    if (_fpsSent || !_ws || _ws.readyState !== 1) return;
    if (_fpsFollow) {
        if (!_hydrated) return;
    } else if (fpCache.size === 0) {
        return;
    }
    _fpsSent = true;
    _ws.send(JSON.stringify(['cached_fps', mruFpKeys(FP_CACHE_MAX)]));
}

// Wire up fp cache persistence: write each new fingerprint to IndexedDB.
setOnPersist(
    /** @param {string} fpId @param {{s: Array<string>, t?: number, u: number}} entry */ (
        fpId,
        entry,
    ) => {
        idbPut(fpId, entry);
    },
);

// ---------------------------------------------------------------------------
// Op resolution -- resolve template payloads to HTML strings in-place
// ---------------------------------------------------------------------------

/**
 * `az-*` attribute names already reported to the main thread. The worker is the
 * right place to discover them: it already walks every op and already knows which
 * fields are HTML, because it builds them. Doing it here rather than by walking
 * the patched DOM keeps the cost off the main thread entirely -- the DOM walk it
 * replaces measured +65-71% on an applyOps batch.
 * @type {Set<string>}
 */
const _seenAzAttrs = new Set();

/** Names found in THIS message and not seen before; drained by the send. */
/** @type {Array<string>} */
let _newAzAttrs = [];

// Tags only, never text. A raw `>` cannot appear in text or in an attribute value
// -- the server escapes both to `&gt;` -- so it only ever closes a tag, which makes
// this a safe way to look at markup alone. Scanning the whole payload instead let
// ordinary prose containing the literal `az-prevent-default` latch the page's wheel
// listeners non-passive, and let arbitrary user text register listeners; over-
// matching is NOT harmless for a directive. A `?raw` payload is markup by
// definition, so scanning its tags is right.
//
// The obvious cheaper fix -- one pass over the payload, then `lastIndexOf('<')`
// per hit to test whether it landed inside a tag -- is 2x faster on markup and
// **640x slower on prose** (2.4 ms vs 0.004 ms for 14 KB of chat text carrying
// `az-` tokens), because the backward scan degrades on exactly the user-authored
// content this guard exists for. Cost that scales with attacker-supplied text is
// the wrong trade. What pays instead is scanning the payload STRUCTURE rather than
// the assembled markup (see scanAzAttrs): the statics are per fingerprint, so they
// are scanned once, and every later frame scans only its dynamics.
const TAG_RE = /<[^>]*>/g;

// Matches an `az-*` attribute NAME inside one tag. Anchored on the separator before
// it and on what legally follows a name (`=`, whitespace for a bare attribute, or
// `/`/`>` closing the tag). The name class takes `_`, `.` and `:` as well, because
// the HTML parser keeps all three in an attribute name and the docs prescribe the
// underscore form (`{~"az-my_event", ...}`) -- excluding them made that form work at
// SSR and die the moment the same element arrived by patch.
const AZ_NAME = 'az-([a-z0-9][\\w.:-]*)';
const AZ_ATTR_RE = new RegExp(`[\\s/]${AZ_NAME}(?=[\\s/>=])`, 'gi');

// The same name matched inside ONE dynamic attribute chunk instead of a whole tag.
// A chunk is what `arizona_html:render_attr/2` emits for one slot (` name`,
// ` name="value"`, or nothing), so a name's separators can fall outside it: the one
// in front sits at the end of the static before the slot, the one behind at the
// start of the static after it -- a bare ` az-form-reset` has its `>` in the next
// static and would go unseen without the end-anchored form. Both edges are fixed by
// the template, so the plan below picks the variant per slot: bit 1 = the preceding
// static ends with a separator, bit 0 = the following static starts with one.
const AZ_CHUNK_RES = [
    new RegExp(`[\\s/]${AZ_NAME}(?=[\\s/>=])`, 'gi'),
    new RegExp(`[\\s/]${AZ_NAME}(?=[\\s/>=]|$)`, 'gi'),
    new RegExp(`(?:^|[\\s/])${AZ_NAME}(?=[\\s/>=])`, 'gi'),
    new RegExp(`(?:^|[\\s/])${AZ_NAME}(?=[\\s/>=]|$)`, 'gi'),
];

const SEP_RE = /[\s/]/;
const AFTER_NAME_RE = /[\s/>=]/;

/**
 * Per-fingerprint scan plan, one entry per dynamic slot: `-1` for a content slot
 * (between tags), otherwise the `AZ_CHUNK_RES` index for an attribute slot. Grows
 * with the fingerprint cache and lives as long as it does -- both are keyed by a
 * hash of the statics, so an entry can never go stale.
 * @type {Map<string, Int8Array>}
 */
const _fpPlans = new Map();

/**
 * Record one name, reporting it to the main thread the first time it is seen.
 * @param {string} name
 */
function recordAzAttr(name) {
    const lower = name.toLowerCase();
    if (!_seenAzAttrs.has(lower)) {
        _seenAzAttrs.add(lower);
        _newAzAttrs.push(lower);
    }
}

/**
 * Record every `az-*` attribute name in a run of markup: split into tags first,
 * then look for names inside each tag.
 * @param {string} html
 */
function scanMarkup(html) {
    TAG_RE.lastIndex = 0;
    let tag = TAG_RE.exec(html);
    while (tag !== null) {
        AZ_ATTR_RE.lastIndex = 0;
        let m = AZ_ATTR_RE.exec(tag[0]);
        while (m !== null) {
            recordAzAttr(m[1]);
            m = AZ_ATTR_RE.exec(tag[0]);
        }
        tag = TAG_RE.exec(html);
    }
}

/**
 * Record every `az-*` attribute name in one dynamic attribute chunk.
 * @param {string} chunk
 * @param {number} variant -- index into AZ_CHUNK_RES
 */
function scanAttrChunk(chunk, variant) {
    const re = AZ_CHUNK_RES[variant];
    re.lastIndex = 0;
    let m = re.exec(chunk);
    while (m !== null) {
        recordAzAttr(m[1]);
        m = re.exec(chunk);
    }
}

/**
 * Classify each dynamic slot of a template from its statics. A slot is inside a
 * tag when the last `<` or `>` before it was a `<`; both characters are unambiguous
 * because the server escapes them everywhere else.
 * @param {Array<string>} statics
 * @returns {Int8Array}
 */
function buildPlan(statics) {
    const plan = new Int8Array(statics.length - 1);
    let inTag = false;
    for (let i = 0; i < plan.length; i++) {
        const s = statics[i];
        for (let j = s.length - 1; j >= 0; j--) {
            const c = s.charCodeAt(j);
            if (c === 60) {
                inTag = true;
                break;
            }
            if (c === 62) {
                inTag = false;
                break;
            }
        }
        if (!inTag) {
            plan[i] = -1;
            continue;
        }
        // An empty neighbouring static leaves the boundary character to the
        // adjoining dynamic, which is not knowable per fingerprint. Allow the
        // anchor there: over-reporting a name only ever costs one extra delegated
        // event type, while missing one is a dead event.
        const next = statics[i + 1];
        const headSep = s.length === 0 || SEP_RE.test(s[s.length - 1]);
        const tailSep = next.length === 0 || AFTER_NAME_RE.test(next[0]);
        plan[i] = (headSep ? 2 : 0) | (tailSep ? 1 : 0);
    }
    return plan;
}

/**
 * The scan plan for a fingerprint, scanning its statics on the way in. The statics
 * are constant for the fingerprint (it is their hash), so the names they declare
 * are found once no matter how many frames carry the template.
 * @param {string} f
 * @param {Array<string>} statics
 * @returns {Int8Array}
 */
function planFor(f, statics) {
    let plan = _fpPlans.get(f);
    if (plan === undefined) {
        plan = buildPlan(statics);
        _fpPlans.set(f, plan);
        // Joining elides the dynamics, which is exactly right: they are scanned
        // per frame below. Nothing is lost at the seams because an attribute NAME
        // is always a compile-time literal, so it never spans a slot.
        scanMarkup(statics.join(''));
    }
    return plan;
}

/**
 * Record every `az-*` attribute name a payload introduces.
 *
 * Walks the payload rather than the markup `resolveHtml` builds from it, so the
 * statics -- most of the bytes, and the same bytes on every frame -- are scanned
 * once per fingerprint instead of once per frame. Must run AFTER `resolveHtml`:
 * that is what puts a statics-less payload's fingerprint in the cache.
 * @param {string|{raw: string}|{f: string, s?: Array<string>, t?: number, d: Array<*>}} payload
 */
function scanAzAttrs(payload) {
    if (typeof payload === 'string') {
        scanMarkup(payload);
        return;
    }
    if ('raw' in payload) {
        scanMarkup(payload.raw);
        return;
    }
    const f = payload.f;
    const statics = payload.s || /** @type {{s: Array<string>}} */ (fpCache.get(f)).s;
    const plan = planFor(f, statics);
    // An `?each` payload holds one dynamics list per item, all against the one
    // template -- so one plan covers every item.
    if (payload.t === EACH) {
        for (const itemD of payload.d) scanDynamics(plan, itemD);
    } else {
        scanDynamics(plan, payload.d);
    }
}

/**
 * @param {Int8Array} plan
 * @param {Array<*>} dynamics
 */
function scanDynamics(plan, dynamics) {
    for (let i = 0; i < dynamics.length; i++) {
        const v = dynamics[i];
        const variant = plan[i];
        if (Array.isArray(v)) {
            for (let j = 0; j < v.length; j++) scanValue(v[j], variant);
        } else {
            scanValue(v, variant);
        }
    }
}

/**
 * @param {*} v
 * @param {number} variant -- AZ_CHUNK_RES index, or -1 for a content slot
 */
function scanValue(v, variant) {
    if (typeof v !== 'string') {
        // A nested template payload. An attribute slot never holds one --
        // `arizona_html:render_attr/2` returns the whole ` name="value"` as a
        // binary -- so this is always content and needs no chunk variant.
        if (v !== null && typeof v === 'object') scanAzAttrs(v);
        return;
    }
    if (variant >= 0) {
        scanAttrChunk(v, variant);
        return;
    }
    // A content slot can still hold markup (a nested template with no fingerprint
    // of its own arrives already flattened), so it gets the tag scan -- which is
    // also what keeps prose in that slot from declaring anything.
    scanMarkup(v);
}

/**
 * Walk an ops array (top-level or inner), resolving HTML payloads in-place
 * so the main thread receives pure strings ready for DOM insertion.
 * `OP_REPLACE` only appears at the top level; inner-op cases simply skip it.
 * @param {Array<Array<*>>} ops
 */
function resolveOps(ops) {
    for (const op of ops) {
        // A child-view wrapper: `[ChildViewId, ChildOps]`, which `arizona_diff`'s
        // child-view clause emits inside an ITEM_PATCH's inner ops when a `?stateful`
        // in a stream item re-renders. Its head is the view id, a STRING, so the
        // op-code switch below matches nothing and the child's payloads would reach
        // the main thread unresolved -- `applyItemOps` then hands an `{f,s,d}` object
        // to a text write, which renders as "[object Object]". (A child view at the
        // ROOT level needs nothing here: its ops arrive az-prefixed, as ordinary ops.)
        if (typeof op[0] !== 'number') {
            resolveOps(op[1]);
            continue;
        }
        switch (op[0]) {
            case OP_TEXT: {
                // A scalar text value arrives as a bare string; an HTML fragment (a
                // nested-template / plain-list-each zip-map, or a `?raw` `{raw}` tag)
                // arrives as an object. Record which BEFORE resolveHtml flattens both to
                // a string, so the main thread renders text via a text node (safe) and
                // HTML via innerHTML. (Nothing extra rides the WS wire: text is a string,
                // HTML an object -- the type itself is the discriminator; op[3] is only
                // for the worker -> main-thread message.)
                const payload = op[2];
                const isHtml = typeof payload !== 'string';
                op[2] = resolveHtml(payload);
                op[3] = isHtml;
                if (isHtml) scanAzAttrs(payload);
                break;
            }
            case OP_REPLACE: {
                const payload = op[2];
                op[2] = resolveHtml(payload);
                scanAzAttrs(payload);
                break;
            }
            case OP_INSERT: {
                const payload = op[4];
                op[4] = resolveHtml(payload);
                scanAzAttrs(payload);
                break;
            }
            case OP_ITEM_PATCH:
                resolveOps(op[3]);
                break;
            case OP_LIST_PATCH:
                // Positional plain-list patch: resolve each sub-op's payload.
                // ITEM_PATCH carries inner ops at [2] (vs [3] for the keyed
                // stream form -- no key); INSERT carries item HTML at [2].
                for (const sub of op[2]) {
                    if (sub[0] === OP_ITEM_PATCH) resolveOps(sub[2]);
                    else if (sub[0] === OP_INSERT) {
                        const payload = sub[2];
                        sub[2] = resolveHtml(payload);
                        scanAzAttrs(payload);
                    }
                }
                break;
        }
    }
}

// ---------------------------------------------------------------------------
// WebSocket lifecycle
// ---------------------------------------------------------------------------

/**
 * Compute reconnection delay with step backoff and jitter.
 * Imported from core but re-used here for the reconnect timer.
 */
import { backoff } from './arizona-core.js';

/**
 * Open (or reopen) the WebSocket connection.
 */
function openSocket() {
    if (_ws && _ws.readyState < 2) return;
    if (_reconnectTimer) clearTimeout(_reconnectTimer);
    _reconnectTimer = null;
    _fpsSent = false;
    // A reconnect open promises the `cached_fps` announcement as its first
    // frame (`_az_fps_follow=1` below), so the server defers the full-page
    // resync until it arrives and dedups the payload against it. First
    // connects never flag: SSR already delivered the page, nothing to defer.
    _fpsFollow = _reconnecting;

    if (!_wsUrl) return;

    // Rebuild the WebSocket URL from this worker's own origin instead of
    // using the host/protocol that came in via postMessage. Even though the
    // main thread always constructs _wsUrl from location.host, reconstructing
    // here guarantees the socket target can never be steered off-origin and
    // makes the origin invariant legible to static analyzers.
    const incoming = new URL(_wsUrl);
    const protocol = self.location.protocol === 'https:' ? 'wss:' : 'ws:';
    let url = `${protocol}//${self.location.host}${incoming.pathname}${incoming.search}`;
    if (_reconnecting) url += '&_az_reconnect=1&_az_fps_follow=1';

    const ws = new WebSocket(url);
    _ws = ws;

    ws.onopen = () => {
        if (_ws !== ws) return;
        _heartbeatPending = false;
        _heartbeatInterval = setInterval(() => {
            if (_heartbeatPending) {
                ws.close();
                return;
            }
            _heartbeatPending = true;
            ws.send(SYS_PING);
        }, 30000);

        sendCachedFps();

        postMessage([1, _reconnecting]);
    };

    ws.onmessage = (e) => {
        if (_ws !== ws) return;
        _heartbeatPending = false;
        if (e.data === SYS_PONG) return;

        // Reset the backoff only once a real frame arrives -- a working session,
        // not a bare WS handshake. A server that accepts the socket but drops it
        // before framing (crashing mount, dead backend) thus keeps backing off
        // instead of being hammered at backoff[0].
        _attempt = 0;

        const msg = JSON.parse(e.data);
        const ops = msg.o || null;
        const effects = msg.e || null;

        if (ops) {
            resolveOps(ops);
            flushTouchedFps();
        }

        const firstAfterReconnect = _reconnecting;
        if (_reconnecting) _reconnecting = false;

        // `az-*` names this batch introduced, so the main thread can delegate any
        // new event type without walking the DOM it just patched. Sent as raw
        // attribute names: which of them are events and which are framework
        // directives is the main thread's call, so that list lives in one place.
        const azAttrs = _newAzAttrs;
        _newAzAttrs = [];
        postMessage([0, ops, effects, firstAfterReconnect, azAttrs.length ? azAttrs : null]);
    };

    ws.onclose = (e) => {
        if (_ws !== ws && _ws !== null) return;
        if (_heartbeatInterval) clearInterval(_heartbeatInterval);
        _heartbeatInterval = null;
        _heartbeatPending = false;
        _ws = null;

        postMessage([2, e.code]);

        if (e.code === 1000) return;
        _reconnecting = true;
        _reconnectTimer = setTimeout(openSocket, backoff(_attempt++));
    };

    // Deliberately silent: a WebSocket error is always followed by `onclose`,
    // which reports the code to the main thread and drives the reconnect, and the
    // browser already logs the failed handshake itself. Logging here too would
    // print a duplicate line on every backoff attempt of a server restart. The
    // handler exists only so the error event has a listener.
    ws.onerror = () => {};
}

// ---------------------------------------------------------------------------
// Main thread message handler
// ---------------------------------------------------------------------------

self.onmessage = (e) => {
    const msg = e.data;
    switch (msg[0]) {
        case 0: {
            // [0, wsUrl, isReconnect] -- connect. isReconnect marks a connect
            // that must resync an already-rendered DOM (a bfcache restore
            // respawns the worker against the page state pagehide froze), so
            // the socket opens with `_az_reconnect=1` exactly like an
            // in-worker backoff reconnect.
            _wsUrl = msg[1];
            if (msg[2]) _reconnecting = true;
            openSocket();

            // Hydrate in-memory cache from IDB (cross-session persistence),
            // then announce cached fingerprints to the server. The settle
            // marker runs on success AND failure: a flagged reconnect's
            // mandatory announcement waits for it (see sendCachedFps), so a
            // broken IDB must still unblock the server's deferred resync.
            idbLoadPruned()
                .then((entries) => {
                    if (entries.length > 0) loadFpEntries(entries);
                })
                .catch(() => {})
                .then(() => {
                    _hydrated = true;
                    sendCachedFps();
                });
            break;
        }
        case 1: {
            // [1, jsonString] -- send data
            if (_ws && _ws.readyState === 1) {
                _ws.send(msg[1]);
                break;
            }
            // The socket is down (typically the reconnect backoff window, up to
            // ~10s), so the frame is dropped: replaying it after the reconnect
            // would apply stale intent to a re-mounted view. Say so -- silence
            // here is indistinguishable from a handler that never fired, and this
            // window is exactly when a user retries the click that "did nothing".
            // Reported by size, not content: an event frame carries the
            // auto-collected form fields, passwords included, and parsing one
            // here would make a diagnostic throw on a frame the socket path
            // itself never inspects.
            console.warn(`[arizona] socket not open, dropped a ${msg[1].length}-byte frame`);
            break;
        }
        case 2:
            // [2, code] -- close WS
            if (_ws) _ws.close(msg[1]);
            break;
        case 3: {
            // [3, {path, qs}] -- an SPA navigation changed both the path and the
            // query string, so update the reconnect URL to match: set the
            // framework `_az_path`, drop the previous page's user params, and lay
            // down the navigated-to qs (which already carries any connect()
            // extras). Framework `_az_*` params (e.g. `_az_caps`) are preserved.
            // Without replacing the user params, the original page's query string
            // would persist and the navigated-to one would be lost on reconnect.
            if (!_wsUrl) break;
            const { path, qs } = msg[1];
            const u = new URL(_wsUrl);
            u.searchParams.set('_az_path', path);
            for (const key of [...u.searchParams.keys()]) {
                if (!key.startsWith('_az_')) u.searchParams.delete(key);
            }
            for (const [k, v] of new URLSearchParams(qs)) {
                u.searchParams.append(k, v);
            }
            _wsUrl = u.toString();
            break;
        }
    }
};
