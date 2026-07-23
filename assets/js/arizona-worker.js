/**
 * @module arizona.worker
 *
 * Arizona Web Worker -- owns WebSocket connection, JSON parsing, template
 * resolution, and fingerprint cache persistence (IndexedDB).
 * Sends pre-computed DOM-ready data to the main thread.
 *
 * Worker -> Main protocol (arrays for fast structured clone):
 *   [0, ops|null, effects|null, firstAfterReconnect] -- resolved message
 *   [1, isReconnect]                                  -- WS opened
 *   [2, closeCode]                                    -- WS closed
 *
 * Main -> Worker protocol:
 *   [0, wsUrl]      -- connect (full URL)
 *   [1, jsonString] -- send data (pre-stringified on main)
 *   [2, code]       -- close WS
 */

import {
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
const OP_UPDATE = 3;
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
// Entries carry a last-used stamp that ranks eviction; a store written under a
// shape without it cannot be ranked, so the upgrade rebuilds the store rather
// than mixing the two.
const DB_VERSION = 2;

/** @type {Promise<IDBDatabase>|null} */
let _dbReady = null;

function getDB() {
    if (!_dbReady) {
        _dbReady = new Promise((resolve, reject) => {
            const req = indexedDB.open(DB_NAME, DB_VERSION);
            req.onupgradeneeded = () => {
                const db = req.result;
                if (db.objectStoreNames.contains(STORE)) db.deleteObjectStore(STORE);
                db.createObjectStore(STORE);
            };
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
 * Send cached fingerprint keys to the server exactly once per connection.
 * Called from ws.onopen and after IDB load -- whichever finds both fpCache
 * populated and WS open first actually sends; the other is a no-op.
 * Capped at the most-recently-used `FP_CACHE_MAX`: this frame goes out on every
 * open, reconnects included, and anything left out simply arrives with its
 * statics attached.
 */
function sendCachedFps() {
    if (_fpsSent || fpCache.size === 0 || !_ws || _ws.readyState !== 1) return;
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
 * Walk an ops array (top-level or inner), resolving HTML payloads in-place
 * so the main thread receives pure strings ready for DOM insertion.
 * `OP_REPLACE` only appears at the top level; inner-op cases simply skip it.
 * @param {Array<Array<*>>} ops
 */
function resolveOps(ops) {
    for (const op of ops) {
        switch (op[0]) {
            case OP_TEXT: {
                // A scalar text value arrives as a bare string; an HTML fragment (a
                // nested-template / plain-list-each zip-map, or a `?raw` `{raw}` tag)
                // arrives as an object. Record which BEFORE resolveHtml flattens both to
                // a string, so the main thread renders text via a text node (safe) and
                // HTML via innerHTML. (Nothing extra rides the WS wire: text is a string,
                // HTML an object -- the type itself is the discriminator; op[3] is only
                // for the worker -> main-thread message.)
                const isHtml = typeof op[2] !== 'string';
                op[2] = resolveHtml(op[2]);
                op[3] = isHtml;
                break;
            }
            case OP_UPDATE:
            case OP_REPLACE:
                op[2] = resolveHtml(op[2]);
                break;
            case OP_INSERT:
                op[4] = resolveHtml(op[4]);
                break;
            case OP_ITEM_PATCH:
                resolveOps(op[3]);
                break;
            case OP_LIST_PATCH:
                // Positional plain-list patch: resolve each sub-op's payload.
                // ITEM_PATCH carries inner ops at [2] (vs [3] for the keyed
                // stream form -- no key); INSERT carries item HTML at [2].
                for (const sub of op[2]) {
                    if (sub[0] === OP_ITEM_PATCH) resolveOps(sub[2]);
                    else if (sub[0] === OP_INSERT) sub[2] = resolveHtml(sub[2]);
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

    if (!_wsUrl) return;

    // Rebuild the WebSocket URL from this worker's own origin instead of
    // using the host/protocol that came in via postMessage. Even though the
    // main thread always constructs _wsUrl from location.host, reconstructing
    // here guarantees the socket target can never be steered off-origin and
    // makes the origin invariant legible to static analyzers.
    const incoming = new URL(_wsUrl);
    const protocol = self.location.protocol === 'https:' ? 'wss:' : 'ws:';
    let url = `${protocol}//${self.location.host}${incoming.pathname}${incoming.search}`;
    if (_reconnecting) url += '&_az_reconnect=1';

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

        postMessage([0, ops, effects, firstAfterReconnect]);
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
            // [0, wsUrl] -- connect
            _wsUrl = msg[1];
            openSocket();

            // Hydrate in-memory cache from IDB (cross-session persistence),
            // then announce cached fingerprints to the server.
            idbLoadPruned()
                .then((entries) => {
                    if (entries.length > 0) loadFpEntries(entries);
                    sendCachedFps();
                })
                .catch(() => {});
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
