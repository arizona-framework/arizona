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

import { resolveHtml, fpCache, loadFpEntries, setOnPersist } from './arizona-core.js';

/** Op codes -- must match server and main thread. */
const OP_TEXT = 0;
const OP_UPDATE = 3;
const OP_INSERT = 5;
const OP_ITEM_PATCH = 7;
const OP_REPLACE = 8;

const SYS_PING = '0';
const SYS_PONG = '1';

// ---------------------------------------------------------------------------
// IndexedDB persistence -- one entry per fingerprint, Worker-owned
// ---------------------------------------------------------------------------

const DB_NAME = 'arizona';
const STORE = 'cache';

/** @type {Promise<IDBDatabase>|null} */
let _dbReady = null;

function getDB() {
    if (!_dbReady) {
        _dbReady = new Promise((resolve, reject) => {
            const req = indexedDB.open(DB_NAME, 1);
            req.onupgradeneeded = () => req.result.createObjectStore(STORE);
            req.onsuccess = () => resolve(req.result);
            req.onerror = () => reject(req.error);
        });
    }
    return _dbReady;
}

/** Read all entries as [[fpId, {s, t?}], ...] for cache hydration. */
function idbLoadAll() {
    return getDB().then(db => new Promise((resolve) => {
        /** @type {Array<[string, {s: Array<string>, t?: number}]>} */
        const entries = [];
        const req = db.transaction(STORE).objectStore(STORE).openCursor();
        req.onsuccess = () => {
            const c = req.result;
            if (c) { entries.push([/** @type {string} */ (c.key), c.value]); c.continue(); }
            else resolve(entries);
        };
        req.onerror = () => resolve([]);
    }));
}

/**
 * Write a single fingerprint entry.
 * @param {string} fpId
 * @param {{s: Array<string>, t?: number}} entry
 */
function idbPut(fpId, entry) {
    getDB().then(db => {
        db.transaction(STORE, 'readwrite').objectStore(STORE).put(entry, fpId);
    }).catch(() => {});
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
 */
function sendCachedFps() {
    if (_fpsSent || fpCache.size === 0 || !_ws || _ws.readyState !== 1) return;
    _fpsSent = true;
    _ws.send(JSON.stringify(['cached_fps', Array.from(fpCache.keys())]));
}

// Wire up fp cache persistence: write each new fingerprint to IndexedDB.
setOnPersist(/** @param {string} fpId @param {{s: Array<string>, t?: number}} entry */ (fpId, entry) => {
    idbPut(fpId, entry);
});

// ---------------------------------------------------------------------------
// Op resolution -- resolve template payloads to HTML strings in-place
// ---------------------------------------------------------------------------

/**
 * Walk ops array, resolving HTML payloads in-place so the main thread
 * receives pure strings ready for DOM insertion.
 * @param {Array<Array<*>>} ops
 */
function resolveOps(ops) {
    for (const op of ops) {
        switch (op[0]) {
            case OP_TEXT:
                op[2] = resolveHtml(op[2]);
                break;
            case OP_UPDATE:
                op[2] = resolveHtml(op[2]);
                break;
            case OP_INSERT:
                op[4] = resolveHtml(op[4]);
                break;
            case OP_REPLACE:
                op[2] = resolveHtml(op[2]);
                break;
            case OP_ITEM_PATCH:
                resolveInnerOps(op[3]);
                break;
        }
    }
}

/**
 * Resolve inner ops (within ITEM_PATCH).
 * @param {Array<Array<*>>} innerOps
 */
function resolveInnerOps(innerOps) {
    for (const op of innerOps) {
        switch (op[0]) {
            case OP_TEXT:
                op[2] = resolveHtml(op[2]);
                break;
            case OP_UPDATE:
                op[2] = resolveHtml(op[2]);
                break;
            case OP_INSERT:
                op[4] = resolveHtml(op[4]);
                break;
            case OP_ITEM_PATCH:
                resolveInnerOps(op[3]);
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
    let url = _wsUrl;
    if (_reconnecting) url += '&reconnect=1';

    const ws = new WebSocket(url);
    _ws = ws;

    ws.onopen = () => {
        if (_ws !== ws) return;
        _attempt = 0;
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

        const msg = JSON.parse(e.data);
        const ops = msg.o || null;
        const effects = msg.e || null;

        if (ops) resolveOps(ops);

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
        if (e.code === 4500) return;
        _reconnecting = true;
        _reconnectTimer = setTimeout(openSocket, backoff(_attempt++));
    };

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
            idbLoadAll().then(entries => {
                if (entries.length > 0) loadFpEntries(entries);
                sendCachedFps();
            }).catch(() => {});
            break;
        }
        case 1:
            // [1, jsonString] -- send data
            if (_ws && _ws.readyState === 1) _ws.send(msg[1]);
            break;
        case 2:
            // [2, code] -- close WS
            if (_ws) _ws.close(msg[1]);
            break;
    }
};
