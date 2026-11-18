/**
 * @module arizona.worker
 *
 * Arizona Web Worker -- owns WebSocket connection, JSON parsing, template
 * resolution, and fingerprint cache persistence (IndexedDB).
 * Sends pre-computed DOM-ready data to the main thread.
 *
 * Worker -> Main protocol (arrays for fast structured clone):
 *   [0, ops|null, effects|null, firstAfterReconnect, azAttrs|null, ref|null, shown]
 *                                                     -- resolved message; `ref` is
 *                                                        the reply ref of the frame
 *                                                        it answers, if that one
 *                                                        carried one; `shown` says it
 *                                                        answered a navigate/patch
 *                                                        and put its page on screen
 *   [1, isReconnect]                                  -- WS opened
 *   [2, closeCode]                                    -- WS closed
 *   [3]                                               -- pong to a [4] ping
 *   [4, capped]                                       -- a connect attempt started;
 *                                                        `capped`: scheduled at the
 *                                                        backoff's cap
 *
 * Main -> Worker protocol:
 *   [0, wsUrl, isReconnect, closes] -- connect (full URL; isReconnect
 *                              resyncs an already-rendered DOM, e.g. a bfcache
 *                              restore; `closes` the closes main has seen)
 *   [1, jsonString, nav?]   -- send data (pre-stringified on main); `nav`
 *                              ({ref, path, qs, href, base, closes}) tags a
 *                              navigate/patch frame with the page it asks for,
 *                              the document base a URL resolves against, and
 *                              the closes main had seen when it posted it
 *   [2, code]               -- close WS
 *   [4]                     -- acknowledge the frames sent so far: a ping, whose
 *                              pong the worker reports as [3]
 *   [5]                     -- the open socket's view mounted
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

/** Effect op codes that navigate -- must match `arizona_effect.hrl`. */
const EFFECT_NAVIGATE = 10;
const EFFECT_TRANSITION = 20;
const EFFECT_PATCH = 24;

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

/**
 * The navigate/patch frames sent on the open socket whose reply has not come, by
 * reply ref: the page each asks for, its user query string already merged with the
 * connect() extras. A socket that goes away takes them with it.
 *
 * The URL a reconnect dials (`_wsUrl`) names the page on screen, so it moves when a
 * reply arrives that put its page there -- not when the frame is sent, since a
 * reply lost with the socket leaves the old page up, and the server must resync
 * the view the DOM holds. A reply carrying a navigate/patch effect of its own (a
 * redirect, a full load the server answered the frame with) put nothing there. The
 * move happens here, on arrival, so no reconnect timer can fire between the reply
 * and a main-thread round trip.
 * @type {Map<number, {path: string, qs: string, href: string, base: string}>}
 */
const _navTargets = new Map();

/**
 * Closes reported to main (`[2, ...]`), counted from the base main sent on
 * connect: a navigation tagged with fewer was posted for a socket already gone.
 * @type {number}
 */
let _closes = 0;

/**
 * Does an effect list navigate away from `href`, the page the reply answers for
 * (a `navigate`/`patch`, also inside a `transition`)? One that only names a
 * fragment of that page -- the same path and query, an empty one included -- and
 * is no full load only scrolls it. A URL resolves against `base`, the document
 * base, as the main thread resolves it.
 * @param {Array<Array<*>>|null} effects
 * @param {string} href
 * @param {string} base
 * @returns {boolean}
 */
function navigates(effects, href, base) {
    if (!effects) return false;
    return effects.some((/** @type {Array<*>} */ c) => {
        if (c[0] === EFFECT_NAVIGATE || c[0] === EFFECT_PATCH)
            return c[2]?.full === true || !fragmentOf(c[1], href, base);
        if (c[0] !== EFFECT_TRANSITION) return false;
        const inner = c[2];
        return navigates(Array.isArray(inner?.[0]) ? inner : [inner], href, base);
    });
}

/**
 * Does `url`, resolved against `base`, name a fragment of the page `href`?
 * @param {*} url
 * @param {string} href
 * @param {string} base
 * @returns {boolean}
 */
function fragmentOf(url, href, base) {
    if (typeof url !== 'string') return false;
    let u;
    let page;
    try {
        u = new URL(url, base);
        page = new URL(href);
    } catch {
        return false;
    }
    const hasQuery = (/** @type {string} */ h) => h.split('#')[0].includes('?');
    return (
        (u.hash !== '' || u.href.endsWith('#')) &&
        u.origin === page.origin &&
        u.pathname === page.pathname &&
        u.search === page.search &&
        hasQuery(u.href) === hasQuery(page.href)
    );
}

/**
 * Point the reconnect URL at `path` + `qs`: set the framework `_az_path`, drop the
 * previous page's user params, and lay down `qs` (which already carries any
 * connect() extras). Framework `_az_*` params (e.g. `_az_caps`) are preserved.
 * @param {string} path
 * @param {string} qs
 */
function dialPage(path, qs) {
    if (!_wsUrl) return;
    const u = new URL(_wsUrl);
    u.searchParams.set('_az_path', path);
    for (const key of [...u.searchParams.keys()]) {
        if (!key.startsWith('_az_')) u.searchParams.delete(key);
    }
    for (const [k, v] of new URLSearchParams(qs)) {
        u.searchParams.append(k, v);
    }
    _wsUrl = u.toString();
}

/** @type {number} */
let _attempt = 0;

/** Close codes (must match `arizona_socket.erl`): crash after mount, mount failed. */
const WS_CLOSE_CRASH = 4500;
const WS_CLOSE_MOUNT_FAILED = 4501;
/** What a socket this worker gave up on reports: closed with no close frame. */
const WS_CLOSE_ABNORMAL = 1006;

/**
 * When the open socket's view last mounted, as Date.now(), or null before it
 * has. The main thread tells (the [5] message): it is the one that knows which
 * frame proves a mount.
 * @type {number|null}
 */
let _mountedAt = null;

/**
 * A view that crashes (4500) within this long of its mount does not reset the
 * reconnect backoff: one that crashes right after every remount must keep
 * backing off instead of reconnecting at backoff[0].
 */
const CRASH_STABLE_MS = 10000;

/** @type {ReturnType<typeof setTimeout>|null} */
let _reconnectTimer = null;

/** @type {ReturnType<typeof setInterval>|null} */
let _heartbeatInterval = null;

/** @type {boolean} */
let _heartbeatPending = false;

/**
 * The open socket's pings awaiting their pong, oldest first: `true` for an
 * acknowledgement ping ([4]), `false` for a heartbeat. The server answers frames
 * in order, so each pong belongs to the oldest outstanding ping.
 * @type {boolean[]}
 */
let _pings = [];

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
                const isHtml = typeof op[2] !== 'string';
                op[2] = resolveHtml(op[2]);
                op[3] = isHtml;
                break;
            }
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
import { BACKOFF_CAP_ATTEMPT, backoff } from './arizona-core.js';

/**
 * The next connect attempt was scheduled at the backoff's cap: the reconnects
 * have run through every shorter step without one opening.
 */
let _capped = false;

/**
 * `ws` is no longer this worker's socket: tell the main thread (`[2, code]`) and,
 * unless the close was a deliberate end (1000), schedule the reconnect. Runs when
 * the socket closes, and at once when this worker closes it itself: a close the
 * worker starts on a peer that stopped answering (the heartbeat's) is never
 * answered either, so the browser fires `close` only once it gives up on the
 * handshake, tens of seconds later -- all that time the main thread would take the
 * socket for open while every frame is dropped. Detached here, the socket's late
 * `close` (and any frame it still delivers) finds it is not `_ws` and does nothing.
 * @param {WebSocket} ws
 * @param {number} code
 */
function socketGone(ws, code) {
    if (_ws !== ws) return;
    if (_heartbeatInterval) clearInterval(_heartbeatInterval);
    _heartbeatInterval = null;
    _heartbeatPending = false;
    _ws = null;
    // The navigations it still owed a reply are the main thread's to send again.
    _navTargets.clear();
    _closes++;
    postMessage([2, code]);

    if (code === 1000) return;
    // Reset the backoff only after a working session: one that mounted its
    // view, did not fail to mount (a navigate), and did not crash right after
    // its last mount. A server that accepts the socket but never mounts (a
    // dead backend, a failing mount) thus keeps backing off instead of being
    // hammered at backoff[0].
    if (
        _mountedAt !== null &&
        code !== WS_CLOSE_MOUNT_FAILED &&
        (code !== WS_CLOSE_CRASH || Date.now() - _mountedAt >= CRASH_STABLE_MS)
    ) {
        _attempt = 0;
    }
    _reconnecting = true;
    _capped = _attempt >= BACKOFF_CAP_ATTEMPT;
    _reconnectTimer = setTimeout(openSocket, backoff(_attempt++));
}

/**
 * Open (or reopen) the WebSocket connection.
 */
function openSocket() {
    if (_ws && _ws.readyState < 2) return;
    if (_reconnectTimer) clearTimeout(_reconnectTimer);
    _reconnectTimer = null;
    _fpsSent = false;
    _mountedAt = null;
    _pings = [];
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
    postMessage([4, _capped]);

    ws.onopen = () => {
        if (_ws !== ws) return;
        _heartbeatPending = false;
        _heartbeatInterval = setInterval(() => {
            if (_heartbeatPending) {
                ws.close();
                socketGone(ws, WS_CLOSE_ABNORMAL);
                return;
            }
            _heartbeatPending = true;
            _pings.push(false);
            ws.send(SYS_PING);
        }, 30000);

        sendCachedFps();

        postMessage([1, _reconnecting]);
    };

    ws.onmessage = (e) => {
        if (_ws !== ws) return;
        _heartbeatPending = false;
        if (e.data === SYS_PONG) {
            if (_pings.shift()) postMessage([3]);
            return;
        }

        const msg = JSON.parse(e.data);
        const ops = msg.o || null;
        const effects = msg.e || null;
        // The `az-*` attribute names this frame proves the app can render: the
        // connect frame carries the compile-time union walked from the route's
        // handler and layouts, and any later frame carries only the per-socket
        // delta (a render-observed command attribute, a dynamically mounted
        // component's names, a navigate target's walk). Raw names, not event
        // types: which of them are DOM events and which are framework directives
        // is the main thread's call, so that list lives in one place. The worker
        // only forwards -- deriving the names here would mean a regex over
        // serialized HTML, which cannot tell an attribute NAME from `az-` text
        // inside an attribute VALUE and is wrong in both directions.

        if (ops) {
            resolveOps(ops);
            flushTouchedFps();
        }

        // The flag belongs to the frame that rebuilds the DOM, not merely the first
        // one to arrive. A reconnect opens with the declared `az-*` names, which
        // carry no ops: letting that frame claim the flag makes the main thread
        // restore form state against the DOM the following OP_REPLACE is about to
        // discard, silently losing what the user had typed.
        const firstAfterReconnect = _reconnecting && ops !== null;
        if (firstAfterReconnect) _reconnecting = false;

        const ref = msg.r ?? null;
        const target = ref === null ? undefined : _navTargets.get(ref);
        let shown = false;
        if (target) {
            _navTargets.delete(ref);
            shown = !navigates(effects, target.href, target.base ?? target.href);
            if (shown) dialPage(target.path, target.qs);
        }

        postMessage([0, ops, effects, firstAfterReconnect, msg.a ?? null, ref, shown]);
    };

    ws.onclose = (e) => socketGone(ws, e.code);

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
            _closes = msg[3] ?? 0;
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
            // [1, jsonString, nav?] -- send data
            // A navigation main posted before it saw a close the worker has since
            // reported was meant for that dead socket: main pends or drops it as
            // that close says, and sending it on the new one would send it twice.
            if (msg[2] && msg[2].closes !== _closes) break;
            if (_ws && _ws.readyState === 1) {
                _ws.send(msg[1]);
                if (msg[2]) {
                    const { ref, path, qs, href, base } = msg[2];
                    _navTargets.set(ref, { path, qs, href, base });
                }
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
            // [2, code] -- close WS. Reported at once, as the heartbeat's close is:
            // on a dead peer the socket's own `close` comes only when the browser
            // gives up on the handshake.
            if (_ws) {
                const ws = _ws;
                ws.close(msg[1]);
                socketGone(ws, msg[1]);
            }
            break;
        case 4:
            // [4] -- acknowledge the frames sent so far (see [3] to main).
            if (_ws && _ws.readyState === 1) {
                _pings.push(true);
                _ws.send(SYS_PING);
            }
            break;
        case 5:
            // [5] -- the open socket's view mounted (a first connect's frame, a
            // reconnect's resync, a navigate's OP_REPLACE). A message about a
            // socket that already closed finds none open and is dropped.
            if (_ws && _ws.readyState === 1) _mountedAt = Date.now();
            break;
    }
};
