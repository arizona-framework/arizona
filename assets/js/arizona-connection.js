import { applyEffects, executeJS, formFields, JS_NAVIGATE, osHost } from './arizona-commands.js';
import { allDocs, closeAllPip } from './arizona-docs.js';
import { bindDocumentEvents, noteAzAttrs, unbindDocuments } from './arizona-events.js';
import { destroyAllHooks, mountHooks } from './arizona-hooks.js';
import { setHost } from './arizona-host.js';
import { applyOps, OP, opsMatchTransition } from './arizona-ops.js';
import { _pendingScroll, applyScroll, setPendingScroll } from './arizona-scroll.js';
import { parseTransitionAttr, runTransition } from './arizona-transitions.js';
import { isWebUrl, loadPage } from './arizona-util.js';

/** Worker protocol opcodes -- must match arizona-worker.js. */
const W_CONNECT = 0;

const W_SEND = 1;

const W_CLOSE = 2;

const W_UPDATE_PATH = 3;

const W_ACK_REPLAY = 4;

const W_MOUNTED = 5;

/** WebSocket close codes used by Arizona (must match `arizona_socket.erl`). */
const WS_CLOSE_NORMAL = 1000;

/** The view crashed after it mounted. */
const WS_CLOSE_CRASH = 4500;

/** The view failed to mount. */
const WS_CLOSE_MOUNT_FAILED = 4501;

/**
 * Crash recovery, by the server's close code.
 *
 * - WS_CLOSE_CRASH: the page is current, so the worker's backoff reconnect
 *   remounts the view in place -- no reload, which would throw away client-only
 *   state for nothing.
 * - WS_CLOSE_MOUNT_FAILED: the page itself may be stale (a deploy changed the
 *   route or the assets), so reload it -- after a random
 *   MOUNT_RELOAD_DELAY_MIN..MAX delay, so the tabs a bad deploy breaks don't all
 *   reload at once. The worker keeps reconnecting meanwhile, and a mount that
 *   succeeds first cancels the reload.
 *
 * Consecutive mount-failure reloads of one path are counted in sessionStorage,
 * under one key holding `{path, count}`: it survives the reload of this tab, which
 * is all the count needs, and is gone when the tab closes, so a path (which may
 * carry a secret, a reset token or an invite code) never outlives the tab. A
 * reload of a different path starts over; a successful mount removes the key.
 * Each tab counts on its own: its jittered delay already spreads the tabs out.
 * Past MOUNT_RELOAD_MAX the delay becomes MOUNT_RELOAD_FAILSAFE: a page that
 * cannot mount keeps retrying, slowly, and says why once per page load.
 */
const MOUNT_RELOAD_DELAY_MIN = 5000;

const MOUNT_RELOAD_DELAY_MAX = 10000;

const MOUNT_RELOAD_MAX = 10;

const MOUNT_RELOAD_FAILSAFE = 30000;

const MOUNT_RELOAD_KEY = 'arizona:mount-reloads';

/** @type {ReturnType<typeof setTimeout> | null} */
let _mountReloadTimer = null;

/** Whether this page load already logged entering the failsafe delay. */
let _mountReloadLogged = false;

/**
 * @param {string} path
 * @returns {number} this tab's consecutive mount-failure reloads of `path`; 0
 *   when the stored count is for another path, unreadable, or unavailable
 */
function mountReloadCount(path) {
    try {
        const stored = JSON.parse(sessionStorage.getItem(MOUNT_RELOAD_KEY) ?? 'null');
        return stored?.path === path && Number.isInteger(stored.count) ? stored.count : 0;
    } catch {
        return 0;
    }
}

/**
 * Schedule the reload for a mount failure, unless one is already pending.
 * @returns {number|null} the delay of the reload this call scheduled, or null
 *   when one was already pending
 */
function scheduleMountReload() {
    if (_mountReloadTimer !== null) return null;
    const count = mountReloadCount(location.pathname);
    let delay =
        MOUNT_RELOAD_DELAY_MIN +
        Math.floor(Math.random() * (MOUNT_RELOAD_DELAY_MAX - MOUNT_RELOAD_DELAY_MIN + 1));
    if (count >= MOUNT_RELOAD_MAX) {
        delay = MOUNT_RELOAD_FAILSAFE;
        if (!_mountReloadLogged) {
            _mountReloadLogged = true;
            console.error(
                `[arizona] the view failed to mount ${count} times in a row; ` +
                    `retrying every ${MOUNT_RELOAD_FAILSAFE / 1000} s.`,
            );
        }
    }
    _mountReloadTimer = setTimeout(() => {
        _mountReloadTimer = null;
        // Count the URL actually reloaded: the path may have changed meanwhile.
        const path = location.pathname;
        try {
            sessionStorage.setItem(
                MOUNT_RELOAD_KEY,
                JSON.stringify({ path, count: mountReloadCount(path) + 1 }),
            );
        } catch {
            /* sessionStorage unavailable -- reload anyway */
        }
        location.reload();
    }, delay);
    return delay;
}

function cancelMountReload() {
    if (_mountReloadTimer !== null) clearTimeout(_mountReloadTimer);
    _mountReloadTimer = null;
}

/** A view mounted: a mount-failure loop in this tab, if any, is over. */
function mountSucceeded() {
    cancelMountReload();
    // The worker's reconnect backoff keys on mounts too; this is their one source.
    workerPost(W_MOUNTED);
    try {
        sessionStorage.removeItem(MOUNT_RELOAD_KEY);
    } catch {
        /* sessionStorage unavailable -- nothing to clear */
    }
}

/** @type {Worker|null} */
let _worker = null;

/** @type {boolean} */
let _connected = false;

/**
 * The frame that proves the open connection's view mounted, or null once it
 * arrived (or with no connection). The server sends nothing before a first
 * connect's mount, so any frame proves it; a reconnect's first frame only
 * declares the `az-*` names ahead of the deferred mount, so there the resync
 * (`firstAfterReconnect`) does.
 * @type {'frame'|'resync'|null}
 */
let _mountProof = null;

/**
 * An `arizona:disconnected` went out and no `arizona:connected` has since: the
 * outage it announced is still on, so a further close (a failed reconnect
 * attempt) belongs to it and announces nothing.
 * @type {boolean}
 */
let _outage = false;

/**
 * An `arizona:connected` went out on this page load: a later one remounts a view
 * the page already had, whatever the socket's own reconnect flag says (a first
 * connect that never opened still reconnects with it).
 * @type {boolean}
 */
let _hadMount = false;

/**
 * Closes seen so far. A mount's `arizona:connected` waits for its frame's ops,
 * which a view transition defers; a close in between voids the announcement.
 * @type {number}
 */
let _closeCount = 0;

/**
 * Announce a connection change to the page as a `CustomEvent` on `document`.
 * @param {'arizona:connected'|'arizona:disconnected'} type
 * @param {object} detail
 */
function dispatchConnection(type, detail) {
    _outage = type === 'arizona:disconnected';
    document.dispatchEvent(new CustomEvent(type, { detail }));
}

/** Announce a mounted view, unless the socket closed after `closes` was read. */
function announceConnected(/** @type {number} */ closes) {
    if (closes !== _closeCount) return;
    const reconnect = _hadMount;
    _hadMount = true;
    dispatchConnection('arizona:connected', { reconnect });
}

// Path + query the server last rendered. Tracked so the popstate handler can
// tell a real cross-page back/forward (which needs a server navigate) from a
// same-page fragment change (hash only -- scroll, no round-trip). Mirrors the
// same-page fast path in the az-navigate click handler.
/** @type {string} */
let _currentPath = '';

/** @type {string} */
let _currentQs = '';

// Extra query params passed to `connect(endpoint, params)`. Unlike the page's
// own query string they are connection-level config, constant across SPA
// navigations, so they are re-merged into every reconnect URL (the page qs is
// replaced by the navigated-to qs; these ride along).
/** @type {string} */
let _connectQs = '';

/**
 * Typed form fields saved on an abnormal close, restored by the next resync.
 * `replay` says whether that restore re-sends the form's `az-change`.
 * @type {Map<string, {fields: Object<string, string|string[]>, azChange: string|null, replay: boolean}>}
 */
const _savedForms = new Map();

/**
 * Forms whose `az-change` the last restore replayed, until the server proves it
 * handled the replay (see W_ACK_REPLAY). A crash (4500) before that may have been
 * caused by the replayed values, so the next restore puts them back without
 * replaying them.
 * @type {Set<string>}
 */
const _replayedForms = new Set();

/**
 * The current connection's form replay: how many of its parts have yet to post
 * their events -- the restore itself, plus any command that defers its work (a
 * view transition's update, a fetch's response effects). The acknowledgement
 * goes out when it reaches zero, so it always follows every replayed event.
 * Cleared on close, so a late part of an old replay acknowledges nothing.
 * @type {{pending: number}|null}
 */
let _replay = null;

/** The replay whose commands are executing right now, if any. @type {{pending: number}|null} */
let _replayRunning = null;

/** @param {{pending: number}} replay */
function replayPartDone(replay) {
    replay.pending--;
    if (replay.pending === 0 && replay === _replay) workerPost(W_ACK_REPLAY);
}

/**
 * Hold the replay executing now (if any) open for work a command runs later.
 * Returns the runner for that work: it runs it as part of the replay, then
 * releases the hold -- once, however many times it is called.
 * @returns {(fn: () => void) => void}
 */
function holdReplay() {
    const replay = _replayRunning;
    if (replay) replay.pending++;
    let held = replay !== null;
    return (fn) => {
        const outer = _replayRunning;
        if (held) _replayRunning = replay;
        try {
            fn();
        } finally {
            _replayRunning = outer;
            if (held) {
                held = false;
                replayPartDone(/** @type {{pending: number}} */ (replay));
            }
        }
    };
}

// Teardown for the connection currently owning the module state, so a second
// `connect()` can retire the first instead of orphaning its Worker. Cleared by
// the teardown itself.
/** @type {(() => void)|null} */
let _teardown = null;

/**
 * Set right before an unload the framework starts while the current history
 * entry already names a page the DOM is not showing: a `leavePage` replace (a
 * navigate the server degraded to a full load, its entry pushed before asking)
 * and a popstate served by `location.reload()` (the browser already moved to the
 * destination entry). A full load that is not a replace leaves this page's
 * entry in place, so its scroll is stamped for Back to restore. The scroll on screen belongs to the previous page, so the unload
 * stamp must not write it onto that entry. Cleared by `pagehide`.
 */
let _unloadingToOtherPage = false;

/**
 * Save the current scroll position onto the current (outgoing) history entry so
 * back/popstate can restore it. When leaving via a patch, also tag the outgoing
 * entry `_azNav: 'patch'` so a later back-navigation to it replays as a patch
 * (keeping the view) rather than the default navigate -- important when the
 * entry was a full page load, which carries no tag of its own. The server still
 * corrects the verb (a cross-handler patch falls back to navigate), so the tag
 * is only a hint. The same write records the edge being created towards the
 * next entry (`_azTransitionNext`: the pending view transition, or none -- a
 * plain navigation overwrites an older transitioned edge from this entry) and
 * the entry's position (`_azIdx`), which popstate needs to tell which edge a
 * traversal crossed -- one replaceState per outgoing entry, not one per concern
 * (Safari rate-limits history writes to ~100/30s, then throws SecurityError).
 * @param {string} [navKind]
 */
function saveCurrentScroll(navKind) {
    const { _azTransitionNext: _, ...st } = history.state || {};
    const next = {
        ...st,
        _azScroll: { x: window.scrollX, y: window.scrollY },
        _azIdx: _historyIdx,
    };
    if (navKind === 'patch') next._azNav = 'patch';
    if (_pendingTransition) next._azTransitionNext = _pendingTransition;
    history.replaceState(next, '', location.href);
}

/**
 * Position of the current history entry among this document's SPA entries,
 * stamped on each as `_azIdx`. A view transition belongs to an EDGE between two
 * entries, and each entry has two (one per direction), so popstate compares the
 * destination's index with this one to know which edge it crossed: Back replays
 * the destination's `_azTransitionNext`, Forward its `_azTransitionPrev`. An
 * entry the hash fast path pushes carries no index and never animates (a hash
 * jump has no transition).
 */
let _historyIdx = 0;

// --------------------------------------------------------------------------
// View transitions
// --------------------------------------------------------------------------
// A view transition wraps a DOM change in document.startViewTransition so the
// browser cross-fades (or, with user CSS, morphs) old -> new. Transitions are
// not tied to navigation -- they wrap any DOM change the framework drives:
//   - a synchronous client effect (toggle/add_class/...) -- wrapped immediately
//   - an az-navigate round-trip -- the OP_REPLACE arrives a message later
//   - a push_event -- the resulting server diff arrives a message later
// Real <a href> navigations animate via the user's `@view-transition` CSS, no
// code here.
//
// For the async cases (navigate/push_event) `_pendingTransition` holds the
// intent until the server response lands; the worker message handler then wraps
// that message's ops+effects together. `kind` says which batch to wait for:
// 'replace' (a navigation's page swap -- a stray text/attr tick is ignored) or
// 'any' (the next non-empty diff from a push_event). Set by the az-transition
// attribute, an arizona_js:transition command, or replayed from history state
// on back/forward. Synchronous effects never set it -- they wrap in place.

/** @type {{types?: string[], kind: 'replace'|'any'}|null} */
let _pendingTransition = null;

// The page-swap window of a navigate: from posting the `navigate` frame until its
// reply lands. The server replaces the root view when it handles the frame, but
// the old DOM stays on screen until the `OP_REPLACE` arrives, and an event sent
// from it in between is dispatched by view id alone -- to the NEW root, which
// often shares the id (`page`) and then either crashes on an event it does not
// know or runs the old page's intent. The server cannot tell such an event from
// one sent after the swap (frames carry no page epoch), so the client drops it,
// exactly as a full page load would.
//
// Counted, not flagged: navigates can be in flight back to back, and the window
// must stay open until the LAST one is answered. The server answers each navigate
// frame exactly once and in order -- with its `OP_REPLACE`, or with a navigate
// effect (a middleware redirect, which re-arms through `sendNav`, or a full-page
// load, where the server kept the old view) -- so each such frame settles one
// (`settleNavReplies`). A close loses every reply still owed: the count moves to
// `_navAwaitResync`, which the reconnect's resync `OP_REPLACE` clears.
//
// There is no timeout escape hatch, on purpose: every server answer to a navigate
// frame (replace, redirect, full load) settles it, and a crash closes the socket,
// which hands over to the resync. A server change that could leave a navigate
// frame unanswered must add one, or the page stops sending events.
//
// Residuals, both where the wire carries no reply id: a navigate effect in an
// old-page push frame (a timer redirect) that lands ahead of the reply settles
// one navigate early; so does the `OP_REPLACE` of a patch the server degrades to
// a replace (a cross-handler patch) while a navigate is in flight, letting a
// stale click through before the navigate's own reply. A patch does not arm:
// it usually keeps the view, and the client cannot know ahead that the server
// will degrade it.
let _navInFlight = 0;

let _navAwaitResync = false;

// Bumped per armed navigate and by `disconnect()`, so a send scheduled before
// either (a debounced or throttled push_event) is dropped when it fires later.
let _navEpoch = 0;

/**
 * Send a view event unless it was raised on a page a navigate is replacing, or
 * on a connection since torn down.
 * Framework frames (navigate, patch, cached_fps, ping) do not come through here.
 * A drop is reported, like the worker's socket-down drop: silence would read as
 * a handler that never fired. By event name only -- the payload may carry the
 * auto-collected form fields.
 * @param {string} name event name, for the warning
 * @param {string} msg encoded `[target, event, payload]` frame
 * @param {number} [epoch] `_navEpoch` when the event was raised
 */
function sendEvent(name, msg, epoch = _navEpoch) {
    if (_navInFlight > 0 || _navAwaitResync || epoch !== _navEpoch) {
        console.warn(`[arizona] dropped stale event "${name}"`);
        return;
    }
    workerPost(W_SEND, msg);
}

// The user-facing query string carried to the worker for the next reconnect
// after an SPA navigation: the navigated-to page qs plus the connection-level
// `connect()` extras (constant across navigations). The worker replaces the
// previous page's user params with this, preserving the framework `_az_*` keys.
/**
 * @param {string} qs navigated-to page query string (no leading `?`)
 * @returns {string}
 */
function reconnectUserQs(qs) {
    return [qs, _connectQs].filter(Boolean).join('&');
}

/**
 * Perform an SPA navigation. Shared code path for `az-navigate` clicks and
 * `arizona_js:navigate/1,2` effects.
 *
 * @param {string} path     Path portion (no hash, no query), sent to the server.
 * @param {string} qs       Query string without leading `?`, sent to the server.
 * @param {string} hash     Fragment without leading `#`; used client-side
 *                          to scroll to the target after OP_REPLACE.
 * @param {{replace?: boolean, noscroll?: boolean, fullUrl?: string, kind?: string}} opts
 *   - replace  Use `replaceState` instead of `pushState`. Does not save
 *              outgoing scroll, does not reset scroll.
 *   - noscroll Push only: skip the scroll-to-top/hash after REPLACE.
 *   - fullUrl  Exact URL to write to history (defaults to
 *              `path + '?' + qs + '#' + hash`). Lets the click handler
 *              preserve the original href verbatim.
 *   - kind     `'navigate'` (default, replaces the root view) or `'patch'`
 *              (in-place: the server keeps the view and re-renders it). The
 *              kind is the WS frame verb and is tagged onto the history entry
 *              (`_azNav`) so popstate replays the same mode on back/forward.
 *
 * Reads `_pendingTransition` (set by the caller from the az-transition attribute
 * or a transition command): on a push it is stamped onto both ends of the new
 * edge -- the outgoing entry's `_azTransitionNext`, the new entry's
 * `_azTransitionPrev` -- so popstate replays it only when traversing this edge.
 * A replace creates no edge: it keeps the replaced entry's stamps.
 */
function navigateTo(path, qs, hash, opts) {
    const kind = opts.kind === 'patch' ? 'patch' : 'navigate';
    const pathAndQs = qs ? `${path}?${qs}` : path;
    const fullUrl = opts.fullUrl || (hash ? `${pathAndQs}#${hash}` : pathAndQs);
    /** @type {{_azTransitionPrev?: typeof _pendingTransition, _azTransitionNext?: typeof _pendingTransition, _azNav?: string, _azIdx?: number}} */
    const navState = {};
    if (kind === 'patch') navState._azNav = 'patch';
    if (opts.replace) {
        // Same position, a different page: the edges into and out of this
        // position are unchanged, so the replaced entry's stamps carry over
        // (Forward into the new page crosses the same edge Back out of it
        // does) and a pending transition stamps nothing -- it animates this
        // swap, not an edge.
        const cur = history.state || {};
        navState._azIdx = _historyIdx;
        if (cur._azTransitionPrev) navState._azTransitionPrev = cur._azTransitionPrev;
        if (cur._azTransitionNext) navState._azTransitionNext = cur._azTransitionNext;
        history.replaceState(navState, '', fullUrl);
    } else {
        // One replaceState: saveCurrentScroll stamps scroll, patch tag, index
        // AND the outgoing end of this edge onto the outgoing entry together.
        saveCurrentScroll(kind);
        _historyIdx += 1;
        navState._azIdx = _historyIdx;
        if (_pendingTransition) navState._azTransitionPrev = _pendingTransition;
        history.pushState(navState, '', fullUrl);
        if (!opts.noscroll) setPendingScroll({ kind: 'push', hash, patch: kind === 'patch' });
    }
    sendNav(kind, path, qs);
}

/**
 * Settle the navigate replies a server frame carries: its `OP_REPLACE` or a
 * top-level navigate effect. The first frame after a reconnect is the resync,
 * which clears the post-close wait instead. Called
 * before the frame is applied, so its own effects -- a redirect re-arming, the new
 * page's push_event -- see the settled count.
 * @param {Array<Array<*>>|null} ops
 * @param {Array<Array<*>>|null} effects
 * @param {boolean} resync
 */
function settleNavReplies(ops, effects, resync) {
    if (resync) {
        _navAwaitResync = false;
        return;
    }
    if (_navInFlight === 0) return;
    if (ops?.some((op) => op[0] === OP.REPLACE) || effects?.some((eff) => eff[0] === JS_NAVIGATE)) {
        _navInFlight--;
    }
}

/**
 * Post a navigate/patch frame and retarget the reconnect URL. A navigate arms the
 * page-swap window (`_navInFlight`) -- only on a live socket: a frame posted while
 * disconnected is dropped by the worker and gets no reply, and the reconnect's
 * resync is what puts the new page on screen.
 * @param {'navigate'|'patch'} kind
 * @param {string} path
 * @param {string} qs
 */
function sendNav(kind, path, qs) {
    if (kind === 'navigate' && _connected) {
        _navInFlight++;
        _navEpoch++;
    }
    workerPost(W_SEND, JSON.stringify([kind, { path, qs }]));
    workerPost(W_UPDATE_PATH, { path, qs: reconnectUserQs(qs) });
    _currentPath = path;
    _currentQs = qs;
}

/**
 * Load `url` as a real page. `replace` finishes a navigation already in history
 * -- the server degrading the client's own navigate frame, whose entry was pushed
 * before asking -- so it must not add a second entry; the browser only folds the
 * load into the current entry by itself when the URL is identical.
 *
 * Only an http(s) URL may be loaded. Any other scheme (`javascript:`, `data:`)
 * would run in this page, and the URL can come from a server that echoed a
 * request parameter -- so it is dropped with a warning, never loaded.
 * @param {string} url
 * @param {boolean} [replace]
 */
function leavePage(url, replace) {
    if (!isWebUrl(url)) return;
    // A replace finishes a navigation whose entry already names `url`, so the
    // scroll on screen is not that entry's to keep (see _unloadingToOtherPage).
    _unloadingToOtherPage = Boolean(replace);
    loadPage(url, replace);
}

/**
 * Where a `navigate`/`patch` to `u` goes: `'leave'` (another origin, which
 * `pushState` refuses, so it loads as a page), `'hash'` (a new fragment on this
 * page, which only scrolls) or `'server'` (an SPA frame).
 * @param {URL} u
 * @returns {'leave'|'hash'|'server'}
 */
function navRoute(u) {
    if (u.origin !== location.origin) return 'leave';
    // A bare `#` is a fragment too (an empty one, `hash` reads ''), as for the browser.
    const fragment = u.hash !== '' || u.href.endsWith('#');
    if (fragment && u.pathname === location.pathname && u.search === location.search) {
        return 'hash';
    }
    return 'server';
}

/**
 * Run a `navigate`/`patch`, from an effect or an `az-navigate`/`az-patch` click.
 * The URL resolves against the CURRENT page, as `pushState` resolves it, so the
 * frame asks the server for the page the address bar will show (`?page=2` on
 * `/users/1/` is `/users/1/?page=2`). A link to this very URL is a real
 * navigation too; only a new fragment on this page scrolls in place.
 *
 * `transition` (when given) becomes `_pendingTransition` only if a frame is
 * sent: a hash jump or a page load has no server reply to wrap, so an intent set
 * there would animate the next unrelated batch.
 * @param {string} full
 * @param {{replace?: boolean, noscroll?: boolean}} opts
 * @param {'navigate'|'patch'} kind
 * @param {typeof _pendingTransition} [transition]
 */
function navigateEffect(full, opts, kind, transition) {
    const u = new URL(full, location.href);
    const route = navRoute(u);
    if (route === 'leave') {
        leavePage(full, opts.replace);
        return;
    }
    const hash = u.hash ? u.hash.slice(1) : '';
    if (route === 'hash') {
        if (opts.replace) {
            history.replaceState(history.state, '', u.href);
        } else {
            saveCurrentScroll();
            history.pushState(null, '', u.href);
        }
        if (!opts.noscroll) applyScroll({ kind: 'push', hash });
        return;
    }
    if (transition !== undefined) _pendingTransition = transition;
    const qs = u.search ? u.search.slice(1) : '';
    navigateTo(u.pathname, qs, hash, { ...opts, fullUrl: full, kind });
}

/**
 * Post a control message to the Worker. No-op if the Worker isn't
 * spawned. The Worker is the authority on transport state -- pre-open
 * `W_SEND` messages are dropped by the Worker itself, so the main
 * thread doesn't gate on `_connected` before posting.
 * @param {number} opcode
 * @param {...*} args
 */
function workerPost(opcode, ...args) {
    if (_worker) _worker.postMessage([opcode, ...args]);
}

/**
 * Send an event to the root view. The target goes out as null and the server
 * resolves it against the root view id it already holds -- see pushEventTo.
 * @param {string} event
 * @param {*} [payload]
 */
export function pushEvent(event, payload) {
    pushEventTo(null, event, payload);
}

/**
 * Send an event to a specific view by id, or to the root view when `view` is
 * null/undefined.
 *
 * A null target is the wire value for "the root view", NOT a missing value: the
 * server maps a non-binary target to the root view id it is already tracking
 * (`arizona_socket:event_target/2`). Resolving it here from the DOM instead
 * would be a guess -- `document.querySelector('[az-view]')` returns the FIRST
 * marker in document order, which is not the root view when the page carries a
 * marker for a component that was never registered as a live view (a ?stateful
 * rendered outside the live tree). Such a guess sends a plausible-looking id the
 * server cannot recognize, and it drops the event; null is always recoverable.
 * @param {string|null|undefined} view
 * @param {string} event
 * @param {*} [payload]
 */
export function pushEventTo(view, event, payload) {
    sendEvent(event, JSON.stringify([view ?? null, event, payload]));
}

// Whether this page has already handed the shell its OS-event callback. Module
// scope, and deliberately NOT cleared by disconnect(): the shell's onEvent contract
// is a bare callback with no unregister, so a registration outlives the connection
// that made it and a second connect() would double every OS event forever.
let _osEventBound = false;

/**
 * Save form state for all forms with id attributes. Called on disconnect
 * to preserve user input across reconnections, and again just before the resync
 * replaces the view, to keep what the user typed while disconnected.
 *
 * The values are always the DOM's, so the user's typing is never lost. Only the
 * `az-change` replay is withheld: for the `suspect` forms (see `_replayedForms`),
 * and for a form already saved without it that has not been restored since.
 * @param {Set<string>|null} [suspect]
 */
export function saveFormState(suspect = null) {
    const held = new Map(_savedForms);
    _savedForms.clear();
    // Every hosting document, not just the main one: a view popped out to a PiP
    // window keeps its forms in THAT window's document, and a reconnect has to
    // preserve the user's typing there exactly as it does on the main page.
    for (const doc of allDocs()) {
        doc.querySelectorAll('form[id]').forEach((form) => {
            const data = formFields(
                new FormData(/** @type {HTMLFormElement} */ (form)),
                /** @type {HTMLFormElement} */ (form),
            );
            const azChange = form.getAttribute('az-change') || null;
            const replay = !suspect?.has(form.id) && (held.get(form.id)?.replay ?? true);
            _savedForms.set(form.id, { fields: data, azChange, replay });
        });
    }
}

/**
 * @typedef {{doc: Document, el: Element, id: string|null, form: string|null,
 *   name: string|null, nth: number, sel: [number, number, string]|null}} FocusMark
 */

/**
 * Record each hosting document's focused element before a resync replaces the
 * view: the element itself (a kept az-nodiff element survives the resync as the
 * same node), and what the fresh markup will carry again for one that is
 * rebuilt -- its id, or its form's id plus its name and position among that
 * form's same-named fields. The caret and selection ride along -- restoring
 * values moves the caret to the end.
 * @returns {FocusMark[]}
 */
function captureFocus() {
    /** @type {FocusMark[]} */
    const marks = [];
    for (const doc of allDocs()) {
        const el = doc.activeElement;
        if (!el || el === doc.body || el === doc.documentElement) continue;
        const f = /** @type {HTMLInputElement} */ (el);
        const owner = f.form;
        const form = owner?.id || null;
        const name = f.name || null;
        let nth = 0;
        if (owner && form && name) {
            nth = [...owner.elements]
                .filter((x) => /** @type {any} */ (x).name === name)
                .indexOf(el);
        }
        /** @type {[number, number, string]|null} */
        let sel = null;
        try {
            if (typeof f.selectionStart === 'number') {
                sel = [
                    f.selectionStart,
                    /** @type {number} */ (f.selectionEnd),
                    f.selectionDirection || 'none',
                ];
            }
        } catch {}
        marks.push({ doc, el, id: el.id || null, form, name, nth, sel });
    }
    return marks;
}

/**
 * Put focus (and the caret) back on the element a `captureFocus` mark names, when
 * the resync replaced it and nothing else took focus in the meantime -- a resync
 * effect that focused another element wins.
 * @param {FocusMark[]} marks
 */
function restoreFocus(marks) {
    for (const { doc, el, id, form, name, nth, sel } of marks) {
        const active = doc.activeElement;
        if (active === el) continue; // untouched: focus and caret survived
        if (active && active !== doc.body && active !== doc.documentElement) continue;
        // Duck-typed, not `instanceof`: a popped-out (PiP) view's elements belong
        // to another window's constructors.
        /** @type {any} */
        let target = null;
        if (el.isConnected) {
            // Kept (inside a kept az-nodiff element): the same node, but moving
            // it through the parsed fragment detached it, which dropped focus.
            target = el;
        } else if (id) {
            target = doc.getElementById(id);
        }
        if (!target && form && name) {
            const f = /** @type {any} */ (doc.getElementById(form));
            if (f?.elements) {
                target =
                    [...f.elements].filter((/** @type {any} */ x) => x.name === name)[nth] || null;
            }
        }
        if (typeof target?.focus !== 'function') continue;
        target.focus({ preventScroll: true });
        if (sel) {
            try {
                target.setSelectionRange(sel[0], sel[1], /** @type {any} */ (sel[2]));
            } catch {}
        }
    }
}

/**
 * Restore form state after reconnection. Sets field values on matching forms
 * and replays az-change events to sync server state, once: a replayed form is
 * tracked in `_replayedForms` until the server acknowledges the replay.
 */
export function restoreFormState() {
    _replay = { pending: 1 }; // the restore itself
    for (const [formId, { fields, azChange, replay }] of _savedForms) {
        // Saved from any hosting document, so look in all of them -- a form popped
        // out to a PiP window is not reachable from the main document.
        let form = null;
        for (const doc of allDocs()) {
            form = doc.getElementById(formId);
            if (form) break;
        }
        if (!form) continue;
        const formEl = /** @type {HTMLFormElement} */ (form);
        // A duplicate name (a repeated text input, a checkbox group) was saved as
        // an array. Value-setting fields consume it positionally, so track how many
        // values each name has handed out; checkbox groups match by value instead.
        /** @type {Object<string, number>} */
        const cursor = Object.create(null);
        /** @param {string} name @returns {string|undefined} */
        const nextValue = (name) => {
            const val = fields[name];
            if (!Array.isArray(val)) return /** @type {string} */ (val);
            const i = cursor[name] || 0;
            cursor[name] = i + 1;
            return val[i];
        };
        for (const el of formEl.elements) {
            if (el instanceof HTMLInputElement) {
                if (!el.name || el.type === 'file') continue;
                if (el.type === 'checkbox') {
                    // Check by value membership: a checkbox group saves only the
                    // checked boxes' values, so `name in fields` would tick them all.
                    const val = fields[el.name];
                    el.checked = Array.isArray(val) ? val.includes(el.value) : val === el.value;
                } else if (el.type === 'radio') {
                    el.checked = fields[el.name] === el.value;
                } else if (el.name in fields) {
                    const v = nextValue(el.name);
                    // A readonly field is server-owned -- the user cannot have
                    // typed into it -- so the resync's fresh value stands. Its
                    // saved value is still consumed above, keeping a repeated
                    // name's positions aligned. A hidden input is restored: a
                    // widget (rich text, date picker, tags) may hold the user's
                    // input there, which outweighs a stale server-owned value.
                    if (v !== undefined && !el.readOnly) el.value = v;
                }
            } else if (el instanceof HTMLSelectElement) {
                if (!el.name || !(el.name in fields)) continue;
                if (el.multiple) {
                    const val = fields[el.name];
                    const arr = Array.isArray(val) ? val : [val];
                    for (const opt of el.options) opt.selected = arr.includes(opt.value);
                } else {
                    const v = nextValue(el.name);
                    if (v !== undefined) el.value = v;
                }
            } else if (el instanceof HTMLTextAreaElement) {
                if (el.name && el.name in fields) {
                    const v = nextValue(el.name);
                    if (v !== undefined && !el.readOnly) el.value = v;
                }
            }
        }
        if (replay && azChange && _connected) {
            _replayRunning = _replay;
            try {
                executeJS(form, null, JSON.parse(azChange));
            } finally {
                _replayRunning = null;
            }
            _replayedForms.add(formId);
        }
    }
    _savedForms.clear();
    // The server handles a socket's frames in order, and an event synchronously,
    // so the pong to a ping sent after every replayed event proves they were
    // handled without crashing the view.
    if (_replayedForms.size > 0) replayPartDone(/** @type {{pending: number}} */ (_replay));
}

/**
 * Bootstrap: spawn Worker, set up document-level event delegation for all
 * supported event types, and wire up form submission and drag-and-drop.
 *
 * Returns a `disconnect` function that tears down every listener this call
 * registered and terminates the Worker. Idempotent -- calling it twice is
 * a no-op. Useful for tests that spin Arizona up and down repeatedly, and
 * for host apps that need to shut Arizona down on route change.
 *
 * The connection owns module-level state (the Worker handle, the connected flag,
 * the saved forms), so only one can be live: calling `connect` again retires the
 * previous one first. Left unretired, its Worker would keep a second socket
 * applying ops to the same document, and its `disconnect` -- reading the same
 * module state -- would terminate THIS connection's Worker instead of its own.
 *
 * @param {string} endpoint
 * @param {Object<string, unknown>} [params]
 * @returns {() => void} disconnect
 */
export function connect(endpoint, params = {}) {
    if (_teardown) {
        console.warn('[arizona] connect() called twice; disconnecting the previous connection');
        _teardown();
    }
    const controller = new AbortController();
    const signal = controller.signal;
    const prevScrollRestoration = /** @type {any} */ (history).scrollRestoration;

    // Event delegation on the main document: az-* events, form submit, and
    // drag-and-drop. (requestPip binds the same set on a PiP window's document.)
    bindDocumentEvents(document, signal);

    // Take over scroll restoration so the browser doesn't scroll to a stale
    // position before OP_REPLACE swaps in the new content. See the block
    // comment above applyScroll for the full model.
    if ('scrollRestoration' in history) history.scrollRestoration = 'manual';

    // Seed the rendered path/qs so the popstate handler can distinguish a
    // fragment-only change (scroll, no server round-trip) from a real
    // cross-page navigation.
    _currentPath = location.pathname;
    _currentQs = location.search ? location.search.slice(1) : '';
    // Resume the entry index a reload or a full-document return lands on.
    _historyIdx = typeof history.state?._azIdx === 'number' ? history.state._azIdx : 0;

    // 'manual' also switches off the browser's own restore for a full-document
    // load of an existing entry (a reload, a Back with no bfcache), so restore
    // the position `pagehide` stamped onto this entry. A new entry carries no
    // stamp and falls through to the hash (or stays at the top).
    const savedScroll = history.state?._azScroll;
    if (savedScroll) {
        requestAnimationFrame(() => applyScroll({ kind: 'pop', hash: '', saved: savedScroll }));
    } else if (location.hash) {
        // On initial load with a URL hash, honor it -- with scrollRestoration
        // set to 'manual', the browser may have skipped or raced its native
        // anchor jump, so we take care of it ourselves.
        const hash = location.hash.slice(1);
        requestAnimationFrame(() => applyScroll({ kind: 'push', hash }));
    }

    // SPA navigation: az-navigate (replace the view) or az-patch (keep the view,
    // re-render in place) -- both boolean attrs on <a>. The path is read from
    // href (hash stripped before sending). az-navigate sends ["navigate", ...]
    // and the server replies OP_REPLACE; az-patch sends ["patch", ...] and the
    // server replies a diff. Scroll resets to top (or #hash) on new nav; opt out
    // with az-noscroll.
    document.addEventListener(
        'click',
        (e) => {
            const me = /** @type {MouseEvent} */ (e);
            // Let the browser handle modifier-key and non-primary clicks (open in
            // new tab/window, etc.) so the link isn't hijacked.
            if (me.button !== 0 || me.ctrlKey || me.metaKey || me.shiftKey || me.altKey) return;
            const el = /** @type {HTMLAnchorElement} */ (
                /** @type {Element} */ (e.target).closest('[az-navigate], [az-patch]')
            );
            if (!el || !_connected) return;
            // An app listener already handled the click, or the browser is meant
            // to open the link elsewhere (`target`) or save it (`download`).
            if (e.defaultPrevented || el.hasAttribute('download')) return;
            const target = el.getAttribute('target');
            if (target && target.toLowerCase() !== '_self') return;
            const href = el.getAttribute('href');
            if (!href) return;
            // Another origin cannot be SPA-navigated (pushState refuses it, and
            // its pathname is not ours to request): the browser follows the link.
            if (el.origin !== location.origin) return;
            const isPatch = el.hasAttribute('az-patch');

            e.preventDefault();

            // az-transition opts this navigation into a view transition. A patch
            // produces a diff (no OP_REPLACE), so its intent is consumed by the
            // first response batch ('any'); a navigate waits for the OP_REPLACE.
            const t = parseTransitionAttr(el);
            // `el.href` is resolved against the document base, as the browser would.
            navigateEffect(
                el.href,
                { noscroll: el.hasAttribute('az-noscroll') },
                isPatch ? 'patch' : 'navigate',
                t ? { types: t.types, kind: isPatch ? 'any' : 'replace' } : null,
            );
        },
        { signal },
    );

    // Browser back/forward: send navigate on popstate so the server
    // renders the correct page for the current URL. Restore the saved
    // scroll position (or #hash target) after REPLACE applies.
    window.addEventListener(
        'popstate',
        (e) => {
            const path = location.pathname;
            const qs = location.search ? location.search.slice(1) : '';
            const hash = location.hash ? location.hash.slice(1) : '';
            const saved = e.state?._azScroll || null;
            // Which edge this traversal crossed: going back, the one from the
            // destination to its next entry; going forward, the one into it from
            // its previous entry. An entry with no index has no SPA edge, and a
            // multi-step jump (`history.go(+-2)`) crossed several, so it is not
            // animated at all rather than by whichever edge sits next to the
            // destination (which would differ between the two directions).
            const idx = e.state?._azIdx;
            const hasIdx = typeof idx === 'number';
            const transition = !hasIdx
                ? null
                : idx === _historyIdx - 1
                  ? e.state._azTransitionNext
                  : idx === _historyIdx + 1
                    ? e.state._azTransitionPrev
                    : null;
            if (hasIdx) _historyIdx = idx;
            // Fragment-only change (path + query unchanged): a same-page hash
            // jump fires popstate in some browsers, but it needs no server
            // round-trip -- just scroll. Mirrors the click handler's same-page
            // fast path so in-page anchors don't trigger a full OP_REPLACE.
            // Needs no connection either, so it runs even while disconnected.
            if (path === _currentPath && qs === _currentQs) {
                applyScroll({ kind: 'pop', hash, saved });
                return;
            }
            // A cross-page popstate cannot be served over a down WebSocket, and
            // the browser has already changed the URL -- returning would desync
            // URL and content and leave the reconnect URL stale (a reconnect
            // would resync to the pre-back path, and a later Forward would look
            // fragment-only). Degrade to a full load of the URL the browser now
            // shows, like a disconnected az-navigate click falls through to the
            // browser.
            if (!_connected) {
                _unloadingToOtherPage = true;
                location.reload();
                return;
            }
            // Replay the same mode the edge was navigated with (tagged `_azNav`),
            // so back/forward over a patch re-patches rather than replacing.
            const navKind = e.state?._azNav === 'patch' ? 'patch' : 'navigate';
            setPendingScroll({ kind: 'pop', hash, saved, patch: navKind === 'patch' });
            // Replay the transition of the edge just crossed, in either
            // direction; an edge navigated without one stays instant.
            _pendingTransition = transition || null;
            sendNav(navKind, path, qs);
        },
        { signal },
    );

    // The WS URL is (re)built at every worker spawn -- the Worker can't access
    // location.*. Framework keys are `_az_`-prefixed so they can't collide with
    // user params or the page's own query string; the user-facing qs is the
    // tracked page qs plus the connect() extras (server reaches them via
    // arizona_req:params/1). Building from `_currentPath`/`_currentQs` rather
    // than a connect-time snapshot matters for the bfcache respawn: an SPA
    // navigation moves the path only inside the worker (W_UPDATE_PATH), the
    // worker dies on pagehide, and the respawned one must target the route the
    // restored page actually shows.
    /** @type {Record<string, string>} */
    const stringParams = {};
    for (const k of Object.keys(params)) stringParams[k] = String(params[k]);
    _connectQs = new URLSearchParams(stringParams).toString();
    const buildWsUrl = () => {
        const protocol = location.protocol === 'https:' ? 'wss:' : 'ws:';
        const pagePath = encodeURIComponent(_currentPath);
        const userQs = reconnectUserQs(_currentQs);
        const qs = userQs ? `_az_path=${pagePath}&${userQs}` : `_az_path=${pagePath}`;
        // Native-shell capabilities (Electron/Tauri/...) advertised at the WS
        // handshake so the live process can answer ?capability(...). Absent in a
        // plain browser (no `__arizona_os__`), so capsQs is empty and nothing
        // changes. Rides reconnect because the Worker reuses this URL's search.
        const osCaps = osHost()?.capabilities;
        const capsQs = osCaps ? `&_az_caps=${encodeURIComponent(JSON.stringify(osCaps))}` : '';
        return `${protocol}//${location.host}${endpoint}?${qs}${capsQs}`;
    };

    /** @type {Function|null} */
    let _onmessageHook = null;

    // Spawn the Worker, wire its message handler, and open the socket. Extracted
    // so a bfcache restore (`pageshow`) can re-establish the connection that
    // `pagehide` tore down. `reconnect` flags that spawn: the restored page's
    // DOM already exists with the live state it had at pagehide, so the worker
    // must open with `_az_reconnect=1` -- only that makes the server send the
    // full-page resync (and the first frame restore any saved form state).
    /** @param {boolean} reconnect */
    const spawnWorker = (reconnect) => {
        // Worker is co-located with this script. This static
        // `new Worker(new URL(..., import.meta.url), { type: 'module' })` shape is
        // what bundlers (Vite/Rollup/rolldown) statically detect: Arizona's own
        // build emits the sibling `arizona-worker.min.js` next to
        // `arizona.min.js` and rewrites this reference to it, and a consumer that
        // re-bundles the built client gets the worker auto-emitted,
        // content-hashed, and its URL rewritten (no runtime-string 404).
        _worker = new Worker(new URL('./arizona-worker.js', import.meta.url), { type: 'module' });

        // A Worker that fails to load or throws at top level never posts [1, ...],
        // so without this the page just silently never connects.
        _worker.onerror = (e) => {
            console.error('[arizona] worker error:', e.message || e);
        };

        _worker.onmessage = (e) => {
            const msg = e.data;
            switch (msg[0]) {
                case 0: {
                    // [0, ops|null, effects|null, firstAfterReconnect, azAttrs|null]
                    // A navigate's reply is its new view's OP_REPLACE, which proves
                    // that mount too. Only the first proof on a socket is a
                    // connection event: a later one is a navigate on a live socket.
                    let mounted = false;
                    const closes = _closeCount;
                    if (
                        _mountProof === 'frame' ||
                        (_mountProof === 'resync' && msg[3]) ||
                        msg[1]?.some((/** @type {Array<*>} */ op) => op[0] === OP.REPLACE)
                    ) {
                        mounted = _mountProof !== null;
                        _mountProof = null;
                        mountSucceeded();
                    }
                    // The app's `az-*` vocabulary, collected by the parse transform
                    // and sent on the connect frame. Delegated BEFORE the ops land,
                    // so a listener exists the moment an element declaring one does.
                    if (msg[4]) noteAzAttrs(msg[4]);
                    const apply = () => {
                        // A patch-scroll intent lives exactly until the first frame
                        // after the patch request. applyOps consumes it when the
                        // frame carries ops (the patch reply diff -- scroll applies);
                        // otherwise it is cleared here, so a no-op patch (the server
                        // sends nothing back) can't leave the intent armed for an
                        // unrelated later diff to yank the scroll. Identity-checked
                        // so an intent armed by THIS frame's effects (a JS_PATCH
                        // command) survives to its own reply. Residual race: for a
                        // truly silent patch the next frame -- whatever it is -- is
                        // indistinguishable from a slow patch reply, so one
                        // unrelated ops-frame can still scroll; there is no reply
                        // id on the wire to do better client-side.
                        const armedScroll = _pendingScroll;
                        // Here, not on arrival: a transition defers this, and the
                        // old DOM stays live (and gated) until the swap is applied.
                        settleNavReplies(msg[1], msg[2], !!msg[3]);
                        // Re-save what was typed while disconnected: the resync's
                        // OP_REPLACE is about to discard it.
                        if (msg[3] && _savedForms.size > 0) saveFormState();
                        // ...and so is the element being typed into: put focus
                        // and caret back once the values are restored.
                        const focus = msg[3] ? captureFocus() : null;
                        if (msg[1]) applyOps(msg[1], !!msg[3]);
                        if (msg[2]) applyEffects(msg[2]);
                        if (msg[3]) restoreFormState();
                        if (focus) restoreFocus(focus);
                        if (armedScroll?.patch && _pendingScroll === armedScroll) {
                            setPendingScroll(null);
                        }
                        // Here, not after the dispatch below: a transition defers
                        // this, and a listener must see the remounted view.
                        if (mounted) announceConnected(closes);
                    };
                    // A pending transition wraps its batch -- ops and effects
                    // together, in order, so the swap and any effect fall inside
                    // one snapshot. 'replace' (navigate) waits for the page-swap
                    // batch, ignoring stray ticks; 'any' (push_event) takes its
                    // first response message and is then consumed either way, so a
                    // no-diff event can't leave the intent dangling onto a later one.
                    const pt = _pendingTransition;
                    const wrap = pt && opsMatchTransition(msg[1], pt.kind);
                    if (pt && (wrap || pt.kind === 'any')) _pendingTransition = null;
                    if (wrap) {
                        runTransition({ types: pt.types }, apply);
                    } else {
                        apply();
                    }
                    if (_onmessageHook) {
                        _onmessageHook({
                            data: JSON.stringify({
                                ...(msg[1] ? { o: msg[1] } : {}),
                                ...(msg[2] ? { e: msg[2] } : {}),
                            }),
                        });
                    }
                    break;
                }
                case 1: {
                    // [1, isReconnect]
                    _connected = true;
                    _mountProof = msg[1] ? 'resync' : 'frame';
                    document.documentElement.classList.add('az-connected');
                    document.documentElement.classList.remove('az-disconnected');
                    if (!msg[1]) {
                        mountHooks(document);
                    }
                    break;
                }
                case 2: {
                    // [2, closeCode]
                    const code = msg[1];
                    _closeCount++;
                    _connected = false;
                    _mountProof = null;
                    // The replies still owed died with the socket; the reconnect's
                    // resync is what replaces the page now.
                    if (_navInFlight > 0) {
                        _navInFlight = 0;
                        _navAwaitResync = true;
                    }
                    document.documentElement.classList.add('az-disconnected');
                    document.documentElement.classList.remove('az-connected');
                    // A crash after the mount needs no reload: the worker's
                    // reconnect remounts the view in place.
                    const reloadInMs =
                        code === WS_CLOSE_MOUNT_FAILED ? scheduleMountReload() : null;
                    // A crash before the server acknowledged the last replay may
                    // come from the replayed values: keep them, but restore them
                    // next time without replaying.
                    if (code !== WS_CLOSE_NORMAL) {
                        saveFormState(code === WS_CLOSE_CRASH ? _replayedForms : null);
                    }
                    _replayedForms.clear();
                    _replay = null;
                    // A 1000 close is a deliberate end, which the worker does not
                    // reconnect after. Once per outage, since the worker's failed
                    // reconnect attempts each report a close too -- and again when
                    // this close scheduled a reload, the app's cue to save state.
                    if (code !== WS_CLOSE_NORMAL && (!_outage || reloadInMs !== null)) {
                        dispatchConnection('arizona:disconnected', { code, reloadInMs });
                    }
                    break;
                }
                case 3:
                    // [3] -- the server handled the form replay (see W_ACK_REPLAY).
                    _replayedForms.clear();
                    break;
            }
        };

        // Send connect message to Worker, rebuilding the URL from the
        // currently tracked route (see buildWsUrl).
        workerPost(W_CONNECT, buildWsUrl(), reconnect);
    };

    spawnWorker(false);

    // Let the native shell (if any) inject OS events (window focus/blur, capture
    // state, ...) into the ROOT view's normal event handling. The shell calls
    // cb(name, payload); we relay it as an ordinary pushEvent so the view's
    // handle_event/3 sees it like any other event. Registered ONCE PER PAGE -- not
    // per spawnWorker, bfcache restore, or connect(): the shell's listener is
    // persistent and has no unregister, so disconnect() cannot take it back and a
    // second registration would deliver every OS event twice. The callback relays
    // through module state, so it keeps addressing the CURRENT connection. A bare
    // callback is the documented contextBridge/Tauri-listen form (an object of
    // functions can't cross contextBridge).
    if (!_osEventBound) {
        _osEventBound = true;
        osHost()?.onEvent?.((name, payload) => pushEvent(name, payload));
    }

    // window._ws proxy for E2E test compatibility
    if (typeof window !== 'undefined') {
        /** @type {any} */ (window)._ws = {
            get readyState() {
                return _connected ? 1 : 3;
            },
            /** @param {string} data */ send(data) {
                workerPost(W_SEND, data);
            },
            /** @param {number} [code] */ close(code) {
                workerPost(W_CLOSE, code || WS_CLOSE_NORMAL);
            },
            set onmessage(fn) {
                _onmessageHook = fn;
            },
            get onmessage() {
                return _onmessageHook;
            },
        };
    }

    // Stamp the scroll onto the entry being left, so a full-document return to
    // it (reload, Back without bfcache) can restore it at connect -- 'manual'
    // scrollRestoration stops the browser doing it. One write per unload, not
    // per scroll. Both events: Chromium snapshots a reloading entry's state
    // before `pagehide` runs, so only a `beforeunload` write survives a reload,
    // while `pagehide` is the one that fires on every unload (iOS Safari skips
    // `beforeunload`). Neither listener kept a page out of Chromium's bfcache
    // (checked with its own eligibility report); other engines unverified. A
    // throw (Safari's history-write rate limit) only loses the restore, so it
    // is swallowed rather than allowed to abort the `pagehide` teardown.
    const stampUnloadScroll = () => {
        if (_unloadingToOtherPage) return;
        try {
            const st = history.state || {};
            history.replaceState(
                { ...st, _azScroll: { x: window.scrollX, y: window.scrollY } },
                '',
            );
        } catch {}
    };
    window.addEventListener('beforeunload', stampUnloadScroll, { signal });

    // Back/forward cache: a live WebSocket makes the page ineligible for the
    // bfcache, so tear the worker (and its socket) down when the page is hidden,
    // then re-establish it if the page is later restored from the cache. The
    // close must be synchronous -- by the time `pagehide` fires the socket is
    // still open (precisely why the page would otherwise be excluded), and an
    // async `workerPost(W_CLOSE)` may not run before the page is frozen.
    window.addEventListener(
        'pagehide',
        () => {
            stampUnloadScroll();
            _unloadingToOtherPage = false;
            if (_worker) {
                // Preserve typed form fields first: the pageshow reconnect asks
                // the server for a full resync, whose OP_REPLACE rebuilds the
                // DOM bfcache had preserved -- the same save/restore that
                // covers an abnormal close covers this.
                saveFormState();
                _worker.terminate();
                _worker = null;
                _connected = false;
                _mountProof = null;
                // Same as a close: the owed navigate replies die with the socket.
                if (_navInFlight > 0) {
                    _navInFlight = 0;
                    _navAwaitResync = true;
                }
                _replayedForms.clear();
                _replay = null;
                cancelMountReload();
                // Reflect the dropped connection so a bfcache-restored snapshot
                // shows disconnected until `pageshow` reconnects (the worker is
                // killed abruptly, so no [2, closeCode] arrives to do this).
                document.documentElement.classList.remove('az-connected');
                document.documentElement.classList.add('az-disconnected');
            }
        },
        { signal },
    );
    window.addEventListener(
        'pageshow',
        (e) => {
            // Only on a real bfcache restore; a normal load already has a
            // worker. A restore is semantically a reconnect: the DOM exists
            // with evolved live state, so the server must resync it.
            if (e.persisted && !_worker) spawnWorker(true);
        },
        { signal },
    );

    let disconnected = false;
    _teardown = function disconnect() {
        if (disconnected) return;
        disconnected = true;
        _teardown = null;
        controller.abort();
        if (_worker) {
            _worker.terminate();
            _worker = null;
        }
        _connected = false;
        _mountProof = null;
        _outage = false;
        _replayedForms.clear();
        _replay = null;
        cancelMountReload();
        setPendingScroll(null);
        _unloadingToOtherPage = false;
        _pendingTransition = null;
        _navInFlight = 0;
        _navAwaitResync = false;
        _connectQs = '';
        _savedForms.clear();
        closeAllPip();
        unbindDocuments();
        destroyAllHooks();
        // Debounce/throttle timers live in a WeakMap keyed by element and aren't
        // iterable, so they are left to expire: each captured the epoch this
        // bump retires and drops its send, which would otherwise reach the NEXT
        // connection's worker carrying this one's view id.
        _navEpoch++;
        document.documentElement.classList.remove('az-connected');
        document.documentElement.classList.remove('az-disconnected');
        if ('scrollRestoration' in history) {
            /** @type {any} */ (history).scrollRestoration = prevScrollRestoration;
        }
        if (typeof window !== 'undefined') {
            delete (/** @type {any} */ (window)._ws);
        }
    };
    return _teardown;
}

// The shared runtime (delegation, commands, hooks) reaches the socket and the
// history through `host`; this module is what serves a live page.
setHost({
    ready: () => _connected,
    epoch: () => _navEpoch,
    sendEvent,
    awaitsReply: (url) => navRoute(new URL(url, location.href)) === 'server',
    armTransition: (t) => {
        _pendingTransition = t;
    },
    holdReplay,
    navigate: navigateEffect,
    leavePage,
});
