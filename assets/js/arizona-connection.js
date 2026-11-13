import { applyChoosers } from './arizona-attrs.js';
import {
    actedOn,
    bindControls,
    edited,
    formControls,
    formOf,
    heldSelect,
    markTouched,
    ownedControl,
    propose,
    proposed,
    resetRestored,
    serverBatch,
    serverChoice,
    settle,
    showDefaults,
    standingPick,
    sweepKept,
    userEdited,
    withStandIns,
} from './arizona-bound.js';
import { applyEffects, executeJS, osHost, releaseParked, sendsValues } from './arizona-commands.js';
import { allDocs, closeAllPip, findViewRoot } from './arizona-docs.js';
import { bindDocumentEvents, noteAzAttrs, unbindDocuments } from './arizona-events.js';
import { destroyAllHooks, mountHooks } from './arizona-hooks.js';
import { setHost } from './arizona-host.js';
import { applyOps, OP, opsMatchTransition } from './arizona-ops.js';
import { _pendingScroll, applyScroll, setPendingScroll } from './arizona-scroll.js';
import { parseTransitionAttr, runTransition } from './arizona-transitions.js';
import { isWebUrl, loadPage, warnOnce } from './arizona-util.js';

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

// The reconnect restore keeps what the user typed across a resync that rebuilds the
// DOM, by four rules (see `.claude/rules/js.md`):
// 1. A unit is a control, or a named radio group of one form owner: what one
//    proposal captures. The server owns a unit when any of it is (`ownedControl`).
// 2. A saved unit pairs with a fresh one of its kind: a text or select by name, then
//    stable id, else position (`matchControls`); a box by name, kind, value and
//    occurrence (a radio group through its checked radio).
// 3. A saved unit holds the user's state when it was enabled at the drop, and
//    unbound, or server-owned and edited since the last reconcile.
// 4. A fresh unit takes it when it can show it exactly, and is unbound, or enabled,
//    not withheld and proposed by a replay. Otherwise the unit shows the resync's.

/**
 * A named control as saved: its `id` when that names it alone among its scope's
 * controls, its `kind` (see `kindOf`), a box's value (`box`, else null) and
 * checkedness (`on`), the `value` of a control restored by value (a select: its
 * chosen values), whether the unit holds the user's state (`user`, rule 3), whether
 * that is an edit of a server-owned unit (`mine`), when the user last acted on the
 * unit (`at`, see `actedOn`), and whether its replay may run (`replay`: a crash did not make
 * it suspect).
 * @typedef {{id: string|null, kind: string, box: string|null, on: boolean,
 *   value: string|string[]|undefined, user: boolean, mine: boolean, at: number,
 *   replay: boolean}} SavedControl
 */

/**
 * Typed form fields saved on an abnormal close, restored by the next resync, by
 * `formKey`: the controls of a form by the view holding them (a view is its id), so
 * a form's controls in one view never take the state of another view's, whatever
 * form owns them. `controls` holds each name's controls in order (see
 * `matchControls`); `replay` is false when a crash withholds the replay of a form
 * owning any of them.
 * @type {Map<string, {view: string, id: string, controls: Map<string, SavedControl[]>,
 *   replay: boolean}>}
 */
const _savedForms = new Map();

/**
 * The `_savedForms` key of the controls in the view of id `view` (`''` outside every
 * view) of a form of id `id`, and the `_replayedForms` key of a form, by its own view
 * and id: a form is withheld as a whole, the controls naming it from another view
 * (`form=`) too. An id holds no U+0000, so it holds one and a formless key
 * (`unitKey`) two.
 * @param {string} view
 * @param {string} id
 */
const formKey = (view, id) => `${view}\0${id}`;

/** The `_replayedForms` key of form `form` (see `formKey`). @param {Element} form */
const formKeyOf = (form) => formKey(viewOf(form), form.id);

/**
 * A saved formless unit: its `controls` under pairing name `name` in the view of id
 * `view` (see `viewOf`).
 * @typedef {{view: string, name: string, controls: SavedControl[]}} FormlessUnit
 */

/**
 * Server-owned controls outside every form, saved by the view holding them (a view
 * is its id: a remount under another id is another view), each control by its id
 * (`fieldKey`), each named radio group by its name -- a group spanning views in
 * parts, one per view. A unit is restored only into the fresh view of its view's id,
 * wherever that is now; a view gone, its state goes nowhere.
 * @type {FormlessUnit[]}
 */
let _savedFormless = [];

/**
 * What control `el` holds, as a restore puts it back: `text` (a textarea or a
 * text-like input, which share their text), `select`, `multi` (a multiple select),
 * a box (`checkbox`, `radio`), a password or an input that sanitizes its value (a
 * number, a range, a color, a date or time) by its type, or `other`. A saved state
 * is restored only into a control of its kind: a select's choice is no text, a
 * checkbox's value no choice, a password is shown by no other field, and text in a
 * range would show its midpoint.
 * @param {any} el
 * @returns {string}
 */
function kindOf(el) {
    if (el.localName === 'textarea') return 'text';
    if (el.localName === 'select') return el.multiple ? 'multi' : 'select';
    if (el.localName !== 'input') return 'other';
    const t = el.type;
    if (['file', 'submit', 'image', 'reset', 'button'].includes(t)) return 'other';
    const own = ['checkbox', 'radio', 'password', 'number', 'range', 'color'];
    return own.includes(t) || /^(date|month|week|time|datetime-local)$/.test(t) ? t : 'text';
}

/** A form control's pairing name. @param {any} el */
const formName = (el) => el.name;

/**
 * A formless control's pairing name: a named radio's group, else its id
 * (`fieldKey`), else none.
 * @param {any} el
 */
const formlessName = (el) =>
    el.type === 'radio' && el.name ? el.name : el.id ? fieldKey(el.id) : '';

/**
 * The server-owned controls of `doc` outside every form: the formless controls a
 * restore takes back.
 * @param {Document} doc
 */
const formless = (doc) =>
    [...doc.querySelectorAll('input, textarea, select')].filter(
        (/** @type {any} */ el) => !formOf(el) && ownedControl(el),
    );

/**
 * The controls of `els` by the name `nameOf` gives them, in order; nameless ones
 * are left out.
 * @param {any[]} els
 * @param {(el: any) => string} nameOf
 * @returns {Map<string, any[]>}
 */
function namedControls(els, nameOf) {
    const by = new Map();
    for (const el of els) {
        const name = nameOf(el);
        if (!name) continue;
        const list = by.get(name);
        if (list) list.push(el);
        else by.set(name, [el]);
    }
    return by;
}

/**
 * The name of the control of `els` carrying each id, or null for an id two of them
 * share.
 * @param {any[]} els
 * @param {(el: any) => string} nameOf
 * @returns {Object<string, string|null>}
 */
function idNames(els, nameOf) {
    /** @type {Object<string, string|null>} */
    const n = Object.create(null);
    for (const el of els) if (el.id) n[el.id] = el.id in n ? null : nameOf(el);
    return n;
}

/**
 * Pair each fresh control of `named` that is no box (by name; `ids` names each id's
 * control) with the saved control it takes the user's state from, and a key naming
 * that control apart from the others of its scope. Boxes are left out of the ids and
 * positions, since they pair by value: a radio added to a group keeps a text field
 * sharing its name in place. A
 * name whose saved ids each name one control of that name in the fresh scope pairs
 * by id (and its controls with no id by their position among those), so a resync
 * that inserts a control of the same name earlier keeps each edit on its control --
 * as long as its id-less controls are still the ones saved without an id: every
 * fresh id was saved, or as many controls have none as did. Any other name pairs by
 * position: an id two controls share names neither, one minted at mount changes on
 * the remount a reconnect runs, and a control that gains an id would shift the
 * id-less ones after it onto their neighbours.
 * @param {Map<string, any[]>} named
 * @param {Object<string, string|null>} ids
 * @param {Map<string, SavedControl[]>} saved
 * @returns {Map<any, [SavedControl, string]>}
 */
function matchControls(named, ids, saved) {
    const out = new Map();
    for (const [name, all] of named) {
        // Boxes pair by value (see `restoreScope`), so they take no position here.
        const els = all.filter((el) => el.type !== 'checkbox' && el.type !== 'radio');
        const was = saved.get(name)?.filter((c) => c.box === null);
        if (!was) continue;
        /** @type {Map<string, number>} */
        const at = new Map();
        /** @type {number[]} */
        const idless = [];
        was.forEach((c, i) => {
            if (c.id) at.set(c.id, i);
            else idless.push(i);
        });
        const fresh = els.filter((el) => !el.id).length;
        const byId =
            at.size > 0 &&
            (fresh === idless.length || els.every((el) => !el.id || at.has(el.id))) &&
            [...at.keys()].every((id) => ids[id] === name);
        let k = 0;
        els.forEach((el, n) => {
            let i = n;
            if (byId) i = el.id ? (at.get(el.id) ?? -1) : (idless[k++] ?? -1);
            if (i >= 0 && i < was.length) out.set(el, [was[i], `${name}\0${i}`]);
        });
    }
    return out;
}

/**
 * The options of select `el` as choices: each one's value and its occurrence among
 * the options of that value, so options sharing a value are told apart.
 * @param {any} el
 * @returns {string[]}
 */
function choices(el) {
    /** @type {Object<string, number>} */
    const n = Object.create(null);
    return [...el.options].map((o) => {
        n[o.value] = (n[o.value] ?? -1) + 1;
        return `${n[o.value]}\0${o.value}`;
    });
}

/**
 * The value control `el` is restored by (a select: its selected choices, see
 * `choices`), else undefined: a single select showing no option holds no choice. A
 * box's (restored by checkedness) is never read.
 * @param {any} el
 * @returns {string|string[]|undefined}
 */
function savedValue(el) {
    const kind = kindOf(el);
    if (kind !== 'select' && kind !== 'multi') return kind === 'other' ? undefined : el.value;
    const on = choices(el).filter((_, i) => el.options[i].selected);
    return on.length > 0 || el.multiple ? on : undefined;
}

/**
 * Select the choices `v` of select `el`, and say whether it did: a choice the resync
 * no longer offers keeps the resync's selection, since selecting the others alone
 * would show a set the user never chose -- unless `partial`: an unbound multiple
 * select keeps the choices still offered, as it does live when a diff removes one
 * of its options.
 * @param {any} el
 * @param {string[]} v
 * @param {boolean} partial
 * @returns {boolean}
 */
function selectChoices(el, v, partial) {
    const cs = choices(el);
    const offered = new Set(cs);
    if (!partial && !v.every((x) => offered.has(x))) return false;
    const want = new Set(v);
    cs.forEach((x, i) => {
        el.options[i].selected = want.has(x);
    });
    return true;
}

/**
 * Show saved control `c` in fresh control `el`, and say whether it did: only into a
 * control of its kind, and only what it can show exactly -- a readonly field keeps
 * the resync's value (the user cannot have typed into it), and so does a field that
 * would sanitize the value (a textarea's line breaks in a text input) or a select no
 * longer offering the choice, but an unbound (`partial`) multiple select takes the
 * choices still offered. A hidden input is restored: a widget (rich text, a date
 * picker, tags) may hold the user's input there, which outweighs a stale
 * server-owned value.
 * @param {any} el
 * @param {SavedControl} c
 * @param {boolean} partial
 * @returns {boolean}
 */
function show(el, c, partial) {
    if (c.kind !== kindOf(el)) return false;
    if (c.box !== null) {
        el.checked = c.on;
        return true;
    }
    const v = c.value;
    if (v === undefined) return false;
    if (Array.isArray(v)) return selectChoices(el, v, partial && el.multiple);
    // Tried on a copy: a value written and taken back would leave the field dirty,
    // no longer showing a default the server writes.
    const probe = el.cloneNode();
    probe.value = v;
    if (el.readOnly || probe.value !== v) return false;
    el.value = v;
    return true;
}

/**
 * The saved scope of fresh controls `els` named by `nameOf`: each name's controls in
 * order, each with what rule 3 makes of it. A radio holds its group's ownership and
 * edit, since the server owns a group as one unit. `replay` says whether a control's
 * replay may run.
 * @param {any[]} els
 * @param {(el: any) => string} nameOf
 * @param {(group: any[]) => boolean} replay
 * @returns {Map<string, SavedControl[]>}
 */
function saveScope(els, nameOf, replay) {
    const ids = idNames(els, nameOf);
    /** @type {Map<string, SavedControl[]>} */
    const out = new Map();
    for (const [name, list] of namedControls(els, nameOf)) {
        // An id that names two controls of a name names neither, so the whole name
        // is saved by position.
        const shared = list.some((el) => el.id && ids[el.id] === null);
        const radios = list.filter((el) => el.type === 'radio');
        // The group's ownership and edit, read once: it is every radio's.
        const group = radios.length > 0 && ownedControl(radios[0]);
        // A server-owned part's pick, when it still stands: what it holds, whatever
        // the group shows since (a pop-back, a move), as the user's edit. An unbound
        // part holds what it shows: code may move it after the user's pick.
        const pick = radios.length > 0 ? standingPick(radios) : null;
        const groupMine = group && !!pick;
        // When the user last chose a radio of this part of the group.
        const groupAt = Math.max(0, ...radios.map(actedOn));
        const groupReplay = replay(radios);
        // The server's own choice, which a part holding it holds as none of the user's.
        const theirs = group ? serverChoice(radios) : null;
        out.set(
            name,
            list.map((el) => {
                const radio = el.type === 'radio';
                const owns = radio ? group : ownedControl(el);
                const mine =
                    !el.matches(':disabled') && (radio ? groupMine : owns && userEdited(el));
                const box = radio || el.type === 'checkbox';
                return {
                    id: (!shared && el.id) || null,
                    kind: kindOf(el),
                    box: box ? el.value : null,
                    on: radio && group && pick ? el === pick : el.checked,
                    value: box ? undefined : savedValue(el),
                    user: !el.matches(':disabled') && (!owns || mine) && el !== theirs,
                    mine,
                    at: radio ? groupAt : 0,
                    replay: radio ? groupReplay : replay([el]),
                };
            }),
        );
    }
    return out;
}

/**
 * Restore saved scope `saved` into fresh controls `els` named by `nameOf`, by the
 * four rules. Returns the server-owned controls it gave the user's state, with
 * their pairing key: a replay must propose them, or they show the server's.
 * @param {any[]} els
 * @param {(el: any) => string} nameOf
 * @param {Map<string, SavedControl[]>} saved
 * @returns {Array<[any, string]>}
 */
function restoreScope(els, nameOf, saved) {
    const named = namedControls(els, nameOf);
    const match = matchControls(named, idNames(els, nameOf), saved);
    /** Each name's saved boxes, by kind and value, in order. */
    /** @type {Map<string, SavedControl[]>} */
    const boxes = new Map();
    for (const [name, cs] of saved)
        for (const c of cs) {
            if (c.box === null) continue;
            const k = `${name}\0${c.kind}\0${c.box}`;
            const list = boxes.get(k);
            if (list) list.push(c);
            else boxes.set(k, [c]);
        }
    /** @type {Array<[any, string]>} */
    const out = [];
    for (const [name, list] of named) {
        // Each fresh box's position among those of its name, kind and value: boxes
        // sharing a value (value-less checkboxes all submit "on") pair by order.
        /** @type {Map<string, number>} */
        const seen = new Map();
        /** @type {Map<any, [SavedControl|undefined, string]>} */
        const pair = new Map();
        for (const el of list) {
            if (el.type !== 'checkbox' && el.type !== 'radio') continue;
            const k = `${name}\0${el.type}\0${el.value}`;
            const n = seen.get(k) ?? 0;
            seen.set(k, n + 1);
            pair.set(el, [boxes.get(k)?.[n], `${name}\0v${el.value}\0${n}`]);
        }
        const radios = list.filter((el) => el.type === 'radio');
        const group = radios.length > 0 && ownedControl(radios[0]);
        /**
         * The fresh units of the name, each with the control the user's state goes
         * into: a radio group is one unit, restored through the radio its saved
         * checked radio pairs with.
         */
        /** @type {Array<[any, SavedControl|undefined, string]>} */
        const units = [];
        for (const el of list) {
            if (el.type === 'radio') continue;
            const [c, slot] = pair.get(el) ?? match.get(el) ?? [undefined, ''];
            units.push([el, c, slot]);
        }
        const pick = radios.find((r) => pair.get(r)?.[0]?.on);
        if (pick) units.push([pick, pair.get(pick)?.[0], name]);
        for (const [el, c, slot] of units) {
            if (!c?.user) continue;
            const owns = el.type === 'radio' ? group : ownedControl(el);
            if (owns && (el.matches(':disabled') || !c.mine || !c.replay || !_connected)) continue;
            if (!show(el, c, !owns)) continue;
            if (!owns) continue;
            // As the user made it: of two choices a later restore can show only one
            // of, it takes the later.
            markTouched(el, c.at || undefined);
            out.push([el, slot]);
        }
    }
    return out;
}

/**
 * Of the parts of radio name `name` -- each a view's saved radios of it (`cs`) and
 * that view's fresh controls (`fresh`) -- the index of the one whose choice its
 * group shows: the part the user acted on last, of those holding no choice or one a
 * radio of their view can still take, by value and order among those of that
 * value; null when no part qualifies. That part holding none (a return to the
 * server's choice), it restores nothing and the server's shows.
 * @param {Array<{cs: SavedControl[], fresh: any[]}>} parts
 * @param {string} name
 * @returns {number|null}
 */
function lastActed(parts, name) {
    const at = (/** @type {{cs: SavedControl[]}} */ p) => p.cs[0]?.at ?? 0;
    const choice = (/** @type {{cs: SavedControl[]}} */ p) =>
        p.cs.find((c) => c.kind === 'radio' && c.on && c.user);
    const placeable = (/** @type {{cs: SavedControl[], fresh: any[]}} */ p) => {
        const c = /** @type {SavedControl} */ (choice(p));
        const n = p.cs.filter((x) => x.kind === 'radio' && x.box === c.box);
        const fresh = p.fresh.filter(
            (el) => el.type === 'radio' && el.name === name && el.value === c.box,
        );
        return fresh.length > n.indexOf(c);
    };
    /** @type {number|null} */
    let last = null;
    parts.forEach((p, i) => {
        if (choice(p) && !placeable(p)) return;
        if (last === null || at(p) > at(parts[last])) last = i;
    });
    return last;
}

/**
 * The pairing name of a formless control by its id: an id cannot hold U+0000 (the
 * HTML parser replaces it), so it never reads as a radio group's name.
 * @param {string} id
 */
const fieldKey = (id) => `\0${id}`;

/**
 * The id of the view holding control `el`: its nearest `az-view`, else none.
 * @param {Element} el
 */
const viewOf = (el) => el.closest('[az-view]')?.id ?? '';

/**
 * Controls `els` by the view holding each (see `viewOf`), in order.
 * @param {any[]} els
 * @returns {Map<string, any[]>}
 */
function byView(els) {
    /** @type {Map<string, any[]>} */
    const out = new Map();
    for (const el of els) {
        const v = viewOf(el);
        const list = out.get(v);
        if (list) list.push(el);
        else out.set(v, [el]);
    }
    return out;
}

/**
 * The `_replayedForms` key of the formless unit `name` (see `formlessName`) of
 * `view`: a view's units apart from another's, whatever document holds them. It
 * starts with U+0000, which no id holds (the HTML parser replaces it), so it never
 * names a form.
 * @param {string} view
 * @param {string} name
 */
const unitKey = (view, name) => `\0${view}\0${name}`;

/**
 * The `_replayedForms` keys of formless unit `els` (a control, or a radio group's
 * radios in one document), one per view holding them: the same function of the unit
 * at the replay, at the save a crash makes suspect, and at the restore that withholds
 * it, so a group spanning views, or split across documents by a pop-out after it
 * replayed, is withheld whole.
 * @param {Element[]} els
 */
const unitKeys = (els) => [...new Set(els.map((el) => unitKey(viewOf(el), formlessName(el))))];

/**
 * Forms whose `az-change` the last restore replayed, until the server proves it
 * handled the replay (see W_ACK_REPLAY). A crash (4500) before that may have been
 * caused by the replayed values, so the next restore puts them back without
 * replaying them.
 * @type {Set<string>}
 */
const _replayedForms = new Set();

/**
 * The `_replayedForms` keys a crash made suspect, withheld from replay until the
 * next restore. Kept apart from what is saved: a field holding the server's value
 * at the crash saves nothing, yet the user may edit it before the resync.
 * @type {Set<string>}
 */
const _withheld = new Set();

/**
 * Whether a save waits on the next restore: the resync saves again then, to keep
 * what was typed while disconnected, also when the drop found nothing to keep.
 */
let _unrestored = false;

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
 * next entry (`_azTransitionNext`: the view transition the navigation runs under,
 * or none -- a plain navigation overwrites an older transitioned edge from this
 * entry) and
 * the entry's position (`_azIdx`), which popstate needs to tell which edge a
 * traversal crossed -- one replaceState per outgoing entry, not one per concern
 * (Safari rate-limits history writes to ~100/30s, then throws SecurityError).
 * @param {string} [navKind]
 * @param {typeof _pendingTransition} [own] the navigation's own view transition
 */
function saveCurrentScroll(navKind, own = null) {
    const { _azTransitionNext: _, ...st } = history.state || {};
    const next = {
        ...st,
        _azScroll: { x: window.scrollX, y: window.scrollY },
        _azIdx: _historyIdx,
    };
    if (navKind === 'patch') next._azNav = 'patch';
    if (own) next._azTransitionNext = own;
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
// 'any' (the reply to the push_event or patch it wraps, `_transitionRef`). Set by
// the az-transition attribute, an arizona_js:transition command, or replayed from
// history state on back/forward. Synchronous effects never set it -- they wrap in
// place.

/** @type {{types?: string[], kind: 'replace'|'any'}|null} */
let _pendingTransition = null;

// The reply ref of the frame an 'any' transition waits for, bound when that frame
// is sent (`null` until then). The reply is the frame carrying it back: a server
// push landing first is not wrapped, and a reply that changes nothing still
// answers, so the intent never outlives the frame it was armed for. A frame is
// tagged only here, so every other event goes out without a ref.
/** @type {number|null} */
let _transitionRef = null;

/**
 * Arm (or, with `null`, drop) the pending view transition; the frame it waits
 * for is bound when it is sent.
 * @param {typeof _pendingTransition} t
 */
function armPendingTransition(t) {
    _pendingTransition = t;
    _transitionRef = null;
}

// Frames that arrived while an earlier frame's apply waits in a transition's
// update callback, which the browser runs later. Each frame is a delta against
// the one before, so applying one early leaves the older frame to land last and
// overwrite it, with nothing to show the page is stale. `null` when no apply is
// deferred; `disconnect()` drops it, so a torn-down connection's frames neither
// apply nor hold the next connection's.
/** @type {Array<() => void>|null} */
let _heldFrames = null;

// Bumped by `disconnect()`. A held frame can tear the connection down while the
// held frames are being applied (an effect whose listener reconnects), and the
// frames after it belong to the torn-down connection.
let _frameGen = 0;

/**
 * Apply a server frame in arrival order, wrapped in a view transition when
 * `transition` is set.
 * @param {() => void} apply
 * @param {{types?: string[]}|null} transition
 */
function applyFrame(apply, transition) {
    if (_heldFrames) {
        _heldFrames.push(() => applyFrame(apply, transition));
        return;
    }
    if (!transition) {
        apply();
        return;
    }
    /** @type {Array<() => void>} */
    const held = [];
    _heldFrames = held;
    runTransition(transition, () => {
        if (_heldFrames !== held) return;
        // Each frame on its own: once `_heldFrames` is cleared, a throw would
        // drop every frame after it with nothing left to apply them. The first
        // error is rethrown after the rest have run.
        /** @type {unknown[]} */
        const errors = [];
        const gen = _frameGen;
        try {
            apply();
        } catch (err) {
            errors.push(err);
        }
        _heldFrames = null;
        for (const f of held) {
            if (_frameGen !== gen) break;
            try {
                f();
            } catch (err) {
                errors.push(err);
            }
        }
        if (errors.length) throw errors[0];
    });
}

// Reply refs, unique per page: a frame tagged with one is answered by exactly one
// frame carrying it back (`"r"`), even when it changes nothing -- the protocol
// otherwise sends nothing for such a frame, which no client can tell from a slow
// reply. Never reused, so a reply owed to a socket that closed cannot settle a
// frame the next socket was sent.
let _nextRef = 1;

function nextRef() {
    return _nextRef++;
}

// The page-swap window of a navigate: from posting the `navigate` frame until its
// reply lands. The server replaces the root view when it handles the frame, but
// the old DOM stays on screen until the `OP_REPLACE` arrives, and an event sent
// from it in between is dispatched by view id alone -- to the NEW root, which
// often shares the id (`page`) and then either crashes on an event it does not
// know or runs the old page's intent. The server cannot tell such an event from
// one sent after the swap (frames carry no page epoch), so the client drops it,
// exactly as a full page load would.
//
// Kept by reply ref, not counted: navigates can be in flight back to back, and the
// window must stay open until the LAST one is answered. Every navigate frame
// carries a ref, which the server answers with exactly one frame -- its
// `OP_REPLACE`, or a navigate effect (a middleware redirect, which re-arms through
// `sendNav`, or a full-page load, where the server kept the old view) -- so only
// that frame settles it (`settleNavReplies`). A navigate effect in an old-page push
// (a timer redirect) and the `OP_REPLACE` of a patch the server degraded to a
// replace answer no navigate ref, so neither closes the window early. A close loses
// every reply still owed: the window moves to `_navAwaitResync`, which the
// reconnect's resync `OP_REPLACE` clears.
//
// There is no timeout escape hatch, on purpose: the server answers every ref'd
// frame, and a crash closes the socket, which hands over to the resync. A patch
// does not arm: it usually keeps the view, and the client cannot know ahead that
// the server will degrade it.
/** @type {Set<number>} */
const _navRefs = new Set();

let _navAwaitResync = false;

// The server-owned controls each unanswered proposal frame captured, by its reply
// ref. Settled when the reply is applied; a close drops them, since no reply comes.
/** @type {Map<number, Array<*>>} */
const _owed = new Map();

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
 *
 * `transition` is the view transition the event was raised under: when it is
 * the pending 'any' one, the frame is tagged with the reply ref it waits for. A
 * dropped event takes that transition with it, and so does one raised while
 * disconnected (the worker drops it, so no reply comes), so the intent cannot
 * settle onto a later, unrelated reply. A 'replace' transition waits for a
 * navigate, not for an event raised under it, so neither touches it.
 *
 * `caps` are the server-owned controls the event proposes values for: its frame
 * waits on a reply too (sharing the transition's ref), and a dropped one lets them go.
 * @param {string} name event name, for the warning
 * @param {Array<*>} frame `[target, event, payload]`
 * @param {number} [epoch] `_navEpoch` when the event was raised
 * @param {typeof _pendingTransition} [transition]
 * @param {Array<*>|null} [caps]
 */
function sendEvent(name, frame, epoch = _navEpoch, transition = null, caps = null) {
    const waits =
        transition !== null &&
        transition === _pendingTransition &&
        transition.kind === 'any' &&
        _transitionRef === null;
    if (_navRefs.size > 0 || _navAwaitResync || epoch !== _navEpoch) {
        console.warn(`[arizona] dropped stale event "${name}"`);
        if (waits) armPendingTransition(null);
        if (caps) settle(caps, false);
        return;
    }
    let ref = null;
    if (waits && _connected) ref = _transitionRef = nextRef();
    else if (waits) armPendingTransition(null);
    // Disconnected, the worker drops the frame: no reply will come.
    if (caps && _connected) {
        ref ??= nextRef();
        _owed.set(ref, caps);
    } else if (caps) {
        settle(caps, false);
    }
    if (ref !== null) frame = [...frame, ref];
    workerPost(W_SEND, JSON.stringify(frame, wellFormed));
}

/**
 * JSON.stringify escapes a lone surrogate (a field a script set, say) as `\ud800`,
 * which the server cannot decode: it drops the frame, ref and all, so a reply that
 * proposals and transitions wait on never comes. U+FFFD is what UTF-8 can carry.
 * @param {string} _key
 * @param {unknown} v
 */
function wellFormed(_key, v) {
    return typeof v === 'string' && /** @type {any} */ (v).isWellFormed?.() === false
        ? /** @type {any} */ (v).toWellFormed()
        : v;
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
 * `own` is the view transition this navigation was issued under (`null` for
 * none): the one it stamps onto history and the one a patch binds to its reply.
 * Any other pending transition is not this navigation's -- a debounced
 * `transition(push_event)` still waiting for its event, say -- so it is never
 * stamped onto this edge, a patch leaves it waiting (the event still goes out on
 * this page), and a navigate drops it (the page it belongs to is going away, and
 * its event with it).
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
 * The navigation's own transition (`own`, from the az-transition attribute or a
 * transition command): on a push it is stamped onto both ends of the new
 * edge -- the outgoing entry's `_azTransitionNext`, the new entry's
 * `_azTransitionPrev` -- so popstate replays it only when traversing this edge.
 * A replace creates no edge: it keeps the replaced entry's stamps.
 * @param {typeof _pendingTransition} [own]
 */
function navigateTo(path, qs, hash, opts, own = null) {
    const kind = opts.kind === 'patch' ? 'patch' : 'navigate';
    const ref = nextRef();
    if (kind === 'navigate' && _pendingTransition?.kind === 'any' && _pendingTransition !== own) {
        armPendingTransition(null);
    }
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
        saveCurrentScroll(kind, own);
        _historyIdx += 1;
        navState._azIdx = _historyIdx;
        if (own) navState._azTransitionPrev = own;
        history.pushState(navState, '', fullUrl);
        if (!opts.noscroll) {
            setPendingScroll({ kind: 'push', hash, patch: kind === 'patch', ref });
        }
    }
    sendNav(kind, path, qs, ref, own);
}

/**
 * Settle the navigate a server frame answers, by the reply ref it carries. The
 * first frame after a reconnect is the resync, which clears the post-close wait
 * instead. Called before the frame is applied, so its own effects -- a redirect
 * re-arming, the new page's push_event -- see the window settled.
 * @param {number|null} ref
 * @param {boolean} resync
 */
function settleNavReplies(ref, resync) {
    if (resync) {
        _navAwaitResync = false;
        return;
    }
    if (ref !== null) _navRefs.delete(ref);
}

/**
 * A socket that went away owes no more replies. An open page-swap window waits
 * for the reconnect's resync instead, and a transition waiting for a reply no
 * longer comes would otherwise sit armed until something re-armed it.
 */
function dropOwedReplies() {
    for (const caps of _owed.values()) settle(caps, false);
    _owed.clear();
    if (_navRefs.size > 0) {
        _navRefs.clear();
        _navAwaitResync = true;
    }
    if (_transitionRef !== null) armPendingTransition(null);
}

/**
 * Post a navigate/patch frame, tagged with its reply ref, and retarget the
 * reconnect URL. A navigate arms the page-swap window (`_navRefs`) -- only on a
 * live socket: a frame posted while disconnected is dropped by the worker and gets
 * no reply, and the reconnect's resync is what puts the new page on screen. A
 * patch issued under the pending 'any' transition (`own`) is the frame that
 * transition waits for -- on a live socket; a dropped frame takes it with it.
 * @param {'navigate'|'patch'} kind
 * @param {string} path
 * @param {string} qs
 * @param {number} [ref]
 * @param {typeof _pendingTransition} [own]
 */
function sendNav(kind, path, qs, ref = nextRef(), own = null) {
    if (kind === 'navigate' && _connected) {
        _navRefs.add(ref);
        _navEpoch++;
    }
    if (
        own !== null &&
        own === _pendingTransition &&
        own.kind === 'any' &&
        _transitionRef === null
    ) {
        if (kind === 'patch' && _connected) _transitionRef = ref;
        else armPendingTransition(null);
    }
    workerPost(W_SEND, JSON.stringify([kind, { path, qs }, ref]));
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
 * `transition` (when given) is the view transition this navigation runs under,
 * `null` for none. It becomes `_pendingTransition` only if a frame is sent: a
 * hash jump or a page load has no server reply to wrap, so an intent set there
 * would animate the next unrelated batch. Re-giving the pending one (a
 * transition command's own navigate) keeps it as armed.
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
    if (transition !== undefined && transition !== _pendingTransition) {
        armPendingTransition(transition);
    }
    const qs = u.search ? u.search.slice(1) : '';
    navigateTo(u.pathname, qs, hash, { ...opts, fullUrl: full, kind }, transition ?? null);
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
    sendEvent(event, [view ?? null, event, payload]);
}

// Whether this document's restored form state was already reset (see `connect`).
let _restoreChecked = false;

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
 * The values are always the DOM's. Only the `az-change` replay is withheld: for
 * the `suspect` forms and fields (see `_replayedForms`) until the next restore,
 * whether or not this save finds anything of them to keep. A withheld replay
 * proposes nothing, so the restore keeps what the user typed except in
 * server-owned (`az-bound`) fields, which show the resync's value, the server's.
 * @param {Set<string>|null} [suspect]
 */
export function saveFormState(suspect = null) {
    if (suspect) for (const key of suspect) _withheld.add(key);
    _unrestored = true;
    _savedForms.clear();
    _savedFormless = [];
    // Every hosting document, not just the main one: a view popped out to a PiP
    // window keeps its forms in THAT window's document, and a reconnect has to
    // preserve the user's typing there exactly as it does on the main page.
    for (const doc of allDocs()) {
        // The controls of a form, by the view holding each and the form's id: what a
        // restore finds again. A form is a part of its own view even holding none.
        /** @type {Map<string, {view: string, id: string, els: any[], replay: boolean}>} */
        const parts = new Map();
        const owners = [...doc.querySelectorAll('form[id]')].map((f) => [f, null]);
        const controls = [...doc.querySelectorAll('input, textarea, select')].map((el) => [
            formOf(el),
            el,
        ]);
        for (const [form, el] of [...owners, ...controls]) {
            if (!form?.id) continue;
            const view = viewOf(el ?? form);
            const key = formKey(view, form.id);
            const part = parts.get(key) ?? {
                view,
                id: form.id,
                els: /** @type {any[]} */ ([]),
                replay: true,
            };
            parts.set(key, part);
            if (el) part.els.push(el);
            if (_withheld.has(formKeyOf(form))) part.replay = false;
        }
        for (const [key, { view, id, els, replay }] of parts)
            _savedForms.set(key, {
                view,
                id,
                controls: saveScope(els, formName, () => replay),
                replay,
            });
        // Outside every form, each view's controls apart. A unit's replay is its own
        // event's: withheld when it replayed before the crash.
        for (const [view, els] of byView(formless(doc)))
            for (const [name, controls] of saveScope(
                els,
                formlessName,
                (unit) => !unitKeys(unit).some((k) => _withheld.has(k)),
            ))
                _savedFormless.push({ view, name, controls });
    }
}

/**
 * The commands of a control's own event a replay proposes with: `az-change`, else
 * `az-input`, whichever pushes an event. Any other command would run with nothing
 * to propose (a toggle, a navigate), so a field with none is not replayed.
 * @param {Element} el
 * @returns {string|null}
 */
function ownProposal(el) {
    for (const name of ['az-change', 'az-input']) {
        const cmds = proposing(el.getAttribute(name), false);
        if (cmds) return cmds;
    }
    return null;
}

/**
 * `raw` when it is a command list that sends values (see `sendsValues`), else
 * null. A static `az-*` value is app data and need not be JSON (`az-input="lookup"`),
 * so a parse failure is an answer, not an error: a throw here would escape the
 * close handler and the resync before either finished.
 * @param {string|null} raw
 * @param {boolean} fetch
 * @returns {string|null}
 */
function proposing(raw, fetch) {
    if (!raw) return null;
    try {
        return sendsValues(JSON.parse(raw), fetch) ? raw : null;
    } catch {
        return null;
    }
}

/**
 * Run commands as part of the current replay. Contained like a delegated event
 * (`arizona-events.js`): a command list that parses can still crash the
 * interpreter, and a throw here would end the restore halfway -- the other forms
 * unrestored, the saved state unspent, and the frame's connect never announced.
 * Warned once per form or control (`key`), so one failing replay does not hide
 * another's; a delegated event instead warns once per attribute name page-wide.
 * @param {Element} el
 * @param {string} cmds
 * @param {string} key
 */
function runReplay(el, cmds, key) {
    _replayRunning = _replay;
    try {
        executeJS(el, null, JSON.parse(cmds));
    } catch (err) {
        const detail = err instanceof Error ? err.message : String(err);
        warnOnce(`[arizona] a replayed command list did not run: ${detail}`, `replay ${key}`);
    } finally {
        _replayRunning = null;
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
 *
 * A server-owned field gets what the user typed only when the user edited it (an
 * `input` or `change` since a reconcile last showed the server's value, and apart
 * from its default: a diff can move a select's default under an untouched one, and
 * an untouched one saved an older server value) and a replay proposes it: its
 * form's `az-change`, else its own event (once per field, and per radio group from
 * its checked radio). Left unproposed, it would show a value the server never had,
 * so it shows the server's. Whether a replay proposed it is read off the
 * captures it made, not off the attributes: an `az-change` that toggles or fetches
 * proposes nothing.
 */
export function restoreFormState() {
    _replay = { pending: 1 }; // the restore itself
    /**
     * Owned controls showing what the user typed, with their `_replayedForms` keys
     * and the warning key of their own replay: the form's key and the control's
     * pairing key (two controls sharing a name are two failures), else the formless
     * unit's keys (`unitKeys`), which name it alone. U+0000 cannot occur in a parsed
     * id or name, so no form or other control reads the same.
     */
    /** @type {Array<[any, string[], string]>} */
    const typed = [];
    /** Each form the restore wrote into, by its key (`formKeyOf`). */
    /** @type {Map<any, string>} */
    const replays = new Map();
    /**
     * The controls and forms under each live view root, by `formKey`, read once per
     * root however many saved parts it takes.
     * @type {Map<any, {els: Map<string, any[]>, forms: Map<string, any[]>}>}
     */
    const byRoot = new Map();
    const indexOf = (/** @type {any} */ root) => {
        let ix = byRoot.get(root);
        if (ix) return ix;
        ix = { els: new Map(), forms: new Map() };
        const add = (
            /** @type {Map<string, any[]>} */ m,
            /** @type {string} */ k,
            /** @type {any} */ x,
        ) => {
            const list = m.get(k);
            if (list) list.push(x);
            else m.set(k, [x]);
        };
        for (const el of root.querySelectorAll('input, textarea, select')) {
            const f = formOf(el);
            if (f?.id) add(ix.els, formKey(viewOf(el), f.id), el);
        }
        for (const f of [root, ...root.querySelectorAll('form')])
            if (f.localName === 'form' && f.id) add(ix.forms, formKey(viewOf(f), f.id), f);
        byRoot.set(root, ix);
        return ix;
    };
    /**
     * Each saved part found again: its fresh controls and forms.
     * @type {Array<{view: string, controls: Map<string, SavedControl[]>, replay: boolean,
     *   els: any[], forms: any[]}>}
     */
    const found = [];
    for (const [key, { view, controls, replay }] of _savedForms) {
        // Only into the controls of a form of its id in the live view of its view's
        // id, wherever that is (a popped-out view is in its window's document).
        const root = view ? findViewRoot(view) : document;
        if (!root) continue;
        const ix = indexOf(root);
        const els = ix.els.get(key) ?? [];
        // The forms of its id in that view (the view's own element too), replayed
        // whatever controls they hold now.
        const forms = ix.forms.get(key) ?? [];
        if (els.length === 0 && forms.length === 0) continue;
        found.push({ view, controls, replay, els, forms });
    }
    // A radio group of a form spanning views is restored as one, like one outside
    // every form: from the part the user acted on last (see `lastActed`), the others
    // giving up their radios of the name.
    /** @type {Map<any, Map<string, number[]>>} parts by fresh form, then radio name */
    const groups = new Map();
    found.forEach(({ controls, els }, i) => {
        const form = els.length > 0 ? formOf(els[0]) : null;
        if (!form) return;
        for (const [name, cs] of controls) {
            if (!cs.some((c) => c.kind === 'radio')) continue;
            const names = groups.get(form) ?? new Map();
            groups.set(form, names);
            names.set(name, [...(names.get(name) ?? []), i]);
        }
    });
    for (const names of groups.values())
        for (const [name, is] of names) {
            if (is.length < 2) continue;
            const radiosOf = (/** @type {number} */ i) =>
                /** @type {SavedControl[]} */ (found[i].controls.get(name)).filter(
                    (c) => c.kind === 'radio',
                );
            // No act in any part: each holds what it held at the drop (rule 4).
            if (is.every((i) => (radiosOf(i)[0]?.at ?? 0) === 0)) continue;
            const last = lastActed(
                is.map((i) => ({ cs: radiosOf(i), fresh: found[i].els })),
                name,
            );
            is.forEach((i, k) => {
                if (k === last) return;
                const f = found[i];
                f.controls = new Map(f.controls);
                f.controls.set(
                    name,
                    /** @type {SavedControl[]} */ (f.controls.get(name)).filter(
                        (c) => c.kind !== 'radio',
                    ),
                );
            });
        }
    for (const { view, controls, replay, els, forms } of found) {
        // Matched by local name, not `instanceof`: a resync re-parses a popped-out
        // (PiP) form in its window's realm, whose constructors are not ours.
        const mine = restoreScope(els, formName, controls);
        if (!replay || !_connected) continue;
        for (const form of [...forms, ...els.map(formOf)])
            if (!replays.has(form)) replays.set(form, formKeyOf(form));
        // Only what the restore wrote: a hook or widget that set an owned control
        // apart from its default when the resync mounted it is no user's edit.
        for (const [el, at] of mine) {
            if (!edited(el)) continue;
            const key = /** @type {string} */ (replays.get(formOf(el)));
            // Its warning key names its part too: two parts of a form pair apart.
            typed.push([el, [key], `${key}\0${view}\0${at}`]);
        }
    }
    // Each form's `az-change` as the resync renders it, once, replayed only when it
    // sends the fields: anything else (a toggle, a navigate) would just run its side
    // effect again.
    for (const [form, key] of replays) {
        const azChange = proposing(form.getAttribute('az-change'), true);
        if (!azChange || _withheld.has(key)) continue;
        runReplay(form, azChange, key);
        _replayedForms.add(key);
    }
    // Each saved formless unit into the fresh view of its view's id, and only there.
    for (const doc of allDocs()) {
        const els = formless(doc);
        const views = byView(els);
        // The document's radios of each name: its group.
        /** @type {Map<string, any[]>} */
        const radiosOf = new Map();
        for (const el of /** @type {any[]} */ (els))
            if (el.type === 'radio' && el.name) {
                const list = radiosOf.get(el.name);
                if (list) list.push(el);
                else radiosOf.set(el.name, [el]);
            }
        /** What each view of the document takes back, by name. */
        /** @type {Map<string, Map<string, SavedControl[]>>} */
        const take = new Map();
        const keep = (/** @type {FormlessUnit} */ u) => {
            const at = take.get(u.view) ?? new Map();
            at.set(u.name, u.controls);
            take.set(u.view, at);
        };
        /** The parts of each radio name, one per view. */
        /** @type {Map<string, FormlessUnit[]>} */
        const parts = new Map();
        for (const u of _savedFormless) {
            // A view id names one live view: a window not yet closed may still hold
            // the copy the resync replaced.
            // The view itself, holding a control of the unit's or not: a part the user
            // acted on last decides even when nothing of it is left to restore.
            if ((u.view ? findViewRoot(u.view)?.ownerDocument : document) !== doc) continue;
            if (u.name.startsWith('\0')) {
                keep(u);
                continue;
            }
            const list = parts.get(u.name);
            if (list) list.push(u);
            else parts.set(u.name, [u]);
        }
        // A radio name is one group per document, which shows one state: that of the
        // part the user acted on last, of those whose choice still has a radio to pair
        // with in its view -- the server's when that act left it holding none.
        for (const [name, us] of parts) {
            const last = lastActed(
                us.map((u) => ({ cs: u.controls, fresh: views.get(u.view) ?? [] })),
                name,
            );
            if (last !== null) keep(us[last]);
        }
        /** @type {Array<[any, string]>} */
        const mine = [];
        // In the document's order, so the replay follows it.
        for (const [view, vEls] of views) {
            const saved = take.get(view);
            if (saved) mine.push(...restoreScope(vEls, formlessName, saved));
        }
        // A radio group's replay proposes the document's whole group.
        for (const [el] of mine) {
            if (!edited(el)) continue;
            const unit = el.type === 'radio' && el.name ? (radiosOf.get(el.name) ?? [el]) : [el];
            const keys = unitKeys(unit);
            typed.push([el, keys, keys.join('\n')]);
        }
    }
    _savedForms.clear();
    _savedFormless = [];
    _withheld.clear();
    _unrestored = false;
    for (const [el, keys, warnKey] of typed) {
        const own = ownProposal(el);
        if (!own || proposed(el)) continue;
        runReplay(el, own, warnKey);
        for (const key of keys) _replayedForms.add(key);
    }
    // What no replay proposes shows the server's again: a statement, ending the pick
    // the restore wrote there.
    serverBatch(() => showDefaults(typed.map(([el]) => el)));
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

    // A reload or back/forward refills controls from the browser's saved form
    // state, which a server-owned one must not show: the server never had it.
    // Firefox refills while parsing; Chromium on a back/forward only after `load`,
    // just before `pageshow`, so a back/forward still loading resets again then,
    // except what the user edited meanwhile (a refill raises no `input`).
    // Once per document: the navigation type outlives a disconnect(), and a later
    // connect() would reset what the user typed since.
    const navType = /** @type {any} */ (performance.getEntriesByType?.('navigation')[0])?.type;
    if (!_restoreChecked && (navType === 'reload' || navType === 'back_forward')) {
        _restoreChecked = true;
        resetRestored(document);
        if (navType === 'back_forward' && document.readyState !== 'complete') {
            window.addEventListener('pageshow', () => resetRestored(document), {
                once: true,
                signal,
            });
        }
    }

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
            // A plain patch link runs under no transition rather than clearing
            // one: a transition still waiting for its (debounced) event stays on
            // this page, and the patch must not take it from it.
            const t = parseTransitionAttr(el);
            // `el.href` is resolved against the document base, as the browser would.
            navigateEffect(
                el.href,
                { noscroll: el.hasAttribute('az-noscroll') },
                isPatch ? 'patch' : 'navigate',
                t
                    ? { types: t.types, kind: isPatch ? 'any' : 'replace' }
                    : isPatch
                      ? undefined
                      : null,
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
            const ref = nextRef();
            setPendingScroll({ kind: 'pop', hash, saved, patch: navKind === 'patch', ref });
            // Replay the transition of the edge just crossed, in either
            // direction; an edge navigated without one stays instant.
            armPendingTransition(transition || null);
            sendNav(navKind, path, qs, ref, transition || null);
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
                    // [0, ops|null, effects|null, firstAfterReconnect, azAttrs|null, ref|null]
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
                        // A patch-scroll intent lives until its patch's reply: the
                        // frame carrying the patch's ref, which the server sends
                        // even when the patch changed nothing. It scrolls when that
                        // reply carries a diff and is dropped either way; a push
                        // landing first is not the reply and leaves it armed. (A
                        // patch the server degraded to a replace scrolls on its
                        // OP_REPLACE, inside applyOps, like a navigate.)
                        const armedScroll = _pendingScroll;
                        const patchReply =
                            armedScroll?.patch === true &&
                            msg[5] !== null &&
                            msg[5] === armedScroll.ref;
                        // Here, not on arrival: a transition defers this, and the
                        // old DOM stays live (and gated) until the swap is applied.
                        settleNavReplies(msg[5], !!msg[3]);
                        // Re-save what was typed while disconnected: the resync's
                        // OP_REPLACE is about to discard it.
                        if (msg[3] && _unrestored) saveFormState();
                        // ...and so is the element being typed into: put focus
                        // and caret back once the values are restored.
                        const focus = msg[3] ? captureFocus() : null;
                        // One server batch: each radio group it leaves another
                        // checked radio was stated -- unless a reconnect's, whose
                        // state the restore writes the user's picks over.
                        serverBatch(() => {
                            if (msg[1]) {
                                applyOps(msg[1], !!msg[3]);
                                releaseParked();
                                sweepKept();
                            }
                            // After the ops: the frame's own writes are the defaults the
                            // controls it answers are reconciled to.
                            const caps = _owed.get(msg[5]);
                            if (caps) {
                                _owed.delete(msg[5]);
                                settle(caps, true);
                            }
                            // Identity-checked: an OP_REPLACE in this frame consumed it.
                            if (patchReply && _pendingScroll === armedScroll) {
                                if (msg[1]?.length) applyScroll(armedScroll);
                                setPendingScroll(null);
                            }
                            if (msg[2]) applyEffects(msg[2]);
                        }, !!msg[3]);
                        if (msg[3]) restoreFormState();
                        if (focus) restoreFocus(focus);
                        // Here, not after the dispatch below: a transition defers
                        // this, and a listener must see the remounted view.
                        if (mounted) announceConnected(closes);
                    };
                    // A pending transition wraps its batch -- ops and effects
                    // together, in order, so the swap and any effect fall inside
                    // one snapshot. 'replace' (navigate) waits for the page-swap
                    // batch, ignoring stray ticks; 'any' (push_event, patch) waits
                    // for the reply to the frame it wraps and is consumed by it
                    // either way, so a reply with no diff cannot leave the intent
                    // dangling onto a later batch.
                    const pt = _pendingTransition;
                    const answers =
                        pt?.kind === 'any' && _transitionRef !== null && msg[5] === _transitionRef;
                    const wrap =
                        pt !== null &&
                        (pt.kind === 'replace' || answers) &&
                        opsMatchTransition(msg[1], pt.kind);
                    if (wrap || answers) armPendingTransition(null);
                    applyFrame(apply, wrap ? { types: pt.types } : null);
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
                        applyChoosers(document);
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
                    dropOwedReplies();
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
                // Same as a close: the owed replies die with the socket.
                dropOwedReplies();
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
        armPendingTransition(null);
        _heldFrames = null;
        _frameGen++;
        dropOwedReplies();
        _navRefs.clear();
        _navAwaitResync = false;
        _connectQs = '';
        _savedForms.clear();
        _savedFormless = [];
        _withheld.clear();
        _unrestored = false;
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
    // An 'any' transition armed inside the page-swap window waits for an event
    // the window is about to drop; arming it would only displace the navigate's
    // own transition, which then dies with the dropped event.
    armTransition: (t) => {
        if (t.kind === 'any' && (_navRefs.size > 0 || _navAwaitResync)) return;
        armPendingTransition(t);
    },
    holdReplay,
    formControls,
    withStandIns,
    heldSelect,
    serverBatch: (fn) => serverBatch(fn),
    navigate: navigateEffect,
    leavePage,
    propose,
    release: (caps) => settle(/** @type {any} */ (caps), false),
    bindDocument: bindControls,
});
