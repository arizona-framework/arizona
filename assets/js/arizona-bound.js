import { applyChooser, syncsProp } from './arizona-attrs.js';
import { _pipWindows, allDocs } from './arizona-docs.js';
import { notifyUpdated } from './arizona-hooks.js';

// Server-owned form controls (`az-bound`). The browser keeps a control's attribute
// (its default: what the server writes) apart from its property (what is shown), and
// once the user edits, they detach. The diff only ships changes, so an edit the
// server refused leaves the property showing a value the server never had, and the
// echo of an older keystroke would overwrite newer typing. The rule: a bound property
// shows the server's value, except while a proposal capturing it is unanswered or the
// control is composing; on the reply that empties its captures, if the live value is
// still the captured one, the property is set to the default.
//
// A unit is what one proposal captures and one reconcile writes: a control, a radio
// group (checking one radio unchecks the rest), or a select (all its options).

/**
 * @typedef {Array<[any, string, unknown]>} State each member, its property and value
 * @typedef {{caps: Set<Capture>, deferred: State|null,
 *   members: () => Array<[any, string]>,
 *   writes: Map<Element, Map<string, () => void>>|null}} Unit
 * @typedef {{u: Unit, s: State}} Capture
 */

/** @type {WeakMap<Element, Unit>} a control, or a select */
const _units = new WeakMap();
// A radio group's unit, by each of its radios: a group is its radios now, wherever
// a pop-out or pop-back moved them, not the owner it had when a proposal captured it.
/** @type {WeakMap<Element, Unit>} */
const _radioUnits = new WeakMap();
/** @type {WeakSet<EventTarget>} controls an IME composition is running in */
const _composing = new WeakSet();
// Controls the user changed (an `input` or `change` on them) since a reconcile last
// showed the server's value. A control apart from its default is the user's edit
// only if touched: a diff that inserts an option before the one a select shows for
// none selected moves its default, and the browser keeps showing the old one.
/** @type {WeakSet<EventTarget>} */
const _touched = new WeakSet();
// When the user last chose each radio, in `_actSeq` order: of two radio groups'
// states a reconnect can show only one of, the restore takes the one the user acted
// on last. An act is a trusted `input` or `change` on a radio checked after it: the
// browser fires those only when the user (or an activation, `click()`) changed the
// checkedness, while code dispatching an event is untrusted -- so no record of what a
// group showed is needed to tell them apart.
/** @type {WeakMap<EventTarget, number>} */
const _acted = new WeakMap();
// The group each act chose in (see `groupKey`). A pick is for that group: once the
// radio is in another it counts for none, and back in its own it counts again.
/** @type {WeakMap<EventTarget, string>} */
const _actGroup = new WeakMap();

// When the server last stated the group a radio's pick was made in, in `_actSeq`
// order: a batch of its own (see `serverBatch`) after which the group's checked radio
// is another -- whatever moved it: a write, an insert, a removal, a rename, a form
// arriving or leaving. A pick stands only when made after its own radio's stamp, so
// only the radios whose pick may still stand (`_candidates`) take one: those whose
// act group (`_actGroup`) was stated, wherever the radio is now.
/** @type {WeakMap<Element, number>} */
const _serverAt = new WeakMap();
// When the form a radio's pick was made in was last reset, in `_actSeq` order, with
// the reset event: a listener may cancel the reset after it was stamped, which the
// event tells.
/** @type {WeakMap<Element, [number, Event]>} */
const _resetAt = new WeakMap();
// The radios whose pick may still stand: added by an act, let go by the stamp that
// ends it. Held weakly: a removed radio is not kept alive. With none, a batch looks
// nothing up.
/** @type {Set<{deref: () => Element|undefined}>} */
const _candidates = new Set();
// ES2021, past the checked target: a browser without it holds the radio, which
// `candidates` lets go of once its pick no longer stands.
const Ref = /** @type {any} */ (globalThis).WeakRef;
// Server batches running (see `serverBatch`): one nested in another is the outer's.
let _batch = 0;
/** @type {WeakSet<Element>} radios `_candidates` holds */
const _candidate = new WeakSet();

/**
 * Note radio `el` as one whose pick may stand (see `_candidates`).
 * @param {Element} el
 */
function candidate(el) {
    if (_candidate.has(el)) return;
    _candidate.add(el);
    _candidates.add(Ref ? new Ref(el) : { deref: () => el });
}

/** The owner of radio `r`'s group: its form, else its root node. @param {any} r */
const ownerOf = (r) => r.form || r.getRootNode();

let _actSeq = 0;
// Captures alive anywhere, and units holding diff writes: the op path asks nothing
// more while there are none.
let _live = 0;
/** @type {Set<Unit>} */
const _kept = new Set();

/** @param {Element} el @returns {string[]} */
function tokens(el) {
    return (el.getAttribute('az-bound') || '').split(' ');
}

/**
 * Does the server own `el`'s `name`? An option's selection is also owned through its
 * select's `value` (`az-bound="value"`): a select whose server-rendered value picks
 * among options the compiler could not mark (a component renders them) owns them all.
 * @param {Element} el
 * @param {string} name
 * @returns {boolean}
 */
function bound(el, name) {
    if (!syncsProp(el, name)) return false;
    if (tokens(el).includes(name)) return true;
    if (name !== 'selected') return false;
    const sel = el.closest('select');
    return !!sel && tokens(sel).includes('value');
}

/**
 * Is `el` diffed? Its nearest view or az-nodiff element must be a view that is not
 * itself az-nodiff: outside every view (a layout) or in an az-nodiff element, a
 * component compiled live still carries `az-bound`, but its default never changes,
 * so reconciling to it would revert every keystroke. A child view under an az-nodiff
 * element is registered and diffs its own template.
 * @param {Element} el
 */
function live(el) {
    const n = el.closest('[az-view],[az-nodiff]');
    return !!n && !n.hasAttribute('az-nodiff');
}

/**
 * Does the server own `el`'s `name` and update it?
 * @param {Element} el
 * @param {string} name
 */
export function owned(el, name) {
    return bound(el, name) && live(el);
}

/**
 * The radios of a group: those of its name in the owner's tree whose form owner is
 * `owner` (a `form="id"` radio outside the form element counts), or, formless, whose
 * root node is.
 * @param {Node} owner
 * @param {string} name
 */
function radios(owner, name) {
    // An index by name, never every control: a form's own controls of that name or
    // id (one element, or a list of them), else the document's of that name; a
    // shadow root has none.
    const o = /** @type {any} */ (owner);
    let all;
    if (o.localName === 'form') {
        const n = o.elements.namedItem(name);
        all = !n ? [] : n.nodeType === 1 ? [n] : n;
    } else all = o.getElementsByName ? o.getElementsByName(name) : o.querySelectorAll('input');
    return [...all].filter(
        (/** @type {any} */ r) =>
            r.localName === 'input' &&
            r.type === 'radio' &&
            r.name === name &&
            ownerOf(r) === owner,
    );
}

/**
 * The unit holding `el`'s bound `name`; `make` creates it when missing.
 * @param {any} el
 * @param {string} name
 * @param {boolean} make
 * @returns {Unit|null}
 */
function unitFor(el, name, make) {
    /** @type {Map<any, Unit>|WeakMap<any, Unit>|undefined} */
    // A nameless radio is in no group: the browser checks it independently.
    if (name === 'checked' && el.type === 'radio' && el.name) return radioUnit(el, make);
    let key = el;
    /** @type {() => Array<[any, string]>} */
    let members;
    // A select's options are one unit keyed by the select, which its `value` (a write
    // `defer` holds) finds too.
    if (name === 'selected') {
        key = el.closest('select');
        members = () => [...key.options].map((o) => [o, 'selected']);
    } else {
        members = () => tokens(el).flatMap((p) => (syncsProp(el, p) ? [[el, p]] : []));
    }
    if (key === null) return null;
    let u = _units.get(key);
    if (!u && make) {
        u = { caps: new Set(), deferred: null, members, writes: null };
        _units.set(key, u);
    }
    return u || null;
}

/**
 * The unit of radio `el`'s group: the one a radio of it holds (the one with the most
 * captures, after a pop-back merged two groups), else a new one when `make`. Its
 * members are the groups its radios are in when asked, so a reply or a diff write
 * reaches them wherever they moved.
 * @param {any} el
 * @param {boolean} make
 * @returns {Unit|null}
 */
function radioUnit(el, make) {
    const group = targets(el);
    /** @type {Unit|null} */
    let u = null;
    for (const r of group) {
        const x = _radioUnits.get(r);
        if (x && (!u || x.caps.size > u.caps.size)) u = x;
    }
    if (!u) {
        if (!make) return null;
        /** @type {Set<any>} */
        let rs = new Set(group);
        const unit = /** @type {Unit} */ ({ caps: new Set(), deferred: null, writes: null });
        unit.members = () => {
            /** @type {Set<any>} */
            const now = new Set();
            for (const r of rs) if (r.isConnected) for (const t of targets(r)) now.add(t);
            if (now.size > 0) rs = now;
            for (const r of rs) _radioUnits.set(r, unit);
            return [...rs].map((r) => [r, 'checked']);
        };
        u = unit;
    }
    return u;
}

/**
 * The unit a proposal from control `c` captures, or null: a server-owned control (see
 * `ownedControl`), or a radio of a server-owned group. Any radio of the group proposes
 * it, so a form whose only bound radio is disabled still holds the group it submits.
 * @param {any} c
 */
function controlUnit(c) {
    if (c.type === 'radio' && c.name) return ownedControl(c) ? unitFor(c, 'checked', true) : null;
    for (const [e, p] of props(c)) if (owned(e, p)) return unitFor(e, p, true);
    return null;
}

/** @param {Unit} u @returns {State} */
function stateOf(u) {
    return u.members().map(([e, p]) => [e, p, e[p]]);
}

/**
 * Capture the bound units a push_event from trigger `el` proposes values for: the
 * control itself, or a form's submitted controls. The live DOM, not the payload (an
 * explicit payload may override `value`). A prevented default is no proposal: the
 * browser reverts the change (a controlled checkbox), so the server's write must
 * reach the property. A submit is always prevented, but that reverts nothing.
 * @param {Element} el
 * @param {Event|null} event
 * @returns {Capture[]|null}
 */
export function propose(el, event) {
    if (event?.defaultPrevented && event.type !== 'submit') return null;
    const ctrls =
        el.localName === 'form'
            ? [
                  .../** @type {HTMLFormElement} */ (el).elements,
                  ...formAway(/** @type {HTMLFormElement} */ (el)),
              ].filter((c) => /** @type {any} */ (c).name && !c.matches(':disabled'))
            : [el];
    /** @type {Set<Unit>} */
    const units = new Set();
    for (const c of ctrls) {
        const u = controlUnit(c);
        if (u) units.add(u);
    }
    if (units.size === 0) return null;
    return [...units].map((u) => {
        /** @type {Capture} */
        const cap = { u, s: stateOf(u) };
        u.caps.add(cap);
        u.deferred = null;
        _live++;
        return cap;
    });
}

/**
 * A proposal is answered (`reply`) or will never be (dropped, superseded). A unit
 * whose last capture goes is released, and reconciled against that capture when its
 * reply removed it.
 * @param {Capture[]} caps
 * @param {boolean} reply
 */
export function settle(caps, reply) {
    for (const cap of caps) {
        const u = cap.u;
        if (!u.caps.delete(cap)) continue;
        _live--;
        if (u.caps.size === 0) release(u, reply ? cap.s : null);
    }
}

/**
 * Does a proposal still wait on `el`'s `name`?
 * @param {Element} el
 * @param {string} name
 * @returns {boolean}
 */
function held(el, name) {
    return _live > 0 && bound(el, name) && (unitFor(el, name, false)?.caps.size ?? 0) > 0;
}

/**
 * Does a proposal wait on select `sel`'s options? Then nothing writes them, the
 * marking its `value` makes neither (`applyChoosers`): the release does.
 * @param {Element} sel
 */
export const heldSelect = (sel) => _live > 0 && (_units.get(sel)?.caps.size ?? 0) > 0;

/**
 * Hold a diff write to `el`'s `name` (an attribute, or a textarea's content) while a
 * proposal waits on it: not even the attribute is written, since Firefox clears a
 * partial date when its value attribute changes. The latest one per element and
 * name is applied when the unit is released. Returns whether `write` was held; one
 * that goes through supersedes a write kept past a release (see `release`).
 * @param {Element} el
 * @param {string} name
 * @param {() => void} write the write, attribute (or content) only
 * @returns {boolean}
 */
export function defer(el, name, write) {
    if ((_live === 0 && _kept.size === 0) || !bound(el, name)) return false;
    const u = unitFor(el, name, false);
    if (!u) return false;
    if (u.caps.size === 0) {
        u.writes?.get(el)?.delete(name);
        return false;
    }
    if (!u.writes) {
        u.writes = new Map();
        _kept.add(u);
    }
    let w = u.writes.get(el);
    if (!w) {
        w = new Map();
        u.writes.set(el, w);
    }
    w.set(name, write);
    return true;
}

/**
 * A unit has no capture left: apply the writes held for it (an element removed
 * meanwhile drops its own), then reconcile to the defaults they set. Bad input
 * (mid-typing "1e-", a partial date) reads as "", so both would clear the text: the
 * reconcile is dropped and the writes are kept until the field's next input or
 * focusout with good input (`bindControls`). A composition only delays the
 * reconcile to its end: a dirty control ignores a default write.
 * @param {Unit} u
 * @param {State|null} captured null for a proposal no reply answers
 */
function release(u, captured) {
    serverBatch(() => releaseIn(u, captured));
}

/**
 * See `release`.
 * @param {Unit} u
 * @param {State|null} captured
 */
function releaseIn(u, captured) {
    const ms = u.members();
    if (ms.some(([e]) => e.isConnected && e.validity?.badInput)) return;
    const writes = u.writes;
    if (writes) {
        u.writes = null;
        _kept.delete(u);
        for (const [e, w] of writes) if (e.isConnected) for (const write of w.values()) write();
    }
    // A select's value marks its options' defaults, held with the rest while a
    // proposal waited (`heldSelect`): options a batch rendered meanwhile too.
    const sel = ms[0]?.[1] === 'selected' ? ms[0][0].closest('select') : null;
    if (sel?.isConnected && sel.hasAttribute('value')) applyChooser(sel, false);
    if (!captured) return;
    if (ms.some(([e]) => _composing.has(e))) {
        u.deferred = captured;
        return;
    }
    // A reply may add or remove a radio or an option: the captured members still
    // there decide, and a new one counts as unchanged.
    const now = new Set(ms.map(([e]) => e));
    if (captured.some(([e, p, v]) => now.has(e) && e[p] !== v)) return;
    writeDefaults(ms);
}

/**
 * Let go of the writes kept for removed controls: no input or focusout will come
 * from them, and the op path asks every write while any unit keeps one. Called
 * after a batch of diff ops, which is what removes controls.
 */
export function sweepKept() {
    for (const u of _kept) {
        if (u.caps.size > 0) continue;
        const w = /** @type {Map<Element, unknown>} */ (u.writes);
        if ([...w.keys()].some((e) => e.isConnected)) continue;
        u.writes = null;
        _kept.delete(u);
    }
}

/**
 * Set each property to its default where they differ (an echo writes nothing, so no
 * caret move and no undo-stack damage). Unchecking first: checking a radio or a
 * single select's option unsets its siblings implicitly, which notifies no hook.
 * @param {Array<[any, string]>} ms
 */
function writeDefaults(ms) {
    for (const [e] of ms) _touched.delete(e.localName === 'option' ? e.closest('select') : e);
    const ds = defaults(ms);
    for (const [e, p] of ms) if (p === 'value' && e.value !== e.defaultValue) writeValue(e);
    for (const on of [false, true]) {
        ms.forEach(([e, p], i) => {
            if (p !== 'value' && ds[i] === on && e[p] !== on) {
                e[p] = on;
                notifyUpdated(e);
            }
        });
    }
}

/**
 * The default of each member's property: what a form reset shows. Of a single
 * select's options that is the last one marked selected, else (one row shown) its
 * first enabled one, which the browser shows selected though none is marked.
 * @param {Array<[any, string]>} ms
 * @returns {unknown[]}
 */
function defaults(ms) {
    /** @type {Map<Element, Element|null>} */
    const picks = new Map();
    return ms.map(([e, p]) => {
        if (p === 'value') return e.defaultValue;
        if (p === 'checked') return e.defaultChecked;
        const s = e.closest('select');
        if (!s || s.multiple) return e.defaultSelected;
        if (!picks.has(s)) picks.set(s, resetPick(s));
        return picks.get(s) === e;
    });
}

/**
 * The option a single select `s` shows selected after a form reset, or null.
 * @param {any} s
 */
function resetPick(s) {
    let first = null;
    let last = null;
    for (const o of s.options) {
        if (o.defaultSelected) last = o;
        else if (!first && !o.matches(':disabled')) first = o;
    }
    return last || (s.size > 1 ? null : first);
}

/**
 * Show a text control's default, keeping a focused one's selection: a value write
 * moves the caret to the end. Types without a selection throw on access.
 * @param {any} e
 */
function writeValue(e) {
    let sel = null;
    if (e.ownerDocument.activeElement === e) {
        try {
            if (typeof e.selectionStart === 'number')
                sel = [e.selectionStart, e.selectionEnd, e.selectionDirection];
        } catch {}
    }
    e.value = e.defaultValue;
    if (sel) {
        const n = e.value.length;
        try {
            e.setSelectionRange(Math.min(sel[0], n), Math.min(sel[1], n), sel[2]);
        } catch {}
    }
    notifyUpdated(e);
}

/**
 * Track IME compositions in `doc`: a reconcile waits for one to end, and runs then
 * (Chrome may fire no input after compositionend, so nothing else would). A field's
 * next input or focusout applies the writes kept past a release for its bad input.
 * @param {Document} doc
 * @param {AbortSignal} signal
 */
export function bindControls(doc, signal) {
    const opts = { capture: true, signal };
    // Stamped as it is dispatched, before the reset runs: a later listener may cancel
    // it, which `standingPick` reads off the event. The server's too, changing no radio.
    // It ends the picks made in the form, wherever their radios are now.
    doc.addEventListener(
        'reset',
        (e) => {
            const at = /** @type {[number, Event]} */ ([++_actSeq, e]);
            const k = `${formKey(e.target)}\0`;
            for (const r of candidates(true))
                if (_actGroup.get(r)?.startsWith(k)) _resetAt.set(r, at);
        },
        opts,
    );
    doc.addEventListener(
        'compositionstart',
        (e) => _composing.add(/** @type {any} */ (e.target)),
        opts,
    );
    doc.addEventListener(
        'compositionend',
        (e) => {
            const el = /** @type {Element} */ (e.target);
            _composing.delete(el);
            const u = _units.get(el);
            if (u && u.caps.size === 0 && (u.deferred || u.writes)) {
                const s = u.deferred;
                u.deferred = null;
                release(u, s);
            }
        },
        opts,
    );
    // Only while it shows something apart from the server's value: the `change` a
    // blur fires after a reconcile showed the server's would otherwise count it.
    const touch = (/** @type {Event} */ e) => {
        const t = /** @type {any} */ (e.target);
        // A radio's event is an edit only when it is an act, which touches the radio
        // chosen whatever owns its group then: a pop-back may merge it into a
        // server-owned one, which then holds the user's choice.
        if (t.type === 'radio' && t.name) {
            if (noteChoice(t, e)) _touched.add(t);
        } else if (targets(t).some(edited)) _touched.add(t);
    };
    doc.addEventListener('change', touch, opts);
    const retry = (/** @type {Event} */ e) => {
        if (e.type === 'input') touch(e);
        if (_kept.size === 0) return;
        const u = _units.get(/** @type {Element} */ (e.target));
        if (u && u.caps.size === 0 && u.writes) release(u, null);
    };
    doc.addEventListener('input', retry, opts);
    doc.addEventListener('focusout', retry, opts);
}

/**
 * The bound properties of control `el` (a select: its options').
 * @param {any} el
 * @returns {Array<[any, string]>}
 */
function props(el) {
    return el.localName === 'select'
        ? [...el.options].map((o) => [o, 'selected'])
        : tokens(el).map((p) => [el, p]);
}

/**
 * Does the server own control `el`: a property of its unit bound and diffed (see
 * `owned`)? A select's unit is its options, and a named radio's its group: one bound
 * radio makes every radio of the group the server's.
 * @param {Element} el
 */
export function ownedControl(el) {
    const r = /** @type {any} */ (el);
    if (r.type === 'radio' && r.name) return targets(r).some((x) => owned(x, 'checked'));
    return props(el).some(([e, p]) => owned(e, p));
}

/**
 * Does a proposal wait on a bound property of control `el` (a radio: of its group)?
 * @param {Element} el
 */
export function proposed(el) {
    return targets(el).some((c) => props(c).some(([e, p]) => held(e, p)));
}

/**
 * Does control `el` show a server-owned property apart from its default? A radio of a
 * server-owned group answers for its group, every radio of it: a choice that moved
 * only unbound radios is an edit too.
 * @param {Element} el
 */
export function edited(el) {
    const r = /** @type {any} */ (el);
    if (r.type === 'radio' && r.name && ownedControl(r))
        return targets(r).some((x) => x.checked !== x.defaultChecked);
    const ms = props(el);
    const ds = defaults(ms);
    return ms.some(([e, p], i) => owned(e, p) && e[p] !== ds[i]);
}

/**
 * The controls of `el`'s unit an `input` or `change` lands on: a named radio's
 * group, an option's select, else `el`.
 * @param {any} el
 * @returns {any[]}
 */
function targets(el) {
    if (el.type === 'radio' && el.name) return radios(ownerOf(el), el.name);
    const c = el.localName === 'option' ? el.closest('select') : el;
    return c ? [c] : [];
}

/**
 * Count what a restore wrote into control `el` (a radio: its group) as the user's
 * change: the element is fresh, so no `input` has landed on it, and a drop before
 * the replay's reply would otherwise save the server's value in its place. A write
 * that left it at its default is no change. `at` is when the user made the change
 * the restore wrote (see `actedAt`), now when unknown.
 * @param {Element} el
 * @param {number} [at]
 */
export function markTouched(el, at = ++_actSeq) {
    const cs = targets(el);
    if (!cs.some(edited)) return;
    for (const c of cs) _touched.add(c);
    if (/** @type {any} */ (el).type !== 'radio') return;
    _acted.set(el, at);
    _actGroup.set(el, groupKey(el));
    candidate(el);
}

// The radios a relayed pick was warned about, once each (weak: a removed radio is
// not kept alive).
/** @type {WeakSet<Element>} */
const _relayWarned = new WeakSet();

/**
 * Note an event `e` on radio `t`, and say whether it is an act: trusted (see
 * `_acted`), which the browser fires only on the radio it checked.
 * @param {any} t
 * @param {Event} e
 * @returns {boolean}
 */
function noteChoice(t, e) {
    if (!e.isTrusted) {
        // A script relaying the user's pick by checking the radio and dispatching an
        // event makes no act: a reconnect silently restores the server's choice. One
        // announcing again the radio holding its group's latest act (the user's pick,
        // or a restore's) relays nothing.
        const at = _acted.get(t) ?? 0;
        if (
            t.checked &&
            !t.defaultChecked &&
            !_relayWarned.has(t) &&
            targets(t).some((r) => owned(r, 'checked')) &&
            !(at > 0 && targets(t).every((r) => (_acted.get(r) ?? 0) <= at))
        ) {
            _relayWarned.add(t);
            console.warn(
                `[arizona] a script checked radio ${t.name}=${t.value} and dispatched '${e.type}': ` +
                    'no user act, so a reconnect restores the server choice. Relay the pick with radio.click().',
            );
        }
        return false;
    }
    if (!t.checked) return false;
    _acted.set(t, ++_actSeq);
    _actGroup.set(t, groupKey(t));
    candidate(t);
    return true;
}

/**
 * When the user last chose radio `el` (see `_acted`), in the order of their acts,
 * while it is in the group that act chose in (see `_actGroup`); else 0.
 * @param {Element} el
 */
export function actedOn(el) {
    if (_actGroup.get(el) !== groupKey(el)) return 0;
    return _acted.get(el) ?? 0;
}

/**
 * The fields of `form` a pop-out moved into another document, which the form owns
 * as the server renders it (`formOf`) though `form.elements` cannot see them. None
 * while no window is open.
 * @param {HTMLFormElement} form
 * @returns {Element[]}
 */
export function formAway(form) {
    if (_pipWindows.size === 0) return [];
    const out = [];
    for (const d of allDocs())
        if (d !== form.ownerDocument)
            for (const el of d.querySelectorAll('input, select, textarea, button'))
                if (formOf(el) === form && !fencedInline(el)) out.push(el);
    return out;
}

/**
 * Is `el`, in a window, disabled where its view sits inline: inside a disabled fieldset
 * there but not in that fieldset's first `legend` (the window's own document cannot see
 * it).
 * @param {Element} el
 * @returns {boolean}
 */
function fencedInline(el) {
    for (const { win, placeholder } of _pipWindows.values())
        if (win.document === el.ownerDocument) {
            for (let n = placeholder.parentElement; n; n = n.parentElement)
                if (n.localName === 'fieldset' && n.hasAttribute('disabled')) {
                    const legend = [...n.children].find((c) => c.localName === 'legend');
                    if (!legend?.contains(placeholder)) return true;
                }
            return placeholder.parentElement ? fencedInline(placeholder.parentElement) : false;
        }
    return false;
}

/**
 * Run `build` (a `FormData` of `form`) with a stand-in in the form's own document for
 * each field a pop-out moved into another document, at that field's place inline: its
 * view's placeholder, or, for a field of the page naming a form a window holds, beside
 * that window's view. The browser then orders, disables (a disabled fieldset around the
 * place, its first legend excepted) and shows them to `formdata` listeners as it would
 * inline. A stand-in carries only what the field submits -- a hidden input per entry
 * (`_charset_` and `dirname` as the browser handles a hidden one, with the direction
 * the field takes in its window), a textarea for a textarea (the
 * value it submits: one wrapping hard is laid out apart to read it), a file input
 * holding the files -- no id or attribute of the app's, so nothing else reads it; it
 * is gone before `build` returns, in the same task (no paint), though a
 * `MutationObserver` sees it come and go, and a slot its view's placeholder is
 * assigned to fires `slotchange`.
 * @template T
 * @param {HTMLFormElement} form
 * @param {() => T} build
 * @returns {T}
 */
export function withStandIns(form, build) {
    const away = formAway(form);
    if (away.length === 0) return build();
    const doc = form.ownerDocument;
    /** @type {Element[]} */
    const made = [];
    // The last stand-in after each view: the next field after it goes after that one.
    /** @type {Map<Node, Node>} */
    const last = new Map();
    try {
        for (const el of away.sort(inlineOrder)) {
            const at = placeIn(el, form);
            if (!at) continue;
            let ref = at.after ? (last.get(at.ref) ?? at.ref) : at.ref;
            for (const s of standIns(/** @type {any} */ (el), doc)) {
                if (!form.contains(ref.parentNode ?? ref)) s.setAttribute('form', form.id);
                ref.parentNode?.insertBefore(s, at.after ? ref.nextSibling : ref);
                made.push(s);
                if (at.after) {
                    ref = s;
                    last.set(at.ref, s);
                }
            }
        }
        return build();
    } finally {
        for (const s of made) s.remove();
    }
}

/**
 * Where field `el`, in another document than `form`, sits inline in the form's own
 * document: before the node its chain of placeholders reaches there (a window within a
 * window followed up), or, for one that never reaches it (a field naming a form a
 * window holds), before or after the form's view by where the two chains part.
 * @param {Element} el
 * @param {HTMLFormElement} form
 * @returns {{ref: Node, after: boolean}|null}
 */
function placeIn(el, form) {
    const doc = form.ownerDocument;
    const x = chainOf(el);
    const at = x.find((n) => n.ownerDocument === doc);
    if (at) return { ref: at, after: false };
    const y = chainOf(form);
    let i = 0;
    while (i < x.length && i < y.length && x[i] === y[i]) i++;
    const view = [...doc.body.children].find((v) => v.contains(form));
    if (!view || i === x.length || i === y.length) return null;
    return { ref: view, after: !!(y[i].compareDocumentPosition(x[i]) & 4) };
}

/**
 * The stand-ins in document `doc` of field `el`: elements carrying what it submits.
 * None for a field that submits nothing where it is (in a `datalist`, disabled there,
 * a button -- one in another document cannot submit the form).
 * @param {any} el
 * @param {Document} doc
 * @returns {Element[]}
 */
function standIns(el, doc) {
    if (!el.name || el.matches(':disabled') || el.closest('datalist')) return [];
    /** @param {string} value @returns {Element} */
    const hidden = (value) => {
        const h = doc.createElement('input');
        // A hidden input named `_charset_` submits the charset: only a hidden field is.
        if (el.type !== 'hidden' && el.name.toLowerCase() === '_charset_') h.hidden = true;
        else h.type = 'hidden';
        h.name = el.name;
        h.value = value;
        return h;
    };
    /** @param {Element} s @returns {Element} */
    const directed = (s) => {
        const dn = el.getAttribute('dirname');
        if (dn !== null) s.setAttribute('dirname', dn);
        const dir = dirOf(el);
        if (dir) s.setAttribute('dir', dir);
        return s;
    };
    if (el.localName === 'select')
        return [...el.options]
            .filter((o) => o.selected && !o.matches(':disabled'))
            .map((o) => hidden(o.value));
    if (el.localName === 'textarea') {
        const t = doc.createElement('textarea');
        t.name = el.name;
        t.hidden = true;
        t.value =
            el.getAttribute('wrap')?.toLowerCase() === 'hard' ? hardWrapped(el, doc) : el.value;
        return [directed(t)];
    }
    if (el.localName !== 'input') return [];
    const type = el.type;
    if (type === 'submit' || type === 'image' || type === 'reset' || type === 'button') return [];
    if (type === 'checkbox' || type === 'radio') return el.checked ? [hidden(el.value)] : [];
    if (type === 'file') {
        const f = doc.createElement('input');
        f.type = 'file';
        f.name = el.name;
        f.hidden = true;
        f.files = el.files;
        return [f];
    }
    const h = hidden(el.value);
    const dirnamed = ['hidden', 'text', 'search', 'tel', 'url', 'email', 'password'].includes(type);
    return [dirnamed ? directed(h) : h];
}

/**
 * The value textarea `el`, wrapping hard, submits: a browser may wrap by the lines it
 * lays out, which a hidden or box-less stand-in has none of, so a copy is laid out,
 * unseen, in a form of its own in a closed shadow root at the end of `doc` -- as wide
 * as the field's text (its `cols` when the field has no box) and in its font -- and
 * read back. The page's CSS cannot reach it, the `formdata` event of its build stays
 * in that root, and it is gone before this returns.
 * @param {any} el
 * @param {Document} doc
 * @returns {string}
 */
function hardWrapped(el, doc) {
    // In a closed shadow root: the page's CSS cannot reach the copy, and the
    // `formdata` event its build fires (not composed) never leaves it.
    const host = doc.createElement('az-measure');
    // Important: no page rule on the host may hide it, which would leave no lines.
    for (const [k, v] of [
        ['display', 'block'],
        ['position', 'absolute'],
        ['left', '0'],
        ['top', '0'],
        ['visibility', 'hidden'],
    ])
        host.style.setProperty(k, v, 'important');
    const root = host.attachShadow({ mode: 'closed' });
    const f = doc.createElement('form');
    const t = doc.createElement('textarea');
    t.name = 'v';
    t.setAttribute('wrap', 'hard');
    if (el.hasAttribute('cols')) t.setAttribute('cols', el.getAttribute('cols'));
    t.value = el.value;
    // Its language: `hyphens` and `line-break` break lines by it.
    const lang = el.closest('[lang]')?.getAttribute('lang');
    if (lang) t.setAttribute('lang', lang);
    const cs = el.ownerDocument.defaultView?.getComputedStyle(el);
    if (cs) {
        const st = t.style;
        st.cssText = 'padding:0;border:0;overflow:hidden;box-sizing:content-box';
        const pad = parseFloat(cs.paddingLeft) + parseFloat(cs.paddingRight);
        if (el.clientWidth > 0) st.width = `${Math.max(0, el.clientWidth - (pad || 0))}px`;
        // Longhands: the `font` shorthand may read back empty.
        for (const p of [
            'font-style',
            'font-variant',
            'font-weight',
            'font-stretch',
            'font-size',
            'line-height',
            'font-family',
            'letter-spacing',
            'word-spacing',
            'white-space',
            'overflow-wrap',
            'word-break',
            'tab-size',
            'text-transform',
            'text-indent',
            'hyphens',
            'line-break',
            'font-kerning',
            'font-feature-settings',
            'font-variation-settings',
            'font-variant-ligatures',
            'font-size-adjust',
        ])
            st.setProperty(p, cs.getPropertyValue(p));
    }
    f.append(t);
    root.append(f);
    (doc.body ?? doc.documentElement).append(host);
    try {
        return String(new FormData(f).get('v') ?? '');
    } finally {
        host.remove();
    }
}

/**
 * The direction `el` takes from itself or its ancestors in its window, up to its view
 * (a valid `dir`, ASCII case-insensitive, an `auto` ancestor resolved by the window);
 * null when none there decides it, the place inline then deciding as it would.
 * @param {Element} el
 * @returns {string|null}
 */
function dirOf(el) {
    for (let n = /** @type {Element|null} */ (el); n; n = n.parentElement) {
        const d = n.getAttribute('dir')?.toLowerCase();
        if (d === 'ltr' || d === 'rtl' || (d === 'auto' && n === el)) return d;
        if (d === 'auto') return el.matches(':dir(rtl)') ? 'rtl' : 'ltr';
        if (n.parentElement === n.ownerDocument.body) return null;
    }
    return null;
}

/**
 * Every field of `form`, its own and those a pop-out moved into another document
 * (`formAway`), in tree order as if each window's view were back at its placeholder;
 * null when none is away, `form.elements` being all of them then.
 * @param {HTMLFormElement} form
 * @returns {Element[]|null}
 */
export function formControls(form) {
    const away = formAway(form);
    if (away.length === 0) return null;
    return [...form.elements, ...away].sort(inlineOrder);
}

/**
 * Node `n`, after each placeholder up from its window to the page, top first.
 * @param {Node} n
 * @returns {Node[]}
 */
function chainOf(n) {
    for (const { win, placeholder } of _pipWindows.values())
        if (win.document === n.ownerDocument) return [...chainOf(placeholder), n];
    return [n];
}

/**
 * Tree order of two nodes as if every popped-out view were back at its placeholder:
 * compared where their chains of placeholders (a window inside a window included) part.
 * @param {Node} a
 * @param {Node} b
 * @returns {number}
 */
function inlineOrder(a, b) {
    const x = chainOf(a);
    const y = chainOf(b);
    let i = 0;
    while (i < x.length && i < y.length && x[i] === y[i]) i++;
    if (i === x.length || i === y.length) return x.length - y.length;
    return x[i].compareDocumentPosition(y[i]) & 4 ? -1 : 1;
}

/**
 * The form a control belongs to as the server renders it: its form owner, else the
 * form its `form` attribute names in whichever document holds it now (a pop-out may
 * have moved the control or the form into a window), else, popped out (PiP), the form
 * holding its view's placeholder inline (`formAround`).
 * @param {any} el
 * @returns {any}
 */
export function formOf(el) {
    if (el.form) return el.form;
    const named = el.getAttribute('form');
    if (named !== null) {
        for (const d of allDocs()) {
            const f = d.getElementById(named);
            if (f?.localName === 'form') return f;
        }
        return null;
    }
    return formAround(el);
}

/**
 * The form around node `n` as if every popped-out view were back at its placeholder:
 * the one around its window's placeholder, that window's own included.
 * @param {Node} n
 * @returns {HTMLFormElement|null}
 */
function formAround(n) {
    for (const { win, placeholder } of _pipWindows.values())
        if (win.document === n.ownerDocument)
            return placeholder.parentElement?.closest('form') ?? formAround(placeholder);
    return null;
}

/**
 * The group radio `el` is in as the server renders it: its form (`formOf`) by the
 * view holding it and its id -- a view is its id, and two views may each render a
 * form of one id -- or the form itself when it has none, and its name. A form
 * re-rendered under its id is the same group.
 * @param {any} el
 * @returns {string}
 */
function groupKey(el) {
    return `${formKey(formOf(el))}\0${el.name}`;
}

/**
 * Form `f`'s part of a group key (see `groupKey`); '' for none.
 * @param {any} f
 * @returns {string}
 */
function formKey(f) {
    return f ? (f.id ? `${f.closest('[az-view]')?.id ?? ''}\0${f.id}` : ids(f)) : '';
}

/** A distinct key for each id-less form. */
/** @type {WeakMap<Element, string>} */
const _formIds = new WeakMap();
let _formSeq = 0;
/** @param {Element} f */
function ids(f) {
    const k = _formIds.get(f) ?? `\0${++_formSeq}`;
    _formIds.set(f, k);
    return k;
}

/**
 * The radios whose pick may still stand (see `_candidates`): stamped no later than
 * their act -- those in a document now, or, `all`, those out of every document too (a
 * widget moving one). One gone, or stamped since, is let go.
 * @param {boolean} [all]
 */
function candidates(all = false) {
    const out = [];
    for (const w of _candidates) {
        const r = /** @type {any} */ (w.deref());
        if (r && (_serverAt.get(r) ?? 0) < (_acted.get(r) ?? 0)) {
            if (all || r.isConnected) out.push(r);
        } else {
            _candidates.delete(w);
            if (r) _candidate.delete(r);
        }
    }
    return out;
}

/**
 * The checked radios of every group, by `groupKey`, across the page and its windows.
 * @returns {Map<string, any[]>}
 */
function checkedRadios() {
    /** @type {Map<string, any[]>} */
    const out = new Map();
    for (const doc of allDocs())
        for (const r of /** @type {Iterable<any>} */ (doc.querySelectorAll('input:checked'))) {
            if (r.type !== 'radio' || !r.name) continue;
            const k = groupKey(r);
            const on = out.get(k) ?? [];
            out.set(k, on);
            on.push(r);
        }
    return out;
}

/**
 * Run `fn`, a batch of the server's: a frame's ops and effects (deferred ones too,
 * see `arizona-commands.js`), a held write applied, or the restore showing the
 * server's state. Each group whose checked radio it left another was stated (see
 * `_serverAt`). `quiet`: a reconnect frame, rendering the server's state ahead of the
 * restore, which writes the user's picks over it -- it states nothing.
 * @template T
 * @param {() => T} fn
 * @param {boolean} [quiet]
 * @returns {T}
 */
export function serverBatch(fn, quiet = false) {
    if (_batch > 0) return fn();
    const cs = quiet ? [] : candidates(true);
    const before = cs.length > 0 ? checkedRadios() : null;
    _batch++;
    try {
        return fn();
    } finally {
        _batch--;
        // The page a reconnect frame replaced is gone for good: let go of the picks
        // held in it (a browser without WeakRef holds them).
        if (quiet)
            for (const w of _candidates) {
                const r = w.deref();
                if (r?.isConnected) continue;
                _candidates.delete(w);
                if (r) _candidate.delete(r);
            }
        if (before) {
            const after = checkedRadios();
            const at = ++_actSeq;
            // A statement is about a group: it ends the picks made in that group,
            // wherever their radios are now.
            /** @type {Set<string>} */
            const stated = new Set();
            for (const k of new Set([...before.keys(), ...after.keys()])) {
                const b = before.get(k) ?? [];
                const a = after.get(k) ?? [];
                if (b.length !== a.length || b.some((r) => !a.includes(r))) stated.add(k);
            }
            for (const r of cs)
                if (stated.has(/** @type {string} */ (_actGroup.get(r)))) _serverAt.set(r, at);
        }
    }
}

/**
 * The radio of `rs` (a part of one group, as it is now) the user's pick stands on:
 * the one they chose last, when no server write of the group's `checked` and no reset
 * of its form came after; else null.
 * @param {any[]} rs
 * @returns {any}
 */
export function standingPick(rs) {
    let p = null;
    let at = 0;
    for (const r of rs) {
        const a = actedOn(r);
        if (a > at) [p, at] = [r, a];
    }
    if (!p) return null;
    const reset = _resetAt.get(p);
    if (reset && !reset[1].defaultPrevented && reset[0] > at) return null;
    return (_serverAt.get(p) ?? 0) < at ? p : null;
}

/**
 * Is control `el` (a radio: its group) the user's edit: changed by the user since it
 * last showed the server's value, and still apart from its default?
 * @param {Element} el
 */
export function userEdited(el) {
    const cs = targets(el);
    return cs.some((c) => _touched.has(c)) && cs.some(edited);
}

/**
 * The radio the server renders checked among radios `rs` (a part of a group): the
 * only one checked by default, else null (none, or several an effect left checked by
 * default).
 * @param {any[]} rs
 * @returns {Element|null}
 */
export function serverChoice(rs) {
    const ds = rs.filter((r) => r.defaultChecked);
    return ds.length === 1 ? ds[0] : null;
}

/**
 * Show the defaults of the server-owned properties of `els` no proposal waits on. A
 * named radio shows its whole group's: the server owns a group as one unit, so unchecking
 * the radio alone would leave the group empty when the one the server checks is
 * another (an unbound radio of the group included).
 * @param {Iterable<Element>} els
 */
export function showDefaults(els) {
    /** @type {Array<[any, string]>} */
    const ms = [];
    /** @type {Set<any>} */
    const seen = new Set();
    for (const el of els) {
        const r = /** @type {any} */ (el);
        if (r.type === 'radio' && r.name) {
            const group = targets(r);
            if (seen.has(group[0])) continue;
            seen.add(group[0]);
            if (group.some((g) => owned(g, 'checked') && !held(g, 'checked')))
                for (const g of group) ms.push([g, 'checked']);
            continue;
        }
        for (const [e, p] of props(el)) if (owned(e, p) && !held(e, p)) ms.push([e, p]);
    }
    writeDefaults(ms);
}

/**
 * Show the defaults of live bound controls the browser filled from its own form
 * state on a reload or back/forward: values the server never had. Autofill is the
 * user's, so it stays, and so does a value a proposal still waits on (by `pageshow`
 * the user may have edited: its reply decides then), and so does a unit the user
 * changed since it last showed the server's value (a radio group by any of its
 * radios): a refill raises no `input`.
 * @param {Document} doc
 */
export function resetRestored(doc) {
    showDefaults(
        [...doc.querySelectorAll('[az-bound]')].filter(
            (el) => live(el) && !autofilled(el) && !targets(el).some((c) => _touched.has(c)),
        ),
    );
}

/** @param {Element} el */
function autofilled(el) {
    for (const s of [':autofill', ':-webkit-autofill']) {
        try {
            if (el.matches(s)) return true;
        } catch {}
    }
    return false;
}
