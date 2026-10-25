import { executeJS, flushTimer, maybeResetForm, socketNeeds } from './arizona-commands.js';
import { mountHooks } from './arizona-hooks.js';
import { host } from './arizona-host.js';
import { withTransitionAttr } from './arizona-transitions.js';
import { warnOnce } from './arizona-util.js';

/**
 * The exported `mountHooks`: the escape hatch a host app calls after inserting
 * markup Arizona did not render. That markup is the one source of `az-*` names
 * the compile-time set cannot cover, so this form also scans for them. The op
 * path deliberately calls `mountHooks` instead -- server-rendered markup declares
 * nothing the connect frame did not already deliver, so walking it again would
 * re-derive what we were told, per element per op.
 * @param {Element|Document} root
 */
export function mountHooksPublic(root) {
    scanEvents(root);
    mountHooks(root);
}

/**
 * Run the `az-<eventType>` command for one delegated event.
 *
 * `capture` picks the resolver, and the two are not interchangeable. A BUBBLING
 * event walks its propagation path, so `closest()` correctly finds an ancestor
 * that opted in. A NON-BUBBLING event has no path -- it is dispatched at the
 * target alone -- so the only element that legitimately owns it is the target
 * itself. Using `closest()` there would run an ancestor's command for an event
 * that never reached it, which is what makes `mouseenter`/`mouseleave`
 * (dispatched separately per element) fire once per nesting level.
 * @param {Event} e
 * @param {string} attr the `az-<eventType>` attribute name
 * @param {string} sel the same name as an escaped attribute selector
 * @param {boolean} capture
 */
function runDelegated(e, attr, sel, capture) {
    const t = /** @type {Element} */ (e.target);
    // Not always an Element: a viewport `scroll`, the page-level `mouseenter`, and
    // a server-sent `dispatch_event` all target the Document, which has no closest().
    if (t?.nodeType !== 1) return;
    const el = capture ? (t.hasAttribute(attr) ? t : null) : t.closest(sel);
    if (!el) return;
    // Before the empty-value return: `az-<event>=""` with `az-prevent-default`
    // declares no command but still asks for the default to be suppressed. Before
    // the connection check too: a declared suppression holds while disconnected
    // (a submit button's click would otherwise submit its form natively).
    if (el.hasAttribute('az-prevent-default')) e.preventDefault();
    if (!host.ready()) return;
    const raw = el.getAttribute(attr);
    if (!raw) return;
    // The value is a command list by DECLARATION, so it is not inspected to decide
    // that. Command-versus-data is settled where it is provable -- at compile time
    // for a visible builder call, at render time for an opaque dynamic that
    // evaluates to a command (arizona_event_attrs) -- so a delegated name was
    // proved to carry commands somewhere. No value test here could stand in for
    // that: `arizona_effect:encode/1` emits a single command unwrapped
    // (`[0,"inc"]`), which makes an app's array of ids a structurally valid
    // command list.
    //
    // The try/catch is containment, not discrimination, and it must cover the
    // EXECUTION too: a proven name can still share the DOM with an app-data
    // attribute of the same name (`arizona.set_attr` writing `az-close`, host
    // markup), whose value parses but crashes the interpreter. Nothing may throw
    // out of a document listener -- it is per dispatch on types that can fire at
    // pointer-move rate -- hence warnOnce, keyed by the message so one bad
    // attribute reports once.
    try {
        const cmds = JSON.parse(raw);
        executeJS(el, e, withTransitionAttr(el, cmds));
    } catch (err) {
        const detail = err instanceof Error ? err.message : String(err);
        // Keyed by the attribute, not the message: a churning bad value would
        // otherwise mint a new entry per distinct error snippet.
        warnOnce(`[arizona] ${attr} did not run as a command list: ${detail}`, attr);
    }
}

/**
 * Delegate a DOM event type on `target` (a Document) via delegated listeners.
 * Bound to the supplied AbortSignal so all delegated listeners can be torn down
 * together. Key filtering is handled by the JS_ON_KEY command inside executeJS,
 * not by attribute name suffixes.
 *
 * Two listeners, split on the event's own `bubbles` flag, because one phase
 * cannot serve both kinds. Bubble phase keeps the delegated path unchanged for
 * every event that bubbles -- including letting an inner `stopPropagation()`
 * suppress an ancestor's command, which capture would override. Capture phase is
 * the only way to see a non-bubbling event at all (`toggle`, `close`, `play`,
 * `load`, `scrollend`, ...), since those never reach the document by bubbling.
 * @param {Document} target
 * @param {string} eventType
 * @param {AbortSignal} signal
 */
function handleEvent(target, eventType, signal) {
    // Hoisted out of the listener: both are pure functions of the type, and the
    // listener runs per dispatch on types that can fire at pointer-move rate.
    // CSS.escape because the type reaches here from a raw attribute name and the
    // HTML parser keeps `.` and `:` in one, so `az-turbo:load` would otherwise be
    // an invalid selector and throw out of the listener on every dispatch.
    const attr = `az-${eventType}`;
    const sel = `[${CSS.escape(attr)}]`;
    // The capture listener needs the flag and the bubble one does not: capture sees
    // EVERY event descending to the target, so without it a bubbling event would run
    // its command twice (once here, once on the way back up). The bubble phase is
    // skipped outright for a non-bubbling event, so that listener only ever sees the
    // kind it is for.
    /** @type {EventListener} */
    const capFn = (e) => {
        if (!e.bubbles) runDelegated(e, attr, sel, true);
    };
    /** @type {EventListener} */
    const bubFn = (e) => runDelegated(e, attr, sel, false);
    /** @type {AddEventListenerOptions} */
    const capOpts = { signal, capture: true };
    /** @type {AddEventListenerOptions} */
    const bubOpts = { signal };
    if (PASSIVE_FORCED.has(eventType)) {
        // Chrome makes a document-level wheel/mousewheel/touchstart/touchmove
        // listener passive whenever `passive` is UNSPECIFIED, which would turn
        // `az-prevent-default` into a silent no-op on exactly those four. Stating
        // it is therefore mandatory here, in both directions: `true` keeps the
        // scroll fast path for a page that only observes these events, `false` is
        // what makes preventDefault take effect on a page that declares it.
        // `noteAzAttrs` reads the directive out of a frame's set before binding
        // anything from that frame -- but the declaration can also arrive AFTER
        // the type is bound (a later delta frame, a runtime attribute write), so
        // a passive registration is recorded for `declarePreventDefault` to
        // rebind. Only the bubble listener needs recording: the four are all
        // NATIVELY_BUBBLING, so the capture listener below never binds for them.
        capOpts.passive = !_preventDefaultDeclared;
        bubOpts.passive = !_preventDefaultDeclared;
        if (!_preventDefaultDeclared) {
            let perDoc = _passiveBound.get(target);
            if (!perDoc) {
                perDoc = new Map();
                _passiveBound.set(target, perDoc);
            }
            perDoc.set(eventType, bubFn);
        }
    }
    // Capture exists only to reach events that cannot bubble. Registering it for a
    // type the platform always bubbles is worse than useless: it changes what those
    // types respond to (a shim's `new Event('change')` defaults to bubbles:false and
    // would now round-trip on views that opted into nothing), and on wheel/touchmove
    // it puts a document listener that can never do work in front of every scroll
    // frame.
    if (!NATIVELY_BUBBLING.has(eventType)) target.addEventListener(eventType, capFn, capOpts);
    target.addEventListener(eventType, bubFn, bubOpts);
}

/**
 * The event types Chrome forces passive on a Document, so `preventDefault` there
 * is ignored unless the listener opts out explicitly.
 */
const PASSIVE_FORCED = new Set(['wheel', 'mousewheel', 'touchstart', 'touchmove']);

/**
 * Types the platform always dispatches with `bubbles: true`, so the bubble phase
 * already sees every real one and a capture listener would only add reach. The
 * bootstrap seven plus the forced-passive four.
 */
const NATIVELY_BUBBLING = new Set([
    'click',
    'change',
    'input',
    'keydown',
    'keyup',
    'focusin',
    'focusout',
    'wheel',
    'mousewheel',
    'touchstart',
    'touchmove',
]);

/**
 * Has the app declared `az-prevent-default` anywhere? Read when a forced-passive
 * type is bound, which is why the whole name set is scanned for it before any
 * type is delegated: `passive` is fixed at registration, and re-registering to
 * change it would leave the listener behind an app listener added in between.
 */
let _preventDefaultDeclared = false;

/**
 * The forced-passive bubble listeners currently bound, per document, so a late
 * `az-prevent-default` can rebind them non-passive. Passive is fixed at
 * registration; entries are dropped once rebound (the flag never flips back).
 * @type {Map<Document, Map<string, EventListener>>}
 */
export const _passiveBound = new Map();

/**
 * Record the `az-prevent-default` declaration, re-registering any
 * forced-passive listener that was bound before it arrived -- otherwise a page
 * whose declaration lands in a later frame keeps ignoring the preventDefault
 * it asked for.
 */
function declarePreventDefault() {
    if (_preventDefaultDeclared) return;
    _preventDefaultDeclared = true;
    for (const [doc, perDoc] of _passiveBound) {
        const rec = _eventDocs.get(doc);
        if (rec) {
            for (const [type, fn] of perDoc) {
                doc.removeEventListener(type, fn);
                doc.addEventListener(type, fn, { signal: rec.signal, passive: false });
            }
        }
    }
    _passiveBound.clear();
}

/**
 * `az-*` suffixes that are framework directives rather than DOM event types.
 *
 * Only `submit` and `drop` are load-bearing: they name real DOM events AND have
 * their own dedicated listeners below (submit flushes debounced inputs, honors
 * `az-form-reset` and defers to `fetch`; drop carries the drag bookkeeping), so
 * delegating them generically would run their commands a second time.
 *
 * The rest name no DOM event at all, so binding one would only ever register a
 * listener that never fires. They are listed as hygiene, not as a safety net --
 * forgetting one is harmless, and only a future directive named after a real DOM
 * event would need to be added here.
 */
const AZ_DIRECTIVES = new Set([
    'submit',
    'drop',
    'view',
    'local',
    'key',
    'hook',
    'target',
    'debounce',
    'throttle',
    'transition',
    'prevent-default',
    'form-reset',
    'noscroll',
    'nodiff',
    'navigate',
    'patch',
]);

/**
 * The az-* DOM events delegated so far. Bootstrapped with the types every app
 * uses, because the server's name set arrives one round trip after the socket
 * opens and a click in that window must not find an empty document. Grown by
 * `noteAzAttrs` from the set, and by `noteAzAttr` from a runtime attribute write.
 * Monotonic for the life of the page.
 */
const _eventTypes = new Set([
    'click',
    'change',
    'input',
    'keydown',
    'keyup',
    'focusin',
    'focusout',
]);

/**
 * Documents currently bound, and the tear-down for the ones whose listeners hang
 * off a controller the connection does not own (a PiP window's).
 * @type {Map<Document, {signal: AbortSignal, abort?: () => void}>}
 */
export const _eventDocs = new Map();

/**
 * Delegate the app's whole `az-*` vocabulary, as collected by the parse transform
 * and unioned by the server. Arrives once, on the connect frame, so an
 * `az-<event>` attribute works for ANY event type with no list to maintain --
 * including a custom element's own (`az-sl-change` binds `sl-change`). The
 * suffix is used verbatim as the `addEventListener` type, so there is no mapping
 * table to keep in sync with the platform.
 *
 * `az-prevent-default` is read out of the set FIRST. It decides `passive` for the
 * four types the platform forces, and `passive` is fixed when a listener is
 * registered -- so it has to be known before this loop binds one of them.
 * @param {string[]} names raw `az-*` attribute names, already lowercase
 */
export function noteAzAttrs(names) {
    if (names.includes('az-prevent-default')) declarePreventDefault();
    for (const name of names) noteAzAttr(name);
}

/**
 * Delegate one `az-*` attribute NAME. The single place the directive/event triage
 * lives, so the server's set and a runtime attribute write cannot drift apart.
 * @param {string} name
 */
export function noteAzAttr(name) {
    if (!name.startsWith('az-')) return;
    // Attribute names are ASCII-lowercased by the HTML parser, and
    // `addEventListener` types are case-sensitive, so an unlowered `az-Close`
    // would bind a dead `Close` while the DOM stores `az-close`.
    const type = name.slice(3).toLowerCase();
    if (type === 'prevent-default') declarePreventDefault();
    else if (!_eventTypes.has(type) && !AZ_DIRECTIVES.has(type)) bindEventType(type);
}

/**
 * Delegate `type` on every bound document. Idempotent: `_eventTypes` gates the
 * call, and every bound document has every type in it.
 * @param {string} type
 */
function bindEventType(type) {
    _eventTypes.add(type);
    for (const [doc, rec] of _eventDocs) handleEvent(doc, type, rec.signal);
}

/**
 * Discover the DOM events `root` declares and delegate any not already bound.
 *
 * ONLY for markup the server never rendered: a host app that inserts its own
 * `az-*` markup and calls the exported `mountHooks` for it. Server-rendered
 * markup needs nothing here -- its names are in the compile-time set, which
 * arrives before any of it can be patched in.
 *
 * `getAttributeNames()` on real DOM, so this is exact: the HTML parser already
 * decided what is an attribute name and what is text inside a value.
 * @param {Element|Document} root
 */
function scanEvents(root) {
    const doc = root.nodeType === 9 ? /** @type {Document} */ (root) : root.ownerDocument;
    if (!doc) return;
    const walker = doc.createTreeWalker(root, NodeFilter.SHOW_ELEMENT);
    /** @type {Node|null} */
    let node = walker.currentNode;
    for (; node; node = walker.nextNode()) {
        const el = /** @type {Element} */ (node);
        // getAttributeNames() rather than walking `attributes`: the NamedNodeMap
        // accessor is far slower, and this runs per element. hasAttributes first,
        // so the (typical) attribute-less element costs no array allocation.
        if (el.hasAttributes?.()) for (const name of el.getAttributeNames()) noteAzAttr(name);
    }
}

/**
 * Whether an [az-key] item drag is in flight: set on its `dragstart`, cleared
 * on `dragend` or when an az-drop claims the drop. Module-wide, not per
 * document, so an item dragged between the main page and a popped-out view is
 * still recognised.
 */
let _itemDrag = false;

/**
 * Tear down every bound document's delegated listeners and drop the bookkeeping:
 * a page's teardown. The caller's own signal covers the MAIN document only; a PiP
 * document's listeners hang off requestPip's own controller, so without this they
 * would survive and the next start re-arm them. `_eventTypes` deliberately
 * survives: it is what this PAGE delegates, so a restart rebinds the same types.
 */
export function unbindDocuments() {
    for (const rec of _eventDocs.values()) rec.abort?.();
    _eventDocs.clear();
    _passiveBound.clear();
    _itemDrag = false;
}

/**
 * Bind every delegated DOM event on `target` (a Document): the az-* event types
 * plus form submit and drag-and-drop. Called for the main document on connect,
 * and for a PiP document when a view is popped out. Page-level concerns
 * (az-navigate, popstate, scroll) are wired only for the main document.
 * @param {Document} target
 * @param {AbortSignal} signal
 * @param {(() => void)} [abort] tear-down for documents whose listeners hang off a
 *   controller the connection does not own (a PiP window's).
 */
export function bindDocumentEvents(target, signal, abort) {
    _eventDocs.set(target, { signal, abort });
    for (const type of _eventTypes) handleEvent(target, type, signal);

    // Form submission: flush any pending debounced/throttled inputs first so
    // the server sees final values, then execute JS commands from az-submit.
    // az-form-reset opts in to clearing the form after submit.
    target.addEventListener(
        'submit',
        (e) => {
            const form = /** @type {Element} */ (e.target).closest('[az-submit]');
            if (!form) return;
            // Unconditionally, before anything can return: an az-submit form that is
            // let through submits natively with its own method -- a GET unless it
            // says otherwise -- which puts every field, a password included, in the
            // URL. Before the first open and during reconnect backoff that used to
            // be exactly what happened.
            e.preventDefault();
            const raw = form.getAttribute('az-submit');
            /** @type {{event?: boolean, fetch?: boolean, nav?: Array<*>}} */
            let needs = {};
            try {
                const cmds = raw ? JSON.parse(raw) : null;
                if (cmds) needs = socketNeeds(cmds, {}, false);
                // While the socket is down (the worker drops pre-open sends):
                // - an SPA navigation alone degrades to a full load of its URL, as a
                //   disconnected az-navigate click does;
                // - anything else needing the socket drops the submission whole rather
                //   than run it in part, keeping the typed fields (no reset), with a
                //   warning and `arizona:submit-dropped` on the form for app feedback.
                //   A navigation beside a fetch drops too: the load would cancel it.
                // - a `fetch` needs no socket and runs, so an HTTP login form works
                //   before the first open.
                if (!host.ready() && needs.nav && !needs.event && !needs.fetch) {
                    host.leavePage(needs.nav[1], needs.nav[2]?.replace);
                    return;
                }
                if (!host.ready() && (needs.event || needs.nav)) {
                    console.warn('[arizona] az-submit dropped: disconnected');
                    form.dispatchEvent(
                        new CustomEvent('arizona:submit-dropped', {
                            bubbles: true,
                            detail: { reason: 'disconnected' },
                        }),
                    );
                    return;
                }
                // Flush pending debounced inputs so the server sees them before the
                // submit. Only while connected: a flush now would send them into a
                // closed socket, while a still-armed timer can deliver them later.
                if (host.ready())
                    form.querySelectorAll('[az-debounce],[az-throttle]').forEach(flushTimer);
                if (cmds) executeJS(form, e, withTransitionAttr(form, cmds));
            } catch (err) {
                const detail = err instanceof Error ? err.message : String(err);
                warnOnce(
                    `[arizona] az-submit did not run as a command list: ${detail}`,
                    'az-submit',
                );
            }
            // A fetch command resets on its own 2xx response (so a validation error
            // keeps the typed fields); everything else resets synchronously here.
            if (!needs.fetch) maybeResetForm(form);
        },
        { signal },
    );

    // Drag-and-drop: uses az-key on draggable items and az-drop on the
    // container. dragstart stores the item's key; drop executes the az-drop
    // command with auto-collected {data_transfer, drop_index} payload. The drop
    // zone is the az-drop container itself, not its items, so an empty container
    // accepts a drop (drop_index -1). Only an item drag is claimed: `dragover`
    // and `drop` are cancelled inside a container, and only while a drag that
    // began on an [az-key] is in flight -- a text or file drop (into an input in
    // the container's header, onto blank space, or anywhere outside one) is the
    // browser's own and is left alone.
    target.addEventListener(
        'dragstart',
        (e) => {
            const keyEl = /** @type {Element} */ (e.target).closest('[az-key]');
            if (keyEl && e.dataTransfer) {
                e.dataTransfer.setData('text/plain', keyEl.getAttribute('az-key') || '');
                _itemDrag = true;
            }
        },
        { signal },
    );
    // Fires on the source after a drop, a cancel (Escape) or a drop outside.
    target.addEventListener(
        'dragend',
        () => {
            _itemDrag = false;
        },
        { signal },
    );
    target.addEventListener(
        'dragover',
        (e) => {
            if (_itemDrag && /** @type {Element} */ (e.target).closest('[az-drop]'))
                e.preventDefault();
        },
        { signal },
    );
    target.addEventListener(
        'drop',
        (e) => {
            if (!_itemDrag) return;
            const container = /** @type {Element} */ (e.target).closest('[az-drop]');
            if (!container) return;
            _itemDrag = false;
            e.preventDefault();
            if (!host.ready()) return;
            const raw = container.getAttribute('az-drop');
            if (!raw) return;
            try {
                executeJS(container, e, withTransitionAttr(container, JSON.parse(raw)));
            } catch (err) {
                const detail = err instanceof Error ? err.message : String(err);
                warnOnce(`[arizona] az-drop did not run as a command list: ${detail}`, 'az-drop');
            }
        },
        { signal },
    );
}
