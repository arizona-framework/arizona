import { resolveTarget } from './arizona-commands.js';
import { host } from './arizona-host.js';

// ---------------------------------------------------------------------------
// Hook system -- element lifecycle hooks via az-hook attribute
//
// Hooks let client-side JS interact with server-managed elements. Register
// hook definitions before calling connect(). Any element with az-hook="Name"
// is tracked and receives lifecycle callbacks as the server patches the DOM.
//
// Each callback receives `this` bound to the hook instance:
//   this.el           -- the DOM element
//   this.pushEvent(name, payload) -- send an event to the server's
//       handle_event/3, routed to the element's enclosing az-view
//
// Lifecycle:
//   mounted()   -- element entered the DOM (SSR hydration, OP_INSERT,
//                 OP_REPLACE, or OP_TEXT with new content)
//   updated()   -- element stayed in the DOM but its attributes or inner
//                 content changed (OP_SET_ATTR, OP_REM_ATTR, OP_TEXT)
//   destroyed() -- element is about to be removed (OP_REMOVE_NODE, OP_REMOVE,
//                 OP_REPLACE, OP_TEXT). Called before the DOM mutation --
//                 this.el is still attached
//
// A hook's own methods are reachable as `this.method()` from any lifecycle
// callback (the instance's prototype is the hook def), and state assigned to
// `this` is per-instance -- so shared logic factors into a helper. Example -- a
// chart hook that draws on mount and redraws on update via its own draw(),
// cleans up on destroy, and notifies the server it's ready:
//
//   import { hooks, connect } from './arizona.js';
//   hooks.Chart = {
//       mounted() {
//           this.chart = new ChartLib(this.el);   // per-instance state
//           this.draw();
//           this.pushEvent('chart_ready', { width: this.el.offsetWidth });
//       },
//       updated()   { this.draw(); },
//       destroyed() { this.chart.destroy(); },
//       draw()      { this.chart.render(this.el.dataset); }, // own helper method
//   };
//   connect('/ws');
//
// On the server, handle_event/3 receives the pushed event:
//
//   handle_event(<<"chart_ready">>, #{<<"width">> := W}, Bindings) ->
//       {Bindings#{chart_width => W}, []}.
// ---------------------------------------------------------------------------

/** @type {Object<string, {mounted?: function, updated?: function, destroyed?: function} & Object<string, *>>} */
export const hooks = {};

// The instance's prototype is the hook def (see mountHook), so beyond the base
// fields it also exposes the def's helper methods plus arbitrary per-instance state.
/** @type {Map<Element, {el: Element, __name: string, pushEvent: function} & Object<string, *>>} */
export const _hooks = new Map();

/**
 * Mount a hook on an element with az-hook. Creates an instance, stores it,
 * and calls mounted() if defined. Skips if already tracked or hook not registered.
 *
 * The instance's prototype IS the hook definition, so a hook's own helper
 * methods are reachable as `this.method()` from any lifecycle callback.
 * el/__name/pushEvent are assigned as OWN properties -- they shadow the
 * def and stay framework-owned -- and any per-instance state a hook assigns to
 * `this` (e.g. `this.chart` in mounted) is an own property too, so it never
 * writes through to the shared def or leaks across instances.
 * @param {Element} el
 */
export function mountHook(el) {
    if (_hooks.has(el)) return;
    const name = el.getAttribute('az-hook');
    if (!name || !hooks[name]) return;
    const def = hooks[name];
    const instance = Object.create(def);
    instance.el = el;
    instance.__name = name;
    /** @param {string} eventName @param {*} payload */
    instance.pushEvent = (eventName, payload) => {
        host.sendEvent(eventName, JSON.stringify([resolveTarget(el), eventName, payload || {}]));
    };
    _hooks.set(el, instance);
    if (def.mounted) runHookCallback(instance, def.mounted, 'mounted');
}

/**
 * Invoke a hook lifecycle callback in isolation: a throwing user hook is logged
 * and swallowed so it can't abort the op batch it runs inside (the server
 * snapshot has already advanced, so a bubbling throw would desync the DOM).
 * @param {{__name: string} & Object<string, *>} instance
 * @param {Function} cb
 * @param {string} phase
 */
function runHookCallback(instance, cb, phase) {
    try {
        cb.call(instance);
    } catch (err) {
        // Static format string: a hook name / op code goes in an ARGUMENT, never in
        // the format position, where a `%s` in it would consume the `err` that
        // follows (and `%c` would restyle the line) -- see native_client.js.
        console.error('[arizona] hook %s %s() threw', instance.__name, phase, err);
    }
}

/**
 * Run hook lifecycle callback `phase` on `el` if it is tracked and the
 * hook def exports that phase.
 * @param {Element} el
 * @param {'mounted'|'updated'|'destroyed'} phase
 */
function runHookPhase(el, phase) {
    const instance = _hooks.get(el);
    if (!instance) return;
    const def = hooks[instance.__name];
    if (def?.[phase]) runHookCallback(instance, def[phase], phase);
}

/**
 * Destroy a hook on an element. Calls destroyed() if defined, then removes from tracking.
 * @param {Element} el
 */
export function destroyHook(el) {
    runHookPhase(el, 'destroyed');
    _hooks.delete(el);
}

/**
 * Call updated() on a hooked element if it is tracked.
 * @param {Element} el
 */
export function notifyUpdated(el) {
    runHookPhase(el, 'updated');
}

/**
 * Mount hooks on all [az-hook] elements within root (inclusive). `nodeType`, not
 * `instanceof Element`: root may come from a PiP document, whose realm has its own
 * Element constructor (a Document is nodeType 9, so it still skips the self-check).
 * @param {Element|Document} root
 */
export function mountHooks(root) {
    // Nothing to mount against: `mountHook` no-ops without a def, so the scan below can
    // only find elements it will ignore. Worth answering first because this runs per
    // ELEMENT per op -- a slot re-render of a 500-item list is 500 subtree queries for
    // one op, and an app with no hooks at all pays every one of them.
    if (!anyHookDefs()) return;
    if (root.nodeType === 1 && /** @type {Element} */ (root).hasAttribute('az-hook'))
        mountHook(/** @type {Element} */ (root));
    root.querySelectorAll('[az-hook]').forEach(mountHook);
}

/**
 * Run destroyed() on every tracked hook and forget them: a page's teardown.
 * Without it, instances leak when host code removes the DOM by means Arizona did
 * not observe (third-party libs, a test's `document.body.innerHTML = ''`).
 */
export function destroyAllHooks() {
    for (const el of [..._hooks.keys()]) destroyHook(el);
}

/**
 * Is any hook definition registered? `hooks` is a plain object apps assign onto, so
 * there is nothing to cache against; `for...in` over an empty object allocates nothing
 * and returns immediately.
 * @returns {boolean}
 */
export function anyHookDefs() {
    for (const _name in hooks) return true;
    return false;
}

/**
 * Destroy hooks on descendant [az-hook] elements only (not root itself).
 * Used by ops that replace inner content but keep the element (TEXT).
 * @param {Element} root
 */
export function destroyChildHooks(root) {
    // `destroyHook` only acts on elements present in `_hooks`, so with nothing mounted
    // the scan cannot find anything to destroy. Same per-element-per-op frequency as
    // `mountHooks`.
    if (_hooks.size === 0) return;
    root.querySelectorAll('[az-hook]').forEach((el) => {
        destroyHook(el);
    });
}

/**
 * Destroy hooks on root and all descendant [az-hook] elements.
 * Used by ops that remove the element itself (REPLACE, REMOVE_NODE, REMOVE).
 * @param {Element} root
 */
export function destroyHooks(root) {
    destroyHook(root);
    destroyChildHooks(root);
}
