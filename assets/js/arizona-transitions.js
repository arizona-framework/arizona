import { JS_TRANSITION } from './arizona-commands.js';

/**
 * Whether a view transition should run now: the API exists and the user has
 * not asked to reduce motion. Guarded for jsdom, which lacks matchMedia.
 * @returns {boolean}
 */
function canTransition() {
    if (typeof document.startViewTransition !== 'function') return false;
    if (!window.matchMedia) return true;
    return !window.matchMedia('(prefers-reduced-motion: reduce)').matches;
}

/**
 * Whether the browser supports the view-transition `types` option (the
 * object-form startViewTransition + :active-view-transition-type selector).
 * Older View-Transition browsers still cross-fade; they just ignore types.
 * @returns {boolean}
 */
function supportsVTTypes() {
    return (
        typeof CSS !== 'undefined' &&
        !!CSS.supports &&
        CSS.supports('selector(:active-view-transition-type(x))')
    );
}

/**
 * Run `fn` (a DOM mutation) inside a view transition when possible, else run
 * it directly. Uses the object form to pass `types` when supported.
 * @param {{types?: string[]}|null} opts
 * @param {() => void} fn
 */
export function runTransition(opts, fn) {
    if (!canTransition()) {
        fn();
        return;
    }
    const types = opts?.types;
    const vt =
        types?.length && supportsVTTypes()
            ? document.startViewTransition({ update: fn, types })
            : document.startViewTransition(fn);
    // `ready` rejects when the transition is skipped -- a duplicate
    // view-transition-name, or interruption by a newer transition (rapid nav).
    // The DOM still updates; swallow it so it isn't an unhandled rejection.
    vt?.ready?.catch(() => {});
}

/**
 * Read the `az-transition` attribute into a transition opts object, or null if
 * absent. The value is a space-separated list of view-transition type names
 * (like `class`); tokens are trimmed and empties dropped, so a bare attribute
 * or stray whitespace yields an empty `types` list (plain cross-fade).
 * @param {Element} el
 * @returns {{types: string[]}|null}
 */
export function parseTransitionAttr(el) {
    if (!el.hasAttribute('az-transition')) return null;
    const raw = el.getAttribute('az-transition') || '';
    return { types: raw.split(/\s+/).filter(Boolean) };
}

/**
 * Wrap an element's parsed event commands in a synthetic transition command when
 * the element carries `az-transition`, so the attribute animates whatever those
 * commands do (a client effect, a navigate, or a push_event) -- not just links.
 * A no-op without the attribute.
 * @param {Element} el
 * @param {Array<*>} cmds
 * @returns {Array<*>}
 */
export function withTransitionAttr(el, cmds) {
    const t = parseTransitionAttr(el);
    return t ? [JS_TRANSITION, t, cmds] : cmds;
}
