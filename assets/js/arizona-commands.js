import { applyRemAttrOp, applySetAttrOp } from './arizona-attrs.js';
import { allDocs, exitPip, requestPip } from './arizona-docs.js';
import { notifyUpdated } from './arizona-hooks.js';
import { host } from './arizona-host.js';
import { set, setAll } from './arizona-locals.js';
import { runTransition } from './arizona-transitions.js';
import { warnOnce } from './arizona-util.js';

/**
 * Append an encoded query to a URL, before its fragment: everything after the first
 * `#` is the fragment, which fetch() never sends, so a query appended there is lost.
 * Spliced rather than rebuilt through `URL`, which would make a relative URL absolute
 * and re-serialize a query the caller already encoded.
 * @param {string} url
 * @param {string} qs
 * @returns {string}
 */
function appendQuery(url, qs) {
    const at = url.indexOf('#');
    const base = at < 0 ? url : url.slice(0, at);
    const sep = !base.includes('?') ? '?' : /[?&]$/.test(base) ? '' : '&';
    return base + sep + qs + (at < 0 ? '' : url.slice(at));
}

/**
 * A GET/HEAD `fetch` body as query parameters: one per map key, a list repeating
 * its key, a `null` left out, and a nested map (or a map inside a list) JSON-encoded
 * since a query has no nesting. Anything but a map has no keys to name parameters
 * by, so it is refused loudly rather than guessed at.
 * @param {unknown} body
 * @returns {URLSearchParams}
 */
function bodyQuery(body) {
    const params = new URLSearchParams();
    if (body === null || typeof body !== 'object' || Array.isArray(body)) {
        console.warn('[arizona] GET fetch body is not a map, not sent:', body);
        return params;
    }
    for (const [key, value] of Object.entries(body)) {
        for (const item of Array.isArray(value) ? value : [value]) {
            if (item == null) continue;
            params.append(key, typeof item === 'object' ? JSON.stringify(item) : String(item));
        }
    }
    return params;
}

/**
 * Process server-side effects. Same op codes as JS commands (arizona_js.hrl).
 * Each effect is executed through the unified executeJS interpreter.
 * @param {Array<Array<*>>} effects
 */
export function applyEffects(effects) {
    for (const eff of effects) {
        // Isolate each effect: `applyOps` already isolates per op, so this is the only
        // step that can throw out of the worker's message handler, where it would skip
        // the `restoreFormState()` queued behind it and discard the user's typed input
        // on the very reconnect meant to preserve it. Event-attribute commands stay
        // unisolated on purpose -- they run inside a DOM listener, where a throw is
        // reported and costs nothing, and a bad selector or on_key regex there is a
        // developer error that should stay loud.
        try {
            executeJS(document.documentElement, null, eff);
        } catch (err) {
            console.error(
                '[arizona] effect %s failed; skipping',
                Array.isArray(eff[0]) ? eff[0][0] : eff[0],
                err,
            );
        }
    }
}

/**
 * Determine which server-side view handles this element's events.
 * Explicit az-target takes priority; otherwise walk up to the nearest az-view.
 * @param {Element} el
 * @returns {string|null}
 */
export function resolveTarget(el) {
    return el.getAttribute('az-target') || el.closest('[az-view]')?.id || null;
}

/**
 * The submit button that initiated a submit event, or null. Passed as the
 * second arg to `new FormData(form, submitter)` so a form with multiple named
 * submit buttons reports which one fired (its name/value in the field data),
 * matching a native form POST. A non-submit event (a click) has no submitter,
 * so a plain button never drags the form's fields along -- gathering stays a
 * property of submitting the form, not of the trigger element.
 * @param {Event|null} event
 * @returns {HTMLElement|null}
 */
function submitter(event) {
    return /** @type {any} */ (event)?.submitter ?? null;
}

/**
 * The names in `form` that hold a list by their markup: a `<select multiple>`,
 * a checkbox name shared by more than one box (a group), and any field whose
 * name ends in `[]` -- the convention for a list whose rendered size can be one
 * (a data-driven group showing a single box), which markup alone cannot tell
 * apart from a single field. Only enabled ones, since FormData leaves a
 * disabled control out entirely; buttons never count (only the submitter is
 * submitted, and it is a single value).
 * @param {HTMLFormElement} form
 * @returns {Set<string>}
 */
function multiValuedNames(form) {
    /** @type {Set<string>} */
    const names = new Set();
    /** @type {Map<string, {boxes: number, enabled: boolean}>} */
    const groups = new Map();
    for (const el of form.elements) {
        const f = /** @type {any} */ (el);
        const name = f.name;
        if (!name) continue;
        // `:disabled` is a selector match, asked only of a field that can hold a
        // list: a form payload runs this walk on every event the form raises.
        if (name.endsWith('[]') ? isSubmittableField(el) : el.tagName === 'SELECT' && f.multiple) {
            if (!el.matches(':disabled')) names.add(name);
        } else if (el.tagName === 'INPUT' && f.type === 'checkbox') {
            const g = groups.get(name) || { boxes: 0, enabled: false };
            g.boxes += 1;
            g.enabled ||= !el.matches(':disabled');
            groups.set(name, g);
        }
    }
    for (const [name, g] of groups) if (g.boxes > 1 && g.enabled) names.add(name);
    return names;
}

/**
 * Whether `el` contributes its own value to FormData: an input other than a
 * button-like one, a select or a textarea.
 * @param {Element} el
 * @returns {boolean}
 */
function isSubmittableField(el) {
    if (el.tagName === 'SELECT' || el.tagName === 'TEXTAREA') return true;
    if (el.tagName !== 'INPUT') return false;
    const t = /** @type {HTMLInputElement} */ (el).type;
    return t !== 'submit' && t !== 'button' && t !== 'image' && t !== 'reset';
}

/**
 * Collect a form's fields, keeping a duplicate name as an array. A checkbox
 * group and a `<select multiple>` each submit one entry per selected value, so
 * the flat `Object.fromEntries` shape silently keeps only the last of them --
 * and those two, plus any `name[]` field, are arrays even with one value or
 * none (`[]`), chosen by the field's markup rather than by how many values
 * happen to be selected, so a handler sees one shape. The key is the name as
 * written, brackets included, as a native form POST sends it.
 * @param {FormData} fd
 * @param {HTMLFormElement} form
 * @returns {Object<string, string|string[]>}
 */
export function formFields(fd, form) {
    // Prototype-less: a field name is author data, and on a plain object one named
    // `constructor` or `toString` would read back the inherited method (`in` is
    // true, so the value became `[fn, v]`) and `__proto__` would hit the setter.
    /** @type {Object<string, string|string[]>} */
    const data = Object.create(null);
    for (const name of multiValuedNames(form)) data[name] = [];
    for (const [k, v] of fd.entries()) {
        if (k in data) {
            const prev = data[k];
            // Every array here was built by this function, so it is extended in
            // place: a long list (a big multiple select) stays linear.
            if (Array.isArray(prev)) prev.push(/** @type {string} */ (v));
            else data[k] = [prev, /** @type {string} */ (v)];
        } else {
            data[k] = /** @type {string} */ (v);
        }
    }
    return data;
}

/**
 * Auto-collect payload from an element based on its type and event context.
 * Drop -> {data_transfer, drop_index}, Forms -> FormData (incl. the submitter),
 * inputs/selects/textareas -> {value}, otherwise -> {}.
 * @param {Element} el
 * @param {Event|null} event
 * @returns {Object<string, *>}
 */
function autoPayload(el, event) {
    if (event && /** @type {any} */ (event).dataTransfer) {
        // The container's own child the drop landed in -- not the nearest
        // [az-key], which may be a keyed descendant of an item (a nested list)
        // or, on an empty container, an ancestor of the container itself.
        let item = /** @type {Element|null} */ (event.target);
        while (item && item.parentElement !== el) item = item.parentElement;
        const children = Array.from(el.querySelectorAll(':scope > [az-key]'));
        return {
            data_transfer: /** @type {any} */ (event).dataTransfer.getData('text/plain'),
            drop_index: item ? children.indexOf(item) : -1,
        };
    }
    const tag = el.tagName;
    if (tag === 'FORM')
        return formFields(
            new FormData(/** @type {HTMLFormElement} */ (el), submitter(event)),
            /** @type {HTMLFormElement} */ (el),
        );
    if (tag === 'INPUT' || tag === 'SELECT' || tag === 'TEXTAREA') {
        const field = /** @type {any} */ (el);
        // A checkbox/radio reports what it WOULD submit, which is the same string
        // whether or not it is on, so its state has to ride alongside the value.
        if (field.type === 'checkbox' || field.type === 'radio')
            return { value: field.value || '', checked: field.checked };
        // `HTMLSelectElement.value` is only the FIRST selected option, so a multiple
        // select would report one of its values and silently drop the rest.
        if (tag === 'SELECT' && field.multiple)
            return { value: Array.from(field.selectedOptions, (o) => o.value) };
        return { value: field.value || '' };
    }
    return {};
}

// JS command op codes -- must match include/arizona_effect.hrl
export const JS_PUSH_EVENT = 0,
    JS_TOGGLE = 1,
    JS_SHOW = 2,
    JS_HIDE = 3,
    JS_ADD_CLASS = 4,
    JS_REMOVE_CLASS = 5,
    JS_TOGGLE_CLASS = 6,
    JS_SET_ATTR = 7,
    JS_REMOVE_ATTR = 8,
    JS_DISPATCH_EVENT = 9,
    JS_NAVIGATE = 10,
    JS_FOCUS = 11,
    JS_BLUR = 12,
    JS_SCROLL_TO = 13,
    JS_SET_TITLE = 14,
    JS_RELOAD = 15,
    JS_ON_KEY = 16,
    JS_SET_LOCAL = 17,
    JS_REQUEST_PIP = 18,
    JS_EXIT_PIP = 19,
    JS_TRANSITION = 20,
    JS_TOGGLE_ATTR = 21,
    JS_FETCH = 22,
    JS_OS = 23,
    JS_PATCH = 24,
    JS_RESET_FORM = 25,
    JS_SELECT = 26,
    JS_COPY_TO_CLIPBOARD = 27,
    JS_SHOW_MODAL = 28,
    JS_CLOSE_MODAL = 29;

// arizona_js credentials atoms -> fetch() credentials mode
/** @type {Record<string, RequestCredentials>} */
const CREDENTIALS = { same_origin: 'same-origin', include: 'include', omit: 'omit' };

/**
 * The native-shell bridge the embedding shell (Electron/Tauri/...) installs on
 * the page before connect(); `undefined` in a plain browser, where every OS
 * command/capability is a safe no-op.
 * @returns {{
 *   capabilities?: Record<string, unknown>,
 *   invoke?: (name: string, args: unknown[]) => Promise<unknown>,
 *   onEvent?: (cb: (name: string, payload: any) => void) => void,
 * } | undefined}
 */
export function osHost() {
    return /** @type {any} */ (globalThis).__arizona_os__;
}

/**
 * Reset a form after a successful az-submit, when it opted in with `az-form-reset`.
 * Shared by the submit listener (synchronous, for push_event-style commands) and the
 * `fetch` command (deferred to a 2xx response, so the fields survive a validation error).
 * @param {Element} form
 */
export function maybeResetForm(form) {
    if (form.hasAttribute('az-form-reset')) /** @type {HTMLFormElement} */ (form).reset();
}

/**
 * What a parsed command (single or list) needs the WebSocket for, so a submit made
 * while it is down can be settled up front: `event` (a `push_event`), `fetch`, and
 * `nav`, the first SPA `navigate`/`patch` that runs unconditionally (a `full`
 * navigate is a plain page load and needs no socket). A navigation under `on_key`
 * runs only on a matching key, so it counts as `event`: it cannot be degraded to a
 * page load that may never have been asked for. `fetch` also tells the submit
 * listener to leave `az-form-reset` to the fetch's own response, wherever the fetch
 * sits (a transition runs it inside its callback, an on_key on a matching key).
 * @param {Array<*>} cmds
 * @param {{event?: boolean, fetch?: boolean, nav?: Array<*>}} acc
 * @param {boolean} conditional
 * @returns {{event?: boolean, fetch?: boolean, nav?: Array<*>}}
 */
export function socketNeeds(cmds, acc, conditional) {
    const list = Array.isArray(cmds[0]) ? cmds : [cmds];
    for (const c of list) {
        if (c[0] === JS_PUSH_EVENT) acc.event = true;
        else if (c[0] === JS_FETCH) acc.fetch = true;
        else if (c[0] === JS_PATCH || (c[0] === JS_NAVIGATE && !c[2]?.full)) {
            if (conditional) acc.event = true;
            else acc.nav ??= c;
        } else if (c[0] === JS_TRANSITION) socketNeeds(c[2], acc, conditional);
        else if (c[0] === JS_ON_KEY) socketNeeds(c[2], acc, true);
    }
    return acc;
}

/**
 * Call `fn` with the FIRST element matching `sel` (cast to `HTMLElement`),
 * searched across the main document and any PiP documents. First-match only --
 * the single-target effects `focus`/`blur`/`scroll_to` use this, since those act
 * on one element by definition. The broadcast effects use `withQueryAll`.
 * @param {string} sel
 * @param {(el: HTMLElement) => void} fn
 */
function withQuery(sel, fn) {
    for (const doc of allDocs()) {
        const t = /** @type {HTMLElement|null} */ (doc.querySelector(sel));
        if (t) {
            fn(t);
            return;
        }
    }
}

/**
 * Call `fn` for EVERY element matching `sel`, across the main document and any
 * PiP documents. Used by the broadcast effects (`toggle`/`show`/`hide`/
 * `*_class`/`*_attr`), so a selector matching several elements affects them all
 * -- honoring the builders' "all elements matching the selector" contract.
 * @param {string} sel
 * @param {(el: HTMLElement) => void} fn
 */
function withQueryAll(sel, fn) {
    for (const doc of allDocs()) {
        for (const t of doc.querySelectorAll(sel)) {
            fn(/** @type {HTMLElement} */ (t));
        }
    }
}

/**
 * Execute JS commands from an az-* attribute value.
 * Single command: [opcode, ...args]
 * Multiple commands: [[opcode, ...args], [opcode, ...args]]
 * @param {Element} el - the element that triggered the event
 * @param {Event|null} event - the DOM event (null for programmatic dispatch)
 * @param {Array<*>} cmds - parsed JSON command(s)
 */
export function executeJS(el, event, cmds) {
    runCommands(el, event, cmds, '');
}

/**
 * `executeJS` with the position of `cmds` in the attribute's command tree: `prefix`
 * is the index path of the wrapper (transition/on_key) holding them, '' at the
 * top level. A debounced/throttled push_event keys its timer by that position.
 * @param {Element} el
 * @param {Event|null} event
 * @param {Array<*>} cmds
 * @param {string} prefix
 */
function runCommands(el, event, cmds, prefix) {
    const commands = Array.isArray(cmds[0]) ? cmds : [cmds];
    for (let i = 0; i < commands.length; i++) execOne(el, event, commands[i], prefix, i);
}

/**
 * Execute a single JS command (the per-command body extracted from executeJS).
 * @param {Element} el
 * @param {Event|null} event
 * @param {Array<*>} cmd
 * @param {string} [prefix] index path of the wrapper holding `cmd` (see runCommands)
 * @param {number} [index] position of `cmd` in its list
 */
function execOne(el, event, cmd, prefix = '', index = 0) {
    const op = cmd[0];
    switch (op) {
        case JS_TRANSITION: {
            // Wrap the inner command(s)' DOM change in a view transition. A sync
            // effect (toggle/add_class/...) wraps in place; navigate/push_event
            // produce a future server diff, so stash the intent and let the
            // worker message handler wrap the matching batch.
            const opts = cmd[1] || {};
            const inner = cmd[2];
            const innerCmds = /** @type {Array<Array<*>>} */ (
                Array.isArray(inner[0]) ? inner : [inner]
            );
            // A navigate/patch that sends no frame (a hash jump, a page load)
            // has no reply to wait for; a hash jump then wraps in place.
            const toServer = (/** @type {Array<*>} */ c) => !c[2]?.full && host.awaitsReply(c[1]);
            const kind = innerCmds.some((c) => c[0] === JS_NAVIGATE && toServer(c))
                ? 'replace'
                : innerCmds.some(
                        (c) => c[0] === JS_PUSH_EVENT || (c[0] === JS_PATCH && toServer(c)),
                    )
                  ? 'any'
                  : null;
            if (kind) {
                host.armTransition({ types: opts.types, kind });
                runCommands(el, event, inner, `${prefix}${index}.`);
            } else {
                // The update callback runs inside startViewTransition, OUTSIDE
                // every caller's containment -- a throwing command would become
                // one unhandled rejection per dispatch. Same warn-once contract
                // as runDelegated.
                const later = host.holdReplay();
                runTransition(opts, () =>
                    later(() => {
                        try {
                            runCommands(el, event, inner, `${prefix}${index}.`);
                        } catch (err) {
                            const detail = err instanceof Error ? err.message : String(err);
                            warnOnce(
                                `[arizona] az-transition command failed: ${detail}`,
                                'az-transition',
                            );
                        }
                    }),
                );
            }
            break;
        }
        case JS_PUSH_EVENT: {
            const evt = cmd[1];
            const payload =
                cmd.length > 2 ? { ...autoPayload(el, event), ...cmd[2] } : autoPayload(el, event);
            const msg = JSON.stringify([resolveTarget(el), evt, payload]);
            if (event) {
                const epoch = host.epoch();
                scheduleSend(el, event, prefix, index, () => {
                    host.sendEvent(evt, msg, epoch);
                });
            } else {
                host.sendEvent(evt, msg);
            }
            break;
        }
        case JS_TOGGLE:
            withQueryAll(cmd[1], (t) => {
                t.hidden = !t.hidden;
                notifyUpdated(t);
            });
            break;
        case JS_SHOW:
            withQueryAll(cmd[1], (t) => {
                t.hidden = false;
                notifyUpdated(t);
            });
            break;
        case JS_HIDE:
            withQueryAll(cmd[1], (t) => {
                t.hidden = true;
                notifyUpdated(t);
            });
            break;
        case JS_ADD_CLASS:
            withQueryAll(cmd[1], (t) => {
                t.classList.add(cmd[2]);
                notifyUpdated(t);
            });
            break;
        case JS_REMOVE_CLASS:
            withQueryAll(cmd[1], (t) => {
                t.classList.remove(cmd[2]);
                notifyUpdated(t);
            });
            break;
        case JS_TOGGLE_CLASS:
            withQueryAll(cmd[1], (t) => {
                t.classList.toggle(cmd[2]);
                notifyUpdated(t);
            });
            break;
        case JS_SET_ATTR:
            withQueryAll(cmd[1], (t) => applySetAttrOp(t, cmd[2], cmd[3]));
            break;
        case JS_REMOVE_ATTR:
            withQueryAll(cmd[1], (t) => applyRemAttrOp(t, cmd[2]));
            break;
        case JS_TOGGLE_ATTR:
            // 3 args: presence toggle (remove if present, else set bare). 5 args:
            // value toggle (cmd[3] <-> cmd[4]; any other current value -> cmd[3]).
            // Each match is toggled on its own current state.
            withQueryAll(cmd[1], (t) => {
                if (cmd.length === 3) {
                    if (t.hasAttribute(cmd[2])) applyRemAttrOp(t, cmd[2]);
                    else applySetAttrOp(t, cmd[2], '');
                } else {
                    applySetAttrOp(t, cmd[2], t.getAttribute(cmd[2]) === cmd[3] ? cmd[4] : cmd[3]);
                }
            });
            break;
        case JS_DISPATCH_EVENT:
            // Every hosting document, like the selector effects: a popped-out
            // (PiP) view is a first-class Arizona document (it gets the same
            // event delegation and the same server patches), so a listener there
            // is as legitimate a target as one on the main document. A fresh
            // event per document -- an Event carries its own dispatch state.
            for (const doc of allDocs()) {
                doc.dispatchEvent(new CustomEvent(cmd[1], { detail: cmd[2] || {} }));
            }
            break;
        case JS_NAVIGATE: {
            const opts = cmd[2] || {};
            if (opts.full) {
                // Full-page navigation: let the browser load the URL normally.
                // Used when the target isn't a live route (a controller/asset
                // path, or a 404) and so can't be SPA-navigated.
                host.leavePage(cmd[1], opts.replace);
                break;
            }
            host.navigate(cmd[1], opts, 'navigate');
            break;
        }
        case JS_PATCH:
            // In-place SPA navigation: keep the view, re-render via handle_update.
            host.navigate(cmd[1], cmd[2] || {}, 'patch');
            break;
        case JS_FETCH: {
            // HTTP request via fetch() with no page reload. Unlike push_event (WS,
            // can't set cookies), the response can carry a real Set-Cookie, applied
            // natively by the browser. The controller returns the {e:[...]} effects
            // wire payload; we apply it against the enclosing view element below, so a
            // push_event in the response resolves to (and re-renders) the submitting
            // view without scraping the form -- pubsub is for broadcasting to other views.
            const url = cmd[1];
            const opts = cmd[2] || {};
            const form = /** @type {HTMLFormElement|null} */ (el?.closest?.('form') ?? null);
            const method = (opts.method || form?.getAttribute('method') || 'post').toUpperCase();
            const headers = { accept: 'application/json', ...(opts.headers || {}) };
            let target = url;
            let body;
            if (method === 'GET' || method === 'HEAD') {
                // No request body for GET/HEAD -- carry the payload in the query string
                // instead (fetch is otherwise POST-oriented: it sets cookies). The same
                // precedence as a body: an explicit `body` wins over the form's fields.
                let params = null;
                if (opts.body !== undefined) params = bodyQuery(opts.body);
                else if (form) {
                    params = new URLSearchParams(
                        /** @type {any} */ (new FormData(form, submitter(event))),
                    );
                }
                const qs = params ? params.toString() : '';
                if (qs) target = appendQuery(url, qs);
            } else if (opts.body !== undefined) {
                body = JSON.stringify(opts.body);
                headers['content-type'] = 'application/json';
            } else if (form) {
                // Mirror a normal form POST: application/x-www-form-urlencoded.
                // (multipart / file uploads are a documented non-goal.)
                body = new URLSearchParams(
                    /** @type {any} */ (new FormData(form, submitter(event))),
                );
            }
            const onError = (/** @type {object} */ detail) => {
                if (opts.on_error) runCommands(el, event, opts.on_error, `${prefix}${index}e.`);
                document.dispatchEvent(new CustomEvent('arizona:fetch-error', { detail }));
            };
            const later = host.holdReplay();
            fetch(target, {
                method,
                body,
                credentials: CREDENTIALS[opts.credentials] || 'same-origin',
                headers,
                keepalive: opts.keep_alive === true,
            })
                .then((resp) =>
                    resp.text().then((text) =>
                        later(() => {
                            // Apply the effects body whenever it parses -- even on a 4xx, so
                            // the server can drive inline validation with a real status. The
                            // effects run against the enclosing view element (not the form, not
                            // document), so a `push_event` in the response resolves to the
                            // submitting view and re-renders it via handle_event (no pubsub) --
                            // without scraping the form's fields into the event payload (the
                            // view element isn't a form, so autoPayload is empty; the controller
                            // passes any result explicitly). An empty 2xx body (a cookie-only
                            // response) applies nothing. on_error runs only when there is no
                            // usable effects body: a non-JSON page or an empty non-2xx.
                            let effects = null;
                            if (text) {
                                try {
                                    effects = JSON.parse(text).e || [];
                                } catch {
                                    effects = null;
                                }
                            } else if (resp.ok) {
                                effects = [];
                            }
                            // Effect application is isolated from the trailing network
                            // .catch: a throw here is an app bug on a request that
                            // SUCCEEDED (the body parsed), so it is logged -- letting it
                            // flow into the .catch would fire on_error/arizona:fetch-error
                            // as a phantom network failure, after effects partially applied.
                            if (effects !== null) {
                                try {
                                    executeJS(el?.closest?.('[az-view]') ?? el, null, effects);
                                } catch (err) {
                                    console.error('[arizona] fetch response effect threw', err);
                                }
                            } else onError({ url, status: resp.status });
                            // Honor az-form-reset only on a 2xx success, so a validation
                            // error (a non-2xx) keeps the typed fields. Same isolation as
                            // the effects above.
                            if (resp.ok && form) {
                                try {
                                    maybeResetForm(form);
                                } catch (err) {
                                    console.error('[arizona] az-form-reset failed', err);
                                }
                            }
                        }),
                    ),
                )
                .catch((error) => later(() => onError({ url, error })));
            break;
        }
        case JS_FOCUS:
            withQuery(cmd[1], (t) => t.focus());
            break;
        case JS_BLUR:
            withQuery(cmd[1], (t) => t.blur());
            break;
        case JS_RESET_FORM:
            // Broadcast (all matches, main + PiP docs), like the class/visibility
            // effects: reset every matching form. A non-form match (no reset()) is
            // a safe no-op. Fires updated() so a hook observes it like a diff.
            withQueryAll(cmd[1], (t) => {
                const f = /** @type {any} */ (t);
                if (typeof f.reset === 'function') {
                    f.reset();
                    notifyUpdated(t);
                }
            });
            break;
        case JS_SELECT:
            // First match only, like focus/blur/scroll_to. Selection is not a DOM
            // mutation, so no notifyUpdated. A non-input/textarea match (no
            // select()) is a safe no-op.
            withQuery(cmd[1], (t) => {
                const f = /** @type {any} */ (t);
                if (typeof f.select === 'function') f.select();
            });
            break;
        case JS_COPY_TO_CLIPBOARD:
            // First match only. Copy the matched element's value (form control) or
            // textContent to the clipboard. Requires a secure context + user gesture
            // (event command only); a missing/blocked clipboard is a safe no-op. Not
            // a DOM mutation, so no notifyUpdated.
            withQuery(cmd[1], (t) => {
                const text = /** @type {any} */ (t).value ?? t.textContent ?? '';
                // writeText rejects without permission / a secure context; catch it
                // (like JS_OS) so a blocked copy is a logged no-op, not an unhandled
                // rejection.
                navigator.clipboard
                    ?.writeText?.(text)
                    ?.catch?.((err) => console.error('[arizona] copy_to_clipboard failed:', err));
            });
            break;
        case JS_SHOW_MODAL:
            // First match only. Open the matched <dialog> as a true modal (top
            // layer, ::backdrop, ESC-to-close). A non-dialog match (no showModal())
            // is a safe no-op. Fires updated() so a hook observes it like a diff.
            withQuery(cmd[1], (t) => {
                const f = /** @type {any} */ (t);
                if (typeof f.showModal === 'function') {
                    f.showModal();
                    notifyUpdated(t);
                }
            });
            break;
        case JS_CLOSE_MODAL:
            // First match only. Close the matched <dialog>. A non-dialog match (no
            // close()) is a safe no-op. Fires updated() so a hook observes it.
            withQuery(cmd[1], (t) => {
                const f = /** @type {any} */ (t);
                if (typeof f.close === 'function') {
                    f.close();
                    notifyUpdated(t);
                }
            });
            break;
        case JS_SCROLL_TO:
            // Smooth is the default, and opts merge onto it rather than replacing
            // it: passing only an alignment (`#{block => center}`) would otherwise
            // fall back to scrollIntoView's own `behavior: 'auto'` and scroll
            // instantly. An explicit `behavior` still wins.
            withQuery(cmd[1], (t) => t.scrollIntoView({ behavior: 'smooth', ...cmd[2] }));
            break;
        case JS_SET_TITLE:
            document.title = cmd[1];
            break;
        case JS_RELOAD:
            location.reload();
            break;
        case JS_ON_KEY: {
            const f = cmd[1];
            const lk =
                event && /** @type {any} */ (event).key
                    ? /** @type {any} */ (event).key.toLowerCase()
                    : '';
            if (Array.isArray(f) ? f.includes(lk) : new RegExp(f).test(lk))
                runCommands(el, event, cmd[2], `${prefix}${index}.`);
            break;
        }
        case JS_SET_LOCAL: {
            // Client-owned slot update -- never sent to the server. cmd[3]:
            // absent => closest view (the trigger); a viewId string => that
            // view; true => all views.
            const scope = cmd[3];
            if (scope === true) {
                setAll(cmd[1], cmd[2]);
            } else {
                const viewId = scope ?? resolveTarget(el);
                if (viewId) set(viewId, cmd[1], cmd[2]);
            }
            break;
        }
        case JS_REQUEST_PIP:
            // requestWindow rejects without a user gesture; catch it (like JS_OS)
            // so a server-pushed request_pip is a logged no-op, not an unhandled
            // rejection.
            requestPip(cmd[1], cmd[2] || {}).catch((err) =>
                console.error('[arizona] request_pip failed:', cmd[1], err),
            );
            break;
        case JS_EXIT_PIP:
            exitPip(cmd[1]);
            break;
        case JS_OS: {
            // Native-shell (OS) command -- delegate to the embedding shell's
            // invoke if one is present (Electron/Tauri/...). A plain browser has
            // no `__arizona_os__`, so this is a safe no-op. invoke() is async;
            // log (don't crash) on rejection so a failing OS command is visible.
            const args = cmd.slice(2);
            const r = osHost()?.invoke?.(cmd[1], args);
            r?.catch?.((err) => console.error('[arizona] OS command failed:', cmd[1], args, err));
            break;
        }
    }
}

/**
 * One deferred send's state. An element holds one per (event type, command
 * position), so two push_events in one command list, or `az-input` beside
 * `az-keydown`, each keep their own timer instead of the later one replacing the
 * earlier.
 * @typedef {{
 *   id?: ReturnType<typeof setTimeout>,
 *   pending?: Function|null,
 *   prevKey?: string
 * }} TimerState
 */

/**
 * Per-element timer state, keyed by DOM element. WeakMap so entries are
 * GC'd when elements are removed from the document.
 * @type {WeakMap<Element, {slots: Map<string, TimerState>, eventBound?: boolean, blurBound?: boolean}>}
 */
const _timers = new WeakMap();

/**
 * Three send modes controlled by az-debounce and az-throttle attributes:
 *
 * 1. Immediate -- no attributes: send fires right away.
 *
 * 2. Numeric debounce/throttle -- az-debounce="300" or az-throttle="300":
 *    debounce resets the timer on every event; throttle sends immediately
 *    then suppresses for the interval (trailing send if events arrived).
 *    For keydown throttle, a different key resets the interval so each
 *    distinct key gets through promptly.
 *
 * 3. Event-name debounce -- az-debounce="blur": stores the latest send
 *    but only flushes when the named DOM event fires on the element.
 *    Useful for sending input values only when the field loses focus.
 *
 * For modes 2 and 3, blur auto-flushes pending sends so data isn't lost
 * when the user tabs away or clicks elsewhere.
 *
 * Timing is per (event type, command position), not per element: two commands
 * in one list, or two event types on one element, each keep their own timer,
 * while a repeat at the same position collapses -- including one whose command
 * the server re-rendered mid-window (a changed payload), which is why the slot is
 * the position and not the command's value. A deferred send is dropped when it
 * fires after a navigate or `disconnect()`: `send` carries the `_navEpoch` of
 * the event, which both bump.
 * @param {Element} el
 * @param {Event|KeyboardEvent} event
 * @param {string} prefix index path of the wrapper holding the command
 * @param {number} index the command's position in its list
 * @param {Function} send
 */
function scheduleSend(el, event, prefix, index, send) {
    const debounceAttr = el.getAttribute('az-debounce') || '';
    const debounceMs = parseInt(debounceAttr, 10);
    const debounceEvent = Number.isNaN(debounceMs) && debounceAttr !== '' ? debounceAttr : '';
    const throttleMs = parseInt(el.getAttribute('az-throttle') || '', 10);
    if (!debounceMs && !throttleMs && !debounceEvent) {
        send();
        return;
    }
    let rec = _timers.get(el);
    if (!rec) {
        rec = { slots: new Map() };
        _timers.set(el, rec);
    }
    const slotKey = `${event.type} ${prefix}${index}`;
    let t = rec.slots.get(slotKey);
    if (!t) {
        t = {};
        rec.slots.set(slotKey, t);
    }
    const slot = t;
    // Event-name debounce: just store the latest send, flush on the named event.
    if (debounceEvent) {
        slot.pending = send;
        if (!rec.eventBound) {
            rec.eventBound = true;
            el.addEventListener(debounceEvent, () => flushTimer(el));
        }
        return;
    }
    if (debounceMs > 0) {
        // Classic debounce: reset timer on every event.
        clearTimeout(slot.id);
        slot.pending = send;
        slot.id = setTimeout(() => {
            slot.id = undefined;
            slot.pending = null;
            send();
        }, debounceMs);
    } else {
        // Throttle: send immediately, suppress duplicates for the interval.
        // For keydown, a different key resets the cooldown so the new key
        // isn't swallowed by the previous key's throttle window.
        if (event.type === 'keydown' && slot.prevKey !== /** @type {KeyboardEvent} */ (event).key) {
            clearTimeout(slot.id);
            slot.id = undefined;
        }
        if (event.type === 'keydown') slot.prevKey = /** @type {KeyboardEvent} */ (event).key;
        slot.pending = send;
        if (slot.id) return;
        send();
        slot.pending = null;
        // The tick re-arms itself while events are still arriving, and only goes
        // idle when it finds nothing pending. Clearing the window BEFORE the
        // trailing send instead left the throttle idle the instant that send
        // landed, so the very next event took the leading edge with no gap at
        // all -- on a pointer drag, two sends stamped at the same millisecond,
        // the first already stale when it left.
        const tick = () => {
            const pending = slot.pending;
            slot.pending = null;
            if (pending) {
                pending();
                slot.id = setTimeout(tick, throttleMs);
            } else {
                slot.id = undefined;
            }
        };
        slot.id = setTimeout(tick, throttleMs);
    }
    // Auto-flush on blur so pending data isn't lost when focus leaves.
    if (!rec.blurBound) {
        rec.blurBound = true;
        el.addEventListener('blur', () => flushTimer(el));
    }
}

/**
 * Immediately send every pending debounced/throttled message for this element.
 * Used by blur auto-flush and form submit (to capture in-flight input values).
 * @param {Element} el
 */
export function flushTimer(el) {
    const rec = _timers.get(el);
    if (!rec) return;
    for (const t of rec.slots.values()) {
        if (t.id) clearTimeout(t.id);
        const pending = t.pending;
        t.id = undefined;
        t.pending = null;
        if (pending) pending();
    }
}
