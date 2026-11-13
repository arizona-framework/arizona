import { afterEach, describe, expect, it, vi } from 'vitest';

// A property test of what a reconnect does to the form fields the user typed in:
// `saveFormState` on the drop, `restoreFormState` after the resync, and the replay
// that proposes the user's edits to the server. Random pages are dropped one to four
// times, any drop after the first possibly a crash (4500), with the user editing them
// through real events and the resync mutating them (fields and options inserted,
// removed and reordered, ids, kinds and values changed, controls disabled or rendered
// bound or unbound, fields moved to another form, server values changed), the server
// refusing some proposals of the replay and acknowledging some replays, formless
// fields in nested child views (remounted under another id, removed, added before or
// after their siblings, reordered, switched in place), a view
// popped out to a second document (a PiP window) or back before a drop and between
// it and the resync, and every field must end in a state the documented rules
// (`.claude/rules/js.md`, `saveFormState()` / `restoreFormState()`) allow.
//
// The guarantees are checked against what the test itself knows -- what the user did
// to which field, which field is which (`data-pk`, which the code under test never
// reads), what each field held at the resync and what the server renders now -- not
// against a model of how the restore pairs fields. An oracle restating the documented
// rules field by field, for a page of one view in one document, is a second check
// (`ORACLE=0` turns it off).
//
// Reproduce a failure with `SEED=<seed> npx vitest run <this file>`; `COLLECT=1`
// reports every failing seed instead of stopping at the first.

const RUNS = Number(process.env.RUNS ?? 500);
const SEED = Number(process.env.SEED ?? 0x5eed);
const ORACLE = process.env.ORACLE !== '0';

/** @type {any} */
let conn = null;

afterEach(() => {
    conn?.restore();
    conn = null;
    pipDoc = null;
    pipWin = null;
    /** @type {any} */ (window).documentPictureInPicture = undefined;
    vi.restoreAllMocks();
    document.body.innerHTML = '';
});

/** A fresh module connected through a mocked Worker, with `html` on the page. */
async function start(/** @type {string} */ html) {
    vi.resetModules();
    const mod = await import('./arizona.js');
    document.body.innerHTML = html;
    /** @type {any[]} */
    const posted = [];
    /** @type {any} */
    let onmsg = null;
    const inst = {
        postMessage: (/** @type {any} */ d) => posted.push(d),
        set onmessage(fn) {
            onmsg = fn;
        },
        get onmessage() {
            return onmsg;
        },
        terminate: vi.fn(),
    };
    const Orig = globalThis.Worker;
    globalThis.Worker = /** @type {any} */ (
        function () {
            return inst;
        }
    );
    const disconnect = mod.connect('/ws');
    const frame = (/** @type {any[]} */ data) => inst.onmessage({ data });
    const c = {
        mod,
        open: (reconnect = false) => frame([1, reconnect]),
        msg: (/** @type {any} */ ops = null, resync = false) =>
            frame([0, ops, null, resync, null, null]),
        /** A frame carrying handler effects alone. */
        fx: (/** @type {any[]} */ effects) => frame([0, null, effects, false, null, null]),
        /** The server's answer to the client frame carrying `ref`, changing nothing. */
        reply: (/** @type {number} */ ref) => frame([0, null, null, false, null, ref]),
        /** The pong proving the server handled every replayed event. */
        ack: () => frame([3]),
        close: (code = 1006) => frame([2, code]),
        posted,
        sent: () => posted.filter((d) => d[0] === 1).map((d) => JSON.parse(d[1])),
        restore() {
            disconnect();
            globalThis.Worker = Orig;
        },
    };
    c.open();
    c.msg();
    frame([0, null, null, false, ['az-input', 'az-change'], null]);
    conn = c;
    return c;
}

/** mulberry32: a small seeded generator, so a failure replays from its seed. */
function rng(/** @type {number} */ seed) {
    let a = seed >>> 0;
    const next = () => {
        a = (a + 0x6d2b79f5) >>> 0;
        let t = a;
        t = Math.imul(t ^ (t >>> 15), t | 1);
        t ^= t + Math.imul(t ^ (t >>> 7), t | 61);
        return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
    };
    const r = {
        next,
        int: (/** @type {number} */ n) => Math.floor(next() * n),
        chance: (/** @type {number} */ p) => next() < p,
        /** @template T @param {T[]} xs @returns {T} */
        pick: (xs) => xs[r.int(xs.length)],
        /** @template T @param {T[]} xs @returns {T[]} */
        shuffle(xs) {
            const ys = [...xs];
            for (let i = ys.length - 1; i > 0; i--) {
                const j = r.int(i + 1);
                [ys[i], ys[j]] = [ys[j], ys[i]];
            }
            return ys;
        },
    };
    return r;
}

/**
 * A field. `live` is false for one rendered inside an az-nodiff element, which the
 * server never diffs: it owns nothing there, `az-bound` or not. A select's `server`
 * and state are option tokens (see `toks`).
 * A field's `view` is the view rendering it: its form's for a field of a form, `page`,
 * the root, for one outside it naming it (`form=`), any view for a formless one.
 * A field of a form also has its form's view (`fview`) and HTML id (`fhtml`), set by
 * `sync`. Its `doc` is the document it is in at the moment (`main`, or `pip` when its
 * view is popped out), set from the DOM by `locate`.
 * @typedef {{key: string, region: string, form: string|null, name: string,
 *   kind: string, id: string|null, bound: boolean, live: boolean, own: boolean,
 *   disabled: boolean, readonly: boolean, server: any, box: string,
 *   options: string[], view: string, fview?: string, fhtml?: string, sub?: boolean,
 *   doc?: string, gone?: boolean}} Field
 * A page's `forms` are each rendered in a `view` with an HTML id (`html`): two forms
 * in two views may share one, as two items of a list render the same form. A form's
 * `id` names it in the test alone (its region, its events). A form may be its view's
 * own element (`root`, its HTML id the view's), and may render a child view inside
 * it (`inner`), whose fields (`sub`) are the form's in that view. A page's `views`
 * are its child views, each nested in its `parent` (`page` or another child view):
 * a form view (`kind` `form`) or a view inside a form (`inner`) holds that form's
 * fields alone.
 * @typedef {{id: string, push: boolean, view: string, html: string, root?: boolean,
 *   inner?: string|null}} Form
 * @typedef {{id: string, parent: string, kind?: string}} View
 * @typedef {{forms: Form[], regions: Object<string, Field[]>, views: View[]}} Page
 */

const REGIONS = ['before', 'f1', 'f2', 'after', 'formless'];
const OPTIONS = ['o0', 'o1', 'o2', 'o3', 'o4'];
// `o1` is also an option value, so a text paired with a select could pass for a choice.
const TEXTS = ['', 's1', 's2', 'o1'];
/** The values a typed input may hold: each sanitizes what it is given. */
/** @type {Object<string, string[]>} */
const POOLS = {
    number: ['', '1', '2'],
    range: ['50', '10', '90'],
    color: ['#000000', '#ff0000', '#00ff00'],
    date: ['', '2024-01-01', '2024-02-02'],
};
const pool = (/** @type {string} */ k) => POOLS[k] ?? TEXTS;
const TYPED = ['number', 'range', 'color', 'date', 'password', 'email'];
const KINDS = [
    'text',
    'textarea',
    'hidden',
    'url',
    ...TYPED,
    'checkbox',
    'radio',
    'select',
    'multi',
];
const FAMILIES = {
    text: ['text', 'text', 'textarea', 'hidden', 'email', 'url'],
    checkbox: ['checkbox'],
    radio: ['radio'],
    select: ['select'],
    multi: ['multi'],
    // A name whose fields are of one typed input.
    typed: TYPED,
    // A name whose fields are of different kinds.
    mixed: KINDS,
};
const isBox = (/** @type {string} */ k) => k === 'checkbox' || k === 'radio';
/** Is `k` restored by value: a textarea or an input that is no box? */
const isText = (/** @type {string} */ k) =>
    k === 'text' || k === 'textarea' || k === 'hidden' || k === 'url' || TYPED.includes(k);
/**
 * What a kind is restored by: the text-like fields share their text; an input that
 * sanitizes its value (a number, a range, a color, a date) and a password are each
 * their own.
 */
const klass = (/** @type {string} */ k) =>
    ['text', 'textarea', 'hidden', 'email', 'url'].includes(k) ? 'text' : k;
/** Does a field of kind `k` show a line break? @param {string} k */
const keepsBreaks = (k) => k === 'textarea' || k === 'hidden';
/**
 * Can a field of kind `k` show text `v` exactly? A text input drops a line break,
 * and an email or url input also the whitespace around the text.
 * @param {string} k
 * @param {string} v
 */
const fits = (k, v) =>
    (keepsBreaks(k) || !v.includes('\n')) && (!['email', 'url'].includes(k) || v === v.trim());
/** Can `readonly` apply to a field of kind `k`? */
const readonlyable = (/** @type {string} */ k) =>
    ['text', 'textarea', 'password', 'email', 'url', 'number', 'date'].includes(k);
/**
 * The options of `os` (values, in order) as tokens: a value and its occurrence among
 * the options of that value, so options sharing a value are told apart.
 * @param {string[]} os
 */
const toks = (os) => {
    /** @type {Object<string, number>} */
    const n = {};
    return os.map((o) => {
        n[o] = (n[o] ?? -1) + 1;
        return `${o}#${n[o]}`;
    });
};
/** The value of option token `t`. @param {string} t */
const tokValue = (t) => t.replace(/#\d+$/, '');

/** A page generator: fresh keys, ids and checkbox values for one run. */
function gen(/** @type {ReturnType<typeof rng>} */ r) {
    let n = 0;
    const fresh = (/** @type {string} */ p) => `${p}${n++}`;
    /** @param {Field} f */
    const serverOf = (f) => {
        if (isText(f.kind)) return r.pick(pool(f.kind));
        if (isBox(f.kind)) return r.chance(0.4);
        if (f.kind === 'select') return [r.pick(toks(f.options))];
        return toks(f.options).filter(() => r.chance(0.4));
    };
    const optionsOf = () => {
        const os = r.shuffle(OPTIONS).slice(0, 2 + r.int(3));
        // Options sharing a value.
        if (r.chance(0.15)) os.splice(r.int(os.length + 1), 0, r.pick(os));
        return os;
    };
    /** Whether each radio group bound as a whole is bound. */
    /** @type {Map<string, boolean>} */
    const radioBound = new Map();
    /**
     * @param {string} region
     * @param {string|null} form
     * @param {string} name
     * @param {string} kind
     * @param {boolean} push
     * @returns {Field}
     */
    const field = (region, form, name, kind, push) => {
        let bound = kind !== 'hidden' && r.chance(form ? 0.4 : 0.8);
        // Mostly a radio group is bound as a whole; some mix bound and unbound radios.
        if (kind === 'radio' && r.chance(0.7)) {
            const g = `${form}\0${name}`;
            bound = radioBound.get(g) ?? bound;
            radioBound.set(g, bound);
        }
        /** @type {Field} */
        const f = {
            key: fresh('k'),
            region,
            form,
            name,
            kind,
            // A formless field has an id, as only that pairs it; a formless radio
            // pairs by its group's name, so some have none.
            id:
                (!form && !(kind === 'radio' && r.chance(0.5))) || r.chance(0.35)
                    ? fresh('i')
                    : null,
            bound,
            live: !r.chance(0.05),
            // A form's az-change already proposes its fields. A formless field
            // proposes itself; any radio of a server-owned group proposes the group.
            own: (bound || kind === 'radio') && (form ? !push && r.chance(0.5) : r.chance(0.85)),
            disabled: r.chance(0.12),
            readonly: readonlyable(kind) && r.chance(0.1),
            server: null,
            box: fresh('v'),
            options: kind === 'select' || kind === 'multi' ? optionsOf() : [],
            view: 'page',
        };
        f.server = serverOf(f);
        return f;
    };
    /** @type {Map<string, Array<{name: string, family: string}>>} */
    const names = new Map();
    /** Formless radio names, shared so formless radios form groups. */
    /** @type {string[]} */
    const formlessRadios = [];
    /** @param {string} form */
    const nameOf = (form, newProb = 0.2) => {
        const ns = names.get(form) ?? [];
        names.set(form, ns);
        // A name another form has too.
        const others = [...names].flatMap(([k, v]) => (k === form ? [] : v));
        if (others.length > 0 && r.chance(0.1)) {
            const nm = r.pick(others);
            if (!ns.includes(nm)) ns.push(nm);
            return nm;
        }
        if (ns.length > 0 && !r.chance(newProb)) return r.pick(ns);
        const nm = { name: fresh('n'), family: r.pick(Object.keys(FAMILIES)) };
        ns.push(nm);
        return nm;
    };
    /** @param {Page} page @param {string} region @param {string|null} form */
    const fieldIn = (page, region, form) => {
        if (!form) {
            // Radios often, so formless groups form.
            const kind = r.pick([...KINDS.filter((k) => k !== 'hidden'), 'radio', 'radio']);
            let name = fresh('x');
            if (kind === 'radio') {
                if (formlessRadios.length > 0 && r.chance(0.8)) name = r.pick(formlessRadios);
                else formlessRadios.push(name);
            }
            const f = field(region, null, name, kind, false);
            f.view = viewIn(page);
            return f;
        }
        const push = /** @type {any} */ (page.forms.find((x) => x.id === form)).push;
        const { name, family } = nameOf(form);
        return field(region, form, name, r.pick(FAMILIES[/** @type {'text'} */ (family)]), push);
    };
    /** A view of `page` a formless field is rendered in. @param {Page} page */
    const viewIn = (page) => r.pick(['page', 'page', ...plain(page).map((v) => v.id)]);
    /** @param {Page} page */
    const normalize = (page) => {
        sync(page);
        const fs = REGIONS.flatMap((reg) => page.regions[reg]);
        // Some boxes share the value of an earlier box of their name and kind, as
        // value-less checkboxes all submit "on".
        for (const [i, f] of fs.entries()) {
            if (!isBox(f.kind) || !r.chance(0.12)) continue;
            const like = fs
                .slice(0, i)
                .filter((x) => x.kind === f.kind && x.form === f.form && x.name === f.name);
            if (like.length > 0) f.box = r.pick(like).box;
        }
        // At most one checked radio a group, as a server renders it.
        const seen = new Set();
        for (const f of fs) {
            if (f.kind !== 'radio' || !f.server) continue;
            const g = `${f.form}\0${f.name}`;
            if (seen.has(g)) f.server = false;
            seen.add(g);
        }
    };
    /**
     * Some formless fields of two views share an id: a child view rendering a field
     * its parent renders too, or the page rendering a field a popped-out view does.
     * @param {Page} page
     */
    const sharedFreeIds = (page) => {
        if (page.views.length === 0) return;
        for (let i = r.int(3); i > 0; i--) {
            const fs = page.regions.formless.filter((f) => f.kind !== 'radio');
            const [a, b] = r.shuffle(fs);
            if (a && b && a.view !== b.view) b.id = a.id;
        }
    };
    return {
        /** @returns {Page} */
        page() {
            /** @type {Page} */
            const page = {
                forms: [],
                regions: { before: [], f1: [], f2: [], after: [], formless: [] },
                views: [],
            };
            // Child views, nested in the page or in one another.
            if (r.chance(0.6))
                for (let i = 1 + r.int(3); i > 0; i--)
                    page.views.push({
                        id: fresh('c'),
                        parent: r.pick(['page', ...page.views.map((v) => v.id)]),
                    });
            // Forms in the page or in a child view; two in two views may share an id.
            // A form may be a child view's own element, and may hold a child view.
            const formView = () =>
                plain(page).length > 0 && r.chance(0.5) ? r.pick(plain(page)).id : 'page';
            for (const id of ['f1', 'f2'].slice(0, r.chance(0.15) ? 0 : 1 + r.int(2))) {
                const push = r.chance(0.5);
                if (r.chance(0.25)) {
                    const v = fresh('c');
                    page.views.push({ id: v, parent: formView(), kind: 'form' });
                    page.forms.push({ id, push, view: v, html: v, root: true, inner: null });
                    continue;
                }
                const view = formView();
                const twin = page.forms.find((f) => !f.root && f.view !== view);
                const html = twin && r.chance(0.4) ? twin.html : id;
                let inner = null;
                if (r.chance(0.35)) {
                    inner = fresh('c');
                    page.views.push({ id: inner, parent: view, kind: 'inner' });
                }
                page.forms.push({ id, push, view, html, inner });
            }
            for (const { id, push } of page.forms) {
                for (let i = 1 + r.int(4); i > 0; i--) {
                    const { name, family } = nameOf(id, 1);
                    const fs = [];
                    for (let j = 1 + r.int(3); j > 0; j--)
                        fs.push(
                            field(
                                id,
                                id,
                                name,
                                r.pick(FAMILIES[/** @type {'text'} */ (family)]),
                                push,
                            ),
                        );
                    // Some in the child view the form holds.
                    for (const f of fs) f.sub = r.chance(0.4);
                    // An id two fields share names neither.
                    if (fs.length > 1 && r.chance(0.1)) fs[0].id = fs[1].id = fresh('sh');
                    page.regions[id].push(...fs);
                }
                // An id fields of two names share names neither.
                const fs = page.regions[id];
                if (fs.length > 1 && r.chance(0.08)) {
                    const [a, b] = r.shuffle(fs);
                    a.id ??= fresh('sh');
                    b.id = a.id;
                }
            }
            for (const reg of linkable(page).length > 0 ? ['before', 'after'] : [])
                for (let i = r.int(3); i > 0; i--)
                    page.regions[reg].push(fieldIn(page, reg, r.pick(linkable(page)).id));
            for (let i = r.int(5); i > 0; i--)
                page.regions.formless.push(fieldIn(page, 'formless', null));
            // Some pages hold several formless radio groups, each replayed and
            // withheld apart.
            if (r.chance(0.3))
                for (let i = 2 + r.int(2); i > 0; i--) {
                    const name = fresh('x');
                    for (let j = 2 + r.int(2); j > 0; j--) {
                        const f = field('formless', null, name, 'radio', false);
                        f.view = viewIn(page);
                        page.regions.formless.push(f);
                    }
                }
            sharedFreeIds(page);
            normalize(page);
            return page;
        },
        /** The resync's page: `page` mutated. @param {Page} page @returns {Page} */
        mutate(page) {
            /** @type {Page} */
            const next = {
                forms: page.forms.map((f) => ({ ...f })),
                regions: Object.fromEntries(
                    REGIONS.map((reg) => [
                        reg,
                        page.regions[reg].map((f) => ({
                            ...f,
                            server: Array.isArray(f.server) ? [...f.server] : f.server,
                            options: [...f.options],
                            doc: undefined,
                            gone: false,
                        })),
                    ]),
                ),
                views: page.views.map((v) => ({ ...v })),
            };
            // A child view remounted under a new id (a `?stateful` child whose id
            // changed), removed (its fields and children going to its parent), or
            // added.
            for (const v of [...next.views]) {
                if (r.chance(0.2)) {
                    const id = fresh('c');
                    for (const f of next.regions.formless) if (f.view === v.id) f.view = id;
                    for (const f of next.forms) {
                        if (f.view === v.id) f.view = id;
                        if (f.root && f.view === id) f.html = id;
                        if (f.inner === v.id) f.inner = id;
                    }
                    for (const w of next.views) if (w.parent === v.id) w.parent = id;
                    v.id = id;
                } else if (r.chance(0.06) && !v.kind && !next.forms.some((f) => f.view === v.id)) {
                    for (const f of next.regions.formless) if (f.view === v.id) f.view = v.parent;
                    for (const w of next.views) if (w.parent === v.id) w.parent = v.parent;
                    next.views.splice(next.views.indexOf(v), 1);
                }
            }
            // A view added, after its siblings or before them.
            if (r.chance(0.1)) {
                const v = {
                    id: fresh('c'),
                    parent: r.pick(['page', ...plain(next).map((w) => w.id)]),
                };
                if (r.chance(0.5)) next.views.unshift(v);
                else next.views.push(v);
            }
            // Sibling views reordered (keyed children), ids kept.
            if (next.views.length > 1 && r.chance(0.15)) next.views = r.shuffle(next.views);
            // Two sibling views trade places with their ids kept on the content: each
            // place now renders the other view (tabs switched), so what was typed in one
            // must not land in the other.
            const sibs = plain(next).filter((v) =>
                plain(next).some((w) => w !== v && w.parent === v.parent),
            );
            if (sibs.length > 1 && r.chance(0.1)) {
                const a = r.pick(sibs);
                const b = r.pick(sibs.filter((w) => w !== a && w.parent === a.parent));
                if (b) {
                    const [ia, ib] = [next.views.indexOf(a), next.views.indexOf(b)];
                    for (const f of [...next.regions.formless, ...next.forms])
                        if (f.view === a.id) f.view = b.id;
                        else if (f.view === b.id) f.view = a.id;
                    for (const w of next.views)
                        if (w.parent === a.id) w.parent = b.id;
                        else if (w.parent === b.id) w.parent = a.id;
                    [next.views[ia], next.views[ib]] = [
                        { ...a, id: b.id },
                        { ...b, id: a.id },
                    ];
                }
            }
            for (const reg of REGIONS) {
                const fs = next.regions[reg];
                const form = reg === 'formless' ? null : page.forms.find((x) => x.id === reg)?.id;
                if (reg !== 'formless' && reg !== 'before' && reg !== 'after' && !form) continue;
                if (fs.length > 0 && r.chance(0.3)) fs.splice(r.int(fs.length), 1);
                if (r.chance(0.4) && (form !== undefined || linkable(next).length > 0)) {
                    const owner = form === undefined ? r.pick(linkable(next)).id : form;
                    fs.splice(r.int(fs.length + 1), 0, fieldIn(next, reg, owner));
                }
                // A box inserted into or removed from a name it shares with text
                // fields or selects (a radio group and its "Other" text field).
                const shared = fs.filter((f) => f.form && !isBox(f.kind));
                if (shared.length > 0 && r.chance(0.2)) {
                    const { name, form: owner } = r.pick(shared);
                    const boxes = fs.filter((f) => f.name === name && isBox(f.kind));
                    if (boxes.length > 0 && r.chance(0.5)) {
                        fs.splice(fs.indexOf(r.pick(boxes)), 1);
                    } else {
                        const push = !!page.forms.find((x) => x.id === owner)?.push;
                        const box = field(reg, owner, name, r.pick(['radio', 'checkbox']), push);
                        fs.splice(r.int(fs.length + 1), 0, box);
                    }
                }
                if (fs.length > 1 && r.chance(0.3)) {
                    const [i, j] = [r.int(fs.length), r.int(fs.length)];
                    [fs[i], fs[j]] = [fs[j], fs[i]];
                }
                for (const f of fs) {
                    // The same field rendered as another kind of control.
                    if (r.chance(0.04)) {
                        f.kind = r.pick(KINDS.filter((k) => k !== f.kind));
                        f.options = f.kind === 'select' || f.kind === 'multi' ? optionsOf() : [];
                        f.box = fresh('v');
                        f.readonly = false;
                        if (f.kind === 'hidden') f.bound = false;
                        f.own = (f.bound || f.kind === 'radio') && f.own;
                        f.server = serverOf(f);
                    }
                    if (r.chance(0.15)) f.id = f.id && r.chance(0.5) ? null : fresh('i');
                    // A formless field the resync renders in another view, a field of a
                    // form in or out of the child view it holds.
                    if (!f.form && r.chance(0.08)) f.view = viewIn(next);
                    if (f.form && r.chance(0.05)) f.sub = !f.sub;
                    if (!f.form && !f.id && f.kind !== 'radio') f.id = fresh('i');
                    if (r.chance(0.1)) f.disabled = !f.disabled;
                    if (readonlyable(f.kind) && r.chance(0.05)) f.readonly = !f.readonly;
                    if (r.chance(0.03)) f.live = !f.live;
                    // A server-owned field the resync renders unbound, or the reverse.
                    if (f.kind !== 'radio' && f.kind !== 'hidden' && r.chance(0.05)) {
                        f.bound = !f.bound;
                        f.own = f.own && f.bound;
                    }
                    if (f.options.length > 0 && r.chance(0.3)) {
                        if (f.options.length > 1 && r.chance(0.5))
                            f.options.splice(r.int(f.options.length), 1);
                        const missing = OPTIONS.filter((o) => !f.options.includes(o));
                        if (missing.length > 0 && r.chance(0.5))
                            f.options.splice(r.int(f.options.length + 1), 0, r.pick(missing));
                        if (r.chance(0.1))
                            f.options.splice(r.int(f.options.length + 1), 0, r.pick(f.options));
                        if (r.chance(0.5)) f.options = r.shuffle(f.options);
                        const ts = toks(f.options);
                        f.server = f.server.filter((/** @type {string} */ o) => ts.includes(o));
                        if (f.kind === 'select' && f.server.length === 0) f.server = [r.pick(ts)];
                    }
                    if (r.chance(0.3)) f.server = serverOf(f);
                    if (isBox(f.kind) && r.chance(0.05)) f.box = fresh('v');
                    // What a select shows is in option order.
                    if (f.options.length > 0)
                        f.server = toks(f.options).filter((o) => f.server.includes(o));
                }
            }
            // A field moving to the other form.
            const [a, b] = page.forms;
            if (a && b && r.chance(0.3)) {
                const [from, to] = r.shuffle([a.id, b.id]);
                const fs = next.regions[from];
                if (fs.length > 0) {
                    const [f] = fs.splice(r.int(fs.length), 1);
                    f.form = to;
                    f.region = to;
                    f.sub = false;
                    next.regions[to].splice(r.int(next.regions[to].length + 1), 0, f);
                }
            }
            sharedFreeIds(next);
            sync(next);
            // A server-owned radio group the resync renders unbound, or the reverse:
            // every radio of it alike.
            /** @type {Map<string, boolean|null>} */
            const flips = new Map();
            for (const f of ordered(next)) {
                if (f.kind !== 'radio') continue;
                const k = `${f.form}\0${f.name}`;
                if (!flips.has(k))
                    flips.set(
                        k,
                        r.chance(0.05)
                            ? !named(next, f).some((x) => x.kind === 'radio' && x.bound)
                            : null,
                    );
                const to = flips.get(k);
                if (to === null || to === undefined) continue;
                f.bound = to;
            }
            normalize(next);
            return next;
        },
    };
}

/**
 * The forms of `page` a control outside them may name (`form=`): those of an HTML id
 * no other form has, as `form=` names the first form of its id in the document.
 * @param {Page} page
 */
const linkable = (page) =>
    page.forms.filter((f) => page.forms.filter((g) => g.html === f.html).length === 1);

/** The child views of `page` that hold no form's fields alone. @param {Page} page */
const plain = (page) => page.views.filter((v) => !v.kind);

/**
 * Give each field of a form of `page` its form's view and HTML id, and its view: its
 * form's, or the page's for one outside it.
 * @param {Page} page
 */
function sync(page) {
    for (const reg of ['before', 'after', 'f1', 'f2'])
        for (const f of page.regions[reg]) {
            const form = /** @type {Form} */ (page.forms.find((x) => x.id === f.form));
            f.fview = form.view;
            f.fhtml = form.html;
            const outside = reg === 'before' || reg === 'after';
            f.sub = !outside && !!f.sub && !!form.inner;
            f.view = outside ? 'page' : f.sub ? /** @type {string} */ (form.inner) : form.view;
        }
}

const esc = (/** @type {string} */ s) => s.replace(/&/g, '&amp;').replace(/"/g, '&quot;');

/**
 * A live radio is the sole content of a slot of its own, which a diff re-renders
 * (`OP_TEXT`, see `serverWrites`).
 * @param {Field} f
 */
function fieldHtml(f) {
    const html = controlHtml(f);
    if (!f.live) return `<span az-nodiff>${html}</span>`;
    return f.kind === 'radio'
        ? `<span az="${f.key}"><!--az:${f.key}-->${html}<!--/az--></span>`
        : html;
}

/** @param {Field} f */
function controlHtml(f) {
    const a = [`data-pk="${f.key}"`, `name="${esc(f.name)}"`];
    if (f.id) a.push(`id="${f.id}"`);
    if (f.region === 'before' || f.region === 'after') a.push(`form="${f.fhtml}"`);
    if (f.disabled) a.push('disabled');
    if (f.readonly) a.push('readonly');
    if (f.bound && f.kind !== 'select' && f.kind !== 'multi')
        a.push(`az-bound="${isBox(f.kind) ? 'checked' : 'value'}"`);
    if (f.own) a.push(`az-change='[0,"own_${f.key}"]'`);
    const at = a.join(' ');
    if (f.kind === 'textarea') return `<textarea ${at}>${f.server}</textarea>`;
    if (isText(f.kind)) return `<input type="${f.kind}" value="${f.server}" ${at}>`;
    if (isBox(f.kind))
        return `<input type="${f.kind}" value="${f.box}"${f.server ? ' checked' : ''} ${at}>`;
    const ts = toks(f.options);
    const os = f.options
        .map(
            (o, i) =>
                `<option value="${o}"${f.server.includes(ts[i]) ? ' selected' : ''}` +
                `${f.bound ? ' az-bound="selected"' : ''}>${o}</option>`,
        )
        .join('');
    return `<select ${at}${f.kind === 'multi' ? ' multiple' : ''}>${os}</select>`;
}

/** @param {Page} page */
function render(page) {
    const fs = (/** @type {string} */ reg) => page.regions[reg].map(fieldHtml).join('');
    /** Form `f`: its fields, then the child view it holds. @param {Form} f */
    const form = (f) => {
        const own = page.regions[f.id].filter((x) => !x.sub);
        const sub = page.regions[f.id].filter((x) => x.sub);
        const inner = f.inner
            ? `<div id="${f.inner}" az-view>${sub.map(fieldHtml).join('')}</div>`
            : '';
        return (
            `<form id="${f.html}" data-form="${f.id}"${f.root ? ' az-view' : ''}` +
            `${f.push ? ` az-change='[0,"chg_${f.id}"]'` : ''}>${own.map(fieldHtml).join('')}${inner}</form>`
        );
    };
    /** The forms of view `v` other than its own element. @param {string} v */
    const forms = (v) =>
        page.forms
            .filter((f) => f.view === v && !f.root)
            .map(form)
            .join('');
    /** The formless fields of view `v`, then its child views (each its forms first). @param {string} v */
    const free = (v) =>
        page.regions.formless
            .filter((f) => f.view === v)
            .map(fieldHtml)
            .join('') +
        page.views
            .filter((c) => c.parent === v && c.kind !== 'inner')
            .map((c) =>
                c.kind === 'form'
                    ? form(/** @type {Form} */ (page.forms.find((f) => f.root && f.view === c.id)))
                    : `<div id="${c.id}" az-view>${forms(c.id)}${free(c.id)}</div>`,
            )
            .join('');
    return `<div id="page" az-view>${fs('before')}${forms('page')}${fs('after')}${free('page')}</div>`;
}

/** Every field of `page` in document order (see `render`). @param {Page} page */
const ordered = (page) => {
    const fields = (/** @type {Form} */ f) => [
        ...page.regions[f.id].filter((x) => !x.sub),
        ...page.regions[f.id].filter((x) => x.sub),
    ];
    const forms = (/** @type {string} */ v) =>
        page.forms.filter((f) => f.view === v && !f.root).flatMap(fields);
    /** @type {Field[]} */
    const out = [...page.regions.before, ...forms('page'), ...page.regions.after];
    const walk = (/** @type {string} */ v) => {
        out.push(...page.regions.formless.filter((f) => f.view === v));
        for (const c of page.views)
            if (c.parent === v && c.kind === 'form')
                out.push(
                    ...fields(
                        /** @type {Form} */ (page.forms.find((f) => f.root && f.view === c.id)),
                    ),
                );
            else if (c.parent === v && c.kind !== 'inner') {
                out.push(...forms(c.id));
                walk(c.id);
            }
    };
    walk('page');
    // A radio a diff removed until the resync renders it again.
    return out.filter((f) => !f.gone);
};

/** The PiP window's document while one is open, else null. @type {Document|null} */
let pipDoc = null;
/** The PiP window while one is open, else null. @type {{close: () => void}|null} */
let pipWin = null;

/** The live element of field `f`, in whichever document holds it. @param {Field} f @returns {any} */
const elOf = (f) =>
    document.querySelector(`[data-pk="${f.key}"]`) ?? pipDoc?.querySelector(`[data-pk="${f.key}"]`);

/** Note the document each field of `page` is in now. @param {Page} page */
/** The document view `v` is in now, as `locate` names it; null when it is gone. @param {string} v */
const viewDoc = (v) =>
    document.getElementById(v) ? 'main' : pipWin && pipDoc?.getElementById(v) ? 'pip' : null;

function locate(page) {
    for (const f of ordered(page)) f.doc = elOf(f)?.ownerDocument === document ? 'main' : 'pip';
}

/** What a control shows. @param {any} el */
function stateOf(el) {
    if (isBox(el.type)) return el.checked;
    if (el.localName === 'select') {
        const ts = toks([...el.options].map((o) => o.value));
        return ts.filter((_, i) => el.options[i].selected);
    }
    return el.value;
}

/** A state as a comparable choice: a select's options as a set. @param {any} s */
const choice = (s) => JSON.stringify(Array.isArray(s) ? [...s].sort() : s);

/** What field `f` shows. @param {Field} f */
const shows = (f) => stateOf(elOf(f));

/**
 * What a field of a form is restored within: the controls of its view belonging to a
 * form of its form's HTML id (rule 2: a view is its id). Null outside every form.
 * @param {Field} f
 */
const ident = (f) => (f.form === null ? null : `${f.view}\0${f.fhtml}`);

/** The fields of `page` sharing `f`'s name and `ident` (or formlessness). */
const named = (/** @type {Page} */ page, /** @type {Field} */ f) =>
    ordered(page).filter((x) => ident(x) === ident(f) && x.name === f.name);

/**
 * The radio group of `f` in `page`, as the browser sees it: its form's radios of its
 * name, or the formless ones of its name in its document.
 */
const groupIn = (/** @type {Page} */ page, /** @type {Field} */ f) =>
    ordered(page).filter(
        (x) => x.kind === 'radio' && x.name === f.name && x.form === f.form && x.doc === f.doc,
    );

/** The controls a proposal from `f` holds in `page`: a radio's group, else `f`. */
const unitIn = (/** @type {Page} */ page, /** @type {Field} */ f) =>
    f.kind === 'radio' ? groupIn(page, f) : [f];

/** Does the server own control `f`: bound, and diffed? */
const owned = (/** @type {Field} */ f) => f.bound && f.live;

/** Does the server own `f` in `page` (a radio: its group, by any of its radios)? */
const ownsIn = (/** @type {Page} */ page, /** @type {Field} */ f) => unitIn(page, f).some(owned);

/**
 * Does server-owned `f` (a radio: its group, every radio of it) show something apart
 * from the server's, as `page` renders it? That is what makes an `input` or `change`
 * on it an edit.
 */
const apart = (/** @type {Page} */ page, /** @type {Field} */ f) =>
    ownsIn(page, f) && unitIn(page, f).some((x) => choice(shows(x)) !== choice(x.server));

/** @param {Field} f */
const nameKey = (f) => `${f.form}\0${f.name}`;

/** The key of `f`'s name in its `ident` (outside every form, `nameKey`). @param {Field} f */
const partKey = (f) => (f.form === null ? nameKey(f) : `${ident(f)}\0${f.name}`);

/**
 * The values field `f` showing state `s` submits; an unchecked box as `!value`.
 * @param {Field} f
 * @param {any} s
 * @returns {string[]}
 */
function valuesOf(f, s) {
    if (isBox(f.kind)) return [s ? f.box : `!${f.box}`];
    return Array.isArray(s) ? s.map(tokValue) : [s];
}

/** How many of `xs` are `x`. @param {string[]} xs @param {string} x */
const count = (xs, x) => xs.filter((y) => y === x).length;

/**
 * What the test knows, independent of the code under test: the fields the user
 * edited since they last showed the server's state (a radio: its whole group), and
 * every value the user put into a name, by `partKey`.
 */
function truth() {
    return {
        /** @type {Set<string>} */
        edits: new Set(),
        /** @type {Map<string, Set<string>>} values by `partKey` */
        values: new Map(),
        /**
         * How many times at most each field submitted each value the user chose (a
         * multiple select may offer one value twice), by field key.
         * @type {Map<string, Map<string, number>>}
         */
        counts: new Map(),
        /**
         * When the user last chose each radio (the server's too: an act on its
         * group), in the order of their acts, and the last of those.
         * @type {Map<string, number>}
         */
        at: new Map(),
        /**
         * The group each act chose in, its form and name: a pick counts only while its
         * radio is in that group.
         * @type {Map<string, string>}
         */
        actIn: new Map(),
        seq: 0,
        /**
         * When the server last stated each radio's group -- a batch of its own (one
         * frame) after which the group's checked radios are others (see `serverOp`) --
         * or a reset of its form ran, in the order of the acts: stamped on every radio
         * the group (its name in its form, across documents) held before or holds
         * after. A pick made before its own stamp no longer stands.
         * @type {Map<string, number>}
         */
        stamp: new Map(),
    };
}

/**
 * Record how many times field `f` submits each value it shows.
 * @param {ReturnType<typeof truth>} gt
 * @param {Field} f
 */
function noteCounts(gt, f) {
    const sub = valuesOf(f, shows(f));
    const n = gt.counts.get(f.key) ?? new Map();
    for (const v of sub) n.set(v, Math.max(n.get(v) ?? 0, count(sub, v)));
    gt.counts.set(f.key, n);
}

/**
 * The user's edits: real values set and real `input` and `change` events.
 * @param {ReturnType<typeof rng>} r
 * @param {Page} page
 * @param {ReturnType<typeof truth>} gt
 */
function userEdits(r, page, gt) {
    for (const f of ordered(page)) {
        const el = elOf(f);
        if (el.matches(':disabled') || el.readOnly || !r.chance(0.35)) continue;
        // A textarea's line break, which a text input would drop, and whitespace
        // around the text, which an email or url input would.
        const more = f.kind === 'textarea' ? ['u1', ' u2 ', 'l1\nl2'] : ['u1', ' u2 '];
        if (isText(f.kind)) el.value = r.pick([...pool(f.kind), ...(POOLS[f.kind] ? [] : more)]);
        else if (f.kind === 'checkbox') el.checked = !el.checked;
        else if (f.kind === 'radio') {
            // A user's click on the checked radio changes nothing and fires no event in
            // a browser; dispatched here it is code's, no edit and no act.
            // A radio a write turned into a checkbox is no radio to pick.
            if (f.awayType) continue;
            if (el.checked) {
                for (const type of ['input', 'change'])
                    el.dispatchEvent(new Event(type, { bubbles: true }));
                continue;
            }
            // A pick of another radio is an act, a return to the server's too.
            // The user's click: trusted events, unlike code's (see `noise`).
            acted(page, gt, f);
            el.click();
            landed(page, gt, f, false);
            continue;
        } else if (f.kind === 'select') el.selectedIndex = r.int(el.options.length);
        else for (const o of el.options) o.selected = r.chance(0.5);
        landed(page, gt, f);
    }
}

/**
 * The user's act choosing radio `f`, and the group it chose in (see `truth`).
 * @param {Page} page
 * @param {ReturnType<typeof truth>} gt
 * @param {Field} f
 */
function acted(_page, gt, f) {
    gt.at.set(f.key, ++gt.seq);
    gt.actIn.set(f.key, f.awayKey ?? nameKey(f));
}

/** The group radio `f` is in now: written away, the one it is in meanwhile. @param {Field} f */
const groupKey = (f) => (f.awayType ? null : (f.awayKey ?? nameKey(f)));

/**
 * A batch of the server's, `fn` (one frame): each radio group, as the test knows it
 * (see `groupKey`), whose checked radios after it are not those before it was
 * stated -- every radio it held before or holds after takes the stamp (see `truth`).
 * @param {Page} page
 * @param {ReturnType<typeof truth>} gt
 * @param {() => void} fn
 */
function serverOp(page, gt, fn) {
    const before = readGroups(page);
    fn();
    stampChanged(gt, before, readGroups(page));
}

/**
 * `serverOp` for a batch the page applies later (a deferred effect): `fn` awaits it.
 * @param {Page} page
 * @param {ReturnType<typeof truth>} gt
 * @param {() => Promise<void>} fn
 */
async function serverOpAsync(page, gt, fn) {
    const before = readGroups(page);
    await fn();
    stampChanged(gt, before, readGroups(page));
}

/**
 * Each group's checked elements, as the test knows the groups.
 * @param {Page} page
 */
function readGroups(page) {
    /** @type {Map<string, {on: Element[]}>} */
    const m = new Map();
    for (const x of ordered(page)) {
        const k = x.kind === 'radio' ? groupKey(x) : null;
        const el = elOf(x);
        if (k === null || !el?.isConnected) continue;
        const g = m.get(k) ?? { on: [] };
        m.set(k, g);
        if (/** @type {any} */ (el).checked) g.on.push(el);
    }
    return m;
}

/**
 * The groups whose checked elements differ, before or after, were stated: each pick
 * made in one of them (see `truth`'s `actIn`) takes the stamp, wherever its radio is.
 * @param {ReturnType<typeof truth>} gt
 * @param {ReturnType<typeof readGroups>} before
 * @param {ReturnType<typeof readGroups>} after
 */
function stampChanged(gt, before, after) {
    const t = ++gt.seq;
    const none = { on: [] };
    /** @type {Set<string>} */
    const stated = new Set();
    for (const k of new Set([...before.keys(), ...after.keys()])) {
        const b = before.get(k) ?? none;
        const a = after.get(k) ?? none;
        if (b.on.length !== a.on.length || b.on.some((e) => !a.on.includes(e))) stated.add(k);
    }
    for (const [key, k] of gt.actIn) if (stated.has(k)) gt.stamp.set(key, t);
}

/**
 * Server effects `effects`, delivered in a frame, in a view transition the page runs
 * later, or as a fetch response's: each is a batch of the server's (see `serverOp`).
 * @param {ReturnType<typeof rng>} r
 * @param {any} c
 * @param {Page} page
 * @param {ReturnType<typeof truth>} gt
 * @param {Array<Array<*>>} effects
 */
async function deliver(r, c, page, gt, effects) {
    const how = r.pick(['frame', 'frame', 'transition', 'fetch']);
    if (how === 'frame') {
        serverOp(page, gt, () => c.fx(effects));
        return;
    }
    bump(`effects in a ${how}`);
    const d = /** @type {any} */ (document);
    const origFetch = globalThis.fetch;
    /** @type {Function[]} */
    const later = [];
    if (how === 'transition')
        d.startViewTransition = (/** @type {any} */ fn) => {
            later.push(typeof fn === 'function' ? fn : fn.update);
            return { ready: Promise.resolve(), finished: Promise.resolve() };
        };
    /** @type {any} */ else
        globalThis.fetch = vi.fn().mockResolvedValue({
            ok: true,
            status: 200,
            text: () => Promise.resolve(JSON.stringify({ e: effects })),
        });
    try {
        await serverOpAsync(page, gt, async () => {
            if (how === 'transition') {
                c.fx([[20, {}, effects]]);
                for (const f of later) f();
            } else {
                c.fx([[22, '/x', { method: 'post', body: {} }]]);
                for (let i = 0; i < 3; i++) await new Promise((res) => setTimeout(res, 0));
            }
        });
    } finally {
        delete d.startViewTransition;
        globalThis.fetch = origFetch;
    }
}

/**
 * A statement about the group of radio `f` (see `truth`): its radios, in every
 * document, take its stamp.
 * @param {Page} page
 * @param {ReturnType<typeof truth>} gt
 * @param {Field} f
 */
function stated(page, gt, f) {
    const t = ++gt.seq;
    for (const x of ordered(page))
        if (x.kind === 'radio' && !x.gone && x.name === f.name && x.form === f.form)
            gt.stamp.set(x.key, t);
}

/**
 * The user's `input` and `change` on field `f`, its value already set (`dispatch`
 * false: a click fired them), and what the test knows of them.
 * @param {Page} page
 * @param {ReturnType<typeof truth>} gt
 * @param {Field} f
 * @param {boolean} [dispatch]
 */
function landed(page, gt, f, dispatch = true) {
    if (dispatch)
        for (const type of ['input', 'change'])
            elOf(f).dispatchEvent(new Event(type, { bubbles: true }));
    // An event lands as an edit while the control shows something apart from the
    // server's.
    if (apart(page, f)) for (const x of unitIn(page, f)) gt.edits.add(x.key);
    for (const x of unitIn(page, f)) {
        const vs = gt.values.get(partKey(x)) ?? new Set();
        for (const v of valuesOf(x, shows(x))) vs.add(v);
        gt.values.set(partKey(x), vs);
        noteCounts(gt, x);
    }
}

/**
 * What each field of `page` held at the resync: its state, whether it was disabled
 * (it submitted nothing, so nothing was saved for it), and whether it was the user's
 * edit of a server-owned control (`mine`; a radio: its group's).
 * @typedef {{f: Field, state: any, off: boolean, mine: boolean}} Held
 * @param {Page} page
 * @param {ReturnType<typeof truth>} gt
 * @returns {Map<string, Held>}
 */
function held(page, gt) {
    /** @type {Map<string, Held>} */
    const out = new Map();
    for (const f of ordered(page)) {
        const el = elOf(f);
        // Rule 3 judges the unit as it is at the drop: a radio group a pop-back merged
        // is one group, edited when any of its radios was.
        // A radio group's edit is its pick, while it stands: made after the group's
        // last statement of the server's (see `truth`).
        const mine =
            ownsIn(page, f) &&
            (f.kind === 'radio'
                ? standing(gt, unitIn(page, f))
                : unitIn(page, f).some((x) => gt.edits.has(x.key))) &&
            apart(page, f);
        out.set(f.key, { f, state: stateOf(el), off: el.matches(':disabled'), mine });
    }
    return out;
}

/**
 * Does the user's last pick among radios `u` stand: made, and after its stamp?
 * @param {ReturnType<typeof truth>} gt
 * @param {Field[]} u
 */
function standing(gt, u) {
    const at = (/** @type {Field} */ x) =>
        gt.actIn.get(x.key) === nameKey(x) ? (gt.at.get(x.key) ?? 0) : 0;
    const last = u.reduce((a, b) => (at(b) > at(a) ? b : a), u[0]);
    return !!last && at(last) > 0 && (gt.stamp.get(last.key) ?? 0) < at(last);
}

/**
 * A saved control: `keys` are what a crash withholds of its unit (`modelKey`).
 * @typedef {{key: string, kind: string, id: string|null, off: boolean,
 *   value: any, box: [string, boolean]|null, owned: boolean, mine: boolean,
 *   keys: string[]}} Saved
 */

/** A formless field's name in its scope: a radio's group, else its id. @param {Field} f */
const freeName = (f) => (f.kind === 'radio' ? f.name : `\0${f.id}`);

/** What a crash withholds when form `f` replayed before it: the form of its view and HTML id. */
const formModelKey = (/** @type {Form} */ f) => `F\0${f.view}\0${f.html}`;

/** The form of `page` of test id `id`. */
const formOf = (/** @type {Page} */ page, /** @type {string} */ id) =>
    /** @type {Form} */ (page.forms.find((x) => x.id === id));

/**
 * What a crash withholds when `f`'s unit replayed before it: its form (`formModelKey`);
 * outside every form a control by its view and id, a radio group by its name (a
 * group spanning views or, after a pop-out, documents is the group that replayed,
 * every part of it).
 * @param {Field} f
 */
const modelKey = (f) =>
    f.form
        ? `F\0${f.fview}\0${f.fhtml}`
        : f.kind === 'radio'
          ? `r\0${f.view}\0${f.name}`
          : `c\0${f.view}\0${f.id}`;

/**
 * What the drop saves, as the documented rules describe it: per form, each name's
 * controls in form order; outside every form, the server-owned units, each control
 * by its id and each radio group by its name.
 * @param {Page} page
 * @param {Map<string, Held>} pre
 */
function snapshot(page, pre) {
    /**
     * @param {Field[]} fs
     * @param {(f: Field) => string} nameOf
     * @param {(f: Field) => string[]} keys
     */
    const scope = (fs, nameOf, keys) => {
        /** @type {Map<string, number>} */
        const ids = new Map();
        for (const f of fs) if (f.id) ids.set(f.id, (ids.get(f.id) ?? 0) + 1);
        /** @type {Map<string, Saved[]>} */
        const byName = new Map();
        for (const f of fs) {
            const h = /** @type {Held} */ (pre.get(f.key));
            let value;
            if (!h.off && isText(f.kind)) value = h.state;
            else if (!h.off && f.kind === 'select') value = h.state[0];
            else if (!h.off && f.kind === 'multi') value = h.state;
            const s = {
                key: f.key,
                kind: f.kind,
                id: f.id,
                off: h.off,
                value,
                box: isBox(f.kind) ? /** @type {[string, boolean]} */ ([f.box, h.state]) : null,
                owned: ownsIn(page, f),
                // Per control: an unbound radio of a server-owned group is no edit
                // of a server-owned control, though its group's is.
                mine: owned(f) && h.mine,
                keys: keys(f),
            };
            byName.set(nameOf(f), [...(byName.get(nameOf(f)) ?? []), s]);
        }
        // An id two controls of the scope share names neither: its whole name is
        // saved by position.
        for (const ss of byName.values())
            if (ss.some((s) => s.id && /** @type {number} */ (ids.get(s.id)) > 1))
                for (const s of ss) s.id = null;
        return byName;
    };
    /** @type {Map<string, Map<string, Saved[]>>} */
    const forms = new Map();
    for (const { id } of page.forms)
        forms.set(
            id,
            scope(
                ordered(page).filter((f) => f.form === id),
                (f) => f.name,
                (f) => [modelKey(f)],
            ),
        );
    const free = page.regions.formless.filter((f) => !f.gone && ownsIn(page, f));
    const formless = scope(free, freeName, (f) => [modelKey(f)]);
    return { forms, formless };
}

/**
 * The documented pairing of a name's fresh fields `fs` with its saved `ws`, boxes
 * left out of both: by id when every saved id names one fresh field of the scope,
 * of this name (a box too), and the
 * id-less fields are still the ones saved without one (every fresh id was saved,
 * or as many fields have none as did); else by position.
 * @param {Field[]} fs
 * @param {Saved[]} ws
 * @param {Field[]} all every fresh field of the scope
 * @param {(f: Field) => string} nameOf
 */
function pairing(fs, ws, all, nameOf) {
    const savedIds = ws.filter((w) => w.id).map((w) => w.id);
    const freshIdless = fs.filter((f) => !f.id).length;
    const savedIdless = ws.filter((w) => !w.id);
    const byId =
        savedIds.length > 0 &&
        savedIds.every((id) => {
            const owners = all.filter((f) => f.id === id);
            return owners.length === 1 && nameOf(owners[0]) === nameOf(fs[0]);
        }) &&
        (freshIdless === savedIdless.length || fs.every((f) => !f.id || savedIds.includes(f.id)));
    /** @type {Map<Field, Saved>} */
    const out = new Map();
    let k = 0;
    fs.forEach((f, i) => {
        const w = byId ? (f.id ? ws.find((x) => x.id === f.id) : savedIdless[k++]) : ws[i];
        if (w) out.set(f, w);
    });
    return out;
}

/**
 * The documented outcome of a restore of `page` from `saved`, field by field: every
 * field's state and the events the replay sends. A second check: `guarantees` does
 * not rely on it.
 * @param {Page} page
 * @param {ReturnType<typeof snapshot>} saved
 * @param {(key: string) => boolean} replayOk whether a form (or formless unit, by
 *   its `modelKey`) is not withheld by a crash before its last replay's ack
 */
function oracle(page, saved, replayOk) {
    /** @type {Map<Field, any>} */
    const want = new Map();
    for (const f of ordered(page)) want.set(f, f.server);
    /** @type {Set<Field>} */
    const captured = new Set();
    /** @type {string[]} */
    const events = [];
    const editedF = (/** @type {Field} */ f) =>
        ownsIn(page, f) && unitIn(page, f).some((x) => choice(want.get(x)) !== choice(x.server));
    /** @type {Field[]} */
    const typed = [];
    /** Set `f` to `v`; a checked radio unchecks the rest of its group. */
    const check = (/** @type {Field} */ f, /** @type {any} */ v) => {
        if (f.kind === 'radio' && v) for (const g of groupIn(page, f)) want.set(g, false);
        want.set(f, v);
    };
    const owns = (/** @type {Field} */ f) => ownsIn(page, f);
    /** Does a replay of saved `w` run: connected, and none of its unit's keys withheld? */
    const R = (/** @type {Saved} */ w) => w.keys.every(replayOk);
    /**
     * Restore one scope: its fresh fields `fs` from `byName`, named by `nameOf`.
     * Returns the server-owned fields given the user's state.
     * @param {Field[]} fs
     * @param {Map<string, Saved[]>} byName
     * @param {(f: Field) => string} nameOf
     */
    const scope = (fs, byName, nameOf) => {
        /** @type {Map<string, Field[]>} */
        const names = new Map();
        for (const f of fs) names.set(nameOf(f), [...(names.get(nameOf(f)) ?? []), f]);
        /** @type {Map<Field, Saved>} */
        const pair = new Map();
        for (const [name, nfs] of names) {
            const ws = byName.get(name);
            if (!ws) continue;
            // Boxes pair by value, so they take no id or position here.
            const texts = nfs.filter((f) => !isBox(f.kind));
            const was = ws.filter((w) => !w.box);
            if (texts.length > 0)
                for (const [f, w] of pairing(texts, was, fs, nameOf)) pair.set(f, w);
        }
        /** The radio groups the user edited, read off their radios. */
        const mineNames = new Set();
        for (const [name, ws] of byName)
            if (ws.some((w) => w.kind === 'radio' && w.mine)) mineNames.add(name);
        /** Does `w` hold the user's state, not a server-owned control's left alone? */
        const theirs = (/** @type {Saved} */ w, /** @type {string} */ name) =>
            !w.owned || (w.kind === 'radio' ? mineNames.has(name) : w.mine);
        /** @param {Field} f @param {Saved[]} ws */
        const boxOf = (f, ws) => {
            // The saved box of its kind and value at its position among those.
            const same = (/** @type {{kind: string}} */ x, /** @type {string} */ box) =>
                x.kind === f.kind && box === f.box;
            const n = fs.filter((x) => nameOf(x) === nameOf(f) && same(x, x.box)).indexOf(f);
            const w = ws.filter((x) => x.box && same(x, x.box[0]))[n];
            return w && !w.off && theirs(w, nameOf(f)) ? w : undefined;
        };
        // A radio group holding no choice of the user's, or whose chosen radio no
        // fresh radio may take, is written not at all.
        const stuck = new Set();
        for (const [name, ws] of byName) {
            const pick = ws.find(
                (w) => w.kind === 'radio' && !w.off && w.box?.[1] && theirs(w, name),
            );
            const takes =
                pick &&
                fs.some(
                    (f) =>
                        f.kind === 'radio' &&
                        nameOf(f) === name &&
                        (!f.disabled || !owns(f)) &&
                        boxOf(f, ws) === pick,
                );
            if (!takes && ws.some((w) => w.kind === 'radio')) stuck.add(name);
        }
        /** @type {Field[]} */
        const ours = [];
        for (const f of fs) {
            const ws = byName.get(nameOf(f)) ?? [];
            if (f.kind === 'radio' && stuck.has(nameOf(f))) continue;
            // A checkbox or radio pairs by kind and value, any other field as
            // `pairing` says.
            let P = isBox(f.kind) ? boxOf(f, ws) : pair.get(f);
            if (P && !theirs(P, nameOf(f))) P = undefined;
            const mine = !!P && R(P) && (f.kind === 'radio' ? mineNames.has(nameOf(f)) : P.mine);
            // A server-owned control keeps the resync's state unless it is the user's
            // replayed edit, and always when the resync disabled it.
            if (owns(f) && (!mine || f.disabled)) continue;
            if (!P) continue;
            /** @type {any} */
            let v;
            if (isBox(f.kind)) {
                v = /** @type {[string, boolean]} */ (P.box)[1];
            } else if (isText(f.kind)) {
                if (klass(P.kind) !== klass(f.kind) || typeof P.value !== 'string') continue;
                // What it cannot show exactly: a line break in a text input.
                if (f.readonly || !fits(f.kind, P.value)) continue;
                v = P.value;
            } else {
                if (P.value === undefined || P.kind !== f.kind) continue;
                const pvs = Array.isArray(P.value) ? P.value : [P.value];
                // An unbound multiple select keeps the saved choices still offered.
                const partial = f.kind === 'multi' && !owns(f);
                const ts = toks(f.options);
                if (!partial && !pvs.every((x) => ts.includes(x))) continue;
                v = ts.filter((o) => pvs.includes(o));
            }
            check(f, v);
            if (owns(f)) ours.push(f);
        }
        return ours;
    };
    for (const form of page.forms) {
        const { id, push } = form;
        const byName = saved.forms.get(id);
        if (!byName) continue;
        const fs = ordered(page).filter((f) => f.form === id);
        const ours = scope(fs, byName, (f) => f.name);
        if (!replayOk(formModelKey(form))) continue;
        for (const f of ours) if (editedF(f)) typed.push(f);
        if (push) {
            events.push(`chg_${id}`);
            // Any enabled radio of a server-owned group proposes the group.
            for (const f of fs)
                if ((f.kind === 'radio' ? owns(f) : owned(f)) && !f.disabled)
                    for (const g of unitIn(page, f)) captured.add(g);
        }
    }
    const free = page.regions.formless.filter((f) => ownsIn(page, f));
    for (const f of scope(free, saved.formless, freeName)) if (editedF(f)) typed.push(f);
    for (const f of typed) {
        if (!f.own || captured.has(f) || (f.kind === 'radio' && !want.get(f))) continue;
        events.push(`own_${f.key}`);
        for (const g of unitIn(page, f)) captured.add(g);
    }
    // A field shown as the user's that no replay proposes shows the server's (a
    // radio: its whole group).
    for (const f of typed)
        if (!captured.has(f)) for (const g of unitIn(page, f)) want.set(g, g.server);
    return { want, events };
}

/**
 * The guarantees of `.claude/rules/js.md`, from what the test knows: `old` and `page`
 * (the pages before and after the resync), `pre` (what each field held at the
 * resync), `gt` (what the user did), `sent` (the events the restore sent) and
 * `suspect` (the forms and formless fields a crash withholds the replay of).
 * @param {string} ctx
 * @param {Page} old
 * @param {Page} page
 * @param {Map<string, Held>} pre
 * @param {ReturnType<typeof truth>} gt
 * @param {any[][]} sent
 * @param {Set<string>} suspect
 * @returns {Map<string, number>} when the user made the choice each formless radio
 *   group shows, by the key of the radio showing it
 */
function guarantees(ctx, old, page, pre, gt, sent, suspect) {
    /** @type {Map<string, number>} */
    const restoredAt = new Map();
    const fail = (/** @type {string} */ why, /** @type {Field} */ f) =>
        `${ctx}\n${why}: ${f.key} (${f.kind} ${f.form ?? 'formless'}/${f.name}) shows ` +
        `${choice(shows(f))}, server ${choice(f.server)}`;
    /** What withholds `f`'s replay: its unit as the drop saved it. */
    const replayKey = (/** @type {Field} */ f) => modelKey(pre.get(f.key)?.f ?? f);
    const events = sent.map((x) => x[1]);
    const pushes = (/** @type {Field} */ f) =>
        f.form !== null && !!page.forms.find((x) => x.id === f.form)?.push;
    /** Did a replay propose `f` (a radio: its group)? */
    const proposed = (/** @type {Field} */ f) =>
        (f.form !== null &&
            events.includes(`chg_${f.form}`) &&
            ownsIn(page, f) &&
            // A radio of a server-owned group proposes the group, bound or not.
            unitIn(page, f).some((x) => !x.disabled)) ||
        unitIn(page, f).some((x) => events.includes(`own_${x.key}`));
    /**
     * The shape of `f`'s name: which fields, of which kinds and values, in order. A
     * field that is no box is placed among its name's other such fields alone: boxes
     * pair by value, so one inserted or removed moves no edit.
     */
    const shape = (/** @type {Page} */ p, /** @type {Field} */ f) =>
        JSON.stringify(
            named(p, f)
                .filter((x) => isBox(f.kind) || !isBox(x.kind))
                .map((x) => [x.key, x.kind, isBox(x.kind) ? x.box : '']),
        );
    /**
     * The field of `old` whose state `f` takes, where the rules promise one: itself on
     * a name the resync kept in place, the field of its id on a name whose every field
     * has an id of its own both times, and itself by id when formless. Else null: a
     * reshaped name may move an edit by position.
     * @param {Field} f
     * @returns {Field|null}
     */
    const twin = (f) => {
        if (!f.form) {
            // By its id among the server-owned formless fields of its id in its view
            // (rule 2: a view is its id): itself when it is in the view it was saved
            // in and those are the same fields in the same order.
            const o = pre.get(f.key)?.f;
            if (!o || o.form || o.id !== f.id || o.view !== f.view) return null;
            const keys = (/** @type {Field[]} */ xs) => xs.map((x) => x.key).join();
            // Boxes pair by value, so they take no position.
            const of = (/** @type {Page} */ p, /** @type {Field} */ x) =>
                !x.form &&
                x.id === f.id &&
                x.view === f.view &&
                ownsIn(p, x) &&
                (isBox(f.kind) || !isBox(x.kind));
            return keys(ordered(old).filter((x) => of(old, x))) ===
                keys(ordered(page).filter((x) => of(page, x)))
                ? o
                : null;
        }
        if (shape(old, f) === shape(page, f))
            return ordered(old).find((x) => x.key === f.key) ?? null;
        // A checkbox pairs by value: by itself when it alone of its name has its kind
        // and value, before the resync and after.
        if (f.kind === 'checkbox') {
            const like = (/** @type {Page} */ p) =>
                named(p, f).filter((x) => x.kind === f.kind && x.box === f.box);
            const [a, b] = [like(old), like(page)];
            return a.length === 1 && b.length === 1 ? a[0] : null;
        }
        const own = (/** @type {Page} */ p) => {
            const ids = ordered(p)
                .filter((x) => ident(x) === ident(f))
                .map((x) => x.id);
            return named(p, f).every((x) => x.id && ids.filter((i) => i === x.id).length === 1);
        };
        if (isBox(f.kind) || !own(old) || !own(page)) return null;
        if (!named(old, f).every((x) => named(page, f).some((y) => y.id === x.id))) return null;
        return named(old, f).find((x) => x.id === f.id) ?? null;
    };
    // (3) No saved state is restored onto more fields of a form's name than held it.
    /** @type {Map<string, Field[]>} */
    const byName = new Map();
    for (const f of ordered(page))
        if (f.form && !isBox(f.kind) && f.kind !== 'multi')
            byName.set(partKey(f), [...(byName.get(partKey(f)) ?? []), f]);
    for (const fs of byName.values()) {
        /** @type {Map<string, number>} */
        const shown = new Map();
        for (const f of fs) {
            const k = `${klass(f.kind)}\0${choice(shows(f))}`;
            if (choice(shows(f)) !== choice(f.server)) shown.set(k, (shown.get(k) ?? 0) + 1);
        }
        /** @type {Map<string, number>} */
        const had = new Map();
        for (const x of named(old, fs[0])) {
            const h = /** @type {Held} */ (pre.get(x.key));
            const k = `${klass(x.kind)}\0${choice(h.state)}`;
            if (!h.off && (!ownsIn(old, x) || h.mine)) had.set(k, (had.get(k) ?? 0) + 1);
        }
        for (const [k, n] of shown)
            expect(
                n,
                `${ctx}\n${fs[0].name}: ${k} restored onto more fields than held it`,
            ).toBeLessThanOrEqual(had.get(k) ?? 0);
    }

    // Outside every form no state lands in a view of another id than the one it was
    // saved from: a field showing a state the user made took it from a field of its
    // id in its own view.
    for (const f of page.regions.formless) {
        if (f.kind === 'radio' || choice(shows(f)) === choice(f.server)) continue;
        expect(
            ordered(old).some(
                (x) =>
                    !x.form &&
                    x.id === f.id &&
                    x.view === f.view &&
                    choice(/** @type {Held} */ (pre.get(x.key)).state) === choice(shows(f)),
            ),
            fail(`a state in view ${f.view} it was not saved from`, f),
        ).toBe(true);
    }

    /**
     * The fields of `old` whose state `f` may take: its name's, or outside every form
     * its id's in its view.
     */
    const donors = (/** @type {Field} */ f) =>
        f.form
            ? named(old, f)
            : ordered(old).filter((x) => !x.form && x.id === f.id && x.view === f.view);

    for (const f of ordered(page)) {
        // Outside every form only a server-owned unit takes the user's state.
        if (!f.form && !ownsIn(page, f))
            expect(choice(shows(f)), fail('an unbound formless field takes a state', f)).toBe(
                choice(f.server),
            );
        if (f.kind === 'radio') continue; // as a group, below
        const s = shows(f);
        const server = choice(s) === choice(f.server);
        const owns = ownsIn(page, f);
        const was = pre.get(f.key);
        // (1) A server-owned field shows the server's state unless the user edited
        // what it takes its state from, its replay is not withheld, the resync left it
        // enabled, and a replay proposes it.
        if (owns && !server) {
            const mine = donors(f).some((x) => /** @type {Held} */ (pre.get(x.key)).mine);
            expect(mine, fail('a server-owned field shows what no edit made', f)).toBe(true);
            // Outside every form a shared id pairs by position: withheld when every
            // field it could have come from was.
            const from = f.form
                ? [pre.get(f.key)?.f ?? f]
                : donors(f).filter(
                      (x) => choice(/** @type {Held} */ (pre.get(x.key)).state) === choice(s),
                  );
            expect(
                from.length > 0 && from.every((x) => suspect.has(modelKey(x))),
                fail('a withheld replay', f),
            ).toBe(false);
            expect(f.disabled, fail('disabled by the resync', f)).toBe(false);
            expect(proposed(f), fail('a user value no replay proposes', f)).toBe(true);
        }
        // (2) No foreign state: a field shows the server's state or what a field of
        // its name held at the resync (formless: of its id), enabled then and of a
        // kind restored alike (a checkbox: of its value) -- the field it was typed
        // into wherever the rules promise that.
        if (!server) {
            const t = twin(f);
            const from = (t ? [t] : donors(f)).filter((x) => {
                const h = /** @type {Held} */ (pre.get(x.key));
                if (h.off || klass(x.kind) !== klass(f.kind)) return false;
                // What a server-owned control the user left alone held was the server's.
                if (ownsIn(old, x) && !h.mine) return false;
                if (isBox(f.kind) && x.box !== f.box) return false;
                if (choice(h.state) === choice(s)) return true;
                // An unbound multiple select keeps the saved choices still offered.
                const offered = h.state.filter?.((/** @type {string} */ o) =>
                    toks(f.options).includes(o),
                );
                return f.kind === 'multi' && !owns && choice(offered) === choice(s);
            });
            expect(from.length > 0, fail('a state no field held at the resync', f)).toBe(true);
            expect(f.readonly, fail('a readonly field takes a saved state', f)).toBe(false);
        }
        // (4) An unbound field the rules pair with itself, enabled at the drop and not
        // readonly, keeps what it held when it can show it exactly, disabled by the
        // resync or not -- unless it was server-owned then, and the user left it alone
        // (that was the server's).
        const t = twin(f);
        const theirs = was && (!ownsIn(old, was.f) || was.mine);
        if (!owns && f.form && was && theirs && !was.off && !f.readonly && t?.key === f.key) {
            /** @type {any} */
            let want = was.state;
            if (klass(t.kind) !== klass(f.kind) || (f.kind === 'checkbox' && t.box !== f.box))
                want = null;
            // What it cannot show exactly keeps the resync's state (rule 4).
            else if (isText(f.kind) && !fits(f.kind, want)) want = null;
            else if (f.kind === 'select' && !toks(f.options).includes(want[0])) want = null;
            else if (f.kind === 'multi')
                want = want.filter((/** @type {string} */ o) => toks(f.options).includes(o));
            if (want !== null)
                expect(choice(s), fail(`an unbound field loses ${choice(want)}`, f)).toBe(
                    choice(want),
                );
        }
        // (5) A server-owned field the user edited, paired with itself by the rules,
        // enabled then and now, its replay not withheld and some replay able to
        // propose it, shows what the user put there.
        const ok =
            owns &&
            was?.mine &&
            !was.off &&
            !f.disabled &&
            !f.readonly &&
            t?.key === f.key &&
            t.kind === f.kind &&
            (!isBox(f.kind) || t.box === f.box) &&
            !suspect.has(replayKey(f)) &&
            (pushes(f) || f.own) &&
            (!Array.isArray(was.state) ||
                was.state.every((/** @type {string} */ o) => toks(f.options).includes(o)));
        if (ok && was) {
            expect(choice(s), fail(`a server-owned edit loses ${choice(was.state)}`, f)).toBe(
                choice(was.state),
            );
            if (!server) expect(proposed(f), fail('an edit no replay proposes', f)).toBe(true);
        }
    }

    // Radio groups, as units. After the resync every field is in one document; at
    // the drop a name was one group per document holding it (a view popped out),
    // each with its own choice.
    /** @type {Map<string, Field[]>} */
    const groups = new Map();
    for (const f of ordered(page))
        if (f.kind === 'radio') groups.set(`${nameKey(f)}\0${f.doc}`, groupIn(page, f));
    const at = (/** @type {Field} */ x) =>
        gt.actIn.get(x.key) === nameKey(x) ? (gt.at.get(x.key) ?? 0) : 0;
    const live = new Set(['page', ...page.views.map((v) => v.id)]);
    // A radio's part: a form's controls of a view (that view and the form's HTML id),
    // else a view's.
    const where = (/** @type {Field} */ x) => (x.form ? `${ident(x)}` : x.view);
    for (const g of groups.values()) {
        const f = g[0];
        const on = g.filter((x) => shows(x));
        const tag = `${ctx}\nradio group ${f.form ?? 'formless'}/${f.name}: ${JSON.stringify(
            g.map((x) => [x.key, x.view, x.box, x.disabled, x.bound, x.server, shows(x)]),
        )}`;
        const owns = ownsIn(page, f);
        // A form's group is withheld whole; a formless one by each view's part (the
        // part its choice came from, below).
        const withheld = !!f.form && suspect.has(modelKey(f));
        // (6) A group the server renders a choice in shows one: the user cannot
        // uncheck a radio, so an empty group is a choice nobody made.
        if (g.some((x) => x.server)) expect(on.length, `${tag}\nan empty group`).toBe(1);
        // Its name's radios at the drop, in every view and document.
        const all = ordered(old).filter(
            (x) => x.kind === 'radio' && x.name === f.name && x.form === f.form,
        );
        const ownedThen = all.some((x) => ownsIn(old, x));
        const views = [...new Set(all.map(where))];
        // A server-owned group, or one whose radios the user chose in two views, is
        // restored as the user's acts decide; an unbound one in one view keeps what it
        // held at the drop (rule 4).
        // Outside every form only a server-owned group is saved.
        if (!f.form && !ownedThen) {
            for (const x of g)
                expect(shows(x), `${tag}\nan unbound formless group takes a choice`).toBe(
                    !!x.server,
                );
            continue;
        }
        if (!f.form || ownedThen || (views.length > 1 && all.some((x) => at(x) > 0))) {
            actGroup(g, all, views, ownedThen, tag, owns, withheld);
            continue;
        }
        // Each part keeps what it held: parts a window split apart may each hold a
        // choice, of which the group shows one.
        const helds = all.filter((x) => pre.get(x.key)?.state && !pre.get(x.key)?.off);
        const was = helds.length === 1 ? helds[0] : null;
        if (on.length > 0 && !on[0].server) {
            expect(
                helds.some((x) => x.box === on[0].box),
                `${tag}\na choice the group did not hold`,
            ).toBe(true);
            if (owns)
                expect(owns, `${tag}\na server-owned group shows what no edit made`).toBe(false);
        }
        const now = was && g.find((x) => x.key === was.key);
        // A server-owned group now takes no choice an unbound one held (rule 4).
        if (now && !owns && now.view === was.view && shape(old, now) === shape(page, now))
            expect(shows(now), `${tag}\nthe choice ${now.key} it held is lost`).toBe(true);
    }

    /**
     * (6) A radio group from what the test knows alone: the user's acts on its name
     * and the view (by id) each radio was in at the drop. Each view's part holds the
     * radio the user's last act there chose -- none when a server write on the name
     * superseded that act, it was disabled at the drop or removed, or, the group
     * server-owned, it chose the server's -- and the group shows the choice of the
     * part acted on last, of those holding none or a choice its radio, still in that
     * view, can take; the server's when that part holds none. An unbound group's part
     * a server write superseded holds what it held at the drop.
     * @param {Field[]} g
     * @param {Field[]} all
     * @param {string[]} views
     * @param {boolean} ownedThen
     * @param {string} tag
     * @param {boolean} owns
     * @param {boolean} withheld
     */
    function actGroup(g, all, views, ownedThen, tag, owns, withheld) {
        const held = (/** @type {Field[]} */ u) =>
            u.find((x) => pre.get(x.key)?.state && !pre.get(x.key)?.off) ?? null;
        const parts = views
            .map((v) => all.filter((x) => where(x) === v))
            // Outside every form only a part whose group was server-owned at the drop
            // was saved. An unbound part no act touched holds what it held (rule 4).
            .filter(
                (u) =>
                    (u.some((x) => at(x) > 0) || (!ownsIn(old, u[0]) && !!held(u))) &&
                    (u[0].form !== null || ownsIn(old, u[0])) &&
                    // Outside every form a name is one group per document: a part counts
                    // only where its view is now (a resync keeps a popped-out one in its
                    // window), the group's document.
                    (u[0].form !== null || viewDoc(u[0].view) === g[0]?.doc),
            )
            .map((u) => {
                const last = u.reduce((a, b) => (at(b) > at(a) ? b : a));
                const h = pre.get(last.key);
                const ownedPart = ownsIn(old, u[0]);
                /** @type {Field|null} */
                // An unbound part holds what it shows: code may have moved it after
                // the user's pick. A server-owned one holds its pick while it stands.
                let r = last;
                if (!ownedPart) r = held(u);
                else if (at(last) === 0 || (gt.stamp.get(last.key) ?? 0) > at(last)) r = null;
                // A pick of the radio the server checks by default at the drop is a
                // return to its choice.
                else if (!h || h.off || last.server) r = null;
                return {
                    u,
                    view: u[0].view,
                    where: where(u[0]),
                    at: at(last),
                    r,
                    // A form's part has its place in its form in its view: with no form
                    // of its id and no control naming one left there, it is gone.
                    kept:
                        live.has(u[0].view) &&
                        (u[0].form === null ||
                            ordered(page).some((x) => ident(x) === ident(u[0])) ||
                            page.forms.some((x) => x.view === u[0].view && x.html === u[0].fhtml)),
                    ownedPart,
                };
            });
        /** Is part `h`'s choice certainly placeable: its radio still in its view, alone of its value there? */
        const placeable = (/** @type {(typeof parts)[number]} */ h) => {
            const r = h.r;
            const now = r && g.find((x) => x.key === r.key);
            return (
                !!now &&
                !!r &&
                where(now) === h.where &&
                h.kept &&
                now.box === r.box &&
                g.filter((x) => where(x) === h.where && x.box === r.box).length === 1 &&
                h.u.filter((x) => x.box === r.box).length === 1
            );
        };
        const s = g.find((x) => shows(x));
        if (process.env.DBGA)
            process.stderr.write(
                `DBGA ${tag.split('\n').at(-1)}\n  parts=${JSON.stringify(parts.map((h) => [h.view, h.at, h.r?.key, h.kept, h.u.map((x) => [x.key, x.box, at(x), pre.get(x.key)?.state, pre.get(x.key)?.off, pre.get(x.key)?.mine, gt.stamp.get(x.key), x.doc])]))} owns=${owns} ownedThen=${ownedThen} withheld=${withheld}\n`,
            );
        if (g.some((x) => shows(x) !== !!x.server)) {
            // It is the choice a part holds, in that part's own view, and no part the
            // user acted on later, its view still there, holds none or a choice that
            // certainly has its radio (radios sharing a value pair by order).
            const from = parts
                .filter((h) => h.r?.box === s?.box && !!s && where(s) === h.where)
                .reduce(
                    (/** @type {(typeof parts)[number]|null} */ a, h) =>
                        !a || h.at > a.at ? h : a,
                    null,
                );
            expect(
                !!from,
                `${tag}\na choice no part the user acted on holds in its view (${s?.view})`,
            ).toBe(true);
            if (!from || !s) return;
            for (const h of parts)
                if (h.at > from.at && h.kept)
                    expect(
                        !!h.r && !placeable(h),
                        `${tag}\nan older choice over the part the user acted on last`,
                    ).toBe(true);
            if (owns) {
                // A server-owned group takes only a server-owned part's edit (rule 4).
                expect(
                    from.ownedPart,
                    `${tag}\nan unbound part's choice in a server-owned group`,
                ).toBe(true);
                // Withheld as the drop saved it: by the form it was in then.
                const fromHeld = suspect.has(modelKey(/** @type {Field} */ (from.r)));
                expect(withheld || fromHeld, `${tag}\na withheld replay`).toBe(false);
                expect(proposed(s), `${tag}\na user choice no replay proposes`).toBe(true);
            }
            // A restore writes an act only into a server-owned group (it is the
            // group's edit then); an unbound group's restored choice is what it held.
            if (owns) restoredAt.set(s.key, from.at);
        }
        // The part the user acted on last, its choice certainly placeable in its own
        // view, shows it when the restore can propose it.
        const last = parts.reduce(
            (/** @type {(typeof parts)[number]|null} */ a, h) => (!a || h.at > a.at ? h : a),
            null,
        );
        if (!last?.r || !placeable(last)) return;
        const now = /** @type {Field} */ (g.find((x) => x.key === last.r?.key));
        // An unbound field takes the user's state in a form alone: outside every form
        // only a server-owned unit is restored.
        const lastHeld = withheld || suspect.has(modelKey(last.r));
        const restores = owns
            ? last.ownedPart && !now.disabled && !lastHeld && (pushes(now) || now.own)
            : now.form !== null;
        if (restores)
            expect(shows(now), `${tag}\nthe user's last choice ${now.key} is lost`).toBe(true);
    }

    // (8) After a crash, nothing the last restore replayed is replayed again.
    for (const [, ev] of sent) {
        if (ev.startsWith('chg_'))
            expect(
                suspect.has(formModelKey(formOf(page, ev.slice(4)))),
                `${ctx}\n${ev} replayed again after a crash`,
            ).toBe(false);
        const f = ordered(page).find((x) => ev === `own_${x.key}`);
        const o = f && pre.get(f.key)?.f;
        if (!f || !o) continue;
        // What it replays came from its own state, or, outside every form, from any
        // field of its id in its view holding it (a shared id pairs by position).
        const from =
            f.kind === 'radio' || f.form
                ? [o]
                : ordered(old).filter(
                      (x) =>
                          !x.form &&
                          x.id === f.id &&
                          x.view === f.view &&
                          choice(/** @type {Held} */ (pre.get(x.key)).state) === choice(shows(f)),
                  );
        if (f.kind === 'radio' || o.id === f.id)
            expect(
                from.length > 0 && from.every((x) => suspect.has(modelKey(x))),
                `${ctx}\n${ev} replayed again after a crash`,
            ).toBe(false);
    }

    // (9) A form the drop saved and the resync renders again in its view (as the
    // view's own element too) replays its `az-change`, touched or not and whatever
    // controls it holds now, unless a crash withholds it.
    for (const fm of page.forms) {
        const o = old.forms.find((x) => x.id === fm.id);
        if (!fm.push || !o || o.view !== fm.view || o.html !== fm.html) continue;
        if (suspect.has(formModelKey(o))) continue;
        expect(events.includes(`chg_${fm.id}`), `${ctx}\nchg_${fm.id} is not replayed`).toBe(true);
    }

    // (7) A replay sends only values the user chose, the server's, or what an unbound
    // field held at the resync, and an own replay only an enabled server-owned
    // field's, of values the user chose.
    for (const [, ev, payload] of sent) {
        if (ev.startsWith('chg_')) {
            const form = ev.slice(4);
            for (const [name, v] of Object.entries(payload)) {
                // The form's fields of the name, each taking its state from the fields of
                // its part (`partKey`) at the resync.
                const cur = ordered(page).filter((y) => y.form === form && y.name === name);
                const parts = new Set(cur.map(partKey));
                const from = ordered(old).filter(
                    (y) => parts.has(partKey(y)) || (y.form === form && y.name === name),
                );
                const ok = new Set();
                for (const x of [...cur, ...from])
                    for (const y of gt.values.get(partKey(x)) ?? []) ok.add(y);
                for (const x of cur) for (const y of valuesOf(x, x.server)) ok.add(y);
                for (const x of from) {
                    const h = /** @type {Held} */ (pre.get(x.key));
                    if (!owned(x) && !h.off) for (const y of valuesOf(x, h.state)) ok.add(y);
                }
                const vs = Array.isArray(v) ? v : [v];
                for (const x of vs)
                    expect(
                        ok.has(x),
                        `${ctx}\n${ev} replays ${name}=${x}, which nobody chose`,
                    ).toBe(true);
                // Nor more of a value than the fields of its name submitted: each field
                // as many as the user chose or it held unbound, plus what the server
                // renders in it now.
                const keys = new Set([...cur, ...from].map((y) => y.key));
                for (const x of new Set(vs)) {
                    let most = 0;
                    for (const key of keys) {
                        const h = pre.get(key);
                        const now = ordered(page).find((y) => y.key === key);
                        most += Math.max(
                            gt.counts.get(key)?.get(x) ?? 0,
                            h && !owned(h.f) && !h.off ? count(valuesOf(h.f, h.state), x) : 0,
                        );
                        if (now) most += count(valuesOf(now, now.server), x);
                    }
                    expect(
                        count(vs, x),
                        `${ctx}\n${ev} replays ${name}=${x} more times than chosen`,
                    ).toBeLessThanOrEqual(most);
                }
            }
        } else if (ev.startsWith('own_')) {
            const f = ordered(page).find((x) => `own_${x.key}` === ev);
            expect(
                !!f && !f.disabled && ownsIn(page, f),
                `${ctx}\n${ev} replays no enabled bound field`,
            ).toBe(true);
            if (!f) continue;
            // Outside every form a field pairs by its id, so the values of a field
            // that shared it at the resync are its own to replay.
            const ok = new Set(gt.values.get(partKey(f)) ?? []);
            if (!f.form && f.id)
                for (const x of ordered(old))
                    if (!x.form && x.id === f.id && x.view === f.view)
                        for (const v of gt.values.get(partKey(x)) ?? []) ok.add(v);
            const vs = isBox(f.kind)
                ? [payload.checked ? payload.value : `!${payload.value}`]
                : Array.isArray(payload.value)
                  ? payload.value
                  : [payload.value];
            for (const x of vs)
                expect(ok.has(x), `${ctx}\n${ev} replays ${x}, which the user never chose`).toBe(
                    true,
                );
        }
    }
    return restoredAt;
}

/** How often each interesting case came up, printed with `STATS=1`. */
/** @type {Object<string, number>} */
const seen = {};
const bump = (/** @type {string} */ k, n = 1) => {
    seen[k] = (seen[k] ?? 0) + n;
};

/**
 * Open view `id` in a PiP window, a second document, as `requestPip` does on a user
 * gesture; closing it brings the view back.
 * @param {any} c
 * @param {string} id
 */
async function popOut(c, id) {
    const doc = document.implementation.createHTMLDocument('pip');
    /** @type {Record<string, Function>} */
    const handlers = {};
    const win = {
        document: doc,
        addEventListener(/** @type {string} */ t, /** @type {Function} */ fn) {
            handlers[t] = fn;
        },
        close() {
            handlers.pagehide?.();
            if (pipWin === win) [pipDoc, pipWin] = [null, null];
        },
    };
    /** @type {any} */ (window).documentPictureInPicture = { requestWindow: async () => win };
    await c.mod.requestPip(id);
    [pipDoc, pipWin] = [doc, win];
}

/**
 * Maybe pop a view of `page` out to a PiP window, or bring the popped one back.
 * Returns whether a window was open at any point.
 * @param {ReturnType<typeof rng>} r
 * @param {any} c
 * @param {Page} page
 */
async function pipStep(r, c, page) {
    const was = pipWin !== null;
    if (pipWin && r.chance(0.25)) {
        pipWin.close();
        bump('pop-backs');
    } else if (!pipWin && r.chance(0.5)) {
        // A child view most often: the resync brings it back inline, into the
        // page's document. Not one holding a form a field outside it names: `form=`
        // names a form of its own document, so the field would be no form's.
        const named = new Set([...page.regions.before, ...page.regions.after].map((f) => f.fview));
        /** Is view `v` or a view in it one such form is in? @param {string} v @returns {boolean} */
        const holds = (v) => named.has(v) || page.views.some((w) => w.parent === v && holds(w.id));
        const ids = page.views.map((v) => v.id).filter((v) => !holds(v));
        await popOut(c, ids.length > 0 && r.chance(0.75) ? r.pick(ids) : 'page');
        bump('pop-outs');
    }
    locate(page);
    return was || pipWin !== null;
}

/**
 * Events code dispatches that change nothing -- a `change` or `input` on a field as
 * it is, an unchecked radio included: no edit, and no act.
 * @param {ReturnType<typeof rng>} r
 * @param {Page} page
 */
function noise(r, page) {
    for (const f of ordered(page))
        if (r.chance(0.08))
            elOf(f).dispatchEvent(new Event(r.pick(['change', 'input']), { bubbles: true }));
}

/**
 * The server moving a radio group's choice while connected: an effect (`set_attr`,
 * `remove_attr` or `toggle_attr` of `checked`, after removing it from the radios
 * holding it) or a diff re-rendering the group's radios (`OP_TEXT`). What the server
 * renders follows. It is no act of the user's, and a re-rendered radio is a fresh one
 * no act touched.
 * @param {ReturnType<typeof rng>} r
 * @param {any} c
 * @param {Page} page
 * @param {ReturnType<typeof truth>} gt
 */
async function serverWrites(r, c, page, gt) {
    const radios = ordered(page).filter((f) => f.kind === 'radio');
    const sel = (/** @type {Field} */ x) => `[data-pk="${x.key}"]`;
    for (const f of radios) {
        if (f.gone || !r.chance(0.3)) continue;
        const name = radios.filter((x) => !x.gone && x.form === f.form && x.name === f.name);
        if (f.doc !== 'main' || name.some((x) => !x.live)) continue;
        const op = r.pick(['set', 'unset', 'toggle', 'text', 'gone']);
        // The radio the group showed before the write, which the user may choose again.
        const prev = name.find((x) => shows(x));
        if (op === 'gone') {
            // The server checks it, then a diff removes it: the group shows none.
            for (const x of name) x.server = x === f;
            const was = name.filter((x) => x !== f && elOf(x).hasAttribute('checked'));
            await deliver(r, c, page, gt, [
                ...was.map((x) => [8, sel(x), 'checked']),
                [7, sel(f), 'checked', ''],
            ]);
            serverOp(page, gt, () => {
                c.msg([[0, `${f.view}:${f.key}`, '', true]]);
                f.gone = true;
            });
            gt.at.delete(f.key);
            gt.edits.delete(f.key);
            // In its document: a window's radios are another document's group.
            const here = name.filter((x) => x !== f && x.doc === f.doc);
            expect(
                here.map((x) => shows(x)),
                `the server's removal of ${f.key}`,
            ).toEqual(here.map(() => false));
        } else if (op === 'text') {
            for (const x of name) x.server = x === f;
            serverOp(page, gt, () =>
                c.msg(name.map((x) => [0, `${x.view}:${x.key}`, controlHtml(x), true])),
            );
            for (const x of name) gt.at.delete(x.key);
        } else if (op === 'unset' || (op === 'toggle' && elOf(f).hasAttribute('checked'))) {
            f.server = false;
            await deliver(r, c, page, gt, [[op === 'unset' ? 8 : 21, sel(f), 'checked']]);
        } else {
            const was = name.filter((x) => x !== f && elOf(x).hasAttribute('checked'));
            for (const x of name) x.server = x === f;
            await deliver(r, c, page, gt, [
                ...was.map((x) => [8, sel(x), 'checked']),
                op === 'set' ? [7, sel(f), 'checked', ''] : [21, sel(f), 'checked'],
            ]);
        }
        if (op === 'set' || op === 'text')
            expect(
                name.filter((x) => x.doc === f.doc).map((x) => shows(x)),
                `the server's ${op} of ${f.key}`,
            ).toEqual(name.filter((x) => x.doc === f.doc).map((x) => x === f));
        bump(`server ${op}s`);
        // The user choosing again what the write moved the group away from: an act.
        const back = prev && !prev.gone && elOf(prev);
        if (back && !back.checked && !back.matches(':disabled') && r.chance(0.6)) {
            acted(page, gt, prev);
            back.click();
            landed(page, gt, prev, false);
            bump('choices after a server write');
        }
    }
}

/**
 * A form reset (the user's reset button, or code): every field of the form shows its
 * default, which states every radio group of it (see `truth`).
 * @param {ReturnType<typeof rng>} r
 * @param {Page} page
 * @param {ReturnType<typeof truth>} gt
 */
function resets(r, page, gt) {
    const forms = page.forms.filter((f) => !f.root);
    if (forms.length === 0) return;
    const f = r.pick(forms);
    const el = /** @type {any} */ (
        [document, pipDoc].map((d) => d?.querySelector(`[data-form="${f.id}"]`)).find((x) => x)
    );
    if (!el) return;
    el.reset();
    for (const x of ordered(page))
        if (x.kind === 'radio' && !x.gone && x.form === f.id) stated(page, gt, x);
    bump('resets');
}

/**
 * An effect writing a radio's `name`, `type` or `form` away and back, the user acting
 * meanwhile: it leaves its group and returns. Moved in checked, it states the group
 * it joins (see `joined`); a pick made away is for the group it was made in.
 * @param {ReturnType<typeof rng>} r
 * @param {any} c
 * @param {Page} page
 * @param {ReturnType<typeof truth>} gt
 */
function renames(r, c, page, gt) {
    const radios = ordered(page).filter((f) => f.kind === 'radio' && !f.gone);
    if (radios.length === 0) return;
    const f = r.pick(radios);
    const sel = `[data-pk="${f.key}"]`;
    const attr = r.pick(['name', 'type', 'form']);
    const back =
        attr === 'name'
            ? [7, sel, 'name', f.name]
            : attr === 'type'
              ? [7, sel, 'type', 'radio']
              : f.region === 'before' || f.region === 'after'
                ? [7, sel, 'form', f.fhtml]
                : [8, sel, 'form'];
    const away = attr === 'name' ? 'zz' : attr === 'type' ? 'checkbox' : 'nowhere';
    serverOp(page, gt, () => {
        c.fx([[7, sel, attr, away]]);
        if (attr === 'type') f.awayType = true;
        else f.awayKey = attr === 'name' ? `${f.form}\0zz` : `null\0${f.name}`;
    });
    if (r.chance(0.5)) userEdits(r, page, gt);
    serverOp(page, gt, () => {
        c.fx([back]);
        delete f.awayKey;
        delete f.awayType;
    });
    bump(`${attr} writes away and back`);
}

/**
 * Move the radios naming a form (`form=`) out of its group and back, code firing a
 * `change` on every radio meanwhile: an effect writing the form's id away and back,
 * or a pop-out of the form's view and back, the user editing while it is out.
 * @param {ReturnType<typeof rng>} r
 * @param {any} c
 * @param {Page} page
 * @param {ReturnType<typeof truth>} gt
 */
async function regroup(r, c, page, gt) {
    const codeChanges = () => {
        for (const f of ordered(page))
            if (f.kind === 'radio') elOf(f).dispatchEvent(new Event('change', { bubbles: true }));
    };
    const named = new Set([...page.regions.before, ...page.regions.after].map((f) => f.form));
    const forms = page.forms.filter((f) => named.has(f.id) && !f.root);
    if (forms.length === 0) return;
    const f = r.pick(forms);
    const el = document.querySelector(`[data-form="${f.id}"]`);
    if (!el) return;
    if (!pipWin && f.view !== 'page' && r.chance(0.5)) {
        await popOut(c, f.view);
        locate(page);
        userEdits(r, page, gt);
        codeChanges();
        /** @type {any} */ (pipWin).close();
        locate(page);
        bump('form pop-outs and back');
        return;
    }
    const sel = `[data-form="${f.id}"]`;
    // The radios naming it leave its group for the formless one, and come back:
    // moved in checked, each states the group it joins (see `joined`).
    // Its radios move with it: a group is the form's id as the server renders it, so
    // those in it are the renamed form's meanwhile, and those naming it (`form=`) are
    // no form's.
    const moved = ordered(page).filter((x) => x.kind === 'radio' && !x.gone && x.form === f.id);
    const naming = (/** @type {Field} */ x) => x.region === 'before' || x.region === 'after';
    serverOp(page, gt, () => {
        c.fx([[7, sel, 'id', `${f.html}-away`]]);
        for (const x of moved)
            x.awayKey = naming(x) ? `null\0${x.name}` : `${f.id}-away\0${x.name}`;
    });
    codeChanges();
    if (r.chance(0.5)) userEdits(r, page, gt);
    serverOp(page, gt, () => {
        c.fx([[7, sel, 'id', f.html]]);
        for (const x of moved) delete x.awayKey;
    });
    bump('form id writes away and back');
}

/** One run: a page dropped one to four times, checked after every resync. */
async function run(/** @type {number} */ seed) {
    const r = rng(seed);
    const g = gen(r);
    let page = g.page();
    const c = await start(render(page));
    locate(page);
    const gt = truth();
    /** What the last restore replayed, by `modelKey`: a crash's suspects. */
    /** @type {Set<string>} */
    let replayed = new Set();
    const drops = 1 + r.int(4);
    for (let d = 0; d < drops; d++) {
        // Views popped out and back before the drop, the user editing and code
        // dispatching events between, and between the drop and the resync.
        let pip = false;
        for (let i = 1 + r.int(3); i > 0; i--) {
            pip = (await pipStep(r, c, page)) || pip;
            userEdits(r, page, gt);
            noise(r, page);
        }
        // The server moving radio groups, code dispatching events, and the user
        // acting again after.
        if (r.chance(0.7)) {
            await serverWrites(r, c, page, gt);
            noise(r, page);
            userEdits(r, page, gt);
        }
        if (r.chance(0.3)) await regroup(r, c, page, gt);
        if (r.chance(0.25)) renames(r, c, page, gt);
        if (r.chance(0.2)) {
            resets(r, page, gt);
            noise(r, page);
            if (r.chance(0.5)) userEdits(r, page, gt);
        }
        noise(r, page);
        // A crash most often right after a replay, which it makes suspect.
        const code = d > 0 && r.chance(replayed.size > 0 ? 0.6 : 0.2) ? 4500 : 1006;
        c.close(code);
        if (r.chance(0.4)) pip = (await pipStep(r, c, page)) || pip;
        if (r.chance(0.4)) userEdits(r, page, gt);
        noise(r, page);
        c.open(true);
        const popped = pipWin ? JSON.stringify([pipDoc?.querySelector('[az-view]')?.id]) : 'none';
        // A resync keeps a popped-out view in its window: sometimes the window closes
        // first, as a pop-back while disconnected does.
        if (pipWin && r.chance(0.5)) {
            pipWin.close();
            locate(page);
            bump('pop-backs before the resync');
        }
        if (pipWin)
            bump(
                `resyncs keeping ${pipDoc?.querySelector('[az-view]')?.id === 'page' ? 'the page' : 'a child view'} in its window`,
            );
        const pre = held(page, gt);
        const old = page;
        page = g.mutate(page);
        const suspect = code === 4500 ? replayed : new Set();
        const before = c.sent().length;
        const posted = c.posted.length;
        // The resync re-renders the page; a popped-out page stays in its window.
        // Each field's element before the resync: what it replaces holds no act.
        const els = new Map(ordered(old).map((f) => [f.key, elOf(f)]));
        c.msg([[8, 'page', render(page)]], true);
        locate(page);
        const sent = c.sent().slice(before);
        const ctx =
            `seed ${seed}, drop ${d + 1}, popped ${popped}, code ${code}\n` +
            `old: ${render(old)}\nnew: ${render(page)}`;
        const restoredAt = guarantees(ctx, old, page, pre, gt, sent, suspect);
        // The oracle restates the rules for one view in one document.
        if (ORACLE && !pip && old.views.length === 0 && page.views.length === 0) {
            const o = oracle(page, snapshot(old, pre), (k) => !suspect.has(k));
            expect(
                ordered(page).map((f) => [f.key, choice(shows(f))]),
                `${ctx}\nthe oracle's states`,
            ).toEqual(ordered(page).map((f) => [f.key, choice(o.want.get(f))]));
            expect(sent.map((x) => x[1]).sort(), `${ctx}\nthe oracle's replay`).toEqual(
                [...o.events].sort(),
            );
        }
        replayed = new Set();
        for (const [, ev] of sent) {
            if (ev.startsWith('chg_')) replayed.add(formModelKey(formOf(page, ev.slice(4))));
            const f = ordered(page).find((x) => ev === `own_${x.key}`);
            // A formless radio's replay proposes its document's whole group, every
            // view's part of it.
            if (f)
                for (const x of f.kind === 'radio' && !f.form ? groupIn(page, f) : [f])
                    replayed.add(modelKey(x));
        }
        // The server answers some of the replay's proposals, refusing them (a reply
        // changing nothing), and may prove it handled the whole replay.
        // What the restore showed is the user's choice or the server's.
        // A fresh control carries no statement of the server's from before.
        for (const f of ordered(page)) if (elOf(f) !== els.get(f.key)) gt.stamp.delete(f.key);
        // A reply is a batch of the server's: a refusal reconciling a group to the
        // server's choice states it.
        for (const x of sent)
            if (typeof x[3] === 'number' && r.chance(0.5)) {
                serverOp(page, gt, () => c.reply(x[3]));
                bump('replies');
            }
        if (c.posted.slice(posted).some((x) => x[0] === 4) && r.chance(0.5)) {
            serverOp(page, gt, () => c.ack());
            replayed = new Set();
            bump('acks');
        }
        // What a field shows now is its own to submit on a later replay: a value the
        // restore moved onto it by position, too.
        for (const f of ordered(page)) {
            noteCounts(gt, f);
            if (choice(shows(f)) === choice(f.server)) continue;
            const vs = gt.values.get(partKey(f)) ?? new Set();
            for (const v of valuesOf(f, shows(f))) vs.add(v);
            gt.values.set(partKey(f), vs);
        }
        // What the restore showed as the user's is still an edit; what it showed as
        // the server's is not.
        gt.edits = new Set(
            ordered(page)
                .filter((f) => apart(page, f))
                .map((f) => f.key),
        );
        // The resync rendered fresh controls, which no act touched (a kept az-nodiff
        // one aside); a formless group the restore showed the user's choice in holds
        // when the user acted on the group that choice came from.
        for (const f of ordered(page)) if (elOf(f) !== els.get(f.key)) gt.at.delete(f.key);
        for (const [key, t] of restoredAt) {
            gt.at.set(key, t);
            const f = ordered(page).find((x) => x.key === key);
            if (f) gt.actIn.set(key, nameKey(f));
        }
        bump('resyncs');
        if (pip) bump('resyncs with a PiP window');
        if (page.views.length > 0) bump('resyncs with child views');
        if (code === 4500) bump(`crash at drop ${d + 1}`);
        bump('form replays', events(sent, 'chg_'));
        bump('own replays', events(sent, 'own_'));
        const fs = ordered(page);
        bump('user states shown', fs.filter((f) => choice(shows(f)) !== choice(f.server)).length);
        bump(
            'kind changes',
            fs.filter((f) => pre.has(f.key) && pre.get(f.key)?.f.kind !== f.kind).length,
        );
        bump(
            'mixed radio groups',
            fs.filter(
                (f) => f.kind === 'radio' && groupIn(page, f).some((x) => owned(x) !== owned(f)),
            ).length,
        );
        bump(
            'shared box values',
            fs.filter(
                (f) => isBox(f.kind) && named(page, f).some((x) => x !== f && x.box === f.box),
            ).length,
        );
        bump('formless radios', page.regions.formless.filter((f) => f.kind === 'radio').length);
        bump('az-nodiff fields', fs.filter((f) => !f.live).length);
        bump('typed inputs', fs.filter((f) => TYPED.includes(f.kind)).length);
        bump(
            'shared option values',
            fs.filter((f) => new Set(f.options).size < f.options.length).length,
        );
        bump(
            'bound changes',
            fs.filter((f) => pre.has(f.key) && pre.get(f.key)?.f.bound !== f.bound).length,
        );
        bump(
            'form changes',
            fs.filter((f) => pre.has(f.key) && pre.get(f.key)?.f.form !== f.form).length,
        );
        bump(
            'names in two forms',
            fs.filter(
                (f) => f.form && fs.some((x) => x.form && x.form !== f.form && x.name === f.name),
            ).length,
        );
        bump('form radios own', fs.filter((f) => f.form && f.kind === 'radio' && f.own).length);
    }
    c.restore();
    conn = null;
    // A window this run left open is no later run's.
    [pipDoc, pipWin] = [null, null];
}

/** How many of `sent` are events starting with `prefix`. */
const events = (/** @type {any[][]} */ sent, /** @type {string} */ prefix) =>
    sent.filter((x) => x[1].startsWith(prefix)).length;

describe('a reconnect restores what the user typed as documented', () => {
    it(
        `holds for ${RUNS} random pages from seed ${SEED}`,
        async () => {
            for (let i = 0; i < RUNS; i++)
                try {
                    await run(SEED + i);
                } catch (e) {
                    if (process.env.COLLECT) {
                        const m = String(/** @type {any} */ (e)?.message ?? e);
                        const why = m
                            .split('\n')
                            .filter((l) => !/^(old|new):/.test(l))
                            .slice(0, 3)
                            .join(' | ');
                        process.stderr.write(`FAIL ${SEED + i} ${why.slice(0, 400)}\n`);
                        conn?.restore();
                        conn = null;
                        pipDoc = null;
                        pipWin = null;
                        document.body.innerHTML = '';
                        continue;
                    }
                    if (/** @type {any} */ (e)?.message?.startsWith('seed')) throw e;
                    throw new Error(`seed ${SEED + i}: ${e}`, { cause: e });
                }
            if (process.env.STATS) process.stderr.write(`${JSON.stringify(seen)}\n`);
        },
        Math.max(300000, RUNS * 250),
    );
});
