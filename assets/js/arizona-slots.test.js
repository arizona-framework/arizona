import { beforeEach, describe, expect, it, vi } from 'vitest';
import { applyOps, hooks, mountHooks, OP, resolveEl } from './arizona.js';

// ---------------------------------------------------------------------------
// Marker-anchored slots: targets that name NO element, and marker pairs that
// nest inside one another.
//
// Every fixture below is REAL `arizona_render` SSR output paired with the REAL
// `arizona_diff` op for the same state change (captured from the compiled
// templates), so the client contract is pinned against the server rather than
// against a hand-drawn DOM.
// ---------------------------------------------------------------------------

beforeEach(() => {
    document.body.innerHTML = '';
    for (const k of Object.keys(hooks)) delete hooks[k];
});

// A page embedding a child component whose WHOLE body is a bare dynamic. The
// child's root slot is anchored by its own marker pair NESTED inside the parent
// slot's pair, and NO element carries the child slot's az -- so the server op
// (`page:<childAz>`) has only a comment to aim at.
const PAGE_AZ = '1YA8LBE-0';
const CHILD_AZ = '1YA8LBE-0-9RDGCU-0';

/** Put real SSR body markup inside the real live-root <main>. */
function setupPage(body) {
    document.body.innerHTML = `<main az="${PAGE_AZ}" az-view id="page">${body}</main>`;
}

/** The live root's current markup, minus the root element's own tag. */
const pageBody = () => document.querySelector('main').innerHTML;

/** `?html(case ?get(show) of true -> {p,...}; false -> <<>> end)` as a whole body. */
const condBody = (inner) =>
    `<h1>Title</h1><!--az:${PAGE_AZ}--><!--az:${CHILD_AZ}-->${inner}<!--/az--><!--/az-->`;

describe('resolveEl -- marker-only slot targets', () => {
    it('resolves a whole-template bare dynamic to the marker parent', () => {
        setupPage(condBody(''));
        // No element carries CHILD_AZ; the target is anchored by a comment only.
        expect(document.querySelector(`[az="${CHILD_AZ}"]`)).toBeNull();
        const el = resolveEl(`page:${CHILD_AZ}`);
        expect(el).not.toBeNull();
        expect(el).toBe(document.querySelector('main'));
    });

    it('returns null when neither an element nor a marker carries the az', () => {
        setupPage(condBody(''));
        expect(resolveEl('page:NOPE-0')).toBeNull();
    });

    it('keeps element lookup ahead of the marker scan', () => {
        // `<p az="0">` both carries the az AND holds the slot marker. The
        // element must win -- and no marker scan should even run.
        document.body.innerHTML = '<div id="v" az-view><p az="0"><!--az:0-->x<!--/az--></p></div>';
        const spy = vi.spyOn(document, 'createTreeWalker');
        const el = resolveEl('v:0');
        expect(el.tagName).toBe('P');
        expect(el.getAttribute('az')).toBe('0');
        expect(spy).not.toHaveBeenCalled();
        spy.mockRestore();
    });

    it('still falls back to the base element for a compound az:n target', () => {
        document.body.innerHTML =
            '<div id="v" az-view><p az="0"><!--az:0-->A<!--/az--><!--az:0:1-->B<!--/az--></p></div>';
        const el = resolveEl('v:0:1');
        expect(el.getAttribute('az')).toBe('0');
    });

    it('resolves a marker-only target once per batch (per-batch els memo)', () => {
        setupPage(condBody('A'));
        const spy = vi.spyOn(document, 'createTreeWalker');
        applyOps([
            [OP.TEXT, `page:${CHILD_AZ}`, 'B'],
            [OP.TEXT, `page:${CHILD_AZ}`, 'C'],
        ]);
        expect(spy).toHaveBeenCalledTimes(1);
        expect(pageBody()).toBe(condBody('C'));
        spy.mockRestore();
    });
});

describe('applyOps -- ops addressed to a marker-only slot', () => {
    it('applies the conditional-only banner toggle in both directions', () => {
        const warn = vi.spyOn(console, 'warn').mockImplementation(() => {});
        setupPage(condBody(''));

        // hidden -> shown: the branch re-renders as an HTML fragment.
        applyOps([[OP.TEXT, `page:${CHILD_AZ}`, '<p class="banner">Shown</p>', true]]);
        expect(pageBody()).toBe(condBody('<p class="banner">Shown</p>'));

        // shown -> hidden: the empty branch is a scalar.
        applyOps([[OP.TEXT, `page:${CHILD_AZ}`, '']]);
        expect(pageBody()).toBe(condBody(''));

        expect(warn).not.toHaveBeenCalled();
        warn.mockRestore();
    });

    it('applies a value update to a `?html(?get(x))` body', () => {
        setupPage(condBody('A'));
        applyOps([[OP.TEXT, `page:${CHILD_AZ}`, 'B']]);
        expect(pageBody()).toBe(condBody('B'));
    });

    it('applies a list re-render to a `?html(?each(...))` body', () => {
        const item = (t) => `<li az="1Q5CICB-0"><!--az:1Q5CICB-0-->${t}<!--/az--></li>`;
        setupPage(condBody(item('a')));
        applyOps([[OP.TEXT, `page:${CHILD_AZ}`, item('a') + item('b'), true]]);
        expect(pageBody()).toBe(condBody(item('a') + item('b')));
    });

    it('applies a wholesale re-render to a `?html(?stateless(...))` body', () => {
        const leaf = (t) =>
            `<span az="${CHILD_AZ}-B60BG2-0" class="leaf">` +
            `<!--az:${CHILD_AZ}-B60BG2-0-->${t}<!--/az--></span>`;
        setupPage(condBody(leaf('A')));
        applyOps([[OP.TEXT, `page:${CHILD_AZ}`, leaf('B'), true]]);
        expect(pageBody()).toBe(condBody(leaf('B')));
    });

    it('applies a value update inside a mixed top-level fragment', () => {
        // `?html([~"head ", ?get(text), {b, [], [~"tail"]}])` -- the bare dynamic
        // sits between static text and a sibling element, all inside the parent
        // slot's marker pair.
        setupPage(richBody('A'));
        applyOps([[OP.TEXT, `page:${MIX_AZ}`, 'B']]);
        expect(pageBody()).toBe(richBody('B'));
    });

    it('leaves element-anchored slots patching exactly as before', () => {
        document.body.innerHTML =
            '<div id="v" az-view><p az="0"><!--az:0-->A<!--/az--> and <!--az:0:1-->B<!--/az--></p></div>';
        applyOps([[OP.TEXT, 'v:0:1', 'B2']]);
        expect(document.querySelector('p').outerHTML).toBe(
            '<p az="0"><!--az:0-->A<!--/az--> and <!--az:0:1-->B2<!--/az--></p>',
        );
    });
});

// ---------------------------------------------------------------------------
// Slot walkers must track marker NESTING depth.
//
// Real SSR for a parent slot that swaps between a child component and plain
// text. The child's body is a mixed top-level fragment, so its own marker pair
// sits INSIDE the parent slot's pair, followed by a sibling element:
//
//   <!--az:PARENT-->head <!--az:CHILD-->A<!--/az--><b>tail</b><!--/az-->
//
// A walker that stops at the FIRST <!--/az--> stops at the CHILD's closer: the
// slot's tail is left behind on a re-render (visible corruption) and a stray
// closer accumulates on every toggle.
// ---------------------------------------------------------------------------

const MIX_AZ = '1YA8LBE-0-1TLYFEV-0';

/** The `rich` branch: a nested marker pair followed by a sibling element. */
const richBody = (v, bAttrs = '') =>
    `<h1>Title</h1><!--az:${PAGE_AZ}-->head <!--az:${MIX_AZ}-->${v}<!--/az-->` +
    `<b${bAttrs}>tail</b><!--/az-->`;

/** The `plain` branch: the same parent slot re-rendered to a scalar. */
const plainBody = () => `<h1>Title</h1><!--az:${PAGE_AZ}-->plain text<!--/az-->`;

/** Real op for rich -> plain (a scalar branch value). */
const toPlain = [OP.TEXT, `page:${PAGE_AZ}`, 'plain text'];

/** Real op for plain -> rich (the branch re-renders as an HTML fragment). */
const toRich = (bAttrs = '') => [
    OP.TEXT,
    `page:${PAGE_AZ}`,
    `head <!--az:${MIX_AZ}-->A<!--/az--><b${bAttrs}>tail</b>`,
    true,
];

/** Count the `<!--/az-->` closers that are direct children of the live root. */
function closerCount() {
    let n = 0;
    for (const node of document.querySelector('main').childNodes) {
        if (node.nodeType === 8 && node.data === '/az') n++;
    }
    return n;
}

describe('applyOps -- re-rendering a slot that contains a nested marker pair', () => {
    it('replaces the whole slot, not just up to the inner closer', () => {
        setupPage(richBody('A'));
        applyOps([toPlain]);
        // The inner pair AND the <b> after it belong to the slot -- all gone.
        expect(pageBody()).toBe(plainBody());
        expect(document.querySelector('main').textContent).toBe('Titleplain text');
    });

    it('keeps the closer count constant across repeated toggles', () => {
        // rich carries two closers (the inner pair's and the slot's), plain one.
        // Under-walking leaks one more on every cycle, without bound.
        setupPage(richBody('A'));
        expect(closerCount()).toBe(2);
        for (let i = 0; i < 3; i++) {
            applyOps([toPlain]);
            expect(pageBody()).toBe(plainBody());
            expect(closerCount()).toBe(1);
            applyOps([toRich()]);
            expect(pageBody()).toBe(richBody('A'));
            expect(closerCount()).toBe(2);
        }
    });

    it('walks hook lifecycle past the inner pair', () => {
        const mounted = vi.fn();
        const destroyed = vi.fn();
        hooks.Tail = { mounted, destroyed };
        setupPage(richBody('A', ' az-hook="Tail"'));
        mountHooks(document);
        expect(mounted).toHaveBeenCalledOnce();

        // The <b> sits after the inner closer, still inside the slot: destroying
        // the slot must tear its hook down.
        applyOps([toPlain]);
        expect(destroyed).toHaveBeenCalledOnce();

        // ...and re-rendering the branch must mount the fresh one.
        applyOps([toRich(' az-hook="Tail"')]);
        expect(mounted).toHaveBeenCalledTimes(2);
    });

    it('handles a slot nested two pairs deep', () => {
        const INNER_AZ = `${MIX_AZ}-DEEP-0`;
        const deep = (v) =>
            `<h1>Title</h1><!--az:${PAGE_AZ}-->a<!--az:${MIX_AZ}-->b` +
            `<!--az:${INNER_AZ}-->${v}<!--/az-->c<!--/az-->d<!--/az-->`;
        setupPage(deep('X'));
        applyOps([[OP.TEXT, `page:${PAGE_AZ}`, 'flat']]);
        expect(pageBody()).toBe(`<h1>Title</h1><!--az:${PAGE_AZ}-->flat<!--/az-->`);
    });
});

// ---------------------------------------------------------------------------
// A child view inside a stream item: OP_ITEM_PATCH carries the child-view
// wrapper.
//
// `arizona_socket:flatten_ops/2` unwraps `[ChildViewId, ChildOps]` only at TOP
// level, so a `?stateful` child inside a stream `?each` item ships that wrapper
// INSIDE the item patch. Real captured op:
//
//   [7, "page:1MGI7U2-1", "1", [["c-1", [[0, "1MZMHYB-1", "ONE"]]]]]
//
// `op[0]` is a view-id STRING, so a switch on op codes alone silently drops it.
// ---------------------------------------------------------------------------

const STREAM_AZ = '1MGI7U2-1';
const CHILD_LABEL_AZ = '1MZMHYB-1';

/** Real SSR: a stream whose single keyed item wraps a `?stateful` child view. */
function setupStreamWithChild(label) {
    document.body.innerHTML =
        '<div az="1MGI7U2-0" az-view id="page">' +
        `<ul az="${STREAM_AZ}"><!--az:${STREAM_AZ}-->` +
        '<li az="1HWIIPK-0" az-key="1"><!--az:1HWIIPK-0-->' +
        '<div az="1MZMHYB-0" az-view id="c-1">' +
        `<span az="${CHILD_LABEL_AZ}" class="label"><!--az:${CHILD_LABEL_AZ}-->${label}<!--/az--></span>` +
        '</div>' +
        '<!--/az--></li>' +
        '<!--/az--></ul></div>';
}

describe('applyOps -- child-view ops inside OP.ITEM_PATCH', () => {
    it('dispatches the wrapper against the named child view', () => {
        setupStreamWithChild('one');
        applyOps([
            [
                OP.ITEM_PATCH,
                `page:${STREAM_AZ}`,
                '1',
                [['c-1', [[OP.TEXT, CHILD_LABEL_AZ, 'ONE']]]],
            ],
        ]);
        expect(document.querySelector('#c-1 .label').textContent).toBe('ONE');
    });

    it('resolves the child view by id, not by position in the item', () => {
        // Two child views under one item: the wrapper must reach the named one
        // and leave the sibling untouched.
        setupStreamWithChild('one');
        document
            .querySelector('#c-1')
            .insertAdjacentHTML(
                'afterend',
                `<div az="1MZMHYB-0" az-view id="c-2"><span az="${CHILD_LABEL_AZ}" class="label"><!--az:${CHILD_LABEL_AZ}-->two<!--/az--></span></div>`,
            );
        applyOps([
            [
                OP.ITEM_PATCH,
                `page:${STREAM_AZ}`,
                '1',
                [['c-2', [[OP.TEXT, CHILD_LABEL_AZ, 'TWO']]]],
            ],
        ]);
        expect(document.querySelector('#c-1 .label').textContent).toBe('one');
        expect(document.querySelector('#c-2 .label').textContent).toBe('TWO');
    });

    it('applies a nested grandchild wrapper', () => {
        setupStreamWithChild('one');
        document
            .querySelector('#c-1 .label')
            .insertAdjacentHTML(
                'afterend',
                '<div az="1GC-0" az-view id="g-1"><em az="1GC-1"><!--az:1GC-1-->deep<!--/az--></em></div>',
            );
        applyOps([
            [
                OP.ITEM_PATCH,
                `page:${STREAM_AZ}`,
                '1',
                [['c-1', [['g-1', [[OP.TEXT, '1GC-1', 'DEEP']]]]]],
            ],
        ]);
        expect(document.querySelector('#g-1 em').textContent).toBe('DEEP');
    });

    it('warns when the named child view is not in the DOM', () => {
        const warn = vi.spyOn(console, 'warn').mockImplementation(() => {});
        setupStreamWithChild('one');
        applyOps([
            [OP.ITEM_PATCH, `page:${STREAM_AZ}`, '1', [['gone', [[OP.TEXT, CHILD_LABEL_AZ, 'X']]]]],
        ]);
        expect(warn).toHaveBeenCalledOnce();
        expect(String(warn.mock.calls[0][0])).toContain('gone');
        // The item is otherwise untouched.
        expect(document.querySelector('#c-1 .label').textContent).toBe('one');
        warn.mockRestore();
    });

    it('warns on an unrecognized item op code instead of dropping it silently', () => {
        const warn = vi.spyOn(console, 'warn').mockImplementation(() => {});
        setupStreamWithChild('one');
        applyOps([[OP.ITEM_PATCH, `page:${STREAM_AZ}`, '1', [[99, CHILD_LABEL_AZ]]]]);
        expect(warn).toHaveBeenCalledOnce();
        expect(String(warn.mock.calls[0][0])).toContain('99');
        warn.mockRestore();
    });
});

// ---------------------------------------------------------------------------
// A marker-only hit names the slot's PARENT, so a destructive op must refuse it.
//
// Real SSR for a stream `?each` among static siblings, the shape whose slot az is
// COMPOUND (`10MBGFX-0:1`) and whose base az is the VIEW ROOT's own az -- which
// `querySelector` cannot return (an element is not its own descendant). So the
// element arms both miss and only the marker scan hits, returning the live root.
// `OP_UPDATE` / `OP_REMOVE_NODE` on that hit would innerHTML-wipe or delete the
// whole view; they must warn and skip instead. Marker-aware ops keep the hit.
// ---------------------------------------------------------------------------

const SIB_ROOT_AZ = '10MBGFX-0';
const SIB_EACH_AZ = '10MBGFX-0:1';

/** Real SSR item markup for the stream. */
const sibItem = (key, label) =>
    `<li az="1HWIIPK-0" az-key="${key}"><!--az:1HWIIPK-0-->${label}<!--/az--></li>`;

/** Real SSR body of the live root, for the given rendered stream items. */
const sibBody = (items) =>
    `<p>header</p><!--az:${SIB_ROOT_AZ}-->T<!--/az-->` +
    `<!--az:${SIB_EACH_AZ}-->${items}<!--/az--><p>footer</p>`;

function setupSiblings() {
    document.body.innerHTML = `<div az="${SIB_ROOT_AZ}" az-view id="page">${sibBody(sibItem('1', 'a'))}</div>`;
}

const sibRoot = () => document.getElementById('page');

/** The worker-resolved HTML the container full-render op carries. */
const SIB_NEW_ITEMS = sibItem('1', 'a') + sibItem('2', 'b');

describe('applyOps -- destructive ops on a marker-only target', () => {
    it('refuses the real stream-among-siblings OP_UPDATE instead of wiping the view', () => {
        const warn = vi.spyOn(console, 'warn').mockImplementation(() => {});
        setupSiblings();
        const before = sibRoot().innerHTML;
        applyOps([[OP.UPDATE, `page:${SIB_EACH_AZ}`, SIB_NEW_ITEMS]]);
        // Nothing destroyed: header, the sibling title slot's markers, and footer
        // all survive, exactly as on a target that never resolved at all.
        expect(sibRoot().innerHTML).toBe(before);
        expect(warn).toHaveBeenCalledOnce();
        expect(String(warn.mock.calls[0][0])).toContain(SIB_EACH_AZ);
        warn.mockRestore();
    });

    it('refuses OP_REMOVE_NODE on a marker-only target', () => {
        const warn = vi.spyOn(console, 'warn').mockImplementation(() => {});
        setupSiblings();
        applyOps([[OP.REMOVE_NODE, `page:${SIB_EACH_AZ}`]]);
        expect(sibRoot()).not.toBeNull();
        expect(warn).toHaveBeenCalledOnce();
        warn.mockRestore();
    });

    it('refuses OP_REPLACE on a marker-only target', () => {
        const warn = vi.spyOn(console, 'warn').mockImplementation(() => {});
        setupSiblings();
        applyOps([[OP.REPLACE, `page:${SIB_EACH_AZ}`, '<section id="gone"></section>']]);
        expect(sibRoot()).not.toBeNull();
        expect(document.getElementById('gone')).toBeNull();
        expect(warn).toHaveBeenCalledOnce();
        warn.mockRestore();
    });

    it('still applies the marker-aware OP_TEXT container re-render', () => {
        const warn = vi.spyOn(console, 'warn').mockImplementation(() => {});
        setupSiblings();
        applyOps([[OP.TEXT, `page:${SIB_EACH_AZ}`, SIB_NEW_ITEMS, true]]);
        // The stream content actually updates, and only the slot changed.
        expect(sibRoot().innerHTML).toBe(sibBody(SIB_NEW_ITEMS));
        expect(warn).not.toHaveBeenCalled();
        warn.mockRestore();
    });

    it('leaves destructive ops on an element-anchored target working', () => {
        const warn = vi.spyOn(console, 'warn').mockImplementation(() => {});
        document.body.innerHTML = '<div id="v" az-view><div az="0">old</div></div>';
        applyOps([[OP.UPDATE, 'v:0', '<em>new</em>']]);
        expect(document.querySelector('[az="0"]').innerHTML).toBe('<em>new</em>');
        expect(warn).not.toHaveBeenCalled();
        warn.mockRestore();
    });

    // A stream `?each` container op is resolved relative to the ELEMENT, so on a
    // marker-only container the ops that place a node by container position land
    // outside the slot: a tail OP_INSERT appends after the footer, an OP_MOVE
    // prepend lands before the header. Silent misplacement is worse than the
    // dropped-with-a-warning behaviour these had before the marker fallback
    // existed, so they are refused too. The position-INDEPENDENT item ops
    // (OP_REMOVE, OP_ITEM_PATCH) find their target by `az-key` and stay correct.
    it('refuses a tail OP_INSERT on a marker-only stream container', () => {
        const warn = vi.spyOn(console, 'warn').mockImplementation(() => {});
        setupSiblings();
        const before = sibRoot().innerHTML;
        applyOps([[OP.INSERT, `page:${SIB_EACH_AZ}`, '2', -1, sibItem('2', 'b')]]);
        expect(sibRoot().innerHTML).toBe(before);
        expect(warn).toHaveBeenCalledOnce();
        warn.mockRestore();
    });

    it('refuses an OP_MOVE on a marker-only stream container', () => {
        const warn = vi.spyOn(console, 'warn').mockImplementation(() => {});
        document.body.innerHTML =
            `<div az="${SIB_ROOT_AZ}" az-view id="page">` +
            `${sibBody(sibItem('1', 'a') + sibItem('2', 'b'))}</div>`;
        const before = sibRoot().innerHTML;
        applyOps([[OP.MOVE, `page:${SIB_EACH_AZ}`, '2', null]]);
        expect(sibRoot().innerHTML).toBe(before);
        expect(warn).toHaveBeenCalledOnce();
        warn.mockRestore();
    });

    it('still applies the position-independent item ops', () => {
        const warn = vi.spyOn(console, 'warn').mockImplementation(() => {});
        document.body.innerHTML =
            `<div az="${SIB_ROOT_AZ}" az-view id="page">` +
            `${sibBody(sibItem('1', 'a') + sibItem('2', 'b'))}</div>`;
        applyOps([
            [OP.ITEM_PATCH, `page:${SIB_EACH_AZ}`, '1', [[OP.TEXT, '1HWIIPK-0', 'A']]],
            [OP.REMOVE, `page:${SIB_EACH_AZ}`, '2'],
        ]);
        expect(sibRoot().innerHTML).toBe(sibBody(sibItem('1', 'A')));
        expect(warn).not.toHaveBeenCalled();
        warn.mockRestore();
    });
});

// ---------------------------------------------------------------------------
// A slot walker must recognise a FRAMEWORK marker, not any `az:`-prefixed
// comment.
//
// Static text is spliced verbatim (the raw-HTML seam) and `?raw` exists to
// splice trusted stored HTML, so comments authored by a CMS/markdown pipeline
// reach slot content as ordinary bytes. `arizona_html:scope_static/3` already
// states the rule: every framework-emitted az is `<Fp>-<id>`, and the
// fingerprint is what separates a real marker from user-authored bytes.
// Counting a decoy as a nested opener makes the walker swallow the slot's OWN
// closer, deleting the following siblings AND un-anchoring the slot for good.
// ---------------------------------------------------------------------------

describe('applyOps -- decoy `az:` comments inside slot content', () => {
    const decoyBody = (inner) => `<section az="V-2"><!--az:V-2-->${inner}<!--/az--></section>`;

    function setupDecoy(inner) {
        document.body.innerHTML = `<div id="v" az-view>${decoyBody(inner)}<span>sibling</span></div>`;
    }

    it('does not treat a non-fingerprint `az:` comment as a nested opener', () => {
        setupDecoy('x<!--az:fake-->y');
        applyOps([[OP.TEXT, 'v:V-2', 'NEW']]);
        expect(document.querySelector('section').innerHTML).toBe('<!--az:V-2-->NEW<!--/az-->');
        expect(document.querySelector('span')).not.toBeNull();
    });

    it('survives repeated re-renders with a decoy (slot stays anchored)', () => {
        setupDecoy('x<!--az:fake-->y');
        applyOps([[OP.TEXT, 'v:V-2', 'ONE']]);
        applyOps([[OP.TEXT, 'v:V-2', 'TWO']]);
        expect(document.querySelector('section').innerHTML).toBe('<!--az:V-2-->TWO<!--/az-->');
    });

    it.each([
        ['lowercase word', '<!--az:fake-->'],
        ['hyphenated word', '<!--az:foo-bar-->'],
        ['spaced text', '<!--az: build 3-->'],
        ['fingerprint with no id', '<!--az:1ABC-->'],
        ['trailing punctuation', '<!--az:1ABC-0!-->'],
    ])('rejects a decoy that only looks like a marker (%s)', (_name, decoy) => {
        setupDecoy(`x${decoy}y`);
        applyOps([[OP.TEXT, 'v:V-2', 'NEW']]);
        expect(document.querySelector('section').innerHTML).toBe('<!--az:V-2-->NEW<!--/az-->');
        expect(document.querySelector('span')).not.toBeNull();
    });

    it('still counts a real nested marker as an opener', () => {
        // The control for the above: a genuine `<Fp>-<id>` nested pair must keep
        // incrementing depth, or the walker under-walks again.
        setupDecoy('x<!--az:1TLYFEV-0-->y<!--/az-->z');
        applyOps([[OP.TEXT, 'v:V-2', 'NEW']]);
        expect(document.querySelector('section').innerHTML).toBe('<!--az:V-2-->NEW<!--/az-->');
        expect(document.querySelector('span')).not.toBeNull();
    });

    it('never mutates a slot it cannot delimit', () => {
        // A decoy that DOES match the framework shape but has no closer (stored
        // HTML carrying a real-looking marker) leaves the slot unterminated. The
        // walk must abort without deleting anything rather than empty the parent.
        const warn = vi.spyOn(console, 'warn').mockImplementation(() => {});
        setupDecoy('x<!--az:1TLYFEV-0-->y');
        const before = document.querySelector('section').innerHTML;
        applyOps([[OP.TEXT, 'v:V-2', 'NEW']]);
        expect(document.querySelector('section').innerHTML).toBe(before);
        expect(document.querySelector('span')).not.toBeNull();
        expect(warn).toHaveBeenCalledOnce();
        warn.mockRestore();
    });
});

// ---------------------------------------------------------------------------
// A cached marker-only resolution must be re-validated within the batch.
//
// The memo keys on the resolved ELEMENT being connected, but a marker-only hit
// resolves to the slot's PARENT -- which stays connected after an earlier op in
// the same batch re-renders the enclosing slot and destroys the inner marker.
// The stale hit then reaches `applyTextOp`, whose no-marker fallback writes
// `el.textContent` and wipes the parent. OP_TEXT is exactly the op code the
// marker-unsafe guard cannot cover, so the memo itself has to notice.
// ---------------------------------------------------------------------------

describe('applyOps -- stale marker resolution within one batch', () => {
    it('re-validates a cached marker hit instead of wiping the parent', () => {
        const warn = vi.spyOn(console, 'warn').mockImplementation(() => {});
        setupPage(condBody('A'));
        applyOps([
            [OP.TEXT, `page:${CHILD_AZ}`, 'A'],
            // Re-renders the OUTER slot, destroying the inner marker pair.
            [OP.TEXT, `page:${PAGE_AZ}`, 'gone'],
            // Stale memo hit: the <main> is still connected, the marker is not.
            [OP.TEXT, `page:${CHILD_AZ}`, 'B'],
        ]);
        expect(pageBody()).toBe(`<h1>Title</h1><!--az:${PAGE_AZ}-->gone<!--/az-->`);
        expect(document.querySelector('h1')).not.toBeNull();
        expect(warn).toHaveBeenCalledOnce();
        warn.mockRestore();
    });

    it('keeps re-using a cached marker hit that is still valid', () => {
        const warn = vi.spyOn(console, 'warn').mockImplementation(() => {});
        setupPage(condBody('A'));
        applyOps([
            [OP.TEXT, `page:${CHILD_AZ}`, 'B'],
            [OP.TEXT, `page:${CHILD_AZ}`, 'C'],
        ]);
        expect(pageBody()).toBe(condBody('C'));
        expect(warn).not.toHaveBeenCalled();
        warn.mockRestore();
    });
});
