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
