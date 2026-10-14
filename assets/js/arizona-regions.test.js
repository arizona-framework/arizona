import { beforeEach, describe, expect, it, vi } from 'vitest';
import { applyOps, hooks, mountHooks, OP } from './arizona.js';
import { resolveHtml } from './arizona-core.js';

// ---------------------------------------------------------------------------
// Kept az-nodiff regions: a wholesale re-render around a region (a container
// OP_TEXT, the OP_REPLACE resyncing a reconnected view) carries the region's
// existing DOM over when the new render names the same region (`az-region`) at
// the same place.
//
// The fixtures are real `arizona_render` SSR output and real `arizona_diff` ops
// (the `arizona_nodiff_widget` page and a plain list whose items are regions).
// ---------------------------------------------------------------------------

beforeEach(() => {
    document.body.innerHTML = '';
    for (const k of Object.keys(hooks)) delete hooks[k];
});

/** A hook that records its lifecycle and keeps per-instance client state. */
function widgetHook() {
    const events = [];
    hooks.W = {
        mounted() {
            this.el.dataset.state = `client:${this.el.textContent}`;
            events.push(`mount:${this.el.textContent}`);
        },
        destroyed() {
            events.push(`destroy:${this.el.textContent}`);
        },
        updated() {
            events.push(`update:${this.el.textContent}`);
        },
    };
    return events;
}

const LIST_AZ = 'L46T83-0';
const items = () => Array.from(document.querySelectorAll('li'));

function setupList(html) {
    document.body.innerHTML = `<main id="page" az-view><ul az="${LIST_AZ}"><!--az:${LIST_AZ}-->${html}<!--/az--></ul></main>`;
    mountHooks(document);
}

// `?each(fun(I) -> {li, ['az-nodiff', {az_hook, ~"W"}, {az_key, I}], [I]} end, ...)`:
// each item is a region named with its key. Any change to such a list re-renders it wholesale.
const KEYED_TMPL = { f: '1E5G6CX', s: ['<li', ' az-hook="W"', '>', '</li>'], t: 0 };
const KEYED_TOKENS = { x: '1E5G6CX.kSPJT6G', y: '1E5G6CX.k19DTS45' };
const keyed = (k) => `<li az-region="${KEYED_TOKENS[k]}" az-hook="W" az-key="${k}">${k}</li>`;
const keyedOp = (ks) => [
    OP.TEXT,
    `page:${LIST_AZ}`,
    resolveHtml({
        ...KEYED_TMPL,
        d: ks.map((k) => [` az-region="${KEYED_TOKENS[k]}"`, ` az-key="${k}"`, k]),
    }),
    true,
];

describe('keyed list items re-rendered wholesale', () => {
    it('keeps the region of an item still listed when the first is dropped', () => {
        const events = widgetHook();
        setupList(keyed('x') + keyed('y'));
        const [, y] = items();
        y.dataset.state = 'typed';
        applyOps([keyedOp(['y'])]);
        expect(items()).toEqual([y]);
        expect(y.dataset.state).toBe('typed');
        expect(events).toEqual(['mount:x', 'mount:y', 'destroy:x']);
    });

    it('keeps each region with its key across a reorder', () => {
        const events = widgetHook();
        setupList(keyed('x') + keyed('y'));
        const [x, y] = items();
        applyOps([keyedOp(['y', 'x'])]);
        expect(items()).toEqual([y, x]);
        expect(events).toEqual(['mount:x', 'mount:y']);
    });

    it('keeps a region inside an item with that item', () => {
        widgetHook();
        const row = (k) =>
            `<li az-key="${k}"><!--az:R-0--><div az-region="W" az-hook="W">${k}</div><!--/az--></li>`;
        setupList(row('1') + row('2'));
        const [one, two] = Array.from(document.querySelectorAll('[az-region]'));
        applyOps([[OP.TEXT, `page:${LIST_AZ}`, row('2') + row('1'), true]]);
        expect(Array.from(document.querySelectorAll('[az-region]'))).toEqual([two, one]);
    });
});

// Regions in unkeyed plain-list items share one name and place: nothing tells
// which old item a new one is, so none is kept.
describe('unkeyed regions under one name', () => {
    const row = (v) => `<li><!--az:R-0--><div az-region="W" az-hook="W">${v}</div><!--/az--></li>`;

    it('are remounted when the first item is dropped', () => {
        const events = widgetHook();
        setupList(row('a') + row('b'));
        const [a] = items();
        applyOps([[OP.TEXT, `page:${LIST_AZ}`, row('b'), true]]);
        expect(a.isConnected).toBe(false);
        expect(document.querySelector('[az-region]').dataset.state).toBe('client:b');
        expect(events).toEqual(['mount:a', 'mount:b', 'destroy:a', 'destroy:b', 'mount:b']);
    });

    it('are remounted when the items are reversed', () => {
        const events = widgetHook();
        setupList(row('a') + row('b'));
        const before = Array.from(document.querySelectorAll('[az-region]'));
        applyOps([[OP.TEXT, `page:${LIST_AZ}`, row('b') + row('a'), true]]);
        const after = Array.from(document.querySelectorAll('[az-region]'));
        expect(after.some((r) => before.includes(r))).toBe(false);
        expect(after.map((r) => r.dataset.state)).toEqual(['client:b', 'client:a']);
        expect(events.slice(2)).toEqual(['destroy:a', 'destroy:b', 'mount:b', 'mount:a']);
    });
});

// `arizona_nodiff_regions:root_child/1`: a child view whose own root is az-nodiff,
// its id from a binding. A new id is a new view the server addresses by that id.
describe('a child view region', () => {
    const SLOT = '17H5EN6-0';
    const CHILD = '<div az-region="OAJKXF" az-view id="rc1">1</div>';
    const SWITCH = resolveHtml({
        d: [' az-region="OAJKXF"', ' id="rc2"', '1'],
        f: 'OAJKXF',
        s: ['<div', ' az-view', '>', '</div>'],
    });

    it('is replaced when its id changes', () => {
        document.body.innerHTML = `<main id="page" az-view><div az="${SLOT}"><!--az:${SLOT}-->${CHILD}<!--/az--></div></main>`;
        const old = document.getElementById('rc1');
        applyOps([[OP.TEXT, `page:${SLOT}`, SWITCH, true]]);
        expect(old.isConnected).toBe(false);
        expect(document.getElementById('rc2')).not.toBeNull();
    });

    it('is kept under the same id', () => {
        document.body.innerHTML = `<main id="page" az-view><div az="${SLOT}"><!--az:${SLOT}-->${CHILD}<!--/az--></div></main>`;
        const old = document.getElementById('rc1');
        applyOps([[OP.TEXT, `page:${SLOT}`, CHILD, true]]);
        expect(document.getElementById('rc1')).toBe(old);
    });
});

// The `arizona_nodiff_widget` page: the region is a conditional's branch in the
// `#slot` content slot.
const SLOT_AZ = '8ADF55-10';
const LINE = '<div az-region="1B4JTYR.b1.k1GIQY31" az-hook="W" data-kind="line" az-key="0">0</div>';
const BAR = resolveHtml({
    d: [' az-region="1Y1XRZ5.b2.k1GIQY31"', ' az-key="0"', '0'],
    f: '8ADF55-10-1Y1XRZ5',
    s: ['<div', ' az-hook="W" data-kind="bar"', '>', '</div>'],
});
const page = (region) =>
    `<main az="8ADF55-0" az-view id="page"><p az="8ADF55-9" id="count">` +
    `<!--az:8ADF55-9-->0<!--/az--></p><div az="${SLOT_AZ}" id="slot">` +
    `<!--az:${SLOT_AZ}-->${region}<!--/az--></div></main>`;

function setupPage(region) {
    document.body.innerHTML = page(region);
    mountHooks(document);
    return document.querySelector('[az-region]');
}

describe('a slot re-rendered wholesale', () => {
    it('keeps a region named again', () => {
        const events = widgetHook();
        const region = setupPage(LINE);
        applyOps([[OP.TEXT, `page:${SLOT_AZ}`, LINE.replace('>0<', '>1<'), true]]);
        expect(document.querySelector('[az-region]')).toBe(region);
        // Never patched: the region keeps the content it was mounted with.
        expect(region.textContent).toBe('0');
        expect(events).toEqual(['mount:0']);
    });

    it('replaces a region named differently', () => {
        const events = widgetHook();
        const region = setupPage(LINE);
        applyOps([[OP.TEXT, `page:${SLOT_AZ}`, BAR, true]]);
        expect(region.isConnected).toBe(false);
        expect(document.querySelector('[az-region]').dataset.kind).toBe('bar');
        expect(events).toEqual(['mount:0', 'destroy:0', 'mount:0']);
    });

    it('does not move a region into another slot under the same name', () => {
        widgetHook();
        const slots = (a, b) => `<!--az:A-1-->${a}<!--/az--><!--az:A-2-->${b}<!--/az-->`;
        document.body.innerHTML =
            `<main id="page" az-view><div az="${SLOT_AZ}"><!--az:${SLOT_AZ}-->` +
            `${slots(LINE, '')}<!--/az--></div></main>`;
        mountHooks(document);
        const region = document.querySelector('[az-region]');
        applyOps([[OP.TEXT, `page:${SLOT_AZ}`, slots('', LINE), true]]);
        expect(region.isConnected).toBe(false);
        expect(document.querySelector('[az-region]')).not.toBeNull();
    });

    it('carries a region nested in another along with it, and nothing else', () => {
        const mounted = vi.fn();
        hooks.Inner = { mounted };
        const outer =
            '<section az-region="O"><div az-region="I" az-hook="Inner"></div>' +
            '<i az-hook="Inner"></i></section>';
        const region = setupPage(outer);
        expect(mounted).toHaveBeenCalledTimes(2);
        // Markup the widget added: Arizona did not render it, so it mounts nothing.
        region.insertAdjacentHTML('beforeend', '<b az-hook="Inner"></b>');
        applyOps([[OP.TEXT, `page:${SLOT_AZ}`, outer, true]]);
        expect(document.querySelector('[az-region="O"]')).toBe(region);
        expect(mounted).toHaveBeenCalledTimes(2);
    });
});

describe('OP.REPLACE', () => {
    it('keeps a region named again on a resync', () => {
        const events = widgetHook();
        const region = setupPage(LINE);
        applyOps([[OP.REPLACE, 'page', page(LINE)]], true);
        expect(document.querySelector('[az-region]')).toBe(region);
        expect(document.querySelector('main')).not.toBeNull();
        expect(events).toEqual(['mount:0']);
    });

    it('remounts a region named differently on a resync', () => {
        const events = widgetHook();
        const region = setupPage(BAR);
        applyOps([[OP.REPLACE, 'page', page(LINE)]], true);
        expect(region.isConnected).toBe(false);
        expect(events).toEqual(['mount:0', 'destroy:0', 'mount:0']);
    });

    it('keeps nothing on a navigate', () => {
        const events = widgetHook();
        const region = setupPage(LINE);
        applyOps([[OP.REPLACE, 'page', page(LINE)]]);
        expect(region.isConnected).toBe(false);
        expect(events).toEqual(['mount:0', 'destroy:0', 'mount:0']);
    });

    it('does not keep a region holding a live view on a resync', () => {
        widgetHook();
        const withView = '<section az-region="R"><span az-view id="child">0</span></section>';
        const region = setupPage(withView);
        applyOps([[OP.REPLACE, 'page', page(withView)]], true);
        expect(region.isConnected).toBe(false);
    });
});
