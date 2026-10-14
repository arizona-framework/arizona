import { beforeEach, describe, expect, it, vi } from 'vitest';
import { applyOps, hooks, mountHooks, OP } from './arizona.js';
import { resolveHtml } from './arizona-core.js';

// ---------------------------------------------------------------------------
// Kept az-nodiff elements: a wholesale re-render around an az-nodiff element (a container
// OP_TEXT, the OP_REPLACE resyncing a reconnected view) carries the element's
// existing DOM over when the new render names the same az-nodiff element (`az-nodiff="TOKEN"`) at
// the same place.
//
// The fixtures are real `arizona_render` SSR output and real `arizona_diff` ops
// (the `arizona_nodiff_widget` page and a plain list whose items are az-nodiff elements).
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
// each item is an az-nodiff element named with its key. Any change to such a list re-renders it wholesale.
const KEYED_TMPL = { f: '1E5G6CX', s: ['<li', ' az-hook="W"', '>', '</li>'], t: 0 };
const KEYED_TOKENS = { x: '1E5G6CX.kSPJT6G', y: '1E5G6CX.k19DTS45' };
const keyed = (k) => `<li az-nodiff="${KEYED_TOKENS[k]}" az-hook="W" az-key="${k}">${k}</li>`;
const keyedOp = (ks) => [
    OP.TEXT,
    `page:${LIST_AZ}`,
    resolveHtml({
        ...KEYED_TMPL,
        d: ks.map((k) => [` az-nodiff="${KEYED_TOKENS[k]}"`, ` az-key="${k}"`, k]),
    }),
    true,
];

describe('keyed list items re-rendered wholesale', () => {
    it('keeps the az-nodiff element of an item still listed when the first is dropped', () => {
        const events = widgetHook();
        setupList(keyed('x') + keyed('y'));
        const [, y] = items();
        y.dataset.state = 'typed';
        applyOps([keyedOp(['y'])]);
        expect(items()).toEqual([y]);
        expect(y.dataset.state).toBe('typed');
        expect(events).toEqual(['mount:x', 'mount:y', 'destroy:x']);
    });

    it('keeps each az-nodiff element with its key across a reorder', () => {
        const events = widgetHook();
        setupList(keyed('x') + keyed('y'));
        const [x, y] = items();
        applyOps([keyedOp(['y', 'x'])]);
        expect(items()).toEqual([y, x]);
        expect(events).toEqual(['mount:x', 'mount:y']);
    });

    it('keeps an az-nodiff element inside an item with that item', () => {
        widgetHook();
        const row = (k) =>
            `<li az-key="${k}"><!--az:R-0--><div az-nodiff="W" az-hook="W">${k}</div><!--/az--></li>`;
        setupList(row('1') + row('2'));
        const [one, two] = Array.from(document.querySelectorAll('[az-nodiff]'));
        applyOps([[OP.TEXT, `page:${LIST_AZ}`, row('2') + row('1'), true]]);
        expect(Array.from(document.querySelectorAll('[az-nodiff]'))).toEqual([two, one]);
    });
});

// Az-nodiff elements in unkeyed plain-list items share one name and place: nothing tells
// which old item a new one is, so none is kept.
describe('unkeyed az-nodiff elements under one name', () => {
    const row = (v) => `<li><!--az:R-0--><div az-nodiff="W" az-hook="W">${v}</div><!--/az--></li>`;

    it('are remounted when the first item is dropped', () => {
        const events = widgetHook();
        setupList(row('a') + row('b'));
        const [a] = items();
        applyOps([[OP.TEXT, `page:${LIST_AZ}`, row('b'), true]]);
        expect(a.isConnected).toBe(false);
        expect(document.querySelector('[az-nodiff]').dataset.state).toBe('client:b');
        expect(events).toEqual(['mount:a', 'mount:b', 'destroy:a', 'destroy:b', 'mount:b']);
    });

    it('are remounted when the items are reversed', () => {
        const events = widgetHook();
        setupList(row('a') + row('b'));
        const before = Array.from(document.querySelectorAll('[az-nodiff]'));
        applyOps([[OP.TEXT, `page:${LIST_AZ}`, row('b') + row('a'), true]]);
        const after = Array.from(document.querySelectorAll('[az-nodiff]'));
        expect(after.some((r) => before.includes(r))).toBe(false);
        expect(after.map((r) => r.dataset.state)).toEqual(['client:b', 'client:a']);
        expect(events.slice(2)).toEqual(['destroy:a', 'destroy:b', 'mount:b', 'mount:a']);
    });
});

// `?each(fun(I) -> {li, ['az-nodiff', {az_hook, ~"W"}], [I]} end, ...)`: an
// unkeyed item root keeps its directive bare -- client-owned, but nothing names
// which item it is -- so it is never carried over, even alone in the list.
describe('a bare az-nodiff', () => {
    const bare = (v) => `<li az-nodiff az-hook="W">${v}</li>`;

    it('is never kept', () => {
        const events = widgetHook();
        setupList(bare('a'));
        const [a] = items();
        applyOps([[OP.TEXT, `page:${LIST_AZ}`, bare('a'), true]]);
        expect(a.isConnected).toBe(false);
        expect(events).toEqual(['mount:a', 'destroy:a', 'mount:a']);
    });

    it('carries no named az-nodiff element inside it either', () => {
        widgetHook();
        const item = '<li az-nodiff><div az-nodiff="W" az-hook="W">a</div></li>';
        setupList(item);
        const inner = document.querySelector('[az-nodiff="W"]');
        applyOps([[OP.TEXT, `page:${LIST_AZ}`, item, true]]);
        expect(inner.isConnected).toBe(false);
    });
});

// `arizona_nodiff_slots:root_child/1`: a child view whose own root is az-nodiff,
// its id from a binding. A new id is a new view the server addresses by that id.
describe('a child view az-nodiff element', () => {
    const SLOT = '17H5EN6-0';
    const CHILD = '<div az-nodiff="OAJKXF" az-view id="rc1">1</div>';
    const SWITCH = resolveHtml({
        d: [' az-nodiff="OAJKXF"', ' id="rc2"', '1'],
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

// The `arizona_nodiff_widget` page: the az-nodiff element is a conditional's branch in the
// `#slot` content slot.
const SLOT_AZ = '8ADF55-10';
const LINE = '<div az-nodiff="1B4JTYR.b1.k1GIQY31" az-hook="W" data-kind="line" az-key="0">0</div>';
const BAR = resolveHtml({
    d: [' az-nodiff="1Y1XRZ5.b2.k1GIQY31"', ' az-key="0"', '0'],
    f: '8ADF55-10-1Y1XRZ5',
    s: ['<div', ' az-hook="W" data-kind="bar"', '>', '</div>'],
});
const page = (content) =>
    `<main az="8ADF55-0" az-view id="page"><p az="8ADF55-9" id="count">` +
    `<!--az:8ADF55-9-->0<!--/az--></p><div az="${SLOT_AZ}" id="slot">` +
    `<!--az:${SLOT_AZ}-->${content}<!--/az--></div></main>`;

function setupPage(content) {
    document.body.innerHTML = page(content);
    mountHooks(document);
    return document.querySelector('[az-nodiff]');
}

describe('a slot re-rendered wholesale', () => {
    it('keeps an az-nodiff element named again', () => {
        const events = widgetHook();
        const widget = setupPage(LINE);
        applyOps([[OP.TEXT, `page:${SLOT_AZ}`, LINE.replace('>0<', '>1<'), true]]);
        expect(document.querySelector('[az-nodiff]')).toBe(widget);
        // Never patched: the az-nodiff element keeps the content it was mounted with.
        expect(widget.textContent).toBe('0');
        expect(events).toEqual(['mount:0']);
    });

    it('replaces an az-nodiff element named differently', () => {
        const events = widgetHook();
        const widget = setupPage(LINE);
        applyOps([[OP.TEXT, `page:${SLOT_AZ}`, BAR, true]]);
        expect(widget.isConnected).toBe(false);
        expect(document.querySelector('[az-nodiff]').dataset.kind).toBe('bar');
        expect(events).toEqual(['mount:0', 'destroy:0', 'mount:0']);
    });

    it('does not move an az-nodiff element into another slot under the same name', () => {
        widgetHook();
        const slots = (a, b) => `<!--az:A-1-->${a}<!--/az--><!--az:A-2-->${b}<!--/az-->`;
        document.body.innerHTML =
            `<main id="page" az-view><div az="${SLOT_AZ}"><!--az:${SLOT_AZ}-->` +
            `${slots(LINE, '')}<!--/az--></div></main>`;
        mountHooks(document);
        const widget = document.querySelector('[az-nodiff]');
        applyOps([[OP.TEXT, `page:${SLOT_AZ}`, slots('', LINE), true]]);
        expect(widget.isConnected).toBe(false);
        expect(document.querySelector('[az-nodiff]')).not.toBeNull();
    });

    it('carries an az-nodiff element nested in another along with it, and nothing else', () => {
        const mounted = vi.fn();
        hooks.Inner = { mounted };
        const outer =
            '<section az-nodiff="O"><div az-nodiff="I" az-hook="Inner"></div>' +
            '<i az-hook="Inner"></i></section>';
        const widget = setupPage(outer);
        expect(mounted).toHaveBeenCalledTimes(2);
        // Markup the widget added: Arizona did not render it, so it mounts nothing.
        widget.insertAdjacentHTML('beforeend', '<b az-hook="Inner"></b>');
        applyOps([[OP.TEXT, `page:${SLOT_AZ}`, outer, true]]);
        expect(document.querySelector('[az-nodiff="O"]')).toBe(widget);
        expect(mounted).toHaveBeenCalledTimes(2);
    });
});

describe('OP.REPLACE', () => {
    it('keeps an az-nodiff element named again on a resync', () => {
        const events = widgetHook();
        const widget = setupPage(LINE);
        applyOps([[OP.REPLACE, 'page', page(LINE)]], true);
        expect(document.querySelector('[az-nodiff]')).toBe(widget);
        expect(document.querySelector('main')).not.toBeNull();
        expect(events).toEqual(['mount:0']);
    });

    it('remounts an az-nodiff element named differently on a resync', () => {
        const events = widgetHook();
        const widget = setupPage(BAR);
        applyOps([[OP.REPLACE, 'page', page(LINE)]], true);
        expect(widget.isConnected).toBe(false);
        expect(events).toEqual(['mount:0', 'destroy:0', 'mount:0']);
    });

    it('keeps nothing on a navigate', () => {
        const events = widgetHook();
        const widget = setupPage(LINE);
        applyOps([[OP.REPLACE, 'page', page(LINE)]]);
        expect(widget.isConnected).toBe(false);
        expect(events).toEqual(['mount:0', 'destroy:0', 'mount:0']);
    });

    it('does not keep an az-nodiff element holding a live view on a resync', () => {
        widgetHook();
        const withView = '<section az-nodiff="R"><span az-view id="child">0</span></section>';
        const widget = setupPage(withView);
        applyOps([[OP.REPLACE, 'page', page(withView)]], true);
        expect(widget.isConnected).toBe(false);
    });
});
