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

// `?each(fun(#{v := V}) -> {li, ['az-nodiff', {az_hook, ~"W"}], [V]} end, ...)`:
// the region is the item itself. Any change to such a list falls back to the
// wholesale container OP_TEXT.
const LIST_AZ = 'L46T83-0';
const ITEM = (v) => `<li az-region="1EOT1I5" az-hook="W">${v}</li>`;
const ITEM_TMPL = { f: '1EOT1I5', s: ['<li', ' az-hook="W">', '</li>'], t: 0 };
const listOp = (vs) => [
    OP.TEXT,
    `page:${LIST_AZ}`,
    resolveHtml({ ...ITEM_TMPL, d: vs.map((v) => [' az-region="1EOT1I5"', v]) }),
    true,
];

function setupList(vs) {
    document.body.innerHTML =
        `<main id="page" az-view><ul az="${LIST_AZ}"><!--az:${LIST_AZ}-->` +
        `${vs.map(ITEM).join('')}<!--/az--></ul></main>`;
    mountHooks(document);
}

const items = () => Array.from(document.querySelectorAll('li'));

describe('a plain list re-rendered wholesale', () => {
    it('keeps the regions at their positions and mounts only the new one', () => {
        const events = widgetHook();
        setupList(['a', 'b']);
        const [a, b] = items();
        a.dataset.state = 'typed';
        applyOps([listOp(['a', 'b', 'c'])]);
        const after = items();
        expect(after).toHaveLength(3);
        expect(after[0]).toBe(a);
        expect(after[1]).toBe(b);
        expect(a.dataset.state).toBe('typed');
        expect(after[2].textContent).toBe('c');
        expect(events).toEqual(['mount:a', 'mount:b', 'mount:c']);
    });

    it('destroys only the regions the new render no longer holds', () => {
        const events = widgetHook();
        setupList(['a', 'b', 'c']);
        const [a] = items();
        applyOps([listOp(['a'])]);
        expect(items()).toEqual([a]);
        expect(events).toEqual(['mount:a', 'mount:b', 'mount:c', 'destroy:b', 'destroy:c']);
    });
});

// A stream's items carry their key, so a region follows its item, not its position.
// The item root names the key in its `az-region` too
// (`arizona_template:region_token(~"1EOT1I5", undefined, Key)`).
describe('stream items re-rendered wholesale', () => {
    const TOKENS = { 1: '1EOT1I5.kPE60I9', 2: '1EOT1I5.kVE5ITL', 3: '1EOT1I5.k1A4SN17' };
    const KEYED = (k) => `<li az-key="${k}" az-region="${TOKENS[k]}" az-hook="W">${k}</li>`;
    const keyedHtml = (ks) => ks.map(KEYED).join('');

    it('keeps each region with its key', () => {
        const events = widgetHook();
        document.body.innerHTML =
            `<main id="page" az-view><ul az="${LIST_AZ}"><!--az:${LIST_AZ}-->` +
            `${keyedHtml(['1', '2'])}<!--/az--></ul></main>`;
        mountHooks(document);
        const [one, two] = items();
        applyOps([[OP.TEXT, `page:${LIST_AZ}`, keyedHtml(['2', '3', '1']), true]]);
        const after = items();
        expect(after[0]).toBe(two);
        expect(after[2]).toBe(one);
        expect(events).toEqual(['mount:1', 'mount:2', 'mount:3']);
    });

    it('keeps a region inside an item with that item', () => {
        widgetHook();
        const row = (k) =>
            `<li az-key="${k}"><!--az:R-0--><div az-region="W" az-hook="W">${k}</div><!--/az--></li>`;
        document.body.innerHTML =
            `<main id="page" az-view><ul az="${LIST_AZ}"><!--az:${LIST_AZ}-->` +
            `${row('1')}${row('2')}<!--/az--></ul></main>`;
        mountHooks(document);
        const [one, two] = Array.from(document.querySelectorAll('[az-region]'));
        applyOps([[OP.TEXT, `page:${LIST_AZ}`, row('2') + row('1'), true]]);
        expect(Array.from(document.querySelectorAll('[az-region]'))).toEqual([two, one]);
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
        document.body.innerHTML =
            `<main id="page" az-view><div az="${SLOT_AZ}"><!--az:A-1-->${LINE}<!--/az-->` +
            '<!--az:A-2--><!--/az--></div></main>';
        mountHooks(document);
        const region = document.querySelector('[az-region]');
        applyOps([
            [
                OP.TEXT,
                `page:${SLOT_AZ}`,
                `<!--az:A-1--><!--/az--><!--az:A-2-->${LINE}<!--/az-->`,
                true,
            ],
        ]);
        expect(region.isConnected).toBe(false);
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

    it('keeps a region on the markerless path too', () => {
        widgetHook();
        document.body.innerHTML = `<main id="page" az-view><div az="X-0">${LINE}</div></main>`;
        mountHooks(document);
        const region = document.querySelector('[az-region]');
        applyOps([[OP.TEXT, 'page:X-0', LINE, true]]);
        expect(document.querySelector('[az-region]')).toBe(region);
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
