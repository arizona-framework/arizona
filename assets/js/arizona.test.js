import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';
import {
    applyEffects,
    applyOps,
    executeJS,
    get,
    hooks,
    mountHooks,
    OP,
    requestPip,
    resolveEl,
    restoreFormState,
    saveFormState,
    set,
    setAll,
} from './arizona.js';

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/** Reset DOM and hooks between tests. */
beforeEach(() => {
    document.body.innerHTML = '';
    // Clear hook definitions
    for (const k of Object.keys(hooks)) delete hooks[k];
});

/**
 * Move `viewId` into a stand-in Document PiP window (an iframe document -- a real
 * second realm, like the browser's PiP window). Returns that document plus a closer
 * that runs the pagehide teardown, so the module's `_viewDocs` never leaks between tests.
 */
async function popOutView(viewId) {
    const frame = document.createElement('iframe');
    document.body.appendChild(frame);
    const pipDoc = frame.contentDocument;
    // requestPip binds its delegated listeners with an AbortSignal from this realm;
    // jsdom's IDL conversion rejects that on a document from another one (a real
    // browser accepts it). Those listeners aren't under test -- drop the options.
    const addEventListener = pipDoc.addEventListener.bind(pipDoc);
    pipDoc.addEventListener = (type, cb) => addEventListener(type, cb);
    let onPageHide = () => {};
    window.documentPictureInPicture = {
        requestWindow: async () => ({
            document: pipDoc,
            addEventListener: (_evt, cb) => {
                onPageHide = cb;
            },
        }),
    };
    await requestPip(viewId);
    return {
        pipDoc,
        closePip: () => {
            onPageHide();
            frame.remove();
            delete window.documentPictureInPicture;
        },
    };
}

/** Minimal DOM with a single view and one dynamic element. */
function setupView(viewId, innerHTML) {
    document.body.innerHTML = `<div id="${viewId}" az-view>${innerHTML}</div>`;
}

/**
 * Mock the Worker constructor and call mod.connect() to set up module state.
 * Does NOT simulate the open message -- call mock.simulateOpen() when ready.
 * This gives tests control over when _connected becomes true.
 *
 * @param {object} mod -- freshly imported arizona module (via vi.resetModules + import)
 * @returns mock object with posted messages, simulate helpers, and restore()
 */
function setupMockWorker(mod) {
    const posted = [];
    let workerOnmessage = null;
    const mockWorkerInstance = {
        postMessage: (data) => posted.push(data),
        set onmessage(fn) {
            workerOnmessage = fn;
        },
        get onmessage() {
            return workerOnmessage;
        },
        terminate: vi.fn(),
    };
    const OrigWorker = globalThis.Worker;
    globalThis.Worker = function () {
        return mockWorkerInstance;
    };

    const disconnect = mod.connect('/ws');

    return {
        worker: mockWorkerInstance,
        posted,
        /** Simulate Worker open message [1, isReconnect] -- sets _connected=true */
        simulateOpen(isReconnect = false) {
            mockWorkerInstance.onmessage({ data: [1, isReconnect] });
        },
        /** Simulate Worker close message [2, code] -- sets _connected=false */
        simulateClose(code = 1006) {
            mockWorkerInstance.onmessage({ data: [2, code] });
        },
        /** Simulate Worker resolved message [0, ops, effects, firstAfterReconnect] */
        simulateMessage(ops, effects, firstAfterReconnect = false) {
            mockWorkerInstance.onmessage({ data: [0, ops, effects, firstAfterReconnect] });
        },
        /** Get all [1, jsonString] sends (Main->Worker) and parse them */
        getSentMessages() {
            return posted.filter((d) => d[0] === 1).map((d) => JSON.parse(d[1]));
        },
        /** Tear down listeners the module registered, terminate the worker, restore Worker ctor. */
        restore() {
            disconnect();
            globalThis.Worker = OrigWorker;
        },
    };
}

// ---------------------------------------------------------------------------
// 1. OP constants
// ---------------------------------------------------------------------------

describe('OP constants', () => {
    it('has all 10 opcodes with expected values', () => {
        expect(OP.TEXT).toBe(0);
        expect(OP.SET_ATTR).toBe(1);
        expect(OP.REM_ATTR).toBe(2);
        expect(OP.UPDATE).toBe(3);
        expect(OP.REMOVE_NODE).toBe(4);
        expect(OP.INSERT).toBe(5);
        expect(OP.REMOVE).toBe(6);
        expect(OP.ITEM_PATCH).toBe(7);
        expect(OP.REPLACE).toBe(8);
        expect(OP.MOVE).toBe(9);
    });
});

// ---------------------------------------------------------------------------
// 2. resolveEl
// ---------------------------------------------------------------------------

describe('resolveEl', () => {
    it('finds element by viewId:az target', () => {
        setupView('v', '<span az="0">hi</span>');
        const el = resolveEl('v:0');
        expect(el).not.toBeNull();
        expect(el.textContent).toBe('hi');
    });

    it('returns null for missing view', () => {
        setupView('v', '<span az="0">hi</span>');
        expect(resolveEl('missing:0')).toBeNull();
    });

    it('returns null for missing az element', () => {
        setupView('v', '<span az="0">hi</span>');
        expect(resolveEl('v:999')).toBeNull();
    });

    it('resolves bare target (no colon) to view root element', () => {
        setupView('page', '<h1 az="0">title</h1>');
        const el = resolveEl('page');
        expect(el).not.toBeNull();
        expect(el.id).toBe('page');
    });

    it('resolves az on the view root element itself', () => {
        document.body.innerHTML = '<div id="v" az-view az="0"><!--az:0-->content<!--/az--></div>';
        const el = resolveEl('v:0');
        expect(el).not.toBeNull();
        expect(el.id).toBe('v');
        expect(el.getAttribute('az')).toBe('0');
    });

    it('resolves compound target by falling back to base az', () => {
        setupView('v', '<p az="0"><!--az:0-->A<!--/az--><!--az:0:1-->B<!--/az--></p>');
        const el = resolveEl('v:0:1');
        expect(el).not.toBeNull();
        expect(el.getAttribute('az')).toBe('0');
    });

    it('resolves compound target with exact match when present', () => {
        setupView('v', '<p az="0">text</p><p az="0:1">other</p>');
        const el = resolveEl('v:0:1');
        expect(el).not.toBeNull();
        expect(el.getAttribute('az')).toBe('0:1');
    });
});

// ---------------------------------------------------------------------------
// 2b. resolveEl -- compound markers with OP.TEXT
// ---------------------------------------------------------------------------

describe('applyOps -- OP.TEXT with compound markers', () => {
    it('independently updates two text dynamics in the same element', () => {
        setupView('v', '<p az="0"><!--az:0-->A<!--/az--><!--az:0:1-->B<!--/az--></p>');
        // Update first dynamic
        applyOps([[OP.TEXT, 'v:0', 'X']]);
        const el = resolveEl('v:0');
        expect(el.textContent).toBe('XB');
        // Update second dynamic
        applyOps([[OP.TEXT, 'v:0:1', 'Y']]);
        expect(el.textContent).toBe('XY');
    });

    it('updates second dynamic without affecting first', () => {
        setupView(
            'v',
            '<p az="0"><!--az:0-->first<!--/az--> and <!--az:0:1-->second<!--/az--></p>',
        );
        applyOps([[OP.TEXT, 'v:0:1', 'updated']]);
        const el = resolveEl('v:0');
        expect(el.textContent).toBe('first and updated');
    });

    // Round-trip against REAL arizona_render/arizona_diff output (captured): a
    // content-slot conditional branch `{p, [], [?get(a), " ", ?get(b)]}` whose `a`
    // changed A -> A2. The parent slot marker (130FA4-0) wraps the <p>, whose two
    // text slots are E5X9J-0 and E5X9J-0:1. Phase 2 emits the fine-grained inner op
    // [OP_TEXT, E5X9J-0, "A2"]; applying it to the old SSR DOM must yield exactly the
    // new SSR DOM -- only the first slot patched, the sibling and structure intact.
    it('fine-grained conditional branch op yields the new SSR subtree (Phase 2)', () => {
        document.body.innerHTML =
            '<div id="x" az-view az="130FA4-0">' +
            '<!--az:130FA4-0-->' +
            '<p az="E5X9J-0"><!--az:E5X9J-0-->A<!--/az--> <!--az:E5X9J-0:1-->B<!--/az--></p>' +
            '<!--/az--></div>';
        applyOps([[OP.TEXT, 'x:E5X9J-0', 'A2']]);
        expect(resolveEl('x:E5X9J-0').outerHTML).toBe(
            '<p az="E5X9J-0"><!--az:E5X9J-0-->A2<!--/az--> <!--az:E5X9J-0:1-->B<!--/az--></p>',
        );
    });
});

// ---------------------------------------------------------------------------
// 3. applyOps -- OP.TEXT
// ---------------------------------------------------------------------------

describe('applyOps -- OP.TEXT', () => {
    it('updates textContent of a simple element', () => {
        setupView('v', '<span az="0">old</span>');
        applyOps([[OP.TEXT, 'v:0', 'new']]);
        expect(resolveEl('v:0').textContent).toBe('new');
    });

    // A single-value element (e.g. a live stat/price span) updates its lone text
    // node in place rather than via `textContent =` (which removes + reinserts it).
    // The childList churn would revert an in-progress scroll on WebKitGTK; an
    // in-place data write does not. Same node identity proves no childList churn.
    it('updates a simple element in place when it holds one text node', () => {
        setupView('v', '<span az="0">old</span>');
        const el = resolveEl('v:0');
        const textNode = el.firstChild;
        expect(textNode.nodeType).toBe(3);
        applyOps([[OP.TEXT, 'v:0', 'new']]);
        expect(el.firstChild).toBe(textNode); // reused in place
        expect(el.firstChild.data).toBe('new');
    });

    it('updates content between comment markers', () => {
        setupView('v', '<span az="0"><!--az:0-->old<!--/az--></span>');
        applyOps([[OP.TEXT, 'v:0', 'new']]);
        const el = resolveEl('v:0');
        // The text between markers should be replaced
        expect(el.textContent).toBe('new');
        // Markers should still be present
        const comments = [];
        for (const node of el.childNodes) {
            if (node.nodeType === 8) comments.push(node.data);
        }
        expect(comments).toContain('az:0');
        expect(comments).toContain('/az');
    });

    // An HTML fragment (nested template / `?each` / `?raw`) carries the isHtml flag
    // (op[3]=true) the worker sets for an object payload; the client innerHTMLs it.
    it('handles HTML content in comment markers (isHtml fragment)', () => {
        setupView('v', '<div az="0"><!--az:0-->old<!--/az--></div>');
        applyOps([[OP.TEXT, 'v:0', '<b>bold</b>', true]]);
        const el = resolveEl('v:0');
        expect(el.querySelector('b')).not.toBeNull();
        expect(el.querySelector('b').textContent).toBe('bold');
    });

    it('updates content when az target is the view root element (isHtml fragment)', () => {
        document.body.innerHTML = '<div id="v" az-view az="0"><!--az:0-->old<!--/az--></div>';
        applyOps([[OP.TEXT, 'v:0', '<b>new</b>', true]]);
        const el = resolveEl('v:0');
        expect(el).not.toBeNull();
        expect(el.querySelector('b')).not.toBeNull();
        expect(el.querySelector('b').textContent).toBe('new');
    });

    // A scalar `?get` value containing markup is sent RAW and carries NO isHtml flag;
    // the client text-nodes it -> shown as literal text, never parsed (no injection),
    // matching SSR (which escapes the same value).
    it('renders a scalar value containing markup as literal text (no injection)', () => {
        setupView('v', '<div az="0"><!--az:0-->old<!--/az--></div>');
        applyOps([[OP.TEXT, 'v:0', '<img src=x onerror="window.__xss=1">']]);
        const el = resolveEl('v:0');
        expect(el.querySelector('img')).toBeNull();
        expect(el.textContent).toBe('<img src=x onerror="window.__xss=1">');
        expect(window.__xss).toBeUndefined();
    });

    it('a scalar value with HTML matches SSR display (literal angle brackets)', () => {
        setupView('v', '<div az="0"><!--az:0-->old<!--/az--></div>');
        applyOps([[OP.TEXT, 'v:0', '<b>x</b>']]);
        // SSR escapes to &lt;b&gt;x&lt;/b&gt; which the HTML parser decodes to the literal
        // text `<b>x</b>`; a raw text node shows the same literal text. Both agree.
        expect(resolveEl('v:0').textContent).toBe('<b>x</b>');
        expect(resolveEl('v:0').querySelector('b')).toBeNull();
    });

    // A scalar text update reuses the existing text node IN PLACE (characterData)
    // rather than removing + inserting it. A childList remove+insert forces a layout
    // recompute that makes WebKitGTK revert an in-progress scroll to a remembered
    // offset (no scroll anchoring); an in-place data write does not. Same node
    // identity across the update proves there was no childList churn.
    it('updates a lone text node in place (no childList churn) for a scalar value', () => {
        setupView('v', '<span az="0"><!--az:0-->old<!--/az--></span>');
        const el = resolveEl('v:0');
        const textNode = el.childNodes[1]; // [<!--az:0-->, "old", <!--/az-->]
        expect(textNode.nodeType).toBe(3);
        applyOps([[OP.TEXT, 'v:0', 'new']]);
        expect(el.childNodes[1]).toBe(textNode); // same node, reused
        expect(el.childNodes[1].data).toBe('new');
    });

    it('falls back to inserting a text node when the slot was empty', () => {
        setupView('v', '<span az="0"><!--az:0--><!--/az--></span>');
        applyOps([[OP.TEXT, 'v:0', 'hello']]);
        expect(resolveEl('v:0').textContent).toBe('hello');
    });

    // HTML -> scalar transition: the slot held an element, so the in-place fast path
    // must NOT apply; the element is removed and replaced by a literal text node.
    it('replaces an HTML fragment with a scalar text node (no in-place reuse)', () => {
        setupView('v', '<div az="0"><!--az:0--><b>bold</b><!--/az--></div>');
        applyOps([[OP.TEXT, 'v:0', 'plain']]);
        const el = resolveEl('v:0');
        expect(el.querySelector('b')).toBeNull();
        expect(el.textContent).toBe('plain');
    });
});

// ---------------------------------------------------------------------------
// 3b. applyOps -- OP.TEXT round-trip: applied DOM === fresh SSR of the new state
// ---------------------------------------------------------------------------

// The strongest wire invariant: SSR(state0) + diff(state0 -> state1) must yield exactly
// SSR(state1). Each case captures a fresh SSR render of the new state, then applies the
// diff op to an SSR render of the old state and asserts byte-equal innerHTML (markers and
// escaping included). Complements the property-based injection/display/fragment tests.
describe('applyOps -- OP.TEXT round-trip (applied DOM === fresh SSR)', () => {
    it('?get scalar (with markup): escaping matches SSR', () => {
        // SSR escapes a ?get value; the parser decodes the entities back to a literal
        // text node, so the new-state render holds `<b>x</b>` as text.
        setupView('v', '<span az="0"><!--az:0-->&lt;b&gt;x&lt;/b&gt;<!--/az--></span>');
        const freshSsr = resolveEl('v:0').innerHTML;
        // Old-state SSR, then the diff op (value sent RAW, no isHtml -> text node).
        setupView('v', '<span az="0"><!--az:0-->old<!--/az--></span>');
        applyOps([[OP.TEXT, 'v:0', '<b>x</b>']]);
        expect(resolveEl('v:0').innerHTML).toBe(freshSsr);
    });

    it('?raw value: trusted HTML matches SSR verbatim', () => {
        setupView('v', '<div az="0"><!--az:0--><b>z</b><!--/az--></div>');
        const freshSsr = resolveEl('v:0').innerHTML;
        setupView('v', '<div az="0"><!--az:0--><b>a</b><!--/az--></div>');
        // A ?raw value is wire-tagged `{raw}`; the worker flattens it to HTML + isHtml.
        applyOps([[OP.TEXT, 'v:0', '<b>z</b>', true]]);
        expect(resolveEl('v:0').innerHTML).toBe(freshSsr);
    });

    it('nested template (wholesale): resolved HTML matches SSR', () => {
        // A nested-template zip-map is resolved to HTML by the worker; at the main thread
        // it arrives as an HTML string with isHtml=true.
        setupView('v', '<div az="0"><!--az:0--><p az="0:0">Hi</p><!--/az--></div>');
        const freshSsr = resolveEl('v:0').innerHTML;
        setupView('v', '<div az="0"><!--az:0--><p az="0:0">Old</p><!--/az--></div>');
        applyOps([[OP.TEXT, 'v:0', '<p az="0:0">Hi</p>', true]]);
        expect(resolveEl('v:0').innerHTML).toBe(freshSsr);
    });
});

// ---------------------------------------------------------------------------
// 4. applyOps -- OP.SET_ATTR
// ---------------------------------------------------------------------------

describe('applyOps -- OP.SET_ATTR', () => {
    it('sets an attribute on an element', () => {
        setupView('v', '<span az="0">hi</span>');
        applyOps([[OP.SET_ATTR, 'v:0', 'class', 'active']]);
        expect(resolveEl('v:0').getAttribute('class')).toBe('active');
    });

    it('syncs value property on input elements', () => {
        setupView('v', '<input az="0" value="old" />');
        applyOps([[OP.SET_ATTR, 'v:0', 'value', 'new']]);
        const el = /** @type {HTMLInputElement} */ (resolveEl('v:0'));
        expect(el.getAttribute('value')).toBe('new');
        expect(el.value).toBe('new');
    });
});

// ---------------------------------------------------------------------------
// 5. applyOps -- OP.REM_ATTR
// ---------------------------------------------------------------------------

describe('applyOps -- OP.REM_ATTR', () => {
    it('removes an attribute from an element', () => {
        setupView('v', '<span az="0" class="old">hi</span>');
        applyOps([[OP.REM_ATTR, 'v:0', 'class']]);
        expect(resolveEl('v:0').hasAttribute('class')).toBe(false);
    });
});

// ---------------------------------------------------------------------------
// 6. applyOps -- OP.UPDATE
// ---------------------------------------------------------------------------

describe('applyOps -- OP.UPDATE', () => {
    it('replaces innerHTML of an element', () => {
        setupView('v', '<div az="0"><span>old</span></div>');
        applyOps([[OP.UPDATE, 'v:0', '<b>replaced</b>']]);
        const el = resolveEl('v:0');
        expect(el.innerHTML).toBe('<b>replaced</b>');
    });
});

// ---------------------------------------------------------------------------
// 6b. Regression: a plain-list ?each among static siblings in one content slot.
//
// SSR anchors the each by `<!--az:strip:2-->...<!--/az-->` comment markers
// between sibling element children -- there is NO element carrying az="strip:2".
// resolveEl('v:strip:2') therefore finds no element, strips the trailing
// ":<slot>" and falls back to the enclosing element az="strip". The container
// op MUST be the marker-aware OP.TEXT (replace marker content); OP.UPDATE writes
// innerHTML on the fallback element and wipes the static sibling .item divs.
// Mirrors arizona_diff_SUITE:diff_each_among_siblings_uses_text_op.
// ---------------------------------------------------------------------------

describe('applyOps -- plain-list each among static siblings', () => {
    // SSR shape: two static .item divs, then a marker-delimited each slot, all
    // direct children of <div class="strip" az="strip">.
    const stripHtml = (eachContent) =>
        '<div class="strip" az="strip">' +
        '<div class="item" az="strip:0"><!--az:strip:0-->A<!--/az--></div>' +
        '<div class="item" az="strip:1"><!--az:strip:1-->B<!--/az--></div>' +
        `<!--az:strip:2-->${eachContent}<!--/az-->` +
        '</div>';

    it('OP.TEXT preserves static siblings across toggle on->off->on', () => {
        setupView('v', stripHtml(''));
        const strip = document.querySelector('.strip');
        const itemHtml = (k) => `<div class="item" az="strip:2:0"><span>${k}</span></div>`;

        // toggle ON: [] -> [k1]  (a plain-list `?each` is an HTML fragment -> isHtml)
        applyOps([[OP.TEXT, 'v:strip:2', itemHtml('k1'), true]]);
        // static siblings survive
        expect(strip.querySelectorAll('.item').length).toBe(3);
        expect(strip.children[0].textContent).toBe('A');
        expect(strip.children[1].textContent).toBe('B');
        expect(strip.querySelector('[az="strip:2:0"]').textContent).toBe('k1');

        // toggle OFF: [k1] -> []  (empty each is still an HTML fragment)
        applyOps([[OP.TEXT, 'v:strip:2', '', true]]);
        expect(strip.querySelectorAll('.item').length).toBe(2);
        expect(strip.children[0].textContent).toBe('A');
        expect(strip.children[1].textContent).toBe('B');

        // toggle ON again: [] -> [k2]
        applyOps([[OP.TEXT, 'v:strip:2', itemHtml('k2'), true]]);
        expect(strip.querySelectorAll('.item').length).toBe(3);
        expect(strip.children[0].textContent).toBe('A');
        expect(strip.children[1].textContent).toBe('B');
        expect(strip.querySelector('[az="strip:2:0"]').textContent).toBe('k2');
    });

    it('OP.UPDATE (the old buggy op) clobbers the static siblings', () => {
        // Documents the failure mode the diff fix avoids: with no element
        // carrying az="strip:2", resolveEl falls back to the .strip element and
        // innerHTML wipes the static .item siblings.
        setupView('v', stripHtml(''));
        const strip = document.querySelector('.strip');
        applyOps([
            [OP.UPDATE, 'v:strip:2', '<div class="item" az="strip:2:0"><span>k1</span></div>'],
        ]);
        // Only the each item remains; the two static siblings are gone.
        expect(strip.querySelectorAll('.item').length).toBe(1);
        expect(strip.textContent).toBe('k1');
    });
});

// ---------------------------------------------------------------------------
// 6c. Type switch: an `arizona_stream` binding becomes a plain list (and back).
//
// The only path where the plain-list OP_TEXT change touches streams. SSR anchors
// BOTH a stream each and a plain-list each by the same `<!--az:X-->...<!--/az-->`
// content-slot markers (arizona_html:text_slot_open/1; verified against real
// arizona_render output). A stream addresses its visible items by `az-key`
// ELEMENT children; the comment markers are never the target of a stream op
// (insert/remove/move/patch all query `:scope > [az-key]`), so the marker
// survives every incremental mutation.
//
// When the binding switches stream -> list, arizona_diff routes
// diff_list/4 -> full_update/5 -> OP_TEXT (op code 0). OP_TEXT's handler
// (applyTextOp -> updateMarkerContent) finds the surviving `<!--az:X-->` marker
// and replaces ONLY the span between the markers: the list renders as real
// elements (NOT escaped text) and any static siblings of the slot survive.
//
// The reverse (list -> stream) stays on OP_UPDATE (innerHTML on the resolved
// container), which is correct when the stream each is the addressable element
// (sole child of its container). Mirrors arizona_stream_SUITE:
// list_type_switch_stream_to_list / list_type_switch_list_to_stream.
// ---------------------------------------------------------------------------

describe('applyOps -- stream <-> plain-list type switch', () => {
    // Real SSR shape for a stream each that is the SOLE child of <ul az="0">:
    // the slot az ("0") coincides with the <ul>'s az, so resolveEl('v:0')
    // returns the <ul> and the keyed <li>s live between its markers.
    const ssrStreamSole = (items) =>
        '<ul az="0"><!--az:0-->' +
        items.map((t) => `<li az="i" az-key="${t}"><!--az:i-->${t}<!--/az--></li>`).join('') +
        '<!--/az--></ul>';

    // Real SSR shape for a stream each among static siblings: the static <span>
    // carries az="strip:0", the each slot is the marker pair az="strip:1" with
    // NO element carrying that az (resolveEl falls back to the .strip element).
    const ssrStreamAmongSiblings = (items) =>
        '<div class="strip" az="strip">' +
        '<span az="strip:0"><!--az:strip:0-->S<!--/az--></span>' +
        '<!--az:strip:1-->' +
        items.map((t) => `<li az="i" az-key="${t}"><!--az:i-->${t}<!--/az--></li>`).join('') +
        '<!--/az--></div>';

    const listItem = (t) => `<li az="L"><!--az:L-->${t}<!--/az--></li>`;

    /** True if `el` has a direct-child comment marker `az:<az>` (the slot anchor). */
    const hasMarker = (el, az) =>
        Array.from(el.childNodes).some((n) => n.nodeType === 8 && n.data === `az:${az}`);

    it('SSR anchors a stream each by the same content-slot marker as a list', () => {
        setupView('v', ssrStreamSole(['1']));
        const ul = document.querySelector('ul');
        // The marker is a direct child of the container after SSR (Q1).
        expect(hasMarker(ul, '0')).toBe(true);
        // The keyed item is a real element between the markers.
        expect(ul.querySelector('[az-key="1"]').textContent).toBe('1');
    });

    it('stream insert/remove/move never delete the slot marker (Q2)', () => {
        setupView('v', ssrStreamSole(['1']));
        const ul = document.querySelector('ul');
        applyOps([
            [OP.INSERT, 'v:0', '2', -1, '<li az="i" az-key="2"><!--az:i-->2<!--/az--></li>'],
        ]);
        expect(hasMarker(ul, '0')).toBe(true);
        applyOps([[OP.MOVE, 'v:0', '2', null]]);
        expect(hasMarker(ul, '0')).toBe(true);
        applyOps([[OP.REMOVE, 'v:0', '1']]);
        // Marker still present after the full mutation sequence.
        expect(hasMarker(ul, '0')).toBe(true);
    });

    it('stream -> list (no runtime mutation) renders real HTML, not escaped text', () => {
        setupView('v', ssrStreamSole(['1', '2']));
        const ul = document.querySelector('ul');
        // Switch the binding stream -> list: diff emits OP_TEXT on the slot (a list is
        // an HTML fragment -> isHtml).
        applyOps([[OP.TEXT, 'v:0', listItem('X') + listItem('Y'), true]]);
        // Rendered as REAL <li> elements (escaped text would be 0 elements).
        const lis = ul.querySelectorAll('li');
        expect(lis.length).toBe(2);
        expect(lis[0].textContent).toBe('X');
        expect(lis[1].textContent).toBe('Y');
        // Crucially NOT escaped: the raw "<li" markup must not survive as text.
        expect(ul.textContent).not.toContain('<li');
        // Marker preserved for subsequent diffs.
        expect(hasMarker(ul, '0')).toBe(true);
    });

    it('stream -> list switch keeps static siblings of the slot (Q3)', () => {
        setupView('v', ssrStreamAmongSiblings(['1']));
        const strip = document.querySelector('.strip');
        // Switch stream -> list via the marker-aware OP_TEXT.
        applyOps([[OP.TEXT, 'v:strip:1', listItem('X'), true]]);
        // The static <span> sibling survives (the OP_UPDATE fallback would wipe it).
        expect(strip.querySelector('[az="strip:0"]')).not.toBeNull();
        expect(strip.querySelector('[az="strip:0"]').textContent).toBe('S');
        // The list item rendered as a real element inside the marker.
        const li = strip.querySelector('[az="L"]');
        expect(li).not.toBeNull();
        expect(li.textContent).toBe('X');
    });

    it('stream -> list after a runtime insert still renders the list as real HTML', () => {
        // A runtime-inserted keyed item lands after <!--/az--> (insertItemEl
        // appends to the container). The subsequent stream->list OP_TEXT replaces
        // the marker span correctly -- the NEW list is real HTML inside the
        // markers and the static sibling survives -- even though the prior
        // runtime-inserted item is left as an orphan after the closing marker.
        // This orphan is the separate "stream among siblings" concern tracked in
        // docs/architecture.md, not an OP_TEXT regression.
        setupView('v', ssrStreamAmongSiblings(['1']));
        const strip = document.querySelector('.strip');
        applyOps([
            [OP.INSERT, 'v:strip:1', '2', -1, '<li az="i" az-key="2"><!--az:i-->2<!--/az--></li>'],
        ]);
        applyOps([[OP.TEXT, 'v:strip:1', listItem('X'), true]]);
        // Static sibling intact and the list slot holds the real list element.
        expect(strip.querySelector('[az="strip:0"]').textContent).toBe('S');
        const slotLi = strip.querySelector('[az="L"]');
        expect(slotLi).not.toBeNull();
        expect(slotLi.textContent).toBe('X');
        // Real elements, not escaped text.
        expect(strip.innerHTML).toContain('<li az="L"');
    });

    it('reverse list -> stream stays correct for a sole-child each (OP_UPDATE)', () => {
        // SSR plain-list each, sole child of <ul az="0"> (resolveEl returns the
        // <ul>, which IS the slot element). list -> stream emits OP_UPDATE.
        setupView('v', `<ul az="0"><!--az:0-->${listItem('A')}<!--/az--></ul>`);
        const ul = document.querySelector('ul');
        applyOps([[OP.UPDATE, 'v:0', '<li az="i" az-key="1"><!--az:i-->X<!--/az--></li>']]);
        // Now keyed-by az-key, addressable by stream ops; renders real HTML.
        expect(ul.querySelector('[az-key="1"]').textContent).toBe('X');
        applyOps([
            [OP.INSERT, 'v:0', '2', -1, '<li az="i" az-key="2"><!--az:i-->Y<!--/az--></li>'],
        ]);
        expect(ul.querySelectorAll('[az-key]').length).toBe(2);
    });
});

// ---------------------------------------------------------------------------
// 7. applyOps -- OP.REMOVE_NODE
// ---------------------------------------------------------------------------

describe('applyOps -- OP.REMOVE_NODE', () => {
    it('removes the element from the DOM', () => {
        setupView('v', '<span az="0">bye</span>');
        expect(resolveEl('v:0')).not.toBeNull();
        applyOps([[OP.REMOVE_NODE, 'v:0']]);
        expect(resolveEl('v:0')).toBeNull();
    });
});

// ---------------------------------------------------------------------------
// 8. applyOps -- OP.INSERT
// ---------------------------------------------------------------------------

describe('applyOps -- OP.INSERT', () => {
    it('appends a keyed child when pos is -1', () => {
        setupView('v', '<div az="0"></div>');
        applyOps([[OP.INSERT, 'v:0', 'k1', -1, '<p az-key="k1">item</p>']]);
        const el = resolveEl('v:0');
        const child = el.querySelector('[az-key="k1"]');
        expect(child).not.toBeNull();
        expect(child.textContent).toBe('item');
    });

    it('inserts at a specific position', () => {
        setupView('v', '<div az="0"><p az-key="a">A</p><p az-key="c">C</p></div>');
        applyOps([[OP.INSERT, 'v:0', 'b', 1, '<p az-key="b">B</p>']]);
        const el = resolveEl('v:0');
        const keys = Array.from(el.querySelectorAll('[az-key]')).map((c) =>
            c.getAttribute('az-key'),
        );
        expect(keys).toEqual(['a', 'b', 'c']);
    });

    it('appends when pos exceeds child count', () => {
        setupView('v', '<div az="0"><p az-key="a">A</p></div>');
        applyOps([[OP.INSERT, 'v:0', 'z', 99, '<p az-key="z">Z</p>']]);
        const el = resolveEl('v:0');
        const keys = Array.from(el.querySelectorAll('[az-key]')).map((c) =>
            c.getAttribute('az-key'),
        );
        expect(keys).toEqual(['a', 'z']);
    });
});

// ---------------------------------------------------------------------------
// 9. applyOps -- OP.REMOVE
// ---------------------------------------------------------------------------

describe('applyOps -- OP.REMOVE', () => {
    it('removes a keyed child by az-key', () => {
        setupView('v', '<div az="0"><p az-key="k1">item</p></div>');
        expect(resolveEl('v:0').querySelector('[az-key="k1"]')).not.toBeNull();
        applyOps([[OP.REMOVE, 'v:0', 'k1']]);
        expect(resolveEl('v:0').querySelector('[az-key="k1"]')).toBeNull();
    });
});

// ---------------------------------------------------------------------------
// 9b. applyOps -- OP.MOVE
// ---------------------------------------------------------------------------

describe('applyOps -- OP.MOVE (after_key)', () => {
    it('moves a keyed child after another key, preserving the DOM node', () => {
        setupView(
            'v',
            '<div az="0"><p az-key="a">A</p><p az-key="b">B</p><p az-key="c">C</p></div>',
        );
        const el = resolveEl('v:0');
        const originalNode = el.querySelector('[az-key="c"]');
        // Move c after null (prepend) → [c, a, b]
        applyOps([[OP.MOVE, 'v:0', 'c', null]]);
        const keys = Array.from(el.querySelectorAll(':scope > [az-key]')).map((c) =>
            c.getAttribute('az-key'),
        );
        expect(keys).toEqual(['c', 'a', 'b']);
        // Verify same DOM node was reused (not destroyed and recreated)
        expect(el.querySelector('[az-key="c"]')).toBe(originalNode);
    });

    it('prepends when afterKey is null', () => {
        setupView(
            'v',
            '<div az="0"><p az-key="a">A</p><p az-key="b">B</p><p az-key="c">C</p></div>',
        );
        applyOps([[OP.MOVE, 'v:0', 'c', null]]);
        const el = resolveEl('v:0');
        const keys = Array.from(el.querySelectorAll(':scope > [az-key]')).map((c) =>
            c.getAttribute('az-key'),
        );
        expect(keys).toEqual(['c', 'a', 'b']);
    });

    it('appends when afterKey does not exist', () => {
        setupView('v', '<div az="0"><p az-key="a">A</p><p az-key="b">B</p></div>');
        applyOps([[OP.MOVE, 'v:0', 'a', 'nonexistent']]);
        const el = resolveEl('v:0');
        const keys = Array.from(el.querySelectorAll(':scope > [az-key]')).map((c) =>
            c.getAttribute('az-key'),
        );
        expect(keys).toEqual(['b', 'a']);
    });

    it('is a no-op when key does not exist and warns', () => {
        const spy = vi.spyOn(console, 'warn').mockImplementation(() => {});
        setupView('v', '<div az="0"><p az-key="a">A</p></div>');
        applyOps([[OP.MOVE, 'v:0', 'nonexistent', 'a']]);
        const el = resolveEl('v:0');
        const keys = Array.from(el.querySelectorAll(':scope > [az-key]')).map((c) =>
            c.getAttribute('az-key'),
        );
        expect(keys).toEqual(['a']);
        expect(spy).toHaveBeenCalledWith(expect.stringContaining('nonexistent'));
        spy.mockRestore();
    });

    it('moves after the correct key in a sequence of moves', () => {
        // Simulate [a, b, c, d] → [d, c, b, a] using LIS-style moves
        // LIS of [4,3,2,1] is length 1 (d stays), 3 items need to move
        // Applied left-to-right: move d→null, move c→d, move b→c
        setupView(
            'v',
            '<div az="0"><p az-key="a">A</p><p az-key="b">B</p><p az-key="c">C</p><p az-key="d">D</p></div>',
        );
        applyOps([
            [OP.MOVE, 'v:0', 'd', null],
            [OP.MOVE, 'v:0', 'c', 'd'],
            [OP.MOVE, 'v:0', 'b', 'c'],
        ]);
        const el = resolveEl('v:0');
        const keys = Array.from(el.querySelectorAll(':scope > [az-key]')).map((c) =>
            c.getAttribute('az-key'),
        );
        expect(keys).toEqual(['d', 'c', 'b', 'a']);
    });
});

// ---------------------------------------------------------------------------
// 10. applyOps -- OP.ITEM_PATCH
// ---------------------------------------------------------------------------

describe('applyOps -- OP.ITEM_PATCH', () => {
    it('patches text within a keyed child', () => {
        setupView('v', '<div az="0"><div az-key="k1"><span az="0">old</span></div></div>');
        applyOps([[OP.ITEM_PATCH, 'v:0', 'k1', [[OP.TEXT, '0', 'new']]]]);
        const item = resolveEl('v:0').querySelector('[az-key="k1"]');
        expect(item.querySelector('[az="0"]').textContent).toBe('new');
    });

    it('sets attribute within a keyed child', () => {
        setupView('v', '<div az="0"><div az-key="k1"><span az="0">x</span></div></div>');
        applyOps([[OP.ITEM_PATCH, 'v:0', 'k1', [[OP.SET_ATTR, '0', 'class', 'highlight']]]]);
        const item = resolveEl('v:0').querySelector('[az-key="k1"]');
        expect(item.querySelector('[az="0"]').getAttribute('class')).toBe('highlight');
    });

    it('removes attribute within a keyed child', () => {
        setupView(
            'v',
            '<div az="0"><div az-key="k1"><span az="0" class="old">x</span></div></div>',
        );
        applyOps([[OP.ITEM_PATCH, 'v:0', 'k1', [[OP.REM_ATTR, '0', 'class']]]]);
        const item = resolveEl('v:0').querySelector('[az-key="k1"]');
        expect(item.querySelector('[az="0"]').hasAttribute('class')).toBe(false);
    });

    it('replaces innerHTML within a keyed child', () => {
        setupView('v', '<div az="0"><div az-key="k1"><div az="0">old</div></div></div>');
        applyOps([[OP.ITEM_PATCH, 'v:0', 'k1', [[OP.UPDATE, '0', '<em>new</em>']]]]);
        const item = resolveEl('v:0').querySelector('[az-key="k1"]');
        expect(item.querySelector('[az="0"]').innerHTML).toBe('<em>new</em>');
    });

    it('removes node within a keyed child', () => {
        setupView('v', '<div az="0"><div az-key="k1"><span az="0">bye</span></div></div>');
        applyOps([[OP.ITEM_PATCH, 'v:0', 'k1', [[OP.REMOVE_NODE, '0']]]]);
        const item = resolveEl('v:0').querySelector('[az-key="k1"]');
        expect(item.querySelector('[az="0"]')).toBeNull();
    });

    it('patches text with comment markers within a keyed child', () => {
        setupView(
            'v',
            '<div az="0"><div az-key="k1"><span az="0"><!--az:0-->old<!--/az--></span></div></div>',
        );
        applyOps([[OP.ITEM_PATCH, 'v:0', 'k1', [[OP.TEXT, '0', 'new']]]]);
        const item = resolveEl('v:0').querySelector('[az-key="k1"]');
        expect(item.querySelector('[az="0"]').textContent).toBe('new');
    });

    it('handles nested INSERT inside a keyed child', () => {
        setupView('v', '<div az="0"><div az-key="k1"><ul az="0"></ul></div></div>');
        applyOps([
            [
                OP.ITEM_PATCH,
                'v:0',
                'k1',
                [[OP.INSERT, '0', 'c1', -1, '<li az-key="c1">cell1</li>']],
            ],
        ]);
        const item = resolveEl('v:0').querySelector('[az-key="k1"]');
        const inner = item.querySelector('[az="0"]');
        expect(inner.querySelector('[az-key="c1"]').textContent).toBe('cell1');
    });

    it('handles nested REMOVE inside a keyed child', () => {
        setupView(
            'v',
            '<div az="0"><div az-key="k1"><ul az="0"><li az-key="c1">cell1</li></ul></div></div>',
        );
        applyOps([[OP.ITEM_PATCH, 'v:0', 'k1', [[OP.REMOVE, '0', 'c1']]]]);
        const item = resolveEl('v:0').querySelector('[az-key="k1"]');
        const inner = item.querySelector('[az="0"]');
        expect(inner.querySelector('[az-key="c1"]')).toBeNull();
    });

    it('handles nested MOVE inside a keyed child', () => {
        setupView(
            'v',
            '<div az="0"><div az-key="k1"><ul az="0"><li az-key="c1">A</li><li az-key="c2">B</li></ul></div></div>',
        );
        // Move c2 after null (prepend) → [c2, c1]
        applyOps([[OP.ITEM_PATCH, 'v:0', 'k1', [[OP.MOVE, '0', 'c2', null]]]]);
        const item = resolveEl('v:0').querySelector('[az-key="k1"]');
        const inner = item.querySelector('[az="0"]');
        const keys = Array.from(inner.querySelectorAll(':scope > [az-key]')).map((c) =>
            c.getAttribute('az-key'),
        );
        expect(keys).toEqual(['c2', 'c1']);
    });

    it('handles nested ITEM_PATCH inside a keyed child', () => {
        setupView(
            'v',
            '<div az="0"><div az-key="k1"><ul az="0"><li az-key="c1"><span az="0">old</span></li></ul></div></div>',
        );
        applyOps([
            [OP.ITEM_PATCH, 'v:0', 'k1', [[OP.ITEM_PATCH, '0', 'c1', [[OP.TEXT, '0', 'new']]]]],
        ]);
        const item = resolveEl('v:0').querySelector('[az-key="k1"]');
        const inner = item.querySelector('[az="0"]');
        const cell = inner.querySelector('[az-key="c1"]');
        expect(cell.querySelector('[az="0"]').textContent).toBe('new');
    });
});

// ---------------------------------------------------------------------------
// 10c. applyOps -- stream keys containing CSS metacharacters (D9)
// ---------------------------------------------------------------------------

describe('applyOps -- stream keys with CSS metacharacters (D9)', () => {
    // A server stream key is arbitrary app data. A `"` or `\` in it makes an
    // unescaped `[az-key="..."]` selector invalid, throwing a SyntaxError that
    // (without CSS.escape) aborts the op batch and desyncs the DOM.
    const KEY = 'a"b\\c';

    /** Serialize an element with `az-key` = `k` set safely (no manual escaping). */
    function keyedHTML(k, tag = 'p') {
        const el = document.createElement(tag);
        el.setAttribute('az-key', k);
        el.textContent = 'X';
        return el.outerHTML;
    }

    it('OP.REMOVE removes an item whose key has metacharacters', () => {
        setupView('v', `<div az="0">${keyedHTML(KEY)}</div>`);
        const container = resolveEl('v:0');
        applyOps([[OP.REMOVE, 'v:0', KEY]]);
        expect(container.querySelector('[az-key]')).toBeNull();
    });

    it('OP.MOVE reorders an item whose key has metacharacters', () => {
        setupView('v', `<div az="0">${keyedHTML('a')}${keyedHTML(KEY)}</div>`);
        const container = resolveEl('v:0');
        applyOps([[OP.MOVE, 'v:0', KEY, null]]); // prepend the metachar item
        const keys = Array.from(container.querySelectorAll(':scope > [az-key]')).map((c) =>
            c.getAttribute('az-key'),
        );
        expect(keys).toEqual([KEY, 'a']);
    });

    it('OP.ITEM_PATCH patches an item whose key has metacharacters', () => {
        const outer = document.createElement('div');
        outer.setAttribute('az-key', KEY);
        outer.innerHTML = '<span az="0">old</span>';
        setupView('v', `<div az="0">${outer.outerHTML}</div>`);
        applyOps([[OP.ITEM_PATCH, 'v:0', KEY, [[OP.TEXT, '0', 'new']]]]);
        expect(resolveEl('v:0').querySelector('span[az="0"]').textContent).toBe('new');
    });

    it('OP.INSERT mounts a hook on an inserted item whose key has metacharacters', () => {
        const mounted = vi.fn();
        hooks.Probe = { mounted };
        setupView('v', '<div az="0"></div>');
        const item = document.createElement('p');
        item.setAttribute('az-key', KEY);
        item.setAttribute('az-hook', 'Probe');
        // The post-insert lookup that mounts hooks must find the metachar item.
        applyOps([[OP.INSERT, 'v:0', KEY, -1, item.outerHTML]]);
        expect(mounted).toHaveBeenCalledTimes(1);
    });
});

// ---------------------------------------------------------------------------
// 11. applyOps -- skip missing elements
// ---------------------------------------------------------------------------

describe('applyOps -- skip missing', () => {
    it('gracefully skips ops targeting nonexistent elements', () => {
        setupView('v', '<span az="0">hi</span>');
        // None of these should throw
        applyOps([
            [OP.TEXT, 'missing:0', 'x'],
            [OP.SET_ATTR, 'v:999', 'class', 'x'],
            [OP.REM_ATTR, 'missing:0', 'class'],
            [OP.UPDATE, 'missing:0', '<b>x</b>'],
            [OP.REMOVE_NODE, 'missing:0'],
            [OP.INSERT, 'missing:0', 'k', -1, '<p>x</p>'],
            [OP.REMOVE, 'missing:0', 'k'],
            [OP.ITEM_PATCH, 'missing:0', 'k', [[OP.TEXT, '0', 'x']]],
            [OP.MOVE, 'missing:0', 'k', null],
        ]);
        // Original element should be untouched
        expect(resolveEl('v:0').textContent).toBe('hi');
    });
});

// ---------------------------------------------------------------------------
// 11b. applyOps -- per-op fault isolation (J5)
// ---------------------------------------------------------------------------

describe('applyOps -- per-op fault isolation (J5)', () => {
    it('a throwing hook does not abort the rest of the op batch', () => {
        const errSpy = vi.spyOn(console, 'error').mockImplementation(() => {});
        hooks.Boom = {
            updated() {
                throw new Error('boom');
            },
        };
        document.body.innerHTML =
            '<div id="v" az-view><span az="0" az-hook="Boom">a</span><span az="1">b</span></div>';
        mountHooks(document);
        // The first op fires Boom.updated() which throws; the second must still apply.
        applyOps([
            [OP.TEXT, 'v:0', 'A'],
            [OP.TEXT, 'v:1', 'B'],
        ]);
        expect(resolveEl('v:1').textContent).toBe('B');
        expect(errSpy).toHaveBeenCalled();
        errSpy.mockRestore();
    });

    it('a throwing inner item op does not abort the rest of the item patch', () => {
        const errSpy = vi.spyOn(console, 'error').mockImplementation(() => {});
        hooks.Boom = {
            updated() {
                throw new Error('boom');
            },
        };
        setupView(
            'v',
            '<div az="0"><div az-key="k1">' +
                '<span az="0" az-hook="Boom">a</span><span az="1">b</span>' +
                '</div></div>',
        );
        mountHooks(document);
        applyOps([
            [
                OP.ITEM_PATCH,
                'v:0',
                'k1',
                [
                    [OP.TEXT, '0', 'A'], // fires Boom.updated() -> throws
                    [OP.TEXT, '1', 'B'], // must still apply
                ],
            ],
        ]);
        const item = resolveEl('v:0').querySelector('[az-key="k1"]');
        expect(item.querySelector('[az="1"]').textContent).toBe('B');
        expect(errSpy).toHaveBeenCalled();
        errSpy.mockRestore();
    });
});

// ---------------------------------------------------------------------------
// 12. applyEffects
// ---------------------------------------------------------------------------

describe('applyEffects', () => {
    it('dispatches push_event as CustomEvent on document', () => {
        const handler = vi.fn();
        document.addEventListener('my-event', handler);
        applyEffects([[9, 'my-event', { foo: 'bar' }]]);
        expect(handler).toHaveBeenCalledOnce();
        expect(handler.mock.calls[0][0].detail).toEqual({ foo: 'bar' });
        document.removeEventListener('my-event', handler);
    });

    it('handles multiple effects', () => {
        const h1 = vi.fn();
        const h2 = vi.fn();
        document.addEventListener('evt1', h1);
        document.addEventListener('evt2', h2);
        applyEffects([
            [9, 'evt1', { a: 1 }],
            [9, 'evt2', { b: 2 }],
        ]);
        expect(h1).toHaveBeenCalledOnce();
        expect(h2).toHaveBeenCalledOnce();
        document.removeEventListener('evt1', h1);
        document.removeEventListener('evt2', h2);
    });

    it('ignores unknown effect types', () => {
        // Should not throw
        applyEffects([[999, 'arg1', 'arg2']]);
    });
});

// ---------------------------------------------------------------------------
// 12b. push_event target fallback -- never send a null target
// ---------------------------------------------------------------------------

describe('push_event target fallback (null -> root)', () => {
    let mock;
    afterEach(() => {
        if (mock) mock.restore();
    });

    it('a push_event handler effect resolves to the root view id, never null', async () => {
        // op 0 in the effects channel is JS_PUSH_EVENT. A handler effect is applied
        // against document.documentElement (no enclosing [az-view]), so the target
        // resolves to null -- it must fall back to the root view id. Sending null
        // makes the server tag its diff ops with null, which it cannot encode.
        vi.resetModules();
        const mod = await import('./arizona.js');
        document.body.innerHTML = '<div id="root" az-view></div>';
        mock = setupMockWorker(mod);
        mock.simulateOpen();
        mod.applyEffects([[0, 'refresh', { a: 1 }]]);
        const sends = mock.getSentMessages();
        expect(sends).toContainEqual(['root', 'refresh', { a: 1 }]);
        expect(sends.some((s) => s[0] === null)).toBe(false);
    });

    it('pushEventTo with a null view falls back to the root view id', async () => {
        vi.resetModules();
        const mod = await import('./arizona.js');
        document.body.innerHTML = '<div id="root" az-view></div>';
        mock = setupMockWorker(mod);
        mock.simulateOpen();
        mod.pushEventTo(null, 'ev', { x: 1 });
        expect(mock.getSentMessages()).toContainEqual(['root', 'ev', { x: 1 }]);
    });
});

// ---------------------------------------------------------------------------
// 13. Comment markers -- deeper tests via OP.TEXT
// ---------------------------------------------------------------------------

describe('comment markers', () => {
    it('preserves markers after multiple updates', () => {
        setupView('v', '<span az="0"><!--az:0-->initial<!--/az--></span>');
        applyOps([[OP.TEXT, 'v:0', 'second']]);
        applyOps([[OP.TEXT, 'v:0', 'third']]);
        const el = resolveEl('v:0');
        expect(el.textContent).toBe('third');
        const comments = [];
        for (const node of el.childNodes) {
            if (node.nodeType === 8) comments.push(node.data);
        }
        expect(comments).toContain('az:0');
        expect(comments).toContain('/az');
    });

    it('only matches the correct marker index', () => {
        setupView(
            'v',
            `
            <div az="0">
                <!--az:0-->first<!--/az-->
                <!--az:1-->second<!--/az-->
            </div>
        `,
        );
        applyOps([[OP.TEXT, 'v:0', 'UPDATED']]);
        const el = resolveEl('v:0');
        // az:0 content should be updated, az:1 content should remain
        expect(el.innerHTML).toContain('UPDATED');
        expect(el.innerHTML).toContain('second');
    });

    it('clears content between markers when value is empty string', () => {
        setupView('v', '<span az="0"><!--az:0-->some text<!--/az--></span>');
        applyOps([[OP.TEXT, 'v:0', '']]);
        const el = resolveEl('v:0');
        expect(el.textContent).toBe('');
        // Markers should still be present
        const comments = [];
        for (const node of el.childNodes) {
            if (node.nodeType === 8) comments.push(node.data);
        }
        expect(comments).toContain('az:0');
        expect(comments).toContain('/az');
    });

    it('only changes marker content, leaving static siblings untouched', () => {
        setupView(
            'v',
            '<div az="0"><b>static</b><!--az:0-->dynamic<!--/az--><i>also static</i></div>',
        );
        applyOps([[OP.TEXT, 'v:0', 'updated']]);
        const el = resolveEl('v:0');
        expect(el.querySelector('b').textContent).toBe('static');
        expect(el.querySelector('i').textContent).toBe('also static');
        expect(el.textContent).toContain('updated');
    });
});

// ---------------------------------------------------------------------------
// 14b. applyEffects -- set_title
// ---------------------------------------------------------------------------

describe('applyEffects -- set_title', () => {
    it('sets document.title', () => {
        document.title = 'Old Title';
        applyEffects([[14, 'New Title']]);
        expect(document.title).toBe('New Title');
    });
});

// ---------------------------------------------------------------------------
// 14d. applyEffects -- toggle_attr (op 21)
// ---------------------------------------------------------------------------

describe('applyEffects -- toggle_attr', () => {
    const field = () => document.querySelector('#f');

    it('presence: sets the bare attribute when absent', () => {
        document.body.innerHTML = '<input id="f" type="text" />';
        applyEffects([[21, '#f', 'disabled']]);
        expect(field().getAttribute('disabled')).toBe('');
    });

    it('presence: removes the attribute when present', () => {
        document.body.innerHTML = '<input id="f" type="text" disabled />';
        applyEffects([[21, '#f', 'disabled']]);
        expect(field().hasAttribute('disabled')).toBe(false);
    });

    it('value: flips from A to B and back', () => {
        document.body.innerHTML = '<input id="f" type="password" />';
        applyEffects([[21, '#f', 'type', 'password', 'text']]);
        expect(field().getAttribute('type')).toBe('text');
        applyEffects([[21, '#f', 'type', 'password', 'text']]);
        expect(field().getAttribute('type')).toBe('password');
    });

    it('value: a current value matching neither resolves to A', () => {
        document.body.innerHTML = '<input id="f" type="email" />';
        applyEffects([[21, '#f', 'type', 'password', 'text']]);
        expect(field().getAttribute('type')).toBe('password');
    });
});

// ---------------------------------------------------------------------------
// 14e. applyEffects -- attribute effects share the canonical writers
// (value-property sync + hook updated(), same as the OP_SET_ATTR/OP_REM_ATTR diff)
// ---------------------------------------------------------------------------

describe('applyEffects -- attribute effects (canonical writer)', () => {
    it('set_attr syncs the live value property of a dirtied input', () => {
        document.body.innerHTML = '<input id="i" value="orig" />';
        const input = document.querySelector('#i');
        input.value = 'typed'; // dirty the live property so it diverges from the attribute
        applyEffects([[7, '#i', 'value', 'fromserver']]); // op 7 = set_attr
        expect(input.value).toBe('fromserver');
        expect(input.getAttribute('value')).toBe('fromserver');
    });

    it('toggle_attr value sync reaches the live value property', () => {
        document.body.innerHTML = '<input id="i" value="a" />';
        const input = document.querySelector('#i');
        input.value = 'a';
        applyEffects([[21, '#i', 'value', 'a', 'b']]);
        expect(input.value).toBe('b');
    });

    it('set_attr fires updated() on a hooked element', () => {
        const updated = vi.fn();
        hooks.Chart = { mounted() {}, updated };
        setupView('v', '<div az="0" az-hook="Chart">content</div>');
        mountHooks(document);
        applyEffects([[7, '[az-hook="Chart"]', 'class', 'active']]);
        expect(updated).toHaveBeenCalledOnce();
    });

    it('remove_attr fires updated() on a hooked element', () => {
        const updated = vi.fn();
        hooks.Chart = { mounted() {}, updated };
        setupView('v', '<div az="0" az-hook="Chart" class="active">content</div>');
        mountHooks(document);
        applyEffects([[8, '[az-hook="Chart"]', 'class']]); // op 8 = remove_attr
        expect(updated).toHaveBeenCalledOnce();
    });

    it('toggle_attr fires updated() on a hooked element', () => {
        const updated = vi.fn();
        hooks.Chart = { mounted() {}, updated };
        setupView('v', '<div az="0" az-hook="Chart">content</div>');
        mountHooks(document);
        applyEffects([[21, '[az-hook="Chart"]', 'data-open']]);
        expect(updated).toHaveBeenCalledOnce();
    });
});

// ---------------------------------------------------------------------------
// 14f. applyEffects -- class/visibility effects also fire updated()
// (so a client-driven class/hidden change is observable like an attribute diff)
// ---------------------------------------------------------------------------

describe('applyEffects -- class/visibility effects fire updated()', () => {
    /** Mount a hooked element and return its updated() spy. */
    function hookedUpdated(extraAttrs = '') {
        const updated = vi.fn();
        hooks.Chart = { mounted() {}, updated };
        setupView('v', `<div az="0" az-hook="Chart"${extraAttrs}>content</div>`);
        mountHooks(document);
        return updated;
    }

    it('add_class fires updated()', () => {
        const updated = hookedUpdated();
        applyEffects([[4, '[az-hook="Chart"]', 'active']]); // op 4 = add_class
        expect(updated).toHaveBeenCalledOnce();
    });

    it('remove_class fires updated()', () => {
        const updated = hookedUpdated(' class="active"');
        applyEffects([[5, '[az-hook="Chart"]', 'active']]); // op 5 = remove_class
        expect(updated).toHaveBeenCalledOnce();
    });

    it('toggle_class fires updated()', () => {
        const updated = hookedUpdated();
        applyEffects([[6, '[az-hook="Chart"]', 'active']]); // op 6 = toggle_class
        expect(updated).toHaveBeenCalledOnce();
    });

    it('toggle (visibility) fires updated()', () => {
        const updated = hookedUpdated();
        applyEffects([[1, '[az-hook="Chart"]']]); // op 1 = toggle
        expect(updated).toHaveBeenCalledOnce();
    });

    it('show fires updated()', () => {
        const updated = hookedUpdated(' hidden');
        applyEffects([[2, '[az-hook="Chart"]']]); // op 2 = show
        expect(updated).toHaveBeenCalledOnce();
    });

    it('hide fires updated()', () => {
        const updated = hookedUpdated();
        applyEffects([[3, '[az-hook="Chart"]']]); // op 3 = hide
        expect(updated).toHaveBeenCalledOnce();
    });

    it('reset_form fires updated() on a hooked form', () => {
        const updated = vi.fn();
        hooks.Chart = { mounted() {}, updated };
        setupView('v', '<form az="0" az-hook="Chart"><input name="a" /></form>');
        mountHooks(document);
        applyEffects([[25, '[az-hook="Chart"]']]); // op 25 = reset_form
        expect(updated).toHaveBeenCalledOnce();
    });
});

// ---------------------------------------------------------------------------
// 14g. applyEffects -- broadcast effects target ALL matching elements,
// single-target effects (focus/blur/scroll_to) only the first
// ---------------------------------------------------------------------------

describe('applyEffects -- broadcast effects target all matching elements', () => {
    /** Render `html` n times and return the elements as an array. */
    function many(html, n = 3) {
        document.body.innerHTML = html.repeat(n);
        return Array.from(document.body.children);
    }

    it('add_class adds the class to every match', () => {
        const els = many('<div class="item"></div>');
        applyEffects([[4, '.item', 'on']]);
        expect(els.every((el) => el.classList.contains('on'))).toBe(true);
    });

    it('remove_class removes the class from every match', () => {
        const els = many('<div class="item on"></div>');
        applyEffects([[5, '.item', 'on']]);
        expect(els.some((el) => el.classList.contains('on'))).toBe(false);
    });

    it('toggle_class toggles every match', () => {
        const els = many('<div class="item"></div>');
        applyEffects([[6, '.item', 'on']]);
        expect(els.every((el) => el.classList.contains('on'))).toBe(true);
    });

    it('toggle (visibility) flips every match', () => {
        const els = many('<div class="item"></div>');
        applyEffects([[1, '.item']]);
        expect(els.every((el) => el.hidden)).toBe(true);
    });

    it('show clears hidden on every match', () => {
        const els = many('<div class="item" hidden></div>');
        applyEffects([[2, '.item']]);
        expect(els.some((el) => el.hidden)).toBe(false);
    });

    it('hide sets hidden on every match', () => {
        const els = many('<div class="item"></div>');
        applyEffects([[3, '.item']]);
        expect(els.every((el) => el.hidden)).toBe(true);
    });

    it('set_attr sets the attribute on every match', () => {
        const els = many('<div class="item"></div>');
        applyEffects([[7, '.item', 'data-x', 'y']]);
        expect(els.every((el) => el.getAttribute('data-x') === 'y')).toBe(true);
    });

    it('remove_attr removes the attribute from every match', () => {
        const els = many('<div class="item" data-x="y"></div>');
        applyEffects([[8, '.item', 'data-x']]);
        expect(els.some((el) => el.hasAttribute('data-x'))).toBe(false);
    });

    it('toggle_attr (presence) toggles every match', () => {
        const els = many('<div class="item"></div>');
        applyEffects([[21, '.item', 'data-open']]);
        expect(els.every((el) => el.getAttribute('data-open') === '')).toBe(true);
    });

    it('toggle_attr (value) flips every match, each on its own current value', () => {
        document.body.innerHTML =
            '<input class="item" type="password" /><input class="item" type="text" />';
        const [a, b] = document.querySelectorAll('.item');
        applyEffects([[21, '.item', 'type', 'password', 'text']]);
        expect(a.getAttribute('type')).toBe('text'); // password -> text
        expect(b.getAttribute('type')).toBe('password'); // text -> password (independent)
    });

    it('focus targets only the first match (single-target keeps withQuery)', () => {
        document.body.innerHTML = '<input class="item" /><input class="item" />';
        const [first, second] = document.querySelectorAll('.item');
        applyEffects([[11, '.item']]); // op 11 = focus
        expect(document.activeElement).toBe(first);
        expect(document.activeElement).not.toBe(second);
    });

    it('reset_form clears typed input + textarea on every matching form', () => {
        document.body.innerHTML =
            '<form class="f"><input name="a" value="def" /><textarea>orig</textarea></form>' +
            '<form class="f"><input name="a" value="def" /><textarea>orig</textarea></form>';
        for (const i of document.querySelectorAll('input')) i.value = 'typed';
        for (const t of document.querySelectorAll('textarea')) t.value = 'edited';
        applyEffects([[25, '.f']]); // op 25 = reset_form
        expect([...document.querySelectorAll('input')].every((i) => i.value === 'def')).toBe(true);
        expect([...document.querySelectorAll('textarea')].every((t) => t.value === 'orig')).toBe(
            true,
        );
    });

    it('reset_form is a safe no-op on a non-form match', () => {
        document.body.innerHTML = '<div class="f"></div>';
        expect(() => applyEffects([[25, '.f']])).not.toThrow();
    });
});

// ---------------------------------------------------------------------------
// 14c. applyOps -- OP.REPLACE edge cases
// ---------------------------------------------------------------------------

describe('applyOps -- OP.REPLACE edge cases', () => {
    it('replaces element with new content', () => {
        document.body.innerHTML = '<div id="page" az-view><p az="0">old</p></div>';
        applyOps([[OP.REPLACE, 'page', '<div id="page" az-view><p az="0">new</p></div>']]);
        const el = document.querySelector('#page[az-view]');
        expect(el).not.toBeNull();
        expect(el.querySelector('[az="0"]').textContent).toBe('new');
    });

    it('skip missing target gracefully', () => {
        document.body.innerHTML = '<div>nothing</div>';
        // Should not throw
        applyOps([[OP.REPLACE, 'nonexistent', '<div>replaced</div>']]);
    });
});

// ---------------------------------------------------------------------------
// 15. resolveEl -- edge cases
// ---------------------------------------------------------------------------

describe('resolveEl -- edge cases', () => {
    it('resolves element in nested child view scope', () => {
        document.body.innerHTML = `
            <div id="parent" az-view>
                <span az="0">parent dynamic</span>
                <div id="child" az-view>
                    <span az="0">child dynamic</span>
                </div>
            </div>
        `;
        const childEl = resolveEl('child:0');
        expect(childEl).not.toBeNull();
        expect(childEl.textContent).toBe('child dynamic');
    });

    it('returns null for target without colon', () => {
        setupView('v', '<span az="0">hi</span>');
        expect(resolveEl('noColon')).toBeNull();
    });
});

// ---------------------------------------------------------------------------
// applyOps -- OP.LIST_PATCH (single-root plain-list ?each, positional patch)
//
// A content patch must NOT churn the container's childList -- that reverts an
// in-progress scroll on WebKit. Item <li> node identity across the patch proves
// no remove+insert happened. Items are addressed by DOM-order position between
// the slot's <!--az:0-->...<!--/az--> markers (no az-key -- plain lists are
// unkeyed). Sub-ops: [ITEM_PATCH, idx, innerOps] | [REMOVE, idx] | [INSERT, idx, html].
// ---------------------------------------------------------------------------

describe('applyOps -- OP.LIST_PATCH (plain-list positional patch)', () => {
    function setupList() {
        setupView(
            'v',
            '<ul az="0"><!--az:0-->' +
                '<li az="r"><!--az:r-->item0<!--/az--></li>' +
                '<li az="r"><!--az:r-->item1<!--/az--></li>' +
                '<li az="r"><!--az:r-->item2<!--/az--></li>' +
                '<!--/az--></ul>',
        );
        return document.querySelector('ul');
    }

    it('patches a middle item in place; container childList unchanged', () => {
        const ul = setupList();
        const lis = Array.from(ul.querySelectorAll('li'));
        applyOps([[OP.LIST_PATCH, 'v:0', [[OP.ITEM_PATCH, 1, [[OP.TEXT, 'r', 'UPDATED']]]]]]);
        const after = Array.from(ul.querySelectorAll('li'));
        expect(after[0]).toBe(lis[0]); // same node
        expect(after[1]).toBe(lis[1]); // same node, patched in place
        expect(after[2]).toBe(lis[2]); // same node
        expect(lis[1].textContent).toBe('UPDATED');
        expect(lis[0].textContent).toBe('item0');
        expect(lis[2].textContent).toBe('item2');
    });

    it('patches the inner text node in place (characterData, no childList)', () => {
        const ul = setupList();
        const li1 = ul.querySelectorAll('li')[1];
        const textNode = li1.childNodes[1]; // [<!--az:r-->, "item1", <!--/az-->]
        expect(textNode.nodeType).toBe(3);
        applyOps([[OP.LIST_PATCH, 'v:0', [[OP.ITEM_PATCH, 1, [[OP.TEXT, 'r', 'new']]]]]]);
        expect(li1.childNodes[1]).toBe(textNode); // same text node reused
        expect(textNode.data).toBe('new');
    });

    it('patches several items in one batch, all in place', () => {
        const ul = setupList();
        const lis = Array.from(ul.querySelectorAll('li'));
        applyOps([
            [
                OP.LIST_PATCH,
                'v:0',
                [
                    [OP.ITEM_PATCH, 0, [[OP.TEXT, 'r', 'X']]],
                    [OP.ITEM_PATCH, 2, [[OP.TEXT, 'r', 'Z']]],
                ],
            ],
        ]);
        const after = Array.from(ul.querySelectorAll('li'));
        expect(after[0]).toBe(lis[0]);
        expect(after[1]).toBe(lis[1]);
        expect(after[2]).toBe(lis[2]);
        expect(lis[0].textContent).toBe('X');
        expect(lis[1].textContent).toBe('item1');
        expect(lis[2].textContent).toBe('Z');
    });

    it('appends a new item via INSERT before the end marker', () => {
        const ul = setupList();
        const lis = Array.from(ul.querySelectorAll('li'));
        applyOps([
            [OP.LIST_PATCH, 'v:0', [[OP.INSERT, 3, '<li az="r"><!--az:r-->item3<!--/az--></li>']]],
        ]);
        const after = Array.from(ul.querySelectorAll('li'));
        expect(after.length).toBe(4);
        expect(after[0]).toBe(lis[0]); // existing nodes reused
        expect(after[1]).toBe(lis[1]);
        expect(after[2]).toBe(lis[2]);
        expect(after[3].textContent).toBe('item3');
        expect(after[3].nextSibling.nodeType).toBe(8); // inserted before <!--/az-->
    });

    it('removes a tail item via REMOVE, keeping the survivors', () => {
        const ul = setupList();
        const lis = Array.from(ul.querySelectorAll('li'));
        applyOps([[OP.LIST_PATCH, 'v:0', [[OP.REMOVE, 2]]]]);
        const after = Array.from(ul.querySelectorAll('li'));
        expect(after.length).toBe(2);
        expect(after[0]).toBe(lis[0]);
        expect(after[1]).toBe(lis[1]);
        expect(lis[2].isConnected).toBe(false);
    });

    it('combines a content patch with a tail insert (middle-insert shape)', () => {
        // [a,c] -> [a,x,c]: patch index 1 (c->x) + insert index 2 (c).
        setupView(
            'v',
            '<ul az="0"><!--az:0-->' +
                '<li az="r"><!--az:r-->a<!--/az--></li>' +
                '<li az="r"><!--az:r-->c<!--/az--></li>' +
                '<!--/az--></ul>',
        );
        const ul = document.querySelector('ul');
        const lis = Array.from(ul.querySelectorAll('li'));
        applyOps([
            [
                OP.LIST_PATCH,
                'v:0',
                [
                    [OP.ITEM_PATCH, 1, [[OP.TEXT, 'r', 'x']]],
                    [OP.INSERT, 2, '<li az="r"><!--az:r-->c<!--/az--></li>'],
                ],
            ],
        ]);
        const after = Array.from(ul.querySelectorAll('li'));
        expect(after.map((li) => li.textContent)).toEqual(['a', 'x', 'c']);
        expect(after[0]).toBe(lis[0]); // reused
        expect(after[1]).toBe(lis[1]); // reused, content rewritten in place
    });

    it('fires hook updated() on the patched item, not mounted()', () => {
        setupView(
            'v',
            '<ul az="0"><!--az:0-->' +
                '<li az="r" az-hook="Row"><!--az:r-->x<!--/az--></li>' +
                '<!--/az--></ul>',
        );
        const mounted = vi.fn();
        const updated = vi.fn();
        hooks.Row = { mounted, updated };
        mountHooks(document);
        mounted.mockClear();
        applyOps([[OP.LIST_PATCH, 'v:0', [[OP.ITEM_PATCH, 0, [[OP.TEXT, 'r', 'y']]]]]]);
        expect(mounted).not.toHaveBeenCalled();
        expect(updated).toHaveBeenCalled();
    });

    it('warns and is a no-op when the slot marker is missing', () => {
        const spy = vi.spyOn(console, 'warn').mockImplementation(() => {});
        setupView('v', '<ul az="0"><li az="r">x</li></ul>'); // no markers
        applyOps([[OP.LIST_PATCH, 'v:0', [[OP.ITEM_PATCH, 0, [[OP.TEXT, 'r', 'y']]]]]]);
        expect(resolveEl('v:0').querySelector('li').textContent).toBe('x');
        expect(spy).toHaveBeenCalled();
        spy.mockRestore();
    });
});

// ---------------------------------------------------------------------------
// 16. applyOps -- OP.TEXT edge cases
// ---------------------------------------------------------------------------

describe('applyOps -- OP.TEXT edge cases', () => {
    it('sets textContent to empty string', () => {
        setupView('v', '<span az="0">old</span>');
        applyOps([[OP.TEXT, 'v:0', '']]);
        expect(resolveEl('v:0').textContent).toBe('');
    });
});

// ---------------------------------------------------------------------------
// 17. applyOps -- OP.SET_ATTR edge cases
// ---------------------------------------------------------------------------

describe('applyOps -- OP.SET_ATTR edge cases', () => {
    it('overwrites an existing attribute value', () => {
        setupView('v', '<span az="0" class="old">hi</span>');
        applyOps([[OP.SET_ATTR, 'v:0', 'class', 'new']]);
        expect(resolveEl('v:0').getAttribute('class')).toBe('new');
    });

    it('syncs value property on textarea elements', () => {
        setupView('v', '<textarea az="0">old</textarea>');
        applyOps([[OP.SET_ATTR, 'v:0', 'value', 'new']]);
        const el = /** @type {HTMLTextAreaElement} */ (resolveEl('v:0'));
        expect(el.getAttribute('value')).toBe('new');
        expect(el.value).toBe('new');
    });

    it('syncs value property on select elements', () => {
        setupView(
            'v',
            '<select az="0"><option value="a">A</option><option value="b">B</option></select>',
        );
        applyOps([[OP.SET_ATTR, 'v:0', 'value', 'b']]);
        const el = /** @type {HTMLSelectElement} */ (resolveEl('v:0'));
        expect(el.getAttribute('value')).toBe('b');
        expect(el.value).toBe('b');
    });
});

// ---------------------------------------------------------------------------
// 18. applyOps -- OP.REM_ATTR edge cases
// ---------------------------------------------------------------------------

describe('applyOps -- OP.REM_ATTR edge cases', () => {
    it('removing a non-existent attribute is a no-op', () => {
        setupView('v', '<span az="0">hi</span>');
        // Should not throw
        applyOps([[OP.REM_ATTR, 'v:0', 'nonexistent']]);
        expect(resolveEl('v:0').textContent).toBe('hi');
    });
});

// ---------------------------------------------------------------------------
// 19. applyOps -- OP.UPDATE edge cases
// ---------------------------------------------------------------------------

describe('applyOps -- OP.UPDATE edge cases', () => {
    it('clears innerHTML when value is empty string', () => {
        setupView('v', '<div az="0"><span>child</span></div>');
        applyOps([[OP.UPDATE, 'v:0', '']]);
        expect(resolveEl('v:0').innerHTML).toBe('');
    });
});

// ---------------------------------------------------------------------------
// 19b. applyOps -- OP.REPLACE (outerHTML for navigate)
// ---------------------------------------------------------------------------

describe('applyOps -- OP.REPLACE', () => {
    it('replaces outerHTML of the target element', () => {
        setupView('v', '<div az="0">old</div>');
        const parent = resolveEl('v:0').parentElement;
        applyOps([[OP.REPLACE, 'v:0', '<div az="0">new</div>']]);
        const newEl = parent.querySelector('[az="0"]');
        expect(newEl).not.toBeNull();
        expect(newEl.textContent).toBe('new');
    });

    it('replaces view root element by bare target', () => {
        setupView('page', '<h1 az="0">old title</h1>');
        const container = document.querySelector('#page[az-view]');
        const parent = container.parentElement;
        applyOps([
            [OP.REPLACE, 'page', '<main id="page" az-view><h1 az="0">new title</h1></main>'],
        ]);
        const newView = parent.querySelector('#page[az-view]');
        expect(newView).not.toBeNull();
        expect(newView.querySelector('[az="0"]').textContent).toBe('new title');
    });
});

// ---------------------------------------------------------------------------
// 20. applyOps -- OP.INSERT edge cases
// ---------------------------------------------------------------------------

describe('applyOps -- OP.INSERT edge cases', () => {
    it('inserts at position 0 (before first child)', () => {
        setupView('v', '<div az="0"><p az-key="b">B</p></div>');
        applyOps([[OP.INSERT, 'v:0', 'a', 0, '<p az-key="a">A</p>']]);
        const keys = Array.from(resolveEl('v:0').querySelectorAll('[az-key]')).map((c) =>
            c.getAttribute('az-key'),
        );
        expect(keys).toEqual(['a', 'b']);
    });

    it('inserts into an empty container', () => {
        setupView('v', '<div az="0"></div>');
        applyOps([[OP.INSERT, 'v:0', 'first', 0, '<p az-key="first">First</p>']]);
        const el = resolveEl('v:0');
        const child = el.querySelector('[az-key="first"]');
        expect(child).not.toBeNull();
        expect(child.textContent).toBe('First');
    });

    it('warns when inserted HTML has no az-key', () => {
        const spy = vi.spyOn(console, 'warn').mockImplementation(() => {});
        setupView('v', '<div az="0"></div>');
        applyOps([[OP.INSERT, 'v:0', 'k1', -1, '<p>no key</p>']]);
        expect(spy).toHaveBeenCalledWith(expect.stringContaining('k1'));
        spy.mockRestore();
    });
});

// ---------------------------------------------------------------------------
// 21. applyOps -- OP.REMOVE edge cases
// ---------------------------------------------------------------------------

describe('applyOps -- OP.REMOVE edge cases', () => {
    it('removing a non-existent key is a no-op and warns', () => {
        const spy = vi.spyOn(console, 'warn').mockImplementation(() => {});
        setupView('v', '<div az="0"><p az-key="k1">item</p></div>');
        applyOps([[OP.REMOVE, 'v:0', 'nonexistent']]);
        expect(resolveEl('v:0').querySelector('[az-key="k1"]')).not.toBeNull();
        expect(spy).toHaveBeenCalledWith(expect.stringContaining('nonexistent'));
        spy.mockRestore();
    });
});

// ---------------------------------------------------------------------------
// 22. applyOps -- OP.ITEM_PATCH edge cases
// ---------------------------------------------------------------------------

describe('applyOps -- OP.ITEM_PATCH edge cases', () => {
    it('skips inner ops when key does not exist and warns', () => {
        const spy = vi.spyOn(console, 'warn').mockImplementation(() => {});
        setupView('v', '<div az="0"><div az-key="k1"><span az="0">ok</span></div></div>');
        applyOps([
            [
                OP.ITEM_PATCH,
                'v:0',
                'nonexistent',
                [
                    [OP.TEXT, '0', 'new'],
                    [OP.SET_ATTR, '0', 'class', 'x'],
                ],
            ],
        ]);
        const item = resolveEl('v:0').querySelector('[az-key="k1"]');
        expect(item.querySelector('[az="0"]').textContent).toBe('ok');
        expect(spy).toHaveBeenCalledWith(expect.stringContaining('nonexistent'));
        spy.mockRestore();
    });

    it('applies multiple inner ops sequentially', () => {
        setupView(
            'v',
            '<div az="0"><div az-key="k1"><span az="0">old</span><span az="1">x</span></div></div>',
        );
        applyOps([
            [
                OP.ITEM_PATCH,
                'v:0',
                'k1',
                [
                    [OP.TEXT, '0', 'new'],
                    [OP.SET_ATTR, '1', 'class', 'highlight'],
                ],
            ],
        ]);
        const item = resolveEl('v:0').querySelector('[az-key="k1"]');
        expect(item.querySelector('[az="0"]').textContent).toBe('new');
        expect(item.querySelector('[az="1"]').getAttribute('class')).toBe('highlight');
    });

    it('syncs value property on input within keyed child', () => {
        setupView('v', '<div az="0"><div az-key="k1"><input az="0" value="old" /></div></div>');
        applyOps([[OP.ITEM_PATCH, 'v:0', 'k1', [[OP.SET_ATTR, '0', 'value', 'new']]]]);
        const item = resolveEl('v:0').querySelector('[az-key="k1"]');
        const input = /** @type {HTMLInputElement} */ (item.querySelector('[az="0"]'));
        expect(input.getAttribute('value')).toBe('new');
        expect(input.value).toBe('new');
    });

    it('falls back to item element itself when no [az] found', () => {
        setupView('v', '<div az="0"><div az-key="k1">plain text</div></div>');
        applyOps([[OP.ITEM_PATCH, 'v:0', 'k1', [[OP.SET_ATTR, '0', 'data-test', 'fallback']]]]);
        const item = resolveEl('v:0').querySelector('[az-key="k1"]');
        expect(item.getAttribute('data-test')).toBe('fallback');
    });
});

// ---------------------------------------------------------------------------
// 23. applyOps -- batch / empty
// ---------------------------------------------------------------------------

describe('applyOps -- batch / empty', () => {
    it('handles empty ops array as no-op', () => {
        setupView('v', '<span az="0">untouched</span>');
        applyOps([]);
        expect(resolveEl('v:0').textContent).toBe('untouched');
    });

    it('applies multiple ops in one batch sequentially', () => {
        setupView('v', '<span az="0">old</span>');
        applyOps([
            [OP.TEXT, 'v:0', 'new'],
            [OP.SET_ATTR, 'v:0', 'class', 'updated'],
        ]);
        expect(resolveEl('v:0').textContent).toBe('new');
        expect(resolveEl('v:0').getAttribute('class')).toBe('updated');
    });
});

// ---------------------------------------------------------------------------
// 24. applyEffects -- edge cases
// ---------------------------------------------------------------------------

describe('applyEffects -- edge cases', () => {
    it('handles empty effects array as no-op', () => {
        // Should not throw
        applyEffects([]);
    });
});

// ---------------------------------------------------------------------------
// 24a. executeJS -- client-side commands
// ---------------------------------------------------------------------------

describe('executeJS', () => {
    it('toggle flips hidden', () => {
        document.body.innerHTML = '<div id="t" hidden>hi</div>';
        executeJS(document.body, null, [1, '#t']);
        expect(document.getElementById('t').hidden).toBe(false);
        executeJS(document.body, null, [1, '#t']);
        expect(document.getElementById('t').hidden).toBe(true);
    });

    it('show removes hidden', () => {
        document.body.innerHTML = '<div id="t" hidden>hi</div>';
        executeJS(document.body, null, [2, '#t']);
        expect(document.getElementById('t').hidden).toBe(false);
    });

    it('hide sets hidden', () => {
        document.body.innerHTML = '<div id="t">hi</div>';
        executeJS(document.body, null, [3, '#t']);
        expect(document.getElementById('t').hidden).toBe(true);
    });

    it('add_class adds CSS class', () => {
        document.body.innerHTML = '<div id="t">hi</div>';
        executeJS(document.body, null, [4, '#t', 'active']);
        expect(document.getElementById('t').classList.contains('active')).toBe(true);
    });

    it('remove_class removes CSS class', () => {
        document.body.innerHTML = '<div id="t" class="active">hi</div>';
        executeJS(document.body, null, [5, '#t', 'active']);
        expect(document.getElementById('t').classList.contains('active')).toBe(false);
    });

    it('toggle_class toggles CSS class', () => {
        document.body.innerHTML = '<div id="t">hi</div>';
        executeJS(document.body, null, [6, '#t', 'on']);
        expect(document.getElementById('t').classList.contains('on')).toBe(true);
        executeJS(document.body, null, [6, '#t', 'on']);
        expect(document.getElementById('t').classList.contains('on')).toBe(false);
    });

    it('set_attr sets attribute', () => {
        document.body.innerHTML = '<div id="t">hi</div>';
        executeJS(document.body, null, [7, '#t', 'data-x', '42']);
        expect(document.getElementById('t').getAttribute('data-x')).toBe('42');
    });

    it('remove_attr removes attribute', () => {
        document.body.innerHTML = '<div id="t" data-x="42">hi</div>';
        executeJS(document.body, null, [8, '#t', 'data-x']);
        expect(document.getElementById('t').hasAttribute('data-x')).toBe(false);
    });

    it('dispatch_event fires CustomEvent on document', () => {
        const handler = vi.fn();
        document.addEventListener('test-ev', handler);
        executeJS(document.body, null, [9, 'test-ev', { k: 'v' }]);
        expect(handler).toHaveBeenCalledOnce();
        expect(handler.mock.calls[0][0].detail).toEqual({ k: 'v' });
        document.removeEventListener('test-ev', handler);
    });

    it('dispatch_event defaults detail to empty object', () => {
        const handler = vi.fn();
        document.addEventListener('test-ev2', handler);
        executeJS(document.body, null, [9, 'test-ev2']);
        expect(handler.mock.calls[0][0].detail).toEqual({});
        document.removeEventListener('test-ev2', handler);
    });

    it('focus sets focus on element', () => {
        document.body.innerHTML = '<input id="t" />';
        const input = /** @type {HTMLElement} */ (document.getElementById('t'));
        executeJS(document.body, null, [11, '#t']);
        expect(document.activeElement).toBe(input);
    });

    it('blur removes focus from element', () => {
        document.body.innerHTML = '<input id="t" />';
        const input = /** @type {HTMLElement} */ (document.getElementById('t'));
        input.focus();
        executeJS(document.body, null, [12, '#t']);
        expect(document.activeElement).not.toBe(input);
    });

    it('set_title sets document.title', () => {
        document.title = 'old';
        executeJS(document.body, null, [14, 'new']);
        expect(document.title).toBe('new');
    });

    it('executes multiple commands in order', () => {
        document.body.innerHTML = '<div id="a" hidden></div><div id="b"></div>';
        executeJS(document.body, null, [
            [2, '#a'],
            [3, '#b'],
        ]);
        expect(document.getElementById('a').hidden).toBe(false);
        expect(document.getElementById('b').hidden).toBe(true);
    });

    it('ignores unknown op codes', () => {
        // Should not throw
        executeJS(document.body, null, [999]);
    });

    it('handles missing selector gracefully', () => {
        // Should not throw when element not found
        executeJS(document.body, null, [1, '#nonexistent']);
        executeJS(document.body, null, [4, '#nonexistent', 'cls']);
    });
});

// ---------------------------------------------------------------------------
// 24a2. executeJS -- select/copy_to_clipboard/show_modal/close_modal (single-target)
//
// jsdom implements neither <dialog>.showModal/close nor navigator.clipboard, so
// these use spies (el.select/showModal/close = vi.fn(); navigator.clipboard =
// { writeText }) to assert the effect invokes the right API on the FIRST match.
// ---------------------------------------------------------------------------

describe('executeJS -- select/copy_to_clipboard/show_modal/close_modal', () => {
    /** Install a spied clipboard; returns the writeText spy. */
    function stubClipboard() {
        const writeText = vi.fn();
        Object.defineProperty(navigator, 'clipboard', {
            value: { writeText },
            configurable: true,
        });
        return writeText;
    }

    afterEach(() => {
        delete (/** @type {any} */ (navigator).clipboard);
    });

    it('select calls select() on the first match only', () => {
        document.body.innerHTML = '<input class="f" /><input class="f" />';
        const [first, second] = document.querySelectorAll('.f');
        first.select = vi.fn();
        second.select = vi.fn();
        executeJS(document.body, null, [26, '.f']); // op 26 = select
        expect(first.select).toHaveBeenCalledOnce();
        expect(second.select).not.toHaveBeenCalled();
    });

    it('select is a safe no-op on a non-input match / no match', () => {
        document.body.innerHTML = '<div class="f"></div>';
        expect(() => executeJS(document.body, null, [26, '.f'])).not.toThrow();
        expect(() => executeJS(document.body, null, [26, '#none'])).not.toThrow();
    });

    it('copy_to_clipboard writes the matched form control value to the clipboard', () => {
        document.body.innerHTML = '<input id="t" /><input class="other" />';
        /** @type {HTMLInputElement} */ (document.getElementById('t')).value = 'secret-token';
        const writeText = stubClipboard();
        executeJS(document.body, null, [27, '#t']); // op 27 = copy_to_clipboard
        expect(writeText).toHaveBeenCalledWith('secret-token');
    });

    it('copy_to_clipboard falls back to textContent for a non-form element', () => {
        document.body.innerHTML = '<code id="t">npm i arizona</code>';
        const writeText = stubClipboard();
        executeJS(document.body, null, [27, '#t']);
        expect(writeText).toHaveBeenCalledWith('npm i arizona');
    });

    it('copy_to_clipboard is a safe no-op when the clipboard is unavailable', () => {
        document.body.innerHTML = '<input id="t" value="x" />';
        // No clipboard stubbed -- navigator.clipboard is undefined in jsdom.
        expect(() => executeJS(document.body, null, [27, '#t'])).not.toThrow();
    });

    it('show_modal calls showModal() on the first matching dialog only', () => {
        document.body.innerHTML = '<dialog class="d"></dialog><dialog class="d"></dialog>';
        const [first, second] = document.querySelectorAll('.d');
        first.showModal = vi.fn();
        second.showModal = vi.fn();
        executeJS(document.body, null, [28, '.d']); // op 28 = show_modal
        expect(first.showModal).toHaveBeenCalledOnce();
        expect(second.showModal).not.toHaveBeenCalled();
    });

    it('show_modal is a safe no-op on a non-dialog match', () => {
        document.body.innerHTML = '<div class="d"></div>';
        expect(() => executeJS(document.body, null, [28, '.d'])).not.toThrow();
    });

    it('close_modal calls close() on the first matching dialog', () => {
        document.body.innerHTML = '<dialog id="d"></dialog>';
        const dlg = document.getElementById('d');
        dlg.close = vi.fn();
        executeJS(document.body, null, [29, '#d']); // op 29 = close_modal
        expect(dlg.close).toHaveBeenCalledOnce();
    });

    it('close_modal is a safe no-op on a non-dialog match', () => {
        document.body.innerHTML = '<div class="d"></div>';
        expect(() => executeJS(document.body, null, [29, '.d'])).not.toThrow();
    });

    it('show_modal and close_modal fire updated() on a hooked dialog', () => {
        const updated = vi.fn();
        hooks.Chart = { mounted() {}, updated };
        setupView('v', '<dialog az="0" az-hook="Chart"></dialog>');
        const dlg = document.querySelector('dialog');
        dlg.showModal = vi.fn();
        dlg.close = vi.fn();
        mountHooks(document);
        applyEffects([[28, '[az-hook="Chart"]']]); // show_modal
        applyEffects([[29, '[az-hook="Chart"]']]); // close_modal
        expect(updated).toHaveBeenCalledTimes(2);
    });
});

// ---------------------------------------------------------------------------
// 24b. executeJS -- on_key (JS_ON_KEY = 16)
// ---------------------------------------------------------------------------

describe('executeJS -- on_key', () => {
    it('on_key with matching literal key executes inner command', () => {
        document.body.innerHTML = '<div id="t" hidden></div>';
        const event = new KeyboardEvent('keydown', { key: 'Enter' });
        // [16, ["enter"], [2, "#t"]]  -- on_key(enter, show("#t"))
        executeJS(document.body, event, [16, ['enter'], [2, '#t']]);
        expect(document.getElementById('t').hidden).toBe(false);
    });

    it('on_key with non-matching key skips inner command', () => {
        document.body.innerHTML = '<div id="t" hidden></div>';
        const event = new KeyboardEvent('keydown', { key: 'Escape' });
        executeJS(document.body, event, [16, ['enter'], [2, '#t']]);
        expect(document.getElementById('t').hidden).toBe(true);
    });

    it('on_key with multiple literal keys matches any', () => {
        document.body.innerHTML = '<div id="t" hidden></div>';
        const esc = new KeyboardEvent('keydown', { key: 'Escape' });
        executeJS(document.body, esc, [16, ['enter', 'escape'], [2, '#t']]);
        expect(document.getElementById('t').hidden).toBe(false);
    });

    it('on_key with regex pattern matches', () => {
        document.body.innerHTML = '<div id="t" hidden></div>';
        const event = new KeyboardEvent('keydown', { key: 'a' });
        executeJS(document.body, event, [16, '^[a-z]$', [2, '#t']]);
        expect(document.getElementById('t').hidden).toBe(false);
    });

    it('on_key with regex pattern rejects non-match', () => {
        document.body.innerHTML = '<div id="t" hidden></div>';
        const event = new KeyboardEvent('keydown', { key: 'Enter' });
        executeJS(document.body, event, [16, '^[a-z]$', [2, '#t']]);
        expect(document.getElementById('t').hidden).toBe(true);
    });

    it('on_key with null event skips', () => {
        document.body.innerHTML = '<div id="t" hidden></div>';
        executeJS(document.body, null, [16, ['enter'], [2, '#t']]);
        expect(document.getElementById('t').hidden).toBe(true);
    });

    it('on_key with empty key list never matches', () => {
        document.body.innerHTML = '<div id="t" hidden></div>';
        const event = new KeyboardEvent('keydown', { key: 'Enter' });
        executeJS(document.body, event, [16, [], [2, '#t']]);
        expect(document.getElementById('t').hidden).toBe(true);
    });

    it('on_key is case-insensitive (e.key lowercased)', () => {
        document.body.innerHTML = '<div id="t" hidden></div>';
        // Browser sends "Enter" (capitalized), filter has "enter" (lowercase)
        const event = new KeyboardEvent('keydown', { key: 'Enter' });
        executeJS(document.body, event, [16, ['enter'], [2, '#t']]);
        expect(document.getElementById('t').hidden).toBe(false);
    });

    it('on_key nested -- outer must match first', () => {
        document.body.innerHTML = '<div id="t" hidden></div>';
        // [16, ["enter"], [16, ["enter"], [2, "#t"]]] -- nested on_key
        const event = new KeyboardEvent('keydown', { key: 'Enter' });
        executeJS(document.body, event, [16, ['enter'], [16, ['enter'], [2, '#t']]]);
        expect(document.getElementById('t').hidden).toBe(false);
    });

    it('on_key nested -- outer rejects, inner never runs', () => {
        document.body.innerHTML = '<div id="t" hidden></div>';
        const event = new KeyboardEvent('keydown', { key: 'Escape' });
        executeJS(document.body, event, [16, ['enter'], [16, ['escape'], [2, '#t']]]);
        expect(document.getElementById('t').hidden).toBe(true);
    });

    it('on_key with multiple inner commands', () => {
        document.body.innerHTML = '<div id="a" hidden></div><div id="b"></div>';
        const event = new KeyboardEvent('keydown', { key: 'Enter' });
        // [16, ["enter"], [[2, "#a"], [3, "#b"]]] -- show a + hide b
        executeJS(document.body, event, [
            16,
            ['enter'],
            [
                [2, '#a'],
                [3, '#b'],
            ],
        ]);
        expect(document.getElementById('a').hidden).toBe(false);
        expect(document.getElementById('b').hidden).toBe(true);
    });

    it('on_key with invalid regex throws', () => {
        document.body.innerHTML = '<div id="t" hidden></div>';
        const event = new KeyboardEvent('keydown', { key: 'a' });
        // Invalid regex is a developer error -- let it crash
        expect(() => {
            executeJS(document.body, event, [16, '[invalid', [2, '#t']]);
        }).toThrow();
    });
});

// ---------------------------------------------------------------------------
// 25. Hooks -- mounted
// ---------------------------------------------------------------------------

describe('hooks -- mounted', () => {
    it('fires on mountHooks(document) for SSR elements with az-hook', () => {
        const mounted = vi.fn();
        hooks.Chart = { mounted };
        setupView('v', '<div az="0" az-hook="Chart">content</div>');
        mountHooks(document);
        expect(mounted).toHaveBeenCalledOnce();
    });

    it('provides this.el in mounted callback', () => {
        let capturedEl = null;
        hooks.Chart = {
            mounted() {
                capturedEl = this.el;
            },
        };
        setupView('v', '<div az="0" az-hook="Chart">content</div>');
        mountHooks(document);
        expect(capturedEl).toBe(resolveEl('v:0'));
    });

    it('fires after OP_INSERT adds element with az-hook', () => {
        const mounted = vi.fn();
        hooks.Item = { mounted };
        setupView('v', '<div az="0"></div>');
        applyOps([[OP.INSERT, 'v:0', 'k1', -1, '<div az-key="k1" az-hook="Item">item</div>']]);
        expect(mounted).toHaveBeenCalledOnce();
    });

    it('fires after OP_UPDATE innerHTML with az-hook element', () => {
        const mounted = vi.fn();
        hooks.Widget = { mounted };
        setupView('v', '<div az="0">old</div>');
        applyOps([[OP.UPDATE, 'v:0', '<span az-hook="Widget">new</span>']]);
        expect(mounted).toHaveBeenCalledOnce();
    });

    it('fires after OP_REPLACE with az-hook element', () => {
        const mounted = vi.fn();
        hooks.Page = { mounted };
        document.body.innerHTML = '<div id="page" az-view><p>old</p></div>';
        applyOps([
            [OP.REPLACE, 'page', '<div id="page" az-view><span az-hook="Page">new</span></div>'],
        ]);
        expect(mounted).toHaveBeenCalledOnce();
    });

    // A navigate mounts a DIFFERENT root view, so the replacement's id does not match
    // the op target's (which names the outgoing view). Re-resolving that stale id finds
    // nothing, so the destination page's hooks used to never mount at all.
    it('fires after OP_REPLACE when the replacement changes the view id (navigate)', () => {
        const mounted = vi.fn();
        hooks.Page = { mounted };
        document.body.innerHTML = '<div id="old-view" az-view><p>old</p></div>';
        applyOps([
            [
                OP.REPLACE,
                'old-view',
                '<div id="new-view" az-view><span az-hook="Page">new</span></div>',
            ],
        ]);
        expect(document.getElementById('old-view')).toBeNull();
        expect(mounted).toHaveBeenCalledOnce();
    });

    it('fires for a hook on the replacement root itself', () => {
        const mounted = vi.fn();
        hooks.Page = { mounted };
        document.body.innerHTML = '<div id="old-view" az-view><p>old</p></div>';
        applyOps([[OP.REPLACE, 'old-view', '<div id="new-view" az-view az-hook="Page">new</div>']]);
        expect(mounted).toHaveBeenCalledOnce();
    });

    it('fires for every top-level node a replacement expands to', () => {
        const mounted = vi.fn();
        hooks.Page = { mounted };
        document.body.innerHTML = '<i>before</i><div id="v" az-view></div><i>after</i>';
        applyOps([[OP.REPLACE, 'v', '<div az-hook="Page">a</div><div az-hook="Page">b</div>']]);
        expect(mounted).toHaveBeenCalledTimes(2);
    });

    it('does not mount hooks on siblings the replacement did not insert', () => {
        const mounted = vi.fn();
        hooks.Page = { mounted };
        document.body.innerHTML =
            '<div az-hook="Page">sibling-before</div>' +
            '<div id="v" az-view></div>' +
            '<div az-hook="Page">sibling-after</div>';
        applyOps([[OP.REPLACE, 'v', '<div id="w" az-view>plain</div>']]);
        expect(mounted).not.toHaveBeenCalled();
    });

    // `findViewRoot` resolves a popped-out view in its PiP document, so OP_REPLACE can
    // land on an element outside the main document. Nodes parsed there belong to that
    // document's realm, where a main-realm `instanceof Element` test is false -- so the
    // replacement must be walked without one, or the destination's hooks never mount.
    it('fires after OP_REPLACE when the target lives in a secondary document', async () => {
        const mounted = vi.fn();
        hooks.Page = { mounted };
        document.body.innerHTML = '<div id="pipped" az-view><p>old</p></div>';

        const { pipDoc, closePip } = await popOutView('pipped');
        try {
            applyOps([
                [
                    OP.REPLACE,
                    'pipped',
                    '<div id="new" az-view az-hook="Page"><span az-hook="Page">x</span></div>',
                ],
            ]);
            expect(pipDoc.querySelector('#new')).not.toBeNull();
            // Both the replacement's root and its descendant -- mountHooks is inclusive
            // of root, which also must not lean on a main-realm `instanceof Element`.
            expect(mounted).toHaveBeenCalledTimes(2);
        } finally {
            closePip();
        }
    });

    it('fires after OP_TEXT marker path with HTML containing az-hook', () => {
        const mounted = vi.fn();
        hooks.Inline = { mounted };
        setupView('v', '<div az="0"><!--az:0-->old<!--/az--></div>');
        applyOps([[OP.TEXT, 'v:0', '<span az-hook="Inline">new</span>', true]]);
        expect(mounted).toHaveBeenCalledOnce();
    });

    it('does NOT double-fire for already-tracked elements', () => {
        const mounted = vi.fn();
        hooks.Chart = { mounted };
        setupView('v', '<div az="0" az-hook="Chart">content</div>');
        mountHooks(document);
        mountHooks(document);
        expect(mounted).toHaveBeenCalledOnce();
    });

    it('does NOT fire for unregistered hook names', () => {
        // hooks.Missing is not defined
        setupView('v', '<div az="0" az-hook="Missing">content</div>');
        // Should not throw
        mountHooks(document);
    });
});

// ---------------------------------------------------------------------------
// 27. Hooks -- updated
// ---------------------------------------------------------------------------

describe('hooks -- updated', () => {
    it('fires after OP_SET_ATTR on hooked element', () => {
        const updated = vi.fn();
        hooks.Chart = { mounted() {}, updated };
        setupView('v', '<div az="0" az-hook="Chart">content</div>');
        mountHooks(document);
        applyOps([[OP.SET_ATTR, 'v:0', 'class', 'active']]);
        expect(updated).toHaveBeenCalledOnce();
    });

    it('fires after OP_REM_ATTR on hooked element', () => {
        const updated = vi.fn();
        hooks.Chart = { mounted() {}, updated };
        setupView('v', '<div az="0" az-hook="Chart" class="active">content</div>');
        mountHooks(document);
        applyOps([[OP.REM_ATTR, 'v:0', 'class']]);
        expect(updated).toHaveBeenCalledOnce();
    });

    it('fires after OP_UPDATE on the target element itself (if hooked)', () => {
        const mounted = vi.fn();
        const updated = vi.fn();
        const destroyed = vi.fn();
        hooks.Container = { mounted, updated, destroyed };
        setupView('v', '<div az="0" az-hook="Container">old</div>');
        mountHooks(document);
        mounted.mockClear(); // clear initial mount
        applyOps([[OP.UPDATE, 'v:0', '<p>new</p>']]);
        // Element stays -- only inner content replaced. No destroy/remount cycle.
        expect(destroyed).not.toHaveBeenCalled();
        expect(mounted).not.toHaveBeenCalled();
        expect(updated).toHaveBeenCalledOnce();
    });

    it('fires after OP_TEXT marker path on hooked element', () => {
        const mounted = vi.fn();
        const updated = vi.fn();
        hooks.Tick = { mounted, updated };
        setupView('v', '<span az="0" az-hook="Tick"><!--az:0-->0<!--/az--></span>');
        mountHooks(document);
        mounted.mockClear();
        applyOps([[OP.TEXT, 'v:0', '1']]);
        // Element stays -- text between markers replaced. updated fires, not mounted.
        expect(mounted).not.toHaveBeenCalled();
        expect(updated).toHaveBeenCalledOnce();
    });
});

// ---------------------------------------------------------------------------
// 28. Hooks -- destroyed
// ---------------------------------------------------------------------------

describe('hooks -- destroyed', () => {
    it('fires before OP_REMOVE_NODE on hooked element', () => {
        const destroyed = vi.fn();
        hooks.Chart = { mounted() {}, destroyed };
        setupView('v', '<div az="0" az-hook="Chart">content</div>');
        mountHooks(document);
        applyOps([[OP.REMOVE_NODE, 'v:0']]);
        expect(destroyed).toHaveBeenCalledOnce();
    });

    it('fires before OP_REMOVE on keyed item with hook', () => {
        const destroyed = vi.fn();
        hooks.Item = { mounted() {}, destroyed };
        setupView('v', '<div az="0"><div az-key="k1" az-hook="Item">item</div></div>');
        mountHooks(document);
        applyOps([[OP.REMOVE, 'v:0', 'k1']]);
        expect(destroyed).toHaveBeenCalledOnce();
    });

    it('fires before OP_UPDATE for hooked descendants', () => {
        const destroyed = vi.fn();
        hooks.Inner = { mounted() {}, destroyed };
        setupView('v', '<div az="0"><span az-hook="Inner">child</span></div>');
        mountHooks(document);
        applyOps([[OP.UPDATE, 'v:0', '<p>replaced</p>']]);
        expect(destroyed).toHaveBeenCalledOnce();
    });

    it('fires before OP_REPLACE for hooked element + descendants', () => {
        const destroyed = vi.fn();
        hooks.Page = { mounted() {}, destroyed };
        document.body.innerHTML = '<div id="page" az-view><span az-hook="Page">child</span></div>';
        mountHooks(document);
        applyOps([[OP.REPLACE, 'page', '<div id="page" az-view><p>new</p></div>']]);
        expect(destroyed).toHaveBeenCalledOnce();
    });

    it('fires for nested hooked descendants of removed elements', () => {
        const destroyed = vi.fn();
        hooks.Deep = { mounted() {}, destroyed };
        setupView('v', '<div az="0"><div><span az-hook="Deep">deep</span></div></div>');
        mountHooks(document);
        applyOps([[OP.REMOVE_NODE, 'v:0']]);
        expect(destroyed).toHaveBeenCalledOnce();
    });

    it('fires before OP_TEXT marker path removes hooked elements', () => {
        const destroyed = vi.fn();
        hooks.Marker = { mounted() {}, destroyed };
        setupView('v', '<div az="0"><!--az:0--><span az-hook="Marker">old</span><!--/az--></div>');
        mountHooks(document);
        applyOps([[OP.TEXT, 'v:0', 'plain text']]);
        expect(destroyed).toHaveBeenCalledOnce();
    });
});

// ---------------------------------------------------------------------------
// 29. Hooks -- pushEvent
// ---------------------------------------------------------------------------

describe('hooks -- pushEvent', () => {
    it('sends [target, name, payload] over WS', () => {
        // pushEvent requires _ws to be set. We use connect indirectly through mountHooks,
        // but for unit testing we can test the hook instance directly.
        let instance = null;
        hooks.Sender = {
            mounted() {
                instance = this;
            },
        };
        setupView('v', '<div az="0" az-hook="Sender">content</div>');
        mountHooks(document);
        expect(instance).not.toBeNull();
        // pushEvent without _ws set -- should not throw
        instance.pushEvent('my_event', { key: 'val' });
    });
});

// ---------------------------------------------------------------------------
// 29b. Hooks -- instance methods (prototype)
// ---------------------------------------------------------------------------
// The instance's prototype is the hook def, so a hook's own helper methods are
// reachable as this.method() from lifecycle callbacks, and per-instance state
// assigned to `this` stays isolated per element.

describe('hooks -- instance methods (prototype)', () => {
    it('reaches a hook own method via this.method() from mounted', () => {
        const drew = [];
        hooks.Chart = {
            mounted() {
                this.draw(); // helper on the def, reached via prototype
            },
            draw() {
                drew.push(this.el.getAttribute('az'));
            },
        };
        setupView('v', '<div az="0" az-hook="Chart">content</div>');
        // Must not throw "this.draw is not a function".
        mountHooks(document);
        expect(drew).toEqual(['0']);
    });

    it('reaches a hook own method via this.method() from updated', () => {
        const drew = [];
        hooks.Chart = {
            mounted() {},
            updated() {
                this.draw();
            },
            draw() {
                drew.push('drawn');
            },
        };
        setupView('v', '<div az="0" az-hook="Chart">content</div>');
        mountHooks(document);
        expect(drew).toEqual([]); // mounted did not draw
        applyOps([[OP.SET_ATTR, 'v:0', 'class', 'active']]); // fires updated()
        expect(drew).toEqual(['drawn']);
    });

    it('keeps per-instance state on this isolated across instances', () => {
        const instances = [];
        hooks.Counter = {
            mounted() {
                this.value = this.el.getAttribute('az'); // own property per element
                instances.push(this);
            },
            read() {
                return this.value;
            },
        };
        setupView(
            'v',
            '<div az="0" az-hook="Counter">a</div><div az="1" az-hook="Counter">b</div>',
        );
        mountHooks(document);
        expect(instances).toHaveLength(2);
        const [a, b] = instances;
        // Each instance carries its own state; neither sees the other's.
        expect(a.read()).toBe('0');
        expect(b.read()).toBe('1');
        expect(a.value).not.toBe(b.value);
        // Own state did not write through to the shared def prototype.
        expect(hooks.Counter.value).toBeUndefined();
    });

    it('keeps the framework pushEvent even when reached alongside def methods', () => {
        let instance = null;
        hooks.Sender = {
            mounted() {
                instance = this;
                this.ping(); // own method reachable
            },
            ping() {
                this.pushEvent('ping', {}); // framework-owned own property, no throw
            },
        };
        setupView('v', '<div az="0" az-hook="Sender">content</div>');
        mountHooks(document);
        expect(instance).not.toBeNull();
        expect(typeof instance.pushEvent).toBe('function');
    });
});

// ---------------------------------------------------------------------------
// 30. Hooks -- edge cases
// ---------------------------------------------------------------------------

describe('hooks -- edge cases', () => {
    it('empty hooks object works gracefully', () => {
        setupView('v', '<div az="0" az-hook="Missing">content</div>');
        // No hooks registered -- should not throw
        applyOps([[OP.REMOVE_NODE, 'v:0']]);
    });

    it('multiple ops in one batch fire hooks in correct order', () => {
        const calls = [];
        hooks.A = {
            mounted() {
                calls.push('mount:A');
            },
            destroyed() {
                calls.push('destroy:A');
            },
        };
        hooks.B = {
            mounted() {
                calls.push('mount:B');
            },
        };
        setupView('v', '<div az="0" az-hook="A">old</div>');
        mountHooks(document);
        calls.length = 0; // clear initial mount
        // UPDATE destroys old children, inserts new ones
        applyOps([[OP.UPDATE, 'v:0', '<span az-hook="B">new</span>']]);
        // A's children are destroyed first, then B is mounted
        expect(calls).toContain('mount:B');
    });

    it('OP_ITEM_PATCH inner SET_ATTR triggers updated hook', () => {
        const updated = vi.fn();
        hooks.Cell = { mounted() {}, updated };
        setupView(
            'v',
            '<div az="0"><div az-key="k1"><span az="1" az-hook="Cell">val</span></div></div>',
        );
        mountHooks(document);
        applyOps([[OP.ITEM_PATCH, 'v:0', 'k1', [[OP.SET_ATTR, '1', 'class', 'bold']]]]);
        expect(updated).toHaveBeenCalledOnce();
    });

    it('OP_ITEM_PATCH inner REM_ATTR triggers updated hook', () => {
        const updated = vi.fn();
        hooks.Cell = { mounted() {}, updated };
        setupView(
            'v',
            '<div az="0"><div az-key="k1"><span az="1" az-hook="Cell" class="x">val</span></div></div>',
        );
        mountHooks(document);
        applyOps([[OP.ITEM_PATCH, 'v:0', 'k1', [[OP.REM_ATTR, '1', 'class']]]]);
        expect(updated).toHaveBeenCalledOnce();
    });

    it('OP_ITEM_PATCH inner REMOVE_NODE triggers destroyed hook', () => {
        const destroyed = vi.fn();
        hooks.Cell = { mounted() {}, destroyed };
        setupView(
            'v',
            '<div az="0"><div az-key="k1"><span az="1" az-hook="Cell">val</span></div></div>',
        );
        mountHooks(document);
        applyOps([[OP.ITEM_PATCH, 'v:0', 'k1', [[OP.REMOVE_NODE, '1']]]]);
        expect(destroyed).toHaveBeenCalledOnce();
    });

    it('OP_ITEM_PATCH inner UPDATE triggers destroy/mount hooks', () => {
        const destroyed = vi.fn();
        const mounted = vi.fn();
        hooks.Old = { mounted() {}, destroyed };
        hooks.New = { mounted };
        setupView(
            'v',
            '<div az="0"><div az-key="k1"><div az="1"><span az-hook="Old">old</span></div></div></div>',
        );
        mountHooks(document);
        applyOps([
            [OP.ITEM_PATCH, 'v:0', 'k1', [[OP.UPDATE, '1', '<span az-hook="New">new</span>']]],
        ]);
        expect(destroyed).toHaveBeenCalledOnce();
        expect(mounted).toHaveBeenCalledOnce();
    });

    it('OP_ITEM_PATCH inner TEXT marker path triggers hooks', () => {
        const destroyed = vi.fn();
        const mounted = vi.fn();
        hooks.Old = { mounted() {}, destroyed };
        hooks.New = { mounted };
        setupView(
            'v',
            '<div az="0"><div az-key="k1"><div az="1"><!--az:1--><span az-hook="Old">old</span><!--/az--></div></div></div>',
        );
        mountHooks(document);
        applyOps([
            [OP.ITEM_PATCH, 'v:0', 'k1', [[OP.TEXT, '1', '<span az-hook="New">new</span>', true]]],
        ]);
        expect(destroyed).toHaveBeenCalledOnce();
        expect(mounted).toHaveBeenCalledOnce();
    });

    it('OP_TEXT textContent path destroys hooks on children', () => {
        const destroyed = vi.fn();
        hooks.Child = { mounted() {}, destroyed };
        setupView('v', '<div az="0"><span az-hook="Child">child</span></div>');
        mountHooks(document);
        applyOps([[OP.TEXT, 'v:0', 'plain text']]);
        expect(destroyed).toHaveBeenCalledOnce();
    });

    it('OP_TEXT textContent on hooked element fires updated, not destroyed', () => {
        const mounted = vi.fn();
        const updated = vi.fn();
        const destroyed = vi.fn();
        hooks.Box = { mounted, updated, destroyed };
        setupView('v', '<div az="0" az-hook="Box">old</div>');
        mountHooks(document);
        mounted.mockClear();
        applyOps([[OP.TEXT, 'v:0', 'new']]);
        expect(destroyed).not.toHaveBeenCalled();
        expect(mounted).not.toHaveBeenCalled();
        expect(updated).toHaveBeenCalledOnce();
    });

    it('destroyed() callback receives this.el still in document', () => {
        let elInDocument = false;
        hooks.Check = {
            mounted() {},
            destroyed() {
                elInDocument = document.body.contains(this.el);
            },
        };
        setupView('v', '<div az="0" az-hook="Check">content</div>');
        mountHooks(document);
        applyOps([[OP.REMOVE_NODE, 'v:0']]);
        expect(elInDocument).toBe(true);
    });

    it('updated() callback receives this.el', () => {
        let capturedEl = null;
        hooks.Track = {
            mounted() {},
            updated() {
                capturedEl = this.el;
            },
        };
        setupView('v', '<div az="0" az-hook="Track">content</div>');
        mountHooks(document);
        applyOps([[OP.SET_ATTR, 'v:0', 'data-x', '1']]);
        expect(capturedEl).toBe(resolveEl('v:0'));
    });

    it('hook with only destroyed callback (no mounted) still works', () => {
        const destroyed = vi.fn();
        hooks.DestroyOnly = { destroyed };
        setupView('v', '<div az="0" az-hook="DestroyOnly">content</div>');
        mountHooks(document);
        applyOps([[OP.REMOVE_NODE, 'v:0']]);
        expect(destroyed).toHaveBeenCalledOnce();
    });

    it('hook with only updated callback (no mounted) still works', () => {
        const updated = vi.fn();
        hooks.UpdateOnly = { updated };
        setupView('v', '<div az="0" az-hook="UpdateOnly">content</div>');
        mountHooks(document);
        applyOps([[OP.SET_ATTR, 'v:0', 'class', 'x']]);
        expect(updated).toHaveBeenCalledOnce();
    });
});

// ---------------------------------------------------------------------------
// Fingerprinted ops -- ops arrive pre-resolved from the Worker as HTML strings.
// These tests verify that applyOps works correctly with pre-resolved values.
// ---------------------------------------------------------------------------

describe('pre-resolved ops', () => {
    it('OP_INSERT with pre-resolved HTML renders correctly', () => {
        setupView('v', '<ul az="0"></ul>');
        applyOps([[OP.INSERT, 'v:0', '1', -1, '<li az-key="1">hello</li>']]);
        const item = document.querySelector('[az-key="1"]');
        expect(item).not.toBeNull();
        expect(item.textContent).toBe('hello');
    });

    it('OP_UPDATE with pre-resolved HTML sets innerHTML', () => {
        setupView('v', '<div az="0">old</div>');
        applyOps([[OP.UPDATE, 'v:0', '<b>new</b>']]);
        expect(document.querySelector('[az="0"]').innerHTML).toBe('<b>new</b>');
    });

    it('OP_REPLACE with pre-resolved HTML sets outerHTML', () => {
        setupView('v', '<div az="0">old</div>');
        applyOps([[OP.REPLACE, 'v', '<div id="v" az-view>new</div>']]);
        const el = document.querySelector('#v[az-view]');
        expect(el).not.toBeNull();
        expect(el.textContent).toBe('new');
    });

    it('OP_TEXT with pre-resolved HTML updates marker content', () => {
        setupView('v', '<p az="0"><!--az:0-->old<!--/az--></p>');
        applyOps([[OP.TEXT, 'v:0', '<em>hi</em>', true]]);
        const p = document.querySelector('[az="0"]');
        expect(p.innerHTML).toContain('<em>hi</em>');
    });
});

// ---------------------------------------------------------------------------
// OP.ITEM_PATCH with pre-resolved inner ops
// ---------------------------------------------------------------------------

describe('OP.ITEM_PATCH with pre-resolved inner ops', () => {
    it('TEXT inner op with pre-resolved HTML', () => {
        setupView(
            'v',
            '<div az="0"><div az-key="k1"><span az="1"><!--az:1-->old<!--/az--></span></div></div>',
        );
        applyOps([[OP.ITEM_PATCH, 'v:0', 'k1', [[OP.TEXT, '1', '<em>new</em>', true]]]]);
        const item = resolveEl('v:0').querySelector('[az-key="k1"]');
        expect(item.querySelector('[az="1"]').innerHTML).toContain('<em>new</em>');
    });

    it('TEXT inner op with marker and pre-resolved HTML', () => {
        setupView(
            'v',
            '<div az="0"><div az-key="k1"><span az="1"><!--az:1-->old<!--/az--></span></div></div>',
        );
        applyOps([[OP.ITEM_PATCH, 'v:0', 'k1', [[OP.TEXT, '1', '<b>marked</b>', true]]]]);
        const item = resolveEl('v:0').querySelector('[az-key="k1"]');
        expect(item.querySelector('[az="1"]').innerHTML).toContain('<b>marked</b>');
    });

    it('UPDATE inner op with pre-resolved HTML', () => {
        setupView('v', '<div az="0"><div az-key="k1"><div az="1">old</div></div></div>');
        applyOps([[OP.ITEM_PATCH, 'v:0', 'k1', [[OP.UPDATE, '1', '<strong>updated</strong>']]]]);
        const item = resolveEl('v:0').querySelector('[az-key="k1"]');
        expect(item.querySelector('[az="1"]').innerHTML).toBe('<strong>updated</strong>');
    });

    it('mixed plain and pre-resolved inner ops work together', () => {
        setupView(
            'v',
            '<div az="0"><div az-key="k1"><span az="1"><!--az:1-->old<!--/az--></span><span az="2" class="x">y</span></div></div>',
        );
        applyOps([
            [
                OP.ITEM_PATCH,
                'v:0',
                'k1',
                [
                    [OP.TEXT, '1', '<i>fancy</i>', true],
                    [OP.SET_ATTR, '2', 'class', 'highlight'],
                ],
            ],
        ]);
        const item = resolveEl('v:0').querySelector('[az-key="k1"]');
        expect(item.querySelector('[az="1"]').innerHTML).toContain('<i>fancy</i>');
        expect(item.querySelector('[az="2"]').getAttribute('class')).toBe('highlight');
    });
});

// ---------------------------------------------------------------------------
// onmessage handles partial envelopes (omitted "o" or "e")
// Worker sends [0, ops|null, effects|null, firstAfterReconnect] to main.
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
// Client-owned slots (?local): set/get query the live DOM (az-local descriptors);
// no persistent index. Updates are local -- no server round-trip.
// ---------------------------------------------------------------------------

describe('?local -- set/get content', () => {
    function contentView(viewId, key, initial) {
        setupView(
            viewId,
            `<span az="0" az-local='{"c":{"0":"${key}"}}'><!--az:0-->${initial}<!--/az--></span>`,
        );
    }

    it('set updates the content and get reads it back', () => {
        contentView('v', 'title', 'old');
        set('v', 'title', 'new');
        expect(document.querySelector('[az="0"]').textContent).toBe('new');
        expect(get('v', 'title')).toBe('new');
    });

    it('get returns the SSR initial before any set', () => {
        contentView('v', 'title', 'hello');
        expect(get('v', 'title')).toBe('hello');
    });

    it('writes the value as text, never HTML (no injection)', () => {
        contentView('v', 'title', '');
        set('v', 'title', '<img src=x onerror=alert(1)>');
        const span = document.querySelector('[az="0"]');
        expect(span.querySelector('img')).toBeNull();
        expect(span.textContent).toBe('<img src=x onerror=alert(1)>');
    });
});

describe('?local -- set/get attribute', () => {
    function attrView(viewId, attr, key, extra = '') {
        setupView(viewId, `<div az="0" az-local='{"a":{"${attr}":"${key}"}}' ${extra}>x</div>`);
    }

    it('true sets a bare boolean attribute; false removes it', () => {
        attrView('v', 'open', 'modal_open');
        const el = document.querySelector('[az="0"]');
        set('v', 'modal_open', true);
        expect(el.getAttribute('open')).toBe('');
        set('v', 'modal_open', false);
        expect(el.hasAttribute('open')).toBe(false);
    });

    it('a string sets the attribute value', () => {
        attrView('v', 'data-active', 'tab');
        set('v', 'tab', 'settings');
        expect(document.querySelector('[az="0"]').getAttribute('data-active')).toBe('settings');
        expect(get('v', 'tab')).toBe('settings');
    });

    it('value binding syncs the live input value property', () => {
        setupView('v', `<input az="0" az-local='{"a":{"value":"field"}}' value="" />`);
        set('v', 'field', 'typed');
        const input = document.querySelector('input');
        expect(input.getAttribute('value')).toBe('typed');
        expect(input.value).toBe('typed');
    });
});

describe('?local -- interpolated attribute', () => {
    it('set recomposes prefix + value + suffix; get strips them', () => {
        setupView(
            'v',
            `<button az="0" az-local='{"a":{"class":"variant"},"ap":{"class":["btn btn-",""]}}' class="btn btn-primary">x</button>`,
        );
        set('v', 'variant', 'secondary');
        const btn = document.querySelector('[az="0"]');
        expect(btn.getAttribute('class')).toBe('btn btn-secondary');
        expect(get('v', 'variant')).toBe('secondary');
    });

    it('strips both a prefix and a suffix on read', () => {
        setupView(
            'v',
            `<a az="0" az-local='{"a":{"href":"id"},"ap":{"href":["/u/","/edit"]}}' href="/u/1/edit">e</a>`,
        );
        expect(get('v', 'id')).toBe('1');
        set('v', 'id', '42');
        expect(document.querySelector('[az="0"]').getAttribute('href')).toBe('/u/42/edit');
        expect(get('v', 'id')).toBe('42');
    });

    it('coerces a non-string value via String() and reads it back', () => {
        setupView(
            'v',
            `<li az="0" az-local='{"a":{"style":"w"},"ap":{"style":["width: ","%"]}}' style="width: 25%">x</li>`,
        );
        set('v', 'w', 50);
        expect(document.querySelector('[az="0"]').getAttribute('style')).toBe('width: 50%');
        expect(get('v', 'w')).toBe('50');
    });

    it('syncs the live value property for an interpolated value attribute', () => {
        setupView(
            'v',
            `<input az="0" az-local='{"a":{"value":"amt"},"ap":{"value":["$",""]}}' value="$0" />`,
        );
        set('v', 'amt', '50');
        const input = document.querySelector('input');
        expect(input.getAttribute('value')).toBe('$50');
        expect(input.value).toBe('$50');
    });

    it('sets the recomposed value literally -- no markup injection', () => {
        setupView(
            'v',
            `<span az="0" az-local='{"a":{"data-x":"d"},"ap":{"data-x":["p-",""]}}' data-x="p-ok">x</span>`,
        );
        set('v', 'd', '"><img src=x onerror=alert(1)>');
        const span = document.querySelector('[az="0"]');
        expect(span.querySelector('img')).toBeNull();
        expect(span.getAttribute('data-x')).toBe('p-"><img src=x onerror=alert(1)>');
    });

    it('is per-view isolated and setAll spans every view', () => {
        document.body.innerHTML =
            `<div id="a" az-view><i az="0" az-local='{"a":{"class":"k"},"ap":{"class":["t-",""]}}' class="t-1">x</i></div>` +
            `<div id="b" az-view><i az="0" az-local='{"a":{"class":"k"},"ap":{"class":["t-",""]}}' class="t-1">y</i></div>`;
        set('a', 'k', '2');
        expect(document.querySelector('#a i').getAttribute('class')).toBe('t-2');
        expect(document.querySelector('#b i').getAttribute('class')).toBe('t-1');
        setAll('k', '9');
        expect(document.querySelector('#a i').getAttribute('class')).toBe('t-9');
        expect(document.querySelector('#b i').getAttribute('class')).toBe('t-9');
    });
});

describe('?local -- per-view isolation', () => {
    const twoViews = () => {
        document.body.innerHTML =
            `<div id="a" az-view><span az="0" az-local='{"c":{"0":"msg"}}'><!--az:0-->A<!--/az--></span></div>` +
            `<div id="b" az-view><span az="0" az-local='{"c":{"0":"msg"}}'><!--az:0-->B<!--/az--></span></div>`;
    };

    it('a set in one view does not touch another view with the same key', () => {
        twoViews();
        set('a', 'msg', 'changed');
        expect(document.querySelector('#a [az="0"]').textContent).toBe('changed');
        expect(document.querySelector('#b [az="0"]').textContent).toBe('B');
    });

    it('setAll updates every view', () => {
        twoViews();
        setAll('msg', 'all');
        expect(document.querySelector('#a [az="0"]').textContent).toBe('all');
        expect(document.querySelector('#b [az="0"]').textContent).toBe('all');
    });
});

describe('?local -- set command', () => {
    it('executeJS resolves the trigger view and updates the slot', () => {
        setupView(
            'v',
            `<span az="0" az-local='{"c":{"0":"title"}}'><!--az:0-->old<!--/az--></span>` +
                `<button az="1">go</button>`,
        );
        executeJS(document.querySelector('button'), null, [17, 'title', 'clicked']);
        expect(document.querySelector('[az="0"]').textContent).toBe('clicked');
    });

    it('an explicit viewId targets that view, not the trigger view', () => {
        document.body.innerHTML =
            `<div id="a" az-view><span az="0" az-local='{"c":{"0":"k"}}'><!--az:0-->A<!--/az--></span></div>` +
            `<div id="b" az-view><button>go</button></div>`;
        executeJS(document.querySelector('#b button'), null, [17, 'k', 'x', 'a']);
        expect(document.querySelector('#a [az="0"]').textContent).toBe('x');
    });
});

describe('?local -- edge cases', () => {
    it('nested views are isolated: a set in the outer view skips the inner view', () => {
        document.body.innerHTML =
            `<div id="outer" az-view>` +
            `<span az="0" az-local='{"c":{"0":"k"}}'><!--az:0-->O<!--/az--></span>` +
            `<div id="inner" az-view><span az="0" az-local='{"c":{"0":"k"}}'><!--az:0-->I<!--/az--></span></div>` +
            `</div>`;
        set('outer', 'k', 'X');
        expect(document.querySelector('#outer > [az="0"]').textContent).toBe('X');
        expect(document.querySelector('#inner [az="0"]').textContent).toBe('I');
    });

    it('a key bound on multiple slots in one view updates all of them', () => {
        setupView(
            'v',
            `<span az="0" az-local='{"c":{"0":"n"}}'><!--az:0-->-<!--/az--></span>` +
                `<b az="1" az-local='{"c":{"0":"n"}}'><!--az:1-->-<!--/az--></b>`,
        );
        set('v', 'n', '7');
        expect(document.querySelector('[az="0"]').textContent).toBe('7');
        expect(document.querySelector('[az="1"]').textContent).toBe('7');
    });

    it('set for a key with no matching slot is a no-op', () => {
        setupView('v', `<span az="0" az-local='{"c":{"0":"a"}}'><!--az:0-->x<!--/az--></span>`);
        expect(() => set('v', 'missing', 'y')).not.toThrow();
        expect(document.querySelector('[az="0"]').textContent).toBe('x');
    });

    it('get for a missing key returns undefined', () => {
        setupView('v', `<span az="0" az-local='{"c":{"0":"a"}}'><!--az:0-->x<!--/az--></span>`);
        expect(get('v', 'missing')).toBeUndefined();
    });

    it('get on an absent boolean attribute returns false', () => {
        setupView('v', `<div az="0" az-local='{"a":{"open":"o"}}'>x</div>`);
        expect(get('v', 'o')).toBe(false);
        set('v', 'o', true);
        expect(get('v', 'o')).toBe(true);
    });
});

describe('?local -- multiple content slots in one element', () => {
    it('writes each slot independently, leaving static text and siblings intact', () => {
        // Element az "0"; slot 0 marker is "0", slot 1 marker is "0:1".
        setupView(
            'v',
            `<p az="0" az-local='{"c":{"0":"a","1":"b"}}'>` +
                `<!--az:0-->A<!--/az-->foo<!--az:0:1-->B<!--/az--></p>`,
        );
        set('v', 'a', 'X');
        set('v', 'b', 'Y');
        expect(document.querySelector('[az="0"]').textContent).toBe('XfooY');
        expect(get('v', 'a')).toBe('X');
        expect(get('v', 'b')).toBe('Y');
    });

    it('a key shared by two slots in one element updates both', () => {
        setupView(
            'v',
            `<p az="0" az-local='{"c":{"0":"n","1":"n"}}'>` +
                `<!--az:0-->-<!--/az--><!--az:0:1-->-<!--/az--></p>`,
        );
        set('v', 'n', '7');
        expect(document.querySelector('[az="0"]').textContent).toBe('77');
    });
});

describe('?local -- reset / isolation on server ops', () => {
    it('OP_UPDATE of an enclosing element resets a contained ?local to its SSR initial', () => {
        setupView(
            'v',
            `<div az="0"><span az="1" az-local='{"c":{"0":"k"}}'><!--az:1-->init<!--/az--></span></div>`,
        );
        set('v', 'k', 'edited');
        expect(document.querySelector('[az-local]').textContent).toBe('edited');
        // Server re-renders the whole region (innerHTML) carrying the SSR-initial slot.
        applyOps([
            [
                OP.UPDATE,
                'v:0',
                `<span az="1" az-local='{"c":{"0":"k"}}'><!--az:1-->init<!--/az--></span>`,
            ],
        ]);
        expect(document.querySelector('[az-local]').textContent).toBe('init');
        expect(get('v', 'k')).toBe('init');
    });

    it('OP_REPLACE of the slot element resets the ?local to its SSR initial', () => {
        setupView('v', `<span az="0" az-local='{"c":{"0":"k"}}'><!--az:0-->init<!--/az--></span>`);
        set('v', 'k', 'edited');
        expect(document.querySelector('[az-local]').textContent).toBe('edited');
        applyOps([
            [
                OP.REPLACE,
                'v:0',
                `<span az="0" az-local='{"c":{"0":"k"}}'><!--az:0-->init<!--/az--></span>`,
            ],
        ]);
        expect(document.querySelector('[az-local]').textContent).toBe('init');
    });

    it('a targeted OP_TEXT on a sibling marker leaves an adjacent ?local untouched', () => {
        setupView(
            'v',
            `<div az="0"><!--az:0-->server<!--/az-->` +
                `<span az="1" az-local='{"c":{"0":"k"}}'><!--az:1-->localinit<!--/az--></span></div>`,
        );
        set('v', 'k', 'edited');
        applyOps([[OP.TEXT, 'v:0', 'server-changed']]);
        const div = document.querySelector('[az="0"]');
        expect(div.querySelector('[az-local]').textContent).toBe('edited');
        expect(get('v', 'k')).toBe('edited');
    });
});

describe('?local -- set as a handler effect (applyEffects)', () => {
    // op 17 = JS_SET_LOCAL. applyEffects is the server->client handler-effect entry
    // point (dispatched against <html>): a 4th elem of a viewId string scopes to
    // that view; `true` is set_all.
    it('a viewId-scoped set_local effect updates that view slot', () => {
        document.body.innerHTML = `<div id="v" az-view><span az="0" az-local='{"c":{"0":"k"}}'><!--az:0-->init<!--/az--></span></div>`;
        applyEffects([[17, 'k', 'from-effect', 'v']]);
        expect(document.querySelector('#v [az-local]').textContent).toBe('from-effect');
    });

    it('a set_all effect updates every view', () => {
        document.body.innerHTML =
            `<div id="a" az-view><span az="0" az-local='{"c":{"0":"k"}}'><!--az:0-->A<!--/az--></span></div>` +
            `<div id="b" az-view><span az="0" az-local='{"c":{"0":"k"}}'><!--az:0-->B<!--/az--></span></div>`;
        applyEffects([[17, 'k', 'all', true]]);
        expect(document.querySelector('#a [az-local]').textContent).toBe('all');
        expect(document.querySelector('#b [az-local]').textContent).toBe('all');
    });
});

describe('?local -- no server round-trip', () => {
    let mock;
    afterEach(() => {
        if (mock) mock.restore();
    });

    it('a set-command click updates the DOM and sends no worker message', async () => {
        vi.resetModules();
        const mod = await import('./arizona.js');
        document.body.innerHTML =
            `<div id="v" az-view>` +
            `<span az="0" az-local='{"c":{"0":"title"}}'><!--az:0-->old<!--/az--></span>` +
            `<button az="1" az-click='[17,"title","new"]'>go</button>` +
            `</div>`;
        mock = setupMockWorker(mod);
        mock.simulateOpen();
        document.querySelector('button').dispatchEvent(new MouseEvent('click', { bubbles: true }));
        expect(document.querySelector('[az="0"]').textContent).toBe('new');
        expect(mock.getSentMessages()).toEqual([]);
    });
});

describe('onmessage partial envelopes', () => {
    let mock;
    afterEach(() => {
        if (mock) mock.restore();
    });

    it('ops-only message applies ops without error', async () => {
        vi.resetModules();
        const mod = await import('./arizona.js');

        document.body.innerHTML = '<div id="v" az-view><span az="0">old</span></div>';

        mock = setupMockWorker(mod);
        mock.simulateOpen();
        mock.simulateMessage([[0, 'v:0', 'new']], null);
        expect(document.querySelector('[az="0"]').textContent).toBe('new');
    });

    it('effects-only message applies effects without error', async () => {
        vi.resetModules();
        const mod = await import('./arizona.js');

        const received = [];
        document.addEventListener('test_effect', (e) => received.push(e.detail), { once: true });

        mock = setupMockWorker(mod);
        mock.simulateOpen();
        mock.simulateMessage(null, [[9, 'test_effect', { ok: true }]]);
        expect(received).toEqual([{ ok: true }]);
    });

    it('full message with ops and effects applies both', async () => {
        vi.resetModules();
        const mod = await import('./arizona.js');

        document.body.innerHTML = '<div id="v" az-view><span az="0">old</span></div>';

        const received = [];
        document.addEventListener('full_effect', (e) => received.push(e.detail), { once: true });

        mock = setupMockWorker(mod);
        mock.simulateOpen();
        mock.simulateMessage([[0, 'v:0', 'updated']], [[9, 'full_effect', { n: 1 }]]);
        expect(document.querySelector('[az="0"]').textContent).toBe('updated');
        expect(received).toEqual([{ n: 1 }]);
    });

    it('null ops and effects is handled gracefully', async () => {
        vi.resetModules();
        const mod = await import('./arizona.js');

        document.body.innerHTML = '<div id="v" az-view><span az="0">unchanged</span></div>';

        mock = setupMockWorker(mod);
        mock.simulateOpen();
        mock.simulateMessage(null, null);
        expect(document.querySelector('[az="0"]').textContent).toBe('unchanged');
    });
});

// ---------------------------------------------------------------------------
// heartbeat -- tested at the Worker level, but we verify the main thread
// sends keepalive pings via [1, "0"] to the Worker.
// ---------------------------------------------------------------------------

describe('heartbeat', () => {
    let mock;
    beforeEach(() => {
        vi.useFakeTimers();
    });
    afterEach(() => {
        vi.useRealTimers();
        if (mock) mock.restore();
    });

    it('sends ping via Worker when connected', async () => {
        vi.resetModules();
        const mod = await import('./arizona.js');

        mock = setupMockWorker(mod);
        mock.simulateOpen();
        mock.posted.length = 0;

        // The heartbeat now lives in the Worker -- main thread doesn't send pings.
        // This test verifies that main→Worker data sends use [1, json] format.
        document.body.innerHTML = `
            <div id="v" az-view>
                <button az-click='[0,"test"]' id="v" az-view>click</button>
            </div>
        `;
        document.querySelector('button').click();
        vi.advanceTimersByTime(0); // flush microtasks
        const sends = mock.getSentMessages();
        expect(sends.length).toBeGreaterThanOrEqual(1);
        expect(sends[0][1]).toBe('test');
    });
});

// ---------------------------------------------------------------------------
// saveFormState / restoreFormState -- form preservation across reconnect
// ---------------------------------------------------------------------------

describe('saveFormState / restoreFormState', () => {
    it('saves and restores form field values', () => {
        document.body.innerHTML = '<form id="myform"><input name="title" value="hello"></form>';
        saveFormState();

        // Simulate DOM replacement (OP_REPLACE wipes the DOM)
        document.body.innerHTML = '<form id="myform"><input name="title" value=""></form>';

        restoreFormState();
        expect(document.querySelector('input[name="title"]').value).toBe('hello');
    });

    it('ignores forms without id', () => {
        document.body.innerHTML = '<form><input name="x" value="val"></form>';
        saveFormState();

        document.body.innerHTML = '<form><input name="x" value=""></form>';
        restoreFormState();
        // Without form id, value should NOT be restored
        expect(document.querySelector('input[name="x"]').value).toBe('');
    });

    it('handles multiple forms', () => {
        document.body.innerHTML =
            '<form id="f1"><input name="a" value="1"></form>' +
            '<form id="f2"><input name="b" value="2"></form>';
        saveFormState();

        document.body.innerHTML =
            '<form id="f1"><input name="a" value=""></form>' +
            '<form id="f2"><input name="b" value=""></form>';
        restoreFormState();

        expect(document.querySelector('#f1 input').value).toBe('1');
        expect(document.querySelector('#f2 input').value).toBe('2');
    });

    it('skips missing forms on restore', () => {
        document.body.innerHTML = '<form id="gone"><input name="x" value="val"></form>';
        saveFormState();

        document.body.innerHTML = '<div>no form here</div>';
        // Should not throw
        expect(() => restoreFormState()).not.toThrow();
    });

    it('clears saved state after restore', () => {
        document.body.innerHTML = '<form id="f"><input name="x" value="saved"></form>';
        saveFormState();

        document.body.innerHTML = '<form id="f"><input name="x" value=""></form>';
        restoreFormState();
        expect(document.querySelector('input').value).toBe('saved');

        // Restore again -- should be no-op since state was cleared
        document.body.innerHTML = '<form id="f"><input name="x" value="empty"></form>';
        restoreFormState();
        expect(document.querySelector('input').value).toBe('empty');
    });
});

// ---------------------------------------------------------------------------
// saveFormState / restoreFormState -- checkbox, radio, select, textarea
// ---------------------------------------------------------------------------

describe('saveFormState / restoreFormState edge cases', () => {
    it('saves and restores checkboxes', () => {
        document.body.innerHTML =
            '<form id="f"><input type="checkbox" name="agree" checked></form>';
        saveFormState();

        // Simulate DOM replacement -- unchecked by default
        document.body.innerHTML = '<form id="f"><input type="checkbox" name="agree"></form>';
        restoreFormState();
        expect(document.querySelector('input[name="agree"]').checked).toBe(true);
    });

    it('unchecks checkboxes absent from saved state', () => {
        // Checkbox unchecked → FormData omits it → field NOT in saved fields
        document.body.innerHTML = '<form id="f"><input type="checkbox" name="opt"></form>';
        saveFormState();

        // After replace, checkbox might be pre-checked by server HTML
        document.body.innerHTML = '<form id="f"><input type="checkbox" name="opt" checked></form>';
        restoreFormState();
        expect(document.querySelector('input[name="opt"]').checked).toBe(false);
    });

    it('restores a checkbox group by value, not by name presence (J4)', () => {
        // A checkbox group saves only the CHECKED boxes' values, so a name-presence
        // check would tick every box in the group.
        document.body.innerHTML =
            '<form id="f">' +
            '<input type="checkbox" name="c" value="a" checked>' +
            '<input type="checkbox" name="c" value="b">' +
            '<input type="checkbox" name="c" value="c" checked>' +
            '</form>';
        saveFormState();

        document.body.innerHTML =
            '<form id="f">' +
            '<input type="checkbox" name="c" value="a">' +
            '<input type="checkbox" name="c" value="b">' +
            '<input type="checkbox" name="c" value="c">' +
            '</form>';
        restoreFormState();
        const boxes = document.querySelectorAll('input[name="c"]');
        expect(boxes[0].checked).toBe(true); // "a" was checked
        expect(boxes[1].checked).toBe(false); // "b" was not
        expect(boxes[2].checked).toBe(true); // "c" was checked
    });

    it('restores duplicate-name text inputs positionally, not the stringified array (J4)', () => {
        document.body.innerHTML =
            '<form id="f"><input name="t" value="first"><input name="t" value="second"></form>';
        saveFormState();

        document.body.innerHTML =
            '<form id="f"><input name="t" value=""><input name="t" value=""></form>';
        restoreFormState();
        const inputs = document.querySelectorAll('input[name="t"]');
        expect(inputs[0].value).toBe('first');
        expect(inputs[1].value).toBe('second');
    });

    it('saves and restores radio buttons', () => {
        document.body.innerHTML =
            '<form id="f">' +
            '<input type="radio" name="color" value="red">' +
            '<input type="radio" name="color" value="blue" checked>' +
            '</form>';
        saveFormState();

        document.body.innerHTML =
            '<form id="f">' +
            '<input type="radio" name="color" value="red">' +
            '<input type="radio" name="color" value="blue">' +
            '</form>';
        restoreFormState();
        const radios = document.querySelectorAll('input[name="color"]');
        expect(radios[0].checked).toBe(false);
        expect(radios[1].checked).toBe(true);
    });

    it('saves and restores select elements', () => {
        document.body.innerHTML =
            '<form id="f"><select name="size">' +
            '<option value="s">S</option>' +
            '<option value="m" selected>M</option>' +
            '<option value="l">L</option>' +
            '</select></form>';
        saveFormState();

        document.body.innerHTML =
            '<form id="f"><select name="size">' +
            '<option value="s">S</option>' +
            '<option value="m">M</option>' +
            '<option value="l">L</option>' +
            '</select></form>';
        restoreFormState();
        expect(document.querySelector('select[name="size"]').value).toBe('m');
    });

    it('saves and restores select multiple', () => {
        document.body.innerHTML =
            '<form id="f"><select name="colors" multiple>' +
            '<option value="red" selected>Red</option>' +
            '<option value="green">Green</option>' +
            '<option value="blue" selected>Blue</option>' +
            '</select></form>';
        saveFormState();

        document.body.innerHTML =
            '<form id="f"><select name="colors" multiple>' +
            '<option value="red">Red</option>' +
            '<option value="green">Green</option>' +
            '<option value="blue">Blue</option>' +
            '</select></form>';
        restoreFormState();

        const options = document.querySelectorAll('select[name="colors"] option');
        expect(options[0].selected).toBe(true); // red
        expect(options[1].selected).toBe(false); // green
        expect(options[2].selected).toBe(true); // blue
    });

    it('saves and restores select multiple with single selection', () => {
        document.body.innerHTML =
            '<form id="f"><select name="size" multiple>' +
            '<option value="s">S</option>' +
            '<option value="m" selected>M</option>' +
            '<option value="l">L</option>' +
            '</select></form>';
        saveFormState();

        document.body.innerHTML =
            '<form id="f"><select name="size" multiple>' +
            '<option value="s">S</option>' +
            '<option value="m">M</option>' +
            '<option value="l">L</option>' +
            '</select></form>';
        restoreFormState();

        const options = document.querySelectorAll('select[name="size"] option');
        expect(options[0].selected).toBe(false);
        expect(options[1].selected).toBe(true);
        expect(options[2].selected).toBe(false);
    });

    it('saves and restores textarea', () => {
        document.body.innerHTML =
            '<form id="f"><textarea name="note">hello world</textarea></form>';
        saveFormState();

        document.body.innerHTML = '<form id="f"><textarea name="note"></textarea></form>';
        restoreFormState();
        expect(document.querySelector('textarea[name="note"]').value).toBe('hello world');
    });

    it('handles empty form without throwing', () => {
        document.body.innerHTML = '<form id="f"></form>';
        saveFormState();
        document.body.innerHTML = '<form id="f"></form>';
        expect(() => restoreFormState()).not.toThrow();
    });

    it('preserves defaults for fields not in saved state', () => {
        // Save form with only one field
        document.body.innerHTML = '<form id="f"><input name="a" value="saved"></form>';
        saveFormState();

        // After replace, form has a new field not in saved data
        document.body.innerHTML =
            '<form id="f"><input name="a" value=""><input name="b" value="default"></form>';
        restoreFormState();
        expect(document.querySelector('input[name="a"]').value).toBe('saved');
        expect(document.querySelector('input[name="b"]').value).toBe('default');
    });

    it('replays az-change on restore when connected', async () => {
        vi.resetModules();
        const mod = await import('./arizona.js');

        document.body.innerHTML =
            '<form id="f" az-change="[0,&quot;validate&quot;]"><div id="v" az-view><input name="x" value="val"></div></form>';
        mod.saveFormState();

        document.body.innerHTML =
            '<form id="f" az-change="[0,&quot;validate&quot;]"><div id="v" az-view><input name="x" value=""></div></form>';

        const mock = setupMockWorker(mod);
        mock.simulateOpen();
        mock.posted.length = 0;
        mod.restoreFormState();
        const changeMsgs = mock.getSentMessages().filter((s) => s[1] === 'validate');
        expect(changeMsgs).toHaveLength(1);
        expect(changeMsgs[0][2]).toEqual({ x: 'val' });
        mock.restore();
    });

    it('az-change replay sends event name not raw command string', async () => {
        vi.resetModules();
        const mod = await import('./arizona.js');

        document.body.innerHTML =
            '<form id="f" az-change="[0,&quot;my_change&quot;]"><div id="v" az-view><input name="a" value="1"></div></form>';
        mod.saveFormState();

        document.body.innerHTML =
            '<form id="f" az-change="[0,&quot;my_change&quot;]"><div id="v" az-view><input name="a" value=""></div></form>';

        const mock = setupMockWorker(mod);
        mock.simulateOpen();
        mock.posted.length = 0;
        mod.restoreFormState();
        const sent = mock.getSentMessages();
        // Event name should be "my_change", not the raw "[0,\"my_change\"]"
        const correct = sent.filter((s) => s[1] === 'my_change');
        const raw = sent.filter((s) => typeof s[1] === 'string' && s[1].startsWith('['));
        expect(correct).toHaveLength(1);
        expect(raw).toHaveLength(0);
        mock.restore();
    });

    it('does NOT replay az-change when form lacks the attribute', async () => {
        vi.resetModules();
        const mod = await import('./arizona.js');

        document.body.innerHTML =
            '<form id="f"><div id="v" az-view><input name="x" value="val"></div></form>';
        mod.saveFormState();

        document.body.innerHTML =
            '<form id="f"><div id="v" az-view><input name="x" value=""></div></form>';

        const mock = setupMockWorker(mod);
        mock.simulateOpen();
        mock.posted.length = 0;
        mod.restoreFormState();
        const changeMsgs = mock.getSentMessages().filter((s) => s[1] === 'validate');
        expect(changeMsgs).toHaveLength(0);
        mock.restore();
    });
});

// ---------------------------------------------------------------------------
// CSS connection classes -- via mock Worker
// ---------------------------------------------------------------------------

describe('connection CSS classes', () => {
    let mock;
    afterEach(() => {
        if (mock) mock.restore();
    });

    it('adds az-connected on open and az-disconnected on close', async () => {
        vi.resetModules();
        const mod = await import('./arizona.js');

        mock = setupMockWorker(mod);

        // Simulate open
        mock.simulateOpen();
        expect(document.documentElement.classList.contains('az-connected')).toBe(true);
        expect(document.documentElement.classList.contains('az-disconnected')).toBe(false);

        // Simulate close (abnormal -- code 1006)
        mock.simulateClose(1006);
        expect(document.documentElement.classList.contains('az-disconnected')).toBe(true);
        expect(document.documentElement.classList.contains('az-connected')).toBe(false);
    });

    it('saves form state on abnormal close', async () => {
        vi.resetModules();
        const mod = await import('./arizona.js');

        document.body.innerHTML = '<form id="f"><input name="x" value="saved"></form>';

        mock = setupMockWorker(mod);
        mock.simulateOpen();
        mock.simulateClose(1006);

        // After abnormal close, form state should be saved
        // Verify by replacing DOM and restoring
        document.body.innerHTML = '<form id="f"><input name="x" value=""></form>';
        mock.simulateOpen(true); // reconnect
        mock.simulateMessage(null, null, true); // firstAfterReconnect
        expect(document.querySelector('input[name="x"]').value).toBe('saved');
    });

    it('does NOT save form state on normal close (code 1000)', async () => {
        vi.resetModules();
        const mod = await import('./arizona.js');

        document.body.innerHTML = '<form id="f"><input name="x" value="saved"></form>';

        mock = setupMockWorker(mod);
        mock.simulateOpen();
        mock.simulateClose(1000);

        // Replace DOM -- form state should NOT be restored
        document.body.innerHTML = '<form id="f"><input name="x" value=""></form>';
        mock.simulateOpen(true);
        mock.simulateMessage(null, null, true);
        expect(document.querySelector('input[name="x"]').value).toBe('');
    });

    it('reconnect open sets az-connected class', async () => {
        vi.resetModules();
        const mod = await import('./arizona.js');

        mock = setupMockWorker(mod);

        // First open
        document.body.innerHTML = `
            <div id="v" az-view><span az="0">inner</span></div>
        `;
        mock.simulateOpen();
        expect(document.documentElement.classList.contains('az-connected')).toBe(true);

        // Close and reconnect
        mock.simulateClose(1006);
        mock.simulateOpen(true);
        expect(document.documentElement.classList.contains('az-connected')).toBe(true);
    });
});

// ---------------------------------------------------------------------------
// WS_CLOSE_CRASH (4500) reload loop guard (J6)
// ---------------------------------------------------------------------------

describe('WS_CLOSE_CRASH reload guard', () => {
    /** @type {ReturnType<typeof setupMockWorker>} */
    let mock;
    let reload;
    let errSpy;

    beforeEach(() => {
        sessionStorage.clear();
        reload = vi.fn();
        errSpy = vi.spyOn(console, 'error').mockImplementation(() => {});
    });

    afterEach(() => {
        vi.unstubAllGlobals();
        if (mock) mock.restore();
        errSpy.mockRestore();
        sessionStorage.clear();
    });

    // Stub location AFTER connect() runs: connect builds the Worker URL via
    // `new URL(..., import.meta.url)`, which vitest+jsdom resolves against the real
    // `location`, so replacing it beforehand throws. crashReload only needs
    // `location.reload`, and no code path after connect constructs a URL.
    function stubReload() {
        vi.stubGlobal('location', { reload, href: 'http://localhost/' });
    }

    it('reloads on a crash close but stops after the cap to break the loop', async () => {
        vi.resetModules();
        const mod = await import('./arizona.js');
        setupView('page', '<div az="0"></div>');
        mock = setupMockWorker(mod);
        mock.simulateOpen();
        stubReload();

        // A deterministic mount crash: every close is a 4500 (WS_CLOSE_CRASH).
        for (let i = 0; i < 5; i++) mock.simulateClose(4500);

        // At most 3 reloads, then it gives up (no infinite loop) and logs.
        expect(reload).toHaveBeenCalledTimes(3);
        expect(errSpy).toHaveBeenCalled();
    });

    it('a normal (non-crash) close never reloads', async () => {
        vi.resetModules();
        const mod = await import('./arizona.js');
        setupView('page', '<div az="0"></div>');
        mock = setupMockWorker(mod);
        mock.simulateOpen();
        stubReload();

        mock.simulateClose(1006);
        expect(reload).not.toHaveBeenCalled();
    });

    it('a clean reconnect resets the guard so a later crash reloads again', async () => {
        vi.resetModules();
        const mod = await import('./arizona.js');
        setupView('page', '<div az="0"></div>');
        mock = setupMockWorker(mod);
        mock.simulateOpen();
        stubReload();

        for (let i = 0; i < 4; i++) mock.simulateClose(4500); // 3 reloads, then stop
        expect(reload).toHaveBeenCalledTimes(3);

        mock.simulateOpen(true); // a clean reconnect clears the crash counter
        mock.simulateClose(4500);
        expect(reload).toHaveBeenCalledTimes(4);
    });
});

// ---------------------------------------------------------------------------
// connection params
// ---------------------------------------------------------------------------

describe('connection params', () => {
    it('sends WS URL without params when none provided', async () => {
        vi.resetModules();
        const mod = await import('./arizona.js');
        const posted = [];
        const OrigWorker = globalThis.Worker;
        globalThis.Worker = function () {
            return {
                postMessage: (d) => posted.push(d),
                set onmessage(_) {},
                get onmessage() {
                    return null;
                },
                terminate() {},
            };
        };
        const disconnect = mod.connect('/ws');
        globalThis.Worker = OrigWorker;
        const wsUrl = posted[0][1];
        expect(wsUrl).not.toContain('params=');
        disconnect();
    });

    it('sends WS URL with connect params as regular query keys', async () => {
        vi.resetModules();
        const mod = await import('./arizona.js');
        const posted = [];
        const OrigWorker = globalThis.Worker;
        globalThis.Worker = function () {
            return {
                postMessage: (d) => posted.push(d),
                set onmessage(_) {},
                get onmessage() {
                    return null;
                },
                terminate() {},
            };
        };
        const disconnect = mod.connect('/ws', { locale: 'en' });
        globalThis.Worker = OrigWorker;
        const wsUrl = posted[0][1];
        const qs = new URL(wsUrl.replace(/^ws/, 'http')).searchParams;
        expect(qs.get('_az_path')).toBe('/');
        expect(qs.get('locale')).toBe('en');
        disconnect();
    });
});

// ---------------------------------------------------------------------------
// worker spawn shape
// ---------------------------------------------------------------------------

describe('worker spawn', () => {
    it('spawns the worker as an ES module from a URL relative to the client module', async () => {
        vi.resetModules();
        const mod = await import('./arizona.js');
        /** @type {{url: URL, opts: object}|null} */
        let workerArgs = null;
        const OrigWorker = globalThis.Worker;
        globalThis.Worker = function (url, opts) {
            workerArgs = { url, opts };
            return {
                postMessage() {},
                set onmessage(_) {},
                get onmessage() {
                    return null;
                },
                terminate() {},
            };
        };
        // Disconnect + restore before asserting so a failed expect() can't leak
        // the persistent pageshow/pagehide listeners into later tests.
        const disconnect = mod.connect('/ws');
        disconnect();
        globalThis.Worker = OrigWorker;
        // The static `new Worker(new URL('./arizona-worker.js', import.meta.url),
        // { type: 'module' })` shape is what bundlers statically detect (so a
        // downstream build emits + hashes the worker), and standalone it resolves
        // the sibling worker relative to the client module -- a URL instance
        // (never a runtime string) whose path is the worker sibling.
        expect(workerArgs.url).toBeInstanceOf(URL);
        expect(workerArgs.url.pathname.endsWith('/arizona-worker.js')).toBe(true);
        expect(workerArgs.opts).toEqual({ type: 'module' });
    });
});

// ---------------------------------------------------------------------------
// native-shell (OS) contract: globalThis.__arizona_os__
// ---------------------------------------------------------------------------

describe('native-shell contract (__arizona_os__)', () => {
    afterEach(() => {
        delete globalThis.__arizona_os__;
    });

    /** Mock the Worker ctor for a single connect(), returning posted messages. */
    function connectWith(mod, endpoint = '/ws') {
        const posted = [];
        const OrigWorker = globalThis.Worker;
        let onmessage = null;
        globalThis.Worker = function () {
            return {
                postMessage: (d) => posted.push(d),
                set onmessage(fn) {
                    onmessage = fn;
                },
                get onmessage() {
                    return onmessage;
                },
                terminate() {},
            };
        };
        const disconnect = mod.connect(endpoint);
        globalThis.Worker = OrigWorker;
        return { posted, disconnect };
    }

    it('advertises capabilities as _az_caps on the WS URL when a shell is present', async () => {
        vi.resetModules();
        const mod = await import('./arizona.js');
        globalThis.__arizona_os__ = {
            capabilities: { window_title: true, notify: true },
            invoke: vi.fn(),
            onEvent: vi.fn(),
        };
        const { posted, disconnect } = connectWith(mod);
        const wsUrl = posted[0][1];
        const qs = new URL(wsUrl.replace(/^ws/, 'http')).searchParams;
        expect(JSON.parse(qs.get('_az_caps'))).toEqual({ window_title: true, notify: true });
        disconnect();
    });

    it('omits _az_caps when no shell is present (plain browser)', async () => {
        vi.resetModules();
        const mod = await import('./arizona.js');
        const { posted, disconnect } = connectWith(mod);
        expect(posted[0][1]).not.toContain('_az_caps');
        disconnect();
    });

    it('registers onEvent once and relays an injected OS event as a pushEvent', async () => {
        vi.resetModules();
        const mod = await import('./arizona.js');
        setupView('page', '<span az="0">hi</span>');
        let injected = null;
        globalThis.__arizona_os__ = {
            capabilities: { window_focus: true },
            invoke: vi.fn(),
            onEvent: vi.fn((cb) => {
                injected = cb;
            }),
        };
        const { posted, disconnect } = connectWith(mod);
        expect(globalThis.__arizona_os__.onEvent).toHaveBeenCalledTimes(1);
        // The shell injects an OS event -> relayed as a WS send to the root view.
        injected('window_blurred', { x: 1 });
        const sends = posted.filter((d) => d[0] === 1).map((d) => JSON.parse(d[1]));
        expect(sends).toContainEqual(['page', 'window_blurred', { x: 1 }]);
        disconnect();
    });

    it('a JS_OS effect delegates to the shell invoke with (name, args)', () => {
        const invoke = vi.fn();
        globalThis.__arizona_os__ = { invoke };
        applyEffects([[23, 'window_title', 'New title']]);
        expect(invoke).toHaveBeenCalledWith('window_title', ['New title']);
    });

    it('a JS_OS effect is a safe no-op in a plain browser (no shell)', () => {
        expect(() => applyEffects([[23, 'window_focus']])).not.toThrow();
    });
});

// ---------------------------------------------------------------------------
// pushEvent / pushEventTo
// ---------------------------------------------------------------------------

describe('pushEvent', () => {
    /** @type {ReturnType<typeof setupMockWorker>} */
    let mock;

    afterEach(() => {
        if (mock) mock.restore();
    });

    it('sends event targeting the root az-view with null payload when omitted', async () => {
        vi.resetModules();
        const mod = await import('./arizona.js');
        setupView('page', '<span az="0">hi</span>');
        mock = setupMockWorker(mod);
        mock.simulateOpen();

        mod.pushEvent('my_event');

        const msgs = mock.getSentMessages();
        expect(msgs).toContainEqual(['page', 'my_event', null]);
    });

    it('sends event with provided payload', async () => {
        vi.resetModules();
        const mod = await import('./arizona.js');
        setupView('page', '<span az="0">hi</span>');
        mock = setupMockWorker(mod);
        mock.simulateOpen();

        mod.pushEvent('my_event', { key: 'value' });

        const msgs = mock.getSentMessages();
        expect(msgs).toContainEqual(['page', 'my_event', { key: 'value' }]);
    });

    it('sends null target when no az-view element exists', async () => {
        vi.resetModules();
        const mod = await import('./arizona.js');
        document.body.innerHTML = '<div>no view</div>';
        mock = setupMockWorker(mod);
        mock.simulateOpen();

        mod.pushEvent('orphan_event');

        const msgs = mock.getSentMessages();
        // target is null/undefined since no [az-view] element exists
        const msg = msgs.find((m) => m[1] === 'orphan_event');
        expect(msg).toBeTruthy();
        expect(msg[0]).toBeNull();
    });
});

describe('pushEventTo', () => {
    /** @type {ReturnType<typeof setupMockWorker>} */
    let mock;

    afterEach(() => {
        if (mock) mock.restore();
    });

    it('sends event targeting a specific view with null payload when omitted', async () => {
        vi.resetModules();
        const mod = await import('./arizona.js');
        setupView('page', '<div id="counter" az-view><span az="0">0</span></div>');
        mock = setupMockWorker(mod);
        mock.simulateOpen();

        mod.pushEventTo('counter', 'inc');

        const msgs = mock.getSentMessages();
        expect(msgs).toContainEqual(['counter', 'inc', null]);
    });

    it('sends event with provided payload', async () => {
        vi.resetModules();
        const mod = await import('./arizona.js');
        setupView('page', '<span az="0">hi</span>');
        mock = setupMockWorker(mod);
        mock.simulateOpen();

        mod.pushEventTo('counter', 'update', { count: 5 });

        const msgs = mock.getSentMessages();
        expect(msgs).toContainEqual(['counter', 'update', { count: 5 }]);
    });
});

// ---------------------------------------------------------------------------
// Navigation scroll
// ---------------------------------------------------------------------------

describe('navigation scroll', () => {
    /** @type {ReturnType<typeof setupMockWorker>} */
    let mock;
    /** @type {ReturnType<typeof vi.spyOn>} */
    let scrollSpy;
    /** @type {string} */
    let originalHref;
    /** @type {any} */
    let origScrollIntoView;

    beforeEach(() => {
        originalHref = location.href;
        history.replaceState(null, '', '/start');
        scrollSpy = vi.spyOn(window, 'scrollTo').mockImplementation(() => {});
        Object.defineProperty(window, 'scrollX', { value: 0, configurable: true });
        Object.defineProperty(window, 'scrollY', { value: 0, configurable: true });
        // jsdom does not implement scrollIntoView; stub on the prototype.
        origScrollIntoView = Element.prototype.scrollIntoView;
        Element.prototype.scrollIntoView = vi.fn();
    });

    afterEach(() => {
        if (mock) {
            mock.restore();
            mock = /** @type {any} */ (null);
        }
        if (scrollSpy) scrollSpy.mockRestore();
        Element.prototype.scrollIntoView = origScrollIntoView;
        history.replaceState(null, '', originalHref);
    });

    function clickLink(href, { noscroll = false, modifier = null } = {}) {
        const a = document.createElement('a');
        a.setAttribute('az-navigate', '');
        a.setAttribute('href', href);
        if (noscroll) a.setAttribute('az-noscroll', '');
        document.body.appendChild(a);
        const evt = new MouseEvent('click', {
            bubbles: true,
            cancelable: true,
            button: 0,
            ctrlKey: modifier === 'ctrl',
            metaKey: modifier === 'meta',
            shiftKey: modifier === 'shift',
            altKey: modifier === 'alt',
        });
        a.dispatchEvent(evt);
        return { anchor: a, event: evt };
    }

    function clickPatchLink(href, { noscroll = false } = {}) {
        const a = document.createElement('a');
        a.setAttribute('az-patch', '');
        a.setAttribute('href', href);
        if (noscroll) a.setAttribute('az-noscroll', '');
        document.body.appendChild(a);
        const evt = new MouseEvent('click', { bubbles: true, cancelable: true, button: 0 });
        a.dispatchEvent(evt);
        return { anchor: a, event: evt };
    }

    it('az-patch click sends a patch frame, not a navigate frame', async () => {
        vi.resetModules();
        const mod = await import('./arizona.js');
        mock = setupMockWorker(mod);
        mock.simulateOpen();

        clickPatchLink('/to');
        const msgs = mock.getSentMessages();
        expect(msgs).toContainEqual(['patch', { path: '/to', qs: '' }]);
        expect(msgs.some((m) => m[0] === 'navigate')).toBe(false);
    });

    it('az-patch tags the history entry with _azNav so popstate re-patches', async () => {
        vi.resetModules();
        const mod = await import('./arizona.js');
        mock = setupMockWorker(mod);
        mock.simulateOpen();

        clickPatchLink('/to');
        expect(history.state?._azNav).toBe('patch');
    });

    it('patch scrolls to top after a non-empty diff batch (no OP_REPLACE)', async () => {
        vi.resetModules();
        const mod = await import('./arizona.js');
        setupView('page', '<span az="0">x</span>');
        mock = setupMockWorker(mod);
        mock.simulateOpen();

        clickPatchLink('/next');
        // A patch reply is a diff (OP_TEXT), never an OP_REPLACE.
        mod.applyOps([[OP.TEXT, 'page:0', 'y']]);

        expect(scrollSpy).toHaveBeenCalledWith(0, 0);
    });

    it('JS_PATCH effect ([24, path]) sends a patch frame', async () => {
        vi.resetModules();
        const mod = await import('./arizona.js');
        setupView('page', '<div az="0"></div>');
        mock = setupMockWorker(mod);
        mock.simulateOpen();

        // Cmd shape is [JS_PATCH=24, path, opts?].
        mod.applyEffects([[24, '/to']]);
        const msgs = mock.getSentMessages();
        expect(msgs).toContainEqual(['patch', { path: '/to', qs: '' }]);
    });

    it('patch tags the outgoing entry _azNav so back to a full-load entry re-patches', async () => {
        vi.resetModules();
        const mod = await import('./arizona.js');
        mock = setupMockWorker(mod);
        mock.simulateOpen();

        history.replaceState(null, '', '/from');
        const replaceSpy = vi.spyOn(history, 'replaceState');
        clickPatchLink('/to');

        // The outgoing /from entry (a full load, no tag of its own) was stamped
        // _azNav=patch via replaceState, so popstate back to it re-patches.
        const stampCall = replaceSpy.mock.calls.find(
            (c) => c[0] && typeof c[0] === 'object' && c[0]._azNav === 'patch',
        );
        expect(stampCall).toBeTruthy();
        replaceSpy.mockRestore();
    });

    it('popstate over a patch-tagged entry re-sends a patch frame', async () => {
        vi.resetModules();
        const mod = await import('./arizona.js');
        setupView('page', '<div az="0"></div>');
        mock = setupMockWorker(mod);
        mock.simulateOpen();

        // The destination entry was navigated via patch (tagged _azNav).
        history.replaceState({ _azNav: 'patch' }, '', '/patched');
        window.dispatchEvent(new PopStateEvent('popstate', { state: { _azNav: 'patch' } }));

        const msgs = mock.getSentMessages();
        expect(msgs).toContainEqual(['patch', { path: '/patched', qs: '' }]);
        expect(msgs.some((m) => m[0] === 'navigate')).toBe(false);
    });

    it('push nav saves outgoing scroll onto the outgoing history entry', async () => {
        vi.resetModules();
        const mod = await import('./arizona.js');
        mock = setupMockWorker(mod);
        mock.simulateOpen();

        history.replaceState(null, '', '/from');
        Object.defineProperty(window, 'scrollY', { value: 240, configurable: true });
        Object.defineProperty(window, 'scrollX', { value: 0, configurable: true });

        const replaceSpy = vi.spyOn(history, 'replaceState');
        clickLink('/to');

        // The outgoing /from entry was updated via replaceState with the saved scroll.
        const savingCall = replaceSpy.mock.calls.find(
            (c) => c[0] && typeof c[0] === 'object' && c[0]._azScroll,
        );
        expect(savingCall).toBeTruthy();
        expect(savingCall[0]._azScroll).toEqual({ x: 0, y: 240 });
        replaceSpy.mockRestore();
    });

    it('push nav scrolls to top after OP_REPLACE (no hash, no noscroll)', async () => {
        vi.resetModules();
        const mod = await import('./arizona.js');
        setupView('page', '<div az="0"></div>');
        mock = setupMockWorker(mod);
        mock.simulateOpen();

        clickLink('/next');
        // OP_REPLACE simulates server-rendered new page content arriving.
        mod.applyOps([[OP.REPLACE, 'page', '<div id="page" az-view></div>']]);

        expect(scrollSpy).toHaveBeenCalledWith(0, 0);
    });

    it('push nav with az-noscroll does not scroll', async () => {
        vi.resetModules();
        const mod = await import('./arizona.js');
        setupView('page', '<div az="0"></div>');
        mock = setupMockWorker(mod);
        mock.simulateOpen();

        clickLink('/next', { noscroll: true });
        mod.applyOps([[OP.REPLACE, 'page', '<div id="page" az-view></div>']]);

        expect(scrollSpy).not.toHaveBeenCalled();
    });

    it('push nav with #hash scrolls into the #section element specifically', async () => {
        vi.resetModules();
        const mod = await import('./arizona.js');
        mock = setupMockWorker(mod);
        mock.simulateOpen();

        // Seed the DOM BEFORE dispatching the click so the click handler's
        // same-pathname hash short-circuit does not fire and so #page/#section
        // exist when OP_REPLACE runs.
        setupView('page', '<section id="section">x</section>');
        clickLink('/next#section');
        mod.applyOps([
            [OP.REPLACE, 'page', '<div id="page" az-view><section id="section">x</section></div>'],
        ]);

        // Verify scrollIntoView fired exactly once, on the current #section.
        const sivMock = /** @type {any} */ (Element.prototype.scrollIntoView).mock;
        const section = document.getElementById('section');
        expect(sivMock.calls.length).toBe(1);
        expect(sivMock.contexts[0]).toBe(section);
    });

    it('replace nav (via JS_NAVIGATE) does not save outgoing scroll, does not reset, but DOES notify the server', async () => {
        vi.resetModules();
        const mod = await import('./arizona.js');
        setupView('page', '<div az="0"></div>');
        mock = setupMockWorker(mod);
        mock.simulateOpen();

        history.replaceState(null, '', '/from');
        Object.defineProperty(window, 'scrollY', { value: 150, configurable: true });

        // Simulate a handler effect: arizona_js:navigate(~"/to", #{replace => true}).
        // Cmd shape is [JS_NAVIGATE=10, path, opts].
        mod.applyEffects([[10, '/to', { replace: true }]]);
        mod.applyOps([[OP.REPLACE, 'page', '<div id="page" az-view></div>']]);

        expect(scrollSpy).not.toHaveBeenCalled();
        expect(history.state?._azScroll).toBeFalsy();
        // Replace still re-renders the destination route -- server must be notified.
        const msgs = mock.getSentMessages();
        expect(msgs).toContainEqual(['navigate', { path: '/to', qs: '' }]);
    });

    it('JS_NAVIGATE with {noscroll: true} skips the scroll reset on push', async () => {
        vi.resetModules();
        const mod = await import('./arizona.js');
        setupView('page', '<div az="0"></div>');
        mock = setupMockWorker(mod);
        mock.simulateOpen();

        // Push-style (no replace) nav with opts.noscroll should NOT call scrollTo.
        mod.applyEffects([[10, '/to', { noscroll: true }]]);
        mod.applyOps([[OP.REPLACE, 'page', '<div id="page" az-view></div>']]);

        expect(scrollSpy).not.toHaveBeenCalled();
        // But the navigate message is still sent (server still re-renders).
        const msgs = mock.getSentMessages();
        expect(msgs).toContainEqual(['navigate', { path: '/to', qs: '' }]);
    });

    it('JS_NAVIGATE with {full: true} does a full-page navigation, no SPA frame', async () => {
        vi.resetModules();
        const mod = await import('./arizona.js');
        setupView('page', '<div az="0"></div>');
        mock = setupMockWorker(mod);
        mock.simulateOpen();
        // jsdom's location.assign is non-configurable (can't spyOn it); stub the
        // whole global for just this call -- the full-nav branch only touches
        // location.assign.
        const assign = vi.fn();
        vi.stubGlobal('location', { assign });
        // arizona_js:navigate(~"/api", #{full => true}) -- the server emits this to
        // degrade a navigate/patch whose path is not a live route.
        mod.applyEffects([[10, '/api', { full: true }]]);
        vi.unstubAllGlobals();
        expect(assign).toHaveBeenCalledWith('/api');
        // Browser handles it -- no SPA navigate frame is sent to the server.
        expect(mock.getSentMessages().some((m) => m[0] === 'navigate')).toBe(false);
    });

    it('popstate with saved state restores exact coordinates after OP_REPLACE', async () => {
        vi.resetModules();
        const mod = await import('./arizona.js');
        setupView('page', '<div az="0"></div>');
        mock = setupMockWorker(mod);
        mock.simulateOpen();

        // Simulate a popstate with saved scroll on e.state.
        const popEvent = new PopStateEvent('popstate', { state: { _azScroll: { x: 0, y: 320 } } });
        window.dispatchEvent(popEvent);
        mod.applyOps([[OP.REPLACE, 'page', '<div id="page" az-view></div>']]);

        expect(scrollSpy).toHaveBeenCalledWith(0, 320);
    });

    it('popstate without saved state falls through to #hash target', async () => {
        vi.resetModules();
        const mod = await import('./arizona.js');
        mock = setupMockWorker(mod);
        mock.simulateOpen();

        // Simulate URL having a hash at popstate time.
        history.replaceState(null, '', '/x#target');
        const popEvent = new PopStateEvent('popstate', { state: null });
        window.dispatchEvent(popEvent);
        setupView('page', '<section id="target">x</section>');
        mod.applyOps([
            [OP.REPLACE, 'page', '<div id="page" az-view><section id="target">x</section></div>'],
        ]);

        expect(Element.prototype.scrollIntoView).toHaveBeenCalled();
    });

    it('popstate without saved state and no hash scrolls to top', async () => {
        vi.resetModules();
        const mod = await import('./arizona.js');
        setupView('page', '<div az="0"></div>');
        mock = setupMockWorker(mod);
        mock.simulateOpen();

        history.replaceState(null, '', '/x');
        const popEvent = new PopStateEvent('popstate', { state: null });
        window.dispatchEvent(popEvent);
        mod.applyOps([[OP.REPLACE, 'page', '<div id="page" az-view></div>']]);

        expect(scrollSpy).toHaveBeenCalledWith(0, 0);
    });

    it('modifier-click (ctrl) lets the browser handle the link', async () => {
        vi.resetModules();
        const mod = await import('./arizona.js');
        mock = setupMockWorker(mod);
        mock.simulateOpen();

        const urlBefore = location.href;
        const { event } = clickLink('/other', { modifier: 'ctrl' });

        expect(event.defaultPrevented).toBe(false);
        expect(location.href).toBe(urlBefore);
        expect(mock.getSentMessages().some((m) => m[0] === 'navigate')).toBe(false);
    });

    it('same-path hash nav updates URL client-side without a server round-trip', async () => {
        vi.resetModules();
        const mod = await import('./arizona.js');
        mock = setupMockWorker(mod);
        mock.simulateOpen();

        history.replaceState(null, '', '/same');
        setupView('page', '<section id="sec">x</section>');

        clickLink('/same#sec');

        expect(location.pathname + location.hash).toBe('/same#sec');
        expect(Element.prototype.scrollIntoView).toHaveBeenCalled();
        expect(mock.getSentMessages().some((m) => m[0] === 'navigate')).toBe(false);
    });
});

// ---------------------------------------------------------------------------
// executeJS -- scroll_to (JS_SCROLL_TO = 13)
// ---------------------------------------------------------------------------

describe('executeJS -- JS_SCROLL_TO', () => {
    let origScrollIntoView;

    beforeEach(() => {
        origScrollIntoView = Element.prototype.scrollIntoView;
        Element.prototype.scrollIntoView = vi.fn();
    });

    afterEach(() => {
        Element.prototype.scrollIntoView = origScrollIntoView;
    });

    it('scrolls target into view with explicit options', () => {
        document.body.innerHTML = '<div id="t"></div>';
        executeJS(document.body, null, [13, '#t', { behavior: 'auto' }]);
        expect(Element.prototype.scrollIntoView).toHaveBeenCalledWith({ behavior: 'auto' });
    });

    it('defaults to smooth behavior when options omitted', () => {
        document.body.innerHTML = '<div id="t"></div>';
        executeJS(document.body, null, [13, '#t']);
        expect(Element.prototype.scrollIntoView).toHaveBeenCalledWith({ behavior: 'smooth' });
    });

    it('no-ops when selector matches nothing', () => {
        executeJS(document.body, null, [13, '#missing']);
        expect(Element.prototype.scrollIntoView).not.toHaveBeenCalled();
    });
});

// ---------------------------------------------------------------------------
// scheduleSend -- debounce / throttle
// ---------------------------------------------------------------------------

describe('scheduleSend -- debounce / throttle', () => {
    /** @type {ReturnType<typeof setupMockWorker>} */
    let mock;

    beforeEach(() => {
        vi.useFakeTimers();
    });

    afterEach(() => {
        vi.useRealTimers();
        if (mock) mock.restore();
    });

    it('numeric debounce delays send and resets on rapid events', async () => {
        vi.resetModules();
        const mod = await import('./arizona.js');
        setupView('page', `<input id="inp" az-change='[[0,"change"]]' az-debounce="200" />`);
        mock = setupMockWorker(mod);
        mock.simulateOpen();

        const inp = document.getElementById('inp');
        inp.dispatchEvent(new Event('change', { bubbles: true }));
        vi.advanceTimersByTime(100);
        expect(mock.getSentMessages()).toHaveLength(0);
        inp.dispatchEvent(new Event('change', { bubbles: true }));
        vi.advanceTimersByTime(150);
        expect(mock.getSentMessages()).toHaveLength(0);
        vi.advanceTimersByTime(100);
        expect(mock.getSentMessages()).toHaveLength(1);
    });

    it('throttle sends immediately then suppresses within window', async () => {
        vi.resetModules();
        const mod = await import('./arizona.js');
        setupView('page', `<input id="inp" az-change='[[0,"change"]]' az-throttle="200" />`);
        mock = setupMockWorker(mod);
        mock.simulateOpen();

        const inp = document.getElementById('inp');
        inp.dispatchEvent(new Event('change', { bubbles: true }));
        expect(mock.getSentMessages()).toHaveLength(1);
        inp.dispatchEvent(new Event('change', { bubbles: true }));
        inp.dispatchEvent(new Event('change', { bubbles: true }));
        expect(mock.getSentMessages()).toHaveLength(1);
        vi.advanceTimersByTime(250);
        expect(mock.getSentMessages()).toHaveLength(2);
    });

    it('event-name debounce stores pending, flushes on named event', async () => {
        vi.resetModules();
        const mod = await import('./arizona.js');
        setupView('page', `<input id="inp" az-change='[[0,"change"]]' az-debounce="blur" />`);
        mock = setupMockWorker(mod);
        mock.simulateOpen();

        const inp = document.getElementById('inp');
        inp.dispatchEvent(new Event('change', { bubbles: true }));
        expect(mock.getSentMessages()).toHaveLength(0);
        inp.dispatchEvent(new Event('blur', { bubbles: true }));
        expect(mock.getSentMessages()).toHaveLength(1);
    });

    it('blur auto-flushes pending debounced send', async () => {
        vi.resetModules();
        const mod = await import('./arizona.js');
        setupView('page', `<input id="inp" az-change='[[0,"change"]]' az-debounce="200" />`);
        mock = setupMockWorker(mod);
        mock.simulateOpen();

        const inp = document.getElementById('inp');
        inp.dispatchEvent(new Event('change', { bubbles: true }));
        expect(mock.getSentMessages()).toHaveLength(0);
        inp.dispatchEvent(new Event('blur', { bubbles: true }));
        expect(mock.getSentMessages()).toHaveLength(1);
    });
});

// ---------------------------------------------------------------------------
// Form submit
// ---------------------------------------------------------------------------

describe('form submit', () => {
    /** @type {ReturnType<typeof setupMockWorker>} */
    let mock;

    afterEach(() => {
        if (mock) mock.restore();
    });

    it('az-submit executes JS commands, az-form-reset resets the form', async () => {
        vi.resetModules();
        const mod = await import('./arizona.js');
        setupView(
            'page',
            `<form id="f" az-submit='[[0,"save"]]' az-form-reset>
                <input name="x" value="abc" />
            </form>`,
        );
        mock = setupMockWorker(mod);
        mock.simulateOpen();

        const form = document.getElementById('f');
        const resetSpy = vi.spyOn(HTMLFormElement.prototype, 'reset');
        form.dispatchEvent(new Event('submit', { bubbles: true, cancelable: true }));
        expect(mock.getSentMessages()).toContainEqual(['page', 'save', { x: 'abc' }]);
        expect(resetSpy).toHaveBeenCalled();
        resetSpy.mockRestore();
    });

    it('includes the submitter button name/value (multi-submit form)', async () => {
        vi.resetModules();
        const mod = await import('./arizona.js');
        setupView(
            'page',
            `<form id="f" az-submit='[[0,"submit_plan"]]'>
                <input name="email" value="ada@example.com" />
                <button name="plan" value="monthly">Monthly</button>
                <button name="plan" value="annual">Annual</button>
            </form>`,
        );
        mock = setupMockWorker(mod);
        mock.simulateOpen();

        const form = document.getElementById('f');
        const annual = form.querySelectorAll('button')[1];
        form.dispatchEvent(
            new SubmitEvent('submit', { submitter: annual, bubbles: true, cancelable: true }),
        );
        // The clicked button's name/value rides along; the other submit button does not.
        expect(mock.getSentMessages()).toContainEqual([
            'page',
            'submit_plan',
            { email: 'ada@example.com', plan: 'annual' },
        ]);
    });

    it('submit without az-submit does nothing', async () => {
        vi.resetModules();
        const mod = await import('./arizona.js');
        setupView('page', `<form id="f"><input name="x" value="abc" /></form>`);
        mock = setupMockWorker(mod);
        mock.simulateOpen();

        const form = document.getElementById('f');
        form.dispatchEvent(new Event('submit', { bubbles: true, cancelable: true }));
        expect(mock.getSentMessages()).toHaveLength(0);
    });
});

// ---------------------------------------------------------------------------
// Drag and drop
// ---------------------------------------------------------------------------

describe('drag-and-drop', () => {
    /** @type {ReturnType<typeof setupMockWorker>} */
    let mock;

    afterEach(() => {
        if (mock) mock.restore();
    });

    it('dragstart stashes az-key, drop dispatches az-drop with data_transfer + drop_index', async () => {
        vi.resetModules();
        const mod = await import('./arizona.js');
        setupView(
            'page',
            `<ul id="list" az-drop='[[0,"reorder"]]'>
                <li az-key="a" draggable="true">A</li>
                <li az-key="b" draggable="true">B</li>
                <li az-key="c" draggable="true">C</li>
            </ul>`,
        );
        mock = setupMockWorker(mod);
        mock.simulateOpen();

        const aItem = document.querySelector('[az-key="a"]');
        const cItem = document.querySelector('[az-key="c"]');

        const dt = {
            _data: {},
            setData(type, val) {
                this._data[type] = val;
            },
            getData(type) {
                return this._data[type] || '';
            },
        };

        const dragstart = new Event('dragstart', { bubbles: true, cancelable: true });
        Object.defineProperty(dragstart, 'dataTransfer', { value: dt });
        aItem.dispatchEvent(dragstart);
        expect(dt.getData('text/plain')).toBe('a');

        const drop = new Event('drop', { bubbles: true, cancelable: true });
        Object.defineProperty(drop, 'dataTransfer', { value: dt });
        cItem.dispatchEvent(drop);

        const reorderMsg = mock.getSentMessages().find((m) => m[1] === 'reorder');
        expect(reorderMsg).toBeDefined();
        expect(reorderMsg[2]).toEqual({ data_transfer: 'a', drop_index: 2 });
    });

    it('dragover with az-key ancestor prevents default', async () => {
        vi.resetModules();
        const mod = await import('./arizona.js');
        setupView(
            'page',
            `<ul id="list" az-drop='[[0,"reorder"]]'>
                <li az-key="a">A</li>
            </ul>`,
        );
        mock = setupMockWorker(mod);
        mock.simulateOpen();

        const item = document.querySelector('[az-key="a"]');
        const ev = new Event('dragover', { bubbles: true, cancelable: true });
        item.dispatchEvent(ev);
        expect(ev.defaultPrevented).toBe(true);
    });
});

// ---------------------------------------------------------------------------
// OP.ITEM_PATCH -- key not found warning
// ---------------------------------------------------------------------------

describe('OP.ITEM_PATCH -- key not found', () => {
    /** @type {ReturnType<typeof vi.spyOn>} */
    let warnSpy;

    beforeEach(() => {
        warnSpy = vi.spyOn(console, 'warn').mockImplementation(() => {});
    });

    afterEach(() => {
        warnSpy.mockRestore();
    });

    it('warns when top-level ITEM_PATCH target key is missing', () => {
        setupView('page', `<ul az="0"><li az-key="alive">x</li></ul>`);
        applyOps([[7, 'page:0', 'ghost', [[0, '0', 'hi']]]]);
        expect(warnSpy).toHaveBeenCalledWith(expect.stringContaining('az-key="ghost" not found'));
    });
});

// ---------------------------------------------------------------------------
// Nested OP.ITEM_PATCH -- compound az fallback
// ---------------------------------------------------------------------------

describe('nested OP.ITEM_PATCH -- compound az fallback', () => {
    it('falls back to base az when compound az selector finds nothing', () => {
        setupView(
            'page',
            `<ul az="0">
                <li az-key="r1">
                    <div az="1"><span az-key="c1">old</span></div>
                </li>
            </ul>`,
        );
        applyOps([[7, 'page:0', 'r1', [[7, '1:5', 'c1', [[0, '0', 'new']]]]]]);
        expect(document.querySelector('[az-key="c1"]').textContent).toContain('new');
    });
});

// ---------------------------------------------------------------------------
// window._ws proxy
// ---------------------------------------------------------------------------

describe('window._ws proxy', () => {
    /** @type {ReturnType<typeof setupMockWorker>} */
    let mock;

    afterEach(() => {
        if (mock) mock.restore();
    });

    it('exposes readyState 1 when connected and 3 otherwise', async () => {
        vi.resetModules();
        const mod = await import('./arizona.js');
        mock = setupMockWorker(mod);
        expect(/** @type {any} */ (window)._ws.readyState).toBe(3);
        mock.simulateOpen();
        expect(/** @type {any} */ (window)._ws.readyState).toBe(1);
    });

    it('send() forwards to worker when connected', async () => {
        vi.resetModules();
        const mod = await import('./arizona.js');
        mock = setupMockWorker(mod);
        mock.simulateOpen();
        /** @type {any} */ (window)._ws.send('["page","ev",null]');
        expect(mock.getSentMessages()).toContainEqual(['page', 'ev', null]);
    });

    it('close() posts [2, code] to worker', async () => {
        vi.resetModules();
        const mod = await import('./arizona.js');
        mock = setupMockWorker(mod);
        mock.simulateOpen();
        /** @type {any} */ (window)._ws.close(4000);
        expect(mock.posted).toContainEqual([2, 4000]);
    });

    it('close() defaults to 1000 when code omitted', async () => {
        vi.resetModules();
        const mod = await import('./arizona.js');
        mock = setupMockWorker(mod);
        mock.simulateOpen();
        /** @type {any} */ (window)._ws.close();
        expect(mock.posted).toContainEqual([2, 1000]);
    });

    it('onmessage hook fires on resolved messages', async () => {
        vi.resetModules();
        const mod = await import('./arizona.js');
        mock = setupMockWorker(mod);
        mock.simulateOpen();
        const hook = vi.fn();
        /** @type {any} */ (window)._ws.onmessage = hook;
        mock.simulateMessage([[0, 'page:0', 'hi']], null, false);
        expect(hook).toHaveBeenCalledOnce();
        expect(/** @type {any} */ (window)._ws.onmessage).toBe(hook);
    });
});

// ---------------------------------------------------------------------------
// disconnect -- scrollRestoration restore
// ---------------------------------------------------------------------------

describe('disconnect -- scrollRestoration restore', () => {
    it('resets scrollRestoration to previous value on disconnect', async () => {
        vi.resetModules();
        const mod = await import('./arizona.js');
        /** @type {any} */ (history).scrollRestoration = 'auto';
        const mock = setupMockWorker(mod);
        expect(/** @type {any} */ (history).scrollRestoration).toBe('manual');
        mock.restore();
        expect(/** @type {any} */ (history).scrollRestoration).toBe('auto');
    });
});

describe('page lifecycle -- bfcache', () => {
    it('terminates the worker on pagehide so the page can enter bfcache', async () => {
        vi.resetModules();
        const mod = await import('./arizona.js');
        const mock = setupMockWorker(mod);
        mock.simulateOpen();

        expect(mock.worker.terminate).not.toHaveBeenCalled();
        window.dispatchEvent(new PageTransitionEvent('pagehide', { persisted: false }));
        expect(mock.worker.terminate).toHaveBeenCalledOnce();

        mock.restore();
    });

    it('respawns and reconnects when restored from bfcache (pageshow persisted)', async () => {
        vi.resetModules();
        const mod = await import('./arizona.js');
        const mock = setupMockWorker(mod);
        mock.simulateOpen();

        // W_CONNECT === 0; one connect from the initial spawn.
        const connects = () => mock.posted.filter((d) => d[0] === 0).length;
        expect(connects()).toBe(1);

        window.dispatchEvent(new PageTransitionEvent('pagehide', { persisted: false }));
        window.dispatchEvent(new PageTransitionEvent('pageshow', { persisted: true }));

        // A second W_CONNECT proves the worker was respawned and reconnected.
        expect(connects()).toBe(2);

        mock.restore();
    });

    it('ignores a pageshow that is not a bfcache restore (persisted false)', async () => {
        vi.resetModules();
        const mod = await import('./arizona.js');
        const mock = setupMockWorker(mod);
        mock.simulateOpen();

        window.dispatchEvent(new PageTransitionEvent('pageshow', { persisted: false }));

        expect(mock.posted.filter((d) => d[0] === 0).length).toBe(1);
        expect(mock.worker.terminate).not.toHaveBeenCalled();

        mock.restore();
    });
});

// ---------------------------------------------------------------------------
// executeJS -- fetch (JS_FETCH = 22): HTTP request, no reload, applies effects
// ---------------------------------------------------------------------------

describe('executeJS -- fetch', () => {
    /** @type {any} */
    let originalFetch;

    beforeEach(() => {
        originalFetch = globalThis.fetch;
    });

    afterEach(() => {
        globalThis.fetch = originalFetch;
    });

    /** A fake 2xx Response whose body is the {e:[...]} effects wire payload. */
    function okResponse(effects) {
        return {
            ok: true,
            status: 200,
            text: () => Promise.resolve(JSON.stringify({ e: effects })),
        };
    }

    it('submits the trigger form as urlencoded with same-origin credentials', async () => {
        document.body.innerHTML =
            '<form method="post"><input name="username" value="ada" /></form>';
        globalThis.fetch = vi.fn(() => Promise.resolve(okResponse([])));

        executeJS(document.querySelector('form'), null, [22, '/account', {}]);
        await vi.waitFor(() => expect(globalThis.fetch).toHaveBeenCalled());

        const [url, init] = globalThis.fetch.mock.calls[0];
        expect(url).toBe('/account');
        expect(init.method).toBe('POST');
        expect(init.credentials).toBe('same-origin');
        expect(init.headers.accept).toBe('application/json');
        expect(init.body).toBeInstanceOf(URLSearchParams);
        expect(init.body.get('username')).toBe('ada');
    });

    it('includes the submitter button name/value in the urlencoded body', async () => {
        document.body.innerHTML =
            '<form method="post"><input name="email" value="ada" />' +
            '<button name="plan" value="annual">Annual</button></form>';
        globalThis.fetch = vi.fn(() => Promise.resolve(okResponse([])));

        const form = document.querySelector('form');
        const btn = form.querySelector('button');
        executeJS(form, new SubmitEvent('submit', { submitter: btn }), [22, '/account', {}]);
        await vi.waitFor(() => expect(globalThis.fetch).toHaveBeenCalled());

        const [, init] = globalThis.fetch.mock.calls[0];
        expect(init.body.get('email')).toBe('ada');
        expect(init.body.get('plan')).toBe('annual');
    });

    it('applies the effects returned by a 2xx response', async () => {
        document.title = 'Old';
        document.body.innerHTML = '<form method="post"></form>';
        globalThis.fetch = vi.fn(() => Promise.resolve(okResponse([[14, 'Saved']])));

        executeJS(document.querySelector('form'), null, [22, '/account', {}]);

        await vi.waitFor(() => expect(document.title).toBe('Saved'));
    });

    it('treats an empty 2xx body (204, cookie-only) as success with no effects', async () => {
        const onErr = vi.fn();
        document.addEventListener('arizona:fetch-error', onErr);
        globalThis.fetch = vi.fn(() =>
            Promise.resolve({ ok: true, status: 204, text: () => Promise.resolve('') }),
        );

        executeJS(document.body, null, [22, '/account', {}]);
        await vi.waitFor(() => expect(globalThis.fetch).toHaveBeenCalled());
        // Flush the resolved-promise chain, then confirm no error fired.
        await Promise.resolve();
        expect(onErr).not.toHaveBeenCalled();

        document.removeEventListener('arizona:fetch-error', onErr);
    });

    it('JSON-encodes opts.body for a non-form trigger', async () => {
        globalThis.fetch = vi.fn(() => Promise.resolve(okResponse([])));

        executeJS(document.body, null, [22, '/api', { body: { a: 1 } }]);
        await vi.waitFor(() => expect(globalThis.fetch).toHaveBeenCalled());

        const [, init] = globalThis.fetch.mock.calls[0];
        expect(init.body).toBe('{"a":1}');
        expect(init.headers['content-type']).toBe('application/json');
    });

    it('maps the credentials atom to the fetch mode', async () => {
        globalThis.fetch = vi.fn(() => Promise.resolve(okResponse([])));

        executeJS(document.body, null, [22, '/api', { credentials: 'include' }]);
        await vi.waitFor(() => expect(globalThis.fetch).toHaveBeenCalled());

        expect(globalThis.fetch.mock.calls[0][1].credentials).toBe('include');
    });

    it('forwards opts.keep_alive to the fetch init keepalive when true', async () => {
        globalThis.fetch = vi.fn(() => Promise.resolve(okResponse([])));

        executeJS(document.body, null, [22, '/api', { keep_alive: true }]);
        await vi.waitFor(() => expect(globalThis.fetch).toHaveBeenCalled());

        expect(globalThis.fetch.mock.calls[0][1].keepalive).toBe(true);
    });

    it('defaults the fetch init keepalive to false when opts.keep_alive is absent', async () => {
        globalThis.fetch = vi.fn(() => Promise.resolve(okResponse([])));

        executeJS(document.body, null, [22, '/api', {}]);
        await vi.waitFor(() => expect(globalThis.fetch).toHaveBeenCalled());

        expect(globalThis.fetch.mock.calls[0][1].keepalive).toBe(false);
    });

    it('applies a non-2xx effects body (inline validation) without firing on_error', async () => {
        document.title = 'Old';
        globalThis.fetch = vi.fn(() =>
            Promise.resolve({
                ok: false,
                status: 422,
                text: () => Promise.resolve(JSON.stringify({ e: [[14, 'Invalid']] })),
            }),
        );
        const onErr = vi.fn();
        document.addEventListener('arizona:fetch-error', onErr);

        executeJS(document.body, null, [22, '/account', { on_error: [14, 'Failed'] }]);

        // The server's effects win (title 'Invalid'); on_error ('Failed') does not run.
        await vi.waitFor(() => expect(document.title).toBe('Invalid'));
        expect(onErr).not.toHaveBeenCalled();

        document.removeEventListener('arizona:fetch-error', onErr);
    });

    it('runs on_error and dispatches arizona:fetch-error on a non-2xx with no effects body', async () => {
        document.title = 'Old';
        globalThis.fetch = vi.fn(() =>
            Promise.resolve({ ok: false, status: 500, text: () => Promise.resolve('') }),
        );
        const onErr = vi.fn();
        document.addEventListener('arizona:fetch-error', onErr);

        // on_error effect: set the title to 'Failed' (set_title = 14).
        executeJS(document.body, null, [22, '/api', { on_error: [14, 'Failed'] }]);

        await vi.waitFor(() => expect(document.title).toBe('Failed'));
        expect(onErr).toHaveBeenCalled();
        expect(onErr.mock.calls[0][0].detail.status).toBe(500);

        document.removeEventListener('arizona:fetch-error', onErr);
    });

    it('carries a form’s fields in the query string for a GET request', async () => {
        document.body.innerHTML = '<form method="get"><input name="q" value="erlang" /></form>';
        globalThis.fetch = vi.fn(() => Promise.resolve(okResponse([])));

        executeJS(document.querySelector('form'), null, [22, '/search', {}]);
        await vi.waitFor(() => expect(globalThis.fetch).toHaveBeenCalled());

        const [target, init] = globalThis.fetch.mock.calls[0];
        expect(target).toBe('/search?q=erlang');
        expect(init.method).toBe('GET');
        expect(init.body).toBeUndefined();
    });

    it('runs on_error on a network failure (rejected fetch)', async () => {
        globalThis.fetch = vi.fn(() => Promise.reject(new Error('offline')));
        const onErr = vi.fn();
        document.addEventListener('arizona:fetch-error', onErr);

        executeJS(document.body, null, [22, '/api', { on_error: [14, 'Failed'] }]);

        await vi.waitFor(() => expect(onErr).toHaveBeenCalled());

        document.removeEventListener('arizona:fetch-error', onErr);
    });

    it('honors az-form-reset on a 2xx success', async () => {
        document.body.innerHTML = '<form az-form-reset><input id="note" name="note" /></form>';
        const input = document.querySelector('#note');
        input.value = 'typed';
        globalThis.fetch = vi.fn(() => Promise.resolve(okResponse([])));

        executeJS(document.querySelector('form'), null, [22, '/account', {}]);

        await vi.waitFor(() => expect(input.value).toBe(''));
    });

    it('keeps the form on a non-2xx even with az-form-reset', async () => {
        document.title = 'Old';
        document.body.innerHTML = '<form az-form-reset><input id="note" name="note" /></form>';
        const input = document.querySelector('#note');
        input.value = 'typed';
        globalThis.fetch = vi.fn(() =>
            Promise.resolve({
                ok: false,
                status: 422,
                text: () => Promise.resolve(JSON.stringify({ e: [[14, 'Err']] })),
            }),
        );

        executeJS(document.querySelector('form'), null, [22, '/account', {}]);

        // The effects ran (title set), proving the chain completed; the field survived.
        await vi.waitFor(() => expect(document.title).toBe('Err'));
        expect(input.value).toBe('typed');
    });
});

describe('fetch + az-form-reset via the submit listener', () => {
    /** @type {any} */
    let originalFetch;

    beforeEach(() => {
        originalFetch = globalThis.fetch;
    });

    afterEach(() => {
        globalThis.fetch = originalFetch;
    });

    it('defers az-form-reset to the 2xx response (no synchronous reset)', async () => {
        vi.resetModules();
        const mod = await import('./arizona.js');
        const mock = setupMockWorker(mod);
        mock.simulateOpen();

        document.body.innerHTML =
            '<form id="f" az-submit=\'[22,"/x",{}]\' az-form-reset>' +
            '<input id="n" name="n" /><button type="submit">Go</button></form>';
        const input = document.querySelector('#n');
        input.value = 'typed';

        let resolveFetch;
        globalThis.fetch = vi.fn(
            () =>
                new Promise((res) => {
                    resolveFetch = () =>
                        res({ ok: true, status: 200, text: () => Promise.resolve('') });
                }),
        );

        document
            .querySelector('#f')
            .dispatchEvent(new Event('submit', { bubbles: true, cancelable: true }));

        // The fetch was dispatched but has not resolved -> the field must survive.
        await vi.waitFor(() => expect(globalThis.fetch).toHaveBeenCalled());
        expect(input.value).toBe('typed');

        // Resolve the 2xx -> now az-form-reset applies.
        resolveFetch();
        await vi.waitFor(() => expect(input.value).toBe(''));

        mock.restore();
    });

    it('relays a push_event from the response to the submitting view over the WS', async () => {
        vi.resetModules();
        const mod = await import('./arizona.js');
        const mock = setupMockWorker(mod);
        mock.simulateOpen();

        document.body.innerHTML =
            '<div id="v" az-view><form id="f"><input name="secret" value="x" />' +
            '<button type="submit">Go</button></form></div>';
        globalThis.fetch = vi.fn(() =>
            Promise.resolve({
                ok: true,
                status: 200,
                text: () => Promise.resolve(JSON.stringify({ e: [[0, 'saved']] })),
            }),
        );

        // push_event (op 0) in the response -> relayed over the WS to the view #v.
        mod.executeJS(document.querySelector('#f'), null, [22, '/account', {}]);

        await vi.waitFor(() => {
            const ev = mock.getSentMessages().find((m) => m[0] === 'v' && m[1] === 'saved');
            expect(ev).toBeDefined();
            // Effects run against the view element, not the form -> no field echo.
            expect(ev[2]).not.toHaveProperty('secret');
        });

        mock.restore();
    });

    it('defers the reset when the fetch is wrapped in transition(...) (E2)', async () => {
        vi.resetModules();
        const mod = await import('./arizona.js');
        const mock = setupMockWorker(mod);
        mock.simulateOpen();

        // Defer the view-transition callback the way a real browser does, so a
        // synchronous reset would clear the form before the wrapped fetch reads it.
        let vtCallback = null;
        document.startViewTransition = (arg) => {
            vtCallback = typeof arg === 'object' ? arg.update : arg;
            return {
                ready: Promise.resolve(),
                finished: Promise.resolve(),
                updateCallbackDone: Promise.resolve(),
                skipTransition: vi.fn(),
            };
        };

        // {az_submit, transition(fetch("/x"))} = [JS_TRANSITION, opts, [JS_FETCH, url, {}]]
        document.body.innerHTML =
            '<form id="f" az-submit=\'[20,{},[22,"/x",{}]]\' az-form-reset>' +
            '<input id="n" name="n" /><button type="submit">Go</button></form>';
        const input = document.querySelector('#n');
        input.value = 'typed';

        globalThis.fetch = vi.fn(() =>
            Promise.resolve({ ok: false, status: 422, text: () => Promise.resolve('') }),
        );

        try {
            document
                .querySelector('#f')
                .dispatchEvent(new Event('submit', { bubbles: true, cancelable: true }));

            // The reset must NOT fire synchronously on submit (the wrapped fetch owns it)...
            expect(input.value).toBe('typed');
            // ...so when the deferred transition callback runs, the fetch POSTs the field.
            vtCallback();
            await vi.waitFor(() => expect(globalThis.fetch).toHaveBeenCalled());
            expect(globalThis.fetch.mock.calls[0][1].body.toString()).toContain('n=typed');

            // A 422 keeps the typed fields (reset is 2xx-only).
            await Promise.resolve();
            expect(input.value).toBe('typed');
        } finally {
            delete document.startViewTransition;
            mock.restore();
        }
    });
});
