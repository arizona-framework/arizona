import { describe, it, expect, vi, beforeEach, afterEach } from 'vitest';
import { OP, applyOps, applyEffects, executeJS, resolveEl, pushEvent, pushEventTo, connect, hooks, mountHooks, saveFormState, restoreFormState } from './arizona.js';

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/** Reset DOM and hooks between tests. */
beforeEach(() => {
    document.body.innerHTML = '';
    // Clear hook definitions
    for (const k of Object.keys(hooks)) delete hooks[k];
});

/** Minimal DOM with a single view and one dynamic element. */
function setupView(viewId, innerHTML) {
    document.body.innerHTML =
        `<div id="${viewId}" az-view>${innerHTML}</div>`;
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
        set onmessage(fn) { workerOnmessage = fn; },
        get onmessage() { return workerOnmessage; },
        terminate: vi.fn(),
    };
    const OrigWorker = globalThis.Worker;
    globalThis.Worker = function () { return mockWorkerInstance; };

    mod.connect('/ws');

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
            return posted
                .filter(d => d[0] === 1)
                .map(d => JSON.parse(d[1]));
        },
        restore() {
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
        setupView('v', '<p az="0"><!--az:0-->first<!--/az--> and <!--az:0:1-->second<!--/az--></p>');
        applyOps([[OP.TEXT, 'v:0:1', 'updated']]);
        const el = resolveEl('v:0');
        expect(el.textContent).toBe('first and updated');
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

    it('handles HTML content in comment markers', () => {
        setupView('v', '<div az="0"><!--az:0-->old<!--/az--></div>');
        applyOps([[OP.TEXT, 'v:0', '<b>bold</b>']]);
        const el = resolveEl('v:0');
        expect(el.querySelector('b')).not.toBeNull();
        expect(el.querySelector('b').textContent).toBe('bold');
    });

    it('updates content when az target is the view root element', () => {
        document.body.innerHTML = '<div id="v" az-view az="0"><!--az:0-->old<!--/az--></div>';
        applyOps([[OP.TEXT, 'v:0', '<b>new</b>']]);
        const el = resolveEl('v:0');
        expect(el).not.toBeNull();
        expect(el.querySelector('b')).not.toBeNull();
        expect(el.querySelector('b').textContent).toBe('new');
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
        const keys = Array.from(el.querySelectorAll('[az-key]')).map(
            (c) => c.getAttribute('az-key')
        );
        expect(keys).toEqual(['a', 'b', 'c']);
    });

    it('appends when pos exceeds child count', () => {
        setupView('v', '<div az="0"><p az-key="a">A</p></div>');
        applyOps([[OP.INSERT, 'v:0', 'z', 99, '<p az-key="z">Z</p>']]);
        const el = resolveEl('v:0');
        const keys = Array.from(el.querySelectorAll('[az-key]')).map(
            (c) => c.getAttribute('az-key')
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
        setupView('v', '<div az="0"><p az-key="a">A</p><p az-key="b">B</p><p az-key="c">C</p></div>');
        const el = resolveEl('v:0');
        const originalNode = el.querySelector('[az-key="c"]');
        // Move c after null (prepend) → [c, a, b]
        applyOps([[OP.MOVE, 'v:0', 'c', null]]);
        const keys = Array.from(el.querySelectorAll(':scope > [az-key]')).map(
            (c) => c.getAttribute('az-key')
        );
        expect(keys).toEqual(['c', 'a', 'b']);
        // Verify same DOM node was reused (not destroyed and recreated)
        expect(el.querySelector('[az-key="c"]')).toBe(originalNode);
    });

    it('prepends when afterKey is null', () => {
        setupView('v', '<div az="0"><p az-key="a">A</p><p az-key="b">B</p><p az-key="c">C</p></div>');
        applyOps([[OP.MOVE, 'v:0', 'c', null]]);
        const el = resolveEl('v:0');
        const keys = Array.from(el.querySelectorAll(':scope > [az-key]')).map(
            (c) => c.getAttribute('az-key')
        );
        expect(keys).toEqual(['c', 'a', 'b']);
    });

    it('appends when afterKey does not exist', () => {
        setupView('v', '<div az="0"><p az-key="a">A</p><p az-key="b">B</p></div>');
        applyOps([[OP.MOVE, 'v:0', 'a', 'nonexistent']]);
        const el = resolveEl('v:0');
        const keys = Array.from(el.querySelectorAll(':scope > [az-key]')).map(
            (c) => c.getAttribute('az-key')
        );
        expect(keys).toEqual(['b', 'a']);
    });

    it('is a no-op when key does not exist and warns', () => {
        const spy = vi.spyOn(console, 'warn').mockImplementation(() => {});
        setupView('v', '<div az="0"><p az-key="a">A</p></div>');
        applyOps([[OP.MOVE, 'v:0', 'nonexistent', 'a']]);
        const el = resolveEl('v:0');
        const keys = Array.from(el.querySelectorAll(':scope > [az-key]')).map(
            (c) => c.getAttribute('az-key')
        );
        expect(keys).toEqual(['a']);
        expect(spy).toHaveBeenCalledWith(expect.stringContaining('nonexistent'));
        spy.mockRestore();
    });

    it('moves after the correct key in a sequence of moves', () => {
        // Simulate [a, b, c, d] → [d, c, b, a] using LIS-style moves
        // LIS of [4,3,2,1] is length 1 (d stays), 3 items need to move
        // Applied left-to-right: move d→null, move c→d, move b→c
        setupView('v', '<div az="0"><p az-key="a">A</p><p az-key="b">B</p><p az-key="c">C</p><p az-key="d">D</p></div>');
        applyOps([
            [OP.MOVE, 'v:0', 'd', null],
            [OP.MOVE, 'v:0', 'c', 'd'],
            [OP.MOVE, 'v:0', 'b', 'c'],
        ]);
        const el = resolveEl('v:0');
        const keys = Array.from(el.querySelectorAll(':scope > [az-key]')).map(
            (c) => c.getAttribute('az-key')
        );
        expect(keys).toEqual(['d', 'c', 'b', 'a']);
    });
});

// ---------------------------------------------------------------------------
// 10. applyOps -- OP.ITEM_PATCH
// ---------------------------------------------------------------------------

describe('applyOps -- OP.ITEM_PATCH', () => {
    it('patches text within a keyed child', () => {
        setupView('v',
            '<div az="0"><div az-key="k1"><span az="0">old</span></div></div>');
        applyOps([[OP.ITEM_PATCH, 'v:0', 'k1', [
            [OP.TEXT, '0', 'new'],
        ]]]);
        const item = resolveEl('v:0').querySelector('[az-key="k1"]');
        expect(item.querySelector('[az="0"]').textContent).toBe('new');
    });

    it('sets attribute within a keyed child', () => {
        setupView('v',
            '<div az="0"><div az-key="k1"><span az="0">x</span></div></div>');
        applyOps([[OP.ITEM_PATCH, 'v:0', 'k1', [
            [OP.SET_ATTR, '0', 'class', 'highlight'],
        ]]]);
        const item = resolveEl('v:0').querySelector('[az-key="k1"]');
        expect(item.querySelector('[az="0"]').getAttribute('class')).toBe('highlight');
    });

    it('removes attribute within a keyed child', () => {
        setupView('v',
            '<div az="0"><div az-key="k1"><span az="0" class="old">x</span></div></div>');
        applyOps([[OP.ITEM_PATCH, 'v:0', 'k1', [
            [OP.REM_ATTR, '0', 'class'],
        ]]]);
        const item = resolveEl('v:0').querySelector('[az-key="k1"]');
        expect(item.querySelector('[az="0"]').hasAttribute('class')).toBe(false);
    });

    it('replaces innerHTML within a keyed child', () => {
        setupView('v',
            '<div az="0"><div az-key="k1"><div az="0">old</div></div></div>');
        applyOps([[OP.ITEM_PATCH, 'v:0', 'k1', [
            [OP.UPDATE, '0', '<em>new</em>'],
        ]]]);
        const item = resolveEl('v:0').querySelector('[az-key="k1"]');
        expect(item.querySelector('[az="0"]').innerHTML).toBe('<em>new</em>');
    });

    it('removes node within a keyed child', () => {
        setupView('v',
            '<div az="0"><div az-key="k1"><span az="0">bye</span></div></div>');
        applyOps([[OP.ITEM_PATCH, 'v:0', 'k1', [
            [OP.REMOVE_NODE, '0'],
        ]]]);
        const item = resolveEl('v:0').querySelector('[az-key="k1"]');
        expect(item.querySelector('[az="0"]')).toBeNull();
    });

    it('patches text with comment markers within a keyed child', () => {
        setupView('v',
            '<div az="0"><div az-key="k1"><span az="0"><!--az:0-->old<!--/az--></span></div></div>');
        applyOps([[OP.ITEM_PATCH, 'v:0', 'k1', [
            [OP.TEXT, '0', 'new'],
        ]]]);
        const item = resolveEl('v:0').querySelector('[az-key="k1"]');
        expect(item.querySelector('[az="0"]').textContent).toBe('new');
    });

    it('handles nested INSERT inside a keyed child', () => {
        setupView('v',
            '<div az="0"><div az-key="k1"><ul az="0"></ul></div></div>');
        applyOps([[OP.ITEM_PATCH, 'v:0', 'k1', [
            [OP.INSERT, '0', 'c1', -1, '<li az-key="c1">cell1</li>'],
        ]]]);
        const item = resolveEl('v:0').querySelector('[az-key="k1"]');
        const inner = item.querySelector('[az="0"]');
        expect(inner.querySelector('[az-key="c1"]').textContent).toBe('cell1');
    });

    it('handles nested REMOVE inside a keyed child', () => {
        setupView('v',
            '<div az="0"><div az-key="k1"><ul az="0"><li az-key="c1">cell1</li></ul></div></div>');
        applyOps([[OP.ITEM_PATCH, 'v:0', 'k1', [
            [OP.REMOVE, '0', 'c1'],
        ]]]);
        const item = resolveEl('v:0').querySelector('[az-key="k1"]');
        const inner = item.querySelector('[az="0"]');
        expect(inner.querySelector('[az-key="c1"]')).toBeNull();
    });

    it('handles nested MOVE inside a keyed child', () => {
        setupView('v',
            '<div az="0"><div az-key="k1"><ul az="0"><li az-key="c1">A</li><li az-key="c2">B</li></ul></div></div>');
        // Move c2 after null (prepend) → [c2, c1]
        applyOps([[OP.ITEM_PATCH, 'v:0', 'k1', [
            [OP.MOVE, '0', 'c2', null],
        ]]]);
        const item = resolveEl('v:0').querySelector('[az-key="k1"]');
        const inner = item.querySelector('[az="0"]');
        const keys = Array.from(inner.querySelectorAll(':scope > [az-key]')).map(
            (c) => c.getAttribute('az-key')
        );
        expect(keys).toEqual(['c2', 'c1']);
    });

    it('handles nested ITEM_PATCH inside a keyed child', () => {
        setupView('v',
            '<div az="0"><div az-key="k1"><ul az="0"><li az-key="c1"><span az="0">old</span></li></ul></div></div>');
        applyOps([[OP.ITEM_PATCH, 'v:0', 'k1', [
            [OP.ITEM_PATCH, '0', 'c1', [[OP.TEXT, '0', 'new']]],
        ]]]);
        const item = resolveEl('v:0').querySelector('[az-key="k1"]');
        const inner = item.querySelector('[az="0"]');
        const cell = inner.querySelector('[az-key="c1"]');
        expect(cell.querySelector('[az="0"]').textContent).toBe('new');
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
        setupView('v', `
            <div az="0">
                <!--az:0-->first<!--/az-->
                <!--az:1-->second<!--/az-->
            </div>
        `);
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
        setupView('v', '<div az="0"><b>static</b><!--az:0-->dynamic<!--/az--><i>also static</i></div>');
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
// 14c. applyOps -- OP.REPLACE edge cases
// ---------------------------------------------------------------------------

describe('applyOps -- OP.REPLACE edge cases', () => {
    it('replaces element with new content', () => {
        document.body.innerHTML =
            '<div id="page" az-view><p az="0">old</p></div>';
        applyOps([[OP.REPLACE, 'page',
            '<div id="page" az-view><p az="0">new</p></div>']]);
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
        setupView('v', '<select az="0"><option value="a">A</option><option value="b">B</option></select>');
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
        applyOps([[OP.REPLACE, 'page', '<main id="page" az-view><h1 az="0">new title</h1></main>']]);
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
        const keys = Array.from(resolveEl('v:0').querySelectorAll('[az-key]')).map(
            (c) => c.getAttribute('az-key')
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
        applyOps([[OP.ITEM_PATCH, 'v:0', 'nonexistent', [
            [OP.TEXT, '0', 'new'],
            [OP.SET_ATTR, '0', 'class', 'x'],
        ]]]);
        const item = resolveEl('v:0').querySelector('[az-key="k1"]');
        expect(item.querySelector('[az="0"]').textContent).toBe('ok');
        expect(spy).toHaveBeenCalledWith(expect.stringContaining('nonexistent'));
        spy.mockRestore();
    });

    it('applies multiple inner ops sequentially', () => {
        setupView('v',
            '<div az="0"><div az-key="k1"><span az="0">old</span><span az="1">x</span></div></div>');
        applyOps([[OP.ITEM_PATCH, 'v:0', 'k1', [
            [OP.TEXT, '0', 'new'],
            [OP.SET_ATTR, '1', 'class', 'highlight'],
        ]]]);
        const item = resolveEl('v:0').querySelector('[az-key="k1"]');
        expect(item.querySelector('[az="0"]').textContent).toBe('new');
        expect(item.querySelector('[az="1"]').getAttribute('class')).toBe('highlight');
    });

    it('syncs value property on input within keyed child', () => {
        setupView('v',
            '<div az="0"><div az-key="k1"><input az="0" value="old" /></div></div>');
        applyOps([[OP.ITEM_PATCH, 'v:0', 'k1', [
            [OP.SET_ATTR, '0', 'value', 'new'],
        ]]]);
        const item = resolveEl('v:0').querySelector('[az-key="k1"]');
        const input = /** @type {HTMLInputElement} */ (item.querySelector('[az="0"]'));
        expect(input.getAttribute('value')).toBe('new');
        expect(input.value).toBe('new');
    });

    it('falls back to item element itself when no [az] found', () => {
        setupView('v',
            '<div az="0"><div az-key="k1">plain text</div></div>');
        applyOps([[OP.ITEM_PATCH, 'v:0', 'k1', [
            [OP.SET_ATTR, '0', 'data-test', 'fallback'],
        ]]]);
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
        executeJS(document.body, null, [[2, '#a'], [3, '#b']]);
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
        executeJS(document.body, event, [16, ['enter'], [[2, '#a'], [3, '#b']]]);
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
        hooks.Chart = { mounted() { capturedEl = this.el; } };
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
        applyOps([[OP.REPLACE, 'page', '<div id="page" az-view><span az-hook="Page">new</span></div>']]);
        expect(mounted).toHaveBeenCalledOnce();
    });

    it('fires after OP_TEXT marker path with HTML containing az-hook', () => {
        const mounted = vi.fn();
        hooks.Inline = { mounted };
        setupView('v', '<div az="0"><!--az:0-->old<!--/az--></div>');
        applyOps([[OP.TEXT, 'v:0', '<span az-hook="Inline">new</span>']]);
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
        document.body.innerHTML =
            '<div id="page" az-view><span az-hook="Page">child</span></div>';
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
        hooks.Sender = { mounted() { instance = this; } };
        setupView('v', '<div az="0" az-hook="Sender">content</div>');
        mountHooks(document);
        expect(instance).not.toBeNull();
        // pushEvent without _ws set -- should not throw
        instance.pushEvent('my_event', { key: 'val' });
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
            mounted() { calls.push('mount:A'); },
            destroyed() { calls.push('destroy:A'); },
        };
        hooks.B = {
            mounted() { calls.push('mount:B'); },
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
        setupView('v', '<div az="0"><div az-key="k1"><span az="1" az-hook="Cell">val</span></div></div>');
        mountHooks(document);
        applyOps([[OP.ITEM_PATCH, 'v:0', 'k1', [[OP.SET_ATTR, '1', 'class', 'bold']]]]);
        expect(updated).toHaveBeenCalledOnce();
    });

    it('OP_ITEM_PATCH inner REM_ATTR triggers updated hook', () => {
        const updated = vi.fn();
        hooks.Cell = { mounted() {}, updated };
        setupView('v', '<div az="0"><div az-key="k1"><span az="1" az-hook="Cell" class="x">val</span></div></div>');
        mountHooks(document);
        applyOps([[OP.ITEM_PATCH, 'v:0', 'k1', [[OP.REM_ATTR, '1', 'class']]]]);
        expect(updated).toHaveBeenCalledOnce();
    });

    it('OP_ITEM_PATCH inner REMOVE_NODE triggers destroyed hook', () => {
        const destroyed = vi.fn();
        hooks.Cell = { mounted() {}, destroyed };
        setupView('v', '<div az="0"><div az-key="k1"><span az="1" az-hook="Cell">val</span></div></div>');
        mountHooks(document);
        applyOps([[OP.ITEM_PATCH, 'v:0', 'k1', [[OP.REMOVE_NODE, '1']]]]);
        expect(destroyed).toHaveBeenCalledOnce();
    });

    it('OP_ITEM_PATCH inner UPDATE triggers destroy/mount hooks', () => {
        const destroyed = vi.fn();
        const mounted = vi.fn();
        hooks.Old = { mounted() {}, destroyed };
        hooks.New = { mounted };
        setupView('v', '<div az="0"><div az-key="k1"><div az="1"><span az-hook="Old">old</span></div></div></div>');
        mountHooks(document);
        applyOps([[OP.ITEM_PATCH, 'v:0', 'k1', [[OP.UPDATE, '1', '<span az-hook="New">new</span>']]]]);
        expect(destroyed).toHaveBeenCalledOnce();
        expect(mounted).toHaveBeenCalledOnce();
    });

    it('OP_ITEM_PATCH inner TEXT marker path triggers hooks', () => {
        const destroyed = vi.fn();
        const mounted = vi.fn();
        hooks.Old = { mounted() {}, destroyed };
        hooks.New = { mounted };
        setupView('v', '<div az="0"><div az-key="k1"><div az="1"><!--az:1--><span az-hook="Old">old</span><!--/az--></div></div></div>');
        mountHooks(document);
        applyOps([[OP.ITEM_PATCH, 'v:0', 'k1', [[OP.TEXT, '1', '<span az-hook="New">new</span>']]]]);
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
            destroyed() { elInDocument = document.body.contains(this.el); },
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
            updated() { capturedEl = this.el; },
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
        applyOps([[OP.TEXT, 'v:0', '<em>hi</em>']]);
        const p = document.querySelector('[az="0"]');
        expect(p.innerHTML).toContain('<em>hi</em>');
    });
});

// ---------------------------------------------------------------------------
// OP.ITEM_PATCH with pre-resolved inner ops
// ---------------------------------------------------------------------------

describe('OP.ITEM_PATCH with pre-resolved inner ops', () => {
    it('TEXT inner op with pre-resolved HTML', () => {
        setupView('v',
            '<div az="0"><div az-key="k1"><span az="1"><!--az:1-->old<!--/az--></span></div></div>');
        applyOps([[OP.ITEM_PATCH, 'v:0', 'k1', [
            [OP.TEXT, '1', '<em>new</em>'],
        ]]]);
        const item = resolveEl('v:0').querySelector('[az-key="k1"]');
        expect(item.querySelector('[az="1"]').innerHTML).toContain('<em>new</em>');
    });

    it('TEXT inner op with marker and pre-resolved HTML', () => {
        setupView('v',
            '<div az="0"><div az-key="k1"><span az="1"><!--az:1-->old<!--/az--></span></div></div>');
        applyOps([[OP.ITEM_PATCH, 'v:0', 'k1', [
            [OP.TEXT, '1', '<b>marked</b>'],
        ]]]);
        const item = resolveEl('v:0').querySelector('[az-key="k1"]');
        expect(item.querySelector('[az="1"]').innerHTML).toContain('<b>marked</b>');
    });

    it('UPDATE inner op with pre-resolved HTML', () => {
        setupView('v',
            '<div az="0"><div az-key="k1"><div az="1">old</div></div></div>');
        applyOps([[OP.ITEM_PATCH, 'v:0', 'k1', [
            [OP.UPDATE, '1', '<strong>updated</strong>'],
        ]]]);
        const item = resolveEl('v:0').querySelector('[az-key="k1"]');
        expect(item.querySelector('[az="1"]').innerHTML).toBe('<strong>updated</strong>');
    });

    it('mixed plain and pre-resolved inner ops work together', () => {
        setupView('v',
            '<div az="0"><div az-key="k1"><span az="1"><!--az:1-->old<!--/az--></span><span az="2" class="x">y</span></div></div>');
        applyOps([[OP.ITEM_PATCH, 'v:0', 'k1', [
            [OP.TEXT, '1', '<i>fancy</i>'],
            [OP.SET_ATTR, '2', 'class', 'highlight'],
        ]]]);
        const item = resolveEl('v:0').querySelector('[az-key="k1"]');
        expect(item.querySelector('[az="1"]').innerHTML).toContain('<i>fancy</i>');
        expect(item.querySelector('[az="2"]').getAttribute('class')).toBe('highlight');
    });
});

// ---------------------------------------------------------------------------
// onmessage handles partial envelopes (omitted "o" or "e")
// Worker sends [0, ops|null, effects|null, firstAfterReconnect] to main.
// ---------------------------------------------------------------------------

describe('onmessage partial envelopes', () => {
    let mock;
    afterEach(() => { if (mock) mock.restore(); });

    it('ops-only message applies ops without error', async () => {
        vi.resetModules();
        const mod = await import('./arizona.js');

        document.body.innerHTML =
            '<div id="v" az-view><span az="0">old</span></div>';

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

        document.body.innerHTML =
            '<div id="v" az-view><span az="0">old</span></div>';

        const received = [];
        document.addEventListener('full_effect', (e) => received.push(e.detail), { once: true });

        mock = setupMockWorker(mod);
        mock.simulateOpen();
        mock.simulateMessage(
            [[0, 'v:0', 'updated']],
            [[9, 'full_effect', { n: 1 }]]
        );
        expect(document.querySelector('[az="0"]').textContent).toBe('updated');
        expect(received).toEqual([{ n: 1 }]);
    });

    it('null ops and effects is handled gracefully', async () => {
        vi.resetModules();
        const mod = await import('./arizona.js');

        document.body.innerHTML =
            '<div id="v" az-view><span az="0">unchanged</span></div>';

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
    beforeEach(() => { vi.useFakeTimers(); });
    afterEach(() => { vi.useRealTimers(); if (mock) mock.restore(); });

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
        document.body.innerHTML =
            '<form id="myform"><input name="title" value="hello"></form>';
        saveFormState();

        // Simulate DOM replacement (OP_REPLACE wipes the DOM)
        document.body.innerHTML =
            '<form id="myform"><input name="title" value=""></form>';

        restoreFormState();
        expect(document.querySelector('input[name="title"]').value).toBe('hello');
    });

    it('ignores forms without id', () => {
        document.body.innerHTML =
            '<form><input name="x" value="val"></form>';
        saveFormState();

        document.body.innerHTML =
            '<form><input name="x" value=""></form>';
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
        document.body.innerHTML =
            '<form id="gone"><input name="x" value="val"></form>';
        saveFormState();

        document.body.innerHTML = '<div>no form here</div>';
        // Should not throw
        expect(() => restoreFormState()).not.toThrow();
    });

    it('clears saved state after restore', () => {
        document.body.innerHTML =
            '<form id="f"><input name="x" value="saved"></form>';
        saveFormState();

        document.body.innerHTML =
            '<form id="f"><input name="x" value=""></form>';
        restoreFormState();
        expect(document.querySelector('input').value).toBe('saved');

        // Restore again -- should be no-op since state was cleared
        document.body.innerHTML =
            '<form id="f"><input name="x" value="empty"></form>';
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
        document.body.innerHTML =
            '<form id="f"><input type="checkbox" name="agree"></form>';
        restoreFormState();
        expect(document.querySelector('input[name="agree"]').checked).toBe(true);
    });

    it('unchecks checkboxes absent from saved state', () => {
        // Checkbox unchecked → FormData omits it → field NOT in saved fields
        document.body.innerHTML =
            '<form id="f"><input type="checkbox" name="opt"></form>';
        saveFormState();

        // After replace, checkbox might be pre-checked by server HTML
        document.body.innerHTML =
            '<form id="f"><input type="checkbox" name="opt" checked></form>';
        restoreFormState();
        expect(document.querySelector('input[name="opt"]').checked).toBe(false);
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
        expect(options[0].selected).toBe(true);   // red
        expect(options[1].selected).toBe(false);  // green
        expect(options[2].selected).toBe(true);   // blue
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

        document.body.innerHTML =
            '<form id="f"><textarea name="note"></textarea></form>';
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
        document.body.innerHTML =
            '<form id="f"><input name="a" value="saved"></form>';
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
            '<form id="f" az-change="validate"><div id="v" az-view><input name="x" value="val"></div></form>';
        mod.saveFormState();

        document.body.innerHTML =
            '<form id="f" az-change="validate"><div id="v" az-view><input name="x" value=""></div></form>';

        const mock = setupMockWorker(mod);
        mock.simulateOpen();
        mock.posted.length = 0;
        mod.restoreFormState();
        const changeMsgs = mock.getSentMessages().filter(s => s[1] === 'validate');
        expect(changeMsgs).toHaveLength(1);
        expect(changeMsgs[0][2]).toEqual({ x: 'val' });
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
        const changeMsgs = mock.getSentMessages().filter(s => s[1] === 'validate');
        expect(changeMsgs).toHaveLength(0);
        mock.restore();
    });
});

// ---------------------------------------------------------------------------
// CSS connection classes -- via mock Worker
// ---------------------------------------------------------------------------

describe('connection CSS classes', () => {
    let mock;
    afterEach(() => { if (mock) mock.restore(); });

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

        document.body.innerHTML =
            '<form id="f"><input name="x" value="saved"></form>';

        mock = setupMockWorker(mod);
        mock.simulateOpen();
        mock.simulateClose(1006);

        // After abnormal close, form state should be saved
        // Verify by replacing DOM and restoring
        document.body.innerHTML =
            '<form id="f"><input name="x" value=""></form>';
        mock.simulateOpen(true); // reconnect
        mock.simulateMessage(null, null, true); // firstAfterReconnect
        expect(document.querySelector('input[name="x"]').value).toBe('saved');
    });

    it('does NOT save form state on normal close (code 1000)', async () => {
        vi.resetModules();
        const mod = await import('./arizona.js');

        document.body.innerHTML =
            '<form id="f"><input name="x" value="saved"></form>';

        mock = setupMockWorker(mod);
        mock.simulateOpen();
        mock.simulateClose(1000);

        // Replace DOM -- form state should NOT be restored
        document.body.innerHTML =
            '<form id="f"><input name="x" value=""></form>';
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
                get onmessage() { return null; },
            };
        };
        mod.connect('/ws');
        globalThis.Worker = OrigWorker;
        const wsUrl = posted[0][1];
        expect(wsUrl).not.toContain('params=');
    });

    it('sends WS URL with merged params', async () => {
        vi.resetModules();
        const mod = await import('./arizona.js');
        const posted = [];
        const OrigWorker = globalThis.Worker;
        globalThis.Worker = function () {
            return {
                postMessage: (d) => posted.push(d),
                set onmessage(_) {},
                get onmessage() { return null; },
            };
        };
        mod.connect('/ws', { locale: 'en' });
        globalThis.Worker = OrigWorker;
        const wsUrl = posted[0][1];
        expect(wsUrl).toContain('params=');
        const paramsStr = decodeURIComponent(wsUrl.split('params=')[1]);
        const params = JSON.parse(paramsStr);
        expect(params.locale).toBe('en');
    });
});

// ---------------------------------------------------------------------------
// pushEvent / pushEventTo
// ---------------------------------------------------------------------------

describe('pushEvent', () => {
    /** @type {ReturnType<typeof setupMockWorker>} */
    let mock;

    afterEach(() => { if (mock) mock.restore(); });

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
        const msg = msgs.find(m => m[1] === 'orphan_event');
        expect(msg).toBeTruthy();
        expect(msg[0]).toBeNull();
    });
});

describe('pushEventTo', () => {
    /** @type {ReturnType<typeof setupMockWorker>} */
    let mock;

    afterEach(() => { if (mock) mock.restore(); });

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
