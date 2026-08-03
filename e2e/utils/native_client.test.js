import { afterEach, describe, expect, it, vi } from 'vitest';
import { NativeClient } from './native_client.js';

// arizona_effect op codes (must match include/arizona_effect.hrl); the
// cross-language sync is guarded by test/arizona_effect_wire_SUITE.erl.
const EFFECT_PUSH_EVENT = 0;
const EFFECT_NAVIGATE = 10;

// Diff op codes (mirror src/arizona.hrl).
const OP_TEXT = 0;
const OP_SET_ATTR = 1;
const OP_INSERT = 5;
const OP_ITEM_PATCH = 7;
const OP_REPLACE = 8;

// A NativeClient whose socket is replaced by a frame recorder. The constructor
// does not open a socket (that happens in connect()), so a bare instance can be
// driven directly. `sent` collects the raw JSON frames the client sends.
function clientWithRecorder(viewId = 'native_counter') {
    const client = new NativeClient('http://localhost:4040', '/native/counter');
    const sent = [];
    client.ws = { send: (frame) => sent.push(frame) };
    client.viewId = viewId;
    return { client, sent };
}

// The user-visible half of E1: on native there is no form/input auto-collection,
// so an explicit push_event/2 payload is the only way to attach data. These
// assert the dispatcher stitches the full [ViewId, Event, Payload] frame instead
// of dropping cmd[2] and defaulting to {}.
describe('native client effect dispatch', () => {
    it('carries the explicit push_event payload into the [ViewId, Event, Payload] frame', () => {
        const { client, sent } = clientWithRecorder();
        client._runEffect([EFFECT_PUSH_EVENT, 'save', { id: '42' }], true, 'native_counter');
        expect(sent).toHaveLength(1);
        expect(JSON.parse(sent[0])).toEqual(['native_counter', 'save', { id: '42' }]);
    });

    it('sends an empty payload object when push_event carries none', () => {
        const { client, sent } = clientWithRecorder();
        client._runEffect([EFFECT_PUSH_EVENT, 'inc'], true, 'native_counter');
        expect(JSON.parse(sent[0])).toEqual(['native_counter', 'inc', {}]);
    });

    it('routes a tap to the enclosing view of the tapped node, payload intact', () => {
        const { client, sent } = clientWithRecorder('root_view');
        const node = { __view: 'child_view', on_tap: [EFFECT_PUSH_EVENT, 'save', { id: '7' }] };
        client.tap(node);
        expect(JSON.parse(sent[0])).toEqual(['child_view', 'save', { id: '7' }]);
    });

    it('dispatches a navigate frame with the split path and query', () => {
        const { client, sent } = clientWithRecorder();
        client._runEffect([EFFECT_NAVIGATE, '/foo?x=1'], true, undefined);
        expect(JSON.parse(sent[0])).toEqual(['navigate', { path: '/foo', qs: 'x=1' }]);
    });

    it('throws on an unsupported command in strict (tap) mode', () => {
        const { client } = clientWithRecorder();
        expect(() => client._runEffect([99, 'nope'], true)).toThrow();
    });
});

// --------------------------------------------------------------------------
// Op application: the az -> node registry vs. nodes the DIFF creates.
// --------------------------------------------------------------------------

// A view root whose single `#slot` (az "R-0t0") holds the swappable content.
const ROOT_STATICS = [
    '{"type":"Column","az":"R-0","az_view":true,"id":',
    ',"children":[{"type":"#slot","az":"R-0t0","children":[',
    ']}]}',
];

// A stateful CHILD view's subtree: its own `az_view` + `id`, so installing it
// introduces a whole new view id the server will address ops to.
const CHILD_STATICS = [
    '{"type":"Column","az":"C-0","az_view":true,"id":',
    ',"children":[{"type":"#slot","az":"C-0t0","children":[',
    ']}]}',
];

// A keyed stream item with an empty content slot -- where the `?stateful` a
// conditional in the item template switches on lands, via an item-patch INNER op.
const SLOT_ITEM_STATICS = [
    '{"type":"Row","az":"I-0","az_key":',
    ',"children":[{"type":"#slot","az":"I-0t0","children":[',
    ']}]}',
];

// A keyed stream item wrapping a stateful child view (the `?stateful` inside a
// stream `?each` shape).
const ITEM_STATICS = [
    '{"type":"Row","az":"I-0","az_key":',
    ',"children":[{"type":"Column","az":"C-0","az_view":true,"id":',
    ',"children":[{"type":"#slot","az":"C-0t0","children":[',
    ']}]}]}',
];

function newClient() {
    return new NativeClient('http://localhost:4040', '/native/x');
}

// OP_REPLACE of the root view with an empty content slot.
function replaceRoot(client) {
    client._applyOps([[OP_REPLACE, 'native_x', { f: 'R', s: ROOT_STATICS, d: ['native_x', ''] }]]);
}

describe('native client op application', () => {
    afterEach(() => {
        vi.restoreAllMocks();
    });

    // The three-frame repro: OP_REPLACE, an OP_TEXT that installs a subtree, then
    // an op addressed INSIDE that subtree. The registry used to be built only at
    // OP_REPLACE, so every az the second frame introduced was invisible and the
    // third frame hit "unknown target".
    it('addresses a node the diff created via OP_TEXT', () => {
        const client = newClient();
        replaceRoot(client);
        client._applyOps([
            [
                OP_TEXT,
                'native_x:R-0t0',
                { f: 'T', s: ['{"type":"Text","az":"T-0","children":["a"]}'], d: [] },
            ],
        ]);
        client._applyOps([[OP_SET_ATTR, 'native_x:T-0', 'color', 'red']]);
        expect(client.tree().children[0]).toMatchObject({ type: 'Text', color: 'red' });
    });

    // The documented `case ?get(flag) of true -> ?stateful(child, ...)` pattern:
    // the installed payload carries its OWN view id, which must be registered or
    // the child's very first update crashes.
    it('registers a child view id introduced by an OP_TEXT payload', () => {
        const client = newClient();
        replaceRoot(client);
        client._applyOps([
            [OP_TEXT, 'native_x:R-0t0', { f: 'C', s: CHILD_STATICS, d: ['cond_child', '0'] }],
        ]);
        expect(client.tree().children[0].children).toEqual(['0']);

        // An op addressed to the CHILD view, not the root.
        client._applyOps([[OP_TEXT, 'cond_child:C-0t0', '1']]);
        expect(client.tree().children[0].children).toEqual(['1']);
    });

    // A rebuilt slot must not leave the destroyed subtree's azs in the registry,
    // or the map retains detached nodes for the life of the connection.
    it('drops the entries of a subtree an OP_TEXT replaced', () => {
        const client = newClient();
        replaceRoot(client);
        client._applyOps([
            [OP_TEXT, 'native_x:R-0t0', { f: 'C', s: CHILD_STATICS, d: ['cond_child', '0'] }],
        ]);
        expect(client.views.get('cond_child')).toBeDefined();
        client._applyOps([[OP_TEXT, 'native_x:R-0t0', '']]);
        expect(client.views.get('cond_child')).toBeUndefined();
    });

    // A `?stateful` child inside a stream item is addressed by the server through
    // a `[ChildViewId, ChildOps]` wrapper nested in the item patch (flatten_ops/2
    // only unwraps that at top level), so op[0] is a view-id STRING, not an int.
    it('applies a child-view op wrapper nested in an OP_ITEM_PATCH', () => {
        const client = newClient();
        client._applyOps([
            [
                OP_REPLACE,
                'native_l',
                {
                    f: 'R',
                    s: ROOT_STATICS,
                    d: ['native_l', { t: 0, f: 'I', s: ITEM_STATICS, d: [['k1', 'child_1', '0']] }],
                },
            ],
        ]);
        expect(client.tree().children[0].children[0].children).toEqual(['0']);

        client._applyOps([
            [OP_ITEM_PATCH, 'native_l:R-0t0', 'k1', [['child_1', [[OP_TEXT, 'C-0t0', '9']]]]],
        ]);
        expect(client.tree().children[0].children[0].children).toEqual(['9']);
    });

    // A conditional `?stateful` INSIDE a stream item installs the child through an
    // item-patch INNER op, so its view id never appears in a top-level op -- but
    // the child's own ops come back top-level. Indexing an inner rebuild only into
    // the item-local map leaves it unaddressable and its slot frozen.
    it('registers a child view an item-patch inner op installed', () => {
        const client = newClient();
        client._applyOps([
            [
                OP_REPLACE,
                'native_l',
                {
                    f: 'R',
                    s: ROOT_STATICS,
                    d: ['native_l', { t: 0, f: 'S', s: SLOT_ITEM_STATICS, d: [['k1', '']] }],
                },
            ],
        ]);
        client._applyOps([
            [
                OP_ITEM_PATCH,
                'native_l:R-0t0',
                'k1',
                [[OP_TEXT, 'I-0t0', { f: 'C', s: CHILD_STATICS, d: ['inner_kid', '0'] }]],
            ],
        ]);
        expect(client.tree().children[0].children[0].children).toEqual(['0']);

        // A TOP-LEVEL op addressed to the view the inner op created.
        client._applyOps([[OP_TEXT, 'inner_kid:C-0t0', '5']]);
        expect(client.tree().children[0].children[0].children).toEqual(['5']);
    });

    // An inserted stream item's child view is a new view id too -- OP_REPLACE
    // already indexes the items it renders, so an insert must as well.
    it('registers a child view inside an inserted stream item', () => {
        const client = newClient();
        client._applyOps([
            [
                OP_REPLACE,
                'native_l',
                {
                    f: 'R',
                    s: ROOT_STATICS,
                    d: ['native_l', { t: 0, f: 'I', s: ITEM_STATICS, d: [['k1', 'child_1', '0']] }],
                },
            ],
        ]);
        client._applyOps([
            [OP_INSERT, 'native_l:R-0t0', 'k2', -1, { f: 'I', d: ['k2', 'child_2', '0'] }],
        ]);
        client._applyOps([[OP_TEXT, 'child_2:C-0t0', '7']]);
        expect(client.tree().children[1].children[0].children).toEqual(['7']);
    });

    // Per-op isolation: one unresolvable target (or an op code this client does
    // not implement, e.g. OP_LIST_PATCH) must degrade that slot only.
    it('warns and skips a bad op without dropping the rest of the batch', () => {
        const warn = vi.spyOn(console, 'warn').mockImplementation(() => {});
        const client = newClient();
        replaceRoot(client);
        client._applyOps([
            [OP_TEXT, 'native_x:nope', 'ignored'],
            [10, 'native_x:R-0t0', 'unimplemented op code'],
            [OP_TEXT, 'native_x:R-0t0', 'applied'],
        ]);
        expect(client.tree().children).toEqual(['applied']);
        expect(warn).toHaveBeenCalledTimes(2);
    });

    // The announcement is what lets the server elide statics the client already
    // holds; a hardcoded empty list re-ships every template on every reconnect.
    it('announces the fingerprints it actually cached', () => {
        const client = newClient();
        replaceRoot(client);
        expect(client._cachedFps()).toEqual(['R']);
    });

    // Unbounded, the cache accumulates one generation of fingerprints per deploy.
    it('prunes the cache to the most-recently-used FP_CACHE_MAX on announce', () => {
        const client = newClient();
        for (let i = 0; i < 1003; i++) client.fpCache.set(`fp${i}`, { s: ['x'] });
        // Touch the oldest key so it survives the prune as most-recently-used.
        client._statics({ f: 'fp0' });
        const keys = client._cachedFps();
        expect(keys).toHaveLength(1000);
        expect(keys.at(-1)).toBe('fp0');
        expect(keys).not.toContain('fp1');
        expect(client.fpCache.size).toBe(1000);
    });

    // A malformed payload throws inside the op body; the batch must survive it.
    it('warns and skips an op whose payload is malformed', () => {
        const warn = vi.spyOn(console, 'warn').mockImplementation(() => {});
        const client = newClient();
        replaceRoot(client);
        client._applyOps([
            [OP_TEXT, 'native_x:R-0t0', { f: 'never-cached', d: [] }],
            [OP_TEXT, 'native_x:R-0t0', 'applied'],
        ]);
        expect(client.tree().children).toEqual(['applied']);
        expect(warn).toHaveBeenCalledTimes(1);
    });
});
