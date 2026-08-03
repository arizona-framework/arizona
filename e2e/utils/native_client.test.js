import { afterEach, describe, expect, it, vi } from 'vitest';
import { NativeClient } from './native_client.js';

// arizona_effect op codes (must match include/arizona_effect.hrl); the
// cross-language sync is guarded by test/arizona_effect_wire_SUITE.erl.
const EFFECT_PUSH_EVENT = 0;
const EFFECT_NAVIGATE = 10;

// Diff op codes (mirror src/arizona.hrl).
const OP_TEXT = 0;
const OP_SET_ATTR = 1;
const OP_REMOVE_NODE = 4;
const OP_INSERT = 5;
const OP_REMOVE = 6;
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

// A keyed stream item wrapping a nested each, so two CELLS inside one item share
// an `az` -- the item-local registry's equivalent of two stream items sharing one.
const NESTED_ITEM_STATICS = [
    '{"type":"Row","az":"I-0","az_key":',
    ',"children":[{"type":"#slot","az":"I-0t0","children":[',
    ']}]}',
];
const CELL_STATICS = ['{"type":"Cell","az":"N-0","az_key":', ',"children":["x"]}'];

function newClient() {
    return new NativeClient('http://localhost:4040', '/native/x');
}

// Swap in a recording WebSocket so a test can drive `_open()` end to end and
// assert on what the client actually TRANSMITS (mirrors the browser client's
// worker-integration stub).
function installWebSocketStub() {
    const instances = [];
    class MockWS {
        constructor(url) {
            this.url = url;
            this.sent = [];
            instances.push(this);
        }
        send(data) {
            this.sent.push(data);
        }
        close() {}
        simulateOpen() {
            if (this.onopen) this.onopen();
        }
        simulateMessage(data) {
            if (this.onmessage) this.onmessage({ data });
        }
    }
    const orig = globalThis.WebSocket;
    globalThis.WebSocket = MockWS;
    return {
        latest: () => instances[instances.length - 1],
        restore: () => {
            globalThis.WebSocket = orig;
        },
    };
}

// OP_REPLACE of a stream root holding `keys`, all sharing one item fingerprint
// (so every item carries the SAME az values -- the collision the identity check
// in unindexByViews exists for).
function replaceSharedAzList(client, keys) {
    client._applyOps([
        [
            OP_REPLACE,
            'native_l',
            {
                f: 'R',
                s: ROOT_STATICS,
                d: [
                    'native_l',
                    { t: 0, f: 'S', s: SLOT_ITEM_STATICS, d: keys.map((k) => [k, '']) },
                ],
            },
        ],
    ]);
}

// The raw (unflattened) stream items under the root's content slot.
function rawItems(client) {
    return client.root.children[0].children.find(Array.isArray);
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

    // ...and the socket must actually SEND them. Asserting `_cachedFps()` alone
    // leaves the wiring untested: reverting the open handler to a hardcoded `[]`
    // -- the exact bug -- keeps a helper-only test green.
    it('transmits the cached fingerprints on every socket open', () => {
        const ws = installWebSocketStub();
        try {
            const client = newClient();
            client.connect();
            ws.latest().simulateOpen();
            // First open: nothing cached yet.
            expect(JSON.parse(ws.latest().sent[0])).toEqual(['cached_fps', []]);

            // A frame arrives and caches fingerprint "R" ...
            ws.latest().simulateMessage(
                JSON.stringify({
                    o: [[OP_REPLACE, 'native_x', { f: 'R', s: ROOT_STATICS, d: ['native_x', ''] }]],
                }),
            );
            // ... so the next open announces it instead of an empty list.
            client._open();
            ws.latest().simulateOpen();
            expect(JSON.parse(ws.latest().sent[0])).toEqual(['cached_fps', ['R']]);
        } finally {
            ws.restore();
        }
    });

    // A frame that will not parse, and a malformed effect command, are both
    // server output arriving asynchronously: each must cost itself, not the
    // client. (Kotlin proved the regression: `{"e":[[0]]}` threw
    // IndexOutOfBoundsException straight out of the frame handler.)
    it('survives an unparseable frame and a malformed effect command', () => {
        const warn = vi.spyOn(console, 'warn').mockImplementation(() => {});
        const ws = installWebSocketStub();
        try {
            const client = newClient();
            client.connect();
            ws.latest().simulateOpen();
            expect(() => ws.latest().simulateMessage('not json')).not.toThrow();
            // push_event with no event name.
            expect(() => ws.latest().simulateMessage('{"e":[[0]]}')).not.toThrow();
            expect(warn).toHaveBeenCalledTimes(2);

            // Still live: a well-formed frame after both still applies.
            ws.latest().simulateMessage(
                JSON.stringify({
                    o: [[OP_REPLACE, 'native_x', { f: 'R', s: ROOT_STATICS, d: ['native_x', ''] }]],
                }),
            );
            expect(client.tree().id).toBe('native_x');
        } finally {
            ws.restore();
        }
    });

    // Since OP_INSERT indexes what it grafts in, OP_REMOVE has to unindex what it
    // drops -- otherwise a churning stream grows the registry (and pins every
    // detached subtree) once per cycle, for the life of the connection. Each item
    // here carries a child view with its own id, so the leak is unambiguous: one
    // extra registry per cycle, reclaimed only by a reconnect.
    it('does not grow the registry across insert/remove cycles', () => {
        const client = newClient();
        client._applyOps([
            [
                OP_REPLACE,
                'native_l',
                {
                    f: 'R',
                    s: ROOT_STATICS,
                    d: ['native_l', { t: 0, f: 'I', s: ITEM_STATICS, d: [['k1', 'kid', '0']] }],
                },
            ],
        ]);
        const baseline = client.views.get('native_l').size;
        for (let i = 0; i < 50; i++) {
            client._applyOps([
                [OP_INSERT, 'native_l:R-0t0', `x${i}`, -1, { f: 'I', d: [`x${i}`, `kid${i}`, ''] }],
            ]);
            client._applyOps([[OP_REMOVE, 'native_l:R-0t0', `x${i}`]]);
        }
        // Only the root view and the surviving item's child view are left.
        expect([...client.views.keys()].sort()).toEqual(['kid', 'native_l']);
        // The root view's own entries never grow either. (They can SHRINK: every
        // item shares one fingerprint's az values, so the entry for a shared az
        // names whichever item was indexed last, and removing that item drops it.
        // Harmless -- a stream item is only ever addressed through its container
        // by `az_key`, never by "ViewId:az" -- and true of OP_REPLACE on main too.)
        expect(client.views.get('native_l').size).toBeLessThanOrEqual(baseline);
    });

    // A child view an inner op installed, then removed with its item, must not
    // leave a live registry entry pointing into the detached subtree.
    it('drops a removed item child view from the registry', () => {
        const client = newClient();
        replaceSharedAzList(client, ['k1']);
        client._applyOps([
            [
                OP_ITEM_PATCH,
                'native_l:R-0t0',
                'k1',
                [[OP_TEXT, 'I-0t0', { f: 'C', s: CHILD_STATICS, d: ['kid', '0'] }]],
            ],
        ]);
        expect(client.views.get('kid')).toBeDefined();
        client._applyOps([[OP_REMOVE, 'native_l:R-0t0', 'k1']]);
        expect(client.views.get('kid')).toBeUndefined();
    });

    // The `remove` sentinel drops a node one-way, so its registry entries are
    // dead the moment the splice lands -- including a whole child view's.
    it('drops a node removed by the remove sentinel from the registry', () => {
        const client = newClient();
        replaceRoot(client);
        client._applyOps([
            [OP_TEXT, 'native_x:R-0t0', { f: 'C', s: CHILD_STATICS, d: ['cond_child', '0'] }],
        ]);
        expect(client.views.get('cond_child')).toBeDefined();

        client._applyOps([[OP_REMOVE_NODE, 'cond_child:C-0']]);
        expect(client.views.get('cond_child')).toBeUndefined();
        expect(client.tree().children).toEqual([]);
    });

    // THE identity check, per-view half: stream items share az values (one
    // fingerprint, many items), so the registry entry for a shared az names
    // whichever item was indexed last. Unindexing a destroyed item by key alone
    // would delete the entry naming a LIVE sibling.
    it('keeps a surviving sibling registered when an item sharing its az is removed', () => {
        const client = newClient();
        replaceSharedAzList(client, ['k1', 'k2']);
        const reg = client.views.get('native_l');
        // Last-indexed wins: the entry names item k2's slot.
        const survivorSlot = rawItems(client)[1].children[0];
        expect(reg.get('I-0t0')).toBe(survivorSlot);

        client._applyOps([[OP_REMOVE, 'native_l:R-0t0', 'k1']]);
        expect(reg.get('I-0t0')).toBe(survivorSlot);
        expect(reg.get('I-0')).toBe(rawItems(client)[0]);
    });

    // THE identity check, item-local half: two cells of a nested each inside one
    // item share an az, so an inner OP_REMOVE must not unregister the survivor --
    // a later inner op naming that az would then fall back to the ITEM and
    // overwrite the whole row.
    it('keeps a surviving nested cell resolvable when its az-sharing sibling is removed', () => {
        const client = newClient();
        client._applyOps([
            [
                OP_REPLACE,
                'native_l',
                {
                    f: 'R',
                    s: ROOT_STATICS,
                    d: [
                        'native_l',
                        {
                            t: 0,
                            f: 'NI',
                            s: NESTED_ITEM_STATICS,
                            d: [['k1', { t: 0, f: 'CE', s: CELL_STATICS, d: [['n1'], ['n2']] }]],
                        },
                    ],
                },
            ],
        ]);
        client._applyOps([
            [
                OP_ITEM_PATCH,
                'native_l:R-0t0',
                'k1',
                [
                    [OP_REMOVE, 'I-0t0', 'n1'],
                    [OP_TEXT, 'N-0', 'patched'],
                ],
            ],
        ]);
        const row = client.tree().children[0];
        // The surviving cell took the patch, and the row is still a row.
        expect(row.children[0].type).toBe('Cell');
        expect(row.children[0].children).toEqual(['patched']);
    });

    // Build before committing: a malformed OP_REPLACE must leave the previous
    // tree AND registry intact rather than half-clearing them. The payload has to
    // fail LATE -- statics that interleave fine but do not parse -- since an
    // uncached fingerprint dies before either ordering commits anything and so
    // cannot tell them apart. (Android/iOS fail one step later still, inside the
    // tree builder; this client has no tree builder to fail in.)
    it('keeps the previous tree and registry when an OP_REPLACE payload is bad', () => {
        const warn = vi.spyOn(console, 'warn').mockImplementation(() => {});
        const client = newClient();
        replaceRoot(client);
        const before = client.root;

        client._applyOps([[OP_REPLACE, 'native_x', { f: 'BAD', s: ['{"type":"X"'], d: [] }]]);
        expect(warn).toHaveBeenCalledTimes(1);
        expect(client.root).toBe(before);
        expect(client.viewId).toBe('native_x');
        expect(client.views.get('native_x')).toBeDefined();

        // The old registry still resolves, so the view keeps patching.
        client._applyOps([[OP_TEXT, 'native_x:R-0t0', 'still here']]);
        expect(client.tree().children).toEqual(['still here']);
    });

    // Spec parity across the three clients for an OP_INSERT position the server
    // never sends: any out-of-range value appends. Left as `=== -1`, a raw
    // negative index means "count from the end" to splice here, is an exception
    // on Android, and TRAPS on iOS.
    it('appends an insert whose position is out of range', () => {
        const client = newClient();
        replaceSharedAzList(client, ['k1']);
        client._applyOps([[OP_INSERT, 'native_l:R-0t0', 'k2', -5, { f: 'S', d: ['k2', ''] }]]);
        expect(rawItems(client).map((it) => it.az_key)).toEqual(['k1', 'k2']);
    });

    // Spec parity: an inserted item that is ITSELF a view root owns its subtree
    // under its own id, rather than registering in the container's view.
    it('registers an inserted item that is itself a view root', () => {
        const client = newClient();
        replaceSharedAzList(client, ['k1']);
        client._applyOps([
            [
                OP_INSERT,
                'native_l:R-0t0',
                'solo',
                -1,
                { f: 'C', s: CHILD_STATICS, d: ['solo', '0'] },
            ],
        ]);
        expect(client.views.get('solo')).toBeDefined();

        client._applyOps([[OP_TEXT, 'solo:C-0t0', '7']]);
        expect(client.tree().children[1].children).toEqual(['7']);
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
