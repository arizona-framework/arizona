import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';
import { NativeClient } from './json_client.js';

// arizona_effect op codes (must match include/arizona_effect.hrl); the
// cross-language sync is guarded by test/arizona_wire_SUITE.erl.
const EFFECT_PUSH_EVENT = 0;
const EFFECT_NAVIGATE = 10;

// Diff op codes (mirror include/arizona_wire.hrl).
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
function clientWithRecorder(viewId = 'json_counter') {
    const client = new NativeClient('http://localhost:4040', '/json/counter');
    const sent = [];
    client.ws = { send: (frame) => sent.push(frame) };
    client._live = true; // as an open socket leaves it
    client.viewId = viewId;
    return { client, sent };
}

// The user-visible half of E1: on native there is no form/input auto-collection,
// so an explicit push_event/2 payload is the only way to attach data. These
// assert the dispatcher stitches the full [ViewId, Event, Payload] frame instead
// of dropping cmd[2] and defaulting to {}.
describe('JSON client effect dispatch', () => {
    it('carries the explicit push_event payload into the [ViewId, Event, Payload] frame', () => {
        const { client, sent } = clientWithRecorder();
        client._runEffect([EFFECT_PUSH_EVENT, 'save', { id: '42' }], true, 'json_counter');
        expect(sent).toHaveLength(1);
        expect(JSON.parse(sent[0])).toEqual(['json_counter', 'save', { id: '42' }]);
    });

    it('sends an empty payload object when push_event carries none', () => {
        const { client, sent } = clientWithRecorder();
        client._runEffect([EFFECT_PUSH_EVENT, 'inc'], true, 'json_counter');
        expect(JSON.parse(sent[0])).toEqual(['json_counter', 'inc', {}]);
    });

    // A handler effect built with arizona_js:push_event/3 reaches a native client too
    // (handler effects are not target-checked): its options ride last, and a client
    // that tracks no replies runs it as it runs any push_event.
    it('runs a push_event carrying options as one without', () => {
        const { client, sent } = clientWithRecorder();
        client._runEffect(
            [EFFECT_PUSH_EVENT, 'save', { id: '42' }, { on_repeat: 'drop' }],
            true,
            'json_counter',
        );
        expect(JSON.parse(sent[0])).toEqual(['json_counter', 'save', { id: '42' }]);
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
        expect(JSON.parse(sent[0])).toEqual(['navigate', { path: '/foo', qs: 'x=1' }, 1]);
    });

    // A `full` navigate cannot be served on this socket, and sending it back
    // would loop: the client connects fresh to the path instead, retiring the old
    // socket without a reconnect of its own.
    it('connects fresh to the path of a full navigate', () => {
        vi.useFakeTimers();
        const ws = installWebSocketStub();
        try {
            const client = newClient();
            client.connect();
            const old = ws.latest();
            old.simulateOpen();
            old.simulateMessage(
                JSON.stringify({
                    o: [[OP_REPLACE, 'json_x', { f: 'R', s: ROOT_STATICS, d: ['json_x', ''] }]],
                }),
            );
            old.simulateMessage(
                JSON.stringify({ e: [[EFFECT_NAVIGATE, '/gated?tab=b&x=1', { full: true }]] }),
            );

            expect(old.sent.some((f) => f.includes('"navigate"'))).toBe(false);
            expect(old.closedWith).toBe(1000);
            expect(ws.instances).toHaveLength(2);
            const params = new URL(ws.latest().url).searchParams;
            expect(params.get('_az_path')).toBe('/gated');
            // The query rides as plain params, as the browser connect carries it.
            expect(params.get('tab')).toBe('b');
            expect(params.get('x')).toBe('1');
            expect(params.get('_az_reconnect')).toBe('1');
            vi.advanceTimersByTime(60000);
            expect(ws.instances).toHaveLength(2); // the retired socket reconnects nothing
        } finally {
            ws.restore();
            vi.useRealTimers();
        }
    });

    // Backoff parity with the browser worker: a view that crashes right after
    // every remount must keep backing off, not reconnect at backoff[0] forever.
    describe('reconnect backoff', () => {
        const mountFrame = () =>
            JSON.stringify({
                o: [[OP_REPLACE, 'json_x', { f: 'R', s: ROOT_STATICS, d: ['json_x', ''] }]],
            });
        /** Close the open socket with `code`; return how long the client waits to reopen. */
        function reopenDelay(ws, code) {
            const n = ws.instances.length;
            ws.latest().close(code);
            let waited = 0;
            while (ws.instances.length === n && waited < 60000) {
                vi.advanceTimersByTime(50);
                waited += 50;
            }
            return waited;
        }
        const inStep = (ms, [lo, hi]) => ms >= lo && ms <= hi;
        /** A client whose second socket is open with attempt 1 next. */
        function afterOneDrop(ws) {
            const client = newClient();
            client.connect();
            ws.latest().simulateOpen();
            ws.latest().simulateMessage(mountFrame()); // first render
            ws.latest().close(4500); // crashed right after it -> backoff(0), attempt 1 next
            vi.advanceTimersByTime(1200);
            ws.latest().simulateOpen();
            return client;
        }
        let ws;
        let now;
        beforeEach(() => {
            vi.useFakeTimers({ toFake: ['setTimeout', 'clearTimeout', 'setInterval'] });
            now = vi.spyOn(Date, 'now').mockReturnValue(1_000_000);
            ws = installWebSocketStub();
        });
        afterEach(() => {
            ws.restore();
            now.mockRestore();
            vi.useRealTimers();
        });

        it('a crash right after the remount keeps backing off', () => {
            afterOneDrop(ws);
            ws.latest().simulateMessage(mountFrame());
            now.mockReturnValue(1_000_000 + 9_999);
            expect(inStep(reopenDelay(ws, 4500), [1600, 2400])).toBe(true);
        });

        it('a crash long after the mount resets the backoff', () => {
            afterOneDrop(ws);
            ws.latest().simulateMessage(mountFrame());
            now.mockReturnValue(1_000_000 + 10_000);
            expect(inStep(reopenDelay(ws, 4500), [800, 1200])).toBe(true);
        });

        it('a mount failure (4501) never resets the backoff', () => {
            afterOneDrop(ws);
            ws.latest().simulateMessage(mountFrame());
            now.mockReturnValue(1_000_000 + 60_000);
            expect(inStep(reopenDelay(ws, 4501), [1600, 2400])).toBe(true);
        });

        it('a frame that mounts nothing does not reset the backoff', () => {
            afterOneDrop(ws);
            ws.latest().simulateMessage(JSON.stringify({ o: [] }));
            expect(inStep(reopenDelay(ws, 1006), [1600, 2400])).toBe(true);
        });

        it('a drop after a mount resets the backoff', () => {
            afterOneDrop(ws);
            ws.latest().simulateMessage(mountFrame());
            expect(inStep(reopenDelay(ws, 1006), [800, 1200])).toBe(true);
        });
    });

    it('throws on an unsupported command in strict (tap) mode', () => {
        const { client } = clientWithRecorder();
        expect(() => client._runEffect([99, 'nope'], true)).toThrow();
    });

    it('fires nothing for a disabled (false) or missing tap prop', () => {
        const { client, sent } = clientWithRecorder('root_view');
        client.tap({ __view: 'child_view', on_tap: false });
        client.tap({ __view: 'child_view' });
        expect(sent).toEqual([]);
    });

    it('runs every command of a tap prop holding a list, in order', () => {
        const { client, sent } = clientWithRecorder('root_view');
        const node = {
            __view: 'child_view',
            on_tap: [
                [EFFECT_PUSH_EVENT, 'save', { id: '7' }],
                [EFFECT_NAVIGATE, '/done'],
            ],
        };
        client.tap(node);
        expect(sent.map((f) => JSON.parse(f))).toEqual([
            ['child_view', 'save', { id: '7' }],
            ['navigate', { path: '/done', qs: '' }, 1],
        ]);
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
    return new NativeClient('http://localhost:4040', '/json/x');
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
        close(code = 1000) {
            this.closedWith = code;
            if (this.onclose) this.onclose({ code });
        }
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
        instances,
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
            'json_l',
            {
                f: 'R',
                s: ROOT_STATICS,
                d: ['json_l', { t: 0, f: 'S', s: SLOT_ITEM_STATICS, d: keys.map((k) => [k, '']) }],
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
    client._applyOps([[OP_REPLACE, 'json_x', { f: 'R', s: ROOT_STATICS, d: ['json_x', ''] }]]);
}

describe('JSON client op application', () => {
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
                'json_x:R-0t0',
                { f: 'T', s: ['{"type":"Text","az":"T-0","children":["a"]}'], d: [] },
            ],
        ]);
        client._applyOps([[OP_SET_ATTR, 'json_x:T-0', 'color', 'red']]);
        expect(client.tree().children[0]).toMatchObject({ type: 'Text', color: 'red' });
    });

    // A dynamic tap prop carries its command as an array, in the hydration payload
    // and on a later SET_ATTR alike, so the node stays tappable either way.
    it('keeps a dynamic command prop an array, mounted and patched', () => {
        const { client, sent } = clientWithRecorder('json_x');
        replaceRoot(client);
        client._applyOps([
            [
                OP_TEXT,
                'json_x:R-0t0',
                { f: 'B', s: ['{"type":"Button","az":"B-0","on_tap":', '}'], d: [[0, 'a']] },
            ],
        ]);
        expect(client.tree().children[0].on_tap).toEqual([0, 'a']);
        client._applyOps([[OP_SET_ATTR, 'json_x:B-0', 'on_tap', [0, 'b']]]);
        client.tap(client.tree().children[0]);
        expect(JSON.parse(sent.at(-1))).toEqual(['json_x', 'b', {}]);
    });

    // A dynamic boolean prop is a JSON boolean in the hydration payload and on a
    // later SET_ATTR alike, so `false` stays a present prop rather than a missing one.
    it('keeps a dynamic boolean prop a boolean, mounted and patched', () => {
        const client = newClient();
        replaceRoot(client);
        client._applyOps([
            [
                OP_TEXT,
                'json_x:R-0t0',
                { f: 'E', s: ['{"type":"Button","az":"E-0","enabled":', '}'], d: [false] },
            ],
        ]);
        expect(client.tree().children[0].enabled).toBe(false);
        client._applyOps([[OP_SET_ATTR, 'json_x:E-0', 'enabled', true]]);
        expect(client.tree().children[0].enabled).toBe(true);
    });

    // The documented `case ?get(flag) of true -> ?stateful(child, ...)` pattern:
    // the installed payload carries its OWN view id, which must be registered or
    // the child's very first update crashes.
    it('registers a child view id introduced by an OP_TEXT payload', () => {
        const client = newClient();
        replaceRoot(client);
        client._applyOps([
            [OP_TEXT, 'json_x:R-0t0', { f: 'C', s: CHILD_STATICS, d: ['cond_child', '0'] }],
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
            [OP_TEXT, 'json_x:R-0t0', { f: 'C', s: CHILD_STATICS, d: ['cond_child', '0'] }],
        ]);
        expect(client.views.get('cond_child')).toBeDefined();
        client._applyOps([[OP_TEXT, 'json_x:R-0t0', '']]);
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
                'json_l',
                {
                    f: 'R',
                    s: ROOT_STATICS,
                    d: ['json_l', { t: 0, f: 'I', s: ITEM_STATICS, d: [['k1', 'child_1', '0']] }],
                },
            ],
        ]);
        expect(client.tree().children[0].children[0].children).toEqual(['0']);

        client._applyOps([
            [OP_ITEM_PATCH, 'json_l:R-0t0', 'k1', [['child_1', [[OP_TEXT, 'C-0t0', '9']]]]],
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
                'json_l',
                {
                    f: 'R',
                    s: ROOT_STATICS,
                    d: ['json_l', { t: 0, f: 'S', s: SLOT_ITEM_STATICS, d: [['k1', '']] }],
                },
            ],
        ]);
        client._applyOps([
            [
                OP_ITEM_PATCH,
                'json_l:R-0t0',
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
                'json_l',
                {
                    f: 'R',
                    s: ROOT_STATICS,
                    d: ['json_l', { t: 0, f: 'I', s: ITEM_STATICS, d: [['k1', 'child_1', '0']] }],
                },
            ],
        ]);
        client._applyOps([
            [OP_INSERT, 'json_l:R-0t0', 'k2', -1, { f: 'I', d: ['k2', 'child_2', '0'] }],
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
            [OP_TEXT, 'json_x:nope', 'ignored'],
            [10, 'json_x:R-0t0', 'unimplemented op code'],
            [OP_TEXT, 'json_x:R-0t0', 'applied'],
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
                    o: [[OP_REPLACE, 'json_x', { f: 'R', s: ROOT_STATICS, d: ['json_x', ''] }]],
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
                    o: [[OP_REPLACE, 'json_x', { f: 'R', s: ROOT_STATICS, d: ['json_x', ''] }]],
                }),
            );
            expect(client.tree().id).toBe('json_x');
        } finally {
            ws.restore();
        }
    });

    // Each "e" entry is exactly one command: a frame carrying two must send two
    // frames, once each and in order, and a malformed entry between them costs
    // only itself.
    it('runs each server effect entry as one command, in order', () => {
        const warn = vi.spyOn(console, 'warn').mockImplementation(() => {});
        const ws = installWebSocketStub();
        try {
            const client = newClient();
            client.connect();
            ws.latest().simulateOpen();
            ws.latest().simulateMessage(
                JSON.stringify({
                    o: [[OP_REPLACE, 'json_x', { f: 'R', s: ROOT_STATICS, d: ['json_x', ''] }]],
                    e: [
                        [EFFECT_PUSH_EVENT, 'saved', { id: '1' }],
                        [EFFECT_PUSH_EVENT],
                        [EFFECT_NAVIGATE, '/done?x=1'],
                    ],
                }),
            );
            // sent[0] is the open handshake's cached_fps announcement.
            const effects = ws.latest().sent.slice(1);
            expect(effects.map((f) => JSON.parse(f))).toEqual([
                ['json_x', 'saved', { id: '1' }],
                ['navigate', { path: '/done', qs: 'x=1' }, 1],
            ]);
            expect(warn).toHaveBeenCalledTimes(1);
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
                'json_l',
                {
                    f: 'R',
                    s: ROOT_STATICS,
                    d: ['json_l', { t: 0, f: 'I', s: ITEM_STATICS, d: [['k1', 'kid', '0']] }],
                },
            ],
        ]);
        const baseline = client.views.get('json_l').size;
        for (let i = 0; i < 50; i++) {
            client._applyOps([
                [OP_INSERT, 'json_l:R-0t0', `x${i}`, -1, { f: 'I', d: [`x${i}`, `kid${i}`, ''] }],
            ]);
            client._applyOps([[OP_REMOVE, 'json_l:R-0t0', `x${i}`]]);
        }
        // Only the root view and the surviving item's child view are left.
        expect([...client.views.keys()].sort()).toEqual(['json_l', 'kid']);
        // The root view's own entries never grow either. (They can SHRINK: every
        // item shares one fingerprint's az values, so the entry for a shared az
        // names whichever item was indexed last, and removing that item drops it.
        // Harmless -- a stream item is only ever addressed through its container
        // by `az_key`, never by "ViewId:az" -- and true of OP_REPLACE on main too.)
        expect(client.views.get('json_l').size).toBeLessThanOrEqual(baseline);
    });

    // A child view an inner op installed, then removed with its item, must not
    // leave a live registry entry pointing into the detached subtree.
    it('drops a removed item child view from the registry', () => {
        const client = newClient();
        replaceSharedAzList(client, ['k1']);
        client._applyOps([
            [
                OP_ITEM_PATCH,
                'json_l:R-0t0',
                'k1',
                [[OP_TEXT, 'I-0t0', { f: 'C', s: CHILD_STATICS, d: ['kid', '0'] }]],
            ],
        ]);
        expect(client.views.get('kid')).toBeDefined();
        client._applyOps([[OP_REMOVE, 'json_l:R-0t0', 'k1']]);
        expect(client.views.get('kid')).toBeUndefined();
    });

    // The `remove` sentinel drops a node one-way, so its registry entries are
    // dead the moment the splice lands -- including a whole child view's.
    it('drops a node removed by the remove sentinel from the registry', () => {
        const client = newClient();
        replaceRoot(client);
        client._applyOps([
            [OP_TEXT, 'json_x:R-0t0', { f: 'C', s: CHILD_STATICS, d: ['cond_child', '0'] }],
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
        const reg = client.views.get('json_l');
        // Last-indexed wins: the entry names item k2's slot.
        const survivorSlot = rawItems(client)[1].children[0];
        expect(reg.get('I-0t0')).toBe(survivorSlot);

        client._applyOps([[OP_REMOVE, 'json_l:R-0t0', 'k1']]);
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
                'json_l',
                {
                    f: 'R',
                    s: ROOT_STATICS,
                    d: [
                        'json_l',
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
                'json_l:R-0t0',
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

        client._applyOps([[OP_REPLACE, 'json_x', { f: 'BAD', s: ['{"type":"X"'], d: [] }]]);
        expect(warn).toHaveBeenCalledTimes(1);
        expect(client.root).toBe(before);
        expect(client.viewId).toBe('json_x');
        expect(client.views.get('json_x')).toBeDefined();

        // The old registry still resolves, so the view keeps patching.
        client._applyOps([[OP_TEXT, 'json_x:R-0t0', 'still here']]);
        expect(client.tree().children).toEqual(['still here']);
    });

    // Spec parity across the three clients for an OP_INSERT position the server
    // never sends: any out-of-range value appends. Left as `=== -1`, a raw
    // negative index means "count from the end" to splice here, is an exception
    // on Android, and TRAPS on iOS.
    it('appends an insert whose position is out of range', () => {
        const client = newClient();
        replaceSharedAzList(client, ['k1']);
        client._applyOps([[OP_INSERT, 'json_l:R-0t0', 'k2', -5, { f: 'S', d: ['k2', ''] }]]);
        expect(rawItems(client).map((it) => it.az_key)).toEqual(['k1', 'k2']);
    });

    // Spec parity: an inserted item that is ITSELF a view root owns its subtree
    // under its own id, rather than registering in the container's view.
    it('registers an inserted item that is itself a view root', () => {
        const client = newClient();
        replaceSharedAzList(client, ['k1']);
        client._applyOps([
            [OP_INSERT, 'json_l:R-0t0', 'solo', -1, { f: 'C', s: CHILD_STATICS, d: ['solo', '0'] }],
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
            [OP_TEXT, 'json_x:R-0t0', { f: 'never-cached', d: [] }],
            [OP_TEXT, 'json_x:R-0t0', 'applied'],
        ]);
        expect(client.tree().children).toEqual(['applied']);
        expect(warn).toHaveBeenCalledTimes(1);
    });
});

// An event that changes nothing gets no reply, so a client holding the press
// cannot tell it handled from lost. pushEventAwait tags the frame with a ref the
// server always answers, and only a frame naming that ref settles it.
describe('JSON client reply refs', () => {
    function connected() {
        const ws = installWebSocketStub();
        const client = newClient();
        client.connect();
        ws.latest().simulateOpen();
        ws.latest().simulateMessage(
            JSON.stringify({
                o: [[OP_REPLACE, 'json_x', { f: 'R', s: ROOT_STATICS, d: ['json_x', ''] }]],
            }),
        );
        return { ws, client };
    }

    it('tags the event frame with a ref and resolves on the frame naming it', async () => {
        const { ws, client } = connected();
        try {
            const reply = client.pushEventAwait('save', { id: '1' });
            expect(JSON.parse(ws.latest().sent.at(-1))).toEqual(['json_x', 'save', { id: '1' }, 1]);
            let settled = false;
            reply.then(() => {
                settled = true;
            });
            // A push is not the reply, even when it arrives first.
            ws.latest().simulateMessage(JSON.stringify({ o: [[OP_TEXT, 'json_x:R-0t0', 'tick']] }));
            await Promise.resolve();
            expect(settled).toBe(false);
            ws.latest().simulateMessage('{"r":1}');
            await expect(reply).resolves.toBeDefined();
        } finally {
            ws.restore();
        }
    });

    it('resolves on the state the reply produced', async () => {
        const { ws, client } = connected();
        try {
            const reply = client.pushEventAwait('inc');
            ws.latest().simulateMessage(
                JSON.stringify({ r: 1, o: [[OP_TEXT, 'json_x:R-0t0', 'one']] }),
            );
            const tree = await reply;
            expect(tree.children).toEqual(['one']);
        } finally {
            ws.restore();
        }
    });

    it('settles each pending frame by its own ref', async () => {
        const { ws, client } = connected();
        try {
            const first = client.pushEventAwait('a');
            const second = client.pushEventAwait('b');
            let firstDone = false;
            first.then(() => {
                firstDone = true;
            });
            ws.latest().simulateMessage('{"r":2}');
            await expect(second).resolves.toBeDefined();
            expect(firstDone).toBe(false);
            ws.latest().simulateMessage('{"r":1}');
            await expect(first).resolves.toBeDefined();
        } finally {
            ws.restore();
        }
    });

    it('rejects every pending frame when the socket closes', async () => {
        const { ws, client } = connected();
        try {
            const reply = client.pushEventAwait('save');
            client.close();
            await expect(reply).rejects.toThrow(/closed \(1000\)/);
        } finally {
            ws.restore();
        }
    });

    it('tags a navigate with a ref and resolves on its OP_REPLACE', async () => {
        const { ws, client } = connected();
        try {
            const reply = client.navigateAwait('/b?x=1');
            expect(JSON.parse(ws.latest().sent.at(-1))).toEqual([
                'navigate',
                { path: '/b', qs: 'x=1' },
                1,
            ]);
            ws.latest().simulateMessage(
                JSON.stringify({
                    r: 1,
                    o: [[OP_REPLACE, 'json_b', { f: 'B', s: ROOT_STATICS, d: ['json_b', ''] }]],
                }),
            );
            const tree = await reply;
            expect(tree.id).toBe('json_b');
        } finally {
            ws.restore();
        }
    });

    // The frame answering the navigate carries the full navigate that replaces
    // the socket: it settles before the reload fails what the socket still owes.
    it('resolves a navigate answered by a full navigate', async () => {
        const { ws, client } = connected();
        try {
            const reply = client.navigateAwait('/gated');
            ws.latest().simulateMessage(
                JSON.stringify({ r: 1, e: [[EFFECT_NAVIGATE, '/gated', { full: true }]] }),
            );
            await expect(reply).resolves.toBeDefined();
        } finally {
            ws.restore();
        }
    });

    // A full navigate detaches the old socket's handlers, so its close fails
    // nothing: the refs it still owes are failed when it is replaced.
    it('rejects what a socket replaced by a full navigate still owes', async () => {
        const { ws, client } = connected();
        try {
            const reply = client.pushEventAwait('save');
            ws.latest().simulateMessage(
                JSON.stringify({ e: [[EFFECT_NAVIGATE, '/elsewhere', { full: true }]] }),
            );
            await expect(reply).rejects.toThrow(/replaced by a full navigate to \/elsewhere/);
        } finally {
            ws.restore();
        }
    });

    it('rejects at once, and forgets the ref, when the frame cannot be sent', async () => {
        const { ws, client } = connected();
        try {
            client.ws.send = () => {
                throw new Error('still connecting');
            };
            await expect(client.pushEventAwait('save')).rejects.toThrow('still connecting');
            expect(client._replies.size).toBe(0);
        } finally {
            ws.restore();
        }
    });

    it('keeps the reconnect path when a navigate cannot be sent', async () => {
        const { ws, client } = connected();
        try {
            const before = client.wsUrl;
            client.ws.send = () => {
                throw new Error('still connecting');
            };
            await expect(client.navigateAwait('/elsewhere')).rejects.toThrow('still connecting');
            expect(client.wsUrl).toBe(before);
        } finally {
            ws.restore();
        }
    });

    it('rejects a frame whose ref never comes back', async () => {
        vi.useFakeTimers();
        const { ws, client } = connected();
        try {
            const reply = client.pushEventAwait('save', {}, 'json_x', 50);
            const caught = expect(reply).rejects.toThrow(/no reply to save \(ref 1\)/);
            vi.advanceTimersByTime(50);
            await caught;
            // A late reply for a ref already given up on is ignored.
            expect(() => ws.latest().simulateMessage('{"r":1}')).not.toThrow();
        } finally {
            ws.restore();
            vi.useRealTimers();
        }
    });
});

// The reconnect re-mounts the page on screen: what `wsUrl` names moves only when a
// navigate's reply arrives that put its page there, and a navigation a down socket
// could not carry goes out once the reconnect has put that page back.
describe('JSON client reconnect to the page on screen', () => {
    const replace = (id) =>
        JSON.stringify({ o: [[OP_REPLACE, id, { f: 'R', s: ROOT_STATICS, d: [id, ''] }]] });
    const path = (url) => new URL(url).searchParams.get('_az_path');
    const navs = (sock) => sock.sent.map((f) => JSON.parse(f)).filter((f) => f[0] === 'navigate');

    function connected() {
        vi.useFakeTimers();
        const ws = installWebSocketStub();
        const client = newClient();
        client.connect();
        ws.latest().simulateOpen();
        ws.latest().simulateMessage(replace('json_x'));
        return { ws, client };
    }

    /** Drop the socket abnormally and open the reconnect. */
    function redial(ws, code = 1006) {
        ws.latest().close(code);
        vi.advanceTimersByTime(15000);
        ws.latest().simulateOpen();
        return ws.latest();
    }

    afterEach(() => vi.useRealTimers());

    it('does not move the reconnect when a navigate is sent', () => {
        const { ws, client } = connected();
        try {
            client.navigate('/json/b?x=1');
            const sock = redial(ws);
            expect(path(sock.url)).toBe('/json/x');
            expect(sock.url).not.toContain('x=1');
        } finally {
            ws.restore();
        }
    });

    it('moves the reconnect when the reply put the page on screen', () => {
        const { ws, client } = connected();
        try {
            client.navigate('/json/b?x=1');
            const ref = navs(ws.latest()).at(-1)[2];
            ws.latest().simulateMessage(
                JSON.stringify({ ...JSON.parse(replace('json_b')), r: ref }),
            );
            const sock = redial(ws);
            expect(path(sock.url)).toBe('/json/b');
            expect(new URL(sock.url).searchParams.get('x')).toBe('1');
        } finally {
            ws.restore();
        }
    });

    it('does not move the reconnect when the reply navigates again', () => {
        const { ws, client } = connected();
        try {
            client.navigate('/json/gated');
            const ref = navs(ws.latest()).at(-1)[2];
            ws.latest().simulateMessage(
                JSON.stringify({
                    r: ref,
                    e: [[EFFECT_NAVIGATE, '/json/login', { replace: true }]],
                }),
            );
            const sock = redial(ws);
            expect(path(sock.url)).toBe('/json/x');
            // The redirect's own navigate, lost with the socket, goes out after the mount.
            sock.simulateMessage(replace('json_x'));
            expect(navs(sock).map((f) => f[1])).toEqual([{ path: '/json/login', qs: '' }]);
        } finally {
            ws.restore();
        }
    });

    it('moves the reconnect when the reply only navigates to a fragment of the page', () => {
        const { ws, client } = connected();
        try {
            client.navigate('/json/b?x=1');
            const ref = navs(ws.latest()).at(-1)[2];
            ws.latest().simulateMessage(
                JSON.stringify({
                    ...JSON.parse(replace('json_b')),
                    r: ref,
                    e: [[EFFECT_NAVIGATE, '#bottom', {}]],
                }),
            );
            // A fragment has nothing to ask the server: no frame goes out for it.
            expect(navs(ws.latest()).length).toBe(1);
            const sock = redial(ws);
            expect(path(sock.url)).toBe('/json/b');
        } finally {
            ws.restore();
        }
    });

    it('sends a navigate whose reply a close lost again after the reconnect mounts', () => {
        const { ws, client } = connected();
        try {
            client.navigate('/json/b');
            const sock = redial(ws);
            expect(navs(sock)).toEqual([]);
            sock.simulateMessage(replace('json_x'));
            expect(navs(sock).map((f) => f[1])).toEqual([{ path: '/json/b', qs: '' }]);
        } finally {
            ws.restore();
        }
    });

    it('holds a navigate issued while down, the last one winning, for the reconnect', () => {
        const { ws, client } = connected();
        try {
            ws.latest().close(1006);
            client.navigate('/json/a');
            client.navigate('/json/b');
            vi.advanceTimersByTime(15000);
            const sock = ws.latest();
            sock.simulateOpen();
            expect(path(sock.url)).toBe('/json/x');
            sock.simulateMessage(replace('json_x'));
            expect(navs(sock).map((f) => f[1])).toEqual([{ path: '/json/b', qs: '' }]);
        } finally {
            ws.restore();
        }
    });

    it('drops a pending navigate naming the page on screen', () => {
        const { ws, client } = connected();
        try {
            ws.latest().close(1006);
            client.navigate('/json/x');
            vi.advanceTimersByTime(15000);
            ws.latest().simulateOpen();
            ws.latest().simulateMessage(replace('json_x'));
            expect(navs(ws.latest())).toEqual([]);
        } finally {
            ws.restore();
        }
    });

    // An owed navigate is one the server never ran: one it ran and could not
    // complete is answered with a full navigate before the close.
    for (const code of [4500, 4501]) {
        it(`sends again a navigate a ${code} lost once the page on screen is back`, () => {
            const { ws, client } = connected();
            try {
                client.navigate('/json/b');
                const sock = redial(ws, code);
                expect(path(sock.url)).toBe('/json/x');
                sock.simulateMessage(replace('json_x'));
                expect(navs(sock).map((f) => f[1])).toEqual([{ path: '/json/b', qs: '' }]);
            } finally {
                ws.restore();
            }
        });
    }

    it('connects fresh to the page of a navigate the server answered with a full navigate', () => {
        const { ws, client } = connected();
        try {
            client.navigate('/json/b');
            const sock = ws.latest();
            const ref = navs(sock)[0][2];
            sock.simulateMessage(
                JSON.stringify({ r: ref, e: [[10, '/json/b', { full: true, replace: true }]] }),
            );
            expect(ws.instances.length).toBe(2);
            expect(path(ws.latest().url)).toBe('/json/b');
            // The old socket's 4500 is detached: nothing is sent again.
            sock.close(4500);
            ws.latest().simulateOpen();
            ws.latest().simulateMessage(replace('json_b'));
            expect(navs(ws.latest())).toEqual([]);
        } finally {
            ws.restore();
        }
    });

    it('runs no navigation a reply to an older navigate carries: newer wins', () => {
        const { ws, client } = connected();
        try {
            client.navigate('/json/b');
            client.navigate('/json/c');
            const sock = ws.latest();
            const [b] = navs(sock).map((f) => f[2]);
            sock.simulateMessage(
                JSON.stringify({ r: b, e: [[10, '/json/b', { full: true, replace: true }]] }),
            );
            sock.simulateMessage(JSON.stringify({ r: b + 1, e: [[10, '/json/login', {}]] }));
            // The stale full navigate did not replace the socket; the newer reply's
            // redirect went out on it.
            expect(ws.instances.length).toBe(1);
            expect(navs(sock).map((f) => f[1].path)).toEqual(['/json/b', '/json/c', '/json/login']);
        } finally {
            ws.restore();
        }
    });

    // The page on screen is the path and query a navigate asked for, as they were
    // written: a pending navigate naming it is dropped whatever the query's encoding.
    it('drops a pending navigate naming the page on screen, its query percent-encoded', () => {
        const { ws, client } = connected();
        try {
            client.navigate('/json/b?q=a%20b');
            const ref = navs(ws.latest()).at(-1)[2];
            ws.latest().simulateMessage(
                JSON.stringify({ ...JSON.parse(replace('json_b')), r: ref }),
            );
            ws.latest().close(1006);
            client.navigate('/json/b?q=a%20b');
            expect(client._pendingNav).toBeNull();
            vi.advanceTimersByTime(15000);
            ws.latest().simulateOpen();
            ws.latest().simulateMessage(replace('json_b'));
            expect(navs(ws.latest())).toEqual([]);
        } finally {
            ws.restore();
        }
    });

    // Two pages that navigate to each other on connect, every navigate answered with
    // a full navigate: the fresh connections answering navigates issued with no
    // person acting are bounded, 10 then one per 30 s, slow until 30 s pass with none.
    it('bounds the fresh connections answering navigates issued with no person acting', () => {
        const { ws, client } = connected();
        try {
            /** The open socket's view navigates on connect; answered with a full one. */
            const bounce = (to) => {
                const sock = ws.latest();
                sock.simulateMessage(JSON.stringify({ e: [[10, to, {}]] }));
                const ref = navs(sock).at(-1)[2];
                sock.simulateMessage(
                    JSON.stringify({ r: ref, e: [[10, to, { full: true, replace: true }]] }),
                );
            };
            const opened = () => ws.instances.length;
            for (let i = 0; i < 10; i++) {
                const n = opened();
                bounce(i % 2 ? '/json/a' : '/json/b');
                expect(opened()).toBe(n + 1);
                ws.latest().simulateOpen();
                ws.latest().simulateMessage(replace('json_x'));
            }
            const n = opened();
            bounce('/json/a');
            expect(opened()).toBe(n);
            vi.advanceTimersByTime(29999);
            expect(opened()).toBe(n);
            vi.advanceTimersByTime(1);
            expect(opened()).toBe(n + 1);
            ws.latest().simulateOpen();
            ws.latest().simulateMessage(replace('json_x'));
            // Still slow: the next comes right after.
            bounce('/json/b');
            expect(opened()).toBe(n + 1);
        } finally {
            client.close();
            ws.restore();
        }
    });

    it('never bounds a full navigate answering a navigate the user issued', () => {
        const { ws, client } = connected();
        try {
            for (let i = 0; i < 20; i++) {
                const n = ws.instances.length;
                client.noteUserInput(); // a person's touch
                client.navigate('/json/b');
                const sock = ws.latest();
                const ref = navs(sock).at(-1)[2];
                sock.simulateMessage(
                    JSON.stringify({ r: ref, e: [[10, '/json/b', { full: true, replace: true }]] }),
                );
                expect(ws.instances.length).toBe(n + 1);
                ws.latest().simulateOpen();
                ws.latest().simulateMessage(replace('json_x'));
            }
        } finally {
            client.close();
            ws.restore();
        }
    });

    /** The open socket's view navigates on its own (a push), answered with a full one. */
    function bounceOnce(ws, to) {
        const sock = ws.latest();
        sock.simulateMessage(JSON.stringify({ e: [[10, to, {}]] }));
        const ref = navs(sock).at(-1)[2];
        sock.simulateMessage(
            JSON.stringify({ r: ref, e: [[10, to, { full: true, replace: true }]] }),
        );
    }
    const full = () => Array(10).fill({ t: Date.now(), slow: false });

    it('never bounds a user stepping through server redirects', () => {
        const { ws, client } = connected();
        try {
            for (let i = 0; i < 20; i++) {
                const n = ws.instances.length;
                const sock = ws.latest();
                client.noteUserInput(); // a person's touch...
                client.pushEvent('next'); // ...raises the step's event...
                sock.simulateMessage(JSON.stringify({ e: [[10, '/json/b', {}]] })); // ...redirected
                const ref = navs(sock).at(-1)[2];
                sock.simulateMessage(
                    JSON.stringify({ r: ref, e: [[10, '/json/b', { full: true, replace: true }]] }),
                );
                expect(ws.instances.length).toBe(n + 1);
                ws.latest().simulateOpen();
                ws.latest().simulateMessage(replace('json_x'));
                vi.advanceTimersByTime(20000);
            }
        } finally {
            client.close();
            ws.restore();
        }
    });

    it('stays slow while the loop keeps coming, however slowly', () => {
        const { ws, client } = connected();
        try {
            client._navLoads = [{ t: Date.now() - 10000, slow: true }];
            const n = ws.instances.length;
            bounceOnce(ws, '/json/b');
            expect(ws.instances.length).toBe(n);
        } finally {
            client.close();
            ws.restore();
        }
    });

    it('a navigate issued with no input keeps that across its replay', () => {
        const { ws, client } = connected();
        try {
            client._navLoads = full();
            ws.latest().simulateMessage(JSON.stringify({ e: [[10, '/json/b', {}]] }));
            const sock = redial(ws); // its reply lost; sent again once the page is back
            sock.simulateMessage(replace('json_x'));
            const ref = navs(sock).at(-1)[2];
            const n = ws.instances.length;
            sock.simulateMessage(
                JSON.stringify({ r: ref, e: [[10, '/json/b', { full: true, replace: true }]] }),
            );
            expect(ws.instances.length).toBe(n);
        } finally {
            client.close();
            ws.restore();
        }
    });

    it("a person's navigate during a delayed fresh connection connects to their page", () => {
        const { ws, client } = connected();
        try {
            client._navLoads = full();
            const n = ws.instances.length;
            bounceOnce(ws, '/json/b');
            expect(ws.instances.length).toBe(n);
            client.noteUserInput();
            client.navigate('/json/c');
            // At once, straight to their page: not the loop's.
            expect(ws.instances.length).toBe(n + 1);
            expect(path(ws.latest().url)).toBe('/json/c');
            ws.latest().simulateOpen();
            ws.latest().simulateMessage(replace('json_c'));
            expect(navs(ws.latest())).toEqual([]);
            // The delayed one is gone (short of the heartbeat's missed-pong close).
            vi.advanceTimersByTime(31000);
            expect(ws.instances.length).toBe(n + 1);
        } finally {
            client.close();
            ws.restore();
        }
    });

    it('a bare app navigate or pushEvent is no person: the loop stays bounded', () => {
        const { ws, client } = connected();
        try {
            client._navLoads = full();
            client.pushEvent('hello');
            const n = ws.instances.length;
            client.navigate('/json/b'); // the app's own, not a person's
            const sock = ws.latest();
            const ref = navs(sock).at(-1)[2];
            sock.simulateMessage(
                JSON.stringify({ r: ref, e: [[10, '/json/b', { full: true, replace: true }]] }),
            );
            expect(ws.instances.length).toBe(n);
        } finally {
            client.close();
            ws.restore();
        }
    });

    it('a mount clears the input: a view navigating on its own is bounded again', () => {
        const { ws, client } = connected();
        try {
            client._navLoads = full();
            client.noteUserInput();
            ws.latest().simulateMessage(replace('json_x')); // a new view, no input since
            const n = ws.instances.length;
            bounceOnce(ws, '/json/b');
            expect(ws.instances.length).toBe(n);
        } finally {
            client.close();
            ws.restore();
        }
    });

    it("a tap is a person's touch: its navigate is never bounded", () => {
        const { ws, client } = connected();
        try {
            client._navLoads = full();
            client.tap({ on_tap: [10, '/json/b', {}] });
            const sock = ws.latest();
            const ref = navs(sock).at(-1)[2];
            const n = ws.instances.length;
            sock.simulateMessage(
                JSON.stringify({ r: ref, e: [[10, '/json/b', { full: true, replace: true }]] }),
            );
            expect(ws.instances.length).toBe(n + 1);
        } finally {
            client.close();
            ws.restore();
        }
    });

    it('a no-person live hop keeps no input: the fresh connection after it is bounded', () => {
        const { ws, client } = connected();
        try {
            client._navLoads = full();
            client.navigate('/json/r'); // the app's own, no person
            const sock = ws.latest();
            const ref = navs(sock).at(-1)[2];
            sock.simulateMessage(
                JSON.stringify({
                    r: ref,
                    o: [[OP_REPLACE, 'json_r', { f: 'R', s: ROOT_STATICS, d: ['json_r', ''] }]],
                }),
            );
            const n = ws.instances.length;
            bounceOnce(ws, '/json/w');
            expect(ws.instances.length).toBe(n);
        } finally {
            client.close();
            ws.restore();
        }
    });

    it('an app navigate during a delayed fresh connection waits it out', () => {
        const { ws, client } = connected();
        try {
            client._navLoads = full();
            const n = ws.instances.length;
            bounceOnce(ws, '/json/b');
            client.navigate('/json/c'); // the app's own, no person
            expect(ws.instances.length).toBe(n);
        } finally {
            client.close();
            ws.restore();
        }
    });

    it('a navigate a frame runs during a delayed fresh connection waits it out', () => {
        // A person acted after the bounded navigate went out; the frame answering it
        // carries another navigate, which is the server's, not theirs.
        const { ws, client } = connected();
        try {
            client._navLoads = full();
            const sock = ws.latest();
            sock.simulateMessage(JSON.stringify({ e: [[10, '/json/b', {}]] }));
            const ref = navs(sock).at(-1)[2];
            client.noteUserInput();
            const n = ws.instances.length;
            sock.simulateMessage(
                JSON.stringify({
                    r: ref,
                    e: [
                        [10, '/json/b', { full: true, replace: true }],
                        [10, '/json/c', {}],
                    ],
                }),
            );
            expect(ws.instances.length).toBe(n);
            // The delayed connection opens to the page it was for; the frame's own
            // navigate waits, pending, for that page to be back.
            vi.advanceTimersByTime(30000);
            expect(ws.instances.length).toBe(n + 1);
            expect(path(ws.latest().url)).toBe('/json/b');
        } finally {
            client.close();
            ws.restore();
        }
    });

    it('navigateAwait is no person: the fresh connection answering it is bounded', async () => {
        const { ws, client } = connected();
        try {
            client._navLoads = full();
            const sock = ws.latest();
            const p = client.navigateAwait('/json/b').catch(() => {});
            const ref = navs(sock).at(-1)[2];
            const n = ws.instances.length;
            sock.simulateMessage(
                JSON.stringify({ r: ref, e: [[10, '/json/b', { full: true, replace: true }]] }),
            );
            await p;
            expect(ws.instances.length).toBe(n);
        } finally {
            client.close();
            ws.restore();
        }
    });

    it("a person's chain through a live hop stays theirs", () => {
        const { ws, client } = connected();
        try {
            client._navLoads = full();
            client.noteUserInput();
            client.navigate('/json/r');
            const sock = ws.latest();
            const ref = navs(sock).at(-1)[2];
            sock.simulateMessage(
                JSON.stringify({
                    r: ref,
                    o: [[OP_REPLACE, 'json_r', { f: 'R', s: ROOT_STATICS, d: ['json_r', ''] }]],
                }),
            );
            const n = ws.instances.length;
            bounceOnce(ws, '/json/w'); // the live page's view navigates on connect
            expect(ws.instances.length).toBe(n + 1);
        } finally {
            client.close();
            ws.restore();
        }
    });

    it('close() cancels a delayed fresh connection', () => {
        const { ws, client } = connected();
        try {
            client._navLoads = full();
            const n = ws.instances.length;
            bounceOnce(ws, '/json/b');
            client.close();
            vi.advanceTimersByTime(60000);
            expect(ws.instances.length).toBe(n);
        } finally {
            ws.restore();
        }
    });

    // Before a first mount nothing proves a socket will ever open: a navigate
    // connects fresh to its page, as the web client loads it, instead of waiting.
    for (const failed of [false, true]) {
        it(`connects fresh to a navigate before the first mount (${failed ? 'first connect failed' : 'still connecting'})`, () => {
            vi.useFakeTimers();
            const ws = installWebSocketStub();
            try {
                const client = newClient();
                client.connect().catch(() => {});
                if (failed) ws.latest().close(1006);
                client.navigate('/json/b');
                expect(ws.instances.length).toBe(2);
                expect(path(ws.latest().url)).toBe('/json/b');
                ws.latest().simulateOpen();
                ws.latest().simulateMessage(replace('json_b'));
                expect(navs(ws.latest())).toEqual([]);
                expect(client.tree().id).toBe('json_b');
            } finally {
                ws.restore();
            }
        });
    }

    /** Let the reconnect timer start the next attempt, then fail it without opening. */
    function failNextAttempt(ws) {
        const n = ws.instances.length;
        vi.advanceTimersByTime(15000);
        expect(ws.instances.length).toBe(n + 1);
        ws.latest().close(1006);
    }

    it('connects fresh to a navigate issued while down once a later attempt fails', () => {
        const { ws, client } = connected();
        try {
            ws.latest().close(1006);
            client.navigate('/json/b');
            failNextAttempt(ws);
            // A fresh connect to the pending page, as the browser loads it.
            expect(path(ws.latest().url)).toBe('/json/b');
            expect(client._pendingNav).toBeNull();
        } finally {
            ws.restore();
        }
    });

    it('waits while the attempt in flight when the navigate was issued fails', () => {
        const { ws, client } = connected();
        try {
            ws.latest().close(1006);
            vi.advanceTimersByTime(15000); // an attempt starts
            client.navigate('/json/b'); // pended while it is in flight
            ws.latest().close(1006); // it fails
            vi.advanceTimersByTime(15000);
            expect(path(ws.latest().url)).toBe('/json/x');
            ws.latest().close(1006); // one started after the pend fails
            expect(path(ws.latest().url)).toBe('/json/b');
        } finally {
            ws.restore();
        }
    });

    it('connects fresh to a navigate a close lost only once an attempt at the cap fails', () => {
        const { ws, client } = connected();
        try {
            client.navigate('/json/b');
            ws.latest().close(1006); // the reply is lost
            for (let n = 0; n < 3; n++) {
                failNextAttempt(ws); // 1 s, 2 s, 5 s
                expect(client._pendingNav?.path).toBe('/json/b');
            }
            failNextAttempt(ws); // at the 10 s cap
            expect(path(ws.latest().url)).toBe('/json/b');
            expect(client._pendingNav).toBeNull();
        } finally {
            ws.restore();
        }
    });

    it('an attempt that opens and closes before its mount connects nowhere fresh', () => {
        const { ws, client } = connected();
        try {
            ws.latest().close(1006);
            client.navigate('/json/b');
            vi.advanceTimersByTime(15000);
            ws.latest().simulateOpen();
            ws.latest().close(1006);
            expect(client._pendingNav?.path).toBe('/json/b');
            vi.advanceTimersByTime(15000);
            expect(path(ws.latest().url)).toBe('/json/x');
        } finally {
            ws.restore();
        }
    });

    it('drops a pending navigate when one to the page on screen follows it', () => {
        const { ws, client } = connected();
        try {
            ws.latest().close(1006);
            client.navigate('/json/b');
            client.navigate('/json/x');
            expect(client._pendingNav).toBeNull();
            failNextAttempt(ws);
            expect(path(ws.latest().url)).toBe('/json/x');
        } finally {
            ws.restore();
        }
    });

    it('connects fresh to a navigate issued after a deliberate end (1000)', () => {
        const { ws, client } = connected();
        try {
            ws.latest().close(1000);
            const before = ws.instances.length;
            client.navigate('/json/b');
            expect(ws.instances.length).toBe(before + 1);
            expect(path(ws.latest().url)).toBe('/json/b');
            expect(client._pendingNav).toBeNull();
        } finally {
            ws.restore();
        }
    });

    it('sends nothing for a navigate issued after the client was closed', () => {
        const { ws, client } = connected();
        try {
            client.close();
            const before = ws.instances.length;
            client.navigate('/json/b');
            vi.advanceTimersByTime(60000);
            expect(ws.instances.length).toBe(before);
        } finally {
            ws.restore();
        }
    });

    it('connects fresh to a pending navigate after a deliberate end (1000)', () => {
        const { ws, client } = connected();
        try {
            client.navigate('/json/b');
            ws.latest().close(1000);
            expect(path(ws.latest().url)).toBe('/json/b');
            expect(ws.instances.length).toBe(2);
        } finally {
            ws.restore();
        }
    });
});
