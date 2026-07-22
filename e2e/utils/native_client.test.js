import { describe, expect, it } from 'vitest';
import { NativeClient } from './native_client.js';

// arizona_effect op codes (must match include/arizona_effect.hrl); the
// cross-language sync is guarded by test/arizona_effect_wire_SUITE.erl.
const EFFECT_PUSH_EVENT = 0;
const EFFECT_NAVIGATE = 10;

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
