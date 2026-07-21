import { expect, test } from '@playwright/test';
import { NativeClient } from '../utils/native_client.js';

// End-to-end proof of the native (JSON) render target: a real external client
// (no browser) speaks the actual WebSocket wire protocol to the running server,
// reconstructs the widget tree, and drives it via events -- the same contract a
// Kotlin/Swift client implements. The browser path is covered by the other specs;
// this proves the *native* wire works against the real server.
test.describe('native (JSON) wire -- counter', () => {
    test('renders the widget tree and increments over the real socket', async ({ baseURL }) => {
        const client = new NativeClient(baseURL, '/native/counter');
        await client.connect();
        try {
            const root = client.tree();
            // Root view node.
            expect(root.type).toBe('Column');
            expect(root.id).toBe('native_counter');

            const [text, incBtn, decBtn] = root.children;
            // Dynamic text: static "Count: " + the #slot value, spliced.
            expect(text.type).toBe('Text');
            expect(text.children).toEqual(['Count: ', '0']);
            // Event props are raw command arrays ([push_event, name]), not strings.
            expect(incBtn).toMatchObject({ type: 'Button', on_tap: [0, 'inc'] });
            expect(decBtn).toMatchObject({ type: 'Button', on_tap: [0, 'dec'] });

            // Tap "+" -> server round-trip -> OP_TEXT updates the count.
            client.tap(incBtn);
            let after = await client.waitFor((t) => t.children[0].children[1] === '1');
            expect(after.children[0].children).toEqual(['Count: ', '1']);

            // Again -> 2, then "-" -> 1: proves repeated diffs over the live socket.
            client.tap(incBtn);
            await client.waitFor((t) => t.children[0].children[1] === '2');
            client.tap(decBtn);
            after = await client.waitFor((t) => t.children[0].children[1] === '1');
            expect(after.children[0].children).toEqual(['Count: ', '1']);
        } finally {
            client.close();
        }
    });

    test('delivers an explicit push_event/2 payload to the handler', async ({ baseURL }) => {
        const client = new NativeClient(baseURL, '/native/counter');
        await client.connect();
        try {
            // The "Set" button's on_tap carries {value: 42}. The handler matches
            // the required `value` key, so if the client dropped the payload the
            // live process would crash and the count would never reach 42.
            const setBtn = client.tree().children.find((c) => c.on_tap && c.on_tap[1] === 'set');
            expect(setBtn.on_tap).toEqual([0, 'set', { value: 42 }]);
            client.tap(setBtn);
            const after = await client.waitFor((t) => t.children[0].children[1] === '42');
            expect(after.children[0].children).toEqual(['Count: ', '42']);
        } finally {
            client.close();
        }
    });
});
