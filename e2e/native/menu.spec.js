import { expect, test } from '@playwright/test';
import { NativeClient } from '../utils/native_client.js';

// Native (JSON) navigation: a menu whose buttons navigate to other example views
// on the same socket (the server's handle_navigate re-mounts and replies with
// OP_REPLACE; the native client applies it). Covers the navigate command path
// the browser tests don't reach (those drive the browser client, not this one).
test.describe('native (JSON) wire -- menu navigation', () => {
    test('navigates to another view by tapping a navigate button', async ({ baseURL }) => {
        const client = new NativeClient(baseURL, '/native/menu');
        await client.connect();
        try {
            const root = client.tree();
            expect(root.id).toBe('native_menu');
            const [counterBtn, listBtn] = root.children;
            // Event props are raw command arrays: [EFFECT_NAVIGATE, path].
            expect(counterBtn).toMatchObject({ type: 'Button', on_tap: [10, '/native/counter'] });
            expect(listBtn.on_tap).toEqual([10, '/native/list']);

            // Tap "Counter" -> transition to the counter view.
            client.tap(counterBtn);
            await client.waitFor((t) => t.id === 'native_counter');
            expect(client.tree().children[0].children).toEqual(['Count: ', '0']);
        } finally {
            client.close();
        }
    });

    test('navigates via a handler-returned effect (the "e" array)', async ({ baseURL }) => {
        const client = new NativeClient(baseURL, '/native/menu');
        await client.connect();
        try {
            // The last button push_events; the server handler returns a navigate
            // effect, which the client applies from the "e" array.
            const effectBtn = client.tree().children[6];
            expect(effectBtn.on_tap).toEqual([0, 'open_counter']);
            client.tap(effectBtn);
            await client.waitFor((t) => t.id === 'native_counter');
        } finally {
            client.close();
        }
    });

    test('applies ops after navigating (server keeps the process view id)', async ({ baseURL }) => {
        const client = new NativeClient(baseURL, '/native/menu');
        await client.connect();
        // List item texts (each keyed Text node's spliced content).
        const rows = (t) => t.children.map((r) => r.children[0]);
        try {
            // Navigate menu -> list. The OP_REPLACE ViewId stays the process id
            // (native_menu), not the rendered root's id (native_list).
            client.tap(client.tree().children[1]); // "List"
            await client.waitFor((t) => t.id === 'native_list');
            expect(rows(client.tree())).toEqual(['One', 'Two', 'Three']);

            // A stream op now arrives prefixed with the process view id; it must
            // still resolve against the (re-mounted) list and render the new item.
            client.pushEvent('add', { id: '9', text: 'Nine' });
            await client.waitFor((t) => rows(t).includes('Nine'));
        } finally {
            client.close();
        }
    });
});
