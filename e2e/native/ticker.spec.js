import { expect, test } from '@playwright/test';
import { NativeClient } from '../utils/native_client.js';

// Native (JSON) server-pushed updates: a handle_info timer increments a count
// and pushes OP_TEXT unsolicited (the client sends no event). Proves
// timer/pubsub-driven ops reach the native client, which applies any incoming
// ops regardless of trigger.
test.describe('native (JSON) wire -- ticker (server push)', () => {
    test('applies server-pushed count updates without any client event', async ({ baseURL }) => {
        const client = new NativeClient(baseURL, '/native/ticker');
        await client.connect();
        // Text is ["Tick: ", "<count>"]; read the count leaf.
        const count = (t) => Number(t.children[0].children[1]);
        try {
            expect(client.tree().type).toBe('Column');
            expect(count(client.tree())).toBe(0);
            // No pushEvent -- the server's timer drives the updates.
            await client.waitFor((t) => count(t) >= 2, 5000);
            expect(count(client.tree())).toBeGreaterThanOrEqual(2);
        } finally {
            client.close();
        }
    });
});
