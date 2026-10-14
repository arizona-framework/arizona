import { expect, test } from '@playwright/test';
import { NativeClient } from '../utils/native_client.js';

// Native (JSON) multiple independent counters in one view: incrementing one
// region updates only its own count (distinct az slots). Proves independent
// event routing + az-numbering on native.
test.describe('native (JSON) wire -- multi counter', () => {
    test('increments one counter without touching the others', async ({ baseURL }) => {
        const client = new NativeClient(baseURL, '/native/multi');
        await client.connect();
        // Each row is [Text("X: ", count), +Button, -Button]; read the count leaf.
        const counts = (t) => t.children.map((row) => row.children[0].children[1]);
        try {
            const root = client.tree();
            expect(root.id).toBe('native_multi');
            expect(counts(root)).toEqual(['0', '0', '0']);

            // Tap counter B's "+" (second row, first button).
            client.tap(root.children[1].children[1]);
            await client.waitFor((t) => counts(t)[1] === '1');
            expect(counts(client.tree())).toEqual(['0', '1', '0']);

            // Tap counter B's "+" again, and counter C's "+": only those move.
            client.tap(client.tree().children[1].children[1]);
            client.tap(client.tree().children[2].children[1]);
            await client.waitFor((t) => counts(t)[2] === '1');
            expect(counts(client.tree())).toEqual(['0', '2', '1']);
        } finally {
            client.close();
        }
    });
});
