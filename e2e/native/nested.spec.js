import { expect, test } from '@playwright/test';
import { NativeClient } from '../utils/native_client.js';

// Native (JSON) nested event routing: a view embeds two stateful child counters.
// Tapping a child's button routes the event to THAT child's view id (not the
// root), so only that child's count changes -- proving the client resolves a
// tapped node to its enclosing az_view, like the browser. (The `multi` example,
// by contrast, routes per-region events to a single view.)
test.describe('native (JSON) wire -- nested stateful children', () => {
    test('routes a tap to the tapped child, leaving siblings untouched', async ({ baseURL }) => {
        const client = new NativeClient(baseURL, '/native/nested');
        await client.connect();
        // child.children[0] is the Text node ["<label>", "<count>"]; count is last.
        const count = (child) => child.children[0].children.at(-1);
        try {
            const root = client.tree();
            expect(count(root.children[0])).toBe('0'); // A
            expect(count(root.children[1])).toBe('0'); // B

            // Tap child A's button (children[1]) -> routes to child_a only.
            client.tap(root.children[0].children[1]);
            await client.waitFor((t) => count(t.children[0]) === '1');
            expect(count(client.tree().children[1])).toBe('0'); // B untouched

            // Tap child B -> routes to child_b; A stays at 1.
            client.tap(client.tree().children[1].children[1]);
            await client.waitFor((t) => count(t.children[1]) === '1');
            expect(count(client.tree().children[0])).toBe('1');
        } finally {
            client.close();
        }
    });
});
