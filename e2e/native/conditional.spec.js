import { expect, test } from '@playwright/test';
import { NativeClient } from '../utils/native_client.js';

// Native (JSON) wire: a node the DIFF creates must be addressable. The view's
// content slot conditionally renders a stateful child, so toggling it on ships an
// OP_TEXT whose payload introduces the child's own view id -- nothing the first
// OP_REPLACE carried. Tapping the child then produces an op addressed to that new
// view; a client that only indexes the OP_REPLACE tree cannot resolve it.
test.describe('native (JSON) wire -- conditional stateful child', () => {
    test('addresses a child view the diff installed after the first frame', async ({ baseURL }) => {
        const client = new NativeClient(baseURL, '/native/conditional');
        await client.connect();
        try {
            const root = client.tree();
            expect(root.id).toBe('native_conditional');
            // Only the toggle button plus the empty content slot.
            expect(root.children[0]).toMatchObject({ type: 'Button' });

            // Toggle on -> OP_TEXT installs the child's subtree (its own az_view).
            client.tap(root.children[0]);
            const shown = await client.waitFor((t) =>
                t.children.some((c) => c.id === 'cond_child'),
            );
            const child = shown.children.find((c) => c.id === 'cond_child');
            // child.children[0] is the Text node [label, count].
            expect(child.children[0].children).toEqual(['C', '0']);

            // Tap the child's own button: the event routes to `cond_child`, and the
            // reply is an op addressed to a node only the diff ever rendered.
            client.tap(child.children[1]);
            const after = await client.waitFor((t) => {
                const c = t.children.find((n) => n.id === 'cond_child');
                return c && c.children[0].children.at(-1) === '1';
            });
            expect(after.children.find((c) => c.id === 'cond_child').children[0].children).toEqual([
                'C',
                '1',
            ]);
        } finally {
            client.close();
        }
    });
});
