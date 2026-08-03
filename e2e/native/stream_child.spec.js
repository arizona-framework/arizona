import { expect, test } from '@playwright/test';
import { NativeClient } from '../utils/native_client.js';

// Native (JSON) wire: a stateful child installed by an OP_ITEM_PATCH's INNER op.
// Opening the stream item ships `[7, container, key, [[0, itemAz, <child payload>]]]`
// -- the child's view id arrives through an item-scoped op, never a top-level one.
// The child's own ops then come back as top-level "kid-1:az", so the client only
// resolves them if an inner-op rebuild registers the view it created (an
// item-local `az` map alone cannot).
test.describe('native (JSON) wire -- stateful child inside a stream item', () => {
    test('addresses a child view an item-patch inner op installed', async ({ baseURL }) => {
        const client = new NativeClient(baseURL, '/native/stream-child');
        await client.connect();
        // tree(): [Open button, Row]; the Row's children are the label plus the
        // conditional slot (empty until opened).
        const kidOf = (t) => t.children[1].children.find((c) => c && c.id === 'kid-1');
        try {
            expect(kidOf(client.tree())).toBeUndefined();

            // Open the item -> OP_ITEM_PATCH whose inner OP_TEXT installs the child.
            client.tap(client.tree().children[0]);
            const shown = await client.waitFor((t) => kidOf(t) !== undefined);
            expect(kidOf(shown).children[0].children).toEqual(['K', '0']);

            // Tap the child's own button: the event routes to `kid-1`, and the reply
            // is a top-level op addressed to a view only an inner op ever created.
            client.tap(kidOf(client.tree()).children[1]);
            const after = await client.waitFor((t) => {
                const kid = kidOf(t);
                return kid && kid.children[0].children.at(-1) === '1';
            });
            expect(kidOf(after).children[0].children).toEqual(['K', '1']);
        } finally {
            client.close();
        }
    });
});
