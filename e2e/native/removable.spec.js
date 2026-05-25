import { expect, test } from '@playwright/test';
import { NativeClient } from '../utils/native_client.js';

// Native (JSON) OP_REMOVE_NODE: a text dynamic returns the `remove` sentinel, so
// the server diffs to op 4 (target-agnostic; see arizona_diff:make_op/3). The
// client must drop the node, not crash on an unhandled op -- the parity gap with
// the browser client, which handles op 4 via el.remove().
test.describe('native (JSON) wire -- remove node', () => {
    test('drops a node when its dynamic returns the remove sentinel', async ({ baseURL }) => {
        const client = new NativeClient(baseURL, '/native/removable');
        await client.connect();
        // The banner text lives in the first child (a Text node).
        const banner = (t) => t.children[0].children[0];
        try {
            expect(banner(client.tree())).toBe('Banner!');

            // Tap "Hide" -> banner becomes `remove` -> OP_REMOVE_NODE -> the text
            // slot is dropped (the Text node goes empty), no crash.
            client.tap(client.tree().children[1]); // "Hide"
            await client.waitFor((t) => t.children[0].children.length === 0);
            expect(banner(client.tree())).toBeUndefined();
        } finally {
            client.close();
        }
    });
});
