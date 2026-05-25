import { expect, test } from '@playwright/test';
import { NativeClient } from '../utils/native_client.js';

// Native (JSON) conditional/subtree rendering: a tab switcher whose content
// area swaps subtrees when `selected` changes (push_event -> OP_UPDATE of the
// content slot). Proves the native client applies a re-rendered subtree, which
// the counter (text) and list (stream) specs don't cover.
test.describe('native (JSON) wire -- tabs', () => {
    test('swaps the content subtree when a tab is selected', async ({ baseURL }) => {
        const client = new NativeClient(baseURL, '/native/tabs');
        await client.connect();
        // All text leaves anywhere in the tree (shape-robust).
        const texts = (node, out = []) => {
            if (typeof node === 'string') out.push(node);
            else if (node?.children) for (const c of node.children) texts(c, out);
            return out;
        };
        try {
            const root = client.tree();
            expect(root.type).toBe('Column');
            expect(root.id).toBe('native_tabs');
            // Home tab content initially.
            expect(texts(root)).toContain('Welcome home');
            expect(texts(root)).not.toContain('About Arizona');

            // Switch to About -> content subtree swaps.
            client.pushEvent('select_about');
            await client.waitFor((t) => texts(t).includes('About Arizona'));
            expect(texts(client.tree())).not.toContain('Welcome home');

            // Back to Home.
            client.pushEvent('select_home');
            await client.waitFor((t) => texts(t).includes('Welcome home'));
            expect(texts(client.tree())).not.toContain('About Arizona');
        } finally {
            client.close();
        }
    });
});
