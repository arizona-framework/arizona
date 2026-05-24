import { expect, test } from '@playwright/test';
import { NativeClient } from '../utils/native_client.js';

// Native (JSON) stream e2e: a real WebSocket client drives a keyed list over the
// live server, exercising the stream ops the counter spec can't reach --
// OP_INSERT / OP_REMOVE / OP_MOVE / OP_ITEM_PATCH and the OP_UPDATE full
// re-render. Proves the reference client applies the same keyed-list diff the
// browser does, against the real server.
test.describe('native (JSON) wire -- stream list', () => {
    test('applies insert/remove/move/patch/update over the real socket', async ({ baseURL }) => {
        const client = new NativeClient(baseURL, '/native/list');
        await client.connect();
        // [key, text] of each rendered Row (its inner #slot text spliced in).
        const rows = (t) => t.children.map((r) => [r.az_key, r.children[0]]);
        try {
            const root = client.tree();
            expect(root.type).toBe('Column');
            expect(root.id).toBe('native_list');
            expect(rows(root)).toEqual([
                ['1', 'One'],
                ['2', 'Two'],
                ['3', 'Three'],
            ]);

            // OP_INSERT (append).
            client.pushEvent('add', { id: '9', text: 'Nine' });
            await client.waitFor((t) => rows(t).some(([k]) => k === '9'));
            expect(rows(client.tree())).toEqual([
                ['1', 'One'],
                ['2', 'Two'],
                ['3', 'Three'],
                ['9', 'Nine'],
            ]);

            // OP_REMOVE.
            client.pushEvent('remove', { id: '2' });
            await client.waitFor((t) => !rows(t).some(([k]) => k === '2'));
            expect(rows(client.tree())).toEqual([
                ['1', 'One'],
                ['3', 'Three'],
                ['9', 'Nine'],
            ]);

            // OP_MOVE: '1' to index 2 -> [3, 9, 1].
            client.pushEvent('move', { id: '1', pos: 2 });
            await client.waitFor((t) => rows(t)[2][0] === '1');
            expect(rows(client.tree())).toEqual([
                ['3', 'Three'],
                ['9', 'Nine'],
                ['1', 'One'],
            ]);

            // OP_ITEM_PATCH: update an item's text in place.
            client.pushEvent('update', { id: '9', text: 'Updated' });
            await client.waitFor((t) => rows(t).some(([k, v]) => k === '9' && v === 'Updated'));
            expect(rows(client.tree())).toEqual([
                ['3', 'Three'],
                ['9', 'Updated'],
                ['1', 'One'],
            ]);

            // OP_UPDATE (full re-render via reset).
            client.pushEvent('reset', {
                items: [
                    { id: 'a', text: 'A' },
                    { id: 'b', text: 'B' },
                ],
            });
            await client.waitFor((t) => rows(t).length === 2 && rows(t)[0][0] === 'a');
            expect(rows(client.tree())).toEqual([
                ['a', 'A'],
                ['b', 'B'],
            ]);
        } finally {
            client.close();
        }
    });
});
