import { expect, test } from '@playwright/test';
import { NativeClient } from '../utils/native_client.js';

// Native (JSON) wire -- dev reload. A native client has no page to reload, so in
// dev mode the server closes its socket 1001 on a reload broadcast and the client
// reconnects and remounts on fresh code. The view holds a fun made by the module
// the admin endpoint reloads twice; left mounted, its next tap would crash on the
// purged code. (backoff(0) is ~1s, hence the generous waits.)
test.describe('native (JSON) wire -- dev reload', () => {
    test('remounts on fresh code after a reload', async ({ baseURL, request }) => {
        // Enable the dev reloader for the connection opened next only.
        expect((await request.get('/_test/reload?step=arm')).status()).toBe(204);
        const client = new NativeClient(baseURL, '/native/reload-fun');
        await client.connect();
        const closes = [];
        client.ws.addEventListener('close', (e) => closes.push(e.code));
        const count = (t) => t.children[0].children[1];
        try {
            client.tap(client.tree().children[1]); // "+"
            await client.waitFor((t) => count(t) === '1');

            expect((await request.get('/_test/reload?step=fire')).status()).toBe(204);

            // Remounted without any tap: the fresh view starts at 0.
            await client.waitFor((t) => count(t) === '0', 8000);
            expect(closes).toEqual([1001]);

            // The fresh view's fun is the reloaded module's, so a tap works.
            client.tap(client.tree().children[1]);
            await client.waitFor((t) => count(t) === '1', 8000);
        } finally {
            client.close();
        }
    });
});
