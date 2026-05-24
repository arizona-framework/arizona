import { test } from '@playwright/test';
import { NativeClient } from '../utils/native_client.js';

// Native (JSON) reconnect-with-backoff: after a dropped socket (non-1000 close),
// the client reopens on its own -- re-mounting via _az_reconnect=1 and applying
// the fresh OP_REPLACE -- then keeps working. Mirrors the browser worker's
// auto-reconnect. (backoff(0) is ~1s, hence the generous waits.)
test.describe('native (JSON) wire -- reconnect', () => {
    test('reopens and recovers after a dropped connection', async ({ baseURL }) => {
        const client = new NativeClient(baseURL, '/native/counter');
        await client.connect();
        const count = (t) => t.children[0].children[1];
        try {
            // Drive it once so the pre-drop state is distinguishable (count = 1).
            client.tap(client.tree().children[1]); // "+"
            await client.waitFor((t) => count(t) === '1');

            // Force a drop with a non-1000 close -> the client reconnects.
            client.ws.close(4000, 'simulated drop');

            // The reconnect re-mounts fresh, so the count resets to 0.
            await client.waitFor((t) => count(t) === '0', 8000);

            // The new socket works: tapping still round-trips.
            client.tap(client.tree().children[1]);
            await client.waitFor((t) => count(t) === '1', 8000);
        } finally {
            client.close();
        }
    });
});
