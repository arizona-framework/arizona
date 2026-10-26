import { test } from '@playwright/test';
import { NativeClient } from '../utils/json_client.js';

// JSON reconnect-with-backoff: after a dropped socket (non-1000 close),
// the client reopens on its own -- re-mounting via _az_reconnect=1 and applying
// the fresh OP_REPLACE -- then keeps working. Mirrors the browser worker's
// auto-reconnect. (backoff(0) is ~1s, hence the generous waits.)
test.describe('JSON wire -- reconnect', () => {
    test('reopens and recovers after a dropped connection', async ({ baseURL }) => {
        const client = new NativeClient(baseURL, '/json/counter');
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

    test('reconnects to the navigated path, not the launch path', async ({ baseURL }) => {
        const client = new NativeClient(baseURL, '/json/menu');
        await client.connect();
        const count = (t) => t.children[0].children[1];
        try {
            // Navigate menu -> counter, then drive it.
            client.tap(client.tree().children[0]); // "Counter"
            await client.waitFor((t) => t.id === 'json_counter');
            client.tap(client.tree().children[1]); // "+"
            await client.waitFor((t) => count(t) === '1');

            // Drop. Reconnect must re-mount /json/counter (the navigated path),
            // not /json/menu (the launch path) -- so a fresh counter re-renders.
            client.ws.close(4000, 'simulated drop');
            await client.waitFor((t) => t.id === 'json_counter' && count(t) === '0', 8000);

            // ...and the reconnected counter still round-trips.
            client.tap(client.tree().children[1]);
            await client.waitFor((t) => count(t) === '1', 8000);
        } finally {
            client.close();
        }
    });
});
