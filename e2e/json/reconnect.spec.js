import { expect, test } from '@playwright/test';
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

    // A navigate the socket could not carry -- its reply lost with the socket, or
    // issued while the socket is down -- leaves the page on screen: the reconnect
    // re-mounts THAT page, and the navigate goes out once it is back.
    class DialingClient extends NativeClient {
        dials = [];
        _open() {
            this.dials.push(new URL(this.wsUrl).searchParams.get('_az_path'));
            super._open();
        }
    }
    const closed = (client) =>
        new Promise((resolve) => client.ws.addEventListener('close', resolve, { once: true }));

    test('a navigate whose reply a drop lost reconnects to the page on screen, then lands', async ({
        baseURL,
    }) => {
        const client = new DialingClient(baseURL, '/json/menu');
        await client.connect();
        try {
            const down = closed(client);
            client.ws.close(4000, 'simulated drop');
            // Sent into the closing socket: no reply comes.
            client.navigate('/json/counter');
            await down;
            await client.waitFor((t) => t.id === 'json_counter', 8000);
            expect(client.dials).toEqual(['/json/menu', '/json/menu']);
        } finally {
            client.close();
        }
    });

    test('a navigate issued while down waits for the page on screen, then lands', async ({
        baseURL,
    }) => {
        const client = new DialingClient(baseURL, '/json/menu');
        await client.connect();
        try {
            const down = closed(client);
            client.ws.close(4000, 'simulated drop');
            await down;
            client.navigate('/json/list');
            client.navigate('/json/counter');
            await client.waitFor((t) => t.id === 'json_counter', 8000);
            expect(client.dials).toEqual(['/json/menu', '/json/menu']);
            // ...and a later drop reconnects to the page now on screen.
            client.ws.close(4000, 'simulated drop');
            await client.waitFor((t) => t.id === 'json_counter' && client.dials.length === 3, 8000);
            expect(client.dials.at(-1)).toBe('/json/counter');
        } finally {
            client.close();
        }
    });
});

// Two pages that navigate to each other on connect, each navigate failing over the
// socket and answered with a full navigate: the fresh connections answering the
// navigates the server issued are bounded, 10 and then one per 30 s.
test('JSON wire -- a navigate-and-connect loop is bounded', async ({ baseURL }) => {
    test.setTimeout(60000);
    const Native = globalThis.WebSocket;
    let opened = 0;
    globalThis.WebSocket = class extends Native {
        constructor(...args) {
            super(...args);
            opened++;
        }
    };
    const client = new NativeClient(baseURL, '/json/bounce-a');
    try {
        await client.connect().catch(() => {});
        await new Promise((r) => setTimeout(r, 10000));
        // The first connect plus the 10 the bound lets through, not thousands.
        expect(opened).toBeGreaterThanOrEqual(11);
        expect(opened).toBeLessThanOrEqual(12);
        const before = opened;
        await new Promise((r) => setTimeout(r, 25000));
        expect(opened - before).toBeLessThanOrEqual(1);
    } finally {
        client.close();
        globalThis.WebSocket = Native;
    }
});
