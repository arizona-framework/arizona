import { expect, test } from '@playwright/test';

/** Wait for the WebSocket to be open. */
const wsReady = (page) =>
    page.waitForFunction(() => document.documentElement.classList.contains('az-connected'));

test('drain pushes draining event then WS reconnects', async ({ page, request }) => {
    await page.goto('/drainable');
    await wsReady(page);

    // Install a listener for the `draining` CustomEvent that handle_drain's
    // dispatch_event effect emits via applyEffects on the client.
    await page.evaluate(() => {
        window._drainEvents = [];
        document.addEventListener('draining', (e) => {
            window._drainEvents.push(e.detail);
        });
    });

    // Trigger soft-drain via the test admin endpoint.
    const res = await request.get('/_test/drain?listener=http&deadline_ms=5000');
    expect(res.status()).toBe(204);

    // The draining event must arrive before the WS closes.
    await page.waitForFunction(() => window._drainEvents.length > 0);

    // handle_drain returned {stop, ...} → live process exits → WS closes
    // with code 1000. JS client reconnects automatically.
    await page.waitForFunction(() =>
        document.documentElement.classList.contains('az-disconnected'),
    );
    await wsReady(page);
});
