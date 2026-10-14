import { expect, test } from '@playwright/test';

const html = (page) => page.locator('html');

test.describe('crash recovery', () => {
    test('a crash after the mount remounts the view without reloading the page', async ({
        page,
    }) => {
        await page.goto('/crashable');
        await expect(html(page)).toHaveClass(/az-connected/);
        // A full reload would start a fresh window and drop this marker.
        await page.evaluate(() => {
            /** @type {any} */ (window).__azNoReload = true;
        });

        // Move the view off its mount state, so seeing the default again proves
        // the reconnect remounted it rather than just reopening the socket.
        const status = page.locator('p:has-text("Status:")');
        await page.evaluate(() => {
            window._ws.send(JSON.stringify(['crashable', 'set_status', { value: 'changed' }]));
        });
        await expect(status).toContainText('changed');

        // handle_event crashes the live process: the server closes 4500.
        await page.evaluate(() => {
            window._ws.send(JSON.stringify(['crashable', 'crash', {}]));
        });
        await expect(html(page)).toHaveClass(/az-disconnected/);

        // The worker's backoff reconnect remounts the view in place.
        await expect(html(page)).toHaveClass(/az-connected/, { timeout: 5000 });
        await expect(status).toContainText('Status: ok');
        expect(await page.evaluate(() => /** @type {any} */ (window).__azNoReload)).toBe(true);
    });
});
