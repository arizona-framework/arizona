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

        // handle_event crashes the live process: the server closes 4500.
        await page.evaluate(() => {
            window._ws.send(JSON.stringify(['crashable', 'crash', {}]));
        });
        await expect(html(page)).toHaveClass(/az-disconnected/);

        // The worker's backoff reconnect remounts the view in place.
        await expect(html(page)).toHaveClass(/az-connected/, { timeout: 5000 });
        await expect(page.locator('p:has-text("Status:")')).toContainText('ok');
        expect(await page.evaluate(() => /** @type {any} */ (window).__azNoReload)).toBe(true);
    });
});
