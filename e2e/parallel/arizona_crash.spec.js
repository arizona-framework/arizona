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

    test('a failed mount reloads the page after a 5-10 s delay, without spinning', async ({
        page,
    }) => {
        test.setTimeout(60000);
        /** @type {number[]} */
        const loads = [];
        page.on('load', () => loads.push(Date.now()));
        // The page renders at SSR; only the live mount fails (closes 4501).
        await page.goto('/crash-on-connect');
        await expect(page.locator('p:has-text("Status:")')).toBeVisible();
        await expect(html(page)).toHaveClass(/az-disconnected/);

        // The reload waits: the worker's reconnects (which fail to mount too)
        // must not turn into a reload loop.
        await expect.poll(() => loads.length, { timeout: 15000 }).toBe(2);
        expect(loads[1] - loads[0]).toBeGreaterThanOrEqual(5000);
        // Counted in this tab only, and nothing written to localStorage.
        expect(
            await page.evaluate(() => [
                JSON.parse(sessionStorage.getItem('arizona:mount-reloads') ?? 'null'),
                localStorage.length,
            ]),
        ).toEqual([{ path: '/crash-on-connect', count: 1 }, 0]);

        // The reloaded page waits again before the next one.
        await page.waitForTimeout(4000);
        expect(loads).toHaveLength(2);
    });
});
