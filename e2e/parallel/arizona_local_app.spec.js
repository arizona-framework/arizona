import { expect, test } from '@playwright/test';
import { expectStaysConnected } from '../utils/helpers.js';

// ---------------------------------------------------------------------------
// A fully client-only app: nested stateless (tab bar) + stateful (widgets)
// elements, every interaction driven by client-owned ?local slots. After the
// initial WS connect, NO interaction produces a server frame.
// ---------------------------------------------------------------------------

const wsReady = (page) =>
    page.waitForFunction(() => document.documentElement.classList.contains('az-connected'));

test.describe('?local -- fully client-only app', () => {
    test('theme, tabs, and both widgets are interactive with zero server round-trips', async ({
        page,
    }) => {
        await page.goto('/local-app');
        await wsReady(page);

        const app = page.locator('#local_app .app');
        const tabs = page.locator('#local_app .tabs');
        const wa = page.locator('#widget_a');
        const wb = page.locator('#widget_b');

        // Initial SSR state.
        await expect(app).toHaveClass('app theme-light');
        await expect(tabs).toHaveAttribute('data-active', 'home');
        await expect(wa.locator('.status')).toHaveClass('status status-idle');

        // Count any server -> client frame that arrives after connect.
        await page.evaluate(() => {
            /** @type {any} */ (window).__msgs = 0;
            const orig = window._ws.onmessage;
            window._ws.onmessage = (e) => {
                /** @type {any} */ (window).__msgs += 1;
                if (orig) orig(e);
            };
        });

        await expectStaysConnected(page, async () => {
            // Theme (view-scoped client slot, interpolated class).
            await page.getByRole('button', { name: 'Dark' }).click();
            await expect(app).toHaveClass('app theme-dark');

            // Tabs (stateless child, CSS-driven client slot).
            await page.getByRole('button', { name: 'Settings' }).click();
            await expect(tabs).toHaveAttribute('data-active', 'settings');
            await expect(page.locator('#local_app .panel-settings')).toBeVisible();

            // Widget A: status (interp class + label) and a dialog, scoped to widget_a.
            await wa.getByRole('button', { name: 'Active' }).click();
            await expect(wa.locator('.status')).toHaveClass('status status-active');
            await wa.locator('.open-detail').click();
            await expect(wa.locator('dialog')).toBeVisible();
            await wa.getByRole('button', { name: 'Close' }).click();
            await expect(wa.locator('dialog')).toBeHidden();

            // Widget B is independent (per-child isolation): setting it leaves A as-is.
            await wb.getByRole('button', { name: 'Done' }).click();
            await expect(wb.locator('.status')).toHaveClass('status status-done');
            await expect(wa.locator('.status')).toHaveClass('status status-active');

            await page.waitForTimeout(200);
        });

        const msgs = await page.evaluate(() => /** @type {any} */ (window).__msgs);
        expect(msgs, 'a fully client-only app sends zero server frames').toBe(0);
    });
});
