import { expect, test } from '@playwright/test';

const wsReady = (page) =>
    page.waitForFunction(() => document.documentElement.classList.contains('az-connected'));

test.describe('connection params', () => {
    test('SSR renders URL query param', async ({ page }) => {
        await page.goto('/crashable?locale=pt');
        await expect(page.locator('p:has-text("Locale:")')).toContainText('pt');
    });

    test('WS mount receives URL query param', async ({ page }) => {
        await page.goto('/crashable?locale=en');
        await wsReady(page);
        // After WS mount, the param should still be rendered
        await expect(page.locator('p:has-text("Locale:")')).toContainText('en');
    });

    test('page without params renders default', async ({ page }) => {
        await page.goto('/crashable');
        await expect(page.locator('p:has-text("Locale:")')).toContainText('none');
    });
});
