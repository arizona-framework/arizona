import { expect, test } from '@playwright/test';

const wsReady = (page) =>
    page.waitForFunction(() => document.documentElement.classList.contains('az-connected'));

/** A token no other test (or retry) shares, so its count is this test's alone. */
const newToken = () => `t${Date.now()}${Math.random().toString(36).slice(2)}`;

/** How many requests the server answered for `token`. */
async function count(page, token) {
    const resp = await page.request.get(`/fetch-repeat/count?token=${token}`);
    expect(resp.ok()).toBe(true);
    return Number(await resp.text());
}

test.describe('arizona_js:fetch on_repeat', () => {
    test.beforeEach(async ({ page }) => {
        await page.goto('/fetch-repeat');
        await wsReady(page);
    });

    test('a double click on a drop form posts once, busy while in flight', async ({ page }) => {
        const token = newToken();
        const form = page.locator('#drop-form');
        await page.locator('#drop-token').fill(token);
        await page.locator('#drop-submit').dblclick();
        await expect(form).toHaveAttribute('az-busy', '');
        await expect(form).not.toHaveAttribute('az-busy');
        expect(await count(page, token)).toBe(1);
    });

    test('a double Enter in a drop form field posts once', async ({ page }) => {
        const token = newToken();
        const field = page.locator('#drop-token');
        await field.fill(token);
        await field.press('Enter');
        await field.press('Enter');
        await expect(page.locator('#drop-form')).not.toHaveAttribute('az-busy');
        expect(await count(page, token)).toBe(1);
    });

    test('a double click on a default form posts twice', async ({ page }) => {
        const token = newToken();
        const form = page.locator('#allow-form');
        await page.locator('#allow-token').fill(token);
        await page.locator('#allow-submit').dblclick();
        await expect(form).toHaveAttribute('az-busy', '');
        await expect(form).not.toHaveAttribute('az-busy');
        expect(await count(page, token)).toBe(2);
    });
});
