import { expect, test } from '@playwright/test';

const wsReady = (page) =>
    page.waitForFunction(() => document.documentElement.classList.contains('az-connected'));

/**
 * Wait until the server handled every event sent so far: the live process handles
 * them in order, so once `ping`'s reply renders, any repeat sent before it has too.
 */
async function settled(page) {
    const pings = page.locator('#pings');
    const before = Number(await pings.textContent());
    await page.locator('#ping').click();
    await expect(pings).toHaveText(String(before + 1));
}

test.describe('arizona_js:push_event on_repeat', () => {
    test.beforeEach(async ({ page }) => {
        await page.goto('/push-repeat');
        await wsReady(page);
    });

    test('a double click on a drop button sends once, busy until the reply', async ({ page }) => {
        const btn = page.locator('#drop-save');
        await btn.dblclick();
        await expect(btn).toHaveAttribute('az-busy', '');
        await expect(btn).not.toHaveAttribute('az-busy');
        await settled(page);
        await expect(page.locator('#saves')).toHaveText('1');
    });

    test('a double submit of a drop form sends once, busy until the reply', async ({ page }) => {
        const form = page.locator('#drop-form');
        await page.locator('#drop-submit').dblclick();
        await expect(form).toHaveAttribute('az-busy', '');
        await expect(form).not.toHaveAttribute('az-busy');
        await settled(page);
        await expect(page.locator('#submits')).toHaveText('1');
    });

    test('a double Enter in a drop form field sends once', async ({ page }) => {
        const field = page.locator('#drop-field');
        await field.press('Enter');
        await field.press('Enter');
        await expect(page.locator('#drop-form')).not.toHaveAttribute('az-busy');
        await settled(page);
        await expect(page.locator('#submits')).toHaveText('1');
    });

    test('the event goes out again once its reply landed', async ({ page }) => {
        const btn = page.locator('#drop-save');
        await btn.click();
        await expect(page.locator('#saves')).toHaveText('1');
        await btn.click();
        await expect(page.locator('#saves')).toHaveText('2');
    });

    test('a double click on a default button sends twice, never busy', async ({ page }) => {
        const btn = page.locator('#allow-save');
        await btn.dblclick();
        await expect(btn).not.toHaveAttribute('az-busy');
        await settled(page);
        await expect(page.locator('#allowed')).toHaveText('2');
    });
});
