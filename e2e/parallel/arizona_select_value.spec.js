import { expect, test } from '@playwright/test';
import { expectStaysConnected } from '../utils/helpers.js';

// A `value` on a <select> picks the option whose value equals it. HTML has no such
// attribute, so the server marks that option `selected`: the page must load on it
// (before any script runs) and every update must move the choice.

const wsReady = (page) =>
    page.waitForFunction(() => document.documentElement.classList.contains('az-connected'));

const values = (page) =>
    page.evaluate(() => ({
        lit: document.querySelector('#select-value .lit').value,
        list: document.querySelector('#select-value .list').value,
        stream: document.querySelector('#select-value .stream').value,
        multi: [...document.querySelector('#select-value .multi').selectedOptions].map(
            (o) => o.value,
        ),
    }));

test.describe('select value', () => {
    test('the page loads on the chosen option without the client', async ({ browser }) => {
        const context = await browser.newContext({ javaScriptEnabled: false });
        const page = await context.newPage();
        await page.goto('/select-value');
        expect(await page.locator('#select-value .lit').inputValue()).toBe('b');
        expect(await page.locator('#select-value .list').inputValue()).toBe('b');
        expect(await page.locator('#select-value .stream').inputValue()).toBe('b');
        await context.close();
    });

    test('the page loads on the chosen options and updates follow', async ({ page }) => {
        await page.goto('/select-value');
        await wsReady(page);
        expect(await values(page)).toEqual({ lit: 'b', list: 'b', stream: 'b', multi: ['a', 'c'] });

        await expectStaysConnected(page, () => page.locator('#select-value .pick-c').click());
        await expect(page.locator('#select-value .lit')).toHaveValue('c');
        expect(await values(page)).toEqual({ lit: 'c', list: 'c', stream: 'c', multi: ['a', 'c'] });

        await expectStaysConnected(page, () => page.locator('#select-value .pick-many').click());
        await expect.poll(async () => (await values(page)).multi).toEqual(['b']);
    });

    test('a server change wins over the option the user picked', async ({ page }) => {
        await page.goto('/select-value');
        await wsReady(page);
        // The user moves every select off the server's choice...
        for (const cls of ['lit', 'list', 'stream']) {
            await page.locator(`#select-value .${cls}`).selectOption('a');
        }
        // ...then the server moves it to one the user did not pick.
        await expectStaysConnected(page, () => page.locator('#select-value .pick-c').click());
        await expect(page.locator('#select-value .stream')).toHaveValue('c');
        expect(await values(page)).toMatchObject({ lit: 'c', list: 'c', stream: 'c' });
    });
});
