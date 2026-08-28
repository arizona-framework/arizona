import { expect, test } from '@playwright/test';

// The /events view (test/support/arizona_events_demo.erl) proves the whole
// compile-time path over a real socket: the parse transform records `az-toggle`,
// the server ships the set on the connect frame, and the client delegates it.
// `toggle` is not bootstrapped and does not bubble, so it needs both the set and
// the capture-phase listener -- a regression in either leaves the page inert.

const wsReady = (page) =>
    page.waitForFunction(() => document.documentElement.classList.contains('az-connected'));

test('delivers a non-bubbling event the server declared at connect', async ({ page }) => {
    const errors = [];
    page.on('pageerror', (e) => errors.push(e.message));
    await page.goto('/events');
    await wsReady(page);
    await expect(page.locator('#toggles')).toHaveText('0');

    // A real, browser-generated toggle from clicking the summary.
    await page.click('#sum');
    await expect(page.locator('#toggles')).toHaveText('1');
    await page.click('#sum');
    await expect(page.locator('#toggles')).toHaveText('2');

    expect(errors).toEqual([]);
});

test('never delegates an az-* name that only carries app data', async ({ page }) => {
    const errors = [];
    page.on('pageerror', (e) => errors.push(e.message));
    await page.goto('/events');
    await wsReady(page);

    // az-select="[1,2,3]" is a static value, so the transform does not record it.
    // Were it delegated, this dispatch would parse [1,2,3] as a command -- opcode 1
    // is toggle, selector 2 -- and throw out of the document listener.
    await page.evaluate(() =>
        document.getElementById('data').dispatchEvent(new Event('select', { bubbles: true })),
    );
    await page.waitForTimeout(150);
    expect(errors).toEqual([]);
});
