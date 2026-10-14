import { expect, test } from '@playwright/test';

// A plain list whose items are client-owned widgets (`arizona_nodiff_widget_list`,
// the `Widget` hook in `arizona_layout`). Every change to it re-renders the list
// wholesale; the items the new render still holds keep their widget and its state.

const wsReady = (page) =>
    page.waitForFunction(() => document.documentElement.classList.contains('az-connected'));

const items = (page) => page.locator('#list > li');
const input = (page, n) => items(page).nth(n).locator('.widget-input');
const log = (page) => page.evaluate(() => window._widget);

test.beforeEach(async ({ page }) => {
    await page.goto('/nodiff-widget-list');
    await wsReady(page);
    await expect(items(page)).toHaveCount(2);
});

test('adding an item keeps the widgets already in the list', async ({ page }) => {
    await input(page, 0).fill('first');
    await input(page, 1).fill('second');
    await page.locator('#add').click();
    await expect(items(page)).toHaveCount(3);
    await expect(input(page, 0)).toHaveValue('first');
    await expect(input(page, 1)).toHaveValue('second');
    expect(await log(page)).toEqual({
        mounts: 3,
        destroys: 0,
        events: ['mount:a', 'mount:b', 'mount:i3'],
    });
});

test('dropping an item destroys only its widget', async ({ page }) => {
    await input(page, 0).fill('first');
    await page.locator('#drop').click();
    await expect(items(page)).toHaveCount(1);
    await expect(input(page, 0)).toHaveValue('first');
    expect((await log(page)).events).toEqual(['mount:a', 'mount:b', 'destroy:b:true']);
});

test('a reconnect keeps the widgets of the items still listed', async ({ page }) => {
    await page.locator('#add').click();
    await expect(items(page)).toHaveCount(3);
    await input(page, 0).fill('first');
    await page.evaluate(() => window._ws.close(4000));
    await page.waitForFunction(() =>
        document.documentElement.classList.contains('az-disconnected'),
    );
    await wsReady(page);
    // The fresh mount lists two items again: the third widget goes, the others stay.
    await expect(items(page)).toHaveCount(2);
    await expect(input(page, 0)).toHaveValue('first');
    expect((await log(page)).events).toEqual(['mount:a', 'mount:b', 'mount:i3', 'destroy:i3:true']);
});
