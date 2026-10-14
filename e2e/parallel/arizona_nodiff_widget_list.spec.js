import { expect, test } from '@playwright/test';

// A plain list whose items are client-owned widgets (`arizona_nodiff_widget_list`,
// the `Widget` hook in `arizona_layout`). Every change to it re-renders the list
// wholesale. Keyed items keep their widget and its state while they are listed;
// unkeyed ones cannot be told apart, so every widget is remounted for its own item.

const wsReady = (page) =>
    page.waitForFunction(() => document.documentElement.classList.contains('az-connected'));

const items = (page) => page.locator('#list > li');
const kinds = (page) => items(page).locator('.widget-kind');
const input = (page, n) => items(page).nth(n).locator('.widget-input');
const log = (page) => page.evaluate(() => window._widget);

async function open(page, path) {
    await page.goto(path);
    await wsReady(page);
    await expect(items(page)).toHaveCount(2);
    await input(page, 0).fill('first');
    await input(page, 1).fill('second');
}

test.describe('unkeyed items', () => {
    test.beforeEach(async ({ page }) => open(page, '/nodiff-widget-list'));

    test('dropping the first item remounts the rest for their own items', async ({ page }) => {
        // A bare marker: client-owned, but nothing names which item it is.
        await expect(items(page).first()).toHaveAttribute('az-nodiff', '');
        await page.locator('#drop-first').click();
        await expect(kinds(page)).toHaveText(['b']);
        await expect(input(page, 0)).toHaveValue('');
        expect((await log(page)).events).toEqual([
            'mount:a',
            'mount:b',
            'destroy:a:true',
            'destroy:b:true',
            'mount:b',
        ]);
    });

    test('reversing the items remounts each widget for its own item', async ({ page }) => {
        await page.locator('#reverse').click();
        await expect(kinds(page)).toHaveText(['b', 'a']);
        await expect(input(page, 0)).toHaveValue('');
        await expect(input(page, 1)).toHaveValue('');
        expect((await log(page)).events).toEqual([
            'mount:a',
            'mount:b',
            'destroy:a:true',
            'destroy:b:true',
            'mount:b',
            'mount:a',
        ]);
    });
});

test.describe('keyed items', () => {
    test.beforeEach(async ({ page }) => open(page, '/nodiff-widget-list-keyed'));

    test('adding an item keeps the widgets already in the list', async ({ page }) => {
        await page.locator('#add').click();
        await expect(kinds(page)).toHaveText(['a', 'b', 'i3']);
        await expect(input(page, 0)).toHaveValue('first');
        await expect(input(page, 1)).toHaveValue('second');
        expect((await log(page)).events).toEqual(['mount:a', 'mount:b', 'mount:i3']);
    });

    test('dropping the first item destroys only its widget', async ({ page }) => {
        // The region's token: its identity, the item's key included.
        await expect(items(page).first()).toHaveAttribute('az-nodiff', /\.k/);
        await page.locator('#drop-first').click();
        await expect(kinds(page)).toHaveText(['b']);
        await expect(input(page, 0)).toHaveValue('second');
        expect((await log(page)).events).toEqual(['mount:a', 'mount:b', 'destroy:a:true']);
    });

    test('reversing the items keeps each widget with its item', async ({ page }) => {
        await page.locator('#reverse').click();
        await expect(kinds(page)).toHaveText(['b', 'a']);
        await expect(input(page, 0)).toHaveValue('second');
        await expect(input(page, 1)).toHaveValue('first');
        expect((await log(page)).events).toEqual(['mount:a', 'mount:b']);
    });

    // The fresh mount lists [a, b] again: b keeps its widget, a gets a new one.
    test('a reconnect after dropping the first item keeps the widget by key', async ({ page }) => {
        await page.locator('#drop-first').click();
        await expect(kinds(page)).toHaveText(['b']);
        await page.evaluate(() => window._ws.close(4000));
        await page.waitForFunction(() =>
            document.documentElement.classList.contains('az-disconnected'),
        );
        await wsReady(page);
        await expect(kinds(page)).toHaveText(['a', 'b']);
        await expect(input(page, 0)).toHaveValue('');
        await expect(input(page, 1)).toHaveValue('second');
        expect((await log(page)).events).toEqual([
            'mount:a',
            'mount:b',
            'destroy:a:true',
            'mount:a',
        ]);
    });
});
