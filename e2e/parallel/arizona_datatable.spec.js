import { expect, test } from '@playwright/test';

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

const wsReady = (page) =>
    page.waitForFunction(() => document.documentElement.classList.contains('az-connected'));

const tableRows = (page) => page.locator('#page tbody > [az-key]');

const tableRow = (page, key) => page.locator(`#page tbody > [az-key="${key}"]`);

const cellId = (page, key) => page.locator(`#page tbody > [az-key="${key}"] td:nth-child(1)`);

const cellName = (page, key) => page.locator(`#page tbody > [az-key="${key}"] td:nth-child(2)`);

const cellAge = (page, key) => page.locator(`#page tbody > [az-key="${key}"] td:nth-child(3)`);

const deleteBtn = (page, key) =>
    page.locator(`#page tbody > [az-key="${key}"] button[az-click*='"delete_row"']`);

const moveTopBtn = (page, key) =>
    page.locator(`#page tbody > [az-key="${key}"] button[az-click*='"move_top"']`);

const addRowBtn = (page) => page.locator(`button[az-click*='"add_row"']`);

const resetBtn = (page) => page.locator(`button[az-click*='"reset_data"']`);

const shuffleBtn = (page) => page.locator(`button[az-click*='"shuffle"']`);

const sortColBtn = (page, col) => page.locator(`th[az-click*='"sort"'][az-click*='"${col}"']`);

/** Helper to get az-key values in DOM order */
const getRowKeys = async (page) => {
    const rows = tableRows(page);
    const count = await rows.count();
    const keys = [];
    for (let i = 0; i < count; i++) {
        keys.push(await rows.nth(i).getAttribute('az-key'));
    }
    return keys;
};

// ---------------------------------------------------------------------------
// SSR
// ---------------------------------------------------------------------------

test.describe('DataTable -- SSR', () => {
    test('renders title heading', async ({ page }) => {
        await page.goto('/datatable');
        await expect(page.locator('main#page[az-view] h1[az]')).toHaveText('DataTable');
    });

    test('renders 5 rows', async ({ page }) => {
        await page.goto('/datatable');
        await expect(tableRows(page)).toHaveCount(5);
    });

    test('first row is Alice with id=1 and age=30', async ({ page }) => {
        await page.goto('/datatable');
        await expect(cellId(page, '1')).toHaveText('1');
        await expect(cellName(page, '1')).toHaveText('Alice');
        await expect(cellAge(page, '1')).toHaveText('30');
    });

    test('all 5 keys are visible', async ({ page }) => {
        await page.goto('/datatable');
        for (const key of ['1', '2', '3', '4', '5']) {
            await expect(tableRow(page, key)).toBeVisible();
        }
    });

    test('sort headers visible', async ({ page }) => {
        await page.goto('/datatable');
        await expect(sortColBtn(page, 'id')).toBeVisible();
        await expect(sortColBtn(page, 'name')).toBeVisible();
        await expect(sortColBtn(page, 'age')).toBeVisible();
    });

    test('action buttons visible', async ({ page }) => {
        await page.goto('/datatable');
        await expect(addRowBtn(page)).toBeVisible();
        await expect(resetBtn(page)).toBeVisible();
        await expect(shuffleBtn(page)).toBeVisible();
    });

    test('initial DOM order is [1,2,3,4,5]', async ({ page }) => {
        await page.goto('/datatable');
        const keys = await getRowKeys(page);
        expect(keys).toEqual(['1', '2', '3', '4', '5']);
    });
});

// ---------------------------------------------------------------------------
// Connected
// ---------------------------------------------------------------------------

test.describe('DataTable -- connected', () => {
    test('document.title set to DataTable', async ({ page }) => {
        await page.goto('/datatable');
        await wsReady(page);
        await expect(page).toHaveTitle('DataTable');
    });
});

// ---------------------------------------------------------------------------
// Add row
// ---------------------------------------------------------------------------

test.describe('DataTable -- add row', () => {
    test('adds row with id=6', async ({ page }) => {
        await page.goto('/datatable');
        await wsReady(page);
        await addRowBtn(page).click();
        await expect(tableRow(page, '6')).toBeVisible();
        await expect(tableRows(page)).toHaveCount(6);
    });

    test('new row has correct name and age', async ({ page }) => {
        await page.goto('/datatable');
        await wsReady(page);
        await addRowBtn(page).click();
        await expect(cellName(page, '6')).toHaveText('New 6');
        await expect(cellAge(page, '6')).toHaveText('20');
    });

    test('new row appears at end', async ({ page }) => {
        await page.goto('/datatable');
        await wsReady(page);
        await addRowBtn(page).click();
        await expect(tableRows(page)).toHaveCount(6);
        await expect(tableRows(page).nth(5)).toHaveAttribute('az-key', '6');
    });

    test('sequential adds increment id', async ({ page }) => {
        await page.goto('/datatable');
        await wsReady(page);
        await addRowBtn(page).click();
        await expect(tableRow(page, '6')).toBeVisible();
        await addRowBtn(page).click();
        await expect(tableRow(page, '7')).toBeVisible();
        await expect(tableRows(page)).toHaveCount(7);
    });

    test('new row has action buttons', async ({ page }) => {
        await page.goto('/datatable');
        await wsReady(page);
        await addRowBtn(page).click();
        await expect(deleteBtn(page, '6')).toBeVisible();
        await expect(moveTopBtn(page, '6')).toBeVisible();
    });
});

// ---------------------------------------------------------------------------
// Delete row
// ---------------------------------------------------------------------------

test.describe('DataTable -- delete row', () => {
    test('delete first row', async ({ page }) => {
        await page.goto('/datatable');
        await wsReady(page);
        await deleteBtn(page, '1').click();
        await expect(tableRow(page, '1')).not.toBeAttached();
        await expect(tableRows(page)).toHaveCount(4);
        // Siblings preserved
        await expect(tableRow(page, '2')).toBeVisible();
        await expect(tableRow(page, '5')).toBeVisible();
    });

    test('delete middle row', async ({ page }) => {
        await page.goto('/datatable');
        await wsReady(page);
        await deleteBtn(page, '3').click();
        await expect(tableRow(page, '3')).not.toBeAttached();
        await expect(tableRows(page)).toHaveCount(4);
        await expect(tableRow(page, '2')).toBeVisible();
        await expect(tableRow(page, '4')).toBeVisible();
    });

    test('delete last row', async ({ page }) => {
        await page.goto('/datatable');
        await wsReady(page);
        await deleteBtn(page, '5').click();
        await expect(tableRow(page, '5')).not.toBeAttached();
        await expect(tableRows(page)).toHaveCount(4);
        await expect(tableRow(page, '4')).toBeVisible();
    });
});

// ---------------------------------------------------------------------------
// Sort
// ---------------------------------------------------------------------------

test.describe('DataTable -- sort', () => {
    test('sort by age asc reorders rows', async ({ page }) => {
        await page.goto('/datatable');
        await wsReady(page);
        await sortColBtn(page, 'age').click();
        // Ages: Bob=25, Diana=28, Alice=30, Eve=32, Charlie=35
        // Wait for Bob (key=2) to be first
        await expect(tableRows(page).nth(0)).toHaveAttribute('az-key', '2');
        const keys = await getRowKeys(page);
        expect(keys).toEqual(['2', '4', '1', '5', '3']);
    });

    test('sort by age desc (toggle)', async ({ page }) => {
        await page.goto('/datatable');
        await wsReady(page);
        await sortColBtn(page, 'age').click();
        // Wait for asc sort to apply
        await expect(tableRows(page).nth(0)).toHaveAttribute('az-key', '2');
        // Toggle to desc
        await sortColBtn(page, 'age').click();
        // Desc: Charlie=35, Eve=32, Alice=30, Diana=28, Bob=25
        await expect(tableRows(page).nth(0)).toHaveAttribute('az-key', '3');
        const keys = await getRowKeys(page);
        expect(keys).toEqual(['3', '5', '1', '4', '2']);
    });

    test('sort by name preserves order (already alphabetical)', async ({ page }) => {
        await page.goto('/datatable');
        await wsReady(page);
        await sortColBtn(page, 'name').click();
        const keys = await getRowKeys(page);
        expect(keys).toEqual(['1', '2', '3', '4', '5']);
    });

    test('sort does not change row count', async ({ page }) => {
        await page.goto('/datatable');
        await wsReady(page);
        await sortColBtn(page, 'age').click();
        await expect(tableRows(page)).toHaveCount(5);
    });

    test('row data preserved after sort', async ({ page }) => {
        await page.goto('/datatable');
        await wsReady(page);
        await sortColBtn(page, 'age').click();
        await expect(cellName(page, '1')).toHaveText('Alice');
        await expect(cellAge(page, '1')).toHaveText('30');
        await expect(cellName(page, '2')).toHaveText('Bob');
        await expect(cellAge(page, '2')).toHaveText('25');
    });
});

// ---------------------------------------------------------------------------
// Move to top
// ---------------------------------------------------------------------------

test.describe('DataTable -- move to top', () => {
    test('move last to first', async ({ page }) => {
        await page.goto('/datatable');
        await wsReady(page);
        await moveTopBtn(page, '5').click();
        await expect(tableRows(page).nth(0)).toHaveAttribute('az-key', '5');
        const keys = await getRowKeys(page);
        expect(keys[0]).toBe('5');
        expect(keys).toHaveLength(5);
    });

    test('move middle to first', async ({ page }) => {
        await page.goto('/datatable');
        await wsReady(page);
        await moveTopBtn(page, '3').click();
        await expect(tableRows(page).nth(0)).toHaveAttribute('az-key', '3');
        const keys = await getRowKeys(page);
        expect(keys[0]).toBe('3');
    });

    test('move first is no-op (no crash)', async ({ page }) => {
        await page.goto('/datatable');
        await wsReady(page);
        await moveTopBtn(page, '1').click();
        const keys = await getRowKeys(page);
        expect(keys[0]).toBe('1');
        expect(keys).toHaveLength(5);
    });

    test('data preserved after move', async ({ page }) => {
        await page.goto('/datatable');
        await wsReady(page);
        await moveTopBtn(page, '5').click();
        await expect(cellName(page, '5')).toHaveText('Eve');
        await expect(cellAge(page, '5')).toHaveText('32');
    });
});

// ---------------------------------------------------------------------------
// Reset
// ---------------------------------------------------------------------------

test.describe('DataTable -- reset', () => {
    test('after add, reset restores original 5', async ({ page }) => {
        await page.goto('/datatable');
        await wsReady(page);
        await addRowBtn(page).click();
        await expect(tableRows(page)).toHaveCount(6);
        await resetBtn(page).click();
        await expect(tableRows(page)).toHaveCount(5);
        const keys = await getRowKeys(page);
        expect(keys).toEqual(['1', '2', '3', '4', '5']);
    });

    test('after delete, reset restores original 5', async ({ page }) => {
        await page.goto('/datatable');
        await wsReady(page);
        await deleteBtn(page, '3').click();
        await expect(tableRows(page)).toHaveCount(4);
        await resetBtn(page).click();
        await expect(tableRows(page)).toHaveCount(5);
        const keys = await getRowKeys(page);
        expect(keys).toEqual(['1', '2', '3', '4', '5']);
    });

    test('after sort, reset restores original order', async ({ page }) => {
        await page.goto('/datatable');
        await wsReady(page);
        await sortColBtn(page, 'age').click();
        // Wait for sort to apply
        await expect(tableRows(page).nth(0)).toHaveAttribute('az-key', '2');
        let keys = await getRowKeys(page);
        expect(keys).toEqual(['2', '4', '1', '5', '3']);
        await resetBtn(page).click();
        // Wait for reset to apply -- key 1 should be first again
        await expect(tableRows(page).nth(0)).toHaveAttribute('az-key', '1');
        keys = await getRowKeys(page);
        expect(keys).toEqual(['1', '2', '3', '4', '5']);
    });

    test('after reset, add uses id=6', async ({ page }) => {
        await page.goto('/datatable');
        await wsReady(page);
        await addRowBtn(page).click();
        await expect(tableRow(page, '6')).toBeVisible();
        await resetBtn(page).click();
        await expect(tableRows(page)).toHaveCount(5);
        await addRowBtn(page).click();
        await expect(tableRow(page, '6')).toBeVisible();
        await expect(cellName(page, '6')).toHaveText('New 6');
    });
});

// ---------------------------------------------------------------------------
// Shuffle
// ---------------------------------------------------------------------------

test.describe('DataTable -- shuffle', () => {
    test('preserves all 5 rows', async ({ page }) => {
        await page.goto('/datatable');
        await wsReady(page);
        await shuffleBtn(page).click();
        await expect(tableRows(page)).toHaveCount(5);
        for (const key of ['1', '2', '3', '4', '5']) {
            await expect(tableRow(page, key)).toBeVisible();
        }
    });

    test('preserves row data', async ({ page }) => {
        await page.goto('/datatable');
        await wsReady(page);
        await shuffleBtn(page).click();
        await expect(cellName(page, '1')).toHaveText('Alice');
        await expect(cellAge(page, '1')).toHaveText('30');
        await expect(cellName(page, '3')).toHaveText('Charlie');
        await expect(cellAge(page, '3')).toHaveText('35');
    });

    test('page remains functional after shuffle', async ({ page }) => {
        await page.goto('/datatable');
        await wsReady(page);
        await shuffleBtn(page).click();
        await expect(tableRows(page)).toHaveCount(5);
        // Add a row
        await addRowBtn(page).click();
        await expect(tableRows(page)).toHaveCount(6);
        await expect(tableRow(page, '6')).toBeVisible();
    });
});

// ---------------------------------------------------------------------------
// SPA navigation
// ---------------------------------------------------------------------------

test.describe('DataTable -- SPA navigation', () => {
    test('navigate from home to datatable', async ({ page }) => {
        await page.goto('/');
        await wsReady(page);
        await page.click('a[href="/datatable"]');
        await expect(page.locator('main h1')).toHaveText('DataTable');
    });

    test('URL updates on navigate', async ({ page }) => {
        await page.goto('/');
        await wsReady(page);
        await page.click('a[href="/datatable"]');
        await expect(page.locator('main h1')).toHaveText('DataTable');
        expect(page.url()).toContain('/datatable');
    });

    test('document.title updates on navigate', async ({ page }) => {
        await page.goto('/');
        await wsReady(page);
        await page.click('a[href="/datatable"]');
        await expect(page.locator('main h1')).toHaveText('DataTable');
        await expect(page).toHaveTitle('DataTable');
    });

    test('navigate back with browser back button', async ({ page }) => {
        await page.goto('/');
        await wsReady(page);
        await page.click('a[href="/datatable"]');
        await expect(page.locator('main h1')).toHaveText('DataTable');
        await page.goBack();
        await expect(page.locator('main h1')).toHaveText('Welcome');
        expect(page.url()).not.toContain('/datatable');
    });

    test('round trip home → datatable → home', async ({ page }) => {
        await page.goto('/');
        await wsReady(page);
        await page.click('a[href="/datatable"]');
        await expect(page.locator('main h1')).toHaveText('DataTable');
        await page.click('a[href="/"]');
        await expect(page.locator('main h1')).toHaveText('Welcome');
        await expect(page).toHaveTitle('Welcome');
    });

    test('layout nav links preserved', async ({ page }) => {
        await page.goto('/');
        await wsReady(page);
        await page.click('a[href="/datatable"]');
        await expect(page.locator('main h1')).toHaveText('DataTable');
        await expect(page.locator('nav a[href="/"]')).toBeVisible();
        await expect(page.locator('nav a[href="/about"]')).toBeVisible();
        await expect(page.locator('nav a[href="/datatable"]')).toBeVisible();
    });
});

// ---------------------------------------------------------------------------
// Combined operations
// ---------------------------------------------------------------------------

test.describe('DataTable -- combined operations', () => {
    test('add + sort + delete + reset', async ({ page }) => {
        await page.goto('/datatable');
        await wsReady(page);
        // Add a row
        await addRowBtn(page).click();
        await expect(tableRows(page)).toHaveCount(6);
        // Sort by age -- new row (age=20) goes first
        await sortColBtn(page, 'age').click();
        await expect(tableRows(page).nth(0)).toHaveAttribute('az-key', '6');
        // Delete row 3 (Charlie, age=35, now last)
        await deleteBtn(page, '3').click();
        await expect(tableRow(page, '3')).not.toBeAttached();
        await expect(tableRows(page)).toHaveCount(5);
        // Reset restores original 5
        await resetBtn(page).click();
        await expect(tableRow(page, '3')).toBeVisible();
        await expect(tableRow(page, '6')).not.toBeAttached();
        await expect(tableRows(page)).toHaveCount(5);
    });

    test('sort + move + shuffle', async ({ page }) => {
        await page.goto('/datatable');
        await wsReady(page);
        // Sort by age
        await sortColBtn(page, 'age').click();
        await expect(tableRows(page).nth(0)).toHaveAttribute('az-key', '2');
        // Move first (Bob, age 25) to top -- already first after age sort
        await moveTopBtn(page, '2').click();
        // Shuffle
        await shuffleBtn(page).click();
        await expect(tableRows(page)).toHaveCount(5);
        for (const key of ['1', '2', '3', '4', '5']) {
            await expect(tableRow(page, key)).toBeVisible();
        }
    });

    test('multiple add/delete cycles', async ({ page }) => {
        await page.goto('/datatable');
        await wsReady(page);
        // Delete all
        for (const key of ['1', '2', '3', '4', '5']) {
            await deleteBtn(page, key).click();
        }
        await expect(tableRows(page)).toHaveCount(0);
        // Add 3
        await addRowBtn(page).click();
        await addRowBtn(page).click();
        await addRowBtn(page).click();
        await expect(tableRows(page)).toHaveCount(3);
        // Delete middle
        await deleteBtn(page, '7').click();
        await expect(tableRows(page)).toHaveCount(2);
        await expect(tableRow(page, '6')).toBeVisible();
        await expect(tableRow(page, '8')).toBeVisible();
    });
});
