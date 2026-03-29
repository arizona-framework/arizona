import { test, expect } from '@playwright/test';

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

const wsReady = (page) =>
    page.waitForFunction(() =>
        document.documentElement.classList.contains('az-connected'));

/** The parent view root. */
const view = (page) =>
    page.locator('#mixed[az-view]');

/** The stateless card child (inside section.left). */
const card = (page) =>
    page.locator('#mixed section.left > div');

const cardSpan = (page) =>
    page.locator('#mixed section.left > div > span');

/** The dynamic section (rack). */
const rackSection = (page) =>
    page.locator('#mixed > section:nth-of-type(2)');

/** The dynamic paragraph (message). */
const messagePara = (page) =>
    page.locator('#mixed > p');

/** Action buttons. */
const showBtn = (page) =>
    page.locator(`#mixed button[az-click*='"show"']`);

const hideBtn = (page) =>
    page.locator(`#mixed button[az-click*='"hide"']`);

const updateCardBtn = (page) =>
    page.locator(`#mixed button[az-click*='"update_card"']`);

// ---------------------------------------------------------------------------
// SSR -- initial render
// ---------------------------------------------------------------------------

test.describe('Mixed children -- SSR', () => {
    test('card renders with initial class and label', async ({ page }) => {
        await page.goto('/mixed');
        await expect(card(page)).toHaveClass('card');
        await expect(cardSpan(page)).toHaveText('Initial');
    });

    test('rack section has "rack absent" class', async ({ page }) => {
        await page.goto('/mixed');
        await expect(rackSection(page)).toHaveClass('rack absent');
    });

    test('rack section shows "Empty" content', async ({ page }) => {
        await page.goto('/mixed');
        await expect(rackSection(page)).toContainText('Empty');
    });

    test('message paragraph shows "Hello"', async ({ page }) => {
        await page.goto('/mixed');
        await expect(messagePara(page)).toHaveText('Hello');
    });

    test('all action buttons are visible', async ({ page }) => {
        await page.goto('/mixed');
        await expect(showBtn(page)).toBeVisible();
        await expect(hideBtn(page)).toBeVisible();
        await expect(updateCardBtn(page)).toBeVisible();
    });
});

// ---------------------------------------------------------------------------
// Show event -- this is the core bug scenario
// ---------------------------------------------------------------------------

test.describe('Mixed children -- show event', () => {
    test('rack section class changes to "rack present"', async ({ page }) => {
        await page.goto('/mixed');
        await wsReady(page);
        await showBtn(page).click();
        await expect(rackSection(page)).toHaveClass('rack present');
    });

    test('rack section content changes to "World"', async ({ page }) => {
        await page.goto('/mixed');
        await wsReady(page);
        await showBtn(page).click();
        await expect(rackSection(page)).toContainText('World');
    });

    test('message updates to "Showing: World"', async ({ page }) => {
        await page.goto('/mixed');
        await wsReady(page);
        await showBtn(page).click();
        await expect(messagePara(page)).toHaveText('Showing: World');
    });

    test('card is NOT affected by show event', async ({ page }) => {
        await page.goto('/mixed');
        await wsReady(page);
        await showBtn(page).click();
        // Card should still have its original class and label
        await expect(card(page)).toHaveClass('card');
        await expect(cardSpan(page)).toHaveText('Initial');
    });
});

// ---------------------------------------------------------------------------
// Hide event -- reverses show
// ---------------------------------------------------------------------------

test.describe('Mixed children -- hide event', () => {
    test('rack section reverts to "rack absent"', async ({ page }) => {
        await page.goto('/mixed');
        await wsReady(page);
        await showBtn(page).click();
        await expect(rackSection(page)).toHaveClass('rack present');
        await hideBtn(page).click();
        await expect(rackSection(page)).toHaveClass('rack absent');
    });

    test('rack section content reverts to "Empty"', async ({ page }) => {
        await page.goto('/mixed');
        await wsReady(page);
        await showBtn(page).click();
        await expect(rackSection(page)).toContainText('World');
        await hideBtn(page).click();
        await expect(rackSection(page)).toContainText('Empty');
    });

    test('message updates to "Hidden"', async ({ page }) => {
        await page.goto('/mixed');
        await wsReady(page);
        await showBtn(page).click();
        await expect(messagePara(page)).toHaveText('Showing: World');
        await hideBtn(page).click();
        await expect(messagePara(page)).toHaveText('Hidden');
    });
});

// ---------------------------------------------------------------------------
// Update card -- stateless child changes only
// ---------------------------------------------------------------------------

test.describe('Mixed children -- update card', () => {
    test('card class changes to "card-updated"', async ({ page }) => {
        await page.goto('/mixed');
        await wsReady(page);
        await updateCardBtn(page).click();
        await expect(card(page)).toHaveClass('card-updated');
    });

    test('card label changes to "Updated"', async ({ page }) => {
        await page.goto('/mixed');
        await wsReady(page);
        await updateCardBtn(page).click();
        await expect(cardSpan(page)).toHaveText('Updated');
    });

    test('rack section is NOT affected', async ({ page }) => {
        await page.goto('/mixed');
        await wsReady(page);
        await updateCardBtn(page).click();
        await expect(rackSection(page)).toHaveClass('rack absent');
        await expect(rackSection(page)).toContainText('Empty');
    });

    test('message is NOT affected', async ({ page }) => {
        await page.goto('/mixed');
        await wsReady(page);
        await updateCardBtn(page).click();
        await expect(messagePara(page)).toHaveText('Hello');
    });
});

// ---------------------------------------------------------------------------
// Combined -- show then update card
// ---------------------------------------------------------------------------

test.describe('Mixed children -- combined operations', () => {
    test('show then update card: both update correctly', async ({ page }) => {
        await page.goto('/mixed');
        await wsReady(page);
        // Show
        await showBtn(page).click();
        await expect(rackSection(page)).toHaveClass('rack present');
        await expect(rackSection(page)).toContainText('World');
        await expect(messagePara(page)).toHaveText('Showing: World');
        // Update card
        await updateCardBtn(page).click();
        await expect(card(page)).toHaveClass('card-updated');
        await expect(cardSpan(page)).toHaveText('Updated');
        // Rack and message should be unaffected by card update
        await expect(rackSection(page)).toHaveClass('rack present');
        await expect(rackSection(page)).toContainText('World');
        await expect(messagePara(page)).toHaveText('Showing: World');
    });

    test('update card then show: both update correctly', async ({ page }) => {
        await page.goto('/mixed');
        await wsReady(page);
        // Update card first
        await updateCardBtn(page).click();
        await expect(card(page)).toHaveClass('card-updated');
        await expect(cardSpan(page)).toHaveText('Updated');
        // Show
        await showBtn(page).click();
        await expect(rackSection(page)).toHaveClass('rack present');
        await expect(rackSection(page)).toContainText('World');
        await expect(messagePara(page)).toHaveText('Showing: World');
        // Card should still be updated
        await expect(card(page)).toHaveClass('card-updated');
        await expect(cardSpan(page)).toHaveText('Updated');
    });

    test('show then hide then show: full cycle', async ({ page }) => {
        await page.goto('/mixed');
        await wsReady(page);
        await showBtn(page).click();
        await expect(rackSection(page)).toHaveClass('rack present');
        await hideBtn(page).click();
        await expect(rackSection(page)).toHaveClass('rack absent');
        await expect(messagePara(page)).toHaveText('Hidden');
        await showBtn(page).click();
        await expect(rackSection(page)).toHaveClass('rack present');
        await expect(messagePara(page)).toHaveText('Showing: World');
    });
});
