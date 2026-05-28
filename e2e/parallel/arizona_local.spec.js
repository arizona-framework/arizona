import { expect, test } from '@playwright/test';
import { expectStaysConnected } from '../utils/helpers.js';

// ---------------------------------------------------------------------------
// Client-owned slots (?local): the browser owns these slots and updates them
// locally -- no WebSocket round-trip. The server renders the initial value
// once and never diffs them.
// ---------------------------------------------------------------------------

const wsReady = (page) =>
    page.waitForFunction(() => document.documentElement.classList.contains('az-connected'));

const dialog = (page) => page.locator('#local_demo dialog');
const dialogTitle = (page) => page.locator('#local_demo dialog h3 span');
const openBtn = (page) => page.getByRole('button', { name: 'Open dialog' });
const closeBtn = (page) => page.getByRole('button', { name: 'Close' });
const tabs = (page) => page.locator('#local_demo .tabs');
const homePanel = (page) => page.locator('#local_demo .panel-home');
const settingsPanel = (page) => page.locator('#local_demo .panel-settings');
const homeTab = (page) => page.getByRole('button', { name: 'Home' });
const settingsTab = (page) => page.getByRole('button', { name: 'Settings' });

// ---------------------------------------------------------------------------
// SSR -- initial render
// ---------------------------------------------------------------------------

test.describe('?local -- SSR', () => {
    test('dialog starts closed (open attribute absent)', async ({ page }) => {
        await page.goto('/local');
        await expect(dialog(page)).not.toHaveAttribute('open');
        await expect(dialog(page)).toBeHidden();
    });

    test('tabs default to home: home panel visible, settings hidden', async ({ page }) => {
        await page.goto('/local');
        await expect(tabs(page)).toHaveAttribute('data-active', 'home');
        await expect(homePanel(page)).toBeVisible();
        await expect(settingsPanel(page)).toBeHidden();
    });
});

// ---------------------------------------------------------------------------
// Dialog -- bound attribute (open) + bound content (title)
// ---------------------------------------------------------------------------

test.describe('?local -- dialog', () => {
    test('opening sets the open attribute and the title content', async ({ page }) => {
        await page.goto('/local');
        await wsReady(page);
        await openBtn(page).click();
        await expect(dialog(page)).toHaveAttribute('open', '');
        await expect(dialog(page)).toBeVisible();
        await expect(dialogTitle(page)).toHaveText('Hello from the client!');
    });

    test('closing removes the open attribute', async ({ page }) => {
        await page.goto('/local');
        await wsReady(page);
        await openBtn(page).click();
        await expect(dialog(page)).toBeVisible();
        await closeBtn(page).click();
        await expect(dialog(page)).not.toHaveAttribute('open');
        await expect(dialog(page)).toBeHidden();
    });
});

// ---------------------------------------------------------------------------
// Tabs -- one bound container attribute drives CSS
// ---------------------------------------------------------------------------

test.describe('?local -- tabs', () => {
    test('switching tabs flips data-active and panel visibility', async ({ page }) => {
        await page.goto('/local');
        await wsReady(page);

        await settingsTab(page).click();
        await expect(tabs(page)).toHaveAttribute('data-active', 'settings');
        await expect(settingsPanel(page)).toBeVisible();
        await expect(homePanel(page)).toBeHidden();

        await homeTab(page).click();
        await expect(tabs(page)).toHaveAttribute('data-active', 'home');
        await expect(homePanel(page)).toBeVisible();
        await expect(settingsPanel(page)).toBeHidden();
    });
});

// ---------------------------------------------------------------------------
// No server round-trip
// ---------------------------------------------------------------------------

test.describe('?local -- client-only', () => {
    test('updating a slot sends no server message and stays connected', async ({ page }) => {
        await page.goto('/local');
        await wsReady(page);

        // Count any server -> client message that arrives after the click.
        await page.evaluate(() => {
            /** @type {any} */ (window).__msgs = 0;
            const orig = window._ws.onmessage;
            window._ws.onmessage = (e) => {
                /** @type {any} */ (window).__msgs += 1;
                if (orig) orig(e);
            };
        });

        await expectStaysConnected(page, async () => {
            await openBtn(page).click();
            await expect(dialog(page)).toBeVisible();
            await settingsTab(page).click();
            await expect(settingsPanel(page)).toBeVisible();
            await page.waitForTimeout(200);
        });

        const msgs = await page.evaluate(() => /** @type {any} */ (window).__msgs);
        expect(msgs, 'no server round-trip for ?local updates').toBe(0);
    });

    test('a forced reconnect resets slots to their SSR initial', async ({ page }) => {
        await page.goto('/local');
        await wsReady(page);

        // Open the dialog locally (no round-trip).
        await openBtn(page).click();
        await expect(dialog(page)).toBeVisible();

        // close(4000) is neither 1000 (normal, no reconnect) nor 4500 (crash):
        // the client auto-reconnects and the server fresh-mounts the view (the
        // old live process died -- drain is server-initiated only), so client-owned
        // slots reset to their SSR initial (dialog closed).
        await page.evaluate(() => window._ws.close(4000));
        await wsReady(page);
        await expect(dialog(page)).toBeHidden();
    });
});
