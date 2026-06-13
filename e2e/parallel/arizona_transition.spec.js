import { expect, test } from '@playwright/test';
import { expectStaysConnected } from '../utils/helpers.js';

/** Wait for the WebSocket to be open. */
const wsReady = (page) =>
    page.waitForFunction(() => document.documentElement.classList.contains('az-connected'));

// View transitions are progressive enhancement; these assert that wrapping the
// SPA navigation / client effect in startViewTransition does not break the
// behaviour (navigation completes, the connection survives, the shared element
// is present on both pages). Frame-accurate animation is not asserted -- it is
// inherently timing-dependent.
test.describe('view transitions', () => {
    test('SSR renders the shared-element card and the transition links', async ({ page }) => {
        await page.goto('/transitions');
        await expect(page.locator('main h1')).toHaveText('Transitions');
        await expect(page.locator('#card.vt-card')).toBeVisible();
        await expect(page.getByRole('link', { name: 'Open (cross-fade)' })).toBeVisible();
        await expect(page.getByRole('link', { name: 'Open (slide)' })).toBeVisible();
    });

    test('view-transition-name is applied to the shared card', async ({ page }) => {
        await page.goto('/transitions');
        const name = await page
            .locator('#card')
            .evaluate((el) => getComputedStyle(el).viewTransitionName);
        expect(name).toBe('vt-card');
    });

    test('az_transition link navigates and keeps the connection alive', async ({ page }) => {
        await page.goto('/transitions');
        await wsReady(page);
        await expectStaysConnected(page, async () => {
            await page.getByRole('link', { name: 'Open (cross-fade)' }).click();
            await expect(page.locator('main h1')).toHaveText('Transition detail');
        });
        // The shared element exists on the destination too (the morph target).
        await expect(page.locator('#card.vt-card')).toBeVisible();
    });

    test('typed (slide) link navigates with a view-transition type', async ({ page }) => {
        await page.goto('/transitions');
        await wsReady(page);
        await expectStaysConnected(page, async () => {
            await page.getByRole('link', { name: 'Open (slide)' }).click();
            await expect(page.locator('main h1')).toHaveText('Transition detail');
        });
    });

    test('back link returns to the list', async ({ page }) => {
        await page.goto('/transitions/detail');
        await wsReady(page);
        await expectStaysConnected(page, async () => {
            await page.getByRole('link', { name: 'Back (cross-fade)' }).click();
            await expect(page.locator('main h1')).toHaveText('Transitions');
        });
    });

    test('browser back after a transitioned nav returns to the list', async ({ page }) => {
        await page.goto('/transitions');
        await wsReady(page);
        await page.getByRole('link', { name: 'Open (cross-fade)' }).click();
        await expect(page.locator('main h1')).toHaveText('Transition detail');
        await expectStaysConnected(page, async () => {
            await page.goBack();
            await expect(page.locator('main h1')).toHaveText('Transitions');
        });
    });

    test('client-side toggle wrapped in a transition shows the panel', async ({ page }) => {
        await page.goto('/transitions');
        await wsReady(page);
        const panel = page.locator('#panel');
        await expect(panel).toBeHidden();
        await page.getByRole('button', { name: 'Toggle panel' }).click();
        await expect(panel).toBeVisible();
    });
});
