import { expect, test } from '@playwright/test';

// ---------------------------------------------------------------------------
// Document Picture-in-Picture in a real Chromium: a popped-out view keeps being
// patched in its window, and what comes back inline when the window closes is
// what the server rendered last, never a stale copy.
// ---------------------------------------------------------------------------

const wsReady = (page) =>
    page.waitForFunction(() => document.documentElement.classList.contains('az-connected'));

/** Wait until the PiP window holds an element with `id`. */
const inPip = (page, id) =>
    page.waitForFunction((id) => {
        const pip = /** @type {any} */ (window).documentPictureInPicture.window;
        return !!pip?.document.getElementById(id);
    }, id);

const pipOpen = (page) =>
    page.evaluate(() => /** @type {any} */ (window).documentPictureInPicture.window !== null);

test('a navigate of a popped-out root comes back inline as the new page', async ({ page }) => {
    await page.goto('/pip');
    await wsReady(page);
    await page.locator('#pip_demo').getByRole('button', { name: 'Pop out', exact: true }).click();
    await inPip(page, 'pip_demo');

    // The root is resolved in the window, so the new page lands there.
    await page.getByRole('link', { name: 'About' }).click();
    await inPip(page, 'about-page');

    await page.evaluate(() => /** @type {any} */ (window).documentPictureInPicture.window.close());

    await expect(page.locator('#about-page')).toBeVisible();
    await expect(page.locator('#pip_demo')).toHaveCount(0);
});

test('a parent re-render that brings a popped-out child back inline closes its window', async ({
    page,
}) => {
    await page.goto('/pip-parent');
    await wsReady(page);
    await page.locator('#pip_child').getByRole('button', { name: 'Pop out', exact: true }).click();
    await inPip(page, 'pip_child');

    // The swap re-renders the child's slot wholesale, placeholder included.
    await page.locator('#swap').click();

    await expect(page.locator('article #pip_child')).toBeVisible();
    await expect.poll(() => pipOpen(page)).toBe(false);
    await expect(page.locator('#pip_child')).toHaveCount(1);
});
