import { expect, test } from '@playwright/test';

// ---------------------------------------------------------------------------
// In-place SPA navigation (az-patch). The whole view is persistent chrome with
// a section-driven content slot and a chrome-owned `bump` counter. Clicking an
// az-patch link switches the :section: the server keeps the live view and
// re-renders it via handle_update/3, so the content swaps while the counter
// (and the live process) survive -- no remount. The patch reaction's set_title
// effect updates the document title. Back/forward replays as a patch.
// ---------------------------------------------------------------------------

const wsReady = (page) =>
    page.waitForFunction(() => document.documentElement.classList.contains('az-connected'));

test('az-patch keeps the live view: content swaps, chrome state survives', async ({ page }) => {
    await page.goto('/patch-demo/overview');
    await wsReady(page);
    await expect(page.locator('#content')).toHaveText('overview');

    // Bump the chrome-owned counter (it lives in the shell, not the content slot).
    await page.locator('#bump').click();
    await page.locator('#bump').click();
    await expect(page.locator('#clicks')).toHaveText('2');

    // Patch to another section: same handler -> in-place diff, no remount.
    await page.locator('a[href="/patch-demo/projects"]').click();
    await expect(page).toHaveURL(/\/patch-demo\/projects$/);
    await expect(page.locator('#content')).toHaveText('projects');
    // The counter SURVIVED -- the root view was not remounted.
    await expect(page.locator('#clicks')).toHaveText('2');
    // The handle_update set_title effect applied.
    await expect(page).toHaveTitle('projects');

    // A second patch, then back: both the patched entry and the original
    // full-load entry re-patch (the outgoing entry is _azNav-tagged), so the
    // counter survives all the way back.
    await page.locator('a[href="/patch-demo/settings"]').click();
    await expect(page.locator('#content')).toHaveText('settings');

    await page.goBack();
    await expect(page).toHaveURL(/\/patch-demo\/projects$/);
    await expect(page.locator('#content')).toHaveText('projects');
    await expect(page.locator('#clicks')).toHaveText('2');

    await page.goBack();
    await expect(page).toHaveURL(/\/patch-demo\/overview$/);
    await expect(page.locator('#content')).toHaveText('overview');
    await expect(page.locator('#clicks')).toHaveText('2');
});
