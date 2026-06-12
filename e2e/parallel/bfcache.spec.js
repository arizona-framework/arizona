import { expect, test } from '@playwright/test';

// ---------------------------------------------------------------------------
// Page-lifecycle handling for back/forward cache (bfcache) eligibility.
//
// A live WebSocket makes a page ineligible for the bfcache, so arizona tears the
// worker (and its socket) down on `pagehide` and reconnects on `pageshow`
// (persisted). This test drives those lifecycle events against a REAL worker +
// real WebSocket and asserts the teardown/reconnect round-trip works.
//
// It deliberately does NOT assert Chromium actually froze the page into the
// bfcache: a CDP-driven browser (Playwright) disables bfcache, so the real
// eligibility is verified out-of-band with Lighthouse. What this guards is the
// runtime behavior of the fix, beyond the mocked-worker unit test.
// ---------------------------------------------------------------------------

/** Wait for the WebSocket to be open (the worker reports connected). */
const wsReady = (page) =>
    page.waitForFunction(() => document.documentElement.classList.contains('az-connected'));

const wsClosed = (page) =>
    page.waitForFunction(() => document.documentElement.classList.contains('az-disconnected'));

test('tears down the socket on pagehide and reconnects on pageshow', async ({ page }) => {
    await page.goto('/');
    await wsReady(page);

    // pagehide: the worker is terminated and the connection drops so the page
    // can enter the bfcache.
    await page.evaluate(() =>
        window.dispatchEvent(new PageTransitionEvent('pagehide', { persisted: false })),
    );
    await wsClosed(page);

    // pageshow (bfcache restore): the worker is respawned and the socket
    // reconnects -- the page is live again without a reload.
    await page.evaluate(() =>
        window.dispatchEvent(new PageTransitionEvent('pageshow', { persisted: true })),
    );
    await wsReady(page);

    // A non-persisted pageshow (normal load) must not spawn a second worker.
    await page.evaluate(() =>
        window.dispatchEvent(new PageTransitionEvent('pageshow', { persisted: false })),
    );
    await expect(page.locator('html')).toHaveClass(/az-connected/);
});
