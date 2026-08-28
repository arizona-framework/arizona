import { expect, test } from '@playwright/test';

// The /events view (test/support/arizona_events_demo.erl) declares no non-bubbling
// event at SSR, so `toggle` is a type the client has never delegated. Arming
// renders a `<details az-toggle>` that arrives as a patch: the worker has to scan
// that markup, report the name, and the main thread has to bind it before the
// element is used. Every other test of that chain mocks the worker; this one runs
// it over a real socket, and a native `toggle` (which does not bubble) on top.

const wsReady = (page) =>
    page.waitForFunction(() => document.documentElement.classList.contains('az-connected'));

test('delegates an event type that only a patch introduces', async ({ page }) => {
    await page.goto('/events');
    await wsReady(page);

    // Nothing on the page declares `toggle` yet.
    await expect(page.locator('[az-toggle]')).toHaveCount(0);
    await expect(page.locator('#toggles')).toHaveText('0');

    await page.click('#arm');
    await expect(page.locator('#det')).toBeVisible();

    // A real, browser-generated toggle. It does not bubble, so it is only
    // reachable at all through the capture-phase listener.
    await page.click('#sum');
    await expect(page.locator('#toggles')).toHaveText('1');

    // And it keeps working, rather than firing once off a stale binding.
    await page.click('#sum');
    await expect(page.locator('#toggles')).toHaveText('2');
});
