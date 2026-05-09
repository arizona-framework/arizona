import { expect, test } from '@playwright/test';
import { expectStaysConnected } from '../utils/connection-watcher.js';

const wsReady = (page) =>
    page.waitForFunction(() => document.documentElement.classList.contains('az-connected'));

test.describe('middleware halt on WS navigate', () => {
    test('redirects to /login via arizona_js:navigate effect', async ({ page }) => {
        await page.goto('/navigate-halt');
        await wsReady(page);
        // Clicking an az-navigate link triggers an SPA navigate frame over WS.
        // The target route's middleware halts via `arizona_req:redirect(Req, "/login")`,
        // which the server translates into an `arizona_js:navigate("/login")` effect.
        // The client applies the effect: pushState + fresh HTTP handshake to /login.
        // The WS must stay connected -- a server-side crash (e.g. the
        // restricted-keys collision this branch fixes) would close 4500,
        // reload, and silently absorb into a passing assertion below.
        await expectStaysConnected(page, async () => {
            await page.click('#protected-link');
            await expect(page).toHaveURL('/login');
        });
        await expect(page.locator('h1')).toHaveText('Sign in');
    });
});
