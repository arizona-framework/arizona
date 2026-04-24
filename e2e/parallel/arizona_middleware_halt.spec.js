import { expect, test } from '@playwright/test';

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
        await page.click('#protected-link');
        await expect(page).toHaveURL('/login');
        await expect(page.locator('h1')).toHaveText('Sign in');
    });
});
