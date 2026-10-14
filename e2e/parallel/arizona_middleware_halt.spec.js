import { expect, test } from '@playwright/test';
import { expectStaysConnected } from '../utils/helpers.js';

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
        // The WS must stay connected -- a failed mount of the target (e.g. a
        // restricted-keys collision) would close 4501 and reload the page,
        // silently absorbed into a passing assertion below.
        await expectStaysConnected(page, async () => {
            await page.click('#protected-link');
            await expect(page).toHaveURL('/login');
        });
        await expect(page.locator('h1')).toHaveText('Sign in');
    });

    test('carries a flash set before the halt-redirect to the target page', async ({ page }) => {
        await page.goto('/navigate-halt');
        await wsReady(page);
        // The /protected middleware sets a flash then halts with a redirect. Over
        // the WS navigate there is no Set-Cookie leg, so the flash rides the socket
        // in-process to /login; /login reads it via fetch_flash and renders it --
        // the flash-over-navigate round-trip, delivered exactly once with no cookie.
        await expectStaysConnected(page, async () => {
            await page.click('#protected-link');
            await expect(page).toHaveURL('/login');
        });
        await expect(page.locator('#flash')).toHaveText('Please sign in first.');
    });

    test('a halt without a redirect loads the response in place, one history entry', async ({
        page,
    }) => {
        await page.goto('/navigate-halt');
        await wsReady(page);
        // The middleware halts with no redirect: the server answers the SPA
        // navigate with a full navigation, and HTTP serves the halt's 403.
        const before = await page.evaluate(() => history.length);
        const response = page.waitForResponse((r) => new URL(r.url()).pathname === '/forbidden');
        await page.click('#forbidden-link');
        expect((await response).status()).toBe(403);
        await expect(page).toHaveURL('/forbidden');
        // The client pushed /forbidden before asking; loading that same URL is a
        // replace per the HTML navigate algorithm, so there is one new entry and
        // one Back leaves the halt.
        expect(await page.evaluate(() => history.length)).toBe(before + 1);
        await page.goBack();
        await expect(page).toHaveURL('/navigate-halt');
    });
});
