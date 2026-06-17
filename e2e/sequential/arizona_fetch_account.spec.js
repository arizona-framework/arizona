import { expect, test } from '@playwright/test';

const wsReady = (page) =>
    page.waitForFunction(() => document.documentElement.classList.contains('az-connected'));

const sidCookie = async (context) => (await context.cookies()).find((c) => c.name === 'sid');

test.describe
    .serial('arizona_js:fetch', () => {
        test('posts via fetch: cookie set, effect applied, view repainted, no reload, field kept', async ({
            page,
            context,
        }) => {
            await page.goto('/fetch-account');
            await wsReady(page);

            // Sentinel: a full-page reload would wipe this window flag.
            await page.evaluate(() => {
                window.__noReload = true;
            });

            // Type into the field we expect to survive the submit (a static input, never diffed).
            await page.locator('#note').fill('keep me');

            // Baseline: no sid cookie, status idle, count 0.
            expect(await sidCookie(context)).toBeUndefined();
            await expect(page.locator('#status')).toHaveAttribute('data-saved', 'no');
            await expect(page.locator('#saved-count')).toHaveText('Saved: 0');

            // Submit -> arizona_js:fetch (no native POST, no navigation).
            await page.locator('#account-form button[type="submit"]').click();

            // (1) The request-local effect from the fetch response applied.
            await expect(page.locator('#status')).toHaveAttribute('data-saved', 'yes');

            // (2) The live view repainted via pubsub -> WebSocket: server-computed
            //     content (the message) is rendered server-authoritatively, not by an
            //     imperative DOM effect.
            await expect(page.locator('#saved-count')).toHaveText('Saved: 1');
            await expect(page.locator('#message')).toHaveText('Account updated');

            // (3) The controller's HttpOnly cookie was applied by the browser.
            const sid = await sidCookie(context);
            expect(sid).toBeDefined();
            expect(sid.value).toBe('rotated');
            expect(sid.httpOnly).toBe(true);

            // (4) No full-page reload happened, and the typed field survived.
            expect(await page.evaluate(() => window.__noReload)).toBe(true);
            await expect(page.locator('#note')).toHaveValue('keep me');
        });
    });
