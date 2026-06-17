import { expect, test } from '@playwright/test';

const wsReady = (page) =>
    page.waitForFunction(() => document.documentElement.classList.contains('az-connected'));

test('fetch updates the submitting view via a push_event response (no pubsub)', async ({
    page,
    context,
}) => {
    await page.goto('/fetch-push');
    await wsReady(page);

    await expect(page.locator('#message')).toHaveText('');

    // Submit -> arizona_js:fetch. The controller sets a cookie and returns a
    // push_event, which the client relays over the WS -> handle_event re-renders.
    await page.locator('#push-form button[type="submit"]').click();

    await expect(page.locator('#message')).toHaveText('Saved via push_event');

    // The same fetch still set the HttpOnly cookie.
    const cookie = (await context.cookies()).find((c) => c.name === 'pushed');
    expect(cookie).toBeDefined();
    expect(cookie.value).toBe('1');
    expect(cookie.httpOnly).toBe(true);
});
