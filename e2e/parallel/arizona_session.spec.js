import { expect, test } from '@playwright/test';

const wsReady = (page) =>
    page.waitForFunction(() => document.documentElement.classList.contains('az-connected'));

const sessionCookie = async (context) =>
    (await context.cookies()).find((c) => c.name === 'az_session');

test('session write loop: fetch rotates the encrypted cookie, view updates live, value survives reload', async ({
    page,
    context,
}) => {
    await page.goto('/session');
    await wsReady(page);

    // Baseline: no session yet, name unset.
    expect(await sessionCookie(context)).toBeUndefined();
    await expect(page.locator('#current-name')).toHaveText('Name: (none)');
    await expect(page.locator('#status')).toHaveAttribute('data-saved', 'no');

    // Sentinel: a full-page reload would wipe this window flag.
    await page.evaluate(() => {
        window.__noReload = true;
    });

    await page.locator('#name-input').fill('Ada');
    await page.locator('#session-form button[type="submit"]').click();

    // (1) The view repainted live via push_event, and the request-local effect applied --
    //     no full-page reload.
    await expect(page.locator('#current-name')).toHaveText('Name: Ada');
    await expect(page.locator('#status')).toHaveAttribute('data-saved', 'yes');
    expect(await page.evaluate(() => window.__noReload)).toBe(true);

    // (2) The encrypted session cookie was set: HttpOnly and opaque (the plaintext name
    //     must not be readable from the cookie value).
    const cookie = await sessionCookie(context);
    expect(cookie).toBeDefined();
    expect(cookie.httpOnly).toBe(true);
    expect(cookie.value.length).toBeGreaterThan(0);
    expect(cookie.value).not.toContain('Ada');

    // (3) The round-trip: reload -> the server decrypts the cookie via fetch_session and
    //     re-renders the persisted name. Proves encrypt -> cookie -> decrypt end to end.
    await page.reload();
    await wsReady(page);
    await expect(page.locator('#current-name')).toHaveText('Name: Ada');
});
