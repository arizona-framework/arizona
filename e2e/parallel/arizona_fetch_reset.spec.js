import { expect, test } from '@playwright/test';

const wsReady = (page) =>
    page.waitForFunction(() => document.documentElement.classList.contains('az-connected'));

test('az-form-reset on a fetch 2xx keeps what was typed while the request was in flight', async ({
    page,
}) => {
    await page.goto('/fetch-reset');
    await wsReady(page);

    // Hold the request in flight until the user has started the next message.
    /** @type {(r: import('@playwright/test').Route) => void} */
    let held = () => {};
    const sent = new Promise((resolve) => {
        held = resolve;
    });
    await page.route('**/fetch-reset/submit', (route) => held(route));

    await page.locator('#to').pressSequentially('ada@example.com');
    await page.locator('#msg').pressSequentially('message 1');
    await page.locator('#urgent').check();
    await page.locator('#reset-form button[type="submit"]').click();
    const route = /** @type {import('@playwright/test').Route} */ (await sent);
    expect(route.request().postData()).toContain('msg=message+1');

    // The next message, typed before the first one's reply.
    await page.locator('#msg').press('ControlOrMeta+a');
    await page.locator('#msg').pressSequentially('message 2');
    await route.continue();

    // The fields sent and untouched since reset; the one typed into keeps its text.
    await expect(page.locator('#to')).toHaveValue('');
    await expect(page.locator('#urgent')).not.toBeChecked();
    await expect(page.locator('#msg')).toHaveValue('message 2');
    // The browser's own reset: the emptied required field is not flagged invalid.
    expect(await page.locator('#to').evaluate((e) => e.matches(':user-invalid'))).toBe(false);

    // Focus and caret stayed in the message being typed.
    await page.keyboard.type(' more');
    await expect(page.locator('#msg')).toHaveValue('message 2 more');
});
