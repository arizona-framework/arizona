import { expect, test } from '@playwright/test';

const wsReady = (page) =>
    page.waitForFunction(() => document.documentElement.classList.contains('az-connected'));

test('a multi-submit-button form reports which button fired plus its fields', async ({ page }) => {
    await page.goto('/form-submitter');
    await wsReady(page);

    await expect(page.locator('#chosen')).toHaveText('');

    await page.locator('#email').fill('ada@example.com');

    // One az_submit push_event: the payload carries the typed field AND which
    // named submit button fired (via new FormData(form, submitter)).
    await page.locator('#plan-form').getByRole('button', { name: 'Annual' }).click();
    await expect(page.locator('#chosen')).toHaveText('annual');
    await expect(page.locator('#email-echo')).toHaveText('ada@example.com');

    // Same form, other button -> the payload reports the other plan.
    await page.locator('#plan-form').getByRole('button', { name: 'Monthly' }).click();
    await expect(page.locator('#chosen')).toHaveText('monthly');
    await expect(page.locator('#email-echo')).toHaveText('ada@example.com');
});

// A submit let through while the socket is down submits natively with the form's
// own method -- a GET, putting every field in the URL. During reconnect backoff
// (and before the first open) the az-submit listener must still swallow it.
test('an az-submit form never submits natively while disconnected', async ({ page }) => {
    await page.goto('/form-submitter');
    await wsReady(page);
    const url = page.url();

    await page.locator('#email').fill('ada@example.com');
    // 4000: an abnormal close, so the worker backs off before reconnecting.
    await page.evaluate(() => window._ws.close(4000));
    await page.waitForFunction(() =>
        document.documentElement.classList.contains('az-disconnected'),
    );
    await page.evaluate(() => document.getElementById('plan-form').requestSubmit());

    // Give a native submission time to navigate, then prove it did not.
    await page.waitForTimeout(500);
    expect(page.url()).toBe(url);
});
