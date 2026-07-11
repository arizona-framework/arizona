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
