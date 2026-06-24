import { expect, test } from '@playwright/test';

const wsReady = (page) =>
    page.waitForFunction(() => document.documentElement.classList.contains('az-connected'));

test('fetch runs on_error commands and fires arizona:fetch-error when the response has no effects body', async ({
    page,
}) => {
    await page.goto('/fetch-error');
    await wsReady(page);

    // Capture the arizona:fetch-error DOM event the failure path dispatches.
    await page.evaluate(() => {
        window.__fetchError = false;
        document.addEventListener('arizona:fetch-error', () => {
            window.__fetchError = true;
        });
    });

    await expect(page.locator('#fetch-status')).not.toHaveClass(/errored/);

    // Submit -> arizona_js:fetch. The controller replies 500 with no effects body, so the
    // client takes the failure path and runs the on_error list (add a class + set an attr).
    await page.locator('#error-form button[type="submit"]').click();

    // Both on_error commands ran -- the list-unwrap reached the client.
    await expect(page.locator('#fetch-status')).toHaveClass(/errored/);
    await expect(page.locator('#fetch-status')).toHaveAttribute('data-errored', 'yes');

    // The failure path also dispatched arizona:fetch-error on document.
    await expect.poll(() => page.evaluate(() => window.__fetchError)).toBe(true);
});
