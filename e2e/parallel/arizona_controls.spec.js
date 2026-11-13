import { expect, test } from '@playwright/test';

// Server-owned controls: once the user edits a control, the attribute a server write
// changes is only its default. A refused event changes no binding, so no op answers
// it, and the control must still show what the server rendered; a server push must
// not clobber what the user is typing and has not sent.

const wsReady = (page) =>
    page.waitForFunction(() => document.documentElement.classList.contains('az-connected'));

test.describe('server-owned controls', () => {
    test.beforeEach(async ({ page }) => {
        await page.goto('/controls');
        await wsReady(page);
    });

    test('a refused value shows the stored one again', async ({ page }) => {
        const qty = page.locator('#qty');
        await qty.fill('42');
        await qty.press('Tab');
        await expect(page.locator('#shown')).toHaveText('42');
        await expect(qty).toHaveValue('42');

        await qty.fill('999');
        await qty.press('Tab');
        await expect(qty).toHaveValue('42');
        await expect(page.locator('#shown')).toHaveText('42');
    });

    test('a refused checkbox shows its stored state', async ({ page }) => {
        const box = page.locator('#locked');
        await box.click();
        await expect(box).not.toBeChecked();
    });

    test('a push does not clobber unsent typing; leaving the field shows it', async ({ page }) => {
        const note = page.locator('#note');
        await page.locator('#arm').click();
        await note.click();
        await note.fill('typing');
        await expect(note).toHaveAttribute('value', 'pushed');
        await expect(note).toHaveValue('typing');
        await expect(note).toBeFocused();

        await note.press('Tab');
        await expect(note).toHaveValue('pushed');
    });
});
