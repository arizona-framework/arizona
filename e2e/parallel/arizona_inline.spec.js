import { expect, test } from '@playwright/test';
import { expectStaysConnected } from '../utils/helpers.js';

// The /inline view (test/support/arizona_inline.erl) reads every binding into a
// body variable BEFORE the ?html call (the "hoisted read" pattern) across several
// shapes: a plain hoisted read (count), a derived chain (doubled = count * 2), and
// a statement-form case bind (parity). The binding-read inlining feature rewrites
// each interpolated variable back into its slot so it tracks `count` and diffs.
// If any slot regressed to "frozen" (empty deps), its <span> would not update on
// click -- this spec asserts every slot patches over the real WebSocket.

const wsReady = (page) =>
    page.waitForFunction(() => document.documentElement.classList.contains('az-connected'));

const incBtn = (page) => page.locator(`#inline button[az-click*='"inc"']`);
const countText = (page) => page.locator('#inline .count');
const doubledText = (page) => page.locator('#inline .doubled');
const parityText = (page) => page.locator('#inline .parity');

const expectState = async (page, { count, doubled, parity }) => {
    await expect(countText(page)).toHaveText(count);
    await expect(doubledText(page)).toHaveText(doubled);
    await expect(parityText(page)).toHaveText(parity);
};

test.describe('binding-read inlining', () => {
    test('hoisted reads track and diff over the wire', async ({ page }) => {
        await page.goto('/inline');
        await wsReady(page);

        // Baseline (count = 0).
        await expectState(page, { count: '0', doubled: '0', parity: 'even' });

        // Each click mutates `count` server-side; every hoisted slot must re-render.
        await expectStaysConnected(page, () => incBtn(page).click());
        await expectState(page, { count: '1', doubled: '2', parity: 'odd' });

        await expectStaysConnected(page, () => incBtn(page).click());
        await expectState(page, { count: '2', doubled: '4', parity: 'even' });
    });
});
