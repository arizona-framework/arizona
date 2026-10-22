import { expect, test } from '@playwright/test';

// An unlimited stream changed in ONE event that inserts a key and deletes it again,
// with a positional insert or a move queued while that key was in the source. The
// key never reaches the client, so an op placed against an order that counted it
// lands one slot off (or, for a move anchored on it, at the tail). `arizona_stream_SUITE`
// pins the ops the server emits; this applies them through the real client and
// compares the list with `#server-order`, the stream's own final order rendered
// beside it. The later cases are shapes that were already right, kept as guards.

const wsReady = (page) =>
    page.waitForFunction(() => document.documentElement.classList.contains('az-connected'));

const CASES = 12;

// One test, and so one fresh page, per case: a case that goes wrong leaves the client
// off the order the server believes it holds, which the next case's reset would build on.
for (let n = 1; n <= CASES; n++) {
    test(`case ${n}: the client ends in the server order`, async ({ page }) => {
        await page.goto('/stream-positions');
        await wsReady(page);
        const list = page.locator('#list > li');
        const order = page.locator('#server-order');
        const startButton = page.locator(`#start-${n}`);
        const start = await startButton.getAttribute('data-start');
        await startButton.click();
        await expect(order).toHaveText(start);
        await expect(list).toHaveText(start.split(','));
        await page.locator(`#run-${n}`).click();
        await expect(order).not.toHaveText(start);
        await expect(list).toHaveText((await order.textContent()).split(','));
    });
}
