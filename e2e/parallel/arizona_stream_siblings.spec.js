import { expect, test } from '@playwright/test';

// A stream `?each` sharing one content slot with static siblings. Its slot az is
// compound and carried by no element of its own, and the compound BASE az is the view
// root's, which a descendant-only querySelector cannot return -- so the client can only
// reach the slot through its `<!--az:X-->` marker.
//
// `arizona_diff_SUITE` / `arizona_render_SUITE` pin that the server EMITS the
// marker-aware OP_TEXT for a container full render. These assert the other half, in a
// real browser: that applying it patches the slot in place and leaves the siblings
// standing. A whole-element write here resolves to the root and innerHTML-wipes header,
// title and footer, which is the regression this shape exists to catch.
//
// `arizona_datatable.spec.js` does not cover it: its each is the sole child of a tbody,
// the one shape that survives either way.

const wsReady = (page) =>
    page.waitForFunction(() => document.documentElement.classList.contains('az-connected'));

const items = (page) => page.locator('#host > .item');
const header = (page) => page.locator('#host > .header');
const footer = (page) => page.locator('#host > .footer');

test.beforeEach(async ({ page }) => {
    await page.goto('/stream-siblings');
    await wsReady(page);
});

test('SSR anchors the each among its static siblings', async ({ page }) => {
    await expect(header(page)).toHaveText('header');
    await expect(footer(page)).toHaveText('footer');
    await expect(items(page)).toHaveText(['one', 'two']);
    // The siblings and the stream share one parent -- that adjacency is the whole point.
    await expect(page.locator('#host')).toContainText('StreamSiblings');
});

// THE discriminating case. A keyed reset stays incremental (per-item ops), so only the
// stream-to-map type switch produces a wholesale container re-render addressed to the
// each's compound slot az -- an az no element carries, whose base is `#host`. A
// whole-element write would resolve to `#host` and innerHTML-wipe header, title and
// footer; under the marker-aware OP_TEXT only the slot's own span is replaced.
test('a container full render keeps the static siblings', async ({ page }) => {
    await page.locator('#switch').click();
    await expect(items(page)).toHaveText(['switched']);
    await expect(header(page)).toHaveText('header');
    await expect(footer(page)).toHaveText('footer');
    await expect(page.locator('#host')).toContainText('StreamSiblings');
});

test('a keyed reset keeps the static siblings', async ({ page }) => {
    await page.locator('#reset').click();
    await expect(items(page)).toHaveText(['reset']);
    await expect(header(page)).toHaveText('header');
    await expect(footer(page)).toHaveText('footer');
    await expect(page.locator('#host')).toContainText('StreamSiblings');
});

// Membership is NOT enough here. `OP_INSERT` places by position against the resolved
// container, and for this shape that container is `#host`, so an unanchored insert
// appends the item AFTER the footer -- still a `#host > .item`, still reading
// "one, two, added" in document order. Only the node's position relative to the slot's
// closing marker distinguishes correct from broken, so assert that.
const shapeOf = (page) =>
    page.evaluate(() =>
        [...document.querySelector('#host').childNodes]
            .map((n) =>
                n.nodeType === 8
                    ? n.data.startsWith('/')
                        ? '/slot'
                        : 'slot'
                    : n.nodeType === 1 && n.classList.contains('item')
                      ? 'item'
                      : n.nodeType === 1
                        ? n.className
                        : null,
            )
            .filter(Boolean)
            .join(','),
    );

test('an incremental insert lands inside the slot, not after the siblings', async ({ page }) => {
    await page.locator('#add').click();
    await expect(items(page)).toHaveText(['one', 'two', 'added']);
    // Every item must sit between the each's opening and closing markers, with the
    // footer still last. An unanchored append yields `...,/slot,footer,item`.
    expect(await shapeOf(page)).toBe('header,slot,/slot,slot,item,item,item,/slot,footer');
    await expect(header(page)).toHaveText('header');
    await expect(footer(page)).toHaveText('footer');
});

test('the siblings survive a full render that follows an incremental one', async ({ page }) => {
    // Ordering matters: the insert leaves per-item state the container render must not
    // trip over, and it is the sequence a real list goes through.
    await page.locator('#add').click();
    await expect(items(page)).toHaveText(['one', 'two', 'added']);
    await page.locator('#reset').click();
    await expect(items(page)).toHaveText(['reset']);
    await expect(header(page)).toHaveText('header');
    await expect(footer(page)).toHaveText('footer');
});
