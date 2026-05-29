import { expect, test } from '@playwright/test';
import { expectStaysConnected } from '../utils/helpers.js';

// ---------------------------------------------------------------------------
// ?local inside nested ?stateful children: per-view scoping, and the
// client-owned slot surviving server-driven updates (the child's own event and
// a parent-propagated prop change) -- all without a round-trip for the sets.
// ---------------------------------------------------------------------------

const wsReady = (page) =>
    page.waitForFunction(() => document.documentElement.classList.contains('az-connected'));

const note = (page, id) => page.locator(`#${id} .note`);
const count = (page, id) => page.locator(`#${id} .count`);
const label = (page, id) => page.locator(`#${id} .label`);
const pnote = (page) => page.locator('#local_nested .pnote');

const countMessages = (page) =>
    page.evaluate(() => {
        /** @type {any} */ (window).__msgs = 0;
        const orig = window._ws.onmessage;
        window._ws.onmessage = (e) => {
            /** @type {any} */ (window).__msgs += 1;
            if (orig) orig(e);
        };
    });
const messageCount = (page) => page.evaluate(() => /** @type {any} */ (window).__msgs);

test.describe('?local in nested children -- SSR', () => {
    test('each child and the parent render their own initial slot values', async ({ page }) => {
        await page.goto('/local-nested');
        await expect(note(page, 'child_a')).toHaveText('untouched');
        await expect(note(page, 'child_b')).toHaveText('untouched');
        await expect(label(page, 'child_a')).toHaveText('Label: v1');
        await expect(count(page, 'child_a')).toHaveText('Count: 0');
        await expect(pnote(page)).toHaveText('Parent note: parent');
    });
});

test.describe('?local in nested children -- client-only isolation', () => {
    test('a child set is scoped to that child; a parent set never reaches children', async ({
        page,
    }) => {
        await page.goto('/local-nested');
        await wsReady(page);
        await countMessages(page);

        await expectStaysConnected(page, async () => {
            // Edit child_a's note: only child_a changes.
            await page.locator('#child_a .edit').click();
            await expect(note(page, 'child_a')).toHaveText('edited');
            await expect(note(page, 'child_b')).toHaveText('untouched');
            await expect(pnote(page)).toHaveText('Parent note: parent');

            // Edit the parent note: children are untouched (set(parent) skips child views).
            await page.locator('#local_nested .edit-pnote').click();
            await expect(pnote(page)).toHaveText('Parent note: parent-edited');
            await expect(note(page, 'child_a')).toHaveText('edited');
            await expect(note(page, 'child_b')).toHaveText('untouched');
            await page.waitForTimeout(200);
        });

        expect(await messageCount(page), 'client-owned sets send no server frame').toBe(0);
    });
});

test.describe('?local in nested children -- survives server updates', () => {
    test("a child's own server event preserves its client-owned local", async ({ page }) => {
        await page.goto('/local-nested');
        await wsReady(page);

        await page.locator('#child_a .edit').click(); // client-only: note -> "edited"
        await expect(note(page, 'child_a')).toHaveText('edited');

        // Server round-trip: child_a increments. The count updates over WS, but the
        // diff-skipped ?local note survives.
        await page.locator('#child_a .inc').click();
        await expect(count(page, 'child_a')).toHaveText('Count: 1');
        await expect(note(page, 'child_a')).toHaveText('edited');
        await expect(note(page, 'child_b')).toHaveText('untouched');
    });

    test('a parent-propagated prop change preserves children client-owned locals', async ({
        page,
    }) => {
        await page.goto('/local-nested');
        await wsReady(page);

        await page.locator('#child_a .edit').click(); // client-only: child_a note -> "edited"
        await expect(note(page, 'child_a')).toHaveText('edited');

        // Server round-trip: parent relabels, propagating a new prop to both children.
        // Both labels update over WS, but child_a's diff-skipped ?local survives.
        await page.locator('#local_nested .relabel').click();
        await expect(label(page, 'child_a')).toHaveText('Label: v2');
        await expect(label(page, 'child_b')).toHaveText('Label: v2');
        await expect(note(page, 'child_a')).toHaveText('edited');
        await expect(note(page, 'child_b')).toHaveText('untouched');
    });
});

test.describe('?local in nested children -- server-driven set (handler effect)', () => {
    test('a set_all handler effect resets both children notes from the server', async ({
        page,
    }) => {
        await page.goto('/local-nested');
        await wsReady(page);

        // Edit child_a's note client-side first.
        await page.locator('#child_a .edit').click();
        await expect(note(page, 'child_a')).toHaveText('edited');

        // Server event whose handler returns [arizona_js:set_all("note", "reset")] --
        // the ?local is updated via an EFFECT, applied client-side to every view.
        await page.getByRole('button', { name: 'Reset notes' }).click();
        await expect(note(page, 'child_a')).toHaveText('reset');
        await expect(note(page, 'child_b')).toHaveText('reset');
    });
});
