import { expect, test } from '@playwright/test';
import { expectStaysConnected } from '../utils/helpers.js';

// PROBE SPEC (not a permanent fixture): does an OP_TEXT targeting a MARKERLESS
// raw-text element (<textarea>/<title>/<style>) actually reach the DOM, and what
// does it clobber?

const wsReady = (page) =>
    page.waitForFunction(() => document.documentElement.classList.contains('az-connected'));

const bump = (page) => page.locator(`#rawtext button[az-click*='"bump"']`);
const evil = (page) => page.locator(`#rawtext button[az-click*='"evil"']`);

test.describe('raw-text slot patching', () => {
    test('markerless raw-text elements patch', async ({ page }) => {
        await page.goto('/rawtext');
        await wsReady(page);

        // SSR baseline.
        expect(await page.locator('#rawtext .plain').textContent()).toBe('A');
        expect(await page.locator('#rawtext .sole').inputValue()).toBe('A');
        expect(await page.locator('#rawtext .mixed').inputValue()).toBe('Hello A');
        expect(await page.locator('#rawtext .two').inputValue()).toBe('X-A');
        expect(await page.locator('#rawtext .ti').textContent()).toBe('A');

        await expectStaysConnected(page, () => bump(page).click());

        // Give the patch a beat, then read everything at once.
        await expect(page.locator('#rawtext .plain')).toHaveText('B');
        const after = await page.evaluate(() => ({
            sole: document.querySelector('#rawtext .sole').value,
            soleContent: document.querySelector('#rawtext .sole').textContent,
            mixed: document.querySelector('#rawtext .mixed').value,
            two: document.querySelector('#rawtext .two').value,
            ti: document.querySelector('#rawtext .ti').textContent,
            styleColor: getComputedStyle(document.querySelector('#probe-style')).color,
        }));
        console.log('AFTER BUMP:', JSON.stringify(after, null, 2));

        expect(after.sole).toBe('B');
        expect(after.mixed).toBe('Hello B');
        expect(after.two).toBe('Y-B');
        expect(after.ti).toBe('B');
        expect(after.styleColor).toBe('rgb(9, 8, 7)');
    });

    test('user-typed value vs a server patch', async ({ page }) => {
        await page.goto('/rawtext');
        await wsReady(page);

        const ta = page.locator('#rawtext .sole');
        await ta.click();
        await ta.fill('USER TYPED');
        // Put the caret in the middle so we can see if a patch moves it.
        await page.evaluate(() => {
            const el = document.querySelector('#rawtext .sole');
            el.setSelectionRange(4, 4);
        });

        await expectStaysConnected(page, () => bump(page).click());
        await expect(page.locator('#rawtext .plain')).toHaveText('B');

        const after = await page.evaluate(() => {
            const el = document.querySelector('#rawtext .sole');
            return {
                value: el.value,
                defaultValue: el.defaultValue,
                textContent: el.textContent,
                selStart: el.selectionStart,
                selEnd: el.selectionEnd,
                focused: document.activeElement === el,
            };
        });
        console.log('AFTER TYPING + PATCH:', JSON.stringify(after, null, 2));
        // Deliberately no assertion on `value` -- this test EXISTS to report what
        // actually happens to a dirty textarea.
        expect(after.textContent).toBe('B');
    });

    test('a partial change keeps the static text and the sibling dynamic', async ({ page }) => {
        await page.goto('/rawtext');
        await wsReady(page);
        expect(await page.locator('#rawtext .two').inputValue()).toBe('X-A');
        expect(await page.locator('#rawtext .mixed').inputValue()).toBe('Hello A');

        // Only `v` changes; `pre` (= "X") and the statics must survive.
        await expectStaysConnected(page, () =>
            page.locator(`#rawtext button[az-click*='"only_v"']`).click(),
        );
        await expect(page.locator('#rawtext .plain')).toHaveText('Z');

        const after = await page.evaluate(() => ({
            mixed: document.querySelector('#rawtext .mixed').value,
            two: document.querySelector('#rawtext .two').value,
        }));
        console.log('AFTER PARTIAL CHANGE:', JSON.stringify(after));
        expect(after.mixed).toBe('Hello Z');
        expect(after.two).toBe('X-Z');
    });

    test('an escapable raw-text slot cannot inject markup', async ({ page }) => {
        await page.goto('/rawtext');
        await wsReady(page);

        await expectStaysConnected(page, () => evil(page).click());
        await expect(page.locator('#rawtext .plain')).toContainText('a & b < c');

        const after = await page.evaluate(() => ({
            sole: document.querySelector('#rawtext .sole').value,
            imgCount: document.querySelectorAll('#rawtext img').length,
            pwned: window.__pwned ?? null,
            soleChildElements: document.querySelector('#rawtext .sole').children.length,
        }));
        console.log('AFTER EVIL:', JSON.stringify(after, null, 2));

        expect(after.pwned).toBe(null);
        expect(after.imgCount).toBe(0);
        expect(after.soleChildElements).toBe(0);
        expect(after.sole).toBe('a & b < c </textarea><img src=x onerror=window.__pwned=1>');
    });
});
