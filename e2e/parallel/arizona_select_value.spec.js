import { expect, test } from '@playwright/test';
import { expectStaysConnected } from '../utils/helpers.js';

// A `value` on a <select> picks the option whose value equals it. HTML has no such
// attribute, so the server marks that option `selected`: the page must load on it
// (before any script runs) and every update must move the choice. That holds for
// options a component or a child view renders too, which the server marks in the
// page it serves and the client keeps in step.

const wsReady = (page) =>
    page.waitForFunction(() => document.documentElement.classList.contains('az-connected'));

const values = (page) =>
    page.evaluate(() => ({
        lit: document.querySelector('#select-value .lit').value,
        list: document.querySelector('#select-value .list').value,
        stream: document.querySelector('#select-value .stream').value,
        component: document.querySelector('#select-value .component').value,
        child: document.querySelector('#select-value .child').value,
        multi: [...document.querySelector('#select-value .multi').selectedOptions].map(
            (o) => o.value,
        ),
    }));

test.describe('select value', () => {
    test('the page loads on the chosen option without the client', async ({ browser }) => {
        const context = await browser.newContext({ javaScriptEnabled: false });
        const page = await context.newPage();
        await page.goto('/select-value');
        expect(await page.locator('#select-value .lit').inputValue()).toBe('b');
        expect(await page.locator('#select-value .list').inputValue()).toBe('b');
        expect(await page.locator('#select-value .stream').inputValue()).toBe('b');
        expect(await page.locator('#select-value .component').inputValue()).toBe('b');
        expect(await page.locator('#select-value .child').inputValue()).toBe('b');
        await context.close();
    });

    test('the page loads on the chosen options and updates follow', async ({ page }) => {
        await page.goto('/select-value');
        await wsReady(page);
        const all = (v) => ({ lit: v, list: v, stream: v, component: v, child: v });
        expect(await values(page)).toEqual({ ...all('b'), multi: ['a', 'c'] });

        await expectStaysConnected(page, () => page.locator('#select-value .pick-c').click());
        await expect(page.locator('#select-value .lit')).toHaveValue('c');
        expect(await values(page)).toEqual({ ...all('c'), multi: ['a', 'c'] });

        await expectStaysConnected(page, () => page.locator('#select-value .pick-many').click());
        await expect.poll(async () => (await values(page)).multi).toEqual(['b']);
    });

    test('a server change wins over the option the user picked', async ({ page }) => {
        await page.goto('/select-value');
        await wsReady(page);
        // The user moves every select off the server's choice...
        for (const cls of ['lit', 'list', 'stream', 'component', 'child']) {
            await page.locator(`#select-value .${cls}`).selectOption('a');
        }
        // ...then the server moves it to one the user did not pick.
        await expectStaysConnected(page, () => page.locator('#select-value .pick-c').click());
        await expect(page.locator('#select-value .stream')).toHaveValue('c');
        expect(await values(page)).toMatchObject({
            lit: 'c',
            list: 'c',
            stream: 'c',
            component: 'c',
            child: 'c',
        });
    });

    test('an option a child view renders after the value names it is selected', async ({
        page,
    }) => {
        await page.goto('/select-value');
        await wsReady(page);
        // The value names `d`, which the child view does not render yet...
        await expectStaysConnected(page, () => page.locator('#select-value .pick-d').click());
        await expect(page.locator('#select-value .lit')).toHaveValue('a');
        // ...until the parent hands it one more option.
        await expectStaysConnected(page, () => page.locator('#select-value .add-d').click());
        await expect(page.locator('#select-value .child')).toHaveValue('d');
        // And a form reset would return to it: it carries the `selected` attribute.
        expect(
            await page.evaluate(() =>
                [...document.querySelector('#select-value .child').options]
                    .filter((o) => o.defaultSelected)
                    .map((o) => o.value),
            ),
        ).toEqual(['d']);
    });

    // A `?local` value under `multiple`: a whole one holds the list of picks, and in a
    // list each static part is a pick and the local owns its own. The page loads on
    // them, the client reads the value the same way at start, and a `set` moves the
    // picks the local owns, the static ones staying.
    test('a local multiple value loads and moves on the options it names', async ({ page }) => {
        const picked = (cls) =>
            page.evaluate(
                (c) =>
                    [...document.querySelector(`#select-value .${c}`).selectedOptions].map(
                        (o) => o.value,
                    ),
                cls,
            );
        await page.goto('/select-value');
        await wsReady(page);
        expect(await picked('local-many')).toEqual(['a', 'c']);
        expect(await picked('local-tail')).toEqual(['a', 'c']);
        await page.locator('#select-value .set-many').click();
        expect(await picked('local-many')).toEqual(['c', 'ac']);
        await page.locator('#select-value .set-tail').click();
        expect(await picked('local-tail')).toEqual(['a', 'ac']);
    });

    // A `?local` value is the client's even when its initial came from a binding: the
    // page loads on it, and a later server change of that binding leaves the option
    // the user picked alone.
    test('a server change never moves a local value', async ({ page }) => {
        await page.goto('/select-value');
        await wsReady(page);
        const bound = page.locator('#select-value .local-bound');
        await expect(bound).toHaveValue('b');
        await bound.selectOption('a');
        await expectStaysConnected(page, () => page.locator('#select-value .pick-c').click());
        await expect(page.locator('#select-value .lit')).toHaveValue('c');
        await expect(bound).toHaveValue('a');
    });
});
