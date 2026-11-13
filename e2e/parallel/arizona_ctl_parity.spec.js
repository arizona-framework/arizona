import { expect, test } from '@playwright/test';
import { serverFrames } from '../utils/helpers.js';

// Server-owned form controls (`az-ctl`, test/support/arizona_ctl_parity.erl): the
// core scenarios of a larger design, ported. A marked control shows the server's value
// once every event carrying an edit is answered; these prove it where jsdom cannot:
// real caret, IME and undo. `#seq` counts the events the server handled, so waiting
// on it waits for a reply to be applied.

const wsReady = (page) =>
    page.waitForFunction(() => document.documentElement.classList.contains('az-connected'));

/** @param {import('@playwright/test').Page} page */
async function open(page) {
    const frames = serverFrames(page);
    await page.goto('/ctl-parity');
    await wsReady(page);
    return frames;
}

/** @param {import('@playwright/test').Page} page */
const seq = async (page) => Number(await page.locator('#seq').textContent());

/**
 * Runs `action` and waits until the server handled `n` more events.
 * @param {import('@playwright/test').Page} page
 * @param {number} n
 * @param {() => Promise<unknown>} action
 */
async function handled(page, n, action) {
    const before = await seq(page);
    await action();
    await expect(page.locator('#seq')).toHaveText(String(before + n));
}

/**
 * Waits until every frame the actions so far provoked is applied: the ping/pong
 * barrier orders the socket, a macrotask later the main thread has applied it.
 * @param {import('@playwright/test').Page} page
 * @param {{settle: () => Promise<void>}} frames
 */
async function quiesce(page, frames) {
    await frames.settle();
    await page.evaluate(() => new Promise((r) => setTimeout(r, 100)));
}

/** @param {import('@playwright/test').Locator} loc */
const caret = (loc) => loc.evaluate((el) => [el.selectionStart, el.selectionEnd]);

/** @param {import('@playwright/test').Page} page @param {string} name */
const checkedRadios = (page, name) =>
    page.evaluate(
        (n) =>
            [...document.querySelectorAll(`input[name="${n}"]`)]
                .filter((r) => r.checked)
                .map((r) => r.id),
        name,
    );

/** @param {import('@playwright/test').Locator} loc */
const selectedValues = (loc) => loc.evaluate((s) => [...s.selectedOptions].map((o) => o.value));

test.describe('server-owned controls', () => {
    test('a refused edit reverts, a normalised one shows, an accepted one keeps the caret', async ({
        page,
    }) => {
        await open(page);

        // Refused: the server keeps "12", so the field shows it again.
        const digits = page.locator('#digits');
        await digits.click();
        await page.keyboard.press('End');
        await handled(page, 1, () => page.keyboard.type('a'));
        await expect(digits).toHaveValue('12');

        // Accepted in the middle of the text: an echo writes nothing, so the caret stays.
        await handled(page, 1, () => page.keyboard.type('3'));
        await expect(digits).toHaveValue('123');
        await page.keyboard.press('Home');
        await page.keyboard.press('ArrowRight');
        await handled(page, 1, () => page.keyboard.type('9'));
        await expect(digits).toHaveValue('1923');
        expect(await caret(digits)).toEqual([2, 2]);

        // Normalised while typing in the middle: the server's value shows, caret kept.
        const upper = page.locator('#upper');
        await upper.click();
        await handled(page, 3, () => page.keyboard.type('abc'));
        await expect(upper).toHaveValue('ABC');
        await page.keyboard.press('Home');
        await page.keyboard.press('ArrowRight');
        await handled(page, 1, () => page.keyboard.type('x'));
        await expect(upper).toHaveValue('AXBC');
        expect(await caret(upper)).toEqual([2, 2]);

        // Normalised to the value the server already has: no diff op, still shown.
        const trimmed = page.locator('#trimmed');
        await trimmed.fill('hello');
        await handled(page, 1, () => trimmed.press('Tab'));
        await expect(trimmed).toHaveValue('HELLO');
        await trimmed.fill('  hello ');
        await handled(page, 1, () => trimmed.press('Tab'));
        await expect(trimmed).toHaveValue('HELLO');
    });

    test('fast debounced typing keeps every character and the caret', async ({ page }) => {
        await open(page);
        const deb = page.locator('#deb');
        await deb.click();
        // Gaps longer than the 40ms debounce send mid-typing, and the server answers
        // each in 80ms, so echoes of older text land while newer text is typed.
        const text = 'the quick brown fox';
        for (const ch of text) {
            await page.keyboard.type(ch);
            await page.waitForTimeout(50);
        }
        await expect(page.locator('#deb-echo')).toHaveText(text);
        await expect(deb).toHaveValue(text);

        // Typing in the middle while replies arrive: the caret never jumps to the end.
        for (let i = 0; i < 9; i++) await page.keyboard.press('ArrowLeft');
        for (const ch of 'very ') {
            await page.keyboard.type(ch);
            await page.waitForTimeout(50);
        }
        const want = 'the quick very brown fox';
        await expect(page.locator('#deb-echo')).toHaveText(want);
        await expect(deb).toHaveValue(want);
        expect(await caret(deb)).toEqual([15, 15]);
    });

    test('a refused checkbox shows the server state', async ({ page }) => {
        await open(page);
        const agree = page.locator('#agree');
        await handled(page, 1, () => agree.click());
        await expect(agree).not.toBeChecked();
    });

    test('radio groups and selects show the server state', async ({ page }) => {
        await open(page);

        // A radio group proposed by its form.
        await handled(page, 1, () => page.locator('#s1-l').click());
        expect(await checkedRadios(page, 'size1')).toEqual(['s1-m']);
        await handled(page, 1, () => page.locator('#s1-s').click());
        expect(await checkedRadios(page, 'size1')).toEqual(['s1-s']);

        // A radio group of a form, two of its radios outside the form element.
        await handled(page, 1, () => page.locator('#s2-c').click());
        expect(await checkedRadios(page, 'size2')).toEqual(['s2-a']);
        await handled(page, 1, () => page.locator('#s2-b').click());
        expect(await checkedRadios(page, 'size2')).toEqual(['s2-b']);

        // A formless radio group.
        await handled(page, 1, () => page.locator('#s3-z').click());
        expect(await checkedRadios(page, 'size3')).toEqual(['s3-x']);
        await handled(page, 1, () => page.locator('#s3-y').click());
        expect(await checkedRadios(page, 'size3')).toEqual(['s3-y']);

        // A select and a select multiple.
        const color = page.locator('#color');
        await handled(page, 1, () => color.selectOption('blue'));
        await expect(color).toHaveValue('red');
        await handled(page, 1, () => color.selectOption('green'));
        await expect(color).toHaveValue('green');

        const multi = page.locator('#multi');
        await handled(page, 1, () => multi.selectOption(['x', 'z']));
        expect(await selectedValues(multi)).toEqual(['x']);
        await handled(page, 1, () => multi.selectOption(['y']));
        expect(await selectedValues(multi)).toEqual(['y']);
    });

    // A single select with no option selected shows its first enabled one, though
    // none is marked: that is the server's state, not an edit.
    test('a select with no option selected shows its first enabled one', async ({ page }) => {
        const frames = await open(page);
        const pick = page.locator('#pick');
        await expect(pick).toHaveValue('one');
        await expect(page.locator('#rpick')).toHaveValue('b');

        // An untouched one is not reset by the reconcile of its form's proposal.
        await page.locator('#pnote').fill('typed');
        await handled(page, 1, () => page.locator('#pnote').press('Tab'));
        await quiesce(page, frames);
        await expect(page.locator('#fpick')).toHaveValue('x');

        // Refused: it shows the first enabled option again, not an empty select.
        await handled(page, 1, () => pick.selectOption('two'));
        await expect(pick).toHaveValue('one');
        expect(await pick.evaluate((s) => s.selectedIndex)).toBe(0);
        await handled(page, 1, () => pick.selectOption('three'));
        await expect(pick).toHaveValue('three');
    });

    test('a textarea the user typed in shows the server update', async ({ page }) => {
        await open(page);
        const notes = page.locator('#notes');
        await notes.fill('typed by the user');
        await handled(page, 1, () => page.locator('#set-notes').click());
        await expect(notes).toHaveValue('From the server');
    });

    test('a field with no event is never reconciled', async ({ page }) => {
        await open(page);
        const quiet = page.locator('#quiet');
        await quiet.fill('typed');
        // Another control's event and reply: nothing proposed #quiet, so it keeps the text.
        await handled(page, 1, () => page.locator('#agree').click());
        await expect(quiet).toHaveValue('typed');
    });

    test('an IME composition is not interrupted and reconciles after it ends', async ({
        page,
        browserName,
    }) => {
        test.skip(browserName !== 'chromium', 'IME is driven through the Chrome DevTools Protocol');
        const frames = await open(page);
        const upper = page.locator('#upper');
        await upper.focus();
        await page.evaluate(() => {
            const log = { ends: 0 };
            window._ime = log;
            document.getElementById('upper').addEventListener('compositionend', () => {
                log.ends++;
            });
        });
        const cdp = await page.context().newCDPSession(page);

        // Mid-composition: the input proposes "ab", the server answers "AB".
        await handled(page, 1, () =>
            cdp.send('Input.imeSetComposition', { text: 'ab', selectionStart: 2, selectionEnd: 2 }),
        );
        await quiesce(page, frames);
        await expect(upper).toHaveValue('ab');
        expect(await page.evaluate(() => window._ime.ends)).toBe(0);

        // Committing ends the composition; then the server's value shows.
        await cdp.send('Input.insertText', { text: 'ab' });
        await expect(upper).toHaveValue('AB');
        expect(await page.evaluate(() => window._ime.ends)).toBe(1);
    });

    test('native undo still undoes the last keystrokes after mid-typing echo replies', async ({
        page,
    }) => {
        await open(page);
        const free = page.locator('#free');
        await free.click();
        await page.keyboard.press('End');
        await handled(page, 3, () => page.keyboard.type('abc', { delay: 30 }));
        await expect(free).toHaveValue('seedabc');
        await page.keyboard.press('ControlOrMeta+z');
        // Browsers group typing into undo steps differently; any of these means the
        // undo stack survived the replies.
        expect(['seed', 'seeda', 'seedab']).toContain(await free.inputValue());
    });
});
