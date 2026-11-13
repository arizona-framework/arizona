import { expect, test } from '@playwright/test';

// ---------------------------------------------------------------------------
// Document Picture-in-Picture in a real Chromium: a popped-out view keeps being
// patched in its window, and what comes back inline when the window closes is
// what the server rendered last, never a stale copy.
// ---------------------------------------------------------------------------

const wsReady = (page) =>
    page.waitForFunction(() => document.documentElement.classList.contains('az-connected'));

/** Wait until the PiP window holds an element with `id`. */
const inPip = (page, id) =>
    page.waitForFunction((id) => {
        const pip = /** @type {any} */ (window).documentPictureInPicture.window;
        return !!pip?.document.getElementById(id);
    }, id);

const pipOpen = (page) =>
    page.evaluate(() => /** @type {any} */ (window).documentPictureInPicture.window !== null);

test('a navigate of a popped-out root comes back inline as the new page', async ({ page }) => {
    await page.goto('/pip');
    await wsReady(page);
    await page.locator('#pip_demo').getByRole('button', { name: 'Pop out', exact: true }).click();
    await inPip(page, 'pip_demo');

    // The root is resolved in the window, so the new page lands there.
    await page.getByRole('link', { name: 'About' }).click();
    await inPip(page, 'about-page');

    await page.evaluate(() => /** @type {any} */ (window).documentPictureInPicture.window.close());

    await expect(page.locator('#about-page')).toBeVisible();
    await expect(page.locator('#pip_demo')).toHaveCount(0);
});

const pipText = (page, selector) =>
    page.evaluate((selector) => {
        const pip = /** @type {any} */ (window).documentPictureInPicture.window;
        return pip?.document.querySelector(selector)?.textContent ?? null;
    }, selector);

/** Has the popped-out child's tick moved on from `before`, in the window? */
const ticked = async (page, before) => {
    const now = await pipText(page, '#pip_child .pip-count');
    return now !== null && now !== before;
};

/** Type into the popped-out child's widget, in the window. */
const typeInWidget = (page, value) =>
    page.evaluate((value) => {
        const pip = /** @type {any} */ (window).documentPictureInPicture.window;
        pip.document.querySelector('#pip_child .widget-input').value = value;
    }, value);

/** The widget input's value in the window, and the Widget hook's mount log. */
const widgetState = (page) =>
    page.evaluate(() => {
        const pip = /** @type {any} */ (window).documentPictureInPicture.window;
        const { mounts, destroys } = /** @type {any} */ (window)._widget;
        return {
            value: pip?.document.querySelector('#pip_child .widget-input')?.value ?? null,
            mounts,
            destroys,
        };
    });

const exitPip = (page, id) =>
    page.evaluate(async (id) => (await import('/priv/arizona.min.js')).exitPip(id), id);

test('exit_pip from the page a navigate swapped into the window closes it', async ({ page }) => {
    await page.goto('/pip');
    await wsReady(page);
    await page.locator('#pip_demo').getByRole('button', { name: 'Pop out', exact: true }).click();
    await inPip(page, 'pip_demo');
    await page.getByRole('link', { name: 'About' }).click();
    await inPip(page, 'about-page');

    await exitPip(page, 'about-page');

    await expect.poll(() => pipOpen(page)).toBe(false);
    await expect(page.locator('#about-page')).toBeVisible();
});

test('a parent re-render keeps a popped-out child in its window', async ({ page }) => {
    await page.goto('/pip-parent');
    await wsReady(page);
    await page.locator('#pip_child').getByRole('button', { name: 'Pop out', exact: true }).click();
    await inPip(page, 'pip_child');
    await typeInWidget(page, 'typed');

    // The swap re-renders the child's slot wholesale, placeholder included: the
    // fresh child moves into the window.
    await page.locator('#swap').click();
    await page.waitForFunction(() => !!document.querySelector('#pip_parent article'));

    expect(await pipOpen(page)).toBe(true);
    await expect(page.locator('#pip_child')).toHaveCount(0);
    // Its az-nodiff widget came along, never remounted.
    expect(await widgetState(page)).toEqual({ value: 'typed', mounts: 1, destroys: 0 });
    // Still live in the window: the server's ticks keep reaching it.
    const before = await pipText(page, '#pip_child .pip-count');
    expect(before).toMatch(/^Tick: \d+$/);
    await expect.poll(() => ticked(page, before)).toBe(true);

    // Closing brings it back into the new wrapper.
    await exitPip(page, 'pip_child');
    await expect(page.locator('#pip_parent article #pip_child')).toBeVisible();
});

test('a reconnect keeps a popped-out child in its window', async ({ page }) => {
    await page.goto('/pip-parent');
    await wsReady(page);
    await page.locator('#pip_child').getByRole('button', { name: 'Pop out', exact: true }).click();
    await inPip(page, 'pip_child');
    await typeInWidget(page, 'typed');

    // Mark the root the resync's OP_REPLACE will swap out, to wait for it.
    await page.evaluate(() => {
        /** @type {any} */ (document.getElementById('pip_parent'))._stale = true;
        /** @type {any} */ (window)._ws.close(4000);
    });
    await page.waitForFunction(() => {
        const root = /** @type {any} */ (document.getElementById('pip_parent'));
        return root && !root._stale;
    });

    expect(await pipOpen(page)).toBe(true);
    await expect(page.locator('#pip_child')).toHaveCount(0);
    expect(await widgetState(page)).toEqual({ value: 'typed', mounts: 1, destroys: 0 });
    // The remounted child keeps ticking in the window.
    const before = await pipText(page, '#pip_child .pip-count');
    expect(before).toMatch(/^Tick: \d+$/);
    await expect.poll(() => ticked(page, before)).toBe(true);
});

/**
 * The entries of the `FormData` the client builds when form `#pf` submits (a file as
 * `[name, file name, type, size]`).
 */
const submitted = (page) =>
    page.evaluate(() => {
        // Keep each FormData built from a form, to read what the submit posts.
        /** @type {FormData[]} */
        const built = [];
        const Native = window.FormData;
        window.FormData = /** @type {any} */ (
            class extends Native {
                constructor(/** @type {any} */ f, /** @type {any} */ s) {
                    super(f, s);
                    if (f?.id === 'pf') built.push(this);
                }
            }
        );
        try {
            /** @type {any} */ (document.getElementById('pf')).requestSubmit();
        } finally {
            window.FormData = Native;
        }
        return [...(built.at(-1)?.entries() ?? [])].map(([k, v]) =>
            typeof v === 'string' ? [k, v] : [k, v.name, v.type, v.size],
        );
    });

const popOutFields = async (page) => {
    await page.goto('/pip-form');
    await wsReady(page);
    await page.locator('#pip_file').getByRole('button', { name: 'Pop out', exact: true }).click();
    await inPip(page, 'pip_file');
};

// A file chosen in a popped-out view's file input is the form's: its submit posts the
// file itself, built by the browser's own FormData as if the view were inline.
test('a form posts the file chosen in a popped-out view', async ({ page }) => {
    await popOutFields(page);
    await page.evaluate(() => {
        const pip = /** @type {any} */ (window).documentPictureInPicture.window;
        const dt = new DataTransfer();
        dt.items.add(new File(['hello'], 'note.txt', { type: 'text/plain' }));
        pip.document.querySelector('#pip_file [name=up]').files = dt.files;
    });
    const entries = await submitted(page);
    expect(entries.find(([k]) => k === 'up')).toEqual(['up', 'note.txt', 'text/plain', 5]);
});

// A popped-out textarea wrapping hard posts the lines it would inline -- its width
// and font, laid out -- even where its view's place inline has no box (a closed
// `details`): the browser wraps by the lines it lays out.
for (const closed of [false, true])
    test(`a form posts a popped-out textarea's hard wrap${closed ? ' from a closed details' : ''}`, async ({
        page,
    }) => {
        await popOutFields(page);
        if (closed)
            await page.evaluate(() => {
                /** @type {any} */ (document.getElementById('pd')).open = false;
            });
        const away = await submitted(page);
        await exitPip(page, 'pip_file');
        await expect(page.locator('#pd #pip_file')).toHaveCount(1);
        await page.evaluate(() => {
            /** @type {any} */ (document.getElementById('pd')).open = true;
        });
        const inline = await submitted(page);
        const ta = inline.find(([k]) => k === 'ta');
        expect(ta?.[1]).toContain('\n');
        expect(away).toEqual(inline);
    });
