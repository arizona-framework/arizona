import { expect, test } from '@playwright/test';

// A client-owned widget in an az-nodiff element (`arizona_nodiff_widget`, the `Widget`
// hook in `arizona_layout`): the server decides which az-nodiff element the slot holds, the widget
// owns the DOM inside it. The CT suites pin the ops; these assert the client side --
// that a kept az-nodiff element keeps the widget's own state and a replaced one runs the hook's
// lifecycle.

const wsReady = (page) =>
    page.waitForFunction(() => document.documentElement.classList.contains('az-connected'));

const widget = (page) => page.locator('#slot [az-hook="Widget"]');
const input = (page) => page.locator('#slot .widget-input');
const kind = (page) => page.locator('#slot .widget-kind');
const log = (page) => page.evaluate(() => window._widget);

test.beforeEach(async ({ page }) => {
    await page.goto('/nodiff-widget');
    await wsReady(page);
    await expect(kind(page)).toHaveText('line');
});

test('the same branch chosen again keeps the widget and its state', async ({ page }) => {
    await input(page).fill('typed');
    await page.locator('#rerun').click();
    await page.locator('#bump').click();
    await expect(page.locator('#count')).toHaveText('1');
    await expect(input(page)).toHaveValue('typed');
    expect(await log(page)).toEqual({ mounts: 1, destroys: 0, events: ['mount:line'] });
});

test('switching branches destroys the widget before removing it', async ({ page }) => {
    await input(page).fill('typed');
    await page.locator('#bar').click();
    await expect(kind(page)).toHaveText('bar');
    await expect(input(page)).toHaveValue('');
    expect((await log(page)).events).toEqual(['mount:line', 'destroy:line:true', 'mount:bar']);
});

test('hiding and showing the az-nodiff element unmounts and remounts the widget', async ({
    page,
}) => {
    await page.locator('#hide').click();
    await expect(widget(page)).toHaveCount(0);
    await page.locator('#line').click();
    await expect(kind(page)).toHaveText('line');
    expect((await log(page)).events).toEqual(['mount:line', 'destroy:line:true', 'mount:line']);
});

test('two branches calling the same helper are two az-nodiff elements', async ({ page }) => {
    await page.locator('#helper-a').click();
    await expect(kind(page)).toHaveText('helper');
    await input(page).fill('typed');
    await page.locator('#helper-b').click();
    await expect(input(page)).toHaveValue('');
    expect((await log(page)).events).toEqual([
        'mount:line',
        'destroy:line:true',
        'mount:helper',
        'destroy:helper:true',
        'mount:helper',
    ]);
});

test('a new az_key remounts the widget', async ({ page }) => {
    await input(page).fill('typed');
    await page.locator('#rekey').click();
    await expect(input(page)).toHaveValue('');
    expect((await log(page)).events).toEqual(['mount:line', 'destroy:line:true', 'mount:line']);
});

const reconnect = async (page) => {
    await page.evaluate(() => window._ws.close(4000));
    await page.waitForFunction(() =>
        document.documentElement.classList.contains('az-disconnected'),
    );
    await wsReady(page);
};

// A reconnect mounts the view afresh and resyncs it whole; an az-nodiff element the new render
// names again is carried over, so the widget and its client state survive.
test('a reconnect keeps the widget and its state', async ({ page }) => {
    await input(page).fill('typed');
    await reconnect(page);
    await expect(input(page)).toHaveValue('typed');
    expect(await log(page)).toEqual({ mounts: 1, destroys: 0, events: ['mount:line'] });
});

// The fresh mount holds the initial branch again, so an az-nodiff element switched away from
// it is a different az-nodiff element after the reconnect.
test('an az-nodiff element switched before a reconnect is remounted by it', async ({ page }) => {
    await page.locator('#bar').click();
    await expect(kind(page)).toHaveText('bar');
    await input(page).fill('typed');
    await reconnect(page);
    await expect(kind(page)).toHaveText('line');
    await expect(input(page)).toHaveValue('');
    expect((await log(page)).events).toEqual([
        'mount:line',
        'destroy:line:true',
        'mount:bar',
        'destroy:bar:true',
        'mount:line',
    ]);
});

// The same for a new `az_key`: the fresh mount is back on the initial key.
test('an az-nodiff element rekeyed before a reconnect is remounted by it', async ({ page }) => {
    await page.locator('#rekey').click();
    await expect.poll(async () => (await log(page)).mounts).toBe(2);
    await input(page).fill('typed');
    await reconnect(page);
    await expect(input(page)).toHaveValue('');
    expect((await log(page)).events).toEqual([
        'mount:line',
        'destroy:line:true',
        'mount:line',
        'destroy:line:true',
        'mount:line',
    ]);
});
