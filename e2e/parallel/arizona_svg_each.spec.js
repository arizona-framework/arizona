import { expect, test } from '@playwright/test';

// An `?each` inside `<svg>` that is empty at mount and filled by a patch.
//
// A detached `<template>` parses in HTML context, so a `<rect>` the diff created
// came out an `HTMLUnknownElement` in the XHTML namespace: every attribute correct,
// nothing rendered. SSR never shows it (the page parser namespace-adjusts inside
// `<svg>`), so it only appears for content absent at first paint -- which reads as
// bad data rather than a DOM bug.
//
// `arizona.test.js` pins the namespace the client creates. These assert the half
// jsdom cannot: that the node actually lays out. An unknown element has no `getBBox`
// and a 0x0 box even with correct `x`/`width`/`height`.

const wsReady = (page) =>
    page.waitForFunction(() => document.documentElement.classList.contains('az-connected'));

const bars = (page) => page.locator('#bars > *');

test.beforeEach(async ({ page }) => {
    await page.goto('/svg-each');
    await wsReady(page);
});

test('the each starts empty, so every bar is created by the diff', async ({ page }) => {
    await expect(bars(page)).toHaveCount(0);
});

test('a patched-in SVG child is created in the SVG namespace and lays out', async ({ page }) => {
    await page.locator('#add').click();
    await expect(bars(page)).toHaveCount(1);

    const probe = await page.evaluate(() => {
        const rect = document.querySelector('#bars > *');
        const box = rect.getBoundingClientRect();
        return {
            ns: rect.namespaceURI,
            ctor: rect.constructor.name,
            hasGetBBox: typeof rect.getBBox === 'function',
            width: box.width,
            height: box.height,
        };
    });

    expect(probe.ns).toBe('http://www.w3.org/2000/svg');
    // The constructor is the tell the namespace alone can hide: an HTML-namespaced
    // `rect` is an HTMLUnknownElement.
    expect(probe.ctor).not.toBe('HTMLUnknownElement');
    expect(probe.hasGetBBox).toBe(true);
    // The actual symptom: a 0x0 box despite correct geometry attributes.
    expect(probe.width).toBeGreaterThan(0);
    expect(probe.height).toBeGreaterThan(0);
});

test('an SVG <title> in a stateless child updates on a patch', async ({ page }) => {
    // The child is a separate template with no call site, so it classifies `title`
    // as HTML raw text and its slot comes out markerless -- no marker to patch. The
    // update has to arrive by re-rendering the child whole against its own slot.
    // An SVG `<title>` is the accessible name, so a frozen one shows nothing on
    // screen; only reading the text catches it.
    const caption = () => page.evaluate(() => document.querySelector('#chart > title').textContent);

    expect(await caption()).toBe('Chart');
    await page.locator('#rename').click();
    await expect.poll(caption).toBe('Renamed');
});

test('further inserts keep the namespace', async ({ page }) => {
    await page.locator('#add').click();
    await page.locator('#add').click();
    await expect(bars(page)).toHaveCount(2);
    const namespaces = await page.evaluate(() =>
        Array.from(document.querySelectorAll('#bars > *')).map((e) => e.namespaceURI),
    );
    expect(namespaces).toEqual(['http://www.w3.org/2000/svg', 'http://www.w3.org/2000/svg']);
});
