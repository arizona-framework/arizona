import { expect, test } from '@playwright/test';

// The /events view (test/support/arizona_events_demo.erl) proves the whole
// compile-time path over a real socket: the parse transform records `az-toggle`,
// the server ships the set on the connect frame, and the client delegates it.
// `toggle` is not bootstrapped and does not bubble, so it needs both the set and
// the capture-phase listener -- a regression in either leaves the page inert.

const wsReady = (page) =>
    page.waitForFunction(() => document.documentElement.classList.contains('az-connected'));

test('delivers a non-bubbling event the server declared at connect', async ({ page }) => {
    const errors = [];
    page.on('pageerror', (e) => errors.push(e.message));
    await page.goto('/events');
    await wsReady(page);
    await expect(page.locator('#toggles')).toHaveText('0');

    // A real, browser-generated toggle from clicking the summary.
    await page.click('#sum');
    await expect(page.locator('#toggles')).toHaveText('1');
    await page.click('#sum');
    await expect(page.locator('#toggles')).toHaveText('2');

    expect(errors).toEqual([]);
});

test('never delegates an az-* name that only carries app data', async ({ page }) => {
    const errors = [];
    page.on('pageerror', (e) => errors.push(e.message));
    // The containment would turn a wrong delegation into a console warning, not
    // a pageerror -- so the warning stream is the assertion that carries this
    // test's failure mode.
    const warnings = [];
    page.on('console', (m) => {
        if (m.text().includes('[arizona]')) warnings.push(m.text());
    });
    await page.goto('/events');
    await wsReady(page);

    // az-select="[1,2,3]" is a static value, so the transform does not record it.
    // Were it delegated, this dispatch would parse [1,2,3] as a command -- opcode 1
    // is toggle, selector 2 -- and crash the interpreter.
    await page.evaluate(() =>
        document.getElementById('data').dispatchEvent(new Event('select', { bubbles: true })),
    );
    await page.waitForTimeout(150);
    expect(errors).toEqual([]);
    expect(warnings).toEqual([]);
});

// The /opaque-events view (test/support/arizona_opaque_events.erl) proves the
// render-time path: its commands are opaque `?get` dynamics the transform cannot
// record, so the names reach the client only as observations -- `az-mouseenter`
// on the connect frame (proved by the connect mount's render), `az-dblclick` as
// the delta on the frame whose branch first renders it.

test('delivers an event proved only by the render, at connect', async ({ page }) => {
    const errors = [];
    page.on('pageerror', (e) => errors.push(e.message));
    await page.goto('/opaque-events');
    await wsReady(page);
    await expect(page.locator('#entered')).toHaveText('0');

    // A real hover: mouseenter is not bootstrapped, so it fires only because the
    // connect frame carried the render-observed name.
    await page.hover('#enter');
    await expect(page.locator('#entered')).toHaveText('1');
    expect(errors).toEqual([]);
});

test('never delegates dynamic app data, even shaped like a command', async ({ page }) => {
    const errors = [];
    page.on('pageerror', (e) => errors.push(e.message));
    // As above: a wrong delegation surfaces as a contained console warning, so
    // assert the warning stream too.
    const warnings = [];
    page.on('console', (m) => {
        if (m.text().includes('[arizona]')) warnings.push(m.text());
    });
    await page.goto('/opaque-events');
    await wsReady(page);

    // {az_select, ?get(ids)} renders az-select="[1,2,3]" -- app data the compile
    // step cannot classify. The render proved it is NOT a command, so `select` is
    // never delegated; were it, this dispatch would execute [1,2,3] (toggle,
    // selector 2) and crash the interpreter.
    await page.evaluate(() =>
        document.getElementById('data').dispatchEvent(new Event('select', { bubbles: true })),
    );
    await page.waitForTimeout(150);
    expect(errors).toEqual([]);
    expect(warnings).toEqual([]);
});

test('delivers an event proved by a later frame, via its delta', async ({ page }) => {
    const errors = [];
    page.on('pageerror', (e) => errors.push(e.message));
    await page.goto('/opaque-events');
    await wsReady(page);

    // The branch declaring az-dblclick renders on the reveal reply; the same
    // frame carries the name, so the type is bound before anything can fire it.
    await page.click('#reveal');
    await expect(page.locator('#late')).toBeVisible();
    await page.dblclick('#late');
    await expect(page.locator('#doubled')).toHaveText('1');
    expect(errors).toEqual([]);
});

// The /dyn-page view (test/support/arizona_dyn_page.erl) is the documented
// `?stateful(?get(page), ...)` idiom: the module is data, so the compile-time
// walk cannot follow it -- instantiation observes it, and a swapped-in page's
// names arrive on the swap's own reply.

test('a runtime-bound page swap delivers the new page events', async ({ page }) => {
    const errors = [];
    page.on('pageerror', (e) => errors.push(e.message));
    await page.goto('/dyn-page');
    await wsReady(page);

    await page.click('#swap');
    await expect(page.locator('#pb')).toHaveText('page b:0');
    // pointerup is not bootstrapped; the tap counts only because the swap
    // reply's delta carried page b's names.
    await page.locator('#pb').dispatchEvent('pointerup', { bubbles: true });
    await expect(page.locator('#pb')).toHaveText('page b:1');
    expect(errors).toEqual([]);
});
