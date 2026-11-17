import { expect, test } from '@playwright/test';

const wsReady = (page) =>
    page.waitForFunction(() => document.documentElement.classList.contains('az-connected'));

/** Stop the listener for `ms`, and make sure this page's socket goes down with it. */
async function outage(page, request, ms) {
    expect((await request.get(`/_test/outage?ms=${ms}`)).status()).toBe(204);
    await page.waitForTimeout(500);
    await page.evaluate(() => window._ws.close(4000));
    await page.waitForFunction(() =>
        document.documentElement.classList.contains('az-disconnected'),
    );
}

// Sequential: the outage drops every connection on the server.

// With nothing pending, the page waits out the outage and resyncs in place once
// the server is back: no page load, the typed text kept.
test('an outage resyncs the page in place once the server restarts', async ({ page, request }) => {
    test.setTimeout(45000);
    await page.goto('/ctl-parity');
    await wsReady(page);
    await page.fill('#runbound', 'typed-by-user');
    let loads = 0;
    page.on('load', () => loads++);
    await outage(page, request, 6000);
    await page.waitForTimeout(3000);
    expect(loads).toBe(0);
    await wsReady(page);
    await expect(page.locator('#runbound')).toHaveValue('typed-by-user');
    expect(loads).toBe(0);
});

// A navigation pended during the outage waits for the attempt in flight; once an
// attempt started after it fails, it runs as with no socket -- a load of its URL,
// as a link the user follows while disconnected.
test('a navigate during an outage loads its page once a reconnect attempt fails', async ({
    page,
    request,
}) => {
    test.setTimeout(45000);
    await page.goto('/');
    await wsReady(page);
    await outage(page, request, 8000);
    const load = page.waitForRequest(
        (r) => r.resourceType() === 'document' && new URL(r.url()).pathname === '/about',
        { timeout: 15000 },
    );
    await page.evaluate(async () => {
        const mod = await import('/priv/arizona.min.js');
        mod.applyEffects([[10, '/about', {}]]);
    });
    await load;
    // Leave the server up for whatever runs next.
    await expect
        .poll(async () => (await request.get('/').catch(() => null))?.status(), {
            timeout: 20000,
        })
        .toBe(200);
});

// The same to page#fragment: the load is of the address bar's own URL with its
// fragment, which `location.replace` would turn into a same-document scroll. The
// entry this client pushed is reloaded instead, and lands at its fragment.
test('a navigate during an outage to page#fragment loads its page too', async ({
    page,
    request,
}) => {
    test.setTimeout(45000);
    await page.goto('/');
    await wsReady(page);
    await outage(page, request, 8000);
    const load = page.waitForRequest(
        (r) => r.resourceType() === 'document' && new URL(r.url()).pathname === '/about',
        { timeout: 15000 },
    );
    await page.evaluate(async () => {
        const mod = await import('/priv/arizona.min.js');
        mod.applyEffects([[10, '/about#team', {}]]);
    });
    await load;
    await expect.poll(() => new URL(page.url()).hash).toBe('#team');
    // Leave the server up for whatever runs next.
    await expect
        .poll(async () => (await request.get('/').catch(() => null))?.status(), {
            timeout: 20000,
        })
        .toBe(200);
});
