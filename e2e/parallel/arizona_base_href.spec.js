import { expect, test } from '@playwright/test';

// ---------------------------------------------------------------------------
// A relative navigate/patch effect under `<base href>`. The browser resolves the
// history URL against the document base, so the frame must ask the server for
// that same page -- not the one the URL names relative to the current path.
// The command is a click attribute (`click` is proven by the page's own button),
// added with a `<base>` once the page is live.
// ---------------------------------------------------------------------------

const wsReady = (page) =>
    page.waitForFunction(() => document.documentElement.classList.contains('az-connected'));

/** Give the page `<base href=base>` (none for null) and a button running `cmd` on click. */
const addRelativeNav = (page, base, cmd) =>
    page.evaluate(
        ([href, json]) => {
            if (href !== null) {
                const b = document.createElement('base');
                b.setAttribute('href', href);
                document.head.appendChild(b);
            }
            const btn = document.createElement('button');
            btn.id = 'rel-nav';
            btn.setAttribute('az-click', json);
            document.querySelector('nav').appendChild(btn);
        },
        [base, JSON.stringify(cmd)],
    );

test('a relative navigate effect renders the page the address bar shows', async ({ page }) => {
    await page.goto('/patch-demo/overview');
    await wsReady(page);
    await addRelativeNav(page, '/', [10, 'scroll-home']);
    await page.locator('#rel-nav').click();
    await expect(page).toHaveURL(/\/scroll-home$/);
    await expect(page.locator('#page > h1')).toHaveText('Scroll Home');
});

test('a relative patch effect patches to the page the address bar shows', async ({ page }) => {
    await page.goto('/patch-demo/overview');
    await wsReady(page);
    await page.locator('#bump').click();
    await expect(page.locator('#clicks')).toHaveText('1');
    await addRelativeNav(page, '/patch-demo/x/', [24, '../projects']);
    await page.locator('#rel-nav').click();
    await expect(page).toHaveURL(/\/patch-demo\/projects$/);
    await expect(page.locator('#content')).toHaveText('projects');
    // Patched in place: the live view, and its counter, survived.
    await expect(page.locator('#clicks')).toHaveText('1');
});

// A controller's `reply_redirect(~"scroll-home")` answers `/fetch-redirect`: the
// relative `Location` names `/scroll-home`, resolved against the endpoint's URL as
// an HTTP 3xx would be -- not against the page that posted (`/patch-demo/`) or
// its `<base>`.
for (const base of [null, '/sub/']) {
    test(`a relative reply_redirect lands where HTTP would, ${base ? `under ${base}` : 'with no base'}`, async ({
        page,
    }) => {
        await page.goto('/patch-demo/overview');
        await wsReady(page);
        await addRelativeNav(page, base, [22, '/fetch-redirect', {}]);
        await page.locator('#rel-nav').click();
        await expect(page).toHaveURL((u) => u.pathname === '/scroll-home');
        await expect(page.locator('#page > h1')).toHaveText('Scroll Home');
    });
}

// A response's SPA navigate/patch is a command of the page, resolving against the
// document as on the socket: only a full navigate is a `Location`.
test('a fragment navigate in a fetch response scrolls the page, not the endpoint', async ({
    page,
}) => {
    await page.goto('/patch-demo/overview');
    await wsReady(page);
    await addRelativeNav(page, null, [22, '/fetch-effects/hash', {}]);
    await page.locator('#rel-nav').click();
    await expect(page).toHaveURL(
        (u) => u.pathname === '/patch-demo/overview' && u.hash === '#hello',
    );
    await expect(page.locator('#content')).toHaveText('overview');
    await expect(page.locator('#rel-nav')).toBeAttached();
});

test('a query patch in a fetch response patches the page, not the endpoint', async ({ page }) => {
    await page.goto('/patch-demo/overview');
    await wsReady(page);
    await page.locator('#bump').click();
    await expect(page.locator('#clicks')).toHaveText('1');
    await addRelativeNav(page, null, [22, '/fetch-effects/patch', {}]);
    await page.locator('#rel-nav').click();
    await expect(page).toHaveURL(
        (u) => u.pathname === '/patch-demo/overview' && u.search === '?tab=2',
    );
    await expect(page.locator('#content')).toHaveText('overview');
    await expect(page.locator('#clicks')).toHaveText('1');
});

// A relative navigate under `<base href>` to a view under other layouts: the server
// answers the frame with the full load of the page the base resolved it to (from
// `/patch-demo/overview`, `crash-nav-twin` names `/crash-nav-twin` only under the
// base), which is that navigation's own page, so the entry it pushed reloads,
// keeping its state.
test('a relative navigate answered with a load reloads the entry it pushed', async ({ page }) => {
    await page.goto('/patch-demo/overview');
    await wsReady(page);
    await addRelativeNav(page, '/', [10, 'crash-nav-twin']);
    const loaded = page.waitForEvent('load', { timeout: 15000 });
    await page.locator('#rel-nav').click();
    await loaded;
    await expect(page.locator('section.scroll-section #crash_nav')).toHaveCount(1);
    expect(new URL(page.url()).pathname).toBe('/crash-nav-twin');
    expect(await page.evaluate(() => performance.getEntriesByType('navigation')[0].type)).toBe(
        'reload',
    );
    expect(typeof (await page.evaluate(() => history.state?._azIdx))).toBe('number');
});
