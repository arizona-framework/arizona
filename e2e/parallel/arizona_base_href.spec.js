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

/** Give the page `<base href=base>` and a button running `cmd` on click. */
const addRelativeNav = (page, base, cmd) =>
    page.evaluate(
        ([href, json]) => {
            const b = document.createElement('base');
            b.setAttribute('href', href);
            document.head.appendChild(b);
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
