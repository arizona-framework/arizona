import { expect, test } from '@playwright/test';

// The /unproven view (test/support/arizona_unproven_data.erl) holds static
// values under `az-change`, `az-submit` and `az-click` that parse as command
// lists -- a cross-origin fetch carrying the form's fields, and a push_event --
// under names no template proves. The client must run none of them, whatever
// common DOM event the name collides with.

const wsReady = (page) =>
    page.waitForFunction(() => document.documentElement.classList.contains('az-connected'));

test('runs no command from a data value under an unproven az-* name', async ({ page }) => {
    const errors = [];
    page.on('pageerror', (e) => errors.push(e.message));
    const evil = [];
    await page.route('https://evil.example/**', (route) => {
        evil.push(route.request().postData());
        return route.fulfill({ status: 200, contentType: 'application/json', body: '{}' });
    });
    await page.goto('/unproven');

    // The page carries what it proves, so the client knows it before the socket opens.
    // It is the body's last child, written before the `</body>` ending the page: the client
    // reads the last JSON script carrying that id.
    const embed = page.locator('script[type="application/json"]#arizona-attrs');
    expect(
        await embed.evaluate((el) => el.parentElement === document.body && !el.nextElementSibling),
    ).toBe(true);
    // In the bytes served, not only after the parser's error recovery: the document
    // ends with the embed inside the body, so nothing follows `</html>`.
    const served = await (await page.request.get('/unproven')).text();
    expect(served).toMatch(
        /<script type="application\/json" id="arizona-attrs">[^<]*<\/script><\/body><\/html>$/,
    );
    const names = JSON.parse(await embed.textContent());
    expect(names).toContain('az-dblclick');
    for (const name of ['az-change', 'az-submit', 'az-click']) expect(names).not.toContain(name);

    await wsReady(page);
    await page.click('#go');
    await page.fill('#secret', 'typed');
    await page.locator('#secret').press('Tab'); // commits the edit: a real `change`
    await page.click('#send');
    // Frames are handled in order, so anything sent above has landed before this.
    await page.dblclick('#ping');
    await expect(page.locator('#pings')).toHaveText('1');

    await expect(page.locator('#hits')).toHaveText('0');
    expect(evil).toEqual([]);
    // Still never submitted natively.
    expect(new URL(page.url()).pathname).toBe('/unproven');
    expect(new URL(page.url()).search).toBe('');
    expect(errors).toEqual([]);
});
