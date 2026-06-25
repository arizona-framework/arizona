import { expect, test } from '@playwright/test';

// ---------------------------------------------------------------------------
// Native-shell (OS) capability seam. A fake `window.__arizona_os__` installed
// before the page scripts (the Electron-preload equivalent) drives the real
// arizona.js client end-to-end:
//   - capability negotiation (_az_caps) -> ?capability -> capability-gated UI,
//   - server-emitted + client-triggered OS commands -> the shell's invoke(),
//   - inbound OS events -> the view's handle_event/3.
// In a plain browser (no shell) the OS UI is absent and OS commands no-op.
// ---------------------------------------------------------------------------

const wsReady = (page) =>
    page.waitForFunction(() => document.documentElement.classList.contains('az-connected'));

const setTitleBtn = (page) => page.locator('#os_demo #set-title');
const lastEvent = (page) => page.locator('#os_demo #last-event');
const status = (page) => page.locator('#os_demo #status');

/** Install a fake native-shell bridge before any page script runs. */
function installFakeShell(page) {
    return page.addInitScript(() => {
        /** @type {any} */ (window).__osInvokes = [];
        /** @type {any} */ (window).__arizona_os__ = {
            capabilities: { window_title: true },
            invoke(name, args) {
                /** @type {any} */ (window).__osInvokes.push([name, args]);
                return Promise.resolve();
            },
            onEvent(cb) {
                /** @type {any} */ (window).__osEmit = cb;
            },
        };
    });
}

test.describe('OS seam -- shell present', () => {
    test('negotiates caps, runs server + client OS commands, receives OS events', async ({
        page,
    }) => {
        await installFakeShell(page);
        await page.goto('/os');
        await wsReady(page);

        // Capability negotiated: the gated window-control button repaints in on
        // connect (the ?connected self-cast diff). Absent at SSR, present now.
        await expect(setTitleBtn(page)).toBeVisible();

        // The server re-asserted the title declaratively on connect -- a
        // server-emitted OS effect routed through the real client to invoke().
        await page.waitForFunction(() =>
            /** @type {any} */ (window).__osInvokes.some(
                ([n, a]) => n === 'window_title' && a[0] === 'OS demo (connected)',
            ),
        );

        // A client-triggered OS command: the az_click effect runs in execOne with
        // no server round-trip and reaches invoke().
        await setTitleBtn(page).click();
        await page.waitForFunction(() =>
            /** @type {any} */ (window).__osInvokes.some(
                ([n, a]) => n === 'window_title' && a[0] === 'Clicked title',
            ),
        );

        // An inbound OS event the shell injects reaches handle_event/3 and updates
        // the view through the normal diff.
        await expect(lastEvent(page)).toHaveText('none');
        await page.evaluate(() =>
            /** @type {any} */ (window).__osEmit('window_state', { state: 'blurred' }),
        );
        await expect(lastEvent(page)).toHaveText('blurred');
    });
});

test.describe('OS seam -- plain browser (no shell)', () => {
    test('the OS UI is absent and the view still connects', async ({ page }) => {
        await page.goto('/os');
        await wsReady(page);
        // With no shell, ?capability(window_title) is false, so the button never
        // renders -- and the server-emitted OS effect no-ops harmlessly.
        await expect(setTitleBtn(page)).toHaveCount(0);
        await expect(status(page)).toHaveText('Connected');
    });
});
