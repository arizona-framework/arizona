import { expect } from '@playwright/test';

/**
 * Runs `action` and fails if `<html>` ever gets the `az-disconnected`
 * class while it runs.
 *
 * Why this matters: when a server-side handler crashes inside an
 * arizona_live gen_server, the WS closes with code 4500 and
 * `assets/js/arizona.js` reloads the page. Functional tests that only
 * check the destination DOM still pass because SSR re-renders the
 * fresh page. Wrapping a navigation/click/event with this helper
 * surfaces the silent crash by asserting the WebSocket connection
 * survived the action.
 *
 * @param {import('@playwright/test').Page} page
 * @param {() => Promise<void>} action
 */
export async function expectStaysConnected(page, action) {
    const handle = await page.evaluateHandle(() => {
        const state = { count: 0 };
        const obs = new MutationObserver(() => {
            if (document.documentElement.classList.contains('az-disconnected')) {
                state.count += 1;
            }
        });
        obs.observe(document.documentElement, {
            attributes: true,
            attributeFilter: ['class'],
        });
        return state;
    });
    try {
        await action();
        const count = await handle.evaluate((s) => s.count);
        expect(count, 'WebSocket should stay connected during this action').toBe(0);
    } finally {
        await handle.dispose();
    }
}
