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

// Arizona's system frames: the client sends `0`, the server answers `1`.
const PING = '0';
const PONG = '1';

/**
 * Records every frame the server sends on the page's WebSocket.
 *
 * Call this BEFORE `page.goto`: Playwright emits `websocket` when the socket
 * opens, and Arizona opens it from a Worker while the page boots.
 *
 * `expectNone` is what makes a "nothing was sent to the server" assertion
 * deterministic. It pings and waits for the answering pong: WebSocket frames
 * are ordered, so any frame the preceding actions provoked has already arrived
 * by the time the pong does. A fixed sleep can only guess at that, and the
 * client cannot supply the barrier itself -- the Worker swallows pongs, so the
 * page never sees one.
 *
 * @param {import('@playwright/test').Page} page
 */
export function serverFrames(page) {
    /** @type {string[]} */
    const frames = [];
    page.on('websocket', (ws) => {
        ws.on('framereceived', (f) => frames.push(String(f.payload)));
    });
    const pongs = () => frames.filter((f) => f === PONG).length;

    /** Pings and waits for the answering pong -- the ordering barrier. */
    async function settle() {
        const before = pongs();
        await page.evaluate((ping) => {
            /** @type {any} */ (window)._ws.send(ping);
        }, PING);
        await expect.poll(pongs).toBeGreaterThan(before);
    }

    return {
        settle,
        /**
         * Settles, then fails if the server sent anything but pongs.
         *
         * @param {string} message
         */
        async expectNone(message) {
            await settle();
            expect(
                frames.filter((f) => f !== PONG),
                message,
            ).toEqual([]);
        },
    };
}
