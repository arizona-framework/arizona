// @ts-check
import { defineConfig } from '@playwright/test';

const PORT = process.env.PORT || '4041';
const BASE_URL = `http://localhost:${PORT}`;

export default defineConfig({
    timeout: 15000,
    fullyParallel: true,
    // One retry on CI, none locally. A retried test is reported as `flaky`
    // rather than passing silently, so a timing-dependent failure still shows
    // up -- it just doesn't fail the whole job on a single bad roll.
    retries: process.env.CI ? 1 : 0,
    use: {
        baseURL: BASE_URL,
        // The CI job uploads `test-results/` when the e2e step fails, which was
        // empty without these: a flake failed the job with nothing to look at.
        // The trace rides the retry (the run that already knows it is suspect),
        // the screenshot is cheap enough to take on every failure.
        trace: 'on-first-retry',
        screenshot: 'only-on-failure',
    },
    projects: [
        {
            name: 'parallel',
            testDir: './e2e/parallel',
        },
        {
            // One worker so the drain spec cannot overlap the others: it soft-drains
            // the whole listener, which stops every live process on the server and
            // forces every open page to remount. Isolation of per-test *state* is
            // not this project's job -- `arizona_chat` scopes its pubsub channel to
            // the room in the URL, so serializing is not what keeps it clean.
            name: 'sequential',
            testDir: './e2e/sequential',
            fullyParallel: false,
            workers: 1,
        },
        {
            // Native (JSON) wire e2e: a real WebSocket client (no browser),
            // exercising the ?native render target end-to-end.
            name: 'native',
            testDir: './e2e/native',
        },
    ],
    webServer: {
        command: './scripts/start_test_server.sh',
        url: BASE_URL,
        reuseExistingServer: !process.env.CI,
        stdout: 'pipe',
        stderr: 'pipe',
        timeout: 30000,
        env: {
            ERLANG_EXTRA_ARGS: '-noshell',
            PORT,
        },
    },
});
