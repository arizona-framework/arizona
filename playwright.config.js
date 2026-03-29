// @ts-check
import { defineConfig } from '@playwright/test';

const PORT = process.env.PORT || '4041';
const BASE_URL = `http://localhost:${PORT}`;

export default defineConfig({
  timeout: 15000,
  fullyParallel: true,
  use: {
    baseURL: BASE_URL,
  },
  projects: [
    {
      name: 'parallel',
      testDir: './e2e/parallel',
    },
    {
      name: 'sequential',
      testDir: './e2e/sequential',
      fullyParallel: false,
      workers: 1,
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
