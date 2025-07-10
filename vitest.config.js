import { defineConfig } from 'vitest/config';

export default defineConfig({
  test: {
    include: ['assets/**/*.test.js'],
    exclude: ['_build/**', '.github/**'],
    coverage: {
      provider: 'v8',
      reportsDirectory: '_build/test/cover/js',
      include: ['assets/**/*.js'],
      exclude: ['assets/**/*.test.js', 'assets/**/__mocks__/**', 'assets/**/e2e/**'],
      thresholds: {
        statements: 60,
        branches: 80,
        functions: 45,
        lines: 60,
      },
    },
  },
});
