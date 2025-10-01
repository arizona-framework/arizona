import { defineConfig } from 'vitest/config';

export default defineConfig({
  test: {
    include: ['assets/js/__tests__/unit/**/*.test.js'],
    exclude: ['_build/**', '.github/**'],
    coverage: {
      provider: 'v8',
      reportsDirectory: '_build/test/cover/js',
      include: ['assets/js/client/**/*.js'],
      exclude: ['assets/js/__tests__/**'],
      thresholds: {
        statements: 60,
        branches: 80,
        functions: 45,
        lines: 60,
      },
    },
  },
});
