import { defineConfig } from 'vitest/config';

export default defineConfig({
  test: {
    include: ['assets/**/*.test.js'],
    exclude: ['_build/**', '.github/**'],
    coverage: {
      provider: 'v8',
      reportsDirectory: '_build/test/cover/js',
      include: ['assets/**/*.mjs'],
      exclude: ['assets/**/*.test.js', 'assets/**/__mocks__/**'],
    },
  },
});
