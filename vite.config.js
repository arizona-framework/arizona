import { defineConfig } from 'vite';
import { resolve } from 'node:path';
import { visualizer } from 'rollup-plugin-visualizer';
import dts from 'vite-plugin-dts';
import tsconfigPaths from 'vite-tsconfig-paths';
import license from 'rollup-plugin-license';
import eslint from 'vite-plugin-eslint';
import filesize from 'rollup-plugin-filesize';
import banner from 'rollup-plugin-banner2';

export default defineConfig(({ mode }) => {
  const isDev = mode === 'development';
  const isAnalyze = process.env.ANALYZE === 'true';

  return {
    plugins: [
      // ESLint integration
      eslint({
        include: ['assets/**/*.js'],
        exclude: ['node_modules/**'],
      }),

      tsconfigPaths(),

      // TypeScript declarations from JSDoc
      dts({
        include: [
          'assets/js/client/index.js',
          'assets/js/client/arizona.js',
          'assets/js/client/logger/index.js',
          'assets/js/client/logger/arizona-logger.js',
          'assets/js/client/logger/arizona-console-logger.js',
        ],
        outDir: 'priv/static/assets/types',
        rollupTypes: false,
      }),
    ],

    worker: {
      rollupOptions: {
        output: {
          entryFileNames: '[name].min.js',
          chunkFileNames: 'chunks/[name].min.js',
          assetFileNames: 'assets/[name].min.js',
        },
      },
    },

    build: {
      lib: {
        entry: {
          index: 'assets/js/client/index.js',
          arizona: 'assets/js/client/arizona.js',
          'logger/index': 'assets/js/client/logger/index.js',
          'logger/arizona-logger': 'assets/js/client/logger/arizona-logger.js',
          'logger/arizona-console-logger': 'assets/js/client/logger/arizona-console-logger.js',
        },
        formats: ['es'],
      },
      outDir: 'priv/static/assets/js',
      minify: !isDev,
      sourcemap: true,
      reportCompressedSize: !isDev,
      emptyOutDir: false,
      rollupOptions: {
        output: {
          format: 'es',
          entryFileNames: '[name].min.js',
          chunkFileNames: 'chunks/[name].min.js',
          assetFileNames: 'assets/[name].min.js',
        },
        plugins: [
          // License management
          license({
            sourcemap: true,
            banner: {
              commentStyle: 'regular',
              content: {
                file: resolve('LICENSE.md'),
                encoding: 'utf-8',
              },
            },
            thirdParty: {
              output: resolve('priv/static/assets/js/LICENSES.txt'),
              includePrivate: false,
            },
          }),

          // Banner with version info
          banner(() => {
            const pkg = require('./package.json');
            return `/*! Arizona Framework Client v${pkg.version} | ${pkg.license} License */`;
          }),

          // File size monitoring
          filesize({
            showMinifiedSize: false,
            showGzippedSize: true,
          }),

          // Bundle analysis (only when ANALYZE=true)
          ...(isAnalyze
            ? [
                visualizer({
                  filename: 'build-analysis.html',
                  open: true,
                  gzipSize: true,
                  brotliSize: true,
                }),
              ]
            : []),
        ],
      },
    },
  };
});
