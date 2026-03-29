import { resolve } from 'node:path';
import { createRequire } from 'node:module';
import { defineConfig, createLogger } from 'vite';
import license from 'rollup-plugin-license';
import filesize from 'rollup-plugin-filesize';
import { visualizer } from 'rollup-plugin-visualizer';

const require = createRequire(import.meta.url);
const pkg = require('./package.json');

const logger = createLogger();
for (const method of ['warn', 'warnOnce']) {
    const original = logger[method].bind(logger);
    logger[method] = (msg, options) => {
        original(msg, options);
        process.exitCode = 1;
    };
}

export default defineConfig(({ mode }) => ({
    customLogger: logger,
    plugins: [
        license({
            thirdParty: {
                output: {
                    file: resolve(import.meta.dirname, 'priv/static/LICENSES.txt'),
                },
            },
        }),
        filesize({ showBrotliSize: true }),
        ...(process.env.ANALYZE === 'true'
            ? [visualizer({
                  filename: resolve(import.meta.dirname, 'build-analysis.html'),
                  open: true,
                  gzipSize: true,
                  brotliSize: true,
              })]
            : []),
    ],
    build: {
        rollupOptions: {
            onwarn(warning) {
                throw new Error(warning.message);
            },
        },
        target: 'es2020',
        outDir: resolve(import.meta.dirname, 'priv/static'),
        emptyOutDir: true,
        sourcemap: mode === 'development',
        minify: 'terser',
        terserOptions: {
            compress: {
                passes: 2,
                pure_getters: true,
                unsafe_methods: true,
            },
            mangle: {
                properties: {
                    regex: /^__/,
                },
            },
            format: {
                preamble: `/*! Arizona v${pkg.version} | Apache-2.0 */`,
            },
        },
        lib: {
            entry: {
                arizona: resolve(import.meta.dirname, 'assets/js/arizona.js'),
                'arizona-worker': resolve(import.meta.dirname, 'assets/js/arizona-worker.js'),
                'arizona-reloader': resolve(import.meta.dirname, 'assets/js/arizona-reloader.js'),
            },
            formats: ['es'],
            fileName: (_format, entryName) => entryName + '.min.js',
        },
    },
    test: {
        environment: 'jsdom',
        include: ['assets/**/*.test.js'],
    },
}));
