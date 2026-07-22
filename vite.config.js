import { createRequire } from 'node:module';
import { resolve } from 'node:path';
import { constants as zlibConstants } from 'node:zlib';
import filesize from 'rollup-plugin-filesize';
import license from 'rollup-plugin-license';
import { visualizer } from 'rollup-plugin-visualizer';
import { createLogger, defineConfig } from 'vite';
import { compression, defineAlgorithm } from 'vite-plugin-compression2';

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
    // Relative base so the built `new Worker(new URL('./arizona-worker.min.js',
    // import.meta.url))` reference stays `./`-relative to `arizona.min.js`
    // instead of resolving against the origin root. Standalone usage serves the
    // worker as a sibling of the client (e.g. `/assets/js/`), and a downstream
    // bundler re-detects the relative `new URL(..., import.meta.url)` to emit +
    // content-hash the worker. An origin-absolute `/arizona-worker.min.js` would
    // 404 standalone and be treated as a passthrough (never re-hashed) downstream.
    base: './',
    plugins: [
        license({
            thirdParty: {
                output: {
                    file: resolve(import.meta.dirname, 'priv/static/assets/js/LICENSES.txt'),
                },
            },
        }),
        filesize({ showBrotliSize: true }),
        // Emit precompressed `.br` and `.gz` siblings so `roadrunner_static` can
        // serve them (nginx `brotli_static` / `gzip_static` style) over the
        // zero-copy sendfile path, preferring brotli for clients that accept it.
        // Both at max quality (build-time, immutable assets); originals are kept
        // as the fallback for clients that accept neither. Node's zlib brotli and
        // gzip are reproducible (no embedded mtime/filename), keeping
        // `check-dirty` stable.
        compression({
            include: /\.min\.js$/,
            threshold: 0,
            deleteOriginalAssets: false,
            algorithms: [
                defineAlgorithm('brotliCompress', {
                    params: {
                        [zlibConstants.BROTLI_PARAM_QUALITY]: zlibConstants.BROTLI_MAX_QUALITY,
                    },
                }),
                defineAlgorithm('gzip', { level: 9 }),
            ],
        }),
        ...(process.env.ANALYZE === 'true'
            ? [
                  visualizer({
                      filename: resolve(import.meta.dirname, 'build-analysis.html'),
                      open: true,
                      gzipSize: true,
                      brotliSize: true,
                  }),
              ]
            : []),
    ],
    build: {
        rollupOptions: {
            onwarn(warning) {
                throw new Error(warning.message);
            },
        },
        target: 'es2020',
        outDir: resolve(import.meta.dirname, 'priv/static/assets/js'),
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
                'arizona-reloader': resolve(import.meta.dirname, 'assets/js/arizona-reloader.js'),
            },
            formats: ['es'],
            fileName: (_format, entryName) => `${entryName}.min.js`,
        },
    },
    // The Worker is spawned from `arizona.js` via the bundler-idiomatic
    // `new Worker(new URL('./arizona-worker.js', import.meta.url), { type: 'module' })`.
    // Vite compiles it as an ES module worker and emits it as the un-hashed
    // sibling `arizona-worker.min.js` (the name `arizona_server` serves and
    // downstream consumers resolve), rewriting the reference in `arizona.min.js`
    // to that sibling. Keeping the static `new URL(..., import.meta.url)` shape in
    // the built output lets a consumer's bundler auto-emit + content-hash the
    // worker instead of leaving a runtime string that 404s.
    worker: {
        format: 'es',
        rollupOptions: {
            output: {
                entryFileNames: 'arizona-worker.min.js',
                chunkFileNames: 'arizona-worker-[hash].min.js',
                // The worker runs through Vite's separate (rolldown) build, which
                // does NOT apply the main build's `minify: 'terser'` -- it only
                // honours rolldown-native minify on the worker's own output. Turn
                // it on so `arizona-worker.min.js` is actually minified (a single
                // dense line) and its `.min.js` name is accurate, matching the
                // terser-minified client.
                minify: true,
            },
        },
    },
    test: {
        environment: 'jsdom',
        // The reference native client (e2e/utils/native_client.js) has unit tests
        // beside it; the browser client's live under assets/.
        include: ['assets/**/*.test.js', 'e2e/utils/**/*.test.js'],
        setupFiles: ['./assets/js/test-setup.js'],
        coverage: {
            provider: 'v8',
            include: ['assets/**/*.js'],
            exclude: ['assets/**/*.test.js', 'assets/js/test-setup.js'],
            thresholds: {
                perFile: true,
                statements: 80,
                branches: 80,
                functions: 80,
                lines: 80,
            },
        },
    },
}));
