import { createRequire } from 'node:module';
import { resolve } from 'node:path';
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
    plugins: [
        license({
            thirdParty: {
                output: {
                    file: resolve(import.meta.dirname, 'priv/static/assets/js/LICENSES.txt'),
                },
            },
        }),
        filesize({ showBrotliSize: true }),
        // Emit precompressed `.gz` siblings so `roadrunner_static` can serve
        // them (nginx `gzip_static` style) over the zero-copy sendfile path.
        // Level 9 (build-time, immutable assets); originals are kept as the
        // fallback for clients that don't accept gzip. Node's zlib gzip is
        // reproducible (no embedded mtime/filename), keeping `check-dirty` stable.
        compression({
            include: /\.min\.js$/,
            threshold: 0,
            deleteOriginalAssets: false,
            algorithms: [defineAlgorithm('gzip', { level: 9 })],
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
                'arizona-worker': resolve(import.meta.dirname, 'assets/js/arizona-worker.js'),
                'arizona-reloader': resolve(import.meta.dirname, 'assets/js/arizona-reloader.js'),
            },
            formats: ['es'],
            fileName: (_format, entryName) => `${entryName}.min.js`,
        },
    },
    test: {
        environment: 'jsdom',
        include: ['assets/**/*.test.js'],
        coverage: {
            provider: 'v8',
            include: ['assets/**/*.js'],
            exclude: ['assets/**/*.test.js'],
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
