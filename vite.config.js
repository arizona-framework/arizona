import { dirname, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { defineConfig } from 'vite';

const __dirname = dirname(fileURLToPath(import.meta.url));

export default defineConfig(({ mode }) => {
  const isDev = mode === 'development';

  return {
    build: {
      outDir: 'priv/static/assets/js',
      minify: !isDev,
      sourcemap: true,
      reportCompressedSize: !isDev,
      emptyOutDir: false,
      lib: {
        entry: {
          arizona: resolve(__dirname, 'assets/js/arizona.js'),
          'arizona-worker': resolve(__dirname, 'assets/js/arizona-worker.js'),
        },
        name: 'Arizona',
        fileName: (_format, entryName) => {
          return `${entryName}.min.js`;
        },
        formats: ['es'],
      },
      rollupOptions: {
        external: [],
      },
    },
  };
});
