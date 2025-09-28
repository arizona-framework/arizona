import { defineConfig } from 'vite';

export default defineConfig(({ mode: _mode }) => {
  return {
    build: {
      outDir: 'priv/static/assets/js',
      minify: true,
      sourcemap: true,
      reportCompressedSize: true,
      emptyOutDir: false,
      rollupOptions: {
        input: 'assets/js/arizona.js',
        output: {
          entryFileNames: 'arizona.min.js',
          chunkFileNames: 'chunks/[name]-[hash].js',
          assetFileNames: 'assets/[name]-[hash][extname]',
        },
      },
    },
  };
});
