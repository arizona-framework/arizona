/* global process */
import * as esbuild from 'esbuild';

Promise.all([
  esbuild.build({
    entryPoints: ['./assets/js/arizona.mjs'],
    outfile: './priv/static/assets/js/arizona.min.js',
    bundle: true,
    minify: true,
    sourcemap: true,
  }),
  esbuild.build({
    entryPoints: ['./assets/js/arizona-worker.mjs'],
    outfile: './priv/static/assets/js/arizona-worker.min.js',
    bundle: true,
    minify: true,
    sourcemap: true,
  })
]).catch((error) => {
  console.error(error);
  process.exit(1);
});
