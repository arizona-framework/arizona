// The client runtime as one ES module, built from its source. `assets/js/arizona.js`
// is an entry that imports the modules beside it, and the client benches hand the
// page the source inline (or serve it alone), where those imports cannot resolve.
// Unminified, so the top-level function names the benches instrument survive.
import { join } from 'node:path';
import { rollup } from 'rollup';

/** @param {string} root the repository root */
export async function clientSource(root) {
    const bundle = await rollup({
        input: join(root, 'assets/js/arizona.js'),
        // The shared modules call each other at run time only (the interpreter and
        // the delegation are mutually recursive), which ES modules handle.
        onwarn(warning, warn) {
            if (warning.code !== 'CIRCULAR_DEPENDENCY') warn(warning);
        },
    });
    try {
        const { output } = await bundle.generate({ format: 'es' });
        return output[0].code;
    } finally {
        await bundle.close();
    }
}
