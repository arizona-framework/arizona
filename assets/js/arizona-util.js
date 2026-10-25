/** Messages already emitted by `warnOnce`, so a repeating event cannot spam the console. */
const _warned = new Set();

/** @param {string} msg */
export function warnOnce(msg, key = msg) {
    if (_warned.has(key)) return;
    _warned.add(key);
    console.warn(msg);
}
