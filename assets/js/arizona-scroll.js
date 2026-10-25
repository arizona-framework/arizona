// --------------------------------------------------------------------------
// Scroll on SPA navigation
// --------------------------------------------------------------------------
// Semantics:
//   push (az-navigate click, arizona_js:navigate/1,2 without replace)
//     -> save outgoing scroll to history entry; scroll to top (or #hash)
//        after OP_REPLACE. Opt out with az-noscroll / {noscroll: true}.
//   replace (arizona_js:navigate with #{replace => true})
//     -> in-place URL swap; do NOT save outgoing scroll; do NOT reset.
//   popstate (back)
//     -> restore the saved scroll stored on the destination entry.
//   popstate (forward, after back)
//     -> destination entry has no saved scroll (we only save on push),
//        so falls through to #hash-or-top. Restoring forward-nav scroll
//        is a deliberate non-goal for this release; adding it later
//        should use a state-ID-keyed Map + sessionStorage, not
//        replaceState-on-scroll.
//   full-document load of an existing entry (reload, Back without bfcache)
//     -> 'manual' disables the browser's restore here too, so `pagehide`
//        stamps the scroll onto the entry it leaves and `connect` restores it.

/**
 * Pending scroll intent set when az-navigate/az-patch/popstate is handled. A
 * navigate applies it after its OP_REPLACE; a patch (`patch: true`) applies it
 * after the first non-empty diff batch (it has no OP_REPLACE), and is dropped
 * at the end of the first worker frame after the patch either way (see the
 * worker message handler), so a no-op patch can't leave it armed indefinitely.
 * @type {{kind: 'push'|'pop', hash: string, saved?: {x:number,y:number}|null, patch?: boolean}|null}
 */
export let _pendingScroll = null;

/**
 * Arm (or, with `null`, drop) the pending scroll intent.
 * @param {typeof _pendingScroll} intent
 */
export function setPendingScroll(intent) {
    _pendingScroll = intent;
}

/**
 * Apply a scroll intent. pop+saved restores prior position; otherwise scroll
 * to #hash target if present, else to top.
 * @param {{kind: 'push'|'pop', hash: string, saved?: {x:number,y:number}|null}} p
 */
export function applyScroll(p) {
    if (p.kind === 'pop' && p.saved) {
        window.scrollTo(p.saved.x, p.saved.y);
        return;
    }
    if (p.hash) {
        const el = document.getElementById(p.hash);
        if (el) {
            el.scrollIntoView();
            return;
        }
    }
    window.scrollTo(0, 0);
}
