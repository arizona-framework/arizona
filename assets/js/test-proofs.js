/**
 * The `az-*` names the suites' pages declare commands under. The client runs a
 * command only under a proven name, and a live page learns its names from what
 * the server embeds in it (`#arizona-attrs`) before the socket opens -- so a
 * harness standing in for that page embeds them before `connect`.
 */
export const TEST_ATTRS = [
    'az-click',
    'az-change',
    'az-input',
    'az-keydown',
    'az-keyup',
    'az-focusin',
    'az-focusout',
    'az-submit',
    'az-drop',
];

/**
 * Embed `names` as the server does for a live page, for the next `connect` to
 * read. Returns the remover.
 * @param {string[]} [names]
 * @returns {() => void}
 */
export function embedAttrs(names = TEST_ATTRS) {
    const el = document.createElement('script');
    el.type = 'application/json';
    el.id = 'arizona-attrs';
    el.textContent = JSON.stringify(names);
    document.head.appendChild(el);
    return () => el.remove();
}
