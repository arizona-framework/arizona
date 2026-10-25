/**
 * What the runtime shared by every way of serving a page (event delegation,
 * the command interpreter, hooks) asks of whatever serves it: whether a live
 * socket is open, how an event reaches the server, how a navigation happens.
 *
 * The shared modules call only through `host` and never import the connection,
 * which is what keeps them usable without one. `arizona-connection.js` installs
 * the live implementations when it is loaded.
 *
 * @typedef {object} Host
 * @property {() => boolean} isConnected whether a live socket is open
 * @property {() => number} epoch the navigation epoch a deferred event captures
 * @property {(name: string, msg: string, epoch?: number) => void} sendEvent send a
 *   view event raised in `epoch` (default: the current one)
 * @property {(url: string) => boolean} awaitsReply whether a navigate/patch to `url`
 *   is answered by a frame, so a view transition can wait for it
 * @property {(t: {types?: string[], kind: 'any'|'replace'}) => void} armTransition arm a view
 *   transition for the reply a command just sent awaits
 * @property {() => (fn: () => void) => void} holdReplay hold the replay running now
 *   open for work a command runs later
 * @property {(url: string, opts: Object<string, *>, kind: 'navigate'|'patch') => void} navigate
 *   a navigate or patch effect
 * @property {(url: string, replace?: boolean) => void} leavePage load `url` as a real page
 */

/** @type {Host} */
export const host = /** @type {Host} */ ({});

/**
 * Install the implementations `host` calls.
 * @param {Host} impl
 */
export function setHost(impl) {
    Object.assign(host, impl);
}
