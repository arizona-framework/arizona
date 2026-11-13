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
 * @property {() => boolean} ready whether delegated commands run now: on a live
 *   page while its socket is open, on a page with no server always
 * @property {() => number} epoch the navigation epoch a deferred event captures
 * @property {(name: string, frame: Array<*>, epoch?: number,
 *   transition?: {types?: string[], kind: 'any'|'replace'}|null,
 *   ctls?: Array<[Element, string|boolean]>|null) => void} sendEvent send
 *   the view event `frame` (`[target, event, payload]`) raised in `epoch` (default:
 *   the current one), under the view transition that waits for its reply, if any,
 *   carrying the state of the server-owned controls `ctls` as it was raised
 * @property {(el: Element, event: Event|null) => Array<[Element, string|boolean]>|null} carry
 *   the server-owned controls an event raised on `el` sends the state of, with that state
 * @property {(el: Element, name: string, val: string|null) => boolean|'defer'} holds
 *   whether a server write of `name` to `el` (a control, or an option of one) must leave
 *   its live state alone, or (`'defer'`) not happen yet
 * @property {(e: Event) => void} ctlEvent a focusout, input or IME composition event
 * @property {(url: string) => boolean} awaitsReply whether a navigate/patch to `url`
 *   is answered by a frame, so a view transition can wait for it
 * @property {(t: {types?: string[], kind: 'any'|'replace'}) => void} armTransition arm a view
 *   transition for the reply a command just sent awaits
 * @property {() => (fn: () => void) => void} holdReplay hold the replay running now
 *   open for work a command runs later
 * @property {(url: string, opts: Object<string, *>, kind: 'navigate'|'patch',
 *   transition?: {types?: string[], kind: 'any'|'replace'}) => void} navigate
 *   a navigate or patch effect, under the view transition running it, if any
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
