import { default as ArizonaPatcher } from './arizona-patcher.js';
export default ArizonaMorphdomPatcher;
/**
 * Morphdom-based patcher (requires morphdom peer dependency)
 *
 * Efficiently diffs and patches only changed parts of the DOM.
 * Preserves focus, selection, and minimizes DOM operations.
 *
 * Default opinionated behavior:
 * - Skips updates for elements with data-arizona-update="false"
 * - Skips updates for identical nodes (optimization)
 *
 * @example
 * // With default options
 * import { ArizonaMorphdomPatcher } from '@arizona-framework/client/patcher/morphdom';
 * const patcher = new ArizonaMorphdomPatcher();
 *
 * // With custom options
 * const patcher = new ArizonaMorphdomPatcher({
 *   onBeforeElUpdated: (fromEl, toEl) => true,
 *   childrenOnly: false
 * });
 */
declare class ArizonaMorphdomPatcher extends ArizonaPatcher {
    /**
     * @param {Object} [options] - Morphdom options (if not provided, uses Arizona defaults)
     */
    constructor(options?: Object);
    options: Object | {
        onBeforeElUpdated(fromEl: any, toEl: any): boolean;
    };
    patch(target: any, html: any): void;
}
