import morphdom from 'morphdom';
import ArizonaPatcher from './arizona-patcher.js';

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
class ArizonaMorphdomPatcher extends ArizonaPatcher {
  /**
   * @param {Object} [options] - Morphdom options (if not provided, uses Arizona defaults)
   */
  constructor(options) {
    super();
    // Use provided options or Arizona defaults
    this.options = options || {
      onBeforeElUpdated(fromEl, toEl) {
        // Skip update if data-arizona-update="false"
        if (toEl.dataset?.arizonaUpdate === 'false') {
          return false;
        }
        // Skip update if nodes are identical
        return !fromEl.isEqualNode(toEl);
      },
    };
  }

  patch(target, html) {
    morphdom(target, html, this.options);
  }
}

export default ArizonaMorphdomPatcher;
