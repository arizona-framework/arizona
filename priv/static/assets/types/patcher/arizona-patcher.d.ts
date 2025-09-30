export default ArizonaPatcher;
/**
 * Base class for Arizona DOM patching strategies
 *
 * Implementations must provide a `patch` method that updates
 * the target element with new HTML content.
 */
declare class ArizonaPatcher {
    /**
     * Patch the DOM element with new HTML
     * @param {HTMLElement} target - The DOM element to patch
     * @param {string} html - The new HTML content
     * @param {Object} [options={}] - Patcher-specific options
     * @returns {void}
     */
    patch(target: HTMLElement, html: string, options?: Object): void;
}
