/**
 * Arizona Hierarchical Client
 *
 * Standalone module for managing hierarchical structures and HTML generation.
 * Used by both arizona-worker.js and arizona.js for consistent structure handling.
 *
 * Features:
 * - Initialize with hierarchical structures from WebSocket
 * - Apply diffs from arizona_differ to update structures
 * - Generate HTML from hierarchical structures
 * - Support for nested stateful/stateless components and lists
 */

/**
 * ArizonaHierarchical - Client-side hierarchical structure manager
 */
export default class ArizonaHierarchical {
  constructor() {
    this.structure = new Map();
  }

  /**
   * Initialize with hierarchical structure from server
   * @param {Object} structure - Hierarchical structure from arizona_hierarchical
   */
  initialize(structure) {
    this.structure = new Map(Object.entries(JSON.parse(JSON.stringify(structure)))); // Deep clone and convert to Map
  }

  /**
   * Apply diff changes from arizona_differ to hierarchical structure
   * @param {string} statefulId - Stateful ID to render
   * @param {Array|Object} changes - Changes in format: [[ElementIndex, Changes]] or hierarchical structure
   */
  applyDiff(statefulId, changes) {
    // Check if changes is a hierarchical structure (fingerprint mismatch case)
    if (changes?.type === 'stateful') {
      this.structure.set(changes.id, changes);
      return;
    }

    if (!this.structure.has(statefulId)) {
      const sanitizedStatefulId = String(statefulId).replace(/\r|\n/g, '');
      console.warn(`[Arizona] StatefulId '${sanitizedStatefulId}' not found in structure`);
    }

    for (const [elementIndex, newValue] of changes) {
      // Check if newValue is a hierarchical structure (fingerprint mismatch)
      if (newValue && typeof newValue === 'object' && newValue.type) {
        this.structure.get(statefulId).dynamic[elementIndex - 1] = newValue;
      } else if (Array.isArray(newValue)) {
        const element = this.structure.get(statefulId).dynamic[elementIndex - 1];
        if (element && element.type === 'list') {
          this.structure.get(statefulId).dynamic[elementIndex - 1].dynamic = newValue;
        } else if (element && element.type === 'stateless') {
          // Traditional stateless diff - array of [index, value] pairs
          newValue.forEach(([index, value]) => {
            this.structure.get(statefulId).dynamic[elementIndex - 1].dynamic[index - 1] = value;
          });
        } else {
          this.structure.get(statefulId).dynamic[elementIndex - 1] = newValue;
        }
      } else {
        this.structure.get(statefulId).dynamic[elementIndex - 1] = newValue;
      }
    }
  }

  /**
   * Generate HTML for stateful components
   * @param {Object} element - Stateful element with id
   * @returns {string} Generated HTML
   */
  generateStatefulHTML(statefulId) {
    const struct = this.structure.get(statefulId);
    if (!struct) {
      const sanitizedStatefulId = String(statefulId).replace(/\r|\n/g, '');
      console.warn(`[Arizona] StatefulId '${sanitizedStatefulId}' not found in structure`);
    }

    // Components always have static and dynamic arrays
    return this.zipStaticDynamic(struct.static, struct.dynamic);
  }

  /**
   * Generate HTML for stateless components
   * @param {Object} element - Stateless element with static and dynamic arrays
   * @returns {string} Generated HTML
   */
  generateStatelessHTML(element) {
    return this.zipStaticDynamic(element.static, element.dynamic);
  }

  /**
   * Generate HTML for list components
   * @param {Object} listElement - List element with static template and dynamic data
   * @returns {string} Generated HTML
   */
  generateListHTML(listElement) {
    const { static: staticParts, dynamic: dynamicPartsList } = listElement;
    return dynamicPartsList.reduce((acc, dynamicParts) => {
      return acc + this.zipStaticDynamic(staticParts, dynamicParts);
    }, '');
  }

  /**
   * Zip static and dynamic arrays into HTML (matches arizona_renderer:zip_static_dynamic/2)
   * @param {Array} staticParts - Static HTML parts
   * @param {Array} dynamicParts - Dynamic content parts
   * @returns {string} Generated HTML
   */
  zipStaticDynamic(staticParts, dynamicParts) {
    const elements = [];
    const maxLength = Math.max(staticParts.length, dynamicParts.length);

    for (let i = 0; i < maxLength; i++) {
      if (i < staticParts.length) {
        elements.push(staticParts[i]);
      }
      if (i < dynamicParts.length) {
        elements.push(this.normalizeDynamicElement(dynamicParts[i]));
      }
    }

    return elements.join('');
  }

  /**
   * Normalize a dynamic element to string (handles stateful, stateless, lists, etc.)
   * @param {*} element - Dynamic element to normalize
   * @returns {string} Normalized string content
   */
  normalizeDynamicElement(element) {
    if (typeof element === 'string') {
      return element;
    } else if (element && element.type === 'stateful') {
      // Recursively render nested stateful component
      return this.generateStatefulHTML(element.id);
    } else if (element && element.type === 'stateless') {
      // Render stateless structure inline
      return this.generateStatelessHTML(element);
    } else if (element && element.type === 'list') {
      // Render list elements
      return this.generateListHTML(element);
    } else if (Array.isArray(element)) {
      // Handle nested arrays (iodata from server) - flatten recursively
      return this.flattenIoData(element);
    } else {
      // Fallback for other types (numbers, etc.)
      return String(element);
    }
  }

  /**
   * Flatten nested arrays (iodata from Erlang server) into a single string
   * This handles the complex nested array structures that come from render_list
   * and other server-side rendering operations that produce iodata.
   *
   * @param {Array|string|number} element - Element to flatten
   * @returns {string} Flattened string without commas between array elements
   */
  flattenIoData(element) {
    if (typeof element === 'string') {
      return element;
    } else if (typeof element === 'number') {
      return String(element);
    } else if (Array.isArray(element)) {
      // Recursively flatten nested arrays
      return element
        .map((item) => {
          return this.flattenIoData(item);
        })
        .join('');
    } else if (element && typeof element === 'object') {
      // Handle special object types
      if (element.type === 'stateful') {
        return this.generateStatefulHTML(element.id);
      } else if (element.type === 'stateless') {
        return this.generateStatelessHTML(element);
      } else if (element.type === 'list') {
        return this.generateListHTML(element);
      } else {
        // For other objects, try to convert to string
        return String(element);
      }
    } else {
      // Fallback for null, undefined, etc.
      return String(element || '');
    }
  }

  /**
   * Get current structure (for debugging/testing)
   * @returns {Object} Deep copy of current structure
   */
  getStructure() {
    return JSON.parse(JSON.stringify(Object.fromEntries(this.structure)));
  }

  /**
   * Check if structure has been initialized
   * @returns {boolean} True if structure contains any components
   */
  isInitialized() {
    return this.structure.size > 0;
  }

  /**
   * Get all component IDs
   * @returns {string[]} Array of all component IDs
   */
  getComponentIds() {
    return Array.from(this.structure.keys());
  }

  /**
   * Clear all structure data
   */
  clear() {
    this.structure = new Map();
  }

  /**
   * Create a patch object that can be sent to arizona.js for DOM updating
   * This is used by the worker to send structured data to the main thread
   * @param {string} statefulId - Stateful ID to render
   * @returns {Object} Patch object with statefulId and HTML
   */
  createPatch(statefulId) {
    return {
      type: 'html_patch',
      statefulId,
      html: this.generateStatefulHTML(statefulId),
    };
  }
}
