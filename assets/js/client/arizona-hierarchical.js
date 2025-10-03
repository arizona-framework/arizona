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
   * Merge new structures from fingerprint mismatches into existing Map
   * @param {Object} newStructures - New structures from arizona_hierarchical_dict
   */
  mergeStructures(newStructures) {
    for (const [id, data] of Object.entries(newStructures)) {
      console.log('[Arizona] Merging new structure into map:', id);
      this.structure.set(id, data);
    }
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

    const component = this.structure.get(statefulId);
    console.log('[Arizona] Applying diff to', statefulId);
    console.log('[Arizona] Changes:', changes);
    console.log(
      '[Arizona] Component dynamic before:',
      JSON.parse(JSON.stringify(component.dynamic))
    );
    for (const [elementIndex, newValue] of changes) {
      console.log('[Arizona] Applying change - index:', elementIndex, 'value:', newValue);
      this.applyDiffValue(component.dynamic, elementIndex - 1, newValue);
    }
    console.log(
      '[Arizona] Component dynamic after:',
      JSON.parse(JSON.stringify(component.dynamic))
    );
  }

  /**
   * Recursively apply a diff value to a container at a specific index
   * Handles all types: hierarchical structures, lists, stateless components, and simple values
   * @private
   * @param {Array} container - The dynamic array to update
   * @param {number} targetIndex - The index to update (0-based)
   * @param {*} newValue - The new value to apply
   * @returns {void}
   */
  applyDiffValue(container, targetIndex, newValue) {
    const existing = container[targetIndex];

    // 1. Existing is stateful reference - handle replacements/updates/removals
    if (existing?.type === 'stateful') {
      // 1a. Replacing with component reference (fingerprint mismatch)
      if (newValue?.type === 'stateful') {
        console.log('[Arizona] Updating component reference:', newValue.id);
        container[targetIndex] = newValue;
        return;
      }

      // 1b. Applying nested diff
      if (Array.isArray(newValue)) {
        const component = this.structure.get(existing.id);
        if (!component) {
          const sanitizedId = String(existing.id).replace(/\r|\n/g, '');
          console.warn(`[Arizona] Component '${sanitizedId}' not found in structure`);
          return;
        }
        console.log('[Arizona] Applying nested diff to:', existing.id);
        newValue.forEach(([index, value]) => {
          this.applyDiffValue(component.dynamic, index - 1, value);
        });
        return;
      }

      // 1c. Removing component (replacing with simple value)
      console.log('[Arizona] Removing component reference:', existing.id);
      container[targetIndex] = newValue;
      return;
    }

    // 2. New value is hierarchical reference (stateful/stateless/list)
    if (newValue?.type) {
      container[targetIndex] = newValue;
      return;
    }

    // 3. New value is array
    if (Array.isArray(newValue)) {
      // 3a. Existing is list - replace dynamic data
      if (existing?.type === 'list') {
        existing.dynamic = newValue;
        return;
      }

      // 3b. Existing is stateless - apply nested diff
      if (existing?.type === 'stateless') {
        newValue.forEach(([index, value]) => {
          this.applyDiffValue(existing.dynamic, index - 1, value);
        });
        return;
      }

      // 3c. Replace with array
      container[targetIndex] = newValue;
      return;
    }

    // 4. Simple value - replace
    container[targetIndex] = newValue;
  }

  /**
   * Generate HTML for stateful components
   * @param {Object} element - Stateful element with id
   * @returns {string} Generated HTML
   */
  generateStatefulHTML(statefulId) {
    console.log('[Arizona] generateStatefulHTML for', statefulId);
    const struct = this.structure.get(statefulId);
    if (!struct) {
      const sanitizedStatefulId = String(statefulId).replace(/\r|\n/g, '');
      console.warn(`[Arizona] StatefulId '${sanitizedStatefulId}' not found in structure`);
      throw new Error(`Component ${sanitizedStatefulId} not found`);
    }

    console.log('[Arizona] Structure found, generating HTML...');
    // Components always have static and dynamic arrays
    const html = this.zipStaticDynamic(struct.static, struct.dynamic);
    console.log('[Arizona] HTML generated for', statefulId, 'length:', html.length);
    return html;
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
    console.log('[Arizona] createPatch called for', statefulId);
    const html = this.generateStatefulHTML(statefulId);
    console.log('[Arizona] HTML generated, length:', html.length);
    return {
      type: 'html_patch',
      statefulId,
      html,
    };
  }
}
