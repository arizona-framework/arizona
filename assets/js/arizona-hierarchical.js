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
export class ArizonaHierarchical {
  constructor() {
    this.structure = {};
  }

  /**
   * Initialize with hierarchical structure from server
   * @param {Object} structure - Hierarchical structure from arizona_hierarchical
   */
  initialize(structure) {
    this.structure = JSON.parse(JSON.stringify(structure)); // Deep clone
  }

  /**
   * Apply diff changes from arizona_differ to hierarchical structure
   * @param {Array} changes - Changes in format: [[StatefulId, [[ElementIndex, Changes]]]]
   */
  applyDiff(changes) {
    for (const [statefulId, elementChanges] of changes) {
      if (!this.structure[statefulId]) {
        console.warn(`[Arizona] StatefulId ${statefulId} not found in structure`);
        continue;
      }

      for (const [elementIndex, newValue] of elementChanges) {
        this.structure[statefulId][elementIndex] = newValue;
      }
    }
  }

  /**
   * Generate HTML from current hierarchical structure
   * @param {string} componentId - Component ID to render (defaults to 'root')
   * @returns {string} Generated HTML
   */
  generateHTML(componentId = 'root') {
    const component = this.structure[componentId];
    if (!component) {
      return '';
    }

    const elements = [];
    const sortedIndexes = Object.keys(component)
      .map((k) => parseInt(k))
      .sort((a, b) => a - b);

    for (const index of sortedIndexes) {
      const element = component[index];

      if (typeof element === 'string') {
        elements.push(element);
      } else if (element && element.type === 'stateful') {
        // Recursively render nested stateful component
        elements.push(this.generateHTML(element.id));
      } else if (element && element.type === 'stateless') {
        // Render stateless structure inline
        elements.push(this.generateHTMLFromStructure(element.structure));
      } else if (element && element.type === 'list') {
        // Render list elements
        elements.push(this.generateListHTML(element));
      } else {
        // Fallback for other types (numbers, etc.)
        elements.push(String(element));
      }
    }

    return elements.join('');
  }

  /**
   * Generate HTML from a structure object (for stateless components)
   * @param {Object} structure - Structure object with indexed elements
   * @returns {string} Generated HTML
   */
  generateHTMLFromStructure(structure) {
    const elements = [];
    const sortedIndexes = Object.keys(structure)
      .map((k) => parseInt(k))
      .sort((a, b) => a - b);

    for (const index of sortedIndexes) {
      const element = structure[index];
      if (typeof element === 'string') {
        elements.push(element);
      } else {
        elements.push(String(element));
      }
    }

    return elements.join('');
  }

  /**
   * Generate HTML for list components
   * @param {Object} listElement - List element with static template and dynamic data
   * @returns {string} Generated HTML
   */
  generateListHTML(listElement) {
    const { static: staticParts, dynamic } = listElement;
    const items = [];

    for (const dynamicItem of dynamic) {
      const itemElements = [];

      // Get dynamic indexes in order
      const dynamicIndexes = Object.keys(dynamicItem)
        .map((k) => parseInt(k))
        .sort((a, b) => a - b);

      // Interleave static and dynamic parts
      let staticIndex = 0;
      for (const dynIndex of dynamicIndexes) {
        // Add static part before this dynamic element
        if (staticIndex < staticParts.length) {
          itemElements.push(staticParts[staticIndex]);
          staticIndex++;
        }
        // Add dynamic element
        itemElements.push(String(dynamicItem[dynIndex]));
      }

      // Add remaining static parts
      while (staticIndex < staticParts.length) {
        itemElements.push(staticParts[staticIndex]);
        staticIndex++;
      }

      items.push(itemElements.join(''));
    }

    return items.join('');
  }

  /**
   * Get current structure (for debugging/testing)
   * @returns {Object} Deep copy of current structure
   */
  getStructure() {
    return JSON.parse(JSON.stringify(this.structure));
  }

  /**
   * Check if structure has been initialized
   * @returns {boolean} True if structure contains any components
   */
  isInitialized() {
    return Object.keys(this.structure).length > 0;
  }

  /**
   * Get component by ID
   * @param {string} componentId - Component ID to retrieve
   * @returns {Object|null} Component structure or null if not found
   */
  getComponent(componentId) {
    return this.structure[componentId] || null;
  }

  /**
   * Check if component exists
   * @param {string} componentId - Component ID to check
   * @returns {boolean} True if component exists
   */
  hasComponent(componentId) {
    return componentId in this.structure;
  }

  /**
   * Get all component IDs
   * @returns {string[]} Array of all component IDs
   */
  getComponentIds() {
    return Object.keys(this.structure);
  }

  /**
   * Clear all structure data
   */
  clear() {
    this.structure = {};
  }

  /**
   * Create a patch object that can be sent to arizona.js for DOM updating
   * This is used by the worker to send structured data to the main thread
   * @param {string} statefulId - Stateful ID to render (defaults to 'root')
   * @returns {Object} Patch object with statefulId and HTML
   */
  createPatch(statefulId = 'root') {
    return {
      type: 'html_patch',
      statefulId: statefulId,
      html: this.generateHTML(statefulId),
      timestamp: Date.now(),
    };
  }

  /**
   * Create an initial render patch (used on first load)
   * @param {string} statefulId - Stateful ID to render (defaults to 'root')
   * @returns {Object} Initial render patch object
   */
  createInitialPatch(statefulId = 'root') {
    return {
      type: 'initial_render',
      statefulId: statefulId,
      html: this.generateHTML(statefulId),
      structure: this.getStructure(),
      timestamp: Date.now(),
    };
  }

  /**
   * Validate a hierarchical structure format
   * @param {Object} structure - Structure to validate
   * @returns {boolean} True if structure is valid
   */
  static validateStructure(structure) {
    if (!structure || typeof structure !== 'object') {
      return false;
    }

    for (const [componentId, component] of Object.entries(structure)) {
      if (!component || typeof component !== 'object') {
        return false;
      }

      // Check that component has numeric indexes
      const indexes = Object.keys(component);
      for (const index of indexes) {
        if (isNaN(parseInt(index))) {
          return false;
        }
      }
    }

    return true;
  }

  /**
   * Validate diff changes format
   * @param {Array} changes - Changes to validate
   * @returns {boolean} True if changes format is valid
   */
  static validateDiff(changes) {
    if (!Array.isArray(changes)) {
      return false;
    }

    for (const change of changes) {
      if (!Array.isArray(change) || change.length !== 2) {
        return false;
      }

      const [statefulId, elementChanges] = change;
      if (typeof statefulId !== 'string' || !Array.isArray(elementChanges)) {
        return false;
      }

      for (const elementChange of elementChanges) {
        if (!Array.isArray(elementChange) || elementChange.length !== 2) {
          return false;
        }

        const [elementIndex] = elementChange;
        if (isNaN(parseInt(elementIndex))) {
          return false;
        }
      }
    }

    return true;
  }
}

/**
 * Utility functions for working with hierarchical structures
 */
export const ArizonaHierarchicalUtils = {
  /**
   * Deep clone a structure
   * @param {Object} structure - Structure to clone
   * @returns {Object} Deep cloned structure
   */
  cloneStructure(structure) {
    return JSON.parse(JSON.stringify(structure));
  },

  /**
   * Merge two structures (second structure wins on conflicts)
   * @param {Object} base - Base structure
   * @param {Object} overlay - Overlay structure
   * @returns {Object} Merged structure
   */
  mergeStructures(base, overlay) {
    const merged = this.cloneStructure(base);

    for (const [componentId, component] of Object.entries(overlay)) {
      merged[componentId] = { ...merged[componentId], ...component };
    }

    return merged;
  },

  /**
   * Extract all text content from a structure
   * @param {Object} structure - Structure to extract text from
   * @returns {string} Extracted text content
   */
  extractTextContent(structure) {
    const client = new ArizonaHierarchical();
    client.initialize(structure);
    const html = client.generateHTML();

    // Simple HTML tag removal for text extraction
    return html.replace(/<[^>]*>/g, '').trim();
  },

  /**
   * Get statistics about a structure
   * @param {Object} structure - Structure to analyze
   * @returns {Object} Statistics object
   */
  getStructureStats(structure) {
    let componentCount = 0;
    let elementCount = 0;
    let statefulRefs = 0;
    let statelessComponents = 0;
    let listComponents = 0;

    for (const [componentId, component] of Object.entries(structure)) {
      componentCount++;

      for (const element of Object.values(component)) {
        elementCount++;

        if (element && typeof element === 'object') {
          if (element.type === 'stateful') {
            statefulRefs++;
          } else if (element.type === 'stateless') {
            statelessComponents++;
          } else if (element.type === 'list') {
            listComponents++;
          }
        }
      }
    }

    return {
      componentCount,
      elementCount,
      statefulRefs,
      statelessComponents,
      listComponents,
    };
  },
};

/**
 * Default export for convenience
 */
export default ArizonaHierarchical;
