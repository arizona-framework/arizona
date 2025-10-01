// Import dependencies
import ArizonaWorker from './arizona-worker?worker&inline';

/**
 * @typedef {Object} ArizonaClientOptions
 * @property {import('./patcher/arizona-patcher.js').default} [patcher] - DOM patcher implementation
 * @property {import('./logger/arizona-logger.js').default} [logger] - Logger implementation
 */

/**
 * @typedef {Object} ConnectOptions
 * @property {string} [wsPath] - WebSocket path (default: '/live')
 */

/**
 * @typedef {Object} EventParams
 * @property {*} [key] - Event parameters
 */

/**
 * Arizona Framework JavaScript Client
 * Provides real-time WebSocket communication with the Arizona server
 */
export default class ArizonaClient {
  /**
   * Creates a new Arizona client instance
   * @param {ArizonaClientOptions} [opts={}] - Client configuration options
   */
  constructor(opts = {}) {
    /** @type {Worker|null} */
    this.worker = null;
    /** @type {boolean} */
    this.connected = false;
    /** @type {Map<string, Set<Function>>} */
    this.eventListeners = new Map();
    /** @type {import('./patcher/arizona-patcher.js').default|null} */
    this.patcher = opts.patcher || null;
    /** @type {import('./logger/arizona-logger.js').default|null} */
    this.logger = opts.logger || null;
  }

  /**
   * Initialize worker if not already created
   * @private
   * @returns {void}
   */
  initializeWorker() {
    if (this.worker) return;

    // Use Vite's worker import pattern - more efficient and bundler-aware
    this.worker = new ArizonaWorker();

    this.worker.onmessage = (event) => {
      this.handleWorkerMessage(event.data);
    };
  }

  /**
   * Connect to the Arizona WebSocket server
   * @param {string} websocketEndpoint - WebSocket endpoint path
   * @returns {void}
   */
  connect(websocketEndpoint) {
    if (this.connected) return;

    this.initializeWorker();

    const protocol = window.location.protocol === 'https:' ? 'wss:' : 'ws:';
    const host = window.location.host;
    const path = window.location.pathname;
    const qs = window.location.search;
    const encodedPath = encodeURIComponent(path);
    const encodeQs = qs ? encodeURIComponent(qs.substring(1)) : '';
    const wsUrl = `${protocol}//${host}${websocketEndpoint}?path=${encodedPath}&qs=${encodeQs}`;

    this.worker.postMessage({
      type: 'connect',
      data: { url: wsUrl },
    });
  }

  /**
   * Push an event to the Arizona server
   * @param {string} event - Event name
   * @param {EventParams} [params={}] - Event parameters
   * @returns {void}
   */
  pushEvent(event, params = {}) {
    if (!this.connected) return;

    this.worker.postMessage({
      type: 'send',
      data: {
        type: 'event',
        event,
        params,
      },
    });
  }

  /**
   * Push an event to a specific stateful component
   * @param {string} statefulId - Target stateful component ID
   * @param {string} event - Event name
   * @param {EventParams} [params={}] - Event parameters
   * @returns {void}
   */
  pushEventTo(statefulId, event, params = {}) {
    if (!this.connected) return;

    this.worker.postMessage({
      type: 'send',
      data: {
        type: 'event',
        stateful_id: statefulId,
        event,
        params,
      },
    });
  }

  /**
   * Disconnect from the Arizona WebSocket server
   * @returns {void}
   */
  disconnect() {
    if (this.worker) {
      this.worker.terminate();
      this.worker = null;
    }
    this.connected = false;
  }

  /**
   * Handle messages from the worker thread
   * @private
   * @param {Object} message - Worker message
   * @returns {void}
   */
  handleWorkerMessage(message) {
    const { type, data } = message;

    try {
      switch (type) {
        case 'status':
          this.handleStatus(data);
          break;
        case 'initial_render':
          break;
        case 'html_patch':
          this.handleHtmlPatch(data);
          break;
        case 'error':
          this.handleWorkerError(data);
          break;
        case 'reload':
          this.handleReload(data);
          break;
        case 'dispatch':
          this.handleDispatch(data);
          break;
        case 'redirect':
          this.handleRedirect(data);
          break;
        default:
          this.handleUnknownMessage(message);
      }
    } catch (error) {
      this.logger?.error('Error handling worker message:', error);
    }
  }

  handleStatus(data) {
    if (data.status === 'connected') {
      this.connected = true;
      this.logger?.info('Connected to WebSocket');
      this.emit('connected', data);
    } else if (data.status === 'disconnected') {
      this.connected = false;
      this.logger?.info('Disconnected from WebSocket');
      this.emit('disconnected', data);
    }
  }

  handleHtmlPatch(data) {
    this.logger?.debug('Applying HTML patch');

    // Apply HTML patch to DOM
    this.applyHtmlPatch(data.patch);
  }

  applyHtmlPatch(patch) {
    const target = document.getElementById(patch.statefulId);

    if (!target) {
      this.logger?.warning(`Target element not found: ${patch.statefulId}`);
      return;
    }

    if (!this.patcher) {
      this.logger?.error('No patcher configured. Please provide a patcher implementation.');
      return;
    }

    try {
      // Use provided patcher to patch the DOM
      this.patcher.patch(target, patch.html);

      this.logger?.debug('Patch applied successfully');
    } catch (error) {
      this.logger?.error('Error applying HTML patch:', error);
    }
  }

  handleWorkerError(data) {
    this.logger?.error('Worker Error:', data.error);
    this.emit('error', data);
  }

  handleReload(data) {
    switch (data.file_type) {
      case 'css':
        this.logger?.info('CSS file changed. Refreshing stylesheets without page reload...');
        document.querySelectorAll('link[rel="stylesheet"]').forEach((link) => {
          const href = link.href.split('?')[0];
          link.href = `${href}?t=${Date.now()}`;
        });
        break;
      default:
        this.logger?.info(`${data.file_type || 'File'} changed. Reloading page...`);
        window.location.reload();
    }
  }

  handleDispatch(data) {
    this.logger?.debug('Dispatching event:', data.event);
    this.emit(data.event, data.data);
  }

  handleRedirect(data) {
    this.logger?.info('Redirecting to:', data.url);

    // Perform the redirect with safe option access
    window.open(data.url, data.options?.target, data.options?.window_features);
  }

  handleUnknownMessage(message) {
    this.logger?.warning('Unknown worker message:', message);
  }

  /**
   * Check if client is connected to server
   * @returns {boolean} True if connected
   */
  isConnected() {
    return this.connected;
  }

  /**
   * Subscribe to an Arizona event
   * @param {string} event - Event name (e.g., 'connected', 'disconnected')
   * @param {Function} callback - Callback function to invoke when event occurs
   * @returns {Function} Unsubscribe function
   */
  on(event, callback) {
    if (typeof callback !== 'function') {
      this.logger?.error(`on: callback must be a function, got ${typeof callback}`);
      return () => {};
    }

    if (!this.eventListeners.has(event)) {
      this.eventListeners.set(event, new Set());
    }

    this.eventListeners.get(event).add(callback);
    this.logger?.debug(`Subscribed to event: ${event}`);

    // Return unsubscribe function
    return () => {
      return this.off(event, callback);
    };
  }

  /**
   * Subscribe to an Arizona event that will only fire once
   * @param {string} event - Event name
   * @param {Function} callback - Callback function to invoke when event occurs
   * @returns {Function} Unsubscribe function
   */
  once(event, callback) {
    if (typeof callback !== 'function') {
      this.logger?.error(`once: callback must be a function, got ${typeof callback}`);
      return () => {};
    }

    const wrapper = (data) => {
      callback(data);
      this.off(event, wrapper);
    };

    return this.on(event, wrapper);
  }

  /**
   * Unsubscribe from an Arizona event
   * @param {string} event - Event name
   * @param {Function} callback - Callback function to remove
   * @returns {void}
   */
  off(event, callback) {
    const listeners = this.eventListeners.get(event);
    if (listeners) {
      listeners.delete(callback);
      this.logger?.debug(`Unsubscribed from event: ${event}`);

      // Clean up empty listener sets
      if (listeners.size === 0) {
        this.eventListeners.delete(event);
      }
    }
  }

  /**
   * Remove all listeners for a specific event, or all events if no event specified
   * @param {string} [event] - Optional event name. If not provided, removes all listeners for all events
   * @returns {void}
   */
  removeAllListeners(event) {
    if (event) {
      this.eventListeners.delete(event);
      this.logger?.debug(`Removed all listeners for event: ${event}`);
    } else {
      this.eventListeners.clear();
      this.logger?.debug('Removed all event listeners');
    }
  }

  /**
   * Emit an Arizona event to all subscribed listeners
   * @private
   * @param {string} event - Event name
   * @param {*} data - Event data to pass to listeners
   * @returns {void}
   */
  emit(event, data) {
    const listeners = this.eventListeners.get(event);
    if (listeners) {
      listeners.forEach((callback) => {
        try {
          callback(data);
        } catch (error) {
          this.logger?.error(`Error in event listener for '${event}':`, error);
        }
      });
    }
  }
}
