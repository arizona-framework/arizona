// Import dependencies
import ArizonaWorker from './arizona-worker?worker&inline';
import morphdom from 'morphdom';

/**
 * @typedef {Object} ArizonaClientOptions
 * @property {'silent' | 'error' | 'warning' | 'info' | 'debug'} [logLevel] - Log level for client output
 */

/**
 * @typedef {Object} ConnectOptions
 * @property {string} [wsPath] - WebSocket path (default: '/live')
 */

/**
 * @typedef {Object} EventParams
 * @property {*} [key] - Event parameters
 */

// Log levels (aligned with Erlang logger levels)
const LOG_LEVELS = {
  silent: -1,
  error: 3,
  warning: 4,
  info: 6,
  debug: 7,
};

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
    /** @type {number} */
    this.logLevel = LOG_LEVELS[opts.logLevel] ?? LOG_LEVELS.silent; // Default: silent (production-safe)
    /** @type {Map<string, Set<Function>>} */
    this.eventListeners = new Map();
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
   * Send an event to the Arizona server
   * @param {string} event - Event name
   * @param {EventParams} [params={}] - Event parameters
   * @returns {void}
   */
  sendEvent(event, params = {}) {
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
   * Send an event to a specific stateful component
   * @param {string} statefulId - Target stateful component ID
   * @param {string} event - Event name
   * @param {EventParams} [params={}] - Event parameters
   * @returns {void}
   */
  sendEventTo(statefulId, event, params = {}) {
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
        case 'dispatch_to':
          this.handleDispatchTo(data);
          break;
        case 'redirect':
          this.handleRedirect(data);
          break;
        default:
          this.handleUnknownMessage(message);
      }
    } catch (error) {
      this.error('Error handling worker message:', error);
    }
  }

  handleStatus(data) {
    if (data.status === 'connected') {
      this.connected = true;
      this.info('Connected to WebSocket');
      this.emit('connected', data);
    } else if (data.status === 'disconnected') {
      this.connected = false;
      this.info('Disconnected from WebSocket');
      this.emit('disconnected', data);
    }
  }

  handleHtmlPatch(data) {
    this.debug('Applying HTML patch');

    // Apply HTML patch to DOM
    this.applyHtmlPatch(data.patch);
  }

  applyHtmlPatch(patch) {
    const target = document.getElementById(patch.statefulId);

    if (!target) {
      this.warning(`Target element not found: ${patch.statefulId}`);
      return;
    }

    try {
      // Use Morphdom to efficiently patch the DOM
      morphdom(target, patch.html, {
        onBeforeElUpdated(fromEl, toEl) {
          // Skip update if data-arizona-update="false"
          if (toEl.dataset?.arizonaUpdate === 'false') {
            return false;
          } else {
            // Skip update if nodes are identical
            return !fromEl.isEqualNode(toEl);
          }
        },
      });

      this.debug('Patch applied successfully');

      // Trigger custom event for other code to listen to
      target.dispatchEvent(
        new CustomEvent('arizona:patched', {
          detail: { patch },
        })
      );
    } catch (error) {
      this.error('Error applying HTML patch:', error);
    }
  }

  handleWorkerError(data) {
    this.error('Worker Error:', data.error);
    this.emit('error', data);
  }

  handleReload(data) {
    switch (data.file_type) {
      case 'css':
        this.info('CSS file changed. Refreshing stylesheets without page reload...');
        document.querySelectorAll('link[rel="stylesheet"]').forEach((link) => {
          const href = link.href.split('?')[0];
          link.href = `${href}?t=${Date.now()}`;
        });
        break;
      default:
        this.info(`${data.file_type || 'File'} changed. Reloading page...`);
        window.location.reload();
    }
  }

  handleDispatchTo(data) {
    this.debug('WebSocket dispatch to:', data);
    const elem = document.querySelector(data.selector);
    elem.dispatchEvent(new CustomEvent(data.event, data.options));
  }

  handleRedirect(data) {
    this.info('Redirecting to:', data.url);

    // Perform the redirect with safe option access
    window.open(data.url, data.options?.target, data.options?.window_features);
  }

  handleUnknownMessage(message) {
    this.warning('Unknown worker message:', message);
  }

  /**
   * Check if client is connected to server
   * @returns {boolean} True if connected
   */
  isConnected() {
    return this.connected;
  }

  // Logging methods aligned with Erlang logger levels

  /**
   * Log error message (always shown)
   * @param {string} message - Error message
   * @param {...*} args - Additional arguments
   * @returns {void}
   */
  error(message, ...args) {
    // Always show errors (critical for debugging)
    console.error(`[Arizona] ${message}`, ...args);
  }

  /**
   * Log warning message (shown if log level allows)
   * @param {string} message - Warning message
   * @param {...*} args - Additional arguments
   * @returns {void}
   */
  warning(message, ...args) {
    if (this.logLevel >= LOG_LEVELS.warning) {
      console.warn(`[Arizona] ${message}`, ...args);
    }
  }

  /**
   * Log info message (shown if log level allows)
   * @param {string} message - Info message
   * @param {...*} args - Additional arguments
   * @returns {void}
   */
  info(message, ...args) {
    if (this.logLevel >= LOG_LEVELS.info) {
      console.log(`[Arizona] ${message}`, ...args);
    }
  }

  /**
   * Log debug message (shown if log level allows)
   * @param {string} message - Debug message
   * @param {...*} args - Additional arguments
   * @returns {void}
   */
  debug(message, ...args) {
    if (this.logLevel >= LOG_LEVELS.debug) {
      console.log(`[Arizona] ${message}`, ...args);
    }
  }

  /**
   * Subscribe to an Arizona event
   * @param {string} event - Event name (e.g., 'connected', 'disconnected')
   * @param {Function} callback - Callback function to invoke when event occurs
   * @returns {Function} Unsubscribe function
   */
  on(event, callback) {
    if (typeof callback !== 'function') {
      this.error(`on: callback must be a function, got ${typeof callback}`);
      return () => {};
    }

    if (!this.eventListeners.has(event)) {
      this.eventListeners.set(event, new Set());
    }

    this.eventListeners.get(event).add(callback);
    this.debug(`Subscribed to event: ${event}`);

    // Return unsubscribe function
    return () => {
      return this.off(event, callback);
    };
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
      this.debug(`Unsubscribed from event: ${event}`);

      // Clean up empty listener sets
      if (listeners.size === 0) {
        this.eventListeners.delete(event);
      }
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
          this.error(`Error in event listener for '${event}':`, error);
        }
      });
    }
  }
}
