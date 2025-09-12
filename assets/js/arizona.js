// Import dependencies
import morphdom from 'morphdom';
import ArizonaHierarchical from './arizona-hierarchical.js';

// Log levels (aligned with Erlang logger levels)
const LOG_LEVELS = {
  silent: -1,
  error: 3,
  warning: 4,
  info: 6,
  debug: 7,
};

// Arizona Client API
export default class ArizonaClient {
  constructor(opts = {}) {
    this.worker = null;
    this.connected = false;
    this.hierarchical = new ArizonaHierarchical();
    this.logLevel = LOG_LEVELS[opts.logLevel] ?? LOG_LEVELS.silent; // Default: silent (production-safe)
  }

  connect(opts = {}) {
    if (this.connected) return;

    const wsPath = opts.wsPath || '/live';
    const workerPath = opts.workerPath || '/assets/js/arizona-worker.min.js';

    this.worker = new Worker(workerPath, { type: 'module' });

    const protocol = window.location.protocol === 'https:' ? 'wss:' : 'ws:';
    const host = window.location.host;
    const path = window.location.pathname;
    const qs = window.location.search;
    const encodedPath = encodeURIComponent(path);
    const encodeQs = qs ? encodeURIComponent(qs.substring(1)) : '';
    const wsUrl = `${protocol}//${host}${wsPath}?path=${encodedPath}&qs=${encodeQs}`;

    this.worker.postMessage({
      type: 'connect',
      data: { url: wsUrl },
    });

    this.worker.onmessage = (event) => {
      this.handleWorkerMessage(event.data);
    };
  }

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

  disconnect() {
    if (this.worker) {
      this.worker.terminate();
      this.worker = null;
    }
    this.connected = false;
    this.hierarchical.clear();
  }

  handleWorkerMessage(message) {
    const { type, data } = message;

    try {
      switch (type) {
        case 'status':
          this.handleStatus(data);
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
        case 'reply':
          this.handleReply(data);
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
    } else if (data.status === 'disconnected') {
      this.connected = false;
      this.info('Disconnected from WebSocket');
    }
    this.dispatchArizonaEvent('status', data);
  }

  handleHtmlPatch(data) {
    const { patch, isInitial } = data;

    if (isInitial) {
      this.handleInitialRender(patch);
    } else {
      this.handleDiffPatch(patch);
    }
  }

  handleInitialRender(patch) {
    this.debug('Applying initial render');

    // Store structure in client-side hierarchical instance for debugging
    if (patch.structure) {
      this.hierarchical.initialize(patch.structure);
    }

    // Apply initial HTML to DOM
    this.applyHtmlPatch(patch);
  }

  handleDiffPatch(patch) {
    this.debug('Applying diff patch');

    // Apply HTML patch to DOM
    this.applyHtmlPatch(patch);
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
          // Skip update if nodes are identical
          return !fromEl.isEqualNode(toEl);
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
    this.dispatchArizonaEvent('error', data);
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

  handleReply(data) {
    this.debug('WebSocket reply:', data);
    this.dispatchArizonaEvent('reply', data);
  }

  handleRedirect(data) {
    this.info('Redirecting to:', data.url);
    this.dispatchArizonaEvent('redirect', data);

    // Perform the redirect with safe option access
    window.open(data.url, data.options?.target, data.options?.window_features);
  }

  handleUnknownMessage(message) {
    this.warning('Unknown worker message:', message);
  }

  dispatchArizonaEvent(eventType, eventData) {
    document.dispatchEvent(
      new CustomEvent('arizonaEvent', {
        detail: { type: eventType, data: eventData },
      })
    );
  }

  isConnected() {
    return this.connected;
  }

  // Logging methods aligned with Erlang logger levels
  error(message, ...args) {
    // Always show errors (critical for debugging)
    console.error(`[Arizona] ${message}`, ...args);
  }

  warning(message, ...args) {
    if (this.logLevel >= LOG_LEVELS.warning) {
      console.warn(`[Arizona] ${message}`, ...args);
    }
  }

  info(message, ...args) {
    if (this.logLevel >= LOG_LEVELS.info) {
      console.log(`[Arizona] ${message}`, ...args);
    }
  }

  debug(message, ...args) {
    if (this.logLevel >= LOG_LEVELS.debug) {
      console.log(`[Arizona] ${message}`, ...args);
    }
  }
}
