// Import dependencies
import morphdom from 'morphdom';
import ArizonaHierarchical from './arizona-hierarchical.js';

// Arizona Client API
export default class ArizonaClient {
  constructor() {
    this.worker = null;
    this.connected = false;
    this.hierarchical = new ArizonaHierarchical();
  }

  connect(opts = {}) {
    if (this.connected) return;

    const wsPath = opts.wsPath || '/live/websocket';
    const workerPath = opts.workerPath || '/assets/js/arizona-worker.min.js';

    this.worker = new Worker(workerPath, { type: 'module' });

    const protocol = window.location.protocol === 'https:' ? 'wss:' : 'ws:';
    const host = window.location.host;
    const path = window.location.pathname;
    const encodedPath = encodeURIComponent(path);
    const wsUrl = `${protocol}//${host}${wsPath}?path=${encodedPath}`;

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
        case 'message':
          // Pass through other WebSocket messages
          this.handleWebSocketMessage(data);
          break;
        default:
          console.warn('[Arizona] Unknown worker message type:', type);
          break;
      }
    } catch (error) {
      console.error('[Arizona] Error handling worker message:', error);
    }
  }

  handleStatus(data) {
    if (data.status === 'connected') {
      this.connected = true;
      console.log('[Arizona] Connected to WebSocket');
    } else if (data.status === 'disconnected') {
      this.connected = false;
      console.log('[Arizona] Disconnected from WebSocket');
    }
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
    console.log('[Arizona] Applying initial render');

    // Store structure in client-side hierarchical instance for debugging
    if (patch.structure) {
      this.hierarchical.initialize(patch.structure);
    }

    // Apply initial HTML to DOM
    this.applyHtmlPatch(patch);
  }

  handleDiffPatch(patch) {
    console.log('[Arizona] Applying diff patch');

    // Apply HTML patch to DOM
    this.applyHtmlPatch(patch);
  }

  applyHtmlPatch(patch) {
    const target = document.getElementById(patch.statefulId);

    if (!target) {
      console.warn(`[Arizona] Target element not found: ${patch.statefulId}`);
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

      console.log(`[Arizona] Patch applied successfully`);

      // Trigger custom event for other code to listen to
      target.dispatchEvent(
        new CustomEvent('arizona:patched', {
          detail: { patch },
        })
      );
    } catch (error) {
      console.error('[Arizona] Error applying HTML patch:', error);
    }
  }

  handleWorkerError(data) {
    console.error('[Arizona Worker Error]:', data.error);
  }

  handleWebSocketMessage(data) {
    // Handle other WebSocket message types that aren't processed by the worker
    console.log('[Arizona] WebSocket message:', data);
  }

  // Utility methods
  getHierarchicalStructure() {
    return this.hierarchical.getStructure();
  }

  isConnected() {
    return this.connected;
  }
}
