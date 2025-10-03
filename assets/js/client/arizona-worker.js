// Import ArizonaHierarchical for client-side structure management
import ArizonaHierarchical from './arizona-hierarchical.js';

// Arizona Worker for WebSocket connection with hierarchical rendering
class ArizonaWorker {
  constructor() {
    this.socket = null;
    this.connected = false;
    this.messageQueue = [];
    this.hierarchical = new ArizonaHierarchical();

    self.onmessage = (event) => {
      // Validate message structure
      // Note: This is a dedicated worker, so messages can only come from the parent window
      // Origin checking is not applicable for dedicated workers
      if (!event.data || typeof event.data !== 'object' || !event.data.type) {
        return;
      }

      const { type, data } = event.data;

      switch (type) {
        case 'connect':
          this.connect(data.url, data.expectedHost);
          break;
        case 'send':
          this.sendMessage(data);
          break;
        case 'disconnect':
          this.disconnect();
          break;
      }
    };
  }

  connect(url, expectedHost) {
    if (this.connected) return;

    // Validate WebSocket URL to prevent client-side request forgery
    if (!this.isValidWebSocketUrl(url, expectedHost)) {
      this.postMessage({
        type: 'error',
        data: { error: 'Invalid WebSocket URL' },
      });
      return;
    }

    this.socket = new WebSocket(url);

    this.socket.onopen = () => {
      this.connected = true;
      this.postMessage({ type: 'status', data: { status: 'connected' } });
      this.flushMessageQueue();
    };

    this.socket.onmessage = (event) => {
      const message = JSON.parse(event.data);
      this.handleWebSocketMessage(message);
    };

    this.socket.onclose = () => {
      this.connected = false;
      this.postMessage({ type: 'status', data: { status: 'disconnected' } });
    };

    this.socket.onerror = (error) => {
      this.postMessage({ type: 'error', data: { error: error.toString() } });
    };
  }

  isValidWebSocketUrl(url, expectedHost) {
    try {
      const parsedUrl = new URL(url);

      // Only allow WebSocket protocols
      if (parsedUrl.protocol !== 'ws:' && parsedUrl.protocol !== 'wss:') {
        return false;
      }

      // Validate hostname matches expected host to prevent request forgery
      // The expectedHost comes from window.location.host in the main thread
      if (expectedHost && parsedUrl.host !== expectedHost) {
        return false;
      }

      // Prevent path traversal attacks in the pathname
      if (parsedUrl.pathname.includes('../') || parsedUrl.pathname.includes('..\\')) {
        return false;
      }

      return true;
    } catch {
      return false;
    }
  }

  sendMessage(data) {
    const message = JSON.stringify(data);

    if (this.connected && this.socket.readyState === WebSocket.OPEN) {
      this.socket.send(message);
    } else {
      this.messageQueue.push(message);
    }
  }

  flushMessageQueue() {
    while (this.messageQueue.length > 0) {
      const message = this.messageQueue.shift();
      if (this.socket.readyState === WebSocket.OPEN) {
        this.socket.send(message);
      } else {
        this.messageQueue.unshift(message);
        break;
      }
    }
  }

  disconnect() {
    if (this.socket) {
      this.socket.close();
      this.socket = null;
    }
    this.connected = false;
    this.hierarchical.clear();
  }

  handleWebSocketMessage(message) {
    try {
      switch (message.type) {
        case 'initial_render':
          this.handleInitialRender(message);
          break;
        case 'diff':
          this.handleDiff(message);
          break;
        case 'reload':
          this.handleReload(message);
          break;
        case 'dispatch':
          this.handleDispatch(message);
          break;
        case 'reply':
          this.handleReply(message);
          break;
        case 'redirect':
          this.handleRedirect(message);
          break;
        default:
          this.handleUnknownMessage(message);
      }
    } catch (error) {
      this.postMessage({
        type: 'error',
        data: { error: `Message handling failed: ${error.message}` },
      });
    }
  }

  handleInitialRender(message) {
    // Initialize hierarchical structure
    this.hierarchical.initialize(message.structure);
  }

  handleDiff(message) {
    if (!this.hierarchical.isInitialized()) {
      throw new Error('Hierarchical structure not initialized');
    }

    // Merge new structures from fingerprint mismatches
    if (message.structure && Object.keys(message.structure).length > 0) {
      this.hierarchical.mergeStructures(message.structure);
    }

    // Apply diff to hierarchical structure
    this.hierarchical.applyDiff(message.stateful_id, message.changes);

    // Create HTML patch
    const patch = this.hierarchical.createPatch(message.stateful_id);

    // Send to main thread for DOM application
    this.postMessage({
      type: 'html_patch',
      data: { patch },
    });
  }

  handleReload(message) {
    // Send reload message to main thread
    this.postMessage({
      type: 'reload',
      data: message,
    });
  }

  handleDispatch(message) {
    // Send dispatch event message to main thread
    this.postMessage({
      type: 'dispatch',
      data: message,
    });
  }

  handleReply(message) {
    // Send reply message to main thread
    this.postMessage({
      type: 'reply',
      data: message,
    });
  }

  handleRedirect(message) {
    // Send redirect message to main thread
    this.postMessage({
      type: 'redirect',
      data: {
        url: message.url,
        target: message.target,
      },
    });
  }

  handleUnknownMessage(message) {
    // Pass through other message types unchanged
    this.postMessage(message);
  }

  postMessage(data) {
    self.postMessage(data);
  }
}

new ArizonaWorker();
