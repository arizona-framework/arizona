// Arizona Client API
export class ArizonaClient {
  constructor() {
    this.worker = null;
    this.connected = false;
  }

  connect(wsPath = '/live') {
    if (this.connected) return;

    this.worker = new Worker('assets/js/arizona-worker.js', { type: 'module' });

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
      const { type, data } = event.data;
      if (type === 'status' && data.status === 'connected') {
        this.connected = true;
      }
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
  }
}
