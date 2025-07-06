// Arizona WebWorker for WebSocket connection
class ArizonaWebSocketWorker {
  constructor() {
    this.socket = null;
    this.connected = false;
    this.messageQueue = [];

    self.onmessage = (event) => {
      const { type, data } = event.data;

      switch (type) {
        case 'connect':
          this.connect(data.url);
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

  connect(url) {
    if (this.connected) return;

    this.socket = new WebSocket(url);

    this.socket.onopen = () => {
      this.connected = true;
      this.postMessage({ type: 'status', data: { status: 'connected' } });
      this.flushMessageQueue();
    };

    this.socket.onmessage = (event) => {
      const message = JSON.parse(event.data);
      this.postMessage({ type: 'message', data: message });
    };

    this.socket.onclose = () => {
      this.connected = false;
      this.postMessage({ type: 'status', data: { status: 'disconnected' } });
    };

    this.socket.onerror = (error) => {
      this.postMessage({ type: 'error', data: { error: error.toString() } });
    };
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
  }

  postMessage(data) {
    self.postMessage(data);
  }
}

new ArizonaWebSocketWorker();
