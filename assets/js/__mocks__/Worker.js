// Mock Worker for Jest testing
class MockWorker {
  constructor(scriptURL, options) {
    this.scriptURL = scriptURL;
    this.options = options;
    this.onmessage = null;
    this.onerror = null;
    this.terminated = false;
    
    // Store posted messages for testing
    this.postedMessages = [];
  }

  postMessage(data) {
    if (this.terminated) {
      throw new Error('Worker has been terminated');
    }
    
    this.postedMessages.push(data);
    
    // Simulate worker processing
    setTimeout(() => {
      this.simulateWorkerResponse(data);
    }, 0);
  }

  terminate() {
    this.terminated = true;
  }

  // Test helpers
  simulateMessage(data) {
    if (this.onmessage && !this.terminated) {
      this.onmessage({ data });
    }
  }

  simulateError(error) {
    if (this.onerror && !this.terminated) {
      this.onerror(error);
    }
  }

  simulateWorkerResponse(originalMessage) {
    const { type, data } = originalMessage;
    
    switch (type) {
      case 'connect':
        this.simulateMessage({
          type: 'status',
          data: { status: 'connected' }
        });
        break;
      case 'send':
        // Echo back the sent message
        this.simulateMessage({
          type: 'message',
          data: data
        });
        break;
      case 'disconnect':
        this.simulateMessage({
          type: 'status',
          data: { status: 'disconnected' }
        });
        break;
    }
  }

  getLastPostedMessage() {
    return this.postedMessages[this.postedMessages.length - 1];
  }

  getAllPostedMessages() {
    return [...this.postedMessages];
  }

  clearPostedMessages() {
    this.postedMessages = [];
  }
}

export default MockWorker;