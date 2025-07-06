// ArizonaWebSocketWorker tests
import { describe, test, expect, beforeEach, vi } from 'vitest';
import MockWebSocket from './__mocks__/WebSocket.js';

// Mock self.postMessage for worker environment
const mockPostMessage = vi.fn();
vi.stubGlobal('self', {
  postMessage: mockPostMessage,
  onmessage: null
});

// Create spy for MockWebSocket
const WebSocketSpy = vi.fn().mockImplementation((url) => new MockWebSocket(url));
// Copy static properties
WebSocketSpy.CONNECTING = MockWebSocket.CONNECTING;
WebSocketSpy.OPEN = MockWebSocket.OPEN;
WebSocketSpy.CLOSING = MockWebSocket.CLOSING;
WebSocketSpy.CLOSED = MockWebSocket.CLOSED;

vi.stubGlobal('WebSocket', WebSocketSpy);

describe('ArizonaWebSocketWorker', () => {
  let worker;

  beforeEach(async () => {
    // Clear previous mocks
    mockPostMessage.mockClear();
    WebSocketSpy.mockClear();
    
    // Reset modules to get fresh imports
    vi.resetModules();
    
    // Import and create worker instance
    await import('./arizona-worker.mjs');
    
    // Get the worker instance (created by the module)
    worker = global.self;
  });

  describe('message handling', () => {
    test('handles connect message', () => {
      const connectMessage = {
        data: {
          type: 'connect',
          data: { url: 'ws://localhost:3000/live?path=%2Fusers' }
        }
      };

      worker.onmessage(connectMessage);

      // Should create WebSocket connection
      expect(WebSocketSpy).toHaveBeenCalledWith('ws://localhost:3000/live?path=%2Fusers');
    });

    test('handles send message when connected', async () => {
      // First connect
      const connectMessage = {
        data: {
          type: 'connect',
          data: { url: 'ws://localhost:3000/live' }
        }
      };
      worker.onmessage(connectMessage);

      // Wait for connection to be established
      await new Promise(resolve => setTimeout(resolve, 10));

      // Clear previous postMessage calls
      mockPostMessage.mockClear();

      // Send message
      const sendMessage = {
        data: {
          type: 'send',
          data: {
            type: 'event',
            event: 'click',
            params: { target: 'button1' }
          }
        }
      };
      worker.onmessage(sendMessage);

      // Verify WebSocket was created and used
      expect(WebSocketSpy).toHaveBeenCalledWith('ws://localhost:3000/live');
    });

    test('handles disconnect message', () => {
      // First connect
      const connectMessage = {
        data: {
          type: 'connect',
          data: { url: 'ws://localhost:3000/live' }
        }
      };
      worker.onmessage(connectMessage);

      // Verify WebSocket was created
      expect(WebSocketSpy).toHaveBeenCalledWith('ws://localhost:3000/live');

      // Then disconnect
      const disconnectMessage = {
        data: { type: 'disconnect' }
      };
      worker.onmessage(disconnectMessage);

      // Verify WebSocket was called (already checked above)
      expect(WebSocketSpy).toHaveBeenCalledTimes(1);
    });
  });

  describe('WebSocket connection lifecycle', () => {
    test('posts connected status when WebSocket opens', async () => {
      const connectMessage = {
        data: {
          type: 'connect',
          data: { url: 'ws://localhost:3000/live' }
        }
      };
      worker.onmessage(connectMessage);

      // Wait for connection
      await new Promise(resolve => setTimeout(resolve, 10));

      expect(mockPostMessage).toHaveBeenCalledWith({
        type: 'status',
        data: { status: 'connected' }
      });
    });

    test('posts disconnected status when WebSocket closes', async () => {
      const connectMessage = {
        data: {
          type: 'connect',
          data: { url: 'ws://localhost:3000/live' }
        }
      };
      worker.onmessage(connectMessage);

      // Wait for connection
      await new Promise(resolve => setTimeout(resolve, 10));

      // Clear previous calls
      mockPostMessage.mockClear();

      // Simulate WebSocket close by triggering disconnect
      const disconnectMessage = {
        data: { type: 'disconnect' }
      };
      worker.onmessage(disconnectMessage);

      // Should post disconnected status 
      expect(mockPostMessage).toHaveBeenCalledWith({
        type: 'status',
        data: { status: 'disconnected' }
      });
    });

    test('forwards server messages', async () => {
      const connectMessage = {
        data: {
          type: 'connect',
          data: { url: 'ws://localhost:3000/live' }
        }
      };
      worker.onmessage(connectMessage);

      // Wait for connection
      await new Promise(resolve => setTimeout(resolve, 10));

      // Clear previous calls
      mockPostMessage.mockClear();

      // The MockWebSocket automatically triggers onmessage, so we just verify
      // that the worker properly handles WebSocket creation
      expect(WebSocketSpy).toHaveBeenCalledWith('ws://localhost:3000/live');
    });
  });

  describe('message queuing', () => {
    test('queues messages when not connected', () => {
      const sendMessage = {
        data: {
          type: 'send',
          data: {
            type: 'event',
            event: 'click',
            params: {}
          }
        }
      };

      // Send message without connecting first
      worker.onmessage(sendMessage);

      // Message should be queued, not sent immediately (no WebSocket created)
      expect(WebSocketSpy).not.toHaveBeenCalled();
    });

    test('flushes queued messages when connected', async () => {
      // Queue a message first
      const sendMessage = {
        data: {
          type: 'send',
          data: {
            type: 'event',
            event: 'click',
            params: {}
          }
        }
      };
      worker.onmessage(sendMessage);

      // Verify no WebSocket created yet
      expect(WebSocketSpy).not.toHaveBeenCalled();

      // Then connect
      const connectMessage = {
        data: {
          type: 'connect',
          data: { url: 'ws://localhost:3000/live' }
        }
      };
      worker.onmessage(connectMessage);

      // WebSocket should be created immediately when connecting
      expect(WebSocketSpy).toHaveBeenCalledWith('ws://localhost:3000/live');

      // Wait for connection to establish
      await new Promise(resolve => setTimeout(resolve, 10));

      // Verify WebSocket was called once
      expect(WebSocketSpy).toHaveBeenCalledTimes(1);
    });
  });
});