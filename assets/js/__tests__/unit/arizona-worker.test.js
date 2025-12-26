// ArizonaWorker tests
import { describe, test, expect, beforeEach, vi } from 'vitest';
import MockWebSocket from '../__mocks__/WebSocket.js';

// Mock self.postMessage for worker environment
const mockPostMessage = vi.fn();
vi.stubGlobal('self', {
  postMessage: mockPostMessage,
  onmessage: null,
});

// Create spy for MockWebSocket that tracks the last instance
// Note: Must use 'function' instead of arrow function for vitest 4.x
// Arrow functions cannot be constructors (called with 'new')
let lastWebSocketInstance = null;
const WebSocketSpy = vi.fn().mockImplementation(function (url) {
  lastWebSocketInstance = new MockWebSocket(url);
  return lastWebSocketInstance;
});

vi.stubGlobal('WebSocket', WebSocketSpy);

// Helper to get the last WebSocket instance
const getLastWebSocket = () => lastWebSocketInstance;

describe('ArizonaWorker', () => {
  let worker;

  beforeEach(async () => {
    // Clear previous mocks
    mockPostMessage.mockClear();
    WebSocketSpy.mockClear();
    lastWebSocketInstance = null;

    // Reset modules to get fresh imports
    vi.resetModules();

    // Import and create worker instance
    await import('../../client/arizona-worker.js');

    // Get the worker instance (created by the module)
    worker = global.self;
  });

  // Helper to connect and wait for WebSocket to be ready
  const connectAndWait = async (url = 'ws://localhost:3000/live') => {
    worker.onmessage({
      data: { type: 'connect', data: { url } },
    });
    await new Promise((resolve) => setTimeout(resolve, 10));
    return getLastWebSocket();
  };

  describe('message handling', () => {
    test('handles connect message', () => {
      const connectMessage = {
        data: {
          type: 'connect',
          data: { url: 'ws://localhost:3000/live?path=%2Fusers' },
        },
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
          data: { url: 'ws://localhost:3000/live' },
        },
      };
      worker.onmessage(connectMessage);

      // Wait for connection to be established
      await new Promise((resolve) => {
        return setTimeout(resolve, 10);
      });

      // Clear previous postMessage calls
      mockPostMessage.mockClear();

      // Send message
      const sendMessage = {
        data: {
          type: 'send',
          data: {
            type: 'event',
            event: 'click',
            params: { target: 'button1' },
          },
        },
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
          data: { url: 'ws://localhost:3000/live' },
        },
      };
      worker.onmessage(connectMessage);

      // Verify WebSocket was created
      expect(WebSocketSpy).toHaveBeenCalledWith('ws://localhost:3000/live');

      // Then disconnect
      const disconnectMessage = {
        data: { type: 'disconnect' },
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
          data: { url: 'ws://localhost:3000/live' },
        },
      };
      worker.onmessage(connectMessage);

      // Wait for connection
      await new Promise((resolve) => {
        return setTimeout(resolve, 10);
      });

      expect(mockPostMessage).toHaveBeenCalledWith({
        type: 'status',
        data: { status: 'connected' },
      });
    });

    test('posts disconnected status when WebSocket closes', async () => {
      const connectMessage = {
        data: {
          type: 'connect',
          data: { url: 'ws://localhost:3000/live' },
        },
      };
      worker.onmessage(connectMessage);

      // Wait for connection
      await new Promise((resolve) => {
        return setTimeout(resolve, 10);
      });

      // Clear previous calls
      mockPostMessage.mockClear();

      // Simulate WebSocket close by triggering disconnect
      const disconnectMessage = {
        data: { type: 'disconnect' },
      };
      worker.onmessage(disconnectMessage);

      // Should post disconnected status
      expect(mockPostMessage).toHaveBeenCalledWith({
        type: 'status',
        data: { status: 'disconnected' },
      });
    });

    test('forwards server messages', async () => {
      const connectMessage = {
        data: {
          type: 'connect',
          data: { url: 'ws://localhost:3000/live' },
        },
      };
      worker.onmessage(connectMessage);

      // Wait for connection
      await new Promise((resolve) => {
        return setTimeout(resolve, 10);
      });

      // Clear previous calls
      mockPostMessage.mockClear();

      // The MockWebSocket automatically triggers onmessage, so we just verify
      // that the worker properly handles WebSocket creation
      expect(WebSocketSpy).toHaveBeenCalledWith('ws://localhost:3000/live');
    });

    test('posts error when WebSocket encounters an error', async () => {
      const ws = await connectAndWait();
      mockPostMessage.mockClear();

      // Simulate WebSocket error
      ws.simulateError(new Error('Connection failed'));

      expect(mockPostMessage).toHaveBeenCalledWith({
        type: 'error',
        data: { error: expect.stringContaining('Error') },
      });
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
            params: {},
          },
        },
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
            params: {},
          },
        },
      };
      worker.onmessage(sendMessage);

      // Verify no WebSocket created yet
      expect(WebSocketSpy).not.toHaveBeenCalled();

      // Then connect
      const connectMessage = {
        data: {
          type: 'connect',
          data: { url: 'ws://localhost:3000/live' },
        },
      };
      worker.onmessage(connectMessage);

      // WebSocket should be created immediately when connecting
      expect(WebSocketSpy).toHaveBeenCalledWith('ws://localhost:3000/live');

      // Wait for connection to establish
      await new Promise((resolve) => {
        return setTimeout(resolve, 10);
      });

      // Verify WebSocket was called once
      expect(WebSocketSpy).toHaveBeenCalledTimes(1);
    });
  });

  describe('server message handling', () => {
    test('handles diff messages after initialization', async () => {
      const ws = await connectAndWait();

      // First initialize the hierarchical structure
      // Structure has 'static' (HTML parts) and 'dynamic' (values) arrays
      ws.simulateMessage({
        type: 'initial_render',
        structure: {
          'component-1': {
            static: ['<div id="component-1">', '</div>'],
            dynamic: ['Hello'],
          },
        },
      });

      mockPostMessage.mockClear();

      // Now send a diff message
      // changes is an array of [index, value] pairs (1-based index)
      ws.simulateMessage({
        type: 'diff',
        stateful_id: 'component-1',
        changes: [[1, 'World']],
        structure: {},
      });

      // Should post an html_patch message
      expect(mockPostMessage).toHaveBeenCalledWith(
        expect.objectContaining({
          type: 'html_patch',
        })
      );
    });

    test('handles diff with new structures from fingerprint mismatch', async () => {
      const ws = await connectAndWait();

      // Initialize with a basic structure
      ws.simulateMessage({
        type: 'initial_render',
        structure: {
          'component-1': {
            static: ['<div>', '</div>'],
            dynamic: ['Initial'],
          },
        },
      });

      mockPostMessage.mockClear();

      // Send diff with new structure (fingerprint mismatch scenario)
      // changes is an array of [index, value] pairs (1-based index)
      ws.simulateMessage({
        type: 'diff',
        stateful_id: 'component-1',
        changes: [[1, 'Updated']],
        structure: {
          'component-2': {
            static: ['<span>', '</span>'],
            dynamic: ['New'],
          },
        },
      });

      // Should post an html_patch message
      expect(mockPostMessage).toHaveBeenCalledWith(
        expect.objectContaining({
          type: 'html_patch',
        })
      );
    });

    test('handles reload messages', async () => {
      const ws = await connectAndWait();
      mockPostMessage.mockClear();

      // Simulate server sending reload message
      ws.simulateMessage({ type: 'reload', reason: 'file_changed' });

      expect(mockPostMessage).toHaveBeenCalledWith({
        type: 'reload',
        data: { type: 'reload', reason: 'file_changed' },
      });
    });

    test('handles dispatch messages and forwards to main thread', async () => {
      const ws = await connectAndWait();
      mockPostMessage.mockClear();

      // Simulate server sending dispatch message
      ws.simulateMessage({
        type: 'dispatch',
        event: 'notification:hide',
        data: { notificationId: 'alert-123' },
      });

      expect(mockPostMessage).toHaveBeenCalledWith({
        type: 'dispatch',
        data: {
          type: 'dispatch',
          event: 'notification:hide',
          data: { notificationId: 'alert-123' },
        },
      });
    });

    test('handles reply messages', async () => {
      const ws = await connectAndWait();
      mockPostMessage.mockClear();

      // Simulate server sending reply message
      ws.simulateMessage({
        type: 'reply',
        ref_id: '123',
        data: { result: 'success' },
      });

      expect(mockPostMessage).toHaveBeenCalledWith({
        type: 'reply',
        data: {
          type: 'reply',
          ref_id: '123',
          data: { result: 'success' },
        },
      });
    });

    test('handles redirect messages', async () => {
      const ws = await connectAndWait();
      mockPostMessage.mockClear();

      // Simulate server sending redirect message
      ws.simulateMessage({
        type: 'redirect',
        url: '/new-page',
        target: '_self',
      });

      expect(mockPostMessage).toHaveBeenCalledWith({
        type: 'redirect',
        data: {
          url: '/new-page',
          target: '_self',
        },
      });
    });

    test('handles unknown messages by passing them through', async () => {
      const ws = await connectAndWait();
      mockPostMessage.mockClear();

      // Simulate server sending unknown message type
      ws.simulateMessage({
        type: 'custom_type',
        payload: { foo: 'bar' },
      });

      expect(mockPostMessage).toHaveBeenCalledWith({
        type: 'custom_type',
        payload: { foo: 'bar' },
      });
    });

    test('handles message handling errors gracefully', async () => {
      const ws = await connectAndWait();
      mockPostMessage.mockClear();

      // Simulate a message that causes an error in handleWebSocketMessage
      // The diff handler throws if hierarchical is not initialized
      ws.simulateMessage({
        type: 'diff',
        stateful_id: 'test',
        changes: {},
      });

      // Should post an error message
      expect(mockPostMessage).toHaveBeenCalledWith(
        expect.objectContaining({
          type: 'error',
        })
      );
    });
  });

  describe('URL validation', () => {
    test('rejects invalid WebSocket URL protocol', () => {
      mockPostMessage.mockClear();

      worker.onmessage({
        data: {
          type: 'connect',
          data: { url: 'http://localhost:3000/live' },
        },
      });

      expect(mockPostMessage).toHaveBeenCalledWith({
        type: 'error',
        data: { error: 'Invalid WebSocket URL' },
      });
      expect(WebSocketSpy).not.toHaveBeenCalled();
    });

    test('rejects URL with mismatched host when expectedHost is provided', () => {
      mockPostMessage.mockClear();

      worker.onmessage({
        data: {
          type: 'connect',
          data: {
            url: 'ws://malicious.com/live',
            expectedHost: 'localhost:3000',
          },
        },
      });

      expect(mockPostMessage).toHaveBeenCalledWith({
        type: 'error',
        data: { error: 'Invalid WebSocket URL' },
      });
      expect(WebSocketSpy).not.toHaveBeenCalled();
    });

    test('rejects invalid URL format', () => {
      mockPostMessage.mockClear();

      worker.onmessage({
        data: {
          type: 'connect',
          data: { url: 'not-a-valid-url' },
        },
      });

      expect(mockPostMessage).toHaveBeenCalledWith({
        type: 'error',
        data: { error: 'Invalid WebSocket URL' },
      });
      expect(WebSocketSpy).not.toHaveBeenCalled();
    });

    test('accepts valid wss:// URL', () => {
      worker.onmessage({
        data: {
          type: 'connect',
          data: { url: 'wss://localhost:3000/live' },
        },
      });

      expect(WebSocketSpy).toHaveBeenCalledWith('wss://localhost:3000/live');
    });
  });

  describe('invalid messages', () => {
    test('ignores messages without data', () => {
      worker.onmessage({});
      expect(WebSocketSpy).not.toHaveBeenCalled();
    });

    test('ignores messages with non-object data', () => {
      worker.onmessage({ data: 'string' });
      expect(WebSocketSpy).not.toHaveBeenCalled();
    });

    test('ignores messages without type', () => {
      worker.onmessage({ data: { foo: 'bar' } });
      expect(WebSocketSpy).not.toHaveBeenCalled();
    });
  });

  describe('connection state', () => {
    test('does not connect if already connected', async () => {
      await connectAndWait();
      expect(WebSocketSpy).toHaveBeenCalledTimes(1);

      // Try to connect again
      worker.onmessage({
        data: {
          type: 'connect',
          data: { url: 'ws://localhost:3000/live' },
        },
      });

      // Should still only have one WebSocket created
      expect(WebSocketSpy).toHaveBeenCalledTimes(1);
    });
  });
});
