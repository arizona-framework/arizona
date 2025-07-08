// ArizonaClient tests
import { describe, test, expect, beforeEach, afterEach, vi } from 'vitest';
import ArizonaClient from './arizona.js';
import MockWorker from './__mocks__/Worker.js';
import MockWebSocket from './__mocks__/WebSocket.js';

// Mock global APIs
vi.stubGlobal('Worker', MockWorker);
vi.stubGlobal('WebSocket', MockWebSocket);
vi.stubGlobal('window', {
  location: {
    protocol: 'http:',
    host: 'localhost:3000',
    pathname: '/users',
  },
});

describe('ArizonaClient', () => {
  let client;

  beforeEach(() => {
    client = new ArizonaClient();
    // Clear any stored messages from previous tests
    if (global.Worker.prototype.clearPostedMessages) {
      global.Worker.prototype.clearPostedMessages();
    }
  });

  afterEach(() => {
    if (client.worker) {
      client.disconnect();
    }
  });

  describe('constructor', () => {
    test('initializes with correct default state', () => {
      expect(client.worker).toBeNull();
      expect(client.connected).toBe(false);
    });
  });

  describe('connect()', () => {
    test('creates worker and generates correct WebSocket URL', () => {
      client.connect();

      expect(client.worker).toBeInstanceOf(Worker);
      expect(client.worker.scriptURL).toBe('/assets/js/arizona-worker.min.js');
      expect(client.worker.options).toEqual({ type: 'module' });

      const postedMessage = client.worker.getLastPostedMessage();
      expect(postedMessage).toEqual({
        type: 'connect',
        data: { url: 'ws://localhost:3000/live/websocket?path=%2Fusers' },
      });
    });

    test('uses custom WebSocket path', () => {
      client.connect({ wsPath: '/custom/ws' });

      const postedMessage = client.worker.getLastPostedMessage();
      expect(postedMessage.data.url).toBe('ws://localhost:3000/custom/ws?path=%2Fusers');
    });

    test('uses wss:// for HTTPS', () => {
      window.location.protocol = 'https:';
      client.connect();

      const postedMessage = client.worker.getLastPostedMessage();
      expect(postedMessage.data.url).toBe('wss://localhost:3000/live/websocket?path=%2Fusers');

      // Reset for other tests
      window.location.protocol = 'http:';
    });

    test('does not create new worker if already connected', () => {
      client.connect();
      const firstWorker = client.worker;

      client.connected = true;
      client.connect();

      expect(client.worker).toBe(firstWorker);
    });

    test('sets connected state when worker reports connected status', (done) => {
      client.connect();

      // Simulate worker connection success
      setTimeout(() => {
        expect(client.connected).toBe(true);
        done();
      }, 10);
    });
  });

  describe('sendEvent()', () => {
    beforeEach(() => {
      client.connect();
      client.connected = true;
    });

    test('sends event when connected', () => {
      client.sendEvent('click', { target: 'button1' });

      const messages = client.worker.getAllPostedMessages();
      const eventMessage = messages.find((msg) => {
        return msg.type === 'send';
      });

      expect(eventMessage).toEqual({
        type: 'send',
        data: {
          type: 'event',
          event: 'click',
          params: { target: 'button1' },
        },
      });
    });

    test('uses empty params by default', () => {
      client.sendEvent('submit');

      const messages = client.worker.getAllPostedMessages();
      const eventMessage = messages.find((msg) => {
        return msg.type === 'send';
      });

      expect(eventMessage.data.params).toEqual({});
    });

    test('does not send when not connected', () => {
      client.connected = false;
      client.worker.clearPostedMessages();

      client.sendEvent('click');

      const messages = client.worker.getAllPostedMessages();
      expect(messages.length).toBe(0);
    });
  });

  describe('disconnect()', () => {
    test('terminates worker and resets state', () => {
      client.connect();
      client.connected = true;

      const worker = client.worker;
      client.disconnect();

      expect(worker.terminated).toBe(true);
      expect(client.worker).toBeNull();
      expect(client.connected).toBe(false);
    });

    test('handles disconnect when no worker exists', () => {
      expect(() => {
        return client.disconnect();
      }).not.toThrow();
      expect(client.connected).toBe(false);
    });
  });
});
