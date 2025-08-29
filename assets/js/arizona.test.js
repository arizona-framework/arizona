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
        data: { url: 'ws://localhost:3000/live/websocket?path=%2Fusers&qs=' },
      });
    });

    test('uses custom WebSocket path', () => {
      client.connect({ wsPath: '/custom/ws' });

      const postedMessage = client.worker.getLastPostedMessage();
      expect(postedMessage.data.url).toBe('ws://localhost:3000/custom/ws?path=%2Fusers&qs=');
    });

    test('uses wss:// for HTTPS', () => {
      window.location.protocol = 'https:';
      client.connect();

      const postedMessage = client.worker.getLastPostedMessage();
      expect(postedMessage.data.url).toBe('wss://localhost:3000/live/websocket?path=%2Fusers&qs=');

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

    test('sets connected state when worker reports connected status', async () => {
      client.connect();

      // Give the mock worker time to process and respond
      await new Promise((resolve) => {
        return setTimeout(resolve, 50);
      });

      expect(client.connected).toBe(true);
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

    test('sends to arizona_view handle_event callback (no stateful_id)', () => {
      client.sendEvent('click', { target: 'button1' });

      const messages = client.worker.getAllPostedMessages();
      const eventMessage = messages.find((msg) => {
        return msg.type === 'send';
      });

      expect(eventMessage.data).not.toHaveProperty('stateful_id');
      expect(eventMessage.data.event).toBe('click');
    });

    test('does not send when not connected', () => {
      client.connected = false;
      client.worker.clearPostedMessages();

      client.sendEvent('click');

      const messages = client.worker.getAllPostedMessages();
      expect(messages.length).toBe(0);
    });
  });

  describe('sendEventTo()', () => {
    beforeEach(() => {
      client.connect();
      client.connected = true;
    });

    test('sends event directly to arizona_stateful component', () => {
      client.sendEventTo('counter_component', 'increment', { amount: 1 });

      const messages = client.worker.getAllPostedMessages();
      const eventMessage = messages.find((msg) => {
        return msg.type === 'send';
      });

      expect(eventMessage).toEqual({
        type: 'send',
        data: {
          type: 'event',
          stateful_id: 'counter_component',
          event: 'increment',
          params: { amount: 1 },
        },
      });
    });

    test('uses empty params by default when targeting stateful component', () => {
      client.sendEventTo('form_component', 'submit');

      const messages = client.worker.getAllPostedMessages();
      const eventMessage = messages.find((msg) => {
        return msg.type === 'send';
      });

      expect(eventMessage).toEqual({
        type: 'send',
        data: {
          type: 'event',
          stateful_id: 'form_component',
          event: 'submit',
          params: {},
        },
      });
    });

    test('always includes stateful_id for arizona_stateful targeting', () => {
      client.sendEventTo('widget_123', 'update', { value: 42 });

      const messages = client.worker.getAllPostedMessages();
      const eventMessage = messages.find((msg) => {
        return msg.type === 'send';
      });

      expect(eventMessage.data).toHaveProperty('stateful_id', 'widget_123');
    });

    test('does not send when not connected', () => {
      client.connected = false;
      client.worker.clearPostedMessages();

      client.sendEventTo('component_id', 'action');

      const messages = client.worker.getAllPostedMessages();
      expect(messages.length).toBe(0);
    });

    test('separates concerns from sendEvent by targeting specific stateful components', () => {
      // sendEvent goes to arizona_view
      client.sendEvent('page_action', { data: 'view_level' });

      // sendEventTo goes to arizona_stateful
      client.sendEventTo('stateful_comp', 'component_action', { data: 'component_level' });

      const messages = client.worker.getAllPostedMessages();
      const viewMessage = messages.find((msg) => msg.data.event === 'page_action');
      const statefulMessage = messages.find((msg) => msg.data.event === 'component_action');

      expect(viewMessage.data).not.toHaveProperty('stateful_id');
      expect(statefulMessage.data).toHaveProperty('stateful_id', 'stateful_comp');
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
