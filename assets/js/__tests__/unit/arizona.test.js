// Arizona tests
import { describe, test, expect, beforeEach, afterEach, vi } from 'vitest';

// Mock morphdom at the top level
vi.mock('morphdom', () => ({
  default: vi.fn(),
}));

import Arizona from '../../client/arizona.js';
import { ArizonaConsoleLogger, LOG_LEVELS } from '../../client/logger/index.js';
import MockWorker from '../__mocks__/Worker.js';
import MockWebSocket from '../__mocks__/WebSocket.js';
import morphdom from 'morphdom';

// Mock global APIs
vi.stubGlobal('Worker', MockWorker);
vi.stubGlobal('WebSocket', MockWebSocket);

const mockDocument = {
  dispatchEvent: vi.fn(),
  getElementById: vi.fn().mockReturnValue(null),
  querySelector: vi.fn().mockReturnValue(null),
};
vi.stubGlobal('document', mockDocument);

vi.stubGlobal('window', {
  location: {
    protocol: 'http:',
    host: 'localhost:3000',
    pathname: '/users',
    reload: vi.fn(),
  },
});

describe('Arizona', () => {
  let client;
  let logger;

  beforeEach(() => {
    logger = new ArizonaConsoleLogger({ logLevel: LOG_LEVELS.debug });
    client = new Arizona({ logger });
    // Clear any stored messages from previous tests
    if (global.Worker.prototype.clearPostedMessages) {
      global.Worker.prototype.clearPostedMessages();
    }
    // Clear document event dispatch mock
    mockDocument.dispatchEvent.mockClear();
    mockDocument.querySelector.mockClear();
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
    test('creates inline worker and generates correct WebSocket URL', () => {
      client.connect('/live');

      expect(client.worker).toBeInstanceOf(Worker);
      // Inline workers have embedded script source rather than file URLs
      expect(client.worker.scriptURL).toBeDefined();
      expect(client.worker.options).toEqual({ type: 'module', name: undefined });

      const postedMessage = client.worker.getLastPostedMessage();
      expect(postedMessage).toEqual({
        type: 'connect',
        data: { url: 'ws://localhost:3000/live?path=%2Fusers&qs=' },
      });
    });

    test('uses custom WebSocket path', () => {
      client.connect('/custom/ws');

      const postedMessage = client.worker.getLastPostedMessage();
      expect(postedMessage.data.url).toBe('ws://localhost:3000/custom/ws?path=%2Fusers&qs=');
    });

    test('uses wss:// for HTTPS', () => {
      window.location.protocol = 'https:';
      client.connect('/live');

      const postedMessage = client.worker.getLastPostedMessage();
      expect(postedMessage.data.url).toBe('wss://localhost:3000/live?path=%2Fusers&qs=');

      // Reset for other tests
      window.location.protocol = 'http:';
    });

    test('does not create new worker if already connected', () => {
      client.connect('/live');
      const firstWorker = client.worker;

      client.connected = true;
      client.connect('/live');

      expect(client.worker).toBe(firstWorker);
    });

    test('sets connected state when worker reports connected status', async () => {
      client.connect('/live');

      // Give the mock worker time to process and respond
      await new Promise((resolve) => {
        return setTimeout(resolve, 50);
      });

      expect(client.connected).toBe(true);
    });
  });

  describe('pushEvent()', () => {
    beforeEach(() => {
      client.connect('/live');
      client.connected = true;
    });

    test('sends event when connected', () => {
      client.pushEvent('click', { target: 'button1' });

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
      client.pushEvent('submit');

      const messages = client.worker.getAllPostedMessages();
      const eventMessage = messages.find((msg) => {
        return msg.type === 'send';
      });

      expect(eventMessage.data.params).toEqual({});
    });

    test('sends to arizona_view handle_event callback (no stateful_id)', () => {
      client.pushEvent('click', { target: 'button1' });

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

      client.pushEvent('click');

      const messages = client.worker.getAllPostedMessages();
      expect(messages.length).toBe(0);
    });
  });

  describe('pushEventTo()', () => {
    beforeEach(() => {
      client.connect('/live');
      client.connected = true;
    });

    test('sends event directly to arizona_stateful component', () => {
      client.pushEventTo('counter_component', 'increment', { amount: 1 });

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
      client.pushEventTo('form_component', 'submit');

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
      client.pushEventTo('widget_123', 'update', { value: 42 });

      const messages = client.worker.getAllPostedMessages();
      const eventMessage = messages.find((msg) => {
        return msg.type === 'send';
      });

      expect(eventMessage.data).toHaveProperty('stateful_id', 'widget_123');
    });

    test('does not send when not connected', () => {
      client.connected = false;
      client.worker.clearPostedMessages();

      client.pushEventTo('component_id', 'action');

      const messages = client.worker.getAllPostedMessages();
      expect(messages.length).toBe(0);
    });

    test('separates concerns from pushEvent by targeting specific stateful components', () => {
      // pushEvent goes to arizona_view
      client.pushEvent('page_action', { data: 'view_level' });

      // pushEventTo goes to arizona_stateful
      client.pushEventTo('stateful_comp', 'component_action', { data: 'component_level' });

      const messages = client.worker.getAllPostedMessages();
      const viewMessage = messages.find((msg) => {
        return msg.data.event === 'page_action';
      });
      const statefulMessage = messages.find((msg) => {
        return msg.data.event === 'component_action';
      });

      expect(viewMessage.data).not.toHaveProperty('stateful_id');
      expect(statefulMessage.data).toHaveProperty('stateful_id', 'stateful_comp');
    });
  });

  describe('disconnect()', () => {
    test('terminates worker and resets state', () => {
      client.connect('/live');
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

  describe('custom event dispatching', () => {
    test('does not dispatch events for reload messages', () => {
      client.handleReload({});

      expect(mockDocument.dispatchEvent).not.toHaveBeenCalled();
      expect(window.location.reload).toHaveBeenCalled();
    });

    test('does not dispatch events for HTML patch messages', () => {
      const patchData = { patch: { statefulId: 'test', html: '<div>test</div>' } };
      client.handleHtmlPatch(patchData);

      expect(mockDocument.dispatchEvent).not.toHaveBeenCalled();
    });

    test('handles dispatch messages and emits events', () => {
      const callback = vi.fn();
      client.on('customClick', callback);

      const dispatchData = {
        event: 'customClick',
        data: { userId: 123, action: 'increment' },
      };

      client.handleDispatch(dispatchData);

      expect(callback).toHaveBeenCalledWith({ userId: 123, action: 'increment' });
    });

    test('handles dispatch with namespaced component events', () => {
      const callback = vi.fn();
      client.on('counter_123:incr', callback);

      const dispatchData = {
        event: 'counter_123:incr',
        data: { count: 5 },
      };

      client.handleDispatch(dispatchData);

      expect(callback).toHaveBeenCalledWith({ count: 5 });
    });

    test('handles dispatch with multiple subscribers', () => {
      const callback1 = vi.fn();
      const callback2 = vi.fn();
      const callback3 = vi.fn();

      client.on('dataLoaded', callback1);
      client.on('dataLoaded', callback2);
      client.on('dataLoaded', callback3);

      client.handleDispatch({
        event: 'dataLoaded',
        data: { status: 'success' },
      });

      expect(callback1).toHaveBeenCalledWith({ status: 'success' });
      expect(callback2).toHaveBeenCalledWith({ status: 'success' });
      expect(callback3).toHaveBeenCalledWith({ status: 'success' });
    });
  });

  describe('Event subscription system', () => {
    test('on() subscribes to events and returns unsubscribe function', () => {
      const callback = vi.fn();
      const unsubscribe = client.on('connected', callback);

      expect(typeof unsubscribe).toBe('function');
      expect(client.eventListeners.has('connected')).toBe(true);
      expect(client.eventListeners.get('connected').has(callback)).toBe(true);
    });

    test('on() with non-function callback returns noop', () => {
      const unsubscribe = client.on('connected', 'not-a-function');

      expect(typeof unsubscribe).toBe('function');
      expect(client.eventListeners.has('connected')).toBe(false);
    });

    test('emit() calls all subscribed listeners with event data', () => {
      const callback1 = vi.fn();
      const callback2 = vi.fn();
      const eventData = { status: 'connected' };

      client.on('connected', callback1);
      client.on('connected', callback2);
      client.emit('connected', eventData);

      expect(callback1).toHaveBeenCalledWith(eventData);
      expect(callback2).toHaveBeenCalledWith(eventData);
      expect(callback1).toHaveBeenCalledTimes(1);
      expect(callback2).toHaveBeenCalledTimes(1);
    });

    test('emit() handles listener errors gracefully', () => {
      const error = new Error('Listener error');
      const failingCallback = vi.fn(() => {
        throw error;
      });
      const successCallback = vi.fn();

      client.on('connected', failingCallback);
      client.on('connected', successCallback);
      client.emit('connected', { status: 'connected' });

      expect(failingCallback).toHaveBeenCalled();
      expect(successCallback).toHaveBeenCalled();
    });

    test('emit() with no listeners does nothing', () => {
      expect(() => client.emit('nonexistent', {})).not.toThrow();
    });

    test('off() unsubscribes callback from event', () => {
      const callback = vi.fn();
      client.on('connected', callback);
      client.off('connected', callback);

      expect(client.eventListeners.has('connected')).toBe(false);
      client.emit('connected', {});
      expect(callback).not.toHaveBeenCalled();
    });

    test('off() cleans up empty listener sets', () => {
      const callback = vi.fn();
      client.on('connected', callback);
      expect(client.eventListeners.has('connected')).toBe(true);

      client.off('connected', callback);
      expect(client.eventListeners.has('connected')).toBe(false);
    });

    test('unsubscribe function returned by on() works correctly', () => {
      const callback = vi.fn();
      const unsubscribe = client.on('connected', callback);

      client.emit('connected', {});
      expect(callback).toHaveBeenCalledTimes(1);

      unsubscribe();
      client.emit('connected', {});
      expect(callback).toHaveBeenCalledTimes(1);
    });

    test('handleStatus emits connected event', () => {
      const callback = vi.fn();
      client.on('connected', callback);

      client.handleStatus({ status: 'connected' });

      expect(callback).toHaveBeenCalledWith({ status: 'connected' });
      expect(client.connected).toBe(true);
    });

    test('handleStatus emits disconnected event', () => {
      const callback = vi.fn();
      client.on('disconnected', callback);

      client.handleStatus({ status: 'disconnected' });

      expect(callback).toHaveBeenCalledWith({ status: 'disconnected' });
      expect(client.connected).toBe(false);
    });

    test('multiple listeners can subscribe to same event', () => {
      const callback1 = vi.fn();
      const callback2 = vi.fn();
      const callback3 = vi.fn();

      client.on('connected', callback1);
      client.on('connected', callback2);
      client.on('connected', callback3);

      client.emit('connected', {});

      expect(callback1).toHaveBeenCalledTimes(1);
      expect(callback2).toHaveBeenCalledTimes(1);
      expect(callback3).toHaveBeenCalledTimes(1);
    });

    test('removing one listener does not affect others', () => {
      const callback1 = vi.fn();
      const callback2 = vi.fn();

      client.on('connected', callback1);
      client.on('connected', callback2);
      client.off('connected', callback1);

      client.emit('connected', {});

      expect(callback1).not.toHaveBeenCalled();
      expect(callback2).toHaveBeenCalledTimes(1);
    });

    test('handleWorkerError emits error event', () => {
      const callback = vi.fn();
      client.on('error', callback);

      const errorData = { error: 'Something went wrong' };
      client.handleWorkerError(errorData);

      expect(callback).toHaveBeenCalledWith(errorData);
    });

    test('once() subscribes to event that fires only once', () => {
      const callback = vi.fn();
      client.once('dataLoaded', callback);

      // First emit - callback should be called
      client.emit('dataLoaded', { data: 'first' });
      expect(callback).toHaveBeenCalledTimes(1);
      expect(callback).toHaveBeenCalledWith({ data: 'first' });

      // Second emit - callback should NOT be called again
      client.emit('dataLoaded', { data: 'second' });
      expect(callback).toHaveBeenCalledTimes(1);
    });

    test('once() with non-function callback returns noop', () => {
      const unsubscribe = client.once('connected', 'not-a-function');

      expect(typeof unsubscribe).toBe('function');
      expect(client.eventListeners.has('connected')).toBe(false);
    });

    test('once() returns unsubscribe function that works before event fires', () => {
      const callback = vi.fn();
      const unsubscribe = client.once('dataLoaded', callback);

      // Unsubscribe before event fires
      unsubscribe();

      // Emit event - callback should NOT be called
      client.emit('dataLoaded', { data: 'test' });
      expect(callback).not.toHaveBeenCalled();
    });

    test('removeAllListeners() removes all listeners for specific event', () => {
      const callback1 = vi.fn();
      const callback2 = vi.fn();
      const callback3 = vi.fn();

      client.on('event1', callback1);
      client.on('event1', callback2);
      client.on('event2', callback3);

      expect(client.eventListeners.has('event1')).toBe(true);
      expect(client.eventListeners.has('event2')).toBe(true);

      client.removeAllListeners('event1');

      expect(client.eventListeners.has('event1')).toBe(false);
      expect(client.eventListeners.has('event2')).toBe(true);

      // Verify event1 listeners are removed
      client.emit('event1', {});
      expect(callback1).not.toHaveBeenCalled();
      expect(callback2).not.toHaveBeenCalled();

      // Verify event2 listener still works
      client.emit('event2', {});
      expect(callback3).toHaveBeenCalled();
    });

    test('removeAllListeners() without argument removes all listeners', () => {
      const callback1 = vi.fn();
      const callback2 = vi.fn();
      const callback3 = vi.fn();

      client.on('event1', callback1);
      client.on('event2', callback2);
      client.on('event3', callback3);

      expect(client.eventListeners.size).toBe(3);

      client.removeAllListeners();

      expect(client.eventListeners.size).toBe(0);

      // Verify no listeners are called
      client.emit('event1', {});
      client.emit('event2', {});
      client.emit('event3', {});
      expect(callback1).not.toHaveBeenCalled();
      expect(callback2).not.toHaveBeenCalled();
      expect(callback3).not.toHaveBeenCalled();
    });
  });

  describe('data-arizona-update="false" third-party framework integration', () => {
    let targetElement;

    beforeEach(() => {
      // Create a target element for morphing
      targetElement = {
        id: 'test-component',
        dispatchEvent: vi.fn(),
      };
      mockDocument.getElementById.mockReturnValue(targetElement);

      // Clear morphdom mock before each test
      vi.mocked(morphdom).mockClear();
    });

    afterEach(() => {
      mockDocument.getElementById.mockReturnValue(null);
    });

    test('skips DOM updates for elements with data-arizona-update="false"', () => {
      let callback;
      vi.mocked(morphdom).mockImplementation((target, html, options) => {
        callback = options.onBeforeElUpdated;
        return target;
      });

      client.applyHtmlPatch({
        statefulId: 'test-component',
        html: '<div>Updated content</div>',
      });

      // Test the core feature: skip elements with data-arizona-update="false"
      const fromEl = { isEqualNode: vi.fn() };
      const skipEl = { dataset: { arizonaUpdate: 'false' } };
      expect(callback(fromEl, skipEl)).toBe(false);
      expect(fromEl.isEqualNode).not.toHaveBeenCalled();

      // Test normal behavior: process other elements with equality check
      const normalEl = { dataset: {} };
      fromEl.isEqualNode.mockReturnValue(false);
      expect(callback(fromEl, normalEl)).toBe(true); // Different nodes = update
      expect(fromEl.isEqualNode).toHaveBeenCalledWith(normalEl);

      fromEl.isEqualNode.mockReturnValue(true);
      expect(callback(fromEl, normalEl)).toBe(false); // Same nodes = skip
    });

    test('handles missing target element gracefully', () => {
      mockDocument.getElementById.mockReturnValue(null);

      client.applyHtmlPatch({ statefulId: 'missing', html: '<div></div>' });

      expect(vi.mocked(morphdom)).not.toHaveBeenCalled();
    });

    test('handles morphdom errors gracefully', () => {
      const error = new Error('Morphdom failed');
      vi.mocked(morphdom).mockImplementation(() => {
        throw error;
      });

      expect(() => {
        client.applyHtmlPatch({ statefulId: 'test-component', html: '<div></div>' });
      }).not.toThrow();
    });
  });
});
