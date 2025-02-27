/* global morphdom */
'use strict';

globalThis['arizona'] = (() => {
  // --------------------------------------------------------------------
  // API function definitions
  // --------------------------------------------------------------------

  function connect(params = {}, callback, opts) {
    if (typeof callback === "function") {
      _subscribe("connect", callback, opts)
    }

    const searchParams = Object.fromEntries([...new URLSearchParams(window.location.search)]);
    const queryParams = {
      ...searchParams,
      ...params,
      path: location.pathname,
    };
    _sendMsgToWorker("connect", queryParams)
  }

  function on(eventName, callback, opts) {
    _subscribe(eventName, callback, opts)
  }

  function event(eventName) {
    function emmit(viewId, payload) {
      _sendMsgToWorker("event", [viewId, eventName, payload])
    }

    function subscribe(callback, opts) {
      _subscribe(eventName, callback, opts)
    }

    return Object.freeze({ emmit, subscribe })
  }

  // --------------------------------------------------------------------
  // Private functions
  // --------------------------------------------------------------------

  function _subscribe(eventName, callback, opts = {}) {
    if (
      typeof eventName !== 'string' ||
      typeof callback !== 'function' ||
      typeof opts !== 'object' ||
      Array.isArray(opts)
    ) {
      console.error('[Arizona] invalid subscribe data:', {
        eventName,
        callback,
        opts,
      });
      return;
    }

    let eventSubs = subscribers.get(eventName);
    if (!eventSubs) eventSubs = new Map();

    const id = Math.random();
    eventSubs.set(id, { id, callback, opts });
    subscribers.set(eventName, eventSubs);
    unsubscribers.set(id, eventName);

    console.table({
      action: 'subscribed',
      eventName,
      id,
      subscribers,
      unsubscribers,
    });

    return function() {
      _unsubscribe(id);
    };
  }

  function _unsubscribe(id) {
    const eventName = unsubscribers.get(id);
    if (!eventName) return;
    const eventSubs = subscribers.get(eventName);
    if (!eventSubs) return;
    eventSubs.delete(id);
    eventSubs.size
      ? subscribers.set(eventName, eventSubs)
      : subscribers.delete(eventName);
    unsubscribers.delete(id);
    console.table({
      action: 'unsubscribed',
      eventName,
      id,
      subscribers,
      unsubscribers,
    });
  }

  function _sendMsgToWorker(subject, attachment) {
    worker.postMessage({ subject, attachment });
  }

  // --------------------------------------------------------------------
  // Namespace initialization
  // --------------------------------------------------------------------

  const worker = new Worker('assets/js/arizona/worker.js');
  const subscribers = new Map();
  const unsubscribers = new Map();

  worker.addEventListener('message', function(e) {
    console.log('[WebWorker] msg:', e.data);

    const { eventName, payload } = e.data;
    switch (eventName) {
      case 'patch': {
        const [viewId, html] = payload;
        const elem = document.getElementById(viewId);
        morphdom(elem, html, {
          onBeforeElUpdated: (from, to) => !from.isEqualNode(to),
        });
      }
    }
    subscribers.get(eventName)?.forEach(function({ id, callback, opts }) {
      callback(payload);
      opts.once && _unsubscribe(id);
    });
  });

  worker.addEventListener('error', function(e) {
    console.error('[WebWorker] error:', e);
  });

  return Object.freeze({ connect, on, event });
})();
