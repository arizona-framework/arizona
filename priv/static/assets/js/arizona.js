/* global morphdom */
'use strict';

globalThis['arizona'] = (() => {
  // --------------------------------------------------------------------
  // API function definitions
  // --------------------------------------------------------------------

  function connect(params = {}, callback, opts) {
    const ref = generateRef()
    if (typeof callback === "function") {
      _subscribe(ref, 'connected', callback, opts)
    }
    const searchParams = Object.fromEntries([
      ...new URLSearchParams(window.location.search),
    ]);
    const queryParams = {
      ...searchParams,
      ...params,
      session_id: sessionId,
      path: location.pathname,
    };
    _sendMsgToWorker('connect', { ref, queryParams });
  }

  function send(eventName, viewId, payload, callback, opts) {
    let ref
    if (typeof callback === "function") {
      ref = generateRef()
      _subscribe(ref, eventName, callback, opts)
    }
    _sendMsgToWorker('event', [ref, viewId, eventName, payload]);
  }

  function channel(topic, viewId) {
    let joined = false
    const _topicSubscribers = []

    function on(eventName, callback, opts) {
      const ref = generateRef()
      eventName = JSON.stringify({ topic, eventName })
      const unsubscribe = _subscribe(ref, eventName, callback, opts)
      _topicSubscribers.push(unsubscribe)
      return this
    }

    function join(params) {
      return new Promise((resolve, reject) => {
        if (joined) reject("alreadyJoined")

        const ref = generateRef()
        _subscribe(ref, 'join', ([status, payload]) => {
          joined = status === "ok"
          joined ? resolve(payload) : reject(payload)
        }, { once: true })
        _sendMsgToWorker('join', [ref, viewId, topic, params])
        resolve(_leave)
      })
    }

    function _leave() {
      _topicSubscribers.forEach((unsubscribe) => unsubscribe())
      _topicSubscribers.length = 0
    }

    return Object.freeze({ on, join })
  }

  // --------------------------------------------------------------------
  // Private functions
  // --------------------------------------------------------------------

  function generateRef() {
    return Math.random().toString(36).substring(2, 9);
  }

  function _subscribe(ref, eventName, callback, opts = {}) {
    if (
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

    eventSubs.set(ref, { ref, callback, opts });
    subscribers.set(eventName, eventSubs);
    unsubscribers.set(ref, eventName);

    console.table({
      action: 'subscribed',
      eventName,
      ref,
      subscribers,
      unsubscribers,
    });

    return function() {
      _unsubscribe(ref);
    };
  }

  function _unsubscribe(ref) {
    const eventName = unsubscribers.get(ref);
    if (!eventName) return;
    const eventSubs = subscribers.get(eventName);
    if (!eventSubs) return;
    eventSubs.delete(ref);
    eventSubs.size
      ? subscribers.set(eventName, eventSubs)
      : subscribers.delete(eventName);
    unsubscribers.delete(ref);
    console.table({
      action: 'unsubscribed',
      eventName,
      ref,
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

  const sessionId = Math.random()
  const worker = new Worker('assets/js/arizona/worker.js');
  const subscribers = new Map();
  const unsubscribers = new Map();

  worker.addEventListener('message', function(e) {
    console.info('[WebWorker] msg:', e.data);

    const { ref, viewId, eventName, payload } = e.data;
    switch (eventName) {
      case 'patch': {
        const elem = document.getElementById(viewId);
        morphdom(elem, payload, {
          onBeforeElUpdated: (from, to) => !from.isEqualNode(to),
        });
        break
      }
      case 'leave': {
        _unsubscribe(payload)
        return
      }
    }

    const eventSubs = subscribers.get(eventName)
    if (!(eventSubs instanceof Map)) return

    if (typeof ref === "string" && ref.length) {
      const subs = eventSubs.get(ref)
      if (!subs?.callback) return
      subs.callback(payload)
    } else {
      eventSubs.forEach(function({ id, callback, opts }) {
        callback(payload);
        opts.once && _unsubscribe(id);
      });
    }
  });

  worker.addEventListener('error', function(e) {
    console.error('[WebWorker] error:', e);
  });

  return Object.freeze({ connect, send, channel });
})();
