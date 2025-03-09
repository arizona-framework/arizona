/* global morphdom */
'use strict';

globalThis['arizona'] = (() => {
  // --------------------------------------------------------------------
  // API function definitions
  // --------------------------------------------------------------------

  function connect(params = {}, callback, opts) {
    const ref = generateRef();
    if (typeof callback === 'function') {
      _subscribe(ref, 'connected', callback, opts);
    }
    const searchParams = Object.fromEntries([
      ...new URLSearchParams(window.location.search),
    ]);
    const queryParams = {
      ...searchParams,
      ...params,
      path: location.pathname,
    };
    _sendMsgToWorker('connect', { ref, queryParams });
  }

  function send(eventName, viewId, payload, callback, opts) {
    let ref;
    if (typeof callback === 'function') {
      ref = generateRef();
      _subscribe(ref, eventName, callback, opts);
    }
    _sendMsgToWorker('event', [ref, viewId, eventName, payload]);
  }

  function event(eventName, viewId) {
    let joined = false;
    const _members = [];

    function join(payload) {
      return new Promise((resolve, reject) => {
        if (joined) reject('alreadyJoined');

        const ref = generateRef();
        _subscribe(
          ref,
          'join',
          ([status, payload]) => {
            joined = status === 'ok';
            joined ? resolve(payload) : reject(payload);
          },
          { once: true },
        );
        _sendMsgToWorker('join', [ref, viewId, eventName, payload]);
      });
    }

    function handle(callback, opts) {
      const ref = generateRef();
      const unsubscribe = _subscribe(ref, eventName, callback, opts);
      _members.push(unsubscribe);
      return this;
    }

    function leave() {
      _members.forEach((unsubscribe) => unsubscribe());
      _members.length = 0;
    }

    return Object.freeze({ join, handle, leave });
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
    const members = subscribers.get(eventName);
    if (!members) return;
    members.delete(ref);
    members.size
      ? subscribers.set(eventName, members)
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
        break;
      }
      case 'leave': {
        _unsubscribe(payload);
        return;
      }
    }

    const members = subscribers.get(eventName);
    if (!(members instanceof Map)) return;

    if (typeof ref === 'string' && ref.length) {
      const member = members.get(ref);
      if (!member?.callback) return;
      member.callback(payload);
    } else {
      members.forEach(function({ ref, callback, opts }) {
        callback(payload);
        opts.once && _unsubscribe(ref);
      });
    }
  });

  worker.addEventListener('error', function(e) {
    console.error('[WebWorker] error:', e);
  });

  return Object.freeze({ connect, send, event });
})();
