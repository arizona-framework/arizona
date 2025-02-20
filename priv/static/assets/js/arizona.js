"use strict";

globalThis["arizona"] = (() => {
  const worker = new Worker("assets/js/arizona/worker.js");
  const subscribers = new Map();
  const unsubscribers = new Map();

  // API functions

  function connect(params) {
    params = {
      ...params,
      path: location.pathname,
    };
    send(undefined, "connect", params);
  }

  function send(viewId, event, payload) {
    sendMsgToWorker.bind(this)(viewId, event, payload);
  }

  function subscribe(eventName, callback, opts = {}) {
    if (typeof eventName !== "string" ||
      typeof callback !== "function" ||
      (typeof opts !== "object" || Array.isArray(opts))
    ) {
      console.error("[Arizona] invalid subscribe data:", { eventName, callback, opts })
      return
    }

    let eventSubs = subscribers.get(eventName);
    if (!eventSubs) eventSubs = new Map();

    const id = Math.random();
    eventSubs.set(id, { id, callback, opts });
    subscribers.set(eventName, eventSubs);
    unsubscribers.set(id, eventName);

    console.table({
      action: "subscribed",
      eventName,
      id,
      subscribers,
      unsubscribers,
    });

    return function() {
      unsubscribe(id);
    };
  }

  function unsubscribe(id) {
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
      action: "unsubscribed",
      eventName,
      id,
      subscribers,
      unsubscribers,
    });
  }

  // Private functions

  function sendMsgToWorker(viewId, event, payload) {
    worker.postMessage({ viewId, event, payload });
  }

  // Init

  worker.addEventListener("message", function(e) {
    console.log("[WebWorker] msg:", e.data);

    const { event, payload } = e.data;
    subscribers.get(event)?.forEach(function({ id, callback, opts }) {
      callback(payload);
      opts.once && unsubscribe(id);
    });
  });

  worker.addEventListener("error", function(e) {
    console.error("[WebWorker] error:", e);
  });

  return Object.freeze({ connect, send, subscribe, unsubscribe });
})();
