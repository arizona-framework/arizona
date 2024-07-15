/*global morphdom*/
"use strict";

globalThis["arizona"] = (() => {
  // --------------------------------------------------------------------
  // API function definitions
  // --------------------------------------------------------------------

  function subscribe(eventName, callback) {
    let eventSubs = subscribers.get(eventName);
    if (!eventSubs) eventSubs = new Map();
    const id = Math.random();
    eventSubs.set(id, { id, callback });
    subscribers.set(eventName, eventSubs);
    unsubscribers.set(id, eventName);
    console.table({
      action: "subscribed",
      eventName,
      id,
      subscribers,
      unsubscribers,
    });
    return function () {
      unsubscribe(id);
    };
  }

  function subscribeOnce(eventName, callback) {
    const onceWrapper = (...args) => {
      callback.apply(this, args);
      unsubscribe(eventName, onceWrapper);
    };
    subscribe(eventName, onceWrapper);
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

  function send(event, payloadOrCallback, callbackOrOpts, optsOrNull) {
    typeof payloadOrCallback === "function"
      ? sendMsgToWorker.bind(this)(
          event,
          undefined,
          payloadOrCallback,
          callbackOrOpts,
        )
      : sendMsgToWorker.bind(this)(
          event,
          payloadOrCallback,
          callbackOrOpts,
          optsOrNull,
        );
  }

  function connect(params, callback, opts) {
    params = {
      ...params,
      path: location.pathname,
    };
    send("connect", params, callback, opts);
  }

  // --------------------------------------------------------------------
  // Private
  // --------------------------------------------------------------------

  const subscribers = new Map();
  const unsubscribers = new Map();
  const worker = new Worker("assets/js/arizona-worker.js");

  function sendMsgToWorker(event, payload, callback, opts = {}) {
    if (!opts.target && this instanceof HTMLElement) {
      opts.target = this.getAttribute("arizona-target");
    }
    const target = document.querySelector(opts.target);
    const arizonaId = target?.getAttribute("arizona-id") || "[0]";
    callback && subscribeOnce(event, callback);
    worker.postMessage({ target: arizonaId, event, payload });
  }

  function applyPatch(elem, html) {
    morphdom(elem, html, {
      // Can I make morphdom blaze through the DOM tree even faster? Yes.
      // @see https://github.com/patrick-steele-idem/morphdom#can-i-make-morphdom-blaze-through-the-dom-tree-even-faster-yes
      onBeforeElUpdated: function (fromEl, toEl) {
        // spec - https://dom.spec.whatwg.org/#concept-node-equals
        if (fromEl.isEqualNode(toEl)) {
          return false;
        }

        return true;
      },
    });
  }

  // --------------------------------------------------------------------
  // Namespace initialization
  // --------------------------------------------------------------------

  worker.addEventListener("message", function (e) {
    console.log("[WebWorker] msg:", e.data);
    const { event, payload } = e.data;
    switch (event) {
      case "patch": {
        const { target, html } = payload;
        const elem = document.querySelector(
          `[arizona-id="${JSON.stringify(target)}"]`,
        );
        applyPatch(elem, html);
        break;
      }
    }
    subscribers.get(event)?.forEach(function ({ id, callback }) {
      callback(payload);
    });
  });

  worker.addEventListener("error", function (e) {
    console.error("[WebWorker] error:", e);
  });

  return { subscribe, subscribeOnce, unsubscribe, send, connect };
})();
