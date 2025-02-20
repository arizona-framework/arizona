"use strict";

globalThis["arizona"] = (() => {
  // Init

  const worker = new Worker("assets/js/arizona/worker.js");

  worker.addEventListener("message", function(e) {
    console.log("[WebWorker] msg:", e.data);
  });

  worker.addEventListener("error", function(e) {
    console.error("[WebWorker] error:", e);
  });

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

  // Private functions

  function sendMsgToWorker(viewId, event, payload) {
    worker.postMessage({ viewId, event, payload });
  }

  return Object.freeze({ connect, send });
})();
