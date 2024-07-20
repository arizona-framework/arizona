/*global morphdom*/
'use strict';

globalThis['arizona'] = (() => {
  // --------------------------------------------------------------------
  // API function definitions
  // --------------------------------------------------------------------

  function on(eventName, listener) {
    listeners[eventName] = listeners[eventName] = [];
    listeners[eventName].push(listener);

    logTable('on', eventName);
  }

  function once(eventName, listener) {
    const onceWrapper = (payload) => {
      listener(payload);
      off(eventName, onceWrapper);
    };
    on(eventName, onceWrapper);

    logTable('once', eventName);
  }

  function off(eventName, listener) {
    listeners[eventName] = (listeners[eventName] || []).filter(
      (needle) => needle !== listener,
    );

    logTable('off', eventName);
  }

  function emit(eventName, payload) {
    (listeners[eventName] || []).forEach((listener) => {
      listener(payload);
    });
  }

  function send(eventName, payload, listener) {
    sendToWorker.bind(this)(eventName, payload, listener);
  }

  function connect(listener) {
    send('connect', { path: location.pathname }, listener);
  }

  // --------------------------------------------------------------------
  // Private
  // --------------------------------------------------------------------

  const listeners = {};
  const worker = new Worker('assets/js/arizona-worker.js');

  function logTable(action, eventName) {
    console.table({ action, eventName, listeners });
  }

  function logDebug(scope, data) {
    console.debug(`${scope}: ${data}`);
  }

  function logError(scope, data) {
    console.error(`${scope}: ${data}`);
  }

  function sendToWorker(eventName, payload, listener) {
    let target;
    if (this instanceof HTMLElement) {
      target = document.querySelector(this.getAttribute('arizona-target'));
    }
    target = target?.getAttribute('arizona-id') || '[0]';

    listener && once(eventName, listener);

    worker.postMessage({ eventName, payload, target });
  }

  function applyPatch(elem, html) {
    morphdom(elem, html, {
      onBeforeElUpdated: (from, to) => !from.isEqualNode(to),
    });
  }

  // --------------------------------------------------------------------
  // Namespace initialization
  // --------------------------------------------------------------------

  worker.addEventListener('message', (event) => {
    // `event` is:
    // {
    //   data: {
    //     eventName: <string>,
    //     payload: {
    //       target: <array>,
    //       html: <string>
    //     },
    //   }
    // }
    logDebug('[WebWorker] message', event.data);

    const { eventName, payload } = event.data;
    if (eventName === 'patch') {
      const { target, html } = payload;
      const elem = document.querySelector(
        `[arizona-id="${JSON.stringify(target)}"]`,
      );
      applyPatch(elem, html);
    }

    emit(eventName, payload);
  });

  worker.addEventListener('error', (event) =>
    logError('[WebWorker] error', event),
  );

  return { on, once, off, send, connect };
})();
