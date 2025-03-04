/* global patch */
'use strict';

const state = {
  queryParams: {},
  socket: null,
  views: [],
  eventQueue: [],
};

self.importScripts('/assets/js/arizona/patch.js');

// Messages from client
self.onmessage = function(e) {
  const { data: msg } = e;

  console.info('[WebWorker] client sent:', msg);

  if (typeof msg !== 'object' || !msg.subject) {
    console.error('[WebWorker] invalid message format:', msg);
    return;
  }

  switch (msg.subject) {
    case 'connect': {
      const { ref, queryParams } = msg.attachment
      connect(ref, queryParams);
      break;
    }
    default:
      sendMsgToServer(msg);
  }
};

function connect(ref, queryParams) {
  return new Promise((resolve) => {
    const url = genSocketUrl(queryParams);
    const socket = new WebSocket(url);

    state.queryParams = queryParams;
    state.socket = socket;

    socket.onopen = function() {
      console.info('[WebSocket] connected:', state);

      const queuedEvents = [...state.eventQueue]
      state.eventQueue.length = 0;
      queuedEvents.forEach(sendMsgToServer);

      sendMsgToClient(ref, undefined, 'connected', true);

      resolve();
    };

    socket.onclose = function(e) {
      console.info('[WebSocket] disconnected:', e);
      sendMsgToClient(ref, undefined, 'connected', false);
    };

    // Messages from server
    socket.onmessage = function(e) {
      console.info('[WebSocket] msg:', e.data);
      const data = JSON.parse(e.data);
      Array.isArray(data) ? data.forEach(handleEvent) : handleEvent(data);
    };
  });
}

function handleEvent(data) {
  const eventName = data[0];
  const [ref, viewId, payload] = data[1];
  switch (eventName) {
    case 'init': {
      state.views = payload;
      break;
    }
    case 'patch': {
      const rendered = state.views[viewId];
      const html = patch(rendered, payload);
      sendMsgToClient(ref, viewId, 'patch', html);
      break;
    }
    default: {
      sendMsgToClient(ref, viewId, eventName, payload);
      break;
    }
  }
}

function sendMsgToClient(ref, viewId, eventName, payload) {
  self.postMessage({ ref, viewId, eventName, payload });
}

function sendMsgToServer({ subject, attachment }) {
  if (!state.socket) {
    state.eventQueue.push({ subject, attachment });
    console.warn("[WebSocket] not ready to send messages")
  } else if (isSocketOpen()) {
    state.socket.send(JSON.stringify([subject, attachment]));
  }
}

function isSocketOpen() {
  return state.socket.readyState === WebSocket.OPEN;
}

function genSocketUrl(queryParams) {
  const proto = 'ws';
  const host = location.host;
  const uri = '/websocket';
  const queryString = `?${Object.keys(queryParams)
    .map((key) => `${key}=${encodeURIComponent(queryParams[key])}`)
    .join('&')}`;
  return `${proto}://${host}${uri}${queryString}`;
}
