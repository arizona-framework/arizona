/* global patch */
'use strict';

const state = {
  params: {},
  socket: null,
  views: [],
};

self.importScripts('/assets/js/arizona/patch.js');

// Messages from client
self.onmessage = function(e) {
  const { data: msg } = e;

  console.log('[WebWorker] client sent:', msg);

  if (typeof msg !== 'object' || !msg.eventName) {
    console.error('[WebWorker] invalid message format:', msg);
    return;
  }

  const { viewId, eventName, payload } = msg;
  switch (eventName) {
    case 'connect':
      connect(payload);
      break;
    default:
      if (!viewId) {
        console.error('[WebWorker] missing view ID');
        return;
      }

      sendToServer([viewId, eventName, payload]);
  }
};

function connect(params) {
  return new Promise((resolve) => {
    const url = genSocketUrl(params);
    const socket = new WebSocket(url);

    state.params = params;
    state.socket = socket;

    socket.onopen = function() {
      console.log('[WebSocket] connected:', state);
      sendToClient('connect');

      resolve();
    };

    socket.onclose = function(e) {
      console.log('[WebSocket] disconnected:', e);
    };

    // Messages from server
    socket.onmessage = function(e) {
      console.log('[WebSocket] msg:', e.data);
      const data = JSON.parse(e.data);
      Array.isArray(data) ? data.forEach(handleEvent) : handleEvent(data);
    };
  });
}

function handleEvent(data) {
  const eventName = data[0];
  const payload = data[1];
  switch (eventName) {
    case 'init': {
      state.views = payload;
      break;
    }
    case 'patch': {
      const [viewId, diff] = payload;
      const rendered = state.views[viewId];
      const html = patch(rendered, diff);
      sendToClient('patch', [viewId, html]);
      break;
    }
    default: {
      sendToClient(eventName, payload);
      break;
    }
  }
}

function sendToClient(eventName, payload) {
  self.postMessage({ eventName, payload });
}

function sendToServer([viewId, eventName, payload]) {
  state.socket.send(
    payload
      ? JSON.stringify([viewId, eventName, payload], state)
      : JSON.stringify([viewId, eventName]),
  );
}

function genSocketUrl(params) {
  const proto = 'ws';
  const host = location.host;
  const uri = '/websocket';
  const qs = `?${Object.keys(params)
    .map((key) => `${key}=${encodeURIComponent(params[key])}`)
    .join('&')}`;
  return `${proto}://${host}${uri}${qs}`;
}
