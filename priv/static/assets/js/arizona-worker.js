/* global patch */
'use strict';

const state = {
  queryParams: {},
  socket: null,
  views: [],
};

self.importScripts('/assets/js/arizona/patch.js');

// Messages from client
self.onmessage = function (e) {
  const { data: msg } = e;

  console.log('[WebWorker] client sent:', msg);

  if (typeof msg !== 'object' || !msg.subject) {
    console.error('[WebWorker] invalid message format:', msg);
    return;
  }

  switch (msg.subject) {
    case 'connect':
      connect(msg.attachment);
      break;
    default:
      sendMsgToServer(msg);
  }
};

function connect(queryParams) {
  return new Promise((resolve) => {
    const url = genSocketUrl(queryParams);
    const socket = new WebSocket(url);

    state.queryParams = queryParams;
    state.socket = socket;

    socket.onopen = function () {
      console.log('[WebSocket] connected:', state);
      sendMsgToClient('connect');

      resolve();
    };

    socket.onclose = function (e) {
      console.log('[WebSocket] disconnected:', e);
    };

    // Messages from server
    socket.onmessage = function (e) {
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
      sendMsgToClient('patch', [viewId, html]);
      break;
    }
    default: {
      sendMsgToClient(eventName, payload);
      break;
    }
  }
}

function sendMsgToClient(eventName, payload) {
  self.postMessage({ eventName, payload });
}

function sendMsgToServer(msg) {
  state.socket.send(JSON.stringify(msg));
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
