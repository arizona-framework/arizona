/* global patch */
'use strict';

const state = {
  queryParams: {},
  socket: null,
  views: [],
};

self.importScripts('/assets/js/arizona/patch.js');

// Messages from client
self.onmessage = function(e) {
  const { data: msg } = e;

  console.log('[WebWorker] client sent:', msg);

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

function connect(id, queryParams) {
  return new Promise((resolve) => {
    const url = genSocketUrl(queryParams);
    const socket = new WebSocket(url);

    state.queryParams = queryParams;
    state.socket = socket;

    socket.onopen = function() {
      console.log('[WebSocket] connected:', state);
      sendMsgToClient(id, 'connected', true);

      resolve();
    };

    socket.onclose = function(e) {
      console.log('[WebSocket] disconnected:', e);
      sendMsgToClient(id, 'connected', false);
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
  state.socket.send(JSON.stringify([subject, attachment]));
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
