/* global patch */
"use strict";

const state = {
  params: {},
  socket: null,
  rendered: [],
};

self.importScripts("/assets/js/arizona/patch.js");

// Messages from client
self.onmessage = function (e) {
  const { data: msg } = e;

  console.log("[WebWorker] client sent:", msg);

  if (typeof msg !== "object" || !msg.event) {
    console.error("[WebWorker] invalid message format:", msg);
    return;
  }

  const { viewId, event, payload } = msg;
  switch (event) {
    case "connect":
      connect(payload);
      break;
    default:
      if (!viewId) {
        console.error("[WebWorker] missing view ID");
        return;
      }

      sendMsgToServer([viewId, event, payload]);
  }
};

function connect(params) {
  return new Promise((resolve) => {
    const url = genSocketUrl(params);
    const socket = new WebSocket(url);

    state.params = params;
    state.socket = socket;

    socket.onopen = function () {
      console.log("[WebSocket] connected:", state);
      sendMsgToClient("connect");

      resolve();
    };

    socket.onclose = function (e) {
      console.log("[WebSocket] disconnected:", e);
    };

    // Messages from server
    socket.onmessage = function (e) {
      console.log("[WebSocket] msg:", e.data);
      const data = JSON.parse(e.data);
      Array.isArray(data) ? data.forEach(handleEvent) : handleEvent(data);
    };
  });
}

function handleEvent(data) {
  const event = data[0];
  const payload = data[1];
  switch (event) {
    case "init": {
      state.rendered = payload;
      break;
    }
    case "patch": {
      const [viewId, diff] = payload;
      const html = patch(state.rendered, diff);
      sendMsgToClient("patch", [viewId, html]);
      break;
    }
    default: {
      sendMsgToClient(event, payload);
      break;
    }
  }
}

function sendMsgToClient(event, payload) {
  self.postMessage({ event, payload });
}

function sendMsgToServer([viewId, event, payload]) {
  state.socket.send(
    payload
      ? JSON.stringify([viewId, event, payload], state)
      : JSON.stringify([viewId, event]),
  );
}

function genSocketUrl(params) {
  const proto = "ws";
  const host = location.host;
  const uri = "/websocket";
  const qs = `?${Object.keys(params)
    .map((key) => `${key}=${encodeURIComponent(params[key])}`)
    .join("&")}`;
  return `${proto}://${host}${uri}${qs}`;
}
