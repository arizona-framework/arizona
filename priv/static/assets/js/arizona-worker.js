"use strict"

const state = {
    fulfilled: false,
    params: {},
    socket: null,
    eventQueue: [],
}

// Messages from client.
self.onmessage = function(e) {
    console.log("[WebWorker] client sent:", e.data)

    if (typeof e.data !== "object" || !e.data.event) {
        console.error("Invalid Arizona WebWorker message", e)
        return
    }

    const {event, payload} = e.data
    switch(event) {
        case "connect":
            connect(payload)
            break
        case "disconnect":
            disconnect()
            break
        default:
            sendMsgToServer([event, payload])
    }
}

function connect(params) {
    return new Promise((resolve) => {
        const url = genSocketUrl(params)
        const socket = new WebSocket(url)

        state.params = params
        state.socket = socket

        socket.onopen = function () {
            state.fulfilled = true

            state.eventQueue.forEach(sendMsgToServer)
            state.eventQueue.length = 0

            console.log("[WebSocket] connected", state)
            sendMsgToClient("connect")

            resolve()
        }

        socket.onclose = function (e) {
            console.log("[WebSocket] disconnected", e)
            sendMsgToClient("disconnect")
        }

        // Messages from server.
        socket.onmessage = function (e) {
            console.log("[WebSocket] msg:", e.data)
        }
    })
}

function disconnect() {
    state.socket.close()
}

function reconnect() {
    console.log("[WebSocket] reconnecting...")
    sendMsgToClient("reconnecting")
    const params = {
        ...state.params,
        reconnecting: "true",
    }
    connect(params)
}

function sendMsgToClient(event, payload) {
    self.postMessage({event, payload})
}

function sendMsgToServer([event, payload]) {
    if (!state.socket) {
        state.eventQueue.push([event, payload])
        state.fulfilled
            ? console.warn("[WebSocket] not ready to send messages")
            : reconnect()
    } else if (isSocketOpen()) {
        state.socket.send(payload
            ? JSON.stringify([event, payload], state)
            : JSON.stringify(event)
        )
    } else {
        state.fulfilled && state.eventQueue.push([event, payload])
        isSocketClosed() && reconnect()
	}
}

function isSocketOpen() {
    return state.socket.readyState === WebSocket.OPEN
}

function isSocketClosed() {
    return state.socket.readyState === WebSocket.CLOSED
}

function genSocketUrl(params) {
    const protocol = "ws"
    const host = location.host
    const uri = "/websocket"
    const qs = `?${Object
        .keys(params)
        .map(key => `${key}=${encodeURIComponent(params[key])}`)
        .join("&")
    }`
    return `${protocol}://${host}${uri}${qs}`
}

