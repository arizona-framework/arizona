"use strict"

const state = {
    fulfilled: false,
    params: {},
    socket: null,
    eventQueue: [],
    tree: [],
    types: [],
}

// Client incoming messages.
self.onmessage = function(e) {
    if (typeof e.data !== "object" || !e.data.event) {
        console.error("Invalid Arizona WebWorker message", e)
        return
    }
    const {event, payload} = e.data
    switch(event) {
        case "connect":
            state.params = payload
            connect(state.params)
            break
        case "disconnect":
            disconnect()
            break
        default:
            sendMsgToServer([event, payload])
    }
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

function connect(params) {
    return new Promise((resolve) => {
        const url = genSocketUrl(params)
        const socket = new WebSocket(url)

        state.socket = socket

        socket.onopen = function () {
            state.fulfilled = true

            state.eventQueue.forEach(sendMsgToServer)
            state.eventQueue.length = 0

            console.log("Arizona WebSocket is connected", state)
            sendMsgToClient("connect")

            resolve()
        }

        socket.onclose = function (e) {
            console.log("Arizona WebSocket connection closed", e)
            sendMsgToClient("disconnect")
        }

        // Server incoming messages.
        socket.onmessage = function (e) {
            console.log("Arizona WebSocket received a message =>", e.data)
            const data = JSON.parse(e.data)
            Array.isArray(data)
                ? data.forEach(handleEvent)
                : handleEvent(data)
        }
    })
}

function handleEvent(data) {
    const event = data[0]
    const payload = data[1]
    switch(event) {
        case "init":
            state.tree = payload[0]
            state.types = payload[1]
            break
        case "patch":
            sendMsgToClient("patch", applyPatch(payload))
            break
        case "reply":
            sendMsgToClient(payload[0], payload[1])
            break
        case "reconnect":
            console.log("Arizona WebSocket reconnected")
            break
        default:
            sendMsgToClient(event, payload)
            break
    }
}

function disconnect() {
    state.socket.close()
}

function reconnect() {
    console.log("Arizona WebSocket is reconnecting...")
    sendMsgToClient("reconnecting")
    const params = {
        ...state.params,
        reconnecting: "true",
        tree: JSON.stringify(state.tree),
        types: JSON.stringify(state.types),
    }
    connect(params)
}

function applyPatch(changes) {
    Object.entries(changes).forEach(([i, v]) => {
        state.tree[i] = v
    })
    return Object.values(state.tree).join("")
}

function sendMsgToClient(event, payload) {
    self.postMessage({event, payload})
}

function sendMsgToServer([event, payload]) {
    if (!state.socket) {
        state.eventQueue.push([event, payload])
        state.fulfilled
            ? console.warn("Arizona WebSocket is not ready")
            : reconnect()
    } else if (isSocketOpen()) {
        state.socket.send(payload
            ? JSON.stringify([event, payload], state)
            : JSON.stringify(event)
        )
    } else {
        state.eventQueue.push([event, payload])
        isSocketClosed() && reconnect()
	}
}

function isSocketOpen() {
    return state.socket.readyState === WebSocket.OPEN
}

function isSocketClosed() {
    return state.socket.readyState === WebSocket.CLOSED
}
