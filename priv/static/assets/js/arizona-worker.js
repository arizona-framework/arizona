"use strict"

const state = {
    fulfilled: false,
    params: {},
    socket: null,
    eventQueue: [],
    tree: []
}

// Messages from client.
self.onmessage = function(e) {
    console.log("[WebWorker] client sent:", e.data)

    if (typeof e.data !== "object" || !e.data.target || !e.data.event) {
        console.error("Invalid Arizona WebWorker message", e)
        return
    }

    const {target, event, payload} = e.data
    switch(event) {
        case "connect":
            connect(payload)
            break
        case "disconnect":
            disconnect()
            break
        default:
            sendMsgToServer([target, event, payload])
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
            const data = JSON.parse(e.data)
            Array.isArray(data)
                ? data.forEach(handleEvent)
                : handleEvent(data)
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

function handleEvent(data) {
    const event = data[0]
    const payload = data[1]
    switch(event) {
        case "init":
            state.tree = payload
            break
        case "patch":
            sendMsgToClient("patch", applyPatch(payload))
            break
        default:
            sendMsgToClient(event, payload)
            break
    }
}

function applyPatch([target, changes]) {
    const tree = target === "root"
        ? state.tree
        : getTargetTree(target, state.tree)
    changes.forEach(c => { applyChanges(c, tree) })
    const html = tree.flat(Infinity).join("")
    return {target, html}
}

function applyChanges([indexes, v], tree) {
    if (indexes.length === 1) {
        tree[indexes[0]] = v
    } else {
        const [i, ...rest] = indexes
        applyChanges([rest, v], tree[i])
    }
}

function getTargetTree(path, tree) {
    if (path.length === 1) {
        return tree[path[0]]
    } else {
        const [i, ...rest] = path
        return getTargetTree(rest, tree[i])
    }
}

function sendMsgToClient(event, payload) {
    self.postMessage({event, payload})
}

function sendMsgToServer([target, event, payload]) {
    if (!state.socket) {
        state.eventQueue.push([target, event, payload])
        state.fulfilled
            ? console.warn("[WebSocket] not ready to send messages")
            : reconnect()
    } else if (isSocketOpen()) {
        state.socket.send(payload
            ? JSON.stringify([target, event, payload], state)
            : JSON.stringify([target, event])
        )
    } else {
        state.fulfilled && state.eventQueue.push([target, event, payload])
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

