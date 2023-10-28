"use strict"

function arizonaFactory(opts = {}) {
    // Worker

    const workerURL = opts.workerURL || "assets/arizona-worker.js"
    const worker = new Worker(workerURL)

    worker.addEventListener("message", function(e) {
        console.log("Arizona received a message from WebWorker =>", e.data)
        const { event, payload } = e.data
        subscribers.get(event)?.forEach(
            function({id, callback, opts}) {
                callback(payload)
                opts.once && unsubscribe(id)
            }
        )
    })

    worker.addEventListener("error", function(e) {
        console.error("Arizona received and error from WebWorker =>", e)
    })

    // Subscribers

    const subscribers = new Map()
    const unsubscribers = new Map()

    // @todo Subscribe to global events.
    function subscribe(eventName, callback, opts = {}) {
        let eventSubs = subscribers.get(eventName)
        if (!eventSubs) eventSubs = new Map()
        // @todo Improve id
        const id = Math.random()
        eventSubs.set(id, {id, callback, opts})
        subscribers.set(eventName, eventSubs)
        unsubscribers.set(id, eventName)
        console.table({action: "subscribed", eventName, id, subscribers, unsubscribers})
        return function() {
            unsubscribe(id)
        }
    }

    function subscribeOnce(event, callback, opts = {}) {
        return subscribe(event, callback, { ...opts, once: true })
    }

    function unsubscribe(id) {
        const eventName = unsubscribers.get(id)
        if (!eventName) return
        const eventSubs = subscribers.get(eventName)
        if (!eventSubs) return
        eventSubs.delete(id)
        eventSubs.size
            ? subscribers.set(eventName, eventSubs)
            : subscribers.delete(eventName)
        unsubscribers.delete(id)
        console.table({action: "unsubscribed", eventName, id, subscribers, unsubscribers})
    }

    //  Utils

    function connect(params, callback, opts) {
        send("connect", params, callback, opts)
    }

    function send(event, payloadOrCallback, callbackOrOpts, optsOrNull) {
        typeof payloadOrCallback === "function"
            ? sendMsgToWorker(event, undefined, payloadOrCallback, callbackOrOpts)
            : sendMsgToWorker(event, payloadOrCallback, callbackOrOpts, optsOrNull)
    }

    function sendMsgToWorker(event, payload, callback, opts = {}) {
        callback && subscribeOnce(event, callback, opts)
        worker.postMessage({event, payload})
    }

    // Observer

    document.addEventListener("DOMContentLoaded", () => {
        // @todo More events.
        const eventAttributes = [
            "arz-click",
        ]

        eventAttributes.forEach((attributeName) => {
            document
                .querySelectorAll(`[${attributeName}]`)
                .forEach((target) => installEvent(target, attributeName))
        })

        function installEvent(target, attributeName) {
            const [_, listenerType] = attributeName.split("arz-")
            const event = target.getAttribute(attributeName)
            const callback = observerNodeEventListener(event)
            event
                ? target.addEventListener(listenerType, callback)
                : target.removeEventListener(listenerType, callback)
        }

        function observerNodeEventListener(event) {
            return function(e) {
                e.target.name
                    ? send(event, {[e.target.name]: e.target.value})
                    : send(event)
            }
        }

        const observer = new MutationObserver((mutations) => {
            for (const node of mutations) {
                installEvent(node.target, node.attributeName)
            }
        })
        observer.observe(document.body, {
            attributeFilter: eventAttributes,
            subtree: true,
        })
    })

    return {
        connect,
        send,
        on: subscribe,
        once: subscribeOnce,
    }
}
