"use strict"

console.log("[Arizona] loading...")

const worker = new Worker("assets/js/arizona-worker.js")

worker.addEventListener("message", function(e) {
    console.log("[WebWorker] msg:", e.data)
})

worker.addEventListener("error", function(e) {
    console.error("[WebWorker] error:", e)
})

worker.postMessage({event: "connect", payload: {}})

console.log("[Arizona] ready")

