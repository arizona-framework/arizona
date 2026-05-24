package dev.arizona.client

import android.os.Handler
import android.os.Looper
import androidx.annotation.VisibleForTesting
import androidx.compose.runtime.MutableState
import androidx.compose.runtime.mutableStateOf
import kotlinx.serialization.json.Json
import kotlinx.serialization.json.JsonArray
import kotlinx.serialization.json.JsonNull
import kotlinx.serialization.json.JsonObject
import kotlinx.serialization.json.add
import kotlinx.serialization.json.addJsonObject
import kotlinx.serialization.json.buildJsonArray
import kotlinx.serialization.json.int
import kotlinx.serialization.json.jsonArray
import kotlinx.serialization.json.jsonObject
import kotlinx.serialization.json.jsonPrimitive
import okhttp3.OkHttpClient
import okhttp3.Request
import okhttp3.Response
import okhttp3.WebSocket
import okhttp3.WebSocketListener

/** Connection state, surfaced as Compose state so the UI can show a placeholder. */
enum class ConnStatus { CONNECTING, CONNECTED, DISCONNECTED }

/**
 * Reconnect backoff: step delays (ms) by attempt, capped at 10s, with ±20%
 * jitter. Mirrors assets/js/arizona-core.js (and e2e/utils/native_client.js).
 * Pure -> JVM-testable without an emulator.
 */
fun backoffDelayMs(attempt: Int): Long {
    val delays = longArrayOf(1000, 2000, 5000, 10000)
    val base = if (attempt < delays.size) delays[attempt] else 10000L
    return Math.round(base * (0.8 + Math.random() * 0.4))
}

// Heartbeat (mirror assets/js/arizona-worker.js): ping "0" every 30s, pong "1";
// a pong still pending at the next tick means the socket is silently dead.
private const val SYS_PING = "0"
private const val SYS_PONG = "1"
private const val HEARTBEAT_MS = 30_000L

/**
 * Connects to an Arizona server's WebSocket and renders its `?native` view.
 *
 * Native has no SSR page, so we connect with `_az_reconnect=1` to make the live
 * process emit its `mount_and_render` output as the first `OP_REPLACE`. Incoming
 * ops mutate the [root] node tree (Compose snapshot state) on the main thread,
 * driving recomposition. Mirrors e2e/utils/native_client.js.
 */
class AzClient(baseUrl: String, path: String) {
    private val wsUrl = baseUrl.replaceFirst("http", "ws") +
        "/ws?_az_path=" + java.net.URLEncoder.encode(path, "UTF-8") + "&_az_reconnect=1"

    /** The current root widget node; `null` until the first frame arrives. */
    val root: MutableState<Node?> = mutableStateOf(null)

    /** Connection state, for a "connecting"/"no connection" UI placeholder. */
    val status: MutableState<ConnStatus> = mutableStateOf(ConnStatus.CONNECTING)

    private val cache = FingerprintCache()
    private val interleaver = Interleaver(cache)

    // viewId -> (az -> node). Per-view so two instances of the same stateful
    // child (which share a fingerprint's az values) don't collide.
    private val views = HashMap<String, MutableMap<String, Node>>()
    private val main = Handler(Looper.getMainLooper())
    private val http = OkHttpClient()
    private var ws: WebSocket? = null
    private var viewId: String? = null

    // Reconnect bookkeeping. `closing` (set on an intentional close) suppresses
    // reconnect; `reconnectAttempt` indexes the backoff and is touched only on
    // the main thread (reset on a healthy frame, bumped when scheduling).
    @Volatile
    private var closing = false
    private var reconnectAttempt = 0

    // Heartbeat state, touched only on the main thread (like reconnectAttempt).
    // A self-rescheduling Runnable is the Handler equivalent of setInterval.
    private var heartbeatPending = false
    private val heartbeat = object : Runnable {
        override fun run() {
            val sock = ws ?: return
            if (heartbeatPending) {
                sock.cancel() // missed pong -> drop -> reconnect via onFailure
                return
            }
            heartbeatPending = true
            sock.send(SYS_PING)
            main.postDelayed(this, HEARTBEAT_MS)
        }
    }

    fun connect() {
        status.value = ConnStatus.CONNECTING
        ws = http.newWebSocket(
            Request.Builder().url(wsUrl).build(),
            object : WebSocketListener() {
                override fun onOpen(webSocket: WebSocket, response: Response) {
                    webSocket.send("""["cached_fps",[]]""")
                    main.post { startHeartbeat() }
                }

                override fun onMessage(webSocket: WebSocket, text: String) {
                    main.post { heartbeatPending = false } // any frame -> socket live
                    if (text == SYS_PONG) return // pong
                    val msg = Json.parseToJsonElement(text).jsonObject
                    msg["o"]?.jsonArray?.let { ops ->
                        main.post {
                            applyOps(ops)
                            status.value = ConnStatus.CONNECTED
                            reconnectAttempt = 0 // healthy frame -> reset backoff
                        }
                    }
                    // Handler-returned effects: dispatch the portable ones, skip
                    // web-only effects (set_title, dispatch_event, ...).
                    msg["e"]?.jsonArray?.let { effects ->
                        main.post { for (eff in effects) runEffect(eff.jsonArray, strict = false) }
                    }
                }

                override fun onFailure(webSocket: WebSocket, t: Throwable, response: Response?) {
                    // A failure is an abrupt drop (no close code): treat as non-1000.
                    scheduleReconnect(1006)
                }

                override fun onClosed(webSocket: WebSocket, code: Int, reason: String) {
                    scheduleReconnect(code)
                }
            },
        )
    }

    // Flip to DISCONNECTED and, unless we closed on purpose (`closing`) or it was
    // a normal close (1000), reopen with backoff -- re-mounting via _az_reconnect=1.
    // All on the main thread, so reconnectAttempt is touched from one thread only.
    // Mirrors the JS reference client's onclose.
    private fun scheduleReconnect(code: Int) {
        main.post {
            stopHeartbeat() // the old socket is dead; the new one restarts it
            status.value = ConnStatus.DISCONNECTED
            if (closing || code == 1000) return@post
            main.postDelayed({ if (!closing) connect() }, backoffDelayMs(reconnectAttempt++))
        }
    }

    // Ping every 30s; a pong still pending at the next tick means the socket is
    // silently dead. Idempotent -- drops any prior loop before starting one.
    private fun startHeartbeat() {
        main.removeCallbacks(heartbeat)
        heartbeatPending = false
        main.postDelayed(heartbeat, HEARTBEAT_MS)
    }

    private fun stopHeartbeat() {
        main.removeCallbacks(heartbeat)
        heartbeatPending = false
    }

    fun close() {
        closing = true
        stopHeartbeat()
        ws?.close(1000, null)
    }

    /** Abruptly drop the socket (like a network failure) to exercise reconnect. */
    @VisibleForTesting
    fun forceDrop() {
        ws?.cancel()
    }

    /** Fire the command read from a node's tap prop (e.g. `on_tap`), routing the
     *  event to the node's enclosing view (the root, or a stateful child). */
    fun tap(node: Node, prop: String = "on_tap") {
        node.props[prop]?.jsonArray?.let { runEffect(it, strict = true, target = node.viewId) }
    }

    // Run one effect command (a tap prop or a server "e" entry). `strict` errors
    // on an unsupported command (taps); non-strict skips it (web-only effects in
    // the "e" stream don't apply to native). `target` (a tap's enclosing view id)
    // routes the event; the server's "e" effects pass none -> root.
    private fun runEffect(cmd: JsonArray, strict: Boolean, target: String? = null) {
        when (cmd[0].jsonPrimitive.int) {
            Effect.PUSH_EVENT -> pushEvent(cmd[1].jsonPrimitive.content, target = target)
            Effect.NAVIGATE -> navigate(cmd[1].jsonPrimitive.content)
            else -> if (strict) error("unsupported command: $cmd")
        }
    }

    /**
     * SPA navigate: transition to a new view on the same socket (the server's
     * handle_navigate re-mounts and replies with OP_REPLACE). Mirrors the browser
     * sending ['navigate', {path, qs}].
     */
    fun navigate(path: String) {
        val parts = path.split("?", limit = 2)
        val frame = buildJsonArray {
            add("navigate")
            addJsonObject {
                put("path", kotlinx.serialization.json.JsonPrimitive(parts[0]))
                put("qs", kotlinx.serialization.json.JsonPrimitive(parts.getOrElse(1) { "" }))
            }
        }
        ws?.send(frame.toString())
    }

    // Send an event frame [ViewId, Event, Payload]. ViewId defaults to the root
    // view; a tap routes to the tapped node's enclosing view (so events reach a
    // stateful child). The payload is arbitrary JSON (matching the wire and the
    // JS reference client) -- handlers may expect numbers or arrays, not just
    // strings (e.g. a stream move's numeric pos).
    fun pushEvent(
        event: String,
        payload: JsonObject = JsonObject(emptyMap()),
        target: String? = null,
    ) {
        val vid = target ?: viewId ?: return
        val frame = buildJsonArray {
            add(vid)
            add(event)
            add(payload)
        }
        ws?.send(frame.toString())
    }

    private fun applyOps(ops: JsonArray) {
        // Top-level ops address nodes as "ViewId:az" via the per-view registry.
        for (op in ops) dispatch(op.jsonArray) { target -> resolve(target) }
    }

    // Apply one op, resolving its target via [resolveNode]. Top-level ops pass
    // "ViewId:az"; an OP_ITEM_PATCH's inner ops pass a bare az resolved within
    // the patched item (mirrors the browser worker's applyItemOps).
    private fun dispatch(a: JsonArray, resolveNode: (String) -> Node) {
        when (a[0].jsonPrimitive.int) {
            Op.REPLACE -> {
                viewId = a[1].jsonPrimitive.content
                val json = Json.parseToJsonElement(interleaver.interleave(a[2].jsonObject))
                views.clear()
                val node = buildTree(json)
                indexByViews(node, views)
                root.value = node
            }
            Op.TEXT -> {
                // Usually a scalar, but a nested-template dynamic (e.g. a
                // conditional subtree) ships a {f,s,d} payload; decode handles both.
                val node = resolveNode(a[1].jsonPrimitive.content)
                node.children.clear()
                addChild(node, Json.parseToJsonElement(interleaver.decode(a[2])), node.viewId)
            }
            Op.UPDATE -> {
                // Re-render a node's content (e.g. a stream reset rebuilds the
                // whole each-list).
                val node = resolveNode(a[1].jsonPrimitive.content)
                node.children.clear()
                addChild(node, Json.parseToJsonElement(interleaver.decode(a[2])), node.viewId)
            }
            Op.SET_ATTR -> {
                resolveNode(a[1].jsonPrimitive.content).props[a[2].jsonPrimitive.content] = a[3]
            }
            Op.REM_ATTR -> {
                resolveNode(a[1].jsonPrimitive.content).props.remove(a[2].jsonPrimitive.content)
            }
            Op.INSERT -> {
                val container = resolveNode(a[1].jsonPrimitive.content)
                val pos = a[3].jsonPrimitive.int
                val item = buildTree(Json.parseToJsonElement(interleaver.decode(a[4])), container.viewId)
                if (pos == -1 || pos >= container.children.size) container.children.add(item)
                else container.children.add(pos, item)
            }
            Op.REMOVE -> {
                val container = resolveNode(a[1].jsonPrimitive.content)
                val i = indexOfKey(container, a[2].jsonPrimitive.content)
                if (i != -1) container.children.removeAt(i)
            }
            Op.MOVE -> {
                val container = resolveNode(a[1].jsonPrimitive.content)
                val i = indexOfKey(container, a[2].jsonPrimitive.content)
                if (i == -1) return
                val item = container.children.removeAt(i)
                val afterKey = a[3]
                if (afterKey is JsonNull) {
                    container.children.add(0, item)
                } else {
                    val r = indexOfKey(container, afterKey.jsonPrimitive.content)
                    if (r == -1) container.children.add(item) else container.children.add(r + 1, item)
                }
            }
            Op.ITEM_PATCH -> {
                val container = resolveNode(a[1].jsonPrimitive.content)
                val i = indexOfKey(container, a[2].jsonPrimitive.content)
                if (i != -1) applyInner(container.children[i] as Node, a[3].jsonArray)
            }
            else -> error("unhandled op code: ${a[0]}")
        }
    }

    // Apply an OP_ITEM_PATCH's inner ops, scoped to one keyed item: inner ops
    // carry bare az indices resolved within the item's own subtree.
    private fun applyInner(item: Node, innerOps: JsonArray) {
        val local = HashMap<String, Node>()
        indexByAz(item, local)
        for (op in innerOps) dispatch(op.jsonArray) { az -> local[az] ?: item }
    }

    // Index of the keyed child (by its `az_key` prop) in a stream container, or -1.
    private fun indexOfKey(container: Node, key: String): Int =
        container.children.indexOfFirst {
            it is Node && it.props["az_key"]?.jsonPrimitive?.content == key
        }

    // Resolve a top-level op's "ViewId:az" target within that view's own registry,
    // so two instances of the same stateful child (sharing a fingerprint's az
    // values) don't collide. Mirrors the browser scoping az to getElementById.
    private fun resolve(target: String): Node {
        val viewId = target.substringBefore(':')
        val az = target.substringAfter(':')
        return views[viewId]?.get(az) ?: error("unknown target: $target")
    }
}
