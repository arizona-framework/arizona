package dev.arizona.client

import android.os.Handler
import android.os.Looper
import android.util.Log
import androidx.annotation.VisibleForTesting
import androidx.compose.runtime.MutableState
import androidx.compose.runtime.mutableStateOf
import kotlinx.serialization.json.Json
import kotlinx.serialization.json.JsonArray
import kotlinx.serialization.json.JsonElement
import kotlinx.serialization.json.JsonNull
import kotlinx.serialization.json.JsonObject
import kotlinx.serialization.json.JsonPrimitive
import kotlinx.serialization.json.add
import kotlinx.serialization.json.addJsonObject
import kotlinx.serialization.json.buildJsonArray
import kotlinx.serialization.json.int
import kotlinx.serialization.json.intOrNull
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
private const val TAG = "AzClient"

/**
 * Connects to an Arizona server's WebSocket and renders its `?native` view.
 *
 * Native has no SSR page, so we connect with `_az_reconnect=1` to make the live
 * process emit its `mount_and_render` output as the first `OP_REPLACE`. Incoming
 * ops mutate the [root] node tree (Compose snapshot state) on the main thread,
 * driving recomposition. Mirrors e2e/utils/native_client.js.
 */
class AzClient(baseUrl: String, path: String) {
    private val wsBase = baseUrl.replaceFirst("http", "ws") + "/ws?_az_path="

    // The reconnect URL. Updated in navigate() to the navigated path, so a dropped
    // socket re-mounts where the user is, not the launch path (mirrors the browser
    // worker updating _wsUrl on navigate).
    private var wsUrl = wsBase + java.net.URLEncoder.encode(path, "UTF-8") + "&_az_reconnect=1"

    /** The current root widget node; `null` until the first frame arrives. */
    val root: MutableState<Node?> = mutableStateOf(null)

    /** Connection state, for a "connecting"/"no connection" UI placeholder. */
    val status: MutableState<ConnStatus> = mutableStateOf(ConnStatus.CONNECTING)

    private val cache = FingerprintCache()
    private val interleaver = Interleaver(cache)

    // viewId -> (az -> node). Per-view so two instances of the same stateful
    // child (which share a fingerprint's az values) don't collide.
    @VisibleForTesting
    internal val views = HashMap<String, MutableMap<String, Node>>()
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
                    // On main so the fingerprint cache is only ever read/written
                    // from one thread (the op applier owns it).
                    main.post {
                        webSocket.send(cachedFpsFrame())
                        startHeartbeat()
                    }
                }

                override fun onMessage(webSocket: WebSocket, text: String) {
                    main.post { handleText(text) }
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

    /**
     * Apply one received text frame. Synchronous and UI-agnostic (Compose state
     * writes aside), so the JVM unit tests drive it directly -- it is the unit of
     * behavior the socket callbacks marshal onto the main thread. Mirrors iOS's
     * `handleText`.
     */
    @VisibleForTesting
    internal fun handleText(text: String) {
        heartbeatPending = false // any frame -> socket live
        if (text == SYS_PONG) return // pong
        val msg = Json.parseToJsonElement(text).jsonObject
        msg["o"]?.jsonArray?.let { ops ->
            applyOps(ops)
            status.value = ConnStatus.CONNECTED
            reconnectAttempt = 0 // healthy frame -> reset backoff
        }
        // Handler-returned effects: dispatch the portable ones, skip web-only
        // effects (set_title, dispatch_event, ...).
        msg["e"]?.jsonArray?.let { effects ->
            for (eff in effects) runEffect(eff.jsonArray, strict = false)
        }
    }

    /**
     * The `["cached_fps", [...]]` announcement: the fingerprints this client holds
     * statics for, so the server can omit them from what it renders next (a
     * reconnect mounts a fresh live process with an empty sent set, so without the
     * announcement every reconnect re-ships every template's statics). Native
     * deliberately does NOT pass `_az_fps_follow`: the server keeps the immediate
     * resync, and this seeds the process for the frames after it.
     */
    @VisibleForTesting
    internal fun cachedFpsFrame(): String =
        buildJsonArray {
            add("cached_fps")
            add(buildJsonArray { cache.announce().forEach { add(it) } })
        }.toString()

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
            Effect.PUSH_EVENT ->
                pushEvent(
                    cmd[1].jsonPrimitive.content,
                    payload = cmd.getOrNull(2)?.jsonObject ?: JsonObject(emptyMap()),
                    target = target,
                )
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
        // Keep the reconnect URL in sync with the navigated route, so a drop
        // re-mounts here, not the launch path. Reconnect is a fresh mount, so only
        // _az_path (the route) matters.
        wsUrl = wsBase + java.net.URLEncoder.encode(parts[0], "UTF-8") + "&_az_reconnect=1"
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
        // Top-level ops address nodes as "ViewId:az" via the per-view registry;
        // OP_REMOVE_NODE searches the whole tree to splice the node out.
        val scope = viewScope()
        for (op in ops) dispatch(op, root.value, scope)
    }

    /**
     * How one op batch addresses and re-indexes nodes: [resolve] maps an op target
     * to a node (null when unknown), and [unindexChildren]/[reindex] keep the
     * registry in step with the tree whenever an op discards a node's children or
     * grafts a new subtree on. Three exist -- the top-level one ("ViewId:az" over
     * the per-view registry), the per-item one in [applyInner], and the child-view
     * one in [applyChildViewOps].
     */
    private class Scope(
        val resolve: (String) -> Node?,
        val unindexChildren: (Node) -> Unit,
        val reindex: (Node) -> Unit,
    )

    private fun viewScope() = Scope(::resolve, ::unindexChildrenInViews, ::reindexInViews)

    private fun unindexChildrenInViews(node: Node) {
        for (child in node.children) if (child is Node) unindexByViews(child, views)
    }

    private fun reindexInViews(node: Node) = indexByViews(node, views)

    // Apply one op through [scope]. Top-level ops pass "ViewId:az"; an
    // OP_ITEM_PATCH's inner ops pass a bare az resolved within the patched item
    // (mirrors the browser worker's applyItemOps). [scopeRoot] bounds
    // OP_REMOVE_NODE's parent search (the whole tree, or one patched item).
    //
    // Each op is isolated: an unexpected wire shape (or a target this client
    // cannot resolve) must degrade one slot, not take the process down. Mirrors
    // the browser client's per-op try/catch in applyOps.
    private fun dispatch(op: JsonElement, scopeRoot: Node?, scope: Scope) {
        try {
            dispatchOne(op.jsonArray, scopeRoot, scope)
        } catch (e: Exception) {
            Log.w(TAG, "op $op failed; skipping", e)
        }
    }

    private fun dispatchOne(a: JsonArray, scopeRoot: Node?, scope: Scope) {
        // A child view's ops ride in a `[ChildViewId, ChildOps]` wrapper that
        // `flatten_ops/2` unwraps only at TOP level -- so a ?stateful child inside
        // a stream item ships the wrapper as an OP_ITEM_PATCH INNER op, whose first
        // element is a view-id string rather than an op code.
        val head = a[0]
        val code = if (head is JsonPrimitive && !head.isString) head.intOrNull else null
        if (code == null) {
            applyChildViewOps(head.jsonPrimitive.content, a[1].jsonArray, scopeRoot)
            return
        }
        if (code == Op.REPLACE) {
            val json = Json.parseToJsonElement(interleaver.interleave(a[2].jsonObject))
            // The live view id is the rendered root's `id` (== the server's
            // socket.view_id, what it prefixes pushed ops with), NOT a[1] --
            // after a navigate a[1] is the OLD id (the replace target). Mirrors
            // the browser reading the new root's az-view id from the DOM.
            viewId = json.jsonObject["id"]?.jsonPrimitive?.content
            views.clear()
            val node = buildTree(json, viewId)
            indexByViews(node, views)
            root.value = node
            return
        }
        val target = a[1].jsonPrimitive.content
        val node = scope.resolve(target)
        if (node == null) {
            // Loud like the browser client's missing-target warn: a silently
            // dropped op reads as "nothing happened" and costs a debugging trip.
            Log.w(TAG, "op $code target \"$target\" not found; skipping")
            return
        }
        when (code) {
            Op.TEXT, Op.UPDATE -> {
                // OP_TEXT is usually a scalar, but a nested-template dynamic (e.g.
                // a conditional subtree) ships a {f,s,d} payload; OP_UPDATE
                // re-renders a node's content wholesale (e.g. a stream reset).
                // Either way the children are REPLACED, so the registry has to
                // follow: drop the destroyed subtree's entries and index the new
                // one, or every az the payload introduced -- a nested child view's
                // id included -- is unaddressable.
                scope.unindexChildren(node)
                node.children.clear()
                addChild(node, Json.parseToJsonElement(interleaver.decode(a[2])), node.viewId)
                scope.reindex(node)
            }
            Op.REMOVE_NODE -> {
                // A dynamic returned the `remove` sentinel: drop the node from its
                // parent. One-way -- bringing it back needs a parent re-render.
                removeFromParent(scopeRoot, node)
            }
            Op.SET_ATTR -> {
                node.props[a[2].jsonPrimitive.content] = a[3]
            }
            Op.REM_ATTR -> {
                node.props.remove(a[2].jsonPrimitive.content)
            }
            Op.INSERT -> {
                val pos = a[3].jsonPrimitive.int
                val item = buildTree(Json.parseToJsonElement(interleaver.decode(a[4])), node.viewId)
                if (pos == -1 || pos >= node.children.size) node.children.add(item)
                else node.children.add(pos, item)
                // Index the new item, like OP_REPLACE already indexes the items it
                // renders: a ?stateful child inside a stream item owns its own view
                // id, and that view's ops arrive addressed to it.
                scope.reindex(item)
            }
            Op.REMOVE -> {
                val i = indexOfKey(node, a[2].jsonPrimitive.content)
                if (i != -1) node.children.removeAt(i)
            }
            Op.MOVE -> {
                val i = indexOfKey(node, a[2].jsonPrimitive.content)
                if (i == -1) return
                val item = node.children.removeAt(i)
                val afterKey = a[3]
                if (afterKey is JsonNull) {
                    node.children.add(0, item)
                } else {
                    val r = indexOfKey(node, afterKey.jsonPrimitive.content)
                    if (r == -1) node.children.add(item) else node.children.add(r + 1, item)
                }
            }
            Op.ITEM_PATCH -> {
                val i = indexOfKey(node, a[2].jsonPrimitive.content)
                if (i != -1) applyInner(node.children[i] as Node, a[3].jsonArray)
            }
            else -> Log.w(TAG, "unhandled op code $code; skipping")
        }
    }

    // Apply an OP_ITEM_PATCH's inner ops, scoped to one keyed item: inner ops
    // carry bare az indices resolved within the item's own subtree.
    private fun applyInner(item: Node, innerOps: JsonArray) {
        val local = HashMap<String, Node>()
        indexByAz(item, local)
        val scope = Scope(
            resolve = { az -> local[az] ?: item },
            unindexChildren = { node ->
                for (child in node.children) if (child is Node) unindexByAz(child, local)
            },
            reindex = { node -> indexByAz(node, local) },
        )
        for (op in innerOps) dispatch(op, item, scope)
    }

    // Apply a child view's ops (the `[ChildViewId, ChildOps]` wrapper): their
    // targets are bare `az`s resolved inside that child's own registry, exactly as
    // the top-level flattened "ChildViewId:az" form resolves. [scopeRoot] stays the
    // enclosing item so OP_REMOVE_NODE's parent search still finds the node.
    private fun applyChildViewOps(childViewId: String, childOps: JsonArray, scopeRoot: Node?) {
        val scope = Scope(
            resolve = { az -> views[childViewId]?.get(az) },
            unindexChildren = ::unindexChildrenInViews,
            reindex = ::reindexInViews,
        )
        for (op in childOps) dispatch(op, scopeRoot, scope)
    }

    // Find [target] within [parent]'s subtree and remove it from its parent's
    // children. Used by OP_REMOVE_NODE (the `remove` sentinel).
    private fun removeFromParent(parent: Node?, target: Node): Boolean {
        if (parent == null) return false
        val i = parent.children.indexOf(target)
        if (i != -1) {
            parent.children.removeAt(i)
            return true
        }
        for (c in parent.children) if (c is Node && removeFromParent(c, target)) return true
        return false
    }

    // Index of the keyed child (by its `az_key` prop) in a stream container, or -1.
    private fun indexOfKey(container: Node, key: String): Int =
        container.children.indexOfFirst {
            it is Node && it.props["az_key"]?.jsonPrimitive?.content == key
        }

    // Resolve a top-level op's "ViewId:az" target within that view's own registry,
    // so two instances of the same stateful child (sharing a fingerprint's az
    // values) don't collide. Mirrors the browser scoping az to getElementById.
    // Null when unknown -- the caller warns and skips the op.
    private fun resolve(target: String): Node? =
        views[target.substringBefore(':')]?.get(target.substringAfter(':'))
}
