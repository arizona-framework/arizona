package dev.arizona.client

import android.os.Handler
import android.os.Looper
import androidx.compose.runtime.MutableState
import androidx.compose.runtime.mutableStateOf
import kotlinx.serialization.json.Json
import kotlinx.serialization.json.JsonArray
import kotlinx.serialization.json.JsonNull
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

    private val cache = FingerprintCache()
    private val interleaver = Interleaver(cache)
    private val registry = HashMap<String, Node>()
    private val main = Handler(Looper.getMainLooper())
    private val http = OkHttpClient()
    private var ws: WebSocket? = null
    private var viewId: String? = null

    fun connect() {
        ws = http.newWebSocket(
            Request.Builder().url(wsUrl).build(),
            object : WebSocketListener() {
                override fun onOpen(webSocket: WebSocket, response: Response) {
                    webSocket.send("""["cached_fps",[]]""")
                }

                override fun onMessage(webSocket: WebSocket, text: String) {
                    if (text == "1") return // pong
                    val msg = Json.parseToJsonElement(text).jsonObject
                    msg["o"]?.jsonArray?.let { ops -> main.post { applyOps(ops) } }
                }
            },
        )
    }

    fun close() {
        ws?.close(1000, null)
    }

    /** Fire a push_event command read from a node's tap prop (e.g. `on_tap`). */
    fun tap(node: Node, prop: String = "on_tap") {
        val cmd = node.props[prop]?.jsonArray ?: return
        if (cmd[0].jsonPrimitive.int == Effect.PUSH_EVENT) {
            pushEvent(cmd[1].jsonPrimitive.content)
        }
    }

    fun pushEvent(event: String, payload: Map<String, String> = emptyMap()) {
        val vid = viewId ?: return
        val frame = buildJsonArray {
            add(vid)
            add(event)
            addJsonObject { payload.forEach { (k, v) -> put(k, kotlinx.serialization.json.JsonPrimitive(v)) } }
        }
        ws?.send(frame.toString())
    }

    private fun applyOps(ops: JsonArray) {
        // Top-level ops address nodes as "ViewId:az" via the global registry.
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
                registry.clear()
                val node = buildTree(json)
                indexByAz(node, registry)
                root.value = node
            }
            Op.TEXT -> {
                val node = resolveNode(a[1].jsonPrimitive.content)
                node.children.clear()
                node.children.add(a[2].jsonPrimitive.content)
            }
            Op.UPDATE -> {
                // Re-render a node's content (e.g. a stream reset rebuilds the
                // whole each-list).
                val node = resolveNode(a[1].jsonPrimitive.content)
                node.children.clear()
                addChild(node, Json.parseToJsonElement(interleaver.decode(a[2])))
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
                val item = buildTree(Json.parseToJsonElement(interleaver.decode(a[4])))
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

    // target is "ViewId:az"; resolve by the az suffix.
    private fun resolve(target: String): Node {
        val az = target.substringAfter(':')
        return registry[az] ?: error("unknown az target: $target")
    }
}
