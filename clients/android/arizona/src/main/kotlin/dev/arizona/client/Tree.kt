package dev.arizona.client

import androidx.compose.runtime.mutableStateMapOf
import androidx.compose.runtime.mutableStateListOf
import androidx.compose.runtime.snapshots.SnapshotStateList
import androidx.compose.runtime.snapshots.SnapshotStateMap
import kotlinx.serialization.json.JsonArray
import kotlinx.serialization.json.JsonElement
import kotlinx.serialization.json.JsonObject
import kotlinx.serialization.json.JsonPrimitive
import kotlinx.serialization.json.booleanOrNull
import kotlinx.serialization.json.jsonArray
import kotlinx.serialization.json.jsonObject
import kotlinx.serialization.json.jsonPrimitive

/**
 * A widget node in the rendered tree. `props` and `children` are Compose
 * snapshot state, so the op applier mutating them triggers recomposition.
 *
 * A child is either a [Node] (an element, possibly a transparent `#slot`) or a
 * [String] (text). `#slot` nodes are kept (they carry the `az` that `OP_TEXT`
 * targets); the renderer treats them transparently.
 */
class Node(val type: String, val az: String?) {
    val props: SnapshotStateMap<String, JsonElement> = mutableStateMapOf()
    val children: SnapshotStateList<Any> = mutableStateListOf()

    /** The enclosing view's id (nearest `az_view` ancestor, or the root), so a
     *  tap routes its event to the owning view -- the root or a stateful child. */
    var viewId: String? = null

    companion object {
        const val SLOT = "#slot"
    }
}

/**
 * Parse an interleaved JSON tree into a [Node] tree, stamping each node with its
 * enclosing view ([view]). The root view is the live process id (the OP_REPLACE
 * ViewId) -- after a navigate that differs from the rendered root's `id` attr, so
 * the root uses the passed [view], not its own `id`; a nested `az_view` child
 * switches to its own id (see [addChild]).
 */
fun buildTree(json: JsonElement, view: String? = null): Node {
    val obj = json.jsonObject
    val node = Node(
        type = obj.getValue("type").jsonPrimitive.content,
        az = obj["az"]?.jsonPrimitive?.content,
    )
    for ((k, v) in obj) {
        if (k != "type" && k != "az" && k != "children") node.props[k] = v
    }
    node.viewId = view
    obj["children"]?.jsonArray?.forEach { addChild(node, it, view) }
    return node
}

// Append a child, splicing each-expansion arrays into the parent. #slot objects
// are kept as Nodes; stream items (each-array entries) become keyed child Nodes.
// A child that is itself a view root (az_view) owns its subtree; otherwise it
// stays in the parent's [view].
internal fun addChild(parent: Node, child: JsonElement, view: String? = null) {
    when (child) {
        is JsonArray -> child.forEach { addChild(parent, it, view) }
        is JsonObject -> {
            val childView =
                if (child["az_view"]?.jsonPrimitive?.booleanOrNull == true) {
                    child["id"]?.jsonPrimitive?.content ?: view
                } else {
                    view
                }
            parent.children.add(buildTree(child, childView))
        }
        is JsonPrimitive -> parent.children.add(child.content)
    }
}

/** Index every node carrying an `az` (incl. `#slot`s) so ops can target it. */
fun indexByAz(node: Node, registry: MutableMap<String, Node>) {
    node.az?.let { registry[it] = node }
    for (child in node.children) if (child is Node) indexByAz(child, registry)
}

/**
 * Index nodes per enclosing view (`viewId` -> `az` -> node), so a "ViewId:az" op
 * target resolves within the right view -- two instances of the same stateful
 * child share az values (from a shared fingerprint), but live in distinct views.
 */
fun indexByViews(node: Node, views: MutableMap<String, MutableMap<String, Node>>) {
    val v = node.viewId
    if (v != null) node.az?.let { views.getOrPut(v) { HashMap() }[it] = node }
    for (child in node.children) if (child is Node) indexByViews(child, views)
}
