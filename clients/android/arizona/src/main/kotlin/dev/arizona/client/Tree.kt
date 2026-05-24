package dev.arizona.client

import androidx.compose.runtime.mutableStateMapOf
import androidx.compose.runtime.mutableStateListOf
import androidx.compose.runtime.snapshots.SnapshotStateList
import androidx.compose.runtime.snapshots.SnapshotStateMap
import kotlinx.serialization.json.JsonArray
import kotlinx.serialization.json.JsonElement
import kotlinx.serialization.json.JsonObject
import kotlinx.serialization.json.JsonPrimitive
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

    companion object {
        const val SLOT = "#slot"
    }
}

/** Parse an interleaved JSON tree (the root element) into a [Node] tree. */
fun buildTree(json: JsonElement): Node {
    val obj = json.jsonObject
    val node = Node(
        type = obj.getValue("type").jsonPrimitive.content,
        az = obj["az"]?.jsonPrimitive?.content,
    )
    for ((k, v) in obj) {
        if (k != "type" && k != "az" && k != "children") node.props[k] = v
    }
    obj["children"]?.jsonArray?.forEach { addChild(node, it) }
    return node
}

// Append a child, splicing each-expansion arrays into the parent. #slot objects
// are kept as Nodes; stream items (each-array entries) become keyed child Nodes.
internal fun addChild(parent: Node, child: JsonElement) {
    when (child) {
        is JsonArray -> child.forEach { addChild(parent, it) }
        is JsonObject -> parent.children.add(buildTree(child))
        is JsonPrimitive -> parent.children.add(child.content)
    }
}

/** Index every node carrying an `az` (incl. `#slot`s) so ops can target it. */
fun indexByAz(node: Node, registry: MutableMap<String, Node>) {
    node.az?.let { registry[it] = node }
    for (child in node.children) if (child is Node) indexByAz(child, registry)
}
