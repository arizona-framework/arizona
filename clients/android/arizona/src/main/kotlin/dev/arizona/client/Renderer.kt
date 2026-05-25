package dev.arizona.client

import androidx.compose.runtime.Composable

/**
 * A widget renderer: maps a node's `type` string to a Composable. Receiver is
 * the [RenderContext], giving access to child rendering, text flattening, and
 * event dispatch. The framework is vocabulary-agnostic -- the host app registers
 * whatever widget types its `?native` views use.
 */
typealias Widget = @Composable RenderContext.(node: Node) -> Unit

/** Maps widget `type` -> Composable. Populated by the host app. */
class WidgetRegistry {
    private val widgets = HashMap<String, Widget>()

    fun register(type: String, widget: Widget): WidgetRegistry {
        widgets[type] = widget
        return this
    }

    fun get(type: String): Widget? = widgets[type]
}

/**
 * Render context handed to every widget. Provides:
 *  - [Children]: render a node's child *elements* (splicing transparent `#slot`s);
 *  - [text]: the node's flattened text (its string descendants, through `#slot`s);
 *  - [tap]: fire a node's tap command back to the server.
 */
class RenderContext(private val registry: WidgetRegistry, val client: AzClient) {

    @Composable
    fun render(node: Node) {
        if (node.type == Node.SLOT) {
            Children(node) // transparent fragment
            return
        }
        val widget = registry.get(node.type) ?: error("no widget registered for '${node.type}'")
        widget(this, node)
    }

    /** Render a node's child elements (recursing through `#slot`s); text is ignored here. */
    @Composable
    fun Children(node: Node) {
        for (child in node.children) {
            if (child is Node) render(child)
        }
    }

    /** Concatenate a node's text descendants, flattening through `#slot`s. */
    fun text(node: Node): String {
        val sb = StringBuilder()
        collectText(node, sb)
        return sb.toString()
    }

    private fun collectText(node: Node, sb: StringBuilder) {
        for (child in node.children) {
            when (child) {
                is String -> sb.append(child)
                is Node -> collectText(child, sb)
            }
        }
    }

    fun tap(node: Node, prop: String = "on_tap") = client.tap(node, prop)
}

/** Render the client's current root tree, if any. */
@Composable
fun ArizonaView(client: AzClient, registry: WidgetRegistry) {
    val ctx = RenderContext(registry, client)
    client.root.value?.let { ctx.render(it) }
}
