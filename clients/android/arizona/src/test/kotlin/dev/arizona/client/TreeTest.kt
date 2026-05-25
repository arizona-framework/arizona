package dev.arizona.client

import kotlinx.serialization.json.Json
import org.junit.Assert.assertEquals
import org.junit.Test

/**
 * Pure JVM tests (no emulator) of per-view stamping/indexing -- the part that
 * decides which view an op's "ViewId:az" target resolves in. Run with
 * ./gradlew :arizona:testDebugUnitTest
 */
class TreeTest {

    // After a navigate, the OP_REPLACE ViewId (the live process id) differs from
    // the rendered root's `id` attr, and the server prefixes ops with the process
    // id. So the root view must be the passed process id, not the node's `id` --
    // regression for the "unknown target: native_menu:..." crash on menu->list.
    @Test
    fun rootViewIsTheProcessIdNotTheRenderedRootId() {
        val json = Json.parseToJsonElement(
            """
            {"type":"Column","az":"F-0","az_view":true,"id":"native_list",
             "children":[{"type":"#slot","az":"F-0t0","children":[]}]}
            """.trimIndent(),
        )
        val root = buildTree(json, "native_menu") // process id from OP_REPLACE
        val slot = root.children[0] as Node
        assertEquals("native_menu", root.viewId)
        assertEquals("native_menu", slot.viewId)

        val views = HashMap<String, MutableMap<String, Node>>()
        indexByViews(root, views)
        // The slot resolves under the process id, matching the server's prefix.
        assertEquals(slot, views["native_menu"]?.get("F-0t0"))
    }

    // A nested stateful child owns its subtree under its own id (the server
    // prefixes that child's ops with its id), while the root stays the process id.
    @Test
    fun nestedAzViewChildSwitchesView() {
        val json = Json.parseToJsonElement(
            """
            {"type":"Column","az":"P-0","az_view":true,"id":"native_nested","children":[
              {"type":"Column","az":"C-0","az_view":true,"id":"child_a","children":[
                {"type":"#slot","az":"C-0t0","children":[]}]}]}
            """.trimIndent(),
        )
        val root = buildTree(json, "native_nested")
        val child = root.children[0] as Node
        val childSlot = child.children[0] as Node
        assertEquals("native_nested", root.viewId)
        assertEquals("child_a", child.viewId)
        assertEquals("child_a", childSlot.viewId)

        val views = HashMap<String, MutableMap<String, Node>>()
        indexByViews(root, views)
        assertEquals(root, views["native_nested"]?.get("P-0"))
        assertEquals(child, views["child_a"]?.get("C-0"))
    }
}
