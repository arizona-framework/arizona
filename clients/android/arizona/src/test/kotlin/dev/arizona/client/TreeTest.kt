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

    // The live view id is the rendered root's own `id` (OP_REPLACE passes it, and
    // the server prefixes both client-event replies and pushed ops with it).
    // buildTree stamps it on the root + its non-view descendants; indexByViews
    // registers them under it -- so "rootId:az" targets (incl. server pushes,
    // which broke TickerE2ETest) resolve.
    @Test
    fun rootSubtreeIndexedUnderTheRenderedRootId() {
        val json = Json.parseToJsonElement(
            """
            {"type":"Column","az":"F-0","az_view":true,"id":"native_ticker",
             "children":[{"type":"#slot","az":"F-0t0","children":[]}]}
            """.trimIndent(),
        )
        val root = buildTree(json, "native_ticker") // OP_REPLACE passes the root's id
        val slot = root.children[0] as Node
        assertEquals("native_ticker", root.viewId)
        assertEquals("native_ticker", slot.viewId)

        val views = HashMap<String, MutableMap<String, Node>>()
        indexByViews(root, views)
        assertEquals(slot, views["native_ticker"]?.get("F-0t0"))
    }

    // A nested stateful child owns its subtree under its own id (the server
    // prefixes that child's ops with its id), while the root stays its own id.
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
