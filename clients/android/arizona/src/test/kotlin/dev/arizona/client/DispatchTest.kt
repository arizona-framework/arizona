package dev.arizona.client

import org.junit.Assert.assertEquals
import org.junit.Assert.assertNotNull
import org.junit.Assert.assertNull
import org.junit.Test

/**
 * Pure JVM tests (no emulator) of op application: frames go in through
 * [AzClient.handleText], the resulting node tree comes out. The focus is the
 * `az -> node` registry vs. nodes the DIFF creates -- coverage the on-device e2e
 * cannot give cheaply. Mirrors clients/ios Tests/AzWireTests/DispatchTests.swift
 * and e2e/utils/native_client.test.js.
 *
 * Run with ./gradlew :arizona:testDebugUnitTest
 */
class DispatchTest {

    // A view root whose single `#slot` (az "R-0t0") holds the swappable content.
    private val rootStatics =
        """["{\"type\":\"Column\",\"az\":\"R-0\",\"az_view\":true,\"id\":",""" +
            """",\"children\":[{\"type\":\"#slot\",\"az\":\"R-0t0\",\"children\":[","]}]}"]"""

    // A stateful CHILD view's subtree: its own `az_view` + `id`, so installing it
    // introduces a whole new view id the server will address ops to.
    private val childStatics =
        """["{\"type\":\"Column\",\"az\":\"C-0\",\"az_view\":true,\"id\":",""" +
            """",\"children\":[{\"type\":\"#slot\",\"az\":\"C-0t0\",\"children\":[","]}]}"]"""

    // A keyed stream item wrapping a stateful child view (the `?stateful` inside a
    // stream `?each` shape).
    private val itemStatics =
        """["{\"type\":\"Row\",\"az\":\"I-0\",\"az_key\":",""" +
            """",\"children\":[{\"type\":\"Column\",\"az\":\"C-0\",\"az_view\":true,\"id\":",""" +
            """",\"children\":[{\"type\":\"#slot\",\"az\":\"C-0t0\",\"children\":[","]}]}]}"]"""

    private fun newClient() = AzClient("http://localhost:4040", "/native/x")

    private fun frame(ops: String) = """{"o":[$ops]}"""

    // OP_REPLACE of the root view with an empty content slot.
    private fun replaceRoot(client: AzClient) {
        client.handleText(
            frame("""[8,"native_x",{"f":"R","s":$rootStatics,"d":["native_x",""]}]"""),
        )
    }

    // OP_REPLACE of a stream root holding one keyed item that wraps a child view.
    private fun replaceList(client: AzClient) {
        client.handleText(
            frame(
                """[8,"native_l",{"f":"R","s":$rootStatics,"d":["native_l",""" +
                    """{"t":0,"f":"I","s":$itemStatics,"d":[["k1","child_1","0"]]}]}]""",
            ),
        )
    }

    private fun node(parent: Node, i: Int) = parent.children[i] as Node

    // The slot's only element child (the content the diff installed).
    private fun content(client: AzClient) = node(node(client.root.value!!, 0), 0)

    // The three-frame repro: OP_REPLACE, an OP_TEXT that installs a subtree, then
    // an op addressed INSIDE that subtree. The registry used to be built only at
    // OP_REPLACE, so every az the second frame introduced was invisible and the
    // third frame hit "unknown target".
    @Test
    fun addressesANodeTheDiffCreatedViaOpText() {
        val client = newClient()
        replaceRoot(client)
        client.handleText(
            frame(
                """[0,"native_x:R-0t0",""" +
                    """{"f":"T","s":["{\"type\":\"Text\",\"az\":\"T-0\",\"children\":[\"a\"]}"],""" +
                    """"d":[]}]""",
            ),
        )
        client.handleText(frame("""[1,"native_x:T-0","color","red"]"""))

        val text = content(client)
        assertEquals("Text", text.type)
        assertEquals("\"red\"", text.props["color"].toString())
    }

    // The documented `case ?get(flag) of true -> ?stateful(child, ...)` pattern:
    // the installed payload carries its OWN view id, which must be registered or
    // the child's very first update crashes.
    @Test
    fun registersAChildViewIdIntroducedByAnOpTextPayload() {
        val client = newClient()
        replaceRoot(client)
        client.handleText(
            frame("""[0,"native_x:R-0t0",{"f":"C","s":$childStatics,"d":["cond_child","0"]}]"""),
        )
        val child = content(client)
        assertEquals("cond_child", child.viewId)
        assertEquals("0", node(child, 0).children[0])

        // An op addressed to the CHILD view, not the root.
        client.handleText(frame("""[0,"cond_child:C-0t0","1"]"""))
        assertEquals("1", node(content(client), 0).children[0])
    }

    // A rebuilt slot must not leave the destroyed subtree's azs in the registry,
    // or the map retains detached nodes for the life of the connection.
    @Test
    fun dropsTheEntriesOfASubtreeAnOpTextReplaced() {
        val client = newClient()
        replaceRoot(client)
        client.handleText(
            frame("""[0,"native_x:R-0t0",{"f":"C","s":$childStatics,"d":["cond_child","0"]}]"""),
        )
        assertNotNull(client.views["cond_child"])
        client.handleText(frame("""[0,"native_x:R-0t0",""]"""))
        assertNull(client.views["cond_child"])
    }

    // A `?stateful` child inside a stream item is addressed by the server through
    // a `[ChildViewId, ChildOps]` wrapper nested in the item patch (flatten_ops/2
    // only unwraps that at top level), so the first element is a view-id STRING.
    @Test
    fun appliesAChildViewOpWrapperNestedInAnItemPatch() {
        val client = newClient()
        replaceList(client)
        val item = node(node(client.root.value!!, 0), 0)
        assertEquals("0", node(node(item, 0), 0).children[0])

        client.handleText(frame("""[7,"native_l:R-0t0","k1",[["child_1",[[0,"C-0t0","9"]]]]]"""))
        assertEquals("9", node(node(item, 0), 0).children[0])
    }

    // An inserted stream item's child view is a new view id too -- OP_REPLACE
    // already indexes the items it renders, so an insert must as well.
    @Test
    fun registersAChildViewInsideAnInsertedStreamItem() {
        val client = newClient()
        replaceList(client)
        client.handleText(
            frame("""[5,"native_l:R-0t0","k2",-1,{"f":"I","d":["k2","child_2","0"]}]"""),
        )
        client.handleText(frame("""[0,"child_2:C-0t0","7"]"""))

        val inserted = node(node(client.root.value!!, 0), 1)
        assertEquals("7", node(node(inserted, 0), 0).children[0])
    }

    // Per-op isolation: an unresolvable target and an op code this client does not
    // implement (e.g. OP_LIST_PATCH) must degrade those slots only.
    @Test
    fun skipsABadOpWithoutDroppingTheRestOfTheBatch() {
        val client = newClient()
        replaceRoot(client)
        client.handleText(
            frame(
                """[0,"native_x:nope","ignored"],""" +
                    """[10,"native_x:R-0t0","unimplemented op code"],""" +
                    """[0,"native_x:R-0t0","applied"]""",
            ),
        )
        assertEquals("applied", node(client.root.value!!, 0).children[0])
    }

    // A malformed payload throws inside the op body; the batch must survive it.
    @Test
    fun skipsAnOpWhosePayloadIsMalformed() {
        val client = newClient()
        replaceRoot(client)
        client.handleText(
            frame(
                """[0,"native_x:R-0t0",{"f":"never-cached","d":[]}],""" +
                    """[0,"native_x:R-0t0","applied"]""",
            ),
        )
        assertEquals("applied", node(client.root.value!!, 0).children[0])
    }

    // The announcement is what lets the server elide statics the client already
    // holds; a hardcoded empty list re-ships every template on every reconnect.
    @Test
    fun announcesTheFingerprintsItActuallyCached() {
        val client = newClient()
        replaceRoot(client)
        assertEquals("""["cached_fps",["R"]]""", client.cachedFpsFrame())
    }
}
