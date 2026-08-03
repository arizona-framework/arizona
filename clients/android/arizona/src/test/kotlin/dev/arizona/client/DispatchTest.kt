package dev.arizona.client

import org.junit.Assert.assertEquals
import org.junit.Assert.assertNotNull
import org.junit.Assert.assertNull
import org.junit.Assert.assertTrue
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

    // A keyed stream item with an empty content slot -- where the `?stateful` a
    // conditional in the item template switches on lands, via an item-patch INNER op.
    private val slotItemStatics =
        """["{\"type\":\"Row\",\"az\":\"I-0\",\"az_key\":",""" +
            """",\"children\":[{\"type\":\"#slot\",\"az\":\"I-0t0\",\"children\":[","]}]}"]"""

    // A keyed stream item wrapping a stateful child view (the `?stateful` inside a
    // stream `?each` shape).
    private val itemStatics =
        """["{\"type\":\"Row\",\"az\":\"I-0\",\"az_key\":",""" +
            """",\"children\":[{\"type\":\"Column\",\"az\":\"C-0\",\"az_view\":true,\"id\":",""" +
            """",\"children\":[{\"type\":\"#slot\",\"az\":\"C-0t0\",\"children\":[","]}]}]}"]"""

    // A keyed stream item wrapping a nested each, so two CELLS inside one item
    // share an `az` -- the item-local registry's equivalent of two stream items
    // sharing one.
    private val nestedItemStatics =
        """["{\"type\":\"Row\",\"az\":\"I-0\",\"az_key\":",""" +
            """",\"children\":[{\"type\":\"#slot\",\"az\":\"I-0t0\",\"children\":[","]}]}"]"""
    private val cellStatics =
        """["{\"type\":\"Cell\",\"az\":\"N-0\",\"az_key\":",",\"children\":[\"x\"]}"]"""

    private fun newClient() = AzClient("http://localhost:4040", "/native/x")

    // OP_REPLACE of a stream root holding `keys`, all sharing one item fingerprint
    // (so every item carries the SAME az values -- the collision the identity check
    // in unindexByViews exists for).
    private fun replaceSharedAzList(client: AzClient, keys: List<String>) {
        val items = keys.joinToString(",") { """["$it",""]""" }
        client.handleText(
            frame(
                """[8,"native_l",{"f":"R","s":$rootStatics,"d":["native_l",""" +
                    """{"t":0,"f":"S","s":$slotItemStatics,"d":[$items]}]}]""",
            ),
        )
    }

    // The stream items under the root's content slot.
    private fun items(client: AzClient) = node(client.root.value!!, 0).children

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

    // A conditional `?stateful` INSIDE a stream item installs the child through an
    // item-patch INNER op, so its view id never appears in a top-level op -- but the
    // child's own ops come back top-level. Indexing an inner rebuild only into the
    // item-local map leaves it unaddressable and its slot frozen.
    @Test
    fun registersAChildViewAnItemPatchInnerOpInstalled() {
        val client = newClient()
        client.handleText(
            frame(
                """[8,"native_l",{"f":"R","s":$rootStatics,"d":["native_l",""" +
                    """{"t":0,"f":"S","s":$slotItemStatics,"d":[["k1",""]]}]}]""",
            ),
        )
        client.handleText(
            frame(
                """[7,"native_l:R-0t0","k1",""" +
                    """[[0,"I-0t0",{"f":"C","s":$childStatics,"d":["inner_kid","0"]}]]]""",
            ),
        )
        // Row -> #slot I-0t0 -> Column(inner_kid) -> #slot C-0t0 -> text.
        val item = node(node(client.root.value!!, 0), 0)
        val kidSlot = { node(node(node(item, 0), 0), 0).children[0] }
        assertEquals("0", kidSlot())

        // A TOP-LEVEL op addressed to the view the inner op created.
        client.handleText(frame("""[0,"inner_kid:C-0t0","5"]"""))
        assertEquals("5", kidSlot())
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

    // Spec parity across the three clients for an OP_INSERT position the server
    // never sends: any out-of-range value appends. Left as `== -1`, a raw negative
    // index is an exception here, "count from the end" in JS, and a TRAP in Swift.
    @Test
    fun appendsAnInsertWhosePositionIsOutOfRange() {
        val client = newClient()
        replaceList(client)
        client.handleText(
            frame("""[5,"native_l:R-0t0","k2",-5,{"f":"I","d":["k2","child_2","0"]}]"""),
        )
        val slot = node(client.root.value!!, 0)
        assertEquals(2, slot.children.size)
        assertEquals("\"k2\"", (slot.children[1] as Node).props["az_key"].toString())
    }

    // Spec parity: an inserted item that is ITSELF a view root owns its subtree
    // under its own id. Stamping it with the container's view instead registers it
    // in the parent's registry and its ops resolve nowhere.
    @Test
    fun registersAnInsertedItemThatIsItselfAViewRoot() {
        val client = newClient()
        replaceList(client)
        // The item payload's root carries az_view + id, unlike the usual az_key row.
        client.handleText(
            frame("""[5,"native_l:R-0t0","k2",-1,{"f":"C","s":$childStatics,"d":["solo","0"]}]"""),
        )
        assertNotNull(client.views["solo"])

        client.handleText(frame("""[0,"solo:C-0t0","7"]"""))
        val inserted = node(node(client.root.value!!, 0), 1)
        assertEquals("7", node(inserted, 0).children[0])
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

    // A payload whose statics yield something that is not a node (no `type`) fails
    // inside the tree builder, not the decoder -- it still has to cost one op. The
    // iOS mirror needs this stated explicitly: a Swift trap is not catchable, so
    // that client's builder throws rather than calling `preconditionFailure`.
    @Test
    fun skipsAnOpWhosePayloadIsNotANode() {
        val client = newClient()
        replaceRoot(client)
        client.handleText(
            frame(
                """[0,"native_x:R-0t0",{"f":"B","s":["{\"az\":\"B-0\",\"children\":[]}"],"d":[]}],""" +
                    """[0,"native_x:R-0t0","applied"]""",
            ),
        )
        assertEquals("applied", node(client.root.value!!, 0).children[0])
    }

    // The announcement is what lets the server elide statics the client already
    // holds; a hardcoded empty list re-ships every template on every reconnect.
    // Asserted on what the open handshake TRANSMITS, not on the helper's return
    // value: a helper-only assertion leaves the wiring free to revert.
    @Test
    fun transmitsTheCachedFingerprintsOnEverySocketOpen() {
        val client = newClient()
        val sent = mutableListOf<String>()
        // First open: nothing cached yet.
        client.handleOpen { sent.add(it) }
        assertEquals("""["cached_fps",[]]""", sent[0])

        // A frame arrives and caches fingerprint "R" ...
        replaceRoot(client)
        // ... so the next open announces it instead of an empty list.
        client.handleOpen { sent.add(it) }
        assertEquals("""["cached_fps",["R"]]""", sent[1])
    }

    // A frame that will not parse, and a malformed effect command, are both server
    // output arriving on the main looper: an exception escaping here is not a lost
    // frame, it is the app dying. (`[0]` is a push_event with no event name.)
    @Test
    fun survivesAnUnparseableFrameAndAMalformedEffectCommand() {
        val client = newClient()
        replaceRoot(client)
        client.handleText("not json")
        client.handleText("""{"e":[[0]]}""")

        // Still live: a well-formed frame after both still applies.
        client.handleText(frame("""[0,"native_x:R-0t0","applied"]"""))
        assertEquals("applied", node(client.root.value!!, 0).children[0])
    }

    // Since OP_INSERT indexes what it grafts in, OP_REMOVE has to unindex what it
    // drops -- otherwise a churning stream grows the registry (and pins every
    // detached subtree) once per cycle, for the life of the connection. Each item
    // here carries a child view with its own id, so the leak is unambiguous.
    @Test
    fun doesNotGrowTheRegistryAcrossInsertRemoveCycles() {
        val client = newClient()
        replaceList(client)
        val baseline = client.views.getValue("native_l").size
        for (i in 0 until 50) {
            client.handleText(
                frame("""[5,"native_l:R-0t0","x$i",-1,{"f":"I","d":["x$i","kid$i",""]}]"""),
            )
            client.handleText(frame("""[6,"native_l:R-0t0","x$i"]"""))
        }
        // Only the root view and the surviving item's child view are left.
        assertEquals(listOf("child_1", "native_l"), client.views.keys.sorted())
        // The root view's own entries never grow either. (They can SHRINK: every
        // item shares one fingerprint's az values, so the entry for a shared az
        // names whichever item was indexed last, and removing that item drops it.
        // Harmless -- a stream item is only ever addressed through its container by
        // `az_key`, never by "ViewId:az".)
        assertTrue(client.views.getValue("native_l").size <= baseline)
    }

    // A child view an inner op installed, then removed with its item, must not
    // leave a live registry entry pointing into the detached subtree.
    @Test
    fun dropsARemovedItemChildViewFromTheRegistry() {
        val client = newClient()
        replaceSharedAzList(client, listOf("k1"))
        client.handleText(
            frame(
                """[7,"native_l:R-0t0","k1",""" +
                    """[[0,"I-0t0",{"f":"C","s":$childStatics,"d":["kid","0"]}]]]""",
            ),
        )
        assertNotNull(client.views["kid"])
        client.handleText(frame("""[6,"native_l:R-0t0","k1"]"""))
        assertNull(client.views["kid"])
    }

    // The `remove` sentinel drops a node one-way, so its registry entries are dead
    // the moment the splice lands -- including a whole child view's.
    @Test
    fun dropsANodeRemovedByTheRemoveSentinelFromTheRegistry() {
        val client = newClient()
        replaceRoot(client)
        client.handleText(
            frame("""[0,"native_x:R-0t0",{"f":"C","s":$childStatics,"d":["cond_child","0"]}]"""),
        )
        assertNotNull(client.views["cond_child"])

        client.handleText(frame("""[4,"cond_child:C-0"]"""))
        assertNull(client.views["cond_child"])
        assertEquals(0, node(client.root.value!!, 0).children.size)
    }

    // THE identity check, per-view half: stream items share az values (one
    // fingerprint, many items), so the registry entry for a shared az names
    // whichever item was indexed last. Unindexing a destroyed item by key alone
    // would delete the entry naming a LIVE sibling.
    @Test
    fun keepsASurvivingSiblingRegisteredWhenAnItemSharingItsAzIsRemoved() {
        val client = newClient()
        replaceSharedAzList(client, listOf("k1", "k2"))
        val registry = client.views.getValue("native_l")
        // Last-indexed wins: the entry names item k2's slot.
        val survivorSlot = node(items(client)[1] as Node, 0)
        assertEquals(survivorSlot, registry["I-0t0"])

        client.handleText(frame("""[6,"native_l:R-0t0","k1"]"""))
        assertEquals(survivorSlot, registry["I-0t0"])
        assertEquals(items(client)[0], registry["I-0"])
    }

    // THE identity check, item-local half: two cells of a nested each inside one
    // item share an az, so an inner OP_REMOVE must not unregister the survivor -- a
    // later inner op naming that az would then fall back to the ITEM and overwrite
    // the whole row.
    @Test
    fun keepsASurvivingNestedCellResolvableWhenItsAzSharingSiblingIsRemoved() {
        val client = newClient()
        client.handleText(
            frame(
                """[8,"native_l",{"f":"R","s":$rootStatics,"d":["native_l",""" +
                    """{"t":0,"f":"NI","s":$nestedItemStatics,"d":[["k1",""" +
                    """{"t":0,"f":"CE","s":$cellStatics,"d":[["n1"],["n2"]]}]]}]}]""",
            ),
        )
        client.handleText(
            frame("""[7,"native_l:R-0t0","k1",[[6,"I-0t0","n1"],[0,"N-0","patched"]]]"""),
        )
        // The surviving cell took the patch, and the row is still a row.
        val row = items(client)[0] as Node
        val cell = node(node(row, 0), 0)
        assertEquals("Cell", cell.type)
        assertEquals("patched", cell.children[0])
    }

    // Build before committing: a malformed OP_REPLACE must leave the previous tree
    // AND registry intact rather than half-clearing them. The payload has to fail
    // LATE -- statics that interleave and parse fine but yield a node with no
    // `type` -- since an uncached fingerprint dies before either ordering commits
    // anything and so cannot tell them apart.
    @Test
    fun keepsThePreviousTreeAndRegistryWhenAnOpReplacePayloadIsBad() {
        val client = newClient()
        replaceRoot(client)
        val before = client.root.value

        client.handleText(
            frame("""[8,"native_x",{"f":"BAD","s":["{\"az\":\"B-0\",\"children\":[]}"],"d":[]}]"""),
        )
        assertEquals(before, client.root.value)
        assertNotNull(client.views["native_x"])

        // The old registry still resolves, so the view keeps patching.
        client.handleText(frame("""[0,"native_x:R-0t0","still here"]"""))
        assertEquals("still here", node(client.root.value!!, 0).children[0])
    }
}
