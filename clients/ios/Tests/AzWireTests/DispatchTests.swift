import XCTest
@testable import AzWire

/// Op-dispatch tests driven through the client with a mock transport and a
/// synchronous main-thread shim, asserting on the resulting node tree. This
/// exercises OP_REPLACE / per-view resolve / OP_TEXT / the full stream-op set on
/// any platform -- coverage the Android client only gets via on-device e2e.
final class DispatchTests: XCTestCase {
    private let counterFrame = ##"{"d":["native_counter","0"],"f":"1M2KTR","s":["{\"type\":\"Column\",\"az\":\"1M2KTR-0\",\"az_view\":true,\"id\":",",\"children\":[{\"type\":\"Text\",\"az\":\"1M2KTR-1\",\"children\":[\"Count: \",{\"type\":\"#slot\",\"az\":\"1M2KTR-1t0\",\"children\":[","]}]},{\"type\":\"Button\",\"az\":\"1M2KTR-2\",\"on_tap\":[0,\"inc\"],\"children\":[\"+\"]},{\"type\":\"Button\",\"az\":\"1M2KTR-3\",\"on_tap\":[0,\"dec\"],\"children\":[\"-\"]}]}"]}"##

    private let listFrame = ##"{"d":["native_list",{"d":[["1","One"],["2","Two"],["3","Three"]],"f":"O1M0B","s":["{\"type\":\"Row\",\"az\":\"O1M0B-0\",\"az_key\":",",\"children\":[{\"type\":\"#slot\",\"az\":\"O1M0B-0t0\",\"children\":[","]}]}"],"t":0}],"f":"JW7VZ","s":["{\"type\":\"Column\",\"az\":\"JW7VZ-0\",\"az_view\":true,\"id\":",",\"children\":[{\"type\":\"#slot\",\"az\":\"JW7VZ-0t0\",\"children\":[","]}]}"]}"##

    private func newClient(path: String) -> AzClient {
        AzClient(baseUrl: "http://localhost:4040", path: path, makeTransport: { _ in MockTransport() }, runOnMain: { $0() })
    }

    // The stream container's keyed child rows (root -> #slot container -> rows).
    private func rows(_ client: AzClient) -> [Node] {
        guard let root = client.root, let container = childNodes(root).first else { return [] }
        return childNodes(container)
    }

    private func rowKeys(_ client: AzClient) -> [String] {
        rows(client).map { $0.props["az_key"]?.stringValue ?? "?" }
    }

    private func rowText(_ client: AzClient, key: String) -> String? {
        rows(client).first { $0.props["az_key"]?.stringValue == key }.map(flatText)
    }

    func testReplaceThenTextUpdatesTheCount() {
        let client = newClient(path: "/native/counter")
        client.handleText("{\"o\":[[8,\"native_counter\",\(counterFrame)]]}")

        XCTAssertEqual(client.status, .connected)
        XCTAssertEqual(client.root?.type, "Column")
        XCTAssertTrue(flatText(client.root!).contains("Count: 0"))

        // OP_TEXT against "ViewId:az" -- resolved through the per-view registry.
        client.handleText("{\"o\":[[0,\"native_counter:1M2KTR-1t0\",\"1\"]]}")
        XCTAssertTrue(flatText(client.root!).contains("Count: 1"))
        XCTAssertFalse(flatText(client.root!).contains("Count: 0"))
    }

    func testStreamInsertRemoveMovePatch() {
        let client = newClient(path: "/native/list")
        client.handleText("{\"o\":[[8,\"native_list\",\(listFrame)]]}")
        XCTAssertEqual(rowKeys(client), ["1", "2", "3"])
        XCTAssertEqual(rowText(client, key: "2"), "Two")

        // OP_INSERT (append): pos -1.
        client.handleText("{\"o\":[[5,\"native_list:JW7VZ-0t0\",\"9\",-1,{\"f\":\"O1M0B\",\"d\":[\"9\",\"Nine\"]}]]}")
        XCTAssertEqual(rowKeys(client), ["1", "2", "3", "9"])
        XCTAssertEqual(rowText(client, key: "9"), "Nine")

        // OP_REMOVE by az_key.
        client.handleText("{\"o\":[[6,\"native_list:JW7VZ-0t0\",\"2\"]]}")
        XCTAssertEqual(rowKeys(client), ["1", "3", "9"])

        // OP_MOVE "1" to after "9".
        client.handleText("{\"o\":[[9,\"native_list:JW7VZ-0t0\",\"1\",\"9\"]]}")
        XCTAssertEqual(rowKeys(client), ["3", "9", "1"])

        // OP_ITEM_PATCH "9": an inner OP_TEXT scoped to the item's own az map.
        client.handleText("{\"o\":[[7,\"native_list:JW7VZ-0t0\",\"9\",[[0,\"O1M0B-0t0\",\"Updated\"]]]]}")
        XCTAssertEqual(rowText(client, key: "9"), "Updated")
    }

    func testConnectSendsCachedFpsAndAppliesFirstFrame() {
        var captured: MockTransport?
        let client = AzClient(
            baseUrl: "http://localhost:4040",
            path: "/native/counter",
            makeTransport: { _ in let m = MockTransport(); captured = m; return m },
            runOnMain: { $0() })
        client.connect()
        captured?.simulateOpen()
        XCTAssertEqual(captured?.sent.first, "[\"cached_fps\",[]]")

        captured?.simulateText("{\"o\":[[8,\"native_counter\",\(counterFrame)]]}")
        XCTAssertEqual(client.status, .connected)
        XCTAssertTrue(flatText(client.root!).contains("Count: 0"))
    }

    func testTapRoutesEventToTheEnclosingView() {
        var captured: MockTransport?
        let client = AzClient(
            baseUrl: "http://localhost:4040",
            path: "/native/counter",
            makeTransport: { _ in let m = MockTransport(); captured = m; return m },
            runOnMain: { $0() })
        client.connect()
        captured?.simulateOpen()
        captured?.simulateText("{\"o\":[[8,\"native_counter\",\(counterFrame)]]}")

        let incButton = childNodes(client.root!).first { $0.props["on_tap"]?.arrayValue?[1].stringValue == "inc" }
        client.tap(incButton!)
        XCTAssertEqual(captured?.sent.last, "[\"native_counter\",\"inc\",{}]")
    }

    // A push_event/2 effect carries an explicit payload. On native there is no
    // form/input auto-collection, so the explicit payload is the only way to
    // attach data -- dropping it (defaulting to `{}`) makes a handler matching a
    // required key crash the live process.
    // ----------------------------------------------------------------------
    // The az -> node registry vs. nodes the DIFF creates. Mirrors
    // clients/android .../DispatchTest.kt and e2e/utils/native_client.test.js.
    // ----------------------------------------------------------------------

    // A view root whose single `#slot` (az "R-0t0") holds the swappable content.
    private let rootStatics = ##"["{\"type\":\"Column\",\"az\":\"R-0\",\"az_view\":true,\"id\":",",\"children\":[{\"type\":\"#slot\",\"az\":\"R-0t0\",\"children\":[","]}]}"]"##

    // A stateful CHILD view's subtree: its own `az_view` + `id`, so installing it
    // introduces a whole new view id the server will address ops to.
    private let childStatics = ##"["{\"type\":\"Column\",\"az\":\"C-0\",\"az_view\":true,\"id\":",",\"children\":[{\"type\":\"#slot\",\"az\":\"C-0t0\",\"children\":[","]}]}"]"##

    // A keyed stream item with an empty content slot -- where the `?stateful` a
    // conditional in the item template switches on lands, via an item-patch INNER op.
    private let slotItemStatics = ##"["{\"type\":\"Row\",\"az\":\"I-0\",\"az_key\":",",\"children\":[{\"type\":\"#slot\",\"az\":\"I-0t0\",\"children\":[","]}]}"]"##

    // A keyed stream item wrapping a stateful child view (the `?stateful` inside a
    // stream `?each` shape).
    private let itemStatics =##"["{\"type\":\"Row\",\"az\":\"I-0\",\"az_key\":",",\"children\":[{\"type\":\"Column\",\"az\":\"C-0\",\"az_view\":true,\"id\":",",\"children\":[{\"type\":\"#slot\",\"az\":\"C-0t0\",\"children\":[","]}]}]}"]"##

    private func frame(_ ops: String) -> String { "{\"o\":[\(ops)]}" }

    // OP_REPLACE of the root view with an empty content slot.
    private func replaceRoot(_ client: AzClient) {
        client.handleText(frame(##"[8,"native_x",{"f":"R","s":\##(rootStatics),"d":["native_x",""]}]"##))
    }

    // OP_REPLACE of a stream root holding one keyed item that wraps a child view.
    private func replaceList(_ client: AzClient) {
        client.handleText(
            frame(
                ##"[8,"native_l",{"f":"R","s":\##(rootStatics),"d":["native_l",{"t":0,"f":"I","s":\##(itemStatics),"d":[["k1","child_1","0"]]}]}]"##
            ))
    }

    private func child(_ node: Node, _ i: Int) -> Node {
        guard case let .node(n) = node.children[i] else { preconditionFailure("not a node child") }
        return n
    }

    // The slot's only element child (the content the diff installed).
    private func content(_ client: AzClient) -> Node { child(child(client.root!, 0), 0) }

    // The three-frame repro: OP_REPLACE, an OP_TEXT that installs a subtree, then
    // an op addressed INSIDE that subtree. The registry used to be built only at
    // OP_REPLACE, so every az the second frame introduced was invisible and the
    // third frame hit "unknown target".
    func testAddressesANodeTheDiffCreatedViaOpText() {
        let client = newClient(path: "/native/x")
        replaceRoot(client)
        client.handleText(
            frame(
                ##"[0,"native_x:R-0t0",{"f":"T","s":["{\"type\":\"Text\",\"az\":\"T-0\",\"children\":[\"a\"]}"],"d":[]}]"##
            ))
        client.handleText(frame(##"[1,"native_x:T-0","color","red"]"##))

        let text = content(client)
        XCTAssertEqual(text.type, "Text")
        XCTAssertEqual(text.props["color"]?.stringValue, "red")
    }

    // The documented `case ?get(flag) of true -> ?stateful(child, ...)` pattern:
    // the installed payload carries its OWN view id, which must be registered or
    // the child's very first update crashes.
    func testRegistersAChildViewIdIntroducedByAnOpTextPayload() {
        let client = newClient(path: "/native/x")
        replaceRoot(client)
        client.handleText(
            frame(##"[0,"native_x:R-0t0",{"f":"C","s":\##(childStatics),"d":["cond_child","0"]}]"##))
        XCTAssertEqual(content(client).viewId, "cond_child")
        XCTAssertEqual(flatText(content(client)), "0")

        // An op addressed to the CHILD view, not the root.
        client.handleText(frame(##"[0,"cond_child:C-0t0","1"]"##))
        XCTAssertEqual(flatText(content(client)), "1")
    }

    // A rebuilt slot must not leave the destroyed subtree's azs in the registry,
    // or the map retains detached nodes for the life of the connection.
    func testDropsTheEntriesOfASubtreeAnOpTextReplaced() {
        let client = newClient(path: "/native/x")
        replaceRoot(client)
        client.handleText(
            frame(##"[0,"native_x:R-0t0",{"f":"C","s":\##(childStatics),"d":["cond_child","0"]}]"##))
        XCTAssertNotNil(client.views["cond_child"])
        client.handleText(frame(##"[0,"native_x:R-0t0",""]"##))
        XCTAssertNil(client.views["cond_child"])
    }

    // A `?stateful` child inside a stream item is addressed by the server through
    // a `[ChildViewId, ChildOps]` wrapper nested in the item patch (flatten_ops/2
    // only unwraps that at top level), so the first element is a view-id STRING.
    func testAppliesAChildViewOpWrapperNestedInAnItemPatch() {
        let client = newClient(path: "/native/l")
        replaceList(client)
        let item = child(child(client.root!, 0), 0)
        XCTAssertEqual(flatText(item), "0")

        client.handleText(frame(##"[7,"native_l:R-0t0","k1",[["child_1",[[0,"C-0t0","9"]]]]]"##))
        XCTAssertEqual(flatText(item), "9")
    }

    // A conditional `?stateful` INSIDE a stream item installs the child through an
    // item-patch INNER op, so its view id never appears in a top-level op -- but the
    // child's own ops come back top-level. Indexing an inner rebuild only into the
    // item-local map leaves it unaddressable and its slot frozen.
    func testRegistersAChildViewAnItemPatchInnerOpInstalled() {
        let client = newClient(path: "/native/l")
        client.handleText(
            frame(
                ##"[8,"native_l",{"f":"R","s":\##(rootStatics),"d":["native_l",{"t":0,"f":"S","s":\##(slotItemStatics),"d":[["k1",""]]}]}]"##
            ))
        client.handleText(
            frame(
                ##"[7,"native_l:R-0t0","k1",[[0,"I-0t0",{"f":"C","s":\##(childStatics),"d":["inner_kid","0"]}]]]"##
            ))
        let item = child(child(client.root!, 0), 0)
        XCTAssertEqual(flatText(item), "0")

        // A TOP-LEVEL op addressed to the view the inner op created.
        client.handleText(frame(##"[0,"inner_kid:C-0t0","5"]"##))
        XCTAssertEqual(flatText(item), "5")
    }

    // An inserted stream item's child view is a new view id too -- OP_REPLACE
    // already indexes the items it renders, so an insert must as well.
    func testRegistersAChildViewInsideAnInsertedStreamItem() {
        let client = newClient(path: "/native/l")
        replaceList(client)
        client.handleText(frame(##"[5,"native_l:R-0t0","k2",-1,{"f":"I","d":["k2","child_2","0"]}]"##))
        client.handleText(frame(##"[0,"child_2:C-0t0","7"]"##))

        XCTAssertEqual(flatText(child(child(client.root!, 0), 1)), "7")
    }

    // Per-op isolation: an unresolvable target and an op code this client does not
    // implement (e.g. OP_LIST_PATCH) must degrade those slots only.
    func testSkipsABadOpWithoutDroppingTheRestOfTheBatch() {
        let client = newClient(path: "/native/x")
        replaceRoot(client)
        client.handleText(
            frame(
                ##"[0,"native_x:nope","ignored"],[10,"native_x:R-0t0","unimplemented op code"],[0,"native_x:R-0t0","applied"]"##
            ))
        XCTAssertEqual(flatText(child(client.root!, 0)), "applied")
    }

    // A malformed payload throws inside the op body; the batch must survive it.
    func testSkipsAnOpWhosePayloadIsMalformed() {
        let client = newClient(path: "/native/x")
        replaceRoot(client)
        client.handleText(
            frame(
                ##"[0,"native_x:R-0t0",{"f":"never-cached","d":[]}],[0,"native_x:R-0t0","applied"]"##
            ))
        XCTAssertEqual(flatText(child(client.root!, 0)), "applied")
    }

    // A payload whose statics yield something that is not a node (no `type`) fails
    // inside `buildTree`, not the decoder. `buildTree`/`addChild` therefore throw
    // instead of trapping: they run inside the per-op `do`/`catch`, and a Swift trap
    // is not catchable, so a `preconditionFailure` there would kill the app over one
    // slot (Kotlin's counterpart raises, and its catch already handles it).
    func testSkipsAnOpWhosePayloadIsNotANode() {
        let client = newClient(path: "/native/x")
        replaceRoot(client)
        client.handleText(
            frame(
                ##"[0,"native_x:R-0t0",{"f":"B","s":["{\"az\":\"B-0\",\"children\":[]}"],"d":[]}],[0,"native_x:R-0t0","applied"]"##
            ))
        XCTAssertEqual(flatText(child(client.root!, 0)), "applied")
    }

    // The announcement is what lets the server elide statics the client already
    // holds; a hardcoded empty list re-ships every template on every reconnect.
    func testAnnouncesTheFingerprintsItActuallyCached() {
        let client = newClient(path: "/native/x")
        replaceRoot(client)
        XCTAssertEqual(client.cachedFpsFrame(), ##"["cached_fps",["R"]]"##)
    }

    func testTapCarriesTheExplicitPayload() {
        var captured: MockTransport?
        let client = AzClient(
            baseUrl: "http://localhost:4040",
            path: "/native/counter",
            makeTransport: { _ in let m = MockTransport(); captured = m; return m },
            runOnMain: { $0() })
        client.connect()
        captured?.simulateOpen()
        // Turn the `inc` button's on_tap into a push_event with a payload.
        let frame = counterFrame.replacingOccurrences(
            of: ##"[0,\"inc\"]"##, with: ##"[0,\"save\",{\"id\":\"42\"}]"##)
        captured?.simulateText("{\"o\":[[8,\"native_counter\",\(frame)]]}")

        let saveButton = childNodes(client.root!).first { $0.props["on_tap"]?.arrayValue?[1].stringValue == "save" }
        client.tap(saveButton!)
        XCTAssertEqual(captured?.sent.last, "[\"native_counter\",\"save\",{\"id\":\"42\"}]")
    }
}
