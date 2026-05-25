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
}
