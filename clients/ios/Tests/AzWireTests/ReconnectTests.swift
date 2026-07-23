import XCTest
@testable import AzWire

/// Reconnect-path tests: one socket at a time, no matter how a transport reports
/// a drop. `URLSessionWebSocketTask` reports one drop through both the failed
/// `receive` and the delegate's `didCloseWith`, and the two can arrive seconds
/// apart -- so the client cannot assume a close is delivered exactly once, and it
/// must ignore a close belonging to a socket it has already replaced.
///
/// Two concurrent sockets means two live processes on the server: the second
/// mount replaces the rendered tree from scratch, so state produced on the first
/// one vanishes. That is what flaked the Simulator reconnect e2e -- the tap after
/// a reconnect incremented one live process while a second mount reset the view.
final class ReconnectTests: XCTestCase {
    private let counterFrame = ##"{"d":["native_counter","0"],"f":"1M2KTR","s":["{\"type\":\"Column\",\"az\":\"1M2KTR-0\",\"az_view\":true,\"id\":",",\"children\":[{\"type\":\"Text\",\"az\":\"1M2KTR-1\",\"children\":[\"Count: \",{\"type\":\"#slot\",\"az\":\"1M2KTR-1t0\",\"children\":[","]}]},{\"type\":\"Button\",\"az\":\"1M2KTR-2\",\"on_tap\":[0,\"inc\"],\"children\":[\"+\"]},{\"type\":\"Button\",\"az\":\"1M2KTR-3\",\"on_tap\":[0,\"dec\"],\"children\":[\"-\"]}]}"]}"##

    /// What the injected seams recorded: one entry per socket the client opened
    /// and one per reconnect it scheduled.
    private final class Recorder {
        var sockets: [MockTransport] = []
        var scheduled: [DispatchWorkItem] = []
    }

    /// A client whose sockets and backoff timer are both under the test's control:
    /// every `connect()` appends a fresh mock, and every scheduled reconnect lands
    /// in the recorder instead of on the main queue.
    private final class Harness {
        let client: AzClient
        private let rec: Recorder

        init() {
            let rec = Recorder()
            self.rec = rec
            client = AzClient(
                baseUrl: "http://localhost:4040",
                path: "/native/counter",
                makeTransport: { _ in
                    let socket = MockTransport()
                    rec.sockets.append(socket)
                    return socket
                },
                runOnMain: { $0() },
                runAfter: { _, item in rec.scheduled.append(item) })
        }

        var socketCount: Int { rec.sockets.count }

        /// Reconnects scheduled but not yet run, ignoring cancelled ones.
        var liveReconnects: [DispatchWorkItem] { rec.scheduled.filter { !$0.isCancelled } }

        /// The socket the client would send on right now.
        var current: MockTransport { rec.sockets[rec.sockets.count - 1] }

        /// Run every reconnect the client has scheduled so far.
        func runReconnects() {
            for item in liveReconnects { item.perform() }
        }
    }

    private func mountFrame(count: String) -> String {
        let frame = counterFrame.replacingOccurrences(of: ##"["native_counter","0"]"##, with: ##"["native_counter","\##(count)"]"##)
        return "{\"o\":[[8,\"native_counter\",\(frame)]]}"
    }

    /// Bring a harness up to a rendered counter on its first socket.
    private func connected() -> Harness {
        let h = Harness()
        h.client.connect()
        h.current.simulateOpen()
        h.current.simulateText(mountFrame(count: "0"))
        XCTAssertEqual(h.client.status, .connected)
        return h
    }

    // One drop reported twice must still open one socket. Honoring the second
    // report opens a parallel socket whose own mount frame resets the view.
    func testDuplicateCloseReportOpensOneSocket() {
        let h = connected()
        let dropped = h.current

        dropped.simulateClose(1006)
        dropped.simulateClose(1006) // the same drop, reported a second time

        XCTAssertEqual(h.liveReconnects.count, 1, "one drop must schedule one reconnect")
        h.runReconnects()
        XCTAssertEqual(h.socketCount, 2, "expected exactly one replacement socket")
    }

    // The e2e shape: drop, reconnect, render -- then the dead socket reports its
    // drop again. That late report must not disturb the healthy socket, or the
    // next tap increments a live process the UI is no longer showing.
    func testLateCloseFromRetiredSocketLeavesTheLiveSocketAlone() {
        let h = connected()
        let dropped = h.current

        dropped.simulateClose(1006)
        h.runReconnects()
        let reopened = h.current
        reopened.simulateOpen()
        reopened.simulateText(mountFrame(count: "0"))
        XCTAssertEqual(h.client.status, .connected)

        // The retired socket finally reports its drop.
        dropped.simulateClose(1006)

        XCTAssertEqual(h.client.status, .connected, "a retired socket must not disconnect the live one")
        XCTAssertTrue(h.liveReconnects.isEmpty, "a retired socket must not schedule a reconnect")

        // The tap still reaches the socket that rendered what the user sees.
        let inc = childNodes(h.client.root!).first { $0.props["on_tap"]?.arrayValue?[1].stringValue == "inc" }
        h.client.tap(inc!)
        XCTAssertEqual(reopened.sent.last, "[\"native_counter\",\"inc\",{}]")
        XCTAssertEqual(h.socketCount, 2, "no third socket")
    }

    // A frame from a retired socket must not be applied: it would re-mount the
    // view and throw away what the live socket has rendered.
    func testFrameFromRetiredSocketIsDropped() {
        let h = connected()
        let dropped = h.current

        dropped.simulateClose(1006)
        h.runReconnects()
        let reopened = h.current
        reopened.simulateOpen()
        reopened.simulateText(mountFrame(count: "0"))
        reopened.simulateText("{\"o\":[[0,\"native_counter:1M2KTR-1t0\",\"1\"]]}")
        XCTAssertTrue(flatText(h.client.root!).contains("Count: 1"))

        dropped.simulateText(mountFrame(count: "0"))

        XCTAssertTrue(
            flatText(h.client.root!).contains("Count: 1"),
            "a retired socket's frame must not re-mount the view")
    }

    // connect() must retire the socket it replaces: an orphaned socket keeps its
    // live process on the server and keeps pushing frames the UI would apply.
    func testConnectRetiresTheSocketItReplaces() {
        let h = connected()
        let first = h.current

        h.client.connect()
        h.current.simulateOpen()
        h.current.simulateText(mountFrame(count: "0"))

        XCTAssertTrue(first.cancelled, "the replaced socket must be closed")
        XCTAssertTrue(h.liveReconnects.isEmpty, "closing the replaced socket must not reconnect")

        first.simulateText(mountFrame(count: "5"))
        XCTAssertTrue(
            flatText(h.client.root!).contains("Count: 0"),
            "a replaced socket's frame must not be applied")
    }
}
