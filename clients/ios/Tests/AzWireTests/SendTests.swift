import XCTest
@testable import AzWire

/// Outgoing frames the client cannot send are reported, not silently lost. A tap
/// whose event never left the device looks exactly like one the server ignored,
/// so without the report an e2e failure cannot say which side dropped it.
final class SendTests: XCTestCase {
    private let mount = ##"{"o":[[8,"v",{"d":["v"],"f":"F1","s":["{\"type\":\"Column\",\"az\":\"F1-0\",\"az_view\":true,\"id\":","}"]}]]}"##

    private var warnings: [String] = []
    private var savedSink: ((String) -> Void)!

    override func setUp() {
        warnings = []
        savedSink = azWarnSink
        azWarnSink = { [unowned self] in self.warnings.append($0) }
    }

    override func tearDown() {
        azWarnSink = savedSink
    }

    private func client(_ socket: MockTransport) -> AzClient {
        AzClient(
            baseUrl: "http://localhost:4040",
            path: "/json/v",
            makeTransport: { _ in socket },
            runOnMain: { $0() },
            runAfter: { _, _ in })
    }

    func testEventBeforeAnyViewMountedIsReported() {
        let socket = MockTransport()
        let c = client(socket)
        c.connect()
        socket.simulateOpen()

        c.pushEvent("inc")

        XCTAssertEqual(socket.sent.count, 1, "only the cached_fps announcement goes out")
        XCTAssertEqual(warnings, [#"event "inc" dropped: no view mounted yet"#])
    }

    func testEventAfterTheSocketDroppedIsReported() {
        let socket = MockTransport()
        let c = client(socket)
        c.connect()
        socket.simulateOpen()
        socket.simulateText(mount)
        socket.simulateClose(1006)

        c.pushEvent("inc")

        XCTAssertEqual(socket.sent.count, 1, "the event must not reach the dead socket")
        XCTAssertEqual(warnings, [#"frame dropped: no socket: ["v","inc",{}]"#])
    }

    func testNavigateBeforeConnectIsReported() {
        let c = client(MockTransport())

        c.navigate("/json/menu")

        // The frame's object keys come out in no fixed order, so compare it decoded.
        let prefix = "frame dropped: no socket: "
        XCTAssertEqual(warnings.count, 1)
        XCTAssertEqual(warnings.first?.hasPrefix(prefix), true)
        XCTAssertEqual(
            warnings.first.map { JSONValue.parse(String($0.dropFirst(prefix.count))) },
            JSONValue.parse(#"["navigate",{"path":"/json/menu","qs":""}]"#))
    }

    func testEventOnAMountedViewIsSentWithoutAReport() {
        let socket = MockTransport()
        let c = client(socket)
        c.connect()
        socket.simulateOpen()
        socket.simulateText(mount)

        c.pushEvent("inc")

        XCTAssertEqual(socket.sent.last, #"["v","inc",{}]"#)
        XCTAssertEqual(warnings, [])
    }
}
