import SwiftUI
import AzWire
import AzClientUI

// The Simulator shares the Mac's loopback, so it reaches the local test server
// directly -- no tunnel (unlike Android's `adb reverse`).
private let serverBaseURL = "http://localhost:4040"

struct ContentView: View {
    @StateObject private var client: ArizonaClient
    private let showDebugToolbar: Bool

    // The app's widget vocabulary -- the iOS counterpart of MainActivity's
    // Column/Row/Text/Button registrations. The library renders the tree by
    // dispatching each node's `type` to one of these.
    private let registry: WidgetRegistry = {
        WidgetRegistry()
            .register("Column") { ctx, node in
                AnyView(VStack(alignment: .leading, spacing: 8) { ctx.children(node) }.padding(16))
            }
            .register("Row") { ctx, node in
                AnyView(HStack(spacing: 8) { ctx.children(node) })
            }
            .register("Text") { ctx, node in
                AnyView(Text(ctx.text(node)))
            }
            .register("Button") { ctx, node in
                AnyView(Button(ctx.text(node)) { ctx.tap(node) })
            }
    }()

    init() {
        let args = ProcessInfo.processInfo.arguments
        var path = "/native/menu"
        if let i = args.firstIndex(of: "-az-path"), i + 1 < args.count {
            path = args[i + 1]
        }
        let raw = AzClient(
            baseUrl: serverBaseURL,
            path: path,
            makeTransport: { LoggingTransport(URLSessionWebSocketTransport(url: $0)) })
        _client = StateObject(wrappedValue: ArizonaClient(raw))
        showDebugToolbar = args.contains("-uitest")
    }

    var body: some View {
        VStack(alignment: .leading, spacing: 8) {
            Button("\u{2630} Menu") { client.navigate("/native/menu") }
            content
            if showDebugToolbar {
                DebugToolbar(client: client)
            }
        }
        .padding()
        .onAppear { client.connect() }
    }

    @ViewBuilder
    private var content: some View {
        switch client.status {
        case .disconnected:
            Text("\u{26A0} No server connection \u{2014} is it running on :4040?")
        default:
            if client.root == nil {
                Text("Connecting\u{2026}")
            } else {
                ArizonaView(client, registry: registry)
            }
        }
    }
}

/// A `-uitest`-only control panel for the e2e cases XCUITest can't reach by pure
/// UI taps: the keyed-list view (no buttons -- driven via client.pushEvent) and a
/// forced socket drop (reconnect). Each button carries an accessibility id the
/// tests tap; the round-trip over the wire is still real. The payloads mirror the
/// Android ListE2ETest exactly.
private struct DebugToolbar: View {
    let client: ArizonaClient

    var body: some View {
        HStack(spacing: 6) {
            Button("add") { client.pushEvent("add", payload: .object(["id": .string("9"), "text": .string("Nine")])) }
                .accessibilityIdentifier("dbg-add")
            Button("remove") { client.pushEvent("remove", payload: .object(["id": .string("2")])) }
                .accessibilityIdentifier("dbg-remove")
            Button("move") { client.pushEvent("move", payload: .object(["id": .string("1"), "pos": .int(2)])) }
                .accessibilityIdentifier("dbg-move")
            Button("update") { client.pushEvent("update", payload: .object(["id": .string("9"), "text": .string("Updated")])) }
                .accessibilityIdentifier("dbg-update")
            Button("reset") {
                client.pushEvent("reset", payload: .object([
                    "items": .array([
                        .object(["id": .string("a"), "text": .string("A")]),
                        .object(["id": .string("b"), "text": .string("B")]),
                    ]),
                ]))
            }
            .accessibilityIdentifier("dbg-reset")
            Button("drop") { client.forceDrop() }
                .accessibilityIdentifier("dbg-drop")
        }
        .font(.caption)
    }
}

/// Logs every frame in both directions, so the console of a failed UI test says
/// whether a tap's event left the app and whether the server's reply came back --
/// a tap that changed nothing is otherwise silent on both counts.
///
/// The callbacks capture `self` strongly on purpose: AzClient releases its
/// transport on the first close, and a weak wrapper would then swallow a socket's
/// later reports, hiding the duplicate-close handling the reconnect e2e covers.
private final class LoggingTransport: WebSocketTransport {
    var onOpen: (() -> Void)?
    var onText: ((String) -> Void)?
    var onClose: ((Int) -> Void)?

    private let inner: WebSocketTransport

    init(_ inner: WebSocketTransport) {
        self.inner = inner
        inner.onOpen = {
            NSLog("%@", "[arizona-sample] open")
            self.onOpen?()
        }
        inner.onText = { text in
            NSLog("%@", "[arizona-sample] <- \(text)")
            self.onText?(text)
        }
        inner.onClose = { code in
            NSLog("%@", "[arizona-sample] close \(code)")
            self.onClose?(code)
        }
    }

    func connect() { inner.connect() }

    func send(_ text: String) {
        NSLog("%@", "[arizona-sample] -> \(text)")
        inner.send(text)
    }

    func close(code: Int) { inner.close(code: code) }

    func cancel() {
        NSLog("%@", "[arizona-sample] cancel")
        inner.cancel()
    }
}
