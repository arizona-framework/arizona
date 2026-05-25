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
        _client = StateObject(wrappedValue: ArizonaClient(baseUrl: serverBaseURL, path: path))
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
