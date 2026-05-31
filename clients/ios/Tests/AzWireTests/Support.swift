import Foundation
@testable import AzWire

/// In-memory transport for the logic tests: records sent frames and lets a test
/// drive `onOpen`/`onText`/`onClose` synchronously.
final class MockTransport: WebSocketTransport {
    var onOpen: (() -> Void)?
    var onText: ((String) -> Void)?
    var onClose: ((Int) -> Void)?
    private(set) var sent: [String] = []

    func connect() {}
    func send(_ text: String) { sent.append(text) }
    func close(code: Int) { onClose?(code) }
    func cancel() { onClose?(1006) }

    func simulateOpen() { onOpen?() }
    func simulateText(_ text: String) { onText?(text) }
}

/// Concatenate a node's text descendants (through `#slot`s and elements).
func flatText(_ node: Node) -> String {
    var out = ""
    for child in node.children {
        switch child {
        case let .text(s): out += s
        case let .node(n): out += flatText(n)
        }
    }
    return out
}

/// A node's element (non-text) children.
func childNodes(_ node: Node) -> [Node] {
    node.children.compactMap { if case let .node(n) = $0 { return n }; return nil }
}
