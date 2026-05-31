#if canImport(SwiftUI)
import SwiftUI
import AzWire

/// A widget: maps a node to a SwiftUI view. The `RenderContext` gives access to
/// child rendering, text flattening, and event dispatch. The framework is
/// vocabulary-agnostic -- the host app registers whatever widget types its
/// `?native` views use. Mirrors Android's `Widget` typealias.
public typealias Widget = (RenderContext, Node) -> AnyView

/// Maps a widget `type` -> view builder. Populated by the host app.
public final class WidgetRegistry {
    private var widgets: [String: Widget] = [:]

    public init() {}

    @discardableResult
    public func register(_ type: String, _ widget: @escaping Widget) -> WidgetRegistry {
        widgets[type] = widget
        return self
    }

    func get(_ type: String) -> Widget? { widgets[type] }
}

/// Render context handed to every widget. Provides child rendering (splicing
/// transparent `#slot`s), flattened text, and tap dispatch. Mirrors Android's
/// `RenderContext`.
public struct RenderContext {
    let registry: WidgetRegistry
    public let client: AzClient

    /// Render a node: a `#slot` is transparent (its children splice in);
    /// otherwise dispatch to the registered widget.
    public func render(_ node: Node) -> AnyView {
        if node.type == Node.slot {
            return children(node)
        }
        guard let widget = registry.get(node.type) else {
            preconditionFailure("no widget registered for '\(node.type)'")
        }
        return widget(self, node)
    }

    /// Render a node's child elements (recursing through `#slot`s); text is
    /// ignored here.
    public func children(_ node: Node) -> AnyView {
        let nodes: [Node] = node.children.compactMap {
            if case let .node(n) = $0 { return n }
            return nil
        }
        return AnyView(ForEach(Array(nodes.enumerated()), id: \.offset) { _, child in
            self.render(child)
        })
    }

    /// Concatenate a node's text descendants, flattening through `#slot`s.
    public func text(_ node: Node) -> String {
        var out = ""
        collectText(node, into: &out)
        return out
    }

    private func collectText(_ node: Node, into out: inout String) {
        for child in node.children {
            switch child {
            case let .text(s): out += s
            case let .node(n): collectText(n, into: &out)
            }
        }
    }

    /// Fire a node's tap command back to the server.
    public func tap(_ node: Node, prop: String = "on_tap") {
        client.tap(node, prop: prop)
    }
}

/// A SwiftUI-observable wrapper over `AzClient`: bridges AzWire's UI-agnostic
/// `onChange` callback to SwiftUI by republishing on each applied frame / status
/// change, so views observing it (the tree *and* a connection placeholder)
/// re-render. The Compose-SnapshotState analogue.
public final class ArizonaClient: ObservableObject {
    public let raw: AzClient

    public var root: Node? { raw.root }
    public var status: ConnStatus { raw.status }

    public init(baseUrl: String, path: String) {
        raw = AzClient(baseUrl: baseUrl, path: path)
        raw.onChange = { [weak self] in self?.objectWillChange.send() }
    }

    /// Wrap an existing client (e.g. one built with an injected transport).
    public init(_ client: AzClient) {
        raw = client
        raw.onChange = { [weak self] in self?.objectWillChange.send() }
    }

    public func connect() { raw.connect() }
    public func close() { raw.close() }
    public func navigate(_ path: String) { raw.navigate(path) }
    public func pushEvent(_ event: String, payload: JSONValue = .object([:])) { raw.pushEvent(event, payload: payload) }
    public func tap(_ node: Node, prop: String = "on_tap") { raw.tap(node, prop: prop) }
    public func forceDrop() { raw.forceDrop() }
}

/// Render the client's current root tree, if any. Observes the client so applied
/// frames re-render. Mirrors Android's `ArizonaView`.
public struct ArizonaView: View {
    @ObservedObject private var client: ArizonaClient
    private let registry: WidgetRegistry

    public init(_ client: ArizonaClient, registry: WidgetRegistry) {
        _client = ObservedObject(wrappedValue: client)
        self.registry = registry
    }

    public var body: some View {
        let context = RenderContext(registry: registry, client: client.raw)
        if let root = client.root {
            context.render(root)
        }
    }
}
#endif
