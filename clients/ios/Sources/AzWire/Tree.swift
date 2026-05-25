import Foundation

/// A child of a node: a nested element (possibly a transparent `#slot`) or text.
public enum Child: Equatable {
    case node(Node)
    case text(String)
}

/// A widget node in the rendered tree. `props` and `children` are mutated in
/// place by the op applier; the UI layer observes the client and re-renders.
///
/// `#slot` nodes are kept (they carry the `az` that `OP_TEXT` targets); the
/// renderer treats them transparently. Mirrors Android's `Node`.
public final class Node {
    public let type: String
    public let az: String?
    public var props: [String: JSONValue] = [:]
    public var children: [Child] = []

    /// The enclosing view's id (nearest `az_view` ancestor, or the root), so a
    /// tap routes its event to the owning view -- the root or a stateful child.
    public var viewId: String?

    public static let slot = "#slot"

    public init(type: String, az: String?) {
        self.type = type
        self.az = az
    }
}

extension Node: Equatable {
    // Identity equality: the op applier locates nodes by reference (OP_REMOVE_NODE).
    public static func == (lhs: Node, rhs: Node) -> Bool { lhs === rhs }
}

/// Parse an interleaved JSON tree into a `Node` tree, stamping each node with its
/// enclosing view (`view`). The root view is the live process id (the OP_REPLACE
/// ViewId) -- after a navigate that differs from the rendered root's `id`, so the
/// root uses the passed `view`, not its own `id`; a nested `az_view` child
/// switches to its own id (see `addChild`).
public func buildTree(_ json: JSONValue, view: String?) -> Node {
    guard case let .object(obj) = json else {
        preconditionFailure("buildTree expects an object: \(json)")
    }
    let node = Node(type: obj["type"]!.stringValue!, az: obj["az"]?.stringValue)
    for (k, v) in obj where k != "type" && k != "az" && k != "children" {
        node.props[k] = v
    }
    node.viewId = view
    if let kids = obj["children"]?.arrayValue {
        for child in kids { addChild(node, child, view) }
    }
    return node
}

/// Append a child, splicing each-expansion arrays into the parent. `#slot`
/// objects are kept as nodes; stream items (each-array entries) become keyed
/// child nodes. A child that is itself a view root (`az_view`) owns its subtree;
/// otherwise it stays in the parent's `view`.
func addChild(_ parent: Node, _ child: JSONValue, _ view: String?) {
    switch child {
    case let .array(arr):
        for c in arr { addChild(parent, c, view) }
    case let .object(obj):
        let childView = obj["az_view"]?.boolValue == true ? (obj["id"]?.stringValue ?? view) : view
        parent.children.append(.node(buildTree(child, view: childView)))
    default:
        parent.children.append(.text(child.contentString))
    }
}

/// Index every node carrying an `az` (incl. `#slot`s) so ops can target it.
public func indexByAz(_ node: Node, _ registry: inout [String: Node]) {
    if let az = node.az { registry[az] = node }
    for child in node.children {
        if case let .node(n) = child { indexByAz(n, &registry) }
    }
}

/// Index nodes per enclosing view (`viewId` -> `az` -> node), so a "ViewId:az" op
/// target resolves within the right view -- two instances of the same stateful
/// child share az values (from a shared fingerprint) but live in distinct views.
public func indexByViews(_ node: Node, _ views: inout [String: [String: Node]]) {
    if let v = node.viewId, let az = node.az {
        views[v, default: [:]][az] = node
    }
    for child in node.children {
        if case let .node(n) = child { indexByViews(n, &views) }
    }
}
