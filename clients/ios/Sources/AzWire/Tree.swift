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
///
/// Throws `WireError.malformed` (rather than trapping) on a payload that is not a
/// node: this runs inside the op applier's per-op `do`/`catch`, and a Swift trap
/// is not catchable -- one bad payload would take the app down instead of costing
/// one slot. Mirrors Kotlin's `getValue`, whose exception the same catch handles.
public func buildTree(_ json: JSONValue, view: String?) throws -> Node {
    guard case let .object(obj) = json else {
        throw WireError.malformed("buildTree expects an object: \(json)")
    }
    guard let type = obj["type"]?.stringValue else {
        throw WireError.malformed("node has no string `type`: \(json)")
    }
    let node = Node(type: type, az: obj["az"]?.stringValue)
    for (k, v) in obj where k != "type" && k != "az" && k != "children" {
        node.props[k] = v
    }
    node.viewId = view
    if let kids = obj["children"]?.arrayValue {
        for child in kids { try addChild(node, child, view) }
    }
    return node
}

/// Append a child, splicing each-expansion arrays into the parent. `#slot`
/// objects are kept as nodes; stream items (each-array entries) become keyed
/// child nodes. A child that is itself a view root (`az_view`) owns its subtree;
/// otherwise it stays in the parent's `view`. Throws for the same reason
/// `buildTree` does.
func addChild(_ parent: Node, _ child: JSONValue, _ view: String?) throws {
    switch child {
    case let .array(arr):
        for c in arr { try addChild(parent, c, view) }
    case let .object(obj):
        let childView = obj["az_view"]?.boolValue == true ? (obj["id"]?.stringValue ?? view) : view
        let node = try buildTree(child, view: childView)
        parent.children.append(.node(node))
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

/// The inverse of `indexByAz`: drop `node`'s subtree from an item-scoped registry
/// before the ops discard it. Identity-checked, like `unindexByViews`.
public func unindexByAz(_ node: Node, _ registry: inout [String: Node]) {
    if let az = node.az, registry[az] === node { registry[az] = nil }
    for child in node.children {
        if case let .node(n) = child { unindexByAz(n, &registry) }
    }
}

/// Index nodes per enclosing view (`viewId` -> `az` -> node), so a "ViewId:az" op
/// target resolves within the right view -- two instances of the same stateful
/// child share az values (from a shared fingerprint) but live in distinct views.
///
/// Every (re)built subtree goes through here, not just the one `OP_REPLACE`
/// renders: a node the DIFF creates (an `OP_TEXT` payload that is a nested
/// template, an inserted stream item) is otherwise unaddressable, and a nested
/// `az_view` in such a payload never gets its view id registered at all.
public func indexByViews(_ node: Node, _ views: inout [String: [String: Node]]) {
    if let v = node.viewId, let az = node.az {
        views[v, default: [:]][az] = node
    }
    for child in node.children {
        if case let .node(n) = child { indexByViews(n, &views) }
    }
}

/// The inverse of `indexByViews`: drop `node`'s subtree from the per-view registry
/// before the ops discard it, so a rebuilt slot leaves no entry pointing at a
/// detached node. Identity-checked -- stream items share az values (one
/// fingerprint, many items), so a destroyed item must never delete an entry that
/// now names a surviving one.
public func unindexByViews(_ node: Node, _ views: inout [String: [String: Node]]) {
    if let v = node.viewId, let az = node.az, views[v]?[az] === node {
        views[v]?[az] = nil
        if views[v]?.isEmpty == true { views[v] = nil }
    }
    for child in node.children {
        if case let .node(n) = child { unindexByViews(n, &views) }
    }
}
