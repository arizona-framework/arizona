import Foundation
import Dispatch

/// Connection state, for a "connecting" / "no connection" UI placeholder.
public enum ConnStatus: Equatable, Sendable {
    case connecting
    case connected
    case disconnected
}

/// Reconnect backoff: step delays (ms) by attempt, capped at 10s, with +/-20%
/// jitter. Pure -> testable without a socket. Mirrors the reference client.
public func backoffDelayMs(_ attempt: Int) -> Int {
    let delays = [1000, 2000, 5000, 10000]
    let base = attempt < delays.count ? delays[attempt] : 10000
    return Int((Double(base) * (0.8 + Double.random(in: 0..<1) * 0.4)).rounded())
}

// Heartbeat (mirror assets/js/arizona-worker.js): ping "0" every 30s, pong "1";
// a pong still pending at the next tick means the socket is silently dead.
private let sysPing = "0"
private let sysPong = "1"
private let heartbeatMs = 30_000

/// Connects to an Arizona server's WebSocket and renders its `?native` view.
///
/// Native has no SSR page, so we connect with `_az_reconnect=1` to make the live
/// process emit its `mount_and_render` output as the first `OP_REPLACE`. Incoming
/// ops mutate the `root` node tree in place; `onChange` fires so a UI layer can
/// re-render. The apply path (`handleText`/`dispatch`) is synchronous and
/// UI-agnostic, so it is unit-testable on any platform. Mirrors
/// e2e/utils/native_client.js and Android's `AzClient`.
public final class AzClient {
    /// The current root widget node; nil until the first frame arrives.
    public private(set) var root: Node?

    /// Connection state, for a placeholder UI.
    public private(set) var status: ConnStatus = .connecting

    /// Invoked on the main thread after any state change (a frame applied or the
    /// status changed), so a UI layer can re-render. AzWire stays UI-agnostic.
    public var onChange: (() -> Void)?

    private let wsBase: String
    // The reconnect URL. Updated in navigate() to the navigated path, so a dropped
    // socket re-mounts where the user is, not the launch path.
    private var wsUrl: String

    private let cache = FingerprintCache()
    private lazy var interleaver = Interleaver(cache)

    // viewId -> (az -> node). Per-view so two instances of the same stateful child
    // (which share a fingerprint's az values) don't collide.
    private var views: [String: [String: Node]] = [:]
    private var viewId: String?

    private let makeTransport: (URL) -> WebSocketTransport
    private let runOnMain: (@escaping () -> Void) -> Void
    private var transport: WebSocketTransport?

    private var closing = false
    private var reconnectAttempt = 0
    private var heartbeatPending = false
    private var heartbeatItem: DispatchWorkItem?
    private var reconnectItem: DispatchWorkItem?

    /// - Parameters:
    ///   - makeTransport: factory for the socket (a fresh one per connect, like
    ///     the Android client). Defaults to `URLSessionWebSocketTransport` on
    ///     Apple platforms; tests inject a mock.
    ///   - runOnMain: marshals transport callbacks onto the main thread. Tests
    ///     pass a synchronous closure.
    public init(
        baseUrl: String,
        path: String,
        makeTransport: ((URL) -> WebSocketTransport)? = nil,
        runOnMain: @escaping (@escaping () -> Void) -> Void = { DispatchQueue.main.async(execute: $0) }
    ) {
        let scheme: String
        if let r = baseUrl.range(of: "http") {
            scheme = baseUrl.replacingCharacters(in: r, with: "ws")
        } else {
            scheme = baseUrl
        }
        self.wsBase = scheme + "/ws?_az_path="
        self.wsUrl = scheme + "/ws?_az_path=" + Self.encodeURIComponent(path) + "&_az_reconnect=1"
        self.runOnMain = runOnMain
        self.makeTransport = makeTransport ?? { url in
            #if canImport(Darwin)
            return URLSessionWebSocketTransport(url: url)
            #else
            preconditionFailure("no default transport on this platform; inject makeTransport")
            #endif
        }
    }

    public func connect() {
        closing = false
        status = .connecting
        guard let url = URL(string: wsUrl) else { preconditionFailure("bad ws url: \(wsUrl)") }
        let transport = makeTransport(url)
        self.transport = transport
        transport.onOpen = { [weak self] in
            self?.runOnMain {
                guard let self else { return }
                self.transport?.send("[\"cached_fps\",[]]")
                self.startHeartbeat()
            }
        }
        transport.onText = { [weak self] text in
            self?.runOnMain { self?.handleText(text) }
        }
        transport.onClose = { [weak self] code in
            self?.runOnMain { self?.scheduleReconnect(code) }
        }
        transport.connect()
    }

    public func close() {
        closing = true
        stopHeartbeat()
        reconnectItem?.cancel()
        transport?.close(code: 1000)
    }

    /// Abruptly drop the socket (like a network failure) to exercise reconnect.
    public func forceDrop() {
        transport?.cancel()
    }

    /// Apply one received text frame. Synchronous and UI-agnostic (the unit of
    /// behavior the logic tests drive directly).
    func handleText(_ text: String) {
        heartbeatPending = false // any frame proves the socket is live
        if text == sysPong { return }
        reconnectAttempt = 0 // a real frame -> reset backoff
        let msg = JSONValue.parse(text)
        if let ops = msg["o"]?.arrayValue { applyOps(ops) }
        status = .connected
        // Handler-returned effects: dispatch the portable ones, skip web-only.
        if let effects = msg["e"]?.arrayValue {
            for eff in effects { runEffect(eff.arrayValue ?? [], strict: false, target: nil) }
        }
        onChange?()
    }

    /// Fire the command read from a node's tap prop (e.g. `on_tap`), routing the
    /// event to the node's enclosing view (the root, or a stateful child).
    public func tap(_ node: Node, prop: String = "on_tap") {
        guard let cmd = node.props[prop]?.arrayValue else { return }
        runEffect(cmd, strict: true, target: node.viewId)
    }

    /// Send an event frame `[ViewId, Event, Payload]`. ViewId defaults to the
    /// root view; a tap routes to the tapped node's enclosing view.
    public func pushEvent(_ event: String, payload: JSONValue = .object([:]), target: String? = nil) {
        guard let vid = target ?? viewId else { return }
        transport?.send(JSONValue.array([.string(vid), .string(event), payload]).serialized)
    }

    /// SPA navigate: transition to a new view on the same socket (the server's
    /// handle_navigate re-mounts and replies with OP_REPLACE).
    public func navigate(_ path: String) {
        let parts = path.split(separator: "?", maxSplits: 1, omittingEmptySubsequences: false).map(String.init)
        let p = parts[0]
        let qs = parts.count > 1 ? parts[1] : ""
        transport?.send(JSONValue.array([.string("navigate"), .object(["path": .string(p), "qs": .string(qs)])]).serialized)
        // Keep the reconnect URL in sync with the navigated route, so a drop
        // re-mounts here, not the launch path. Reconnect is a fresh mount, so only
        // _az_path (the route) matters.
        wsUrl = wsBase + Self.encodeURIComponent(p) + "&_az_reconnect=1"
    }

    // MARK: - Op application

    private func applyOps(_ ops: [JSONValue]) {
        // Top-level ops address nodes as "ViewId:az" via the per-view registry;
        // OP_REMOVE_NODE searches the whole tree to splice the node out.
        for op in ops {
            dispatch(op.arrayValue ?? [], scopeRoot: root) { [unowned self] target in self.resolve(target) }
        }
    }

    // Apply one op, resolving its target via `resolve`. Top-level ops pass
    // "ViewId:az"; an OP_ITEM_PATCH's inner ops pass a bare az resolved within the
    // patched item. `scopeRoot` bounds OP_REMOVE_NODE's search.
    func dispatch(_ a: [JSONValue], scopeRoot: Node?, resolve: (String) -> Node) {
        guard let code = a[0].intValue else { preconditionFailure("op code not an int: \(a)") }
        switch code {
        case Op.replace:
            // The live view id is the rendered root's `id` (== the server's
            // socket.view_id, what it prefixes pushed ops with), NOT a[1] -- after
            // a navigate a[1] is the OLD id (the replace target).
            let json = interleaver.interleave(a[2])
            viewId = json["id"]?.stringValue
            views = [:]
            let node = buildTree(json, view: viewId)
            indexByViews(node, &views)
            root = node
        case Op.text, Op.update:
            // Usually a scalar, but a nested-template dynamic ships a {f,s,d}
            // payload; decode handles both.
            let node = resolve(a[1].stringValue!)
            node.children = []
            addChild(node, interleaver.decode(a[2]), node.viewId)
        case Op.removeNode:
            // A dynamic returned the `remove` sentinel: drop the node from its
            // parent. One-way -- bringing it back needs a parent re-render.
            removeFromParent(scopeRoot, resolve(a[1].stringValue!))
        case Op.setAttr:
            resolve(a[1].stringValue!).props[a[2].stringValue!] = a[3]
        case Op.remAttr:
            resolve(a[1].stringValue!).props[a[2].stringValue!] = nil
        case Op.insert:
            let container = resolve(a[1].stringValue!)
            let pos = a[3].intValue!
            let item = buildTree(interleaver.decode(a[4]), view: container.viewId)
            if pos == -1 || pos >= container.children.count {
                container.children.append(.node(item))
            } else {
                container.children.insert(.node(item), at: pos)
            }
            // Note: the inserted item's `az`s are NOT added to the per-view
            // registry. Safe only because stream items are diffed via
            // OP_ITEM_PATCH (resolved item-locally), never by a top-level op.
        case Op.remove:
            let container = resolve(a[1].stringValue!)
            if let i = indexOfKey(container, a[2].stringValue!) { container.children.remove(at: i) }
        case Op.move:
            let container = resolve(a[1].stringValue!)
            guard let i = indexOfKey(container, a[2].stringValue!) else { return }
            let item = container.children.remove(at: i)
            if case .null = a[3] {
                container.children.insert(item, at: 0)
            } else if let r = indexOfKey(container, a[3].stringValue!) {
                container.children.insert(item, at: r + 1)
            } else {
                container.children.append(item)
            }
        case Op.itemPatch:
            let container = resolve(a[1].stringValue!)
            if let i = indexOfKey(container, a[2].stringValue!), case let .node(item) = container.children[i] {
                applyInner(item, a[3].arrayValue ?? [])
            }
        default:
            preconditionFailure("unhandled op code: \(code)")
        }
    }

    // Apply an OP_ITEM_PATCH's inner ops, scoped to one keyed item: inner ops
    // carry bare az indices resolved within the item's own subtree.
    private func applyInner(_ item: Node, _ innerOps: [JSONValue]) {
        var local: [String: Node] = [:]
        indexByAz(item, &local)
        for op in innerOps {
            dispatch(op.arrayValue ?? [], scopeRoot: item) { az in local[az] ?? item }
        }
    }

    // Find `target` within `parent`'s subtree and remove it from its parent's
    // children. Used by OP_REMOVE_NODE.
    @discardableResult
    private func removeFromParent(_ parent: Node?, _ target: Node) -> Bool {
        guard let parent else { return false }
        if let i = parent.children.firstIndex(where: { if case let .node(n) = $0 { return n === target }; return false }) {
            parent.children.remove(at: i)
            return true
        }
        for child in parent.children {
            if case let .node(n) = child, removeFromParent(n, target) { return true }
        }
        return false
    }

    // Index of the keyed child (by its `az_key` prop) in a stream container, or nil.
    private func indexOfKey(_ container: Node, _ key: String) -> Int? {
        container.children.firstIndex {
            if case let .node(n) = $0 { return n.props["az_key"]?.stringValue == key }
            return false
        }
    }

    // Resolve a top-level op's "ViewId:az" target within that view's own registry.
    private func resolve(_ target: String) -> Node {
        guard let colon = target.firstIndex(of: ":") else { preconditionFailure("bad target: \(target)") }
        let viewId = String(target[..<colon])
        let az = String(target[target.index(after: colon)...])
        guard let node = views[viewId]?[az] else { preconditionFailure("unknown target: \(target)") }
        return node
    }

    // Run one effect command (a tap prop or a server "e" entry). `strict` traps on
    // an unsupported command (taps); non-strict skips it (web-only effects don't
    // apply to native). `target` (a tap's enclosing view id) routes the event.
    private func runEffect(_ cmd: [JSONValue], strict: Bool, target: String?) {
        guard let code = cmd.first?.intValue else {
            if strict { assertionFailure("bad command: \(cmd)") }
            return
        }
        switch code {
        case Effect.pushEvent: pushEvent(cmd[1].stringValue!, target: target)
        case Effect.navigate: navigate(cmd[1].stringValue!)
        default: if strict { assertionFailure("unsupported command: \(cmd)") }
        }
    }

    // MARK: - Reconnect & heartbeat (main-thread timers; only armed once connected)

    // Flip to DISCONNECTED and, unless we closed on purpose or it was a normal
    // close (1000), reopen with backoff -- re-mounting via _az_reconnect=1.
    private func scheduleReconnect(_ code: Int) {
        stopHeartbeat()
        status = .disconnected
        onChange?()
        if closing || code == 1000 { return }
        let delay = backoffDelayMs(reconnectAttempt)
        reconnectAttempt += 1
        let item = DispatchWorkItem { [weak self] in
            guard let self, !self.closing else { return }
            self.connect()
        }
        reconnectItem = item
        DispatchQueue.main.asyncAfter(deadline: .now() + .milliseconds(delay), execute: item)
    }

    private func startHeartbeat() {
        heartbeatItem?.cancel()
        heartbeatPending = false
        scheduleHeartbeat()
    }

    private func scheduleHeartbeat() {
        let item = DispatchWorkItem { [weak self] in self?.heartbeatTick() }
        heartbeatItem = item
        DispatchQueue.main.asyncAfter(deadline: .now() + .milliseconds(heartbeatMs), execute: item)
    }

    private func heartbeatTick() {
        guard let transport else { return }
        if heartbeatPending {
            transport.cancel() // missed pong -> drop -> reconnect via onClose
            return
        }
        heartbeatPending = true
        transport.send(sysPing)
        scheduleHeartbeat()
    }

    private func stopHeartbeat() {
        heartbeatItem?.cancel()
        heartbeatItem = nil
        heartbeatPending = false
    }

    // encodeURIComponent semantics (matches the JS reference client / Android's
    // URLEncoder): percent-encode everything but the unreserved set.
    private static func encodeURIComponent(_ s: String) -> String {
        let allowed = CharacterSet(
            charactersIn: "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_.!~*'()")
        return s.addingPercentEncoding(withAllowedCharacters: allowed) ?? s
    }
}
