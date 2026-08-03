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

/// Diagnostics sink. AzWire has no logging dependency (it must build on any
/// platform), so a skipped op is reported on stdout -- the counterpart of
/// Android's `Log.w` and the reference client's `console.warn`.
func azWarn(_ message: String) {
    print("[arizona] \(message)")
}

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
    // (which share a fingerprint's az values) don't collide. Internal (not
    // private) so the logic tests can assert on it.
    var views: [String: [String: Node]] = [:]
    private var viewId: String?

    private let makeTransport: (URL) -> WebSocketTransport
    private let runOnMain: (@escaping () -> Void) -> Void
    private let runAfter: (Int, DispatchWorkItem) -> Void
    private var transport: WebSocketTransport?

    // Bumped on every connect and on every accepted close, and captured by value
    // in each socket's callbacks. A callback whose generation is stale belongs to
    // a socket we have already retired, so it is dropped -- see `handleClose`.
    private var generation = 0

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
    ///   - runAfter: schedules the backoff-delayed reconnect. Tests capture the
    ///     work item and run it by hand, so the reconnect path is exercised
    ///     without waiting out a real backoff.
    public init(
        baseUrl: String,
        path: String,
        makeTransport: ((URL) -> WebSocketTransport)? = nil,
        runOnMain: @escaping (@escaping () -> Void) -> Void = { DispatchQueue.main.async(execute: $0) },
        runAfter: @escaping (Int, DispatchWorkItem) -> Void = { ms, item in
            DispatchQueue.main.asyncAfter(deadline: .now() + .milliseconds(ms), execute: item)
        }
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
        self.runAfter = runAfter
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
        stopHeartbeat()
        reconnectItem?.cancel()
        reconnectItem = nil
        // Retire the previous socket *before* closing it, so its close
        // notification lands on a stale generation and does not reconnect. It
        // still has to be closed: an orphaned socket keeps its live process on
        // the server and keeps pushing frames the UI would apply.
        generation += 1
        transport?.cancel()
        let gen = generation
        status = .connecting
        guard let url = URL(string: wsUrl) else { preconditionFailure("bad ws url: \(wsUrl)") }
        let transport = makeTransport(url)
        self.transport = transport
        transport.onOpen = { [weak self] in
            self?.runOnMain { self?.handleOpen(gen) }
        }
        transport.onText = { [weak self] text in
            self?.runOnMain { self?.handleFrame(text, gen) }
        }
        transport.onClose = { [weak self] code in
            self?.runOnMain { self?.handleClose(code, gen) }
        }
        transport.connect()
    }

    public func close() {
        closing = true
        stopHeartbeat()
        reconnectItem?.cancel()
        reconnectItem = nil
        transport?.close(code: 1000)
    }

    /// Abruptly drop the socket (like a network failure) to exercise reconnect.
    public func forceDrop() {
        transport?.cancel()
    }

    // MARK: - Socket callbacks (generation-guarded)

    private func handleOpen(_ gen: Int) {
        guard gen == generation, let transport else { return }
        transport.send(cachedFpsFrame())
        startHeartbeat()
    }

    /// The `["cached_fps", [...]]` announcement: the fingerprints this client holds
    /// statics for, so the server can omit them from what it renders next (a
    /// reconnect mounts a fresh live process with an empty sent set, so without the
    /// announcement every reconnect re-ships every template's statics). Native
    /// deliberately does NOT pass `_az_fps_follow`: the server keeps the immediate
    /// resync, and this seeds the process for the frames after it.
    func cachedFpsFrame() -> String {
        JSONValue.array([.string("cached_fps"), .array(cache.announce().map { .string($0) })])
            .serialized
    }

    private func handleFrame(_ text: String, _ gen: Int) {
        guard gen == generation else { return }
        handleText(text)
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
        let scope = viewScope()
        for op in ops { dispatch(op, scopeRoot: root, scope: scope) }
    }

    /// How one op batch addresses and re-indexes nodes: `resolve` maps an op
    /// target to a node (nil when unknown), and `unindexChildren`/`reindex` keep
    /// the registry in step with the tree whenever an op discards a node's children
    /// or grafts a new subtree on. Three exist -- the top-level one ("ViewId:az"
    /// over the per-view registry), the per-item one in `applyInner`, and the
    /// child-view one in `applyChildViewOps`.
    struct Scope {
        let resolve: (String) -> Node?
        let unindexChildren: (Node) -> Void
        let reindex: (Node) -> Void
    }

    private func viewScope() -> Scope {
        Scope(
            resolve: { [unowned self] target in self.resolve(target) },
            unindexChildren: { [unowned self] node in self.unindexChildrenInViews(node) },
            reindex: { [unowned self] node in self.reindexInViews(node) }
        )
    }

    private func unindexChildrenInViews(_ node: Node) {
        for child in node.children {
            if case let .node(n) = child { unindexByViews(n, &views) }
        }
    }

    private func reindexInViews(_ node: Node) {
        indexByViews(node, &views)
    }

    // Apply one op through `scope`. Top-level ops pass "ViewId:az"; an
    // OP_ITEM_PATCH's inner ops pass a bare az resolved within the patched item.
    // `scopeRoot` bounds OP_REMOVE_NODE's search.
    //
    // Each op is isolated: an unexpected wire shape (or a target this client
    // cannot resolve) must degrade one slot, not take the app down. Mirrors the
    // browser client's per-op try/catch in applyOps -- which is why the wire
    // helpers on this path throw `WireError` instead of calling
    // `preconditionFailure` (a trap is not catchable).
    func dispatch(_ op: JSONValue, scopeRoot: Node?, scope: Scope) {
        do {
            try dispatchOne(op.arrayValue ?? [], scopeRoot: scopeRoot, scope: scope)
        } catch {
            azWarn("op \(op.serialized) failed; skipping: \(error)")
        }
    }

    // Positional operand access that reports a short op as a recoverable wire
    // error instead of trapping on the array bound.
    private func operand(_ a: [JSONValue], _ i: Int) throws -> JSONValue {
        guard i < a.count else { throw WireError.malformed("op \(a) has no operand \(i)") }
        return a[i]
    }

    private func text(_ a: [JSONValue], _ i: Int) throws -> String {
        guard let s = try operand(a, i).stringValue else {
            throw WireError.malformed("op \(a) operand \(i) is not a string")
        }
        return s
    }

    private func dispatchOne(_ a: [JSONValue], scopeRoot: Node?, scope: Scope) throws {
        guard a.count >= 2 else { throw WireError.malformed("op too short: \(a)") }
        // A child view's ops ride in a `[ChildViewId, ChildOps]` wrapper that
        // `flatten_ops/2` unwraps only at TOP level -- so a ?stateful child inside
        // a stream item ships the wrapper as an OP_ITEM_PATCH INNER op, whose first
        // element is a view-id string rather than an op code.
        guard let code = a[0].intValue else {
            guard let childViewId = a[0].stringValue else {
                throw WireError.malformed("op head is neither an op code nor a view id: \(a[0])")
            }
            applyChildViewOps(childViewId, a[1].arrayValue ?? [], scopeRoot: scopeRoot)
            return
        }
        if code == Op.replace {
            // The live view id is the rendered root's `id` (== the server's
            // socket.view_id, what it prefixes pushed ops with), NOT a[1] -- after
            // a navigate a[1] is the OLD id (the replace target).
            let raw = try operand(a, 2)
            let json = try interleaver.interleave(raw)
            // Build BEFORE committing: a malformed payload must leave the previous
            // tree and registry intact, not half-cleared.
            let node = try buildTree(json, view: json["id"]?.stringValue)
            viewId = node.viewId
            views = [:]
            indexByViews(node, &views)
            root = node
            return
        }
        let target = try text(a, 1)
        guard let node = scope.resolve(target) else {
            // Loud like the browser client's missing-target warn: a silently
            // dropped op reads as "nothing happened" and costs a debugging trip.
            azWarn("op \(code) target \"\(target)\" not found; skipping")
            return
        }
        switch code {
        case Op.text, Op.update:
            // OP_TEXT is usually a scalar, but a nested-template dynamic (e.g. a
            // conditional subtree) ships a {f,s,d} payload; OP_UPDATE re-renders a
            // node's content wholesale (e.g. a stream reset). Either way the
            // children are REPLACED, so the registry has to follow: drop the
            // destroyed subtree's entries and index the new one, or every az the
            // payload introduced -- a nested child view's id included -- is
            // unaddressable.
            let raw = try operand(a, 2)
            let payload = try interleaver.decode(raw)
            scope.unindexChildren(node)
            node.children = []
            try addChild(node, payload, node.viewId)
            scope.reindex(node)
        case Op.removeNode:
            // A dynamic returned the `remove` sentinel: drop the node from its
            // parent. One-way -- bringing it back needs a parent re-render.
            //
            // The removed node's registry entries are deliberately left: a later op
            // naming that az would still RESOLVE and would patch the detached node.
            // That is unreachable only because the server never re-addresses a removed
            // az (and addresses stream items by `az_key`, not "ViewId:az") -- a
            // convention, not a check. Re-validating would mean walking to the root on
            // every resolve, since nodes carry no parent link; if that convention ever
            // changes, unindex here.
            removeFromParent(scopeRoot, node)
        case Op.setAttr:
            let name = try text(a, 2)
            node.props[name] = try operand(a, 3)
        case Op.remAttr:
            let name = try text(a, 2)
            node.props[name] = nil
        case Op.insert:
            let posValue = try operand(a, 3)
            guard let pos = posValue.intValue else {
                throw WireError.malformed("insert position is not an int: \(a)")
            }
            let raw = try operand(a, 4)
            let payload = try interleaver.decode(raw)
            let item = try buildTree(payload, view: node.viewId)
            if pos == -1 || pos >= node.children.count {
                node.children.append(.node(item))
            } else {
                node.children.insert(.node(item), at: pos)
            }
            // Index the new item, like OP_REPLACE already indexes the items it
            // renders: a ?stateful child inside a stream item owns its own view id,
            // and that view's ops arrive addressed to it.
            scope.reindex(item)
        case Op.remove:
            let key = try text(a, 2)
            if let i = indexOfKey(node, key) { node.children.remove(at: i) }
        case Op.move:
            let key = try text(a, 2)
            guard let i = indexOfKey(node, key) else { return }
            let item = node.children.remove(at: i)
            let afterKey = try operand(a, 3)
            if case .null = afterKey {
                node.children.insert(item, at: 0)
            } else if let after = afterKey.stringValue, let r = indexOfKey(node, after) {
                node.children.insert(item, at: r + 1)
            } else {
                node.children.append(item)
            }
        case Op.itemPatch:
            let key = try text(a, 2)
            let inner = try operand(a, 3).arrayValue ?? []
            if let i = indexOfKey(node, key), case let .node(item) = node.children[i] {
                applyInner(item, inner)
            }
        default:
            azWarn("unhandled op code \(code); skipping")
        }
    }

    // Apply an OP_ITEM_PATCH's inner ops, scoped to one keyed item: inner ops
    // carry bare az indices resolved within the item's own subtree.
    //
    // Addressing is item-local, but index MAINTENANCE is both: an inner op that
    // rebuilds a subtree can introduce a nested `az_view` -- a `?stateful` child a
    // conditional in the item template just switched on -- and that child's own ops
    // arrive as TOP-LEVEL "ChildViewId:az" targets. Keeping them out of the
    // per-view registry leaves the child unaddressable and its slot frozen.
    private func applyInner(_ item: Node, _ innerOps: [JSONValue]) {
        var local: [String: Node] = [:]
        indexByAz(item, &local)
        let scope = Scope(
            resolve: { az in local[az] ?? item },
            unindexChildren: { [unowned self] node in
                for child in node.children {
                    if case let .node(n) = child { unindexByAz(n, &local) }
                }
                self.unindexChildrenInViews(node)
            },
            reindex: { [unowned self] node in
                indexByAz(node, &local)
                self.reindexInViews(node)
            }
        )
        for op in innerOps { dispatch(op, scopeRoot: item, scope: scope) }
    }

    // Apply a child view's ops (the `[ChildViewId, ChildOps]` wrapper): their
    // targets are bare `az`s resolved inside that child's own registry, exactly as
    // the top-level flattened "ChildViewId:az" form resolves. `scopeRoot` stays the
    // enclosing item so OP_REMOVE_NODE's parent search still finds the node.
    private func applyChildViewOps(_ childViewId: String, _ childOps: [JSONValue], scopeRoot: Node?) {
        let scope = Scope(
            resolve: { [unowned self] az in self.views[childViewId]?[az] },
            unindexChildren: { [unowned self] node in self.unindexChildrenInViews(node) },
            reindex: { [unowned self] node in self.reindexInViews(node) }
        )
        for op in childOps { dispatch(op, scopeRoot: scopeRoot, scope: scope) }
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
    // Nil when unknown (or unscoped) -- the caller warns and skips the op.
    private func resolve(_ target: String) -> Node? {
        guard let colon = target.firstIndex(of: ":") else { return nil }
        let viewId = String(target[..<colon])
        let az = String(target[target.index(after: colon)...])
        return views[viewId]?[az]
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
        case Effect.pushEvent:
            pushEvent(cmd[1].stringValue!, payload: cmd.count > 2 ? cmd[2] : .object([:]), target: target)
        case Effect.navigate: navigate(cmd[1].stringValue!)
        default: if strict { assertionFailure("unsupported command: \(cmd)") }
        }
    }

    // MARK: - Reconnect & heartbeat (main-thread timers; only armed once connected)

    // Flip to DISCONNECTED and, unless we closed on purpose or it was a normal
    // close (1000), reopen with backoff -- re-mounting via _az_reconnect=1.
    //
    // Exactly one close is honored per socket: accepting it bumps the generation,
    // so any further notification carrying `gen` is dropped. A transport is free
    // to report the same drop more than once -- `URLSessionWebSocketTask` reports
    // it through both the failed `receive` and the delegate's `didCloseWith`, and
    // the two can arrive seconds apart -- and the second report would otherwise
    // open a *second* socket. Two sockets means two live processes: the later
    // mount replaces the rendered tree from scratch, silently discarding the
    // state the user just produced on the first one. The same guard drops a late
    // close from a socket already superseded by `connect()`, which would
    // otherwise flip a healthy connection to DISCONNECTED.
    private func handleClose(_ code: Int, _ gen: Int) {
        guard gen == generation else { return }
        generation += 1
        transport = nil
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
        runAfter(delay, item)
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
