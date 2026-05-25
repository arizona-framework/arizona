import Foundation

/// The WebSocket seam the client depends on. The real implementation
/// (`URLSessionWebSocketTransport`) is Apple-only; tests inject a mock. Mirrors
/// the OkHttp `WebSocket` / `WebSocketListener` split in the Android client.
public protocol WebSocketTransport: AnyObject {
    var onOpen: (() -> Void)? { get set }
    var onText: ((String) -> Void)? { get set }
    /// Fired with a close code on a graceful close or an abrupt drop (1006).
    var onClose: ((Int) -> Void)? { get set }

    func connect()
    func send(_ text: String)
    func close(code: Int)
    /// Abruptly drop the socket (a network failure / a missed heartbeat).
    func cancel()
}

#if canImport(Darwin)
/// The production transport over `URLSessionWebSocketTask` (Apple platforms only;
/// the Linux Foundation implementation is unreliable, and the logic tests inject
/// a mock instead).
public final class URLSessionWebSocketTransport: NSObject, WebSocketTransport {
    public var onOpen: (() -> Void)?
    public var onText: ((String) -> Void)?
    public var onClose: ((Int) -> Void)?

    private let url: URL
    private var task: URLSessionWebSocketTask?
    private lazy var session = URLSession(configuration: .default, delegate: self, delegateQueue: nil)

    public init(url: URL) {
        self.url = url
        super.init()
    }

    public func connect() {
        let task = session.webSocketTask(with: url)
        self.task = task
        task.resume()
        receive()
    }

    public func send(_ text: String) {
        task?.send(.string(text)) { _ in }
    }

    public func close(code: Int) {
        let closeCode = URLSessionWebSocketTask.CloseCode(rawValue: code) ?? .normalClosure
        task?.cancel(with: closeCode, reason: nil)
        task = nil
    }

    public func cancel() {
        task?.cancel(with: .abnormalClosure, reason: nil)
        task = nil
    }

    private func receive() {
        task?.receive { [weak self] result in
            guard let self else { return }
            switch result {
            case let .success(message):
                if case let .string(text) = message { self.onText?(text) }
                self.receive() // keep listening
            case .failure:
                self.onClose?(1006) // abrupt drop
            }
        }
    }
}

extension URLSessionWebSocketTransport: URLSessionWebSocketDelegate {
    public func urlSession(
        _ session: URLSession,
        webSocketTask: URLSessionWebSocketTask,
        didOpenWithProtocol proto: String?
    ) {
        onOpen?()
    }

    public func urlSession(
        _ session: URLSession,
        webSocketTask: URLSessionWebSocketTask,
        didCloseWith closeCode: URLSessionWebSocketTask.CloseCode,
        reason: Data?
    ) {
        onClose?(closeCode.rawValue)
    }
}
#endif
