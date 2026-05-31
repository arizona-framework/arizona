import Foundation

/// Diff op codes (mirror src/arizona.hrl).
public enum Op {
    public static let text = 0
    public static let setAttr = 1
    public static let remAttr = 2
    public static let update = 3
    public static let removeNode = 4
    public static let insert = 5
    public static let remove = 6
    public static let itemPatch = 7
    public static let replace = 8
    public static let move = 9
}

/// Effect command op codes (mirror include/arizona_effect.hrl). Only the two
/// portable effects are meaningful to a native client; the rest are web-only.
public enum Effect {
    public static let pushEvent = 0
    public static let navigate = 10
}

/// Caches a fingerprint's statics so later frames can omit them (the server
/// sends `{f, d}` once `{f, s, d}` has been seen, deduplicated by fingerprint).
public final class FingerprintCache {
    private struct Entry {
        let statics: [String]
        let t: Int?
    }

    private var byFp: [String: Entry] = [:]

    public init() {}

    /// Statics for `payload`, caching them if present, else from a prior frame.
    public func statics(_ payload: [String: JSONValue]) -> [String] {
        guard let f = payload["f"]?.stringValue else {
            preconditionFailure("payload has no fingerprint: \(payload)")
        }
        if let s = payload["s"]?.arrayValue {
            byFp[f] = Entry(statics: s.map { $0.stringValue ?? "" }, t: payload["t"]?.intValue)
        }
        guard let entry = byFp[f] else { preconditionFailure("uncached fingerprint: \(f)") }
        return entry.statics
    }
}

/// Stitches a `{f, s, d}` payload into a JSON value, JSON-encoding each dynamic
/// (string -> quoted, number -> as-is) -- the native counterpart of the browser's
/// string-concatenating zip. Recurses into nested `{f, s, d}` payloads (child
/// components) and `{t: 0, ...}` item-lists. Mirrors Android's `Interleaver`.
public final class Interleaver {
    private let cache: FingerprintCache

    public init(_ cache: FingerprintCache) {
        self.cache = cache
    }

    /// Interleave a `{f, s, d}` payload into its rendered JSON value (an object).
    public func interleave(_ payload: JSONValue) -> JSONValue {
        JSONValue.parse(interleaveString(payload))
    }

    /// Decode an op payload to its JSON value: a `{t:0}` each-list -> a JSON
    /// array, a `{f, s, d}` template -> an object, a scalar -> itself. Used to
    /// materialize OP_INSERT items and OP_TEXT/OP_UPDATE content. (Returns a
    /// value directly rather than Android's JSON string, sidestepping top-level
    /// scalar-fragment parsing; behavior is identical.)
    public func decode(_ payload: JSONValue) -> JSONValue {
        if case .object = payload { return JSONValue.parse(encodeValue(payload)) }
        return payload
    }

    private func interleaveString(_ payload: JSONValue) -> String {
        guard case let .object(obj) = payload else {
            preconditionFailure("interleave expects an object: \(payload)")
        }
        return interleaveWith(cache.statics(obj), payload["d"]?.arrayValue ?? [])
    }

    private func interleaveWith(_ statics: [String], _ dynamics: [JSONValue]) -> String {
        var out = statics[0]
        for (i, v) in dynamics.enumerated() {
            out += encodeValue(v) + statics[i + 1]
        }
        return out
    }

    private func encodeValue(_ v: JSONValue) -> String {
        guard case let .object(obj) = v else { return v.serialized } // a scalar
        if obj["t"]?.intValue == 0 {
            // An ?each expansion -> a JSON array of items the parent splices in.
            let s = cache.statics(obj)
            let items = (obj["d"]?.arrayValue ?? []).map { interleaveWith(s, $0.arrayValue ?? []) }
            return "[" + items.joined(separator: ",") + "]"
        }
        return interleaveString(v) // a nested {f, s, d} template
    }
}
