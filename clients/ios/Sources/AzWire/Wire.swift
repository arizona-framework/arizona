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

/// A wire-shape failure the op applier recovers from by skipping ONE op. Swift's
/// counterpart of the exceptions the Kotlin and JS clients catch per op: a
/// `preconditionFailure` here would take the whole app down over a single slot.
public enum WireError: Error {
    case malformed(String)
}

/// Fingerprint-cache bound (mirrors `FP_CACHE_MAX` in assets/js/arizona-core.js).
/// A fingerprint hashes a template's statics, so every deploy mints a new key and
/// orphans the old one: unbounded, a long-lived client accumulates one generation
/// per deploy. Evicting is never wrong -- the cache is content-addressed, so the
/// server re-sends the statics for any key the client did not announce, and a miss
/// costs bytes only -- but it is only SAFE between connections: once a socket has
/// announced a key the server stops shipping that template's statics, so dropping
/// it mid-connection would leave a payload the client cannot resolve. The prune
/// therefore runs at announce time; the map is free to grow within a session.
public let fpCacheMax = 1000

/// Caches a fingerprint's statics so later frames can omit them (the server
/// sends `{f, d}` once `{f, s, d}` has been seen, deduplicated by fingerprint).
public final class FingerprintCache {
    private struct Entry {
        let statics: [String]
        let t: Int?
        // Monotonic use stamp: ranks the announce-time prune (Swift dictionaries
        // are unordered, so the stamp is what Android gets from an access-ordered
        // LinkedHashMap and JS from Map insertion order).
        var used: UInt64
    }

    private var byFp: [String: Entry] = [:]
    private var clock: UInt64 = 0

    public init() {}

    /// Statics for `payload`, caching them if present, else from a prior frame.
    public func statics(_ payload: [String: JSONValue]) throws -> [String] {
        guard let f = payload["f"]?.stringValue else {
            throw WireError.malformed("payload has no fingerprint: \(payload)")
        }
        clock += 1
        if let s = payload["s"]?.arrayValue {
            byFp[f] = Entry(
                statics: s.map { $0.stringValue ?? "" }, t: payload["t"]?.intValue, used: clock)
        } else {
            byFp[f]?.used = clock
        }
        guard let entry = byFp[f] else { throw WireError.malformed("uncached fingerprint: \(f)") }
        return entry.statics
    }

    /// The cached fingerprints, most-recently-used LAST, after pruning down to
    /// `limit`. Announced on every socket open so the server can omit statics this
    /// client already holds.
    public func announce(limit: Int = fpCacheMax) -> [String] {
        let ranked = byFp.sorted { $0.value.used < $1.value.used }.map { $0.key }
        guard ranked.count > limit else { return ranked }
        for key in ranked.prefix(ranked.count - limit) { byFp[key] = nil }
        return Array(ranked.suffix(limit))
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
    ///
    /// Throws (rather than trapping) on any payload it cannot resolve -- an
    /// uncached fingerprint, a statics/dynamics length mismatch, a non-object --
    /// so the op applier can skip that one op instead of the app dying. See
    /// `WireError`.
    public func interleave(_ payload: JSONValue) throws -> JSONValue {
        try JSONValue.parseChecked(interleaveString(payload))
    }

    /// Decode an op payload to its JSON value: a `{t:0}` each-list -> a JSON
    /// array, a `{f, s, d}` template -> an object, a scalar -> itself. Used to
    /// materialize OP_INSERT items and OP_TEXT/OP_UPDATE content. (Returns a
    /// value directly rather than Android's JSON string, sidestepping top-level
    /// scalar-fragment parsing; behavior is identical.)
    public func decode(_ payload: JSONValue) throws -> JSONValue {
        if case .object = payload { return try JSONValue.parseChecked(encodeValue(payload)) }
        return payload
    }

    private func interleaveString(_ payload: JSONValue) throws -> String {
        guard case let .object(obj) = payload else {
            throw WireError.malformed("interleave expects an object: \(payload)")
        }
        return try interleaveWith(cache.statics(obj), payload["d"]?.arrayValue ?? [])
    }

    private func interleaveWith(_ statics: [String], _ dynamics: [JSONValue]) throws -> String {
        guard statics.count == dynamics.count + 1 else {
            throw WireError.malformed("\(statics.count) statics for \(dynamics.count) dynamics")
        }
        var out = statics[0]
        for (i, v) in dynamics.enumerated() {
            out += try encodeValue(v)
            out += statics[i + 1]
        }
        return out
    }

    private func encodeValue(_ v: JSONValue) throws -> String {
        guard case let .object(obj) = v else { return v.serialized } // a scalar
        if obj["t"]?.intValue == 0 {
            // An ?each expansion -> a JSON array of items the parent splices in.
            let s = try cache.statics(obj)
            var items: [String] = []
            for itemD in obj["d"]?.arrayValue ?? [] {
                items.append(try interleaveWith(s, itemD.arrayValue ?? []))
            }
            return "[" + items.joined(separator: ",") + "]"
        }
        return try interleaveString(v) // a nested {f, s, d} template
    }
}
