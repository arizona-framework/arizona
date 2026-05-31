import Foundation

/// A dynamic JSON value: decoded from arbitrary wire JSON and re-serialized to
/// canonical JSON. The Swift counterpart of kotlinx `JsonElement` (Android) and
/// raw `JSON` values (the browser/reference client): the `?native` wire carries
/// open-ended props, so the tree and op payloads are modeled structurally rather
/// than with fixed Codable structs.
public enum JSONValue: Equatable, Sendable {
    case null
    case bool(Bool)
    case int(Int)
    case double(Double)
    case string(String)
    case array([JSONValue])
    case object([String: JSONValue])
}

extension JSONValue: Decodable {
    public init(from decoder: Decoder) throws {
        let c = try decoder.singleValueContainer()
        if c.decodeNil() {
            self = .null
        } else if let b = try? c.decode(Bool.self) {
            self = .bool(b)
        } else if let i = try? c.decode(Int.self) {
            self = .int(i)
        } else if let d = try? c.decode(Double.self) {
            self = .double(d)
        } else if let s = try? c.decode(String.self) {
            self = .string(s)
        } else if let a = try? c.decode([JSONValue].self) {
            self = .array(a)
        } else if let o = try? c.decode([String: JSONValue].self) {
            self = .object(o)
        } else {
            throw DecodingError.dataCorruptedError(in: c, debugDescription: "unsupported JSON value")
        }
    }
}

extension JSONValue {
    /// Parse a JSON string (an object or array at top level) into a value.
    public static func parse(_ json: String) -> JSONValue {
        do {
            return try JSONDecoder().decode(JSONValue.self, from: Data(json.utf8))
        } catch {
            preconditionFailure("invalid JSON: \(error) -- \(json)")
        }
    }

    public var stringValue: String? {
        if case let .string(s) = self { return s }
        return nil
    }

    public var intValue: Int? {
        switch self {
        case let .int(i): return i
        case let .double(d): return Int(d)
        default: return nil
        }
    }

    public var boolValue: Bool? {
        if case let .bool(b) = self { return b }
        return nil
    }

    public var arrayValue: [JSONValue]? {
        if case let .array(a) = self { return a }
        return nil
    }

    public var objectValue: [String: JSONValue]? {
        if case let .object(o) = self { return o }
        return nil
    }

    /// Object-key lookup; nil for non-objects or absent keys.
    public subscript(_ key: String) -> JSONValue? {
        if case let .object(o) = self { return o[key] }
        return nil
    }

    /// Array-index access; traps on non-arrays (matches the reference clients'
    /// positional op access like `op[1]`).
    public subscript(_ index: Int) -> JSONValue {
        guard case let .array(a) = self else { preconditionFailure("not an array: \(self)") }
        return a[index]
    }

    /// The unquoted scalar content, for a text child (mirrors kotlinx
    /// `JsonPrimitive.content`).
    public var contentString: String {
        switch self {
        case let .string(s): return s
        case let .int(i): return String(i)
        case let .double(d): return Self.formatDouble(d)
        case let .bool(b): return b ? "true" : "false"
        case .null: return "null"
        case .array, .object: return serialized
        }
    }

    /// Canonical JSON form, JSON-encoding each value (string -> quoted,
    /// number/bool -> bare): mirrors `JSON.stringify` / kotlinx
    /// `JsonPrimitive.toString`.
    public var serialized: String {
        switch self {
        case .null:
            return "null"
        case let .bool(b):
            return b ? "true" : "false"
        case let .int(i):
            return String(i)
        case let .double(d):
            return Self.formatDouble(d)
        case let .string(s):
            return Self.quote(s)
        case let .array(a):
            return "[" + a.map { $0.serialized }.joined(separator: ",") + "]"
        case let .object(o):
            return "{" + o.map { Self.quote($0.key) + ":" + $0.value.serialized }.joined(separator: ",") + "}"
        }
    }

    static func formatDouble(_ d: Double) -> String {
        if d.isFinite, abs(d) < 9e15, d == d.rounded() { return String(Int(d)) }
        return String(d)
    }

    static func quote(_ s: String) -> String {
        var out = "\""
        for ch in s.unicodeScalars {
            switch ch {
            case "\"": out += "\\\""
            case "\\": out += "\\\\"
            case "\n": out += "\\n"
            case "\r": out += "\\r"
            case "\t": out += "\\t"
            case "\u{08}": out += "\\b"
            case "\u{0C}": out += "\\f"
            default:
                if ch.value < 0x20 {
                    out += String(format: "\\u%04x", ch.value)
                } else {
                    out.unicodeScalars.append(ch)
                }
            }
        }
        return out + "\""
    }
}
