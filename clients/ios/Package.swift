// swift-tools-version:5.9
import PackageDescription

// The Arizona native iOS client. `AzWire` is the pure wire-protocol logic
// (Foundation only) -- unit-tested on any platform with `swift test`, no Mac
// required. `AzClientUI` (added with the SwiftUI renderer) layers SwiftUI on top
// and only builds where SwiftUI exists. Mirrors the Android client's
// :arizona library / JVM-test split.
let package = Package(
    name: "Arizona",
    platforms: [.iOS(.v16), .macOS(.v13)],
    products: [
        .library(name: "AzWire", targets: ["AzWire"]),
        .library(name: "AzClientUI", targets: ["AzClientUI"]),
    ],
    targets: [
        .target(name: "AzWire"),
        .target(name: "AzClientUI", dependencies: ["AzWire"]),
        .testTarget(name: "AzWireTests", dependencies: ["AzWire"]),
    ]
)
