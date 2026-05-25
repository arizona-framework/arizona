import XCTest
@testable import AzWire

/// Pure tests of per-view stamping/indexing -- the part that decides which view
/// an op's "ViewId:az" target resolves in. Shared semantics with the Android
/// client's TreeTest. Run anywhere: swift test.
final class TreeTests: XCTestCase {
    // The live view id is the rendered root's own `id`; buildTree stamps it on the
    // root + its non-view descendants, and indexByViews registers them under it,
    // so "rootId:az" targets (incl. server pushes) resolve.
    func testRootSubtreeIndexedUnderTheRenderedRootId() {
        let json = JSONValue.parse(##"{"type":"Column","az":"F-0","az_view":true,"id":"native_ticker","children":[{"type":"#slot","az":"F-0t0","children":[]}]}"##)
        let root = buildTree(json, view: "native_ticker") // OP_REPLACE passes the root's id
        guard case let .node(slot) = root.children[0] else { return XCTFail("expected a slot child") }
        XCTAssertEqual(root.viewId, "native_ticker")
        XCTAssertEqual(slot.viewId, "native_ticker")

        var views: [String: [String: Node]] = [:]
        indexByViews(root, &views)
        XCTAssertTrue(views["native_ticker"]?["F-0t0"] === slot)
    }

    // A nested stateful child owns its subtree under its own id, while the root
    // stays its own id.
    func testNestedAzViewChildSwitchesView() {
        let json = JSONValue.parse(##"{"type":"Column","az":"P-0","az_view":true,"id":"native_nested","children":[{"type":"Column","az":"C-0","az_view":true,"id":"child_a","children":[{"type":"#slot","az":"C-0t0","children":[]}]}]}"##)
        let root = buildTree(json, view: "native_nested")
        guard case let .node(child) = root.children[0] else { return XCTFail("expected a child view") }
        guard case let .node(childSlot) = child.children[0] else { return XCTFail("expected a child slot") }
        XCTAssertEqual(root.viewId, "native_nested")
        XCTAssertEqual(child.viewId, "child_a")
        XCTAssertEqual(childSlot.viewId, "child_a")

        var views: [String: [String: Node]] = [:]
        indexByViews(root, &views)
        XCTAssertTrue(views["native_nested"]?["P-0"] === root)
        XCTAssertTrue(views["child_a"]?["C-0"] === child)
    }
}
