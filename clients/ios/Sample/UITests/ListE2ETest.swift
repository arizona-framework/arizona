import XCTest

/// On-device keyed-list e2e -- the analogue of the Android ListE2ETest. The list
/// view has no buttons, so the stream ops are driven via the `-uitest` debug
/// toolbar (each button calls client.pushEvent with the same payloads the Android
/// test sends), exercising OP_INSERT / OP_REMOVE / OP_MOVE / OP_ITEM_PATCH /
/// OP_UPDATE over a real socket. XCUITest is out-of-process and can't call the
/// client directly, so the toolbar is the stand-in.
final class ListE2ETest: XCTestCase {
    private var app: XCUIApplication!

    override func setUpWithError() throws {
        continueAfterFailure = false
        app = launchSample()
    }

    func testAppliesStreamOpsOverTheWire() {
        tapButton(app, "List")
        waitForText(app, "One")
        waitForText(app, "Two")
        waitForText(app, "Three")

        // OP_INSERT (append) -- add {id: 9, text: Nine}.
        tapButton(app, "dbg-add")
        waitForText(app, "Nine")

        // OP_REMOVE -- remove {id: 2}.
        tapButton(app, "dbg-remove")
        waitForNoText(app, "Two")

        // OP_MOVE -- numeric pos, move {id: 1} to index 2 (item stays present).
        tapButton(app, "dbg-move")
        waitForText(app, "One")

        // OP_ITEM_PATCH -- update item 9's text in place.
        tapButton(app, "dbg-update")
        waitForText(app, "Updated")
        waitForNoText(app, "Nine")

        // OP_UPDATE (full re-render via reset) -- an array-of-objects payload.
        tapButton(app, "dbg-reset")
        waitForText(app, "A")
        waitForText(app, "B")
        waitForNoText(app, "Three")
    }
}
