import XCTest

/// On-device OP_REMOVE_NODE (the `remove` sentinel). Navigate to
/// /native/removable and tap "Hide": the banner's dynamic returns `remove`, the
/// server diffs to op 4, and the client drops the node rather than crash. The
/// Android analogue is RemovableE2ETest.
final class RemovableE2ETest: XCTestCase {
    private var app: XCUIApplication!

    override func setUpWithError() throws {
        continueAfterFailure = false
        app = launchSample()
    }

    func testDropsNodeOnTheRemoveSentinel() {
        tapButton(app, "Removable")
        waitForText(app, "Banner!")

        tapButton(app, "Hide")
        waitForNoText(app, "Banner!")
    }
}
