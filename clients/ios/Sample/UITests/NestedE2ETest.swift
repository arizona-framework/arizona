import XCTest

/// On-device nested event routing -- the analogue of the Android NestedE2ETest.
/// Navigate to /native/nested (two stateful child counters); tapping a child's
/// button routes the event to THAT child's view id, so only its count changes.
/// Each child shows "<label><count>" (e.g. "A0") and a button labelled "<label>".
final class NestedE2ETest: XCTestCase {
    private var app: XCUIApplication!

    override func setUpWithError() throws {
        continueAfterFailure = false
        app = launchSample()
    }

    func testRoutesTapToTheTappedChild() {
        tapButton(app, "Nested")
        waitForText(app, "A0")
        waitForText(app, "B0")

        // Tap child A's button ("A") -> routes to child_a only.
        tapButton(app, "A")
        waitForText(app, "A1")
        waitForText(app, "B0") // B untouched

        // Tap child B's button ("B") -> routes to child_b; A stays at 1.
        tapButton(app, "B")
        waitForText(app, "B1")
        waitForText(app, "A1")
    }
}
