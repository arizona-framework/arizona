import XCTest

/// On-device (Simulator) e2e -- the XCUITest analogue of the Android
/// CounterE2ETest. Launch the menu, navigate to /native/counter, and drive it:
/// tap "+", count goes up; tap "-", down. Each change is a real server round-trip
/// (push_event -> diff -> OP_TEXT). Requires the test server on localhost:4040.
final class CounterE2ETest: XCTestCase {
    private var app: XCUIApplication!

    override func setUpWithError() throws {
        continueAfterFailure = false
        app = launchSample()
    }

    func testRendersCounterAndIncrementsOverTheWire() {
        tapButton(app, "Counter")
        waitForText(app, "Count: 0")

        tapButton(app, "+")
        waitForText(app, "Count: 1")
        tapButton(app, "+")
        waitForText(app, "Count: 2")

        tapButton(app, "-")
        waitForText(app, "Count: 1")
    }
}
