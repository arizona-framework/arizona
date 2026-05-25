import XCTest

/// On-device multiple independent counters: navigate to /native/multi and tap the
/// middle counter's "+"; only its count changes. The Android analogue is
/// MultiE2ETest.
final class MultiE2ETest: XCTestCase {
    private var app: XCUIApplication!

    override func setUpWithError() throws {
        continueAfterFailure = false
        app = launchSample()
    }

    func testIncrementsOneCounterIndependently() {
        tapButton(app, "Multi")
        waitForText(app, "A: 0")

        // Tap counter B's "+" (the second of the three "+" buttons).
        let plusButtons = app.buttons.matching(NSPredicate(format: "label == %@", "+"))
        let second = plusButtons.element(boundBy: 1)
        XCTAssertTrue(second.waitForExistence(timeout: 10), "expected a second '+' button")
        second.tap()

        waitForText(app, "B: 1")
        // A and C untouched.
        waitForText(app, "A: 0")
        waitForText(app, "C: 0")
    }
}
