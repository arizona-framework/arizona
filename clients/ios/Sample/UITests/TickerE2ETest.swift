import XCTest

/// On-device server push: navigate to /native/ticker; a handle_info timer pushes
/// count updates unsolicited, so the displayed count rises without any tap. The
/// Android analogue is TickerE2ETest.
final class TickerE2ETest: XCTestCase {
    private var app: XCUIApplication!

    override func setUpWithError() throws {
        continueAfterFailure = false
        app = launchSample()
    }

    func testCountRisesFromServerPush() {
        tapButton(app, "Ticker")
        // No tap: the server's timer drives the increments.
        waitForText(app, "Tick: 1")
        waitForText(app, "Tick: 2")
    }
}
