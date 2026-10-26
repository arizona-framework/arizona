import XCTest

/// On-device server push: navigate to /json/ticker; a handle_info timer pushes
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
        // No tap: the server's timer drives the increments. It ticks every
        // second and XCUITest polls about as often, so waiting for an exact
        // "Tick: N" can step over the one window that label is on screen. Read
        // whatever count is showing, then wait for a larger one.
        let tick = app.descendants(matching: .any)
            .matching(NSPredicate(format: "label BEGINSWITH %@", "Tick: ")).firstMatch
        XCTAssertTrue(tick.waitForExistence(timeout: 20), "expected a 'Tick: N' label")
        guard let first = count(tick.label) else {
            return XCTFail("unparseable tick label '\(tick.label)'")
        }
        waitUntil("the tick count to rise past \(first)") {
            tick.exists && (self.count(tick.label) ?? -1) > first
        }
    }

    private func count(_ label: String) -> Int? {
        Int(label.dropFirst("Tick: ".count))
    }
}
