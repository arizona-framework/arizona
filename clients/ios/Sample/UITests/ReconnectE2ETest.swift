import XCTest

/// On-device reconnect-with-backoff -- the analogue of the Android
/// ReconnectE2ETest. Navigate to the counter and drive it; force an abrupt socket
/// drop (the `-uitest` toolbar's "drop" button calls client.forceDrop()); the
/// client reopens on its own (re-mounting via _az_reconnect=1, so the count
/// resets to 0) and the new socket works -- no relaunch.
final class ReconnectE2ETest: XCTestCase {
    private var app: XCUIApplication!

    override func setUpWithError() throws {
        continueAfterFailure = false
        app = launchSample()
    }

    func testReopensAndRecoversAfterADroppedConnection() {
        tapButton(app, "Counter")
        waitForText(app, "Count: 0")

        // Drive it once so the pre-drop state is distinguishable (count = 1).
        tapButton(app, "+")
        waitForText(app, "Count: 1")

        // Abruptly drop the socket -> reconnect with backoff. The placeholder
        // flashes during the ~1s gap, then a fresh mount re-renders at 0.
        tapButton(app, "dbg-drop")
        waitForText(app, "Count: 0", timeout: 15)

        // The new socket round-trips.
        tapButton(app, "+")
        waitForText(app, "Count: 1")
    }
}
