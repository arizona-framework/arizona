import XCTest

/// On-device navigation: from the menu, navigate to a view (same-socket
/// transition -> OP_REPLACE), then the chrome "Menu" button back. The Android
/// analogue is MenuE2ETest.
final class MenuE2ETest: XCTestCase {
    private var app: XCUIApplication!

    override func setUpWithError() throws {
        continueAfterFailure = false
        app = launchSample()
    }

    func testNavigatesToViewAndBack() {
        // Forward: menu -> counter.
        tapButton(app, "Counter")
        waitForText(app, "Count: 0")

        // Back: chrome "Menu" button -> the menu re-mounts.
        tapButton(app, "\u{2630} Menu")
        waitForText(app, "Counter")
    }
}
