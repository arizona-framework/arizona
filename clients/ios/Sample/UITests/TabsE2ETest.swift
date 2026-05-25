import XCTest

/// On-device conditional rendering: navigate to /native/tabs and switch tabs;
/// the content subtree swaps (push_event -> OP_UPDATE). The Android analogue is
/// TabsE2ETest.
final class TabsE2ETest: XCTestCase {
    private var app: XCUIApplication!

    override func setUpWithError() throws {
        continueAfterFailure = false
        app = launchSample()
    }

    func testSwapsContentWhenTabSelected() {
        tapButton(app, "Tabs")
        waitForText(app, "Welcome home")

        tapButton(app, "About")
        waitForText(app, "About Arizona")
    }
}
