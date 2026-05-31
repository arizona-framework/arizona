import XCTest

extension XCUIApplication {
    /// The first element of any type whose accessibility label matches `label`
    /// (a SwiftUI Text's content, or a Button's title). Mirrors Android's
    /// `onNodeWithText`, which matches any node carrying the text.
    func firstElement(label: String) -> XCUIElement {
        descendants(matching: .any).matching(NSPredicate(format: "label == %@", label)).firstMatch
    }
}

extension XCTestCase {
    /// Wait until an element showing `text` is present -- i.e. a server frame has
    /// arrived and rendered. The XCUITest analogue of the Android e2e's
    /// `waitForText`.
    func waitForText(
        _ app: XCUIApplication,
        _ text: String,
        timeout: TimeInterval = 20,
        file: StaticString = #filePath,
        line: UInt = #line
    ) {
        XCTAssertTrue(
            app.firstElement(label: text).waitForExistence(timeout: timeout),
            "expected to find '\(text)'", file: file, line: line)
    }

    /// Wait until no element showing `text` is present -- e.g. after a stream
    /// remove or an OP_REMOVE_NODE.
    func waitForNoText(
        _ app: XCUIApplication,
        _ text: String,
        timeout: TimeInterval = 20,
        file: StaticString = #filePath,
        line: UInt = #line
    ) {
        let gone = XCTNSPredicateExpectation(
            predicate: NSPredicate(format: "exists == false"),
            object: app.firstElement(label: text))
        XCTAssertEqual(
            XCTWaiter().wait(for: [gone], timeout: timeout), .completed,
            "expected '\(text)' to disappear", file: file, line: line)
    }

    /// Wait for a button (by title or accessibility id) and tap it.
    func tapButton(
        _ app: XCUIApplication,
        _ label: String,
        timeout: TimeInterval = 20,
        file: StaticString = #filePath,
        line: UInt = #line
    ) {
        let button = app.buttons[label]
        XCTAssertTrue(
            button.waitForExistence(timeout: timeout),
            "expected button '\(label)'", file: file, line: line)
        button.tap()
    }

    /// Launch the sample app with the `-uitest` debug toolbar enabled.
    func launchSample() -> XCUIApplication {
        let app = XCUIApplication()
        app.launchArguments = ["-uitest"]
        app.launch()
        return app
    }
}
