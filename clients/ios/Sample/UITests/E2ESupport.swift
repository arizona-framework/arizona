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

    /// Launch the sample app with the `-uitest` debug toolbar enabled, and wait
    /// until it has rendered its first server frame.
    ///
    /// Every case starts from the server-rendered menu, so the menu's first
    /// button proves the whole chain is up: app launched, WebSocket connected,
    /// mount frame applied. That cold start is a different budget from an
    /// in-session round-trip -- it also pays the app launch, which a loaded CI
    /// runner can stretch to tens of seconds -- so it gets its own, and the
    /// per-assertion timeouts stay tight. Failing here names the launch rather
    /// than blaming whichever assertion happened to come first.
    func launchSample(
        file: StaticString = #filePath,
        line: UInt = #line
    ) -> XCUIApplication {
        let app = XCUIApplication()
        app.launchArguments = ["-uitest"]
        app.launch()
        XCTAssertTrue(
            app.buttons["Counter"].waitForExistence(timeout: 60),
            "the app never rendered its first server frame -- is the test server on :4040?",
            file: file, line: line)
        return app
    }
}
