package dev.arizona.sample

import androidx.compose.ui.test.junit4.createAndroidComposeRule
import androidx.compose.ui.test.onAllNodesWithText
import androidx.compose.ui.test.onNodeWithText
import androidx.compose.ui.test.performClick
import org.junit.Rule
import org.junit.Test

/**
 * On-device (emulator) e2e -- the Playwright analogue for the native target.
 * Launches the demo app, which renders the server's /native/counter view over a
 * real WebSocket, and drives it: tap "+", count goes up; tap "-", it goes down.
 * Each change is a real server round-trip (push_event -> diff -> OP_TEXT).
 *
 * Requires the Arizona test server reachable at 10.0.2.2:4040 (the emulator's
 * route to the host). Run with: ./gradlew :sample:connectedCheck
 */
class CounterE2ETest {
    @get:Rule
    val rule = createAndroidComposeRule<MainActivity>()

    private fun waitForText(text: String, timeoutMillis: Long = 10_000) {
        rule.waitUntil(timeoutMillis) {
            rule.onAllNodesWithText(text).fetchSemanticsNodes().isNotEmpty()
        }
    }

    @Test
    fun rendersCounterAndIncrementsOverTheWire() {
        // The first frame arrives asynchronously over the WebSocket.
        waitForText("Count: 0")

        // Tap "+" -> server round-trip -> count becomes 1, then 2.
        rule.onNodeWithText("+").performClick()
        waitForText("Count: 1")
        rule.onNodeWithText("+").performClick()
        waitForText("Count: 2")

        // Tap "-" -> back to 1.
        rule.onNodeWithText("-").performClick()
        waitForText("Count: 1")
    }
}
