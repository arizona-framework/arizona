package dev.arizona.sample

import androidx.compose.ui.test.junit4.createAndroidComposeRule
import androidx.compose.ui.test.onNodeWithText
import androidx.compose.ui.test.performClick
import org.junit.Rule
import org.junit.Test

/**
 * On-device (emulator) e2e -- the Playwright analogue for the native target.
 * The launcher opens the menu; this navigates to /native/counter and drives it:
 * tap "+", count goes up; tap "-", down. Each change is a real server round-trip
 * (push_event -> diff -> OP_TEXT).
 *
 * Requires the Arizona test server reachable at 10.0.2.2:4040. Run with:
 * ./gradlew :sample:connectedCheck
 */
class CounterE2ETest {
    @get:Rule
    val rule = createAndroidComposeRule<MainActivity>()

    @Test
    fun rendersCounterAndIncrementsOverTheWire() {
        // Launcher shows the menu; navigate to the counter.
        rule.waitForText("Counter")
        rule.onNodeWithText("Counter").performClick()
        rule.waitForText("Count: 0")

        // Tap "+" -> server round-trip -> count becomes 1, then 2.
        rule.onNodeWithText("+").performClick()
        rule.waitForText("Count: 1")
        rule.onNodeWithText("+").performClick()
        rule.waitForText("Count: 2")

        // Tap "-" -> back to 1.
        rule.onNodeWithText("-").performClick()
        rule.waitForText("Count: 1")
    }
}
