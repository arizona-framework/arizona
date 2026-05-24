package dev.arizona.sample

import androidx.compose.ui.test.junit4.createAndroidComposeRule
import androidx.compose.ui.test.onAllNodesWithText
import androidx.compose.ui.test.onNodeWithText
import androidx.compose.ui.test.performClick
import org.junit.Rule
import org.junit.Test

/**
 * On-device multiple independent counters: navigate to /native/multi and tap the
 * middle counter's "+"; only its count changes. Run with
 * ./gradlew :sample:connectedCheck (server at 10.0.2.2:4040).
 */
class MultiE2ETest {
    @get:Rule
    val rule = createAndroidComposeRule<MainActivity>()

    @Test
    fun incrementsOneCounterIndependently() {
        rule.waitForText("Multi")
        rule.onNodeWithText("Multi").performClick()
        rule.waitForText("A: 0")

        // Tap counter B's "+" (the second of the three "+" buttons).
        rule.onAllNodesWithText("+")[1].performClick()
        rule.waitForText("B: 1")
        // A and C untouched.
        rule.onNodeWithText("A: 0").assertExists()
        rule.onNodeWithText("C: 0").assertExists()
    }
}
