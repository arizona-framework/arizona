package dev.arizona.sample

import androidx.compose.ui.test.junit4.createAndroidComposeRule
import androidx.compose.ui.test.onNodeWithText
import androidx.compose.ui.test.performClick
import org.junit.Rule
import org.junit.Test

/**
 * On-device OP_REMOVE_NODE (the `remove` sentinel). Navigate to /native/removable
 * and tap "Hide": the banner's text dynamic returns `remove`, the server diffs to
 * op 4, and the client must drop the node rather than crash. The Playwright
 * analogue is e2e/native/removable.spec.js.
 *
 * Requires the Arizona test server reachable at localhost:4040 (adb reverse
 * tcp:4040 tcp:4040). Run with: ./gradlew :sample:connectedCheck
 */
class RemovableE2ETest {
    @get:Rule
    val rule = createAndroidComposeRule<MainActivity>()

    @Test
    fun dropsNodeOnTheRemoveSentinel() {
        rule.waitForText("Removable")
        rule.onNodeWithText("Removable").performClick()
        rule.waitForText("Banner!")

        rule.onNodeWithText("Hide").performClick()
        rule.waitForNoText("Banner!")
    }
}
