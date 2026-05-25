package dev.arizona.sample

import androidx.compose.ui.test.junit4.createAndroidComposeRule
import androidx.compose.ui.test.onNodeWithText
import androidx.compose.ui.test.performClick
import org.junit.Rule
import org.junit.Test

/**
 * On-device nested event routing -- the Kotlin analogue of e2e/native/nested.spec.js.
 * Navigate to /native/nested (two stateful child counters); tapping a child's
 * button routes the event to THAT child's view id, so only its count changes.
 * Each child shows "<label><count>" (e.g. "A0") and a button labelled "<label>".
 *
 * Requires the Arizona test server reachable at localhost:4040 (adb reverse
 * tcp:4040 tcp:4040). Run with: ./gradlew :sample:connectedCheck
 */
class NestedE2ETest {
    @get:Rule
    val rule = createAndroidComposeRule<MainActivity>()

    @Test
    fun routesTapToTheTappedChild() {
        rule.waitForText("Nested")
        rule.onNodeWithText("Nested").performClick()
        rule.waitForText("A0")
        rule.waitForText("B0")

        // Tap child A's button ("A") -> routes to child_a only.
        rule.onNodeWithText("A").performClick()
        rule.waitForText("A1")
        rule.onNodeWithText("B0").assertExists() // B untouched

        // Tap child B's button ("B") -> routes to child_b; A stays at 1.
        rule.onNodeWithText("B").performClick()
        rule.waitForText("B1")
        rule.onNodeWithText("A1").assertExists()
    }
}
