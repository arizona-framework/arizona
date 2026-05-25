package dev.arizona.sample

import androidx.compose.ui.test.junit4.createAndroidComposeRule
import androidx.compose.ui.test.onNodeWithText
import androidx.compose.ui.test.performClick
import org.junit.Rule
import org.junit.Test

/**
 * On-device navigation: from the menu, tap a button to navigate to another view
 * (same-socket transition -> OP_REPLACE), then the chrome "Menu" button to
 * navigate back. Run with ./gradlew :sample:connectedCheck (server at
 * 10.0.2.2:4040).
 */
class MenuE2ETest {
    @get:Rule
    val rule = createAndroidComposeRule<MainActivity>()

    @Test
    fun navigatesToViewAndBack() {
        rule.waitForText("Counter")
        // Forward: menu -> counter.
        rule.onNodeWithText("Counter").performClick()
        rule.waitForText("Count: 0")

        // Back: chrome "Menu" button -> the menu re-mounts.
        rule.onNodeWithText("☰ Menu").performClick()
        rule.waitForText("Counter")
    }
}
