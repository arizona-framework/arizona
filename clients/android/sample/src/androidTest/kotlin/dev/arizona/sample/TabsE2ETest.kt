package dev.arizona.sample

import androidx.compose.ui.test.junit4.createAndroidComposeRule
import androidx.compose.ui.test.onNodeWithText
import androidx.compose.ui.test.performClick
import org.junit.Rule
import org.junit.Test

/**
 * On-device conditional rendering: navigate to /native/tabs and switch tabs;
 * the content subtree swaps (push_event -> OP_UPDATE). Run with
 * ./gradlew :sample:connectedCheck (server at 10.0.2.2:4040).
 */
class TabsE2ETest {
    @get:Rule
    val rule = createAndroidComposeRule<MainActivity>()

    @Test
    fun swapsContentWhenTabSelected() {
        rule.waitForText("Tabs")
        rule.onNodeWithText("Tabs").performClick()
        rule.waitForText("Welcome home")

        rule.onNodeWithText("About").performClick()
        rule.waitForText("About Arizona")
    }
}
