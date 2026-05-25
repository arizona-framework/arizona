package dev.arizona.sample

import androidx.compose.ui.test.junit4.createAndroidComposeRule
import androidx.compose.ui.test.onNodeWithText
import androidx.compose.ui.test.performClick
import org.junit.Rule
import org.junit.Test

/**
 * On-device server push: navigate to /native/ticker; a handle_info timer pushes
 * count updates unsolicited, so the displayed count rises without any tap. Run
 * with ./gradlew :sample:connectedCheck (server at 10.0.2.2:4040).
 */
class TickerE2ETest {
    @get:Rule
    val rule = createAndroidComposeRule<MainActivity>()

    @Test
    fun countRisesFromServerPush() {
        rule.waitForText("Ticker")
        rule.onNodeWithText("Ticker").performClick()
        // No tap: the server's timer drives the increments.
        rule.waitForText("Tick: 1")
        rule.waitForText("Tick: 2")
    }
}
