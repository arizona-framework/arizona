package dev.arizona.sample

import androidx.compose.ui.semantics.SemanticsProperties
import androidx.compose.ui.semantics.getOrNull
import androidx.compose.ui.test.hasText
import androidx.compose.ui.test.junit4.createAndroidComposeRule
import androidx.compose.ui.test.onNodeWithText
import androidx.compose.ui.test.performClick
import org.junit.Rule
import org.junit.Test

/**
 * On-device server push: navigate to /json/ticker; a handle_info timer pushes
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
        // No tap: the server's timer drives the increments. Waiting for an exact
        // "Tick: N" can step over the one second that label is on screen, so
        // read whatever count is showing, then wait for a larger one.
        var first = -1
        rule.waitUntil(10_000) {
            first = tickCount() ?: -1
            first >= 0
        }
        rule.waitUntil(10_000) { (tickCount() ?: -1) > first }
    }

    private fun tickCount(): Int? =
        rule.onAllNodes(hasText("Tick: ", substring = true))
            .fetchSemanticsNodes()
            .firstNotNullOfOrNull { node ->
                node.config.getOrNull(SemanticsProperties.Text)
                    ?.joinToString("") { it.text }
                    ?.removePrefix("Tick: ")
                    ?.toIntOrNull()
            }
}
