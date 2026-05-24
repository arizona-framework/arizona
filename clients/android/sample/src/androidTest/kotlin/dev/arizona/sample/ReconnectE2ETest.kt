package dev.arizona.sample

import androidx.compose.ui.test.junit4.createAndroidComposeRule
import androidx.compose.ui.test.onNodeWithText
import androidx.compose.ui.test.performClick
import org.junit.Rule
import org.junit.Test

/**
 * On-device (emulator) e2e for reconnect-with-backoff. Navigate to the counter
 * and drive it; force an abrupt socket drop; the client reopens on its own
 * (re-mounting via _az_reconnect=1, so the count resets to 0) and the new socket
 * works -- no relaunch. The Playwright analogue is e2e/native/reconnect.spec.js.
 *
 * Requires the Arizona test server reachable at localhost:4040 (adb reverse
 * tcp:4040 tcp:4040). Run with: ./gradlew :sample:connectedCheck
 */
class ReconnectE2ETest {
    @get:Rule
    val rule = createAndroidComposeRule<MainActivity>()

    @Test
    fun reopensAndRecoversAfterADroppedConnection() {
        // Launcher shows the menu; navigate to the counter.
        rule.waitForText("Counter")
        rule.onNodeWithText("Counter").performClick()
        rule.waitForText("Count: 0")

        // Drive it once so the pre-drop state is distinguishable (count = 1).
        rule.onNodeWithText("+").performClick()
        rule.waitForText("Count: 1")

        // Abruptly drop the socket -> onFailure -> reconnect with backoff. The
        // placeholder flashes during the ~1s gap, then a fresh mount re-renders.
        rule.activity.client.forceDrop()

        // The reconnect re-mounts fresh, so the count resets to 0...
        rule.waitForText("Count: 0")
        // ...and the new socket round-trips.
        rule.onNodeWithText("+").performClick()
        rule.waitForText("Count: 1")
    }
}
