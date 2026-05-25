package dev.arizona.sample

import androidx.compose.ui.test.junit4.ComposeTestRule
import androidx.compose.ui.test.onAllNodesWithText

/**
 * Wait until at least one node showing [text] is present -- i.e. a server frame
 * has arrived and rendered. Shared by the on-device example tests, which all
 * launch the menu and navigate to a view before exercising it.
 */
fun ComposeTestRule.waitForText(text: String, timeoutMillis: Long = 10_000) {
    waitUntil(timeoutMillis) {
        onAllNodesWithText(text).fetchSemanticsNodes().isNotEmpty()
    }
}

/** Wait until no node showing [text] is present -- e.g. after a stream remove. */
fun ComposeTestRule.waitForNoText(text: String, timeoutMillis: Long = 10_000) {
    waitUntil(timeoutMillis) {
        onAllNodesWithText(text).fetchSemanticsNodes().isEmpty()
    }
}
