package dev.arizona.sample

import androidx.compose.ui.test.junit4.createAndroidComposeRule
import androidx.compose.ui.test.onNodeWithText
import androidx.compose.ui.test.performClick
import kotlinx.serialization.json.addJsonObject
import kotlinx.serialization.json.buildJsonObject
import kotlinx.serialization.json.put
import kotlinx.serialization.json.putJsonArray
import org.junit.Rule
import org.junit.Test

/**
 * On-device (emulator) e2e for the keyed list -- the Kotlin-renderer analogue of
 * e2e/native/list.spec.js. Navigate to /native/list, then drive the stream ops
 * directly via the client (the list view has no buttons), exercising
 * OP_INSERT / OP_REMOVE / OP_MOVE / OP_ITEM_PATCH / OP_UPDATE on the renderer.
 * Payloads carry real JSON types (numeric `pos`, an array for `reset`), which
 * the Map<String,String> signature couldn't express.
 *
 * Presence/absence/text assertions verify the diff applied; exact positional
 * order after a move stays in the JS spec, where tree introspection is clean.
 *
 * Requires the Arizona test server reachable at localhost:4040 (adb reverse
 * tcp:4040 tcp:4040). Run with: ./gradlew :sample:connectedCheck
 */
class ListE2ETest {
    @get:Rule
    val rule = createAndroidComposeRule<MainActivity>()

    @Test
    fun appliesStreamOpsOverTheWire() {
        // Launcher shows the menu; navigate to the list (initial 3 keyed items).
        rule.waitForText("List")
        rule.onNodeWithText("List").performClick()
        rule.waitForText("One")
        rule.waitForText("Two")
        rule.waitForText("Three")

        val client = rule.activity.client

        // OP_INSERT (append).
        client.pushEvent("add", buildJsonObject { put("id", "9"); put("text", "Nine") })
        rule.waitForText("Nine")

        // OP_REMOVE.
        client.pushEvent("remove", buildJsonObject { put("id", "2") })
        rule.waitForNoText("Two")

        // OP_MOVE: numeric pos -- '1' to index 2. The item stays present (exact
        // order is asserted in the JS spec).
        client.pushEvent("move", buildJsonObject { put("id", "1"); put("pos", 2) })
        rule.waitForText("One")

        // OP_ITEM_PATCH: update item 9's text in place.
        client.pushEvent("update", buildJsonObject { put("id", "9"); put("text", "Updated") })
        rule.waitForText("Updated")
        rule.waitForNoText("Nine")

        // OP_UPDATE (full re-render via reset) -- an array-of-objects payload.
        client.pushEvent(
            "reset",
            buildJsonObject {
                putJsonArray("items") {
                    addJsonObject { put("id", "a"); put("text", "A") }
                    addJsonObject { put("id", "b"); put("text", "B") }
                }
            },
        )
        rule.waitForText("A")
        rule.waitForText("B")
        rule.waitForNoText("Three")
    }
}
