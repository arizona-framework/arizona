package dev.arizona.client

import kotlinx.serialization.json.Json
import kotlinx.serialization.json.int
import kotlinx.serialization.json.jsonArray
import kotlinx.serialization.json.jsonObject
import kotlinx.serialization.json.jsonPrimitive
import org.junit.Assert.assertEquals
import org.junit.Test

/**
 * Pure JVM tests (no emulator) of the interleaver, against the actual first
 * frame the Arizona server emits for /native/counter (captured from the live
 * server). Run with: ./gradlew :arizona:testDebugUnitTest
 */
class InterleaverTest {

    // The {f, s, d} payload from the real OP_REPLACE first frame.
    private val frame0 = """
        {"d":["native_counter","0"],"f":"1M2KTR","s":[
        "{\"type\":\"Column\",\"az\":\"1M2KTR-0\",\"az_view\":true,\"id\":",
        ",\"children\":[{\"type\":\"Text\",\"az\":\"1M2KTR-1\",\"children\":[\"Count: \",{\"type\":\"#slot\",\"az\":\"1M2KTR-1t0\",\"children\":[",
        "]}]},{\"type\":\"Button\",\"az\":\"1M2KTR-2\",\"on_tap\":[0,\"inc\"],\"children\":[\"+\"]},{\"type\":\"Button\",\"az\":\"1M2KTR-3\",\"on_tap\":[0,\"dec\"],\"children\":[\"-\"]}]}"
        ]}
    """.trimIndent()

    @Test
    fun interleavesCounterFrameIntoValidWidgetTree() {
        val payload = Json.parseToJsonElement(frame0).jsonObject
        val tree = Json.parseToJsonElement(Interleaver(FingerprintCache()).interleave(payload)).jsonObject

        assertEquals("Column", tree.getValue("type").jsonPrimitive.content)
        assertEquals("native_counter", tree.getValue("id").jsonPrimitive.content)

        val children = tree.getValue("children").jsonArray
        val text = children[0].jsonObject
        assertEquals("Text", text.getValue("type").jsonPrimitive.content)
        val textKids = text.getValue("children").jsonArray
        assertEquals("Count: ", textKids[0].jsonPrimitive.content)
        val slot = textKids[1].jsonObject
        assertEquals("#slot", slot.getValue("type").jsonPrimitive.content)
        assertEquals("0", slot.getValue("children").jsonArray[0].jsonPrimitive.content)

        // Event props are raw command arrays ([push_event, name]), not strings.
        val incBtn = children[1].jsonObject
        assertEquals(0, incBtn.getValue("on_tap").jsonArray[0].jsonPrimitive.int)
        assertEquals("inc", incBtn.getValue("on_tap").jsonArray[1].jsonPrimitive.content)
    }

    @Test
    fun reusesStaticsCachedByFingerprint() {
        val interleaver = Interleaver(FingerprintCache())
        // First frame caches statics for f=1M2KTR.
        interleaver.interleave(Json.parseToJsonElement(frame0).jsonObject)
        // A later frame sends only {f, d}; statics come from the cache.
        val cachedOnly = Json.parseToJsonElement("""{"f":"1M2KTR","d":["native_counter","5"]}""").jsonObject
        val tree = Json.parseToJsonElement(interleaver.interleave(cachedOnly)).jsonObject
        val slot = tree.getValue("children").jsonArray[0].jsonObject
            .getValue("children").jsonArray[1].jsonObject
        assertEquals("5", slot.getValue("children").jsonArray[0].jsonPrimitive.content)
    }
}
