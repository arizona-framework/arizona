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

    // The {f, s, d} payload from the real /native/list OP_REPLACE first frame: a
    // Column whose #slot wraps an each-array of keyed Row items (captured live).
    private val listFrame0 = """
        {"d":["native_list",{"d":[["1","One"],["2","Two"],["3","Three"]],
        "f":"O1M0B","s":[
        "{\"type\":\"Row\",\"az\":\"O1M0B-0\",\"az_key\":",
        ",\"children\":[{\"type\":\"#slot\",\"az\":\"O1M0B-0t0\",\"children\":[",
        "]}]}"],"t":0}],"f":"JW7VZ","s":[
        "{\"type\":\"Column\",\"az\":\"JW7VZ-0\",\"az_view\":true,\"id\":",
        ",\"children\":[{\"type\":\"#slot\",\"az\":\"JW7VZ-0t0\",\"children\":[",
        "]}]}"]}
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

    @Test
    fun interleavesStreamFrameIntoKeyedRows() {
        val tree = Json.parseToJsonElement(
            Interleaver(FingerprintCache()).interleave(Json.parseToJsonElement(listFrame0).jsonObject),
        ).jsonObject
        assertEquals("Column", tree.getValue("type").jsonPrimitive.content)
        // The container #slot wraps the each-array of keyed Row items.
        val container = tree.getValue("children").jsonArray[0].jsonObject
        assertEquals("#slot", container.getValue("type").jsonPrimitive.content)
        val rows = container.getValue("children").jsonArray[0].jsonArray
        assertEquals(3, rows.size)
        assertEquals("1", rows[0].jsonObject.getValue("az_key").jsonPrimitive.content)
        assertEquals(
            "Three",
            rows[2].jsonObject.getValue("children").jsonArray[0].jsonObject
                .getValue("children").jsonArray[0].jsonPrimitive.content,
        )
    }

    @Test
    fun decodesStreamInsertItemFromCachedStatics() {
        val interleaver = Interleaver(FingerprintCache())
        // The first frame caches the item statics (f=O1M0B).
        interleaver.interleave(Json.parseToJsonElement(listFrame0).jsonObject)
        // OP_INSERT ships only {f, d} for the new item; decode rebuilds the Row.
        val item = Json.parseToJsonElement(
            interleaver.decode(Json.parseToJsonElement("""{"f":"O1M0B","d":["9","Nine"]}""")),
        ).jsonObject
        assertEquals("Row", item.getValue("type").jsonPrimitive.content)
        assertEquals("9", item.getValue("az_key").jsonPrimitive.content)
        val slot = item.getValue("children").jsonArray[0].jsonObject
        assertEquals("Nine", slot.getValue("children").jsonArray[0].jsonPrimitive.content)
    }

    @Test
    fun decodesNavigateCommandProp() {
        // A folded navigate command prop is a raw [10, path] array in the statics
        // (EFFECT_NAVIGATE); it survives interleaving as a JSON array the client
        // dispatches on tap.
        val frame =
            """
            {"d":[],"f":"MENU1","s":[
            "{\"type\":\"Button\",\"on_tap\":[10,\"/native/counter\"],\"children\":[\"Counter\"]}"]}
            """.trimIndent()
        val node = Json.parseToJsonElement(
            Interleaver(FingerprintCache()).interleave(Json.parseToJsonElement(frame).jsonObject),
        ).jsonObject
        assertEquals("Button", node.getValue("type").jsonPrimitive.content)
        val cmd = node.getValue("on_tap").jsonArray
        assertEquals(10, cmd[0].jsonPrimitive.int)
        assertEquals("/native/counter", cmd[1].jsonPrimitive.content)
    }
}
