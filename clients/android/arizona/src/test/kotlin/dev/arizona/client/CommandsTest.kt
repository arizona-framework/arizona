package dev.arizona.client

import kotlinx.serialization.json.Json
import kotlinx.serialization.json.jsonArray
import org.junit.Assert.assertEquals
import org.junit.Test

/**
 * A command value is one command or a list of them, told apart as the web
 * client's executeJS does. Mirrors clients/ios Tests/AzWireTests/DispatchTests.swift
 * (testCommandsTellsOneCommandFromAList) and e2e/utils/native_client.test.js.
 *
 * Run with ./gradlew :arizona:testDebugUnitTest
 */
class CommandsTest {
    private fun arr(json: String) = Json.parseToJsonElement(json).jsonArray

    @Test
    fun keepsASingleCommandWhole() {
        assertEquals(listOf(arr("""[0,"inc"]""")), commands(arr("""[0,"inc"]""")))
    }

    @Test
    fun splitsAListIntoItsCommandsInOrder() {
        assertEquals(
            listOf(arr("""[0,"save",{"id":"7"}]"""), arr("""[10,"/n"]""")),
            commands(arr("""[[0,"save",{"id":"7"}],[10,"/n"]]""")),
        )
    }
}
