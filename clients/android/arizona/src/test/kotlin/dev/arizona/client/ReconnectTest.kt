package dev.arizona.client

import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertNotNull
import org.junit.Assert.assertNull
import org.junit.Assert.assertTrue
import org.junit.Test

/**
 * Reconnect decisions, as pure functions plus the frame handler that feeds them.
 * Mirrors clients/ios Tests/AzWireTests/ReconnectTests.swift and
 * e2e/utils/native_client.test.js.
 *
 * Run with ./gradlew :arizona:testDebugUnitTest
 */
class ReconnectTest {
    private val rootStatics =
        """["{\"type\":\"Column\",\"az\":\"R-0\",\"az_view\":true,\"id\":",""" +
            """",\"children\":[]}"]"""

    // A view that crashes right after every remount must keep backing off.
    @Test
    fun aCrashRightAfterTheMountKeepsBackingOff() {
        assertFalse(resetsBackoff(4500, mountedAt = 1_000, now = 1_000 + 9_999))
        assertTrue(resetsBackoff(4500, mountedAt = 1_000, now = 1_000 + 10_000))
    }

    @Test
    fun aMountFailureNeverResetsTheBackoff() {
        assertFalse(resetsBackoff(4501, mountedAt = 1_000, now = 1_000 + 60_000))
    }

    @Test
    fun onlyAMountedSessionResetsTheBackoff() {
        assertFalse(resetsBackoff(1006, mountedAt = null, now = 1_000))
        assertTrue(resetsBackoff(1006, mountedAt = 1_000, now = 1_000))
    }

    // The mount is the OP_REPLACE, not any frame carrying ops.
    @Test
    fun anOpReplaceMarksTheMount() {
        val client = AzClient("http://localhost:4040", "/native/x")
        client.handleText("""{"o":[]}""")
        assertNull(client.mountedAt)
        client.handleText(
            """{"o":[[8,"native_x",{"f":"R","s":$rootStatics,"d":["native_x"]}]]}""",
        )
        assertNotNull(client.mountedAt)
    }

    // A full navigate keeps the query: it rides as plain params, as the browser
    // connect carries it.
    @Test
    fun theReconnectUrlCarriesPathAndQuery() {
        assertEquals(
            "ws://h/ws?_az_path=%2Fgated&tab=b&x=1&_az_reconnect=1",
            reconnectUrl("ws://h/ws?_az_path=", "/gated?tab=b&x=1"),
        )
        assertEquals(
            "ws://h/ws?_az_path=%2Fgated&_az_reconnect=1",
            reconnectUrl("ws://h/ws?_az_path=", "/gated"),
        )
    }
}
