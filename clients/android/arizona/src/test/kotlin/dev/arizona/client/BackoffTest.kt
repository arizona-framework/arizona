package dev.arizona.client

import org.junit.Assert.assertTrue
import org.junit.Test

/**
 * Pure JVM tests (no emulator) of the reconnect backoff: each attempt's delay
 * lands in the jittered band around its step (±20%), capping at the 10s step.
 * Mirrors e2e/utils/native_client.js's backoff(). Run with:
 * ./gradlew :arizona:testDebugUnitTest
 */
class BackoffTest {

    private fun assertBand(attempt: Int, lo: Long, hi: Long) {
        repeat(10_000) {
            val d = backoffDelayMs(attempt)
            assertTrue("attempt $attempt -> $d not in [$lo, $hi]", d in lo..hi)
        }
    }

    @Test
    fun eachAttemptJittersAroundItsStep() {
        assertBand(0, 800, 1200)
        assertBand(1, 1600, 2400)
        assertBand(2, 4000, 6000)
        assertBand(3, 8000, 12000)
    }

    @Test
    fun capsAtTheLastStepBeyondTheTable() {
        // Attempt >= 4 stays on the 10s step (8000..12000 jittered).
        assertBand(4, 8000, 12000)
        assertBand(99, 8000, 12000)
    }

    @Test
    fun jitterProducesVariation() {
        // Not a constant: over many draws we should see more than one value.
        val seen = (1..1000).map { backoffDelayMs(0) }.toSet()
        assertTrue("expected jitter to vary, got $seen", seen.size > 1)
    }
}
