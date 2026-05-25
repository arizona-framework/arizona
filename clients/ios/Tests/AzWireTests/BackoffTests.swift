import XCTest
@testable import AzWire

/// Pure tests of the reconnect backoff: each attempt's delay lands in the
/// jittered band around its step (+/-20%), capping at the 10s step. Mirrors the
/// reference client's backoff. Run anywhere: swift test.
final class BackoffTests: XCTestCase {
    private func assertBand(_ attempt: Int, _ lo: Int, _ hi: Int) {
        for _ in 0..<10_000 {
            let d = backoffDelayMs(attempt)
            XCTAssertTrue((lo...hi).contains(d), "attempt \(attempt) -> \(d) not in [\(lo), \(hi)]")
        }
    }

    func testEachAttemptJittersAroundItsStep() {
        assertBand(0, 800, 1200)
        assertBand(1, 1600, 2400)
        assertBand(2, 4000, 6000)
        assertBand(3, 8000, 12000)
    }

    func testCapsAtTheLastStepBeyondTheTable() {
        assertBand(4, 8000, 12000)
        assertBand(99, 8000, 12000)
    }

    func testJitterProducesVariation() {
        let seen = Set((0..<1000).map { _ in backoffDelayMs(0) })
        XCTAssertTrue(seen.count > 1, "expected jitter to vary, got \(seen)")
    }
}
