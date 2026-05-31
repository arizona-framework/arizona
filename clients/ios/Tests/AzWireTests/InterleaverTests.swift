import XCTest
@testable import AzWire

/// Pure tests of the interleaver, against the actual first frames the Arizona
/// server emits for /native/counter and /native/list (captured live, shared
/// verbatim with the Android client's InterleaverTest). Run anywhere: swift test.
final class InterleaverTests: XCTestCase {
    // The {f, s, d} payload from the real /native/counter OP_REPLACE first frame.
    let counterFrame = ##"{"d":["native_counter","0"],"f":"1M2KTR","s":["{\"type\":\"Column\",\"az\":\"1M2KTR-0\",\"az_view\":true,\"id\":",",\"children\":[{\"type\":\"Text\",\"az\":\"1M2KTR-1\",\"children\":[\"Count: \",{\"type\":\"#slot\",\"az\":\"1M2KTR-1t0\",\"children\":[","]}]},{\"type\":\"Button\",\"az\":\"1M2KTR-2\",\"on_tap\":[0,\"inc\"],\"children\":[\"+\"]},{\"type\":\"Button\",\"az\":\"1M2KTR-3\",\"on_tap\":[0,\"dec\"],\"children\":[\"-\"]}]}"]}"##

    // The /native/list OP_REPLACE first frame: a Column whose #slot wraps an
    // each-array of keyed Row items.
    let listFrame = ##"{"d":["native_list",{"d":[["1","One"],["2","Two"],["3","Three"]],"f":"O1M0B","s":["{\"type\":\"Row\",\"az\":\"O1M0B-0\",\"az_key\":",",\"children\":[{\"type\":\"#slot\",\"az\":\"O1M0B-0t0\",\"children\":[","]}]}"],"t":0}],"f":"JW7VZ","s":["{\"type\":\"Column\",\"az\":\"JW7VZ-0\",\"az_view\":true,\"id\":",",\"children\":[{\"type\":\"#slot\",\"az\":\"JW7VZ-0t0\",\"children\":[","]}]}"]}"##

    func testInterleavesCounterFrameIntoValidWidgetTree() {
        let tree = Interleaver(FingerprintCache()).interleave(JSONValue.parse(counterFrame))

        XCTAssertEqual(tree["type"]?.stringValue, "Column")
        XCTAssertEqual(tree["id"]?.stringValue, "native_counter")

        let children = tree["children"]!.arrayValue!
        XCTAssertEqual(children[0]["type"]?.stringValue, "Text")
        let textKids = children[0]["children"]!.arrayValue!
        XCTAssertEqual(textKids[0].stringValue, "Count: ")
        XCTAssertEqual(textKids[1]["type"]?.stringValue, "#slot")
        XCTAssertEqual(textKids[1]["children"]!.arrayValue![0].stringValue, "0")

        // Event props are raw command arrays ([push_event, name]), not strings.
        XCTAssertEqual(children[1]["on_tap"]![0].intValue, 0)
        XCTAssertEqual(children[1]["on_tap"]![1].stringValue, "inc")
    }

    func testReusesStaticsCachedByFingerprint() {
        let interleaver = Interleaver(FingerprintCache())
        _ = interleaver.interleave(JSONValue.parse(counterFrame))
        // A later frame sends only {f, d}; statics come from the cache.
        let tree = interleaver.interleave(JSONValue.parse(##"{"f":"1M2KTR","d":["native_counter","5"]}"##))
        let slot = tree["children"]![0]["children"]![1]
        XCTAssertEqual(slot["children"]!.arrayValue![0].stringValue, "5")
    }

    func testInterleavesStreamFrameIntoKeyedRows() {
        let tree = Interleaver(FingerprintCache()).interleave(JSONValue.parse(listFrame))
        XCTAssertEqual(tree["type"]?.stringValue, "Column")
        let container = tree["children"]![0]
        XCTAssertEqual(container["type"]?.stringValue, "#slot")
        let rows = container["children"]![0].arrayValue!
        XCTAssertEqual(rows.count, 3)
        XCTAssertEqual(rows[0]["az_key"]?.stringValue, "1")
        XCTAssertEqual(rows[2]["children"]![0]["children"]![0].stringValue, "Three")
    }

    func testDecodesStreamInsertItemFromCachedStatics() {
        let interleaver = Interleaver(FingerprintCache())
        _ = interleaver.interleave(JSONValue.parse(listFrame))
        // OP_INSERT ships only {f, d} for the new item; decode rebuilds the Row.
        let item = interleaver.decode(JSONValue.parse(##"{"f":"O1M0B","d":["9","Nine"]}"##))
        XCTAssertEqual(item["type"]?.stringValue, "Row")
        XCTAssertEqual(item["az_key"]?.stringValue, "9")
        XCTAssertEqual(item["children"]![0]["children"]![0].stringValue, "Nine")
    }

    func testDecodesNavigateCommandProp() {
        // A folded navigate command prop is a raw [10, path] array in the statics;
        // it survives interleaving as a JSON array the client dispatches on tap.
        let frame = ##"{"d":[],"f":"MENU1","s":["{\"type\":\"Button\",\"on_tap\":[10,\"/native/counter\"],\"children\":[\"Counter\"]}"]}"##
        let node = Interleaver(FingerprintCache()).interleave(JSONValue.parse(frame))
        XCTAssertEqual(node["type"]?.stringValue, "Button")
        XCTAssertEqual(node["on_tap"]![0].intValue, 10)
        XCTAssertEqual(node["on_tap"]![1].stringValue, "/native/counter")
    }
}
